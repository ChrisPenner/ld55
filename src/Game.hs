{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards            #-}

module Game where

import Debug.Trace (trace)
import qualified Data.Set as S
import Data.Set (Set)
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Maybe (maybeToList, listToMaybe)
import Data.Monoid
import Data.Ord (clamp)
import Drawing
import FRP.Yampa hiding (dot, normalize, (*^))
import GHC.Generics hiding (to)
import ParseSpell (parseSpell)
import Router
import SDL hiding (Event, Stereo, Vector, copy, delay, trace)
import Types
import Prelude hiding (last)
import Data.Functor
import Geometry

aspectRatio :: RealFloat a => a
aspectRatio = 16 / 9

logicalSize :: RealFloat a => V2 a
logicalSize = screenSize

screenSize :: RealFloat a => V2 a
screenSize = V2 (h * aspectRatio) h
  where
    h = 540

game :: Int -> SF FrameInfo Renderable
game num_players =
  fmap (\z ->
    mconcat
     [ drawGameTextureOriginRect Texture_Background (OriginRect screenSize 0) 0 0 $ pure False
     , z
     ]
       ) $
  fmap (foldMap oo_render) $
    router (maybe (Other 0)
                  (\case
                    Player _ -> Other 0
                    Other n -> Other $ n + 1
                  )
                  . fmap fst . M.lookupMax) mempty $
      ObjectMap mempty $ M.fromList $ do
        (i, cty) <- zip [0 .. num_players - 1] $ Keyboard : repeat Gamepad
        pure $ (Player i,
          ( GState
              { gs_position = 200 + V2 (fromIntegral i * 200) 0
              , gs_color = V4 255 0 0 255
              , gs_size = 15
              }
          , ourDude i cty
          ))

deltaTime :: SF () Time
deltaTime = loopPre 0 $ proc (_, old) -> do
  nowish <- localTime -< ()
  returnA -< (nowish - old, nowish)

fireBall :: V2 Double -> Dude
fireBall velocity = proc oi -> do
  dt <- deltaTime -< ()
  t <- localTime -< ()
  let timeLeft = ttl - t
  e <- delayEvent ttl <<< now () -< ()
  let newState =
        oi_state oi
          & #gs_position +~ dt *^ velocity
          & #gs_color . _w .~ round (timeLeft * 255 / ttl)
  let commands = e & foldMap \() -> [Unspawn]
  returnA
    -<
      ObjectOutput
        { oo_outbox = mempty,
          oo_commands = commands,
          oo_render = renderGState newState,
          oo_state = newState
        }
  where
    ttl = 2


runRuneSet :: V2 Double -> ControllerType -> Rune -> Rune -> Rune -> Rune -> SF Controller (Renderable, [Rune])
runRuneSet pos cty r1 r2 r3 r4 = proc c -> do
  (r1_ev, draw_rune1) <- runeInput (pos + r1pos) r1 <<< edge -< c_zButton c
  (r2_ev, draw_rune2) <- runeInput (pos + r2pos) r2 <<< edge -< c_xButton c
  (r3_ev, draw_rune3) <- runeInput (pos + r3pos) r3 <<< edge -< c_cButton c
  (r4_ev, draw_rune4) <- runeInput (pos + r4pos) r4 <<< edge -< c_vButton c
  returnA -<
    ( mconcat
        [ draw_rune1
        , draw_rune2
        , draw_rune3
        , draw_rune4
        ]
    , onEvent r1_ev id
      <> onEvent r2_ev id
      <> onEvent r3_ev id
      <> onEvent r4_ev id
    )
      where
  xdist = 40
  ydist = 30
  r1pos =
    case cty of
      Gamepad -> V2 0 ydist
      Keyboard -> V2 0 0
  r2pos =
    case cty of
      Gamepad -> V2 xdist 0
      Keyboard -> V2 50 0
  r3pos =
    case cty of
      Gamepad -> V2 (-xdist) 0
      Keyboard -> V2 100 0
  r4pos =
    case cty of
      Gamepad -> V2 0 (-ydist)
      Keyboard -> V2 150 0

data PlayerState = PlayerState
  { ps_pendingRunes :: [Rune]
  , ps_health :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)


ourDude :: Int -> ControllerType -> Dude
ourDude ctrlix cty = loopPre (PlayerState [] 100) $ proc (oi, pstate) -> do
  lt <- localTime -< ()
  let c = (!! ctrlix) $ fi_controls $ oi_fi oi
  let selfKey = oi_self oi
  let getLetters :: Typeable v => GameMsg v -> [(Key, v)]
      getLetters =  oi ^. #oi_inbox . to oie_mailbox
  let mayStartDash = (snd <$> listToMaybe (getLetters Dash))
  let startDashEvent = maybeToEvent mayStartDash
  dashEndTime <- hold 0 -< (startDashEvent <&> \(dashTime, _) -> dashTime + lt)
  endDashEvent <- edge -< (dashEndTime + 0.1) < lt && isNoEvent startDashEvent
  dashSpeedMult <- hold 1 -< ((snd <$> startDashEvent) <|> (endDashEvent $> 1))
  let speed = defaultSpeed * dashSpeedMult
  dt <- deltaTime -< ()
  let dPos = c_leftStick c ^* speed ^* dt
  (draw_runes, new_runes) <-
    runRuneSet
      (V2 100 450 + V2 (fromIntegral ctrlix * 300) 0)
      cty
      (Left Rune2x)
      (Right RuneProjectile)
      (Right RuneExplosion)
      (Left RuneAndThen) -< c

  let dmgsources = oie_mailbox (oi_inbox oi) DamageSource
      hitting_me = do
        let my_rect = originRectToRect hitbox $ gs_position $ oi_state oi
        dsrc@(_, DamageSrc{..}) <- dmgsources
        guard $ ds_originator /= oi_self oi
        guard $ intersects my_rect $ originRectToRect ds_ore ds_pos
        pure dsrc

  kept <- maintain 0.5 -<
    case hitting_me of
      [] -> noEvent
      xs -> Event $ S.fromList xs

  let hit_me = filter (flip S.member kept) hitting_me

  shoot <- edge -< c_okButton c

  let vel = c_leftStick c

  dirFacing <-
    fmap normalize $
      hold (V2 1 0)
        <<< edgeBy
          ( \_ vel ->
              if vel /= 0
                then Just vel
                else Nothing
          )
          0
      -<
        vel

  let commands =
        shoot & foldMap \() ->
          fmap
            ( Spawn
                Nothing
                GState
                  { gs_position = gs_position $ oi_state oi,
                    gs_color = V4 0 0 0 254,
                    gs_size = 5
                  }
            )
            $ maybe []
                (makeSpells selfKey (dirFacing * 300))
                $ parseSpell Attack $ ps_pendingRunes pstate
  let mayTeleportTo = (getLetters Teleport ^? _head) <&> snd
  let newState = oi_state oi
                   & #gs_position +~ dPos
                   & \s -> case mayTeleportTo of
                             Just p -> s & #gs_position .~ p
                             Nothing -> s

      pos = gs_position newState

      anim =
        case vel == 0 of
          True -> Stand
          False -> Walk

      dir =
        if
            | dot dirFacing (V2 (-1) 0) > 0.7 -> DirLeft
            | dot dirFacing (V2 1 0) > 0.7 -> DirRight
            | dot dirFacing (V2 0 1) > 0.7 -> DirDown
            | dot dirFacing (V2 0 (-1)) > 0.7 -> DirUp
            | otherwise -> DirDown

  sprite <- mkAnim Wizard -< (DrawSpriteDetails (LpcAnim dir anim) 0 $ pure False, pos)

  returnA
    -< let fontsize = 9
           margin = 1
           str = show $ ps_health pstate
           strlen = fromIntegral $ length str
        in
      ( ObjectOutput
          { oo_outbox = fmap (second $ const $ SomeMsg YouHitMe ()) hit_me
          , oo_commands = commands
          , oo_render = mconcat
              [ sprite
              , mconcat $ do
                  let pendingRunes = ps_pendingRunes pstate
                      stride = 20
                      offset = fromIntegral ((length pendingRunes - 1) * stride) / 2
                  (i, rt) <- zip [id @Int 0..] pendingRunes
                  let rune_ore = mkGroundOriginRect $ V2 17 25
                  pure
                    $ drawGameTextureOriginRect
                        (runeTexture rt)
                        rune_ore
                        (pos + V2 (fromIntegral i * stride - offset) (-64))
                        0
                    $ pure False
              , draw_runes
              , mconcat $ do
                  (_, DamageSrc{..}) <- dmgsources
                  pure $ drawOriginRect (V4 255 0 0 128) ds_ore ds_pos
              , drawOriginRect (V4 0 0 0 192)
                    (OriginRect (V2 (fontsize * strlen + 2 * margin) (fontsize + 2 * margin)) 0)
                  $ pos + V2 (- fontsize * (strlen / 2) - margin) (6 - margin)
              , drawText fontsize (V3 255 255 255) str (pos + V2 (- fontsize * (strlen / 2)) 6)
              ]
          , oo_state = newState
          }
      , pstate
          & #ps_health -~ sum (fmap (ds_damage . snd) hit_me)
          & #ps_pendingRunes %~ \pendingRunes ->
              event id (const $ const []) shoot pendingRunes <> new_runes
      )
  where
    defaultSpeed = 100
    hitbox = mkGroundOriginRect $ V2 17 38

renderGState :: GState -> Renderable
renderGState gs =
  drawOriginRect
    (gs_color gs)
    (mkCenterdOriginRect $ gs_size gs)
    (gs_position gs)

withLifetime :: Time -> SF a (Double, Event (), [Command msg c k s])
withLifetime ttl = proc _ -> do
  t <- localTime -< ()
  e <- delayEvent ttl <<< now () -< ()
  returnA -< (t / ttl, e, onEvent' e Unspawn)

spellTtl :: Double
spellTtl = 0.5

onEvent :: (Applicative f, Monoid (f b)) => Event a -> (a -> b) -> f b
onEvent ev f = foldMap (pure . f) ev

onEvent' :: (Applicative f, Monoid (f b)) => Event a -> b -> f b
onEvent' ev = onEvent ev . const

spellContinuation ::
  Key ->
  Time ->
  V2 Double ->
  Maybe Spell ->
  SF GState (Double, Event() {- is about to die -}, [Command GameMsg GameCommand Key GState])
spellContinuation playerKey ttl dir k = proc s -> do
  (perc, on_die, die_cmds) <- withLifetime ttl -< ()
  returnA
    -<
      ( perc,
        on_die,
        mconcat
          [ die_cmds,
            join $ onEvent' on_die $ do
              spell <- maybeToList k
              dude <- makeSpells playerKey dir spell
              pure $ Spawn Nothing s dude
          ]
      )

makeSpells :: Key -> V2 Double -> Spell -> [Dude]
makeSpells playerKey dir  =
  let normalizedDir = normalize dir
   in \case
    (Standard _) -> []
    (Projectile payload k) ->
      pure $ proc oi -> do
        (_, isDying, cmds) <- spellContinuation playerKey 1 normalizedDir k -< oi_state oi
        dt <- deltaTime -< ()
        let st =
              oi_state oi
                & #gs_position +~ normalizedDir * 400 ^* dt
                & #gs_size .~ 10
                & #gs_color .~ V4 0 0 0 255
        let outBox = case payload of
              MovementDesc{} -> onEvent' isDying (playerKey, SomeMsg Teleport $ st ^. #gs_position)
              _ -> mempty
        returnA
          -<
            ObjectOutput
              { oo_outbox = outBox
              , oo_commands = Broadcast (SomeMsg DamageSource
                      ( DamageSrc
                        { ds_originator = playerKey
                        , ds_damage = 20
                        , ds_pos = gs_position st
                        , ds_ore = mkCenterdOriginRect $ gs_size st
                        }
                      ))
                  : cmds
              , oo_render = renderGState st
              , oo_state = st
              }
    (Explosion DamageDesc{} k) ->
      pure $ proc oi -> do
        (perc, _isDying, cmds) <- spellContinuation playerKey spellTtl dir k -< oi_state oi
        let st =
              oi_state oi
                & #gs_size .~ perc *^ 100
                & #gs_color .~ V4 255 0 255 128
        returnA
          -<
            ObjectOutput
              { oo_outbox = mempty,
                oo_commands = cmds,
                oo_render = renderGState st,
                oo_state = st
              }
    (Explosion MovementDesc{p_speedModifier} k) ->
      pure $ proc oi -> do
        (_perc, _isDying, cmds) <- spellContinuation playerKey spellTtl dir k -< oi_state oi
        let st = oi_state oi
        returnA
          -<
            ObjectOutput
              { oo_outbox = [(playerKey, SomeMsg Dash (spellTtl, p_speedModifier))],
                oo_commands = cmds,
                oo_render = mempty,
                oo_state = st
              }
    (Concurrent x y) ->
      makeSpells playerKey (mkRotMatrix (pi / 10) !* dir) x
        <> makeSpells playerKey (mkRotMatrix (-pi / 10) !* dir) y


mkRotMatrix :: Double -> M22 Double
mkRotMatrix theta =
  V2
    (V2 (cos theta) (negate $ sin theta))
    (V2 (sin theta) (cos theta))


cooldown :: Time -> SF (Event a) (Double, Event a, Event ())
cooldown wait = loopPre 0 $ proc (ev, ok_at) -> do
  t <- localTime -< ()
  let is_ok = t >= ok_at

  let gated_ev = gate ev is_ok
  next_ok_at <- hold 0 -< (t + wait) <$ gated_ev

  next_ok <- edge -< next_ok_at <= t
  returnA -< ((clamp (0, 1) ((next_ok_at - t) / wait), gated_ev, next_ok), next_ok_at)


runeInput :: V2 Double -> Rune -> SF (Event a) (Event Rune, Renderable)
runeInput pos gt = proc ev -> do
  (perc_available, on_use, on_refresh) <- cooldown 0.1 -< ev
  end_refresh <- delayEvent 0.15 -< on_refresh
  want_halo <-
    fmap getAny $ hold (Any False)
      -<
        asum
          [ Any True <$ on_refresh,
            Any False <$ end_refresh
          ]

  let ore = mkCenterdOriginRect $ V2 35 50
  returnA
    -<
      ( gt <$ on_use,
        mconcat
          [ drawGameTextureOriginRect (runeTexture gt) ore pos 0 (pure False)
          , drawOriginRect
                (V4 0 0 0
                  $ round
                  $ if perc_available == 0
                        then 0
                        else max 92 $ perc_available * 255
                ) ore pos
          , if want_halo
              then drawOriginRect (V4 255 255 0 64) ore pos
              else mempty
          ]
      )


maintain :: Ord a => Time -> SF (Event (Set a)) (Set a)
maintain t = loopPre mempty $ proc (ev, kept) -> do
  (_, add_ev, _) <- cooldown t -< fmap (S.\\ kept) ev
  remove_ev <- delayEvent t -< add_ev
  let actions = appEndo $ mconcat
        [ event mempty (foldMap $ Endo . S.insert) add_ev
        , event mempty (foldMap $ Endo . S.delete) remove_ev
        ]
  returnA -< (event mempty id add_ev, actions kept)


runeTexture :: Rune -> GameTexture
runeTexture (Left Rune2x) = Texture_UnoPlusTwo
runeTexture (Right RuneProjectile) = Texture_UnoWild
runeTexture (Right RuneExplosion) = Texture_UnoReverse
runeTexture (Left RuneAndThen) = Texture_UnoSkip
