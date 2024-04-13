{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiWayIf #-}

module Game where

import GHC.Generics
import Control.Lens
import Control.Monad
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Word
import Drawing
import FRP.Yampa hiding (dot, normalize, (*^))
import GHC.Generics
import ParseSpell (parseSpell)
import Router
import SDL hiding (delay, Event, Stereo, Vector, copy)
import Types
import Prelude hiding (last)
import Data.Ord (clamp)
import Data.Monoid
import Control.Applicative


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
     , drawText 16 (V3 0 0 0) "yo what up" 30
     ]
       ) $
  fmap (foldMap oo_render) $
    router (maybe 0 (+ 1) . fmap fst . M.lookupMax) mempty $
      ObjectMap mempty $ M.fromList $ do
        (i, cty) <- zip [0 .. num_players - 1] $ Keyboard : repeat Gamepad
        pure $ (i, (GState {gs_position = 200 + V2 (fromIntegral i * 200) 0, gs_color = V4 255 0 0 255, gs_size = 15}, ourDude i cty))

deltaTime :: SF () Time
deltaTime = loopPre 0 $ proc (_, old) -> do
  nowish <- localTime -< ()
  returnA -< (nowish - old, nowish)

playerLogic :: SF (Controller) (V2 Double)
playerLogic = proc c -> do
  dt <- deltaTime -< ()
  returnA -< c_leftStick c * playerSpeed ^* dt
  where
    playerSpeed :: V2 Double
    playerSpeed = 100

data FireballState = FireballState
  { fs_position :: V2 Double,
    fs_velocity :: V2 Double,
    fs_color :: V4 Word8
  }
  deriving stock (Eq, Ord, Show, Generic)

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
  (r1_ev, draw_rune1) <- runeInput (pos + r1pos) Rune2x <<< edge -< c_zButton c
  (r2_ev, draw_rune2) <- runeInput (pos + r2pos) RuneProjectile <<< edge -< c_xButton c
  (r3_ev, draw_rune3) <- runeInput (pos + r3pos) RuneExplosion <<< edge -< c_cButton c
  (r4_ev, draw_rune4) <- runeInput (pos + r4pos) RuneAndThen <<< edge -< c_vButton c
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


ourDude :: Int -> ControllerType -> Dude
ourDude ctrlix cty = loopPre [] $ proc (oi, pendingRunes) -> do
  let c = (!! ctrlix) $ fi_controls $ oi_fi oi
  dPos <- playerLogic -< c
  (draw_runes, new_runes) <-
    runRuneSet (V2 100 450 + V2 (fromIntegral ctrlix * 300) 0) cty Rune2x RuneProjectile RuneExplosion RuneAndThen -< c

  shoot <- edge -< c_okButton c

  let vel = c_leftStick c

  dirFacing <- fmap normalize $
    hold (V2 1 0)
      <<< edgeBy
        ( \_ vel ->
            if vel /= 0
              then Just vel
              else Nothing
        )
        0
      -< vel


  let commands =
        shoot & foldMap \() ->
          fmap
          ( Spawn
             Nothing
             GState
               { gs_position = gs_position $ oi_state oi
               , gs_color = V4 0 0 0 254
               , gs_size = 5
               }
          ) $ makeSpells (dirFacing * 300)
                     (parseSpell Attack pendingRunes)

  let newState = oi_state oi & #gs_position +~ dPos
      pos = gs_position newState

      anim =
        case vel == 0 of
          True -> Slash
          False -> Walk

      dir = if
              | dot dirFacing (V2 (-1) 0) > 0.7 -> DirLeft
              | dot dirFacing (V2 1 0) > 0.7 -> DirRight
              | dot dirFacing (V2 0 1) > 0.7 -> DirDown
              | dot dirFacing (V2 0 (-1)) > 0.7 -> DirUp
              | otherwise -> DirDown


  sprite <- mkAnim Wizard -< (DrawSpriteDetails (LpcAnim dir anim) 0 $ pure False, pos)

  returnA
    -<
      ( ObjectOutput
          { oo_outbox = mempty
          , oo_commands = commands
          , oo_render = mconcat
              [ sprite
              , mconcat $ do
                  let stride = 20
                      offset = fromIntegral ((length pendingRunes - 1) * stride) / 2
                  (i, rt) <- zip [id @Int 0..] pendingRunes
                  let ore = mkGroundOriginRect $ V2 17 25
                  pure
                    $ drawGameTextureOriginRect (runeTexture rt) ore (pos + V2 (fromIntegral i * stride - offset) (-64)) 0
                    $ pure False
              , draw_runes
              ]
          , oo_state = newState
          }
      , event id (const $ const []) shoot $ pendingRunes
          <> new_runes
      )

renderGState :: GState -> Renderable
renderGState gs =
  drawOriginRect (gs_color gs)
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
  Time ->
  V2 Double ->
  Maybe Spell ->
  SF GState (Double, [Command GameMsg GameCommand Key GState])
spellContinuation ttl dir k = proc s -> do
  (perc, on_die, die_cmds) <- withLifetime ttl -< ()
  returnA
    -<
      ( perc,
        mconcat
          [ die_cmds,
            join $ onEvent' on_die $ do
              spell <- maybeToList k
              dude <- makeSpells dir spell
              pure $ Spawn Nothing s dude
          ]
      )

makeSpells :: V2 Double -> Spell -> [Dude]
makeSpells _ (Standard _) = []
makeSpells (normalize -> dir) (Projectile _ k) =
  pure $ proc oi -> do
    (_, cmds) <- spellContinuation 1 dir k -< oi_state oi
    dt <- deltaTime -< ()
    let st =
          oi_state oi
            & #gs_position +~ dir * 400 ^* dt
            & #gs_size .~ 10
            & #gs_color .~ V4 0 0 0 255
    returnA
      -<
        ObjectOutput
          { oo_outbox = mempty,
            oo_commands = cmds,
            oo_render = renderGState st,
            oo_state = st
          }
makeSpells dir (Explosion _ k) =
  pure $ proc oi -> do
    (perc, cmds) <- spellContinuation spellTtl dir k -< oi_state oi
    let st = oi_state oi
          & #gs_size .~ perc *^ 100
          & #gs_color .~ V4 255 0 255 128
    returnA -<
      ObjectOutput
        { oo_outbox = mempty
        , oo_commands = cmds
        , oo_render = renderGState st
        , oo_state = st
        }
makeSpells dir (Concurrent x y) =
  makeSpells (mkRotMatrix (pi / 10) !* dir) x
   <> makeSpells (mkRotMatrix (-pi / 10) !* dir) y


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
  (perc_available, on_use, on_refresh) <- cooldown 1.5 -< ev
  end_refresh <- delayEvent 0.15 -< on_refresh
  want_halo <- fmap getAny $ hold (Any False) -< asum
    [ Any True  <$ on_refresh
    , Any False <$ end_refresh
    ]

  let ore = mkCenterdOriginRect $ V2 35 50

  returnA -<
    ( gt <$ on_use
    , mconcat
        [ drawGameTextureOriginRect (runeTexture gt) ore pos 0 (pure False)
        , drawOriginRect (V4 0 0 0 (round $ if perc_available == 0 then 0 else max 92 (perc_available * 255))) ore pos
        , if want_halo
              then drawOriginRect (V4 255 255 0 64) ore pos
              else mempty
        ]
    )


runeTexture :: Rune -> GameTexture
runeTexture Rune2x = Texture_UnoPlusTwo
runeTexture RuneProjectile = Texture_UnoWild
runeTexture RuneExplosion = Texture_UnoReverse
runeTexture RuneAndThen = Texture_UnoSkip
runeTexture _ = Texture_Rune1


