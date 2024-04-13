{-# LANGUAGE BlockArguments #-}

module Game where

import Control.Lens
import Control.Monad
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Word
import Drawing
import FRP.Yampa hiding (normalize, (*^))
import GHC.Generics
import ParseSpell (parseSpell)
import Router
import SDL hiding (delay, Event, Stereo, Vector, copy)
import Types
import Prelude hiding (last)
import Data.Ord (clamp)
import Data.Monoid
import Control.Applicative

game :: SF FrameInfo Renderable
game =
  fmap (\z ->
    mconcat
     [ z
     , drawText 16 (V3 0 0 0) "yo what up" 30
     ]
       ) $
  fmap (foldMap oo_render) $
    router (maybe 0 (+ 1) . fmap fst . M.lookupMax) mempty $
      ObjectMap mempty $ M.fromList $
        (0, (GState {gs_position = 200, gs_color = V4 255 0 0 255, gs_size = 30}, ourDude))
        : zip [1..] (fmap (GState {gs_position = 200, gs_color = V4 255 0 0 255, gs_size = 30},)
               $ makeSpells (V2 1 1)
               $ traceShowId $ parseSpell
                        Attack
                        [ Rune2x,
                          RuneProjectile,
                          RuneAndThen,
                          RuneNegate,
                          RuneExplosion
                        ]
                    )

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

ourDude :: Dude
ourDude = loopPre [] $ proc (oi, pendingRunes) -> do
  let c = fi_controls $ oi_fi oi
  dPos <- playerLogic -< c
  (okPressed, draw_rune1) <- runeInput (V2 100 500) Texture_UnoSkip <<< edge -< c_okButton c
  (cancelPressed, draw_rune2) <- runeInput (V2 150 500) Texture_UnoWild <<< edge -< c_cancelButton c

  dirFacing <-
    hold (V2 1 0)
      <<< edgeBy
        ( \_ vel ->
            if vel /= 0
              then Just vel
              else Nothing
        )
        0
      -<
        oi ^. #oi_fi . #fi_controls . #c_leftStick

  let commands =
        okPressed & foldMap \() ->
          [ Spawn Nothing (GState {gs_position = gs_position $ oi_state oi, gs_color = V4 0 255 0 254, gs_size = 20}) $ fireBall (dirFacing * 100)
          ]

  let newState = oi_state oi & #gs_position +~ dPos
      pos = gs_position newState
  returnA
    -<
      ( ObjectOutput
          { oo_outbox = mempty
          , oo_commands = commands
          , oo_render = mconcat
              [ renderGState newState
              , mconcat $ do
                  (i, rt) <- zip [0..] pendingRunes
                  let ore = mkCenterdOriginRect $ V2 17 25
                  pure $ drawGameTextureOriginRect rt ore (pos + V2 (i * 20) (-30)) 0 (pure False)
              , draw_rune1
              , draw_rune2
              ]
          , oo_state = newState
          }
      , pendingRunes
          <> onEvent' okPressed     Texture_UnoSkip
          <> onEvent' cancelPressed Texture_UnoWild
      )

renderGState :: GState -> Renderable
renderGState gs =
  drawFilledRect (gs_color gs) $
    Rectangle (P (gs_position gs)) $
      gs_size gs

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
    (_, cmds) <- spellContinuation spellTtl dir k -< oi_state oi
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

runeInput :: V2 Double -> GameTexture -> SF (Event a) (Event a, Renderable)
runeInput pos gt = proc ev -> do
  (perc_available, on_use, on_refresh) <- cooldown 2 -< ev
  end_refresh <- delayEvent 0.15 -< on_refresh
  want_halo <- fmap getAny $ hold (Any False) -< asum
    [ Any True  <$ on_refresh
    , Any False <$ end_refresh
    ]

  let ore = mkCenterdOriginRect $ V2 35 50

  returnA -<
    ( on_use
    , mconcat
        [ drawGameTextureOriginRect gt ore pos 0 (pure False)
        , drawOriginRect (V4 0 0 0 (round $ perc_available * 255)) ore pos
        , if want_halo
              then drawOriginRect (V4 255 255 0 64) ore pos
              else mempty
        ]
    )


