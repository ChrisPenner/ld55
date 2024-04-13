module Router where

import Control.Arrow
import FRP.Yampa hiding (tag)
import           Control.Lens hiding (from, to)
import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable
import           Types

spawn
    :: (Ord k)
    => (forall x. ObjectMap msg k s x -> k)
    -> Maybe k
    -> s
    -> ObjSF msg c k s
    -> ObjectMap msg k s (ObjSF msg c k s)
    -> ObjectMap msg k s (ObjSF msg c k s)
spawn gen Nothing s sf m = m & #objm_map %~ M.insert (gen m) (s, sf)
spawn _ (Just k)  s sf m = m & #objm_map %~ M.insert k (s, sf)

send
    :: (Ord k)
    => k
    -> k
    -> SomeMsg msg
    -> Endo (ObjectMap msg k s (ObjSF msg c k s))
send from to msg = Endo $ #objm_undeliveredMsgs . at to . non mempty <>~ [(from, msg)]

recv :: forall k v msg. (Typeable v, Eq (msg v)) => [(k, SomeMsg msg)] -> msg v -> [(k, v)]
recv [] _ = []
recv ((from, SomeMsg key (val :: v')) : xs) tag
  | Just Refl <- eqT @v @v'
  , key == tag
  = (from, val) : recv xs tag
  | otherwise = recv xs tag

decodeCommand
    :: Ord k
    => (forall x. M.Map k x -> k)
    -> (k -> c -> Endo (ObjectMap msg k s (ObjSF msg c k s)))
    -> k
    -> Command msg c k s
    -> Endo (ObjectMap msg k s (ObjSF msg c k s))
decodeCommand _ _ from Unspawn = Endo $ #objm_map %~ M.delete from
decodeCommand gen _ _ (Spawn k s obj) = Endo $ #objm_map %~ \oo -> M.insert (fromMaybe (gen oo) k) (s, obj) oo
decodeCommand _ _ from (Broadcast msg)
  = Endo $ \objm -> flip appEndo objm
                  $ foldMap (flip (send from) msg)
                  $ M.keys
                  $ objm_map objm
decodeCommand _ other from (OtherCommand c) = other from c

router
    :: forall msg c k s
     . ( Show k, Ord k
       , forall v. Eq (msg v)
       )
    => (forall x. M.Map k x -> k)
    -> (k -> c -> Endo (ObjectMap msg k s (ObjSF msg c k s)))
    -> ObjectMap msg k s (ObjSF msg c k s)
    -> SF FrameInfo (ObjectMap msg k s (ObjectOutput msg c k s))
router gen other st =
  pSwitch
    @(ObjectMap msg k s)
    @(FrameInfo)
    @(ObjectInput msg k s)
    @(ObjectOutput msg c k s)
    @(Endo (ObjectMap msg k s (ObjSF msg c k s)))
    (\rfi col -> col & #objm_map %~
        (M.mapWithKey $ \k (s, a) -> (s, )
            $ (ObjectInput rfi k (fmap fst $ objm_map col)
            $ ObjectInEvents
            $ recv
            $ join
            $ maybeToList
            $ M.lookup k
            $ objm_undeliveredMsgs col
          , ) a
        ))
    st
    ((arr (\(_, om) ->
      flip mappend (pure $ Endo id)
        $ flip foldMap (M.toList $ objm_map om)
        $ \(k, (_, oo)) ->
            pure $ mconcat
              [ Endo $ #objm_map . at k . _Just . _1 .~ oo_state oo
              , flip foldMap (oo_outbox oo)   $ uncurry (send k)
              , flip foldMap (oo_commands oo) $ decodeCommand gen other k
              ]
          )
     >>> notYet)
    )
    (\new f -> router gen other $ appEndo f $ new & #objm_undeliveredMsgs .~ mempty)

