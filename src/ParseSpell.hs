module ParseSpell where

import Control.Lens
import Data.Set (Set)
import Types

payloadWithinSpell :: Traversal' Spell Payload
payloadWithinSpell f = \case
  Standard p -> Standard <$> f p
  Projectile p s -> Projectile <$> f p <*> pure s
  Explosion p s -> Explosion <$> f p <*> pure s
  Concurrent a b -> Concurrent <$> payloadWithinSpell f a <*> payloadWithinSpell f b

twoXSpell :: Spell -> Spell
twoXSpell = over payloadWithinSpell twoXPayload
  where
    twoXPayload = \case
      DamageDesc d m -> DamageDesc (2 * d) (2 * m)
      MovementDesc m -> MovementDesc (2 * m)

negateSpell :: Spell -> Spell
negateSpell = over payloadWithinSpell negatePayload
  where
    negatePayload = \case
      DamageDesc d m -> DamageDesc (-d) (-m)
      MovementDesc m -> MovementDesc (-m)

finisherPayload :: Finisher -> Payload
finisherPayload = \case
  Attack -> DamageDesc 1 0
  Defend -> DamageDesc (-0) 0
  Move -> MovementDesc 1

parseSpell :: Finisher -> [Rune] -> Spell
parseSpell finisher = \case
  [] -> Standard $ finisherPayload finisher
  Rune2x : rs ->
    twoXSpell $ parseSpell finisher rs
  RuneProjectile : rs -> Projectile (finisherPayload finisher) . Just $ parseSpell finisher rs
  RuneExplosion : rs -> (Explosion (finisherPayload finisher)) . Just $ (parseSpell finisher rs)
  RuneAndThen : rs -> parseSpell finisher rs
  RuneNegate : rs -> negateSpell $ parseSpell finisher rs

-- data AttackRune
--   = AttackProjectile
--   | AttackExplosion

-- data ModRune = Mod2x | ModNegate

-- type RunesWithModifiers = [((Payload -> Payload), AttackRune)]
