module ParseSpell where

import Control.Arrow
import Control.Lens
import Data.Either
import Data.Void
import Text.Megaparsec
import Types
import Witherable (mapMaybe)

type Parser = Parsec Void [Rune]

payloads :: Traversal' Spell Payload
payloads f = \case
  Standard p -> Standard <$> f p
  Projectile p s -> Projectile <$> f p <*> traverse (payloads f) s
  Explosion p s -> Explosion <$> f p <*> traverse (payloads f) s
  Concurrent a b -> Concurrent <$> payloads f a <*> payloads f b

dupSpell :: Spell -> Spell
dupSpell orig = setIfNothing orig orig
  where
    setIfNothing x = \case
      Standard p -> Standard p
      Projectile p s ->
        Projectile p ((setIfNothing x <$> s) <|> Just x)
      Explosion p s -> Explosion p ((setIfNothing x <$> s) <|> Just x)
      Concurrent a b -> Concurrent (setIfNothing x a) (setIfNothing x b)

twoXSpell :: Spell -> Spell
twoXSpell = \case
  Standard p -> Concurrent (Standard p) (Standard p)
  Projectile p s -> Concurrent (Projectile p s) (Projectile p s)
  Explosion p s -> Concurrent (Explosion p s) (Explosion p s)
  Concurrent a b -> Concurrent (Concurrent a b) (Concurrent a b)

negateSpell :: Spell -> Spell
negateSpell = over payloads negatePayload
  where
    negatePayload = \case
      DamageDesc d m -> DamageDesc (-d) (-m)
      MovementDesc m -> MovementDesc (-m)

finisherPayload :: Finisher -> Payload
finisherPayload = \case
  Attack -> DamageDesc 1 0
  Defend -> DamageDesc (-1) 0
  Move -> MovementDesc 2

runSpellMods :: [RuneModifier] -> Spell -> Spell
runSpellMods mods spell = foldr applyMod spell mods
  where
    applyMod = \case
      Rune2x -> over payloads twoXPayload
      RuneAndThen -> dupSpell
    twoXPayload = \case
      DamageDesc d m -> DamageDesc (2 * d) (2 * m)
      MovementDesc m -> MovementDesc (2 * m)

parseSpell :: Finisher -> [Rune] -> Maybe Spell
parseSpell finisher runes =
  let (spellMods, rest) = second reverse $ break (not . isModifier) (reverse runes)
      spell = case rest of
        [] -> Nothing
        Left Rune2x : rs ->
          twoXSpell <$> parseSpell finisher rs
        Left RuneAndThen : rs -> parseSpell finisher rs
        Right RuneProjectile : rs -> Just $ Projectile (finisherPayload finisher) $ parseSpell finisher rs
        Right RuneExplosion : rs -> Just $ (Explosion (finisherPayload finisher)) $ parseSpell finisher rs
   in runSpellMods (mapMaybe (either Just (const Nothing)) spellMods) <$> spell
  where
    isModifier = isLeft

-- * [2x, projectile, andthen, 2x]
