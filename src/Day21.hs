module Day21 where

import           Control.Lens
import           Data.List
import           Text.Parsec

type Cost = Int
type Damage = Int
type Armor = Int

data Item = Item {_name ::String, _cost :: Cost, _damage :: Damage, _armor::Armor}
  deriving (Show, Eq)

data Shop = Shop Weapons Armors Rings
  deriving (Show)
type Weapons = [Item]
type Armors = [Item]
type Rings = [Item]

type Inventory = [Item]

data Stats = Stats {_hp :: Int, _sDamage :: Int, _sArmor :: Int}
  deriving (Show)

partOne = do
  is <- invChoices <$> shop
  let stats = map (\i -> (invStat i _cost, invStats i)) is
  return $ minimum $ filter snd $ map (\(c,s) -> (c,winner s bossStats)) stats

partTwo = do
  is <- invChoices <$> shop
  let stats = map (\i -> (invStat i _cost, invStats i)) is
  return $ maximum $ filter (not . snd) $ map (\(c,s) -> (c,winner s bossStats)) stats

winner :: Stats -> Stats -> Bool
winner s1 s2 = r s1 s2 <= r s2 s1
  where
    r p1 p2 = length $ takeWhile (\(_,s) -> _hp s > 0) $ iterate battleRound (p1,p2)

battleRound :: (Stats,Stats) -> (Stats,Stats)
battleRound (s1@(Stats hp1 dmg1 arm1),s2@(Stats hp2 dmg2 arm2))
  | arm2 >= dmg1 = (s1,Stats (hp2-1) dmg2 arm2)
  | otherwise = (s1,Stats (hp2-(dmg1 - arm2)) dmg2 arm2)

invStats :: Inventory -> Stats
invStats i = Stats playerHitPoints (invStat i _damage) (invStat i _armor)

invStat :: Inventory -> (Item -> Int) -> Int
invStat i f = sum $ map f i

invChoices :: Shop -> [Inventory]
invChoices (Shop ws as rs) = do
  w <- ws
  a <- nAs
  r1 <- nRs
  r2 <- delete r1 nRs
  return [w,a ,r1,r2]
  where
    nAs = Item "Naked" 0 0 0:as
    nRs = [Item "NoRing1" 0 0 0,Item "NoRing2" 0 0 0]  ++ rs

bossStats :: Stats
bossStats = Stats 100 8 2
playerHitPoints :: Int
playerHitPoints = 100

shop :: IO Shop
shop = either ( error . show) id  . parse shopParser "" <$> readFile "day21-shop.txt"

shopParser = Shop <$> sectionParser "Weapons:" <*> sectionParser "Armor:" <*> sectionParser "Rings:"
sectionParser t = do
  _ <- string t <* spaces <* string "Cost" <* spaces <* string "Damage" <* spaces <* string "Armor" <* endOfLine
  manyTill itemParser (try (pure () <* endOfLine) <|> eof)
itemParser = do
  name <- many1 alphaNum <* spaces
  cost <- read <$> many1 digit <* spaces
  dmg <- read <$> many1 digit <* spaces
  armor <- read <$> many1 digit <* endOfLine
  return $ Item name cost dmg armor
