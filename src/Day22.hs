{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Day22 where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.List
import           Data.Maybe

data Spell = MagicMissle | Drain | Shield | Poison | Recharge | Fizzle
  deriving (Eq, Show, Enum)
data Effect = ShieldUp | Shielded | ShieldDown | Poisoned | Recharging
  deriving (Eq, Show, Enum)
data Entity = Player | Boss
  deriving (Eq, Show)

data Stats = Stats
  { _hp      :: Int
  , _attack  :: Int
  , _armor   :: Int
  , _mana    :: Int
  , _effects :: [[Effect]]
  }
  deriving (Show)
data GameState = GameState
  { _roundNum   :: Int
  , _turn       :: Entity
  , _player     :: Stats
  , _boss       :: Stats
  , _spellsCast :: [Spell]
  , _gameWinner :: Maybe Entity
  }
  deriving (Show)
makeLenses ''Stats
makeLenses ''GameState

partOne = (\g -> (manaUsage g,g)) $ head $
  sortOn manaUsage $
  filter playerWinner $ last $
  takeWhile (any (isNothing . (^.gameWinner))) $
  iterate (genRounds gameRound) [gameStart]

partTwo = (\g -> (manaUsage g,g)) $ head $
  sortOn manaUsage $
  filter playerWinner $ last $
  takeWhile (any (isNothing . (^.gameWinner))) $
  iterate (genRounds gameRound2) [gameStart]

winner :: GameState -> Maybe Entity
winner gs
  | gs ^. boss ^. hp <= 0 = Just Player
  | gs ^. player ^. hp <= 0 = Just Boss
  | otherwise = Nothing

spellCost :: Spell -> Int
spellCost MagicMissle = 53
spellCost Drain = 73
spellCost Shield = 113
spellCost Poison = 173
spellCost Recharge = 229

spellEffect :: Spell -> State GameState ()
spellEffect MagicMissle = return ()
spellEffect Drain = return ()
spellEffect Shield = do
  gs <- get
  es <- use $ player.effects
  player.effects .= es ++ [concat [[ShieldUp],replicate 5 Shielded,[ShieldDown]]]
spellEffect Poison = do
  gs <- get
  es <- use $ boss.effects
  boss.effects .= es ++ [replicate 6 Poisoned]
spellEffect Recharge = do
  gs <- get
  es <- use $ player.effects
  player.effects .= es ++ [replicate 5 Recharging]

castSpell :: Spell -> State GameState ()
castSpell Fizzle = gameWinner .= Just Boss
castSpell s = do
  spellDamage s
  spellEffect s
  player.mana -= spellCost s
  scs <- use spellsCast
  spellsCast .= scs ++ [s]

spellDamage :: Spell -> State GameState ()
spellDamage MagicMissle = boss.hp -= 4
spellDamage Drain = do
  boss.hp -= 2
  player.hp += 2
spellDamage Shield = return ()
spellDamage Poison = return ()
spellDamage Recharge = return ()

spellsGivenEffects :: [Effect] -> [Spell]
spellsGivenEffects [] = [MagicMissle .. Recharge]
spellsGivenEffects es = [MagicMissle .. Recharge] \\ mapMaybe spellsGivenEffect es
  where
    spellsGivenEffect ShieldUp = Just Shield
    spellsGivenEffect ShieldDown = Nothing
    spellsGivenEffect Shielded = Just Shield
    spellsGivenEffect Poisoned = Just Poison
    spellsGivenEffect Recharging = Just Recharge

nextTurn :: Entity -> Entity
nextTurn Player = Boss
nextTurn Boss = Player

effectResult ::  Effect -> State GameState ()
effectResult ShieldUp = player.armor += 7
effectResult ShieldDown = player.armor -= 7
effectResult Shielded = return ()
effectResult Poisoned = boss.hp -= 3
effectResult Recharging = player.mana += 101

playerWinner :: GameState -> Bool
playerWinner g
  | isJust (g^.gameWinner) = fromJust (g^.gameWinner) == Player
  | otherwise = False

bestState :: [GameState] -> Maybe GameState
bestState gs = if null bs then Nothing else Just (head bs)
  where
    bs = sortOn manaUsage $ filter playerWinner gs
manaUsage :: GameState -> Int
manaUsage gs = sum $ map spellCost (gs^.spellsCast)

genRounds :: (([Spell] -> GameState -> Spell) -> State GameState ()) -> [GameState] -> [GameState]
genRounds gr gs = concatMap nextGs gs
  where
    bestSoFar = bestState gs
    nextGs g
      | isJust (g^.gameWinner) = [g]
      | g^.turn == Boss = [execState (gr undefined) g]
      | isNothing bestSoFar || (isJust bestSoFar && (manaUsage g < manaUsage (fromJust bestSoFar)))
        = map (\s -> execState (gr (\sps _ ->
          if s `elem` sps then s else Fizzle)) g)
          [MagicMissle .. Recharge]
      | otherwise = []

gameRound2 :: ([Spell] -> GameState -> Spell) -> State GameState ()
gameRound2 f = do
  t <- use turn
  when (t==Player) $ player.hp -= 1
  checkWinner $ gameRound f

gameRound :: ([Spell] -> GameState -> Spell) -> State GameState ()
gameRound f = do
  applyEffects
  checkWinner $ do
    t <- use turn
    attackRound f t
    checkWinner $ do
      roundNum += 1
      turn .= nextTurn t

checkWinner :: State GameState () -> State GameState ()
checkWinner f = do
  gW <- use gameWinner
  unless (isJust gW) $ do
    gs <- get
    gameWinner .= winner gs
    when (isNothing (winner gs)) f

applyEffects :: State GameState ()
applyEffects = do
  bEs <- use $ boss.effects
  pEs <- use $ player.effects
  mapM_ (effectResult . head) (pEs ++ bEs)
  boss.effects .= filter (not . null) (map tail bEs)
  player.effects .= filter (not . null) (map tail pEs)

spellsAvailable :: GameState -> [Spell]
spellsAvailable gs = filter (\s -> spellCost s <= m) (spellsGivenEffects es)
  where
    es = concat $ gs^.boss^.effects ++ gs^.player^.effects
    m = gs^.player^.mana

attackRound :: ([Spell] -> GameState -> Spell) -> Entity -> State GameState ()
attackRound _ Boss = do
  att <- use $ boss . attack
  arm <-  use $ player . armor
  let dmg = if (att-arm) < 1 then 1 else att-arm
  player . hp -= dmg
attackRound sp Player = do
  gs <- get
  if null (spellsAvailable gs)
  then gameWinner .= Just Boss
  else castSpell $ sp (spellsAvailable gs) gs

gameStart :: GameState
gameStart = GameState
  { _roundNum = 0
  , _turn = Player
  , _player = Stats
    { _hp = 50
    , _attack = 0
    , _armor = 0
    , _mana = 500
    , _effects = []
    }
  , _boss = Stats
    { _hp = 55
    , _attack = 8
    , _armor = 0
    , _mana = 0
    , _effects = []
    }
  , _spellsCast = []
  , _gameWinner = Nothing
  }
