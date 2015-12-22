{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Day22 where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.List
import           Data.Maybe

data Spell = MagicMissle | Drain | Shield | Poison | Recharge
  deriving (Eq, Show, Enum)
data Effect = ShieldUp | Shielded | ShieldDown | Poisoned | Recharging
  deriving (Eq, Show, Enum)
data Turn = Player | Boss
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
  , _turn       :: Turn
  , _player     :: Stats
  , _boss       :: Stats
  , _spellsCast :: [Spell]
  , _gameOver   :: Bool
  }
  deriving (Show)
makeLenses ''Stats
makeLenses ''GameState

partOne = iterate (execState (gameRound firstSpellChooser)) gameStart

firstSpellChooser :: [Spell] -> GameState -> Spell
firstSpellChooser sps _ = head sps

winner :: GameState -> Maybe Turn
winner gs
  | null $ spellsAvailable gs = Just Boss
  | gs ^. boss ^. hp <= 0 = Just Player
  | gs ^. player ^. hp <= 0 = Just Boss
  | otherwise = Nothing

spellCost :: Spell -> Int
spellCost MagicMissle = 53
spellCost Drain = 73
spellCost Shield = 113
spellCost Poison = 173
spellCost Recharge = 229

spellEffect :: Spell -> [Effect]
spellEffect MagicMissle = []
spellEffect Drain = []
spellEffect Shield = concat [[ShieldUp],replicate 5 Shielded,[ShieldDown]]
spellEffect Poison = replicate 6 Poisoned
spellEffect Recharge = replicate 5 Recharging

castSpell :: Spell -> State GameState ()
castSpell s = do
  spellDamage s
  player.mana -= spellCost s
  scs <- use spellsCast
  spellsCast .= s:scs

spellDamage :: Spell -> State GameState ()
spellDamage MagicMissle = boss.hp -= 4
spellDamage Drain = do
  boss.hp -= 2
  player.hp += 2
spellDamage Shield = return ()
spellDamage Poison = return ()
spellDamage Recharge = return ()

spellsGivenEffect :: Effect -> [Spell]
spellsGivenEffect ShieldUp = [MagicMissle .. Recharge] \\ [Shield]
spellsGivenEffect ShieldDown = [MagicMissle .. Recharge]
spellsGivenEffect Shielded = [MagicMissle .. Recharge] \\ [Shield]
spellsGivenEffect Poisoned = [MagicMissle .. Recharge] \\ [Poison]
spellsGivenEffect Recharging = [MagicMissle .. Recharge] \\ [Recharge]

nextTurn :: Turn -> Turn
nextTurn Player = Boss
nextTurn Boss = Player

effectResult ::  Effect -> State Stats ()
effectResult ShieldUp = armor += 7
effectResult ShieldDown = armor -= 7
effectResult Shielded = return ()
effectResult Poisoned = hp -= 3
effectResult Recharging = mana += 101

gameRound :: ([Spell] -> GameState -> Spell) -> State GameState ()
gameRound f = do
  applyEffects
  checkWinner $ do
    t <- use turn
    attackRound f t
    checkWinner $ do
      roundNum += 1
      turn .= nextTurn t
  where
    checkWinner f = do
      w <- winner <$> get
      gameOver .= isJust w
      when (isNothing w) f

applyEffects :: State GameState ()
applyEffects = return ()
-- do
--   mapM_
--     (\es -> do
--       y .= execState (effectResult (head es)) i
--       --es .= last es
--     ) (i ^. effects)

spellsAvailable :: GameState -> [Spell]
spellsAvailable gs = filter (\s -> spellCost s <= m) sps
  where
    sps = if null sE then [MagicMissle .. Recharge] else sE
    sE = nub $ concatMap spellsGivenEffect $ concat es
    es = gs^.boss^.effects ++ gs^.player^.effects
    m = gs^.player^.mana

attackRound :: ([Spell] -> GameState -> Spell) -> Turn -> State GameState ()
attackRound _ Boss = do
  att <- use $ boss . attack
  arm <-  use $ player . armor
  let dmg = if (att-arm) < 1 then 1 else att-arm
  player . hp -= dmg
attackRound sp Player = do
  gs <- get
  castSpell $ sp (spellsAvailable gs) gs

gameStart :: GameState
gameStart = GameState
  { _roundNum = 0
  , _turn = Boss
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
  , _gameOver = False
  }
