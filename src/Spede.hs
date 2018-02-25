{-# LANGUAGE TemplateHaskell #-}
module Spede where

import System.Random (Random(..), newStdGen, randoms)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.), (<%~))

import Data.Sequence ((|>), ViewL(..), viewl, Seq)
import qualified Data.Sequence as Seq

import Data.Maybe (isJust)

data Game = Game
  { _state   :: GameState
  , _toLight :: Stream Button
  , _toPress :: Seq Button
  , _lit     :: Maybe Button
  , _timer   :: Int
  , _score   :: Int
  , _speed   :: Float
  }

data Stream a = a :| Stream a

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")

toList :: Stream a -> [a]
toList (x :| xs) = x : toList xs

data Button = R | G | Y | B
  deriving (Enum, Bounded, Eq, Show)

data GameState = Freezed | Running | Paused | Dead
  deriving (Eq, Show)

instance Random Button where
  random g = case randomR ( fromEnum (minBound :: Button)
                          , fromEnum (maxBound :: Button)) g of
               (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                      (r, g') -> (toEnum r, g')

makeLenses ''Game

maxPressesBehind :: Int
maxPressesBehind = 3

initialSpeed :: Float
initialSpeed = 100

lightOnDuration :: Int
lightOnDuration = 30

press :: Game -> Button -> Game
press g b = if (g ^. state) /= Running
              then g
              else case viewl (g ^. toPress) of
                     EmptyL -> gameOver g  -- TODO: might not want this?
                     (nextToPress :< restToPress) ->
                       if b == nextToPress
                         then g & toPress .~ restToPress
                                & score   %~ (+1)
                         else gameOver g
start :: Game -> Game
start g =
  if (g ^. state) /= Running
    then g & state .~ Running
           & timer .~ floor initialSpeed
           & score .~ 0
           & toPress .~ Seq.empty
           & lit   .~ Nothing
           & speed .~ initialSpeed
    else g

gameOver g = g & state .~ Dead

step :: Game -> Game
step g =
  let (t, g') = g & timer <%~ (subtract 1)
  in if t >= 0
      then g'
      else
       if length (g ^. toPress) > maxPressesBehind
         then gameOver g
         else g' & lit     .~ (Just nextToLight)
                 & toPress %~ (|> nextToLight)
                 & speed   %~ (*speedUpMultiplier)
                 & toLight .~ (restToLight)
                 & timer   .~ floor (g ^. speed) -- TODO: This probably also needs to start getting quicker
  where
    (nextToLight :| restToLight) = g ^. toLight

initGame :: IO Game
initGame = do
  buttons <- fromList . removeRepeats . randoms <$> newStdGen
  return $ Game Freezed
                buttons
                Seq.empty
                Nothing
                0
                0
                initialSpeed
  where
    removeRepeats :: Eq a => [a] -> [a]
    removeRepeats ls = go ls
      where
        go (l:ls) = l : go (dropWhile (==l) ls)
