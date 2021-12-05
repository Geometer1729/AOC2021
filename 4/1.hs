{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Arrow
import Control.Lens hiding ((.>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Either
import Data.Function
import Data.Functor

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import Flow

pars :: String -> [String]
pars = splitOn "\n\n"

parseInput :: String -> ([Int],[[[Int]]])
parseInput w = let
  (vals':boards') = pars w
  vals = read <$> splitOn "," vals'
  boards = map (map read . words) . lines <$> boards'
    in (vals,boards)

type Board = [[Maybe Int]]

callBoard :: Int -> Board -> Board
callBoard n = map . map $ \case
  Just c 
    | c /= n -> Just c
  _ -> Nothing

isSolved :: Board -> Bool
isSolved board = any (null . catMaybes) $ board ++ transpose board

stepBoard :: Int -> Board -> Either Int Board
stepBoard n board = let
  board' = callBoard n board
                     in if isSolved board'
                       then Left $ n * (sum . catMaybes . concat $ board')
                       else Right board'

stepBoards :: Int -> [Board] -> Either Int [Board]
stepBoards = mapM . stepBoard 

runBoards :: [Int] -> [Board] -> Either Int [Board]
runBoards xs boards = foldM (flip stepBoards) boards xs

runInput :: String -> Int
runInput = parseInput .> second (map.map.map $ Just) .> uncurry runBoards .> fromLeft (-1)

main :: IO ()
main = do
    txt <- readFile "input"
    print $ runInput txt
