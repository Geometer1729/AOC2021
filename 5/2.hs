{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

--{-# LANGUAGE LambdaCase #-}
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

parseLine :: String -> [Int]
parseLine = splitOn "," >=> splitOn " -> " .> map read

lineToMap :: String -> Map (Int,Int) Int
lineToMap w = case parseLine w of
  [x1,y1,x2,y2] 
    | x1 == x2 -> M.fromList [ ((x1,y),1) | y <- [min y1 y2..max y1 y2]]
    | y1 == y2 -> M.fromList [ ((x,y1),1) | x <- [min x1 x2..max x1 x2]]
    | x2-x1 == y2-y1 -> let
        xl = min x1 x2
        yl = min y1 y2
        l  = abs $ y2 - y1
          in M.fromList [ ((xl+i,yl+i),1) | i <- [0..l] ]
    | x2-x1 == y1-y2 -> let
        xl = min x1 x2
        yl = max y1 y2
        l  = abs $ y2 - y1
          in M.fromList [ ((xl+i,yl-i),1) | i <- [0..l] ]
  _ -> error $ "badLine " ++ w

solve :: String -> Int
solve = lines .> map lineToMap .> foldl (M.unionWith (+)) M.empty 
        .> M.filter (>= 2) .> M.elems .> length


main :: IO ()
main = do
    txt <- readFile "input"
    print $ solve txt
