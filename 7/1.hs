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

vals :: String -> [Int]
vals = splitOn "," .> map read

---avg :: [Int] -> Int
---avg xs = sum xs `div` length xs

dist :: [Int] -> Int -> Int
dist xs targ = sum [ abs (x-targ) | x <- xs ]

solve :: String -> Int
solve w = let
  xs = vals w
  a = minimum xs
  b = maximum xs
    in minimum [ dist xs x | x <- [a..b] ]

main :: IO ()
main = do
    txt <- readFile "input"
    print $ solve txt
