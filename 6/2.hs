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

parse :: String -> [(Int,Int)]
parse w = let
  ints = read <$> splitOn "," w
    in [(i,length $ filter (==i) ints) | i <- [0..8] ]

clean :: [(Int,Int)] -> [(Int,Int)]
clean = sort .> groupBy ((==) `on` fst)  .> map (head .> fst &&& map snd .> sum)

step :: [(Int,Int)] -> [(Int,Int)]
step xs = clean $ do
  (timer,num) <- xs
  if timer > 0
     then return (timer-1,num)
     else [(6,num),(8,num)]

main :: IO ()
main = do
    txt <- readFile "input"
    print $ sum . map snd $ iterate step (parse txt) !! 256
