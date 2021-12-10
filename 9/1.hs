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

import Data.Array

type HM = Array (Int,Int) Int

parse :: String -> HM
parse w = let
  css = lines w
  xss = map (map (read.return)) css
  x = length (head xss)
  y = length xss
    in listArray ((0,0),(y-1,x-1)) (concat xss)

vallys :: HM -> Array (Int,Int) Bool
vallys xss = let
  r = bounds xss
    in listArray r
      [  and [ not (r `inRange` n) || (xss!n > v) | n <- [(x-1,y),(x+1,y),(x,y-1),(x,y+1)] ]
      |  p@(x,y) <- range r , let v = xss!p ]

pairs :: Array (Int,Int) Bool -> [(Int,Int)]
pairs xss = fst <$> filter snd (assocs xss)

solve :: String -> Int
solve w = let
  hm = parse w
  vals = pairs (vallys hm)
  danger = [ hm!p + 1 | p <- vals ]
    in sum danger

main :: IO ()
main = do
    txt <- readFile "input"
    print $ solve txt
