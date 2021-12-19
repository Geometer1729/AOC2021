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

import Data.Monoid
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import Flow

import Data.Array

type PT = (Int,Int)

parse :: String -> Array PT Int
parse w = let
  xss = parse' w
    in listArray ((0,0),(length xss -1,length (head xss) -1)) (concat xss)

parse' :: String -> [[Int]]
parse' = lines .> map (map (return .> read))

step :: Array PT Int -> Array PT Int
step = fmap (+1) .> flashes

neighbs :: PT -> Array PT a -> [a]
neighbs pt xss = [ xss!p | p <- neighbs' pt , inRange (bounds xss) p ]

neighbs' :: PT -> [PT]
neighbs' (x,y) = [(x+dx,y+dy) | dx <- [-1..1] , dy <- [-1..1] , (dx,dy) /= (0,0) ]

flashes :: Array PT Int -> Array PT Int
flashes xss = let
  xss' = flashStep xss
  in if xss /= xss'
     then flashes xss'
     else xss' <&> (\case
      11 -> 0
      10 -> error "10"
      x 
        | x < 10 -> x
      x -> error $ show x
                   )

flashStep :: Array PT Int -> Array PT Int
flashStep xss = listArray (bounds xss) $
  [ if xss!p >= 10
       then 11
       else min 10 $ xss!p + length (filter (==10) (neighbs p xss))
  | p <- range (bounds xss) ]


run :: Array PT Int -> Int
run = run' 0

run' :: Int -> Array PT Int -> Int
run' n xss
  | all (== 0) xss = n
  | otherwise = run' (n+1 )(step xss)

main :: IO ()
main = do
  forFile "smal"
  forFile "input"

forFile :: String -> IO ()
forFile w = do
    txt <- readFile w
    let xss = parse txt
    print $ run xss
