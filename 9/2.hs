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
import Data.Ord 

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

import Flow

import Data.Array

type HM = Array PT Int
type PT = (Int,Int)

parse :: String -> HM
parse w = let
  css = lines w
  xss = map (map (read.return)) css
  x = length (head xss)
  y = length xss
    in listArray ((0,0),(y-1,x-1)) (concat xss)

vallys :: HM -> Array PT Bool
vallys xss = let
  r = bounds xss
    in listArray r
      [  and [ not (r `inRange` n) || (xss!n > v) | n <- neighbs p ]
      |  p <- range r , let v = xss!p ]

neighbs :: PT -> [PT]
neighbs (x,y) =[(x-1,y),(x+1,y),(x,y-1),(x,y+1)] 

pairs :: Array PT Bool -> [PT]
pairs xss = fst <$> filter snd (assocs xss)

solve :: String -> Int
solve w = let
  hm = parse w
  vals = pairs (vallys hm)
  basins' = basin hm  <$> vals
  basins = sort .> group .> map head $ basins'
  ls = length <$> basins
    in product $ take 3 $ sortOn Down ls

basin :: HM -> PT -> Set PT
basin xss p = basin' xss (S.singleton p)

basin' :: HM -> Set PT -> Set PT
basin' xss ps = let
  cands = S.union ps $ S.fromList $ concat [neighbs p | p <- S.toList ps ]
  next = S.filter (validate xss ps) cands
                 in if length next > length ps
                       then basin' xss next
                       else next

validate :: HM -> Set PT -> PT -> Bool
validate xss ps p = inRange (bounds xss) p && (xss!p /= 9) && and [ not (inRange (bounds xss) n) || xss!n >= xss!p || n `elem` ps  | n <- neighbs p ]


main :: IO ()
main = do
    txt <- readFile "input"
    print $ solve txt
