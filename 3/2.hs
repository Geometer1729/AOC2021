{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Ord
import Data.Maybe
import Data.Function
import Data.Functor

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S


readLine :: String -> [Int]
readLine = (<&> read . return)

popular :: [[Int]] -> [Int]
popular grid =  let
  (keep:_) = sortOn (Down . length) . reverse . groupBy ((==) `on` head) $ grid 
    in case head keep of
         [] -> []
         (x:_) -> (x:) $ popular (tail <$> keep)

cringe :: [[Int]] -> [Int] 
cringe grid = let
  (keep:_) = sortOn length . groupBy ((==) `on` head) $ grid
    in case head keep of
         [] -> []
         (x:_) -> (x:) $ cringe (tail <$> keep)

bits2Int :: [Int] -> Int
bits2Int = foldl (\a b -> 2*a+b) 0

main :: IO ()
main = do
    txt <- readFile "input"
    let bits = sort $ readLine <$> lines txt :: [[Int]]
    print $ bits2Int (popular bits) * bits2Int (cringe bits)

