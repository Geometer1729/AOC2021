{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Arrow
import Control.Lens hiding ((.>))
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Data.Char
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

type Line = ([[Int]],[[Int]])
type Perm = [Int]

(!?) :: [a] -> Int -> Maybe a
xs !? n = listToMaybe $ drop n xs

cti :: Char -> Int
cti c = ord c - 97

refine :: Perm -> [Perm]
refine perm = do
  x <- [0..6]
  return $ perm ++ [x]


nums :: [[Int]]
nums = map cti <$> ["abcefg","cf","acdeg","acdfg","bcdf","abdfg","abdefg","acf","abcdefg","abcdfg"]

validate :: Perm -> [Int] -> Bool
validate perm word = any (validateFor perm word) nums

validateFor :: Perm -> [Int] -> [Int] -> Bool
validateFor perm word target = length word == length target && and ( do
  i <- [0..6]
  return $ case perm !? i of
    Just x -> ( i `elem` word && x `elem` target ) || (i `notElem` word && x `notElem` target)
    Nothing -> True
                                                               )

findPerm :: [[Int]] -> Perm
findPerm ws = head $ findPerm' ws []

findPerm' :: [[Int]] -> Perm -> [Perm]
findPerm' ws perm = if length perm == 7
                          then return perm
                          else do
                            perm' <- refine perm
                            guard $ all (validate perm') ws 
                            findPerm' ws perm'


lookupPerm :: Perm -> [Int] -> Int
lookupPerm perm word = let
  word' = sort [ x | i <- word , let Just x = perm !? i ]
    in case [ i | i <- [0..9] , nums !! i == word' ] of
         [x] -> x
         _ -> error "word not found"


solveLine :: Line -> [Int]
solveLine (ws,outws) = let
  perm = findPerm ws
    in map (lookupPerm perm) outws

parseLine :: String -> Line
parseLine w = let
  [ws',ows'] = splitOn "|" w
  ws = map cti <$> words ws'
  ows = map cti <$> words ows'
    in (ws,ows)

runLine :: String -> [Int]
runLine = parseLine .> solveLine

asBase10 :: [Int] -> Int
asBase10 = foldl (\a b -> 10*a+b) 0

main :: IO ()
main = do
    txt <- readFile "input"
    let sols = runLine <$> lines txt
    print $ sum (sols <&> asBase10)
