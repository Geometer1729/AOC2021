{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-unused-language-pragma #-}

--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Maybe
import Data.Function
import Data.Functor

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S


readLine :: String -> [Int]
readLine = (<&> read . return)




main :: IO ()
main = do
    txt <- readFile "input"
    let bits = readLine <$> lines txt :: [[Int]]
    let sums = map sum . transpose $ bits
    let len = length bits
    let gammaBits = [ if 2*n > len then 1 else 0 | n <- sums ]
    let gamma = foldl (\a b -> 2*a+b) 0 gammaBits
    let eps = 2^length gammaBits - 1 - gamma
    print gammaBits
    print gamma
    print eps
    print $ gamma * eps
