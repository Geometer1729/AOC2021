{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}

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

main :: IO ()
main = do
    txt <- readFile "input"
    let deps' = read <$> lines txt :: [Int]
    let deps  = zipWith3 (\a b c -> a + b + c) deps' (tail deps') (tail $ tail deps')
    let count = length $ filter (uncurry (<)) (zip deps (tail deps))
    print count

