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


main :: IO ()
main = do
    txt <- readFile "input"
    print txt
