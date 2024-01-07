module Main where
import GrammarTree
import {-# SOURCE #-} Parse

evalAndShowRes :: IdMap -> ExprResult -> IO IdMap
