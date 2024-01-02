module QuadNames where
import GrammarTree
import Control.Monad.State.Lazy
import Exceptions
import Control.Exception (throw)

{- Data Structures -}

data QuadName = QuadName {
                           qget :: StateT IdMap IO Array
                         , qset :: Array -> StateT IdMap IO Array
                         }

{- Get/Set Functions -}

getQIo :: StateT IdMap IO Array
getQIo = get >>= \idm -> case mapLookup "⎕IO" idm of
    Nothing -> undefined -- no value for ⎕IO (shouldn't happen)
    (Just (IdArr a)) -> return a

setQIo :: Array -> StateT IdMap IO Array
setQIo a
    | shape a  /= [1] = throw $ DomainError "(⎕IO←): expected numeric singleton"
    | otherwise = case a `at` 0 of
          ScalarNum n
              | n /= (fromIntegral . floor $ n) -> throw $ DomainError "(⎕IO←): expected integral"
              | otherwise -> do
                    idm <- get
                    put $ mapInsert "⎕IO" (IdArr a) idm
                    return a
          _ -> throw $ DomainError "(⎕IO←): expected number"

{- QuadName's -}

qIo = QuadName getQIo setQIo

{- Utils -}

quadNameList = [
        ("IO", qIo)
    ]

getQuadName :: String -> QuadName
getQuadName id = case lookup id quadNameList of
    Nothing -> throw . SyntaxError $ "no such quad-name: `" ++ id ++ "`"
    (Just qn) -> qn
