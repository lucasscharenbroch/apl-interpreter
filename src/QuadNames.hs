module QuadNames where
import GrammarTree
import Control.Monad.State.Lazy

{- Data Structures -}

data QuadName = QuadName {
                           qget :: StateT IdMap IO Array
                         , qset :: Array -> StateT IdMap IO Array
                         }

{- Get/Set Functions -}

getQIo :: StateT IdMap IO Array
getQIo = get >>= \idm -> case mapLookup "⎕IO" idm of
    Nothing -> undefined
    (Just (IdArr a)) -> return a

setQIo :: Array -> StateT IdMap IO Array
setQIo a
    | shape a  /= [1] = undefined -- TODO domain error
    | otherwise = case a `at` 0 of
          ScalarNum n
              | n /= (fromIntegral . floor $ n) -> undefined -- TODO domain error
              | otherwise -> do
                    idm <- get
                    put $ mapInsert "⎕IO" (IdArr a) idm
                    return a
          _ -> undefined -- TODO domain error

{- QuadName's -}

qIo = QuadName getQIo setQIo

{- Utils -}

quadNameList = [
        ("IO", qIo)
    ]

getQuadName :: String -> QuadName
getQuadName id = case lookup id quadNameList of
    Nothing -> undefined -- TODO undefined quad name
    (Just qn) -> qn
