module IdMap where
import GrammarTree
import Lex
import qualified Data.Map as Map

data Dfn = Dfn [Token]

data Dop = Dop [Token]

data IdEntry = IdArr Array
             | IdFn Function
             | IdOp Operator
             | IdDfn Dfn
             | IdDop Dop

type IdMap = Map.Map String IdEntry

emptyIdMap :: IdMap
emptyIdMap = Map.empty

mapLookup :: String -> IdMap -> Maybe IdEntry
mapLookup = Map.lookup
