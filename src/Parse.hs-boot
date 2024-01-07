module Parse where
import GrammarTree
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Lex

evalMatchFn :: IdMap -> [Token] -> MatchFn a -> Maybe (a, [Token])

type MatchFn a = StateT [Token] (MaybeT (Reader IdMap)) a

parseExpr :: MatchFn ExprResult
parseDfnExpr :: MatchFn DfnExprResult

data ExprResult = ResAtn ArrTreeNode Bool -- Bool = should print?
                | ResFtn FnTreeNode Bool
                | ResOtn OpTreeNode Bool
                | ResNull

data DfnExprResult = DResAtn ArrTreeNode Bool -- Bool = should return?
                   | DResCond ArrTreeNode ArrTreeNode
                   | DResDefaultAlpha ArrTreeNode
                   | DResFtn FnTreeNode -- no bool: can't return function (should be an assignment)
                   | DResOtn OpTreeNode -- nor operator                   (should be an assignment)
                   | DResNull
