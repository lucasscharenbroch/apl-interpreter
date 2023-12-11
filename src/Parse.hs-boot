module Parse where
import GrammarTree
import Lex

type MatchFn a = (IdMap, [Token]) -> Maybe (a, [Token])

parseDfnExpr :: MatchFn DfnExprResult

data DfnExprResult = DResAtn ArrTreeNode Bool -- Bool = should return?
                   | DResCond ArrTreeNode ArrTreeNode
                   | DResFtn FnTreeNode -- no bool: can't return function (should be an assignment)
                   | DResOtn OpTreeNode -- nor operator                   (should be an assignment)
                   | DResNull
