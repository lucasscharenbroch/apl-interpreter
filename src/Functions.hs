module Functions where
import Eval
import GrammarTree

implicitCat :: ArrTreeNode -> ArrTreeNode -> Array
implicitCat x' y' = arrCat x y
    where x = case x' of --
              (ArrInternalMonFn (FnLeafFn fImplicitGroup) _) -> arrFromList [maybeEnclose $ evalArrTree x']
              otherwise -> evalArrTree x'
          y = case y' of
              (ArrInternalMonFn (FnLeafFn fImplicitGroup) _) -> arrFromList [maybeEnclose $ evalArrTree y']
              otherwise -> evalArrTree y'
          maybeEnclose arr = case arrToList arr of
                             (s:[]) -> s
                             _ -> ScalarArr arr

implicitGroup :: ArrTreeNode -> Array
implicitGroup = evalArrTree
