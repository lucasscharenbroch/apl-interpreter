module Functions where
import GrammarTree
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

class (Monad m) => SubEvalM m where
    toEvalM :: m a -> StateT IdMap IO a

newtype IdxOriginM a = IdxOriginM { unIdxOriginM :: Reader Int a }
instance SubEvalM IdxOriginM

iota :: Array -> IdxOriginM Array
shapeOf :: Array -> Array
