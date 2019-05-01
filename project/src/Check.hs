module Check where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast

-- here you can preform static checks


-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors
data WarningMsg = 
    UndefinedVarUse String  -- ^ This is the Warning for use of Undefined variable name
  -- ...
  deriving (Show,Eq)

-- | perform static checking on the Ast
-- the output a set of warning on that input Ast
check :: Ast -> Set WarningMsg
check = undefined
