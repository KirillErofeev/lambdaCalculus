module Tests where

import Types
import Combinators
import Reduction

testCiff = full'' $ apps [iff, true, false, true]
