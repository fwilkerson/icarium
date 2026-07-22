{- | The CLI integration suite, split by subject. Every group drives
@./bin/icarium@ as a subprocess — run @make install@ first, or the specs
test the previous build.
-}
module CliSpec (tests) where

import Test.Tasty (TestTree, testGroup)

import CliCtxSpec qualified
import CliDispatchSpec qualified
import CliEventLogSpec qualified
import CliSearchSpec qualified
import CliSurfaceSpec qualified
import CliTaskSpec qualified
import CliWorktreeSpec qualified

tests :: TestTree
tests =
    testGroup
        "CLI integration"
        [ CliSurfaceSpec.tests
        , CliTaskSpec.tests
        , CliCtxSpec.tests
        , CliSearchSpec.tests
        , CliDispatchSpec.tests
        , CliWorktreeSpec.tests
        , CliEventLogSpec.tests
        ]
