import Test.Tasty

import qualified Test.Client
import qualified Test.Server

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Test.Client.tests
    , Test.Server.tests
    ]
