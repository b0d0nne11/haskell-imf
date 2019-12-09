import Test.Tasty

import qualified Test.Core
import qualified Test.Header
import qualified Test.Mailbox
import qualified Test.MessageId
import qualified Test.Message
import qualified Test.Time

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Test.Core.tests
    , Test.Header.tests
    , Test.Mailbox.tests
    , Test.MessageId.tests
    , Test.Message.tests
    , Test.Time.tests
    ]
