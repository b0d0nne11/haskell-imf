# imf-parsers

This package provides utilities for parsing email messages.

## Quickstart

### Parsing

Using `parse`:
```
$> cabal v2-repl imf-parsers
...
*Data.IMF.Parsers> :set -XOverloadedStrings
*Data.IMF.Parsers> import Data.IMF
*Data.IMF.Parsers Data.IMF> parse "John Smith <jsmith@example.com>" :: Either String Mailbox
Right (Mailbox {mboxDisplay = "John Smith", mboxLocal = "jsmith", mboxDomain = "example.com"})
```
