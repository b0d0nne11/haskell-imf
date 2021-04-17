# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]
### Added
- Add `mailbin` tool to simplfy smtp client testing
### Changed
- Moved to the PolyForm Noncommercial License 1.0.0
- `connectMX` now randomly shuffles MX records at the same priority level
- Bump the minimum base version to 4.13 (ghc version 8.8)

## [0.7.0] - 2020-07-19
### Added
- Added PIPELINING, STARTTLS, and AUTH to server
- Add type classes for objects that have a parser
- Add helper for generating new message IDs
- Add helper for building new messages
### Changed
- Export addrSpec and domain parsers
- Merge the Client and ClientPool modules
- Rename the DateTime modules Time
- Reorganize project to split it up into seperate pacakges
- Rewrite tests using tasty to work better with cabal
- Refactor the client and server modules to simplify the interface

## [0.6.0] - 2019-12-02
### Added
- Add source IP option to SMTP client
- Add stack.yaml.lock to change control
- Add a client pool module
- Add a server module
### Changed
- Seperate init, delivery, and term portions of SMTP chat
- Moved all remaining parsers to attoparsec
- Reorganize project, move top level package to Data and add Types module
- Move client DNS functions to their own module
- Changed to the BSD3 license to be more consistant with community packages
- Cleanup the README
- Updated LTS snapshot to latest, lts-14.12
- Move general chat functions to their own module

## [0.5.0] - 2019-02-10
### Added
- Add support to SMTP client recipient host override
- Add support to SMTP client for AUTH PLAIN/LOGIN
- Add support to SMTP client for STARTTLS
- Add extention list to SMTP client state
- Add support to SMTP client for ELHO with fallback to HELO
- Add support to SMTP client for multiline replies
### Changed
- Move SMTP chat timestamps into the client log
- Stopped modifying line endings in SMTP chat log entries
- Fixed pedantic errors

## [0.4.0]
### Added
- A network module with sending functionality.
- Helper functions to get the addr-spec and angle address format of mailboxes.

## [0.3.0]
### Added
- A foreign function interface and python module.
### Changed
- Reorganized the code to seperate the haskell and python modules.
- Prefixed all parsing functions with a 'p'.
- Rewrote the DateTime parser so that it returns a DateTime directly rather
  than parsing into an intermediate string first.
- Cleaned up all the test code. Moved message fixtures to seperate files.
- Header field parsers should no longer fall back to Optional when a different
  header type has an incorrect value (ex. Optional ("Date", "Foo")).
- Bumped stack's resolver to lts-12.11.
- Changed the Mailbox spec so that the display name is no longer optional. Use
  an empty string for no display name.
- Use a concrete Parser monad.
- Probably other things as well. This is another major rewrite.
### Removed
- The main module. It didn't do much and wasn't a needed anymore.
- The collect combinator. It's the same as fold.

## [0.2.0]
### Added
- New parsers and types for date time, header, message, and message ID.
- Parsing and formatting functions have been generalized using the new
  ParseMessage and FormatMessage type classes.
### Changed
- Most parsers have been refactored using a more applicative style.
- Pretty much everything else. This is a major rewrite.

## [0.1.0]
### Added
- Everything, initial release.
