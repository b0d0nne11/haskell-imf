# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]
### Changed
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
