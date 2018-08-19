#!/usr/bin/env stack
{- stack
     --resolver lts-12.11
     --install-ghc
     runghc
     --package :imf-test
-}
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
