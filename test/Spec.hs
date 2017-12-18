#!/usr/bin/env stack
{- stack
     --resolver lts-9.18
     --install-ghc
     runghc
     --package :imf-test
-}
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
