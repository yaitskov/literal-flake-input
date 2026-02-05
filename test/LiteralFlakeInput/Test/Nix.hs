{-# LANGUAGE MultilineStrings #-}
module LiteralFlakeInput.Test.Nix where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit
import LiteralFlakeInput.Prelude
import LiteralFlakeInput.Nix

test_render :: TestTree
test_render =
  testGroup "render"
  [ testGroup "renderEntry"
    [ testCase "nat"
      $ renderEntry "a" "1" @?= "a=1;"
    , testCase "bool"
      $ renderEntry "a" "true" @?= "a=true;"
    , testCase "false"
      $ renderEntry "a" "false" @?= "a=false;"
    , testCase "null"
      $ renderEntry "a" "null" @?= "a=null;"
    , testCase "string"
      $ renderEntry "a" "hello world" @?= "a=\"hello world\";"
    , testCase "empty"
      $ renderEntry "a" "" @?= "a=\"\";"
    ]
  , testGroup "renderNixDer"
    [ testCase "empty"
      $ renderNixDer (NixDer []) @?= """{...}:{}"""
    , testCase "one"
      $ renderNixDer (NixDer [("key", "value")]) @?= """{...}:{key="value";}"""
    , testCase "two"
      $ renderNixDer (NixDer [("key", "value"), ("key2", "value2")])
      @?= """{...}:{key="value";key2="value2";}"""
    ]
  ]
