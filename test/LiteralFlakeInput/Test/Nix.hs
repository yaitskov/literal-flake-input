{-# LANGUAGE MultilineStrings #-}
module LiteralFlakeInput.Test.Nix where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit
import LiteralFlakeInput.Prelude
import LiteralFlakeInput.Nix
import Nix

test_e :: TestTree
test_e =
  testGroup "e"
  [ testGroup "isUnquotedString"
    [ go "x" True
    , go "a+b" True
    , go "a / b" True
    , go "(a+b)" False
    , go "[a]" False
    , go "{a=1;}" False
    , go "a: a" False
    , go "true" False
    , go "false" False
    , go "null" False
    , go "\"x\"" False
    ]
  , testGroup "inputsFirstBindingPos"
    [ inputsCol "{inputs={x=1;};}" 9
    , inputsLine "{inputs={x=1;};}" 0
    , inputsCol """{
                    inputs = {
                      x=1;
                    };
                  }""" 4
    , inputsLine """{
                    inputs = {
                      x=1;
                    };
                  }""" 2
    ]
  ]
  where
    go s e = testCase (toString s) $ isUnquotedString s @?= e
    inputsCol = inputsgo snd
    inputsLine = inputsgo fst
    inputsgo dim s e =
      testCase (show s <> " " <> show e)
      (((fmap dim . inputsFirstBindingPos) <=< rightToMaybe)
        (parseNixTextLoc s)
        @?= pure e)

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
    , testCase "esc double quote"
      $ renderEntry "a" "hello \" world" @?= "a=\"hello \\\" world\";"
    , testCase "esc left slash"
      $ renderEntry "a" "hello \\" @?= "a=\"hello \\\\\";"
    , testCase "quoted"
      $ renderEntry "a" "\"hello\"" @?= "a=\"hello\";"
    , testCase "list"
      $ renderEntry "a" "[1 2]" @?= "a=[1 2];"
    , testCase "attr-set"
      $ renderEntry "a" "{x = 1; y = 2}" @?= "a={x = 1; y = 2};"
    , testCase "lambda"
      $ renderEntry "a" "x: x + 1" @?= "a=x: x + 1;"
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
