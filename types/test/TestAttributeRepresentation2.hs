module Main(main) where

import Int

import AttributeRepresentation

testValue :: HasCodedValue value => value -> IO ()
testValue = error "foo"

main = testValue (0::Int)

