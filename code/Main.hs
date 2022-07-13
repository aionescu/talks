module Main where

import JSON(parseJSON)

test :: String -> IO ()
test s =
  case parseJSON s of
    Nothing -> putStrLn $ "ERROR: " <> show s
    Just json -> print json

main :: IO ()
main = do
  test "null"
  test "true"
  test "false"
  test "2"
  test "3"
  test "\"abc\""
  test "\"Abc Def 123\""
  test "[2, 3, false,]"
  test "{ \"x\": 2, \"y\": 3 }"
  test "{ \"x\": 2, /* Comment */\n\"y\": 3 , } // Comment"
  test "{ \"x\": 2, \"x\": 3 }"
  test "[[[[[[[[[{/**/}]]]]]]]]]"
  test "{\"a b c\":[{/**/},2\n,\n[{\"[]\":null}]\n,\n]}\n//"
