module JSON(parseJSON) where

import Control.Applicative(Applicative(..), Alternative(..))
import Data.Containers.ListUtils(nubOrd)
import Data.Functor(($>), void)
import Data.List(intercalate)
import Data.Map(Map, fromList, toList)
import Parser

data JSON
  = JNull
  | JBool Bool
  | JNumber Integer
  | JString String
  | JArray [JSON]
  | JObject (Map String JSON)

instance Show JSON where
  show JNull = "null"
  show (JBool True) = "true"
  show (JBool False) = "false"
  show (JNumber n) = show n
  show (JString s) = show s
  show (JArray js) = "[" <> intercalate ", " (show <$> js) <> "]"
  show (JObject fs) = "{" <> intercalate ", " (showField <$> toList fs) <> "}"
    where
      showField (f, j) = show f <> ": " <> show j

singleLine, multiLine, comment :: Parser ()
singleLine = string "//" *> skipManyTill (endOfLine <|> eof)
multiLine = string "/*" *> skipManyTill (string "*/")
comment = singleLine <|> multiLine

ws :: Parser ()
ws = void $ spaces `sepEndBy` comment

str :: String -> Parser String
str s = string s <* ws

jNull :: Parser JSON
jNull = string "null" <* ws $> JNull

jBool :: Parser JSON
jBool = JBool <$> (string "true" $> True <|> string "false" $> False) <* ws

jNumber :: Parser JSON
jNumber = JNumber <$> (sign <*> number) <* ws
  where
    sign = char '-' $> negate <|> pure id
    number = read <$> some digit

quotedStr :: Parser String
quotedStr = between (char '"') (char '"') (many $ satisfy (/= '"')) <* ws

jString :: Parser JSON
jString = JString <$> quotedStr

jArray :: Parser JSON
jArray = between (str "[") (str "]") $ JArray <$> json `sepEndBy` str ","

fields :: Parser [(String, JSON)]
fields =
  between (str "{") (str "}")
  $ liftA2 (,) (quotedStr <* str ":") json `sepEndBy` str ","

checkUnique :: [(String, JSON)] -> Parser JSON
checkUnique fields =
  let names = fst <$> fields
  in
    if length names == length (nubOrd names)
    then pure $ JObject $ fromList fields
    else empty

jObject :: Parser JSON
jObject = fields >>= checkUnique

json :: Parser JSON
json = jNull <|> jBool <|> jNumber <|> jString <|> jArray <|> jObject

jsonDoc :: Parser JSON
jsonDoc = ws *> json <* eof

parseJSON :: String -> Maybe JSON
parseJSON s = fst <$> runParser jsonDoc s
