---
marp: true
paginate: true
theme: uncover
class: invert
style: |
  @import url('https://fonts.googleapis.com/css2?family=Fira+Code&family=Fira+Sans:ital@0;1&display=swap');
  section { font-family: "Fira Sans", sans-serif; }
  code { font-family: "Fira Code", monospace; }
---

![bg left:50% 80%](assets/Haskell.svg)

## **Parser Combinators from Scratch**

<br/>
<br/>

Alex Ionescu
[![](assets/GitHub.png) aionescu](https://github.com/aionescu)

---

## **Parsing 101**

Parsing ≈ Deriving structured data out of text

* Traditionally done using bespoke tools (e.g. `lex`, `yacc`)
* Alternative: *Parser Combinators*
  - More flexible, easier to learn
  - Library, not separate language

---

### **Parser Combinators**


![bg right:40% 50%](assets/Lego.png)

* *Core idea*: Create complex parsers by combining simple ones.
* Functional in nature
  - Composition-based
  - Immutable
  * Haskell is a great choice!

---

### **How should we represent parsers?**

* Example: `int` parser
  ```csharp
  // C#
  int Int32.Parse(string s)
  // C/C++
  int atoi(const char* s)
  ```
* Obvious solution: Functions!
  ```haskell
  type Parser a = String -> a
  ```

---

## **What About Failure?**

* Exceptions?
  ```csharp
  int.Parse("123") // ✅ 123
  int.Parse("abc") // 💥 FormatException
  ```
* Exceptions "lie" about the function's type
* Solution: Algebraic Data Types
  ```haskell
  data Maybe a = Nothing | Just a

  type Parser a = String -> Maybe a
  ```

---

## **How greedy should parsers be?**

If we're trying to parse an `int` out of `"123abc"`, what's the result?

* Failure: The *entirety of the input* does not represent an `int`.
* Success: Parse as much as you can, and return the "leftover" alongside the result.
* Less greedy parsers are more easily composed.

---

## **Final Representation**

```haskell
type Parser a = String -> Maybe (a, String)
```

* Small problem, it's a *type synonym*

---

## ***Final* Final Representation**

```haskell
newtype Parser a =
  Parser { runParser :: String -> Maybe (a, String) }
```

* Easier to define `instance`s for it
* `newtype` means zero overhead

---

# **"Primitive" Parsers**

---

## **Always succeed (`pure`)**

```haskell
pure :: a -> Parser a
pure a = Parser $ \s -> Just (a, s)
```

```haskell
runParser (pure 2) "abc" ≡ Just (2, "abc")
runParser (pure 2) ""    ≡ Just (2, "")
```

---

## **Always fail (`empty`)**

```haskell
empty :: Parser a
empty = Parser $ \s -> Nothing
```

```haskell
runParser empty "abc" ≡ Nothing
runParser empty ""    ≡ Nothing
```

---

## **`eof`**

```haskell
eof :: Parser ()
eof = Parser $ \s ->
  case s of
    "" -> Just ((), "")
    _ -> Nothing
```

```haskell
runParser eof ""    ≡ Just ((), "")
runparser eof "abc" ≡ Nothing
```

---

## **`anyChar`**

```haskell
anyChar :: Parser Char
anyChar = Parser $ \s ->
  case s of
    c : rest -> Just (c, rest)
    [] -> Nothing
```

```haskell
runParser anyChar ""    ≡ Nothing
runParser anyChar "abc" ≡ Just ('a', "bc")
runparesr anyChar "2"   ≡ Just ('2', "")
```

---

## **Let's not reinvent the wheel**

```haskell
-- `String`s are just lists of `Char`s
type String = [Char]

-- `uncons` splits a list into its head and tail
uncons :: [a] -> Maybe (a, [a])
```

```haskell
import Data.List

anyChar :: Parser Char
anyChar = Parser uncons
```

---

## **`satisfy`**

```haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s ->
  case s of
    c : rest | p c -> Just (c, rest)
    _ -> Nothing
```

```haskell
runParser (satisfy isLetter) "abc" ≡ Just ('a', "bc")
runParser (satisfy isLetter) "123" ≡ Nothing
```

---

## **`satisfy`-based parsers**

```haskell
letter :: Parser Char
letter = satisfy isLetter

digit :: Parser Char
digit = satisfy isDigit

char :: Char -> Parser Char
char c = satisfy (== c)
```

```haskell
runParser (char 'a') "abc" ≡ Just ('a', "bc")
runParser (char 'b') "abc" ≡ Nothing
```

---

## **`string`**

```haskell
string :: String -> Parser String
string a = Parser $ \s ->
  if a `isPrefixOf` s
  then Just (a, drop (length a) s)
  else Nothing
```

```haskell
runParser (string "let") "letter" ≡ Just ("let", "ter")
```

---

## **Skipping Whitespace**

```haskell
spaces :: Parser ()
spaces = Parser $ \s -> Just ((), dropWhile isSpace s)
```

```haskell
runParser spaces "   abc" ≡ Just ((), "abc")
runParser spaces "abc"    ≡ Just ((), "abc")
```

---

# **Combinators**

---

## **Transform the result of a parser**

```haskell
fmap :: (a -> b) -> Parser a -> Parser b
fmap f (Parser p) = Parser $ \s ->
  case p s of
    Nothing -> Nothing
    Just (a, s) -> Just (f a, s)
```

```haskell
runParser (fmap toUpper letter) "abc"  ≡ Just ('A', "bc")
runParser (fmap toUpper letter) "1abc" ≡ Nothing
```

---

## **The `Functor` typeclass**

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

```haskell
{-# LANGUAGE InstanceSigs #-}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s ->
    case p s of
      Nothing -> Nothing
      Just (a, s) -> Just (f a, s)
```

---

## **`fmap` Shortcuts**

```haskell
-- Infix aliases for `fmap`
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<&>) :: Functor f => f a -> (a -> b) -> f b

-- Shorthands for fmap with constant function
(<$)  :: Functor f => b -> f a -> f b
($>)  :: Functor f => f a -> b -> f b
void  :: Functor f => f a -> f ()
```

```haskell
f <$> p ≡ fmap f p
p <&> f ≡ fmap f p
x <$ p  ≡ fmap (\_ -> x) p
p $> x  ≡ fmap (\_ -> x) p
void p  ≡ fmap (\_ -> ()) p
```

---

## **Running parsers *sequentially***

```haskell
liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
liftA2 f (Parser pa) (Parser pb) = Parser $ \s ->
  case pa s of
    Nothing -> Nothing
    Just (a, rest) ->
      case pb rest of
        Nothing -> Nothing
        Just (b, rest') -> Just (f a b, rest')
```

```haskell
runParser (liftA2 (,) letter letter) "abc" ≡ Just (('a', 'b'), "c")
runParser (liftA2 (,) letter letter) "a1c" ≡ Nothing
```

---

## **We can `do` better**

```haskell
liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
liftA2 f (Parser pa) (Parser pb) = Parser $ \s -> do
  (a, rest) <- pa s
  (b, rest') <- pb rest
  pure (f a b, rest')
```

```haskell
runParser (liftA2 (,) letter letter) "abc" ≡ Just (('a', 'b'), "c")
runParser (liftA2 (,) letter letter) "a1c" ≡ Nothing
```

---

## **The `Applicative` typeclass**

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
```

```haskell
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)

  liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  liftA2 f (Parser pa) (Parser pb) = Parser $ \s -> do
    (a, rest) <- pa s
    (b, rest') <- pb rest
    pure (f a b, rest')
```

---

## **`Applicative` Shortcuts**

```haskell
-- More useful version of `liftA2`. Also called `ap`.
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- Sequencing operators that ignore one of the results
(<*) :: Applicative f => f a -> f b -> f a
(*>) :: Applicative f => f a -> f b -> f b
```

```haskell
pf <*> pa ≡ liftA2 ($) pf pa

pa <* pb  ≡ liftA2 (\a b -> a) pa pb
pa *> pb  ≡ liftA2 (\a b -> b) pa pb
```

---

## **Running parsers *alternatively***

```haskell
(<|>) :: Parser a -> Parser a -> Parser a
Parser pa <|> Parser pb = Parser $ \s ->
  case pa s of
    Just (a, rest) -> Just (a, rest)
    Nothing -> pb s
```

```haskell
runParser (letter <|> digit) "abc" ≡ Just ('a', "bc")
runParser (letter <|> digit) "1bc" ≡ Just ('1', "bc")

runParser (fmap Left letter <|> fmap Right digit) "abc"
  ≡ Just (Left 'a', "bc")

runParser (fmap Left letter <|> fmap Right digit) "1bc"
  ≡ Just (Right '1', "bc")
```

---

## **The `Alternative` typeclass**

```haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```

```haskell
instance Applicative Parser where
  empty :: Parser a
  empty = Parser $ \s -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser pa <|> Parser pb = Parser $ \s ->
    case pa s of
      Just (a, rest) -> Just (a, rest)
      Nothing -> pb s
```

---

## **Piggybacking off of `Maybe`**

```haskell
(<|>) :: Alternative f => f a -> f a -> f a

instance Alternative Maybe where
  ...
```

```haskell
Parser pa <|> Parser pb = Parser $ \s -> pa s <|> pb s
--        ^^^                                 ^^^
--        on `Parser`s                        on `Maybe`s
```

---

## **💰 Free Parsers**

```haskell
-- "0 or more" (of whatever `f` is/does)
many :: Alternative f => f a -> f [a]

-- "1 or more" (of whatever `f` is/does)
some :: Alternative f => f a -> f [a]
```

```haskell
runParser (many (char 'a')) "aaab" ≡ Just (['a', 'a', 'a'], "b")
runParser (some (char 'a')) "aaab" ≡ Just (['a', 'a', 'a'], "b")

runParser (many (char 'a')) "b"    ≡ Just ([], "b")
runParser (some (char 'a')) "b"    ≡ Nothing

runParser (many spaces)     ""     ≡ ⟂ -- 👻 Infinite loop
```

---

## **`sepBy` & `sepEndBy`**

```haskell
sepBy :: Parser a -> Parser sep -> Parser [a]
sepEndBy :: Parser a -> Parser sep -> Parser [a]

sepBy a sep = liftA2 (:) a (many (sep *> a)) <|> pure []
sepEndBy = -- ...
```

```haskell
runParser (digit `sepBy` char '.')    "1.2.3"
  ≡ Just (['1', '2', '3'], "")

runParser (digit `sepEndBy` char '.') "1.2.3."
  ≡ Just (['1', '2', '3'], "")

runParser (digit `sepEndBy` letter) "1a2b3c"
  ≡ Just (['1', '2', '3'], "")
```

---

## **`skipManyTill`**

```haskell
skipManyTill :: Parser a -> Parser ()
skipManyTill p = Parser $ \s ->
  runParser (void p) s
  <|> case s of
    [] -> Nothing
    _ : rest -> runParser (skipManyTill p) rest
```

---

## **`between`**

```haskell
between :: Parser b -> Parser e -> Parser a -> Parser a
between b e a = b *> a <* e
```

```haskell
runParser (between (char '(') (char ')') letter) "(a)"
  ≡ Just ('a', "")

runParser (between digit digit letter) "1a2"
  ≡ Just ('a', "")
```

---

## **Creating dynamic parsers (`>>=`)**

```haskell
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
Parser p >>= f = Parser $ \s ->
  case p s of
    Nothing -> Nothing
    Just (a, rest) -> runParser (f a) rest
```

```haskell
nTimes n p = many p >>= \l ->
  if length l == n
  then pure l
  else empty
p = number >>= \n -> nTimes n letter

runParser p "2ab" ≡ Just ("ab", "")
runParser p "2a"  ≡ Nothing
```

---

## **The `Monad` typeclass**

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
```

```haskell
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= f = Parser $ \s ->
    case p s of
      Nothing -> Nothing
      Just (a, rest) -> runParser (f a) rest
```

---

# **Parsing JSON**

---

## **Data Representation**

```haskell
data JSON
  = JNull
  | JBool Bool
  | JNumber Integer -- We're cheating here
  | JString String
  | JArray [JSON]
  | JObject (Map String JSON)
```

---

## **Whitespace? Comments?**

* Consistent
* Modular
* Solution: Each subparser consumes *subsequent* whitespace & comments

---

## **Whitespace & Comments**

```haskell
singleLine, multiLine, comment :: Parser ()
singleLine = string "//" *> skipManyTill (endOfLine <|> eof)
multiLine = string "/*" *> skipManyTill (string "*/")
comment = singleLine <|> multiLine

ws :: Parser ()
ws = void $ spaces `sepEndBy` comment

str :: String -> Parser String
str s = string s <* ws
```

---

## **Simple JSON Values**

```haskell
jNull :: Parser JSON
jNull = str "null" $> JNull

jBool :: Parser JSON
jBool =
  str "true" $> JBool True
  <|> str "false" $> JBool False
```

---

## **Numbers**

```haskell
jNumber :: Parser JSON
jNumber = JNumber <$> (sign <*> number) <* ws
  where
    sign = char '-' $> negate <|> pure id
    number = read <$> some digit
```

---

## **Strings**

```haskell
quotedStr :: Parser String
quotedStr =
  between (char '"') (char '"') (many $ satisfy (/= '"'))
  <* ws

jString :: Parser JSON
jString = JString <$> quotedStr
```

---

## **Arrays**

```haskell
jArray :: Parser JSON
jArray =
  between (str "[") (str "]")
  $ JArray <$> json `sepEndBy` str ","
--             ^^^^ 🤨 What's that?

json :: Parser JSON
json = undefined -- We'll return later
```

---

## **Objects**

```haskell
fields :: Parser [(String, JSON)]
fields =
  between (str "{") (str "}")
  $ liftA2 (,) (quotedStr <* str ":") json `sepEndBy` str ","

checkUnique :: [(String, JSON)] -> Parser JSON
checkUnique fields =
  let map = Map.fromList fields
  in
    if Map.size map == length fields
    then pure $ JObject map
    else empty

jObject :: Parser JSON
jObject = fields >>= checkUnique
```

---

## **Tying it all together**

```haskell
json :: Parser JSON
json =
  jNull <|> jBool <|> jNumber <|>
  jString <|> jArray <|> jObject
```

---

## **Wait a minute... 🔁**

* Cyclical definitions
  ```haskell
  jArray = ... json `sepEndBy` str "," ...
  --           ^^^^
  json = ... <|> jArray <|> ...
  --             ^^^^^^
  ```
* That's fine! (Thanks laziness!)
  - Tricky to do in strict languages
  - Simulated with mutability or explicit laziness

---

## **Final Touches**

```haskell
jsonDoc :: Parser JSON
jsonDoc = ws *> json <* eof
-- or:    between ws eof json

parseJSON :: String -> Maybe JSON
parseJSON s = fst <$> runParser jsonDoc s
```

---

## **That's it!**

## **You can now parse JSON, dependency-free.**

---

## **Addendum**

---

## **What I *didn't* talk about**

* Error messages
* Performance & Backtracking (`try`)
* Other stream types (e.g. `Text`, binary streams)
* Expression parsers (`chainl`/`chainr`, operator precedence)
* Combining `Parser` with other monads (`ParserT`)
  - `State`: Context-sensitive languages
  - `Reader`: Config (e.g. localization)

---

## **What libraries to use**

- Haskell: [`parsec`](https://hackage.haskell.org/package/parsec), [`megaparsec`](https://hackage.haskell.org/package/megaparsec), [`attoparsec`](https://hackage.haskell.org/package/attoparsec)
- OCaml: [`angstrom`](https://opam.ocaml.org/packages/angstrom/)
- F#: [`FParsec`](https://www.nuget.org/packages/FParsec)
- C#: [`Pidgin`](https://www.nuget.org/packages/Pidgin/)
- Others: Search `$lang parser combinators`
- Please don't use *this* code in prod

---

## **Work Smart, not Hard**

```haskell
{-# LANGUAGE DerivingVia #-}

-- in Control.Monad.State
newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

newtype Parser a =
  Parser { runParser :: String -> Maybe (a, String) }
    deriving (Functor, Applicative, Alternative, Monad)
    via StateT String Maybe
```

---

## **Thanks for your attention!**

Slides & code available at
[![](assets/GitHub.png) aionescu/parser-combinators-from-scratch](https://github.com/aionescu/parser-combinators-from-scratch)
