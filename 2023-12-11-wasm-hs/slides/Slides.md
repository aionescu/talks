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

<!-- markdownlint-disable first-line-heading     -->
<!-- markdownlint-disable blanks-around-fences   -->
<!-- markdownlint-disable ul-style               -->
<!-- markdownlint-disable no-alt-text            -->
<!-- markdownlint-disable no-space-in-links      -->
<!-- markdownlint-disable no-duplicate-heading   -->
<!-- markdownlint-disable heading-style          -->
<!-- markdownlint-disable blanks-around-headings -->

![bg left:50% 70%](assets/WasmHaskell.png)

### **Making Haskell look like WebAssembly**

###
###

<div style="display: grid; grid-template-columns: 0.8fr 1.2fr">
  <div></div>

  _Alex Ionescu_
  [![](assets/GitHub.png) aionescu](https://github.com/aionescu)
</div>

---

### **Today's topic: `wasm-hs`**

* Started out as a university project
* eDSL for embedding WebAssembly in Haskell
* But first, what's WebAssembly?

---

### **What is WebAssembly?**

* A new portable low-level language
  * Used in web, containers, blockchains, and more
* Focused on safety
  * Type-safe & Memory-safe
  * Sandboxed
* Similar to traditional assembly languages
  * Stack-based, not register-based
  * Structured control flow

---

### **A quick example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  void f(int x) {
    print(x + 1);
  }
  ```

  ```wasm
  (func $f (param $x i32)
    (local.get $x)
    (i32.const 1)
    (i32.add)
    (call $print)
  )
  ```
</div>

---

### **Control-flow example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  void f(int x) {
    if (x != 0)
      print(x);
  }
  ```

  ```wasm
  (func $f (param $x i32)
    (block $b
      (local.get $x)
      (i32.const 0)
      (i32.eq)
      (br_if $b)

      (local.get $x)
      (call $print)
    )
  )
  ```
</div>

---

<div style="display: grid; grid-template-columns: 1fr 1fr">

  <div>
    <h2 id="loop-example"><strong>Loop example</strong></h2>

```c
void f(int x) {
  while (x < 10) {
    print(x);
    x = x + 1;
  }
}
```
  </div>

  ```wasm
  (func $f (param $x i32)
    (block $b
      (loop $l
        (local.get $x)
        (i32.const 10)
        (i32.ge)
        (br_if $b)

        (local.get $x)
        (call $print)

        (local.get $x)
        (i32.const 1)
        (i32.add)
        (local.set $x)

        (br $l))))
  ```
</div>

---

### **Back to `wasm-hs`**

* eDSL for embedding WebAssembly
  * ≈ Library pretending to be a language
  * Write and run WASM programs directly in Haskell
* Main goals
  * Type-safety ("Intrinsically-typed")
  * Ergonomics
  * Performance
  * Spec-compliance (?)

---

### **Quick sneak-peek**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```wasm
  (func $f (param $x i32)

    (block $b
      (loop $l
        (local.get $x)
        (i32.const 10)
        (i32.ge)
        (br_if $b)

        (local.get $x)
        (call $print)

        (local.get $x)
        (i32.const 1)
        (i32.add)
        (local.set $x)

        (br $l))))
  ```

  ```haskell
  fn #f do
    let' #x do
      block #b do
        loop #l do
          local.get #x
          const 10
          gte
          br_if #b

          local.get #x
          call #print

          local.get #x
          const 1
          add
          local.set #x

          br #l
  ```
</div>

---

### **DSL Deep-Dive**

* How to represent WASM programs
  * Abstract syntax tree
  * Type-safety, scoping
* How to run them
  * Interpreter
* How to write them
  * Overloading Haskell syntax
* Lots of interesting tricks along the way

---

### **How we'll proceed**

* We'll cover the DSL in "vertical slices"
* Starting with the most basic WASM instructions
* Then introduce more complex features one by one
* We might not have time for everything 😅

---

### **How to represent WASM?**

* What is a WASM program made of?
  * Programs are made up of _modules_
  * Which are made up of _functions_
  * Which are made up of _instructions_
* Representing instructions
  * They operate on stacks of values
  * "Functions on stacks"
  * `dup :: [a] -> [a, a]`

---

### **Representing instructions**

To represent them in Haskell, we can use a GADT:

```haskell
{-# LANGUAGE GADTs #-}

type Stack = [Type]

data Instr (i :: Stack) (o :: Stack) where
  Dup :: Instr '[a] '[a, a]


```

---

### **Representing instructions**

To represent them in Haskell, we can use a GADT:

```haskell
{-# LANGUAGE GADTs #-}

type Stack = [Type]

data Instr (i :: Stack) (o :: Stack) where
  Dup :: Instr '[a] '[a, a]
  Drop :: Instr '[a] '[]
  Const :: a -> Instr '[] '[a]
```

---

### **Representing instructions**

To represent them in Haskell, we can use a GADT:

```haskell
{-# LANGUAGE GADTs #-}

type Stack = [Type]

data Instr (i :: Stack) (o :: Stack) where
  Dup :: Instr (a : i) (a : a : i)
  Drop :: Instr (a : i) i
  Const :: a -> Instr i (a : i)
```

---

### **Constraining the types**

```haskell
data Instr (i :: Stack) (o :: Stack) where
  Dup :: Instr (a : i) (a : a : i)
  Drop :: Instr (a : i) i
  Const :: a -> Instr i (a : i)

  Add :: Num a => Instr (a : a : i) (a : i)
  Lt :: Ord a => Instr (a : a : i) (Bool : i)
  Print :: Show a => Instr (a : i) i
```

---

### **Sequencing instructions**

```haskell
data Instr (i :: Stack) (o :: Stack) where
  ...

  Seq :: Instr i o -> Instr o o' -> Instr i o'
```

* Represent single instructions and blocks of instructions _uniformly_

---

### **How to run WASM?**

* We could compile to textual WASM code
  * Restricts the language
  * Harder to _embed_
* Let's write an interpreter instead!

---

### **Sketching out the interpreter**

```haskell
eval :: Instr i o -> i -> IO o
```

* Doesn't quite work...
* We need a data structure with "synced" types
* Well-known technique: Heterogenous lists

---

### **Heterogenous lists**

Lists that store values of _different_ types

```haskell
data HList (as :: [Type]) where
  Nil :: HList '[]
  (:>) :: a -> HList as -> HList (a : as)
```

---

### **We can use `HLists` for stacks**

```haskell
eval :: Instr i o -> HList i -> IO (HList o)
```

---

### **Evaluating simple instructions**

```haskell
eval :: Instr i o -> HList i -> IO (HList o)
eval Dup (a :> i) = pure (a :> a :> i)
eval Drop (_ :> i) = pure i



```

---

### **Evaluating simple instructions**

```haskell
eval :: Instr i o -> HList i -> IO (HList o)
eval Dup (a :> i) = pure (a :> a :> i)
eval Drop (_ :> i) = pure i
eval (Const a) i = pure (a :> i)
eval Add (b :> a :> i) = pure (a + b)
eval Print (a :> i) = print a $> i
```

---

### **Evaluating sequences**

```haskell
eval :: Instr i o -> HList i -> IO (HList o)
...
eval (Seq a b) i = do
  o <- eval a i
  eval b o
```

---

### **How to write WASM?**

* We can construct programs directly with the AST
* But that's pretty ugly
  ```haskell
  program :: Instr '[] '[]
  program = Seq (Const 10) (Seq (Const 20) (Seq Add Print))
  ```
* Goal: Look like inline assembly
* GHC allows us to overload _a lot_ of syntax

---

### **First step: `do` for sequencing**

```haskell
program :: Instr '[] '[]
program = do
  Const 10
  Const 20
  Add
  Print
```

* Unfortunately `Instr` can't be a `Monad`
* We can use `RebindableSyntax`

---

### **`RebindableSyntax` in action**

```haskell
{-# LANGUAGE RebindableSyntax #-}

-- RebindableSyntax implies NoImplicitPrelude
import Prelude hiding ((>>))

class MonadLike m where
  (>>) :: m a b -> m b c -> m a c

instance MonadLike Instr where
  (>>) = Seq
```

---

### **Next up: Lowercase aliases**

```haskell
const :: a -> Instr i (a : i)
const = Const

add :: Eq a => Instr (a : a : i) (a : i)
add = Add

print :: Show a => Instr (a : i) i
print = Print

program = do
  const 10
  const 20
  add
  print
```

---

### **Quick recap**

* We've seen an "end-to-end" slice of the DSL
  * Type-safe AST via `GADT`s
  * `HList`s for interpretation
  * `RebindableSyntax` for ergonomics

---

### **We can now run this:**

```haskell
program :: Instr '[] '[]
program = do
  const 10
  const 20
  add
  print
```

* For more complex programs, we need variables

---

### **Variables in WASM**

* In WASM, variables are defined up-front
  * Need initial values
  * Can lead to scope-safety issues
* `wasm-hs` instead implements `let`
  * Delimits a scope, like `block`

---

### **Variables in the AST**

```haskell
program = do
  const 10
  let a do
    local_get a
    const 1
    add
  print
```

---

### **Variables in the AST**

```haskell
data Instr (i :: Stack) (o :: Stack) where
  ...

  Let :: (Instr i o + ???) -> Instr (a : i) o
  LocalGet :: ??? -> Instr i (a : i)
  LocalSet :: ??? -> Instr (a : i) i
```

* We need to encode information about variables in the type of the AST

---

### **Encoding variables in types**

```haskell
data Index vs v a -- Opaque

data Instr (env :: [(Symbol, Type)]) (i :: Stack) (o :: Stack) where
  ...

  Let :: (Instr ((v, a) : env) i o) -> Instr env (a : i) o
  LocalGet :: Index env v a -> Instr i (a : i)
  LocalSet :: Index env v a -> Instr (a : i) i
```

* Slow to interpret (`O(n)` lookup)
* Pollutes the types

---

### **Encoding variables in types**

```haskell
data Index vs v a -- Opaque

data Instr (env :: [(Symbol, Type)]) (i :: Stack) (o :: Stack) where
  Dup :: Instr env (a : i) (a : a : i)
  ...
  Let :: (Instr ((v, a) : env) i o) -> Instr env (a : i) o
  LocalGet :: Index env v a -> Instr i (a : i)
  LocalSet :: Index env v a -> Instr (a : i) i
```

- Slow to interpret (`O(n)` lookup)
- Pollutes the types

---

### **Just delegate to Haskell**

* We're already inside a language with variables
* Let's just implement WASM variables as _Haskell variables_
* Well-known technique: Higher-Order Abstract Syntax (HOAS)

---

### **Higher-Order Abstract Syntax**

```haskell
data Var a -- Opaque

data Instr (i :: Stack) (o :: Stack) where
  ...
  Let :: (Var a -> Instr i o) -> Instr (a : i) o
  LocalGet :: Var a -> Instr i (a : i)
  LocalSet :: Var a -> Instr (a : i) i
```

---

### **Evaluating HOAS**

```haskell
newtype Var a =
  Var { varRef :: IORef a }

eval :: Instr i o -> HList i -> IO (HList o)
...
eval (Let e) (a : i) = do
  var <- Var <$> newIORef a
  eval (e var) i

eval (LocalGet var) i = do
  a <- readIORef (varRef var)
  pure (a : i)
```

---

### **HOAS in practice**

```haskell
program = do
  const 10
  let' (\a ->
    local.get a
    const 1
    add
  )
  print
```

* Doesn't pollute types ✅
* O(1) access & mutation ✅
* Easy to interpret ✅
* Doesn't vibe-check ❌

---

### **Making the function implicit**

```haskell
data Var a -- Opaque

data Instr (i :: Stack) (o :: Stack) where
  ...
  Let :: (Var a -> Instr i o) -> Instr (a : i) o
  LocalGet :: Var a -> Instr i (a : i)
  LocalSet :: Var a -> Instr (a : i) i
```

---

### **Making the function implicit**

```haskell
class Var a -- Opaque

data Instr (i :: Stack) (o :: Stack) where
  ...
  Let :: (Var a => Instr i o) -> Instr (a : i) o
  LocalGet :: Var a => Instr i (a : i)
  LocalSet :: Var a => Instr (a : i) i
```

---

### **Making the function implicit**

```haskell
class Var (v :: Symbol) a -- Opaque

data Instr (i :: Stack) (o :: Stack) where
  ...
  Let :: (Var v a => Instr i o) -> Instr (a : i) o
  LocalGet :: Var v a => Instr i (a : i)
  LocalSet :: Var v a => Instr (a : i) i
```

---

### **Evaluating with `class`**

```haskell
class Var v a where
  varRef :: IORef a

eval :: Instr i o -> HList i -> IO (HList o)
...
eval (Let @v e) (a : i) = do
  r <- newIORef a
  let var = createVarInstance @v r
  eval (e @var) i

eval (LocalGet @v) i = do
  a <- readIORef (varRef @v)
  pure (a : i)
```

---

### **Interlude: How GHC compiles typeclasses**

GHC desugars typeclasses into "dictionary-passing":

* Typeclasses become records of functions (≈ vtables)
* Instances become _values_ of these record types
* Constraints become regular function parameters
* "Instance selection" becomes function application

---

### **Desugaring example: `Eq`**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

  f :: Eq a => a -> Bool
  f x = x == x
  ```
</div>

---

### **Desugaring example: `Eq`**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

  f :: Eq a => a -> Bool
  f x = x == x
  ```

  ```haskell
  data Eq a = MkEq
    { (==) :: a -> a -> Bool
    , (/=) :: a -> a -> Bool }

  f :: Eq a -> a -> Bool
  f eq_a x = (==) eq_a x x
  ```
</div>

---

### **Desugaring example: `Eq`**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  class Eq a where
    (==) :: a -> a -> Bool


  f :: Eq a => a -> Bool
  f x = x == x
  ```

  ```haskell
  newtype Eq a = MkEq
    { (==) :: a -> a -> Bool }


  f :: Eq a -> a -> Bool
  f eq_a x = (==) eq_a x x
  ```
</div>

---

### **Let's apply this to our `eval`**

* GHC compiles instance selection to function application
* We need to go the other way around
  * Not _normally_ possible
  * Might lead to incoherence
* Thankfully, there's an escape hatch
  * There always is 😀

---

### **The `WithDict` class**

* Typeclass provided by GHC
* Enables "dictionary to instance" transformation
* Only works with single-method classes
* ```haskell
  module GHC.Exts where

  class WithDict cls meth where
    withDict :: meth -> (cls => r) -> r
  ```
* GHC automatically generates `WithDict` instances

---

### **Let's use `withDict` in `eval`**

```haskell
class Var v a where
  varRef :: IORef a

eval :: Instr i o -> HList i -> IO (HList o)
...
eval (Let @v e) (a : i) = do
  r <- newIORef a
  let var = createVarInstance @v r
  eval (e @var) i
eval (LocalGet @v) i = do
  a <- readIORef (varRef @v)
  pure (a : i)
```

---

### **Let's use `withDict` in `eval`**

```haskell
class Var v a where
  varRef :: IORef a

eval :: Instr i o -> HList i -> IO (HList o)
...
eval (Let @v e) (a : i) = do
  r <- newIORef a
  withDict @(Var v a) r $ eval e i

eval (LocalGet @v) i = do
  a <- readIORef (varRef @v)
  pure (a : i)
```

---

### **Let's use `withDict` in `eval`**

```haskell
class Var v a | v -> a where
  varRef :: IORef a

eval :: Instr i o -> HList i -> IO (HList o)
...
eval (Let @v e) (a : i) = do
  r <- newIORef a
  withDict @(Var v _) r $ eval e i

eval (LocalGet @v) i = do
  a <- readIORef (varRef @v)
  pure (a : i)
```

---

### **Is this safe?**

* Yes\*

* \*If you **_never_** define instances outside `eval`
* Can we enforce this?
* Yes\*\*!
  ```haskell
  instance Unsatisfiable (Text "...") => Var v a where
    varRef = undefined
  ```
* \*\*Requires GHC 9.8+

---

### **Making variables ergonomic**

```haskell
program = do
  const 10
  let' @"a" do
    localGet @"a"
    const 1
    add
  print
```

* Need explicit annotations ☹️
* Solution: `OverloadedLabels`

---

### **Making variables ergonomic**

```haskell
program = do
  const 10
  let' #a do
    localGet #a
    const 1
    add
  print
```

* Need explicit annotations ☹️
* Solution: `OverloadedLabels`

---

### **`OverloadedLabels` in action**

```haskell
instance KnownSymbol s => IsLabel s (SSymbol s) where
  fromLabel = symbolSing

let' :: SSymbol v -> (Var v a => Instr i o) -> Instr (a : i) o
let' _ = Let
```

---

### **`OverloadedLabels` in action**

```haskell
instance (KnownSymbol s, ss ~ SSymbol s) => IsLabel s ss where
  fromLabel = symbolSing

let' :: SSymbol v -> (Var v a => Instr i o) -> Instr (a : i) o
let' _ = Let
```

---

### **`OverloadedLabels` in action**

```haskell
program = do
  const 10
  let' #a do
    localGet #a
    const 1
    add
  print
```

---

### **"Instruction groups"**

```haskell
program = do
  const 10
  let' #a do
    local.get #a -- <|
    const 1
    add
  print
```

---

### **"Instruction groups"**

* `local.get`, `local.set`
* `i.div` vs. `f.div`
* Can be achieved using `OverloadedRecordDot`
  * Provides "dot-notation" for records
  * Introduced in GHC 9.2

---

### **`OverloadedRecordDot` in action**

```haskell
data Local v a i o =
  Local
  { get :: ...
  , set :: ...
  }

local :: Local v a i o
local =
  Local
  { get = LocalGet
  , set = LocalSet
  }
```

---

### **Achievement unlocked: Variables**

* ...is everyone still awake?

---

### **Structured control-flow**

* WASM doesn't support arbitrary jumps
* Explicitly-scoped labels
  * Introduced by `block` and `loop`
* Labels have _names_
  * We can reuse the "local instance" machinery

---

### **Representing control-flow**

```haskell
class Label (l :: Symbol) (i :: Stack) where
  ...

data Instr (i :: Stack) (o :: Stack) where
  ...

  Block :: (Label l o => Instr i o) -> Instr i o
  Loop :: (Label l i => Instr i o) -> Instr i o

  Br :: Label l i => Instr i o
  BrIf :: Label l i => Instr (Bool : i) i
```

---

### **Representing control-flow**

```haskell
class Label (l :: Symbol) (i :: Stack) where
  ...

data Instr (i :: Stack) (o :: Stack) where
  ...

  Block :: (Label l o => Instr i o) -> Instr i o
  Loop :: (Label l i => Instr i o) -> Instr i o

  Br :: (Label l i, Append i b i') => Instr i o
  BrIf :: Label l i => Instr (Bool : i) i
```

---

### **Appending and unappending stacks**

```haskell
class Append a b c | a b -> c, a c -> b where
  append :: HList a -> HList b -> HList c
  unappend :: HList c -> (HList a, HList b)
```

---

### **Evaluating control-flow instructions**

* Here's where things get interesting:
  ```haskell
  eval :: Instr i o -> HList i -> IO (HList o)
  ...
  eval (Seq a b) i = eval a i >>= \o -> eval b o k
  eval (Block @l e) i = ???
  eval (Br @l) i = ???
  ```
* We need to capture and store "the rest of the program"
* Well-known technique: Continuation-passing style (CPS)

---

### **Continuation-passing style**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  f :: Int -> Int
  f x = x + 1
  ```

  <div></div>
</div>

<div style="display: grid; grid-template-columns: 1fr 1fr">

  <div></div>

  <div></div>
</div>

---

### **Continuation-passing style**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  f :: Int -> Int
  f x = x + 1
  ```

  <div></div>
</div>

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  f (f (f 0))
  ```

  <div></div>
</div>

---

### **Continuation-passing style**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  f :: Int -> Int
  f x = x + 1
  ```

  ```haskell
  f :: Int -> (Int -> r) -> r
  f x k = k (x + 1)
  ```
</div>

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  f (f (f 0))
  ```

  <div></div>
</div>

---

### **Continuation-passing style**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  f :: Int -> Int
  f x = x + 1
  ```

  ```haskell
  f :: Int -> (Int -> r) -> r
  f x k = k (x + 1)
  ```
</div>

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  f (f (f 0))
  ```

  ```haskell
  f 0 (\y -> f y (\z -> f z id))
  ```
</div>

---

### **Rewriting `eval` in CPS**

```haskell
eval :: Instr i o -> HList i -> (HList o -> IO ()) -> IO ()
eval Dup (a : i) k = k (a :> a :> i)
eval Drop (_ : i) k = k i
...
```

---

### **Evaluating `Seq` with CPS**

```haskell
eval :: Instr i o -> HList i -> (HList o -> IO ()) -> IO ()
eval (Seq a b) i k = eval a i (\o -> eval b o k)
```

* How is this helpful?
* This `eval` has a crucial advantage:
  * `eval a i` can choose _not_ to call its continuation

---

### **Branching via continuations**

```haskell
type Cont i = HList i -> IO ()

class Label l i where
  labelCont :: Cont i

eval :: Instr i o -> HList i -> Cont o -> IO ()
...
eval (Block @l e) i k = withDict @(Label l _) k $ eval e i k



```

---

### **Branching via continuations**

```haskell
type Cont i = HList i -> IO ()

class Label l i where
  labelCont :: Cont i

eval :: Instr i o -> HList i -> Cont o -> IO ()
...
eval (Block @l e) i k = withDict @(Label l _) k $ eval e i k
-- block is just callCC!


```

---

### **Branching via continuations**

```haskell
type Cont i = HList i -> IO ()

class Label l i where
  labelCont :: Cont i

eval :: Instr i o -> HList i -> Cont o -> IO ()
...
eval (Block @l e) i k = withDict @(Label l _) k $ eval e i k
eval (Br @l) i k = labelCont @l i


```

---

### **Branching via continuations**

```haskell
type Cont i = HList i -> IO ()

class Label l i where
  labelCont :: Cont i

eval :: Instr i o -> HList i -> Cont o -> IO ()
...
eval (Block @l e) i k = withDict @(Label l _) k $ eval e i k
eval (Br @l) i' k =
  let (i, b) = unappend i'
  in labelCont @l i
```

---

### **Branching via continuations**

```haskell
type Cont i = HList i -> IO ()

class Label l i where
  labelCont :: Cont i

eval :: Instr i o -> HList i -> Cont o -> IO ()
...
eval (Block @l e) i k = withDict @(Label l _) k $ eval e i k
eval (Br @l) i' k = unappend i' \i b -> labelCont @l i

-- unappend :: HList c -> (HList a -> HList b -> r) -> r
```

---

### **Control-flow is under control**

* Let's _jump_ to the next feature (Sorry).

---

### **Dynamic allocation**

* In WASM, this is pretty cumbersome
  * Single untyped buffer (the "memory")
  * Can reinterpret bytes
  * Manual memory management
* `wasm-hs` implements "segments" instead
  - Inspired by the [`MSWasm` paper](https://dl.acm.org/doi/pdf/10.1145/3571208)
  * Typed buffers, lexically-scoped

---

### **Segments in action**

```haskell
program = do
  const 10
  const True
  let_seg #bools do
    const 0
    seg.load #bools
    print

    const 5
    const False
    seg.grow #bools
```

---

### **Implementation: Same as variables**

```haskell
class Seg s a where
  segRef :: IORef (Vector a)
```

* Note: vars, labels, and segments live in different namespaces

---

### **Next _segment_: Functions**

* WASM functions are similar to C functions
* Stack is used for parameters and return values
  * In WASM, params are spilled into variables
  * In `wasm-hs`, they're not (point-free assembly 😜)
* Multiple returns
* Two ~~sides~~ sites of the coin:
  * Definition-site
  * Call-site

---

### **Functions calls**

```haskell
data Instr (i :: Stack) (o :: Stack) where
  ...
  Call :: (Fn f i o, Append i b i', Append o b o') => Instr i' o'

  ```

---

### **Functions calls**

```haskell
data Instr (i :: Stack) (o :: Stack) where
  ...
  Call :: (Fn f i o, Append i b i', Append o b o') => Instr i' o'
  Ret :: (Return i, Append i b i') => Instr i' o
  ```

---

### **Evaluating `Ret`**

```haskell
class Return o where
  returnCont :: Cont o

eval :: Instr i o -> HList i -> Cont o -> IO ()
...
eval (Ret @i) i' k = unappend i' \i b -> returnCont @i i
```

---

### **Evaluating `Call`**

```haskell
class Fn f i o | f -> i o where
  fnBody :: (Return o, Fn f i o) => Instr i o

eval :: Instr i o -> HList i -> Cont o -> IO ()
...
eval (Call @f) i' k =
  unappend i' \i b ->
    let kf o = k (append o b)
    in withDict @(Return _) kf $ eval (fnBody @f) i kf
```

---

### **Part II: Modules**

* Compilation/encapsulation unit of WASM
* Module ≈ Series of definitions
  * Functions
  * Globals
  * Segments
* `wasm-hs` doesn't support multiple modules

---

### **Series of definitions**

* Earlier, we generalized `Instr` to series of instructions
* Now, let's do the opposite: Split up a module into a series of smaller modules

---

### **The `Mod` type**

```haskell
data Mod (before :: Constraint) (after :: Constraint) where
  MSeq :: Mod c c' -> Mod c' c'' -> Mod c c''
```

* Use constraints to encode scoping
  * Have instance <=> definition exists
* `before`: Definitions this module depends on
* `after`: Definitions this module exports

---

### **Global variables**

```haskell
data Mod (before :: Constraint) (after :: Constraint) where
  MSeq :: Mod c c' -> Mod c' c'' -> Mod c c''

  GlobalVar :: a -> Mod c (Var v a, c)




```

---

### **Global segments**

```haskell
data Mod (before :: Constraint) (after :: Constraint) where
  MSeq :: Mod c c' -> Mod c' c'' -> Mod c c''

  GlobalVar :: a -> Mod c (Var v a, c)


  GlobalSeg :: Vector a -> Mod c (Seg s a, c)

```

---

### **Host-shared memory**

```haskell
data Mod (before :: Constraint) (after :: Constraint) where
  MSeq :: Mod c c' -> Mod c' c'' -> Mod c c''

  GlobalVar :: a -> Mod c (Var v a, c)
  GlobalVarRef :: IORef a -> Mod c (Var v a, c)

  GlobalSeg :: Vector a -> Mod c (Seg s a, c)
  GlobalSegRef :: IORef (Vector a) -> Mod c (Seg s a, c)
```

---

### **Function definitions**

```haskell
data Mod (before :: Constraint) (after :: Constraint) where
  ...
  Fn :: ((Return o, Fn f i o, c) => Instr i o) -> Mod c (Fn f i o, c)
```

---

### **Modules need a `main`**

```haskell
data Mod (before :: Constraint) (after :: Constraint) where
  ...
  Main :: (c => Instr '[] '[]) -> Mod c ()
```

---

### **Modules need _exactly one_ `main`**

```haskell
data Mod (before :: Constraint) (after :: Constraint) where
  ...
  Main :: (c => Instr '[] '[]) -> Mod c ()
```

---

### **Can we enforce this?**

* Yes
* Solution: Yet another class
  * Signal the _absence_ of `main`

---

```haskell
{-# LANGUAGE QuantifiedConstraints #-}

class NoMain


instance Unsatisfiable (Text "...") => NoMain where


data Mod (before :: Constraint) (after :: Constraint) where
  ...
  Main :: (c => NoMain) => (c => Instr '[] '[]) -> Mod c ()
```

---

```haskell
{-# LANGUAGE QuantifiedConstraints #-}

class NoMain where
  dummy :: ()

instance Unsatisfiable (Text "...") => NoMain where
  dummy = ()

data Mod (before :: Constraint) (after :: Constraint) where
  ...
  Main :: (c => NoMain) => (c => Instr '[] '[]) -> Mod c ()
```

---

### **Evaluating an entire module**

CPS, but with constraints:

```haskell
evalMod :: c => Mod c c' -> (c' => IO ()) -> IO ()
...
```

---

### **Evaluating global definitions**

```haskell
evalMod :: c => Mod c c' -> (c' => IO ()) -> IO ()
evalMod m k =
  case m of
    MSeq a b -> evalMod a $ evalMod b k

    GlobalVar @v a -> newIORef a >>= \r -> withDict @(Var v _) r k
    GlobalVarRef @v r -> withDict @(Var v _) r k

    GlobalSeg @s v -> newIORef v >>= \r -> withDict @(Seg s _) r k
    GlobalSegRef @s r -> withDict @(Seg s _) r k




```

---

### **Evaluating function definitions**

```haskell
evalMod :: c => Mod c c' -> (c' => IO ()) -> IO ()
evalMod m k =
  case m of
    MSeq a b -> evalMod a $ evalMod b k

    GlobalVar @v a -> newIORef a >>= \r -> withDict @(Var v _) r k
    GlobalVarRef @v r -> withDict @(Var v _) r k

    GlobalSeg @s v -> newIORef v >>= \r -> withDict @(Seg s _) r k
    GlobalSegRef @s r -> withDict @(Seg s _) r k

    Fn @f @i @o e ->
      withDict @(Fn f _ _) @((Return o, Fn f i o) => _) e k

```

---

### **Evaluating `main`**

```haskell
evalMod :: c => Mod c c' -> (c' => IO ()) -> IO ()
evalMod m k =
  case m of
    MSeq a b -> evalMod a $ evalMod b k

    GlobalVar @v a -> newIORef a >>= \r -> withDict @(Var v _) r k
    GlobalVarRef @v r -> withDict @(Var v _) r k

    GlobalSeg @s v -> newIORef v >>= \r -> withDict @(Seg s _) r k
    GlobalSegRef @s r -> withDict @(Seg s _) r k

    Fn @f @i @o e ->
      withDict @(Fn f _ _) @((Return o, Fn f i o) => _) (eval e) k
    Main e -> eval e (\_ -> pure ()) Nil
```

---

### **Where's `NoMain` coming from?**

```haskell
type Module = Mod NoMain ()

evalModule :: Module -> IO ()
evalModule m = withDict @NoMain () $ evalMod m undefined
```

---

### **`RebindableSyntax` for modules**

```haskell
MonadLike :: forall {k}. (k -> k -> Type) -> Constraint

instance MonadLike Mod where -- MonadLike @Constraint
  (>>) = MSeq

instance MonadLike Instr where -- MonadLike @Stack
  (>>) = Seq
```

---

<div style="display: grid; grid-template-columns: 1fr 1fr">
  <div>
    <h2 id="a-module-implementing-factorial"><strong>Implementing factorial in <code>wasm-hs</code></strong></h2>
  </div>

```haskell
factorial :: Int -> Int
factorial n = evalModule do
  fn @'[Int] #factorial do
    dup
    const 1
    gt
    if #_ then do
      dup
      const 1
      sub
      call #factorial
      mul
    else do
      drop
      const 1

  main do
    const n
    call #factorial
    print
```

---

### **Why?**

* Why try to "make Haskell look like WebAssembly?"
  * Seems pretty arbitrary and pointless
* Great vehicle for exploring interesting ideas
  * I hope you learned at least 1 cool thing today
* Reason #1: Having fun!

---

### **Thanks for your attention!**

Code: [![](assets/GitHub.png) aionescu/wasm-hs](https://github.com/aionescu/wasm-hs)
Slides: [![](assets/GitHub.png) aionescu/talks](https://github.com/aionescu/talks)
