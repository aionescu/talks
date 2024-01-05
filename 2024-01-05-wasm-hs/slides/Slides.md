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

### **WebAssembly**

* A new portable low-level language
  * Use cases: web apps, containers, blockchains, and more!
* Focused on safety
  * Type-safe & Memory-safe
  * Sandboxed
* Similar to traditional assembly languages
  * 2 key differences
    * Stack-based, not register-based
    * Structured control flow

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  // C code
  void f(int x) {
    print(x + 1);
  }
  ```

  ```wasm
  ;; Equivalent Wasm code
  (func $f (param $x i32)
    (local.get $x)
    (i32.const 1)
    (i32.add)
    (call $print)
  )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  ```

  ```wasm
  ;; x == 5
  |> (func $f (param $x i32)
       (local.get $x)
       (i32.const 1)
       (i32.add)
       (call $print)
     )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  ```

  ```wasm
  ;; x == 5
     (func $f (param $x i32)
  |>   (local.get $x)
       (i32.const 1)
       (i32.add)
       (call $print)
     )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  5  ]
  ```

  ```wasm
  ;; x == 5
     (func $f (param $x i32)
  |>   (local.get $x)
       (i32.const 1)
       (i32.add)
       (call $print)
     )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  5  ]
  ```

  ```wasm
  ;; x == 5
     (func $f (param $x i32)
       (local.get $x)
  |>   (i32.const 1)
       (i32.add)
       (call $print)
     )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  5  ]
  [  1  ]
  ```

  ```wasm
  ;; x == 5
     (func $f (param $x i32)
       (local.get $x)
  |>   (i32.const 1)
       (i32.add)
       (call $print)
     )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  5  ]
  [  1  ]
  ```

  ```wasm
  ;; x == 5
     (func $f (param $x i32)
       (local.get $x)
       (i32.const 1)
  |>   (i32.add)
       (call $print)
     )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  6  ]
  ```

  ```wasm
  ;; x == 5
     (func $f (param $x i32)
       (local.get $x)
       (i32.const 1)
  |>   (i32.add)
       (call $print)
     )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  6  ]
  ```

  ```wasm
  ;; x == 5
     (func $f (param $x i32)
       (local.get $x)
       (i32.const 1)
       (i32.add)
  |>   (call $print)
     )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  <div>

  ```c
  Stack:
  [  6  ]
  ```

  ```wasm
  (import (func $print (param i32)))
  ```
  </div>

  ```wasm
  ;; x == 5
     (func $f (param $x i32)
       (local.get $x)
       (i32.const 1)
       (i32.add)
  |>   (call $print)
     )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  <div>

  ```c
  Stack:
  ```

  ```c
  Output:
  6
  ```
  </div>

  ```wasm
  ;; x == 5
     (func $f (param $x i32)
       (local.get $x)
       (i32.const 1)
       (i32.add)
  |>   (call $print)
     )
  ```
</div>

---

### **A small WebAssembly example**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  <div>

  ```c
  Stack:
  ```

  ```c
  Output:
  6
  ```
  </div>

  ```wasm
  ;; x == 5
     (func $f (param $x i32)
       (local.get $x)
       (i32.const 1)
       (i32.add)
       (call $print)
  |> )
  ```
</div>

---

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  // C code
  void f(int x) {
    if (x != 0)
      print(x);
  }
  ```

  ```wasm
  ;; Equivalent Wasm code
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

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  ```

  ```wasm
  ;; x == 0
  |> (func $f (param $x i32)
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

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  ```

  ```wasm
  ;; x == 0
     (func $f (param $x i32)
  |>   (block $b
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

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  ```

  ```wasm
  ;; x == 0
     (func $f (param $x i32)
       (block $b
  |>     (local.get $x)
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

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  0  ]
  ```

  ```wasm
  ;; x == 0
     (func $f (param $x i32)
       (block $b
  |>     (local.get $x)
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

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  0  ]
  ```

  ```wasm
  ;; x == 0
     (func $f (param $x i32)
       (block $b
         (local.get $x)
  |>     (i32.const 0)
         (i32.eq)
         (br_if $b)

         (local.get $x)
         (call $print)
       )
     )
  ```
</div>

---

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  0  ]
  [  0  ]
  ```

  ```wasm
  ;; x == 0
     (func $f (param $x i32)
       (block $b
         (local.get $x)
  |>     (i32.const 0)
         (i32.eq)
         (br_if $b)

         (local.get $x)
         (call $print)
       )
     )
  ```
</div>

---

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  0  ]
  [  0  ]
  ```

  ```wasm
  ;; x == 0
     (func $f (param $x i32)
       (block $b
         (local.get $x)
         (i32.const 0)
  |>     (i32.eq)
         (br_if $b)

         (local.get $x)
         (call $print)
       )
     )
  ```
</div>

---

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  1  ]
  ```

  ```wasm
  ;; x == 0
     (func $f (param $x i32)
       (block $b
         (local.get $x)
         (i32.const 0)
  |>     (i32.eq)
         (br_if $b)

         (local.get $x)
         (call $print)
       )
     )
  ```
</div>

---

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  [  1  ]
  ```

  ```wasm
  ;; x == 0
     (func $f (param $x i32)
       (block $b
         (local.get $x)
         (i32.const 0)
         (i32.eq)
  |>     (br_if $b)

         (local.get $x)
         (call $print)
       )
     )
  ```
</div>

---

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```c
  Stack:
  ```

  ```wasm
  ;; x == 0
     (func $f (param $x i32)
       (block $b
         (local.get $x)
         (i32.const 0)
         (i32.eq)
         (br_if $b)

         (local.get $x)
         (call $print)
  |>   )
     )
  ```
</div>

---

### **Structured control-flow**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  <div>

  ```c
  Stack:
  ```

  ```c
  Output:
  ```
  </div>

  ```wasm
  ;; x == 0
     (func $f (param $x i32)
       (block $b
         (local.get $x)
         (i32.const 0)
         (i32.eq)
         (br_if $b)

         (local.get $x)
         (call $print)
       )
  |> )
  ```
</div>

---

### **`wasm-hs`**

* eDSL for embedding Wasm in Haskell
  * Embedded Domain-Specific Language
  * ≈ Library pretending to be a language
* Goals
  - Write and run Wasm programs directly within Haskell
  * Preserve Wasm's focus on safety
  * Look as much as possible like native Wasm

---

### **Quick sneak-peek**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```wasm
  (func $f (param $x i32)

    (block $b
      (local.get $x)
      (i32.const 0)
      (i32.eq)
      (br_if $b)

      (local.get $x)
      (call $print)))
  ```

  ```haskell
  fn #f do
    let' #x do
      block #b do
        local.get #x
        const 0
        eq
        br_if #b

        local.get #x
        call #print
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

### **How to represent WASM?**

* Let's start small
  - What's the smallest unit of Wasm syntax?
  * The _Instruction_
* Representing instructions
  * They operate on stacks of values
  * "Functions on stacks"
  * `add :: [i32, i32] -> [i32]`

---

### **Representing instructions**

To represent them in Haskell, we can use a GADT:

```haskell
{-# LANGUAGE GADTs #-}

type Stack = [Type]

data Instr (i :: Stack) (o :: Stack) where



```

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

### **Instructions with constraints**

```haskell
data Instr (i :: Stack) (o :: Stack) where
  Dup :: Instr (a : i) (a : a : i)
  Drop :: Instr (a : i) i
  Const :: a -> Instr i (a : i)

  Add :: Num a => Instr (a : a : i) (a : i)
  Eq :: Ord a => Instr (a : a : i) (Bool : i)
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

### **How to run Wasm?**

* We could just compile to textual Wasm
  * Restricts the language
  * Harder to _embed_
* Let's write an interpreter instead!

---

### **Sketching out the interpreter**

```haskell
eval :: Instr i o -> i -> IO o
```

* Doesn't quite work...
  * `i` and `o` are _type-level_ lists
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

### **Using `HList`s for stacks**

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
eval Print (a :> i) = print a >> pure i
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

### **How to write Wasm?**

* We can construct programs directly with the AST
* But that's pretty ugly
  ```haskell
  program :: Instr '[] '[]
  program = Seq (Const 10) (Seq (Const 20) (Seq Add Print))
  ```
* Goal: Look like native Wasm
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
  * It doesn't have the right _kind_
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

### **Lowercase aliases**

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
* We can now define and run (simple) programs!

---

### **Variables in Wasm**

* In WASM, variables are defined up-front
  * Need initial values
  * Can lead to scope-safety issues
* `wasm-hs` instead implements scoped variables
  * Introduced by the `let` instruction
  * Delimits a scope, like `block`

---

### **Desired syntax**

```haskell
program = do
  const 10
  let a do
    local.get a
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
data Index vs v a -- Definition omitted

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
data Index vs v a -- Definition omitted

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
data Var a -- Definition omitted

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

### **Just delegate to Haskell (again)**

```haskell
data Var a -- Definition omitted

data Instr (i :: Stack) (o :: Stack) where
  ...
  Let :: (Var a -> Instr i o) -> Instr (a : i) o
  LocalGet :: Var a -> Instr i (a : i)
  LocalSet :: Var a -> Instr (a : i) i
```

---

### **Just delegate to Haskell (again)**

```haskell
class Var a -- Definition omitted

data Instr (i :: Stack) (o :: Stack) where
  ...
  Let :: (Var a => Instr i o) -> Instr (a : i) o
  LocalGet :: Var a => Instr i (a : i)
  LocalSet :: Var a => Instr (a : i) i
```

---

### **Just delegate to Haskell (again)**

```haskell
class Var (v :: Symbol) a -- Definition omitted

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

#### **Interlude: How GHC compiles typeclasses**

GHC desugars typeclasses into "dictionary-passing":

* `class`es become records of functions (≈ vtables)
* `instance`s become _values_ of these record types
* Constraints become regular function parameters
  - `=>` becomes `->`
* Instance selection becomes function application

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

* \*If you **_never_** define instances outside of `eval`
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
    local.get @"a"
    const 1
    add
  print
```

* Need `@"lots"` of `@"quotes"` ☹️
* Solution: `OverloadedLabels`

---

### **Making variables ergonomic**

```haskell
program = do
  const 10
  let' #a do
    local.get #a
    const 1
    add
  print
```

- Need `@"lots"` of `@"quotes"` ☹️
- Solution: `OverloadedLabels`

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

### **Let's recap: Variables**

* `wasm-hs` implements _scoped_ variables
* We used HOAS to represent scopes _safely_ and _efficiently_
* We used _local instances_ and `OverloadedLabels` for ergonomics
* Questions?

---

### **Structured control-flow**

* Wasm doesn't support arbitrary jumps
* Statically-scoped labels
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
  -- Direct style
  f :: Int -> Int
  f x = x + 1
  ```

  <div></div>
</div>

<ul>
  <li data-bespoke-marp-fragment="inactive"></li>
  <li data-bespoke-marp-fragment="inactive"></li>
</ul>

---

### **Continuation-passing style**

<div style="display: grid; grid-template-columns: 1fr 1fr">

  ```haskell
  -- Direct style
  f :: Int -> Int
  f x = x + 1
  ```

  ```haskell
  -- Continuation-passing style
  f :: Int -> (Int -> r) -> r
  f x k = k (x + 1)
  ```
</div>

* Main advantage: "Inversion of control"
* `f` can choose _when_ and _how_ to call `k`

---

### **Rewriting `eval` in CPS**

```haskell
eval :: Instr i o -> HList i -> (HList o -> r) -> r
eval Dup (a : i) k = k (a :> a :> i)
eval Drop (_ : i) k = k i
...
```

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

### **Let's recap: Control-flow**

* `block` is similar to `let`
  - Re-use "local instances" machinery
* Efficiently interpret jumps by using continuations
* Questions?

---

### **Dynamic allocation**

* In Wasm, this is pretty cumbersome
  * Single untyped buffer (the "memory")
  * Can reinterpret bytes
  * Manual memory management
* `wasm-hs` implements "segments" instead
  - Inspired by the [`MSWasm` paper](https://dl.acm.org/doi/pdf/10.1145/3571208)
  * Statically-scoped, typed buffers

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

data Instr (i :: Stack) (o :: Stack) where
  ...
  LetSeg :: (Seg s a => Instr i o) -> Instr (a : Int : i) o
  SegLoad :: Seg s a => Instr (Int : i) (a : i)
  SegStore :: Seg s a => Instr (Int : a : i) i
```

* Note: variables, labels, and segments live in different namespaces

---

### **...and more!**

* `wasm-hs` includes more features than we could cover today:
  * Functions & recursion
  * Modules
  * Host-shared memory
* If you're interested, check out the source code:
  - [![](assets/GitHub.png) aionescu/wasm-hs](https://github.com/aionescu/wasm-hs)
  * Or find me during the breaks

---

### **Why?**

* Why try to "make Haskell look like WebAssembly?"
  * Seems pretty arbitrary and pointless
* Great vehicle for exploring interesting ideas
* Push Haskell's type system to the limit

---

### **Thanks for your attention!**

Code: [![](assets/GitHub.png) aionescu/wasm-hs](https://github.com/aionescu/wasm-hs)
Slides: [![](assets/GitHub.png) aionescu/talks](https://github.com/aionescu/talks)
