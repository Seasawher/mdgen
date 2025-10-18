# block comment in top of the file
this is a block comment

```lean
-- this is a test of inline comment
```

## module comment
this is a module comment

## markdown content
This is a list
* first
* second
* third

This is a [link](hoge).
This is a **bold text**.
This is a `inline code`.
This is a block code:
```lean
def hoge : Nat := 0
```
This is an *italic text*.

````lean
variable (P Q : Prop)

example (hQ : Q) : P → Q := by
  -- test of lean codes
  intro _
  exact hQ

/-- test of doc comment -/
example (h : P) : P ∨ Q := by
  /-
  test of inner block comments
  -/
  apply Or.inl
  exact h

/-- doc comment

/- test of block comment in doc -/
-/
macro "foo" : term => `(0)

/-- test of code block in doc comment

```lean
example : hoge = 42 := by
  rfl
```
-/
def hoge := 42

inductive MyNat where
  | zero
  | succ (n : MyNat)

-- Here is a sample of converting doc comment to block comment
/- info: MyNat.zero : MyNat -/
#check MyNat.zero

namespace MyNat
  /- ## indent block -/

  /- info: zero.succ : MyNat -/
  #check MyNat.succ MyNat.zero

end MyNat

macro "echo" x:str : command => `(#eval $x)
````

## nested comment
Here is a sample of nested block comment:
/- Hi. I am a nested comment! -/

Here is another example of nested block comment:
```lean
/-! ### sample -/

/- wao. this is another sample!! -/

/-- this is doc comment in comment block -/
def foo : Nat := 0
```

```text
-- test for language metadata for code block
echo "Hello, world!"
```

## quoted code

> Here is a sample of quoted code:
> ```lean
> def greet := "hello, world"
> ```
> quoted code says something.

> both quoted and language metadata
> ```hoge
> def quotedHoge := "hoge"
> ```
> quoted hoge says `hoge`.

## Uniform Internal Link Syntax

* [link to Second.md](Second/Second.md)
* [link to Third.md](Third/Third/Third.md)
* [link to Fourth.md](Fourth.md)
