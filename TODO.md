# Version

## v0.5 Arrays, Strings and Built-in functions

- [ ] New types
  - [ ] String
  - [ ] Array
- [ ] Built-in functions

```slang
a = "hello"
a + " world"
```

```slang
[1, 2, 3] + [4, 5, 6]
```

## v0.4 Functions

- [ ] Block
- [ ] Functions
  - [ ] Define
  - [ ] Call
- [ ] Variables
  - [ ] Assign
  - [ ] Use
- [ ] Scope

```slang
add = fn a b -> a + b
```

## v0.3 Control flow

- [ ] Control flow
  - [ ] If / Else
  - [ ] While

```slang
if 1 < 2 {
  1 + 2
} else {
  3 + 4
}
```

## v0.2 Boolean expressions

- [x] Other types
  - [x] Null
  - [x] Bool
  - [x] Float
- [x] Logical operators
- [x] Relational operators
- [x] Report error
  - [x] Lexer
  - [x] Parser
- [x] Fix `3*-2`

```slang
1 == 3 - 2 or 3 * -2 > 0
```

## v0.1 Calculator

- [x] Arithmetic operators
  - [x] Basic lexer
  - [x] Basic parser
    - [x] Handle precedence
  - [x] Basic compiler
  - [x] Basic VM
  - [x] Parenthesis

```slang
30+2*5+2/(1+1)
```
