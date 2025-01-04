# Version

## v0.7 Control flow

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

## v0.6 Advent of Code day 1

```slang
{
  input = "1   2\n3   4\n";
  values = input
    .split("\n")
    .filter(line -> line != "")
    .map(line ->
      line
        .split("   ")
        .map(value -> value.parse_int())
    )
    .unzip()
  ls = values.0.sort();
  rs = values.1.sort();
  print(
    zip(ls, rs)
      .map(r -> abs(r.0 - r.1))
      .sum()
  )
}
```

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

- [ ] Functions
  - [ ] Define
  - [ ] Call

```slang
add = a b -> a + b
add 1 2
```

## v0.3 Variables and Scope

- [x] Block / Scope
- [x] Variable / Assign
  - [x] Lexer / Parser
  - [x] Compiler / VM

```slang
a = {
  a = 1;
  b = 2;
  a + b
}
a + 3
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
