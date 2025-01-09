# Version

## Philosophy

- A block is a function without parameters called immediately
- A program is a block

## v0.7 Advent of Code day 1

```slang
{
  text = input();
  values = text
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

## v0.6 Build-in functions and infix call

- [ ] input()
- [ ] print()
- [ ] split(string)
- [ ] Infix call

```slang
{
  text = input();
  arr = text.split("\n");
  print(arr[1])
}
```

## v0.5 Arrays, Strings

- [ ] Array
- [ ] String

```slang
{
  arr = [1, 2, 3] + [4, 5, 6];
  arr[0] + arr[1] + arr[2] + arr[3] + arr[4] + arr[5]
}
```

```slang
{
  a = "hello"
  a + " world"
}
```

## v0.4 Functions and control flow

- [x] Functions
  - [x] Define
  - [x] Call
- [x] Control flow
  - [x] If
  - [x] While

```slang
{
    pow = |x y| {
        acc = 1;
        while y > 0 {
            acc = acc * x;
            y = y - 1;
        }
        acc
    };
    pow(2 10)
}
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
