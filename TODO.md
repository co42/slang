# Version

## Philosophy

- A block is a function without parameters called immediately
- A program is a block

## v0.6 Advent of Code day 1

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

## v0.5 Built-in functions, infix call, arrays, strings

- [x] Array
- [x] String
- [x] Built-in functions
  - [x] input()
  - [x] print()
  - [x] get(array, index)
  - [x] split(string)
- [x] Infix call

```slang
{
  arr = [1, 2, 3] + [4, 5, 6];
  get(arr, 0) + get(arr, 4)
}
```

```slang
{
  a = "hello"
  a + " world"
}
```

```slang
{
  text = input();
  arr = text.split("\n");
  print(arr[1])
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
