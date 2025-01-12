{
  iter = |xs| {
    i = 0;
    while i < xs.len() {
        yield xs.get(i);
        i = i + 1;
    }
    yield null;
  };

  map = |gen f| {
    x = next gen;
    while x != null {
      yield f(x);
      x = next gen;
    }
    yield null;
  };

  zip = |xs ys| {
    i = 0;
    while i < xs.len() {
      yield [xs.get(i), ys.get(i)];
      i = i + 1;
    }
    yield null;
  };

  unzip = |gen| {
    ls = [];
    rs = [];
    x = next gen;
    while x != null {
      ls = ls + [x.get(0)];
      rs = rs + [x.get(1)];
      x = next gen;
    }
    [ls, rs]
  };

  array = |gen| {
    xs = [];
    x = next gen;
    while x != null {
      xs = xs + [x];
      x = next gen;
    }
    xs
  };

  abs = |x| {
    if x < 0 {
      -x
    } else {
      x
    }
  };

  sum = |gen| {
    s = 0;
    x = next gen;
    while x != null {
      s = s + x;
      x = next gen;
    }
    s
  };

  text = input();
  xs = text.split("\n").iter().map(|line| line.split("   ")).unzip();
  ls = xs.get(0);
  rs = xs.get(1);
  ls = ls.iter().map(|x| int(x)).array();
  ls = ls.sorted();
  rs = rs.iter().map(|x| int(x)).array();
  rs = rs.sorted();
  result = zip(ls rs).map(|l| l.get(0) - l.get(1)).map(abs).sum();
  print(result);
  null
}
