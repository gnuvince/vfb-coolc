class List {
  head: Object;
  next: List;
  conv: A2I <- new A2I;

  init(s: Object, rest: List): List {{
    head <- s;
    next <- rest;
    self;
  }};

  flatten(): String {
    let res: String <- case head of
                         i: Int => conv.i2a(i);
                         s: String => s;
                         o: Object => "";
                       esac
     in
     if (isvoid next) then
       res
     else
       res.concat(next.flatten())
     fi
  };
};

class Main inherits IO {
  main(): Int {
    let hello: String <- "Hello,",
        world: String <- " World!",
        newline: String <- "\n",
        nil: List,
        list: List <- (new List).init(hello,
                         (new List).init(world,
                            (new List).init(newline, nil)))
    in {
      out_string(hello.concat(world.concat(newline)));
      out_string(list.flatten());
      0;
    }
  };
};
