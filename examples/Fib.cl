class Main {
  io: IO <- new IO;

  fib(n: Int): Int {
   if n < 2 then
     n
   else
     fib(n-1) + fib(n-2)
   fi
  };

  main(): Int {{
   io.out_int(fib(10));
   0;
  }};
};
