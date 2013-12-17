class Main inherits IO {
  fib(n: Int): Int {
   if n < 2 then
     n
   else
     fib(n-1) + fib(n-2)
   fi
  };

  main(): IO {
   out_int(fib(10))
  };
};
