# minilanguage

This is an exercise in Haskell. I'm trying to implement an imperative language, to some extent along the lines of [this Hackerrank exercise](https://www.hackerrank.com/challenges/while-language-fp/problem). Most of it is easy and straightforward, but without any previous knowledge the parsing part has been particularly challenging. By now I have at least realized how to improve the code.

The file contains a detailed description of how things are done. Here's a sample program in the language:

    n | # the value of n is passed as a parameter
    i = 1;
    fib_prev = 0;
    fib = 1;
    while i < n do {
      fib_new = fib_prev + fib;
      fib_prev = fib;         
      fib = fib_new;                   
      i = i + 1                    
      }                           
    | fib # the value of fib is returned after execution
    
This program calculates the nth Fibonacci number. It is included in the source file, and is named fibonacci. It can be run by typing "`run [n] fibonacci`" in ghci, where n is a nonnegative integer that will be assigned to the variable `n` before executing the program. After execution, the value of `fib` will be the nth Fibonacci number, and this value is returned in a string: eg. `"fib:55"`if `n` is 10.


The source contains a few more samples.
