# minilanguage

This is an exercise in Haskell. I'm trying to implement an imperative language, to some extent along the lines of [this Hackerrank exercise](https://www.hackerrank.com/challenges/while-language-fp/problem). Most of it is easy and straightforward, but without any previous knowledge the parsing part has been particularly challenging. By now I have at least realized how to improve the code.

The file contains a detailed description of how things are done. Here's a sample program in the language:

    v1 = 1;
    v2 = 0;
    v3 = 1;
    while v1 < v0 do {
        v4 = v2 + v3;
        v2 = v3;
        v3 = v4;
        v1 = v1 + 1
    }   
    | v3

This program calculates the v0th Fibonacci number. It is included in the source file, and is named fibonacci. It can be run by typing "run [n] fibonacci" in ghci, where n is a nonnegative integer. The source contains a few other samples.
