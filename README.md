# Haskell-Forward-Chainer
Use forward chaining to compute the well-founded model for a set of input rules.

An example ruleset would be something like
```
a <= b1, b2, ~c, ~c10
h <= a, b, c
g <= ~e, ~f
f
```
You can read the first line as "a is true if b1 and b2 are true and c and c10 are false", and the last line as "f is true". Using these statements, the program will compute which atoms are true and which are false. 

To run it on the above ruleset, create an input.txt file containing this:
```
[a, [b1, b2], [c, c10]]
[h, [a, b, c], []]
[g, [], [e, f]]
[f, [], []]
```
Compile the program and run it with ./forward-chaining input.txt. It will spit out an output file showing the sets of true and false atoms at each step.
You can also run it online at http://www.compileonline.com/compile_haskell_online.php
