# Eocia

This is a compiler that has:


- [x] Variables
- [x] Conditionals
- [ ] Loops
- [ ] Functions
- [ ] Lambdas
- [ ] ADTs
- [ ] Polymorphism with HM-inference

Implementation of the book "Essentials of compilation: Incremental approach". Available [here](https://github.com/IUCompilerCourse/Essentials-of-Compilation).

## How to run


Step 1: Write code in `output/a.code`. Example:

```ocaml
let x = input in
if x == 0 then
  print 37 in 0
else
  print 67 in 0
```

Step 2. `make && ./output/a`
