# Praktikum CompilerBau - mini Java compiler

build instructions:
  cabal build

## usage:
   straigtline-exe -v <mini-java file>
    -v: output intermediate results"

 compiling and linking mini-java program:
  ./dist/build/main/main HelloWorld.java
  gcc -m32 runtime.c HelloWorld.s -o HelloWorld


## known limitations:
  o no support for class inheritance
  o IO Exception clause are not checked
  o typechecks are not fully implemented,
    not all ShouldFail Tests pass

## additional featurs
  1. compiler errors show file location of errors.
  2. runtime checks before any access to allocated variables, i.e.
     int arrays and objects.
  3. errors numbers follow the pattern :
     rrrrcccee rrrr = row
     	       ccc  = column
	       ee   = error number as seen in Translate

  
