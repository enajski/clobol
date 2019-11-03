# Clobol

## Compile a very small subset of Clojure to gnu-cobol

This project exists because I wanted to learn more about Clojure's AST, compilers and COBOL. 

### Generate COBOL

Start up a REPL.

`clj -R:nREPL -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"`

To compile the examples for yourself, in `clobol.core`:

```clojure
(save "src/clobol/cobol/hello_world.cob" (compile-clojure-ns 'clobol.clojure.hello-world))
```

#### Examples


```clojure
(ns clobol.clojure.hello-world)

(println "Hello World!!")
```

will compile to

```cobol
      IDENTIFICATION DIVISION.
      PROGRAM-ID. hello-world.
      DATA DIVISION.
      PROCEDURE DIVISION.
      DISPLAY "Hello World!!".
```

```clojure
(ns clobol.clojure.let)

(let [n 5]
  (println "Number" n))
```

```cobol
      IDENTIFICATION DIVISION.
      PROGRAM-ID. let.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 n BINARY-LONG VALUE 5.
      PROCEDURE DIVISION.
      DISPLAY "Number" n.
```

### Compile COBOL executable

to compile a Cobol file use `gnu-cobol`. 

`brew install gnu-cobol`

`cobc -x -free hello_world.cob`

`./hello_world`


