# Continuer: An experimetanl programming language based on save-restore continuations

## Introduction

Continuer is an experimental language based continuations.  Unlike other
programming languages, continuer has two primitives for continuations.

### save 

Form:

```
save c
```

Store current contiuation to new variable `c`.  The variable `c` has 
`Continuation` type.  Continuation objects are used by `continue`
primitives.

### continue

Form:

```
continue expr
```

`expr` must be typed to `Continuation`.  At the first, `expr` is evaluated.
Then, the result continuation is called.

Requirement:

- Java 8 or later
- sbt 1.3.0 or later

```sh
$ sbt assembly
...

$ find . -name "*.jar"
./target/scala-2.13/continuer.jar

$ java -jar target/scala-2.13/continuer.jar
...

$ java -jar target/scala-2.13/continuer.jar -e "..."
```

## Note

Current implementation is based on naive CPS transformations.  Loop by
continuations cause `StackOverflowError`.
