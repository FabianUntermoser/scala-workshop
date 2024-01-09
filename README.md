[![License: CC BY-NC 4.0](https://licensebuttons.net/l/by-nc/4.0/80x15.png)](https://creativecommons.org/licenses/by-nc/4.0/)

# Introduction to Functional Programming in Scala

This repository contains the course materials for a workshop on Functional Programming in Scala. You can find lecture
slides, exercises, and projects here. A look at the [Table of Contents](#table-of-contents) shows you the range of
topics we will cover. The workshop starts with a basic introduction into [Scala](https://www.scala-lang.org/) and
Functional Programming and continues into more advanced concepts like type-classes and how to handle side effects.

Since all lecture slides are based on [reveal.js](https://github.com/hakimel/reveal.js) you have to use your arrow keys
to navigate. And you have to navigate in two dimensions. Chapters are organized in columns, slides in rows. To get an
overview of all slides within a lecture press ESC.

## Table of Contents

1. [Introduction](lecture0_introduction/index.html)
2. [Scala 101](lecture1_scala_101/index.html)
3. [Functional Programming 101](lecture2_fp_101/index.html)
4. [Standard Library](lecture3_std_lib/index.html)
5. [Type Classes](lecture4_typeclasses_101/index.html)
6. [Type Class Incarnations](lecture5_typeclasses_incarnations/index.html)
7. [Side Effects](lecture6_side_effects/index.html)
8. [IO](lecture7_io/index.html)

## Follow-Up Materials

This workshop will give you a first introduction to Scala and the Functional Programming paradigm. If you want to go
into greater details you might be interested in the free books provided
by [underscore](https://underscore.io/training/).

## Preparations

### Mandatory

Before you can start you have to install the following tools:

1. JDK 17 or newer
2. [Scala](https://www.scala-lang.org/download/)
3. [SBT](https://www.scala-sbt.org/download.html) - it is the build-tool we will use
4. A Scala IDE, e.g. [IntelliJ](https://www.jetbrains.com/idea/download/)
   or [VSCode](https://code.visualstudio.com/download) with the right plugins (will be covered in the first lecture)
5. `git clone` this repository
 
 You have to install it in that order because each tool relies on the tools installed before.
 
 ### Optional
  - [Ammonite](http://ammonite.io/#Ammonite-REPL) - this is an enhanced REPL which makes it easier to fiddle around with some code. But you don't need it. Scala comes already with a REPL. But it isn't that great :).

## SBT
Here, you find a list of useful SBT commands:

```bash
# start SBT in the repo root and keep it running
sbt

# show all sub-projects
sbt> projects

# switch to a sub-project
sbt> project <name of sub-project>

# compile
sbt> compile

# compile on every file change
sbt> ~compile

# run test
sbt> test

# run a certain test
sbt> Test / testOnly <name of test>
```

If you need some more information take a look into the [introduction slides](https://scalasummerschool.github.io/lectures/introduction/).

## Build the Lectures
To build a lecture go through the following steps:

```bash
$> cd /path/to/repo
$> sbt
# select one of the shown lecture
sbt> projects
sbt> project scala101-lecture

# minimum JS optimizations
sbt> fastCompile

# maximum JS optimizations (Github page)
sbt> fullCompile
sbt> exit

# opens lecture in the default browser
$> open lecture1_scala_101/index.html
```

## License

As stated in the [License file](LICENSE) all lecture slides are provided under Creative Commons BY-NC 4.0. The exercise
code is released under an MIT license.

The following people are authors of the content in this repository:
 - Paul Heymann
 - David Krentzlin
 - Christian Stein
 - Florian Sachse

Georg M. Sorst made some updates to the exercises and lectures. 
