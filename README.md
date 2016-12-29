HBURG (*the acronym stands for Haskell Bottom Up Rewrite Generator*) is a **FREE** and easy to use implementation of a code generator generator based upon [Tree Pattern Matching and Dynamic Programming](http://portal.acm.org/citation.cfm?doid=69558.75700). HBURG is a program that generates tree parsers for cost-augmented tree grammars. It is useful for writing code generators for compilers. Given a mapping of a tree structured intermediate representation onto target machine instructions, HBURG generates a code generator that can be plugged into the instruction selection phase of a compiler.

### Documentation

* [Automatic Code Generation Using Dynamic Programming Techniques](https://www.bytelabs.org/pub/papers/hburg07.pdf)
* [Initial Project Specification](http://www.ssw.jku.at/Teaching/MasterTheses/AutomatischeCodeerzeugung/Aufgabenstellung.pdf)
* [HBURG presentation conveying underlying theory and some nice examples](https://www.bytelabs.org/pub/papers/tpg-presentation07_1.pdf)

### Sources

* [https://www.bytelabs.org/hburg/](https://www.bytelabs.org/hburg/) *(darcs repository)*
	* `darcs get https://www.bytelabs.org/hburg/` *(darcs checkout)*
* [https://github.com/1g0rb0hm/hburg](https://github.com/1g0rb0hm/hburg) *(github repository)*
	* `git clone https://github.com/1g0rb0hm/hburg.git` *(git checkout)*
* [hburg-1.1.3.tar.gz](https://www.bytelabs.org/hburg/hburg-1.1.3.tar.gz) *(source tarball)*

HBURG is released under a [BSD license](http://en.wikipedia.org/wiki/BSD_license) and implemented in [Haskell](http://www.haskell.org/haskellwiki/Haskell). Several Haskell extensions and [GHC](http://www.haskell.org/ghc/)-specific features are used. It is highly recommended to use the [GHC](http://www.haskell.org/ghc/) compiler to build HBURG. You may also need [darcs](http://darcs.net/) to checkout the source code from its origin. A [source tarball](https://www.bytelabs.org/hburg/hburg-1.1.3.tar.gz) together with a [GitHub repository](https://github.com/1g0rb0hm/hburg) exists as well.

### HBURG Installation

#### Hackage Installation

[Hackage](https://hackage.haskell.org/) is the Haskell community's central package archive of open source software. [HBURG](https://hackage.haskell.org/package/hburg) is available as a package on [Hackage](https://hackage.haskell.org/) and can be installed as follows:

```bash
 $ cabal update
 $ cabal install hburg
```

#### Source Installation

The following steps are necessary if you would like to checkout the source code using [darcs](http://darcs.net/), configure, and build manually:

##### Checkout:
```bash
 $ darcs get https://www.bytelabs.org/hburg/ && cd hburg
```

If you are not using **darcs** you may want to download the source distribution: [hburg-1.1.3.tar.gz](https://www.bytelabs.org/hburg/hburg-1.1.3.tar.gz).

#####  Configure:
```bash
 $ runghc Setup.hs configure
```

#####  Build:
```bash
 $ runghc Setup.hs build
```

##### Install:
```bash
 $ runghc Setup.hs install
```

##### Test:
```bash
 $ runghc Test.hs test 
```


### Example

The best way to demonstrate how to use HBURG is by example. Below you find a link to a tarball which includes a compiler written in Java for a simple language. The compiler itself does not perform any optimizations, nor does it produce fully functional and executable assembly code. The following common stages found in almost all compilers are implemented:


* Lexing &amp; Parsing via a [LL(k)](http://en.wikipedia.org/wiki/LL_parser)  [Coco/R](http://ssw.jku.at/coco/) attribute grammar specification (*see src/sl/parser/SL.atg*)
* Construction of an AST and simple context sensitive analysis
* RISC like code generation via an HBURG tree pattern matching grammar specification (*see src/sl/code/risc/RISC.tpg*)

Example programs of the simple language as well as an intermediate AST XML dump and the resulting assembly files can be found in the examples directory of the compiler (*see below on how to build the compiler, the **test** ant target runs the examples through the compiler toolchain*).

* Sources: [java-compiler.tgz](https://www.bytelabs.org/hburg/java-compiler.tgz) *(Example compiler showing how to use HBURG)*
* Building and running the example CISC compiler:

```bash
 $ ant clean parser cisc build test
```
* Building and running the example RISC compiler:

```bash
 $ ant clean parser risc build test
```
* For HBURG [ant](http://ant.apache.org/) integration see the **risc** and **cisc** targets in the *build.xml* file.


### References &amp; Related Projects

* [Code Generation using Tree Matching and Dynamic Programming](http://portal.acm.org/citation.cfm?id=69558.75700) [Alfred V. Aho, Mahadevan Ganapathi, Steven W.K Tjiang]
* [Engineering a Simple and Efficient Code Generator Generator](http://ray.cslab.ece.ntua.gr/~gtsouk/docs/algo/var/iburg.pdf) [Fraser, Ch.W., Hanson D.R., Proebsting T.A.]
* [BEG - A Generator for Efficient Back Ends](http://portal.acm.org/citation.cfm?id=74818.74838&coll=portal&dl=ACM) [Emmelmann, H., Schr&ouml;er, F.-W., Landwehr, R.]
* [Automating the Selection of Code Templates](http://www3.interscience.wiley.com/cgi-bin/abstract/113447075/ABSTRACT) [Horspool, N.R.]
* [Simple and Efficient BURS Table Generation](http://portal.acm.org/citation.cfm?id=143145&dl=GUIDE&dl=ACM) [Proebsting, T.A.]
* [BURS Automata Generation](http://portal.acm.org/citation.cfm?id=203095.203098) [Proebsting, T.A.]
* [iBurg](http://www.cs.princeton.edu/software/iburg/)
* [JBurg](http://jburg.sourceforge.net/)
* [OCamlBurg](http://www.cminusminus.org/tools.html)
