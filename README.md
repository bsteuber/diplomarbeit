This is my master thesis project about a Lisp inspired code generation framework in Haskell.
It is not developed anymore and here for reference only.

Here's the English abstract for my (otherwise German) thesis:

Template based code generation is ubiquitous throughout computer
science. It is used in many different forms, for example the C
preprocessor, PHP or XSL Transformations - just to mention a
few. However, most existing tools are quite cumbersome to use and error
prone due to employed syntax and data structures. An alternative, yet
almost forgotten approach is taken by the Lisp family of languages. There,
S-Expressions are used as a simple and universal data structure for code
representation and are being processed by macros that can be regarded as
"lightweight compilers".


### Building the project and example
- You need GHC (Glasgow Haskell Compiler) installed, for the example also a latex processor.
- To compile and text, run "bin/mage" from the magicl directory.
- For the example, run "bin/mage example" - this should build example/pres.pdf among other files.