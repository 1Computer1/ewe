# ewe

An interpreter and REPL for the lambda calculus.  
To install, run `stack install`.  
Run `ewe --help` for more info.  
Windows users will have to run `chcp 65001` for Unicode characters to display correctly.  

## Untyped

Normal order evaluation, no static types, only lambdas.  
Use with `--language untyped` or `-lu`.  
Made with lots of help from [here](https://crypto.stanford.edu/~blynn/lambda/).  

```
Program     = Definition* EOF
Definition  = Identifier "=" Expression ";"
Expression  = Lambda | Application
Lambda      = "\" Identifier+ "." Expression
Application = Value+
Value       = Identifier | "(" Expression ")"
Identifier  = (AlphaNum | "_")+
```

Line comments are done with `--`, block comments with `{- -}`.  
Unicode syntax is also available:
- `\` can also be `λ` or `^`.
- `=` can also be `:=`, `≔`, `≝`, or `≡`.
- `.` can also be `->`, `→`, `=>`, or `⇒`.

See [here](./examples/untyped-church.ewe) for an example of Church encoding.  

## Simple

Based on the simply typed lambda calculus.  
Eager evaluation, statically typed, not very useful.  
Contains integers, strings, and booleans, and some operations on them.  
Use with `--language simple` or `-ls`.  

```
Program       = Definition* EOF
Definition    = Identifier "=" Expression ";"
Expression    = Lambda | Application | Branch
Lambda        = "\" ("(" Identifier ":" Type ")")+ "." Expression
Application   = Atom+
Branch        = "if" Expression "then" Expression "else" Expression
Atom          = Identifier | Integer | String | Bool | "(" Expression ")"
Integer       = [0-9]+
String        = '"' ('\"' | .)* '"'
Bool          = "true" | "false"
Identifier    = [a-z][A-Za-z0-9_]*
Type          = TypeAtom ("->" TypeAtom)*
TypeAtom      = TypeIdentifier | "(" Type ")"
TypeIdentifer = [A-Z][A-Za-z0-9_]*
```

Line comments are done with `--`, block comments with `{- -}`.  

See [here](./examples/simple-example.ewe) for an example.  
