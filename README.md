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

See [here](./examples/church.ewe) for an example of Church encoding.  
