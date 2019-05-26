# ewe

An interpreter for the lambda calculus.  
To use, `stack install ewe` then `ewe filename`.  

## Syntax

```
Program     = Definition* EOF
Definition  = Identifier "=" Expression ";"
Expression  = Lambda | Application
Lambda      = "\" Identifier+ "." Expression
Application = Value+
Value       = Identifier | "(" Expression ")"
Identifier  = (AlphaNum | "_")+

"\" can also be "λ" or "^".
"=" can also be ":=", "≔", "≝", or "≡".
"." can also be "->", "→", "=>", or "⇒".
Windows users, run `chcp 65001` in your terminal.
```

Line comments are done with `--`, block comments with `{- -}`.  
The `main` symbol must be defined for a program to run.  
ewe also comes with predefined combinators `S`, `K`, `I`, `B`, `C`, `W`, `Y`.  

## Example

```
-- Church encoding

succ = λn f x. f (n f x);
pred = λn f x. n (λg h. h (g f)) (λu. x) (λu. u);

0 = λf x. x;
1 = succ 0;
2 = succ 1;
3 = succ 2;

add = λm n f x. m f (n f x);
mul = λm n f x. m (n f) x;

true = λa b. a;
false = λa b. b;
if = λp a b. p a b;
isZero = λn. n (λx. false) true;

factorial = Y (λr n. if (isZero n) 1 (mul n (r (pred n))));

main = factorial 3;
```

## Credits

Made with lots of help from [here](https://crypto.stanford.edu/~blynn/lambda/).  
