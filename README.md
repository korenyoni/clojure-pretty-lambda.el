#Pretty Lambda for Clojure

Whenever `fn` occurs as a separate word, it is displayed as the greek letter lambda.

##clojure-pretty-lambda.el

Directly modified from pretty-lambdada.el, where Emacs Lisp anonymous function keyword `lambda` is is displayed the greek letter.
All I did was replace the regular expressions.

The file was originally called pretty-lambda-clojure.el, along with the modes being called pretty-lambda-clojure, etc. But I thought
it was more idiomatic to have the word clojure at the front.

##How to use it:

1. Make sure clojure-pretty-lambda.el is in your load path.
2. Require it: `(require 'clojure-pretty-lambda)`
3. Enable `clojure-pretty-lambda-mode`

If anything, the functionality is exactly the same as
[pretty-lambdada.el](http://www.emacswiki.org/emacs/pretty-lambdada.el).

Original Author: David Adams

License: GNU Public License

Contact: yon@yonatankoren.com
