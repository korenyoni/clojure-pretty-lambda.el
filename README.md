#Pretty Lambda for Clojure

Whenever `fn` occurs as a separate word and followed by an opening bracket, it is displayed as the greek letter lambda.

##clojure-pretty-lambda.el

Directly modified from pretty-lambdada.el, where Emacs Lisp anonymous function keyword `lambda` is is displayed the greek letter.
All I did was replace the regular expressions.

The file was originally called pretty-lambda-clojure.el, along with the modes being called pretty-lambda-clojure, etc. But I thought
it was more idiomatic to have the word clojure at the front.

When trying to change the regular expressions so that `fn?` is not changed to a lambda, I had a lot of trouble. After playing around with the logic for hours,
I eventually did some searching and found [this config by cemeric](https://github.com/cemerick/.emacs.d#pretty-lambda-and-co). Only after using this user's regular expressions and
breaking the original prettylambdada.el, I realized exactly how the parameters for font-lock-add-keywords work, namely the indeces in `(match-begin)` and `(match-end)`.

I also made a [post](http://yonatankoren.com/post/4-emacs-cider-and-me) on my blog about my experience with emacs and porting pretty-lambdada.el to Clojure.

##How to use it:

1. Make sure clojure-pretty-lambda.el is in your load path.
2. Require it: `(require 'clojure-pretty-lambda)`
3. Enable `clojure-pretty-lambda-mode`

If anything, the functionality is exactly the same as
[pretty-lambdada.el](http://www.emacswiki.org/emacs/pretty-lambdada.el).

Original Author: David Adams

License: GNU Public License

Contact: yon@yonatankoren.com
