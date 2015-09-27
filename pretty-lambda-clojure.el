;; modified by Yonatan Koren to work for clojure's `fn' keyword
;; contact: yon@yonatankoren.com
;;
;;; pretty-lambda-clojure.el --- Show the word `fn' as the Greek letter lambda.
;;
;; ORIGINAL DESCRIPTION:
;; Filename: pretty-lambdada.el
;; Description: Show the word `lambda' as the Greek letter.
;; Author: Drew Adams
;;         See http://www.emacswiki.org/emacs/PrettyLambda for the original
;;         code snippet and its history.
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2009-2015, Drew Adams, all rights reserved.
;; Created: Sun Jun 14 11:07:04 2009 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Jan  1 11:10:11 2015 (-0800)
;;           By: dradams
;;     Update #: 152
;; URL: http://www.emacswiki.org/pretty-lambdada.el
;; Doc URL: http://www.emacswiki.org/PrettyLambda
;; Keywords: convenience display
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   None
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OMMITED SPECIFIC INFO FOR pretty-lambdada.el 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;###autoload
(defgroup pretty-lambda-clojure nil
  "Display of the word `fn' as the Greek character lambda."
    :group 'convenience :group 'programming)

;;;###autoload
(defcustom pretty-lambda-clojure-auto-modes
  '(lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "*Modes affected by `pretty-lambda-clojure-for-modes'."
  :type '(repeat symbol) :group 'pretty-lambda-clojure)

;;;###autoload
(defun pretty-lambda-clojure-for-modes (&optional turn-off)
  "Use `pretty-lambda-clojure-mode' for modes in `pretty-lambda-clojure-auto-modes'.
`C-u' to turn off."
  (interactive "P")
  (let (hook-var)
    (cond (turn-off
           (dolist (m  pretty-lambda-clojure-auto-modes)
             (remove-hook (setq hook-var (intern (concat (symbol-name m) "-hook")))
                          'turn-on-pretty-lambda-clojure-mode)
             (add-hook hook-var 'turn-off-pretty-lambda-clojure-mode))
           (when (memq major-mode pretty-lambda-clojure-auto-modes)
             (turn-off-pretty-lambda-clojure-mode))) ; Current buffer
          (t
           (dolist (m  pretty-lambda-clojure-auto-modes)
             (remove-hook (setq hook-var (intern (concat (symbol-name m) "-hook")))
                          'turn-off-pretty-lambda-clojure-mode)
             (add-hook hook-var 'turn-on-pretty-lambda-clojure-mode))
           (when (memq major-mode pretty-lambda-clojure-auto-modes)
             (turn-on-pretty-lambda-clojure-mode)))))) ; Current buffer

;;;###autoload
(define-minor-mode pretty-lambda-clojure-mode
    "Buffer-local minor mode to display the word `fn' as the Greek letter lambda.
With ARG, turn mode on if ARG is positive, off otherwise."
  :init-value nil
  (cond (pretty-lambda-clojure-mode
         (pretty-lambda-clojure)
         (font-lock-fontify-buffer))
        (t
         (font-lock-remove-keywords
          nil `(("\\<fn\\>"
                 (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                           ,(make-char 'greek-iso8859-7 107))
                           nil)))))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward "\\<fn\\>" nil t)
             (decompose-region (match-beginning 0) (match-end 0)))))))

;;;###autoload
(define-globalized-minor-mode global-pretty-lambda-clojure-mode
    pretty-lambda-clojure-mode turn-on-pretty-lambda-clojure-mode
    "Global minor mode to display the word `fn' as the Greek letter lambda.
With ARG, turn mode on if ARG is positive, off otherwise.")

;; This was originally from <URL: http://www.emacswiki.org/emacs/PrettyLambda>.
;; See that page for the history of this code snippet.  I just added MODE as an
;; optional argument.
(defun pretty-lambda-clojure (&optional mode)
  "Display the word `fn' as the Greek letter lambda.
Non-nil optional arg means use pretty-lambda-clojure display in that MODE.
nil means use pretty-lambda-clojure display for the current mode."
  (font-lock-add-keywords
   mode `(("\\<fn\\>"
   (0 (progn (compose-region (match-beginning 0) (match-end 0)
        ,(make-char 'greek-iso8859-7 107))
      nil))))))

(defun turn-on-pretty-lambda-clojure-mode  () (pretty-lambda-clojure-mode  1))
(defun turn-off-pretty-lambda-clojure-mode () (pretty-lambda-clojure-mode -1))

;;;;

(provide 'pretty-lambda-clojure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pretty-lambda-clojure.el ends here
