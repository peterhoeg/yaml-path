;;; yaml-path.el --- Display the path of the current YAML element.  -*- lexical-binding: t; -*-

;; Copyright (C) 2011  Craig B. Ludington
;; Copyright (C) 2019  Peter Hoeg

;; Author: Craig B. Ludington <me@alpheus.me>
;; URL: http://alpheus.me/src/emacs/yaml-path/yaml-path.el
;; Keywords: yaml path
;; Version: 0.0.3

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This provides a function to display an xpath-like string
;; for the yaml element on the current line.
;; It's meant to complement Yoshiki Kurihara's yaml-mode.el.
;;
;; It doesn't strictly depend on yaml-mode, but works better
;; with yaml-mode.  (That's because yaml-mode makes syntax-ppss usable
;; for locating comments.)
;;
;; The only function you should need is yaml-path/path.
;; It's convenient to bind that to a key, e.g. C-c C-p.
;; (Instructions for doing that only in yaml-mode are below.)
;;
;;; Installation:

;; To install, just drop this file into a directory in your
;; `load-path' and (optionally) byte-compile it.  To automatically
;; handle files ending in '.yml', add something like:
;;
;;    (require 'yaml-path)
;;
;; to your ~/.emacs (or ~/.emacs.d/init.el).
;;
;; If you've installed yaml-mode, and would like to have C-c C-p show you the path
;; when you're in yaml-mode, add this to your initialization file.
;;
;;    (add-hook 'yaml-mode-hook
;;     '(lambda ()
;;        (define-key yaml-mode-map (kbd "C-c C-p") 'yaml-path/path)))


;;; Known Bugs:
;;
;;    Should use line-number-at-pos and goto-line
;;    instead of using point everywhere.
;;
;;    Should be faster, so that motion functions like next-line, previous-line
;;    could be advised and the path would be continuously displayed.
;;
;;; Code:
;;

(eval-when-compile
  (require 'cl-lib))

;;;###autoload
(defvar yaml-path-maximum-file-size 1000000
  "Maximum file size to enable dynamic current path location.")

;;;###autoload
(defvar yaml-path-separator "."
  "The character used to separate elements in the path.")

(defun yaml-path/empty-line-p ()
  "Is the current line empty?"
  (let ((p (string-match-p "^\\s-*$" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
    (and p (zerop p))))

(defun yaml-path/comment-line-p ()
  "Is the current line a comment?"
  (save-excursion
    (move-to-column (current-indentation))
    (unless (eolp)
      (forward-char))
    (nth 4 (syntax-ppss))))

(defun yaml-path/yaml-line-p ()
  "Is the current line a valid YAML line?"
  (not (or (yaml-path/empty-line-p)
           (yaml-path/comment-line-p))))

(cl-defun yaml-path/prev-yaml (&optional p)
  "Return the point of the yaml before the point `P' (default to current point) or nil."
  (cl-labels ((f ()
                 (unless (= (point) (point-min))
                   (forward-line -1)
                   (beginning-of-line)
                   (if (yaml-path/yaml-line-p)
                       (point)
                     (f)))))
    (save-excursion
      (goto-char (or p (point)))
      (f))))

(cl-defun yaml-path/indentation-at-point (p)
  "Return indentation at `P'."
  (save-excursion
    (goto-char p)
    (current-indentation)))

(cl-defun yaml-path/previous-less-indented-point (starting-point indentation)
  "Return a point on a YAML line (not a comment or empty line) that precedes `STARTING-POINT' and has less `INDENTATION'.
In other words, the parent of the YAML element on the line
containing starting-point."
  (let ((p starting-point)
        (result nil)
        (done? nil))
    (while (not done?)
      (if (setq p (yaml-path/prev-yaml p))
          (when (< (yaml-path/indentation-at-point p) indentation)
            (setq result p
                  done?  t))
        (setq done? t)))
    result))

(cl-defun yaml-path/tag-at-point (p)
  "Return a string representing the yaml tag on the line containing the point `P'."
  (save-excursion (goto-char p)
                  (move-to-column (current-indentation))
                  (substring-no-properties (thing-at-point 'sexp))))

(cl-defun yaml-path/tags-of-interest (p i)
  "Return a list of yaml tags such that each one is on the chain."
  (cl-labels ((f (p i)
                 (let ((next-point (yaml-path/previous-less-indented-point p i)))
                   (when next-point
                     (cons next-point (f next-point (yaml-path/indentation-at-point next-point)))))))
    (save-excursion
      (let ((points (f p i)))
        (cl-mapcar  #'yaml-path/tag-at-point points)))))

;;;###autoload
(cl-defun yaml-path/get-path ()
  "Return the path to the current YAML element."
  (interactive)
  (when (yaml-path/yaml-line-p)
    (let ((path (apply #'concatenate 'string
                       (reverse (cl-mapcar #'(lambda (s) (format "%s%s" s yaml-path-separator))
                                           (yaml-path/tags-of-interest (point) (current-indentation)))))))
      (format "%s%s" path (yaml-path/tag-at-point (point))))))

;;;###autoload
(cl-defun yaml-path/show-path ()
  "Display the path to the current YAML element in the message area."
  (interactive)
  (message (yaml-path/get-path)))

;;;###autoload
(defalias 'yaml-path/path 'yaml-path/show-path)

(defun yaml-path/safe-to-enable-p ()
  "Safe to enable dynamic path update for current file?"
  (< (file-attribute-size (file-attributes (buffer-name) 'integer))
     yaml-path-maximum-file-size))

;;;###autoload
(defun yaml-path/enable-path-in-eldoc-maybe (&optional force enable-mode)
  "Display the path to the current YAML element using eldoc and optionally `ENABLE-MODE'."
  (when (or (yaml-path/safe-to-enable-p)
            force)
    (set (make-local-variable 'eldoc-documentation-function)
         'yaml-path/get-path)
    (when enable-mode
      (eldoc-mode 1))))

;;;###autoload
(defun yaml-path/enable-path-in-modeline-maybe (&optional force enable-mode)
  "Display the path to the current YAML element in the mode line and optionally `ENABLE-MODE'."
  (when (or (yaml-path/safe-to-enable-p)
            force)
    (add-hook 'which-func-functions 'yaml-path/get-path t t)
    (when enable-mode
      (which-function-mode 1))))

;;;###autoload
(defun yaml-path/enable-current-path-maybe (&optional force enable-modes)
  "Conditionally display the path to the current YAML element and optionally `ENABLE-MODES'."
  (yaml-path/enable-path-in-eldoc-maybe force enable-modes)
  (yaml-path/enable-path-in-modeline-maybe force enable-modes))

;;;###autoload
(cl-defun yaml-path/ruby-expression ()
  "Return Ruby expression that selects the current YAML element.
E.g. ['foo']['bar']['baz']"
  (when (yaml-path/yaml-line-p)
    (let* ((path (apply #'concatenate 'string
                        (reverse (cl-mapcar #'(lambda (s) (format "['%s']" s))
                                            (yaml-path/tags-of-interest (point) (current-indentation))))))
           (leaf (format "['%s']" (yaml-path/tag-at-point (point)))))
      (format "%s%s" path leaf))))

;;;###autoload
(cl-defun yaml-path/display-ruby-expression ()
  "Display (in the message area) the  Ruby expression that selects the current YAML element.
The expression is also added to the `kill-ring'.

E.g. ['foo']['bar']['baz']"
  (interactive)
  (let ((msg (yaml-path/ruby-expression)))
    (if msg
        (progn (message msg)
               (kill-new msg))
      (message "There's no YAML element here."))))

;;;###autoload
(cl-defun yaml-path/ruby-invocation ()
  "Return the Ruby command line that selects and prints the current YAML element.
E.g.  \"ruby -e \\\"require 'yaml'\\\" -e \\\"puts YAML.load_file('/tmp/test.yml')['foo']['bar']['baz']\\\"\""
  (let ((expression (yaml-path/ruby-expression)))
    (when expression
      (format "ruby -e \"require 'yaml'\" -e \"puts YAML.load_file('%s')%s\"" (buffer-file-name) expression))))

;;;###autoload
(cl-defun yaml-path/display-ruby-invocation ()
  "Display the Ruby command line that selects and prints the current YAML element.
The command line is also added to the `kill-ring'.

E.g.  \"ruby -e \\\"require 'yaml'\\\" -e \\\"puts YAML.load_file('/tmp/test.yml')['foo']['bar']['baz']\\\"\""
  (interactive)
  (let ((msg (yaml-path/ruby-invocation)))
    (if msg
        (progn (message msg)
               (kill-new msg))
      (message "There's no YAML element here."))))

(cl-defun yaml-path/%%test-ruby-invocation%% ()
  "Fuck it.  Call ruby to check our work."
  (interactive)
  (let ((cmd (yaml-path/ruby-invocation)))
    (if cmd
        (shell-command cmd)
      (message "There's no YAML element here."))))

(provide 'yaml-path)
;;; yaml-path.el ends here
