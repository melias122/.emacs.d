;;; thrift.el --- major mode for fbthrift and Apache Thrift files  -*- lexical-binding: t; -*-

;; Keywords: languages
;; Package-Version: 20200212.1903
;; Package-Commit: 0c613efa4c2f09fb26083dd6537de08ec80e1195
;; Package-Requires: ((emacs "24"))

;; Licensed to the Apache Software Foundation (ASF) under one
;; or more contributor license agreements. See the NOTICE file
;; distributed with this work for additional information
;; regarding copyright ownership. The ASF licenses this file
;; to you under the Apache License, Version 2.0 (the
;; "License"); you may not use this file except in compliance
;; with the License. You may obtain a copy of the License at
;;
;;   http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing,
;; software distributed under the License is distributed on an
;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;; KIND, either express or implied. See the License for the
;; specific language governing permissions and limitations
;; under the License.

;;; Commentary:

;; This is a major mode for syntax highlighting .thrift files.
;;
;; Features:
;;
;; * Precise syntax highlighting based on the thrift parser code.
;; * Highlighting of method definitions
;; * Custom face for struct/argument indexes
;; * Highlighting of doxygen keywords
;;
;; This mode works for both fbthrift and Apache Thrift files.

;;; Code:

(defvar thrift-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Both " and ' are strings.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?' "\"" table)

    ;; Treat < and > as paired delimiters.
    (modify-syntax-entry ?< "(<" table)
    (modify-syntax-entry ?> ")>" table)

    ;; Comments can start with //, /* or # characters.
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    table))

(defgroup thrift nil
  "A major mode for editing .thrift files."
  :group 'languages)

(defface thrift-ordinal-face
  '((t :foreground "orange"))
  "Face used to highlight Thrift indexes."
  :group 'thrift)

(defface thrift-doxygen-key-face
  '((t :foreground "SlateGray"))
  "Face used to highlight @foo in doxygen style comments."
  :group 'thrift)

(defun thrift--doxygen-search (pattern limit)
  "Search for PATTERN in /** comments, up to position LIMIT.
If we find a match, set match-data and put point on the final
location, and return point. Otherwise, return nil and don't move point.

This is intended to be used with `font-lock-keywords'."
  (let (res
        match-data)
    (save-match-data
      ;; Search forward for the first @foo..
      (while (and
              (not res)
              (re-search-forward pattern limit t))
        ;; Set match data to the @foo we found, before we call `looking-at'.
        (setq match-data (match-data))

        (let* ((ppss (syntax-ppss))
               (in-comment-p (nth 4 ppss))
               (comment-start (nth 8 ppss))
               in-doxygen-comment-p)
          ;; Go to the start of the comment, and confirm this is a /** comment.
          (when in-comment-p
            (save-excursion
              (goto-char comment-start)
              (when (looking-at (rx "/**"))
                (setq in-doxygen-comment-p t))))

          (when in-doxygen-comment-p
            (setq res (point))))))
    ;; Set match data and return point, so Emacs knows we found something.
    (when res
      (set-match-data match-data)
      res)))

(defvar thrift-font-lock-keywords
  `((,(regexp-opt
       ;; Useful keywords in thriftl.ll.
       '("namespace"
         "cpp_include"
         "hs_include"
         "include"
         "oneway"
         "typedef"
         "struct"
         "union"
         "exception"
         "extends"
         "throws"
         "service"
         "enum"
         "const"
         ;; Deprecated
         "cpp_type"
         "async")
       'symbols)
     . font-lock-keyword-face)
    ;; Keywords in thriftl.ll that correspond to types.
    (,(regexp-opt
       '("binary"
         "bool"
         "byte"
         "double"
         "float"
         "hash_map"
         "hash_set"
         "i16"
         "i32"
         "i64"
         "list"
         "map"
         "set"
         "slist"
         "stream"
         "string"
         "void"
         ;; Treat optional/required as type keywords, as they occur in that context.
         "optional"
         "required")
       'symbols)
     . font-lock-type-face)
    ;; Reserved words in thirftl.ll that don't currently do anything.
    (,(regexp-opt
       '("abstract"
         "and"
         "args"
         "as"
         "assert"
         "auto"
         "break"
         "case"
         "char"
         "class"
         "continue"
         "declare"
         "def"
         "default"
         "del"
         "delete"
         "do"
         "elif"
         "else"
         "elseif"
         "except"
         "exec"
         "extern"
         "finally"
         "for"
         "foreach"
         "function"
         "global"
         "goto"
         "if"
         "implements"
         "import"
         "in"
         "int"
         "inline"
         "instanceof"
         "interface"
         "is"
         "lambda"
         "long"
         "native"
         "new"
         "not"
         "or"
         "pass"
         "public"
         "print"
         "private"
         "protected"
         "raise"
         "register"
         "return"
         "short"
         "signed"
         "sizeof"
         "static"
         "switch"
         "synchronized"
         "template"
         "this"
         "throw"
         "transient"
         "try"
         "unsigned"
         "var"
         "virtual"
         "volatile"
         "while"
         "with"
         "yield"
         "Object"
         "Client"
         "IFace"
         "Processor")
       'symbols)
     . font-lock-type-face)
    (,(regexp-opt
       '("true" "false")
       'symbols)
     . font-lock-constant-face)
    ;; Constants are all in upper case, and cannot start with a
    ;; digit. We use font-lock-variable-name-face for consistence with
    ;; c-mode.
    (,(rx symbol-start
          (any upper "_")
          (+ (any upper "_" digit))
          symbol-end)
     . font-lock-variable-name-face)
    ;; Highlight type declarations.
    (,(rx symbol-start
          (or "enum" "service" "struct")
          symbol-end
          (+ space)
          symbol-start
          (*
           (seq (syntax word) "."))
          (group (+ (or (syntax word) (syntax symbol))))
          symbol-end)
     1 font-lock-type-face)
    ;; Highlight method declarations
    (,(rx symbol-start
          (group (+ (or (syntax word) (syntax symbol))))
          symbol-end
          "(")
     1 font-lock-function-name-face)
    ;; Highlight other PascalCase symbols as types.
    (,(rx symbol-start
          (group
           (any upper)
           (+ (or (syntax word) (syntax symbol))))
          symbol-end)
     1 font-lock-type-face)

    ;; Highlight struct indexes.
    (,(rx symbol-start
          (group (+ digit))
          symbol-end
          ":")
     1 'thrift-ordinal-face)
    ;; Highlight doxygen items that are followed by a symbol, such as '@param foo_bar'.
    (,(lambda (limit)
        (thrift--doxygen-search
         (rx (or "@param" "@throws") symbol-end
             ;; Highlight @param even before we've started typing the name of the param.
             (zero-or-one
              (1+ space)
              (1+ (or (syntax word) (syntax symbol) ".")))
             symbol-end)
         limit))
     0 'thrift-doxygen-key-face t)
    ;; Highlight standalone doxygen items.
    (,(lambda (limit)
        (thrift--doxygen-search
         (rx (or "@return" "@see") symbol-end)
         limit))
     0 'thrift-doxygen-key-face t)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))

;;;###autoload
(define-derived-mode thrift-mode prog-mode "Thrift"
  "Major mode for editing Thrift files."
  (setq-local font-lock-defaults '(thrift-font-lock-keywords))
  (setq-local comment-start "// "))

(provide 'thrift)
;;; thrift.el ends here
