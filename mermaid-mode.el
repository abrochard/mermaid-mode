;;; mermaid-mode.el --- major mode for working with mermaid graphs -*- lexical-binding: t; -*-

;; Copyright (C) 2019, Adrien Brochard

;; This file is NOT part of Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Version: 1.0
;; Author: Adrien Brochard
;; Keywords: mermaid graphs tools processes
;; URL: https://github.com/abrochard/mermaid-mode
;; License: GNU General Public License >= 3
;; Package-Requires: ((f "0.20.0") (emacs "25.3"))

;;; Commentary:

;; Major mode for working with mermaid graphs.
;; See https://mermaidjs.github.io/

;;; Usage:

;; Currently supporting flow charts and sequence diagrams with syntax coloring and indentation.

;; C-c C-c to compile to an image
;; C-c C-o to open in the live editor
;; C-c C-d to open the official doc

;;; Customization:

;; You can specify the location of mmdc with the variable `mermaid-mmdc-location`,
;; the default assumes you have the binary in your exec PATH.

;; By default `mmdc` will compile to png format.
;; You can change that by setting the variable `mermaid-output-format`.

;;; Code:

(require 'f)

(defgroup mermaid-mode nil
  "Major mode for working with mermaid graphs."
  :group 'extensions
  :link '(url-link :tag "Repository" "https://github.com/abrochard/mermaid-mode"))

(defcustom mermaid-mmdc-location "mmdc"
  "Mmdc location."
  :group 'mermaid-mode)

(defcustom mermaid-output-format ".png"
  "Mmdc output format."
  :group 'mermaid-mode)

(defconst mermaid-font-lock-keywords
      '(("graph \\|subgraph \\|end\\|sequenceDiagram\\|loop \\|alt \\|else \\|opt" . font-lock-keyword-face)
        ("---\\|-?->*\\+?" . font-lock-function-name-face)
        ("LR\\|TD\\|participant \\|Note" . font-lock-constant-face)))

(defun mermaid--locate-declaration (str)
  "Locate a certain declaration and return the line difference and indentation.

STR is the declaration."
  (let ((l (line-number-at-pos)))
    (save-excursion
      (if (re-search-backward str (point-min) t)
          (cons (- l (line-number-at-pos)) (current-indentation))
        (cons -1 -1)))))

(defun mermaid-indent-line ()
  "Indent the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((graph (mermaid--locate-declaration "^graph\\|sequenceDiagram"))
          (subgraph (mermaid--locate-declaration "subgraph \\|loop \\|alt \\|opt"))
          (both (mermaid--locate-declaration "^graph \\|^sequenceDiagram$\\|subgraph \\|loop \\|alt \\|opt"))
          (else (mermaid--locate-declaration "else "))
          (end (mermaid--locate-declaration "^ *end *$")))
      (indent-line-to
       (cond ((equal (car graph) 0) 0) ;; this is a graph declaration
             ((equal (car end) 0) (cdr subgraph)) ;; this is "end", indent to nearest subgraph
             ((equal (car subgraph) 0) (+ 4 (cdr graph))) ;; this is a subgraph
             ((equal (car else) 0) (cdr subgraph)) ;; this is "else:, indent to nearest alt
             ;; everything else
             ((< (car end) 0) (+ 4 (cdr both))) ;; no end in sight
             ((< (car both) (car end)) (+ 4 (cdr both))) ;; (sub)graph declaration closer, +4
             (t (cdr end)) ;; end declaration closer, same indent
             )))))

(defun mermaid-compile ()
  "Compile the current mermaid file using mmdc."
  (interactive)
  (let* ((input (f-filename (buffer-file-name)))
         (output (concat (file-name-sans-extension input) mermaid-output-format)))
    (call-process mermaid-mmdc-location nil "*mmdc*" nil "-i" input "-o" output)))

(defun mermaid-open-browser ()
  "Open the current mermaid graph in the live editor."
  (interactive)
  (let ((data (replace-regexp-in-string "\n" "" (base64-encode-string (buffer-string)))))
    (browse-url-default-browser (concat "https://mermaidjs.github.io/mermaid-live-editor/#/edit/" data))))

(defun mermaid-open-doc ()
  "Open the mermaid home page and doc."
  (interactive)
  (browse-url-default-browser "https://mermaidjs.github.io/"))

(defvar mermaid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'mermaid-compile)
    (define-key map (kbd "C-c C-o") 'mermaid-open-browser)
    (define-key map (kbd "C-c C-d") 'mermaid-open-doc)
    map))

;;;###autoload
(define-derived-mode mermaid-mode fundamental-mode "mermaid"
  (setq font-lock-defaults '(mermaid-font-lock-keywords))
  (setq indent-line-function 'mermaid-indent-line))

(provide 'mermaid-mode)
;;; mermaid-mode.el ends here
