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
;; See https://mermaid-js.github.io/

;;; Usage:

;; Currently supporting flow charts and sequence diagrams with syntax coloring and indentation.

;; C-c C-c to compile to an image
;; C-c C-f to compile file to an image
;; C-c C-r to compile region to an image
;; C-c C-b to compile buffer to an image
;; C-c C-o to open in the live editor
;; C-c C-d to open the official doc

;;; Customization:

;; You can specify the location of `mmdc` with the variable `mermaid-mmdc-location`,
;; the default assumes you have the binary in your exec PATH.

;; By default `mmdc` will compile to `png` format.
;; You can change that by setting the variable `mermaid-output-format`.

;; By default `mmdc` will use `/tmp` to store tmp-files.
;; You can change that by setting the variable `mermaid-tmp-dir`.

;;; Code:

(require 'f)
(require 'browse-url)
(require 'ob)
(require 'ob-eval)

(defgroup mermaid-mode nil
  "Major mode for working with mermaid graphs."
  :group 'extensions
  :link '(url-link :tag "Repository" "https://github.com/abrochard/mermaid-mode"))

(defcustom mermaid-mmdc-location "mmdc"
  "Mmdc location."
  :type 'string
  :group 'mermaid-mode)

(defcustom mermaid-output-format ".png"
  "Mmdc output format."
  :group 'mermaid-mode
  :type 'string)

(defcustom mermaid-tmp-dir "/tmp/"
  "Dir for tmp files."
  :group 'mermaid-mode
  :type 'string)

(defcustom mermaid-flags ""
  "Additional flags to pass to the mermaid-cli."
  :group 'mermaid-mode
  :type 'string)

(defconst mermaid-font-lock-keywords
  '(("graph \\|subgraph \\|end \\|pie \\|gantt \\|classDiagram \\|stateDiagram \\|title \\|sequenceDiagram\\|loop \\|alt \\|else \\|opt" . font-lock-keyword-face)
    ("---\\|-?->*\\+?\\|==>\\|===" . font-lock-function-name-face)
    ("LR\\|TD\\|TB\\|RL\\|DT\\|BT\\|participant \\|Note" . font-lock-constant-face)))

(defvar org-babel-default-header-args:mermaid
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a mermaid source block.")

(defun org-babel-execute:mermaid (body params)
  "Execute command with BODY and PARAMS from src block."
  (let* ((out-file (or (cdr (assoc :file params))
                       (error "Mermaid requires a \":file\" header argument")))
         (temp-file (org-babel-temp-file "mermaid-"))
         (cmd (concat (shell-quote-argument mermaid-mmdc-location)
                      " -o " (org-babel-process-file-name out-file)
                      " -i " temp-file
                      " " mermaid-flags)))
    (with-temp-file temp-file (insert body))
    (org-babel-eval cmd "")
    nil))

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
  (mermaid-compile-file (f-filename (buffer-file-name))))

(defun mermaid-compile-buffer ()
  "Compile the current mermaid buffer using mmdc."
  (interactive)
  (let* ((tmp-file-name (concat mermaid-tmp-dir "current-buffer.mmd")))
    (write-region (point-min) (point-max) tmp-file-name)
    (mermaid-compile-file tmp-file-name)))

(defun mermaid-compile-region ()
  "Compile the current mermaid region using mmdc."
  (interactive)
  (let* ((tmp-file-name (concat mermaid-tmp-dir "current-region.mmd")))
    (when (use-region-p)
      (write-region (region-beginning) (region-end) tmp-file-name)
      (mermaid-compile-file tmp-file-name))))

(defun mermaid-compile-file (file-name)
  "Compile the given mermaid file using mmdc."
  (interactive "fFilename: ")
  (let* ((input file-name)
         (output (concat (file-name-sans-extension input) mermaid-output-format)))
    (apply #'call-process mermaid-mmdc-location nil "*mmdc*" nil (append (split-string mermaid-flags " ") (list "-i" input "-o" output)))
    (display-buffer (find-file-noselect output t))))

(defun mermaid--make-browser-string (diagram)
  "Create live-editor string for browser access.

DIAGRAM is a string of mermaid-js code to be displayed in the live-editor."
  (concat "https://mermaid-js.github.io/mermaid-live-editor/#/edit/"
          (replace-regexp-in-string "\n" ""
                                    (base64-encode-string
                                     (format "{\"code\":%s,\"mermaid\":{\"theme\":\"default\"},\"updateEditor\":false}"
                                             (json-encode diagram))))))

(defun mermaid-open-browser ()
  "Open the current buffer or active region in the mermaid live editor."
  (interactive)
  (browse-url (mermaid--make-browser-string (if (use-region-p)
                                               (buffer-substring-no-properties (region-beginning) (region-end))
                                             (buffer-string)))))

(defun mermaid-open-doc ()
  "Open the mermaid home page and doc."
  (interactive)
  (browse-url "https://mermaid-js.github.io/"))

(defvar mermaid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'mermaid-compile)
    (define-key map (kbd "C-c C-f") 'mermaid-compile-file)
    (define-key map (kbd "C-c C-b") 'mermaid-compile-buffer)
    (define-key map (kbd "C-c C-r") 'mermaid-compile-region)
    (define-key map (kbd "C-c C-o") 'mermaid-open-browser)
    (define-key map (kbd "C-c C-d") 'mermaid-open-doc)
    map))

;;;###autoload
(define-derived-mode mermaid-mode fundamental-mode "mermaid"
  (setq-local font-lock-defaults '(mermaid-font-lock-keywords))
  (setq-local indent-line-function 'mermaid-indent-line))

(provide 'mermaid-mode)
;;; mermaid-mode.el ends here
