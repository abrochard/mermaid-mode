;;; mermaid-indentation-tests.el --- mermaid-mode indentation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026, Konstantin Kharlamov

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

(require 'ert)
(require 'mermaid-mode)

(defun mermaid-test-indentation (before after)
  "Test that indenting BEFORE results in AFTER."
  (with-temp-buffer
    (insert before)
    (mermaid-mode)
    (let ((indent-tabs-mode nil))
      (indent-region (point-min) (point-max)))
    (should (string= after (buffer-string)))))

(ert-deftest mermaid-indent-basic-graph ()
  (mermaid-test-indentation
   "
graph TD
A-->B
C-->D"
   "
graph TD
    A-->B
    C-->D"))

(ert-deftest mermaid-indent-single-subgraph ()
  (mermaid-test-indentation
   "
graph TD
subgraph One
C-->D
E-->F
end
G-->H"
   "
graph TD
    subgraph One
        C-->D
        E-->F
    end
    G-->H"))

(ert-deftest mermaid-indent-nested-subgraphs ()
  :expected-result :failed
  (mermaid-test-indentation
   "
graph TD
subgraph Outer
A-->B
subgraph Inner
C-->D
end
E-->F
end
G-->H"
   "
graph TD
    subgraph Outer
        A-->B
        subgraph Inner
            C-->D
        end
        E-->F
    end
    G-->H"))

(ert-deftest mermaid-indent-sequence-loop ()
  (mermaid-test-indentation
   "
sequenceDiagram
loop Daily check
Alice->>Bob: How are you?
Bob->>Alice: Great!
end"
   "
sequenceDiagram
    loop Daily check
        Alice->>Bob: How are you?
        Bob->>Alice: Great!
    end"))

(ert-deftest mermaid-indent-sequence-alt-else ()
  (mermaid-test-indentation
   "
sequenceDiagram
alt Success
Alice->>Bob: Good
else Failure
Alice->>Bob: Bad
else Unknown
Alice->>Bob: ?
end"
   "
sequenceDiagram
    alt Success
        Alice->>Bob: Good
    else Failure
        Alice->>Bob: Bad
    else Unknown
        Alice->>Bob: ?
    end"))

(ert-deftest mermaid-indent-nested-alt-in-loop ()
  :expected-result :failed
  (mermaid-test-indentation
   "
sequenceDiagram
loop Main loop
alt Happy path
A->>B: Yes
else Sad path
A->>B: No
end
C->>D: Continue
end"
   "
sequenceDiagram
    loop Main loop
        alt Happy path
            A->>B: Yes
        else Sad path
            A->>B: No
        end
        C->>D: Continue
    end"))

(ert-deftest mermaid-indent-open-block ()
  (mermaid-test-indentation
   "
graph TD
subgraph Open
A-->B
C-->D"
   "
graph TD
    subgraph Open
        A-->B
        C-->D"))

(ert-deftest mermaid-indent-gantt ()
  (mermaid-test-indentation
   "
gantt
title Project
section Planning
Task1 :a1, 2024-01-01, 30d
section Execution
Task2 :after a1, 60d"
   "
gantt
    title Project
    section Planning
    Task1 :a1, 2024-01-01, 30d
    section Execution
    Task2 :after a1, 60d"))

(ert-deftest mermaid-indent-er ()
  (mermaid-test-indentation
   "
erDiagram
CUSTOMER ||--o{ ORDER : places
ORDER ||--|{ LINE-ITEM : contains"
   "
erDiagram
    CUSTOMER ||--o{ ORDER : places
    ORDER ||--|{ LINE-ITEM : contains"))

(ert-deftest mermaid-indent-blanks-lines-interjections ()
  "Blank lines are skipped correctly for indentation calculation."
  (mermaid-test-indentation
   "
sequenceDiagram
alt One

A->>B: Hi

else Two
C->>D: Bye"
   "
sequenceDiagram
    alt One

        A->>B: Hi

    else Two
        C->>D: Bye"))

(ert-deftest mermaid-invalid-closer ()
  "Indenting invalid buffer should not cause errors"
  :expected-result :failed
  (mermaid-test-indentation
   "
end"
   "
end"))
