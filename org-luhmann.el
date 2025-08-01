;;; org-luhmann.el --- Luhmann numbering system for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Maintainer: Yibie <yibie@outlook.com>
;; URL: https://github.com/yibie/org-luhmann
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: org-mode, outlines, note-taking, zettelkasten

;; This file is not part of GNU Emacs.

;; The MIT License (MIT)


;; Copyright (c) 2024 Yibie
;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;; The Software is provided "as is", without warranty of any kind, express or implied, including but not limited to the warranties of merchantability,
;; fitness for a particular purpose and noninfringement. In no event shall the authors or copyright holders be liable for any claim, damages or other
;; liability, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the software or the use or other dealings
;; in the Software.

;;; Commentary:

;; This package implements Luhmann's numbering system for org-mode.
;; Enhanced version with intelligent numbering suggestions, validation,
;; and improved branch logic.

;;; Code:

(require 'org)
(require 'cl-lib)

(defgroup org-luhmann nil
  "Luhmann numbering system for org-mode."
  :group 'org
  :prefix "org-luhmann-")

(defcustom org-luhmann-number-regex "^\\([0-9]+\\(?:[a-z]*\\)?\\(?:\\.[0-9]+\\(?:[a-z]*\\)?\\)*\\)"
  "Regular expression to match Luhmann numbers."
  :type 'string
  :group 'org-luhmann)

(defcustom org-luhmann-title-separator " "
  "Separator between Luhmann number and title."
  :type 'string
  :group 'org-luhmann)

(defcustom org-luhmann-smart-ordering t
  "Whether to reorder options based on context analysis."
  :type 'boolean
  :group 'org-luhmann)

;; Workbench configuration
(defcustom org-luhmann-workbench-save-file 
  (expand-file-name "org-luhmann-workbenches.el" user-emacs-directory)
  "File to save all workbenches state across sessions."
  :type 'file
  :group 'org-luhmann)

(defcustom org-luhmann-workbench-card-content-length 300
  "Maximum characters to show in workbench card content."
  :type 'integer
  :group 'org-luhmann)

(defvar org-luhmann-workbenches (make-hash-table :test 'equal)
  "Hash table of workbenches. Key is workbench name, value is card list.")

(defvar org-luhmann-current-workbench "default"
  "Name of currently active workbench.")

(defvar org-luhmann-workbench-buffer-name "*Luhmann Workbench*"
  "Name of the workbench buffer.")



;;------------------------------------------------------------------------------
;; Key Bindings
;;------------------------------------------------------------------------------

(defvar org-luhmann-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l n") 'org-luhmann-add-number)
    (define-key map (kbd "C-c l a") 'org-luhmann-add-node)
    (define-key map (kbd "C-c l v") 'org-luhmann-validate-buffer)
    (define-key map (kbd "C-c l s") 'org-luhmann-statistics)
    (define-key map (kbd "C-c l e") 'org-luhmann-export-region-as-links)
    (define-key map (kbd "C-c l ]") 'org-luhmann-next-unnumbered-heading)
    (define-key map (kbd "C-c l [") 'org-luhmann-previous-unnumbered-heading)
    (define-key map (kbd "C-c l w") 'org-luhmann-workbench-add-subtree)
    (define-key map (kbd "C-c l h") 'org-luhmann-workbench-add-heading)
    (define-key map (kbd "C-c l W") 'org-luhmann-workbench-select)
    (define-key map (kbd "C-c l m") 'org-luhmann-move-subtree)
    map)
  "Keymap for org-luhmann-mode.")

;;;###autoload
(define-minor-mode org-luhmann-mode
  "Minor mode for Luhmann numbering system in org-mode."
  :lighter " Luhmann"
  :group 'org-luhmann
  :keymap org-luhmann-mode-map
  (if org-luhmann-mode
      (message "Org-Luhmann mode enabled")
    (message "Org-Luhmann mode disabled")))

;;;###autoload
(defun org-luhmann-setup ()
  "Setup org-luhmann."
  (interactive)
  (org-luhmann-mode 1)
  (org-luhmann--workbenches-load)
  ;; Optional: Enable display enhancement by default
  (when (bound-and-true-p org-luhmann-display-mode)
    (org-luhmann-display-mode 1)))

;;------------------------------------------------------------------------------
;; Core Data Structures and Parsing (Unchanged)
;;------------------------------------------------------------------------------

(defun org-luhmann--parse-number-part (part)
  "Parse a single part of a Luhmann number.
PART is a string like \"1\", \"1a\", etc.
Returns plist with :number and optional :letter properties."
  (if (string-match "^\\([0-9]+\\)\\([a-z]\\)?$" part)
      (let ((num (string-to-number (match-string 1 part)))
            (letter (match-string 2 part)))
        (if letter
            (list :number num :letter letter)
          (list :number num)))
    (list :number (string-to-number part))))

(defun org-luhmann--parse-luhmann-number (title)
  "Parse Luhmann number from node title.
TITLE is the node title string.
Returns a list of plists, each representing one level."
  (when (string-match "^\\([0-9]+\\(?:[a-z]+\\)?\\(?:\\.[0-9]+\\(?:[a-z]+\\)?\\)*\\)[[:space:]]" title)
    (let* ((number-str (match-string 1 title))
           (parts (split-string number-str "\\.")))
      (mapcar #'org-luhmann--parse-number-part parts))))

(defun org-luhmann--number-to-string (number-parts)
  "Convert parsed number parts back to string."
  (string-join
   (mapcar
    (lambda (part)
      (concat
       (number-to-string (plist-get part :number))
       (or (plist-get part :letter) "")))
    number-parts)
   "."))

(defun org-luhmann--increment-letter (letter)
  "Increment letter sequence."
  (cond
   ((null letter) "a")
   ((string= letter "z") "aa")
   ((string-match "z+$" letter)
    (let* ((len (length letter))
           (match-len (length (match-string 0)))
           (non-z (substring letter 0 (- len match-len))))
      (if (string= non-z "")
          (make-string (1+ len) ?a)
        (concat (substring non-z 0 -1)
                (char-to-string (1+ (aref non-z (1- (length non-z)))))
                (make-string match-len ?a)))))
   (t (let ((last-char (aref letter (1- (length letter)))))
        (concat (substring letter 0 -1)
                (char-to-string (1+ last-char)))))))

;;------------------------------------------------------------------------------
;; Enhanced Document Analysis
;;------------------------------------------------------------------------------

(defun org-luhmann--collect-all-numbers ()
  "Collect all Luhmann numbers in the current buffer.
Returns a list of plists with :number, :level, :point, and :title."
  (let ((numbers nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) \\([0-9]+\\(?:[a-z]*\\)?\\(?:\\.[0-9]+\\(?:[a-z]*\\)?\\)*\\) \\(.*\\)$" nil t)
        (let* ((stars (match-string 1))
               (number-str (match-string 2))
               (title (match-string 3))
               (level (length stars))
               (point (match-beginning 0))
               (parsed (org-luhmann--parse-luhmann-number (concat number-str " " title))))
          (when parsed
            (push (list :number number-str
                       :parsed parsed
                       :level level
                       :point point
                       :title title)
                  numbers)))))
    (nreverse numbers)))

(defun org-luhmann--analyze-numbering-patterns ()
  "Analyze the document's numbering patterns to provide intelligent suggestions.
Returns a plist with analysis results."
  (let* ((numbers (org-luhmann--collect-all-numbers))
         (main-numbers (cl-remove-if-not 
                       (lambda (n) (= 1 (length (plist-get n :parsed))))
                       numbers))
         (branch-depth (apply #'max 
                             (cons 1 (mapcar (lambda (n) (length (plist-get n :parsed))) 
                                           numbers))))
         (letter-usage (cl-count-if 
                       (lambda (n) 
                         (cl-some (lambda (part) (plist-get part :letter))
                                 (plist-get n :parsed)))
                       numbers))
         (total-numbers (length numbers)))
    
    (list :total-numbers total-numbers
          :main-numbers (length main-numbers)
          :max-depth branch-depth
          :letter-usage letter-usage
          :has-letters (> letter-usage 0)
          :numbers numbers)))

(defun org-luhmann--suggest-best-option (context patterns)
  "Suggest the best numbering option based on CONTEXT and PATTERNS."
  (let* ((current (plist-get context :current))
         (prev-sibling (plist-get context :prev-sibling))
         (parent-str (plist-get context :parent-str))
         (level (plist-get context :level))
         (has-letters (plist-get patterns :has-letters))
         (total-numbers (plist-get patterns :total-numbers)))
    
    (cond
     ;; If document is empty or very small, suggest main number
     ((< total-numbers 3) 'main)
     
     ;; If we have a current number and it's not too deep, suggest branching
     ((and current (< level 4)) 'branch-current)
     
     ;; If we have a previous sibling and letters are used in document
     ((and prev-sibling has-letters) 'letter-sequence)
     
     ;; If we have a parent, suggest branching from parent
     ((and parent-str (> level 1)) 'branch-parent)
     
     ;; Default to main number
     (t 'main))))

;;------------------------------------------------------------------------------
;; Enhanced Number Generation
;;------------------------------------------------------------------------------

(defun org-luhmann--get-next-main-number ()
  "Get next available main number in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((max-num 0))
      (while (re-search-forward "^\\*+ \\([0-9]+\\)\\(?:[a-z]\\|[.]\\|[[:space:]]\\)" nil t)
        (let ((num (string-to-number (match-string 1))))
          (setq max-num (max max-num num))))
      (number-to-string (1+ max-num)))))

(defun org-luhmann--get-next-branch-number (base-num)
  "Get next available branch number for BASE-NUM."
  (let ((max-branch 0)
        (branch-pattern (format "^\\*+ %s\\.\\([0-9]+\\)" 
                              (regexp-quote base-num))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward branch-pattern nil t)
        (let ((num (string-to-number (match-string 1))))
          (when (> num max-branch)
            (setq max-branch num)))))
    (1+ max-branch)))

(defun org-luhmann--get-next-parent-branch-number (parent-num)
  "Get next available branch number for PARENT-NUM, ignoring letter suffixes."
  (let ((max-branch 0)
        (branch-pattern (format "^\\*+ %s\\.\\([0-9]+\\)" 
                              (regexp-quote parent-num))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward branch-pattern nil t)
        (let* ((num-str (match-string 1))
               (num (string-to-number num-str)))
          (setq max-branch (max max-branch num)))))
    (1+ max-branch)))

(defun org-luhmann--get-next-sibling-letter (number-str)
  "Get next letter in sequence for NUMBER-STR."
  (if (string-match "\\([0-9.]+\\)\\([a-z]+\\)?$" number-str)
      (let ((current-letters (match-string 2 number-str)))
        (if current-letters
            (org-luhmann--increment-letter current-letters)
          "a"))
    "a"))

;;------------------------------------------------------------------------------
;; Improved Context Analysis
;;------------------------------------------------------------------------------

(defun org-luhmann--get-current-context ()
  "Analyze current context for Luhmann numbering with enhanced logic."
  (save-excursion
    (let* ((at-heading (org-at-heading-p))
           (_ (unless at-heading (org-back-to-heading t)))
           (current-level (org-outline-level))
           (current-title (org-get-heading t t t t))
           (current-num (when current-title
                         (when (string-match org-luhmann-number-regex current-title)
                           (match-string 1 current-title))))
           prev-sibling-str
           parent-str)
      
      ;; Get previous sibling info with better logic
      (save-excursion
        (org-back-to-heading t)
        (when (org-get-previous-sibling)
          (let ((prev-title (org-get-heading t t t t)))
            (when (string-match org-luhmann-number-regex prev-title)
              (setq prev-sibling-str (match-string 1 prev-title))))))
      
      ;; Get parent info
      (save-excursion
        (when (and (> current-level 1) (org-up-heading-safe))
          (let ((parent-title (org-get-heading t t t t)))
            (when (string-match org-luhmann-number-regex parent-title)
              (setq parent-str (match-string 1 parent-title))))))
      
      (list :level current-level
            :current current-num
            :prev-sibling prev-sibling-str
            :parent-str parent-str
            :current-title current-title))))

;;------------------------------------------------------------------------------
;; Enhanced Options Generation
;;------------------------------------------------------------------------------

(defun org-luhmann--get-number-options (context &optional _command patterns)
  "Get available numbering options with intelligent ordering.
PATTERNS is the result from org-luhmann--analyze-numbering-patterns."
  (let* ((level (plist-get context :level))
         (prev-sibling (plist-get context :prev-sibling))
         (current (plist-get context :current))
         (parent-str (plist-get context :parent-str))
         (options nil)
         (suggested-type (when (and patterns org-luhmann-smart-ordering)
                          (org-luhmann--suggest-best-option context patterns))))
    
    ;; Generate all possible options
    (let ((all-options nil))
      
      ;; Main number option
      (push `("New main number" . (main nil ,(eq suggested-type 'main))) all-options)
      
      ;; Letter sequence option
      (let ((base-for-letter (or current prev-sibling)))
        (when base-for-letter
          (let ((next-letter (org-luhmann--get-next-sibling-letter base-for-letter)))
            (push `(,(format "Continue sequence (%s%s)" 
                            (if (string-match "\\(.*?\\)\\([a-z]+\\)?$" base-for-letter)
                                (match-string 1 base-for-letter)
                              base-for-letter)
                            next-letter)
                    . (letter-sequence ,base-for-letter ,(eq suggested-type 'letter-sequence)))
                  all-options))))
      
      ;; Branch from current
      (when current
        (push `(,(format "Branch from current (%s.1)" current)
                . (branch-current ,current ,(eq suggested-type 'branch-current)))
              all-options))
      
      ;; Branch from parent
      (when (and (> level 1) parent-str)
        (let ((next-num (org-luhmann--get-next-parent-branch-number parent-str)))
          (push `(,(format "Branch from parent (%s.%d)" parent-str next-num)
                  . (branch-parent ,parent-str ,(eq suggested-type 'branch-parent)))
                all-options)))
      
      ;; Sort options: suggested first if smart ordering is enabled
      (setq options 
            (if org-luhmann-smart-ordering
                (sort all-options 
                      (lambda (a b)
                        (let ((a-suggested (caddr (cdr a)))
                              (b-suggested (caddr (cdr b))))
                          (cond
                           ((and a-suggested (not b-suggested)) t)
                           ((and b-suggested (not a-suggested)) nil)
                           (t nil)))))
              ;; If smart ordering is disabled, keep original order
              (nreverse all-options))))
    
    options))

(defun org-luhmann--generate-number (option _context &optional _patterns)
  "Generate new number based on OPTION and CONTEXT with enhanced logic."
  (let* ((type (cadr option))
         (base (caddr option)))
    (pcase type
      ('main
       (org-luhmann--get-next-main-number))
      
      ('branch-current
       (when base (concat base ".1")))
      
      ('branch-parent
       (when base
         (let ((next-num (org-luhmann--get-next-parent-branch-number base)))
           (format "%s.%d" base next-num))))
      
      ('letter-sequence
       (when base
         (if (string-match "\\(.*?\\)\\([a-z]+\\)?$" base)
             (concat (match-string 1 base)
                    (org-luhmann--get-next-sibling-letter base))
           (concat base "a")))))))

;;------------------------------------------------------------------------------
;; Validation and Repair Functions
;;------------------------------------------------------------------------------

(defun org-luhmann--validate-number-sequence (numbers)
  "Validate a sequence of Luhmann numbers for consistency.
NUMBERS is a list of number info plists.
Returns a list of issues found."
  (let ((issues nil)
        (seen-numbers (make-hash-table :test 'equal)))
    
    (dolist (num-info numbers)
      (let* ((number-str (plist-get num-info :number))
             (level (plist-get num-info :level))
             (point (plist-get num-info :point)))
        
        ;; Check for duplicates
        (if (gethash number-str seen-numbers)
            (push (list :type 'duplicate
                       :number number-str
                       :point point
                       :message (format "Duplicate number: %s" number-str))
                  issues)
          (puthash number-str t seen-numbers))
        
        ;; Check level consistency
        (let* ((parsed (org-luhmann--parse-luhmann-number (concat number-str " title")))
               (expected-level (length parsed)))
          (when (and parsed (not (org-luhmann--validate-number-level number-str level)))
            (push (list :type 'level-mismatch
                       :number number-str
                       :point point
                       :expected expected-level
                       :actual level
                       :message (format "Level mismatch for %s: expected %d, got %d"
                                      number-str expected-level level))
                  issues)))))
    
    issues))

;;;###autoload
(defun org-luhmann-validate-buffer ()
  "Validate all Luhmann numbers in the current buffer.
Reports issues found."
  (interactive)
  (let* ((numbers (org-luhmann--collect-all-numbers))
         (issues (org-luhmann--validate-number-sequence numbers)))
    
    (if (null issues)
        (message "No issues found in Luhmann numbering.")
      (let ((issue-buffer (get-buffer-create "*Luhmann Validation*")))
        (with-current-buffer issue-buffer
          (erase-buffer)
          (insert "Luhmann Numbering Validation Issues\n")
          (insert "====================================\n\n")
          
          (dolist (issue issues)
            (insert (format "• %s\n" (plist-get issue :message)))
            (when (plist-get issue :point)
              (insert (format "  At position: %d\n" (plist-get issue :point))))
            (insert "\n"))
        
        (display-buffer issue-buffer)
        (message "Found %d issues in Luhmann numbering. See *Luhmann Validation* buffer."
                (length issues)))))))

(defun org-luhmann--validate-number-level (number level)
  "Validate if Luhmann NUMBER is appropriate for heading LEVEL."
  (let* ((parts (org-luhmann--parse-luhmann-number (concat number " title")))
         (total-depth (length parts)))
    (<= total-depth level)))

;;------------------------------------------------------------------------------
;; Enhanced User Functions
;;------------------------------------------------------------------------------

(defun org-luhmann-add-number ()
  "Add Luhmann number to current org headline with intelligent suggestions."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Must be on a heading"))
  
  ;; Ensure buffer is writable
  (org-luhmann--ensure-writable-buffer)
  
  (let* ((context (org-luhmann--get-current-context))
         (patterns (org-luhmann--analyze-numbering-patterns))
         (title (org-get-heading t t t t))
         (_has-number (org-luhmann--parse-luhmann-number title))
         (_current-level (org-outline-level))
         (options (org-luhmann--get-number-options context 'add-node patterns))
         (choice (completing-read "Select numbering option: "
                                 (mapcar #'car options) nil t))
         (option (assoc choice options))
         (number (org-luhmann--generate-number option context patterns)))
    
    (when number
      (let ((new-title (if (string-match org-luhmann-number-regex title)
                          (replace-match (concat number org-luhmann-title-separator) nil nil title)
                        (concat number org-luhmann-title-separator title))))
        (org-edit-headline new-title)
        (message "Added Luhmann number: %s" number)
        number))))

(defun org-luhmann-add-node ()
  "Add a new node with Luhmann number using enhanced context detection.
Works from heading line or anywhere within the subtree content."
  (interactive)
  
  ;; Step 1: Smartly locate the reference heading
  (let* ((reference-heading (org-luhmann--find-reference-heading))
         (context nil)
         (patterns nil)
         (options nil))
    
    (unless reference-heading
      (user-error "Cannot find a reference heading with Luhmann number"))
    
    ;; Step 2: Move to reference heading, get context (identical to original)
    (save-excursion
      (goto-char reference-heading)
      (setq context (org-luhmann--get-current-context))
      (setq patterns (org-luhmann--analyze-numbering-patterns))
      (setq options (org-luhmann--get-number-options context 'add-node patterns)))
    
    ;; Step 3: User selection (identical to original)
    (let* ((choice (completing-read "Select numbering option: "
                                   (mapcar #'car options) nil t))
           (option (assoc choice options))
           (generated-number (org-luhmann--generate-number option context patterns)))
      
      (when generated-number
        ;; Step 4: Insert node based on reference heading (identical to original logic)
        (org-luhmann--insert-node-with-reference reference-heading option generated-number)
        (message "Added node: %s (Type heading text)" generated-number)
        generated-number))))

(defun org-luhmann--find-reference-heading ()
  "Find the reference heading for context analysis.
Returns point of the relevant heading, regardless of cursor position."
  (cond
   ;; Case 1: Directly on heading line
   ((org-at-heading-p)
    (let ((title (org-get-heading t t t t)))
      (if (string-match org-luhmann-number-regex title)
          (point)
        ;; Current heading has no number, find nearest numbered heading
        (org-luhmann--find-nearest-numbered-heading))))
   
   ;; Case 2: In content area, find the parent heading
   ((save-excursion (org-back-to-heading t))
    (save-excursion
      (org-back-to-heading t)
      (let ((title (org-get-heading t t t t)))
        (if (string-match org-luhmann-number-regex title)
            (point)
          ;; Current heading has no number, find nearest numbered heading
          (org-luhmann--find-nearest-numbered-heading)))))
   
   ;; Case 3: Cannot find any heading
   (t nil)))

(defun org-luhmann--find-nearest-numbered-heading ()
  "Find the nearest parent heading with a Luhmann number."
  (save-excursion
    (while (and (org-up-heading-safe)
                (let ((title (org-get-heading t t t t)))
                  (not (string-match org-luhmann-number-regex title)))))
    
    (let ((title (org-get-heading t t t t)))
      (when (string-match org-luhmann-number-regex title)
        (point)))))

(defun org-luhmann--insert-node-with-reference (reference-heading option generated-number)
  "Insert new node based on reference heading and option.
Preserves original insertion logic exactly."
  (let* ((option-type (cadr option))
         (current-level (save-excursion 
                         (goto-char reference-heading)
                         (org-outline-level)))
         (new-level (pcase option-type
                     ('letter-sequence current-level)
                     ('branch-current (1+ current-level))
                     ('branch-parent current-level)
                     ('main 1)
                     (_ current-level)))
         (stars (make-string new-level ?*))
         (new-heading-line (concat stars " " generated-number org-luhmann-title-separator)))
    
    (save-excursion
      (goto-char reference-heading)
      
      (pcase option-type
        ('letter-sequence
         (org-end-of-subtree t)
         (insert "\n" new-heading-line))
        
        ('branch-current
         (org-end-of-subtree)
         (insert "\n" new-heading-line))
        
        ('branch-parent
         (org-end-of-subtree t)
         (insert "\n" new-heading-line))
        
        ('main
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert "\n" new-heading-line))
        
        (_
         (org-end-of-subtree t)
         (insert "\n" new-heading-line))))
    
    ;; Move cursor to the position after the number (after the separator)
    (let ((search-pattern (concat "^" (regexp-quote stars) " " 
                                 (regexp-quote generated-number) 
                                 (regexp-quote org-luhmann-title-separator))))
      (if (re-search-forward search-pattern nil t)
          (progn
            ;; Position cursor after the number and separator, ready for typing the title
            (goto-char (match-beginning 0))
            (forward-char (length (concat stars " " generated-number org-luhmann-title-separator)))
            ;; Ensure we're at the end of the line
            (end-of-line))
        ;; Fallback: if search fails, try to find the heading manually
        (goto-char reference-heading)
        (when (re-search-forward (regexp-quote generated-number) nil t)
          (end-of-line))))))

;;------------------------------------------------------------------------------
;; Navigation and Utility Functions
;;------------------------------------------------------------------------------



;;;###autoload
(defun org-luhmann-next-unnumbered-heading ()
  "Move to the next heading that does not have a Luhmann number."
  (interactive)
  (let ((found nil))
    (save-excursion
      (while (and (not found) (not (eobp)))
        (outline-next-heading)
        (when (org-at-heading-p)
          (let ((title (org-get-heading t t t t)))
            (unless (string-match org-luhmann-number-regex title)
              (setq found (point)))))))
    (if found
        (progn
          (goto-char found)
          (org-fold-show-context)
          (message "Found next unnumbered heading."))
      (message "No more unnumbered headings found."))))

;;;###autoload
(defun org-luhmann-previous-unnumbered-heading ()
  "Move to the previous heading that does not have a Luhmann number."
  (interactive)
  (let ((found nil))
    (save-excursion
      (while (and (not found) (not (bobp)))
        (outline-previous-heading)
        (when (org-at-heading-p)
          (let ((title (org-get-heading t t t t)))
            (unless (string-match org-luhmann-number-regex title)
              (setq found (point)))))))
    (if found
        (progn
          (goto-char found)
          (org-fold-show-context)
          (message "Found previous unnumbered heading."))
      (message "No previous unnumbered headings found."))))



;;------------------------------------------------------------------------------
;; Statistics and Analysis
;;------------------------------------------------------------------------------

;;;###autoload
(defun org-luhmann-statistics ()
  "Display statistics about Luhmann numbering in the current buffer."
  (interactive)
  (let* ((patterns (org-luhmann--analyze-numbering-patterns))
         (numbers (plist-get patterns :numbers))
         (buffer (get-buffer-create "*Luhmann Statistics*")))
    
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Luhmann Numbering Statistics\n")
      (insert "============================\n\n")
      
      (insert (format "Total numbers: %d\n" (plist-get patterns :total-numbers)))
      (insert (format "Main numbers: %d\n" (plist-get patterns :main-numbers)))
      (insert (format "Maximum depth: %d\n" (plist-get patterns :max-depth)))
      (insert (format "Numbers with letters: %d\n" (plist-get patterns :letter-usage)))
      (insert (format "Uses letter sequences: %s\n\n" 
                     (if (plist-get patterns :has-letters) "Yes" "No")))
      
      ;; Depth distribution
      (insert "Depth Distribution:\n")
      (let ((depth-counts (make-hash-table)))
        (dolist (num-info numbers)
          (let ((depth (length (plist-get num-info :parsed))))
            (puthash depth (1+ (gethash depth depth-counts 0)) depth-counts)))
        
        (maphash (lambda (depth count)
                  (insert (format "  Level %d: %d numbers\n" depth count)))
                depth-counts))
      
      (insert "\n")
      
      ;; Main sequence analysis
      (insert "Main Sequences:\n")
      (let ((main-sequences (make-hash-table :test 'equal)))
        (dolist (num-info numbers)
          (let* ((number-str (plist-get num-info :number))
                 (main-num (if (string-match "^\\([0-9]+\\)" number-str)
                             (match-string 1 number-str)
                           number-str)))
            (push num-info (gethash main-num main-sequences nil))))
        
        (maphash (lambda (main-num sequence)
                  (insert (format "  %s: %d numbers\n" main-num (length sequence))))
                main-sequences))
      
      (goto-char (point-min)))
    (display-buffer buffer)))

;;------------------------------------------------------------------------------
;; Export and Documentation
;;------------------------------------------------------------------------------

(defvar org-luhmann-export-history nil
  "History list for org-luhmann link export file choices.")

;; Ensure savehist-mode is enabled and register our history variable
(when (boundp 'savehist-mode)
  (unless savehist-mode
    (savehist-mode 1))
  (when (boundp 'savehist-additional-variables)
    (add-to-list 'savehist-additional-variables 'org-luhmann-export-history)))

;;;###autoload
(defun org-luhmann-export-region-as-links (start end file)
  "Export heading(s) in region or at point as org-mode links to a file.
If a region is active, exports all headings within it. Otherwise,
exports the heading at the current point.

This command always prompts for a target file, offering a history
of previous choices and an option to create a new file."
  (interactive
   (let* ((s (if (use-region-p)
                 (region-beginning)
               (save-excursion (org-back-to-heading t) (point))))
          (e (if (use-region-p)
                 (region-end)
               (save-excursion (org-end-of-subtree t) (point))))
          ;; --- Improved file selection logic ---
          (f (read-file-name "Export links to file: " 
                            default-directory
                            (if org-luhmann-export-history
                                (car org-luhmann-export-history)
                              nil))))
     (list s e f)))
  
  (let ((links '()))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (let* ((id (org-id-get-create))
                 (title (org-get-heading t t t t))
                 (link (format " - [[id:%s][%s]]\n" id title)))
            (push link links)))))
    
    (if (null links)
        (message "No headings found in the selected area.")
      (progn
        ;; Create the directory if it doesn't exist
        (let ((dir (file-name-directory file)))
          (when (and dir (not (file-exists-p dir)))
            (make-directory dir t)))
        
        ;; Smart file handling: append to existing files, create new ones
        (with-temp-buffer
          (insert (string-join (nreverse links) ""))
          (if (file-exists-p file)
              ;; Append to existing file
              (append-to-file (buffer-string) nil file)
            ;; Create new file
            (write-file file)))
        
        ;; Add to history
        (add-to-list 'org-luhmann-export-history file)
        
        (message "Exported %d link(s) to %s" (length links) file)))))

;;------------------------------------------------------------------------------
;; Display Enhancement
;;------------------------------------------------------------------------------

(defcustom org-luhmann-display-style 'star
  "How to display org headlines with Luhmann numbers.
Possible values:
- \\='star: Show traditional org-mode stars (default)
- \\='number: Show only Luhmann numbers"
  :type '(choice
          (const :tag "Traditional stars" star)
          (const :tag "Luhmann numbers" number))
  :group 'org-luhmann)

(defun org-luhmann--display-setup ()
  "Set up the display enhancements."
  (when (eq org-luhmann-display-style 'number)
    (font-lock-add-keywords
     nil
     '(("^\\(\\*+\\) \\([0-9]+\\(?:[a-z]*\\)?\\(?:\\.[0-9]+\\(?:[a-z]*\\)?\\)*\\) "
        (0 (progn
             (compose-region (match-beginning 1) (match-end 1) "")
             nil))))))
  (font-lock-flush))

(defun org-luhmann--display-cleanup ()
  "Clean up the display enhancements."
  (font-lock-remove-keywords
   nil
   '(("^\\(\\*+\\) \\([0-9]+\\(?:[a-z]*\\)?\\(?:\\.[0-9]+\\(?:[a-z]*\\)?\\)*\\) "
      (0 (progn
           (compose-region (match-beginning 1) (match-end 1) "")
           nil)))))
  (font-lock-flush)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ " nil t)
      (decompose-region (match-beginning 0) (match-end 0)))))

;;;###autoload
(define-minor-mode org-luhmann-display-mode
  "Minor mode to enhance display of Luhmann numbers in org-mode."
  :lighter " Luhmann-Display"
  :group 'org-luhmann
  (if org-luhmann-display-mode
      (org-luhmann--display-setup)
    (org-luhmann--display-cleanup)))

;;------------------------------------------------------------------------------
;; Setup
;;------------------------------------------------------------------------------

;;;###autoload
(defun org-luhmann-enhanced-setup ()
  "Setup org-luhmann with enhanced features enabled."
  (interactive)
  (org-luhmann-mode 1)
  (when (y-or-n-p "Enable enhanced display mode? ")
    (org-luhmann-display-mode 1))
  (when (y-or-n-p "Validate existing numbers? ")
    (org-luhmann-validate-buffer))
  (message "Org-Luhmann enhanced mode setup complete!"))

;;------------------------------------------------------------------------------
;; Intelligent Subtree Movement
;;------------------------------------------------------------------------------

;;;###autoload
(defun org-luhmann-move-subtree ()
  "Move current subtree to a new parent with intelligent renumbering."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not at a heading"))
  
  ;; Check and handle read-only buffer
  (when buffer-read-only
    (if (y-or-n-p "Buffer is read-only. Make it writable? ")
        (setq buffer-read-only nil)
      (user-error "Cannot move subtree in read-only buffer")))
  
  (let* ((current-number (org-luhmann-get-current-number))
         (current-title (org-get-heading t t t t)))
    (unless current-number
      (user-error "Current heading doesn't have a Luhmann number"))
    
    (when (y-or-n-p (format "Move subtree '%s'? " current-title))
      (let* ((target-parent (org-luhmann--select-move-target))
             (target-number (org-luhmann--calculate-move-target-number target-parent))
             (subtree-data (org-luhmann--extract-subtree-data)))
        
        (when target-number
          (org-luhmann--perform-subtree-move subtree-data target-parent target-number)
          (message "Moved '%s' to %s" current-title target-number))))))

(defun org-luhmann-get-current-number ()
  "Get Luhmann number of current heading."
  (let ((title (org-get-heading t t t t)))
    (when (and title (string-match org-luhmann-number-regex title))
      (match-string 1 title))))

;; 添加调试版本来检查正则表达式匹配
(defun org-luhmann-get-current-number-debug ()
  "Debug version of get current number."
  (let ((title (org-get-heading t t t t)))
    (message "DEBUG: Current heading title: '%s'" title)
    (message "DEBUG: Using regex: %s" org-luhmann-number-regex)
    (when (and title (string-match org-luhmann-number-regex title))
      (message "DEBUG: Regex matched. Group 1: '%s'" (match-string 1 title))
      (match-string 1 title))))

(defun org-luhmann--ensure-writable-buffer ()
  "Ensure current buffer is writable, asking user if needed."
  (when buffer-read-only
    (if (y-or-n-p "Buffer is read-only. Make it writable? ")
        (setq buffer-read-only nil)
      (user-error "Cannot perform operation in read-only buffer"))))

(defun org-luhmann--select-move-target ()
  "Interactively select target parent for moving subtree."
  (let* ((all-numbers (org-luhmann--collect-all-numbers))
         (current-number (org-luhmann-get-current-number))
         (valid-targets (cl-remove-if 
                        (lambda (n) 
                          ;; Exclude self and its children
                          (let ((num (plist-get n :number)))
                            (or (string= num current-number)
                                (string-prefix-p (concat current-number ".") num))))
                        all-numbers))
         (choices (mapcar (lambda (n) 
                          (format "%s %s" (plist-get n :number) (plist-get n :title)))
                         valid-targets))
         (choice (completing-read "Move to parent: " choices nil t)))
    
    (when choice
      (cl-find-if (lambda (n) 
                   (string= choice (format "%s %s" (plist-get n :number) (plist-get n :title))))
                 valid-targets))))

(defun org-luhmann--calculate-move-target-number (target-parent)
  "Calculate the target number for moved subtree."
  (let* ((parent-number (plist-get target-parent :number))
         (base-number (format "%s.1" parent-number))
         (existing-children (org-luhmann--get-child-numbers parent-number)))
    
    (org-luhmann--find-next-available-number base-number existing-children)))

(defun org-luhmann--get-child-numbers (parent-number)
  "Get all direct child numbers of parent."
  (let ((children nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (format "^\\*+ \\(%s\\.[^.]*\\)" (regexp-quote parent-number)) nil t)
        (let ((child-number (match-string 1)))
          (push child-number children))))
    children))

(defun org-luhmann--find-next-available-number (base-number existing-numbers)
  "Find next available number, using letter suffixes for conflicts."
  (let ((number base-number)
        (suffix ""))
    
    ;; Check if base number is available
    (while (member number existing-numbers)
              ;; If conflict, add or increment letter suffix 
      (setq suffix (if (string= suffix "")
                      "a"
                    (org-luhmann--increment-letter suffix)))
      (setq number (concat base-number suffix)))
    
    number))

(defun org-luhmann--extract-subtree-data ()
  "Extract current subtree data for moving."
  (save-excursion
    (org-back-to-heading t)
    (let* ((start (point))
           (end (progn (org-end-of-subtree t t) (point)))
           (content (buffer-substring-no-properties start end))
           (current-number (org-luhmann-get-current-number-debug)))  ; Use debug version
    
    ;; (message "DEBUG: Extracted subtree data - number: '%s', content length: %d" 
    ;;          current-number (length content))
    
    (list :content content
          :start start
          :end end
          :current-number current-number))))

(defun org-luhmann--perform-subtree-move (subtree-data target-parent target-number)
  "Perform the actual subtree move operation."
  (let* ((content (plist-get subtree-data :content))
         (current-number (plist-get subtree-data :current-number))
         (target-point (plist-get target-parent :point)))
    
    ;; (message "DEBUG: Moving subtree with number '%s' to '%s'" current-number target-number)
    
    ;; Ensure we have the correct current number
    (save-excursion
      (goto-char (plist-get subtree-data :start))
      (let ((actual-number (org-luhmann-get-current-number-debug)))
        (when (and actual-number (not (string= actual-number current-number)))
          ;; (message "WARNING: Stored number '%s' differs from actual number '%s'" current-number actual-number)
          (setq current-number actual-number))))
    
    ;; Only renumber if the number actually needs to change
    (let ((renumbered-content 
           (if (string= current-number target-number)
               (progn 
                 ;; (message "DEBUG: Numbers are the same, no renumbering needed")
                 content)
             (org-luhmann--renumber-subtree-content content current-number target-number))))
      
      ;; Ensure buffer is writable
      (when buffer-read-only
        (setq buffer-read-only nil))
      
      ;; Delete at original position
      (goto-char (plist-get subtree-data :start))
      (org-cut-subtree)
      
      ;; Insert at target position
      (goto-char target-point)
      (org-end-of-subtree)
      ;; Ensure there's a newline after the target position
      (unless (bolp) (insert "\n"))
      ;; Insert renumbered content, ensuring correct formatting
      (insert renumbered-content)
      ;; Ensure there's a newline after insertion, but don't repeat
      (unless (or (eolp) (string= (substring renumbered-content -1) "\n"))
        (insert "\n"))
      
       ;; Note: temporarily disable level adjustment, as the renumbered content should already have the correct level structure
       ;; If level adjustment is actually needed, add debug information here
       ;; (message "DEBUG: Subtree move completed without level adjustment")
       )))

(defun org-luhmann--renumber-subtree-content (content old-number new-number)
  "Renumber all headings in subtree content."
  ;; (message "DEBUG: Renumbering from '%s' to '%s'" old-number new-number)
  ;; (message "DEBUG: Content to renumber:\n%s" content)
  
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    
    ;; Renumber main heading
    (let ((main-pattern (format "^\\(\\*+\\) %s" (regexp-quote old-number))))
      ;; (message "DEBUG: Looking for main heading pattern: %s" main-pattern)
      (when (re-search-forward main-pattern nil t)
        ;; (message "DEBUG: Found main heading, replacing...")
        (goto-char (match-beginning 0))
        (when (looking-at (format "^\\(\\*+\\) %s\\(.*\\)$" (regexp-quote old-number)))
          (let* ((stars (match-string 1))
                 (rest-of-line (match-string 2)))
            ;; (message "DEBUG: Replacing main heading '%s' with '%s'" old-number new-number)
            (replace-match (format "%s %s%s" stars new-number rest-of-line))))))
    
    ;; 重编号所有子标题
    (goto-char (point-min))
    (let ((child-pattern (format "^\\(\\*+\\) %s\\." (regexp-quote old-number))))
      ;; (message "DEBUG: Looking for child heading pattern: %s" child-pattern)
      (while (re-search-forward child-pattern nil t)
        ;; (message "DEBUG: Found child heading at line %d" (line-number-at-pos))
        (goto-char (match-beginning 0))
        (when (looking-at (format "^\\(\\*+\\) \\(%s\\.[^[:space:]]*\\)\\(.*\\)$" (regexp-quote old-number)))
          (let* ((stars (match-string 1))
                 (old-full-number (match-string 2))
                 (rest-of-line (match-string 3))
                 ;; 修复：正确提取后缀，保持层级关系
                 (suffix (substring old-full-number (1+ (length old-number))))
                 (new-full-number (concat new-number "." suffix)))
            ;; (message "DEBUG: Replacing '%s' with '%s' (suffix: '%s')" old-full-number new-full-number suffix)
            (replace-match (format "%s %s%s" stars new-full-number rest-of-line))))))
    
    (let ((result (buffer-string)))
      ;; (message "DEBUG: Final renumbered content:\n%s" result)
      result)))

;; 另外，为了调试，你可以添加一个测试函数
(defun org-luhmann--debug-renumber (content old-number new-number)
  "Debug version of renumber function with messages."
  ;; (message "DEBUG: Renumbering from %s to %s" old-number new-number)
  ;; (message "DEBUG: Original content:\n%s" content)
  
  (let ((result (org-luhmann--renumber-subtree-content content old-number new-number)))
    ;; (message "DEBUG: Renumbered content:\n%s" result)
    result))

(defun org-luhmann--adjust-subtree-level (target-parent)
  "Adjust the level of moved subtree to fit under target parent."
  (let* ((target-level (plist-get target-parent :level))
         (new-level (1+ target-level)))
    
    (save-excursion
      (org-back-to-heading t)
      (let ((current-level (org-outline-level))
            (subtree-start (point))
            (subtree-end (save-excursion (org-end-of-subtree t) (point))))
        ;; (message "DEBUG: Adjusting subtree level from %d to %d (diff: %d)" 
        ;;          current-level new-level (- new-level current-level))
        (when (/= current-level new-level)
          (let ((diff (- new-level current-level)))
            ;; 更精确的层级调整：只调整主标题，让子标题保持相对层级
            (if (> diff 0)
                (dotimes (_ diff) (org-demote))
              (dotimes (_ (- diff)) (org-promote)))))))))

;;------------------------------------------------------------------------------
;; Workbench Functions
;;------------------------------------------------------------------------------

;; Helper functions for multi-workbench support
(defun org-luhmann--get-current-cards ()
  "Get cards from current workbench."
  (gethash org-luhmann-current-workbench org-luhmann-workbenches nil))

(defun org-luhmann--set-current-cards (cards)
  "Set cards for current workbench."
  (puthash org-luhmann-current-workbench cards org-luhmann-workbenches))

(defun org-luhmann--ensure-default-workbench ()
  "Ensure default workbench exists."
  (unless (gethash "default" org-luhmann-workbenches)
    (puthash "default" nil org-luhmann-workbenches)))

(defun org-luhmann--extract-card-info ()
  "Extract card information from current heading and its complete subtree."
  (save-excursion
    (org-back-to-heading t)
    (let* ((title (org-get-heading t t t t))
           (number (when (string-match org-luhmann-number-regex title)
                    (match-string 1 title)))
           (point (point))
           (file (buffer-file-name))
           (level (org-current-level))
           (content ""))
      
      ;; Extract content for display (excluding the heading itself)
      (org-end-of-meta-data t)
      (let ((content-start (point))
            (content-end (save-excursion (org-end-of-subtree t) (point))))
        (setq content (buffer-substring-no-properties content-start content-end))
        (setq content (replace-regexp-in-string "^[ \t\n]*" "" content))
        (setq content (replace-regexp-in-string "[ \t\n]*$" "" content))
        (when (> (length content) org-luhmann-workbench-card-content-length)
          (setq content (concat (substring content 0 org-luhmann-workbench-card-content-length) "..."))))
      
      (list :number number
            :title title
            :content content
            :level level
            :point point
            :file file))))

(defun org-luhmann-workbench-select ()
  "Select or create a workbench."
  (interactive)
  (org-luhmann--ensure-default-workbench)
  (let* ((workbench-names (hash-table-keys org-luhmann-workbenches))
         (choices (append 
                  (mapcar (lambda (name) 
                           (format "%s%s (%d cards)" 
                                  (if (string= name org-luhmann-current-workbench) "[Current] " "")
                                  name
                                  (length (gethash name org-luhmann-workbenches))))
                          workbench-names)
                  '("+ Create new workbench..."
                    "📝 Manage workbenches...")))
         (choice (completing-read "Select workbench: " choices nil t)))
    
    (cond
     ((string= choice "+ Create new workbench...")
      (org-luhmann-workbench-create))
     ((string= choice "📝 Manage workbenches...")
      (org-luhmann-workbench-manage))
     (t
      ;; Extract workbench name from choice
      (let ((name (if (string-match "^\\(?:\\[Current\\] \\)?\\([^ ]+\\)" choice)
                     (match-string 1 choice)
                   choice)))
        (setq org-luhmann-current-workbench name)
        (org-luhmann-workbench-show)
        (message "Switched to workbench: %s" name))))))

(defun org-luhmann-workbench-create ()
  "Create a new workbench."
  (interactive)
  (let ((name (read-string "New workbench name: ")))
    (when (and name (not (string= name "")))
      (puthash name nil org-luhmann-workbenches)
      (setq org-luhmann-current-workbench name)
      (org-luhmann--workbenches-save)
      (org-luhmann-workbench-show)
      (message "Created and switched to workbench: %s" name))))

(defun org-luhmann-workbench-manage ()
  "Manage workbenches (rename, delete)."
  (interactive)
  (let* ((workbench-names (hash-table-keys org-luhmann-workbenches))
         (choices (append 
                  (mapcar (lambda (name) 
                           (format "%s%s (%d cards)" 
                                  (if (string= name org-luhmann-current-workbench) "[Current] " "")
                                  name
                                  (length (gethash name org-luhmann-workbenches))))
                          workbench-names)
                  '("+ Create new workbench..."
                    "🗑️ Delete workbench..."
                    "✏️ Rename workbench...")))
         (choice (completing-read "Manage workbench: " choices nil t)))
    
    (cond
     ((string= choice "+ Create new workbench...")
      (org-luhmann-workbench-create))
     ((string= choice "🗑️ Delete workbench...")
      (org-luhmann-workbench-delete))
     ((string= choice "✏️ Rename workbench...")
      (org-luhmann-workbench-rename))
     (t
      ;; Extract workbench name from choice
      (let ((name (if (string-match "^\\(?:\\[Current\\] \\)?\\([^ ]+\\)" choice)
                     (match-string 1 choice)
                   choice)))
        (setq org-luhmann-current-workbench name)
        (org-luhmann-workbench-show)
        (message "Switched to workbench: %s" name))))))

(defun org-luhmann-workbench-delete ()
  "Delete a workbench."
  (interactive)
  (let* ((workbench-names (hash-table-keys org-luhmann-workbenches))
         (choices (mapcar (lambda (name) 
                           (format "%s (%d cards)" 
                                  name
                                  (length (gethash name org-luhmann-workbenches))))
                        workbench-names))
         (choice (completing-read "Delete workbench: " choices nil t)))
    
    (when choice
      (let ((name (if (string-match "^\\([^ ]+\\)" choice)
                     (match-string 1 choice)
                   choice)))
        (when (and (not (string= name "default"))
                   (y-or-n-p (format "Delete workbench '%s' with %d cards? " 
                                    name (length (gethash name org-luhmann-workbenches)))))
          (remhash name org-luhmann-workbenches)
          (when (string= name org-luhmann-current-workbench)
            (setq org-luhmann-current-workbench "default"))
          (org-luhmann--workbenches-save)
          (org-luhmann-workbench-show)
          (message "Deleted workbench: %s" name))))))

(defun org-luhmann-workbench-rename ()
  "Rename a workbench."
  (interactive)
  (let* ((workbench-names (hash-table-keys org-luhmann-workbenches))
         (choices (mapcar (lambda (name) 
                           (format "%s (%d cards)" 
                                  name
                                  (length (gethash name org-luhmann-workbenches))))
                        workbench-names))
         (choice (completing-read "Rename workbench: " choices nil t)))
    
    (when choice
      (let* ((old-name (if (string-match "^\\([^ ]+\\)" choice)
                         (match-string 1 choice)
                       choice))
            (new-name (read-string (format "Rename '%s' to: " old-name))))
        (when (and new-name (not (string= new-name "")))
          (let ((cards (gethash old-name org-luhmann-workbenches)))
            (puthash new-name cards org-luhmann-workbenches)
            (remhash old-name org-luhmann-workbenches)
            (when (string= old-name org-luhmann-current-workbench)
              (setq org-luhmann-current-workbench new-name))
            (org-luhmann--workbenches-save)
            (org-luhmann-workbench-show)
            (message "Renamed workbench: %s → %s" old-name new-name)))))))

(defun org-luhmann-workbench-add-subtree ()
  "Add all headings in current subtree as separate cards to workbench."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not at a heading"))
  
  (org-luhmann--ensure-default-workbench)
  (let ((current-cards (org-luhmann--get-current-cards))
        (added-cards 0))
    
    ;; Go through the entire subtree, extract all headings as separate cards
    (save-excursion
      (org-back-to-heading t)
      (let ((subtree-end (save-excursion (org-end-of-subtree t) (point))))
        (while (and (<= (point) subtree-end) (not (eobp)))
          (when (org-at-heading-p)
            (let* ((title (org-get-heading t t t t))
                   (number (when (string-match org-luhmann-number-regex title)
                            (match-string 1 title))))
              (when number
                ;; Extract information for each heading separately (without subheadings)
                (let* ((point (point))
                       (file (buffer-file-name))
                       (level (org-current-level))
                       (content ""))
                  
                  ;; Extract content for the current heading only (without subheadings)
                  (org-end-of-meta-data t)
                  (let ((content-start (point))
                        (content-end (save-excursion 
                                       ;; Find the next sibling or higher-level heading, or the end of the subtree
                                       (let ((current-level (org-current-level)))
                                         (org-forward-heading-same-level 1 t)
                                         (while (and (not (eobp)) 
                                                   (> (org-current-level) current-level)
                                                   (<= (point) subtree-end))
                                           (org-forward-heading-same-level 1 t))
                                         (if (> (point) subtree-end)
                                             subtree-end
                                           (point))))))
                    (setq content (buffer-substring-no-properties content-start content-end))
                    (setq content (replace-regexp-in-string "^[ \t\n]*" "" content))
                    (setq content (replace-regexp-in-string "[ \t\n]*$" "" content))
                    
                    ;; Remove subheadings from content
                    (setq content (replace-regexp-in-string "^\\*+.*$" "" content))
                    (setq content (replace-regexp-in-string "\n\n+" "\n\n" content))
                    (setq content (replace-regexp-in-string "^[ \t\n]*" "" content))
                    (setq content (replace-regexp-in-string "[ \t\n]*$" "" content))
                    
                    (when (> (length content) org-luhmann-workbench-card-content-length)
                      (setq content (concat (substring content 0 org-luhmann-workbench-card-content-length) "..."))))
                  
                  (let ((card-info (list :number number
                                        :title title
                                        :content content
                                        :level level
                                        :file file)))
                    
                    ;; Check if already exists
                    (unless (cl-find-if (lambda (card) 
                                       (and (string= (plist-get card :number) number)
                                            (string= (plist-get card :file) file)))
                                     current-cards)
                      (setq current-cards (cons card-info current-cards))
                      (setq added-cards (1+ added-cards)))))))
          
          ;; Move to the next heading
          (if (outline-next-heading)
              (point)
            (goto-char (point-max))))))
    
    ;; Save and update display
    (when (> added-cards 0)
      (org-luhmann--set-current-cards current-cards)
      (org-luhmann--workbenches-save)
      (message "Added %d cards from subtree to workbench: %s" added-cards org-luhmann-current-workbench)
      
      ;; Automatically refresh workbench display
      (let ((workbench-buffer (get-buffer (format "*Luhmann Workbench: %s*" org-luhmann-current-workbench))))
        (when workbench-buffer
          (org-luhmann-workbench-show)))))))

(defun org-luhmann-workbench-add-heading ()
  "Add only the current heading (without subtree) to current workbench."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not at a heading"))
  
  (org-luhmann--ensure-default-workbench)
  (save-excursion
    (org-back-to-heading t)
    (let* ((title (org-get-heading t t t t))
           (number (when (string-match org-luhmann-number-regex title)
                    (match-string 1 title)))
           (point (point))
           (file (buffer-file-name))
           (level (org-current-level))
           (content ""))
      
      (unless number
        (user-error "Current heading doesn't have a Luhmann number"))
      
      ;; Extract content for the current heading only (without any subheadings)
      (org-end-of-meta-data t)
      (let ((content-start (point))
            (content-end (save-excursion 
                          ;; Find the next heading of any level or the end of the buffer
                           (if (outline-next-heading)
                               (point)
                             (point-max)))))
        (setq content (buffer-substring-no-properties content-start content-end))
        (setq content (replace-regexp-in-string "^[ \t\n]*" "" content))
        (setq content (replace-regexp-in-string "[ \t\n]*$" "" content))
        
        ;; Remove any subheadings
        (setq content (replace-regexp-in-string "^\\*+.*$" "" content))
        (setq content (replace-regexp-in-string "\n\n+" "\n\n" content))
        (setq content (replace-regexp-in-string "^[ \t\n]*" "" content))
        (setq content (replace-regexp-in-string "[ \t\n]*$" "" content))
        
        (when (> (length content) org-luhmann-workbench-card-content-length)
          (setq content (concat (substring content 0 org-luhmann-workbench-card-content-length) "..."))))
      
      (let* ((card-info (list :number number
                              :title title
                              :content content
                              :level level
                              :file file))
             (current-cards (org-luhmann--get-current-cards)))
        
        ;; Check if already exists
        (unless (cl-find-if (lambda (card) 
                           (and (string= (plist-get card :number) number)
                                (string= (plist-get card :file) (plist-get card-info :file))))
                         current-cards)
          (org-luhmann--set-current-cards (cons card-info current-cards))
          (org-luhmann--workbenches-save)
          (message "Added heading only: %s to workbench: %s" number org-luhmann-current-workbench)
          
          ;; Automatically refresh workbench display
          (let ((workbench-buffer (get-buffer (format "*Luhmann Workbench: %s*" org-luhmann-current-workbench))))
            (when workbench-buffer
              (org-luhmann-workbench-show))))))))

(defun org-luhmann-workbench-show ()
  "Display workbench as clean org-mode outline with all cards as top-level headings."
  (interactive)
  (let* ((workbench-name org-luhmann-current-workbench)
         (buffer-name (format "*Luhmann Workbench: %s*" workbench-name))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      
      ;; Configure display settings
      (setq-local org-hide-leading-stars t)
      (setq-local org-startup-folded nil)
      (setq-local org-adapt-indentation nil)
      
      ;; Completely hide the font lock rule for stars
      (font-lock-add-keywords nil
        '(("^\\(\\*+ \\)" (1 '(face nil invisible t)))))
      
      ;; Title
      (insert (format "Workbench: %s (%d cards)\n" 
                     workbench-name 
                     (length (org-luhmann--get-current-cards))))
      (insert (make-string 60 ?═))
      (insert "\n\n")
      
      ;; Render cards
      (let ((cards (org-luhmann--get-current-cards)))
        (if (null cards)
            (insert "Workbench is empty.\n\nUse C-c l w to add cards.")
          (dolist (card cards)
            (let* ((start (point))
                   (title (plist-get card :title))
                   (content (plist-get card :content)))
              
              ;; Force all cards to be displayed as top-level headings, regardless of their original level
              (insert (format "* %s\n" title))
              
              ;; Insert content
              (when (and content (not (string= content "")))
                (insert content)
                (insert "\n"))
              
              (insert "\n")
              
              ;; Store complete card information as text properties
              (put-text-property start (point) 'luhmann-card card)))))
      
      ;; Enable workbench mode
      (org-luhmann-workbench-org-mode 1)
      (font-lock-flush)
      (goto-char (point-min))
      
      ;; Jump to the first card
      (when (org-luhmann--get-current-cards)
        (re-search-forward "^\\* " nil t)
        (beginning-of-line)))
    
    (display-buffer buffer)
    (select-window (get-buffer-window buffer))))

(defun org-luhmann--workbench-render ()
  "Render the workbench content."
  (let ((inhibit-read-only t)
        (current-cards (org-luhmann--get-current-cards)))
    (erase-buffer)
    ;; Header
    (insert (format "═══ Workbench: %s (%d cards) ═══\n\n" 
                   org-luhmann-current-workbench 
                   (length current-cards)))
    
    (if (null current-cards)
        (insert "Workbench is empty.\n\nUse C-c l w on any Luhmann-numbered heading to add cards.")
      (let ((card-num 0))
        (dolist (card current-cards)
          (let ((start (point))
                (title (plist-get card :title))
                (content (plist-get card :content)))
            (insert "────────────────────────────────────────\n")
            (insert (format " %s\n" title))
            (when (and content (not (string= content "")))
              (insert (format " %s\n" content)))
            (insert "────────────────────────────────────────\n")
            (put-text-property start (point) 'card-index card-num)
            (setq card-num (1+ card-num))))))))

(defun org-luhmann--workbench-get-current-card-index ()
  "Get the index of card at point."
  (get-text-property (point) 'card-index))

(defun org-luhmann--workbench-goto-card (index)
  "Move point to card at INDEX."
  (goto-char (point-min))
  (let ((current-index 0))
    (while (and (< current-index index) (not (eobp)))
      (when (get-text-property (point) 'card-index)
        (setq current-index (1+ current-index)))
      (forward-line 1))
    
    ;; Position cursor at the beginning of the card
    (when (and (= current-index index) (not (eobp)))
      (forward-line 1))))

(defun org-luhmann-workbench-move-up ()
  "Move current card up."
  (interactive)
  (let ((index (org-luhmann--workbench-get-current-card-index))
        (current-cards (org-luhmann--get-current-cards)))
    (when (and index (> index 0))
      (let ((card (nth index current-cards)))
        (setq current-cards (delq card current-cards))
        (setq current-cards 
              (append (cl-subseq current-cards 0 (1- index))
                     (list card)
                     (cl-subseq current-cards (1- index))))
        (org-luhmann--set-current-cards current-cards)
        (org-luhmann--workbenches-save)
        (org-luhmann--workbench-render)
        (org-luhmann--workbench-goto-card (1- index))
        (org-luhmann--workbench-highlight-card (1- index))
        (message "Moved card up")))))

(defun org-luhmann-workbench-move-down ()
  "Move current card down."
  (interactive)
  (let ((index (org-luhmann--workbench-get-current-card-index))
        (current-cards (org-luhmann--get-current-cards)))
    (when (and index (< index (1- (length current-cards))))
      (let ((card (nth index current-cards)))
        (setq current-cards (delq card current-cards))
        (setq current-cards 
              (append (cl-subseq current-cards 0 (1+ index))
                     (list card)
                     (cl-subseq current-cards (1+ index))))
        (org-luhmann--set-current-cards current-cards)
        (org-luhmann--workbenches-save)
        (org-luhmann--workbench-render)
        (org-luhmann--workbench-goto-card (1+ index))
        (org-luhmann--workbench-highlight-card (1+ index))
        (message "Moved card down")))))

(defun org-luhmann--workbench-highlight-card (index)
  "Briefly highlight card at INDEX."
  (save-excursion
    (goto-char (point-min))
    (let ((card-start nil)
          (card-end nil)
          (current-index 0))
      (while (and (< current-index index) (not (eobp)))
        (when (get-text-property (point) 'card-index)
          (setq current-index (1+ current-index)))
        (forward-line 1))
      
      (when (= current-index index)
        (setq card-start (point))
        (while (and (get-text-property (point) 'card-index) (not (eobp)))
          (forward-line 1))
        (setq card-end (point))
        
        (when (and card-start card-end)
          (let ((overlay (make-overlay card-start card-end)))
            (overlay-put overlay 'face '(:background "yellow" :foreground "black"))
            (run-with-timer 0.8 nil (lambda () (delete-overlay overlay)))))))))

(defun org-luhmann-workbench-remove-card ()
  "Remove current card from workbench."
  (interactive)
  (let ((card (org-luhmann-workbench-get-current-card)))
    (if card
        (when (y-or-n-p (format "Remove card: %s? " (plist-get card :title)))
          (let ((number (plist-get card :number))
                (file (plist-get card :file))
                (current-cards (org-luhmann--get-current-cards)))
            ;; Remove from data structure
            (setq current-cards 
                  (cl-remove-if (lambda (c) 
                                 (and (string= (plist-get c :number) number)
                                      (string= (plist-get c :file) file)))
                               current-cards))
            (org-luhmann--set-current-cards current-cards)
            (org-luhmann--workbenches-save)
            
            ;; Remove from display
            (org-cut-subtree)
            (message "Removed card: %s" (plist-get card :title))))
      (user-error "Not on a workbench card"))))

(defun org-luhmann-workbench-save-order ()
  "Save current card order to workbench."
  (interactive)
  (let ((new-order '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (let ((card (get-text-property (point) 'luhmann-card)))
          (when card
            (push card new-order)))))
    
    (when new-order
      (org-luhmann--set-current-cards (nreverse new-order))
      (org-luhmann--workbenches-save)
      (message "Saved card order for workbench: %s" org-luhmann-current-workbench))))

(defun org-luhmann-workbench-get-current-card ()
  "Get card info at current heading."
  (when (org-at-heading-p)
    (get-text-property (point) 'luhmann-card)))

;; FIXME: Jump-to-original is temporarily disabled because using :point to record position is unsafe
;; When the original file content changes, the recorded :point may point to the wrong position
;;
;; (defun org-luhmann-workbench-jump-to-original ()
;;   "Jump to original location of current card."
;;   (interactive)
;;   (let ((card (org-luhmann-workbench-get-current-card)))
;;     (if card
;;         (let ((file (plist-get card :file))
;;               (point (plist-get card :point)))
;;           (when (and file point (file-exists-p file))
;;             (find-file file)
;;             (goto-char point)
;;             (org-fold-show-context)
;;             (message "Jumped to: %s" (plist-get card :title))))
;;       (user-error "Not on a workbench card"))))
;;
;; (defun org-luhmann-workbench-jump-to-card ()
;;   "Jump to the original location of current card."
;;   (interactive)
;;   (let ((index (org-luhmann--workbench-get-current-card-index))
;;         (current-cards (org-luhmann--get-current-cards)))
;;     (when index
;;       (let* ((card (nth index current-cards))
;;             (file (plist-get card :file))
;;             (point (plist-get card :point)))
;;         (when (and file point)
;;           (find-file file)
;;           (goto-char point)
;;           (org-fold-show-context))))))

(defun org-luhmann-workbench-clear ()
  "Clear all cards from current workbench."
  (interactive)
  (when (y-or-n-p (format "Clear all cards from workbench '%s'? " org-luhmann-current-workbench))
    (org-luhmann--set-current-cards nil)
    (org-luhmann--workbenches-save)
    (org-luhmann--workbench-render)
    (message "Workbench '%s' cleared" org-luhmann-current-workbench)))

(defun org-luhmann-workbench-next-card ()
  "Move to next card in workbench."
  (interactive)
  (let ((current-index (org-luhmann--workbench-get-current-card-index))
        (current-cards (org-luhmann--get-current-cards)))
    (if (and current-index (< current-index (1- (length current-cards))))
        (org-luhmann--workbench-goto-card (1+ current-index))
      (message "No next card"))))

(defun org-luhmann-workbench-previous-card ()
  "Move to previous card in workbench."
  (interactive)
  (let ((current-index (org-luhmann--workbench-get-current-card-index)))
    (if (and current-index (> current-index 0))
        (org-luhmann--workbench-goto-card (1- current-index))
      (message "No previous card"))))

;; Save/Load functions
(defun org-luhmann--workbenches-save ()
  "Save all workbenches state to file."
  (with-temp-buffer
    (insert ";; -*- lexical-binding: t -*-\n")
    (insert (format ";; Org-Luhmann Workbenches State - %s\n" (current-time-string)))
    (insert (format "(setq org-luhmann-workbenches '%S)\n" org-luhmann-workbenches))
    (insert (format "(setq org-luhmann-current-workbench %S)\n" org-luhmann-current-workbench))
    (write-file org-luhmann-workbench-save-file)))

(defun org-luhmann--workbenches-load ()
  "Load all workbenches state from file."
  (when (file-exists-p org-luhmann-workbench-save-file)
    (load-file org-luhmann-workbench-save-file)
    (org-luhmann--ensure-default-workbench)))

(define-derived-mode org-luhmann-workbench-mode special-mode "Luhmann-Workbench"
  "Major mode for Luhmann workbench."
  (setq buffer-read-only t)
  (setq truncate-lines t))

;; Workbench org-mode
(defvar org-luhmann-workbench-org-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)

    (define-key map (kbd "M-<up>") 'org-move-subtree-up)
    (define-key map (kbd "M-<down>") 'org-move-subtree-down)
    (define-key map (kbd "M-p") 'org-move-subtree-up)
    (define-key map (kbd "M-n") 'org-move-subtree-down)
    
    ;; Workbench specific functions
    ;; FIXME: Jump-to-original is temporarily disabled because using :point to record position is unsafe
    ;; (define-key map (kbd "RET") 'org-luhmann-workbench-jump-to-original)
    ;; (define-key map (kbd "C-c C-j") 'org-luhmann-workbench-jump-to-original)
    (define-key map (kbd "C-c C-k") 'org-luhmann-workbench-remove-card)
    (define-key map (kbd "C-c C-s") 'org-luhmann-workbench-save-order)
    (define-key map (kbd "C-c C-c") 'org-luhmann-workbench-clear)
    (define-key map (kbd "g") 'org-luhmann-workbench-show)
    
    ;; Navigation
    (define-key map (kbd "n") 'org-next-visible-heading)
    (define-key map (kbd "p") 'org-previous-visible-heading)
    map)
  "Keymap for org-luhmann workbench in org-mode.")

(define-minor-mode org-luhmann-workbench-org-mode
  "Minor mode for Luhmann workbench in org-mode."
  :lighter " Luhmann Workbench"
  :keymap org-luhmann-workbench-org-mode-map
  (when org-luhmann-workbench-org-mode
    (add-hook 'org-after-refile-insert-hook 'org-luhmann-workbench-save-order nil t)))

(provide 'org-luhmann)
