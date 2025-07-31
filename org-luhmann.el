;;; org-luhmann.el --- Luhmann numbering system for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Maintainer: Yibie <yibie@outlook.com>
;; URL: https://github.com/yibie/org-luhmann
;; Version: 0.3.0
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



;;;###autoload
(define-minor-mode org-luhmann-mode
  "Minor mode for Luhmann numbering system in org-mode."
  :lighter " Luhmann"
  :group 'org-luhmann
  (if org-luhmann-mode
      (message "Org-Luhmann mode enabled")
    (message "Org-Luhmann mode disabled")))

;;;###autoload
(defun org-luhmann-setup ()
  "Setup org-luhmann."
  (interactive)
  (org-luhmann-mode 1)
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
           (non-z (substring letter 0 (- len (match-length 0)))))
      (if (string= non-z "")
          (make-string (1+ len) ?a)
        (concat (substring non-z 0 -1)
                (char-to-string (1+ (aref non-z (1- (length non-z)))))
                (make-string (match-length 0) ?a)))))
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

(defun org-luhmann--get-number-options (context &optional command patterns)
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

(defun org-luhmann--generate-number (option context &optional patterns)
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
  
  (let* ((context (org-luhmann--get-current-context))
         (patterns (org-luhmann--analyze-numbering-patterns))
         (title (org-get-heading t t t t))
         (has-number (org-luhmann--parse-luhmann-number title))
         (current-level (org-outline-level))
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
  "Add a new node with Luhmann number using enhanced logic.
After selecting numbering option, directly insert the heading and 
position cursor after the number for immediate text input."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Must be on a heading"))
  
  (let* ((context (org-luhmann--get-current-context))
         (patterns (org-luhmann--analyze-numbering-patterns))
         (options (org-luhmann--get-number-options context 'add-node patterns))
         (choice (completing-read "Select numbering option: "
                                 (mapcar #'car options) nil t))
         (option (assoc choice options))
         (generated-number (org-luhmann--generate-number option context patterns)))
    
    (when generated-number
      (let* ((option-type (cadr option))
             (current-level (org-outline-level))
             (new-level (pcase option-type
                         ('letter-sequence current-level)
                         ('branch-current (1+ current-level))
                         ('branch-parent current-level)
                         ('main 1)
                         (_ current-level)))
             (stars (make-string new-level ?*))
             (new-heading-line (concat stars " " generated-number org-luhmann-title-separator)))
        
        (save-excursion
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
        
        ;; Move cursor to the end of the new heading for immediate text input
        (let ((search-pattern (concat "^" (regexp-quote stars) " " 
                                     (regexp-quote generated-number) 
                                     (regexp-quote org-luhmann-title-separator))))
          (when (re-search-forward search-pattern nil t)
            (goto-char (match-end 0))
            (message "Type heading text: %s" generated-number)))
        
        generated-number))))

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
          (org-show-context)
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
          (org-show-context)
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
  (add-to-list 'savehist-additional-variables 'org-luhmann-export-history))

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
- 'star: Show traditional org-mode stars (default)
- 'number: Show only Luhmann numbers"
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
;; Key Bindings and Setup
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
    map)
  "Keymap for org-luhmann-mode.")

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

(provide 'org-luhmann)
