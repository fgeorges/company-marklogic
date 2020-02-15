;;; company-marklogic.el --- Company backend for MarkLogic functions.

;; ************************************************************************ ;;
;;  File:       company-marklogic.el                                        ;;
;;  Author:     F. Georges - fgeorges.org - h2o.consulting                  ;;
;;  Date:       2018-12-25                                                  ;;
;;  Tags:                                                                   ;;
;;      Copyright (c) 2018, 2019 Florent Georges (see end of file.)         ;;
;; ------------------------------------------------------------------------ ;;
;;
;;; Package info:
;;
;; Package-Requires: ((emacs "24.4") (company "0.9"))
;; Keywords: company marklogic completion xquery javascript
;; Homepage: https://github.com/fgeorges/company-marklogic
;; Version: 1.0
;;
;;; Commentary:
;;
;; Provide a Company backend for MarkLogic XQuery and JavaScript functions.

;;; Code:

(require 'company)
(require 'company-marklogic-sjs)
(require 'company-marklogic-xqy)

;; TODO: Set from the longest existing function name, instead of a magic number.
(defconst company-marklogic-max-prefix-length 100)

(defconst company-marklogic-prefix-re
  (regexp-opt
   (mapcar (lambda (x) (symbol-name (car x))) company-marklogic-sjs-functions)))

(defconst company-marklogic-sjs-prefix-re
  (concat "^\\(" company-marklogic-prefix-re "\\)\\.[^.]*"))

(defconst company-marklogic-xqy-prefix-re
  (concat "^\\(" company-marklogic-prefix-re "\\):[^:]*"))

(defun company-marklogic-sjs (command &optional arg &rest ignored)
  "Company backend for MarkLogic JavaScript functions.

COMMAND, ARG and IGNORED are the arguments received from Company."
  (interactive (list 'interactive))
  (case command
    (interactive     (company-begin-backend 'company-marklogic-sjs-backend))
    (prefix          (when (eq major-mode 'js-mode)
                       (company-marklogic-sjs-prefix)))
    (candidates      (company-marklogic-sjs-candidates arg))
    (post-completion (insert "()")
                     (backward-char))))

(defun company-marklogic-sjs-prefix ()
  "The prefix command for the MarkLogic JavaScript backend."
  (save-excursion
    (let ((end   (point))
          (start (point-min))
          (limit (min company-marklogic-max-prefix-length (point-min))))
      (when (re-search-backward "[^.a-zA-Z0-9]" limit t)
        (setq start (1+ (point))))
      (let ((res (buffer-substring start end)))
        (when (and (>= (length res) 3)
                   (or (string-prefix-p res "require")
                       (string-prefix-p res "declareUpdate")
                       (string-match company-marklogic-sjs-prefix-re res)))
          res)))))

(defun company-marklogic-sjs-candidates (prefix)
  "The candidates command for the MarkLogic JavaScript backend.

PREFIX is the argument received from Company, for the command `candidates'."
  (let* ((tokens (split-string prefix "\\."))
         (first  (car tokens))
         (second (car (cdr tokens))))
    (cond ((= (length tokens) 1)
           (cond ((string-prefix-p first "require")       '("require"))
                 ((string-prefix-p first "declareUpdate") '("declareUpdate"))))
          ((= (length tokens) 2)
           (seq-filter
            (lambda (name) (string-prefix-p prefix name))
            (cdr (assoc (intern first) company-marklogic-sjs-functions)))))))

(defun company-marklogic-xqy (command &optional arg &rest ignored)
  "Company backend for MarkLogic XQuery functions.

COMMAND, ARG and IGNORED are the arguments received from Company."
  (interactive (list 'interactive))
  (case command
    (interactive     (company-begin-backend 'company-marklogic-xqy-backend))
    (prefix          (when (eq major-mode 'xquery-mode)
                       (company-marklogic-xqy-prefix)))
    (candidates      (company-marklogic-xqy-candidates arg))
    (post-completion (insert "()")
                     (backward-char))))

(defun company-marklogic-xqy-prefix ()
  "The prefix command for the MarkLogic XQuery backend."
  (save-excursion
    (let ((end   (point))
          (start (point-min))
          (limit (min company-marklogic-max-prefix-length (point-min))))
      (when (re-search-backward "[^-:a-zA-Z0-9]" limit t)
        (setq start (1+ (point))))
      (let ((res (buffer-substring start end)))
        (when (string-match company-marklogic-xqy-prefix-re res)
          res)))))

(defun company-marklogic-xqy-candidates (prefix)
  "The candidates command for the MarkLogic XQuery backend.

PREFIX is the argument received from Company, for the command `candidates'."
  (let* ((tokens (split-string prefix ":"))
         (first  (car tokens))
         (second (car (cdr tokens))))
    (when (= (length tokens) 2)
      (seq-filter
       (lambda (name) (string-prefix-p prefix name))
       (cdr (assoc (intern first) company-marklogic-xqy-functions))))))

(provide 'company-marklogic)

;; ------------------------------------------------------------------------ ;;
;;  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS COMMENT.               ;;
;;                                                                          ;;
;;  The contents of this file are subject to the Apache License Version     ;;
;;  2.0 (the "License"); you may not use this file except in compliance     ;;
;;  with the License. You may obtain a copy of the License at               ;;
;;  http://www.apache.org/licenses/.                                        ;;
;;                                                                          ;;
;;  Software distributed under the License is distributed on an "AS IS"     ;;
;;  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See    ;;
;;  the License for the specific language governing rights and limitations  ;;
;;  under the License.                                                      ;;
;;                                                                          ;;
;;  The Original Code is: all this file.                                    ;;
;;                                                                          ;;
;;  The Initial Developer of the Original Code is Florent Georges.          ;;
;;                                                                          ;;
;;  Contributor(s): none.                                                   ;;
;; ------------------------------------------------------------------------ ;;

;;; company-marklogic.el ends here
