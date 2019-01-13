;;; company-ml.el --- Company backend for MarkLogic functions.

;; ************************************************************************ ;;
;;  File:       company-ml.el                                               ;;
;;  Author:     F. Georges - fgeorges.org - h2o.consulting                  ;;
;;  Date:       2018-12-25                                                  ;;
;;  Tags:                                                                   ;;
;;      Copyright (c) 2018, 2019 Florent Georges (see end of file.)         ;;
;; ------------------------------------------------------------------------ ;;
;;
;;; Package info:
;;
;; Package-Requires: ((company "0.9"))
;; Keywords: company marklogic completion xquery javascript
;; Homepage: https://github.com/fgeorges/company-ml
;;
;;; Commentary:
;;
;; Provide a Company backend for MarkLogic XQuery and Javascript functions.

;;; Code:

(require 'company)
(require 'company-ml-sjs)
(require 'company-ml-xqy)

;; TODO: Set from the longest existing function name, instead of a magic number.
(defconst company-ml-max-prefix-length 100)

(defconst company-ml-prefix-re
  (string-join
   (mapcar (lambda (x) (symbol-name (car x))) company-ml-sjs-functions)
   "\\|"))

(defconst company-ml-sjs-prefix-re
  (concat "^\\(" company-ml-prefix-re "\\)\\.[^.]*"))

(defconst company-ml-xqy-prefix-re
  (concat "^\\(" company-ml-prefix-re "\\):[^:]*"))

(defun company-ml-sjs (command &optional arg &rest ignored)
  "Company backend for MarkLogic Javascript functions."
  (interactive (list 'interactive))
  (case command
    (interactive     (company-begin-backend 'company-ml-sjs-backend))
    (prefix          (when (eq major-mode 'js-mode)
                       (company-ml-sjs-prefix)))
    (candidates      (company-ml-sjs-candidates arg))
    (post-completion (insert "()")
                     (backward-char))))

(defun company-ml-sjs-prefix ()
  "The prefix command for the MarkLogic Javascript backend."
  (save-excursion
    (let ((end   (point))
          (start (point-min))
          (limit (min company-ml-max-prefix-length (point-min))))
      (when (re-search-backward "[^.a-zA-Z0-9]" limit t)
        (setq start (1+ (point))))
      (let ((res (buffer-substring start end)))
        (when (and (>= (length res) 3)
                   (or (string-prefix-p res "require")
                       (string-prefix-p res "declareUpdate")
                       (string-match company-ml-sjs-prefix-re res)))
          res)))))

(defun company-ml-sjs-candidates (prefix)
  "The candidates command for the MarkLogic Javascript backend."
  (let* ((tokens (split-string prefix "\\."))
         (first  (car tokens))
         (second (car (cdr tokens))))
    (cond ((= (length tokens) 1)
           (cond ((string-prefix-p first "require")       '("require"))
                 ((string-prefix-p first "declareUpdate") '("declareUpdate"))))
          ((= (length tokens) 2)
           (seq-filter
            (lambda (name) (string-prefix-p prefix name))
            (cdr (assoc (intern first) company-ml-sjs-functions)))))))

(defun company-ml-xqy (command &optional arg &rest ignored)
  "Company backend for MarkLogic XQuery functions."
  (interactive (list 'interactive))
  (case command
    (interactive     (company-begin-backend 'company-ml-xqy-backend))
    (prefix          (when (eq major-mode 'xquery-mode)
                       (company-ml-xqy-prefix)))
    (candidates      (company-ml-xqy-candidates arg))
    (post-completion (insert "()")
                     (backward-char))))

(defun company-ml-xqy-prefix ()
  "The prefix command for the MarkLogic XQuery backend."
  (save-excursion
    (let ((end   (point))
          (start (point-min))
          (limit (min company-ml-max-prefix-length (point-min))))
      (when (re-search-backward "[^-:a-zA-Z0-9]" limit t)
        (setq start (1+ (point))))
      (let ((res (buffer-substring start end)))
        (when (string-match company-ml-xqy-prefix-re res)
          res)))))

(defun company-ml-xqy-candidates (prefix)
  "The candidates command for the MarkLogic XQuery backend."
  (let* ((tokens (split-string prefix ":"))
         (first  (car tokens))
         (second (car (cdr tokens))))
    (when (= (length tokens) 2)
      (seq-filter
       (lambda (name) (string-prefix-p prefix name))
       (cdr (assoc (intern first) company-ml-xqy-functions))))))

(provide 'company-ml)


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
