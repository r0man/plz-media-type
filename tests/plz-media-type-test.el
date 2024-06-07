;;; plz-media-type-test.el --- Test helpers for plz media type -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>

;; This file is part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Test helpers for plz media type.

;;; Code:

;;;; Requirements

(require 'ert)
(require 'json)
(require 'let-alist)
(require 'map)

(require 'plz)

;;;; Variables

(defvar plz-media-type-test-uri-prefix
  "http://localhost"
  "URI prefix for HTTP requests, without trailing slash.
If running httpbin locally, set to \"http://localhost\".")

;;;; Customization


;;;; Commands


;;;; Macros

(cl-defun plz-media-type-test-wait (process &optional (seconds 0.1) (times 100))
  "Wait for SECONDS seconds TIMES times for PROCESS to finish."
  (when process
    ;; Sometimes it seems that the process is killed, the THEN
    ;; function called by its sentinel, and its buffer killed, all
    ;; before this function gets called with the process argument;
    ;; when that happens, tests that use this can fail.  Testing
    ;; whether PROCESS is non-nil seems to fix it, but it's possible
    ;; that something funny is going on...
    (cl-loop for i upto times ;; 10 seconds
             while (equal 'run (process-status process))
             do (sleep-for seconds))))

(cl-defmacro plz-deftest (name () &body docstring-keys-and-body)
  "Like `ert-deftest', but defines tests for both HTTP/1.1 and HTTP/2.
Also defines local function `url' which returns its argument
appended to `plz-media-type-test-uri-prefix' (and any instance of
\"URI-PREFIX\" in URL-PART is replaced with `plz-media-type-test-uri-prefix'
in URL-encoded form)."
  (declare (debug (&define [&name "test@" symbolp]
			   sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  `(progn
     ,@(cl-loop for http-version in '("1.1" "2")
                collect (let ((name (intern (format "%s-http%s" name http-version))))
                          `(ert-deftest ,name ()
                             (let ((plz-curl-default-args
                                    ',(append plz-curl-default-args (list (format "--http%s" http-version)))))
                               (cl-labels ((url (part)
                                             (setf part (replace-regexp-in-string
                                                         "URI-PREFIX" (url-hexify-string plz-media-type-test-uri-prefix)
                                                         part t t))
                                             (concat plz-media-type-test-uri-prefix part)))
                                 ,@docstring-keys-and-body)))))))

(defun plz-media-type-test-make-mock-program (response-file &optional mock-file)
  "Make a shell script that emit a curl response.

RESPONSE-FILE is the file to read the response from.

MOCK-FILE is the file to write the mock program to.  If nil, a
temporary filename is used."
  (let ((mock-file (or mock-file (make-temp-file "plz-media-type-test-mock"))))
    (with-temp-buffer
      (insert "#!/usr/bin/env bash")
      (newline)
      (insert "cat " (expand-file-name response-file) " | pv --quiet --rate-limit 10000")
      (newline)
      (make-directory (file-name-directory mock-file) t)
      (write-region (point-min) (point-max) mock-file)
      (chmod mock-file #o755)
      mock-file)))

(defun plz-media-type-test-save-mock-response (buffer filename)
  "Write the plz HTTP response in BUFFER to FILENAME."
  (with-current-buffer buffer
    (widen)
    (make-directory (file-name-directory filename) t)
    (write-region (point-min) (point-max) filename)
    filename))

(cl-defun plz-media-type-test-save-request
    (method
     url
     &rest rest
     &key headers body filename noquery timeout
     (body-type 'text)
     (connect-timeout plz-connect-timeout)
     (decode t decode-s))
  (let ((filename (expand-file-name (or filename (make-temp-name "plz-media-type")))))
    (plz method url
      :as 'buffer
      :body body
      :body-type body-type
      :connect-timeout connect-timeout
      :decode decode
      :else (lambda (_)
              (plz-media-type-test-save-mock-response (current-buffer) filename))
      :headers headers
      :noquery noquery
      :timeout timeout
      :then (lambda (_)
              (plz-media-type-test-save-mock-response (current-buffer) filename)))))

(defmacro plz-media-type-test-with-mock-response (filename &rest body)
  "Evaluate BODY with a mocked HTTP response from FILENAME."
  (declare (indent 1) (debug (body)))
  `(let ((plz-curl-program (plz-media-type-test-make-mock-program ,filename)))
     ,@body))

(defun plz-media-type-test-response (example)
  "Return the HTTP test response filename for EXAMPLE."
  (if-let (file (locate-dominating-file "." ".git"))
      (let ((filename (expand-file-name (concat file "tests/response/" example))))
        (if (file-exists-p filename)
            filename
          (error "No such HTTP response file: %s" filename)))
    (error "Can't locate dominating plz.el file")))

(defun plz-media-type-test-openai-extract-content (events)
  "Extract the content of the OpenAI EVENTS."
  (thread-last
    (reverse events)
    (seq-map (lambda (event)
               (with-slots (data) event
                 (unless (equal "[DONE]" data)
                   (when-let ((data (json-parse-string data))
                              (choice (seq-first (map-elt data "choices")))
                              (delta (map-elt choice "delta"))
                              (content (map-elt delta "content")))
                     content)))))
    (seq-remove #'null)
    (string-join)))

(defun plz-media-type-test-vertex-extract-content (objects)
  "Extract the content of the Vertext OBJECTS."
  (thread-last
    objects
    (seq-map (lambda (object)
               (seq-mapcat (lambda (candidate)
                             (alist-get 'parts (cdar candidate)))
                           (alist-get 'candidates object))))
    (seq-map (lambda (part)
               (alist-get 'text (car part))))
    (reverse)))

;;;; Footer

(provide 'plz-media-type-test)

;;; plz-media-type-test.el ends here
