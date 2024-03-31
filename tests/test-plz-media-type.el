;;; test-plz-media-type.el --- Plz media type tests -*- lexical-binding: t; -*-

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

;; Plz media type tests

;;; Code:

(require 'ert)
(require 'plz-media-type)
(require 'plz-media-type-test)

(plz-deftest test-plz-media-type-request:application/json:async ()
  (let* ((response)
         (process (plz-media-type-request 'get (url "/json")
                    :as `(media-types ,plz-media-types)
                    :then (lambda (object)
                            (setf response object)))))
    (plz-media-type-test-wait process)
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (let-alist (plz-response-body response)
      (should (equal "Sample Slide Show" .slideshow.title)))))

(plz-deftest test-plz-media-type-request:application/json:sync ()
  (let ((response (plz-media-type-request 'get (url "/json")
                    :as `(media-types ,plz-media-types))))
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (let-alist (plz-response-body response)
      (should (equal "Sample Slide Show" .slideshow.title)))))

(ert-deftest test-plz-media-type-request:application/json:sync-error ()
  (plz-media-type-test-with-mock-response (plz-media-type-test-response "application/json/vertext-unauthenticated.txt")
    (let* ((result (condition-case error
                       (plz-media-type-request 'get "MOCK-URL"
                         :as `(media-types ((application/json . ,(plz-media-type:application/json)))))
                     (plz-error error))))
      (should (equal 'plz-http-error (car result)))
      (should (equal "HTTP error" (cadr result)))
      (let ((error (caddr result)))
        (should (plz-error-p error))
        (let ((response (plz-error-response error)))
          (should (plz-response-p response))
          (should (equal 401 (plz-response-status response)))
          (should (equal '(code . 401) (cadar (elt (plz-response-body response) 0)))))))))

(plz-deftest test-plz-media-type-request:text/html:async ()
  (let* ((response)
         (process (plz-media-type-request 'get (url "/html")
                    :as `(media-types ,plz-media-types)
                    :then (lambda (object)
                            (setf response object)))))
    (plz-media-type-test-wait process)
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (should (equal 'html (car (plz-response-body response))))))

(plz-deftest test-plz-media-type-request:text/html:sync ()
  (let ((response (plz-media-type-request 'get (url "/html")
                    :as `(media-types ,plz-media-types))))
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (should (equal 'html (car (plz-response-body response))))))

(plz-deftest test-plz-media-type-request:application/xml:async ()
  (let* ((response)
         (process (plz-media-type-request 'get (url "/xml")
                    :as `(media-types ,plz-media-types)
                    :then (lambda (object)
                            (setf response object)))))
    (plz-media-type-test-wait process)
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (should (equal 'top (car (plz-response-body response))))))

(plz-deftest test-plz-media-type-request:application/xml:sync ()
  (let ((response (plz-media-type-request 'get (url "/xml")
                    :as `(media-types ,plz-media-types))))
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (should (equal 'top (car (plz-response-body response))))))

(ert-deftest test-plz-media-type-parse ()
  (should (null (plz-media-type-parse nil)))
  (should (null (plz-media-type-parse "")))
  (should (equal (plz-media-type :type 'text :subtype 'html)
                 (plz-media-type-parse "text/html")))
  (should (equal (plz-media-type
                  :type 'text
                  :subtype 'html
                  :parameters '(("charset" . "UTF-8")))
                 (plz-media-type-parse "text/html;charset=UTF-8")))
  (should (equal (plz-media-type
                  :type 'text
                  :subtype 'html
                  :parameters '(("charset" . "UTF-8")
                                ("boundary" . "AaB03x\"")))
                 (plz-media-type-parse "text/html; charset=UTF-8; boundary=\"AaB03x\""))))

(ert-deftest test-plz-media-type-charset ()
  (let ((media-type (plz-media-type-parse "text/html; charset=UTF-8")))
    (should (equal "UTF-8" (plz-media-type-charset media-type)))))

(ert-deftest test-plz-media-type-coding-system ()
  (let ((media-type (plz-media-type-parse "text/html")))
    (should (equal 'utf-8 (plz-media-type-coding-system media-type))))
  (let ((media-type (plz-media-type-parse "text/html; charset=UTF-8")))
    (should (equal 'utf-8 (plz-media-type-coding-system media-type)))))

(ert-deftest test-plz-media-type-symbol ()
  (let ((media-type (plz-media-type-parse "text/html")))
    (should (equal 'text/html (plz-media-type-symbol media-type)))))

(ert-deftest test-plz-media-type-process-filter-error-sync ()
  (plz-media-type-test-with-mock-response (plz-media-type-test-response "application/x-ndjson/ollama-hello.txt")
    (let ((result (condition-case error
                      (plz-media-type-request 'get "MOCK-URL"
                        :as `(media-types ((application/x-ndjson
                                            . ,(plz-media-type:application/x-ndjson
                                                :handler (lambda (_) (signal 'error "boom")))))))
                    (plz-error error))))
      (should (equal 'plz-media-type-filter-error (elt result 0)))
      (should (equal "error in process filter: (error . \"boom\")" (elt result 1)))
      (let ((response (elt result 2)))
        (should (plz-response-p response))
        (should (equal 200 (plz-response-status response)))
        (should (null (plz-response-body response))))
      (should (equal '(error . "boom") (elt result 3))))))

(ert-deftest test-plz-media-type-process-filter-error-async ()
  (plz-media-type-test-with-mock-response (plz-media-type-test-response "application/x-ndjson/ollama-hello.txt")
    (let* ((else) (finally) (then)
           (process (plz-media-type-request 'get "MOCK-URL"
                      :as `(media-types ((application/x-ndjson
                                          . ,(plz-media-type:application/x-ndjson
                                              :handler (lambda (_) (signal 'error "boom"))))))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-media-type-test-wait process)
      (should (equal '(t) finally))
      (should (equal 0 (length then)))
      (should (equal 1 (length else)))
      (seq-doseq (error else)
        (should (plz-error-p error))
        (should (plz-media-type-filter-error-p error))
        (should (equal "error in process filter: (error . \"boom\")" (plz-error-message error)))
        (should (equal '(error . "boom") (plz-media-type-filter-error-cause error)))
        (let ((response (plz-error-response error)))
          (should (plz-response-p response))
          (should (equal 200 (plz-response-status response)))
          (should (null (plz-response-body response))))))))

(ert-deftest test-plz-media-type-request:application/octet-stream:stream ()
  (plz-media-type-test-with-mock-response (plz-media-type-test-response "text/event-stream/openai-hello.txt")
    (let* ((else) (finally) (then)
           (process (plz-media-type-request 'post "https://api.openai.com/v1/chat/completions"
                      :as `(media-types `((t . ,(plz-media-type:application/octet-stream))))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-media-type-test-wait process)
      (should (null else))
      (should (equal '(t) finally))
      (should (equal 1 (length then)))
      (seq-doseq (response then)
        (should (plz-response-p response))
        (should (equal 200 (plz-response-status response)))
        (should (string-match "[DONE]" (plz-response-body response)))))))

(ert-deftest test-plz-media-type-request:application/x-ndjson:async ()
  (let* ((else) (finally) (then) (objects)
         (process (plz-media-type-request 'get "http://localhost/stream/5"
                    :as `(media-types ((application/json
                                        . ,(plz-media-type:application/x-ndjson
                                            :handler (lambda (object)
                                                       (push object objects))))))
                    :else (lambda (object) (push object else))
                    :finally (lambda () (push t finally))
                    :then (lambda (object) (push object then)))))
    (plz-media-type-test-wait process)
    (should (null else))
    (should (equal '(t) finally))
    (should (equal 1 (length then)))
    (seq-doseq (response then)
      (should (plz-response-p response))
      (should (equal 200 (plz-response-status response)))
      (should (null (plz-response-body response))))
    (should (equal 5 (length objects)))
    (should (equal '(0 1 2 3 4)
                   (seq-map (lambda (object) (alist-get 'id object))
                            (reverse objects))))))

(ert-deftest test-plz-media-type-request:application/x-ndjson:sync ()
  (let* ((objects)
         (response (plz-media-type-request 'get "http://localhost/stream/5"
                     :as `(media-types ((application/json
                                         . ,(plz-media-type:application/x-ndjson
                                             :handler (lambda (object)
                                                        (push object objects)))))))))
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (should (null (plz-response-body response)))
    (should (equal 5 (length objects)))
    (should (equal '(0 1 2 3 4)
                   (seq-map (lambda (object) (alist-get 'id object))
                            (reverse objects))))))

(ert-deftest test-plz-media-type-request:application/x-ndjson:ollama ()
  (plz-media-type-test-with-mock-response (plz-media-type-test-response "application/x-ndjson/ollama-hello.txt")
    (let* ((else) (finally) (then) (objects)
           (process (plz-media-type-request 'get "MOCK-URL"
                      :as `(media-types ((application/x-ndjson
                                          . ,(plz-media-type:application/x-ndjson
                                              :handler (lambda (object)
                                                         (push object objects))))))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-media-type-test-wait process)
      (should (null else))
      (should (equal '(t) finally))
      (should (equal 1 (length then)))
      (seq-doseq (response then)
        (should (plz-response-p response))
        (should (equal 200 (plz-response-status response)))
        (should (null (plz-response-body response))))
      (should (equal 27 (length objects)))
      (should (equal '((model . "llama2")
                       (created_at . "2024-03-12T12:05:13.747334659Z")
                       (response . "Hello")
                       (done . :json-false))
                     (seq-elt objects 26)))
      ;; TODO: Fix parsing of last line :/
      (should (equal '((model . "llama2")
                       (created_at . "2024-03-12T12:05:15.467785437Z")
                       (response . "?")
                       (done . :json-false))
                     (seq-elt objects 1))))))

(ert-deftest test-plz-media-type-request:application/json-array:async ()
  (plz-media-type-test-with-mock-response (plz-media-type-test-response "application/json/vertex-hello.txt")
    (let* ((else) (finally) (then) (objects)
           (process (plz-media-type-request 'get "MOCK-URL"
                      :as `(media-types ((application/json
                                          . ,(plz-media-type:application/json-array
                                              :handler (lambda (object)
                                                         (push object objects))))))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-media-type-test-wait process)
      (should (null else))
      (should (equal '(t) finally))
      (should (equal 1 (length then)))
      (seq-doseq (response then)
        (should (plz-response-p response))
        (should (equal 200 (plz-response-status response)))
        (should (null (plz-response-body response))))
      (should (equal 2 (length objects)))
      (should (equal '("Hi there!" " How can I assist you today?")
                     (plz-media-type-test-vertex-extract-content objects))))))

(ert-deftest test-plz-media-type-request:application/json-array:async-poem ()
  (plz-media-type-test-with-mock-response (plz-media-type-test-response "application/json/vertex-poem.txt")
    (let* ((else) (finally) (then) (objects)
           (process (plz-media-type-request 'get "MOCK-URL"
                      :as `(media-types ((application/json
                                          . ,(plz-media-type:application/json-array
                                              :handler (lambda (object)
                                                         (push object objects))))))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-media-type-test-wait process)
      (should (null else))
      (should (equal '(t) finally))
      (should (equal 1 (length then)))
      (seq-doseq (response then)
        (should (plz-response-p response))
        (should (equal 200 (plz-response-status response)))
        (should (null (plz-response-body response))))
      (should (equal 7 (length objects)))
      (let ((parts (plz-media-type-test-vertex-extract-content objects)))
        (should (string-match "**Ode to Emacs, Master of Code" (cl-first parts)))
        (should (equal '(" the sorcerer, dispels all dooms.") (last parts)))))))

(ert-deftest test-plz-media-type-request:application/json-array:sync ()
  (plz-media-type-test-with-mock-response (plz-media-type-test-response "application/json/vertex-hello.txt")
    (let* ((objects)
           (response (plz-media-type-request 'get "MOCK-URL"
                       :as `(media-types ((application/json
                                           . ,(plz-media-type:application/json-array
                                               :handler (lambda (object)
                                                          (push object objects)))))))))
      (should (plz-response-p response))
      (should (equal 200 (plz-response-status response)))
      (should (null (plz-response-body response)))
      (should (equal 2 (length objects)))
      (should (equal '("Hi there!" " How can I assist you today?")
                     (plz-media-type-test-vertex-extract-content objects))))))

(ert-deftest test-plz-media-type-request:application/json-array:async-error ()
  (plz-media-type-test-with-mock-response (plz-media-type-test-response "application/json/vertext-unauthenticated.txt")
    (let* ((else) (finally) (then) (objects)
           (process (plz-media-type-request 'get "MOCK-URL"
                      :as `(media-types ((application/json
                                          . ,(plz-media-type:application/json-array
                                              :handler (lambda (object)
                                                         (push object objects))))))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-media-type-test-wait process)
      (should (equal '(t) finally))
      (should (null then))
      (should (equal 1 (length else)))
      (seq-doseq (error else)
        (should (plz-error-p error))
        (let ((response (plz-error-response error)))
          (should (plz-response-p response))
          (should (equal 401 (plz-response-status response)))
          (should (null (plz-response-body response)))))
      (should (equal 1 (length objects)))
      (should (equal '(code . 401) (cadaar objects))))))

(ert-deftest test-plz-media-type-request:application/json-array:sync-error ()
  (plz-media-type-test-with-mock-response (plz-media-type-test-response "application/json/vertext-unauthenticated.txt")
    (let* ((objects)
           (result (condition-case error
                       (plz-media-type-request 'get "MOCK-URL"
                         :as `(media-types ((application/json
                                             . ,(plz-media-type:application/json-array
                                                 :handler (lambda (object)
                                                            (push object objects)))))))
                     (plz-error error))))
      (should (equal 'plz-http-error (car result)))
      (should (equal "HTTP error" (cadr result)))
      (let ((error (caddr result)))
        (should (plz-error-p error))
        (let ((response (plz-error-response error)))
          (should (plz-response-p response))
          (should (equal 401 (plz-response-status response)))
          (should (null (plz-response-body response)))))
      (should (equal 1 (length objects)))
      (should (equal '(code . 401) (cadaar objects))))))

(plz-deftest test-plz-media-type-request-timeout-sync ()
  (pcase-let* ((start-time (current-time))
               (`(,_signal . (,_message ,(cl-struct plz-error (curl-error `(,code . ,message)))))
		(should-error (plz-media-type-request 'get (url "/delay/5")
				:as `(media-types ,plz-media-types)
                                :then 'sync
                                :timeout 1)
			      :type 'plz-error))
               (end-time (current-time)))
    (should (eq 28 code))
    (should (equal "Operation timeout." message))
    (should (< (time-to-seconds (time-subtract end-time start-time)) 1.1))))

(plz-deftest test-plz-media-type-request-timeout-async ()
  (let* ((start-time (current-time))
         (end-time)
         (plz-error)
         (process (plz-media-type-request 'get (url "/delay/5")
                    :as `(media-types ,plz-media-types)
                    :timeout 1
                    :then #'ignore
                    :else (lambda (e)
                            (setf end-time (current-time)
                                  plz-error e)))))
    (plz-media-type-test-wait process)
    (should (eq 28 (car (plz-error-curl-error plz-error))))
    (should (equal "Operation timeout." (cdr (plz-error-curl-error plz-error))))
    (should (< (time-to-seconds (time-subtract end-time start-time)) 1.1))))

(ert-deftest test-plz-media-type-request-resolve-error-async ()
  (let* ((else) (finally) (then)
         (process (plz-media-type-request 'get "https://httpbinnnnnn.org/get/status/404"
                    :as `(media-types ,plz-media-types)
                    :else (lambda (object) (push object else))
                    :finally (lambda () (push t finally))
                    :then (lambda (object) (push object then)))))
    (plz-media-type-test-wait process)
    (should (equal '(t) finally))
    (should (equal 0 (length then)))
    (should (equal 1 (length else)))
    (seq-doseq (error else)
      (should (plz-error-p error))
      (should (null (plz-error-message error)))
      (should (null (plz-error-response error)))
      (equal '(6 . "Couldn't resolve host. The given remote host was not resolved.")
             (plz-error-curl-error error)))))

(ert-deftest test-plz-media-type-request-resolve-error-sync ()
  (let* ((result (condition-case error
                     (plz-media-type-request 'get "https://httpbinnnnnn.org/get/status/404"
                       :as `(media-types ,plz-media-types))
                   (plz-error error))))
    (should (equal 'plz-curl-error (car result)))
    (should (equal "Curl error" (cadr result)))
    (let ((error (caddr result)))
      (should (plz-error-p error))
      (should (null (plz-error-message error)))
      (should (null (plz-error-response error)))
      (equal '(6 . "Couldn't resolve host. The given remote host was not resolved.")
             (plz-error-curl-error error)))))

;;;; Footer

(provide 'test-plz-media-type)

;;; test-plz-media-type.el ends here
