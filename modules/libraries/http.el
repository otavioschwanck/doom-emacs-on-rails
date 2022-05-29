;;; rails-http-status.el --- Insert rails status      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Otávio Schwanck dos Santos

;; Author: Otávio Schwanck dos Santos <otavioschwanck@gmail.com>
;; Keywords: rails, status, api

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

;; Rails status

;;; Code:

(setq rails-http-statuses '(("100 - continue")
                            ("101 - switching_protocols")
                            ("102 - processing")
                            ("200 - ok")
                            ("201 - created")
                            ("202 - accepted")
                            ("203 - non_authoritative_information")
                            ("204 - no_content")
                            ("205 - reset_content")
                            ("206 - partial_content")
                            ("207 - multi_status")
                            ("226 - im_used")
                            ("300 - multiple_choices")
                            ("301 - moved_permanently")
                            ("302 - found")
                            ("303 - see_other")
                            ("304 - not_modified")
                            ("305 - use_proxy")
                            ("307 - temporary_redirect")
                            ("400 - bad_request")
                            ("401 - unauthorized")
                            ("402 - payment_required")
                            ("403 - forbidden")
                            ("404 - not_found")
                            ("405 - method_not_allowed")
                            ("406 - not_acceptable")
                            ("407 - proxy_authentication_required")
                            ("408 - request_timeout")
                            ("409 - conflict")
                            ("410 - gone")
                            ("411 - length_required")
                            ("412 - precondition_failed")
                            ("413 - request_entity_too_large")
                            ("414 - request_uri_too_long")
                            ("415 - unsupported_media_type")
                            ("416 - requested_range_not_satisfiable")
                            ("417 - expectation_failed")
                            ("422 - unprocessable_entity")
                            ("423 - locked")
                            ("424 - failed_dependency")
                            ("426 - upgrade_required")
                            ("500 - internal_server_error")
                            ("501 - not_implemented")
                            ("502 - bad_gateway")
                            ("503 - service_unavailable")
                            ("504 - gateway_timeout")
                            ("505 - http_version_not_supported")
                            ("507 - insufficient_storage")
                            ("510 - not_extended")))

(defun rails-http-status--insert-by (val)
  (let ((route (completing-read "Select the status: " rails-http-statuses)))
    (insert (concat (when (eq val 1) ":") (nth val (split-string route " - "))))))

(defun rails-http-statuses-insert-code ()
  (interactive)
  (rails-http-status--insert-by 0))

(defun rails-http-statuses-insert-symbol ()
  (interactive)
  (rails-http-status--insert-by 1))

(provide 'rails-http-status)
;;; rails-http-status.el ends here
