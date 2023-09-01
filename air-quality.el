;;; air-quality.el --- Air quality modeline indicator and reporting tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 John Haman

;; Author: John Haman <mail@johnhaman.org>
;; URL: https://github.com/jthaman/air-quality
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience

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

;; Add information about local air quality to the modeline. Air Quality
;; information is downloaded from the Open Weather Map Air Pollution API.

;;; Code:

;;;; Package Imports

(require 'url)
(require 'json)
(require 'cl-lib)

;;;; User options

(defgroup air-quality nil
  "Air quality mode-line indicator."
  :group 'mode-line)

(defcustom air-quality-open-weather-api-key nil
  "API key for Open Weather Map."
  :type '(choice (const  :tag "Unset" nil)
                 (string :tag "Your API key")))

(defvar air-quality-refresh-interval 60
  ;; XXX - turn into a defcustom
  "An integer. Number of minutes between refreshes of air quality information.")

(defvar air-quality-latitude nil
  ;; XXX - likewise
  "A float. Your latitude.")

(defvar air-quality-longitude nil
  ;; XXX - likewise
  "A float. Your longitude.")

;;;; Private Variables

(defvar air-quality--timer nil)

(defvar air-quality--co nil "Carbon Monoxide level (micrograms per cubic-meter).")
(defvar air-quality--no nil "Nitrogen Oxide level (micrograms per cubic-meter).")
(defvar air-quality--no2 nil "Nitrogen Dioxide level (micrograms per cubic-meter). ")
(defvar air-quality--o3 nil "Ozone (micrograms per cubic-meter).")
(defvar air-quality--so2 nil "Sulfur Dioxide (micrograms per cubic-meter).")
(defvar air-quality--pm2_5 nil "PM 2.5 level (micrograms per cubic-meter).")
(defvar air-quality--pm10 nil "PM 10 level (micrograms per cubic-meter).")
(defvar air-quality--nh3 nil "Ammonia level (micrograms per cubic-meter).")
(defvar air-quality--level nil "Overall Air Quality (AQI).")

(defun air-quality--make-api-call (key latitude longitude)
  "Return Open Weather Map URL for LATITUDE and LONGITUDE with API key KEY."
  (url-parse-make-urlobj
   "https" nil nil "api.openweathermap.org" nil
   (concat "/data/2.5/air_pollution/?"
           (url-build-query-string `(("lat" ,latitude)
                                     ("lon" ,longitude)
                                     ("appid" ,key))))))

(defconst air-quality--index-alist '((1 . "Good")
                                     (2 . "Fair")
                                     (3 . "Moderate")
                                     (4 . "Poor")
                                     (5 . "Very Poor")))

(defvar air-quality-indicator
  '(:eval
    (propertize (concat "  " (alist-get air-quality--level
                                        air-quality--index-alist))
                'face 'mode-line-buffer-id
                'help-echo (format "Carbon Monoxide: %d µg/m³
Nitrogen Oxide: %d µg/m³
Nitrogen Dioxide: %d µg/m³
Ozone: %d µg/m³
Sulfur Dioxide: %d µg/m³
PM 2.5: %d µg/m³
PM 10: %d µg/m³
Ammonia: %d µg/m³"
                                   air-quality--co
                                   air-quality--no
                                   air-quality--no2
                                   air-quality--o3
                                   air-quality--so2
                                   air-quality--pm2_5
                                   air-quality--pm10
                                   air-quality--nh3)
                'mouse-face 'mode-line-highlight)))
(put 'air-quality-indicator 'risky-local-variable t)

(defun air-quality--get-update ()
  "Query Open Weather for air quality information."
  (url-retrieve
   (air-quality--make-api-call air-quality-open-weather-api-key
                               air-quality-latitude
                               air-quality-longitude)
   (lambda (_events)
     (goto-char url-http-end-of-headers)
     (let ((json-object-type 'plist)
           (json-key-type 'symbol)
           (json-array-type 'vector))
       (let* ((result (json-read))
              (components (cadddr (aref (plist-get result 'list) 0))))
         (setq air-quality--co (plist-get components 'co))
         (setq air-quality--no (plist-get components 'no))
         (setq air-quality--no2 (plist-get components 'no2))
         (setq air-quality--o3 (plist-get components 'o3))
         (setq air-quality--so2 (plist-get components 'so2))
         (setq air-quality--pm2_5 (plist-get components 'pm2_5))
         (setq air-quality--pm10 (plist-get components 'pm10))
         (setq air-quality--nh3 (plist-get components 'nh3))
         (setq air-quality--level (cadadr (aref (plist-get result 'list) 0))))))))

;;;###autoload
(define-minor-mode air-quality-mode
  "Minor mode for displaying air quality information in the mode line."
  :group 'air-quality
  :global t
  (if air-quality-mode
      (progn
        ;; XXX - check that air-quality-open-weather-api-key,
        ;; air-quality-latitude and air-quality-longitude are set
        (add-to-list 'mode-line-misc-info 'air-quality-indicator t )
        (setq air-quality--timer
              (run-with-timer 0 (* 60 air-quality-refresh-interval)
                              #'air-quality--get-update)))
    (setq mode-line-misc-info (delq 'air-quality-indicator mode-line-misc-info))
    (cancel-timer air-quality--timer))
  (force-mode-line-update))

(provide 'air-quality)
;;; air-quality.el ends here
