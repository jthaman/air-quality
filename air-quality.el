;;; air-quality.el --- Air quality modeline indicator and reporting tool  -*- lexical-binding: t; -*-


;;; Package Imports
(require 'url)
(require 'json)
(require 'cl-lib)


;;; Commentary:
;; Add information about local air quality to the modeline. Air Quality
;; information is downloaded from the Open Weather Map Air Pollution API.


;;; Code:


;;; Public Variables
(defvar air-quality-open-weather-api-key nil
  "A string. API key for Open Weather Map.")

(defvar air-quality-refresh-interval 60
  "An integer. Number of minutes between refreshes of air quality information.")

(defvar air-quality-latitude nil
  "A float. Your latitude.")

(defvar air-quality-longitude nil
  "A float. Your longitude.")


;;; Private Variables
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

(defun air-quality--make-forecast-call (key lat lon)
  "Create an API request for the future forecast of Air Quality information from Open Weather Map."
  (concat "http://api.openweathermap.org/data/2.5/air_pollution/forecast?lat="
          (number-to-string lat)
          "&lon="
          (number-to-string lon)
          "&appid="
          key))

(defun air-quality--make-api-call (key lat lon)
  "Create an API request to Open Weather Map."
  (concat "http://api.openweathermap.org/data/2.5/air_pollution?lat="
          (number-to-string lat)
          "&lon="
          (number-to-string lon)
          "&appid="
          key))

(defvar air-quality--index-alist '((1 . "Good")
                                   (2 . "Fair")
                                   (3 . "Moderate")
                                   (4 . "Poor")
                                   (5 . "Very Poor")))


(defun air-quality--get-update ()
  "Query Open Weather for air quality information."
  (url-retrieve
   (air-quality--make-api-call air-quality-open-weather-api-key
                               air-quality-latitude
                               air-quality-longitude)
   (lambda (events)
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
         (setq air-quality--level (cadadr (aref (plist-get result 'list) 0)))
         (air-quality--set-indicator)
         (run-with-idle-timer 1 nil #'air-quality--append-modeline)
         )))))

(defun air-quality--set-indicator ()
  (defvar-local air-quality-indicator
      (list (propertize (concat "  " (alist-get air-quality--level air-quality--index-alist))
                        'face 'mode-line-buffer-id
                        'help-echo (purecopy (format "Carbon Monoxide: %d µg/m³
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
                                                     air-quality--nh3))
                        'mouse-face 'mode-line-highlight
                        )))
  (put 'air-quality-indicator 'risky-local-variable t))


(defun air-quality--append-modeline ()
  (unless (cl-find '(:eval air-quality-indicator) mode-line-format :test 'equal)
    (setq-default mode-line-format
                  (reverse (append '((:eval air-quality-indicator)) (reverse mode-line-format))))))

;;;###autoload
(defun air-quality-setup-modeline ()

  ;; This could be better...
  ;; if no timer, start one
  (if (null air-quality--timer)
      (setq air-quality--timer
            (run-with-timer 0 (* 60 air-quality-refresh-interval) #'air-quality--get-update))
    ;; otherwise, cancel the timer and start one
    (progn
      (cancel-timer air-quality--timer)
      (setq air-quality--timer
            (run-with-timer 0 (* 60 air-quality-refresh-interval) #'air-quality--get-update))
      )))


(provide 'air-quality)

;;; air_quality.el ends here
