;;; 
;;; ats-cuda-test.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(unless (find-package :ats-cuda) (ql:quickload "ats-cuda"))
(in-package :ats-cuda)

(progn
  (defvar cl nil)
  (defvar crt-cs6 nil)
  (defvar said nil)
  (defvar barock nil)

  (tracker "clarinet.aif"
	   'cl
	   :start 0.0
	   :hop-size 1/4
	   :lowest-frequency 100.0
	   :highest-frequency 20000.0
	   :frequency-deviation 0.05
	   :lowest-magnitude (db-amp -70)
	   :SMR-continuity 0.7
	   :track-length 6
	   :min-segment-length 3
	   :residual "/tmp/cl-res.snd"
	   :verbose nil
	   :debug nil)

  (tracker "crt-cs6.snd" 
	   'crt-cs6
	   :start 0.1
	   :lowest-frequency 500.0
	   :highest-frequency 20000.0
	   :frequency-deviation 0.15
	   :window-cycles 4
	   :window-type 'blackman-harris-4-1
	   :hop-size 1/8
	   :lowest-magnitude (db-amp -90)
	   :amp-threshold -80
	   :track-length 6
	   :min-segment-length 3
	   :last-peak-contribution 0.5
	   :SMR-continuity 0.3
	   :residual "/tmp/crt-cs6-res.snd"
	   :verbose nil
	   :debug nil
	   :optimize t)

  (tracker
   "/home/orm/work/unterricht/frankfurt/ws_22_23/musikinformatik/lisp/snd/said.wav"
   'said
   :start 0.0
   :hop-size 1/4
   :lowest-frequency 100.0
   :highest-frequency 20000.0
   :frequency-deviation 0.1
   :lowest-magnitude (db-amp -30)
   :SMR-continuity 0.7
   :track-length 6
   :min-segment-length 3
   :residual "/tmp/said-res.snd"
   :verbose nil
   :debug nil)

  (tracker
   "/home/orm/work/unterricht/frankfurt/ws_22_23/musikinformatik/lisp/snd/barock.wav" 
   'barock
   :start 0.0
   :hop-size 1/4
   :lowest-frequency 100.0
   :highest-frequency 20000.0
   :frequency-deviation 0.1
   :lowest-magnitude (db-amp -30)
   :SMR-continuity 0.7
   :track-length 6
   :min-segment-length 3
   :residual "/tmp/barock-res.snd"
   :verbose nil
   :debug nil))

(rt-start)

(sin-synth 0.0 cl :duration 10)
(ats->svg cl :brightness 200)

(setf *bw* 10)

(remove-all-ats-cc-responders)

(setf (incudine::logger-level) :info)
(setf (incudine::logger-level) :warn)


(add-ats-cc-responder 2 (lambda (val) (setf *bw* (* 10000 (expt 1/1000 val)))))
incudine::*responders*

(browser-play cl :amp-scale 1 :bw 10)
(browser-play barock :amp-scale 0.1 :res-bal 0.5 :bw 10000)
(browser-play said :amp-scale 0.1 :res-bal 0.5)
(browser-play crt-cs6 :brightness 500 :bw 1000 :amp-scale 1 :res-bal 0.5)

(browser-play "/tmp/said.ats" :brightness 500 :amp-scale 0.01 :bw 40 :res-bal 0.5)

(defun browser-play (ats-sound &rest args)
  (let ((ats-snd (symbol-value
                  (if (or (stringp ats-sound) (typep ats-sound 'pathname))
                      (ats-load ats-sound (intern (string-upcase (pathname-name (pathname ats-sound))) :ats-cuda))
                      ats-sound))))
    (let* ((bw (getf args :bw 40000))
           (x (getf args :soundpos 0))
           (y (getf args :y 0))
           (maxfreq
             (float (+ 100
                       (aref (ats-sound-frq-av ats-snd)
                             (1- (ats-sound-partials ats-snd))))
                    1.0))
           (mousefreq (* (max 0.0 (min y 1.0)) maxfreq)))
      (setf *curr-sound* ats-snd)
      ;; (ats->svg ats-snd :brightness (getf args :brightness 20))
      ;; (broadcast-message "reload")
      ;; (remf args :brightness)
      ;; (remf args :bw)
      ;; (free 2)
      ;; (let ((num-partials (ats-sound-partials ats-snd)))
      ;;   (setf *amod* (incudine::sample-array num-partials :initial-element 1.0d0))
      ;;   (setf *fmod* (incudine::sample-array num-partials :initial-element 1.0d0)))
      ;; (setf *bw* bw)
      ;; (loop for partial below (ats-sound-partials *curr-sound*)
      ;;       for freq = (aref (ats-sound-frq *curr-sound*) partial
      ;;                        (round (* (min 1 (max 0 x)) (1- (ats-sound-frames *curr-sound*)))))
      ;;       do (setf (aref *amod* partial) (get-amp freq mousefreq *bw*)))
      ;; (apply #'incudine::sin-noi-rtc-synth 0.0 ats-snd :amod *amod* :fmod *fmod* :id 2 args)
      )))


(defun browser-play (ats-sound &rest args)
  (let* ((ats-snd (if (or (stringp ats-sound) (typep ats-sound 'pathname))
                      (ats-load ats-sound (intern (string-upcase (pathname-name (pathname ats-sound))) :ats-cuda))
                      ats-sound))
)
ats-snd
    ))

(ats-load "/tmp/said.ats"
          (intern (string-upcase (pathname-name (pathname "/tmp/said.ats"))) :ats-cuda))

(ats-save said "/tmp/said.ats")






(set-control 2 :soundpos 0.2)
(set-control 2 :soundpos 0.5)
(set-control 2 :res-bal 0)


(incudine::sin-noi-rtc-synth)

(free 2)



(fix-freqs barock)
(fix-freqs said)
(defun equal-temperament (fundamental-frequency n)
  (let ((partials (make-array n :initial-element nil)))
    (dotimes (i n)
      (setf (aref partials i) (* fundamental-frequency (expt 2 (/ i 12)))))
    partials))

(equal-temperament 666 16)
#(666 705.6025 747.5597 792.0119 839.1074 889.0033 941.8662 997.87256 1057.2091
  1120.0741 1186.6771 1257.2406 1332 1411.205 1495.1194 1584.0238)



(destructuring-bind (frm frame startfrq endfrq frm2)
    '(783 783 116.82269995511736d0 116.82269995511736d0 784)
  (+ startfrq (* (- endfrq startfrq) (/ (- frm (1- frame)) (- frm2 (1- frame)))))
  )

(let ((ats-sound barock))
  (loop for partial below (ats-sound-partials ats-sound)
        collect (aref (ats-sound-frq ats-sound) partial 0)))

(ats-sound-frames barock)
