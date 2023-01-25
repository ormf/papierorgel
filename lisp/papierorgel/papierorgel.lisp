;;;; Scratchpad für papierorgel.lisp

(unless (find-package :cl-orgelctl) (ql:quickload "papierorgel"))
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
	   :optimize t))

(browser-play cl)

(reconnect-midi)

*midi-in1*

(add-ats-cc-responder 2 (lambda (val) (setf ats-cuda::*bw* (* 10000 (expt 1/1000 val)))))
(remove-ats-channel-cc-responders 5)

(remove-ats-channel-cc-responders 5)

(cl-orgelctl::remove-channel-cc-responders 5)
(setf (incudine::logger-level) :warn)

*midi-in1*
(in-package #:cl-orgelctl)


(defparameter *global-targets* nil)

(setf *global-targets* '((level 1 1) (level 1 2) (level 2 1) (level 2 2) (level 1 3)
                         (level 1 5) (level 1 4) (level 2 11) (level 2 13) (level 2 3)
                         (level 1 15) (level 1 12) (level 2 7) (level 1 6) (level 2 5) (level 1 11)))

(setf *global-targets* '((level 3 1) (level 1 2) (level 1 1) (level 2 5) (level 1 3)
                         (level 1 5) (level 1 4) (level 2 11) (level 2 13) (level 2 3)
                         (level 3 15) (level 2 12) (level 2 7) (level 1 6) (level 2 5) (level 1 11)))


(setf (level 1 1) 0.2)


(switch-targets '((level 3 1) (level 1 2) (level 1 1) (level 2 5) (level 1 3)
                  (level 1 5) (level 1 4) (level 2 11) (level 2 13) (level 2 3)
                  (level 3 15) (level 2 12) (level 2 7) (level 1 6) (level 2 5) (level 1 11))
                :trigger '(bias-pos 1))

(switch-targets '((level 1 1) (level 1 2) (level 2 1) (level 2 2) (level 1 3)
                  (level 1 5) (level 1 4) (level 2 11) (level 2 13) (level 2 3)
                  (level 1 15) (level 1 12) (level 2 7) (level 1 6) (level 2 5) (level 1 11))
                :trigger '(bias-pos 1))


(get-trig-fns '(bias-pos 1))




(digest-route :orgel01 (:global ((bias-cos-n :bias-pos :bias-bw :targets *global-targets*)
                                 *global-targets*))
              nil)

(untrace)
(set-global-faders *global-targets* (n-bias-cos (bias-pos 1) (bias-bw 1) :targets *global-targets*))


(replace-keywords '(bias-cos :bias-pos :bias-bw :targets *global-targets*)
                  (orgel-nr :orgel01))

(defun set-levels ())



(let ((test `(level 1 1)))
  (setf (first test) 0.7))

(orgel-ctl-fader :orgel01 'level 2 0.8)

(gethash :orgel01 *orgeltargets*
         )
(set-faders :orgel01 :level (bias-wippe (bias-pos 1) (bias-bw 1)))

(funcall (bias-wippe (bias-pos 1) (bias-bw 1)) 0.4)

(bias-cos)

(/ 15.0 16)

(bias-bw :orgel01)

(set-faders :orgel01 :level)

(let ((fn (n-bias-cos 7/15 0.5)))
  (loop for x from 0 to 1 by 1/15 collect (funcall fn x)))

(defparameter *targets* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(setf *targets* '(0 1 2 3 4 5 6 7))



*orgel-osc-responder*

(incf (base-freq :orgel01) 103)
