;;; 
;;; init.lisp
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

(in-package :cl-orgelctl)

(cm:cd "/home/orm/work/unterricht/frankfurt/ws_22_23/musikinformatik/papierorgel/lisp/")
(load-presets "./presets/route-presets.lisp")
(load-presets "./presets/presets.lisp")

*curr-state*

(orgel-ctl :orgel01 :level10 0.2)

*orgel-osc-responder*

(permute )
(digest-route-preset)

(register-responders)

(defparameter *global-targets* nil)

(setf *global-targets* '((level 1 1) (level 1 2) (level 2 1) (level 2 2) (level 1 3)
                         (level 1 5) (level 1 4) (level 2 11) (level 2 13) (level 2 3)
                         (level 1 15) (level 1 12) (level 2 7) (level 1 6) (level 2 5) (level 1 11)))

(setf *global-targets* '((level 3 1) (level 1 2) (level 1 1) (level 2 5) (level 1 3)
                         (level 1 5) (level 1 4) (level 2 11) (level 2 13) (level 2 3)
                         (level 3 15) (level 2 12) (level 2 7) (level 1 6) (level 2 5) (level 1 11)))





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
