(in-package :cl-orgelctl)

;;; preset: 9

(digest-route-preset
 10
 `(:preset nil
   :routes (:orgel01
            (:level
             (apply-notch :bias-type
                          (permute (bias-cos :bias-pos :bias-bw)
                                   #(1 3 5 7 9 11 13 15 16 14 12 10 8 6 4 2)))))))

;;; (save-route-presets)
