(in-package :cl-orgelctl)

(setf *route-presets*
#((:preset 0 :routes nil)
  (:preset nil :routes (:orgel01 (:level (n-bias-cos :bias-pos :bias-bw))))
  (:preset nil :routes
           (:orgel01
            (:bias-pos (ccin 0) :bias-bw (ccin 1) :level
             (apply-notch :bias-type (bias-cos :bias-pos :bias-bw)))))
  (:preset nil :routes
           (:orgel01
            (:level (apply-notch :bias-type (bias-wippe :bias-pos :bias-bw)))))
  (:preset nil :routes
           (:orgel01
            (:bias-pos (ccin 0) :bias-bw (ccin 1) :level
             (apply-notch :bias-type
                          (bias-cos :bias-pos :bias-bw :levels
                                    #(0.5 1 0.5 1 0 1 0 1 0 1 0 1 0 1 0 1))))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (bias-cos :bias-pos :bias-bw :levels
                                    #(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0))))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (bias-wippe :bias-pos :bias-bw :levels
                                      #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (bias-wippe :bias-pos :bias-bw :levels
                                      #(0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5
                                        1 0.5 1))))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (bias-wippe :bias-pos :bias-bw :levels
                                      #(0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5
                                        1 0.5 1)))
             :delay
             (apply-notch :bias-type
                          (bias-cos :bias-pos :bias-bw :levels
                                    #(1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5
                                      1 0.5))))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (permute (bias-cos :bias-pos :bias-bw)
                                   #(1 16 2 15 3 14 4 13 5 12 6 11 7 10 8
                                     9))))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (permute (bias-cos :bias-pos :bias-bw)
                                   #(1 3 5 7 9 11 13 15 16 14 12 10 8 6 4
                                     2))))))
  (:preset nil :routes
           (:orgel01
            (:bias-pos (ccin 0) :bias-bw (ccin 1) :global
             ((apply-notch :bias-type
                           (bias-cos :bias-pos :bias-bw :targets
                                     *global-targets*))
              *global-targets*))))
  (:preset nil :routes (:orgel01 (:level (n-bias-cos (ccin 0) (ccin 1)))))
  (:preset nil :routes
           (:orgel01
            (:bias-pos (ccin 0) :bias-bw (ccin 1) :level
             (n-bias-cos :bias-pos :bias-bw))))
  (:preset nil :routes
           (:orgel01
            (:bias-pos (ccin 0) :bias-bw (ccin 1) :level
             (apply-notch :bias-type (n-bias-cos :bias-pos :bias-bw)))))
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))