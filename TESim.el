
(defun raise-tick1 ()
  (insert (concat "raise-tick1  in " (int-to-string tick) "
") )
  )

(defun raise-tick2 ()
  (insert (concat "raise-tick2  in " (int-to-string tick) "
") )

  )
(defun raise-tick3 ()
  (insert (concat "raise-tick3  in " (int-to-string tick) "
") )
  )
(defun raise-tick4 ()
  (insert (concat "raise-tick4  in " (int-to-string tick) "
") )
  )
(defun raise-tick5 ()
  (insert (concat "raise-tick5  in " (int-to-string tick) "
") )
  )
(defun raise-tick6 ()
  (insert (concat "raise-tick6  in " (int-to-string tick) "
") )
  )

(defun raise-fps ()
  (hook-raiser-fps)
  (insert (concat "FPS  in " (int-to-string tick) "
")
          )
  )

(defun hook-raiser-fps ()
  (cond
   ((equal mode "stopping") (if (is-before-hyperPeriod? 3) (s5-simulator-stop) ))
   ((equal mode "updating") (if (is-before-hyperPeriod? 3) (s5-simulator-apply) ))
   )
  )

(defun simulator-stop ()
  (insert "--> Apply STOP <---")
  )

(defun simulator-apply ()
  (insert "--> Apply Configuration <--")
  )



;; COMMANDS

(defun execute-command (command-list)
  (let

      ((valid-commands (remove-if-not (lambda (x)  (eq  (car x) tick) ) command-list)))
    (mapcar (lambda (x) (cond
                         ((equal "start" (nth 1 x)) (start-command) )
                         ((equal "stop" (nth 1 x)) (stop-command))
                         ((equal "update" (nth 1 x)) (update-command))
                         ((equal "replay" (nth 1 x)) (replay-command))
                         ))
            valid-commands)
    )
  )


(defun start-command ()
  (insert (concat "Start Command
" (int-to-string tick)))
  (setq mode "start")
  )

(defun stop-command ()
  (insert (format "STOP command: \n"))
  (if (validStopTick?)
      (progn (setq mode "stop") (insert "STATUS STOP"))
    (progn  (setq mode "stopping") (insert "STATUS STOPPING"))
    )
  )

(defun update-command ()
  (insert (format "UPDATE command: \n"))
  (if (validUpdateTick?)
      (progn (setq mode "update") (insert "STATUS UPDATE"))
    (progn  (setq mode "updating") (insert "STATUS UPDATING"))
    )
  )

(defun replay-command ()
  (insert (format "replay command received"))
  (setq mode "replay")
  )


;; HYPER PERIOD VERIFICATION
(defun validStopTick? ()
  (is-before-hyperPeriod? 3)
  )

(defun validUpdateTick? ()
  (is-before-hyperPeriod? 3)
  )


(defun is-before-hyperPeriod? (period-before)
  (eq (-   (apply #'cl-lcm
                               (mapcar (lambda (x) (car x)) ticks-table)
                               )
           period-before)
      tick)
  )



(defun sim-measurements-trigger ()
  (let
      (
       (trigger-elements (cl-remove-if-not
                          (lambda (x)
                            (and (> tick (nth 2 x))
                                         (equal  (% tick (car x) )  0)))
                                           ticks-table))
       )
    (mapcar (lambda  (x)
              (funcall (nth 1 x)))
            trigger-elements)
    )
  )


(defun set-meassurement-tick ()
  (let
      (
       (next-get-tick (apply #'min (mapcar (lambda (x) (if (and (> tick (car x))
                                                                (/= tick (car x)))
                                                           (- tick (car x))
                                                         (if (/= tick (car x))
                                                             (- (car x) tick)))
                                             )
                                           ticks-table)))
       (next-get-command (apply #'min (mapcar (lambda (x) (if (and
                                                               (> tick (car x))
                                                               (/= tick (car x)))
                                                              (- tick (car x))
                                                            (if (/= tick (car x))
                                                                (- (car x) tick))))
                                              command-list)))
       (remaining-fss (- fss  (% tick fss)))
       )
    (setq tick (+ tick (min next-get-tick next-get-command remaining-fss)))
    )
  )

(defun TESim ()
  (interactive "")
  (let
      (
       (mode "INIT")
       (fss? nil)
       (tick 0)
       (ticks-table '((6 raise-fps 0) (8 raise-tick1 0) (16 raise-tick2 0) (32 raise-tick3 0) (64 raise-tick5 0) (128 raise-tick6 0) (6 raise-tick6 256 0) ))
       (command-list '((10 "start") (265 "stop") (600 "start") (875 "update")))
       (synch-enabled? )
       (master-tick 20)
       (shift-tick 0)
       (fss 2500)
       (simulation-max 2000)
       )
    (switch-to-buffer "*SIM-RESULTS*")
    (while (< tick simulation-max)
      (progn
        (sim-measurements-trigger)
        (execute-command command-list)
        (when (and fss? synch-enabled?)
          (synch-frame-synch)
          )
        (set-meassurement-tick ))
      ))

  )



