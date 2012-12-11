;;Author: Addisu Taddese (addisu@gmail.com)
;;GTP engine for othello based on edax's and Grhino's gtp clients. This
;;implementation is specifically tailored towards CS260's othello implementation.
;;

;; START CHANGE
;; Load the appropriate othello implementation file
(load "othello.lisp")
;; Change this to your package
(in-package :group5)
;; END CHANGE

(use-package :common-lisp)

(defvar *version* "0.3")
(defvar *known-commands* '(
                           ("protocol_version"  gtp-cmd-protocol-version)
                           ("name"              gtp-cmd-name)
                           ("version"           gtp-cmd-version)
                           ("known_command"     gtp-cmd-known-command)
                           ("list_commands"     gtp-cmd-list-commands)
                           ("quit"              gtp-empty-resp)
                           ("boardsize"         gtp-cmd-boardsize)
                           ("clear_board"       gtp-cmd-clear-board)
                           ("komi"              gtp-empty-resp)
                           ("play"              gtp-cmd-play)
                           ("genmove"           gtp-cmd-genmove)
                           ("set_game"          gtp-cmd-set-game)
                           ("list_games"        gtp-cmd-list-games)
                           ("final_score"       gtp-cmd-final-score)
                           ("cputime"           gtp-cmd-cputime)
                           ("showboard"         gtp-cmd-showboard)
                           ("time_settings"     gtp-empty-resp) ;; TODO
                           ("time_left"         gtp-empty-resp) ;; TODO
                           ))
;Unimplemented commands
;"undo"
;"loadsgf"
;"reg_genmove"
;"showboard"
;"time_settings"
;"time_left"

; Set to nil to disable logging
(defparameter *logging* T)
(defvar *log-file* "gtp.log")

(defvar *strategy* 'randomstrategy)
(defvar *board* (initialboard))
(defvar *cpu-time* 0)

; Handle command line. This may not work on your implementation properly
(defun my-command-line ()
  (or
   #+CLISP ext:*args*
   #+SBCL (rest sb-ext:*posix-argv*)
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun legal-char? (chr)
  "Determine if chr is a legal character. According to the GTP protocol, the
   legal characters are all letters, numbers, space, newine and tab"
  (or (graphic-char-p chr)
      (char= chr #\newline)
      (char= chr #\tab) (char= chr #\space)))

(defun replace-tabs (str)
  "Replace tabs with spaces"
  (with-output-to-string (stream)
    (dolist (chr (coerce str 'list))
      (if (char= chr #\tab)
        (write-char #\space  stream)
        (write-char chr stream)))
    )
)

(defun preprocess (str)
  "Clean up input string"
  (replace-tabs (remove-if-not #'legal-char? str))
)

(defun split-string (str)
  "Returns a list containing items in 'string' split from
   occurrences of 'pivot'."
  (loop for n = 0 then (+ pos 1)
        for pos = (search " " str :start2 n)
        if pos collect (subseq str n pos)
        else collect (subseq str n)
        while pos))

;(defun split-string (str)
;  "Optional split-string using regular expressions. Requires the cl-ppcre module
;  to be installed"
;  (cl-ppcre::split "\\s+" str)
;)

(defun parse-int (str)
  "Parse integer from string"
  (if (null str) nil
  (parse-integer str :junk-allowed t)
  )
)

(defun parse-command (str)
  "Turn the command string into a list of command words and parameters after
   cleaning up the string"
  (let* ((lstr (split-string (preprocess str)))
         (id (parse-int (first lstr))))
    (if id (cons id (rest lstr)) (cons 'nil lstr))))

(defun gtp-parse-color (str)
  "Parse the color parameter"
  (cond ((equal str "black") *black*)
        ((equal str "b") *black*)
        ((equal str "white") *white*)
        ((equal str "w") *white*)
        (T *empty*)))

;Board intersections, in this document called vertices, are encoded by a letter
;plus a number. On a 19x19 board the letters go from A to T, excluding I, from
;the left to the right. The numbers go from 1 to 19, from the bottom to the top.
;Thus the lower left corner is called A1, the lower right corner T1, the upper
;left corner A19, and the upper right corner T19. Smaller boards use the obvious
;subset of these coordinates. Larger boards, up to 25x25, are handled by
;extending the letters with U to Z as needed. Boards larger than 25x25 are not
;supported by the protocol.
;
;In a nutshell, the letters refer to columns and the numbers refer to rows. Our
;game uses row*10 + column.
(defun gtp-parse-move (str)
  "Change the rows to our version of coordinates"
  (let* ((coord (coerce str 'list))
         (row (parse-int (string (second coord))))
         (newrow row) ; Reverse direction
         (col (char-int (first coord))) ; Change to ASCII number
         ;'A' = ASCII 65, 'a' = ASCII 97 so becomes 1 
         (newcol (if (> col 96) (- col 96) (- col 64))));
    (+ (* 10 newrow) newcol)))

(defun move-to-string (move)
  "Change from our version of coordinates to suitable for GTP"
  (let* ((coord (multiple-value-list (floor move 10)))
         (newrow (first coord))
         (col (+ 64 (second coord)))
         (colstr (string (code-char col)))
         (rowstr (write-to-string newrow)))
    (concatenate 'string colstr rowstr)))

(defun genmove (player board)
  "Call the appropriate strategy function and generate a move"
  (if (equal 1 (anylegalmove player board))
    (funcall *strategy* player board)
    *pass*
    ))

(defun concat-with-new-line (lst)
  "Concatenate a list of strings with newlines in between"
  (with-output-to-string (stream)
    (dolist (str lst)
      (write-string str stream)
      (terpri stream))))

(defun gtp-empty-resp (&optional params) 
  "Empty response. Params is optional so this function can be 
   called without arguments"
  (declare (ignore params))
  (list t ""))

(defun cpu-genmove (player board)
  "Time instrumentation for genmove"
  (let* ((t-init (get-internal-real-time))
         (move (genmove player board)))
    (incf *cpu-time* (- (get-internal-real-time) t-init))
    move))

(defun gtp-cmd-protocol-version (params)
  "Return the protocol version. This is version 2 of the GTP protocol"
  (declare (ignore params))
  (list t "2"))

(defun gtp-cmd-name (params)
  "Return the name of the strategy concatenated with the name of the package"
  (declare (ignore params))
  (list t (concatenate 'string (package-name *package*) "::" 
                               (string *strategy*))))

(defun gtp-cmd-version (params)
  "Return the version of this implementation of othello-gtp"
  (declare (ignore params))
  (list t *version*))

(defun gtp-cmd-known-command (params)
  "Return true if command is in the list of known commands" 
  (if (assoc (first params) *known-commands* :test #'equal)
    (list t "true")
    (list t "false")))

(defun gtp-cmd-list-commands (params)
  "List all available/known commands" 
  (declare (ignore params))
  (list t (concat-with-new-line (mapcar #'first *known-commands*))))

(defun gtp-cmd-boardsize (params)
  "Check if commanded board size is valid"
  (if (equal (parse-int (first params)) 8)
    (gtp-empty-resp)
    (list nil "unacceptable size")))

(defun gtp-cmd-clear-board (params)
  "Start a new game"
  (declare (ignore params))
  (setf *board* (initialboard))
  (setf *cpu-time* 0)
  (setf *boards* 0)
  (setf *eval-boards* 0)
  (gtp-empty-resp))

(defun gtp-cmd-play (params)
  "Carry out opponent's move on our board"
  (let* ((player (gtp-parse-color (first params)))
         (move (gtp-parse-move (second params))))
    (if (equal 1 (legalp move player *board*))
      (progn
        (makemove move player *board*)
        (gtp-log "Board >" (format nil "~S" *board*))
        (gtp-empty-resp))
      (list nil "illegal move"))))

(defun gtp-cmd-genmove (params)
  "Generate a move using the current strategy"
  (let* ((player (gtp-parse-color (first params)))
         (move (cpu-genmove player *board*)))
    (if (equal move *pass*)
      (list t "pass")
      (progn
        (makemove move player *board*)
        (gtp-log "Board >" (format nil "~S" *board*))
        (list t (move-to-string move))))))

(defun gtp-cmd-set-game (params)
  "Only the game Othello is supported"
  (if (equal (first params) "Othello")
    (gtp-empty-resp)
    (list nil "unsupported game")))

(defun gtp-cmd-list-games (params)
  "Only the game Othello is supported"
  (declare (ignore params))
  (list t "Othello"))

(defun gtp-cmd-final-score (params)
  "Display the final score and the boards generated"
  (declare (ignore params))
  (list t (format nil "B=~A W=~A CPU=~Ss Total Boards=~S"
                  (count-pieces 1 *board*)
                  (count-pieces 2 *board*)
                  (/ (float *cpu-time*) internal-time-units-per-second)
                  *boards*)))

(defun gtp-cmd-cputime (params)
  "Display the cumulative CPU time of the CPU player/strategy"
  (declare (ignore params))
  (list t (format nil "CPU = ~S" 
                  (/ (float *cpu-time*) internal-time-units-per-second))))

(defun gtp-cmd-showboard (params)
  "Display current board"
  (declare (ignore params))
  ;This is handled differently since printboard prints to stdout directly.
  (format t "= ") ; print the success designator
  (printboard *board*)
  ; Make sure there is a new line after the board has been printed 
  (format t "~%") 
  ; Even though the success has been sent already, it will be sent again because
  ; the funcation that called this would normally handle sending data back to
  ; the GTP runner.
  (list t "") 
  )

(defun gtp-handle (cmd params)
  "Handle all GTP commands"
  (let ((handler (assoc cmd *known-commands* :test #'equal)))
    (cond  (handler (funcall (second handler) params))
           (T (list nil "unknown command")))))

(defun gtp-init-log ()
  "Initialize log file"
  (if *logging*
    (with-open-file (stream *log-file* :direction :output :if-exists nil)
      (format stream "GTP Log~%"))))

(defun gtp-log (pre str)
  "Write string to log file"
  (if *logging*
    (with-open-file (stream *log-file* :direction :output :if-exists :append)
      (format stream "~A~A~%" pre str))))

(defun gtp-send (str id)
  "Send a success string to stdout while logging it in our log file"
  (let ((output (format nil "= ~@[~A ~]~A~%~%" id str)))
    (format t "~A" output)
    (gtp-log "sent> " output)))

(defun gtp-fail (str id)
  "Send a failure string to stdout while logging it in our log file"
  (let ((output (format nil "? ~@[~A ~]~A~%~%" id str)))
    (format t "~A" output)
    (gtp-log "error> " output)))

(defun gtp-input ()
  "Read GTP commands from stdin"
  (let ((str (read-line)))
    (gtp-log "received> " str)
    (if (not (equal str "")) str)))

(defun gtp-loop ()
  "Main loop that reads GTP commands from stdin, calles the GTP handler, and
   finally sends the results back"
  (gtp-init-log)
  (loop
    (let ((str (gtp-input)))
      (if (not (null str))
        (let* ((lstr (parse-command str))
               (id (first lstr))
               (cmd (second lstr))
               (params (rest (rest lstr)))
               (retlist (gtp-handle cmd params)))
          (if (first retlist)
            (gtp-send (second retlist) id)
            (gtp-fail (second retlist) id))
          (when (or (equal cmd "quit") (equal cmd "eof")) (return)))
        )
      )
    )
)

; Handle command line arguments. The argument is a an integer index into the
; array of strategies.
(let ((optval (parse-int (first (my-command-line)))))
  ;(if (and optval ( > optval 0))
  (if optval
    ; Set to last strategy if value is above limit
    (progn
      (setf optval (min optval (- (array-dimension *strategies* 0) 2)))
      (setf *strategy* (aref *strategies* optval 2)))
    )
  (gtp-loop))
