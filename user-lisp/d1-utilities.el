;;; d1-utilities.el --- General utility functions  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Collection of general-purpose utility functions for various tasks.
;;

;;; Code:

(defun d1-kill-process-on-port (port)
  "Kill a process running on PORT.
Prompts for a port number.  If multiple processes are listening,
lets you select which one to kill via completion."
  (interactive "nPort number: ")
  (let* ((port-str (number-to-string port))
         (pid-command (format "lsof -ti :%s" port-str))
         (pids (split-string (shell-command-to-string pid-command) "\n" t)))
    (if (null pids)
        (message "No process found running on port %s" port-str)
      (let* ((process-alist
              (mapcar (lambda (pid)
                        (let ((name (string-trim
                                     (shell-command-to-string
                                      (format "ps -p %s -o comm=" pid)))))
                          (cons (format "%s (PID %s)" name pid) pid)))
                      pids))
             (selection (if (= (length pids) 1)
                            (caar process-alist)
                          (completing-read
                           (format "Select process on port %s: " port-str)
                           process-alist nil t)))
             (pid (cdr (assoc selection process-alist))))
        (when (yes-or-no-p (format "Kill %s? " selection))
          (shell-command (format "kill -9 %s" pid))
          (message "Killed %s on port %s" selection port-str))))))

(defun d1--format-uptime (etime)
  "Convert elapsed time ETIME from ps format to human-readable format.
Examples: '5:30' -> '5m', '1:05:30' -> '1h', '2-03:45:12' -> '2d'."
  (cond
   ;; Format: DD-HH:MM:SS (days)
   ((string-match "^\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)$" etime)
    (let ((days (string-to-number (match-string 1 etime)))
          (hours (string-to-number (match-string 2 etime))))
      (if (> hours 0)
          (format "%dd %dh" days hours)
        (format "%dd" days))))
   ;; Format: HH:MM:SS (hours)
   ((string-match "^\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)$" etime)
    (let ((hours (string-to-number (match-string 1 etime)))
          (mins (string-to-number (match-string 2 etime))))
      (cond
       ((> hours 0) (format "%dh %dm" hours mins))
       ((> mins 0) (format "%dm" mins))
       (t (format "%ds" (string-to-number (match-string 3 etime)))))))
   ;; Format: MM:SS (minutes)
   ((string-match "^\\([0-9]+\\):\\([0-9]+\\)$" etime)
    (let ((mins (string-to-number (match-string 1 etime))))
      (if (> mins 0)
          (format "%dm" mins)
        (format "%ds" (string-to-number (match-string 2 etime))))))
   ;; Fallback
   (t etime)))

(defun d1-kill-process ()
  "Kill a process selected from a list using completion.
Lists all running processes with human-friendly formatting and colors."
  (interactive)
  (let* ((ps-output (shell-command-to-string "ps -eo pid,%cpu,%mem,etime,comm"))
         (lines (split-string ps-output "\n" t))
         ;; Skip the header line
         (process-lines (cdr lines))
         ;; Create an alist of (display-string . pid)
         (process-alist
          (mapcar (lambda (line)
                    (when (string-match "^\\s-*\\([0-9]+\\)\\s-+\\([0-9.]+\\)\\s-+\\([0-9.]+\\)\\s-+\\([^ ]+\\)\\s-+\\(.+\\)$" line)
                      (let* ((pid (match-string 1 line))
                             (cpu (string-to-number (match-string 2 line)))
                             (mem (string-to-number (match-string 3 line)))
                             (etime (match-string 4 line))
                             (comm (match-string 5 line))
                             (uptime (d1--format-uptime etime))
                             ;; Determine if CPU or MEM usage is high (> 5%)
                             (cpu-high (> cpu 5.0))
                             (mem-high (> mem 5.0))
                             ;; Format: PID  Uptime  CPU  MEM  Process Name
                             (display (concat
                                       ;; PID: Bold only, no color
                                       (propertize (format "%-8s" pid)
                                                  'face '(:weight bold))
                                       ;; Uptime: Muted with shadow face
                                       (propertize (format "%-10s" uptime)
                                                  'face 'shadow)
                                       ;; CPU: Highlight only if > 5%
                                       (propertize (format "CPU: %-6s" (format "%.1f%%" cpu))
                                                  'face (if cpu-high 'warning 'shadow))
                                       ;; MEM: Highlight only if > 5%
                                       (propertize (format "MEM: %-6s" (format "%.1f%%" mem))
                                                  'face (if mem-high 'warning 'shadow))
                                       ;; Process name: Slightly emphasized
                                       (propertize comm
                                                  'face '(:weight semi-bold)))))
                        (cons display pid))))
                  process-lines))
         ;; Remove nil entries
         (process-alist (delq nil process-alist)))
    (if (null process-alist)
        (message "No processes found")
      (let* ((selection (completing-read "Select process to kill: " process-alist nil t))
             (pid (cdr (assoc selection process-alist))))
        (when pid
          (let* ((name-command (format "ps -p %s -o comm=" pid))
                 (process-name (string-trim (shell-command-to-string name-command)))
                 (prompt (format "Kill process '%s' (PID %s)? " process-name pid)))
            (when (yes-or-no-p prompt)
              (shell-command (format "kill -9 %s" pid))
              (message "Killed process '%s' (PID %s)" process-name pid))))))))

(provide 'd1-utilities)
;;; d1-utilities.el ends here
