(setq graph (make-hash-table))
(setf (gethash ‘m graph) ‘(q r x))
(setf (gethash ‘q graph) ‘(t))
(setf (gethash ‘n graph) ‘(o q u))
(setf (gethash ‘u graph) ‘(t))
(setf (gethash ‘r graph) ‘(u y))
(setf (gethash ‘y graph) ‘(v))
(setf (gethash ‘v graph) ‘(x w))
(setf (gethash ‘o graph) ‘(r s v))
(setf (gethash ‘s graph) ‘(r p))
(setf (gethash ‘p graph) ‘(o s z))
(setf (gethash ‘w graph) ‘(z))
(gethash ‘q graph)


(defun check-cycle-by-dfs (i visited)
    (cond ((null i) nil)
          ((equal i "CYCLE") "CYCLE")
          ((atom i) (if (member i visited) "CYCLE"
            (check-cycle-by-dfs (gethash i graph) (cons i visited))))
          (T (if (equal (check-cycle-by-dfs (first i) visited) "CYCLE") "CYCLE"
            (check-cycle-by-dfs (rest i) visited))))) 

(check-cycle-by-dfs ‘(m q n u r y v o s p w) '())