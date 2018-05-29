(defvar tree ‘((a (b)) c(d)))
(defvar tree2 ‘((w x) (y z)))

(defun dfs (tree)
 (cond ((null tree) nil) ; empty tree, no atoms
       ((atom tree) (cons tree nil)) ;create a list which contains only an atom
       (t (append (dfs (car tree)) ; search the left branch
                 (dfs (cdr tree)))))) ; then the right branch

(dfs tree)
(dfs tree2)

(defun withoutlist (lst)
  (if (null lst) 
      nil
    (if (atom (first lst)) 
        (cons (first lst) 
                (withoutlist (rest lst)))
      (withoutlist (rest lst)))))
  
(defun withoutatom (lst)
  (if (null lst) 
      nil
    (if (atom (first lst)) 
        (withoutatom (rest lst))
      (append (first lst) 
                (withoutatom (rest lst)))
      )))

(defun bfs (tree)
 (cond ((null tree) nil) 
       ((atom tree) (cons tree nil)) 
       (t (append (withoutlist tree)
                  (bfs (withoutatom tree))))))

(bfs tree)
(bfs tree2)

(defun dfsdepth (tree depth)
 (cond  ((< depth 0) nil)
        ((null tree) nil)
        ((atom tree) (cons tree nil)) 
        (t (append (dfsdepth (first tree) (- depth 1)) 
                (dfsdepth (rest tree) depth)))))

(defun caller (tree rest)
  (cond  ((< rest 0) nil)
        ((null tree) nil) 
        (t (append (caller tree (- rest 1)) (dfsdepth tree rest)))))

(defun depth (tree)
	(if (null tree) 0
	(+ 1 (depth (withoutatom tree)))))

(defun dfid (tree)
  (caller tree (depth tree)))

(dfid tree)
(dfid tree2)