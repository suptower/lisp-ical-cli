(in-package :ical-cli)

(defun mergeSortOld (buffer)
  "Perform a mergeSort on the given buffer and return the sorted buffer"
  (let ((left (list))
	(right (list))
	(nx (- (length buffer) 1)))
    (loop for index from 0 to (floor (/ nx 2))
	  do
	     (setf left (append left (list (nth index buffer)))))
    (loop for index from (+ (floor (/ nx 2)) 1) to nx
	  do
	     (setf right (append right (list (nth index buffer)))))
    (setf left (mergeSort left))
    (setf right (mergeSort right))
    (mergeBuffersOld left right)))

(defun merge-lists (left right)
  (let ((output (list))
	(nl (- (length left) 1))
	(nr (- (length right) 1))
	(il 0))
    (iterate:iter (iterate:for i from 0 to (+ nl nr 1))
      (if (> il nl)
	  (progn
	    (setf output (append output (list (nth (- i iL) right))))
	    (iterate:next-iteration)))
      (if (< il (- i nr))
	  (progn
	    (setf output (append output (list (nth il left))))
	    (setf il (+ il 1))
	    (iterate:next-iteration)))
      (if (isEarlier (nth 0 (nth il left)) (nth 0 (nth (- i il) right)))
	  (progn
	    (setf output (append output (list (nth il left))))
	    (setf il (+ il 1)))
	  (setf output (append output (list (nth (- i il) right))))))
    output))
  
(defun merge-sort (list)
  (if (small list) list
      (merge-lists
       (merge-sort (left-half list))
       (merge-sort (right-half list)))))

(defun small (list)
  (or (null list) (null (cdr list))))

(defun right-half (list)
  (last list (ceiling (/ (length list) 2))))

(defun left-half (list)
  (ldiff list (right-half list)))

(defun isEarlier (date1 date2)
  (let ((local1 (createLocalFromHR date1))
	(local2 (createLocalFromHR date2)))
    (if (local-time:timestamp< local1 local2)
	t
	nil)))
