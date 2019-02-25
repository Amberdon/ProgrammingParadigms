;; 1.1. Найти длину линейного списка.
( defun listlen (l)
    ( cond
      ((atom l) 0)
      (t (+ 1 (listlen ( cdr l))))))


(setq s11 `(1 2 3 4 a))
(format t "(listlen `~s) = ~d~%" s11 (listlen s11))

;; 1.2. Найти последний элемент (атом) линейного списка.
;; last
(defun findlast(l)
  ( cond
    ((null (cdr l)) (car l))
    (t (findlast (cdr l)))))

(setq s12 `(1 2 3 4 a))
(format t "(findlast `~s) = ~a~%" s12 (findlast s12))

;; 1.3. Найти заданный атом в линейном списке (результат=позиция):
  ;; а все, б первый (на выбор.)
(defun findatom(a l)
  (cond
    ((eq a (car l)) 1)
    (t (+ 1 (findatom a (cdr l))))))

(defun incall(l)
    (cond
        ((eq l nil) ())
        (t (cons (+ 1 (car l)) (incall (cdr l))))))

(defun findall(a l)
    (cond
        ((eq l nil) ())
        ((eq a (car l)) (cons `1 (incall (findall a (cdr l)))))
        (t (incall (findall a (cdr l))))))

(setq s13 `(1 2 a 3 4 s nil a))
(format t "(findatom `a `~s) = ~d~%" s13 (findatom `a s13))


;; 1.4. Удалить из линейного списка все заданные атомы.
(defun contains(a l)
  (cond
    ((eq l nil) nil)
    ((eq a (car l)) t)
    ((contains a (cdr l)) t)
    (t nil)))

(defun removes(a l)
  (cond
    ((eq l nil) ())
    ((eq a (car l)) (removes a (cdr l)))
    (t (cons (car l) (removes a (cdr l))))))


;; 1.5. Вставить атом в линейный список перед заданным элементом:
;; а) перед всеми, б) только перед первым
;; элемент задан а) номером, б) значением.
(defun insertab(a b l)
    (cond
        ((eq l nil) ())
        ((eq b (car l)) (cons a (cons b (insertab a b (cdr l)))))
        (t (cons (car l) (insertab a b (cdr l))))))

(defun insertaa(a p l)
    (cond
        ((eq l nil) ())
        ((eq p 1) (cons a (cons (car l) (insertab a (car l) (cdr l)))))
        (t (cons (car l) (insertaa a (- p 1) (cdr l))))))

(defun insertba(a p l)
    (cond
        ((eq l nil) ())
        ((eq p 1) (cons a l))
        (t (cons (car l) (insertba a (- p 1) (cdr l))))))

(defun insertbb(a b l)
    (cond
        ((eq l nil) ())
        ((eq a (car l)) (cons a l))
        (t (cons (car l) (insertbb a b (cdr l))))))
;2.0. Перечислить все атомы нелинейного списка слева направо = развернуть в линейный.
(defun roll(l)
    (cond
        ((atom l) l)
        ((atom (car l)) (cons (car l) (roll (cdr l))))
        (t (append (roll (car l)) (roll (cdr l))))))

;2.1. Определить количество атомов в нелинейном списке.
(defun listlenn(l)
    (listlen (roll l)))
