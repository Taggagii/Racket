;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |quick sorting practice|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; insertion sort

;;sorting from list to large to small format
(define test-list (list 12 5 35 1 6 8 3 23 6 347 2 2341 6 1 5 7 67 3 12))


(define (large-small-sort unsorted-list)
  (cond [(empty? unsorted-list) empty]
        [else (large-small-insert (first unsorted-list)
                      (large-small-sort (rest unsorted-list)))]))

(define (large-small-insert value sorted-list)
  (cond [(empty? sorted-list) (list value)]
        [(> value (first sorted-list)) (cons value sorted-list)]
        [(< value (first sorted-list)) (cons (first sorted-list)
                                             (large-small-insert value (rest sorted-list)))]
        [(= value (first sorted-list)) sorted-list]))

;(large-small-sort test-list)

;; sorting from list to small to large format
(define (small-large-sort unsorted-list)
  (cond [(empty? unsorted-list) empty]
        [else (small-large-insert (first unsorted-list)
                      (small-large-sort (rest unsorted-list)))]))

(define (small-large-insert value sorted-list)
  (cond [(empty? sorted-list) (list value)]
        [(< value (first sorted-list)) (cons value sorted-list)]
        [(> value (first sorted-list)) (cons (first sorted-list)
                                             (small-large-insert value (rest sorted-list)))]
        [(= value (first sorted-list)) sorted-list]))

;(small-large-sort test-list)

;; Testing Merge for merge sort

;; merging two sorted lists
;;concept: take the larger of the two and append it

(define sorted-1 (large-small-sort (list 5 23 2 6 2 3 62 2 6 237 2 34 2 3 999 2 31 5)))
(define sorted-2 (large-small-sort (list 23 6 2 3 62 72  331 6 27 8 3 45 1 23 5 21 3 76 32 4 2 34 5 6 2 34)))

(define (large-small-merge list1 list2)
  (cond [(and (empty? list1) (empty? list2)) empty]
        [(empty? list1) list2]
        [(empty? list2) list1]
        [(> (first list1) (first list2)) (cons (first list1)
                                               (large-small-merge (rest list1)
                                                                  list2))]
        [(> (first list2) (first list1)) (cons (first list2)
                                               (large-small-merge list1
                                                                  (rest list2)))]
        [(= (first list1) (first list1)) (cons (first list1)
                                               (large-small-merge (rest list1)
                                                                  (rest list2)))]))
 
(large-small-merge sorted-1 sorted-2) 



