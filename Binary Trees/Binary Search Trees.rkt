;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Binary Search Trees|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (key left right))
;; A Node is a (make-node Nat BT BT)

;; A binary tree (BT) is one of:
;;  * empty
;   * Node


;; bt-template: BT -> Any
(define (bt-template t)
  (cond [(empty? t) ...]
        [(node? t) (... (node-key t)
                        (bt-template (node-left t))
                        (bt-template (node-right t)))]))

(define (bst-from-list list)
  (cond [(empty? list) empty]
        [else (bst-add (bst-from-list (rest list))
                       (first list))]))


(define (bst-add tree value)
  (cond [(empty? tree) (make-node value empty empty)]
        [(> (node-key tree) value) (make-node (node-key tree)
                                              (bst-add (node-left tree) value)
                                              (node-right tree))]
        [(= (node-key tree) value) tree]
        [else (make-node (node-key tree)
                         (node-left tree)
                         (bst-add (node-right tree) value))]))
