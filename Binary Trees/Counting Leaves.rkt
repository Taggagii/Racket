(define-struct node (key left right))
;; A Node is a (make-node Nat BT BT)

;; A binary tree (BT) is one of:
;; * empty
;; * Node

(define (leaf-count tree)
  (cond [(empty? tree) 0]
        [(and (empty? (node-left tree))
              (empty? (node-right tree))) 1]
        [(node? tree) (+ (leaf-count (node-left tree))
                         (leaf-count (node-right tree)))]))