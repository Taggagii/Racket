;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Path work|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A FileSystem is one of:
;; * File
;; * Dir
  
(define-struct file (name size owner))
;; A File is a (make-file Str Nat Sym)

(define-struct dir (name owner contents))
;; A Dir is a (make-dir Str Sym (listof FileSystem))

(define example-fs
  (make-dir "root" 'root
            (list
             (make-dir "Dan" 'dan
                       (list (make-file "log.txt" 785 'dan)
                             (make-file "profile.jpg" 60716 'dan)
                             (make-dir "music" 'dan
                                       (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))))
             (make-dir "Slides" 'teaching
                       (list (make-dir "cs135" 'teaching
                                       (list (make-file "01-intro.pdf" 72244 'teaching)
                                             (make-file "11-trees.pdf" 123124 'teaching)
                                             (make-dir "system" 'root
                                                       (list (make-dir "logs" 'teaching empty)))))))
             (make-file "vmlinuz" 30 'root))))

(define my-filesystem
  (make-dir "root" 'admin
            (list (make-dir "joshu" 'josh
                            (list
                             (make-dir "music" 'josh
                                       (list (make-file "wanderer.mp3" 50000 'josh)
                                             (make-file "around-the-world.mp3" 55000 'josh)
                                             (make-file "lemon-tree.mp3" 52300 'josh)
                                             (make-file "Autumn Leaves.mp3" 2134452 'josh)
                                             (make-file "Rock Candy.mp3" 4226 'josh)
                                             (make-file "Jiv Samba.mp3" 522300 'josh)
                                             (make-file "Cute.mp3" 35326 'josh)
                                             (make-file "Star Eyes.mp3" 252342 'josh)
                                             (make-file "Shiny Stockings.mp3" 63724 'josh)
                                             (make-file "Perdido.mp3" 378373 'josh)
                                             (make-file "Cheese Cake.mp3" 738754 'josh)))
                             (make-dir "homework" 'josh
                                       (list (make-file "a01-q1.rkt" 200 'josh)
                                             (make-file "a01-q2.rkt" 502 'josh)
                                             (make-file "a04-q4.rkt" 550 'josh)))))
                  (make-dir "family" 'family (list
                                              (make-dir "pictures" 'family
                                                        (list (make-file "baby1.jpeg" 52030 'family)
                                                              (make-file "baby2.png" 5023242 'family)
                                                              (make-file "yogurt-container.jpeg" 523223234235 'family)))
                                              (make-dir "blackmail" 'family
                                                        (list (make-file "deaths.csv" 999999999 'family)
                                                              (make-file "embezzlements.xlsm" 2343666 'family)))))
                  (make-dir "guest" 'guest (list ))
                  (make-dir "system32" 'admin (list
                                               (make-dir "drivers" 'admin (list (make-file "3ware.sys" 105000 'admin)
                                                                                (make-file "acpipagr.sys" 18000 'admin)
                                                                                (make-file "amdpsp.sys" 203000 'admin)
                                                                                (make-file "appid.sys" 210000 'admin)))
                                               (make-dir "shell-experience" 'admin (list (make-file "windowUIShellUtils.dll" 234212 'admin)
                                                                                         (make-file "windowsInternalControls.dll" 1231251231 'admin)))))
                  (make-file "ADMIN-BASIC.TXT" 2342 'admin))))


;; -----------------------------------------------------------------------------------------

;; creates a list to all values in the tree (once you have this specializing it is much easier)

;; example-fs
;; going to go through all paths, find the paths of each value in their
;;    contents and then append that to a list
(define (all-paths tree path)
  (cond [(file? tree) (list (append path (list (file-name tree))))]
        [(dir? tree) (cons (append path (list (dir-name tree)))
                           (all-paths/list (dir-contents tree)
                                     (append path (list (dir-name tree)))))]))

 
 
;; all -paths lists takes the contents and their root path and produces
;;    and returns a list of all the paths of the contents
;;    appended to the end fo the taken in path   

(define (all-paths/list contents path)
  (cond [(empty? contents) empty]
        [else (append (all-paths (first contents) path)
                      (all-paths/list (rest contents) path))]))


;; -----------------------------------------------------------------------------------------



;; Finding all paths by a specific person in a more efficent way

;; wrapper function
(define (person-paths/wrapper tree name)
  (persons-paths tree (list) name))

;; example-fs
;; going to go through all paths, find the paths of each value in their
;;    contents and then append that to a list
;; (complex function that does all the thinkinging) (does change from just grabbing all values)
(define (persons-paths tree path master)
  (cond [(file? tree) (cond [(symbol=? (file-owner tree)
                                       master)
                             (list (append path (list (file-name tree))))] ; if a file then return the path to that file
                            [else empty])]
        [(dir? tree) (cond [(symbol=? (dir-owner tree)
                                      master) (cons (append path (list (dir-name tree)))
                                                       (persons-paths/list (dir-contents tree)
                                                                       (append path (list (dir-name tree))) master))] ;if a dir, then return the path to the dir and the paths of everything the dir contains
                           [else (persons-paths/list (dir-contents tree)
                                                 (append path (list (dir-name tree))) master)])])) 
 

  
;; (applies the above function to all the values in a list), (doesn't really change from just grabbing all values)

(define (persons-paths/list contents path master)
  (cond [(empty? contents) empty]
        [else (append ; append the list of current guys paths to the list of the paths of the rest of the guys
                      (persons-paths (first contents) path master) ;get the list of paths of the value
                      (persons-paths/list (rest contents) path master))])) ;get the list of path of the rest
 

;; -----------------------------------------------------------------------------------------
;; Finding all paths for a certain person inefficent way (but with many functions that are useful to know)


;; (owned-by file-system master) consumes a FileSystem (file-system) and a
;;    symbol (master). Produces a list of paths to all FileSystems owned by that
;;    user. 
;; Examples:
(check-expect (owned-by example-fs 'teaching)
              (list 
               (list "root" "Slides")
               (list "root" "Slides" "cs135")
               (list "root" "Slides" "cs135" "01-intro.pdf")
               (list "root" "Slides" "cs135" "11-trees.pdf") 
               (list "root" "Slides" "cs135" "system" "logs")))
(check-expect (owned-by example-fs 'cia) empty)

;; owned-by: FileSystem Sym -> (listof (listof Str))
(define (owned-by file-system person)
  (build-path-list file-system (find-files-by file-system person)))


;; (build-path-list file-system files-by) takes in a FileSystem (file-system)
;;    and a list of Files (files-by) from a specific user. Produces a list
;;    of paths to each file.
;; Examples:
(check-expect (build-path-list example-fs
                               (list (make-dir "Dan" 'dan
                                               (list
                                                (make-file "log.txt" 785 'dan)
                                                (make-file "profile.jpg" 60716 'dan)
                                                (make-dir "music" 'dan (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))))
                                     (make-file "log.txt" 785 'dan)
                                     (make-file "profile.jpg" 60716 'dan)
                                     (make-dir "music" 'dan (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))
                                     (make-file "Thelonius Monk.mp3" 92227584 'dan)))
              (list
               (list "root" "Dan")
               (list "root" "Dan" "log.txt")
               (list "root" "Dan" "profile.jpg")
               (list "root" "Dan" "music")
               (list "root" "Dan" "music" "Thelonius Monk.mp3")))

;; build-path-list: FileSystem (listof Files) -> (listof (listof Str))
(define (build-path-list file-system files-by)
  (cond [(empty? files-by) empty]
        [else (cons (find-path-to file-system (first files-by))
                    (build-path-list file-system (rest files-by)))]))

;; (find-files-by file-system person) takes in a FileSystem (file-system)
;;    and a symbol (person) and produces all files in the FileSystem that
;;    are owned by that person.
;; Examples:
(check-expect (find-files-by example-fs 'dan)
              (list (make-dir "Dan" 'dan
                              (list (make-file "log.txt" 785 'dan)
                                    (make-file "profile.jpg" 60716 'dan)
                                    (make-dir "music" 'dan (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))))
                    (make-file "log.txt" 785 'dan)
                    (make-file "profile.jpg" 60716 'dan)
                    (make-dir "music" 'dan (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))
                    (make-file "Thelonius Monk.mp3" 92227584 'dan)))


;; find-files-by: FileSystem Sym -> (listof Files)
(define (find-files-by file-system person)
  (cond [(file? file-system) (cond [(symbol=? (file-owner file-system)
                                              person)
                                    (cons file-system empty)]
                                   [else empty])]
        [(dir? file-system) (cond [(symbol=? (dir-owner file-system)
                                             person)
                                   (append (list file-system)
                                           (find-files-by/list
                                            (dir-contents file-system)
                                            person))]
                                  [else (find-files-by/list
                                         (dir-contents file-system)
                                         person)])]))

;; (find-files-by/list contents person) takes in a list of FileSystems
;;    (contents) and a symbol (person), produces a list of all files in the
;;    list of FileSystems that are owned by that person.
;; Examples:
(check-expect (find-files-by/list
               (list
                (make-dir
                 "Dan"
                 'dan
                 (list
                  (make-file "log.txt" 785 'dan)
                  (make-file "profile.jpg" 60716 'dan)
                  (make-dir "music" 'dan (list (make-file "Thelonius Monk.mp3" 92227584 'dan)))))
                (make-dir
                 "Slides"
                 'teaching
                 (list
                  (make-dir
                   "cs135"
                   'teaching
                   (list
                    (make-file "01-intro.pdf" 72244 'teaching)
                    (make-file "11-trees.pdf" 123124 'teaching)
                    (make-dir "system" 'root (list (make-dir "logs" 'teaching '())))))))
                (make-file "vmlinuz" 30 'root)) 'root)
              (list
               (make-dir
                "system"
                'root
                (list
                 (make-dir "logs" 'teaching empty)))
               (make-file "vmlinuz" 30 'root)))


;; find-files-by/list (listof FileSystem) Sym -> (listof (listof Str))
(define (find-files-by/list contents person)
  (cond [(empty? contents) empty]
        [else (append (find-files-by (first contents) person)
                      (find-files-by/list (rest contents) person))]))

;; (find-path-to file-system object) takes in a FileSystem (file-system)
;;    and either a Dir or a File (object) and produces the path
;;    in (file-system) that leads to the object.
;; Examples:
(check-expect (find-path-to example-fs (make-file "vmlinuz" 30 'root))
              (list "root" "vmlinuz"))


;; find-path-to: FileSystem (anyof File Dir) -> (listof Str)
(define (find-path-to file-system object)
  (cond [(search file-system object)
         (cond [(file? file-system) (cons (file-name file-system) empty)]
               [(dir? file-system)
                (cons (dir-name file-system)
                      (find-path-to (tree-containing
                                     (dir-contents file-system)
                                     object) object))])] 
        [else empty]))

;; (tree-containing contents object) takes in a list of FileSystems
;;    (contents) and either a Dir or a File (object) and produces
;;    the Dir in (contents) that contains the object. If the object is not in
;;    (contents) returns empty.
;; Examples:
(check-expect (tree-containing (dir-contents example-fs) (make-file "log.txt" 785 'dan))
              (make-dir
               "Dan"
               'dan
               (list
                (make-file "log.txt" 785 'dan)
                (make-file "profile.jpg" 60716 'dan)
                (make-dir
                 "music"
                 'dan
                 (list
                  (make-file
                   "Thelonius Monk.mp3"
                   92227584
                   'dan))))))

;; tree-containing: (listof FileSystem) (anyof File Dir) -> FileSystem
(define (tree-containing contents object)
  (cond [(empty? contents) empty]
        [(search (first contents) object)
         (first contents)]
        [else (tree-containing (rest contents) object)]))

;; (search file-system object) takes in a FileSystem (file-system) and
;;    either a Dir or a File (object). Produces if the (object) is found
;;    within the FileSystem.
;;  Examples:
(check-expect (search example-fs (make-file "test" 234 'noone)) false)
(check-expect (search example-fs (make-file "Thelonius Monk.mp3" 92227584 'dan))
              true)

;; search: FileSystem (anyof File Dir) -> Bool
(define (search file-system object)
  (cond [(equal? file-system object) true]
        [(dir? file-system) (search/list (dir-contents file-system)
                                         object)]
        [else false]))

;; (search/list contents object) takes in a list of FileSystems (contents)
;;    and either a Dir or a File (object). Produces if the (object) is found
;;    is any of the FileSystems in the list.
;; Examples:
(check-expect (search/list (dir-contents example-fs)
                           (make-file "test" 234 'noone)) false)
(check-expect (search/list (dir-contents example-fs)
                           (make-file "Thelonius Monk.mp3" 92227584 'dan))
              true)

;; search/list: (listof FileSystem) (anyof File Dir) -> Bool
(define (search/list contents object)
  (cond [(empty? contents) false]
        [else (or (search (first contents) object)
                  (search/list (rest contents) object))]))
