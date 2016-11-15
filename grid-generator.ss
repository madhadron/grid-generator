;; grid-generator, Copyright 2008, Fred Ross
;; 
;; grid-generator provides external support for visiting arrays of points in the CellM software used
;; with Olympus microscopes in Europe.  It is a DrScheme program.  To create an executable, load this
;; file in DrScheme (http://www.drscheme.org), set the language to 'Module'  under Language->Choose
;; Language..., and then go to Scheme->Create Executable.
;;
;; To use the program, open CellM and make a list of points containing, in order, the bottom left,
;; bottom right, and top right points of your grid.  Then close CellM.  Open grid-generator, and
;; select the name of your point list, tell it the number of rows and columns, and hit 'Generate.'
;; A new list will appear with the name of the list you generated it from followed by '-grid.'  Exit
;; grid-generator, return to CellM, and use that list, which contains the required grid of points.

(require xml)
(require srfi/1)
(require scheme/gui/base)

;; This is for testing purposes only.
(define cellm-xml-file "/Users/ross/data/software/gridgenerator/test.xml")

;; Poor man's unit testing, used extensively below.
(define (test expected computed)
  (unless (equal? expected computed)
    computed))

;; XML handling code
;;
;; All XML handling is done by converting the XML file into an S-expression.  This means that all formatting
;; of the file is lost.  The file after grid-generator gets through with it will not be nicely formatted for
;; humans to read.
;;
;; The OBSPOSTLISTS.XML file contains a toplevel element 'ObsPosLists,' with no attributes.  Its subelements are
;; 'List' with attribute 'name' which holds the name CellM displays for that list.  Its subelements consist of
;; 'Position' with attributes 'num', 'posX', 'posY', and 'posZ', representing the position in the list (counting
;; from 0), and the X-, Y-, and Z-coordinates.
;;
;; Internally we represent positions as lists of four numbers '(num posX posY posZ).  position-elem->position
;; creates that representation from the XML element; position->position-elem is its inverse.
(define (position-elem->position v) 
  (map string->number (map cadr (cadr v))))
(define (position->position-elem v)
  `(Position ,(zip '(num posX posY posZ) (map (compose number->string floor) v))))
(define (test-position-elem->position) 
  (test '(0 -55395000 -40746500 5575810) 
        (position-elem->position 
         '(Position ((num "0") (posX "-55395000") (posY "-40746500") (posZ "5575810"))))))
(define (test-position->position-elem)
  (test '(Position ((num "0") (posX "-55395000") (posY "-40746500") (posZ "5575810")))
        (position->position-elem '(0 -55395000 -40746500 5575810))))

;; Lists are represented internally as a cons of the list name and the list of positions (in effect, '(name pos1 pos2 ...))
;; which means we can use association list tools on lists of lists.
(define (list-elem->list v)
  (let ((name (cadr (caadr v)))
        (positions (map position-elem->position (filter (lambda (x) (not (string? x))) (cddr v)))))
    `(,name . ,positions)))
(define (list->list-elem v)
  `(List ((name ,(car v))) ,@(map position->position-elem (cdr v))))

(define (test-list-elems)
  (let ((v '("devicetest" (0 -55395000 -40746500 5575810) (1 -55767000 -40746500 5565810) (2 -56167000 -40746500 5560810) (3 -56562000 -40746500 5553310) (4 -56959500 -40746500 5549310) (5 -57350500 -40746500 5543310) (6 -57741000 -40762500 5536310) (7 -58183000 -40900500 5532310) (8 -58602000 -40900500 5532310) (9 -58942500 -40900500 5523810) (10 -59313500 -40900500 5518310))))
    (test v (list-elem->list (list->list-elem v)))))
(define (test-list-elem->list)
  (test '("devicetest" (0 -55395000 -40746500 5575810) (1 -55767000 -40746500 5565810) (2 -56167000 -40746500 5560810) (3 -56562000 -40746500 5553310) (4 -56959500 -40746500 5549310) (5 -57350500 -40746500 5543310) (6 -57741000 -40762500 5536310))
        (list-elem->list '(List ((name "devicetest")) "\r\n\t\t" (Position ((num "0") (posX "-55395000") (posY "-40746500") (posZ "5575810"))) "\r\n\t\t" (Position ((num "1") (posX "-55767000") (posY "-40746500") (posZ "5565810"))) "\r\n\t\t" (Position ((num "2") (posX "-56167000") (posY "-40746500") (posZ "5560810"))) "\r\n\t\t" (Position ((num "3") (posX "-56562000") (posY "-40746500") (posZ "5553310"))) "\r\n\t\t" (Position ((num "4") (posX "-56959500") (posY "-40746500") (posZ "5549310"))) "\r\n\t\t" (Position ((num "5") (posX "-57350500") (posY "-40746500") (posZ "5543310"))) "\r\n\t\t" (Position ((num "6") (posX "-57741000") (posY "-40762500") (posZ "5536310")))))))


;; ObsPosLists internally are a list of lists, but because of our representation for list, it turns out
;; to be an association list with the list name as key and the list of positions as values.
(define (obs-pos-lists->lists v)
    (map list-elem->list (filter (lambda (x) (not (string? x))) (cddr v))))

(define (lists->obs-pos-lists v)
  `(ObsPosLists () ,@(map list->list-elem v)))

(define (test-obs-pos-lists)
  (let ((v '(("devicetest" (0 -55395000 -40746500 5575810) (1 -55767000 -40746500 5565810) (2 -56167000 -40746500 5560810) (3 -56562000 -40746500 5553310) (4 -56959500 -40746500 5549310) (5 -57350500 -40746500 5543310) (6 -57741000 -40762500 5536310) (7 -58183000 -40900500 5532310) (8 -58602000 -40900500 5532310) (9 -58942500 -40900500 5523810) (10 -59313500 -40900500 5518310)) ("080404" (0 -64363000 -46172500 5880290) (1 -66167000 -46228000 5880290) (2 -66688500 -46161500 5859290) (3 -67585500 -46320500 5859290) (4 -68011000 -46096500 5859290) (5 -68377500 -45958000 5859290)))))
    (test v (obs-pos-lists->lists (lists->obs-pos-lists v)))))

(define (obs-pos-lists->list-labels v)
  (map car (obs-pos-lists->lists v)))

;; Grid and affine transform code
;; I would like to do 3D transforms, but it is not possible: coplanar points only determine a 2D transform.
;; As it is, I take the mean z coordinate from the three points given.  You must use autofocus to correct
;; for this.
;;
;; For a discussion of how this works and the code from which this was adapted see grids.txt.

(define (v+ . x)
  (map (lambda (k) (apply + k)) (apply zip x)))
(define (test-v+) (test '(6 6) (v+ '(1 1) '(2 2) '(3 3))))
(define (v- . x)
  (map (lambda (k) (apply - k)) (apply zip x)))
(define (test-v-) (test '(0 0) (v- '(3 3) '(2 2) '(1 1))))
(define (vs* sc x)
  (map (lambda (y) (* sc y)) x))
(define (test-vs*) (test '(2 2) (vs* 2 '(1 1))))

(define ((make-labelled-transform a-raw b-raw c-raw max-row max-col) q)
  (let-values (((num x y z) (apply values q))
               ((a b c) (apply values (map (lambda (x) (take (drop x 1) 2)) (list a-raw b-raw c-raw))))
               ((mean-z) (/ (+ (fourth a-raw) (fourth b-raw) (fourth c-raw)) 3)))
    `(,num ,@(v+ (vs* (/ x max-row) (v- b a)) (vs* (/ y max-col) (v- c b)) a) ,mean-z)))

(define (rem a b)
  (- a (* (floor (/ a b)) b)))

(define (seq a b)
  (if (>= a b) '() (cons a (seq (+ a 1) b))))

(define (generate-grid max-row max-col)
  (map (lambda (n) 
         (let* ((x (floor (/ n (+ max-col 1))))
                (y (if (even? x) (rem n (+ max-col 1)) (- max-col (rem n (+ max-col 1))))))
           (list n x y 0)))
       (seq 0 (* (+ max-row 1) (+ max-col 1)))))

(define (test-transform-grid)
  (map (make-labelled-transform '(0 0 0 0) '(0 0 1 0) '(0 -1 1 0) 2 4) (generate-grid 2 4)))

;; Calculate new grids from given points
;; At last, we take the first three positions of a list and create a new list with a grid reaching
;; to those positions.  The function to use is grid-obs-pos-lists which takes the name of the
;; list to grid, the 'opl' (the obsposlist internal representation) to work on, and the number of
;; rows and columns.

(define (list->gridded-list v nrows ncols)
  (let ((base-name (first v))
        (a (second v))
        (b (third v))
        (c (fourth v)))
    `(,(string-append base-name "-grid") ,@(map (make-labelled-transform a b c (- nrows 1) (- ncols 1)) (generate-grid (- nrows 1) (- ncols 1))))))

(define (grid-list name lists nrows ncols)
  (let ((entry (assoc name lists)))
    (if entry
      (cons (list->gridded-list entry nrows ncols) lists)
      lists)))

(define (grid-obs-pos-lists name opl nrows ncols)
  (lists->obs-pos-lists (grid-list name (obs-pos-lists->lists opl) nrows ncols)))

;; Graphical interface
;; This is of interest only to those with a fetish for DrScheme's GUI toolkit.  It is an
;; atrocious hack and I am ashamed of it.

(define xml-doc (read-xml (open-input-file cellm-xml-file)))
(define db (xml->xexpr (document-element xml-doc)))
(define list-names (obs-pos-lists->list-labels db))
(define (write-cellm-xml opl fname)
  (let ((doc (make-document (document-prolog xml-doc) (xexpr->xml opl) (document-misc xml-doc))))
    (write-xml doc (open-output-file fname 'text 'replace))))
(define (run-generator button event)
                        (let ((key (list-ref list-names (car (send list-of-lists get-selections))))
                              (rows (string->number (send rows-entry get-value)))
                              (columns (string->number (send columns-entry get-value))))
                            (begin (set! db (grid-obs-pos-lists key db rows columns))
                                   (write-cellm-xml db cellm-xml-file)
                                   (send list-of-lists set (obs-pos-lists->list-labels db)))))

(define top-frame (new frame% (label "Grid generator")))
(define rows-entry (new text-field% (parent top-frame) (init-value "8") (label "Number of rows:")))
(define columns-entry (new text-field% (parent top-frame) (init-value "12") (label "Number of columns:")))
(define list-of-lists (new list-box% (parent top-frame) (choices list-names) (style '(single)) (label "List to grid:")))
(define button-panel (new horizontal-panel% (parent top-frame)))
(define generate-button (new button% (parent button-panel) (label "Generate") (callback run-generator)))
(define exit-button (new button% (parent button-panel) (label "Exit") (callback (lambda (button event) (exit)))))

;; Ahem: Poof!
(send top-frame show #t)