; CHECK MONOTRI.LSP FOR COMMENTS
; Comments below are "unclean" from the working version.
; Specialty of this version: Minimizes bistructure "punishment"
; by a NEGATIVE evaluation, keeps up POSITIVE judgement for
; evolutionary "winning" insect.

(defvar *slidings* 5)
(defvar *stop* 'SIG-TERM)
(defvar *trigger-answer-length* 60)
(defvar *elements-per-area* 1000)
(defvar *number-of-areas* 2000)
(defvar *history-length* 60)

; (X Y Z) is the only data structure - an implied X-Y-Z=X-triangle.

; turn observation into tristructures:
(defun tristruct (observation)
  (cond ((null observation) nil)
        ((null (cdr observation)) nil)
        ((null (cddr observation)) nil)
        (t (cons
          (list (car observation) (cadr observation) (caddr observation))
          (tristruct (cdr observation))))))
; (tristruct '(A B C D E)) --> ((A B C) (B C D) (C D E))

; STRICT VERSION OF BELOW, BUT HERE STRICTLY SAME ATOMS:

; (defun match-to-observation (el tri-observation)
;   (cond ((null tri-observation) nil)
;         ((equal el (car tri-observation)) 'POSITIVE)
;         ((equal (reverse el) (car tri-observation)) 'POSITIVE)
;         ((equal (list (cadr el) (car el) (caddr el))
;           (car tri-observation))
;           'NEGATIVE)
;         ((equal (reverse (list (cadr el) (car el) (caddr el)))
;           (car tri-observation))
;           'NEGATIVE)
;         ((equal (list (cadr el) (caddr el) (car el))
;           (car tri-observation))
;           'NEGATIVE)
;         ((equal (reverse (list (cadr el) (caddr el) (car el)))
;           (car tri-observation))
;           'NEGATIVE)
;         (t (match-to-observation el (cdr tri-observation)))))
; ; (match-to-observation '(A B C) '((A B C))) --> POSITIVE
; ; (match-to-observation '(B X C) '((A B C))) --> NIL
; ; (match-to-observation '(B A C) '((A B C))) --> NEGATIVE

; RELAXED VERSION OF ABOVE, WORKING WITH VARIABLES:

(defun match-to-observation (el tri-observation)
  (cond ((null tri-observation) nil)
; PRINCIPLE: X Y Z

; EXACT MATCH (though I could do only with variable match, actually):
; +
; X Y Z
; Z Y X

; -
; Y X Z
; Z X Y
; Y Z X
; X Z Y
        ((equal el (car tri-observation)) 'POSITIVE)
        ((equal (reverse el) (car tri-observation)) 'POSITIVE)
        ((equal (list (cadr el) (car el) (caddr el))
          (car tri-observation))
          'NEGATIVE)
        ((equal (reverse (list (cadr el) (car el) (caddr el)))
          (car tri-observation))
          'NEGATIVE)
        ((equal (list (cadr el) (caddr el) (car el))
          (car tri-observation))
          'NEGATIVE)
        ((equal (reverse (list (cadr el) (caddr el) (car el)))
          (car tri-observation))
          'NEGATIVE)

; VARIABLE MATCH:
; -
; ? X Z
; ? Z X
; X Z ?
; Z X ?

; +
; X ? Z
; Z ? X
;         ((equal (list (caar tri-observation) (car el) (caddr el))
;           (car tri-observation))
;           'NEGATIVE)
;         ((equal (list (caar tri-observation) (caddr el) (car el))
;           (car tri-observation))
;           'NEGATIVE)
;         ((equal (list (car el) (caddr el) (caddar tri-observation))
;           (car tri-observation))
;           'NEGATIVE)
;         ((equal (list (caddr el) (car el) (caddar tri-observation))
;           (car tri-observation))
;           'NEGATIVE)
        ((equal (list (car el) (cadar tri-observation) (caddr el))
          (car tri-observation))
          'POSITIVE)
        ((equal (list (caddr el) (cadar tri-observation) (car el))
          (car tri-observation))
          'POSITIVE)
; +
; X Y ?
; Y X ?
; Y Z ?
; Z Y ?
; ? X Y
; ? Y X
; ? Y Z
; ? Z Y
        ((equal (list (car el) (cadr el) (caddar tri-observation))
          (car tri-observation))
          'POSITIVE)
        ((equal (list (cadr el) (car el) (caddar tri-observation))
          (car tri-observation))
          'POSITIVE)
        ((equal (list (cadr el) (caddr el) (caddar tri-observation))
          (car tri-observation))
          'POSITIVE)
        ((equal (list (caddr el) (cadr el) (caddar tri-observation))
          (car tri-observation))
          'POSITIVE)
        ((equal (list (caar tri-observation) (car el) (cadr el))
          (car tri-observation))
          'POSITIVE)
        ((equal (list (caar tri-observation) (cadr el) (car el))
          (car tri-observation))
          'POSITIVE)
        ((equal (list (caar tri-observation) (cadr el) (caddr el))
          (car tri-observation))
          'POSITIVE)
        ((equal (list (caar tri-observation) (caddr el) (cadr el))
          (car tri-observation))
          'POSITIVE)

; OTHERWISE:
        (t (match-to-observation el (cdr tri-observation)))))

; hierarchisation ALSO adjusts the knowledge - 
; LEGAL style, not MATHEMATICS style logic:
(defun proto-match-adjust-knowledge
  (knowledge tri-observation seen-knowledge rejection)
  (cond
    ((null tri-observation)
      (append '(()) knowledge))
    ((null knowledge)
      (append
        '(()) ; INITIALLY, KNOWLEDGE CANNOT CONSIST OUT OF NILs.
        ; NO reverse priority:
        ; if you had seen anything, it would be the result
        (reverse seen-knowledge)
        (reverse rejection)))
    (t
      (let ((mch (match-to-observation (car knowledge) tri-observation)))
        (cond
          ((equal 'POSITIVE mch)
            (append
              '(t)
              (list (car knowledge))
              (reverse seen-knowledge)
              (cdr knowledge)
              (reverse rejection)))
          ((equal 'NEGATIVE mch)
            (proto-match-adjust-knowledge
              (cdr knowledge)
              tri-observation
              seen-knowledge
              (cons (car knowledge) rejection)))
          (t
          ; i.e. (null mch)
            (proto-match-adjust-knowledge
              (cdr knowledge)
              tri-observation
              (cons (car knowledge) seen-knowledge)
              rejection)))))))
; (proto-match-adjust-knowledge
; '((R S X) (V M S) (C B A) (U N X)) '((Z A B) (A B C) (B C D)) nil nil)
; -->
; (T (C B A) (R S X) (V M S) (U N X))
;
; (proto-match-adjust-knowledge
; '((R S X) (V M S) (B A C) (U N X)) '((Z A B) (A B C) (B C D)) nil nil)
; -->
; (NIL (R S X) (V M S) (U N X) (B A C))
;
; (proto-match-adjust-knowledge
; '((R S X) (V M S) (K L M) (U N X)) '((Z A B) (A B C) (B C D)) nil nil)
; -->
; (NIL (R S X) (V M S) (K L M) (U N X))

(defun match-adjust-knowledge (knowledge observation)
  (let ((tristrct (tristruct observation)))
    (let ((proto-result
            (proto-match-adjust-knowledge knowledge tristrct nil nil)))
      (cond
        ((null (car proto-result))
          (cond
            ((null tristrct)
              proto-result)
            (t
            ; then create a hypothesis - you could put "T", but
            ; consing a HYPOTHESIS makes clear this is not really T:
              (cons 'HYPOTHESIS
                (cons (car (reverse tristrct))
                      (cdr (reverse (cdr (reverse proto-result)))))))))
        (t
          proto-result)))))
; (match-adjust-knowledge '((R S X) (V M S) (C B A) (U N X)) '(Z A B C D))
; -->
; (T (C B A) (R S X) (V M S) (U N X))
;
; (match-adjust-knowledge '((R S X) (V M S) (C B A) (U N X)) '(Z A B V D))
; -->
; (HYPOTHESIS (B V D) (R S X) (V M S) (C B A))
;
; (match-adjust-knowledge '((R S X) (V M S) (C B A) (U N X)) '(Z A))
; -->
; (NIL (R S X) (V M S) (C B A) (U N X))

; NEXT: HIERERACHISE:
; Everywhere in the input where the first data structure of the knowledge
; is found, unless the knowledge starts with NIL:

; STRICT HIERARCHISATION VERSION:
; 
; (defun hierarchise (triatoms observation)
;   (cond ((null triatoms) observation)
;         ((null observation) observation)
;         ((null (cdr observation)) observation)
;         ((null (cddr observation)) observation)
;         ((or (equal triatoms
;                     (list (car observation)
;                           (cadr observation)
;                           (caddr observation)))
; ; I tried doing the below with REVERSE triatoms, and it FAILED in SBCL,
; ; but works with Clisp.
;              (equal triatoms
;                     (list (caddr observation)
;                           (cadr observation)
;                           (car observation))))
;           (cons (list (car observation)
;                       (cadr observation)
;                       (caddr observation))
;                 (hierarchise triatoms (cdddr observation))))
;         (t (cons (car observation)
;                  (hierarchise triatoms (cdr observation))))))
; 
; ; (hierarchise '(A B C) '(K L A B C B A E R T C B A B C X Y))
; ; -->
; ; (K L (A B C) B A E R T (C B A) B C X Y)

; RELAXED HIERARCHISATION VERSION:

(defun hierarchise (triatoms observation)
  (cond ((null triatoms) observation)
        ((null observation) observation)
        ((null (cdr observation)) observation)
        ((null (cddr observation)) observation)
        ((or (equal triatoms
                    (list (car observation)
                          (cadr observation)
                          (caddr observation)))
; I tried doing the below with REVERSE triatoms, and it FAILED in SBCL,
; but works with Clisp.
             (equal triatoms
                    (list (caddr observation)
                          (cadr observation)
                          (car observation)))

; NOW COME VARIABLES:

             (equal triatoms
                    (list (car observation)
                          (cadr triatoms)
                          (caddr observation)))

             (equal triatoms
                    (list (caddr observation)
                          (cadr triatoms)
                          (car observation)))

             (equal triatoms
                    (list (car observation)
                          (cadr observation)
                          (caddr triatoms)))

             (equal triatoms
                    (list (cadr observation)
                          (car observation)
                          (caddr triatoms)))

             (equal triatoms
                    (list (caddr observation)
                          (cadr observation)
                          (caddr triatoms)))

             (equal triatoms
                    (list (cadr observation)
                          (caddr observation)
                          (caddr triatoms)))

             (equal triatoms
                    (list (car triatoms)
                          (cadr observation)
                          (caddr observation)))

             (equal triatoms
                    (list (car triatoms)
                          (caddr observation)
                          (cadr observation)))

             (equal triatoms
                    (list (car triatoms)
                          (car observation)
                          (cadr observation)))

             (equal triatoms
                    (list (car triatoms)
                          (cadr observation)
                          (car observation))))

          (cons (list (car observation)
                      (cadr observation)
                      (caddr observation))
                (hierarchise triatoms (cdddr observation))))
        (t (cons (car observation)
                 (hierarchise triatoms (cdr observation))))))

; (hierarchise '(A B C) '(K L A B C B A E R T C B A B C X Y))
; -->
; (K (L A B) (C B A) E R (T C B) (A B C) X Y)


; NEXT: HYPOTHETISE & HIERARCHISE UNTIL TOP OR UNTIL HYPOTHESIS

(defun single-adjust-knowledge-and-hierarchise
          (joint-knowledge-observation-list)
  (let ((kno (car joint-knowledge-observation-list))
        (obs (cadr joint-knowledge-observation-list)))
    (cond
      ; if knowledge is null or the observation is too short, do not compute:
      ((or (null kno) (null obs)) (cons nil joint-knowledge-observation-list))
      ((or (null (cdr obs))) (cons nil joint-knowledge-observation-list))
      ((or (null (cddr obs))) (cons nil joint-knowledge-observation-list))
      (t
        (let ((adjusted-kno (match-adjust-knowledge kno obs)))
          (cond
            ((null (car adjusted-kno))
              (list (car adjusted-kno) (cdr adjusted-kno) obs))
            (t
              (list (car adjusted-kno) (cdr adjusted-kno)
                    (hierarchise (cadr adjusted-kno) obs)))))))))
; (single-adjust-knowledge-and-hierarchise
; (list
; '((R S X) (V M S) (I T S) (M V S) (A B C) (U N X) (C P M) (D O S) (W N T))
; '(K L A B C B A E R T C B A B C X Y)))
; -->
; (T ((A B C) (R S X) (V M S) (I T S) (M V S) (U N X) (C P M) (D O S) (W N T))
;  (K L (A B C) B A E R T (C B A) B C X Y))

; NEXT: RESLIDE

(defun multi-adjust-knowledge-and-hierarchise
        (joint-knowledge-observation-list)
  (let ((adjustment
          (single-adjust-knowledge-and-hierarchise
            joint-knowledge-observation-list)))
    (cond
    ; HERE, I COULD CATCH HYPOTHESES IF I DISLIKED THEM.
    ; So far, I hierarchise till the top, creating hypotheses:
      ((or (null (car adjustment))
           ; the below alternative is in case of lack of hierarchisation:
           (equal (caddr adjustment) (cadr joint-knowledge-observation-list)))
        joint-knowledge-observation-list)
      (t
        (multi-adjust-knowledge-and-hierarchise (cdr adjustment))))))
; [Using the "strict" version above:]
; (multi-adjust-knowledge-and-hierarchise
; (list
; '((R S X) (V M S) (I T S) (M V S) (A B C) (U N X) (C P M) (D O S) (W N T))
; '(K L A B C B A E R D O S T I T S C B A B C X Y)))
; -->
; (((K L ((A B C) B (A E (R D (O (S T I) (T S ((C B A) B (C X Y))))))))
;  ((A B C) B (A E (R D (O (S T I) (T S ((C B A) B (C X Y)))))))
;  (A E (R D (O (S T I) (T S ((C B A) B (C X Y))))))
;  (R D (O (S T I) (T S ((C B A) B (C X Y)))))
;  (O (S T I) (T S ((C B A) B (C X Y)))) (T S ((C B A) B (C X Y)))
;  ((C B A) B (C X Y)) (C X Y) (A B C))
; ((K L ((A B C) B (A E (R D (O (S T I) (T S ((C B A) B (C X Y))))))))))

; The reslider ONLY adjusts the knowledge several times,
; until after the final knowledge adjustment it undertakes hierarchisation
; "for real".
(defun reslider (knowledge observation slide-count)
  (cond
    ((zerop slide-count)
      (multi-adjust-knowledge-and-hierarchise (list knowledge observation)))
    (t
      (let ((mutation 
              (multi-adjust-knowledge-and-hierarchise
                (list knowledge observation))))
        (reslider (car mutation) observation (- slide-count 1))))))
; (reslider
; '((R S X) (V M S) (I T S) (M V S) (A B C) (U N X) (C P M) (D O S) (W N T))
; '(K L A B C B A E R D O S T I T S C B A B C X Y)
; 3)
; --> STRICT VERSION:
; (((K L (A B (C B (A E (R D (O S (T I (T S (C B (A B (C X Y)))))))))))
;   (A B (C B (A E (R D (O S (T I (T S (C B (A B (C X Y))))))))))
;   (C B (A E (R D (O S (T I (T S (C B (A B (C X Y)))))))))
;   (A E (R D (O S (T I (T S (C B (A B (C X Y))))))))
;   (R D (O S (T I (T S (C B (A B (C X Y)))))))
;   (O S (T I (T S (C B (A B (C X Y)))))) (T I (T S (C B (A B (C X Y)))))
;   (T S (C B (A B (C X Y)))) (C B (A B (C X Y))))
;  ((K L (A B (C B (A E (R D (O S (T I (T S (C B (A B (C X Y)))))))))))))
; --> FREE VERSION:
; (((K ((L A B) (C B A) (E R (D (O S T) (I T ((S C B) (A B C) X))))) Y)
;   ((C B A) (E R (D (O S T) (I T ((S C B) (A B C) X)))) Y)
;   (R (D (O S T) (I T ((S C B) (A B C) X))) Y)
;   ((O S T) (I T ((S C B) (A B C) X)) Y) (I T S) ((A B C) X Y) (A B C)
;   (K ((L A B) (C B A) (E R (D (O S T) (((I T S) C B) (A B C) X)))) Y)
;   ((C B A) (E R (D (O S T) (((I T S) C B) (A B C) X))) Y))
;  ((K ((L A B) (C B A) (E R (D (O S T) (I T ((S C B) (A B C) X))))) Y)))
;
; ONLY ONCE WOULD BE (FREE VERSION):
; (multi-adjust-knowledge-and-hierarchise
; (list
; '((R S X) (V M S) (I T S) (M V S) (A B C) (U N X) (C P M) (D O S) (W N T))
; '(K L A B C B A E R D O S T I T S C B A B C X Y)))
; -->
; (((K ((L A B) (C B A) (E R (D (O S T) (((I T S) C B) (A B C) X)))) Y)
;   ((C B A) (E R (D (O S T) (((I T S) C B) (A B C) X))) Y)
;   (R (D (O S T) (((I T S) C B) (A B C) X)) Y)
;   ((O S T) (((I T S) C B) (A B C) X) Y) ((A B C) X Y) (A B C) (I T S) (R S X)
;   (V M S))
;  ((K ((L A B) (C B A) (E R (D (O S T) (((I T S) C B) (A B C) X)))) Y)))

; NEXT: PLAN

(defun two-seek-continuation (two-atoms knowledge)
  (cond
    ((null knowledge)
      nil)
    ((equal (list (caar knowledge) (cadar knowledge)) two-atoms)
      (caddar knowledge))
    (t
      (two-seek-continuation two-atoms (cdr knowledge)))))
; (two-seek-continuation '(A B)
; '((R S X) (V M S) (I T S) (M V S) (A B C) (U N X) (C P M) (D O S) (W N T)))
; --> C
; (two-seek-continuation '(F G)
; '((R S X) (V M S) (I T S) (M V S) (A B C) (U N X) (C P M) (D O S) (W N T)))
; --> NIL

(defun one-seek-continuation (one-atom knowledge)
  (cond
    ((null knowledge)
      nil)
    ((equal (caar knowledge) one-atom)
      (cadar knowledge))
    ((equal (cadar knowledge) one-atom)
      (caddar knowledge))
    (t
      (one-seek-continuation one-atom (cdr knowledge)))))
; (one-seek-continuation 'A
; '((R S X) (V M S) (I T S) (M V S) (A B C) (U N X) (C P M) (D O S) (W N T)))
; --> B
; (one-seek-continuation 'B
; '((R S X) (V M S) (I T S) (M V S) (A B C) (U N X) (C P M) (D O S) (W N T)))
; --> C
; (one-seek-continuation 'K
; '((R S X) (V M S) (I T S) (M V S) (A B C) (U N X) (C P M) (D O S) (W N T)))
; --> NIL

(defun seek-continuation (present knowledge)
  (cond
    ((null knowledge) nil)
    ((null present) nil)
    ((null (cdr present))
    ; that is, only ONE atom available:
      (let ((one-res (one-seek-continuation (car present) knowledge)))
        (cond
          ((not (null one-res))
            one-res)
          ((and (null one-res) (not (listp (car present))))
            nil)
          (t
          ; that is, we can de-compose:
            (seek-continuation (car present) knowledge)))))
    (t
    ; that is, the present is longer than one atom:
      (let ((revpres (reverse present)))
        (let ((two-result
                (two-seek-continuation
                  (list (cadr revpres) (car revpres))
                  knowledge)))
          (cond
            ((not (null two-result))
              two-result)
            (t
              (seek-continuation (list (car revpres)) knowledge))))))))

; (seek-continuation
;   '(K L (A (B C D) E)) '((P Q R) (E F G) ((B C D) E H) (I J K)))
; --> H
; (seek-continuation
;   '(K L (A (B C D) E)) '((P Q R) (E F G) ((B C X) E H) (I J K)))
; --> F
; (seek-continuation
;   '(K L (A (B C D) N)) '((P Q R) (E F G) ((B C X) E H) (I J K)))
; --> NIL

(defun flatten-list (lis)
  (cond
    ((not (listp lis)) (list lis))
    ((null lis) nil)
    ((listp (car lis)) (flatten-list (append (car lis) (cdr lis))))
    (t (cons (car lis) (flatten-list (cdr lis))))))
; (flatten-list '((a ((b c) (e f))))) --> (a b c e f)

(defun mmb (x y)
  (cond ((null y) nil)
        ((equal x (car y)) y)
        (t (mmb x (cdr y)))))

; IT IS MORE SENSIBLE TO TURN ON THE SEQUENCING ALTERNATIVE
; IF YOU INTEND TO WORK WITH *stop* TO SIGNIFY STRUCTURE TERMINATION:

; ; simple alternative:
; ; THUNK
(defun follow-till-termination (present knowledge general-plan)
  (let ((plan (seek-continuation present knowledge)))
    (let ((flat-plan (flatten-list plan)))
      (let ((new-gen-plan (append general-plan flat-plan)))
        (cond
          ((mmb *stop* new-gen-plan)
            (reverse (cdr (mmb *stop* (reverse new-gen-plan)))))
          (t new-gen-plan))))))

; sequencing alternative:
; THUNK
; (defun follow-till-termination (present knowledge general-plan)
;   (let ((plan (seek-continuation present knowledge)))
;     (let ((flat-plan (flatten-list plan)))
;       (let ((new-gen-plan (append general-plan flat-plan)))
;         (cond
;           ((mmb *stop* new-gen-plan)
;             (reverse (cdr (mmb *stop* (reverse new-gen-plan)))))
;           ((or (null plan)
;              (< *trigger-answer-length* (length new-gen-plan)))
;             new-gen-plan)
;           (t
;             (follow-till-termination (list plan) knowledge new-gen-plan)))))))

(defun generate-plan (present knowledge)
  (follow-till-termination present knowledge nil))
; (generate-plan '(K L (A (B C D) E))
;   '((P Q R) (E F G) ((B C D) E ((S T (U V W)) R (G H I))) (I J K)))
; --> (S T U V W R G H I) ; simple alternative
; --> (S T U V W R G H I J K) ; sequencing alternative, follows I into (I J K)

; ----------------------------------------------------------------------------

; THE BELOW IS ADJUSTED STRAIGHT FROM LAIAN:

; KNOWLEDGE SELECTION AND GENERATION

; Now that you have everything needed to create a plan,
; define a function to select the knowledge area which shall be
; used for the analysis of the input substrate. For this,
; simply pick the area which is most similar to the "problem"
; at hand (i.e. the relevant section of input history).
; The "candidate" will be the car of the knowledge-areas,
; and what is called below as "knowledge-areas" is the
; cdr of the knowledge-areas.

(defun set-intersection (set1 set2)
  (cond ((null set1) nil)
        ((and (not (mmb (car set1) (cdr set1)))
              (mmb (car set1) set2))
          (cons (car set1) (set-intersection (cdr set1) set2)))
        (t (set-intersection (cdr set1) set2))))
; (set-intersection '(A B C D E) '(X Y C D Z)) --> (C D)

; select knowledge area to use:
(defun select-knowledge-area (problem candidate knowledge-areas)
  (cond
; car of candidate is the history, cadr of candidate - the knowledge
    ((null knowledge-areas) (cadr candidate))
    ((< (length (set-intersection (car candidate) problem))
        (length (set-intersection (caar knowledge-areas) problem)))
      (select-knowledge-area problem
                             (car knowledge-areas)
                             (cdr knowledge-areas)))
    (t (select-knowledge-area problem candidate (cdr knowledge-areas)))))

; (select-knowledge-area
; '(A B C D E)
; '((A B X Y Z) (KNOWLEDGE 1))
; '(((B X Y Z Q) (KNOWLEDGE 2))
; ((A B C Y Z) (KNOWLEDGE 3))
; ((A B X C Z) (KNOWLEDGE 4))))
; -->
; (KNOWLEDGE 3)

; Knowledge generation:
; Normally, you will load knowledge from a file.
; But if no file exists yet, knowledge may be generated:

(defun gen-knowledge-area (cntr)
  (cond ((zerop cntr) nil)
        (t (cons (list nil nil nil)
                 (gen-knowledge-area (- cntr 1))))))

(defun gen-multiple-knowledge-areas (cntr)
  (cond ((zerop cntr) nil)
        (t (cons (list nil (gen-knowledge-area *elements-per-area*))
                 (gen-multiple-knowledge-areas (- cntr 1))))))

(defun gen-knowledge ()
  (gen-multiple-knowledge-areas *number-of-areas*))

(defun load-knowledge ()
    (cond ((null (probe-file "monotri.txt"))
            (gen-knowledge))
          (t
            (with-open-file (stream "./monotri.txt")
                (read stream)))))

; LOAD the knowledge:
(defvar *knowledge-areas* (load-knowledge))

; SAVE the knowledge:
(defun save-knowledge ()
    (with-open-file (stream "./monotri.txt" :direction :output :if-exists :supersede)
        (format stream (write-to-string *knowledge-areas*)))) ; You cannot use print or princ here.
; sample call:
; (save-knowledge)

; --------------------------------------------------------

; EXECUTION PHASE:
; READ / HISTORISE
; HIERARCHISE
; ANALYSE NOTIONS
; PLAN / OUTPUT

; (defun eexxiitt () '())
(defun eexxiitt () (progn (terpri) (save-knowledge) (quit)))

; a few functions to take whole sections of lists, always counting from 1:
(defun proto-takefirst (fromwhere howmany resultlist)
  (if (or (null fromwhere) (zerop howmany)) (reverse resultlist)
    (proto-takefirst
      (cdr fromwhere) (- howmany 1) (cons (car fromwhere) resultlist))))

 ; take the front part of a list:
(defun takefirst (fromwhere howmany) (proto-takefirst fromwhere howmany '()))

; take the final part of a list:
(defun takelast (fromwhere howmany)
  (reverse (takefirst (reverse fromwhere) howmany)))

; setup instincts
(defun word-instinct (word)
    (cond ((equal word 'I) 'YOU)
          ((equal word 'ME) 'YOU)
          ((equal word 'YOU) 'ME)
          ((equal word 'AM) 'ARE)
          ((equal word 'ARE) 'AM-ARE)
          ((equal word 'MINE) 'YOURS)
          ((equal word 'YOURS) 'MINE)
          ((equal word 'MY) 'YOUR)
          ((equal word 'YOUR) 'MY)
          ((equal word 'MYSELF) 'YOURSELF)
          ((equal word 'YOURSELF) 'MYSELF)
          ((equal word 'WAS) 'WAS-WERE)
          (T word)))

(defun proto-apply-instincts (sentence checked-sentence)
    (cond ((null sentence)
            (reverse checked-sentence))
          (T
            (proto-apply-instincts
                (cdr sentence)
                (cons (word-instinct (car sentence)) checked-sentence)))))

(defun apply-instincts (sentence)
    (proto-apply-instincts sentence '()))
; sample call: (apply-instincts '(I WAS HERE TODAY))
; --> (YOU WAS-WERE HERE TODAY)

; Even when you have a plan, talk only until as long as
; the interaction should not be terminated. The rest of
; the plan is NOT executed.
; THUNK using SIG-TERM:
(defun until-terminator (lis)
  (cond ((null lis) nil)
        ((equal *stop* (car lis)) nil)
        (t (cons (car lis) (until-terminator (cdr lis))))))
; (until-terminator '(A B C D SIG-TERM E F G H)) --> (A B C D)

(defvar *knowledge* nil)
(defvar *human* nil)
(defvar *history* nil)
(defvar *machine* nil)
(defvar *hierarchy* nil)
(defvar *hierarchy-and-knowledge* nil)
(defvar *plan* nil)

; VARIANTS FOR USING *stop* AS WELL AS NOT USING IT ARE IMPLEMENTED
; - THE DEFAULT HERE IS NOT USING IT.

(defun run ()
  (progn
  (finish-output nil) (print '(HUMAN------)) (finish-output nil)
  (terpri)
  (finish-output nil) (setq *human* (read)) (finish-output nil)
  (cond
    ((null *human*) (eexxiitt))
    (t

; WITH *stop*:
; (setq *history* (takelast (append *history* *human* (list *stop*))
;                           *history-length*))
; WITHOUT *stop*:
  (setq *history* (takelast (append *history* *human*)
                            *history-length*))


  (setq *knowledge* (select-knowledge-area *history*
                                           (car *knowledge-areas*)
                                           (cdr *knowledge-areas*)))
  (setq *hierarchy-and-knowledge* (reslider *knowledge*
                                            *history*
                                            *slidings*))
  (setq *knowledge* (car *hierarchy-and-knowledge*))
  (setq *hierarchy* (cadr *hierarchy-and-knowledge*))

  (setq *plan* (until-terminator (generate-plan *hierarchy* *knowledge*)))
  (setq *machine* (apply-instincts
                    (takelast *plan* *trigger-answer-length*)))


; WITH *stop*:
; (cond ((null *machine*) (setq *history* *history*)) ; i.e. do nothing
;       (t (setq *history* (takelast (append *history* *machine* (list *stop*))
;                                    *history-length*))))
; WITHOUT *stop*:
  (cond ((null *machine*) (setq *history* *history*)) ; i.e. do nothing
        (t (setq *history* (takelast (append *history* *machine*)
                                     *history-length*))))


  (setq *knowledge-areas*
          (cons (list *history* *knowledge*)
                (reverse (cdr (reverse *knowledge-areas*)))))
  (finish-output nil)
  (print '(MACHINE----)) (finish-output nil)
  (print *machine*) (finish-output nil)
  (terpri) (finish-output nil)
  (run)))))

(progn
(print '(LOGICAL TRIANGULATION SYSTEM))
(print '(BASED ON IMPLICIT TRIANGULATION))
(print '(USING ONLY TRIANGLES AS DATA STRUCTURE))
(print '(ENTER NIL TO TERMINATE))
(print '(ENTER LIST OF SYMBOLS TO COMMUNICATE))
(terpri)
(finish-output nil)
(run)
)


