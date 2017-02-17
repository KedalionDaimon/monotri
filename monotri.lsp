; MONOTRI

; INTRODUCTION AND EXPLANATION

; This is an artificial intelligence operating on lists of symbols.
; It employs Logical Triangulation. The point of this experiment
; is to demonstrate an intelligent entity with ONLY triangles as a
; data structure. As opposed to earlier experiments, here, individual
; relations between pairs of atoms are not considered: instead, everything
; is handled by means of logical triangles, including the structuring of
; input.

; Everything consists thereby out of symbols. Sequences of three symbols,
; e.g. "A B C", are taken to symbolise a positive logical triangle of
; type vic-vic-ana, namely + A-B-C=A. If such a "triangle" is recognised
; in the input, it is "hierarchised" (i.e. it is replaced by another
; "symbol" within the input, say "X", in the input). So an input sequence as
; "K L A B C M N" would become "K L X M N". Then, the next hierarchisation
; is attempted, and so forth recursively, until finally only one or two
; symbols remain. - In the actual implementation, as the name of the "higher"
; symbol is unimportant, it is just expressed as a sub-list, i.e. actually,
; the hierarchisation is expressed as "K L (A B C) M N".

; (In the below program, the word "atom" does NOT signify a "Lisp atom", but
; such an "atom of knowledge", i.e. A, B, and C are ATOMS, and X (or (A B C))
; is an ATOM as well.)

; Observations may be congruent with a knowledge element - thus giving it
; priority, or incongruent - thus decreasing its priority, or neutral -
; leaving the knowledge element unchanged. "C B A" is congruent to "A B C",
; as both express the same logical triangle, namely + A-B-C=A; but "A C B"
; would be incongruent, expressing + A-C-B=A and contradicting thereby the
; relations of "A B C"; whereas "X Y Z", expressing + X-Y-Z=X, is entirely
; without relation to A B C and therefore considered neutral.

; The reply given to the user is the "continuation" of the present into the
; future according to experience.

; Knowledge is treated as an internal "swarm" of knowledge bases; in each
; exchange with the human a suitable "insect" for the situation is selected
; to handle the challenge. - To ensure in this some "situational awareness",
; the system also has some history of the most recently handled symbols.
; Most knowledge bases (or "insects") overlap in the "history", so the
; exchange appears to the user as an exchange with one single central
; intelligent entity.

; ----------------------------------------------------------------------------

; NEXT: VARIABLES.

; Count of re-considerations of the input - the higher the count,
; the smarter the system - and the slower its operation.
(defvar *slidings* 5)

; Which signal shows termination of a sequence of symbols.
(defvar *stop* 'SIG-TERM)

; What is the maximum count of symbols in an answer.
(defvar *trigger-answer-length* 60)

; How many chains of three symbols each can be known per knowledge area.
(defvar *elements-per-area* 1000)

; How many insects are there.
(defvar *number-of-areas* 2000)

; What is the length of the situational history... should be
; greater or equal to *trigger-answer-length*.
(defvar *history-length* 60)

; ----------------------------------------------------------------------------

; NEXT: STRUCTURING OF KNOWLEDGE AND INPUT.

; (X Y Z) is the only data, a tristructure - an implied X-Y-Z=X-triangle.

; Turn observation into tristructures - these are possibilities
; of input structuring, and in the end, one such possibility will
; be selected for the given hierarchisation step.

(defun tristruct (observation)
  (cond ((null observation) nil)
        ((null (cdr observation)) nil)
        ((null (cddr observation)) nil)
        (t (cons
          (list (car observation) (cadr observation) (caddr observation))
          (tristruct (cdr observation))))))
; (tristruct '(A B C D E)) --> ((A B C) (B C D) (C D E))

; Now, given a knowledge element and the list of tristructures,
; decide whether the "perceived reality" is either SUPPORTING the
; known element, i.e. being POSITIVE to it, or CONTRADICTING it,
; i.e. being NEGATIVE to it, or whether reality is NEUTRAL regarding
; the present knowledge element at hand. The knowledge element is the
; first argument, the observed reality - the list of tristructures
; in the second argument. The decision whether the reaction is
; POSITIVE, NEGATIVE or NEUTRAL will influence whether the knowledge
; element will receive priority, be de-prioritised, or just left where
; it currently is within the knowledge base of the presently used insect.

; STRICT VERSION OF BELOW, BUT HERE STRICTLY "SAME" ATOMS, i.e.
; A B C may only be measured against tristructures consisting out of
; exactly A, B and C. This provides the greatest "certainty" of the
; conclusions, but the conclusions are more narrow, too, as you must
; hope to actually "catch" exactly these three symbols.

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

; RELAXED VERSION OF ABOVE, WORKING WITH "VARIABLES";
; that is, not only A, B, and C lead to conclusions, but also
; A B M, B N C etc. - i.e. any triangle where at least one side
; is in common with the evaluated tristructure. This makes conclusions
; less certain - but far more likely to be found, and it allows for
; hypothetical reasoning. As it is unimportant exactly which "foreign"
; symbol is used in the triangle, it is symbolised with "?" in the comments.

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
        ((equal (list (caar tri-observation) (car el) (caddr el))
          (car tri-observation))
          'NEGATIVE)
        ((equal (list (caar tri-observation) (caddr el) (car el))
          (car tri-observation))
          'NEGATIVE)
        ((equal (list (car el) (caddr el) (caddar tri-observation))
          (car tri-observation))
          'NEGATIVE)
        ((equal (list (caddr el) (car el) (caddar tri-observation))
          (car tri-observation))
          'NEGATIVE)
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

; Hierarchisation, i.e. the structuring of input, ALSO adjusts the knowledge
; of the system which is trying to structure the input, as it decides which
; structures are appropriate, thus raising or destroying priority. (This is
; LEGAL style, not MATHEMATICS style logic.) I.e. the application of a
; conclusion on a real-world problem ALSO adjusts the knowledge base of the
; agent reasoning on the problem.

(defun proto-match-adjust-knowledge
  (knowledge tri-observation seen-knowledge rejection)
  (cond
    ((null tri-observation)
      (append '(()) knowledge))
    ((null knowledge)
      (append
        '(()) ; INITIALLY, KNOWLEDGE CANNOT CONSIST OUT OF NILs.
        ; NO "(reverse priority)":
        ; if you had seen anything, it would be the RESULT of this
        ; function!
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

; ----------------------------------------------------------------------------

; NEXT: HIERERACHISE.

; Replace tristructures in the input with a super-symbol
; (i.e., sub-list them) where the first data structure of the
; knowledge is found, unless the knowledge starts with NIL:

; ; STRICT HIERARCHISATION VERSION - hierarchising with regard to the
; ; tristructure A B C only this tristructure A B C, thus being "narrow",
; ; but "certain":
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
; ; but worked with Clisp. For compatibility, this is without REVERSE.
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

; RELAXED HIERARCHISATION VERSION - this structures NOT ONLY A B C,
; i.e. the structurisation is a more "daring" and less "certain", but
; also "faster" and "more likely applicable".

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
; but works with Clisp. Here, it is without REVERSE for compatibility:
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

; ----------------------------------------------------------------------------

; NEXT: HYPOTHETISE & HIERARCHISE UNTIL TOP (OR UNTIL HYPOTHESIS).

; That is, you know how to do ONE hierarchisation step - now recursively
; do MANY hierarchisation steps, repeatedly structuring the knowledge. I
; chose to structure it to the "top", but if you want to avoid too many
; purely "hypothetical" data structures, you may adjust the code to only
; hierarchise up to (and including) the first tristrcuture hypothesis.
; You NEED to create hypotheses, however, or the system cannot LEARN.

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

; HIERARCHISE ALL THE WAY UP.

; The joint-knowledge-observation-list consists of a list of the KNOWLEDGE
; and the HISTORY (i.e. the input to the system). The output is the MODIFIED
; knowledge and the STRUCTURED input.

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

; ----------------------------------------------------------------------------

; NEXT: RESLIDE.

; Re-sliding means to "re-consider the input several times". The structuring
; of the input is dropped each time - but the ADJUSTED KNOWLEDGE BASE is
; kept - which, done several times, makes the system structure the knowledge
; in a more generally congruent way (i.e. "more intelligently"), as early
; mis-structures become de-prioritised upon reconsideration and are replaced
; by "better" structures of more reliable priority in the knowledge.

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
; ONLY ONCE WITHOUT RESLIDING WOULD BE (FREE VERSION):
; (multi-adjust-knowledge-and-hierarchise
; (list
; '((R S X) (V M S) (I T S) (M V S) (A B C) (U N X) (C P M) (D O S) (W N T))
; '(K L A B C B A E R D O S T I T S C B A B C X Y)))
; -->
; (((K ((L A B) (C B A) (E R (D (O S T) (((I T S) C B) (A B C) X)))) Y)
;   ((C B A) (E R (D (O S T) (((I T S) C B) (A B C) X))) Y)
;   (R (D (O S T) (((I T S) C B) (A B C) X)) Y)
;   ((O S T) (((I T S) C B) (A B C) X) Y) ((A B C) X Y) (A B C) (I T S)
; (R S X) (V M S))
;  ((K ((L A B) (C B A) (E R (D (O S T) (((I T S) C B) (A B C) X)))) Y)))

; ----------------------------------------------------------------------------

; NEXT: PLAN

; Planning means extending the "present". The "present" may consist out of
; one or two symbols, at its top level. (If it consists out of more, e.g.
; because hierarchisation was terminated at a certain level of hypotheses,
; this still can be treated as "two symobols" as the last two are taken.)
; Basically, if you have "A B", you seek a "C" such as expressed by some
; tristructure "A B C" (i.e. C follows on A and B). If you have only "B",
; that is only ONE symbol is known, you try to find a "C" according to
; "A B C" or "B C D". (I.e. with one symbol, you have a better chance for
; a "hit".) If two top symbols are available, but yield not plan, you try
; then only with the last symbol again. If you CANNOT find a continuation
; even with the last atom, you DECOMPOSE it (if it is not elementary), e.g.
; "B" into "I J K", and then you try to find an "L" for either "J K" or "K".
; This you do recursively until the last symbol becomes elementary. If then
; still no continuation can be found - well, then the system replies NIL
; as it has no plan.

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

; The continuation of the present will be possibly a "structured"
; symbol, but in order to give a decent reply to the user, the
; internal knowledge structure of the reply should be removed.

(defun flatten-list (lis)
  (cond
    ((not (listp lis)) (list lis))
    ((null lis) nil)
    ((listp (car lis)) (flatten-list (append (car lis) (cdr lis))))
    (t (cons (car lis) (flatten-list (cdr lis))))))
; (flatten-list '((a ((b c) (e f))))) --> (a b c e f)

; This is like the member function with test "equal"; I do this
; in case I want to use it elswhere than Common Lisp. It shows
; whether x is a member of the list y.

(defun mmb (x y)
  (cond ((null y) nil)
        ((equal x (car y)) y) ; NOT (cdr y), as (x) would be nil!
        (t (mmb x (cdr y)))))

; When do you STOP replying? Well, either you handle I/O as
; "structures" of understanding, and then you reply simply with
; the "continuation structure" found, that is, if "L" was the
; reply to "J K", you answer with "L". OR you say - you insist
; on termination signals from I/O, in which case you try to extend
; L by M, N, O and so forth, until you reach a *stop*. To do this,
; you give the system L as hypothetical present and demand that L
; be continued. The "simple" alternative is just saying "L", the
; "sequencing" alternative tries to "sequence" structures until
; it hits *stop*.

; IT IS MORE SENSIBLE TO TURN ON THE SEQUENCING ALTERNATIVE
; IF YOU INTEND TO WORK WITH *stop* TO SIGNIFY STRUCTURE TERMINATION.

; Simple alternative.
; THUNK
(defun follow-till-termination (present knowledge general-plan)
  (let ((plan (seek-continuation present knowledge)))
    (let ((flat-plan (flatten-list plan)))
      (let ((new-gen-plan (append general-plan flat-plan)))
        (cond
          ((mmb *stop* new-gen-plan)
            (reverse (cdr (mmb *stop* (reverse new-gen-plan)))))
          (t new-gen-plan))))))

; ; Sequencing alternative.
; ; THUNK
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
;             (follow-till-termination (list plan)
;                                      knowledge
;                                      new-gen-plan)))))))

(defun generate-plan (present knowledge)
  (follow-till-termination present knowledge nil))
; (generate-plan '(K L (A (B C D) E))
;   '((P Q R) (E F G) ((B C D) E ((S T (U V W)) R (G H I))) (I J K)))
; --> (S T U V W R G H I) ; simple alternative
; --> (S T U V W R G H I J K) ; sequencing alternative, follows I into (I J K)

; ----------------------------------------------------------------------------

; NEXT: KNOWLEDGE SELECTION AND GENERATION

; THE BELOW IS ADJUSTED STRAIGHT FROM THE SYSTEM LAIAN:

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
    (with-open-file (stream "./monotri.txt"
                            :direction :output :if-exists :supersede)
        (format stream (write-to-string *knowledge-areas*))))
        ; You cannot use print or princ here.
; sample call:
; (save-knowledge)

; ----------------------------------------------------------------------------

; NEXT: PUT IT ALL TOGETHER.

; I.e. create a system that takes input, evaluates it, structures it
; according to the evaluation, changes knoweldge according to the
; evaluation, seeks a plan, and finally replies to the user.

; FIRST, A FEW AUXILIARY FUNCTIONS AND VARIABLES:

; (defun eexxiitt () '()) ; USE THIS IF YOU WANT TO DO A POST-MORTEM IN LISP.
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
; READ HUMAN INPUT:
  (progn
  (finish-output nil) (print '(HUMAN------)) (finish-output nil)
  (terpri)
  (finish-output nil) (setq *human* (read)) (finish-output nil)
  (cond
    ((null *human*) (eexxiitt))
    (t

; WITH *stop*: - consider placing the (list *stop*) actually NOT HERE...
; (setq *history* (takelast (append *history* *human* (list *stop*))
;                           *history-length*))
;
; ... SO THEN JUST DO THE "WITHOUT STOP" VERSION IN ALL CASES,
; BUT SEE BELOW...
;
; WITHOUT *stop*:
  (setq *history* (takelast (append *history* *human*)
                            *history-length*))

; SELECT EXECUTING INSECT:
  (setq *knowledge* (select-knowledge-area *history*
                                           (car *knowledge-areas*)
                                           (cdr *knowledge-areas*)))

; PERFORM EVALUATION, STRUCTURING BOTH INPUT AND KNOWLEDGE:
  (setq *hierarchy-and-knowledge* (reslider *knowledge*
                                            *history*
                                            *slidings*))
  (setq *knowledge* (car *hierarchy-and-knowledge*))
  (setq *hierarchy* (cadr *hierarchy-and-knowledge*))

; FIND PLAN / REPLY TO THE USER:
  (setq *plan* (until-terminator (generate-plan *hierarchy* *knowledge*)))
  (setq *machine* (apply-instincts
                    (takelast *plan* *trigger-answer-length*)))


; WITH *stop*:
; (cond ((null *machine*) (setq *history* *history*)) ; i.e. do nothing
;       (t (setq *history* (takelast (append *history* *machine* (list *stop*))
;                                    *history-length*))))
;
; ... YET INSTEAD OF THE ABOVE, PLACE THE STOP FOR THE HUMAN HERE, LIKE THIS:
;
; (cond ((null *machine*) (setq *history* *history*)) ; i.e. do nothing
;       (t (setq *history* (takelast (append *history* (list *stop*)
;                                            *machine*  (list *stop*))
;                                    *history-length*))))
;
; WITHOUT *stop*:
  (cond ((null *machine*) (setq *history* *history*)) ; i.e. do nothing
        (t (setq *history* (takelast (append *history* *machine*)
                                     *history-length*))))

; UPDATE KNOWLEDGE AREAS / INSECTS OF THE KNOWLEDGE SWARM:
  (setq *knowledge-areas*
          (cons (list *history* *knowledge*)
                (reverse (cdr (reverse *knowledge-areas*)))))

; REPLY TO THE USER:
  (finish-output nil)
  (print '(MACHINE----)) (finish-output nil)
  (print *machine*) (finish-output nil)
  (terpri) (finish-output nil)
  (run)))))

; "I ASSUME I NEED NO INTRODUCTION." - Lestat
(progn
(print '(LOGICAL TRIANGULATION SYSTEM))
(print '(BASED ON IMPLICIT TRIANGULATION))
(print '(USING ONLY TRIANGLES AS DATA STRUCTURE))
(print '(ENTER NIL TO TERMINATE))
(print '(ENTER LIST OF SYMBOLS TO COMMUNICATE))
(terpri)
(finish-output nil)
(run) ; COMMENT THIS OUT IF YOU WANT TO PREVENT AUTOMATIC LAUNCH ON LOADING.
)

; Side-note concerning SAINT:
; http://www.softwarepreservation.org/projects/LISP/lisp15_family/

; Side-note - paper suggested by Peter Voss, very good:
; http://www.cogsys.org/pdf/paper-1-2.pdf

