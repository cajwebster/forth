: [ELSE]
    1 BEGIN
        BEGIN BL WORD COUNT DUP WHILE
            2DUP S" [IF]" S= IF
                2DROP 1+
            ELSE
                2DUP S" [ELSE]" S= IF
                    2DROP 1- DUP IF 1+ THEN
                ELSE
                    S" [THEN]" S= IF
                        1-
                    THEN
                THEN
            THEN ?DUP 0= IF EXIT THEN
        REPEAT 2DROP
    REFILL 0= UNTIL
    DROP
; IMMEDIATE

: [IF]
    0= IF POSTPONE [ELSE] THEN
; IMMEDIATE

: [THEN] ; IMMEDIATE

: CS-PICK PICK ;
: CS-ROLL ROLL ;

: [DEFINED]
    BL WORD FIND
    NIP 0<>
; IMMEDIATE

: [UNDEFINED]
    POSTPONE [DEFINED] INVERT
; IMMEDIATE

: N>R
    DUP
    BEGIN
        DUP
    WHILE
        ROT R> SWAP >R >R
        1-
    REPEAT
    DROP
    R> SWAP >R >R
;

: NR>
    R> R> SWAP >R DUP
    BEGIN
        DUP
    WHILE
        R> R> SWAP >R ROT ROT
        1-
    REPEAT
    DROP
;

: SYNONYM
    DOCOL: IMMEDIATE
    ' POSTPONE LITERAL
    POSTPONE DUP POSTPONE IMMEDIATE?
    POSTPONE STATE POSTPONE @ POSTPONE 0=
    POSTPONE OR POSTPONE IF
        POSTPONE EXECUTE
    POSTPONE ELSE
        POSTPONE COMPILE,
    POSTPONE THEN
    POSTPONE EXIT
;

: .S
    ." ( "
    DEPTH
    BEGIN
        DUP 0>
    WHILE
        DUP PICK .
        1 -
    REPEAT
    DROP
    ." )" CR
;

: WORDS ( -- )
    LATEST @                            \ get address of current dict entry
    BEGIN
        ?DUP                            \ while (curr != null)
    WHILE
        DUP 1 CELLS + 32 TYPE SPACE     \ TYPE name
        @                               \ next word in dictionary
    REPEAT
    CR
;

: ? @ . ;
