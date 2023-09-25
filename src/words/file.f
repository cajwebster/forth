\ Linked list of already included files
0 VALUE INCLUDED-LIST

: APPEND-INCLUDED ( c-addr u -- )
    ALIGN
    \ set included-list to HERE and compile a link to the previous entry
    HERE INCLUDED-LIST , TO INCLUDED-LIST
    \ compile len
    DUP C,
    BEGIN
        ?DUP
    WHILE
        SWAP DUP C@ C, 1 CHARS +
        SWAP 1-
    REPEAT
    DROP
;

: INCLUDED? ( c-addr u -- flag)
    INCLUDED-LIST
    BEGIN
        ?DUP
    WHILE
        DUP 1 CELLS +   ( c-addr1 u curr c-addr2 )
        2SWAP ROT COUNT ( curr c-addr1 u1 c-addr2 u2 )
        2OVER S= IF
            2DROP DROP TRUE
            EXIT
        THEN
        ROT @
    REPEAT
    2DROP FALSE
;

: INCLUDED ( i * x c-addr u -- j * x )
    2DUP APPEND-INCLUDED
    FILE-INPUT

    BEGIN
        REFILL
    WHILE
        INTERPRET
    REPEAT

    POP-INPUT
;

: INCLUDE
    PARSE-NAME INCLUDED
;

: REQUIRED ( i * x c-addr u -- j * x )
    2DUP INCLUDED? INVERT IF
        INCLUDED
    ELSE
        2DROP
    THEN
;

: REQUIRE ( i * x "name" -- j * x )
    PARSE-NAME REQUIRED
;

: BIN ( fam_1 -- fam_2 ) ;

: (
    SOURCE-ID 0> IF
        \ reading from file
        BEGIN
            IN? INVERT IF
                REFILL INVERT IF
                    EXIT
                THEN
            THEN
            ')' PARSE 2DUP + SOURCE OVER + WITHIN IF
                + C@ ')' = IF
                    EXIT
                THEN
            ELSE
                2DROP
            THEN
        AGAIN
    ELSE
        POSTPONE (
    THEN
; IMMEDIATE
