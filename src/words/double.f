: 2LITERAL ( x1 x2 -- )
    SWAP
    POSTPONE LITERAL POSTPONE LITERAL
; IMMEDIATE

: 2CONSTANT ( x1 x2 "<spaces>name" -- )
    DOCOL:
    POSTPONE 2LITERAL
    POSTPONE EXIT
;

: 2VARIABLE
    CREATE 2 CELLS ALLOT
;

: D>S ( d -- n ) DROP ;

: DABS ( d -- ud )
    2DUP D0< IF
        DNEGATE
    THEN
;

: D.R ( d n -- )
    ROT ROT ( n d )
    2DUP D0< IF
        2>R 1- 2R>
        DNEGATE
        TRUE
    ELSE
        FALSE
    THEN
    SWAP 2SWAP ROT
    <# #S #>
    ROT
    OVER - SPACES
    ROT IF
        '-' EMIT
    THEN
    TYPE
;

: D. ( d -- )
    0 D.R
;

: 2ROT ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
    2>R 2SWAP 2R> 2SWAP
;

: 2VALUE ( x1 x2 "<spaces>name" )
    2CONSTANT
;

: TO
    BL WORD FIND DROP
    2 CELLS +
    DUP CELL+ @ ['] LIT = IF
        \ 2VALUE
        STATE @ IF
            POSTPONE LITERAL
            POSTPONE SWAP POSTPONE OVER 2 CELLS POSTPONE LITERAL POSTPONE +
            POSTPONE !
            POSTPONE !
        ELSE
            SWAP OVER 2 CELLS + !
            !
        THEN
    ELSE
        STATE @ IF
            \ compilation
            POSTPONE LITERAL
            POSTPONE !
        ELSE
            !
        THEN
    THEN
; IMMEDIATE
