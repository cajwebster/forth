: \ 10 PARSE DROP DROP ; IMMEDIATE
\ THIS IS A LINE COMMENT

: BL 32 ;
: SPACE BL EMIT ;
: CR 10 EMIT ;

: 1+ 1 + ;
: 1- 1 - ;
: 2* 2 * ;

: CHAR BL WORD 1+ C@ ;

: ' BL WORD FIND DROP ;

: [']
    [ ' LIT DUP , , ] ,
    BL WORD FIND DROP ,
; IMMEDIATE

: LITERAL
    ['] LIT ,
    ,
; IMMEDIATE

: [COMPILE]
    ' ,
; IMMEDIATE

: [CHAR]
    CHAR [COMPILE] LITERAL
; IMMEDIATE

: FALSE [ 0 ] LITERAL ;
: TRUE [ FALSE INVERT ] LITERAL ;

: ( ')' PARSE DROP DROP ; IMMEDIATE
: .( ')' PARSE TYPE ; IMMEDIATE
( THIS IS AN INLINE COMMENT )

: 2DROP ( x1 x2 -- ) DROP DROP ;
: 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 ) 3 PICK 3 PICK ;

: CHARS ( Do nothing because a char is an address unit ) ;
: CELL+ CELLSIZE + ;
: CHAR+ 1 + ;

: COUNT ( c-addr -- c-addr u )
    DUP C@ SWAP 1 CHARS + SWAP
;

: DECIMAL 10 BASE ! ;
: HEX 16 BASE ! ;

: IF
    ['] 0BRANCH ,
    HERE
    0 ,
; IMMEDIATE

: AHEAD
    ['] BRANCH ,
    HERE
    0 ,
; IMMEDIATE

: THEN
    DUP
    HERE SWAP -
    SWAP !
; IMMEDIATE

: ELSE
    ['] BRANCH ,
    HERE
    0 ,
    SWAP
    DUP
    HERE SWAP -
    SWAP !
; IMMEDIATE

: ABS ( n -- u )
    DUP 0< IF NEGATE THEN
;

: S>D ( n -- d )
    0 OVER 0< IF INVERT THEN
;

: BEGIN
    HERE
; IMMEDIATE

: UNTIL
    ['] 0BRANCH ,
    HERE - ,
; IMMEDIATE

: AGAIN
    ['] BRANCH ,
    HERE - ,
; IMMEDIATE

: WHILE
    ['] 0BRANCH ,
    HERE SWAP
    0 ,
; IMMEDIATE

: REPEAT
    ['] BRANCH ,
    HERE - ,
    DUP
    HERE SWAP -
    SWAP !
; IMMEDIATE

: SPACES
    BEGIN
        DUP 0>
    WHILE
        SPACE
        1 -
    REPEAT
    DROP
;

: ."
    '"' PARSE
    STATE @ IF
        \ COMPILATION
        ['] LITSTRING , \ compile LITSTRING
        DUP ,           \ compile length
        BEGIN
            ?DUP 0>
        WHILE
            SWAP        \ put addr on top of stack
            DUP C@ C,   \ compile char
            1 +         \ next char
            SWAP        \ put len on top of stack
            1 -         \ decrement remaining length
        REPEAT
        DROP            \ DROP addr
        ALIGN           \ align HERE
        ['] TYPE ,      \ compile TYPE
    ELSE
        \ INTERPRETATION
        TYPE
    THEN
; IMMEDIATE

: CELLS CELLSIZE * ;

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

: # ( ud1 -- ud2 )
    BASE @ UD/MOD 2SWAP DROP
    DUP 9 > IF
        'A' + 10 -
    ELSE
        '0' +
    THEN
    HOLD
;

: #S ( ud1 -- ud2 )
    BEGIN
        #
        2DUP D0=
    UNTIL
;

: SIGN
    0< IF [CHAR] - HOLD THEN
;

: U.R
    SWAP            \ n u
    0 <# #S #>      \ n str len
    ROT             \ str len n
    OVER - SPACES
    TYPE
;

: U.
    0 U.R SPACE
;

: .R
    SWAP
    DUP 0< IF
        SWAP 1 - SWAP
        NEGATE
        TRUE
    ELSE
        FALSE
    THEN            \ len n neg?
    ROT ROT         \ neg? len n
    0 <# #S #>      \ neg? len str slen
    ROT
    OVER - SPACES   \ neg? str slen
    ROT IF
        '-' EMIT
    THEN
    TYPE
;

: .
    0 .R SPACE
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

: ? @ . ;

: VARIABLE
    CREATE 0 ,
;

: CONSTANT
    DOCOL:
    ['] LIT ,
    ,
    ['] EXIT ,
;

: +!
    DUP @ ROT + SWAP !
;

: CMOVE ( c-addr1 c-addr2 u -- )
    BEGIN
        DUP 0>
    WHILE
        2 PICK C@ ( c-addr1 c-addr2 u c )
        2 PICK C! ( c-addr1 c-addr2 u )
        ROT 1 +
        ROT 1 +
        ROT 1 -
    REPEAT
    2DROP DROP
;

2 CONSTANT NUM-S"-BUFFERS
256 CONSTANT S"-BUFFER-SIZE
VARIABLE S"-BUFFER-IDX 0 S"-BUFFER-IDX !
CREATE S"-BUFFER[] S"-BUFFER-SIZE NUM-S"-BUFFERS * ALLOT
: S"-BUFFER ( -- c-addr )
    S"-BUFFER[] S"-BUFFER-IDX @ S"-BUFFER-SIZE * +
;
: NEXT-S"-BUFFER ( -- )
    S"-BUFFER-IDX @ 1+ NUM-S"-BUFFERS MOD
    S"-BUFFER-IDX !
;

: S"
    '"' PARSE
    STATE @ IF
        \ COMPILATION
        ['] LITSTRING , \ compile LITSTRING
        DUP ,           \ compile length
        BEGIN
            ?DUP 0>
        WHILE
            SWAP        \ put addr on top of stack
            DUP C@ C,   \ compile char
            1 +         \ next char
            SWAP        \ put len on top of stack
            1 -         \ decrement remaining length
        REPEAT
        DROP            \ DROP addr
        ALIGN           \ align HERE
    ELSE
        TUCK S"-BUFFER SWAP
        CMOVE
        S"-BUFFER SWAP
        NEXT-S"-BUFFER
    THEN
; IMMEDIATE

: RECURSE
    LATEST @ >CFA ,
; IMMEDIATE

: DO
    ['] SWAP ,
    ['] 2>R ,
    0 >L HERE
    FALSE
; IMMEDIATE

: ?DO
    ['] 2DUP ,
    ['] <> ,
    [COMPILE] IF
    [COMPILE] DO
    DROP TRUE
; IMMEDIATE

: LEAVE
    ['] BRANCH ,
    L> HERE >L ,
; IMMEDIATE

: +LOOP
    SWAP
    ['] 2R> ,
    ['] +LOOP-COND ,
    ['] 2>R ,
    ['] 0BRANCH ,
    HERE - ,
    L>
    BEGIN
        ?DUP
    WHILE
        DUP @ SWAP  ( next curr )
        DUP HERE    ( next curr curr here )
        SWAP -      ( next curr here-curr )
        SWAP !      ( next )
    REPEAT
    ['] 2R> ,
    ['] 2DROP ,
    IF
        [COMPILE] ELSE
        ['] 2DROP ,
        [COMPILE] THEN
    THEN
; IMMEDIATE

: LOOP
    1 [COMPILE] LITERAL
    [COMPILE] +LOOP
; IMMEDIATE

: I
    ['] 2R@ ,
    ['] DROP ,
; IMMEDIATE

: J
    ['] 2R> ,
    ['] 2R@ ,
    ['] 2SWAP ,
    ['] 2>R ,
    ['] DROP ,
; IMMEDIATE

: UNLOOP
    ['] 2R> ,
    ['] 2DROP ,
; IMMEDIATE

: 2@ DUP CELL+ @ SWAP @ ;
: 2! SWAP OVER ! CELL+ ! ;

: POSTPONE
    ' DUP CFA> IMMEDIATE? IF
        ,
    ELSE
        [COMPILE] LITERAL
        ['] , ,
    THEN
; IMMEDIATE

: ACCEPT ( c-addr +n_1 -- +n_2 )
    TUCK                ( n_1 c-addr n_2 )
    BEGIN
        DUP 0>          ( n_1 c-addr n_2 )
    WHILE
        SWAP KEY        ( n_1 n_2 c-addr c )
        DUP 10 = IF
            2DROP       ( n_1 n_2 )
            -           ( n_1-n_2 )
            EXIT
        THEN
        OVER C!         ( n_1 n_2 c-addr )
        CHAR+ SWAP 1-   ( n_1 c-addr n_2 )
    REPEAT
    BEGIN
        KEY 10 =
    UNTIL
    2DROP
;

: ABORT
    DEPTH ?DUP IF 0 DO DROP LOOP THEN
    QUIT
;

: ABORT"
    POSTPONE S"
    STATE @ IF
        \ COMPILATION
        POSTPONE ROT
        POSTPONE IF
        POSTPONE TYPE
        POSTPONE CR
        POSTPONE ABORT
        POSTPONE THEN
        POSTPONE 2DROP
    ELSE
        \ INTERPRETATION
        ROT
        IF
            TYPE CR
            ABORT
        THEN
        2DROP
    THEN
; IMMEDIATE

: DOES>
    ['] DOES, ,
    HERE SWAP ,
; IMMEDIATE

: :
    : FALSE 0
;

: ;
    POSTPONE ;
    BEGIN
        ?DUP
    WHILE
        DUP @ SWAP      ( next curr )
        DUP HERE SWAP - ( next curr here-curr )
        1 CELLS -
        SWAP !
    REPEAT
    IF HIDDEN LATEST @ >CFA THEN
; IMMEDIATE
2DROP

: :NONAME
    NONAME-DOCOL:
    HIDDEN
    ]
    TRUE 0
;

: WITHIN ( n_1|u_1 n_2|u_2 n_3|u_3 -- flag )
    OVER - >R - R> U<
;

: MARKER
    DOCOL:
    ['] LIT ,
    LATEST @ @ ,
    ['] LATEST ,
    ['] ! ,
    ['] EXIT ,
;

: BUFFER:
    CREATE ALLOT
;

: VALUE
    CONSTANT
;

: TO
    BL WORD FIND DROP
    2 CELLS +
    STATE @ IF
        \ compilation
        POSTPONE LITERAL
        POSTPONE !
    ELSE
        !
    THEN
; IMMEDIATE

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
        2OVER COMPARE IF
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
        BEGIN
            IN?
        WHILE
            INTERPRET
        REPEAT
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

: CASE
    0
; IMMEDIATE

: OF
    POSTPONE OVER
    POSTPONE =
    POSTPONE IF
    POSTPONE DROP
; IMMEDIATE

: ENDOF
    POSTPONE ELSE
; IMMEDIATE

: ENDCASE
    POSTPONE DROP
    BEGIN
        ?DUP
    WHILE
        POSTPONE THEN
    REPEAT
; IMMEDIATE

: C"
    '"' PARSE
    STATE @ IF
        POSTPONE LITSTRING
        DUP 1+ ,
        DUP C,
        BEGIN
            ?DUP 0>
        WHILE
            SWAP
            DUP C@ C,
            1+
            SWAP
            1-
        REPEAT
        DROP
        ALIGN
        POSTPONE DROP
    ELSE
        2DROP
    THEN
; IMMEDIATE

: COMPILE,
    ,
;

CREATE PAD 256 CHARS ALLOT

: ERASE
    BEGIN
        ?DUP 0>
    WHILE
        SWAP 0 OVER C! CHAR+
        SWAP 1-
    REPEAT
    DROP
;

: DEFER
    DOCOL:
    0 POSTPONE LITERAL
    POSTPONE EXECUTE
    POSTPONE EXIT
;

: DEFER!
    2 CELLS + !
;

: DEFER@
    2 CELLS + @
;

: IS
    STATE @ IF
        \ compilation
        POSTPONE ['] POSTPONE DEFER!
    ELSE
        ' DEFER!
    THEN
; IMMEDIATE

: ACTION-OF
    STATE @ IF
        \ compilation
        POSTPONE ['] POSTPONE DEFER@
    ELSE
        ' DEFER@
    THEN
; IMMEDIATE

: S\"-NEXT ( c-addr1 u1 -- c-addr2 u2 c )
    OVER C@ ( c-addr u c )
    DUP '\' = IF
        DROP            ( c-addr u )
        1 CHARS -       ( c-addr u )
        SWAP CHAR+      ( u c-addr )
        DUP C@ CASE     ( u c-addr c )
            'a' OF 07 ENDOF     \ alert
            'b' OF 08 ENDOF     \ backspace
            'e' OF 27 ENDOF     \ escape
            'f' OF 12 ENDOF     \ form feed
            'l' OF 10 ENDOF     \ line feed
            'm' OF -1 ENDOF     \ cr-lf
            'n' OF 10 ENDOF     \ newline
            'q' OF 34 ENDOF     \ double-quote
            'r' OF 13 ENDOF     \ carriage return
            't' OF 09 ENDOF     \ horizontal tab
            'v' OF 11 ENDOF     \ vertical tab
            'z' OF 00 ENDOF     \ null
            '"' OF 34 ENDOF     \ double-quote
            '\' OF 92 ENDOF     \ backslash
            'x' OF
                CHAR+                           ( u c-addr )
                SWAP 1 CHARS -                  ( c-addr u )
                BASE @ >R                       ( c-addr u ) ( R: base )
                HEX
                0 0 3 PICK 2 >NUMBER 2DROP DROP ( c-addr u c ) ( R: base )
                R> BASE !                       ( c-addr u c)
                ROT CHAR+                       ( u c c-addr )
                ROT 1 CHARS -                   ( c c-addr u )
                SWAP ROT                        ( u c-addr c )
            ENDOF
        ENDCASE
        >R SWAP R>  ( c-addr u c )
    THEN
    ROT CHAR+
    ROT 1-
    ROT
;

2 CONSTANT NUM-S\"-BUFFERS
256 CONSTANT S\"-BUFFER-SIZE
VARIABLE S\"-BUFFER-IDX 0 S\"-BUFFER-IDX !
CREATE S\"-BUFFER[] S\"-BUFFER-SIZE NUM-S\"-BUFFERS * ALLOT
: S\"-BUFFER ( -- c-addr )
    S\"-BUFFER[] S\"-BUFFER-IDX @ S\"-BUFFER-SIZE * +
;
: NEXT-S\"-BUFFER ( -- )
    S\"-BUFFER-IDX @ 1+ NUM-S\"-BUFFERS MOD
    S\"-BUFFER-IDX !
;

: S\"
    PARSE-S\"
    STATE @ IF
        POSTPONE LITSTRING
        HERE
        0 ,
        ROT ROT        ( len-addr s-addr s-len )
        BEGIN
            ?DUP
        WHILE
            S\"-NEXT DUP -1 = IF
                DROP
                13 C, 10 C,
            ELSE
                C,
            THEN
        REPEAT
        DROP                    ( len-addr )
        HERE OVER - 1 CELLS -   ( len-addr len )
        SWAP !
        ALIGN
    ELSE
        S\"-BUFFER ROT ROT  ( b-addr s-addr len )
        BEGIN
            ?DUP
        WHILE
            S\"-NEXT        ( b-addr s-addr len c )
            ROT ROT 2>R     ( b-addr c ) ( R: s-addr len )
            DUP -1 = IF
                DROP
                13 OVER C!
                CHAR+
                10 OVER C!
            ELSE
                OVER C!         ( b-addr ) ( R: s-addr len )
            THEN
            CHAR+ 2R>       ( b-addr s-addr len )
        REPEAT
        DROP S\"-BUFFER ( end start )
        SWAP OVER -     ( start len )
        NEXT-S\"-BUFFER
    THEN
; IMMEDIATE

: [ELSE]
    1 BEGIN
        BEGIN BL WORD COUNT DUP WHILE
            2DUP S" [IF]" COMPARE IF
                2DROP 1+
            ELSE
                2DUP S" [ELSE]" COMPARE IF
                    2DROP 1- DUP IF 1+ THEN
                ELSE
                    S" [THEN]" COMPARE IF
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

: /STRING ( c-addr_1 u_1 n -- c-addr_2 u_2 )
    ROT OVER CHARS +    ( u_1 n c-addr_2 )
    ROT ROT -           ( c-addr_2 u_2 )
;

: BIN ( fam_1 -- fam_2 ) ;

." *****************************************" CR
." * HELLO WORLD!                          *" CR
." * FORTH SYSTEM BY github.com/cajwebster *" CR
." *****************************************" CR
UNUSED DUP CELLSIZE / U. ." CELLS FREE (" 1024 / U. ." KiB)" CR
." FORTH READY" CR
