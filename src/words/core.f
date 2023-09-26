: 2DROP DROP DROP ;

: \ 10 PARSE 2DROP ; IMMEDIATE
\ This is a line comment

: ( ')' PARSE 2DROP ; IMMEDIATE
( This is an inline comment )
: .( ')' PARSE TYPE ; IMMEDIATE

: BL ( -- n ) 32 ;
: SPACE ( -- ) BL EMIT ;
: CR ( -- ) 10 EMIT ;

: DECIMAL ( -- ) #10 BASE ! ;
: HEX ( -- ) $10 BASE ! ;

: 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 ) 3 PICK 3 PICK ;

: CHARS ( n -- n ) ;
: CHAR+ ( n -- n) 1 + ;
: CELLS ( n -- n ) CELLSIZE * ;
: CELL+ ( n -- n ) CELLSIZE + ;

: 1+ 1 + ;
: 1- 1 - ;
: 2* 2 * ;

: COUNT ( c-addr -- c-addr u )
    DUP C@ SWAP 1 CHARS + SWAP
;

: ' BL WORD FIND DROP ;

: [']
    [ ' LIT DUP , , ] ,
    BL WORD FIND DROP ,
; IMMEDIATE

: [COMPILE]
    ' ,
; IMMEDIATE

: LITERAL
    ['] LIT ,
    ,
; IMMEDIATE

: RECURSE
    LATEST @ >CFA ,
; IMMEDIATE

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

: POSTPONE
    ' DUP CFA> IMMEDIATE? IF
        ,
    ELSE
        [COMPILE] LITERAL
        ['] , ,
    THEN
; IMMEDIATE

: CHAR PARSE-NAME DROP C@ ;
: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE

: SPACES
    BEGIN
        DUP 0>
    WHILE
        SPACE
        1 -
    REPEAT
    DROP
;

: VARIABLE
    CREATE 0 ,
;

: CONSTANT
    DOCOL:
    POSTPONE LITERAL
    POSTPONE EXIT
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

0 CONSTANT FALSE
FALSE INVERT CONSTANT TRUE

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
        MOVE
        S"-BUFFER SWAP
        NEXT-S"-BUFFER
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

: +! ( n | u a-addr -- )
    DUP @ ROT + SWAP !
;

: 2@ ( a-addr -- x1 x2 )
    DUP CELL+ @ SWAP @
;

: 2! ( x1 x2 a-addr -- )
    SWAP OVER ! CELL+ !
;

: DO
    POSTPONE SWAP
    POSTPONE 2>R
    0 >L HERE
    FALSE
; IMMEDIATE

: ?DO
    POSTPONE 2DUP
    POSTPONE <>
    POSTPONE IF
    POSTPONE DO
    DROP TRUE
; IMMEDIATE

: LEAVE
    ['] BRANCH ,
    L> HERE >L ,
; IMMEDIATE

: +LOOP
    SWAP
    POSTPONE 2R>
    POSTPONE +LOOP-COND
    POSTPONE 2>R
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
    POSTPONE 2R>
    POSTPONE 2DROP
    IF
        POSTPONE ELSE
        POSTPONE 2DROP
        POSTPONE THEN
    THEN
; IMMEDIATE

: LOOP
    1 POSTPONE LITERAL
    POSTPONE +LOOP
; IMMEDIATE

: I
    POSTPONE 2R@
    POSTPONE DROP
; IMMEDIATE

: J
    POSTPONE 2R>
    POSTPONE 2R@
    POSTPONE 2SWAP
    POSTPONE 2>R
    POSTPONE DROP
; IMMEDIATE

: UNLOOP
    POSTPONE 2R>
    POSTPONE 2DROP
; IMMEDIATE

: ABS ( n -- u ) DUP 0< IF NEGATE THEN ;
: WITHIN ( n_1|u_1 n_2|u_2 n_3|u_3 -- flag )
    OVER - >R - R> U<
;
: S>D ( n -- d ) DUP 0< IF -1 ELSE 0 THEN ;

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
        2DUP OR 0=
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

256 CHARS BUFFER: PAD

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
