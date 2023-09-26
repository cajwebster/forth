: /STRING ( c-addr_1 u_1 n -- c-addr_2 u_2 )
    ROT OVER CHARS +    ( u_1 n c-addr_2 )
    ROT ROT -           ( c-addr_2 u_2 )
;

: BLANK ( c-addr u )
    BL FILL
;

: SLITERAL ( c-addr u -- )
    ['] LITSTRING ,
    DUP ,
    BEGIN
        ?DUP
    WHILE
        OVER C@ C,
        1-
        SWAP CHAR+ SWAP
    REPEAT
    DROP
    ALIGN
; IMMEDIATE
