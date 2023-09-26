VARIABLE HANDLER 0 HANDLER !

: CATCH
    PUSH-INPUT
    SP @ >R
    INPUT-SP @ >R
    HANDLER @ >R
    RSP @ HANDLER !
    EXECUTE
    R> HANDLER !
    2R> 2DROP
    DROP-INPUT
    0
;

: THROW
    ?DUP IF
        HANDLER @ RSP !
        R> HANDLER !
        2R> ROT >R
        INPUT-SP ! SP ! DROP R>
        POP-INPUT
    THEN
;

: ABORT -1 THROW ;

VARIABLE ABORT"-ADDR
VARIABLE ABORT"-LEN

: ABORT"
    POSTPONE S"
    STATE @ IF
        \ Compilation
        POSTPONE ROT POSTPONE IF
            POSTPONE ABORT"-LEN POSTPONE !
            POSTPONE ABORT"-ADDR POSTPONE !
            -2 POSTPONE LITERAL POSTPONE THROW
        POSTPONE THEN
        POSTPONE 2DROP
    ELSE
        ROT IF
            ABORT"-LEN !
            ABORT"-ADDR !
            -2 THROW
        THEN
        2DROP
    THEN
; IMMEDIATE

: INTERPRET
    BEGIN
        IN?
    WHILE
        BL WORD                             ( c-addr )
        DUP C@ IF                           ( c-addr )
            FIND ?DUP IF                    ( c-addr | xt immediate )
                -1 = STATE @ AND IF         ( xt )
                    ,
                ELSE
                    EXECUTE
                THEN
            ELSE
                COUNT                       ( c-addr u )
                2DUP PARSE-NUM              ( c-addr u ?num flag )
                ?DUP IF
                    CASE
                        1 OF
                            STATE @ IF              ( c-addr u num )
                                POSTPONE LITERAL    ( c-addr u )
                                2DROP               ( )
                            ELSE
                                ROT ROT             ( num c-addr u )
                                2DROP               ( num )
                            THEN
                        ENDOF
                        -1 OF
                            STATE @ IF              ( c-addr u d )
                                SWAP POSTPONE LITERAL POSTPONE LITERAL
                                2DROP
                            ELSE
                                2>R 2DROP 2R>
                            THEN
                        ENDOF
                    ENDCASE
                ELSE
                    TYPE '?' EMIT CR
                    -13 THROW
                THEN
            THEN
        ELSE
            DROP
        THEN
    REPEAT
;

: QUIT
    POSTPONE [
    R0 RSP !
    CLEAR-INPUT-STACK
    BEGIN
        REFILL
    WHILE
        ['] INTERPRET CATCH
        CASE
            0 OF STATE @ 0= IF ."  OK" CR THEN ENDOF
            POSTPONE [
            DECIMAL
            -1 OF ENDOF
            -2 OF ABORT"-ADDR @ ABORT"-LEN @ TYPE CR ENDOF
            DUP ." Exception #" . CR
        ENDCASE
    REPEAT
    BYE
;

: EVALUATE
    STR-INPUT
    INTERPRET
    POP-INPUT
;
