IDENTIFICATION DIVISION.
PROGRAM-ID.    john-paul-welsh-cipher.

ENVIRONMENT DIVISION.

DATA DIVISION.

WORKING-STORAGE SECTION.

01  str-input       PIC X(15)  VALUE "John Paul Welsh".
01  curr-char       PIC X.
01  chr-shift       PIC S99    VALUE 28.
01  max-shift       PIC S99    VALUE 28.
01  encrypt-input   PIC X(15).
01  encrypt-output  PIC X(16).
01  decrypt-input   PIC X(15).
01  decrypt-output  PIC X(16).
01  solve-input     PIC X(15).
01  solve-output    PIC X(16).
01  iteration       PIC S99.
01  this-char       PIC X.
01  this-index      PIC 99.
01  new-char        PIC X.
01  str-pos         PIC 99     VALUE 01.
01  new-s-index     PIC S99.
01  new-us-index    PIC 99.
01  table-alpha.
    05  idx-char-pair OCCURS 26 TIMES INDEXED BY idx.
        10 table-idx       PIC 99.
        10 table-val       PIC X.

PROCEDURE DIVISION.

PROGRAM-BEGIN.
    MOVE "A" TO idx-char-pair(1).
    MOVE "B" TO idx-char-pair(2).
    MOVE "C" TO idx-char-pair(3).
    MOVE "D" TO idx-char-pair(4).
    MOVE "E" TO idx-char-pair(5).
    MOVE "F" TO idx-char-pair(6).
    MOVE "G" TO idx-char-pair(7).
    MOVE "H" TO idx-char-pair(8).
    MOVE "I" TO idx-char-pair(9).
    MOVE "J" TO idx-char-pair(10).
    MOVE "K" TO idx-char-pair(11).
    MOVE "L" TO idx-char-pair(12).
    MOVE "M" TO idx-char-pair(13).
    MOVE "N" TO idx-char-pair(14).
    MOVE "O" TO idx-char-pair(15).
    MOVE "P" TO idx-char-pair(16).
    MOVE "Q" TO idx-char-pair(17).
    MOVE "R" TO idx-char-pair(18).
    MOVE "S" TO idx-char-pair(19).
    MOVE "T" TO idx-char-pair(20).
    MOVE "U" TO idx-char-pair(21).
    MOVE "V" TO idx-char-pair(22).
    MOVE "W" TO idx-char-pair(23).
    MOVE "X" TO idx-char-pair(24).
    MOVE "Y" TO idx-char-pair(25).
    MOVE "Z" TO idx-char-pair(26).

    DISPLAY "The input string is " str-input.
    DISPLAY "The shift amount is " chr-shift.
    DISPLAY "The max shift amount is " max-shift.

    SET str-input TO FUNCTION UPPER-CASE(str-input).
    SET chr-shift TO FUNCTION MOD(chr-shift, 26).

DO-ENCRYPTION.
    MOVE str-input TO encrypt-input.
    PERFORM 15 TIMES
        MOVE encrypt-input(str-pos:str-pos) TO curr-char
        PERFORM TABLE-SEARCH
 
        ADD this-index, chr-shift GIVING new-s-index

        IF new-s-index > 26 THEN
            COMPUTE new-s-index = new-s-index - 26
        END-IF

        IF new-s-index < 1 THEN
            COMPUTE new-s-index = new-s-index + 26
        END-IF

        MOVE new-s-index TO new-us-index

        MOVE idx-char-pair(new-us-index) TO new-char
        INSPECT curr-char CONVERTING this-char TO new-char

        IF curr-char = " " THEN
            STRING " " encrypt-output INTO encrypt-output
        ELSE
            STRING curr-char encrypt-output INTO encrypt-output
        END-IF
         
        ADD str-pos, 1 GIVING str-pos
    END-PERFORM.
         
    SET encrypt-output TO FUNCTION REVERSE(encrypt-output).
    DISPLAY "Encrypted: " encrypt-output.
    
DO-DECRYPTION.
    MOVE str-input TO decrypt-input.
    SET str-pos TO 1.
    PERFORM 15 TIMES
        MOVE decrypt-input(str-pos:str-pos) TO curr-char
        PERFORM TABLE-SEARCH

        SUBTRACT chr-shift FROM this-index GIVING new-s-index

        IF new-s-index > 26 THEN
            COMPUTE new-s-index = new-s-index - 26
        END-IF

        IF new-s-index < 1 THEN
            COMPUTE new-s-index = new-s-index + 26
        END-IF

        MOVE new-s-index TO new-us-index

        MOVE idx-char-pair(new-us-index) TO new-char
        INSPECT curr-char CONVERTING this-char TO new-char

        IF curr-char = " " THEN
            STRING " " decrypt-output INTO decrypt-output
        ELSE
            STRING curr-char decrypt-output INTO decrypt-output
        END-IF

        ADD str-pos, 1 GIVING str-pos
    END-PERFORM.

    SET decrypt-output TO FUNCTION REVERSE(decrypt-output).
    DISPLAY "Decrypted: " decrypt-output.

DO-SOLVE.
    MOVE str-input TO solve-input.
    MOVE max-shift TO iteration.
    ADD max-shift, 1 GIVING max-shift.

    PERFORM max-shift TIMES
        SET str-pos TO 1
        PERFORM 15 TIMES
            MOVE solve-input(str-pos:str-pos) TO curr-char
            PERFORM TABLE-SEARCH
             
            SUBTRACT iteration FROM this-index GIVING new-s-index

            IF new-s-index > 26 THEN
                COMPUTE new-s-index = new-s-index - 26
            END-IF

            IF new-s-index < 1 THEN
                COMPUTE new-s-index = new-s-index + 26
            END-IF

            MOVE new-s-index TO new-us-index
             
            MOVE idx-char-pair(new-us-index) TO new-char
            INSPECT curr-char CONVERTING this-char TO new-char

            IF curr-char = " " THEN
                STRING " " solve-output INTO solve-output
            ELSE
                STRING curr-char solve-output INTO solve-output
            END-IF

            ADD str-pos, 1 GIVING str-pos
        END-PERFORM

        SET solve-output TO FUNCTION REVERSE(solve-output)
        DISPLAY "Caesar " iteration ": " solve-output
        
        SUBTRACT 1 FROM iteration GIVING iteration
    END-PERFORM.

    PERFORM PROGRAM-DONE.

TABLE-SEARCH.
    SET idx to 1.
    SEARCH idx-char-pair
    WHEN idx-char-pair(idx) = curr-char
        MOVE idx-char-pair(idx) TO this-char
        MOVE idx TO this-index.

PROGRAM-DONE.
    STOP RUN.
