       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIBANK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TX-FILE ASSIGN TO DYNAMIC TX-PATH
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO DYNAMIC OUT-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TX-FILE.
       01  TX-LINE              PIC X(256).
       FD  OUT-FILE.
       01  OUT-LINE             PIC X(256).

       WORKING-STORAGE SECTION.
       77  TX-PATH              PIC X(256).
       77  OUT-PATH             PIC X(256).
       77  EOF                  PIC X VALUE "N".
       77  WS-LINE              PIC X(256).
       77  SEP                  PIC X VALUE ",".

       77  WS-DATE              PIC X(10).
       77  WS-ACCOUNT           PIC X(30).
       77  WS-TYPE              PIC X(6).
       77  WS-AMOUNT-STR        PIC X(20).
       77  WS-AMOUNT-SIGNED     PIC S9(13)V9(2) VALUE 0.

       77  I                    PIC 9(4) COMP VALUE 0.
       77  FOUND                PIC X VALUE "N".

       01  ACCOUNTS.
           05 ACCT-ENTRY OCCURS 100 TIMES.
              10 ACCT-NAME       PIC X(30).
              10 ACCT-BAL        PIC S9(13)V9(2).
       
       77  ACCT-IDX             PIC 9(4) COMP VALUE 1.
       77  FORMATTED-BAL        PIC -(12)9.99.

       PROCEDURE DIVISION.
       MAIN.
           MOVE "data/transactions.csv" TO TX-PATH
           MOVE "data/balances.csv" TO OUT-PATH.

           OPEN INPUT TX-FILE
           OPEN OUTPUT OUT-FILE

           PERFORM UNTIL EOF = "Y"
              READ TX-FILE
                 AT END MOVE "Y" TO EOF
                 NOT AT END
                    MOVE TX-LINE TO WS-LINE
                    PERFORM PARSE-LINE
                    PERFORM ACCUMULATE
              END-READ
           END-PERFORM

           PERFORM WRITE-HEADER
           PERFORM DUMP-BALANCES

           CLOSE TX-FILE
           CLOSE OUT-FILE
           GOBACK.

       PARSE-LINE.
      *    MOVE SPACES TO CSV-RECORD
      *    MOVE WS-LINE TO CSV-RECORD

           UNSTRING WS-LINE DELIMITED BY ALL ","
                INTO WS-DATE
                     WS-ACCOUNT
                     WS-TYPE
                     WS-AMOUNT-STR
           END-UNSTRING

           INSPECT WS-AMOUNT-STR REPLACING ALL "," BY "."
           MOVE FUNCTION NUMVAL(WS-AMOUNT-STR) 
                TO WS-AMOUNT-SIGNED.

           IF WS-TYPE = "DEBIT"
              MULTIPLY -1 BY WS-AMOUNT-SIGNED
           END-IF
           .

       ACCUMULATE.
           MOVE "N" TO FOUND
           MOVE 1 TO ACCT-IDX
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
              IF ACCT-NAME(ACCT-IDX) = WS-ACCOUNT
                 ADD WS-AMOUNT-SIGNED TO ACCT-BAL(ACCT-IDX)
                 MOVE "Y" TO FOUND
                 EXIT PERFORM
              ELSE
                 IF ACCT-NAME(ACCT-IDX) = SPACES
                    MOVE WS-ACCOUNT TO ACCT-NAME(ACCT-IDX)
                    MOVE 0 TO ACCT-BAL(ACCT-IDX)
                    ADD WS-AMOUNT-SIGNED TO ACCT-BAL(ACCT-IDX)
                    MOVE "Y" TO FOUND
                    EXIT PERFORM
                 END-IF
              END-IF
              ADD 1 TO ACCT-IDX
           END-PERFORM
           .

       WRITE-HEADER.
           MOVE "account,balance" TO OUT-LINE
           WRITE OUT-LINE
           .

       DUMP-BALANCES.
           MOVE 1 TO ACCT-IDX
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
              IF ACCT-NAME(ACCT-IDX) NOT = SPACES
                 MOVE ACCT-BAL(ACCT-IDX) TO FORMATTED-BAL
                 STRING
                   ACCT-NAME(ACCT-IDX) DELIMITED BY SPACES
                   ","                  DELIMITED BY SIZE
                   FORMATTED-BAL        DELIMITED BY SIZE
                   INTO OUT-LINE
                 END-STRING
                 WRITE OUT-LINE
              END-IF
              ADD 1 TO ACCT-IDX
           END-PERFORM
           .
