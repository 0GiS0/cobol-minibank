      ******************************************************************
      * COBOL MINIBANK - SISTEMA BANCARIO CON DB2
      * Procesa transacciones via scripts CLI de DB2
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIBANK-DB2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TX-FILE ASSIGN TO TX-PATH
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO OUT-PATH
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DB-BALANCES ASSIGN TO DB-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD  TX-FILE.
       01  TX-LINE              PIC X(256).
       FD  OUT-FILE.
       01  OUT-LINE             PIC X(256).
       FD  DB-BALANCES.
       01  DB-BAL-LINE          PIC X(256).

       WORKING-STORAGE SECTION.

       77  TX-PATH              PIC X(256).
       77  OUT-PATH             PIC X(256).
       77  DB-PATH              PIC X(256).
       77  CMD-LINE             PIC X(512).
       77  RC                   PIC S9(9) COMP.
       77  EOF                  PIC X VALUE "N".
       77  WS-LINE              PIC X(256).
       77  INSERT-SCRIPT        PIC X(256)
           VALUE "bash db2-helpers/insert-transaction-cli.sh".
       77  GET-BALANCES-SCRIPT  PIC X(256)
           VALUE "bash db2-helpers/get-balances-cli.sh".

       77  WS-DATE              PIC X(10).
       77  WS-ACCOUNT           PIC X(30).
       77  WS-TYPE              PIC X(6).
       77  WS-AMOUNT-STR        PIC X(20).
       77  WS-AMOUNT-SIGNED     PIC S9(13)V9(2) VALUE 0.

       PROCEDURE DIVISION.

       MAIN.
           PERFORM CONNECT-TO-DB2.

           MOVE "data/transactions.csv" TO TX-PATH.
           MOVE "data/balances.csv" TO OUT-PATH.
           MOVE "/tmp/db2-balances.csv" TO DB-PATH.

           OPEN INPUT TX-FILE.
           OPEN OUTPUT OUT-FILE.

           PERFORM UNTIL EOF = "Y"
              READ TX-FILE
                 AT END MOVE "Y" TO EOF
                 NOT AT END
                    MOVE TX-LINE TO WS-LINE
                    PERFORM PARSE-LINE
                    PERFORM INSERT-VIA-DB2-CLI
              END-READ
           END-PERFORM.

           PERFORM GET-BALANCES-FROM-DB2.
           PERFORM WRITE-HEADER.
           PERFORM COPY-BALANCES.

           CLOSE TX-FILE.
           CLOSE OUT-FILE.
           PERFORM DISCONNECT-FROM-DB2.
           GOBACK.

       CONNECT-TO-DB2.
           DISPLAY "Conectando a DB2..." UPON CONSOLE.

       DISCONNECT-FROM-DB2.
           DISPLAY "Desconectando de DB2..." UPON CONSOLE.

       PARSE-LINE.
           UNSTRING WS-LINE DELIMITED BY ALL ","
                INTO WS-DATE
                     WS-ACCOUNT
                     WS-TYPE
                     WS-AMOUNT-STR
           END-UNSTRING.
           INSPECT WS-AMOUNT-STR REPLACING ALL "," BY ".".
           MOVE FUNCTION NUMVAL(WS-AMOUNT-STR)
                TO WS-AMOUNT-SIGNED.
           IF WS-TYPE = "DEBIT"
              MULTIPLY -1 BY WS-AMOUNT-SIGNED
           END-IF.

       INSERT-VIA-DB2-CLI.
      *    Preparar los valores para el script bash
           MOVE FUNCTION TRIM(WS-DATE) TO WS-DATE.
           MOVE FUNCTION TRIM(WS-ACCOUNT) TO WS-ACCOUNT.
           MOVE FUNCTION TRIM(WS-TYPE) TO WS-TYPE.
           MOVE FUNCTION TRIM(WS-AMOUNT-STR) TO WS-AMOUNT-STR.

      *    Construir comando para invocar script bash con parámetros
      *    Usar WS-AMOUNT-STR en lugar de WS-AMOUNT-SIGNED
           STRING
               INSERT-SCRIPT DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(WS-DATE) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(WS-ACCOUNT) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(WS-TYPE) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(WS-AMOUNT-STR) DELIMITED BY SIZE
               INTO CMD-LINE
           END-STRING

           CALL "SYSTEM" USING CMD-LINE RETURNING RC.

       GET-BALANCES-FROM-DB2.
           DISPLAY "Consultando saldos desde DB2..." UPON CONSOLE.

      *    Invocar script bash para obtener saldos
           MOVE GET-BALANCES-SCRIPT TO CMD-LINE
           CALL "SYSTEM" USING CMD-LINE RETURNING RC.

       WRITE-HEADER.
           MOVE "account,balance" TO OUT-LINE.
           WRITE OUT-LINE.

       COPY-BALANCES.
           OPEN INPUT DB-BALANCES.
           MOVE "N" TO EOF.
           PERFORM UNTIL EOF = "Y"
              READ DB-BALANCES
                 AT END MOVE "Y" TO EOF
                 NOT AT END
                    MOVE DB-BAL-LINE TO OUT-LINE
                    WRITE OUT-LINE
                    DISPLAY FUNCTION TRIM(DB-BAL-LINE) UPON CONSOLE
              END-READ
           END-PERFORM.
           CLOSE DB-BALANCES.
