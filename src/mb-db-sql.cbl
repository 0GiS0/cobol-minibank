       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBDBSQL.

      * ============================================================
      * 🗄️ Módulo de acceso a DB2 para MiniBank
      * ============================================================
      * Módulo que actúa como intermediario entre la aplicación
      * principal y la base de datos DB2.
      * ============================================================

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BALANCES-FILE ASSIGN TO
               "/tmp/db2-balances.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD  BALANCES-FILE.
       01  BALANCES-RECORD         PIC X(256).

       WORKING-STORAGE SECTION.

       01  WS-DB-NAME          PIC X(16) VALUE 'MINIBANK'.
       01  WS-DB-USER          PIC X(16) VALUE 'db2inst1'.
       01  WS-DB-PASS          PIC X(16) VALUE 'password'.
       01  WS-CONNECTED        PIC X    VALUE 'N'.

       *> Variables auxiliares para CLI DB2
       01  WS-COMMAND          PIC X(512).
       01  WS-RETURN-CODE      PIC S9(9) COMP VALUE 0.
       01  WS-DATE-YYYYMMDD    PIC X(8).
       01  WS-TODAY-DATE       PIC X(10).
       01  WS-AMOUNT-EDIT      PIC -9(13).99.
       01  WS-AMOUNT-STR       PIC X(30).
         01  WS-BALANCE-LINE     PIC X(256).
         01  WS-EOF-FLAG         PIC X     VALUE 'N'.
         01  WS-BALANCE-FOUND    PIC X     VALUE 'N'.
       01  WS-ACC-ID-FROM-FILE PIC X(30).
       01  WS-BAL-FROM-FILE    PIC X(30).

       01  WS-INSERT-SCRIPT    PIC X(100)
           VALUE 'bash db2-helpers/insert-transaction-cli.sh'.
       01  WS-BALANCES-SCRIPT  PIC X(100)
           VALUE 'bash db2-helpers/get-balances-cli.sh'.

       LINKAGE SECTION.
       COPY mb-db-if.

       PROCEDURE DIVISION USING DB-REQUEST.
       MAIN-DB-SECTION.
           EVALUATE DB-FUNC
               WHEN 'INIT    '
                   PERFORM DB-INIT
               WHEN 'FINISH  '
                   PERFORM DB-FINISH
               WHEN 'BALANCE '
                   PERFORM DB-GET-BALANCE
               WHEN 'DEPOSIT '
                   PERFORM DB-DO-DEPOSIT
               WHEN 'WITHDRW '
                   PERFORM DB-DO-WITHDRAW
               WHEN OTHER
                   MOVE 16 TO DB-STATUS
                   MOVE 'Función desconocida' TO DB-MESSAGE
           END-EVALUATE
           GOBACK
           .

       DB-INIT.
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE
           MOVE 'Y' TO WS-CONNECTED
           DISPLAY 'ℹ️  DB2 Conectado (MINIBANK)'
           .

       DB-FINISH.
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE
           MOVE 'N' TO WS-CONNECTED
           DISPLAY 'ℹ️  DB2 Desconectado'
           .

       DB-GET-BALANCE.
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           IF WS-CONNECTED NOT = 'Y'
               MOVE 1 TO DB-STATUS
               MOVE 'No conectado a DB2' TO DB-MESSAGE
               EXIT PARAGRAPH
           END-IF

           MOVE WS-BALANCES-SCRIPT TO WS-COMMAND
           CALL "SYSTEM" USING WS-COMMAND GIVING WS-RETURN-CODE

           IF WS-RETURN-CODE NOT = 0
               MOVE 8 TO DB-STATUS
               MOVE 'Error al obtener saldos (CLI)' TO DB-MESSAGE
               MOVE 0 TO DB-BALANCE
               EXIT PARAGRAPH
           END-IF

           MOVE 'N' TO WS-EOF-FLAG
           MOVE 'N' TO WS-BALANCE-FOUND
           MOVE 0 TO DB-BALANCE

           OPEN INPUT BALANCES-FILE

           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ BALANCES-FILE INTO WS-BALANCE-LINE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       PERFORM PARSE-BALANCE-LINE
                       IF WS-ACC-ID-FROM-FILE = DB-ACCOUNT-ID
                           PERFORM SET-BALANCE-FROM-FILE
                           MOVE 'Y' TO WS-BALANCE-FOUND
                           MOVE 'Y' TO WS-EOF-FLAG
                       END-IF
               END-READ
           END-PERFORM

           CLOSE BALANCES-FILE

           IF WS-BALANCE-FOUND NOT = 'Y'
               MOVE 1 TO DB-STATUS
               MOVE 'Cuenta no encontrada en DB2 (CLI)'
                   TO DB-MESSAGE
           END-IF

           DISPLAY 'DEBUG: Consulta saldo de ' DB-ACCOUNT-ID
           .

       DB-DO-DEPOSIT.
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           IF WS-CONNECTED NOT = 'Y'
               MOVE 1 TO DB-STATUS
               MOVE 'No conectado a DB2' TO DB-MESSAGE
               EXIT PARAGRAPH
           END-IF

           ACCEPT WS-DATE-YYYYMMDD FROM DATE YYYYMMDD
           STRING WS-DATE-YYYYMMDD(1:4)
                  '-' WS-DATE-YYYYMMDD(5:2)
                  '-' WS-DATE-YYYYMMDD(7:2)
               INTO WS-TODAY-DATE
           END-STRING

           MOVE DB-AMOUNT TO WS-AMOUNT-EDIT
           MOVE FUNCTION TRIM(WS-AMOUNT-EDIT) TO WS-AMOUNT-STR

           STRING
               WS-INSERT-SCRIPT DELIMITED BY SIZE
               ' '             DELIMITED BY SIZE
               WS-TODAY-DATE   DELIMITED BY SIZE
               ' '             DELIMITED BY SIZE
               DB-ACCOUNT-ID   DELIMITED BY SPACE
               ' '             DELIMITED BY SIZE
               'CREDIT'        DELIMITED BY SIZE
               ' '             DELIMITED BY SIZE
               WS-AMOUNT-STR   DELIMITED BY SPACE
               INTO WS-COMMAND
           END-STRING

           CALL "SYSTEM" USING WS-COMMAND GIVING WS-RETURN-CODE

           IF WS-RETURN-CODE NOT = 0
               MOVE 8 TO DB-STATUS
               MOVE 'Error en depósito DB2 (CLI)' TO DB-MESSAGE
               EXIT PARAGRAPH
           END-IF

           PERFORM DB-GET-BALANCE
           .

       DB-DO-WITHDRAW.
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           IF WS-CONNECTED NOT = 'Y'
               MOVE 1 TO DB-STATUS
               MOVE 'No conectado a DB2' TO DB-MESSAGE
               EXIT PARAGRAPH
           END-IF

           ACCEPT WS-DATE-YYYYMMDD FROM DATE YYYYMMDD
           STRING WS-DATE-YYYYMMDD(1:4)
                  '-' WS-DATE-YYYYMMDD(5:2)
                  '-' WS-DATE-YYYYMMDD(7:2)
               INTO WS-TODAY-DATE
           END-STRING

           MOVE DB-AMOUNT TO WS-AMOUNT-EDIT
           MOVE FUNCTION TRIM(WS-AMOUNT-EDIT) TO WS-AMOUNT-STR

           STRING
               WS-INSERT-SCRIPT DELIMITED BY SIZE
               ' '             DELIMITED BY SIZE
               WS-TODAY-DATE   DELIMITED BY SIZE
               ' '             DELIMITED BY SIZE
               DB-ACCOUNT-ID   DELIMITED BY SPACE
               ' '             DELIMITED BY SIZE
               'DEBIT'         DELIMITED BY SIZE
               ' '             DELIMITED BY SIZE
               WS-AMOUNT-STR   DELIMITED BY SPACE
               INTO WS-COMMAND
           END-STRING

           CALL "SYSTEM" USING WS-COMMAND GIVING WS-RETURN-CODE

           IF WS-RETURN-CODE NOT = 0
               MOVE 8 TO DB-STATUS
               MOVE 'Error en retirada DB2 (CLI)' TO DB-MESSAGE
               EXIT PARAGRAPH
           END-IF

           PERFORM DB-GET-BALANCE
           .

       *> ============================================================
       *> 🔍 UTILIDADES
       *> ============================================================

       PARSE-BALANCE-LINE.
           UNSTRING WS-BALANCE-LINE
               DELIMITED BY ','
               INTO WS-ACC-ID-FROM-FILE
                    WS-BAL-FROM-FILE
           END-UNSTRING
           .

       SET-BALANCE-FROM-FILE.
           MOVE FUNCTION NUMVAL(WS-BAL-FROM-FILE) TO DB-BALANCE
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE
           .
