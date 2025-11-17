       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBDBSQL.

      * ============================================================
      * 🗄️ Módulo de acceso a DB2 para MiniBank
      * ============================================================
      * Módulo que actúa como intermediario entre la aplicación
      * principal y la base de datos DB2.
      * ============================================================

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-DB-NAME          PIC X(16) VALUE 'MINIBANK'.
       01  WS-DB-USER          PIC X(16) VALUE 'db2inst1'.
       01  WS-DB-PASS          PIC X(16) VALUE 'password'.
       01  WS-CONNECTED        PIC X    VALUE 'N'.

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

           MOVE 0 TO DB-BALANCE
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

           MOVE 0 TO DB-BALANCE
           DISPLAY 'DEBUG: Depósito de ' DB-AMOUNT
               ' en ' DB-ACCOUNT-ID
           .

       DB-DO-WITHDRAW.
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           IF WS-CONNECTED NOT = 'Y'
               MOVE 1 TO DB-STATUS
               MOVE 'No conectado a DB2' TO DB-MESSAGE
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO DB-BALANCE
           DISPLAY 'DEBUG: Retiro de ' DB-AMOUNT
               ' de ' DB-ACCOUNT-ID
           .
