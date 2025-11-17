       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBDBCLI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEBUG-FILE ASSIGN TO "/tmp/mbdbcli-debug.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  DEBUG-FILE.
       01  DEBUG-REC PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-BALANCE         PIC S9(9)V99 COMP-3 VALUE 1000.00.
       01  WS-CONNECTED       PIC X VALUE 'N'.

       *> Datos de cuentas de prueba (stub)
       01  WS-STUB-ACCOUNTS.
           05  WS-STUB-COUNT       PIC 9(2) COMP VALUE 5.
           05  WS-STUB-ACCOUNT OCCURS 5 TIMES INDEXED BY WS-IX.
               10  WS-STUB-ID      PIC X(30).
               10  WS-STUB-BAL     PIC S9(13)V9(2) COMP-3.

       01  WS-COUNTER             PIC 9(2) COMP.

       LINKAGE SECTION.
       COPY mb-db-if.

       PROCEDURE DIVISION USING DB-REQUEST.

           EVALUATE DB-FUNC
               WHEN 'INIT    '
                   PERFORM INIT-STUB-DATA
                   MOVE 'Y' TO WS-CONNECTED
                   MOVE 0   TO DB-STATUS
                   MOVE 'Conectado (modo stub)' TO DB-MESSAGE

               WHEN 'FINISH  '
                   MOVE 'N' TO WS-CONNECTED
                   MOVE 0   TO DB-STATUS
                   MOVE 'Desconectado (modo stub)' TO DB-MESSAGE

               WHEN 'BALANCE '
                   IF WS-CONNECTED = 'Y'
                       MOVE WS-BALANCE TO DB-BALANCE
                       MOVE 0 TO DB-STATUS
                   ELSE
                       MOVE 1 TO DB-STATUS
                       MOVE 'No conectado' TO DB-MESSAGE
                   END-IF

               WHEN 'DEPOSIT '
                   ADD DB-AMOUNT TO WS-BALANCE
                   MOVE WS-BALANCE TO DB-BALANCE
                   MOVE 0 TO DB-STATUS
                   MOVE 'Ingreso realizado (stub)' TO DB-MESSAGE

               WHEN 'WITHDRW '
                   IF DB-AMOUNT > WS-BALANCE
                       MOVE 1 TO DB-STATUS
                       MOVE 'Saldo insuficiente' TO DB-MESSAGE
                   ELSE
                       SUBTRACT DB-AMOUNT FROM WS-BALANCE
                       MOVE WS-BALANCE TO DB-BALANCE
                       MOVE 0 TO DB-STATUS
                       MOVE 'Retiro realizado (stub)' TO DB-MESSAGE
                   END-IF

               WHEN 'LISTACCT '
                   PERFORM LIST-ACCOUNTS-STUB

               WHEN OTHER
                   MOVE 16 TO DB-STATUS
                   MOVE 'FUNCION DESCONOCIDA' TO DB-MESSAGE
           END-EVALUATE.

           GOBACK.

       *> ============================================================
       *> 🗄️ INICIALIZAR DATOS DE PRUEBA
       *> ============================================================
       INIT-STUB-DATA.
           MOVE 'ACC-001' TO WS-STUB-ID(1)
           MOVE 5000.00   TO WS-STUB-BAL(1)

           MOVE 'ACC-002' TO WS-STUB-ID(2)
           MOVE 12500.75  TO WS-STUB-BAL(2)

           MOVE 'ACC-003' TO WS-STUB-ID(3)
           MOVE 750.25    TO WS-STUB-BAL(3)

           MOVE 'ACC-004' TO WS-STUB-ID(4)
           MOVE 25000.00  TO WS-STUB-BAL(4)

           MOVE 'ACC-005' TO WS-STUB-ID(5)
           MOVE 3333.33   TO WS-STUB-BAL(5)
           .

       *> ============================================================
       *> 📋 LISTAR CUENTAS (STUB)
       *> ============================================================
       LIST-ACCOUNTS-STUB.
           IF WS-CONNECTED NOT = 'Y'
               MOVE 1 TO DB-STATUS
               MOVE 'No conectado' TO DB-MESSAGE
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE
           MOVE 'N' TO DB-LIST-TRUNCATED
           MOVE 0 TO DB-LIST-COUNT

           *> Copiar cuentas stub a la lista de respuesta
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > WS-STUB-COUNT
                   OR WS-COUNTER > DB-LIST-MAX
               ADD 1 TO DB-LIST-COUNT
               SET DB-IX TO DB-LIST-COUNT
               MOVE WS-STUB-ID(WS-COUNTER)
                   TO DB-LIST-ACCOUNT-ID(DB-IX)
               MOVE WS-STUB-BAL(WS-COUNTER)
                   TO DB-LIST-BALANCE(DB-IX)
           END-PERFORM

           MOVE 'Cuentas listadas (stub)' TO DB-MESSAGE
           .
