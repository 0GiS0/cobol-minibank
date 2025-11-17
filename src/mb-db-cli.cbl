       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBDBCLI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-BALANCE         PIC S9(9)V99 COMP-3 VALUE 1000.00.
       01  WS-CONNECTED       PIC X VALUE 'N'.

       LINKAGE SECTION.
       COPY mb-db-if.

       PROCEDURE DIVISION USING DB-REQUEST.

           EVALUATE DB-FUNC
               WHEN 'INIT    '
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

               WHEN OTHER
                   MOVE 16 TO DB-STATUS
                   MOVE 'FUNCION DESCONOCIDA' TO DB-MESSAGE
           END-EVALUATE.

           GOBACK.
