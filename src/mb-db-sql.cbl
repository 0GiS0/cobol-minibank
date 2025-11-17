       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBDBSQL.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       01  WS-DB-NAME          PIC X(8)  VALUE 'MINIBANK'.
       01  WS-DB-USER          PIC X(16) VALUE 'db2inst1'.
       01  WS-DB-PASS          PIC X(16) VALUE 'password'.
       *> En producción esto vendría de un fichero seguro / vars entorno

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
                   MOVE 16          TO DB-STATUS
                   MOVE 'FUNCION DESCONOCIDA' TO DB-MESSAGE
           END-EVALUATE
           GOBACK
           .

       DB-INIT.
           MOVE 0      TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           EXEC SQL
               CONNECT TO :WS-DB-NAME USER :WS-DB-USER USING :WS-DB-PASS
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE SQLCODE TO DB-STATUS
               MOVE 'Error conectando a Db2' TO DB-MESSAGE
           END-IF
           .

       DB-FINISH.
           EXEC SQL
               COMMIT
           END-EXEC

           EXEC SQL
               CONNECT RESET
           END-EXEC

           MOVE 0      TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE
           .

       DB-GET-BALANCE.
           MOVE 0      TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE
           MOVE 0      TO DB-BALANCE

           EXEC SQL
               SELECT BALANCE
                 INTO :DB-BALANCE
                 FROM ACCOUNTS
                WHERE ACCOUNT_ID = :DB-ACCOUNT-ID
           END-EXEC

           IF SQLCODE = 0
               CONTINUE
           ELSE
               MOVE SQLCODE TO DB-STATUS
               IF SQLCODE = 100
                   MOVE 'Cuenta no encontrada' TO DB-MESSAGE
               ELSE
                   MOVE 'Error consultando saldo' TO DB-MESSAGE
               END-IF
           END-IF
           .

       DB-DO-DEPOSIT.
           MOVE 0      TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           *> Actualizamos saldo
           EXEC SQL
               UPDATE ACCOUNTS
                  SET BALANCE = BALANCE + :DB-AMOUNT
                WHERE ACCOUNT_ID = :DB-ACCOUNT-ID
           END-EXEC

           IF SQLCODE = 0
               EXEC SQL
                   COMMIT
               END-EXEC

               *> Releer saldo actualizado
               PERFORM DB-GET-BALANCE
           ELSE
               MOVE SQLCODE TO DB-STATUS
               IF SQLCODE = 100
                   MOVE 'Cuenta no encontrada' TO DB-MESSAGE
               ELSE
                   MOVE 'Error al ingresar' TO DB-MESSAGE
               END-IF
           END-IF
           .

       DB-DO-WITHDRAW.
           MOVE 0      TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           *> Primero obtenemos saldo actual
           PERFORM DB-GET-BALANCE

           IF NOT DB-OK
               EXIT PARAGRAPH
           END-IF

           IF DB-BALANCE < DB-AMOUNT
               MOVE 1 TO DB-STATUS
               MOVE 'Saldo insuficiente' TO DB-MESSAGE
               EXIT PARAGRAPH
           END-IF

           EXEC SQL
               UPDATE ACCOUNTS
                  SET BALANCE = BALANCE - :DB-AMOUNT
                WHERE ACCOUNT_ID = :DB-ACCOUNT-ID
           END-EXEC

           IF SQLCODE = 0
               EXEC SQL
                   COMMIT
               END-EXEC

               *> Releer saldo actualizado
               PERFORM DB-GET-BALANCE
           ELSE
               MOVE SQLCODE TO DB-STATUS
               IF SQLCODE = 100
                   MOVE 'Cuenta no encontrada' TO DB-MESSAGE
               ELSE
                   MOVE 'Error al retirar' TO DB-MESSAGE
               END-IF
           END-IF
           .
