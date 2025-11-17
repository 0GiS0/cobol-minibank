       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBMAIN.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY mb-db-if.

       01  WS-OPTION          PIC 9.
       01  WS-EXIT            PIC X      VALUE 'N'.
       01  WS-TMP-AMOUNT      PIC 9(9)V99.
       01  WS-TMP-ACCOUNT     PIC X(10).

    01  WS-MOD-DB-NAME     PIC X(8)   VALUE 'MBDBCLI '.
    01  WS-ENV-DB-MODULE   PIC X(8)   VALUE SPACES.
    *> Usa MBDBCLI (stub) por defecto; sobreescribe con MINIBANK_DB_MODULE

       PROCEDURE DIVISION.
       MAIN-SECTION.
          PERFORM RESOLVE-DB-MODULE
          PERFORM INIT-DB
           PERFORM MENU-LOOP
           PERFORM FINISH-DB
           STOP RUN.

       INIT-DB.
           MOVE 'INIT    '     TO DB-FUNC
           MOVE SPACES         TO DB-ACCOUNT-ID
           MOVE 0              TO DB-AMOUNT
           MOVE 0              TO DB-BALANCE
           MOVE 0              TO DB-STATUS
           MOVE SPACES         TO DB-MESSAGE

           CALL WS-MOD-DB-NAME USING DB-REQUEST

           IF NOT DB-OK
               DISPLAY '*** ERROR AL CONECTAR A DB2: ' DB-MESSAGE
               STOP RUN
           END-IF
           .

       FINISH-DB.
           MOVE 'FINISH  '     TO DB-FUNC
           CALL WS-MOD-DB-NAME USING DB-REQUEST
           .

       RESOLVE-DB-MODULE.
           ACCEPT WS-ENV-DB-MODULE FROM ENVIRONMENT 'MINIBANK_DB_MODULE'
           IF WS-ENV-DB-MODULE NOT = SPACES
               MOVE WS-ENV-DB-MODULE TO WS-MOD-DB-NAME
           END-IF
           .

       MENU-LOOP.
           PERFORM UNTIL WS-EXIT = 'S'
               PERFORM SHOW-MENU
               ACCEPT WS-OPTION
               EVALUATE WS-OPTION
                   WHEN 1
                       PERFORM DO-BALANCE
                   WHEN 2
                       PERFORM DO-DEPOSIT
                   WHEN 3
                       PERFORM DO-WITHDRAW
                   WHEN 9
                       MOVE 'S' TO WS-EXIT
                   WHEN OTHER
                       DISPLAY 'Opción no válida.'
               END-EVALUATE
           END-PERFORM
           .

       SHOW-MENU.
           DISPLAY ' '
           DISPLAY '******** MINI BANK ********'
           DISPLAY '1 - Consultar saldo'
           DISPLAY '2 - Ingresar dinero'
           DISPLAY '3 - Retirar dinero'
           DISPLAY '9 - Salir'
           DISPLAY 'Seleccione opción: ' WITH NO ADVANCING
           .

       ASK-ACCOUNT.
           DISPLAY ' '
           DISPLAY 'Introduzca ID de cuenta (10 caracteres): ' WITH NO ADVANCING
           ACCEPT WS-TMP-ACCOUNT
           MOVE WS-TMP-ACCOUNT TO DB-ACCOUNT-ID
           .

       ASK-AMOUNT.
           DISPLAY 'Importe (ej. 100.50): ' WITH NO ADVANCING
           ACCEPT WS-TMP-AMOUNT
           MOVE WS-TMP-AMOUNT TO DB-AMOUNT
           .

       DO-BALANCE.
           PERFORM ASK-ACCOUNT

           MOVE 'BALANCE ' TO DB-FUNC
           MOVE 0          TO DB-AMOUNT
           MOVE 0          TO DB-STATUS
           MOVE SPACES     TO DB-MESSAGE

           CALL WS-MOD-DB-NAME USING DB-REQUEST

           IF DB-OK
               DISPLAY 'Saldo actual de la cuenta ' DB-ACCOUNT-ID ' : '
                       DB-BALANCE
           ELSE
               DISPLAY '*** ERROR CONSULTANDO SALDO: ' DB-MESSAGE
           END-IF
           .

       DO-DEPOSIT.
           PERFORM ASK-ACCOUNT
           PERFORM ASK-AMOUNT

           MOVE 'DEPOSIT ' TO DB-FUNC
           MOVE 0          TO DB-STATUS
           MOVE SPACES     TO DB-MESSAGE

           CALL WS-MOD-DB-NAME USING DB-REQUEST

           IF DB-OK
               DISPLAY 'Ingreso realizado correctamente.'
               DISPLAY 'Nuevo saldo: ' DB-BALANCE
           ELSE
               DISPLAY '*** ERROR AL INGRESAR: ' DB-MESSAGE
           END-IF
           .

       DO-WITHDRAW.
           PERFORM ASK-ACCOUNT
           PERFORM ASK-AMOUNT

           MOVE 'WITHDRW ' TO DB-FUNC
           MOVE 0          TO DB-STATUS
           MOVE SPACES     TO DB-MESSAGE

           CALL WS-MOD-DB-NAME USING DB-REQUEST

           IF DB-OK
               DISPLAY 'Retirada realizada correctamente.'
               DISPLAY 'Nuevo saldo: ' DB-BALANCE
           ELSE
               DISPLAY '*** ERROR AL RETIRAR: ' DB-MESSAGE
           END-IF
           .
