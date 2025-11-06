      ******************************************************************
      * COBOL MINIBANK - MENU INTERACTIVO CON DB2
      *
      * Programa que mantiene un loop principal recibiendo inputs
      * Muestra menu, procesa opciones, y espera siguiente input
      * Sin salir del programa hasta que el usuario elige salir
      ******************************************************************

      * IDENTIFICATION DIVISION
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIBANK-MENU.

      * ENVIRONMENT DIVISION
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO
               "/tmp/minibank-accounts.tmp"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTIONS-FILE ASSIGN TO
               "/tmp/minibank-transactions.tmp"
               ORGANIZATION IS LINE SEQUENTIAL.

      * DATA DIVISION
       DATA DIVISION.

      * FILE SECTION
       FILE SECTION.
       FD  ACCOUNTS-FILE.
       01  ACCOUNTS-LINE        PIC X(256).

       FD  TRANSACTIONS-FILE.
       01  TRANSACTIONS-LINE    PIC X(256).

      * WORKING-STORAGE SECTION
       WORKING-STORAGE SECTION.

      * VARIABLES DE CONTROL DEL FLUJO
           77  WS-CONTINUE      PIC X VALUE "Y".
           77  WS-SUB-CONTINUE  PIC X VALUE "Y".
           77  WS-MENU-OPTION   PIC 9(2) VALUE 0.
           77  WS-INPUT         PIC X(20).
           77  WS-RC             PIC S9(9) COMP.
           77  EOF               PIC X VALUE "N".
           77  WS-LINE           PIC X(256).
           77  CMD-STRING        PIC X(256).

      * VARIABLES DE DATOS
           77  WS-ACCOUNT-ID    PIC 9(4) COMP.
           77  WS-ACCOUNT-NAME  PIC X(100).
           77  WS-BALANCE       PIC S9(13)V9(2) COMP-3.
           77  WS-FORMATTED-AMT PIC -(12)9.99.

      * ARRAYS PARA ALMACENAR CUENTAS (cargar una sola vez)
           01  ACCOUNTS-TABLE.
               05  ACCOUNT OCCURS 50 TIMES.
                   10  ACC-ID            PIC 9(4) COMP.
                   10  ACC-NAME          PIC X(100).
                   10  ACC-BALANCE       PIC S9(13)V9(2) COMP-3.

      * ARRAY PARA TRANSACCIONES (se carga segun necesidad)
           01  TRANSACTIONS-TABLE.
               05  TRANSACTION OCCURS 200 TIMES.
                   10  TRANS-ID          PIC 9(9) COMP.
                   10  TRANS-DATE        PIC X(10).
                   10  TRANS-TYPE        PIC X(10).
                   10  TRANS-AMOUNT      PIC S9(13)V9(2) COMP-3.

      * CONTADORES Y VARIABLES DE ITERACION
           77  ACCT-COUNT        PIC 9(4) COMP VALUE 0.
           77  TRANS-COUNT       PIC 9(4) COMP VALUE 0.
           77  I                 PIC 9(4) COMP.
           77  J                 PIC 9(4) COMP.

      * VARIABLES PARA PARSING
           77  WS-PARSE-ID       PIC 9(4) COMP.
           77  WS-PARSE-AMT      PIC X(20).

      * PROCEDURE DIVISION
       PROCEDURE DIVISION.

      * =================================================================
      * MAIN - PROGRAMA PRINCIPAL CON LOOP
      * =================================================================
       MAIN.
           DISPLAY " ".
           DISPLAY "==================================================".
           DISPLAY "    💰 BIENVENIDO A MINIBANK 💰".
           DISPLAY "==================================================".
           DISPLAY " ".
           DISPLAY "⏳ Cargando cuentas desde DB2...".

      *    CARGA DATOS UNA SOLA VEZ AL INICIO
           PERFORM LOAD-ACCOUNTS.

      *    LOOP PRINCIPAL - MANTIENE EL PROGRAMA EN EJECUCION
           PERFORM UNTIL WS-CONTINUE = "N"
               PERFORM SHOW-MAIN-MENU
               PERFORM GET-USER-OPTION

      *        PROCESA LA OPCION ELEGIDA
               EVALUATE WS-MENU-OPTION
                   WHEN 1
                       PERFORM SHOW-ALL-ACCOUNTS
                   WHEN 2
                       PERFORM SELECT-AND-VIEW-ACCOUNT
                   WHEN 3
                       MOVE "N" TO WS-CONTINUE
               END-EVALUATE
           END-PERFORM.

           PERFORM SHOW-GOODBYE.
           GOBACK.

      * =================================================================
      * SHOW-MAIN-MENU - Muestra el menu y espera input
      * =================================================================
       SHOW-MAIN-MENU.
           DISPLAY " ".
           DISPLAY "==================================================".
           DISPLAY "              📋 MINIBANK - MENU 📋".
           DISPLAY "==================================================".
           DISPLAY "".
           DISPLAY "  1️⃣  Ver todas las cuentas".
           DISPLAY "  2️⃣  Ver detalles de una cuenta".
           DISPLAY "  3️⃣  Salir del programa".

       GET-USER-OPTION.
           DISPLAY " ".
           DISPLAY "Selecciona una opcion: " WITH NO ADVANCING.
           ACCEPT WS-INPUT.

           IF FUNCTION TEST-NUMVAL(WS-INPUT) = 0
               MOVE FUNCTION NUMVAL(WS-INPUT) TO WS-MENU-OPTION
           ELSE
               MOVE 0 TO WS-MENU-OPTION
           END-IF.

      * =================================================================
      * SHOW-ALL-ACCOUNTS - Opcion 1: Listar todas las cuentas
      * =================================================================
       SHOW-ALL-ACCOUNTS.
           DISPLAY " ".
           DISPLAY "==================================================".
           DISPLAY "          👥 LISTADO DE CUENTAS 👥".
           DISPLAY "==================================================".

           IF ACCT-COUNT = 0
               DISPLAY "❌ No hay cuentas disponibles"
           ELSE
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > ACCT-COUNT
                   MOVE ACC-BALANCE(I) TO WS-FORMATTED-AMT
                   DISPLAY " "
                   DISPLAY "  🏦 Cuenta " I ":"
                   DISPLAY "     ID: " ACC-ID(I)
                   DISPLAY "     Nombre: " ACC-NAME(I)
                   DISPLAY "     Saldo: 💵 $" WS-FORMATTED-AMT
               END-PERFORM
           END-IF.

           DISPLAY " ".
           DISPLAY "==================================================".

      * =================================================================
      * SELECT-AND-VIEW-ACCOUNT - Opcion 2: Ver detalles de una cuenta
      * =================================================================
       SELECT-AND-VIEW-ACCOUNT.
           MOVE "Y" TO WS-SUB-CONTINUE.

      *    LOOP PARA SELECCIONAR Y VER CUENTAS
           PERFORM UNTIL WS-SUB-CONTINUE = "N"
               PERFORM SHOW-ACCOUNT-SELECTION-MENU

               IF WS-MENU-OPTION NOT = 0
                   IF WS-MENU-OPTION = 99
                       MOVE "N" TO WS-SUB-CONTINUE
                   ELSE
                       IF WS-MENU-OPTION >= 1 AND
                          WS-MENU-OPTION <= ACCT-COUNT
                           MOVE ACC-ID(WS-MENU-OPTION)
                               TO WS-ACCOUNT-ID
                           MOVE ACC-NAME(WS-MENU-OPTION)
                               TO WS-ACCOUNT-NAME
                           MOVE ACC-BALANCE(WS-MENU-OPTION)
                               TO WS-BALANCE
                           PERFORM SHOW-ACCOUNT-DETAILS
                       ELSE
                           DISPLAY "Opcion no valida"
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       SHOW-ACCOUNT-SELECTION-MENU.
           DISPLAY " ".
           DISPLAY "==================================================".
           DISPLAY "          🔍 SELECCIONAR CUENTA 🔍".
           DISPLAY "==================================================".

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ACCT-COUNT
               DISPLAY " "
               DISPLAY "  " I ". 💳 " ACC-NAME(I)
                   " (ID: " ACC-ID(I) ")"
           END-PERFORM.

           DISPLAY " ".
           DISPLAY "  99. ◀️  Volver al menu principal".
           DISPLAY " ".
           DISPLAY "Elige una cuenta (1-" ACCT-COUNT " o 99): "
               WITH NO ADVANCING.
           ACCEPT WS-INPUT.

           IF FUNCTION TEST-NUMVAL(WS-INPUT) = 0
               MOVE FUNCTION NUMVAL(WS-INPUT) TO WS-MENU-OPTION
           ELSE
               MOVE 0 TO WS-MENU-OPTION
           END-IF.

      * =================================================================
      * SHOW-ACCOUNT-DETAILS - Muestra saldo y transacciones
      * =================================================================
       SHOW-ACCOUNT-DETAILS.
           DISPLAY " ".
           DISPLAY "==================================================".
           DISPLAY "            📊 DETALLES DE CUENTA 📊".
           DISPLAY "==================================================".
           DISPLAY " ".
           DISPLAY "👤 Nombre: " WS-ACCOUNT-NAME.
           MOVE WS-BALANCE TO WS-FORMATTED-AMT.
           DISPLAY "💰 Saldo Actual: $" WS-FORMATTED-AMT.
           DISPLAY " ".

      *    CARGA TRANSACCIONES PARA ESTA CUENTA
           PERFORM LOAD-TRANSACTIONS.

           DISPLAY "📝 ULTIMAS TRANSACCIONES:".
           DISPLAY " ".

           IF TRANS-COUNT = 0
               DISPLAY "  ❌ No hay transacciones registradas"
           ELSE
               PERFORM DISPLAY-TRANSACTIONS
           END-IF.

           DISPLAY " ".
           DISPLAY "==================================================".
           DISPLAY " ".
           DISPLAY "⏎ Presiona ENTER para volver al menu..."
               WITH NO ADVANCING.
           ACCEPT WS-INPUT.
           MOVE "N" TO WS-SUB-CONTINUE.

      * =================================================================
      * DISPLAY-TRANSACTIONS - Muestra las transacciones
      * =================================================================
       DISPLAY-TRANSACTIONS.
           PERFORM VARYING J FROM 1 BY 1
               UNTIL J > TRANS-COUNT
               MOVE TRANS-AMOUNT(J) TO WS-FORMATTED-AMT

               IF TRANS-TYPE(J) = "CREDIT"
                   DISPLAY "  ➕ " TRANS-DATE(J)
                       " - DEPOSITO: $" WS-FORMATTED-AMT
               ELSE
                   DISPLAY "  ➖ " TRANS-DATE(J)
                       " - RETIRO: $" WS-FORMATTED-AMT
               END-IF
           END-PERFORM.

      * =================================================================
      * LOAD-ACCOUNTS - Carga cuentas desde DB2 (UNA SOLA VEZ)
      * =================================================================
       LOAD-ACCOUNTS.
           MOVE 0 TO ACCT-COUNT.
           MOVE "N" TO EOF.

      *    Ejecuta script Python para obtener cuentas
           MOVE "python3 .devcontainer/get-accounts.py"
               TO CMD-STRING.
           CALL "SYSTEM" USING CMD-STRING
               RETURNING WS-RC.

           IF WS-RC NOT = 0
               DISPLAY "ERROR: No se pudo conectar a DB2"
               GOBACK
           END-IF.

      *    Lee archivo temporal con las cuentas
           OPEN INPUT ACCOUNTS-FILE.
           PERFORM UNTIL EOF = "Y"
               READ ACCOUNTS-FILE
                   AT END MOVE "Y" TO EOF
                   NOT AT END
                       ADD 1 TO ACCT-COUNT
                       IF ACCT-COUNT <= 50
                           PERFORM PARSE-ACCOUNT-LINE
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE ACCOUNTS-FILE.

           DISPLAY "OK: " ACCT-COUNT " cuentas cargadas".
           DISPLAY " ".

       PARSE-ACCOUNT-LINE.
           MOVE ACCOUNTS-LINE TO WS-LINE.
           UNSTRING WS-LINE DELIMITED BY ","
               INTO WS-PARSE-ID
                    ACC-NAME(ACCT-COUNT)
                    WS-PARSE-AMT
           END-UNSTRING.

           MOVE WS-PARSE-ID TO ACC-ID(ACCT-COUNT).
           MOVE FUNCTION NUMVAL(WS-PARSE-AMT)
               TO ACC-BALANCE(ACCT-COUNT).

      * =================================================================
      * LOAD-TRANSACTIONS - Carga transacciones de una cuenta
      * =================================================================
       LOAD-TRANSACTIONS.
           MOVE 0 TO TRANS-COUNT.
           MOVE "N" TO EOF.

      *    Ejecuta script Python para obtener transacciones
           STRING "python3 .devcontainer/get-transactions.py "
               DELIMITED BY SIZE
               WS-ACCOUNT-ID DELIMITED BY SIZE
               INTO CMD-STRING
           END-STRING.

           CALL "SYSTEM" USING CMD-STRING
               RETURNING WS-RC.

      *    Lee archivo temporal con transacciones
           OPEN INPUT TRANSACTIONS-FILE.
           PERFORM UNTIL EOF = "Y"
               READ TRANSACTIONS-FILE
                   AT END MOVE "Y" TO EOF
                   NOT AT END
                       ADD 1 TO TRANS-COUNT
                       IF TRANS-COUNT <= 200
                           PERFORM PARSE-TRANSACTION-LINE
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE TRANSACTIONS-FILE.

       PARSE-TRANSACTION-LINE.
           MOVE TRANSACTIONS-LINE TO WS-LINE.
           UNSTRING WS-LINE DELIMITED BY ","
               INTO TRANS-ID(TRANS-COUNT)
                    TRANS-DATE(TRANS-COUNT)
                    TRANS-TYPE(TRANS-COUNT)
                    WS-PARSE-AMT
           END-UNSTRING.
           MOVE FUNCTION NUMVAL(WS-PARSE-AMT)
               TO TRANS-AMOUNT(TRANS-COUNT).


      * =================================================================
      * SHOW-GOODBYE - Mensaje de despedida
      * =================================================================
       SHOW-GOODBYE.
           DISPLAY " ".
           DISPLAY "==================================================".
           DISPLAY "    👋 Gracias por usar MINIBANK 👋".
           DISPLAY "         ¡Hasta pronto! 😊".
           DISPLAY "==================================================".
           DISPLAY " ".

       END PROGRAM MINIBANK-MENU.

