       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBMAIN.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTIONS-FILE ASSIGN TO
               "/workspaces/cobol-minibank/data/transactions.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BALANCES-FILE ASSIGN TO
               "/workspaces/cobol-minibank/data/balances.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD  TRANSACTIONS-FILE.
       01  TRANSACTIONS-RECORD         PIC X(100).

       FD  BALANCES-FILE.
       01  BALANCES-RECORD             PIC X(100).

       WORKING-STORAGE SECTION.

       *> ============================================================
       *> 📋 SECCION DE CONTROL Y CONFIGURACION
       *> ============================================================

       COPY mb-db-if.

       *> Modo de operación: 'CSV' o 'DB2'
       01  WS-DATA-SOURCE         PIC X(8)  VALUE 'CSV'.
       01  WS-ENV-DATA-SOURCE     PIC X(8)  VALUE SPACES.

       *> Módulo de base de datos si se usa DB2
       01  WS-MOD-DB-NAME         PIC X(16) VALUE 'MBDBSQL '.
       01  WS-ENV-DB-MODULE       PIC X(16) VALUE SPACES.

       *> Variables de menú
       01  WS-OPTION              PIC 9     VALUE 0.
       01  WS-EXIT                PIC X     VALUE 'N'.
       01  WS-TMP-AMOUNT          PIC 9(9)V99.
       01  WS-TMP-ACCOUNT         PIC X(30).

       *> ============================================================
       *> 📊 SECCION DE DATOS EN MEMORIA (para modo CSV)
       *> ============================================================

       01  WS-ACCOUNTS-TABLE.
           05  WS-ACCOUNTS-ARRAY OCCURS 100 TIMES INDEXED BY IX-ACC.
               10  ACC-ID            PIC X(30).
               10  ACC-NAME          PIC X(100).
               10  ACC-BALANCE       PIC S9(13)V9(2) COMP-3.

       01  WS-ACC-COUNT            PIC 999  VALUE 0.
       01  WS-ACC-INDEX            PIC 999.
       01  WS-ACC-FOUND            PIC X    VALUE 'N'.

       *> Variables de procesamiento CSV
       01  WS-CSV-LINE             PIC X(100).
       01  WS-CSV-PARTS.
           05  CSV-DATE            PIC X(10).
           05  CSV-ACCOUNT         PIC X(30).
           05  CSV-TYPE            PIC X(10).
           05  CSV-AMOUNT          PIC 9(9)V99.
       01  WS-EOF-FLAG             PIC X    VALUE 'N'.
       01  WS-CSV-AMOUNT-NUMERIC   PIC S9(13)V9(2) COMP-3.

       PROCEDURE DIVISION.

       *> ============================================================
       *> 🚀 MAIN - FLUJO PRINCIPAL
       *> ============================================================

       MAIN-SECTION.
           PERFORM INITIALIZE-SYSTEM
           PERFORM SHOW-DATA-SOURCE-INFO

           EVALUATE WS-DATA-SOURCE
               WHEN 'CSV'
                   PERFORM LOAD-DATA-FROM-CSV
                   PERFORM INTERACTIVE-MENU
               WHEN 'DB2'
                   PERFORM INIT-DB
                   PERFORM INTERACTIVE-MENU
                   PERFORM FINISH-DB
               WHEN OTHER
                   DISPLAY '❌ Modo no válido: ' WS-DATA-SOURCE
           END-EVALUATE

           STOP RUN
           .

       *> ============================================================
       *> ⚙️ INICIALIZACION
       *> ============================================================

       INITIALIZE-SYSTEM.
           *> Resolver fuente de datos desde variable de entorno
           ACCEPT WS-ENV-DATA-SOURCE
               FROM ENVIRONMENT 'MINIBANK_DATA_SOURCE'

           IF WS-ENV-DATA-SOURCE NOT = SPACES
               MOVE WS-ENV-DATA-SOURCE TO WS-DATA-SOURCE
           END-IF

           *> Resolver módulo de DB2 si lo necesita
           ACCEPT WS-ENV-DB-MODULE
               FROM ENVIRONMENT 'MINIBANK_DB_MODULE'

           IF WS-ENV-DB-MODULE NOT = SPACES
               MOVE WS-ENV-DB-MODULE TO WS-MOD-DB-NAME
           END-IF

           MOVE 0 TO WS-ACC-COUNT
           .

       SHOW-DATA-SOURCE-INFO.
           DISPLAY ' '
           DISPLAY '🏦 ========== MINIBANK ========== '
           DISPLAY '📊 Modo de operación: ' WS-DATA-SOURCE

           EVALUATE WS-DATA-SOURCE
               WHEN 'CSV'
                   DISPLAY '💾 Datos en MEMORIA (CSV)'
               WHEN 'DB2'
                   DISPLAY '🗄️  Datos en DB2'
           END-EVALUATE

           DISPLAY '================================='
           DISPLAY ' '
           .

       *> ============================================================
       *> 📁 CARGA DE DATOS (modo CSV)
       *> ============================================================

       LOAD-DATA-FROM-CSV.
           DISPLAY '📂 Cargando datos desde CSV...'

           OPEN INPUT TRANSACTIONS-FILE

           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ TRANSACTIONS-FILE INTO WS-CSV-LINE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       PERFORM PARSE-AND-PROCESS-CSV
               END-READ
           END-PERFORM

           CLOSE TRANSACTIONS-FILE

           DISPLAY '✅ Datos cargados. Total cuentas: ' WS-ACC-COUNT
           DISPLAY ' '
           .

       PARSE-AND-PROCESS-CSV.
           *> Parsear línea CSV: fecha,cuenta,tipo,monto
           UNSTRING WS-CSV-LINE DELIMITED BY ','
               INTO CSV-DATE CSV-ACCOUNT CSV-TYPE CSV-AMOUNT
           END-UNSTRING

           MOVE CSV-AMOUNT TO WS-CSV-AMOUNT-NUMERIC

           *> Buscar o crear cuenta
           PERFORM FIND-OR-CREATE-ACCOUNT

           *> Procesar transacción
           EVALUATE CSV-TYPE
               WHEN 'CREDIT'
                   ADD WS-CSV-AMOUNT-NUMERIC
                       TO ACC-BALANCE(WS-ACC-INDEX)
               WHEN 'DEBIT'
                   SUBTRACT WS-CSV-AMOUNT-NUMERIC
                       FROM ACC-BALANCE(WS-ACC-INDEX)
           END-EVALUATE
           .

       FIND-OR-CREATE-ACCOUNT.
           MOVE 'N' TO WS-ACC-FOUND

           *> Buscar cuenta existente
           PERFORM VARYING IX-ACC FROM 1 BY 1
               UNTIL IX-ACC > WS-ACC-COUNT
                   OR WS-ACC-FOUND = 'Y'
               IF ACC-ID(IX-ACC) = CSV-ACCOUNT
                   MOVE 'Y' TO WS-ACC-FOUND
                   MOVE IX-ACC TO WS-ACC-INDEX
               END-IF
           END-PERFORM

           *> Si no existe, crear nueva cuenta
           IF WS-ACC-FOUND = 'N'
               ADD 1 TO WS-ACC-COUNT
               MOVE WS-ACC-COUNT TO WS-ACC-INDEX
               MOVE CSV-ACCOUNT TO ACC-ID(WS-ACC-INDEX)
               MOVE SPACES TO ACC-NAME(WS-ACC-INDEX)
               MOVE 0 TO ACC-BALANCE(WS-ACC-INDEX)
           END-IF
           .

       *> ============================================================
       *> 🎮 MENÚ INTERACTIVO
       *> ============================================================

       INTERACTIVE-MENU.
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
                   WHEN 4
                       PERFORM DO-LIST-ACCOUNTS
                   WHEN 9
                       MOVE 'S' TO WS-EXIT
                   WHEN OTHER
                       DISPLAY '❌ Opción no válida.'
               END-EVALUATE
           END-PERFORM
           .

       SHOW-MENU.
           DISPLAY ' '
           DISPLAY '📋 ===== MENÚ PRINCIPAL ===== '
           DISPLAY '1 - Consultar saldo'
           DISPLAY '2 - Ingresar dinero'
           DISPLAY '3 - Retirar dinero'
           DISPLAY '4 - Listar cuentas'
           DISPLAY '9 - Salir'
           DISPLAY '=============================='
           DISPLAY 'Seleccione opción: ' WITH NO ADVANCING
           .

       *> ============================================================
       *> 💼 OPERACIONES BANCARIAS
       *> ============================================================

       ASK-ACCOUNT.
           DISPLAY ' '
           DISPLAY 'ID de cuenta (ej. ACC-001): '
               WITH NO ADVANCING
           ACCEPT WS-TMP-ACCOUNT
           MOVE WS-TMP-ACCOUNT TO DB-ACCOUNT-ID
           .

       ASK-AMOUNT.
           DISPLAY 'Cantidad (ej. 100.50): '
               WITH NO ADVANCING
           ACCEPT WS-TMP-AMOUNT
           MOVE WS-TMP-AMOUNT TO DB-AMOUNT
           .

       DO-BALANCE.
           PERFORM ASK-ACCOUNT

           EVALUATE WS-DATA-SOURCE
               WHEN 'CSV'
                   PERFORM GET-BALANCE-CSV
               WHEN 'DB2'
                   PERFORM GET-BALANCE-DB2
           END-EVALUATE
           .

       GET-BALANCE-CSV.
           PERFORM FIND-ACCOUNT-BY-ID

           IF WS-ACC-FOUND = 'Y'
               DISPLAY '✅ Saldo de ' DB-ACCOUNT-ID ': '
                   ACC-BALANCE(WS-ACC-INDEX)
           ELSE
               DISPLAY '❌ Cuenta no encontrada'
           END-IF
           .

       GET-BALANCE-DB2.
           MOVE 'BALANCE ' TO DB-FUNC
           MOVE 0 TO DB-AMOUNT
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           CALL WS-MOD-DB-NAME USING DB-REQUEST

           IF DB-OK
               DISPLAY '✅ Saldo de ' DB-ACCOUNT-ID ': '
                   DB-BALANCE
           ELSE
               DISPLAY '❌ ERROR: ' DB-MESSAGE
           END-IF
           .

       DO-DEPOSIT.
           PERFORM ASK-ACCOUNT
           PERFORM ASK-AMOUNT

           EVALUATE WS-DATA-SOURCE
               WHEN 'CSV'
                   PERFORM DEPOSIT-CSV
               WHEN 'DB2'
                   PERFORM DEPOSIT-DB2
           END-EVALUATE
           .

       DEPOSIT-CSV.
           PERFORM FIND-ACCOUNT-BY-ID

           IF WS-ACC-FOUND = 'Y'
               ADD WS-TMP-AMOUNT TO ACC-BALANCE(WS-ACC-INDEX)
               DISPLAY '✅ Ingreso realizado'
               DISPLAY '💰 Nuevo saldo: '
                   ACC-BALANCE(WS-ACC-INDEX)
           ELSE
               DISPLAY '❌ Cuenta no encontrada'
           END-IF
           .

       DEPOSIT-DB2.
           MOVE 'DEPOSIT ' TO DB-FUNC
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           CALL WS-MOD-DB-NAME USING DB-REQUEST

           IF DB-OK
               DISPLAY '✅ Ingreso realizado'
               DISPLAY '💰 Nuevo saldo: ' DB-BALANCE
           ELSE
               DISPLAY '❌ ERROR: ' DB-MESSAGE
           END-IF
           .

       DO-WITHDRAW.
           PERFORM ASK-ACCOUNT
           PERFORM ASK-AMOUNT

           EVALUATE WS-DATA-SOURCE
               WHEN 'CSV'
                   PERFORM WITHDRAW-CSV
               WHEN 'DB2'
                   PERFORM WITHDRAW-DB2
           END-EVALUATE
           .

       WITHDRAW-CSV.
           PERFORM FIND-ACCOUNT-BY-ID

           IF WS-ACC-FOUND = 'N'
               DISPLAY '❌ Cuenta no encontrada'
               EXIT PARAGRAPH
           END-IF

           IF ACC-BALANCE(WS-ACC-INDEX) < WS-TMP-AMOUNT
               DISPLAY '❌ Saldo insuficiente'
               DISPLAY '   Saldo actual: '
                   ACC-BALANCE(WS-ACC-INDEX)
               EXIT PARAGRAPH
           END-IF

           SUBTRACT WS-TMP-AMOUNT
               FROM ACC-BALANCE(WS-ACC-INDEX)
           DISPLAY '✅ Retirada realizada'
           DISPLAY '💰 Nuevo saldo: '
               ACC-BALANCE(WS-ACC-INDEX)
           .

       WITHDRAW-DB2.
           MOVE 'WITHDRW ' TO DB-FUNC
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           CALL WS-MOD-DB-NAME USING DB-REQUEST

           IF DB-OK
               DISPLAY '✅ Retirada realizada'
               DISPLAY '💰 Nuevo saldo: ' DB-BALANCE
           ELSE
               DISPLAY '❌ ERROR: ' DB-MESSAGE
           END-IF
           .

       DO-LIST-ACCOUNTS.
           EVALUATE WS-DATA-SOURCE
               WHEN 'CSV'
                   PERFORM LIST-ACCOUNTS-CSV
               WHEN 'DB2'
                   DISPLAY '⚠️  Listar todas las cuentas de DB2'
                   DISPLAY '   (Funcionalidad disponible en próximas'
                   DISPLAY '    versiones)'
           END-EVALUATE
           .

       LIST-ACCOUNTS-CSV.
           DISPLAY ' '
           DISPLAY '📊 ===== CUENTAS REGISTRADAS ====='
           DISPLAY 'ID               | Saldo'
           DISPLAY '----------------------------------------'

           PERFORM VARYING IX-ACC FROM 1 BY 1
               UNTIL IX-ACC > WS-ACC-COUNT
               DISPLAY ACC-ID(IX-ACC)
                   ' | ' ACC-BALANCE(IX-ACC)
           END-PERFORM

           DISPLAY ' '
           .

       *> ============================================================
       *> 🔍 UTILIDADES DE BUSQUEDA
       *> ============================================================

       FIND-ACCOUNT-BY-ID.
           MOVE 'N' TO WS-ACC-FOUND

           PERFORM VARYING IX-ACC FROM 1 BY 1
               UNTIL IX-ACC > WS-ACC-COUNT
                   OR WS-ACC-FOUND = 'Y'
               IF ACC-ID(IX-ACC) = DB-ACCOUNT-ID
                   MOVE 'Y' TO WS-ACC-FOUND
                   MOVE IX-ACC TO WS-ACC-INDEX
               END-IF
           END-PERFORM
           .

       *> ============================================================
       *> 🗄️ INICIALIZACION/FINALIZACION DB2
       *> ============================================================

       INIT-DB.
           MOVE 'INIT    ' TO DB-FUNC
           MOVE SPACES TO DB-ACCOUNT-ID
           MOVE 0 TO DB-AMOUNT
           MOVE 0 TO DB-BALANCE
           MOVE 0 TO DB-STATUS
           MOVE SPACES TO DB-MESSAGE

           CALL WS-MOD-DB-NAME USING DB-REQUEST

           IF NOT DB-OK
               DISPLAY '❌ ERROR AL CONECTAR A DB2: '
                   DB-MESSAGE
               STOP RUN
           END-IF

           DISPLAY '✅ Conectado a DB2'
           DISPLAY ' '
           .

       FINISH-DB.
           MOVE 'FINISH  ' TO DB-FUNC
           CALL WS-MOD-DB-NAME USING DB-REQUEST

           DISPLAY ' '
           DISPLAY '✅ Desconectado de DB2'
           .
