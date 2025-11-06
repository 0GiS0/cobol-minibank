      ******************************************************************
      * 🏦 COBOL MINIBANK - SISTEMA BANCARIO CON DB2
      *
      * Este programa procesa transacciones bancarias desde un archivo
      * CSV e inserta los datos directamente en DB2.
      *
      * Funciones principales:
      * - Lee transacciones desde transactions.csv
      * - Se conecta a DB2 (hostname: db, puerto: 50000)
      * - Inserta transacciones en tabla TRANSACTIONS
      * - Consulta y muestra saldos desde ACCOUNTS
      * - Genera reporte en balances.csv
      ******************************************************************

      * ============================================================
      * 🆔 IDENTIFICATION DIVISION
      * Define la identidad del programa
      * ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIBANK.

      * Incluir el archivo de comunicación con SQL
       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLTYPES END-EXEC.

      * ============================================================
      * 🌐 ENVIRONMENT DIVISION
      * Define los recursos externos (archivos, dispositivos)
      * ============================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    📥 Archivo de transacciones (entrada)
      *    DYNAMIC permite cambiar la ruta en tiempo de ejecución
           SELECT TX-FILE ASSIGN TO DYNAMIC TX-PATH
               ORGANIZATION IS LINE SEQUENTIAL.
      *    📤 Archivo de saldos (salida)
           SELECT OUT-FILE ASSIGN TO DYNAMIC OUT-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

      * ============================================================
      * 📊 DATA DIVISION
      * Define todas las variables y estructuras de datos
      * ============================================================
       DATA DIVISION.

      * ------------------------------------------------------------
      * 📁 FILE SECTION - Estructura de los archivos
      * ------------------------------------------------------------
       FILE SECTION.
       FD  TX-FILE.
       01  TX-LINE              PIC X(256).    *> Línea del archivo de transacciones
       FD  OUT-FILE.
       01  OUT-LINE             PIC X(256).    *> Línea del archivo de salida

      * ------------------------------------------------------------
      * 💾 WORKING-STORAGE SECTION - Variables del programa
      * ------------------------------------------------------------
       WORKING-STORAGE SECTION.

      * 📂 Variables para rutas de archivos
       77  TX-PATH              PIC X(256).    *> Ruta del archivo de transacciones
       77  OUT-PATH             PIC X(256).    *> Ruta del archivo de salida

      * 🔄 Variables de control de flujo
       77  EOF                  PIC X VALUE "N". *> End-Of-File flag (Y/N)
       77  WS-LINE              PIC X(256).    *> Línea de trabajo temporal
       77  SEP                  PIC X VALUE ",". *> Separador CSV (coma)

      * 📝 Variables para parsing de transacciones CSV
      * Formato: fecha,cuenta,tipo,cantidad
       77  WS-DATE              PIC X(10).     *> Fecha de transacción
       77  WS-ACCOUNT           PIC X(30).     *> Número de cuenta
       77  WS-TYPE              PIC X(6).      *> Tipo: CREDIT o DEBIT
       77  WS-AMOUNT-STR        PIC X(20).     *> Cantidad como string
       77  WS-AMOUNT-SIGNED     PIC S9(13)V9(2) VALUE 0. *> Cantidad numérica con signo

      * 🔍 Variables para búsqueda y control de arrays
       77  I                    PIC 9(4) COMP VALUE 0.    *> Contador de bucle
       77  FOUND                PIC X VALUE "N".          *> Flag de búsqueda (Y/N)

      * 🏦 Array de cuentas bancarias (máximo 100 cuentas)
       01  ACCOUNTS.
           05 ACCT-ENTRY OCCURS 100 TIMES.
              10 ACCT-NAME       PIC X(30).              *> Nombre de la cuenta
              10 ACCT-BAL        PIC S9(13)V9(2).        *> Saldo de la cuenta

      * 📊 Variables para generación de reportes
       77  ACCT-IDX             PIC 9(4) COMP VALUE 1.   *> Índice actual del array
       77  FORMATTED-BAL        PIC -(12)9.99.           *> Saldo formateado para salida

      * 🔗 VARIABLES PARA DB2 (HOST VARIABLES)
      * Estas variables se usan en las sentencias EXEC SQL
       77  DB-ACCOUNT-ID        PIC 9(10).               *> ID de cuenta en DB2
       77  DB-ACCOUNT-NAME      PIC X(30).               *> Nombre de cuenta en DB2
       77  DB-BALANCE           PIC S9(13)V9(2).         *> Saldo desde DB2
       77  DB-TX-DATE           PIC X(10).               *> Fecha para DB2
       77  DB-TX-TYPE           PIC X(10).               *> Tipo de transacción (CREDIT/DEBIT)
       77  DB-TX-AMOUNT         PIC S9(13)V9(2).         *> Cantidad para DB2
       77  DB-RC                PIC S9(9) COMP VALUE 0.  *> Return code de SQL

      * 📊 Variables de cursor para consultas
       77  DB-EOF               PIC X VALUE "N".         *> Flag End-Of-Fetch

      * ============================================================
      * 🔗 SQL SECTION - Declarar cursores para consultas
      * ============================================================
       SQL SECTION.

      * Cursor para obtener todas las cuentas y sus saldos
       DECLARE ACCOUNTS-CURSOR CURSOR FOR
           SELECT ACCOUNT_ID, ACCOUNT_NAME, BALANCE
           FROM ACCOUNTS
           ORDER BY ACCOUNT_NAME
           FOR READ ONLY.
           END-EXEC.


      * ============================================================
      * ⚙️ PROCEDURE DIVISION
      * Contiene la lógica principal del programa
      * ============================================================
       PROCEDURE DIVISION.

      * ------------------------------------------------------------
      * 🚀 PROCEDIMIENTO PRINCIPAL
      * Controla el flujo general del procesamiento
      * ------------------------------------------------------------
       MAIN.
      *    ✅ CONECTAR A DB2
           PERFORM CONNECT-DB2.
           IF SQLCODE NOT = 0
              DISPLAY "❌ Error conectando a DB2: " SQLCODE
              GOBACK
           END-IF
           DISPLAY "✅ Conectado a DB2 exitosamente" UPON CONSOLE.

      *    📂 Configurar rutas de archivos
           MOVE "data/transactions.csv" TO TX-PATH
           MOVE "data/balances.csv" TO OUT-PATH.

      *    📂 Abrir archivos para lectura y escritura
           OPEN INPUT TX-FILE      *> Archivo de transacciones (solo lectura)
           OPEN OUTPUT OUT-FILE    *> Archivo de saldos (solo escritura)

      *    🔄 BUCLE PRINCIPAL: Procesar cada transacción
           PERFORM UNTIL EOF = "Y"
              READ TX-FILE
                 AT END MOVE "Y" TO EOF                *> Fin del archivo alcanzado
                 NOT AT END
                    MOVE TX-LINE TO WS-LINE           *> Copiar línea a variable de trabajo
                    PERFORM PARSE-LINE                *> 📝 Analizar campos CSV
                    PERFORM INSERT-TRANSACTION        *> 🔗 Insertar en DB2
              END-READ
           END-PERFORM

      *    📊 Consultar saldos desde DB2 y generar reporte
           PERFORM WRITE-HEADER      *> Escribir encabezado CSV
           PERFORM QUERY-BALANCES    *> Consultar DB2 y escribir saldos

      *    🔒 Cerrar archivos y desconectar
           CLOSE TX-FILE
           CLOSE OUT-FILE
           PERFORM DISCONNECT-DB2
           GOBACK.


      * ------------------------------------------------------------
      * 📝 PARSE-LINE - Analizar línea CSV
      * Separa una línea CSV en sus campos individuales
      * Formato esperado: fecha,cuenta,tipo,cantidad
      * ------------------------------------------------------------
       PARSE-LINE.
      *    🔤 Separar la línea CSV por comas
      *    UNSTRING divide la cadena usando "," como delimitador
           UNSTRING WS-LINE DELIMITED BY ALL ","
                INTO WS-DATE             *> Campo 1: Fecha (2025-01-10)
                     WS-ACCOUNT          *> Campo 2: Cuenta (ACC-001)
                     WS-TYPE             *> Campo 3: Tipo (CREDIT/DEBIT)
                     WS-AMOUNT-STR       *> Campo 4: Cantidad (1000.50)
           END-UNSTRING

      *    🔢 Convertir cantidad de texto a número
      *    Reemplazar comas decimales por puntos (formato internacional)
           INSPECT WS-AMOUNT-STR REPLACING ALL "," BY "."
      *    NUMVAL convierte string a número decimal
           MOVE FUNCTION NUMVAL(WS-AMOUNT-STR)
                TO WS-AMOUNT-SIGNED.

      *    💸 Si es DEBIT, convertir a cantidad negativa
           IF WS-TYPE = "DEBIT"
              MULTIPLY -1 BY WS-AMOUNT-SIGNED
           END-IF
           .

      * ============================================================
      * 🔗 CONNECT-DB2 - Conectar a la base de datos
      * ============================================================
       CONNECT-DB2.
           DISPLAY "🔌 Conectando a DB2..." UPON CONSOLE.
           EXEC SQL
               CONNECT TO minibank USER 'db2inst1' USING 'password'
               WITH URI 'HOSTNAME=db;PORT=50000;'
           END-EXEC.

           EVALUATE SQLCODE
              WHEN 0
                 DISPLAY "✅ Conexión exitosa a DB2" UPON CONSOLE
              WHEN -30081
                 DISPLAY "❌ Error: No se puede contactar el servidor DB2" UPON CONSOLE
              WHEN -30082
                 DISPLAY "❌ Error: Credenciales inválidas" UPON CONSOLE
              WHEN OTHER
                 DISPLAY "❌ Error SQL: " SQLCODE UPON CONSOLE
           END-EVALUATE
           .

      * ============================================================
      * 🔗 INSERT-TRANSACTION - Insertar transacción en DB2
      * ============================================================
       INSERT-TRANSACTION.
      *    Preparar variables para DB2
           MOVE WS-DATE TO DB-TX-DATE
           MOVE WS-ACCOUNT TO DB-ACCOUNT-NAME
           MOVE WS-TYPE TO DB-TX-TYPE
           MOVE WS-AMOUNT-SIGNED TO DB-TX-AMOUNT

      *    Insertar transacción en tabla TRANSACTIONS
           EXEC SQL
               INSERT INTO TRANSACTIONS
                   (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
               VALUES
                   ((SELECT ACCOUNT_ID FROM ACCOUNTS
                     WHERE ACCOUNT_NAME = :DB-ACCOUNT-NAME
                     FETCH FIRST ROW ONLY),
                    :DB-TX-DATE,
                    :DB-TX-TYPE,
                    :DB-TX-AMOUNT)
           END-EXEC.

           EVALUATE SQLCODE
              WHEN 0
                 DISPLAY "✅ Transacción insertada: "
                        WS-ACCOUNT " " WS-TYPE " " WS-AMOUNT-STR
                     UPON CONSOLE
              WHEN 100
                 DISPLAY "⚠️  Cuenta no encontrada: " WS-ACCOUNT
                     UPON CONSOLE
                 PERFORM CREATE-ACCOUNT
              WHEN OTHER
                 DISPLAY "❌ Error insertando transacción: " SQLCODE
                     UPON CONSOLE
           END-EVALUATE
           .

      * ============================================================
      * 🏦 CREATE-ACCOUNT - Crear nueva cuenta si no existe
      * ============================================================
       CREATE-ACCOUNT.
           MOVE WS-ACCOUNT TO DB-ACCOUNT-NAME
           MOVE 0 TO DB-BALANCE

           EXEC SQL
               INSERT INTO ACCOUNTS (ACCOUNT_NAME, BALANCE)
               VALUES (:DB-ACCOUNT-NAME, :DB-BALANCE)
           END-EXEC.

           IF SQLCODE = 0
              DISPLAY "✅ Cuenta creada: " WS-ACCOUNT UPON CONSOLE
              PERFORM INSERT-TRANSACTION
           ELSE
              DISPLAY "❌ Error creando cuenta: " SQLCODE UPON CONSOLE
           END-IF
           .

      * ============================================================
      * 📊 QUERY-BALANCES - Consultar saldos desde DB2
      * ============================================================
       QUERY-BALANCES.
           DISPLAY "📊 Consultando saldos desde DB2..." UPON CONSOLE.

           EXEC SQL
               DECLARE CURSOR1 CURSOR FOR
               SELECT ACCOUNT_NAME, BALANCE
               FROM ACCOUNTS
               ORDER BY ACCOUNT_NAME
           END-EXEC.

           EXEC SQL
               OPEN CURSOR1
           END-EXEC.

           IF SQLCODE NOT = 0
              DISPLAY "❌ Error abriendo cursor: " SQLCODE UPON CONSOLE
              EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO DB-EOF
           PERFORM UNTIL DB-EOF = "Y"
              EXEC SQL
                  FETCH CURSOR1
                  INTO :DB-ACCOUNT-NAME, :DB-BALANCE
              END-EXEC

              EVALUATE SQLCODE
                 WHEN 0
                    MOVE DB-BALANCE TO FORMATTED-BAL
                    STRING
                      DB-ACCOUNT-NAME DELIMITED BY SPACES
                      ","              DELIMITED BY SIZE
                      FORMATTED-BAL    DELIMITED BY SIZE
                      INTO OUT-LINE
                    END-STRING
                    WRITE OUT-LINE
                    DISPLAY "  " DB-ACCOUNT-NAME " " FORMATTED-BAL
                        UPON CONSOLE
                 WHEN 100
                    MOVE "Y" TO DB-EOF
                 WHEN OTHER
                    DISPLAY "❌ Error en fetch: " SQLCODE UPON CONSOLE
                    MOVE "Y" TO DB-EOF
              END-EVALUATE
           END-PERFORM.

           EXEC SQL
               CLOSE CURSOR1
           END-EXEC
           .

      * ============================================================
      * 🔗 DISCONNECT-DB2 - Desconectar de DB2
      * ============================================================
       DISCONNECT-DB2.
           DISPLAY "🔌 Desconectando de DB2..." UPON CONSOLE.
           EXEC SQL
               DISCONNECT ALL
           END-EXEC.

           IF SQLCODE = 0
              DISPLAY "✅ Desconexión exitosa" UPON CONSOLE
           ELSE
              DISPLAY "⚠️  Advertencia al desconectar: " SQLCODE
                  UPON CONSOLE
           END-IF
           .

      * ------------------------------------------------------------
      *  WRITE-HEADER - Escribir encabezado CSV
      * Escribe la primera línea del archivo de salida
      * ------------------------------------------------------------
       WRITE-HEADER.
           MOVE "account,balance" TO OUT-LINE
           WRITE OUT-LINE
           .
