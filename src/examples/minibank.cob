      ******************************************************************
      * 🏦 COBOL MINIBANK - SISTEMA BANCARIO SIMPLIFICADO
      *
      * Versión básica sin DB2
      * Procesa transacciones desde CSV y genera reporte de saldos
      *
      * Funciones principales:
      * - Lee transacciones desde transactions.csv
      * - Procesa depósitos (CREDIT) y retiros (DEBIT)
      * - Calcula saldos por cuenta
      * - Genera reporte en balances.csv
      ******************************************************************

      * ============================================================
      * 🆔 IDENTIFICATION DIVISION
      * Define la identidad del programa
      * ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIBANK.

      * ============================================================
      * 🌐 ENVIRONMENT DIVISION
      * Define los recursos externos (archivos, dispositivos)
      * ============================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    📥 Archivo de transacciones (entrada)
           SELECT TX-FILE ASSIGN TO "data/transactions.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
      *    📤 Archivo de saldos (salida)
           SELECT OUT-FILE ASSIGN TO "data/balances.csv"
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
       01  TX-LINE              PIC X(256).

       FD  OUT-FILE.
       01  OUT-LINE             PIC X(256).

      * ------------------------------------------------------------
      * 💾 WORKING-STORAGE SECTION - Variables del programa
      * ------------------------------------------------------------
       WORKING-STORAGE SECTION.

      *  Variables de control de flujo
       77  EOF                  PIC X VALUE "N".
       77  WS-LINE              PIC X(256).

      * 📝 Variables para parsing de transacciones CSV
      * Formato: fecha,cuenta,tipo,cantidad
       77  WS-DATE              PIC X(10).
       77  WS-ACCOUNT           PIC X(30).
       77  WS-TYPE              PIC X(6).
       77  WS-AMOUNT-STR        PIC X(20).
       77  WS-AMOUNT            PIC S9(13)V9(2) VALUE 0.

      * 🏦 Array de cuentas (máximo 100 cuentas)
       01  ACCOUNTS.
           05 ACCT-ENTRY OCCURS 100 TIMES.
              10 ACCT-NAME       PIC X(30).
              10 ACCT-BAL        PIC S9(13)V9(2).

      * 📊 Variables de búsqueda y reportes
       77  ACCT-COUNT           PIC 9(4) COMP VALUE 0.
       77  I                    PIC 9(4) COMP.
       77  J                    PIC 9(4) COMP.
       77  FOUND                PIC X.
       77  FORMATTED-BAL        PIC -(12)9.99.


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
           DISPLAY "🏦 COBOL MiniBank - Procesando transacciones..."
               UPON CONSOLE.

      *    Abrir archivos
           OPEN INPUT TX-FILE.
           OPEN OUTPUT OUT-FILE.

      *    Escribir encabezado CSV
           PERFORM WRITE-HEADER.

      *    Inicializar array de cuentas
           MOVE 0 TO ACCT-COUNT.
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > 100
               MOVE SPACES TO ACCT-NAME(I)
               MOVE 0 TO ACCT-BAL(I)
           END-PERFORM.

      *    Leer y procesar transacciones
           PERFORM UNTIL EOF = "Y"
               READ TX-FILE
                   AT END MOVE "Y" TO EOF
                   NOT AT END
                       MOVE TX-LINE TO WS-LINE
                       PERFORM PARSE-AND-PROCESS
               END-READ
           END-PERFORM.

      *    Generar reporte
           PERFORM WRITE-REPORT.

      *    Cerrar archivos
           CLOSE TX-FILE.
           CLOSE OUT-FILE.

           DISPLAY "✅ Procesamiento completado"
               UPON CONSOLE.
           DISPLAY "📊 Reporte generado en data/balances.csv"
               UPON CONSOLE.

           GOBACK.


      * ============================================================
      * 📝 PARSE-AND-PROCESS - Analizar y procesar línea CSV
      * Separa una línea CSV en sus campos y actualiza saldos
      * Formato esperado: fecha,cuenta,tipo,cantidad
      * ============================================================
       PARSE-AND-PROCESS.
      *    🔤 Separar la línea CSV por comas
      *    UNSTRING divide la cadena usando "," como delimitador
           UNSTRING WS-LINE DELIMITED BY ALL ","
                INTO WS-DATE             *> Campo 1: Fecha (2025-01-10)
                     WS-ACCOUNT          *> Campo 2: Cuenta (ACC-001)
                     WS-TYPE             *> Campo 3: Tipo (CREDIT/DEBIT)
                     WS-AMOUNT-STR       *> Campo 4: Cantidad (1000.50)
           END-UNSTRING.

      *    🔢 Convertir cantidad de texto a número
      *    Reemplazar comas decimales por puntos (formato internacional)
           INSPECT WS-AMOUNT-STR REPLACING ALL "," BY ".".
      *    NUMVAL convierte string a número decimal
           MOVE FUNCTION NUMVAL(WS-AMOUNT-STR)
                TO WS-AMOUNT.

      *    💸 Si es DEBIT, convertir a cantidad negativa
           IF WS-TYPE = "DEBIT"
              MULTIPLY -1 BY WS-AMOUNT
           END-IF.

      *    🔗 Buscar o crear cuenta y actualizar saldo
           PERFORM FIND-OR-CREATE-ACCOUNT.
           PERFORM UPDATE-BALANCE.


      * ============================================================
      * � FIND-OR-CREATE-ACCOUNT - Buscar cuenta o crear si no existe
      * ============================================================
       FIND-OR-CREATE-ACCOUNT.
      *    Buscar la cuenta en el array
           MOVE "N" TO FOUND.
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ACCT-COUNT OR FOUND = "Y"
               IF ACCT-NAME(I) = WS-ACCOUNT
                   MOVE "Y" TO FOUND
               END-IF
           END-PERFORM.

      *    Si no existe, crear cuenta nueva
           IF FOUND = "N"
               IF ACCT-COUNT < 100
                   ADD 1 TO ACCT-COUNT
                   MOVE WS-ACCOUNT TO ACCT-NAME(ACCT-COUNT)
                   MOVE 0 TO ACCT-BAL(ACCT-COUNT)
                   DISPLAY "✨ Cuenta nueva: " WS-ACCOUNT
                       UPON CONSOLE
               ELSE
                   DISPLAY "❌ Error: Máximo de cuentas alcanzado"
                       UPON CONSOLE
               END-IF
           END-IF.


      * ============================================================
      * 💰 UPDATE-BALANCE - Actualizar saldo de cuenta
      * ============================================================
       UPDATE-BALANCE.
      *    Buscar la cuenta y actualizar su saldo
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ACCT-COUNT
               IF ACCT-NAME(I) = WS-ACCOUNT
                   ADD WS-AMOUNT TO ACCT-BAL(I)
                   MOVE ACCT-BAL(I) TO FORMATTED-BAL
                   DISPLAY "  📝 " WS-ACCOUNT " " WS-TYPE
                       " " FORMATTED-BAL UPON CONSOLE
               END-IF
           END-PERFORM.


      * ============================================================
      * 📊 WRITE-REPORT - Generar reporte de saldos
      * ============================================================
       WRITE-REPORT.
           DISPLAY " " UPON CONSOLE.
           DISPLAY "📄 Generando reporte de saldos..." UPON CONSOLE.

      *    Escribir cada cuenta y su saldo en el archivo de salida
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ACCT-COUNT
               IF ACCT-NAME(I) NOT = SPACES
                   MOVE ACCT-BAL(I) TO FORMATTED-BAL
                   STRING
                       ACCT-NAME(I) DELIMITED BY SPACES
                       "," DELIMITED BY SIZE
                       FUNCTION TRIM(FORMATTED-BAL) DELIMITED BY SIZE
                       INTO OUT-LINE
                   END-STRING
                   WRITE OUT-LINE
                   DISPLAY "  " ACCT-NAME(I) " = " FORMATTED-BAL
                       UPON CONSOLE
               END-IF
           END-PERFORM.


      * ============================================================
      * � WRITE-HEADER - Escribir encabezado CSV
      * Escribe la primera línea del archivo de salida
      * ============================================================
       WRITE-HEADER.
           MOVE "account,balance" TO OUT-LINE.
           WRITE OUT-LINE.
