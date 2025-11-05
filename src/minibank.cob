      ******************************************************************
      * 🏦 COBOL MINIBANK - SISTEMA BANCARIO SIMPLIFICADO
      *
      * Este programa procesa transacciones bancarias desde un archivo
      * CSV y calcula los saldos finales de todas las cuentas.
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
                    PERFORM ACCUMULATE                *> 🧮 Acumular saldo en cuenta
              END-READ
           END-PERFORM

      *    📊 Generar archivo de salida
           PERFORM WRITE-HEADER      *> Escribir encabezado CSV
           PERFORM DUMP-BALANCES     *> Escribir todos los saldos

      *    🔒 Cerrar archivos y terminar programa
           CLOSE TX-FILE
           CLOSE OUT-FILE
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

      * ------------------------------------------------------------
      * 🧮 ACCUMULATE - Acumular transacción en cuenta
      * Busca la cuenta en el array y actualiza su saldo
      * Si no existe, crea una nueva entrada
      * ------------------------------------------------------------
       ACCUMULATE.
      *    🔍 Inicializar búsqueda
           MOVE "N" TO FOUND           *> Flag: cuenta no encontrada aún
           MOVE 1 TO ACCT-IDX          *> Empezar desde el primer elemento

      *    🔄 Buscar cuenta en el array (máximo 100 cuentas)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
              IF ACCT-NAME(ACCT-IDX) = WS-ACCOUNT
      *          ✅ Cuenta encontrada: actualizar saldo existente
                 ADD WS-AMOUNT-SIGNED TO ACCT-BAL(ACCT-IDX)
                 MOVE "Y" TO FOUND
                 EXIT PERFORM         *> Salir del bucle
              ELSE
                 IF ACCT-NAME(ACCT-IDX) = SPACES
      *             🆕 Posición vacía: crear nueva cuenta
                    MOVE WS-ACCOUNT TO ACCT-NAME(ACCT-IDX)
                    MOVE 0 TO ACCT-BAL(ACCT-IDX)    *> Inicializar saldo
                    ADD WS-AMOUNT-SIGNED TO ACCT-BAL(ACCT-IDX)
                    MOVE "Y" TO FOUND
                    EXIT PERFORM      *> Salir del bucle
                 END-IF
              END-IF
              ADD 1 TO ACCT-IDX       *> Avanzar al siguiente elemento
           END-PERFORM
           .

      * ------------------------------------------------------------
      * 📄 WRITE-HEADER - Escribir encabezado CSV
      * Escribe la primera línea del archivo de salida
      * ------------------------------------------------------------
       WRITE-HEADER.
           MOVE "account,balance" TO OUT-LINE
           WRITE OUT-LINE
           .

      * ------------------------------------------------------------
      * 📊 DUMP-BALANCES - Generar reporte de saldos
      * Recorre todas las cuentas y escribe sus saldos al archivo
      * ------------------------------------------------------------
       DUMP-BALANCES.
           MOVE 1 TO ACCT-IDX          *> Empezar desde el primer elemento

      *    🔄 Recorrer array de cuentas
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
              IF ACCT-NAME(ACCT-IDX) NOT = SPACES
      *          📝 Cuenta tiene datos: formatear y escribir
                 MOVE ACCT-BAL(ACCT-IDX) TO FORMATTED-BAL
      *          📋 Construir línea CSV: "cuenta,saldo"
                 STRING
                   ACCT-NAME(ACCT-IDX) DELIMITED BY SPACES  *> Nombre cuenta
                   ","                  DELIMITED BY SIZE    *> Separador
                   FORMATTED-BAL        DELIMITED BY SIZE    *> Saldo formateado
                   INTO OUT-LINE
                 END-STRING
                 WRITE OUT-LINE         *> Escribir línea al archivo
              END-IF
              ADD 1 TO ACCT-IDX         *> Siguiente elemento
           END-PERFORM
           .
