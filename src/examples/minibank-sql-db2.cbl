      ******************************************************************
      * MINIBANK-SQL-DB2: COBOL con EXEC SQL para DB2
      *
      * Proposito: Demostrar operaciones SQL embebidas usando db2precompile
      * Conecta a DB2 y realiza operaciones CRUD
      *
      * Autor: AI Coding Agent
      * Fecha: 2025-11-08
      * Nota: Requiere compilación con db2precompile, no ocesql
      ******************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 MINIBANK-SQL-DB2.

      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.

      * Variables para EXEC SQL BEGIN DECLARE SECTION
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE "minibank".
       01  USERNAME                PIC  X(30) VALUE "db2inst1".
       01  PASSWD                  PIC  X(30) VALUE "password".
       01  ACCT-ID-VAR             PIC  X(30).
       01  ACCT-NAME-VAR           PIC  X(50).
       01  ACCT-BALANCE-VAR        PIC S9(13)V9(2) COMP-3.
       EXEC SQL END DECLARE SECTION END-EXEC.

      * SQLCA para manejo de errores
       EXEC SQL INCLUDE SQLCA END-EXEC.

      * Variables de trabajo
       77  WS-RC                   PIC S9(9) COMP.
       77  WS-SQLCODE              PIC S9(9) VALUE 0.

      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
           DISPLAY "**** MINIBANK SQL DB2 EXAMPLE ****".
           DISPLAY "".

           PERFORM AA-INITIALIZE.
           PERFORM BB-CONNECT-DB.
           PERFORM CC-CREATE-TABLE.
           PERFORM DD-INSERT-DATA.
           PERFORM EE-QUERY-DATA.
           PERFORM FF-DISCONNECT.

           DISPLAY "".
           DISPLAY "✅ Programa completado exitosamente".
           STOP RUN.

      ******************************************************************
       AA-INITIALIZE.
      ******************************************************************
           DISPLAY "📋 Inicializando variables...".
           MOVE "ACC001" TO ACCT-ID-VAR.
           MOVE "Juan Perez" TO ACCT-NAME-VAR.
           MOVE 5000.00 TO ACCT-BALANCE-VAR.
           DISPLAY "  ✓ Variables inicializadas".

      ******************************************************************
       BB-CONNECT-DB.
      ******************************************************************
           DISPLAY "🔗 Conectando a DB2...".
           DISPLAY "  Base de datos: " DBNAME.
           DISPLAY "  Usuario: " USERNAME.

           EXEC SQL
               CONNECT TO :DBNAME USER :USERNAME USING :PASSWD
           END-EXEC.

           MOVE SQLCODE TO WS-SQLCODE.

           IF WS-SQLCODE NOT = 0
               DISPLAY "❌ Error de conexion: " WS-SQLCODE
               DISPLAY "   SQLSTATE: " SQLSTATE
               PERFORM FF-DISCONNECT
               STOP RUN
           ELSE
               DISPLAY "  ✓ Conexion exitosa"
           END-IF.

      ******************************************************************
       CC-CREATE-TABLE.
      ******************************************************************
           DISPLAY "📊 Creando tabla ACCOUNTS...".

           EXEC SQL
               CREATE TABLE ACCOUNTS (
                   ACCOUNT_ID   VARCHAR(30) NOT NULL PRIMARY KEY,
                   ACCOUNT_NAME VARCHAR(50),
                   BALANCE      DECIMAL(15,2)
               )
           END-EXEC.

           MOVE SQLCODE TO WS-SQLCODE.

           EVALUATE WS-SQLCODE
               WHEN 0
                   DISPLAY "  ✓ Tabla creada exitosamente"
               WHEN -601
                   DISPLAY "  ℹ️ Tabla ya existe"
               WHEN OTHER
                   DISPLAY "  ⚠️ Error: " WS-SQLCODE
                   DISPLAY "     SQLSTATE: " SQLSTATE
           END-EVALUATE.

      ******************************************************************
       DD-INSERT-DATA.
      ******************************************************************
           DISPLAY "📝 Insertando datos...".

           EXEC SQL
               INSERT INTO ACCOUNTS
               VALUES (:ACCT-ID-VAR, :ACCT-NAME-VAR, :ACCT-BALANCE-VAR)
           END-EXEC.

           MOVE SQLCODE TO WS-SQLCODE.

           IF WS-SQLCODE = 0
               DISPLAY "  ✓ Datos insertados exitosamente"
               DISPLAY "    Cuenta: " ACCT-ID-VAR
               DISPLAY "    Nombre: " ACCT-NAME-VAR
               DISPLAY "    Saldo: " ACCT-BALANCE-VAR
           ELSE
               DISPLAY "  ❌ Error en INSERT: " WS-SQLCODE
               DISPLAY "     SQLSTATE: " SQLSTATE
           END-IF.

      ******************************************************************
       EE-QUERY-DATA.
      ******************************************************************
           DISPLAY "🔍 Consultando datos...".

           EXEC SQL
               SELECT ACCOUNT_ID, ACCOUNT_NAME, BALANCE
               INTO :ACCT-ID-VAR, :ACCT-NAME-VAR, :ACCT-BALANCE-VAR
               FROM ACCOUNTS
               WHERE ACCOUNT_ID = 'ACC001'
           END-EXEC.

           MOVE SQLCODE TO WS-SQLCODE.

           IF WS-SQLCODE = 0
               DISPLAY "  ✓ Datos consultados exitosamente:"
               DISPLAY "    Cuenta: " ACCT-ID-VAR
               DISPLAY "    Nombre: " ACCT-NAME-VAR
               DISPLAY "    Saldo: " ACCT-BALANCE-VAR
           ELSE IF WS-SQLCODE = 100
               DISPLAY "  ℹ️ No hay registros"
           ELSE
               DISPLAY "  ❌ Error en SELECT: " WS-SQLCODE
               DISPLAY "     SQLSTATE: " SQLSTATE
           END-IF.

      ******************************************************************
       FF-DISCONNECT.
      ******************************************************************
           DISPLAY "🔌 Desconectando de DB2...".

           EXEC SQL
               DISCONNECT ALL
           END-EXEC.

           MOVE SQLCODE TO WS-SQLCODE.

           IF WS-SQLCODE = 0
               DISPLAY "  ✓ Desconexion completada"
           ELSE
               DISPLAY "  ⚠️ Error al desconectar: " WS-SQLCODE
           END-IF.
