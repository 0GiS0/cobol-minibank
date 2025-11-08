      ******************************************************************
      * MINIBANK-SQL: Ejemplo simple de COBOL con EXEC SQL
      *
      * Proposito: Demostrar operaciones basicas de SQL embebido
      * usando Open Cobol ESQL (ocesql)
      *
      * Autor: AI Coding Agent
      * Fecha: 2025-11-08
      ******************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 MINIBANK-SQL.

      ******************************************************************
       DATA                        DIVISION.
      ******************************************************************
       WORKING-STORAGE             SECTION.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE "minibank".
       01  USERNAME                PIC  X(30) VALUE "db2inst1".
       01  PASSWD                  PIC  X(30) VALUE "password".
       01  ACCT-ID-VAR             PIC  X(30).
       01  ACCT-NAME-VAR           PIC  X(50).
       01  ACCT-BALANCE-VAR        PIC S9(13)V9(2) COMP-3.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

      ******************************************************************
       PROCEDURE                   DIVISION.
      ******************************************************************
       MAIN-RTN.
           DISPLAY "**** MINIBANK SQL EXAMPLE ****".
           PERFORM AA-INITIALIZE.
           PERFORM BB-CONNECT-DB.
           PERFORM CC-CREATE-TABLE.
           PERFORM DD-INSERT-DATA.
           PERFORM EE-DISCONNECT.
           STOP RUN.

      ******************************************************************
       AA-INITIALIZE.
      ******************************************************************
           DISPLAY "Inicializando...".
           MOVE "ACC001" TO ACCT-ID-VAR.
           MOVE "Juan Perez" TO ACCT-NAME-VAR.
           MOVE 5000.00 TO ACCT-BALANCE-VAR.

      ******************************************************************
       BB-CONNECT-DB.
      ******************************************************************
           DISPLAY "Conectando a la BD: minibank @ db".
           DISPLAY "  Usuario: " USERNAME.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD
               USING :DBNAME
           END-EXEC.
           IF SQLCODE NOT = ZERO
               DISPLAY "Error de conexion: " SQLCODE
               STOP RUN
           ELSE
               DISPLAY "Conexion exitosa"
           END-IF.

      ******************************************************************
       CC-CREATE-TABLE.
      ******************************************************************
           DISPLAY "Creando tabla ACCOUNTS...".
           EXEC SQL
               CREATE TABLE ACCOUNTS (
                   ACCOUNT_ID   VARCHAR(30),
                   ACCOUNT_NAME VARCHAR(50),
                   BALANCE      DECIMAL(15,2)
               )
           END-EXEC.
           EVALUATE SQLCODE
               WHEN 0
                   DISPLAY "Tabla creada exitosamente"
               WHEN -601
                   DISPLAY "Tabla ya existe"
               WHEN OTHER
                   DISPLAY "Error: " SQLCODE
           END-EVALUATE.

      ******************************************************************
       DD-INSERT-DATA.
      ******************************************************************
           DISPLAY "Insertando datos...".
           EXEC SQL
               INSERT INTO ACCOUNTS
               VALUES (:ACCT-ID-VAR, :ACCT-NAME-VAR, :ACCT-BALANCE-VAR)
           END-EXEC.
           IF SQLCODE = 0
               DISPLAY "Datos insertados exitosamente"
           ELSE
               DISPLAY "Error en INSERT: " SQLCODE
           END-IF.

      ******************************************************************
       EE-DISCONNECT.
      ******************************************************************
           DISPLAY "Desconectando...".
           EXEC SQL
               DISCONNECT ALL
           END-EXEC.
           DISPLAY "Desconexion completada".
