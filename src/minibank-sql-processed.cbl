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

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE "minibank".
       01  USERNAME                PIC  X(30) VALUE "db2inst1".
       01  PASSWD                  PIC  X(30) VALUE "password".
       01  ACCT-ID-VAR             PIC  X(30).
       01  ACCT-NAME-VAR           PIC  X(50).
       01  ACCT-BALANCE-VAR        PIC S9(13)V9(2) COMP-3.
OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

      ******************************************************************
OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(098) VALUE "CREATE TABLE ACCOUNTS ( ACCOUN"
OCESQL  &  "T_ID VARCHAR(30), ACCOUNT_NAME VARCHAR(50), BALANCE DECIMA"
OCESQL  &  "L(15, 2) )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(042) VALUE "INSERT INTO ACCOUNTS VALUES ( "
OCESQL  &  "$1, $2, $3 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
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
OCESQL*    EXEC SQL
OCESQL*        CONNECT :USERNAME IDENTIFIED BY :PASSWD
OCESQL*        USING :DBNAME
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE PASSWD
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 30
OCESQL     END-CALL.
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
OCESQL*    EXEC SQL
OCESQL*        CREATE TABLE ACCOUNTS (
OCESQL*            ACCOUNT_ID   VARCHAR(30),
OCESQL*            ACCOUNT_NAME VARCHAR(50),
OCESQL*            BALANCE      DECIMAL(15,2)
OCESQL*        )
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0001
OCESQL     END-CALL.
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
OCESQL*    EXEC SQL
OCESQL*        INSERT INTO ACCOUNTS
OCESQL*        VALUES (:ACCT-ID-VAR, :ACCT-NAME-VAR, :ACCT-BALANCE-VAR)
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 30
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE ACCT-ID-VAR
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE ACCT-NAME-VAR
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 9
OCESQL          BY VALUE 15
OCESQL          BY VALUE -2
OCESQL          BY REFERENCE ACCT-BALANCE-VAR
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
           IF SQLCODE = 0
               DISPLAY "Datos insertados exitosamente"
           ELSE
               DISPLAY "Error en INSERT: " SQLCODE
           END-IF.

      ******************************************************************
       EE-DISCONNECT.
      ******************************************************************
           DISPLAY "Desconectando...".
OCESQL*    EXEC SQL
OCESQL*        DISCONNECT ALL
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.
           DISPLAY "Desconexion completada".
