       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONNECT-DB2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SQL-FILE ASSIGN TO
               "/tmp/db2-minibank.sql"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD SQL-FILE.
       01 SQL-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-COMMAND PIC X(500).
       01 WS-RETURN-CODE PIC S9(4) COMP.
       01 WS-DBNAME PIC X(30) VALUE 'MINIBANK'.
       01 WS-USERNAME PIC X(30) VALUE 'db2inst1'.
       01 WS-PASSWORD PIC X(30) VALUE 'password'.
       01 WS-DB-PATH PIC X(50) VALUE
          '/opt/ibm/db2/V12.1/bin/db2'.

       PROCEDURE DIVISION.

      * Crear archivo SQL temporal con comandos
       OPEN OUTPUT SQL-FILE.

       MOVE 'CONNECT TO MINIBANK USER db2inst1 USING password'
           TO SQL-RECORD.
       WRITE SQL-RECORD.

       MOVE 'SELECT * FROM ACCOUNTS'
           TO SQL-RECORD.
       WRITE SQL-RECORD.

       MOVE 'TERMINATE' TO SQL-RECORD.
       WRITE SQL-RECORD.

       CLOSE SQL-FILE.

      * Ejecutar el archivo SQL con db2
       STRING
           WS-DB-PATH DELIMITED BY SPACE
           ' -f /tmp/db2-minibank.sql'
           DELIMITED BY SIZE
           INTO WS-COMMAND
       END-STRING

       DISPLAY 'Connecting to MINIBANK with user db2inst1...'
       DISPLAY 'Retrieving accounts from ACCOUNTS table...'

       CALL "system" USING WS-COMMAND GIVING WS-RETURN-CODE.

       IF WS-RETURN-CODE = 0
          DISPLAY 'Query executed successfully!'
       ELSE
          DISPLAY 'DB2 returned code: ' WS-RETURN-CODE
       END-IF.

       STOP RUN.
