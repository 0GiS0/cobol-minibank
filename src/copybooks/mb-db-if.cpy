      * ============================================================
      * 📋 mb-db-if.cpy
      * Interfaz de petición/respuesta hacia el módulo DB
      * ============================================================
       01  DB-REQUEST.
           05 DB-FUNC          PIC X(8).
              88 FUNC-INIT     VALUE 'INIT    '.
              88 FUNC-FINISH   VALUE 'FINISH  '.
              88 FUNC-BALANCE  VALUE 'BALANCE '.
              88 FUNC-DEPOSIT  VALUE 'DEPOSIT '.
              88 FUNC-WITHDRAW VALUE 'WITHDRW '.
              88 FUNC-LISTACCT VALUE 'LISTACCT '.
           05 DB-ACCOUNT-ID    PIC X(30).
           05 DB-ACCOUNT-NAME  PIC X(100).
           05 DB-AMOUNT        PIC S9(13)V9(2) COMP-3.
           05 DB-TRANSACTION-TYPE PIC X(10).
           05 DB-TRANSACTION-DATE PIC X(10).
           05 DB-STATUS        PIC S9(4) COMP.
              88 DB-OK         VALUE 0.
              88 DB-ERROR      VALUE 1 THRU 999.
           05 DB-MESSAGE       PIC X(80).
           05 DB-BALANCE       PIC S9(13)V9(2) COMP-3.
           05 DB-LIST-COUNT    PIC S9(4) COMP VALUE 0.
           05 DB-LIST-MAX      PIC S9(4) COMP VALUE 50.
           05 DB-LIST-TRUNCATED PIC X VALUE 'N'.
              88 DB-LIST-FULL  VALUE 'Y'.
           05 DB-ACCOUNT-LIST OCCURS 50 TIMES INDEXED BY DB-IX.
              10 DB-LIST-ACCOUNT-ID   PIC X(30).
              10 DB-LIST-BALANCE      PIC S9(13)V9(2) COMP-3.
