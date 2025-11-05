      * Layout de línea CSV (no se usa directamente para IO, es referencia)
       01  CSV-RECORD.
           05  CSV-DATE             PIC X(10).
           05  FILLER               PIC X(1).
           05  CSV-ACCOUNT          PIC X(30).
           05  FILLER               PIC X(1).
           05  CSV-TYPE             PIC X(6).
           05  FILLER               PIC X(1).
           05  CSV-AMOUNT           PIC X(20).
