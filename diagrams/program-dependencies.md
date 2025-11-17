%% MiniBank COBOL Program Dependencies
%% Generated: 2025-11-17

flowchart TB

    %% Subgraphs group related artifacts
    subgraph PROGRAMS[COBOL Programs]
        MBMAIN["MBMAIN\nMain interactive app"]
        MBDBSQL["MBDBSQL\nDB2 access module"]
        MBDBCLI["MBDBCLI\nStub DB module"]
    end

    subgraph COPYBOOKS[Copybooks]
        CPY_DB_IF["mb-db-if.cpy\nDB request/response interface"]
    end

    %% Relationships
    MBMAIN -->|CALL (DB2 mode)| MBDBSQL
    MBMAIN -->|CALL (stub mode)| MBDBCLI
    MBMAIN -->|COPY| CPY_DB_IF
    MBDBSQL -->|COPY| CPY_DB_IF
    MBDBCLI -->|COPY| CPY_DB_IF

    %% Styling
    classDef program fill:#0b5bd7,stroke:#083e8f,stroke-width:1px,color:#ffffff,font-weight:bold;
    classDef copybook fill:#ffd54f,stroke:#c8a600,stroke-width:2px,color:#222222,font-weight:bold;
    class MBMAIN,MBDBSQL,MBDBCLI program;
    class CPY_DB_IF copybook;

    %% Legend (pseudo)
    L1["Legend:\nSolid arrow: dependency\nLabel 'CALL': dynamic invocation\nLabel 'COPY': copybook inclusion"]:::legend
    classDef legend fill:#f0f0f0,stroke:#999,color:#222,font-size:10px;
    %% Position legend
    L1 --- MBMAIN

    %% Notes
    %% MBMAIN selects which DB module to CALL using environment variable MINIBANK_DB_MODULE.
    %% Default module name is 'MBDBSQL '. Setting env to 'MBDBCLI ' switches to stub.
