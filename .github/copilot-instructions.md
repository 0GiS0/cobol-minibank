# COBOL MiniBank - AI Coding Agent Instructions

## 🏗️ Architecture Overview

This is a **modular educational COBOL repository** featuring a sophisticated dual-mode architecture:

**Current Modular Architecture (2025):**
- **`mb-main.cbl`** (MBMAIN) - Main interactive application with dual-mode support
- **`mb-db-sql.cbl`** (MBDBSQL) - DB2 database access module 
- **`mb-db-cli.cbl`** (MBDBCLI) - Stub/testing module with simulated data
- **`mb-db-if.cpy`** - Shared copybook defining the interface contract

**Legacy Educational Programs (reference):**
- **`minibank.cob`** - Basic CSV file processing (entry level)
- **`minibank-db2.cob`** - DB2 integration via Python wrapper (intermediate)  
- **`minibank-menu.cob`** - Interactive menu system with DB2 (advanced)

The system demonstrates COBOL's evolution from monolithic programs to modular, pluggable architectures used in modern enterprise systems.

## 🔧 Development Environment

- **Container-based**: Uses `.devcontainer/` with DB2 Express-C database
- **Build System**: GNU COBOL (cobc) via Makefile with specific flags: `-x -Wall -O2 -I src/copybooks`
- **Key Directories**: 
  - `src/` - COBOL source files (.cob)
  - `src/copybooks/` - Shared data structures (.cpy)
  - `data/` - CSV input/output files
  - `.devcontainer/` - DB2 setup, Python interfaces, sample data loaders

## 🚀 Essential Workflows

### Current Modular System Commands
```bash
# Main program with dual-mode support
make run-menu           # Builds mb-main + modules, initializes DB2, runs interactive app

# Individual module builds (for development)
cobc -x -Wall -O2 -I src/copybooks -o src/mb-main src/mb-main.cbl
cobc -x -Wall -O2 -I src/copybooks -o src/mb-db-sql src/mb-db-sql.cbl  
cobc -x -Wall -O2 -I src/copybooks -o src/mb-db-cli src/mb-db-cli.cbl

# Legacy educational programs (still functional)
make run                # Basic CSV processing (minibank.cob)
make run-db2           # DB2 via Python wrapper (minibank-db2.cob)
```

### Dual-Mode Architecture Pattern
The main program (`MBMAIN`) dynamically selects database modules at runtime:

```cobol
*> Environment variable determines which DB module to load
ACCEPT WS-ENV-DB-MODULE FROM ENVIRONMENT 'MINIBANK_DB_MODULE'
MOVE WS-ENV-DB-MODULE TO WS-MOD-DB-NAME

*> Runtime module selection via CALL
CALL WS-MOD-DB-NAME USING DB-REQUEST
```

**Configuration:**
- `MINIBANK_DB_MODULE=MBDBSQL ` → Production DB2 mode
- `MINIBANK_DB_MODULE=MBDBCLI ` → Development/testing mode
- `MINIBANK_DATA_SOURCE=CSV` → Legacy CSV mode (bypasses modules)

### Module Interface Contract
All database modules implement the same interface defined in `mb-db-if.cpy`:
- **Functions**: `INIT`, `FINISH`, `BALANCE`, `DEPOSIT`, `WITHDRW`
- **Data Structure**: Account ID, amount, status, message, balance
- **Return Codes**: 0=success, >0=error with descriptive message

## 📊 COBOL-Specific Conventions

### File Structure (Fixed Format)
- **Columns 1-6**: Sequence numbers (usually spaces in modern COBOL)
- **Column 7**: Indicator area (`*` for comments, `-` for continuation)
- **Columns 8-11**: Area A (DIVISION, SECTION, paragraph names)
- **Columns 12-72**: Area B (statements, data descriptions)
- **73-80**: Ignored (historically for sequence numbers)

### Data Types & Precision
- **Monetary amounts**: `PIC S9(13)V9(2)` (signed, 13 digits + 2 decimals)
- **Account IDs**: `PIC X(30)` (alphanumeric, variable length)
- **Dates**: `PIC X(10)` (YYYY-MM-DD format)
- **Computational fields**: Use `COMP` or `COMP-3` for performance

### Memory Management Patterns
- **Arrays**: `OCCURS` clause with bounds checking
- **File processing**: Sequential with `READ...AT END` pattern
- **String operations**: `UNSTRING` for CSV parsing, `STRING` for formatting
- **Variable initialization**: Explicitly set with `VALUE` clauses

## 🔄 Data Flow Patterns

### Modular Architecture (Current - mb-main.cbl)
```
User Input → MBMAIN → Module Selection → MBDBSQL/MBDBCLI
                                      ↓
DB Request Structure ←→ Database Operations ←→ DB2/Stub Data
                                      ↓
Response Structure ←→ MBMAIN ←→ Display Results
```

**Key Pattern**: Dependency injection via environment variables allows runtime module swapping without recompilation.

### Legacy CSV Processing (minibank.cob)
1. Read `data/transactions.csv` line by line
2. Parse with `UNSTRING` using "," delimiter  
3. Accumulate balances in memory arrays
4. Write formatted output to `data/balances.csv`

### Legacy DB2 Integration (minibank-db2.cob)
1. Process CSV transactions → Python scripts → Insert to DB2
2. Query balances from DB2 → Shell scripts → Temp files
3. COBOL reads temp files and formats final output

### Module Communication Protocol
All database operations use standardized request/response:
```cobol
COPY mb-db-if.           *> Shared interface definition
MOVE 'BALANCE ' TO DB-FUNC
MOVE 'ACC-001' TO DB-ACCOUNT-ID  
CALL WS-MOD-DB-NAME USING DB-REQUEST
IF DB-OK
    DISPLAY 'Balance: ' DB-BALANCE
ELSE
    DISPLAY 'Error: ' DB-MESSAGE
END-IF
```

## 🐛 Common Issues & Debugging

### File Path Issues
- Use absolute paths in `SELECT` statements or environment variables
- Ensure `data/` directory exists before file operations
- Check file permissions in container environment

### COBOL Compilation Errors
- Verify column alignment (especially Area A vs Area B)
- Check for missing periods after statements
- Ensure `COPY` statements reference existing `.cpy` files

### DB2 Connection Issues
- Run `python3 .devcontainer/verify-db2.sh` to check DB2 status
- Environment variables loaded from `.devcontainer/.db2.env`
- Python dependencies managed in `post-create.sh`

## 🧪 Testing & Validation

### Dual-Mode Testing
```bash
# Test with stub data (no DB2 required)
export MINIBANK_DB_MODULE=MBDBCLI
./src/mb-main

# Test with real DB2 connection  
export MINIBANK_DB_MODULE=MBDBSQL
./src/mb-main

# Test script for both modes
./test-dual-mode.sh
```

### Data Sources
- **Sample Data**: `data/transactions.csv` (5 sample transactions for CSV mode)
- **DB2 Data**: Auto-loaded during container startup via `post-create.sh`
- **Expected Outputs**: `data/balances.csv` (for legacy programs)

### DB2 Helper Scripts
Located in `db2-helpers/` for direct database operations:
- `get-balances-cli.sh` - Query account balances
- `insert-transaction-cli.sh` - Add single transaction
- `load-accounts-cli.sh` - Bulk load accounts
- `load-transactions-cli.sh` - Query transaction history

## 🎯 Key Integration Points

### Modular Architecture Files
- **`src/copybooks/mb-db-if.cpy`** - Interface contract between main program and modules
- **Environment Variables**: `MINIBANK_DB_MODULE`, `MINIBANK_DATA_SOURCE` control runtime behavior
- **Module Loading**: Dynamic CALL statements enable plugin-like architecture

### Container Setup & DB2
- **`.devcontainer/init-db2-cli.sh`** - Database schema creation
- **`.devcontainer/load-sample-data-cli.sh`** - Test data population  
- **`.devcontainer/post-create.sh`** - Complete container initialization
- **`db2-helpers/`** - CLI utilities for manual DB2 operations

### Development Workflow
When adding new database operations:
1. Update `mb-db-if.cpy` with new function codes
2. Implement in both `mb-db-sql.cbl` (production) and `mb-db-cli.cbl` (testing)
3. Add corresponding logic in `mb-main.cbl`
4. Test both modes using environment variable switches

## 📝 COBOL Coding Standards

### Language Standards
- Use COBOL '85 syntax in preference to COBOL '74
- Avoid COBOL language features identified as obsolete in ANSI standard
- Declare numeric variables as signed, odd-length, COMPUTATIONAL (packed decimal) for efficiency: `PIC S9(13)V9(2) COMP-3`

### Program Layout Standards
- Use continuous lines of comments to break programs into logical sections:
  ```cobol
  * ============================================================
  * 📊 DATA DIVISION  
  * Define all variables and structures
  * ============================================================
  ```
- Separate different sections with `/` compiler directive in continuation area (column 7)
- Place each phrase of file declarations on separate lines with descriptive comments:
  ```cobol
  SELECT TX-FILE ASSIGN TO "data/transactions.csv"
      ORGANIZATION IS LINE SEQUENTIAL.
  *    📥 Transaction input file (CSV format)
  ```
- Code files in order of frequency of use (most important first)
- Indent code to indicate structure clearly

### Structured Programming Rules  
- Use structured programming operations: `IF/END-IF` rather than `GO/LABEL`
- Use explicit scope terminators (`END-IF`, `END-PERFORM`) on all multi-statement constructs
- Avoid explicit branching (`GO` statements) except for structured 'ITERATE' or 'QUIT' patterns
- Never use `GO` to branch from subroutine to mainline code
- Limit nesting to 3-4 levels maximum - use separate sections if deeper nesting needed
- Use standard READ loop patterns:
  ```cobol
  PERFORM UNTIL EOF = "Y"
     READ TX-FILE
        AT END MOVE "Y" TO EOF
        NOT AT END
           PERFORM PROCESS-RECORD
     END-READ
  END-PERFORM
  ```

### Naming Conventions
- **Field Names**: Follow format `IIMMMM` where:
  - `II` = Format identifier (e.g., `WS`, `TX`, `OUT`)  
  - `MMMM` = Meaningful mnemonic (extend beyond 4 chars for clarity)
- **Program Control Variables**: Prefix with `C-` and use hyphens: `C-CHANGE-MODE`
- **Arrays**: Suffix with `-A`: `ACCOUNTS-A`
- **Array Indices**: Related to array name without suffix: `ACCOUNTS` for `ACCOUNTS-A`
- **Subroutines**: Use hierarchical prefix + mnemonic: `AA-INIT`, `BB-PROCESS`, `ZZ-EXIT`
- **Standard Subroutines**: Begin with `Z` for system-wide routines

### File and Data Handling
- Always test divisor is not zero before division operations
- Fill overflow fields with 999s for reports/displays  
- Use half-adjust when adding fields of different precision levels
- Convert all dates to `YYMMDD` or `CYYMMDD` format before database output
- Use `FILE-STATUS` indicators with meaningful Level 88 names:
  ```cobol
  88  C-EOF               VALUE "10".
  88  C-IO-OK             VALUE "00".
  88  C-IO-ERR            VALUE "01" THRU "99".
  ```

### Documentation Requirements
- Include program header with purpose, author, date, and modification history
- Precede each subroutine with 1-2 lines describing its function
- Comment the use of non-volatile indicators with global scope
- Document standard indicator usage in device files using DDS keywords
- Never hard-code user profiles - always retrieve from data structures