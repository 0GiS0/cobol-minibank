# COBOL MiniBank - AI Coding Agent Instructions

## 🏗️ Architecture Overview

This is a **progressive educational COBOL repository** with 3 programs of increasing complexity:

1. **`minibank.cob`** - Basic CSV file processing (entry level)
2. **`minibank-db2.cob`** - DB2 integration via Python wrapper (intermediate)  
3. **`minibank-menu.cob`** - Interactive menu system with DB2 (advanced)

All programs process banking transactions but use different data sources and interfaces to demonstrate COBOL evolution from batch processing to interactive systems.

## 🔧 Development Environment

- **Container-based**: Uses `.devcontainer/` with DB2 Express-C database
- **Build System**: GNU COBOL (cobc) via Makefile with specific flags: `-x -Wall -O2 -I src/copybooks`
- **Key Directories**: 
  - `src/` - COBOL source files (.cob)
  - `src/copybooks/` - Shared data structures (.cpy)
  - `data/` - CSV input/output files
  - `.devcontainer/` - DB2 setup, Python interfaces, sample data loaders

## 🚀 Essential Workflows

### Build & Run Commands
```bash
# Basic program (most common)
make run                 # Builds + executes minibank.cob

# DB2 integrated program  
make run-db2            # Initializes DB2 + builds + executes minibank-db2.cob

# Interactive menu program
make run-menu           # Loads DB2 data + builds + executes minibank-menu.cob

# Individual builds
make build              # Compiles minibank.cob only
make build-db2          # Compiles minibank-db2.cob only  
make build-menu         # Compiles minibank-menu.cob only
```

### DB2 Integration Pattern
COBOL programs don't use embedded SQL (`EXEC SQL`) - instead they use **Python wrapper pattern**:
```
COBOL → CALL "SYSTEM" → Python script → ibm_db → DB2
```
- Python scripts in `.devcontainer/` handle DB2 operations
- COBOL passes parameters via command line and temp files
- Pattern avoids DB2 precompiler complexity while maintaining educational focus

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

### CSV Processing (minibank.cob)
1. Read `data/transactions.csv` line by line
2. Parse with `UNSTRING` using "," delimiter
3. Accumulate balances in memory arrays
4. Write formatted output to `data/balances.csv`

### DB2 Integration (minibank-db2.cob) 
1. Process CSV transactions → Python → Insert to DB2
2. Query balances from DB2 → Python → Temp file
3. COBOL reads temp file and formats final output

### Interactive Menu (minibank-menu.cob)
1. Load account data from DB2 into COBOL arrays (one-time)
2. Present menu loop with `PERFORM UNTIL`
3. Process user input with `ACCEPT` and validation
4. Query transaction details on-demand via Python interface

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

- **Sample Data**: Located in `data/transactions.csv` (5 sample transactions)
- **Expected Outputs**: `data/balances.csv` should show account totals
- **DB2 Verification**: Use `.devcontainer/connect-db2.py` to query directly
- **Interactive Testing**: Menu program accepts options 1, 2, 3 with validation

## 🎯 Key Integration Points  

- **Python Interface**: `.devcontainer/db2-interface.py` - handles all DB2 operations
- **Data Loading**: `.devcontainer/load-sample-data.py` - populates DB2 with test data
- **Environment Setup**: `.devcontainer/post-create.sh` - one-time container initialization
- **Copybooks**: `src/copybooks/record-layout.cpy` - shared data structures (reference only)

When modifying COBOL programs, maintain the educational progression: basic → DB2 integration → interactive menus.

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