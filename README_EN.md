# 🏦 COBOL MiniBank - Simplified Banking System

**[Español](README.md)** | **English**

## 🤔 What is this project?

This is a **progressive educational repository** for learning **COBOL** (Common Business-Oriented Language) from scratch to integration with enterprise databases.

It includes **3 COBOL programs** with increasing complexity:
1. 📄 **Basic MiniBank** - CSV file processing
2. 🗄️ **MiniBank DB2** - DB2 database integration
3. 🎮 **MiniBank Menu** - Interactive system with menus and queries

Perfect for **beginners who have never seen COBOL** and want to understand how it works in real environments.

---

## 🎯 The 3 Programs in the Repository

### 1️⃣ Basic MiniBank (`minibank.cob`)

**Level:** Beginner  
**Purpose:** Learn COBOL fundamentals with files

**What does it do?**
1. 📥 Reads transactions from `data/transactions.csv`
2. 🔄 Processes deposits (CREDIT) and withdrawals (DEBIT)
3. 🧮 Calculates account balances in memory
4. 📊 Generates report in `data/balances.csv`

**Execute:**
```bash
make run
```

**Input example** (`transactions.csv`):
```csv
2025-01-10,ACC-001,CREDIT,1000.00
2025-01-12,ACC-001,DEBIT,150.25
2025-01-15,ACC-002,CREDIT,500.00
```

**Output** (`balances.csv`):
```csv
account,balance
ACC-001,849.75
ACC-002,500.00
```

**COBOL concepts you'll learn:**
- ✅ 4-division structure (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- ✅ Sequential file reading with `READ`
- ✅ Arrays in COBOL with `OCCURS`
- ✅ CSV parsing with `UNSTRING`
- ✅ Precise decimal arithmetic (`PIC S9(13)V9(2)`)
- ✅ Output formatting with `STRING`

---

### 2️⃣ MiniBank DB2 (`minibank-db2.cob`)

**Level:** Intermediate  
**Purpose:** COBOL + DB2 Database integration

**What does it do?**
1. 📥 Reads transactions from CSV
2. 🗄️ **Inserts each transaction into DB2** (via Python wrapper)
3. 📊 **Queries balances from DB2** instead of calculating in memory
4. 📤 Generates report with database data

**Execute:**
```bash
make run-db2
```

**Architecture:**
```
COBOL Program ──> Python Script ──> DB2 Database
   (minibank-db2.cob)  (db2-interface.py)  (Tables: ACCOUNTS, TRANSACTIONS)
```

**Why use Python as a wrapper?**
- COBOL can call DB2 directly with `EXEC SQL`, BUT it requires DB2 precompiler
- This repository uses **Python + ibm_db** as a bridge to avoid setup complexity
- In real production environments `EXEC SQL` embedded is used (see opinion section below)

**New concepts:**
- ✅ External command calls with `CALL "SYSTEM"`
- ✅ Parameter passing via command line
- ✅ COBOL integration with other languages
- ✅ Temporary file handling (`/tmp`)

---

### 3️⃣ MiniBank Menu (`minibank-menu.cob`)

**Level:** Advanced  
**Purpose:** Complete interactive system with menu

**What does it do?**
1. 🎮 Presents interactive menu to the user
2. 📋 Option 1: View all accounts with their balances
3. 🔍 Option 2: View details of a specific account + transactions
4. 🔄 Keeps the program running until the user chooses to exit

**Execute:**
```bash
make run-menu
```

**Interaction example:**
```
==================================================
    💰 WELCOME TO MINIBANK 💰
==================================================

⏳ Loading accounts from DB2...
OK: 5 accounts loaded.

==================================================
              📋 MINIBANK - MENU 📋
==================================================

  1️⃣  View all accounts
  2️⃣  View account details
  3️⃣  Exit program

Select an option: _
```

**New concepts:**
- ✅ Interactive user input with `ACCEPT`
- ✅ Loops with `PERFORM UNTIL`
- ✅ Input validation with `TEST-NUMVAL`
- ✅ Menu structures and navigation
- ✅ Dynamic data loading from DB2

---

## 🚀 Why COBOL?

COBOL remains the predominant language in:
- 🏦 **Banking systems** (processes 95% of ATM transactions)
- 💳 **Credit card processing**
- 🏢 **Government and insurance systems**
- 📊 **Payroll and accounting applications**

Its main features:
- 📖 **Readability**: English-like syntax
- 🎯 **Decimal precision**: Ideal for financial calculations
- 🏗️ **Stability**: Programs that work for decades without modification
- ⚡ **Massive processing**: Efficiently handles millions of records

---

## 🛠️ Setup and Execution

### 📋 Requirements
- VS Code with **Dev Containers** extension
- Docker Desktop active
- 4GB RAM minimum (for DB2 container)

### 🚀 Quick start (3 steps):
1. 📂 Open the folder in VS Code
2. 🔄 When the popup appears, select **"Reopen in Container"**
3. ⏳ Wait for `postCreate` to finish (installs dependencies and loads data into DB2)

### 🔨 Compile and run the programs:

#### Option 1: Use VS Code Tasks
- **🏗️ Compile**: `Terminal > Run Task > COBOL: build`
- **▶️ Execute**: `Terminal > Run Task > COBOL: run`

#### Option 2: Use Makefile directly
```bash
# Basic program (CSV)
make run

# Program with DB2
make run-db2

# Interactive program with menu
make run-menu

# Clean compiled binaries
make clean
```

**Result:** Compiled files are generated in the project root (`minibank`, `minibank-db2`, `minibank-menu`).

---

## 📂 Project Structure

```
cobol-minibank/
├── src/
│   ├── minibank.cob          # 📝 Program 1: Basic with CSV
│   ├── minibank-db2.cob      # 🗄️ Program 2: DB2 Integration
│   ├── minibank-menu.cob     # 🎮 Program 3: Interactive Menu
│   └── copybooks/            # 📚 Reusable definitions
│       └── record-layout.cpy # CSV record layout
├── data/
│   ├── transactions.csv      # 📥 Input transactions
│   └── balances.csv          # 📤 Output balances (generated)
├── .devcontainer/
│   ├── devcontainer.json     # ⚙️ Dev Container configuration
│   ├── Dockerfile            # 🐳 Image with GnuCOBOL + Python
│   ├── compose.yml           # 🐳 Docker Compose (app + db2)
│   ├── init-db2.sql          # 📊 Database schema
│   ├── db2-interface.py      # 🔗 Python wrapper for DB2
│   ├── get-accounts.py       # 📋 Query accounts from COBOL
│   ├── get-transactions.py   # 📋 Query transactions from COBOL
│   ├── load-sample-data.py   # 🎲 Load sample data
│   └── verify-db2.sh         # ✅ DB2 verification script
├── Makefile                  # 🔧 Build automation
├── .vscode/
│   └── tasks.json            # ⚡ VS Code tasks
└── README.md                 # 📖 This documentation
```

---

## 🎓 Beginner's Learning Guide

### 🔤 Difference between `.cob` and `.cbl` files

**Short answer:** They're the same, just different extensions.

**Common COBOL extensions:**
- `.cob` / `.COB` - More used in Linux/GnuCOBOL environments
- `.cbl` / `.CBL` - More used in IBM mainframes
- `.cpy` / `.CPY` - For copybooks (reusable code)

This repository uses `.cob` because we use **GnuCOBOL** (open source compiler).

---

### 📖 COBOL concepts you'll learn (ordered by program)

#### In `minibank.cob` (Basic):

**1. The 4 COBOL Divisions**
```cobol
IDENTIFICATION DIVISION.    ← Identifies the program
PROGRAM-ID. MINIBANK.

ENVIRONMENT DIVISION.        ← Defines external files
FILE-CONTROL.
    SELECT TX-FILE ASSIGN TO "data/transactions.csv".

DATA DIVISION.               ← Declares variables
WORKING-STORAGE SECTION.
77  WS-AMOUNT  PIC S9(13)V9(2).

PROCEDURE DIVISION.          ← Program logic
MAIN.
    DISPLAY "Hello COBOL".
    GOBACK.
```

**2. Data types with `PICTURE` (PIC)**
```cobol
77  WS-ACCOUNT     PIC X(30).        ← 30-character text
77  WS-AMOUNT      PIC 9(10)V9(2).   ← Number: 10 integers, 2 decimals
77  WS-COUNTER     PIC 9(4) COMP.    ← Binary integer (efficient)
77  WS-SIGNED-AMT  PIC S9(13)V9(2).  ← With sign (+/-)
```

**3. Arrays (Tables) with `OCCURS`**
```cobol
01  ACCOUNTS.
    05 ACCT-ENTRY OCCURS 100 TIMES.
       10 ACCT-NAME    PIC X(30).
       10 ACCT-BAL     PIC S9(13)V9(2).

* Access element 5:
MOVE "John" TO ACCT-NAME(5).
ADD 100 TO ACCT-BAL(5).
```

**4. File reading**
```cobol
OPEN INPUT TX-FILE.
PERFORM UNTIL EOF = "Y"
    READ TX-FILE
        AT END MOVE "Y" TO EOF
        NOT AT END PERFORM PROCESS-LINE
    END-READ
END-PERFORM.
CLOSE TX-FILE.
```

**5. CSV parsing with `UNSTRING`**
```cobol
* Input: "2025-01-10,ACC-001,CREDIT,1000"
UNSTRING WS-LINE DELIMITED BY ","
    INTO WS-DATE
         WS-ACCOUNT
         WS-TYPE
         WS-AMOUNT-STR
END-UNSTRING.
```

**6. Precise decimal arithmetic**
```cobol
* COBOL is perfect for money (doesn't use imprecise float)
ADD WS-AMOUNT TO ACCT-BAL(I).
SUBTRACT 100 FROM WS-BALANCE.
MULTIPLY 1.05 BY WS-AMOUNT.    ← Apply 5% interest
DIVIDE WS-TOTAL BY 12 GIVING WS-MONTHLY.
```

**7. Output formatting with `STRING`**
```cobol
STRING
    ACCT-NAME(I) DELIMITED BY SPACES
    "," DELIMITED BY SIZE
    FORMATTED-BAL DELIMITED BY SIZE
    INTO OUT-LINE
END-STRING.
WRITE OUT-LINE.
```

---

#### In `minibank-db2.cob` (Intermediate):

**8. External command calls**
```cobol
77  CMD-LINE  PIC X(512).
77  RC        PIC S9(9) COMP.

MOVE "python3 db2-interface.py connect" TO CMD-LINE.
CALL "SYSTEM" USING CMD-LINE RETURNING RC.

IF RC = 0
    DISPLAY "✅ Connection successful"
ELSE
    DISPLAY "❌ Connection error"
END-IF.
```

**9. Dynamic command construction**
```cobol
MOVE FUNCTION CONCATENATE(
    "python3 db2-interface.py insert ",
    WS-ACCOUNT, " ",
    WS-DATE, " ",
    WS-TYPE, " ",
    WS-AMOUNT-STR
) TO CMD-LINE.

CALL "SYSTEM" USING CMD-LINE.
```

---

#### In `minibank-menu.cob` (Advanced):

**10. Interactive user input**
```cobol
DISPLAY "Select an option: " WITH NO ADVANCING.
ACCEPT WS-INPUT.

* Validate if it's a number
IF FUNCTION TEST-NUMVAL(WS-INPUT) = 0
    MOVE FUNCTION NUMVAL(WS-INPUT) TO WS-OPTION
ELSE
    DISPLAY "❌ You must enter a number"
END-IF.
```

**11. Loops with menu**
```cobol
PERFORM UNTIL WS-CONTINUE = "N"
    PERFORM SHOW-MENU
    PERFORM GET-USER-CHOICE
    
    EVALUATE WS-CHOICE
        WHEN 1 PERFORM OPTION-1
        WHEN 2 PERFORM OPTION-2
        WHEN 3 MOVE "N" TO WS-CONTINUE
    END-EVALUATE
END-PERFORM.
```

**12. Control structures with `EVALUATE`**
```cobol
EVALUATE WS-TRANSACTION-TYPE
    WHEN "CREDIT"
        ADD WS-AMOUNT TO WS-BALANCE
        DISPLAY "✅ Deposit processed"
    WHEN "DEBIT"
        SUBTRACT WS-AMOUNT FROM WS-BALANCE
        DISPLAY "✅ Withdrawal processed"
    WHEN OTHER
        DISPLAY "❌ Invalid transaction type"
END-EVALUATE.
```

---

## 🔄 Data Flow in Each Program

### Program 1: `minibank.cob`
```
📄 transactions.csv
    ↓ (READ)
🧮 COBOL processes in memory
    ↓ (Calculate balances)
📊 balances.csv
    ↓ (WRITE)
✅ File generated
```

### Program 2: `minibank-db2.cob`
```
📄 transactions.csv
    ↓ (READ)
🧮 COBOL parses each line
    ↓ (CALL SYSTEM)
🐍 Python (db2-interface.py)
    ↓ (INSERT)
🗄️ DB2 Database
    ↓ (SELECT balances)
🐍 Python generates temporary CSV
    ↓ (READ)
🧮 COBOL reads and formats
    ↓ (WRITE)
📊 balances.csv
```

### Program 3: `minibank-menu.cob`
```
🎮 User interacts with menu
    ↓ (ACCEPT)
🧮 COBOL processes option
    ↓ (CALL SYSTEM)
🐍 get-accounts.py or get-transactions.py
    ↓ (SELECT)
🗄️ DB2 Database
    ↓ (Generate /tmp/*.tmp)
🧮 COBOL reads and displays
    ↓ (DISPLAY)
👤 User sees results
    ↓ (Loop)
🔄 Return to menu
```

---

## 🗄️ DB2 Integration

### 🏗️ Database Schema

The repository includes a **DB2 Community Edition** container with the following tables:

```sql
-- Accounts table
CREATE TABLE ACCOUNTS (
    ACCOUNT_ID      INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ACCOUNT_NAME    VARCHAR(100) NOT NULL,
    BALANCE         DECIMAL(15,2) DEFAULT 0,
    CREATED_AT      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UPDATED_AT      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Transactions table
CREATE TABLE TRANSACTIONS (
    TRANSACTION_ID   INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ACCOUNT_ID       INTEGER NOT NULL,
    TRANSACTION_DATE DATE NOT NULL,
    TRANSACTION_TYPE VARCHAR(10) NOT NULL,  -- 'CREDIT' or 'DEBIT'
    AMOUNT           DECIMAL(15,2) NOT NULL,
    CREATED_AT       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS(ACCOUNT_ID)
);
```

### ✅ DB2 Verification

**Automatic during `postCreate`:**
- Creates the `minibank` database
- Creates `ACCOUNTS` and `TRANSACTIONS` tables
- Loads 10 sample transactions

**Manual at any time:**
```bash
.devcontainer/verify-db2.sh
```

This script shows:
- ✅ DB2 connection status
- 📊 Number of accounts and transactions
- 💰 Balance details per account
- 📋 Latest transactions

### 🔌 Connect directly to DB2 (optional)

```bash
# Option 1: Automatic script
.devcontainer/connect-db2.sh

# Option 2: Manual DB2 client
db2 CONNECT TO minibank USER db2inst1 USING password
db2 "SELECT * FROM ACCOUNTS"
db2 "SELECT * FROM TRANSACTIONS ORDER BY TRANSACTION_DATE DESC"
db2 QUIT
```

---

## 💭 Opinion: Is this repository useful and realistic?

### ✅ **Useful and Educational Aspects**

**1. Excellent pedagogical progression**
- ✅ Starts simple (CSV) and adds complexity gradually
- ✅ Each program introduces new concepts without overwhelming
- ✅ Well-commented code (note: code comments are in Spanish, but this documentation makes the concepts accessible to English speakers)

**2. Fundamental COBOL concepts well covered**
- ✅ 4-division structure
- ✅ Sequential file handling (common in COBOL)
- ✅ Decimal arithmetic (critical in finance)
- ✅ Arrays and data structures
- ✅ Interactive input/output

**3. Modern and accessible setup**
- ✅ Dev Containers = zero installation friction
- ✅ GnuCOBOL = free and open source (vs. expensive mainframe)
- ✅ Docker Compose = easy to share

### ⚠️ **Limitations vs. Real Environments**

**1. Python as wrapper is NOT common in production**

**In this repository:**
```cobol
CALL "SYSTEM" USING "python3 db2-interface.py insert ...".
```

**In the real world (mainframe):**
```cobol
EXEC SQL
    INSERT INTO ACCOUNTS (ACCOUNT_NAME, BALANCE)
    VALUES (:WS-ACCOUNT-NAME, :WS-BALANCE)
END-EXEC.
```

**Why does this repo use Python?**
- **Practical reason:** Avoid DB2 precompiler complexity (requires extra configuration)
- **Educational reason:** Show that COBOL can integrate with other languages
- **Limitation:** In real companies, 99% use embedded `EXEC SQL` directly

**Recommendation:** If this repo evolves, adding a 4th example with native `EXEC SQL` would be ideal.

---

**2. DB2 in Docker vs. DB2 on Mainframe**

| Aspect | This Repository | Real Production |
|---------|------------------|-----------------|
| **Database** | DB2 Community (Linux) | DB2 z/OS (Mainframe) or DB2 LUW |
| **Compiler** | GnuCOBOL | IBM Enterprise COBOL |
| **Environment** | Local Dev Container | Mainframe with JCL |
| **Transactions** | Batch (files) | CICS/IMS (online) + Batch |
| **DB Access** | Python wrapper | Embedded EXEC SQL |

**Do people use DB2 in Docker?**
- **In development:** Yes, increasingly (modern CI/CD environments)
- **In production:** No, most use DB2 z/OS on mainframes or DB2 LUW on servers

---

**3. Lack of JCL (Job Control Language)**

In mainframe environments, COBOL runs via **JCL**, not with `make run`:

**Real JCL example:**
```jcl
//MINIBK01 JOB (ACCT),'COBOL MINIBANK',CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=MINIBANK
//STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
//TXFILE   DD DSN=PROD.TRANSACTIONS.CSV,DISP=SHR
//BALFILE  DD DSN=PROD.BALANCES.OUT,DISP=(NEW,CATLG,DELETE)
//SYSOUT   DD SYSOUT=*
```

This repo omits it because JCL is mainframe-specific (doesn't run on Linux).

---

### 🎯 **Recommendations to Make It More Realistic**

#### Improvements that would bring this repo closer to the real world:

**1. Add example with native EXEC SQL**
```cobol
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       EXEC SQL
           CONNECT TO minibank USER db2inst1 USING password
       END-EXEC.
       
       EXEC SQL
           INSERT INTO TRANSACTIONS (ACCOUNT_ID, TX_DATE, TX_TYPE, AMOUNT)
           VALUES (:WS-ACCOUNT-ID, :WS-DATE, :WS-TYPE, :WS-AMOUNT)
       END-EXEC.
       
       IF SQLCODE NOT = 0
           DISPLAY "SQL Error: " SQLCODE
       END-IF.
```

This would require:
- Installing DB2 precompiler (`db2 PREP PROGRAM(minibank-sql.cob)`)
- Setting up DB2 environment variables
- But would show the REAL integration used in production

---

**2. Simulate batch processing with multiple steps**

Create a script that simulates a "job" with several programs:
```bash
#!/bin/bash
# simulate-batch-job.sh

echo "Step 1: Validate transaction file"
./validate-tx

echo "Step 2: Process transactions"
./minibank-db2

echo "Step 3: Generate reports"
./generate-reports

echo "Step 4: Send notifications"
./send-notifications
```

This reflects how real COBOL batch systems work (program chains).

---

**3. Add CICS or similar for online transactions**

Currently everything is batch (files). In reality, banks use:
- **CICS** (Customer Information Control System) for real-time transactions
- **IMS** (Information Management System) for hierarchical databases

You could simulate CICS with a simple HTTP server that calls COBOL:
```python
# cics-simulator.py
from flask import Flask, request
import subprocess

@app.route('/transfer', methods=['POST'])
def transfer():
    # Call COBOL program
    result = subprocess.run(['./minibank-transfer', 
                            request.json['from_account'],
                            request.json['to_account'],
                            request.json['amount']])
    return {'status': 'OK' if result.returncode == 0 else 'ERROR'}
```

---

**4. Use DB2 LUW instead of Python wrapper**

Configure DB2 precompiler to use `EXEC SQL`:
```dockerfile
# In Dockerfile
RUN apt-get install -y ibm-db2-client
ENV DB2_HOME=/opt/ibm/db2
```

And compile with:
```bash
db2 PREP minibank-sql.cob BINDFILE
db2 BIND minibank-sql.bnd
cobc -x -I $DB2_HOME/include minibank-sql.cob -L $DB2_HOME/lib -ldb2
```

---

**5. Add JCL documentation (even if it doesn't work on Linux)**

Include commented examples of how it would run on mainframe:
```
📁 examples/
   └── jcl-samples/
       ├── minibank.jcl          # Job to run minibank
       ├── monthly-report.jcl    # Monthly job
       └── README.md             # JCL explanation
```

This would help students understand the real process.

---

### 📊 **Final Conclusion**

| Criteria | Rating | Comment |
|----------|--------------|------------|
| **Educational value** | ⭐⭐⭐⭐⭐ (5/5) | Excellent for learning COBOL from scratch |
| **Technical realism** | ⭐⭐⭐☆☆ (3/5) | Good but Python wrapper is not common |
| **Modern setup** | ⭐⭐⭐⭐⭐ (5/5) | Dev Containers + Docker is perfect |
| **Pedagogical progression** | ⭐⭐⭐⭐⭐ (5/5) | 3 programs with increasing complexity |
| **Mainframe similarity** | ⭐⭐☆☆☆ (2/5) | Missing JCL, EXEC SQL, CICS |

**Verdict:**
- ✅ **Perfect for learning COBOL** without mainframe access
- ⚠️ **Doesn't replace real experience** in mainframe/CICS environments
- 🎯 **With suggested improvements** could be 90% realistic

---

## 🔧 Modifications and Experiments

Ideas to practice and extend the project:

### Beginner Level:
- 📝 Add minimum balance validation ($0)
- 🔤 Support different date formats
- 📊 Generate report with totals by transaction type

### Intermediate Level:
- 🏦 Implement transfers between accounts
- 📅 Filter transactions by date range
- 💰 Calculate monthly interest

### Advanced Level:
- 🔐 Add user authentication
- 🗄️ Migrate from Python wrapper to native EXEC SQL
- 📈 Implement REST API that calls COBOL (CICS-like)
- 🎭 Create reconciliation program (record matching)

---

## 🧪 Testing and Debugging

### Run tests (if they existed):
```bash
# TODO: Add COBOL testing framework
# Options: COBOL Check, Unit Test Framework
```

### Interactive debugging:
```bash
# GDB works with COBOL binaries
gdb ./minibank
(gdb) break MAIN
(gdb) run
(gdb) print WS-ACCOUNT
```

### View intermediate files:
```bash
# Review processed data
cat data/balances.csv

# View DB2 logs
docker logs cobol-minibank-db-1

# Menu temporary files
cat /tmp/minibank-accounts.tmp
cat /tmp/minibank-transactions.tmp
```

---

## 📚 Additional Resources

### COBOL Documentation:
- 📖 [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/doc/gnucobol.html)
- 📘 [COBOL Programming Course (OpenMainframe)](https://www.openmainframeproject.org/projects/cobol-programming-course)
- 🎓 [IBM Enterprise COBOL for z/OS](https://www.ibm.com/docs/en/cobol-zos)

### DB2 and SQL:
- 🗄️ [IBM DB2 Documentation](https://www.ibm.com/docs/en/db2)
- 🔗 [Python ibm_db Library](https://github.com/ibmdb/python-ibmdb)

### Mainframe and legacy environments:
- 🖥️ [IBM z/OS Basics](https://www.ibm.com/docs/en/zos-basic-skills)
- 📋 [JCL Tutorial](https://www.tutorialspoint.com/jcl/index.htm)
- 🔄 [CICS Transaction Server](https://www.ibm.com/docs/en/cics-ts)

---

## 🐛 Troubleshooting

### "Command not found: cobc"
**Solution:** Make sure you're inside the Dev Container (Reopen in Container).

### "DB2 connection refused"
**Diagnosis:**
```bash
# Verify DB2 is running
docker ps | grep db2

# View DB2 logs
docker logs cobol-minibank-db-1

# Restart container if necessary
docker restart cobol-minibank-db-1
```

### "No such file or directory: data/transactions.csv"
**Solution:**
```bash
# Create directory and sample file
mkdir -p data
cat > data/transactions.csv << 'EOF'
2025-01-10,ACC-001,CREDIT,1000.00
2025-01-12,ACC-001,DEBIT,150.25
EOF
```

### Compiled program won't execute
**Diagnosis:**
```bash
# Check permissions
ls -la minibank*

# Give execution permissions
chmod +x minibank minibank-db2 minibank-menu

# Verify it compiled correctly
file minibank
# Should show: ELF 64-bit LSB executable
```

---

## 🤝 Contributions

This is an educational project. Ideas to contribute:

1. 📝 Add more COBOL program examples
2. 🧪 Implement unit tests with COBOL Check
3. 📖 Translate documentation to other languages
4. 🔧 Add example with native EXEC SQL
5. 🎮 Create web interface that calls COBOL (REST API)
6. 📊 Add reports in PDF/HTML

---

## 📄 License

This project is open source and available under a permissive license for educational purposes.

---

## 👤 Author

Created with 💙 for the developer community who wants to learn modern COBOL.

**Questions or suggestions?** Open an issue in the repository.

---

## 🤖 Specialized COBOL Agents

This repository includes **6 specialized agents** for GitHub Copilot that help you with specific COBOL development tasks:

### 📊 **Impact Analyzer**
Analyzes the impact of changes in COBOL code, dependencies and system architecture.
- ✅ Evaluates which components will be affected by modifications
- ✅ Generates risk reports with time estimates
- ✅ Maps dependencies between COBOL modules

### 🔧 **COBOL Module Builder** 
Implements and develops complete COBOL modules following enterprise standards.
- ✅ Generates production-ready COBOL code
- ✅ Creates copybooks, DB2 interfaces and main programs
- ✅ Applies automatic optimization and best practices

### ⚙️ **JCL Generator**
Generates optimized Job Control Language for mainframe.
- ✅ JCL for compilation, link-editing and execution
- ✅ Specialized jobs for DB2, testing and batch processing
- ✅ Mainframe deployment automation

### 📚 **COBOL Documenter**
Generates complete technical and user documentation.
- ✅ Technical specifications with interface definitions
- ✅ User manuals and operation guides
- ✅ API documentation in Markdown format

### 🎨 **Mermaid Diagram Creator**
Creates Mermaid diagrams optimized for COBOL architectures.
- ✅ Program dependency diagrams
- ✅ Sequence diagrams for business flows
- ✅ ER diagrams for database schemas
- ✅ Mainframe architecture diagrams

### 📋 **COBOL Project Planner**
Plans COBOL enterprise projects with modular architecture.
- ✅ Detailed timelines and resource allocation
- ✅ Risk analysis and mitigation strategies
- ✅ Complete deliverables by project phase

**💡 How to use the agents:**
In GitHub Copilot Chat, simply mention the agent you need:
- `@impact-analyzer "Analyze the impact of modifying mb-db-if.cpy"`
- `@cobol-module-builder "Implement an account validation module"`
- `@mermaid-diagram-creator "Create a diagram of the dual-mode architecture"`

---

## 🎯 Suggested Next Steps

If you're learning COBOL with this repository:

1. ✅ **Complete the basic program** (`minibank.cob`)
   - Understand each division
   - Modify the input CSV
   - Add a simple validation

2. ✅ **Explore DB2 integration** (`minibank-db2.cob`)
   - Execute SQL queries manually
   - Observe how COBOL calls Python
   - Add a new table

3. ✅ **Use the interactive program** (`minibank-menu.cob`)
   - Navigate through the menus
   - Study input handling
   - Add a new menu option

4. ✅ **Create your own COBOL program**
   - Implement a simple calculator
   - Process a products/inventory file
   - Integrate with an external API (via Python wrapper)

5. 🚀 **Next level: Learn about mainframes**
   - Get familiar with JCL
   - Study CICS/IMS
   - Look for IBM z/OS courses

---

**Welcome to the world of COBOL! 🏦💻**
