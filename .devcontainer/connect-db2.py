#!/usr/bin/env python3
"""
🗄️ DB2 Connection Helper for MiniBank
Connect to DB2 and verify/create tables
"""

import sys
import time

try:
    import ibm_db
except ImportError:
    print("❌ ibm-db not installed")
    print("   Install with: pip install ibm-db")
    sys.exit(1)

# Connection parameters
DB_HOST = "db"
DB_PORT = "50000"
DB_NAME = "minibank"
DB_USER = "db2inst1"
DB_PASS = "password"

print("🗄️ Connecting to DB2...")
print(f"   Host: {DB_HOST}:{DB_PORT}")
print(f"   Database: {DB_NAME}")
print(f"   User: {DB_USER}")
print()

# Connection string
dsn = f"DRIVER={{IBM DB2 ODBC DRIVER}};DATABASE={DB_NAME};HOSTNAME={DB_HOST};PORT={DB_PORT};PROTOCOL=TCPIP;UID={DB_USER};PWD={DB_PASS};"

# Try alternative connection string
dsn = f"DATABASE={DB_NAME};HOSTNAME={DB_HOST};PORT={DB_PORT};PROTOCOL=TCPIP;UID={DB_USER};PWD={DB_PASS};"

try:
    print("⏳ Attempting connection...")
    conn = ibm_db.connect(dsn, "", "")
    print("✅ Connected successfully!")

    # Get list of tables
    print()
    print("📊 Available tables:")
    stmt = ibm_db.exec_immediate(conn, "SELECT TABNAME FROM SYSCAT.TABLES WHERE OWNER = ?", (DB_USER,))

    row = ibm_db.fetch_assoc(stmt)
    if row:
        while row:
            print(f"   • {row['TABNAME']}")
            row = ibm_db.fetch_assoc(stmt)
    else:
        print("   (No tables found - run: db2 -tf .devcontainer/init-db2.sql)")

    ibm_db.close(conn)

except Exception as e:
    print(f"❌ Connection failed: {e}")
    print()
    print("💡 Troubleshooting:")
    print("   1. Wait 2-3 minutes for DB2 container to start")
    print("   2. Check status: docker ps | grep db2")
    print("   3. View logs: docker logs cobol-minibank_devcontainer-db-1")
    sys.exit(1)

print()
print("💡 Useful commands:")
print("   Initialize tables: db2 -tf .devcontainer/init-db2.sql")
print("   Query with Python: python3 .devcontainer/query-db2.py")
