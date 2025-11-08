#!/usr/bin/env python3
"""
🔗 DB2 SQL Interface for COBOL via EXEC SQL simulation
Simulates EXEC SQL operations for DB2 by handling SQL commands
"""

import sys
import os
import ibm_db

# Database configuration
DB_CONFIG = {
    'database': 'minibank',
    'hostname': 'localhost',
    'port': 50000,
    'user': 'db2inst1',
    'password': 'password'
}

# Shared connection state
_conn = None

def connect(user, passwd, db):
    """Execute CONNECT command"""
    global _conn
    try:
        dsn = f"DATABASE={db};HOSTNAME={DB_CONFIG['hostname']};PORT={DB_CONFIG['port']};UID={user};PWD={passwd};"
        _conn = ibm_db.connect(dsn, "", "")
        return 0
    except Exception as e:
        print(f"Connection error: {e}", file=sys.stderr)
        return -402

def execute(sql):
    """Execute SQL statement"""
    global _conn
    if not _conn:
        return -402
    try:
        result = ibm_db.exec_immediate(_conn, sql)
        return 0
    except Exception as e:
        print(f"SQL error: {e}", file=sys.stderr)
        return -601

def disconnect():
    """Disconnect from DB2"""
    global _conn
    if _conn:
        ibm_db.close(_conn)
        _conn = None
    return 0

# Command dispatcher
if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(1)

    cmd = sys.argv[1]

    if cmd == "connect":
        user = sys.argv[2] if len(sys.argv) > 2 else DB_CONFIG['user']
        passwd = sys.argv[3] if len(sys.argv) > 3 else DB_CONFIG['password']
        db = sys.argv[4] if len(sys.argv) > 4 else DB_CONFIG['database']
        rc = connect(user, passwd, db)
        sys.exit(rc)

    elif cmd == "execute":
        sql = sys.argv[2] if len(sys.argv) > 2 else ""
        rc = execute(sql)
        sys.exit(rc)

    elif cmd == "disconnect":
        rc = disconnect()
        sys.exit(rc)

    else:
        sys.exit(1)
