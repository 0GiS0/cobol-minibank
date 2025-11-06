#!/usr/bin/env python3
"""
Obtener cuentas de DB2 y exportar a archivo
"""

import ibm_db
import sys

try:
    conn = ibm_db.connect(
        "DATABASE=minibank;HOSTNAME=db;PORT=50000;UID=db2inst1;PWD=password;",
        "", ""
    )

    stmt = ibm_db.exec_immediate(
        conn,
        "SELECT ACCOUNT_ID, ACCOUNT_NAME, BALANCE FROM ACCOUNTS ORDER BY ACCOUNT_ID"
    )

    result = ibm_db.fetch_assoc(stmt)
    with open("/tmp/minibank-accounts.tmp", "w") as f:
        while result:
            f.write(f"{result['ACCOUNT_ID']},{result['ACCOUNT_NAME']},{result['BALANCE']}\n")
            result = ibm_db.fetch_assoc(stmt)

    ibm_db.close(conn)
    sys.exit(0)

except Exception as e:
    print(f"Error: {e}", file=sys.stderr)
    sys.exit(1)
