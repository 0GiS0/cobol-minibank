#!/usr/bin/env python3
"""
Obtener transacciones de DB2 y exportar a archivo
"""

import ibm_db
import sys

account_id = sys.argv[1] if len(sys.argv) > 1 else "1"

try:
    conn = ibm_db.connect(
        "DATABASE=minibank;HOSTNAME=localhost;PORT=50000;UID=db2inst1;PWD=password;",
        "", ""
    )

    stmt = ibm_db.exec_immediate(
        conn,
        f"""SELECT TRANSACTION_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT
           FROM TRANSACTIONS
           WHERE ACCOUNT_ID = {account_id}
           ORDER BY TRANSACTION_DATE DESC
           FETCH FIRST 20 ROWS ONLY"""
    )

    result = ibm_db.fetch_assoc(stmt)
    with open("/tmp/minibank-transactions.tmp", "w") as f:
        while result:
            f.write(f"{result['TRANSACTION_ID']},{result['TRANSACTION_DATE']},{result['TRANSACTION_TYPE']},{result['AMOUNT']}\n")
            result = ibm_db.fetch_assoc(stmt)

    ibm_db.close(conn)
    sys.exit(0)

except Exception as e:
    print(f"Error: {e}", file=sys.stderr)
    sys.exit(1)
