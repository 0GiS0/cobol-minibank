#!/bin/bash
# =====================================================================
# load-accounts-cli.sh - Cargar cuentas desde DB2 (formato CSV)
# =====================================================================

export DB2INSTANCE=db2inst1
export HOME=/home/db2inst1
export LD_LIBRARY_PATH=/opt/ibm/db2/V12.1/lib:$LD_LIBRARY_PATH
export PATH=/opt/ibm/db2/V12.1/bin:$PATH

# Conectar a DB2, ejecutar consulta con salida en formato CSV
{
    echo "CONNECT TO MINIBANK USER db2inst1 USING password"
    echo "SELECT ACCOUNT_ID || ',' || ACCOUNT_NAME || ',' || BALANCE FROM ACCOUNTS ORDER BY ACCOUNT_ID"
    echo "TERMINATE"
} | /opt/ibm/db2/V12.1/bin/db2 2>&1 | grep -E '^[A-Z]{3,}' | head -500 > /tmp/minibank-accounts.tmp

exit 0
