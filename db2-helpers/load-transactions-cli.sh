#!/bin/bash
# =====================================================================
# load-transactions-cli.sh - Cargar transacciones (formato CSV)
# Parámetro: $1=ACCOUNT_ID
# =====================================================================

export DB2INSTANCE=db2inst1
export HOME=/home/db2inst1
export LD_LIBRARY_PATH=/opt/ibm/db2/V12.1/lib:$LD_LIBRARY_PATH
export PATH=/opt/ibm/db2/V12.1/bin:$PATH

ACCOUNT_ID=$1

# Conectar a DB2, ejecutar consulta, guardar en archivo (modo no-interactivo)
# Formato SQL concatenado con comillas (,) para hacer CSV
{
    echo "CONNECT TO MINIBANK USER db2inst1 USING password"
    echo "SELECT TRANSACTION_ID || ',' || TRANSACTION_DATE || ',' || TRANSACTION_TYPE || ',' || AMOUNT FROM TRANSACTIONS WHERE ACCOUNT_ID='$ACCOUNT_ID' ORDER BY TRANSACTION_DATE DESC"
    echo "TERMINATE"
} | /opt/ibm/db2/V12.1/bin/db2 2>&1 | grep -E '^[A-Z0-9]{3,}' | head -500 > /tmp/minibank-transactions.tmp

exit 0
