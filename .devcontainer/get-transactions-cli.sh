#!/bin/bash
# =====================================================================
# get-transactions-cli.sh - Obtener transacciones de DB2 usando CLI
# =====================================================================

ACCOUNT_ID=${1:-"ACC001"}

# Configurar entorno DB2
export DB2INSTANCE=db2inst1
export HOME=/home/db2inst1
export LD_LIBRARY_PATH=/opt/ibm/db2/V12.1/lib:$LD_LIBRARY_PATH
export PATH=/opt/ibm/db2/V12.1/bin:$PATH

# Conectar a DB2 y ejecutar query
/opt/ibm/db2/V12.1/bin/db2 connect to MINIBANK user db2inst1 using password > /dev/null 2>&1

# Ejecutar SELECT y guardar en archivo temporal
/opt/ibm/db2/V12.1/bin/db2 <<EOF 2>/dev/null | grep -v "^$" | tail -n +3 > /tmp/minibank-transactions.tmp
SELECT TRANSACTION_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT
FROM TRANSACTIONS
WHERE ACCOUNT_ID = '$ACCOUNT_ID'
ORDER BY TRANSACTION_DATE DESC
FETCH FIRST 20 ROWS ONLY
WITH UR;
EOF

# Desconectar
/opt/ibm/db2/V12.1/bin/db2 connect reset > /dev/null 2>&1

# Verificar si se obtuvo algún dato
if [ -s /tmp/minibank-transactions.tmp ]; then
    exit 0
else
    exit 1
fi
