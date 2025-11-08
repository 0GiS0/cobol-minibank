#!/bin/bash
# =====================================================================
# get-accounts-cli.sh - Obtener cuentas de DB2 usando CLI
# =====================================================================

# Configurar entorno DB2
export DB2INSTANCE=db2inst1
export HOME=/home/db2inst1
export LD_LIBRARY_PATH=/opt/ibm/db2/V12.1/lib:$LD_LIBRARY_PATH
export PATH=/opt/ibm/db2/V12.1/bin:$PATH

# Conectar y ejecutar query con db2
/opt/ibm/db2/V12.1/bin/db2 connect to MINIBANK user db2inst1 using password > /dev/null 2>&1

# Ejecutar SELECT con CONCAT para formato CSV
{
    echo "SELECT RTRIM(ACCOUNT_ID) || ',' || RTRIM(ACCOUNT_NAME) || ',' || CAST(BALANCE AS VARCHAR(20)) FROM ACCOUNTS ORDER BY ACCOUNT_ID WITH UR;"
    echo "QUIT;"
} | /opt/ibm/db2/V12.1/bin/db2 2>/dev/null | grep -E "^ACC-" | sed 's/[[:space:]]*$//' > /tmp/minibank-accounts.tmp

# Desconectar
/opt/ibm/db2/V12.1/bin/db2 connect reset > /dev/null 2>&1

# Verificar si se obtuvo algún dato
if [ -s /tmp/minibank-accounts.tmp ]; then
    exit 0
else
    exit 1
fi
