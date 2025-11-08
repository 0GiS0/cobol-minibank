#!/bin/bash
# =====================================================================
# load-sample-data-cli.sh - Cargar datos de ejemplo en DB2
# =====================================================================

# Configurar entorno DB2
export DB2INSTANCE=db2inst1
export HOME=/home/db2inst1
export LD_LIBRARY_PATH=/opt/ibm/db2/V12.1/lib:$LD_LIBRARY_PATH
export PATH=/opt/ibm/db2/V12.1/bin:$PATH

echo "📥 Cargando datos de prueba..."

# Conectar a DB2
/opt/ibm/db2/V12.1/bin/db2 connect to MINIBANK user db2inst1 using password > /dev/null 2>&1

# Cargar cuentas desde CSV
echo "👤 Insertando cuentas..."
while IFS=',' read -r DATE ACCOUNT TYPE AMOUNT; do
    ACCT_ID=$(echo "$ACCOUNT" | tr -d ' ')
    if [[ "$ACCT_ID" != "ACCOUNT_ID" ]] && [[ ! -z "$ACCT_ID" ]]; then
        # Insertar cuenta si no existe
        /opt/ibm/db2/V12.1/bin/db2 "INSERT INTO ACCOUNTS (ACCOUNT_ID, ACCOUNT_NAME, BALANCE) SELECT '$ACCT_ID', 'Cuenta $ACCT_ID', 0 FROM SYSIBM.SYSDUMMY1 WHERE NOT EXISTS (SELECT 1 FROM ACCOUNTS WHERE ACCOUNT_ID='$ACCT_ID')" 2>/dev/null || true
    fi
done < /workspaces/cobol-minibank/data/transactions.csv

# Cargar transacciones
echo "💰 Insertando transacciones..."
while IFS=',' read -r DATE ACCOUNT TYPE AMOUNT; do
    ACCT_ID=$(echo "$ACCOUNT" | tr -d ' ')
    if [[ "$ACCT_ID" != "ACCOUNT_ID" ]] && [[ ! -z "$ACCT_ID" ]]; then
        TXN_ID="TXN-$DATE-$ACCT_ID-$TYPE"
        /opt/ibm/db2/V12.1/bin/db2 "INSERT INTO TRANSACTIONS (TRANSACTION_ID, ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT) VALUES ('$TXN_ID', '$ACCT_ID', DATE('$DATE'), '$TYPE', $AMOUNT)" 2>/dev/null || true
    fi
done < /workspaces/cobol-minibank/data/transactions.csv

# Desconectar
/opt/ibm/db2/V12.1/bin/db2 connect reset > /dev/null 2>&1

echo "✅ Datos cargados exitosamente"
