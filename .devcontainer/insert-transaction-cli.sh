#!/bin/bash
# =====================================================================
# insert-transaction-cli.sh - Insertar transacción en DB2
# Parámetros: $1=DATE $2=ACCOUNT_ID $3=TYPE $4=AMOUNT
# =====================================================================

export DB2INSTANCE=db2inst1
export HOME=/home/db2inst1
export LD_LIBRARY_PATH=/opt/ibm/db2/V12.1/lib:$LD_LIBRARY_PATH
export PATH=/opt/ibm/db2/V12.1/bin:$PATH

DATE=$1
ACCOUNT_ID=$2
TX_TYPE=$3
AMOUNT=$4

# Generar ID de transacción único
TXN_ID="TXN-$DATE-$ACCOUNT_ID-$TX_TYPE"

# Conectar a DB2, insertar transacción, y desconectar (modo no-interactivo)
{
    echo "CONNECT TO MINIBANK USER db2inst1 USING password"

    # Insertar cuenta si no existe
    echo "INSERT INTO ACCOUNTS (ACCOUNT_ID, ACCOUNT_NAME, BALANCE) SELECT '$ACCOUNT_ID', 'Cuenta $ACCOUNT_ID', 0 FROM SYSIBM.SYSDUMMY1 WHERE NOT EXISTS (SELECT 1 FROM ACCOUNTS WHERE ACCOUNT_ID='$ACCOUNT_ID')"

    # Insertar transacción
    echo "INSERT INTO TRANSACTIONS (TRANSACTION_ID, ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT) VALUES ('$TXN_ID', '$ACCOUNT_ID', DATE('$DATE'), '$TX_TYPE', $AMOUNT)"

    # Actualizar saldo de la cuenta
    if [[ "$TX_TYPE" == "CREDIT" ]]; then
        echo "UPDATE ACCOUNTS SET BALANCE = BALANCE + $AMOUNT WHERE ACCOUNT_ID='$ACCOUNT_ID'"
    else
        echo "UPDATE ACCOUNTS SET BALANCE = BALANCE - $AMOUNT WHERE ACCOUNT_ID='$ACCOUNT_ID'"
    fi

    echo "TERMINATE"
} | /opt/ibm/db2/V12.1/bin/db2 > /dev/null 2>&1

exit 0
