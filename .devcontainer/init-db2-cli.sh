#!/bin/bash
# =====================================================================
# init-db2-cli.sh - Inicializar DB2 y crear tablas
# =====================================================================

# Configurar entorno DB2
export DB2INSTANCE=db2inst1
export HOME=/home/db2inst1
export LD_LIBRARY_PATH=/opt/ibm/db2/V12.1/lib:$LD_LIBRARY_PATH
export PATH=/opt/ibm/db2/V12.1/bin:$PATH

echo "🔧 Inicializando base de datos MINIBANK..."

# Conectar a DB2
/opt/ibm/db2/V12.1/bin/db2 connect to MINIBANK user db2inst1 using password > /dev/null 2>&1

# Crear tabla ACCOUNTS
echo "📊 Creando tabla ACCOUNTS..."
/opt/ibm/db2/V12.1/bin/db2 "DROP TABLE TRANSACTIONS" 2>/dev/null || true
/opt/ibm/db2/V12.1/bin/db2 "DROP TABLE ACCOUNTS" 2>/dev/null || true
/opt/ibm/db2/V12.1/bin/db2 "CREATE TABLE ACCOUNTS (ACCOUNT_ID VARCHAR(30) NOT NULL PRIMARY KEY, ACCOUNT_NAME VARCHAR(50), BALANCE DECIMAL(15,2))" 2>&1 | grep -v "^$"

# Crear tabla TRANSACTIONS
echo "📝 Creando tabla TRANSACTIONS..."
/opt/ibm/db2/V12.1/bin/db2 "CREATE TABLE TRANSACTIONS (TRANSACTION_ID VARCHAR(30) NOT NULL PRIMARY KEY, ACCOUNT_ID VARCHAR(30), TRANSACTION_DATE DATE, TRANSACTION_TYPE VARCHAR(10), AMOUNT DECIMAL(15,2), FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS(ACCOUNT_ID))" 2>&1 | grep -v "^$"

# Desconectar
/opt/ibm/db2/V12.1/bin/db2 connect reset > /dev/null 2>&1

echo "✅ Inicialización completada"
