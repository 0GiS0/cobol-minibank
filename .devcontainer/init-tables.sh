#!/bin/bash
#############################################################
# 🗄️ Initialize DB2 minibank database
# Este script se ejecuta DENTRO del contenedor db2server
#############################################################

set -e

echo "🗄️ Inicializando BD2 minibank..."

# Conectar como db2inst1 y crear las tablas
docker exec -ti db2server bash -c "su - db2inst1 << 'SQLEOF'
db2 CONNECT TO minibank USER db2inst1 USING password

db2 CREATE TABLE ACCOUNTS (
    ACCOUNT_ID VARCHAR(30) PRIMARY KEY,
    ACCOUNT_NAME VARCHAR(100) NOT NULL,
    BALANCE DECIMAL(15,2) DEFAULT 0.00,
    CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UPDATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)

db2 CREATE TABLE TRANSACTIONS (
    TRANSACTION_ID INT GENERATED ALWAYS AS IDENTITY,
    ACCOUNT_ID VARCHAR(30) NOT NULL,
    TRANSACTION_DATE DATE NOT NULL,
    TRANSACTION_TYPE VARCHAR(10) NOT NULL,
    AMOUNT DECIMAL(15,2) NOT NULL,
    CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (TRANSACTION_ID),
    FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS(ACCOUNT_ID)
)

db2 CREATE INDEX IDX_TRANS_ACCOUNT ON TRANSACTIONS(ACCOUNT_ID)
db2 CREATE INDEX IDX_TRANS_DATE ON TRANSACTIONS(TRANSACTION_DATE)

db2 CONNECT RESET
SQLEOF
"

echo "✅ Tablas creadas exitosamente!"
