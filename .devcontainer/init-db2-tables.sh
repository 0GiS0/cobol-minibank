#!/bin/bash
# Script para inicializar las tablas en DB2

echo "🗄️  Inicializando tablas en DB2..."

# Esperar a que DB2 esté listo
echo "⏳ Esperando a que DB2 esté listo..."
for i in {1..30}; do
    if (echo > /dev/tcp/db/50000) 2>/dev/null; then
        echo "✅ DB2 está listo!"
        break
    fi
    echo "  Intento $i/30..."
    sleep 1
done

# Crear las tablas via docker exec
docker exec -ti db2server bash -c "su - db2inst1 << 'EOF'
db2 CONNECT TO minibank USER db2inst1 USING password

# Crear tabla ACCOUNTS si no existe
db2 DROP TABLE ACCOUNTS
db2 CREATE TABLE ACCOUNTS (
    ACCOUNT_ID INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    ACCOUNT_NAME VARCHAR(30) NOT NULL,
    BALANCE DECIMAL(15,2) DEFAULT 0.00,
    CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UPDATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)

# Crear tabla TRANSACTIONS si no existe
db2 DROP TABLE TRANSACTIONS
db2 CREATE TABLE TRANSACTIONS (
    TRANSACTION_ID INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    ACCOUNT_ID INT NOT NULL,
    TRANSACTION_DATE DATE NOT NULL,
    TRANSACTION_TYPE VARCHAR(10) NOT NULL,
    AMOUNT DECIMAL(15,2) NOT NULL,
    CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS(ACCOUNT_ID)
)

# Crear índices
db2 CREATE INDEX IDX_TRANS_ACCOUNT ON TRANSACTIONS(ACCOUNT_ID)
db2 CREATE INDEX IDX_TRANS_DATE ON TRANSACTIONS(TRANSACTION_DATE)

# Commit
db2 COMMIT

echo "✅ Tablas creadas exitosamente"
db2 LIST TABLES
db2 CONNECT RESET
EOF
"

echo "✅ Inicialización completada"
