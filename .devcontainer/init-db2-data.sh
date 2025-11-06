#!/bin/bash
# ===============================================
# 🗄️  Initialize DB2 Database with Data
# ===============================================
# Este script:
# 1. Crea las tablas ACCOUNTS y TRANSACTIONS
# 2. Carga datos de ejemplo
# 3. Verifica la carga mostrando consultas

set -e

echo "🗄️  ======================================="
echo "    INICIALIZANDO DB2 Y CARGANDO DATOS"
echo "======================================="

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ===============================================
# PASO 1: Esperar a que DB2 esté disponible
# ===============================================
echo -e "\n${BLUE}⏳ Paso 1: Verificando disponibilidad de DB2...${NC}"

DB2_READY=false
for i in {1..60}; do
    if (echo > /dev/tcp/db/50000) 2>/dev/null; then
        echo -e "${GREEN}✅ DB2 está disponible en db:50000${NC}"
        DB2_READY=true
        break
    fi
    if [ $((i % 10)) -eq 0 ]; then
        echo -e "${YELLOW}⏳ Intento $i/60 - esperando DB2...${NC}"
    fi
    sleep 1
done

if [ "$DB2_READY" = false ]; then
    echo -e "${RED}❌ DB2 no respondió en 60 segundos${NC}"
    echo "    Verifica que el contenedor db2server esté corriendo:"
    echo "    docker ps | grep db2server"
    exit 1
fi

sleep 2  # Extra espera para que DB2 finalice inicialización

# ===============================================
# PASO 2: Crear tablas en DB2
# ===============================================
echo -e "\n${BLUE}📊 Paso 2: Creando tablas en DB2...${NC}"

docker exec -ti db2server bash -c "su - db2inst1 << 'SQLEOF'
SET SCHEMA = db2inst1
-- Conectar a la base de datos
CONNECT TO minibank USER db2inst1 USING password

-- Verificar si las tablas existen y eliminarlas
DROP TABLE TRANSACTIONS
DROP TABLE ACCOUNTS

-- Crear tabla ACCOUNTS
CREATE TABLE ACCOUNTS (
    ACCOUNT_ID VARCHAR(30) PRIMARY KEY,
    ACCOUNT_NAME VARCHAR(100) NOT NULL,
    BALANCE DECIMAL(15,2) DEFAULT 0.00,
    CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UPDATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)

-- Crear tabla TRANSACTIONS
CREATE TABLE TRANSACTIONS (
    TRANSACTION_ID INT GENERATED ALWAYS AS IDENTITY,
    ACCOUNT_ID VARCHAR(30) NOT NULL,
    TRANSACTION_DATE DATE NOT NULL,
    TRANSACTION_TYPE VARCHAR(10) NOT NULL,
    AMOUNT DECIMAL(15,2) NOT NULL,
    CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (TRANSACTION_ID),
    FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS(ACCOUNT_ID) ON DELETE CASCADE
)

-- Crear índices para mejor performance
CREATE INDEX IDX_TRANS_ACCOUNT ON TRANSACTIONS(ACCOUNT_ID)
CREATE INDEX IDX_TRANS_DATE ON TRANSACTIONS(TRANSACTION_DATE)

COMMIT

SQLEOF
" 2>&1 | grep -E "DB20000I|SQL|Error" || true

echo -e "${GREEN}✅ Tablas creadas exitosamente${NC}"

# ===============================================
# PASO 3: Cargar datos de ejemplo
# ===============================================
echo -e "\n${BLUE}📝 Paso 3: Cargando datos de ejemplo...${NC}"

docker exec -ti db2server bash -c "su - db2inst1 << 'SQLEOF'
CONNECT TO minibank USER db2inst1 USING password

-- Crear cuentas de ejemplo
INSERT INTO ACCOUNTS (ACCOUNT_ID, ACCOUNT_NAME, BALANCE)
VALUES ('ACC-001', 'Cuenta Corriente Juan', 0.00)

INSERT INTO ACCOUNTS (ACCOUNT_ID, ACCOUNT_NAME, BALANCE)
VALUES ('ACC-002', 'Cuenta Ahorro María', 0.00)

INSERT INTO ACCOUNTS (ACCOUNT_ID, ACCOUNT_NAME, BALANCE)
VALUES ('ACC-003', 'Cuenta Inversión Pedro', 0.00)

-- Insertar transacciones de ejemplo
INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
VALUES ('ACC-001', '2025-01-10', 'CREDIT', 1000.00)

INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
VALUES ('ACC-001', '2025-01-12', 'DEBIT', 150.25)

INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
VALUES ('ACC-002', '2025-01-15', 'CREDIT', 500.00)

INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
VALUES ('ACC-001', '2025-01-18', 'CREDIT', 200.00)

INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
VALUES ('ACC-002', '2025-01-20', 'DEBIT', 50.75)

INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
VALUES ('ACC-003', '2025-01-22', 'CREDIT', 2500.00)

INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
VALUES ('ACC-003', '2025-01-25', 'DEBIT', 350.50)

INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
VALUES ('ACC-002', '2025-01-28', 'CREDIT', 100.00)

INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
VALUES ('ACC-001', '2025-02-01', 'DEBIT', 75.30)

INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
VALUES ('ACC-003', '2025-02-03', 'CREDIT', 450.00)

COMMIT

SQLEOF
" 2>&1 | grep -E "INSERT|Error" || true

echo -e "${GREEN}✅ Datos de ejemplo cargados${NC}"

# ===============================================
# PASO 4: Calcular saldos y mostrar verificación
# ===============================================
echo -e "\n${BLUE}🔍 Paso 4: Verificando datos cargados...${NC}"
echo -e "\n${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${YELLOW}📊 CUENTAS EN EL SISTEMA:${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

docker exec -ti db2server bash -c "su - db2inst1 << 'SQLEOF'
CONNECT TO minibank USER db2inst1 USING password
SET ISOLATION = CS

-- Mostrar cuentas
SELECT 
    ACCOUNT_ID,
    ACCOUNT_NAME,
    BALANCE,
    CREATED_AT
FROM ACCOUNTS
ORDER BY ACCOUNT_ID

CONNECT RESET
SQLEOF
"

# ===============================================
# PASO 5: Mostrar transacciones
# ===============================================
echo -e "\n${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${YELLOW}📋 TRANSACCIONES CARGADAS:${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

docker exec -ti db2server bash -c "su - db2inst1 << 'SQLEOF'
CONNECT TO minibank USER db2inst1 USING password
SET ISOLATION = CS

-- Mostrar transacciones
SELECT 
    TRANSACTION_ID,
    ACCOUNT_ID,
    TRANSACTION_DATE,
    TRANSACTION_TYPE,
    AMOUNT,
    CREATED_AT
FROM TRANSACTIONS
ORDER BY TRANSACTION_ID

CONNECT RESET
SQLEOF
"

# ===============================================
# PASO 6: Mostrar resumen de saldos calculados
# ===============================================
echo -e "\n${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${YELLOW}💰 RESUMEN DE SALDOS (Calculados):${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

docker exec -ti db2server bash -c "su - db2inst1 << 'SQLEOF'
CONNECT TO minibank USER db2inst1 USING password
SET ISOLATION = CS

-- Calcular saldos finales por cuenta
SELECT 
    A.ACCOUNT_ID,
    A.ACCOUNT_NAME,
    COALESCE(A.BALANCE, 0) AS BALANCE_ANTERIOR,
    COALESCE(SUM(T.AMOUNT), 0) AS TOTAL_TRANSACCIONES,
    COALESCE(A.BALANCE, 0) + COALESCE(SUM(T.AMOUNT), 0) AS SALDO_FINAL,
    COUNT(T.TRANSACTION_ID) AS NUM_TRANSACCIONES
FROM ACCOUNTS A
LEFT JOIN TRANSACTIONS T ON A.ACCOUNT_ID = T.ACCOUNT_ID
GROUP BY A.ACCOUNT_ID, A.ACCOUNT_NAME, A.BALANCE
ORDER BY A.ACCOUNT_ID

CONNECT RESET
SQLEOF
"

echo -e "\n${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "\n${GREEN}🎉 ¡Inicialización completada exitosamente!${NC}\n"
echo -e "📌 ${BLUE}Información de conexión:${NC}"
echo "   🗄️  Base de datos: minibank"
echo "   🖥️  Servidor: db:50000"
echo "   👤 Usuario: db2inst1"
echo "   🔑 Contraseña: password"
echo ""
echo -e "${BLUE}📌 Próximos pasos:${NC}"
echo "   1. Compilar programa: ${BLUE}make build${NC}"
echo "   2. Ejecutar programa:  ${BLUE}make run${NC}"
echo "   3. Ver resultados:     ${BLUE}cat data/balances.csv${NC}"
echo ""
