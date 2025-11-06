#!/bin/bash
# ===============================================
# 🔍 Verificar estado de DB2 y datos
# ===============================================
# Script para verificar rápidamente que:
# 1. DB2 está disponible
# 2. Las tablas existen
# 3. Los datos están presentes

set -e

echo "🔍 ======================================="
echo "    VERIFICANDO ESTADO DE DB2"
echo "======================================="

# Colores
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# ===============================================
# Verificar conexión
# ===============================================
echo -e "\n${BLUE}1️⃣ Verificando conexión a DB2...${NC}"

if (echo > /dev/tcp/db/50000) 2>/dev/null; then
    echo -e "${GREEN}✅ DB2 está disponible en db:50000${NC}"
else
    echo -e "${RED}❌ No se puede conectar a DB2 en db:50000${NC}"
    echo -e "${YELLOW}   Asegúrate de que el contenedor db2server esté corriendo:${NC}"
    echo -e "   docker ps | grep db2server"
    exit 1
fi

# ===============================================
# Contar registros
# ===============================================
echo -e "\n${BLUE}2️⃣ Verificando tablas y registros...${NC}"

docker exec -ti db2server bash -c "su - db2inst1 << 'SQLEOF'
CONNECT TO minibank USER db2inst1 USING password
SET ISOLATION = CS

-- Contar cuentas
SELECT COUNT(*) AS "Total Cuentas" FROM ACCOUNTS
SELECT COUNT(*) AS "Total Transacciones" FROM TRANSACTIONS

CONNECT RESET
SQLEOF
" 2>&1 | tail -20

# ===============================================
# Mostrar datos
# ===============================================
echo -e "\n${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}3️⃣ Mostrando datos de cuentas...${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

docker exec -ti db2server bash -c "su - db2inst1 << 'SQLEOF'
CONNECT TO minibank USER db2inst1 USING password
SET ISOLATION = CS

SELECT 
    ACCOUNT_ID,
    ACCOUNT_NAME,
    BALANCE
FROM ACCOUNTS
ORDER BY ACCOUNT_ID

CONNECT RESET
SQLEOF
"

# ===============================================
# Mostrar resumen de transacciones
# ===============================================
echo -e "\n${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}4️⃣ Resumen de transacciones por cuenta...${NC}"
echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"

docker exec -ti db2server bash -c "su - db2inst1 << 'SQLEOF'
CONNECT TO minibank USER db2inst1 USING password
SET ISOLATION = CS

SELECT 
    ACCOUNT_ID,
    TRANSACTION_TYPE,
    COUNT(*) AS "Cantidad",
    SUM(AMOUNT) AS "Total"
FROM TRANSACTIONS
GROUP BY ACCOUNT_ID, TRANSACTION_TYPE
ORDER BY ACCOUNT_ID, TRANSACTION_TYPE

CONNECT RESET
SQLEOF
"

echo -e "\n${GREEN}✅ Verificación completada${NC}\n"
