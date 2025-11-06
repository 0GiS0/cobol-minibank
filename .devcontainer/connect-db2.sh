#!/bin/bash
#############################################################
# 🗄️ DB2 Connection Helper for MiniBank
# Simple port check and Python fallback
#############################################################

DB_HOST="db"
DB_PORT="50000"
DB_NAME="minibank"
DB_USER="db2inst1"

echo "🗄️ Connecting to DB2..."
echo "   Host: $DB_HOST:$DB_PORT"
echo "   Database: $DB_NAME"
echo ""

# Verificar conectividad
echo "⏳ Testing connection..."
if timeout 5 bash -c "(echo >/dev/tcp/$DB_HOST/$DB_PORT) 2>/dev/null"; then
    echo "✅ DB2 port is reachable!"
    
    # Intentar con Python si está disponible
    if command -v python3 &> /dev/null && python3 -c "import ibm_db" 2>/dev/null; then
        echo ""
        echo "�� Using Python to query DB2..."
        python3 .devcontainer/connect-db2.py
    else
        echo ""
        echo "📋 DB2 is ready!"
        echo "   To initialize tables: db2 -tf .devcontainer/init-db2.sql"
        echo "   To connect manually: db2 CONNECT TO $DB_NAME USER $DB_USER USING password"
    fi
else
    echo "⚠️  DB2 not reachable yet"
    echo ""
    echo "💡 It may still be initializing. Try again in a few seconds..."
    exit 1
fi
