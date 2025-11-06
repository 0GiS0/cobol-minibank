#!/bin/bash
set -e

echo "🏦 COBOL MiniBank - Post Create Setup"
echo "=================================="

# Crear directorio de datos
echo "📁 Creating data directory..."
mkdir -p data

# Crear archivo de transacciones de ejemplo si no existe
if [ ! -f data/transactions.csv ]; then
    echo "📝 Creating sample transactions file..."
    cat > data/transactions.csv << 'EOF'
2025-01-10,ACC-001,CREDIT,1000
2025-01-12,ACC-001,DEBIT,150.25
2025-01-15,ACC-002,CREDIT,500
2025-01-18,ACC-001,CREDIT,200
2025-01-20,ACC-002,DEBIT,50.75
2025-01-22,ACC-003,CREDIT,2500
2025-01-25,ACC-003,DEBIT,350.50
2025-01-28,ACC-002,CREDIT,100
2025-02-01,ACC-001,DEBIT,75.30
2025-02-03,ACC-003,CREDIT,450
EOF
    echo "✅ Sample transactions created with $(wc -l < data/transactions.csv) transactions"
else
    echo "✅ Transactions file already exists"
fi

# Configurar Git si no está configurado
if [ -z "$(git config --global user.name)" ]; then
    echo "🔧 Setting up basic Git configuration..."
    git config --global init.defaultBranch main
    git config --global core.autocrlf input
    git config --global core.editor "code --wait"
fi

# Verificar que GnuCOBOL está instalado
echo "�� Verifying COBOL compiler..."
if command -v cobc &> /dev/null; then
    COBOL_VERSION=$(cobc --version | head -n 1)
    echo "✅ $COBOL_VERSION"
else
    echo "❌ COBOL compiler not found!"
    exit 1
fi

# Crear directorio de build si no existe
echo "📁 Creating build directory..."
mkdir -p build

# 🗄️ Configurar DB2 automáticamente
echo ""
echo "🗄️ Setting up DB2..."

# Esperar a que DB2 esté disponible (máximo 180 segundos = 3 minutos)
# Usando /dev/tcp para verificar conexión real, no solo puerto abierto
DB2_READY=false
for i in {1..180}; do
    if (echo > /dev/tcp/db/50000) 2>/dev/null; then
        echo "✅ DB2 is ready (connection successful)"
        DB2_READY=true
        break
    fi
    # Mostrar progreso cada 10 segundos para no saturar output
    if [ $((i % 10)) -eq 0 ]; then
        echo "⏳ Waiting for DB2... ($i/180 seconds)"
    fi
    sleep 1
done

if [ "$DB2_READY" = true ]; then
    sleep 3  # Extra wait for DB2 to be fully initialized
    echo "📊 DB2 ready for connections"

    # ✨ Inicializar BD2 con datos de ejemplo
    echo ""
    echo "🎯 Ejecutando inicialización de DB2..."
    python3 .devcontainer/init-db2-python.py
    python3 .devcontainer/load-sample-data.py
else
    echo "⚠️  DB2 did not start after 3 minutes, but continuing..."
    echo "    The DB2 container may still be initializing."
    echo "    You can manually run: .devcontainer/init-db2-data.sh"
fi

