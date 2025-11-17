#!/bin/bash
# ============================================================
# 🧪 Script de prueba - MiniBank Dual Mode (CSV/DB2)
# ============================================================

set -e

cd /workspaces/cobol-minibank

echo "🏦 ========== PRUEBA DE MINIBANK =========="
echo ""
echo "Modo 1: CSV (Datos en memoria)"
echo "=================================================="
echo ""

# Exportar variables de entorno para modo CSV
export MINIBANK_DATA_SOURCE=CSV

# Crear entrada para el programa (opciones de menú)
{
    echo "4"  # Ver cuentas
    echo "1"  # Consultar saldo ACC-001
    echo "ACC-001"
    echo "9"  # Salir
} | ./src/mb-main

echo ""
echo "=================================================="
echo "Modo 2: DB2 (Datos en base de datos)"
echo "=================================================="
echo ""

# Exportar variables para modo DB2
export MINIBANK_DATA_SOURCE=DB2
export MINIBANK_DB_MODULE=mb-db-sql.so

# Crear entrada de prueba
{
    echo "1"  # Consultar saldo
    echo "ACC-001"
    echo "9"  # Salir
} | ./src/mb-main

echo ""
echo "✅ Prueba completada"
