#!/bin/bash
# ==========================================
# 🔧 Setup para SQL Embebido en GnuCOBOL
# ==========================================
# Este script prepara el entorno para usar EXEC SQL 
# con GnuCOBOL y DB2

echo "🔧 Configurando entorno para SQL embebido..."

# Verificar si DB2 está disponible
if [ ! -d "/opt/ibm/db2" ]; then
    echo "⚠️  DB2 no encontrado en /opt/ibm/db2"
    echo "   El programa compilará pero puede fallar en runtime"
fi

# Verificar si GnuCOBOL tiene soporte SQL
if ! cobc --help | grep -q "sql" 2>/dev/null; then
    echo "⚠️  GnuCOBOL puede no tener soporte SQL completo"
    echo "   Para soporte completo, recompilar GnuCOBOL con --enable-db"
fi

# Crear directorio para archivos SQL temporales
mkdir -p /tmp/cobol-sql

# Configurar variables de entorno DB2 (si existe)
if [ -f "/opt/ibm/db2/V11.5/db2profile" ]; then
    echo "📡 Configurando variables DB2..."
    source /opt/ibm/db2/V11.5/db2profile
fi

echo "✅ Configuración completada"
echo ""
echo "Para usar SQL embebido:"
echo "  make build-sql    # Compilar programa"
echo "  make run-sql      # Ejecutar programa"
echo ""
echo "NOTAS IMPORTANTES:"
echo "• SQL embebido requiere precompilación especial"
echo "• Algunas funciones pueden requerir ajustes según la instalación de DB2"
echo "• Si hay errores de compilación, usar el wrapper Python (minibank-db2.cob)"