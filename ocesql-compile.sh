#!/bin/bash
# ============================================================
# ocesql-compile.sh - Script para precompilar y compilar
#                     archivos COBOL con EXEC SQL
# ============================================================

set -e

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Variables
OCESQL_INCLUDE="/usr/local/share/open-cobol-esql/copy"
OCESQL_LIB="/usr/local/lib"
SRC_DIR="${1:-.}"
OUTPUT_DIR="${2:-./build}"

# Función para mostrar mensajes
log_info() {
    echo -e "${GREEN}✅${NC} $1"
}

log_error() {
    echo -e "${RED}❌${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}⚠️${NC} $1"
}

# Verificar que ocesql está instalado
if ! command -v ocesql &> /dev/null; then
    log_error "ocesql no está instalado. Instálalo con: apt-get install open-cobol-esql"
    exit 1
fi

log_info "Open Cobol ESQL $(ocesql --version | grep Version)"

# Crear directorio de salida si no existe
mkdir -p "$OUTPUT_DIR"

# Procesar cada archivo .cbl con EXEC SQL
for cbl_file in "$SRC_DIR"/*.cbl; do
    if [ ! -f "$cbl_file" ]; then
        continue
    fi

    basename=$(basename "$cbl_file" .cbl)
    processed_file="$SRC_DIR/${basename}-processed.cbl"
    executable="$OUTPUT_DIR/$basename"

    # Verificar si es un archivo ya procesado
    if [[ "$cbl_file" == *"-processed.cbl" ]]; then
        continue
    fi

    # Verificar si contiene EXEC SQL
    if ! grep -q "EXEC SQL" "$cbl_file"; then
        log_warn "Saltando $cbl_file (sin EXEC SQL)"
        continue
    fi

    echo ""
    log_info "Precompilando: $cbl_file"

    # Precompilar
    if ocesql --inc="$OCESQL_INCLUDE" "$cbl_file" "$processed_file"; then
        log_info "Precompilación exitosa: $processed_file"
    else
        log_error "Error en precompilación de $cbl_file"
        continue
    fi

    echo ""
    log_info "Compilando: $processed_file"

    # Compilar
    if cobc -x -Wall \
           -I"$OCESQL_INCLUDE" \
           "$processed_file" \
           -o "$executable" \
           -L"$OCESQL_LIB" \
           -locesql; then
        log_info "Compilación exitosa: $executable"

        # Hacer ejecutable
        chmod +x "$executable"
        log_info "Ejecutable listo: $executable"
    else
        log_error "Error en compilación de $processed_file"
        continue
    fi
done

echo ""
log_info "Proceso completado"
