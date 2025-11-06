# Dev Container base: solo herramientas; tu código queda en el volumen
FROM debian:bookworm

# Usuario no root (VS Code devcontainers)
RUN useradd -ms /bin/bash vscode && apt-get update && apt-get install -y --no-install-recommends \
    gnucobol make gdb nano ca-certificates git curl netcat-openbsd tree \
    python3 python3-pip python3-dev libxml2-utils && \
    rm -rf /var/lib/apt/lists/*

# ==========================================
# Instalar dependencias de compilación (necesarias para ibm-db)
# ==========================================
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential gcc g++ pkg-config && \
    rm -rf /var/lib/apt/lists/*

# ==========================================
# Instalar cliente Python para DB2 (ibm-db)
# ==========================================
# Actualizar pip y setuptools para asegurar compatibilidad
RUN pip install --upgrade pip setuptools wheel --break-system-packages 2>/dev/null || \
    pip3 install --upgrade pip setuptools wheel --break-system-packages

# Instalar ibm-db con reintentos y manejo de errores
RUN pip3 install --no-cache-dir --retries 5 ibm-db --break-system-packages 2>/dev/null || \
    pip install --no-cache-dir --retries 5 ibm-db --break-system-packages && \
    python3 -c "import ibm_db; print('✅ ibm-db importado correctamente')" || \
    echo "⚠️ Advertencia: ibm-db puede requerir instalación manual"

# Locale opcional para tooling
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

WORKDIR /workspaces/cobol-minibank

USER vscode
