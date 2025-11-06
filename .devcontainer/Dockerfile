# Dev Container base: solo herramientas; tu código queda en el volumen
FROM debian:bookworm

# Usuario no root (VS Code devcontainers)
RUN useradd -ms /bin/bash vscode && apt-get update && apt-get install -y --no-install-recommends \
    gnucobol make gdb nano ca-certificates git curl netcat-openbsd tree \
    python3 python3-pip python3-dev libxml2-utils build-essential pkg-config && \
    rm -rf /var/lib/apt/lists/*

# ==========================================
# Instalar cliente Python para DB2
# ==========================================
# Primero instalar dependencias de compilación
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc g++ make python3-dev && \
    rm -rf /var/lib/apt/lists/*

# Instalar paquetes Python para DB2
RUN pip install --upgrade pip setuptools wheel --break-system-packages && \
    pip install --no-cache-dir ibm-db ibm-db-sa sqlalchemy --break-system-packages && \
    echo "✅ ibm-db instalado correctamente"

# Locale opcional para tooling
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

WORKDIR /workspaces/cobol-minibank

USER vscode
