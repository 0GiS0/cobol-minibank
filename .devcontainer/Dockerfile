# Dev Container base: solo herramientas; tu código queda en el volumen
FROM debian:bookworm

# Usuario no root (VS Code devcontainers)
RUN useradd -ms /bin/bash vscode && apt-get update && apt-get install -y --no-install-recommends \
    gnucobol make gdb nano ca-certificates git curl netcat-openbsd tree \
    python3 python3-pip python3-dev libxml2-utils && \
    rm -rf /var/lib/apt/lists/*

# Instalar cliente Python para DB2
RUN pip install --no-cache-dir ibm-db ibm-db-sa sqlalchemy 2>/dev/null || \
    pip install --no-cache-dir ibm-db sqlalchemy || echo "Note: ibm-db install attempted"

# Locale opcional para tooling
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

WORKDIR /workspaces/cobol-minibank

USER vscode
