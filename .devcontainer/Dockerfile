# Dev Container base: solo herramientas; tu código queda en el volumen
FROM debian:bookworm

# Usuario no root (VS Code devcontainers)
RUN useradd -ms /bin/bash vscode && apt-get update && apt-get install -y --no-install-recommends         gnucobol make gdb nano ca-certificates git curl &&     rm -rf /var/lib/apt/lists/*

# Locale opcional para tooling
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

WORKDIR /workspaces/cobol-minibank

USER vscode