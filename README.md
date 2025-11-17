# 🏦 COBOL MiniBank - Entorno de Pruebas con Agentes AI

**Español** | **[English](README_EN.md)**

## 🤔 ¿Qué es este proyecto?

Un **sandbox COBOL educativo** con integración DB2 y un conjunto de **agentes AI especializados** para trabajar con código mainframe de manera moderna.

**Ideal para:**
- 🎓 Aprender COBOL desde cero
- 🧪 Probar código mainframe en un entorno seguro
- 🤖 Experimentar con agentes AI para COBOL
- 📚 Ver patrones de arquitectura modular en COBOL

---

## 🚀 Quick Start

### 1️⃣ Abrir en Dev Container
```bash
# En VS Code
1. Abrir carpeta del repo
2. Click en "Reopen in Container"
3. Esperar que termine el setup (instala DB2 + datos)
```

### 2️⃣ Probar los programas
```bash
# CSV básico (sin DB)
make run

# Sistema con DB2 + menú interactivo
make run-menu

# Sistema modular dual-mode (DB2 o stub)
export MINIBANK_DB_MODULE=MBDBSQL
./src/mb-main
```

### 3️⃣ Usar los agentes AI
Disponibles en `.github/agents/` - Ver sección de agentes abajo 👇

---

## 🤖 Agentes AI Especializados

Este repo incluye **6 agentes expertos en COBOL** que podés usar con GitHub Copilot:

| Agente | Qué hace |
|--------|----------|
| **📚 COBOL Documenter** | Genera documentación técnica completa para sistemas COBOL |
| **🔧 COBOL Module Builder** | Implementa módulos COBOL siguiendo estándares enterprise |
| **📋 COBOL Project Planner** | Planifica proyectos COBOL con arquitectura modular y timelines |
| **📊 Impact Analyzer** | Analiza el impacto de cambios en código y dependencias |
| **⚙️ JCL Generator** | Genera JCL optimizado para compilación y deployment |
| **🎨 Mermaid Diagram Creator** | Crea diagramas de arquitectura y flujos de datos |

**Ubicación:** `.github/agents/` - Cada agente tiene instrucciones detalladas.

---

## 💬 Prompts Listos para Usar

Prompts preconfigurados en `.github/prompts/`:

1. **📚 Documentar código** → `01-cobol-documenter.prompt.md`
2. **🔧 Crear módulo** → `02-cobol-module-builder.prompt.md`
3. **📋 Planificar proyecto** → `03-cobol-project-planner.prompt.md`
4. **📊 Analizar impacto** → `04-impact-analyzer.prompt.md`
5. **⚙️ Generar JCL** → `05-jcl-generator.prompt.md`
6. **🎨 Crear diagramas** → `06-mermaid-diagram-creator.prompt.md`

**Ejemplo de uso:**
```bash
# Con GitHub Copilot CLI
gh copilot --prompt-file .github/prompts/01-cobol-documenter.prompt.md
```

---

## 🏗️ Programas COBOL Incluidos

### Programas Educativos (Legacy)
| Programa | Nivel | Qué hace |
|----------|-------|----------|
| `minibank.cob` | 🟢 Básico | Procesa CSV con transacciones bancarias |
| `minibank-db2.cob` | 🟡 Intermedio | Integración con DB2 vía Python wrapper |
| `minibank-menu.cob` | 🟠 Avanzado | Sistema interactivo con menú y consultas |

### Sistema Modular Actual (2025)
| Módulo | Función |
|--------|---------|
| `mb-main.cbl` | Programa principal con dual-mode support |
| `mb-db-sql.cbl` | Módulo de acceso a DB2 (producción) |
| `mb-db-cli.cbl` | Módulo stub para testing (sin DB) |
| `mb-db-if.cpy` | Interface compartida (contrato de API) |

**Arquitectura modular:** El sistema usa variables de entorno para cambiar entre DB2 real y datos simulados sin recompilar.

---

## 📚 Documentación Adicional

- **📖 [README Detallado](README_DETAILED.md)** - Documentación técnica completa (1000+ líneas)
- **🗄️ [Guía de Bases de Datos](BASES_DE_DATOS.md)** - Comparativa DB2 vs PostgreSQL + ocesql
- **📝 [Custom Instructions](.github/CUSTOM_INSTRUCTIONS.md)** - Reglas para agentes AI
- **🐳 [Setup de DB2](.devcontainer/)** - Configuración del contenedor y database

---

## 🛠️ Comandos Útiles

### Compilación
```bash
make build        # Compila todos los programas
make clean        # Limpia binarios
```

### Testing
```bash
# Modo desarrollo (sin DB2)
export MINIBANK_DB_MODULE=MBDBCLI
./src/mb-main

# Modo producción (con DB2)
export MINIBANK_DB_MODULE=MBDBSQL
./src/mb-main

# Test dual-mode automático
./test-dual-mode.sh
```

### DB2 Helpers
```bash
# Consultar saldos
./db2-helpers/get-balances-cli.sh

# Insertar transacción
./db2-helpers/insert-transaction-cli.sh

# Ver transacciones
./db2-helpers/load-transactions-cli.sh
```

---

## 🎯 ¿Por qué COBOL en 2025?

- **💰 95% de transacciones bancarias** usan COBOL
- **🏢 220 billones de líneas** de código COBOL en producción
- **📊 Procesamiento masivo** de datos financieros
- **🔒 Precisión decimal** crítica para dinero
- **🏗️ Sistemas legacy** que necesitan mantenimiento

**Este repo te ayuda a:** Entender COBOL moderno con herramientas AI que facilitan el trabajo en mainframes.

---

## 🤝 Contribuir

¿Querés agregar agentes, prompts o mejorar los ejemplos?

1. Fork del repo
2. Creá tu rama: `git checkout -b feature/nuevo-agente`
3. Hacé tus cambios
4. Pull request con descripción clara

---

## 📄 Licencia

MIT - Usalo, modificalo, aprendé.

---

**🔥 Pro tip:** Si estás aprendiendo COBOL, empezá con `make run`, después probá `make run-menu`, y finalmente mirá los agentes AI para ver cómo automatizar tareas complejas.
