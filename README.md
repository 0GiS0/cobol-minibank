# 🏦 COBOL MiniBank - Sistema Bancario Simplificado

## 🤔 ¿Qué es este proyecto?

Este es un ejemplo educativo de un **sistema bancario simplificado** escrito en **COBOL** (Common Business-Oriented Language), un lenguaje de programación creado en 1959 y que sigue siendo ampliamente usado en sistemas bancarios y financieros empresariales.

## ⚙️ ¿Qué hace el programa?

El programa simula las operaciones básicas de un banco:

1. 📄 **Lee transacciones** desde un archivo CSV (`data/transactions.csv`)
2. 🔄 **Procesa cada transacción** (depósitos y retiros)
3. 🧮 **Calcula los saldos** de todas las cuentas
4. 📊 **Genera un reporte** con los saldos finales (`data/balances.csv`)

### 💡 Ejemplo de funcionamiento:

**📥 Archivo de entrada** (`transactions.csv`):
```
2025-01-10,ACC-001,CREDIT,1000      # 💰 Depósito de $1000 en cuenta ACC-001
2025-01-12,ACC-001,DEBIT,150.25     # 💸 Retiro de $150.25 de cuenta ACC-001
2025-01-15,ACC-002,CREDIT,500       # 💰 Depósito de $500 en cuenta ACC-002
2025-01-18,ACC-001,CREDIT,200       # 💰 Depósito de $200 en cuenta ACC-001
2025-01-20,ACC-002,DEBIT,50.75      # 💸 Retiro de $50.75 de cuenta ACC-002
```

**📤 Archivo de salida** (`balances.csv`):
```
account,balance
ACC-001,         1049.75             # 💵 $1000 - $150.25 + $200 = $1049.75
ACC-002,          449.25             # 💵 $500 - $50.75 = $449.25
```

## 🚀 ¿Por qué COBOL?

COBOL sigue siendo el lenguaje predominante en:
- 🏦 **Sistemas bancarios** (procesa el 95% de transacciones ATM)
- 💳 **Procesamiento de tarjetas de crédito**
- 🏢 **Sistemas gubernamentales y de seguros**
- 📊 **Aplicaciones de nómina y contabilidad**

Sus características principales:
- 📖 **Legibilidad**: Sintaxis similar al inglés
- 🎯 **Precisión decimal**: Ideal para cálculos financieros
- 🏗️ **Estabilidad**: Programas que funcionan décadas sin modificarse
- ⚡ **Procesamiento masivo**: Maneja millones de registros eficientemente

## 🛠️ Configuración y Ejecución

### 📋 Requisitos
- VS Code + Dev Containers extension
- Docker activo

### 🚀 Pasos para ejecutar:
1. 📂 Abre la carpeta en VS Code
2. 🔄 Cuando te pregunte, selecciona **"Reopen in Container"**
3. 📝 Tras el `postCreate`, tendrás `data/transactions.csv` con datos de muestra

### 🔨 Compilar y ejecutar:
- **🏗️ Compilar**: `Terminal > Run Task > COBOL: build`
- **▶️ Ejecutar**: `Terminal > Run Task > COBOL: run`

El resultado se genera automáticamente en `data/balances.csv`.

## 📂 Estructura del proyecto

```
├── src/
│   ├── minibank.cob          # 💻 Programa principal en COBOL
│   └── copybooks/            # 📚 Definiciones de datos reutilizables
│       └── record-layout.cpy
├── data/
│   ├── transactions.csv      # 📥 Archivo de transacciones (entrada)
│   └── balances.csv         # 📤 Archivo de saldos (salida)
├── Makefile                 # 🔧 Instrucciones de compilación
└── README.md               # 📖 Esta documentación
```

## 🎓 Conceptos COBOL que puedes aprender

1. 🏗️ **Divisiones**: Estructura organizativa del programa
   - `IDENTIFICATION DIVISION`: 🆔 Identifica el programa
   - `ENVIRONMENT DIVISION`: 🌐 Define archivos y recursos
   - `DATA DIVISION`: 📊 Declara variables y estructuras de datos
   - `PROCEDURE DIVISION`: ⚙️ Contiene la lógica del programa

2. 📁 **Manejo de archivos**: Lectura y escritura de archivos CSV
3. 🗂️ **Estructuras de datos**: Arrays y registros para almacenar cuentas
4. 🧮 **Aritmética decimal**: Cálculos precisos con números decimales
5. 🔤 **Procesamiento de cadenas**: Parsing de datos CSV

## �️ Verificación de datos en DB2

El contenedor se inicializa automáticamente con datos de ejemplo. Para verificar que todo está correcto:

### ✅ Verificación automática (durante post-create)
- El script `post-create.sh` ejecuta automáticamente `init-db2-data.sh`
- Se crean las tablas ACCOUNTS y TRANSACTIONS
- Se cargan 10 transacciones de ejemplo
- Se muestran consultas para verificar los datos

### 🔍 Verificación manual en cualquier momento
```bash
# Ver estado de DB2 y datos cargados
.devcontainer/verify-db2.sh
```

Este script muestra:
- ✅ Estado de conexión a DB2
- 📊 Cantidad de cuentas y transacciones
- 💰 Detalles de saldos por cuenta
- 📋 Resumen de transacciones

### 🔄 Reinicializar datos
```bash
# Ejecutar nuevamente el script de inicialización
.devcontainer/init-db2-data.sh
```

## �🔧 Modificaciones posibles

- 📝 Cambiar el archivo de transacciones en `.vscode/tasks.json`
- ✅ Agregar validaciones (saldo mínimo, cuentas válidas)
- 🔄 Implementar diferentes tipos de transacciones
- 📈 Agregar reportes más detallados con fechas

## 🔧 Notas técnicas

- 🐧 Usa **GnuCOBOL** (compilador open source)
- 💾 El binario se genera en tu workspace local
- ✅ Compatible con estándares COBOL modernos
- 🐛 Incluye debugging y herramientas de desarrollo
- 🗄️ DB2 se inicia automáticamente con datos de ejemplo
