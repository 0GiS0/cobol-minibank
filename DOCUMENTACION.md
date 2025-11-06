# 📚 DOCUMENTACIÓN COMPLETA - COBOL MiniBank

## 📋 Índice
1. [Descripción General](#descripción-general)
2. [Flujo de Ejecución](#flujo-de-ejecución)
3. [Componentes del Sistema](#componentes-del-sistema)
4. [Arquitectura Técnica](#arquitectura-técnica)
5. [Análisis de Archivos](#análisis-de-archivos)
6. [Recomendaciones de Limpieza](#recomendaciones-de-limpieza)

---

## Descripción General

**COBOL MiniBank** es un proyecto educativo que demuestra cómo construir un **sistema bancario simplificado** utilizando COBOL con integración a **IBM DB2**.

### Propósito
Procesar transacciones bancarias (depósitos y retiros) desde un archivo CSV, persistirlas en una base de datos DB2, y generar un reporte de saldos finales por cuenta.

### Contexto
- COBOL es el lenguaje de programación usado en el 95% de sistemas bancarios mundiales
- DB2 es la base de datos más usada en mainframes empresariales
- Este proyecto combina ambas tecnologías para demostrar su integración

---

## Flujo de Ejecución

### Diagrama de flujo

```
┌─────────────────────────────────────────────────────────────┐
│  INICIO: Compilación del programa COBOL                     │
├─────────────────────────────────────────────────────────────┤
│  Comando: make build                                        │
│  Acción: Compila src/minibank.cob → src/minibank (ejecutable)
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│  PASO 1: Conexión a DB2                                     │
├─────────────────────────────────────────────────────────────┤
│  - Se conecta al servidor DB2 en hostname: db, puerto: 50000
│  - Usuario: db2inst1, contraseña: password                  │
│  - Se valida la conexión con EXEC SQL CONNECT              │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│  PASO 2: Apertura de archivos                               │
├─────────────────────────────────────────────────────────────┤
│  - INPUT:  data/transactions.csv (lectura)                  │
│  - OUTPUT: data/balances.csv (escritura)                    │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│  PASO 3: Procesamiento de transacciones (BUCLE PRINCIPAL)  │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  PARA CADA línea en transactions.csv:                       │
│  │                                                           │
│  ├─ 3.1: PARSE-LINE                                         │
│  │        Separa: fecha, cuenta, tipo, cantidad             │
│  │        Formato: 2025-01-10,ACC-001,CREDIT,1000           │
│  │                                                           │
│  ├─ 3.2: INSERT-TRANSACTION                                 │
│  │        - Busca cuenta en tabla ACCOUNTS                  │
│  │        - Si no existe → CREATE-ACCOUNT                   │
│  │        - Inserta en tabla TRANSACTIONS                   │
│  │        - Actualiza saldo en ACCOUNTS                     │
│  │                                                           │
│  └─ Continúa con siguiente línea                            │
│                                                              │
│  (Si hay 5 transacciones → 5 iteraciones)                   │
│                                                              │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│  PASO 4: Generación de reporte                              │
├─────────────────────────────────────────────────────────────┤
│  - WRITE-HEADER: Escribe encabezado "account,balance"       │
│  - QUERY-BALANCES:                                          │
│    - Abre cursor SELECT * FROM ACCOUNTS ORDER BY NAME       │
│    - Para cada fila:                                        │
│      - Obtiene: nombre_cuenta, saldo_final                  │
│      - Formatea: ACC-001,1049.75                            │
│      - Escribe en data/balances.csv                         │
│      - Muestra en terminal                                  │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│  PASO 5: Cierre y desconexión                               │
├─────────────────────────────────────────────────────────────┤
│  - CLOSE TX-FILE (transactions.csv)                         │
│  - CLOSE OUT-FILE (balances.csv)                            │
│  - DISCONNECT-DB2                                           │
│  - GOBACK (terminar programa)                               │
└─────────────────────────────────────────────────────────────┘
```

### Ejemplo de ejecución completa

**📥 Entrada: `data/transactions.csv`**
```csv
2025-01-10,ACC-001,CREDIT,1000
2025-01-12,ACC-001,DEBIT,150.25
2025-01-15,ACC-002,CREDIT,500
2025-01-18,ACC-001,CREDIT,200
2025-01-20,ACC-002,DEBIT,50.75
```

**Procesamiento en DB2:**
1. Lee "2025-01-10,ACC-001,CREDIT,1000"
   - Crea cuenta ACC-001 (saldo = 0)
   - Inserta transacción CREDIT
   - Actualiza saldo: ACC-001 = 1000

2. Lee "2025-01-12,ACC-001,DEBIT,150.25"
   - Busca ACC-001 (existe)
   - Inserta transacción DEBIT (-150.25)
   - Actualiza saldo: ACC-001 = 1000 - 150.25 = 849.75

3. (Continúa con las demás transacciones...)

**📤 Salida: `data/balances.csv`**
```csv
account,balance
ACC-001,1049.75
ACC-002,449.25
```

---

## Componentes del Sistema

### 1. Base de Datos DB2

#### Tablas necesarias:

**ACCOUNTS**
```sql
CREATE TABLE ACCOUNTS (
    ACCOUNT_ID INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    ACCOUNT_NAME VARCHAR(30) UNIQUE NOT NULL,
    BALANCE DECIMAL(15,2) DEFAULT 0
);
```

**TRANSACTIONS**
```sql
CREATE TABLE TRANSACTIONS (
    TRANSACTION_ID INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    ACCOUNT_ID INT NOT NULL,
    TRANSACTION_DATE DATE,
    TRANSACTION_TYPE VARCHAR(10),
    AMOUNT DECIMAL(15,2),
    FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS(ACCOUNT_ID)
);
```

### 2. Programas COBOL

#### `src/minibank.cob` (Programa Principal)
- **Líneas**: 350
- **Tipo**: Programa principal con EXEC SQL
- **Conexión**: Directa a DB2 usando embedded SQL
- **Divisiones**:
  1. **IDENTIFICATION**: Define PROGRAM-ID MINIBANK
  2. **ENVIRONMENT**: Define archivos CSV (INPUT TX-FILE, OUTPUT OUT-FILE)
  3. **DATA**: Declara variables y estructuras
  4. **SQL SECTION**: Declara cursores SQL
  5. **PROCEDURE**: Contiene la lógica

#### Variables principales:

| Variable | Tipo | Propósito |
|----------|------|-----------|
| `TX-PATH` | PIC X(256) | Ruta archivo transacciones |
| `OUT-PATH` | PIC X(256) | Ruta archivo salida |
| `WS-ACCOUNT` | PIC X(30) | Número de cuenta actual |
| `WS-TYPE` | PIC X(6) | Tipo transacción (CREDIT/DEBIT) |
| `WS-AMOUNT-SIGNED` | PIC S9(13)V9(2) | Cantidad con signo |
| `ACCOUNTS` | OCCURS 100 TIMES | Array de cuentas en memoria |

#### Procedimientos:

| Procedimiento | Función |
|---------------|---------|
| `MAIN` | Orquesta todo el flujo |
| `PARSE-LINE` | Separa CSV por comas |
| `INSERT-TRANSACTION` | Inserta en tabla TRANSACTIONS |
| `CREATE-ACCOUNT` | Crea nueva cuenta si no existe |
| `QUERY-BALANCES` | Consulta y reporta saldos |
| `CONNECT-DB2` | Establece conexión SQL |
| `DISCONNECT-DB2` | Cierra conexión SQL |
| `WRITE-HEADER` | Escribe encabezado CSV |

#### `src/minibank-db2.cob` (Programa Alternativo)
- **Líneas**: ~150
- **Tipo**: Programa con interfaz Python
- **Conexión**: Indirecta vía Python (usa CALL "SYSTEM")
- **Estado**: Alternativa no utilizada en el flujo principal

---

## Arquitectura Técnica

### Stack Tecnológico

```
┌─────────────────────────────────────────────┐
│         VS Code + Dev Container             │
├─────────────────────────────────────────────┤
│                                             │
│  ┌──────────────────────────────────────┐   │
│  │   Container: Debian Bookworm         │   │
│  │   - GnuCOBOL 3.x                     │   │
│  │   - Make, Git, Curl                  │   │
│  │   - Python 3 + ibm-db                │   │
│  └──────────────────────────────────────┘   │
│                                             │
│  ┌──────────────────────────────────────┐   │
│  │   Volumen: /workspaces/cobol-minibank│   │
│  │   - Código fuente                     │   │
│  │   - Datos (entrada/salida)            │   │
│  │   - Configuración                     │   │
│  └──────────────────────────────────────┘   │
└─────────────────────────────────────────────┘
           │
           │ red bridge (docker-compose)
           │
┌─────────────────────────────────────────────┐
│     Container: IBM DB2 Community             │
│     - Hostname: db2server                    │
│     - Puerto: 50000                         │
│     - BD: minibank                          │
└─────────────────────────────────────────────┘
```

### Flujo de compilación

```bash
make build
├── cobc (GnuCOBOL compiler)
│   ├── Input:  src/minibank.cob
│   ├── Input:  src/copybooks/*.cpy (includes)
│   ├── Flag:   -x (generar ejecutable)
│   ├── Flag:   -Wall (warnings)
│   ├── Flag:   -O2 (optimización)
│   └── Output: src/minibank (executable)
└── Resultado: Programa compilado listo para ejecutar
```

### Comunicación SQL

**Embedded SQL en COBOL:**
```cobol
EXEC SQL
    INSERT INTO TRANSACTIONS
        (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
    VALUES
        ((SELECT ACCOUNT_ID FROM ACCOUNTS
          WHERE ACCOUNT_NAME = :DB-ACCOUNT-NAME),
         :DB-TX-DATE,
         :DB-TX-TYPE,
         :DB-TX-AMOUNT)
END-EXEC.
```

Las variables con prefijo `:` son "host variables" - variables de COBOL usadas en SQL.

---

## Análisis de Archivos

### Estructura de directorios completa

```
cobol-minibank/
│
├── 📄 README.md                          ✅ MANTENER - Documentación principal
├── 📄 Makefile                           ✅ MANTENER - Instrucciones build
├── 📄 DOCUMENTACION.md                   ✅ NUEVO - Este documento
│
├── 📁 src/                               Código fuente
│   ├── 📄 minibank.cob                   ✅ MANTENER - Programa principal (USADO)
│   ├── ⚠️  minibank-db2                  ❌ ELIMINAR - Ejecutable compilado
│   ├── ⚠️  minibank-db2.cob              ❌ ELIMINAR - Programa alternativo no usado
│   └── 📁 copybooks/
│       └── ⚠️  record-layout.cpy         ❌ ELIMINAR - Nunca se incluye
│
├── 📁 data/                              Datos
│   ├── 📄 transactions.csv               ✅ MANTENER - Entrada de ejemplo
│   └── 📄 balances.csv                   ✅ MANTENER - Salida generada
│
├── 📁 .devcontainer/                     Configuración
│   ├── 📄 devcontainer.json              ✅ MANTENER - Config VS Code
│   ├── 📄 Dockerfile                     ✅ MANTENER - Imagen contenedor
│   ├── 📄 compose.yml                    ✅ MANTENER - Docker Compose
│   ├── 📄 post-create.sh                 ✅ MANTENER - Script inicialización
│   ├── 📄 .db2.env                       ✅ MANTENER - Variables DB2
│   ├── 📄 init-db2-tables.sh             ✅ MANTENER - Crear tablas
│   ├── 📄 init-db2.sql                   ✅ MANTENER - SQL inicial
│   ├── 📄 db2-interface.py               ⚠️  REVISAR - Usado por minibank-db2 (obsoleto)
│   ├── 📄 connect-db2.py                 ⚠️  REVISAR - Helper DB2
│   ├── 📄 connect-db2.sh                 ⚠️  REVISAR - Helper shell
│   ├── 📄 init-db2.py                    ⚠️  REVISAR - Helper Python
│   ├── 📄 DB2-SETUP.md                   ⚠️  REVISAR - Documentación DB2
│   └── 📄 init-tables.sh                 ⚠️  REVISAR - Helper shell
│
├── 📁 .vscode/                           Configuración IDE
│   ├── 📄 tasks.json                     ✅ MANTENER - Tasks VS Code
│   └── 📄 launch.json                    ✅ MANTENER - Debug config
│
├── 📁 .git/                              ✅ MANTENER - Control de versiones
├── 📄 .gitignore                         ✅ MANTENER - Exclusiones git
│
└── 📁 build/                             ❌ ELIMINAR - Directorio VACÍO
```

---

## Recomendaciones de Limpieza

### 🔴 ELIMINAR (Archivos no utilizados)

#### 1. **`src/minibank-db2`** (Ejecutable)
- **Razón**: Se regenera automáticamente con `make build-db2`
- **Acción**: Usar `make clean`
- **Comando**:
  ```bash
  rm -f src/minibank-db2
  ```

#### 2. **`src/minibank-db2.cob`** (Programa alternativo)
- **Razón**:
  - Nunca se compila en el flujo normal (tasks.json solo usa `build` y `run`)
  - Usa interfaz Python que agrega complejidad innecesaria
  - El programa principal `minibank.cob` ya hace todo lo necesario con embedded SQL
- **Acción**: Eliminar archivo
- **Comando**:
  ```bash
  rm src/minibank-db2.cob
  ```
- **Además eliminar del Makefile**: Targets `build-db2` y `run-db2`

#### 3. **`src/copybooks/record-layout.cpy`** (Copybook no utilizado)
- **Razón**:
  - Archivo definido pero nunca incluido en el programa COBOL
  - Solo contiene estructura de referencia
  - El programa usa UNSTRING para parsing, no esta estructura
- **Acción**: Eliminar archivo
- **Comando**:
  ```bash
  rm -rf src/copybooks
  ```

#### 4. **`build/`** (Directorio vacío)
- **Razón**: No se utiliza en la compilación (build ocurre en `src/`)
- **Acción**: Eliminar directorio
- **Comando**:
  ```bash
  rmdir build
  ```

### 🟡 REVISAR/LIMPIAR (Archivos heredados de DB2)

Si el proyecto decide usar **DB2 con embedded SQL** (RECOMENDADO):

#### Archivos que se pueden eliminar:
- `.devcontainer/db2-interface.py` - Solo usado por `minibank-db2.cob`
- `.devcontainer/connect-db2.py` - Helper Python para la versión alternativa
- `.devcontainer/init-db2.py` - Helper Python
- `.devcontainer/connect-db2.sh` - Helper shell
- `.devcontainer/init-tables.sh` - Helper shell redundante
- `.devcontainer/DB2-SETUP.md` - Documentación de setup (reemplazar con versión simplificada)

#### Archivos a MANTENER:
- `.devcontainer/init-db2-tables.sh` - Crea estructuras DB2 necesarias
- `.devcontainer/init-db2.sql` - SQL de inicialización
- `.devcontainer/.db2.env` - Variables de entorno DB2

### 🟢 MANTENER (Crítico para funcionamiento)

- `src/minibank.cob` - ✅ Programa principal único
- `data/transactions.csv` - ✅ Datos de ejemplo
- `data/balances.csv` - ✅ Archivo de salida
- `.devcontainer/` (todos los archivos necesarios)
- `Makefile` (versión limpia sin targets obsoletos)
- `.vscode/tasks.json` (con solo targets válidos)
- `README.md` y `DOCUMENTACION.md`

---

## Resumen de archivos a eliminar

### Comando de limpieza completo:

```bash
# 1. Eliminar ejecutables compilados
make clean

# 2. Eliminar programa DB2 alternativo
rm src/minibank-db2.cob

# 3. Eliminar copybook no utilizado
rm -rf src/copybooks

# 4. Eliminar directorio build vacío
rmdir build

# 5. Opcionalmente, eliminar helpers Python/Shell (si se usa embedded SQL)
rm .devcontainer/db2-interface.py
rm .devcontainer/connect-db2.py
rm .devcontainer/init-db2.py
rm .devcontainer/connect-db2.sh
rm .devcontainer/init-tables.sh
```

### Archivos a modificar:

1. **`Makefile`** - Eliminar targets `build-db2` y `run-db2`
2. **`.vscode/tasks.json`** - Mantener solo targets `build` y `run` (ya están correctos)

---

## Métricas del Proyecto

| Métrica | Valor |
|---------|-------|
| Líneas de código COBOL | ~350 (minibank.cob) |
| Divisiones COBOL | 5 (Identification, Environment, Data, SQL, Procedure) |
| Procedimientos | 7 principales + helpers |
| Variables principales | 15+ |
| Tablas DB2 | 2 (ACCOUNTS, TRANSACTIONS) |
| Archivos entrada | 1 (transactions.csv) |
| Archivos salida | 1 (balances.csv) |

---

## Conclusión

El proyecto está bien estructurado pero tiene **artefactos de desarrollo** que pueden eliminarse:
- ❌ **3 archivos innecesarios** (minibank-db2.cob, record-layout.cpy, build/)
- ❌ **1 programa alternativo** que añade complejidad
- ❌ **~5-6 helpers Python/Shell** heredados

Después de la limpieza, el proyecto será más **simple, mantenible y claro** con el programa COBOL principal usando embedded SQL contra DB2.
