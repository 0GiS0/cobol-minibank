# 🏦 COBOL MiniBank - Sistema Bancario Simplificado

**Español** | **[English](README_EN.md)**

## 🤔 ¿Qué es este proyecto?

Este es un **repositorio educativo progresivo** para aprender **COBOL** (Common Business-Oriented Language) desde cero hasta integración con bases de datos empresariales. 

Incluye **3 programas COBOL** de complejidad creciente:
1. 📄 **MiniBank Básico** - Procesamiento de archivos CSV
2. 🗄️ **MiniBank DB2** - Integración con base de datos DB2
3. 🎮 **MiniBank Menu** - Sistema interactivo con menús y consultas

Perfecto para **principiantes que nunca han visto COBOL** y quieren entender cómo funciona en entornos reales.

---

## 🎯 Los 3 Programas del Repositorio

### 1️⃣ MiniBank Básico (`minibank.cob`)

**Nivel:** Principiante  
**Propósito:** Aprender fundamentos de COBOL con archivos

**¿Qué hace?**
1. 📥 Lee transacciones desde `data/transactions.csv`
2. 🔄 Procesa depósitos (CREDIT) y retiros (DEBIT)
3. 🧮 Calcula saldos por cuenta en memoria
4. 📊 Genera reporte en `data/balances.csv`

**Ejecutar:**
```bash
make run
```

**Ejemplo de entrada** (`transactions.csv`):
```csv
2025-01-10,ACC-001,CREDIT,1000.00
2025-01-12,ACC-001,DEBIT,150.25
2025-01-15,ACC-002,CREDIT,500.00
```

**Salida** (`balances.csv`):
```csv
account,balance
ACC-001,849.75
ACC-002,500.00
```

**Conceptos COBOL que aprenderás:**
- ✅ Estructura de 4 divisiones (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- ✅ Lectura secuencial de archivos con `READ`
- ✅ Arrays en COBOL con `OCCURS`
- ✅ Parsing de CSV con `UNSTRING`
- ✅ Aritmética decimal precisa (`PIC S9(13)V9(2)`)
- ✅ Formateo de salida con `STRING`

---

### 2️⃣ MiniBank DB2 (`minibank-db2.cob`)

**Nivel:** Intermedio  
**Propósito:** Integración COBOL + Base de datos DB2

**¿Qué hace?**
1. 📥 Lee transacciones desde CSV
2. 🗄️ **Inserta cada transacción en DB2** (vía Python wrapper)
3. 📊 **Consulta saldos desde DB2** en lugar de calcularlos en memoria
4. 📤 Genera reporte con datos de la base de datos

**Ejecutar:**
```bash
make run-db2
```

**Arquitectura:**
```
COBOL Program ──> Python Script ──> DB2 Database
   (minibank-db2.cob)  (db2-interface.py)  (Tables: ACCOUNTS, TRANSACTIONS)
```

**¿Por qué usa Python como wrapper?**
- COBOL puede llamar a DB2 directamente con `EXEC SQL`, PERO requiere precompilador DB2
- Este repositorio usa **Python + ibm_db** como puente para evitar complejidad de setup
- En entornos reales de producción se usa `EXEC SQL` embebido (veremos opinión más abajo)

**Conceptos nuevos:**
- ✅ Llamadas a comandos externos con `CALL "SYSTEM"`
- ✅ Paso de parámetros vía línea de comandos
- ✅ Integración COBOL con otros lenguajes
- ✅ Manejo de archivos temporales (`/tmp`)

---

### 3️⃣ MiniBank Menu (`minibank-menu.cob`)

**Nivel:** Avanzado  
**Propósito:** Sistema interactivo completo con menú

**¿Qué hace?**
1. 🎮 Presenta menú interactivo al usuario
2. 📋 Opción 1: Ver todas las cuentas con sus saldos
3. 🔍 Opción 2: Ver detalles de una cuenta específica + transacciones
4. 🔄 Mantiene el programa corriendo hasta que el usuario elige salir

**Ejecutar:**
```bash
make run-menu
```

**Ejemplo de interacción:**
```
==================================================
    💰 BIENVENIDO A MINIBANK 💰
==================================================

⏳ Cargando cuentas desde DB2...
OK: 5 cuentas cargadas.

==================================================
              📋 MINIBANK - MENU 📋
==================================================

  1️⃣  Ver todas las cuentas
  2️⃣  Ver detalles de una cuenta
  3️⃣  Salir del programa

Selecciona una opcion: _
```

**Conceptos nuevos:**
- ✅ Input interactivo del usuario con `ACCEPT`
- ✅ Loops con `PERFORM UNTIL`
- ✅ Validación de inputs con `TEST-NUMVAL`
- ✅ Estructuras de menú y navegación
- ✅ Carga dinámica de datos desde DB2

---

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

---

## 🛠️ Configuración y Ejecución

### 📋 Requisitos
- VS Code con extensión **Dev Containers**
- Docker Desktop activo
- 4GB RAM mínimo (para el contenedor DB2)

### 🚀 Inicio rápido (3 pasos):
1. 📂 Abre la carpeta en VS Code
2. 🔄 Cuando aparezca el popup, selecciona **"Reopen in Container"**
3. ⏳ Espera a que termine el `postCreate` (instala dependencias y carga datos en DB2)

### 🔨 Compilar y ejecutar los programas:

#### Opción 1: Usar VS Code Tasks
- **🏗️ Compilar**: `Terminal > Run Task > COBOL: build`
- **▶️ Ejecutar**: `Terminal > Run Task > COBOL: run`

#### Opción 2: Usar Makefile directamente
```bash
# Programa básico (CSV)
make run

# Programa con DB2
make run-db2

# Programa interactivo con menú
make run-menu

# Limpiar binarios compilados
make clean
```

**Resultado:** Los archivos compilados se generan en la raíz del proyecto (`minibank`, `minibank-db2`, `minibank-menu`).

---

## 📂 Estructura del Proyecto

```
cobol-minibank/
├── src/
│   ├── minibank.cob          # 📝 Programa 1: Básico con CSV
│   ├── minibank-db2.cob      # 🗄️ Programa 2: Integración con DB2
│   ├── minibank-menu.cob     # 🎮 Programa 3: Menú interactivo
│   └── copybooks/            # 📚 Definiciones reutilizables
│       └── record-layout.cpy # Layout de registros CSV
├── data/
│   ├── transactions.csv      # 📥 Transacciones de entrada
│   └── balances.csv          # 📤 Saldos de salida (generado)
├── .devcontainer/
│   ├── devcontainer.json     # ⚙️ Configuración del Dev Container
│   ├── Dockerfile            # 🐳 Imagen con GnuCOBOL + Python
│   ├── compose.yml           # 🐳 Docker Compose (app + db2)
│   ├── init-db2.sql          # 📊 Esquema de base de datos
│   ├── db2-interface.py      # 🔗 Wrapper Python para DB2
│   ├── get-accounts.py       # 📋 Consultar cuentas desde COBOL
│   ├── get-transactions.py   # 📋 Consultar transacciones desde COBOL
│   ├── load-sample-data.py   # 🎲 Cargar datos de ejemplo
│   └── verify-db2.sh         # ✅ Script de verificación de DB2
├── Makefile                  # 🔧 Build automation
├── .vscode/
│   └── tasks.json            # ⚡ VS Code tasks
└── README.md                 # 📖 Esta documentación
```

---

## 🎓 Guía de Aprendizaje para Principiantes

### 🔤 Diferencia entre archivos `.cob` y `.cbl`

**Respuesta corta:** Son lo mismo, solo extensiones diferentes.

**Extensiones comunes en COBOL:**
- `.cob` / `.COB` - Más usado en entornos Linux/GnuCOBOL
- `.cbl` / `.CBL` - Más usado en mainframes IBM
- `.cpy` / `.CPY` - Para copybooks (código reutilizable)

Este repositorio usa `.cob` porque usamos **GnuCOBOL** (compilador open source).

---

### 📖 Conceptos COBOL que aprenderás (ordenados por programa)

#### En `minibank.cob` (Básico):

**1. Las 4 Divisiones de COBOL**
```cobol
IDENTIFICATION DIVISION.    ← Identifica el programa
PROGRAM-ID. MINIBANK.

ENVIRONMENT DIVISION.        ← Define archivos externos
FILE-CONTROL.
    SELECT TX-FILE ASSIGN TO "data/transactions.csv".

DATA DIVISION.               ← Declara variables
WORKING-STORAGE SECTION.
77  WS-AMOUNT  PIC S9(13)V9(2).

PROCEDURE DIVISION.          ← Lógica del programa
MAIN.
    DISPLAY "Hola COBOL".
    GOBACK.
```

**2. Tipos de datos con `PICTURE` (PIC)**
```cobol
77  WS-ACCOUNT     PIC X(30).        ← Texto de 30 caracteres
77  WS-AMOUNT      PIC 9(10)V9(2).   ← Número: 10 enteros, 2 decimales
77  WS-COUNTER     PIC 9(4) COMP.    ← Entero binario (eficiente)
77  WS-SIGNED-AMT  PIC S9(13)V9(2).  ← Con signo (+/-)
```

**3. Arrays (Tablas) con `OCCURS`**
```cobol
01  ACCOUNTS.
    05 ACCT-ENTRY OCCURS 100 TIMES.
       10 ACCT-NAME    PIC X(30).
       10 ACCT-BAL     PIC S9(13)V9(2).

* Acceder al elemento 5:
MOVE "Juan" TO ACCT-NAME(5).
ADD 100 TO ACCT-BAL(5).
```

**4. Lectura de archivos**
```cobol
OPEN INPUT TX-FILE.
PERFORM UNTIL EOF = "Y"
    READ TX-FILE
        AT END MOVE "Y" TO EOF
        NOT AT END PERFORM PROCESS-LINE
    END-READ
END-PERFORM.
CLOSE TX-FILE.
```

**5. Parsing de CSV con `UNSTRING`**
```cobol
* Input: "2025-01-10,ACC-001,CREDIT,1000"
UNSTRING WS-LINE DELIMITED BY ","
    INTO WS-DATE
         WS-ACCOUNT
         WS-TYPE
         WS-AMOUNT-STR
END-UNSTRING.
```

**6. Aritmética decimal precisa**
```cobol
* COBOL es perfecto para dinero (no usa float impreciso)
ADD WS-AMOUNT TO ACCT-BAL(I).
SUBTRACT 100 FROM WS-BALANCE.
MULTIPLY 1.05 BY WS-AMOUNT.    ← Aplicar 5% interés
DIVIDE WS-TOTAL BY 12 GIVING WS-MONTHLY.
```

**7. Formateo de salida con `STRING`**
```cobol
STRING
    ACCT-NAME(I) DELIMITED BY SPACES
    "," DELIMITED BY SIZE
    FORMATTED-BAL DELIMITED BY SIZE
    INTO OUT-LINE
END-STRING.
WRITE OUT-LINE.
```

---

#### En `minibank-db2.cob` (Intermedio):

**8. Llamadas a comandos externos**
```cobol
77  CMD-LINE  PIC X(512).
77  RC        PIC S9(9) COMP.

MOVE "python3 db2-interface.py connect" TO CMD-LINE.
CALL "SYSTEM" USING CMD-LINE RETURNING RC.

IF RC = 0
    DISPLAY "✅ Conexión exitosa"
ELSE
    DISPLAY "❌ Error al conectar"
END-IF.
```

**9. Construcción dinámica de comandos**
```cobol
MOVE FUNCTION CONCATENATE(
    "python3 db2-interface.py insert ",
    WS-ACCOUNT, " ",
    WS-DATE, " ",
    WS-TYPE, " ",
    WS-AMOUNT-STR
) TO CMD-LINE.

CALL "SYSTEM" USING CMD-LINE.
```

---

#### En `minibank-menu.cob` (Avanzado):

**10. Input interactivo del usuario**
```cobol
DISPLAY "Selecciona una opción: " WITH NO ADVANCING.
ACCEPT WS-INPUT.

* Validar si es número
IF FUNCTION TEST-NUMVAL(WS-INPUT) = 0
    MOVE FUNCTION NUMVAL(WS-INPUT) TO WS-OPTION
ELSE
    DISPLAY "❌ Debes ingresar un número"
END-IF.
```

**11. Loops con menú**
```cobol
PERFORM UNTIL WS-CONTINUE = "N"
    PERFORM SHOW-MENU
    PERFORM GET-USER-CHOICE
    
    EVALUATE WS-CHOICE
        WHEN 1 PERFORM OPTION-1
        WHEN 2 PERFORM OPTION-2
        WHEN 3 MOVE "N" TO WS-CONTINUE
    END-EVALUATE
END-PERFORM.
```

**12. Estructuras de control con `EVALUATE`**
```cobol
EVALUATE WS-TRANSACTION-TYPE
    WHEN "CREDIT"
        ADD WS-AMOUNT TO WS-BALANCE
        DISPLAY "✅ Depósito procesado"
    WHEN "DEBIT"
        SUBTRACT WS-AMOUNT FROM WS-BALANCE
        DISPLAY "✅ Retiro procesado"
    WHEN OTHER
        DISPLAY "❌ Tipo de transacción inválido"
END-EVALUATE.
```

---

## 🔄 Flujo de Datos en Cada Programa

### Programa 1: `minibank.cob`
```
📄 transactions.csv
    ↓ (READ)
🧮 COBOL procesa en memoria
    ↓ (Calcula saldos)
📊 balances.csv
    ↓ (WRITE)
✅ Archivo generado
```

### Programa 2: `minibank-db2.cob`
```
📄 transactions.csv
    ↓ (READ)
🧮 COBOL parsea cada línea
    ↓ (CALL SYSTEM)
🐍 Python (db2-interface.py)
    ↓ (INSERT)
🗄️ DB2 Database
    ↓ (SELECT balances)
🐍 Python genera CSV temporal
    ↓ (READ)
🧮 COBOL lee y formatea
    ↓ (WRITE)
📊 balances.csv
```

### Programa 3: `minibank-menu.cob`
```
🎮 Usuario interactúa con menú
    ↓ (ACCEPT)
🧮 COBOL procesa opción
    ↓ (CALL SYSTEM)
🐍 get-accounts.py o get-transactions.py
    ↓ (SELECT)
🗄️ DB2 Database
    ↓ (Genera /tmp/*.tmp)
🧮 COBOL lee y muestra
    ↓ (DISPLAY)
👤 Usuario ve resultados
    ↓ (Loop)
🔄 Vuelve al menú
```

---

## 🗄️ Integración con DB2

### 🏗️ Esquema de Base de Datos

El repositorio incluye un contenedor **DB2 Community Edition** con las siguientes tablas:

```sql
-- Tabla de cuentas
CREATE TABLE ACCOUNTS (
    ACCOUNT_ID      INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ACCOUNT_NAME    VARCHAR(100) NOT NULL,
    BALANCE         DECIMAL(15,2) DEFAULT 0,
    CREATED_AT      TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UPDATED_AT      TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Tabla de transacciones
CREATE TABLE TRANSACTIONS (
    TRANSACTION_ID   INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ACCOUNT_ID       INTEGER NOT NULL,
    TRANSACTION_DATE DATE NOT NULL,
    TRANSACTION_TYPE VARCHAR(10) NOT NULL,  -- 'CREDIT' o 'DEBIT'
    AMOUNT           DECIMAL(15,2) NOT NULL,
    CREATED_AT       TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS(ACCOUNT_ID)
);
```

### ✅ Verificación de DB2

**Automática durante `postCreate`:**
- Se crea la base de datos `minibank`
- Se crean las tablas `ACCOUNTS` y `TRANSACTIONS`
- Se cargan 10 transacciones de ejemplo

**Manual en cualquier momento:**
```bash
.devcontainer/verify-db2.sh
```

Este script muestra:
- ✅ Estado de conexión a DB2
- 📊 Cantidad de cuentas y transacciones
- 💰 Detalles de saldos por cuenta
- 📋 Últimas transacciones

### 🔌 Conectarse directamente a DB2 (opcional)

```bash
# Opción 1: Script automático
.devcontainer/connect-db2.sh

# Opción 2: Cliente DB2 manual
db2 CONNECT TO minibank USER db2inst1 USING password
db2 "SELECT * FROM ACCOUNTS"
db2 "SELECT * FROM TRANSACTIONS ORDER BY TRANSACTION_DATE DESC"
db2 QUIT
```

---

## 💭 Opinión: ¿Este repositorio es útil y realista?

### ✅ **Aspectos Útiles y Educativos**

**1. Progresión pedagógica excelente**
- ✅ Empieza simple (CSV) y añade complejidad gradualmente
- ✅ Cada programa introduce conceptos nuevos sin abrumar
- ✅ Código bien comentado en español (accesible para hispanohablantes)

**2. Conceptos COBOL fundamentales bien cubiertos**
- ✅ Estructura de 4 divisiones
- ✅ Manejo de archivos secuenciales (común en COBOL)
- ✅ Aritmética decimal (crítico en finanzas)
- ✅ Arrays y estructuras de datos
- ✅ Input/output interactivo

**3. Setup moderno y accesible**
- ✅ Dev Containers = cero fricciones de instalación
- ✅ GnuCOBOL = gratis y open source (vs. mainframe caro)
- ✅ Docker Compose = fácil de compartir

### ⚠️ **Limitaciones vs. Entornos Reales**

**1. Python como wrapper NO es común en producción**

**En este repositorio:**
```cobol
CALL "SYSTEM" USING "python3 db2-interface.py insert ...".
```

**En el mundo real (mainframe):**
```cobol
EXEC SQL
    INSERT INTO ACCOUNTS (ACCOUNT_NAME, BALANCE)
    VALUES (:WS-ACCOUNT-NAME, :WS-BALANCE)
END-EXEC.
```

**¿Por qué este repo usa Python?**
- **Razón práctica:** Evitar complejidad del precompilador DB2 (requiere configuración extra)
- **Razón educativa:** Mostrar que COBOL puede integrarse con otros lenguajes
- **Limitación:** En empresas reales, el 99% usa `EXEC SQL` embebido directamente

**Recomendación:** Si este repo evoluciona, agregar un 4º ejemplo con `EXEC SQL` nativo sería ideal.

---

**2. DB2 en Docker vs. DB2 en Mainframe**

| Aspecto | Este Repositorio | Producción Real |
|---------|------------------|-----------------|
| **Base de datos** | DB2 Community (Linux) | DB2 z/OS (Mainframe) o DB2 LUW |
| **Compilador** | GnuCOBOL | IBM Enterprise COBOL |
| **Entorno** | Dev Container local | Mainframe con JCL |
| **Transacciones** | Batch (archivos) | CICS/IMS (online) + Batch |
| **Acceso a DB** | Python wrapper | EXEC SQL embebido |

**¿La gente usa DB2 en Docker?**
- **En desarrollo:** Sí, cada vez más (entornos modernos de CI/CD)
- **En producción:** No, la mayoría usa DB2 z/OS en mainframes o DB2 LUW en servidores

---

**3. Falta de JCL (Job Control Language)**

En entornos de mainframe, COBOL se ejecuta vía **JCL**, no con `make run`:

**Ejemplo de JCL real:**
```jcl
//MINIBK01 JOB (ACCT),'COBOL MINIBANK',CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=MINIBANK
//STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
//TXFILE   DD DSN=PROD.TRANSACTIONS.CSV,DISP=SHR
//BALFILE  DD DSN=PROD.BALANCES.OUT,DISP=(NEW,CATLG,DELETE)
//SYSOUT   DD SYSOUT=*
```

Este repo lo omite porque JCL es específico de mainframe (no corre en Linux).

---

### 🎯 **Recomendaciones para Hacerlo Más Realista**

#### Mejoras que acercarían este repo al mundo real:

**1. Agregar ejemplo con EXEC SQL nativo**
```cobol
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       EXEC SQL
           CONNECT TO minibank USER db2inst1 USING password
       END-EXEC.
       
       EXEC SQL
           INSERT INTO TRANSACTIONS (ACCOUNT_ID, TX_DATE, TX_TYPE, AMOUNT)
           VALUES (:WS-ACCOUNT-ID, :WS-DATE, :WS-TYPE, :WS-AMOUNT)
       END-EXEC.
       
       IF SQLCODE NOT = 0
           DISPLAY "Error SQL: " SQLCODE
       END-IF.
```

Esto requeriría:
- Instalar DB2 precompiler (`db2 PREP PROGRAM(minibank-sql.cob)`)
- Configurar variables de entorno DB2
- Pero mostraría la integración REAL usada en producción

---

**2. Simular procesamiento batch con múltiples pasos**

Crear un script que simule un "job" con varios programas:
```bash
#!/bin/bash
# simulate-batch-job.sh

echo "Step 1: Validar archivo de transacciones"
./validate-tx

echo "Step 2: Procesar transacciones"
./minibank-db2

echo "Step 3: Generar reportes"
./generate-reports

echo "Step 4: Enviar notificaciones"
./send-notifications
```

Esto refleja cómo funcionan los sistemas batch COBOL reales (cadenas de programas).

---

**3. Agregar CICS o similar para transacciones online**

Actualmente todo es batch (archivos). En la realidad, bancos usan:
- **CICS** (Customer Information Control System) para transacciones en tiempo real
- **IMS** (Information Management System) para bases de datos jerárquicas

Podrías simular CICS con un servidor HTTP simple que llame a COBOL:
```python
# cics-simulator.py
from flask import Flask, request
import subprocess

@app.route('/transfer', methods=['POST'])
def transfer():
    # Llamar programa COBOL
    result = subprocess.run(['./minibank-transfer', 
                            request.json['from_account'],
                            request.json['to_account'],
                            request.json['amount']])
    return {'status': 'OK' if result.returncode == 0 else 'ERROR'}
```

---

**4. Usar DB2 LUW en lugar de Python wrapper**

Configurar el precompilador DB2 para poder usar `EXEC SQL`:
```dockerfile
# En Dockerfile
RUN apt-get install -y ibm-db2-client
ENV DB2_HOME=/opt/ibm/db2
```

Y compilar con:
```bash
db2 PREP minibank-sql.cob BINDFILE
db2 BIND minibank-sql.bnd
cobc -x -I $DB2_HOME/include minibank-sql.cob -L $DB2_HOME/lib -ldb2
```

---

**5. Agregar documentación de JCL (aunque no funcione en Linux)**

Incluir ejemplos comentados de cómo se ejecutaría en mainframe:
```
📁 examples/
   └── jcl-samples/
       ├── minibank.jcl          # Job para ejecutar minibank
       ├── monthly-report.jcl    # Job mensual
       └── README.md             # Explicación de JCL
```

Esto ayudaría a estudiantes a entender cómo es el proceso real.

---

### 📊 **Conclusión Final**

| Criterio | Calificación | Comentario |
|----------|--------------|------------|
| **Valor educativo** | ⭐⭐⭐⭐⭐ (5/5) | Excelente para aprender COBOL desde cero |
| **Realismo técnico** | ⭐⭐⭐☆☆ (3/5) | Bueno pero el wrapper Python no es común |
| **Setup moderno** | ⭐⭐⭐⭐⭐ (5/5) | Dev Containers + Docker es perfecto |
| **Progresión pedagógica** | ⭐⭐⭐⭐⭐ (5/5) | 3 programas con complejidad creciente |
| **Similitud con mainframe** | ⭐⭐☆☆☆ (2/5) | Falta JCL, EXEC SQL, CICS |

**Veredicto:**
- ✅ **Perfecto para aprender COBOL** sin tener acceso a mainframe
- ⚠️ **No reemplaza experiencia real** en entornos mainframe/CICS
- 🎯 **Con las mejoras sugeridas** podría ser 90% realista

---

## 🔧 Modificaciones y Experimentos

Ideas para practicar y extender el proyecto:

### Nivel Principiante:
- 📝 Agregar validación de saldo mínimo ($0)
- 🔤 Soportar diferentes formatos de fecha
- 📊 Generar reporte con totales por tipo de transacción

### Nivel Intermedio:
- 🏦 Implementar transferencias entre cuentas
- 📅 Filtrar transacciones por rango de fechas
- 💰 Calcular intereses mensuales

### Nivel Avanzado:
- 🔐 Agregar autenticación de usuarios
- 🗄️ Migrar de Python wrapper a EXEC SQL nativo
- 📈 Implementar API REST que llame a COBOL (CICS-like)
- 🎭 Crear programa de reconciliación (matching de registros)

---

## 🧪 Testing y Debugging

### Ejecutar tests (si existieran):
```bash
# TODO: Agregar framework de testing COBOL
# Opciones: COBOL Check, Unit Test Framework
```

### Debugging interactivo:
```bash
# GDB funciona con binarios COBOL
gdb ./minibank
(gdb) break MAIN
(gdb) run
(gdb) print WS-ACCOUNT
```

### Ver archivos intermedios:
```bash
# Revisar datos procesados
cat data/balances.csv

# Ver logs de DB2
docker logs cobol-minibank-db-1

# Archivos temporales del menú
cat /tmp/minibank-accounts.tmp
cat /tmp/minibank-transactions.tmp
```

---

## 📚 Recursos Adicionales

### Documentación de COBOL:
- 📖 [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/doc/gnucobol.html)
- 📘 [COBOL Programming Course (OpenMainframe)](https://www.openmainframeproject.org/projects/cobol-programming-course)
- 🎓 [IBM Enterprise COBOL for z/OS](https://www.ibm.com/docs/en/cobol-zos)

### DB2 y SQL:
- 🗄️ [IBM DB2 Documentation](https://www.ibm.com/docs/en/db2)
- 🔗 [Python ibm_db Library](https://github.com/ibmdb/python-ibmdb)

### Mainframe y entornos legacy:
- 🖥️ [IBM z/OS Basics](https://www.ibm.com/docs/en/zos-basic-skills)
- 📋 [JCL Tutorial](https://www.tutorialspoint.com/jcl/index.htm)
- 🔄 [CICS Transaction Server](https://www.ibm.com/docs/en/cics-ts)

---

## 🐛 Troubleshooting

### "Command not found: cobc"
**Solución:** Asegúrate de estar dentro del Dev Container (Reopen in Container).

### "DB2 connection refused"
**Diagnóstico:**
```bash
# Verificar que DB2 está corriendo
docker ps | grep db2

# Ver logs de DB2
docker logs cobol-minibank-db-1

# Reiniciar contenedor si es necesario
docker restart cobol-minibank-db-1
```

### "No such file or directory: data/transactions.csv"
**Solución:**
```bash
# Crear directorio y archivo de ejemplo
mkdir -p data
cat > data/transactions.csv << 'EOF'
2025-01-10,ACC-001,CREDIT,1000.00
2025-01-12,ACC-001,DEBIT,150.25
EOF
```

### Programa compilado no se ejecuta
**Diagnóstico:**
```bash
# Ver permisos
ls -la minibank*

# Dar permisos de ejecución
chmod +x minibank minibank-db2 minibank-menu

# Verificar que se compiló correctamente
file minibank
# Debería mostrar: ELF 64-bit LSB executable
```

---

## 🤝 Contribuciones

Este es un proyecto educativo. Ideas para contribuir:

1. 📝 Agregar más ejemplos de programas COBOL
2. 🧪 Implementar tests unitarios con COBOL Check
3. 📖 Traducir documentación a otros idiomas
4. 🔧 Agregar ejemplo con EXEC SQL nativo
5. 🎮 Crear interfaz web que llame a COBOL (REST API)
6. 📊 Agregar reportes en PDF/HTML

---

## 📄 Licencia

Este proyecto es de código abierto y está disponible bajo una licencia permisiva para fines educativos.

---

## 👤 Autor

Creado con 💙 para la comunidad de desarrolladores que quieren aprender COBOL moderno.

**¿Preguntas o sugerencias?** Abre un issue en el repositorio.

---

## 🎯 Próximos Pasos Sugeridos

Si estás aprendiendo COBOL con este repositorio:

1. ✅ **Completa el programa básico** (`minibank.cob`)
   - Entiende cada división
   - Modifica el CSV de entrada
   - Agrega una validación simple

2. ✅ **Explora la integración DB2** (`minibank-db2.cob`)
   - Ejecuta consultas SQL manualmente
   - Observa cómo COBOL llama a Python
   - Agrega una nueva tabla

3. ✅ **Usa el programa interactivo** (`minibank-menu.cob`)
   - Navega por los menús
   - Estudia el manejo de inputs
   - Agrega una nueva opción al menú

4. ✅ **Crea tu propio programa COBOL**
   - Implementa una calculadora simple
   - Procesa un archivo de productos/inventario
   - Integra con una API externa (vía Python wrapper)

5. 🚀 **Siguiente nivel: Aprende sobre mainframes**
   - Familiarízate con JCL
   - Estudia CICS/IMS
   - Busca cursos de IBM z/OS

---

**¡Bienvenido al mundo de COBOL! 🏦💻**
