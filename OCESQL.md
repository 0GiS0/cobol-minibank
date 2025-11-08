# Open Cobol ESQL (ocesql) - Guía de Uso

## ¿Qué es ocesql?

**Open Cobol ESQL** es un precompilador que convierte código COBOL con sentencias SQL embebidas (`EXEC SQL...END-EXEC`) en COBOL puro que puede ser compilado por GNU COBOL.

### Flujo de compilación con ocesql:

```
minibank-sql.cbl (EXEC SQL)
    ↓ ocesql (precompilador)
minibank-sql-processed.cbl (COBOL puro con CALL a funciones)
    ↓ cobc (compilador)
minibank-sql (ejecutable)
```

---

## Instalación

La instalación de ocesql está automatizada en el Dockerfile del dev container. Incluye:

1. **Dependencias necesarias**:
   - `libpq-dev` - Headers de PostgreSQL
   - `autoconf`, `automake`, `libtool`, `m4` - Autotools
   - `bison`, `flex` - Generadores de parser

2. **Compilación desde fuente**:
   - Descarga Open Cobol ESQL v1.4 desde GitHub
   - Compila con `./autogen.sh`, `./configure`, `make`
   - Instala en `/usr/local`

3. **Verificación**:
   ```bash
   ocesql --version
   # Open Cobol ESQL (Ocesql)
   # Version 1.4.0
   ```

---

## Estructura de un programa COBOL con EXEC SQL

### Elementos obligatorios:

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MINIBANK-SQL.

DATA DIVISION.
WORKING-STORAGE SECTION.

* Variables SQL deben estar en DECLARE SECTION
EXEC SQL BEGIN DECLARE SECTION END-EXEC.
    01  ACCT-ID        PIC X(30).
    01  ACCT-NAME      PIC X(50).
    01  ACCT-BALANCE   PIC S9(13)V9(2) COMP-3.
EXEC SQL END DECLARE SECTION END-EXEC.

* Incluir archivo de control de SQL
EXEC SQL INCLUDE SQLCA END-EXEC.

PROCEDURE DIVISION.
    PERFORM AA-CONNECT.
    STOP RUN.

AA-CONNECT.
    EXEC SQL
        CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
    END-EXEC.
    IF SQLCODE = 0
        DISPLAY "Conexión exitosa"
    END-IF.
```

### Reglas importantes:

1. **DECLARE SECTION**: Todas las variables host (que se usan en SQL) deben estar aquí
   - NO usar estructuras anidadas (sin 05 level items dentro)
   - Solo usar `PIC X`, `PIC 9`, `PIC S9` y `COMP-3`

2. **SQLCA**: Obligatorio incluir `EXEC SQL INCLUDE SQLCA END-EXEC`
   - Proporciona `SQLCODE` para verificar errores

3. **Bind variables**: Usar `:nombre-variable` para referenciar variables COBOL en SQL

---

## Uso de ocesql

### Precompilar un archivo:

```bash
ocesql --inc=/usr/local/share/open-cobol-esql/copy \
       input.cbl \
       output.cbl
```

**Opciones**:
- `--inc=DIR`: Directorio de includes (obligatorio para encontrar `sqlca.cbl`)
- `input.cbl`: Archivo con `EXEC SQL`
- `output.cbl`: Archivo procesado (salida)

### Compilar el resultado:

```bash
cobc -x -Wall \
     -I/usr/local/share/open-cobol-esql/copy \
     output.cbl \
     -o programa \
     -Q -Wl,--no-as-needed \
     -locesql
```

**Opciones**:
- `-I/usr/local/share/open-cobol-esql/copy`: Path para encontrar `sqlca.cbl`
- `-Q -Wl,--no-as-needed`: **IMPORTANTE** - Necesario para resolver módulos OCESQLConnect, OCESQLExec, etc. al runtime
- `-locesql`: Enlazar librería `libocesql.so`

**⚠️ Nota sobre el flag `-Q -Wl,--no-as-needed`:**

Sin este flag, puede obtener error en tiempo de ejecución:
```
libcob: error: module 'OCESQLConnect' not found
```

Este flag instruye al enlazador C que no elimine símbolos sin usar, permitiendo que libocesql.so cargue correctamente sus módulos. Es equivalente a exportar `COB_LDFLAGS="-Wl,--no-as-needed"` antes de ejecutar el programa.

---

## Automatizar con Makefile

En el `Makefile` del proyecto puedes agregar:

```makefile
# Precompilar y compilar con ocesql
build-sql:
	ocesql --inc=/usr/local/share/open-cobol-esql/copy \
	       src/minibank-sql.cbl \
	       src/minibank-sql-processed.cbl
	cobc -x -Wall \
	     -I/usr/local/share/open-cobol-esql/copy \
	     src/minibank-sql-processed.cbl \
	     -o build/minibank-sql \
	     -Q -Wl,--no-as-needed \
	     -locesql

run-sql: build-sql
	./build/minibank-sql
```

---

## Sentencias SQL soportadas

### CONNECT/DISCONNECT:
```cobol
EXEC SQL
    CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
END-EXEC.

EXEC SQL
    DISCONNECT ALL
END-EXEC.
```

### CREATE TABLE / INSERT / UPDATE / DELETE:
```cobol
EXEC SQL
    CREATE TABLE ACCOUNTS (
        ACCOUNT_ID VARCHAR(30),
        BALANCE DECIMAL(15,2)
    )
END-EXEC.

EXEC SQL
    INSERT INTO ACCOUNTS VALUES (:ACCT-ID, :BALANCE)
END-EXEC.

EXEC SQL
    UPDATE ACCOUNTS SET BALANCE = :BALANCE WHERE ID = :ACCT-ID
END-EXEC.
```

### SELECT con CURSOR:
```cobol
EXEC SQL
    DECLARE CURSOR1 CURSOR FOR
    SELECT * FROM ACCOUNTS
END-EXEC.

EXEC SQL OPEN CURSOR1 END-EXEC.

PERFORM UNTIL SQLCODE = 100
    EXEC SQL
        FETCH CURSOR1 INTO :ACCT-ID, :BALANCE
    END-EXEC
    IF SQLCODE = 0
        DISPLAY "Cuenta: " ACCT-ID " Saldo: " BALANCE
    END-IF
END-PERFORM.

EXEC SQL CLOSE CURSOR1 END-EXEC.
```

---

## Códigos de error SQLCODE

| SQLCODE | Significado |
|---------|------------|
| 0 | Operación exitosa |
| 100 | Fin de datos (EOF en FETCH) |
| -601 | Tabla ya existe |
| < 0 | Error de base de datos |

---

## Ejemplos en el proyecto

- `src/minibank-sql.cbl` - Archivo fuente con EXEC SQL
- `src/minibank-sql-processed.cbl` - Archivo precompilado (generado)
- `build/minibank-sql` - Ejecutable compilado

---

## Limitaciones actuales

- Requiere PostgreSQL como motor de base de datos
- Las funciones OCESQLConnect, etc. deben estar disponibles en tiempo de ejecución
- Necesita que libocesql.so esté en el LD_LIBRARY_PATH

---

## Referencias

- [Repositorio oficial Open Cobol ESQL](https://github.com/opensourcecobol/Open-COBOL-ESQL)
- [Documentación PostgreSQL](https://www.postgresql.org/docs/)
- [GNU COBOL Manual](https://sourceforge.net/projects/gnucobol/files/gnucobol/)
