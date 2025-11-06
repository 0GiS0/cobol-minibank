# 🗄️ DB2 Setup Guide para COBOL MiniBank

## 📋 Situación actual

La extensión `IBM.db2forzosdeveloperextension` es para **Db2 z/OS (mainframe)**, no para Db2 Community Edition que tienes en Docker. Por eso daba error.

**Solución**: Usaremos el cliente DB2 nativo desde terminal, que es más directo y funcional.

---

## 🚀 Cómo conectarse a DB2

### Opción 1: Script automático (Más fácil)

```bash
.devcontainer/connect-db2.sh
```

Este script:
- ✅ Espera a que DB2 esté listo
- ✅ Muestra las tablas disponibles
- ✅ Inicia sesión interactiva
- ✅ Maneja errores automáticamente

### Opción 2: Terminal DB2 manual

```bash
# Conectar a la base de datos
db2 CONNECT TO minibank USER db2inst1 USING password

# Ver comandos disponibles
db2 ?

# Queries útiles
db2 "SELECT * FROM ACCOUNTS"
db2 "SELECT * FROM TRANSACTIONS"

# Salir
db2 QUIT
```

### Opción 3: Ejecutar SQL file

```bash
# Crear las tablas desde el script SQL
db2 -tf .devcontainer/init-db2.sql

# Ver las tablas creadas
db2 "LIST TABLES"
```

---

## 📊 Estructura de datos

### Tabla ACCOUNTS
```
ACCOUNT_ID      VARCHAR(30)      PRIMARY KEY
ACCOUNT_NAME    VARCHAR(100)
BALANCE         DECIMAL(15,2)
CREATED_AT      TIMESTAMP
UPDATED_AT      TIMESTAMP
```

### Tabla TRANSACTIONS
```
TRANSACTION_ID  INT              PRIMARY KEY (auto-generated)
ACCOUNT_ID      VARCHAR(30)      FOREIGN KEY
TRANSACTION_DATE DATE
TRANSACTION_TYPE VARCHAR(10)     (CREDIT o DEBIT)
AMOUNT          DECIMAL(15,2)
CREATED_AT      TIMESTAMP
```

---

## 🔗 Próximo paso: Integrar COBOL con DB2

Para que tu programa COBOL **inserte datos en DB2** en lugar de solo generar CSV:

```cobol
EXEC SQL
  INSERT INTO ACCOUNTS (ACCOUNT_ID, ACCOUNT_NAME, BALANCE)
  VALUES (:WS-ACCOUNT, :WS-ACCOUNT-NAME, :WS-BALANCE)
END-EXEC
```

**¿Quieres que integre COBOL + DB2?** Dime y lo hacemos.

---

## ⚡ Troubleshooting

### "Command db2 not found"
- DB2 client tools no están en el container
- **Solución**: Los instalaremos si lo necesitas

### "Connection refused"
- DB2 no está corriendo
- **Verifica**: `docker ps | grep db2`

### "Authentication failed"
- Usuario/contraseña incorrectos
- **Datos correctos**:
  - User: `db2inst1`
  - Password: `password`
  - Database: `minibank`

---

## 📝 Notas

- El container `app` puede conectar a `db:50000` directamente (red Docker)
- Las tablas persisten en el volumen `db2_data`
- Puedes ejecutar SQL files con: `db2 -tf archivo.sql`

