# 🚀 GUÍA RÁPIDA - Verificar DB2 y Datos

## ¿Qué sucede al iniciar el dev container?

### Automático (post-create.sh)
```
1. ✅ Verifica que GnuCOBOL está instalado
2. ✅ Espera a que DB2 esté disponible (máx 3 minutos)
3. ✅ Ejecuta init-db2-data.sh automáticamente:
   └─ Crea tablas ACCOUNTS y TRANSACTIONS
   └─ Carga 10 transacciones de ejemplo
   └─ Muestra consultas para verificación
   └─ Calcula y muestra saldos finales
4. ✅ Listo para compilar y ejecutar
```

---

## 📊 Datos que se cargan automáticamente

### Cuentas (3 cuentas)
```
ACC-001  │ Cuenta Corriente Juan  │ $0.00
ACC-002  │ Cuenta Ahorro María    │ $0.00
ACC-003  │ Cuenta Inversión Pedro │ $0.00
```

### Transacciones (10 transacciones)
```
Fecha        │ Cuenta  │ Tipo   │ Monto
─────────────┼─────────┼────────┼──────────
2025-01-10   │ ACC-001 │ CREDIT │ $1,000.00
2025-01-12   │ ACC-001 │ DEBIT  │ $150.25
2025-01-15   │ ACC-002 │ CREDIT │ $500.00
2025-01-18   │ ACC-001 │ CREDIT │ $200.00
2025-01-20   │ ACC-002 │ DEBIT  │ $50.75
2025-01-22   │ ACC-003 │ CREDIT │ $2,500.00
2025-01-25   │ ACC-003 │ DEBIT  │ $350.50
2025-01-28   │ ACC-002 │ CREDIT │ $100.00
2025-02-01   │ ACC-001 │ DEBIT  │ $75.30
2025-02-03   │ ACC-003 │ CREDIT │ $450.00
```

### Saldos finales esperados
```
ACC-001  → $1,049.75  (1000 - 150.25 + 200 - 75.30)
ACC-002  → $449.25    (500 - 50.75 + 100)
ACC-003  → $2,599.50  (2500 - 350.50 + 450)
```

---

## 🔍 Verificar datos en cualquier momento

### Opción 1: Ver la salida del post-create
Ya la viste al iniciar el contenedor. Busca la sección:
```
🗄️ ═════════════════════════════════════════════════════
    INICIALIZANDO DB2 Y CARGANDO DATOS
═════════════════════════════════════════════════════
```

### Opción 2: Ejecutar verificación manual
```bash
# En la terminal del contenedor:
.devcontainer/verify-db2.sh
```

Output esperado:
```
🔍 ═════════════════════════════════════════════════════
    VERIFICANDO ESTADO DE DB2
═════════════════════════════════════════════════════

1️⃣ Verificando conexión a DB2...
✅ DB2 está disponible en db:50000

2️⃣ Verificando tablas y registros...
Total Cuentas                  3
Total Transacciones           10

3️⃣ Mostrando datos de cuentas...
ACCOUNT_ID  ACCOUNT_NAME              BALANCE
───────────────────────────────────────────────────────
ACC-001     Cuenta Corriente Juan      1049.75
ACC-002     Cuenta Ahorro María        449.25
ACC-003     Cuenta Inversión Pedro    2599.50

4️⃣ Resumen de transacciones por cuenta...
ACCOUNT_ID  TRANSACTION_TYPE  Cantidad  Total
───────────────────────────────────────────────
ACC-001     CREDIT                  2   1200.00
ACC-001     DEBIT                   2   -225.55
ACC-002     CREDIT                  2    600.00
ACC-002     DEBIT                   1    -50.75
ACC-003     CREDIT                  2   2950.00
ACC-003     DEBIT                   1   -350.50

✅ Verificación completada
```

### Opción 3: Consulta manual en DB2
```bash
# Conectar directamente a DB2
docker exec -ti db2server bash -c "su - db2inst1 << 'EOF'
CONNECT TO minibank USER db2inst1 USING password
SELECT * FROM ACCOUNTS
SELECT * FROM TRANSACTIONS
CONNECT RESET
EOF
"
```

---

## 🔄 Reinicializar datos si es necesario

Si necesitas limpiar y recargar todo:

```bash
# Opción 1: Ejecutar el script nuevamente
.devcontainer/init-db2-data.sh

# Opción 2: Borrar contenedor DB2 y reiniciar
docker compose -f .devcontainer/compose.yml down
docker compose -f .devcontainer/compose.yml up -d
```

---

## ⚙️ Scripts disponibles

| Script | Propósito | Cuándo ejecutar |
|--------|-----------|-----------------|
| `post-create.sh` | Setup inicial completo | Auto (postCreateCommand) |
| `init-db2-data.sh` | Crear tablas y cargar datos | Manual si se necesita reiniciar |
| `verify-db2.sh` | Verificar estado de datos | Manual para verificación |
| `connect-db2.sh` | Conectar interactivamente a DB2 | Manual para debugging |

---

## 🎯 Flujo completo

```
1. Abrir workspace en Dev Container
   ↓
2. Esperar post-create.sh
   ├─ Crea datos de transacciones
   ├─ Verifica GnuCOBOL
   ├─ Espera a DB2
   └─ Ejecuta init-db2-data.sh
   ↓
3. VER OUTPUT (verás consultas de verificación)
   ├─ ✅ Cuentas cargadas
   ├─ ✅ Transacciones cargadas
   └─ ✅ Saldos calculados
   ↓
4. Compilar programa COBOL:  make build
   ↓
5. Ejecutar programa:         make run
   ↓
6. Ver resultado:             cat data/balances.csv
```

---

## 🐛 Troubleshooting

### ❌ "DB2 no respondió"
```
⚠️ DB2 did not start after 3 minutes
```

**Solución:**
```bash
# Verificar que el contenedor está corriendo
docker ps | grep db2server

# Si no está, reiniciar
docker compose -f .devcontainer/compose.yml restart db
```

### ❌ "Error conectando a DB2 desde COBOL"
**Posibles causas:**
- Base de datos no existe
- Tablas no se crearon
- Credenciales incorrectas

**Solución:**
```bash
# Reiniciar todo
.devcontainer/init-db2-data.sh

# Verificar
.devcontainer/verify-db2.sh
```

### ❌ "Datos no aparecen en verificación"
**Solución:**
```bash
# Reiniciar completamente DB2
docker compose -f .devcontainer/compose.yml down
docker compose -f .devcontainer/compose.yml up -d
# Esperar 30 segundos
sleep 30
# Reinicializar datos
.devcontainer/init-db2-data.sh
```

---

## 📝 Notas importantes

✅ **Los datos se preservan** entre:
- Recompilaciones del programa COBOL
- Redeploys del contenedor app
- Reintentos de ejecución

❌ **Los datos se PIERDEN** cuando:
- Ejecutas `docker compose down` (elimina volumen)
- Detienes y eliminas el contenedor db2server

---

## 💡 Tips útiles

**Ver logs de DB2:**
```bash
docker logs db2server | tail -50
```

**Ejecutar comando SQL manual:**
```bash
docker exec -ti db2server bash -c "su - db2inst1 << 'EOF'
CONNECT TO minibank USER db2inst1 USING password
-- Tu SQL aquí
CONNECT RESET
EOF
"
```

**Contar registros rápidamente:**
```bash
docker exec -ti db2server bash -c "su - db2inst1 << 'EOF'
CONNECT TO minibank USER db2inst1 USING password
SELECT COUNT(*) AS "Cuentas" FROM ACCOUNTS
SELECT COUNT(*) AS "Transacciones" FROM TRANSACTIONS
CONNECT RESET
EOF
"
```
