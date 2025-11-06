# 🏦 RESUMEN EJECUTIVO - COBOL MiniBank

## ¿QUÉ HACE EL PROYECTO?

Un sistema bancario que:
1. 📖 Lee transacciones desde un CSV
2. 💾 Las guarda en base de datos **DB2**
3. 🧮 Calcula saldos finales
4. 📊 Genera reporte de salida

## EL FLUJO EN 5 PASOS

```
Transacciones CSV → Programa COBOL → Base Datos DB2 → Consulta Saldos → Reporte CSV
     (entrada)    (src/minibank.cob)   (ACCOUNTS +    (SELECT * FROM  (salida)
                                       TRANSACTIONS)   ACCOUNTS)
```

## ARCHIVOS PRINCIPALES

| Archivo | Tipo | Propósito | Estado |
|---------|------|-----------|--------|
| `src/minibank.cob` | COBOL | Programa principal con SQL embebido | ✅ ACTIVO |
| `data/transactions.csv` | CSV | Datos de entrada | ✅ NECESARIO |
| `data/balances.csv` | CSV | Salida del programa | ✅ NECESARIO |
| `.devcontainer/` | Config | Entorno Docker con DB2 | ✅ NECESARIO |
| `Makefile` | Build | Compilación del COBOL | ✅ NECESARIO |

## ARCHIVOS A ELIMINAR ❌

| Archivo | Razón | Impacto |
|---------|-------|--------|
| `src/minibank-db2.cob` | Programa alternativo no usado | Código muerto |
| `src/minibank-db2` | Ejecutable compilado de anterior | Regenerable |
| `src/copybooks/record-layout.cpy` | Estructura de referencia no incluida | Muerto |
| `build/` | Directorio vacío | Confusión |

## ARCHIVOS OBSOLETOS (Usar con cuidado)

Si decides usar **embedded SQL** (recomendado, ya lo hace el programa principal):

```
.devcontainer/db2-interface.py        ← Solo para minibank-db2 (obsoleto)
.devcontainer/connect-db2.py          ← Helper Python (obsoleto)
.devcontainer/init-db2.py             ← Helper Python (obsoleto)
.devcontainer/connect-db2.sh          ← Helper shell (obsoleto)
.devcontainer/init-tables.sh          ← Helper shell (obsoleto)
```

## TECNOLOGÍAS USADAS

- **COBOL** (GnuCOBOL 3.x) - Lenguaje de programación
- **DB2** (Community Edition) - Base de datos empresarial
- **Docker** - Contenedores
- **CSV** - Formato de datos

## COMANDOS ÚTILES

```bash
make build          # Compilar programa COBOL
make run            # Compilar + Ejecutar
make clean          # Limpiar ejecutables
```

## RESULTADO DE EJECUCIÓN

**Entrada** (transactions.csv):
```
2025-01-10,ACC-001,CREDIT,1000
2025-01-12,ACC-001,DEBIT,150.25
2025-01-15,ACC-002,CREDIT,500
2025-01-18,ACC-001,CREDIT,200
2025-01-20,ACC-002,DEBIT,50.75
```

**Salida** (balances.csv):
```
account,balance
ACC-001,1049.75
ACC-002,449.25
```

---

Ver archivo `DOCUMENTACION.md` para análisis completo.
