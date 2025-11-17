---
agent: jcl-generator
name: ⚙️ JCL Generator
description: 'Generates optimized Job Control Language for COBOL compilation and deployment'
---

# ⚙️ Test Prompt: JCL Generator

## Descripción
Prueba el agente **JCL Generator** creando scripts JCL para compilación, link-edit y ejecución.

## Prompt

```
Genera Jobs en JCL (Job Control Language) optimizados para el proyecto COBOL MiniBank para las siguientes operaciones:

## Job 1: Compilación Completa del Proyecto

**Requisitos:**
- Compilar todos los módulos COBOL en orden de dependencia:
  1. Copybooks: mb-db-if.cpy
  2. MBDBCLI (módulo CLI)
  3. MBDBSQL (módulo DB2)
  4. MBMAIN (programa principal)
- Usar compilador Enterprise COBOL (IGYCRCTL) o alternativa moderna
- Generar: OBJECT files, LIST files, XREF
- Redirigir errores a SYSOUT
- Parar en primer error (MAXCC=4)

**Consideraciones:**
- Copybook library: MY.COBOL.COPYLIB
- Source library: MY.SOURCE.COBOL
- Object library: MY.OBJECT.COBOL
- Compiler parms: 'LIB,OBJECT,LIST,XREF'

## Job 2: Link-Edit y Generación de Load Module

**Requisitos:**
- Link-edit los OBJECTs generados
- Crear LOAD MODULE: MBMAIN
- Linkage: MBMAIN -> MBDBSQL + MBDBCLI
- Resolver Referencias externas
- Generar: MAP file, XREF, symbol table
- Ubicación: MY.LOAD.LIBRARY

**Pasos:**
1. Link-edit MBMAIN.OBJ
2. Link-edit MBDBSQL.OBJ
3. Link-edit MBDBCLI.OBJ
4. Link-edit con resolución de referencias

## Job 3: Ejecución de Pruebas Unitarias

**Requisitos:**
- Ejecutar cada módulo con datos de test
- MBDBCLI con modo stubbed (sin DB2 real)
- MBDBSQL con DB2 Connection (si disponible)
- Generar reportes de test
- Archivos de entrada: data/transactions.csv
- Archivos de salida: data/test-results.txt

**Procedimiento:**
1. Setup (limpieza de datasets temporales)
2. Ejecutar MBMAIN con parámetros de test
3. Validar returncode
4. Generar reporte de test

## Job 4: Carga de Datos de Prueba en DB2

**Requisitos:**
- Crear tablas en DB2 (si no existen)
- Cargar datos de test desde CSV
- Validar integridad de datos
- Generar reporte de carga

**Tablas:**
- ACCOUNTS (account_id, customer_name, balance)
- TRANSACTIONS (tx_id, account_id, amount, tx_date)
- AUDIT_LOG (log_id, account_id, operation, result, timestamp)

## Job 5: Backup y Recovery

**Requisitos:**
- Backup de las tablas DB2 principales
- Exportar datos a datasets para backup
- Generar punto de recuperación
- Verificar integridad de backup

**Datasets a Backup:**
- DB2.ACCOUNTS
- DB2.TRANSACTIONS
- DB2.AUDIT_LOG

## Especificaciones Técnicas

### Requisitos de JCL:
- Syntax: JCL estándar z/OS
- Performance: Optimizar uso de SYSDA/DASD
- Error Handling: Condition codes y COND statements
- Seguridad: Incluir parámetros de acceso (CLASS, NOTIFY)

### Accounting Information:
- Usar JOB Statement: (ACCT,COBOL,CLASS=A,MSGCLASS=H)
- Priority (PRTY): 6 para jobs normales, 8 para urgentes
- Tiempo máximo: 30 minutos

### Datasets Temporales:
- UNIT: Preferir SYSDA
- SPACE: Estimaciones realistas
- DISP: (NEW,DELETE) para temporales
- RECFM/LRECL: Configuración apropiada

### Reporting:
- SYSPRINT: Lista de compilación
- SYSOUT: Mensajes y errores
- Datasets de salida con resultados

## Deliverables

Generar 5 archivos JCL:

1. **BUILD-COMPILE.jcl** - Compilación completa
2. **BUILD-LINKEDIT.jcl** - Link-edit y Load Module
3. **TEST-EXECUTION.jcl** - Ejecución de tests
4. **LOAD-TEST-DATA.jcl** - Carga de datos DB2
5. **BACKUP-RECOVERY.jcl** - Backup de datos

Cada archivo debe:
- Tener header comentado con propósito
- Incluir instrucciones de uso
- Contener condition codes y error handling
- Ser listo para producción
```

## Resultado Esperado
5 archivos JCL profesionales listos para:
- Compilación y build en z/OS
- Ejecución en ambiente mainframe
- Deployment en producción
- Testing y QA

## Notas
- Este prompt prueba conocimiento de JCL enterprise
- Valida estándares de mainframe (SYSDA, SPACE, RECFM, etc.)
- Verifica integración con Enterprise COBOL
- Comprueba manejo de DB2 desde JCL
