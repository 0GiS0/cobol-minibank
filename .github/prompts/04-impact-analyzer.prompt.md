---
agent: impact-analyzer
name: 📊 Impact Analyzer
description: 'Analyzes impact of changes in COBOL code, dependencies and system architecture'
---

# 📊 Test Prompt: Impact Analyzer

## Descripción
Prueba el agente **Impact Analyzer** analizando el impacto de cambios en la interfaz COBOL.

## Prompt

```
Analiza el impacto de los siguientes cambios propuestos en el proyecto COBOL MiniBank:

## Cambios Propuestos

### Cambio 1: Modificación de mb-db-if.cpy
Agregar nuevos campos a la interfaz de datos del módulo de base de datos:

**Cambios:**
- Agregar campo: DB-TRANSACTION-ID (PIC X(20)) - ID único de transacción
- Agregar campo: DB-TIMESTAMP (PIC X(19)) - Timestamp de la operación (YYYY-MM-DD HH:MM:SS)
- Agregar campo: DB-OPERATION-CODE (PIC 9(3)) - Código de operación (para auditoría)
- Deprecar: DB-FUNC (mover a 'OLD-DB-FUNC')

**Razón:** Mejorar trazabilidad y auditoría de operaciones

### Cambio 2: Nueva Función en MBDBSQL
Agregar función AUDIT-LOG para registrar todas las operaciones en tabla de auditoría

**Especificación:**
- Función: AUDIT-LOG
- Registra: Usuario, operación, account-id, resultado, timestamp
- Tabla: AUDIT_LOG (nueva)
- Índices: account_id, timestamp, user_id

### Cambio 3: Refactoring de MBMAIN
Cambiar la forma en que MBMAIN invoca los módulos (de CALL estática a CALL dinámica):
- De: `CALL 'MBDBSQL' USING DB-REQUEST`
- A: `CALL WS-DYNAMIC-MODULE USING DB-REQUEST`

## Análisis Requerido

Para cada cambio, proporciona:

1. **Análisis de Dependencias**
   - ¿Qué programas COBOL serán afectados?
   - ¿Qué copybooks dependientes?
   - ¿Qué procesos batch?
   - ¿Qué scriptsJCL?

2. **Programas Impactados**
   - Listar cada programa afectado
   - Tipo de cambio requerido (recompilación, modificación, etc.)
   - Estimación de esfuerzo (horas)

3. **Análisis de Riesgo**
   - Risk score (1-10): Bajo, Medio, Alto, Crítico
   - Argumentación del score
   - Escenarios de fallo potencial

4. **Impacto en Testing**
   - ¿Qué tipos de tests se requieren? (unit, integration, regression)
   - Casos de test críticos
   - Esfuerzo estimado de testing

5. **Impacto en Performance**
   - ¿Mejorará o degradará performance?
   - Estimaciones de impacto
   - Consideraciones de CICS/DB2

6. **Impacto en Capacidad**
   - ¿Se requiere más CPU/memoria?
   - ¿Cambios en almacenamiento DB2?
   - Impacto en tiempo de ejecución batch

7. **Impacto en Mantenimiento**
   - ¿Cambios en procedimientos operacionales?
   - ¿Nuevos logs o monitoreo?
   - ¿Cambios en documentación?

8. **Timeline de Implementación Recomendado**
   - Orden de implementación
   - Paralelización posible
   - Duración total estimada

9. **Plan de Rollback**
   - Cómo revertir cada cambio
   - Procedimientos de fallback
   - Testing de rollback

10. **Métricas de Validación**
    - Cómo validar que el cambio fue exitoso
    - Métricas a monitorear post-deployment
    - SLAs a mantener
```

## Resultado Esperado
Reporte completo de análisis de impacto incluyendo:
- Matriz de dependencias
- Lista de programas afectados
- Risk assessment
- Plan de testing
- Timeline y esfuerzo estimado
- Recomendaciones

## Notas
- Este prompt prueba análisis profundo de dependencias
- Valida evaluación de riesgos COBOL
- Verifica comprensión de arquitectura modular
- Comprueba impacto en infraestructura mainframe
