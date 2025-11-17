---
agent: cobol-project-planner
name: 📋 COBOL Project Planner
description: 'Plans COBOL enterprise projects with modular architecture and timelines'
---

# 📋 Test Prompt: COBOL Project Planner

## Descripción
Prueba el agente **COBOL Project Planner** planificando un proyecto de modernización de MiniBank.

## Prompt

```
Planifica un proyecto enterprise COBOL de modernización del sistema MiniBank con los siguientes objetivos:

## Objetivo General
Modernizar MiniBank de un sistema monolítico a una arquitectura modular con capacidad de integración de APIs REST y mejora de performance.

## Contexto Actual
- Sistema: COBOL MiniBank (2025)
- Módulos actuales: MBMAIN, MBDBSQL, MBDBCLI
- Base de datos: DB2 v11.x
- Plataforma: z/OS / GNU COBOL
- Equipo: 4 desarrolladores COBOL, 1 DBA, 1 Arquitecto
- Timeline: 16 semanas

## Requisitos del Proyecto

### Fase 1: Refactoring y Optimización (Semanas 1-4)
- Extraer lógica de validación en módulo MBVAL
- Crear módulo de seguridad MBSEC (autenticación/autorización)
- Optimizar queries DB2 en MBDBSQL
- Crear suite de unit tests en COBOL

### Fase 2: Extensión de Funcionalidades (Semanas 5-10)
- Módulo MBTXLOG para logging de transacciones
- Módulo MBEXPORT para exportar datos a formatos modernos (JSON, XML)
- Sistema de auditoría completo
- API REST proxy para acceso a funciones core

### Fase 3: Integración y Deployment (Semanas 11-14)
- Integración con sistemas externos (API REST)
- Pipeline CI/CD para compilación y testing
- Documentación técnica completa
- Training del equipo de mantenimiento

### Fase 4: Post-Lanzamiento (Semanas 15-16)
- UAT (User Acceptance Testing)
- Monitoreo en producción
- Optimizaciones post-deployment

## Requerimientos para el Plan

El plan debe incluir:

1. **Arquitectura Propuesta**
   - Diagrama de módulos
   - Dependencias entre componentes
   - Decisiones arquitecturales y justificaciones

2. **Desglose de Tareas (Work Breakdown Structure)**
   - Por fase
   - Dependencias entre tareas
   - Hitos clave

3. **Resource Allocation**
   - Asignación por persona/rol
   - Carga de trabajo por semana
   - Riesgos de recursos

4. **Tecnologías y Herramientas**
   - Requisitos: GNU COBOL 2.2+, DB2 11.x, Git, Jenkins/GitHub Actions
   - Herramientas de testing: COBOL Unit, SonarQube
   - Documentación: Markdown, Mermaid diagrams

5. **Métricas de Éxito**
   - Cobertura de testing (objetivo: >80%)
   - Reducción de tiempo de procesamiento batch (20%)
   - Disponibilidad del sistema (99.9%)
   - Documentación al 100%

6. **Riesgos y Mitigación**
   - Identificar riesgos principales
   - Proponer estrategias de mitigación
   - Plan B para escenarios críticos

7. **Timeline Visual**
   - Gantt chart en formato texto o Mermaid
   - Hitos principales
   - Puntos de decisión

8. **Budget Estimado**
   - Horas/persona por fase
   - Recursos de infraestructura
   - Costo total aproximado
```

## Resultado Esperado
Documento de planificación del proyecto en formato Markdown con:
- Arquitectura propuesta
- Desglose de tareas detallado
- Asignación de recursos
- Timeline completo
- Análisis de riesgos

## Notas
- Este prompt prueba capacidades de planificación enterprise
- Valida descomposición de proyecto complejo
- Verifica coordinación de múltiples componentes
- Comprueba timeline y resource planning realista
