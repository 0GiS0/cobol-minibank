---
agent: mermaid-diagram-creator
name: 🎨 Mermaid Diagram Creator
description: 'Creates optimized Mermaid diagrams for COBOL architectures, dependencies and data flows'
---

# 🎨 Test Prompt: Mermaid Diagram Creator

## Descripción
Prueba el agente **Mermaid Diagram Creator** generando diagramas visuales de la arquitectura COBOL.

## Prompt

```
Crea una serie de diagramas Mermaid para documentar visualmente el proyecto COBOL MiniBank. Los diagramas deben ser profesionales, informativos y optimizados para documentación técnica.

## Diagrama 1: Arquitectura del Sistema

**Tipo:** Architecture Diagram

**Componentes:**
- Mainframe z/OS con:
  - DB2 Database (tablas: ACCOUNTS, TRANSACTIONS, AUDIT_LOG)
  - CICS Transaction Server
  - Batch Job Scheduler
- Capa COBOL:
  - MBMAIN (🎮 Main Controller)
  - MBDBSQL (🗄️ DB2 Access Module)
  - MBDBCLI (🧪 Testing/CLI Module)
  - MBSEC (🔐 Security Module)
- Interfaces:
  - REST API Proxy (para acceso moderno)
  - CSV Files (legacy)
  - Jenkins/GitHub Actions (CI/CD)

**Requisitos:**
- Mostrar flujo de datos
- Indicar dependencias
- Incluir almacenamiento (DB2)
- Mostrar puntos de entrada/salida

## Diagrama 2: Dependencias de Programas

**Tipo:** Graph/Flowchart

**Componentes:**
- MBMAIN llama a:
  - MBSEC (autenticación)
  - MBDBSQL o MBDBCLI (basado en ENV)
  - MBUTIL (utilidades)
- MBDBSQL llama a:
  - Copybooks: mb-db-if.cpy
  - Rutinas de DB2
- MBDBCLI llama a:
  - Copybooks: mb-db-if.cpy
  - Stub data

**Indicadores:**
- Llamadas directas vs indirectas
- Módulos dinámicos vs estáticos
- Decisiones basadas en variables de ambiente

## Diagrama 3: Flujo de Datos (Data Flow)

**Tipo:** Flowchart

**Proceso:**
1. Usuario -> MBMAIN (input)
2. MBMAIN -> MBSEC (validación)
3. MBSEC -> MBMAIN (credentials OK?)
4. MBMAIN -> MBDBSQL/MBDBCLI (DB request)
5. DB Module -> DB2 o Stub Data
6. DB Module -> MBMAIN (response)
7. MBMAIN -> User (output)

**Incluir:**
- Caminos de éxito y error
- Códigos de retorno
- Validaciones principales

## Diagrama 4: Ciclo de Vida de una Transacción

**Tipo:** Sequence Diagram

**Participantes:**
- User
- MBMAIN
- MBSEC
- MBDBSQL
- DB2
- AUDIT_LOG

**Escenario: Depósito a Cuenta**
1. User solicita depósito
2. MBMAIN recibe solicitud
3. MBSEC valida usuario/cuenta
4. MBDBSQL inicia transacción
5. Obtiene balance actual
6. Actualiza balance
7. Registra en AUDIT_LOG
8. Confirma transacción
9. Retorna resultado a usuario

## Diagrama 5: Estructura de Datos (Entity Relationship)

**Tipo:** ER Diagram

**Entidades:**
- ACCOUNTS
  - account_id (PK)
  - customer_name
  - balance
  - created_date
  - status

- TRANSACTIONS
  - tx_id (PK)
  - account_id (FK)
  - amount
  - tx_type (DEPOSIT, WITHDRW)
  - tx_date

- AUDIT_LOG
  - log_id (PK)
  - account_id (FK)
  - operation (BALANCE, DEPOSIT, WITHDRW)
  - result (SUCCESS, FAILURE)
  - timestamp
  - user_id

**Relaciones:**
- ACCOUNTS 1:N TRANSACTIONS
- ACCOUNTS 1:N AUDIT_LOG

## Diagrama 6: Pipeline CI/CD

**Tipo:** Flowchart

**Etapas:**
1. Git Push
2. Compile (cobc -x -Wall -O2)
3. Unit Tests
4. SonarQube Analysis
5. Build Load Module
6. Deploy to DEV
7. Integration Tests
8. Deploy to PROD
9. Smoke Tests
10. Monitoring

**Decisiones:**
- Si tests fallan -> ABORT
- Si sonar quality gate falla -> ABORT
- Si deploy falla -> ROLLBACK

## Diagrama 7: Casos de Uso

**Tipo:** Use Case Diagram

**Actores:**
- Bank Customer
- Bank Teller
- Administrator
- System

**Casos de Uso:**
- Check Balance
- Deposit Funds
- Withdraw Funds
- Transfer Funds
- View Transaction History
- Audit Trail Review
- System Administration
- User Management

## Diagrama 8: Estructura Modular (Gantt Timeline)

**Tipo:** Gantt Chart

**Proyecto:** Modernización MiniBank (16 semanas)

**Fases:**
- Fase 1: Refactoring (Semanas 1-4)
  - Módulo MBVAL
  - Módulo MBSEC
  - Optimización DB2
- Fase 2: Extensión (Semanas 5-10)
  - MBTXLOG
  - MBEXPORT
  - Sistema de Auditoría
- Fase 3: Integración (Semanas 11-14)
  - API REST
  - CI/CD Pipeline
  - Documentación
- Fase 4: Post-Launch (Semanas 15-16)
  - UAT
  - Production Monitoring

## Especificaciones Generales

- **Estilo:** Professional, clean, readable
- **Colores:** Use consistent color scheme
- **Iconos/Emojis:** Use para claridad (🎮, 🗄️, 🔐, etc.)
- **Anotaciones:** Explicar puntos clave
- **Tamaño:** Optimizado para documentación (no demasiado grande)
- **Validez:** Todos deben ser válidos Mermaid syntax

## Deliverables

Generar un archivo Markdown (diagrams.md) que contenga:
1. Todos los 8 diagramas
2. Descripción de cada diagrama
3. Leyenda/explicaciones
4. Links a documentación relacionada

Los diagramas deben ser:
- Listos para visualizar (Mermaid Live, GitHub)
- Listos para PDF export
- Incluidos en documentación técnica
```

## Resultado Esperado
Archivo `docs/diagrams.md` con:
- 8 diagramas Mermaid professionalmente diseñados
- Descripciones detalladas
- Validación de sintaxis Mermaid
- Listo para documentación técnica formal

## Notas
- Este prompt prueba habilidad de visualización técnica
- Valida comprensión de arquitectura COBOL
- Verifica diversidad de tipos de diagramas
- Comprueba claridad y profesionalismo visual
