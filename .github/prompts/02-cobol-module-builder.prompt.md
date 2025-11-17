---
agent: cobol-module-builder
name: 🔧 COBOL Module Builder
description: 'Implements and develops COBOL modules following enterprise standards'
---

## Descripción
Prueba el agente **COBOL Module Builder** implementando un nuevo módulo de seguridad para el sistema.

## Prompt

```
Implementa un nuevo módulo COBOL llamado MBSEC (Security Module) para el proyecto MiniBank que proporcione autenticación y validación de usuarios.

Requisitos:

1. **Especificación del Módulo**
   - Program ID: MBSEC
   - Propósito: Validar credenciales de usuario y autorización
   - Interface: Usar la misma estructura que mb-db-if.cpy (request/response)

2. **Funcionalidades**
   - AUTH-USR: Validar usuario y contraseña
   - AUTH-PIN: Validar PIN de 4 dígitos
   - VALIDATE-ACC: Validar acceso a una cuenta específica
   - LOG-ATTEMPT: Registrar intentos de acceso (exitosos y fallidos)

3. **Estructura de Datos**
   - Aceptar parametros: usuario, contraseña, account-id, PIN
   - Retornar: código de éxito/error, mensaje descriptivo, nivel de permisos

4. **Validaciones**
   - Usuario no puede ser vacío
   - Contraseña mínimo 8 caracteres
   - PIN debe ser numérico de 4 dígitos
   - Account ID debe existir en el formato esperado

5. **Estándares de Código**
   - Seguir estructura COBOL '85
   - Incluir manejo robusto de errores
   - Comentarios estructurados y descriptivos
   - Usar COMP-3 para campos numéricos
   - Scope terminators (END-IF, END-PERFORM, etc.)
   - Máximo 3-4 niveles de anidamiento

6. **Seguridad**
   - Implementar límite de intentos fallidos (5 intentos)
   - Bloqueo temporal de cuenta después de 5 fallos
   - Log de todos los intentos (incluyendo fallidos)
   - Nunca mostrar contraseña en logs

7. **Deliverables**
   - src/mb-sec.cbl - Módulo COBOL compilable
   - Comentarios de header con propósito y autor
   - Copiar métodos para interfaces de datos si es necesario
```

## Resultado Esperado
Archivo `src/mb-sec.cbl` con implementación completa del módulo de seguridad, listo para compilar y usar.

## Notas
- Este prompt prueba la implementación de código COBOL completo
- Valida seguimiento de estándares COBOL enterprise
- Verifica optimizaciones (COMP-3, etc.)
- Comprueba manejo de errores robusto
