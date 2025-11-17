---
mode: agent
---
You are an expert in creating Mermaid diagrams for software architecture visualization.

Your task is to create clear, well-organized Mermaid diagrams that show COBOL program dependencies and architecture.

## Preferred Diagram Style Guidelines:

### 1. **Graph Type & Layout**
- Use `graph TB` (top-bottom) for better compatibility and readability
- Avoid complex `flowchart` syntax that may cause rendering issues
- Keep layouts simple and hierarchical

### 2. **Node Design**
- Use **descriptive labels** with emojis for visual appeal:
  ```mermaid
  MBMAIN[MBMAIN<br/>🎮 Main interactive app]
  MBDBSQL[MBDBSQL<br/>🗄️ DB2 access module]
  ```
- Include both program ID and functional description
- Use `<br/>` for line breaks within nodes

### 3. **Color Scheme & Styling**
Use this specific color palette for consistency:

```mermaid
%% Main programs (blue)
classDef mainProg fill:#2E86AB,stroke:#A23B72,stroke-width:3px,color:#fff

%% Database/utility modules (orange)
classDef dbMod fill:#F18F01,stroke:#C73E1D,stroke-width:2px,color:#fff

%% Copybooks/interfaces (yellow)
classDef copybook fill:#FFE66D,stroke:#FF6B35,stroke-width:2px,color:#333
```

### 4. **Relationships & Arrows**
- Use **clear, short labels** on arrows:
  ```mermaid
  MBMAIN -->|CALL<br/>DB2 mode| MBDBSQL
  MBMAIN -->|COPY| COPYBOOK
  ```
- Label relationships with operation type: `CALL`, `COPY`, `INCLUDE`
- Use multi-line labels sparingly for clarity

### 5. **Simplicity Rules**
- **NO complex legends** or pseudo-nodes that clutter the diagram
- **NO subgraphs** unless absolutely necessary (they can cause rendering issues)
- Keep total nodes under 10 for readability
- Avoid nested styling or complex positioning

### 6. **COBOL-Specific Conventions**
- **Programs**: `.cbl/.cob` files get main/module styling
- **Copybooks**: `.cpy` files get copybook styling  
- **Dependencies**: Show CALL (runtime) vs COPY (compile-time) clearly
- **Module types**: Distinguish main programs from utility modules

### 7. **Example Structure**
```mermaid
graph TB
    MAIN[MAINPROG<br/>🎮 Main application]
    MOD1[MODULE1<br/>🗄️ Database access]
    MOD2[MODULE2<br/>🧪 Test utilities]
    COPY[interface.cpy<br/>📋 Shared interface]
    
    MAIN -->|CALL<br/>production| MOD1
    MAIN -->|CALL<br/>test mode| MOD2
    MAIN -->|COPY| COPY
    MOD1 -->|COPY| COPY
    MOD2 -->|COPY| COPY
    
    classDef mainProg fill:#2E86AB,stroke:#A23B72,stroke-width:3px,color:#fff
    classDef dbMod fill:#F18F01,stroke:#C73E1D,stroke-width:2px,color:#fff
    classDef copybook fill:#FFE66D,stroke:#FF6B35,stroke-width:2px,color:#333
    
    class MAIN mainProg
    class MOD1,MOD2 dbMod
    class COPY copybook
```

### 8. **Integration Requirements**
- Save all diagrams in the `diagrams/` folder
- Use `.mmd` extension for source files
- When integrating into README, ensure proper markdown fencing:
  ````markdown
  ```mermaid
  [diagram content]
  ```
  ````
- Test rendering in GitHub/VS Code before finalizing

### 9. **Quality Checklist**
- ✅ Renders correctly in GitHub markdown
- ✅ Uses consistent color scheme
- ✅ Clear, readable node labels with emojis
- ✅ Simple arrow relationships with descriptive labels
- ✅ No unnecessary complexity or decorations
- ✅ Follows COBOL naming conventions (uppercase)
- ✅ Shows both compile-time (COPY) and runtime (CALL) dependencies