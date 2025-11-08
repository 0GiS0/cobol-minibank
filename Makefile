APP=src/minibank
APP_DB2=src/minibank-db2
APP_MENU=src/minibank-menu
APP_SQL=build/minibank-sql
APP_SQL_DB2=build/minibank-sql-db2
SRC=src/minibank.cob
SRC_DB2=src/minibank-db2.cob
SRC_MENU=src/minibank-menu.cob
SRC_SQL=src/minibank-sql.cbl
SRC_SQL_PROCESSED=src/minibank-sql-processed.cbl
SRC_SQL_DB2=src/minibank-sql-db2.cbl

COBC=cobc
COBCFLAGS=-x -Wall -O2 -I src/copybooks

# Flags para ocesql + PostgreSQL
OCESQL=ocesql
OCESQL_INCLUDE=/usr/local/share/open-cobol-esql/copy
OCESQL_LIB=/usr/local/lib
OCESQL_FLAGS=-x -Wall -I$(OCESQL_INCLUDE) -L$(OCESQL_LIB) -locesql

# Configuración para db2precompile
DB2_PRECOMPILE=db2precompile
DB2_LIB=/opt/ibm/db2/V*/lib
DB2_INCLUDE=/opt/ibm/db2/V*/include
DB2_FLAGS=-x -Wall -I$(DB2_INCLUDE) -L$(DB2_LIB) -ldb2

.PHONY: build build-db2 build-menu build-sql build-sql-db2 run run-db2 run-menu run-sql run-sql-db2 init-db2 clean

build:
	$(COBC) $(COBCFLAGS) -o $(APP) $(SRC)

build-db2:
	$(COBC) $(COBCFLAGS) -o $(APP_DB2) $(SRC_DB2)

build-menu:
	$(COBC) $(COBCFLAGS) -o $(APP_MENU) $(SRC_MENU)

build-sql:
	@echo "🔧 Precompilando archivo con EXEC SQL (ocesql para PostgreSQL)..."
	@mkdir -p build
	@$(OCESQL) --inc=$(OCESQL_INCLUDE) $(SRC_SQL) $(SRC_SQL_PROCESSED)
	@echo "📝 Compilando código precompilado..."
	@$(COBC) $(OCESQL_FLAGS) -Q -Wl,--no-as-needed -o $(APP_SQL) $(SRC_SQL_PROCESSED)
	@echo "✅ Compilación exitosa: $(APP_SQL)"

build-sql-db2:
	@echo "🔧 Precompilando archivo con EXEC SQL (db2precompile para DB2)..."
	@mkdir -p build
	@if command -v db2precompile >/dev/null 2>&1; then \
		./db2-compile.sh $(SRC_SQL_DB2) $(APP_SQL_DB2); \
	else \
		echo "❌ db2precompile no encontrado. Instala el cliente DB2."; \
		exit 1; \
	fi

run: build
	@mkdir -p data
	@./$(APP) && \
	echo "✅ Hecho. Salida en data/balances.csv"

run-db2: build-db2
	@mkdir -p data
	@DB2_SILENT=1 ./$(APP_DB2) && \
	echo "✅ Hecho. Salida en data/balances.csv"

run-menu: init-db2 build-menu
	@echo "Iniciando programa interactivo MINIBANK..."
	@./$(APP_MENU)

run-sql: build-sql
	@mkdir -p data
	@echo "🏦 Ejecutando MINIBANK con SQL embebido (ocesql/PostgreSQL)..."
	@COB_LDFLAGS="-Wl,--no-as-needed" LD_LIBRARY_PATH=$(OCESQL_LIB):$$LD_LIBRARY_PATH $(APP_SQL) && \
	echo "✅ Programa ejecutado correctamente" || echo "⚠️ Ejecución finalizada con advertencia"

run-sql-db2: build-sql-db2
	@mkdir -p data
	@echo "🏦 Ejecutando MINIBANK con SQL embebido (db2precompile/DB2)..."
	@export LD_LIBRARY_PATH=$(DB2_LIB):$$LD_LIBRARY_PATH && \
	$(APP_SQL_DB2) && \
	echo "✅ Programa ejecutado correctamente" || echo "⚠️ Ejecución finalizada con advertencia"

init-db2:
	@echo "🔧 Inicializando DB2..."
	@.devcontainer/init-db2-cli.sh
	@.devcontainer/load-sample-data-cli.sh

clean:
	rm -f $(APP) $(APP_DB2) $(APP_MENU) $(APP_SQL) $(APP_SQL_DB2)
