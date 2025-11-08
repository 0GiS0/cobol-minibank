APP=src/minibank
APP_DB2=src/minibank-db2
APP_MENU=src/minibank-menu
APP_SQL=build/minibank-sql
SRC=src/minibank.cob
SRC_DB2=src/minibank-db2.cob
SRC_MENU=src/minibank-menu.cob
SRC_SQL=src/minibank-sql.cbl
SRC_SQL_PROCESSED=src/minibank-sql-processed.cbl

COBC=cobc
COBCFLAGS=-x -Wall -O2 -I src/copybooks

# Flags para ocesql + PostgreSQL
OCESQL=ocesql
OCESQL_INCLUDE=/usr/local/share/open-cobol-esql/copy
OCESQL_LIB=/usr/local/lib
OCESQL_FLAGS=-x -Wall -I$(OCESQL_INCLUDE) -L$(OCESQL_LIB) -locesql

.PHONY: build build-db2 build-menu build-sql run run-db2 run-menu run-sql init-db2 clean

build:
	$(COBC) $(COBCFLAGS) -o $(APP) $(SRC)

build-db2:
	$(COBC) $(COBCFLAGS) -o $(APP_DB2) $(SRC_DB2)

build-menu:
	$(COBC) $(COBCFLAGS) -o $(APP_MENU) $(SRC_MENU)

build-sql:
	@echo "🔧 Precompilando archivo con EXEC SQL..."
	@mkdir -p build
	@$(OCESQL) --inc=$(OCESQL_INCLUDE) $(SRC_SQL) $(SRC_SQL_PROCESSED)
	@echo "📝 Compilando código precompilado..."
	@$(COBC) $(OCESQL_FLAGS) -Q -Wl,--no-as-needed -o $(APP_SQL) $(SRC_SQL_PROCESSED)
	@echo "✅ Compilación exitosa: $(APP_SQL)"

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
	@echo "🏦 Ejecutando MINIBANK con SQL embebido..."
	@COB_LDFLAGS="-Wl,--no-as-needed" LD_LIBRARY_PATH=$(OCESQL_LIB):$$LD_LIBRARY_PATH $(APP_SQL) && \
	echo "✅ Programa ejecutado correctamente"

init-db2:
	@python3 .devcontainer/init-db2.py
	@python3 .devcontainer/load-sample-data.py

clean:
	rm -f $(APP) $(APP_DB2) $(APP_MENU) $(APP_SQL)
