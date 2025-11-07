APP=src/minibank
APP_DB2=src/minibank-db2
APP_MENU=src/minibank-menu
APP_SQL=src/minibank-sql
SRC=src/minibank.cob
SRC_DB2=src/minibank-db2.cob
SRC_MENU=src/minibank-menu.cob
SRC_SQL=src/minibank-sql.cbl
COBC=cobc
COBCFLAGS=-x -Wall -O2 -I src/copybooks
# Flags especiales para SQL embebido (requiere librerías DB2)
SQLCOBCFLAGS=-x -Wall -O2 -I src/copybooks -ldb2 -L/opt/ibm/db2/V11.5/lib64

.PHONY: build build-db2 build-menu build-sql run run-db2 run-menu run-sql init-db2 clean

build:
	$(COBC) $(COBCFLAGS) -o $(APP) $(SRC)

build-db2:
	$(COBC) $(COBCFLAGS) -o $(APP_DB2) $(SRC_DB2)

build-menu:
	$(COBC) $(COBCFLAGS) -o $(APP_MENU) $(SRC_MENU)

build-sql:
	@echo "🔧 Compilando programa con SQL embebido..."
	@echo "⚠️  NOTA: Requiere GnuCOBOL con soporte SQL y librerías DB2"
	$(COBC) $(SQLCOBCFLAGS) -o $(APP_SQL) $(SRC_SQL)

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

run-sql: init-db2 build-sql
	@mkdir -p data
	@echo "🏦 Ejecutando MINIBANK con SQL embebido..."
	@./$(APP_SQL) && \
	echo "✅ Hecho. Salida en data/balances-sql.csv"

init-db2:
	@python3 .devcontainer/init-db2.py
	@python3 .devcontainer/load-sample-data.py

clean:
	rm -f $(APP) $(APP_DB2) $(APP_MENU) $(APP_SQL)
