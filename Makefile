APP=src/minibank
APP_DB2=src/minibank-db2
APP_MENU=src/minibank-menu
SRC=src/minibank.cob
SRC_DB2=src/minibank-db2.cob
SRC_MENU=src/minibank-menu.cob
COBC=cobc
COBCFLAGS=-x -Wall -O2 -I src/copybooks

.PHONY: build build-db2 build-menu run run-db2 run-menu init-db2 clean

build:
	$(COBC) $(COBCFLAGS) -o $(APP) $(SRC)

build-db2:
	$(COBC) $(COBCFLAGS) -o $(APP_DB2) $(SRC_DB2)

build-menu:
	$(COBC) $(COBCFLAGS) -o $(APP_MENU) $(SRC_MENU)

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

init-db2:
	@python3 .devcontainer/init-db2.py
	@python3 .devcontainer/load-sample-data.py

clean:
	rm -f $(APP) $(APP_DB2) $(APP_MENU)
