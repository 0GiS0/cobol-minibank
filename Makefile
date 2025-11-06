APP=src/minibank
APP_DB2=src/minibank-db2
SRC=src/minibank.cob
SRC_DB2=src/minibank-db2.cob
COBC=cobc
COBCFLAGS=-x -Wall -O2 -I src/copybooks

.PHONY: build build-db2 run run-db2 clean

build:
	$(COBC) $(COBCFLAGS) -o $(APP) $(SRC)

build-db2:
	$(COBC) $(COBCFLAGS) -o $(APP_DB2) $(SRC_DB2)

run: build
	@mkdir -p data
	@./$(APP) && \
	echo "✅ Hecho. Salida en data/balances.csv"

run-db2: build-db2
	@mkdir -p data
	@./$(APP_DB2) && \
	echo "✅ Hecho. Salida en data/balances.csv"

clean:
	rm -f $(APP) $(APP_DB2)
