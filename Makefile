APP=src/minibank
SRC=src/minibank.cob
COBC=cobc
COBCFLAGS=-x -Wall -O2 -I src/copybooks

build:
	$(COBC) $(COBCFLAGS) -o $(APP) $(SRC)

run:
	@mkdir -p data
	@./$(APP) && \
	echo "✅ Hecho. Salida en data/balances.csv"

clean:
	rm -f $(APP)