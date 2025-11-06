#!/usr/bin/env python3
"""
🔗 DB2 Interface for COBOL MiniBank
Proporciona operaciones SQL simples para COBOL vía línea de comandos
"""

import sys
import os
import ibm_db
from datetime import datetime

# Configuración de conexión
DB_CONFIG = {
    'database': 'minibank',
    'hostname': 'db',
    'port': 50000,
    'user': 'db2inst1',
    'password': 'password'
}

conn = None

def connect_db2():
    """Conectar a DB2"""
    global conn
    try:
        dsn = (f"DATABASE={DB_CONFIG['database']};HOSTNAME={DB_CONFIG['hostname']};"
               f"PORT={DB_CONFIG['port']};UID={DB_CONFIG['user']};PWD={DB_CONFIG['password']};")
        conn = ibm_db.connect(dsn, "", "")
        # No mostrar mensaje en modo silencioso
        if os.environ.get('DB2_SILENT') != '1':
            print("✅ Conectado a DB2 exitosamente")
        return True
    except Exception as e:
        print(f"❌ Error conectando a DB2: {e}", file=sys.stderr)
        return False

def disconnect_db2():
    """Desconectar de DB2"""
    global conn
    if conn:
        ibm_db.close(conn)
        if os.environ.get('DB2_SILENT') != '1':
            print("✅ Desconexión exitosa")

def insert_transaction(account_name, tx_date, tx_type, amount):
    """Insertar una transacción"""
    if not conn:
        print("❌ No hay conexión a DB2", file=sys.stderr)
        return False

    try:
        # Primero, obtener el ID de la cuenta (o crear si no existe)
        sql_check = f"SELECT ACCOUNT_ID FROM ACCOUNTS WHERE ACCOUNT_NAME = '{account_name}'"
        stmt = ibm_db.exec_immediate(conn, sql_check)
        result = ibm_db.fetch_assoc(stmt)

        if result:
            account_id = result['ACCOUNT_ID']
        else:
            # Crear la cuenta si no existe
            sql_create = (f"INSERT INTO ACCOUNTS (ACCOUNT_NAME, BALANCE) "
                         f"VALUES ('{account_name}', 0)")
            ibm_db.exec_immediate(conn, sql_create)
            if os.environ.get('DB2_SILENT') != '1':
                print(f"✅ Cuenta creada: {account_name}")

            # Obtener el ID de la cuenta recién creada
            stmt = ibm_db.exec_immediate(conn, sql_check)
            result = ibm_db.fetch_assoc(stmt)
            account_id = result['ACCOUNT_ID']

        # Insertar la transacción
        sql_insert = (f"INSERT INTO TRANSACTIONS "
                     f"(ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT) "
                     f"VALUES ({account_id}, '{tx_date}', '{tx_type}', {amount})")
        ibm_db.exec_immediate(conn, sql_insert)

        # Actualizar el saldo de la cuenta
        if tx_type == 'CREDIT':
            sql_update = f"UPDATE ACCOUNTS SET BALANCE = BALANCE + {amount} WHERE ACCOUNT_ID = {account_id}"
        else:  # DEBIT
            sql_update = f"UPDATE ACCOUNTS SET BALANCE = BALANCE - {amount} WHERE ACCOUNT_ID = {account_id}"

        ibm_db.exec_immediate(conn, sql_update)
        if os.environ.get('DB2_SILENT') != '1':
            print(f"✅ Transacción insertada: {account_name} {tx_type} {amount}")
        return True

    except Exception as e:
        print(f"❌ Error insertando transacción: {e}", file=sys.stderr)
        return False

def get_balances():
    """Obtener todos los saldos"""
    if not conn:
        print("❌ No hay conexión a DB2", file=sys.stderr)
        return []

    try:
        sql = "SELECT ACCOUNT_NAME, BALANCE FROM ACCOUNTS ORDER BY ACCOUNT_NAME"
        stmt = ibm_db.exec_immediate(conn, sql)

        balances = []
        result = ibm_db.fetch_assoc(stmt)
        while result:
            balances.append({
                'account': result['ACCOUNT_NAME'],
                'balance': result['BALANCE']
            })
            result = ibm_db.fetch_assoc(stmt)

        return balances
    except Exception as e:
        print(f"❌ Error consultando saldos: {e}", file=sys.stderr)
        return []

def main():
    if len(sys.argv) < 2:
        print("Uso: db2-interface.py <comando> [argumentos]")
        print("Comandos:")
        print("  connect                           - Conectar a DB2")
        print("  disconnect                        - Desconectar de DB2")
        print("  insert <cuenta> <fecha> <tipo> <monto> - Insertar transacción")
        print("  balances                          - Mostrar todos los saldos")
        sys.exit(1)

    command = sys.argv[1]

    if command == "connect":
        connect_db2()
    elif command == "disconnect":
        disconnect_db2()
    elif command == "insert":
        if len(sys.argv) != 6:
            print("❌ insert requiere: <cuenta> <fecha> <tipo> <monto>")
            sys.exit(1)
        account = sys.argv[2]
        date = sys.argv[3]
        tx_type = sys.argv[4]
        amount = sys.argv[5]
        connect_db2()
        insert_transaction(account, date, tx_type, amount)
        disconnect_db2()
    elif command == "balances":
        connect_db2()
        balances = get_balances()
        for item in balances:
            print(f"{item['account']},{item['balance']}")
        disconnect_db2()
    else:
        print(f"❌ Comando desconocido: {command}")
        sys.exit(1)

if __name__ == "__main__":
    main()
