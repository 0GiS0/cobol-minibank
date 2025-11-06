#!/usr/bin/env python3
"""
📊 Cargar datos de ejemplo en DB2
Script para insertar cuentas y transacciones de ejemplo
"""

import ibm_db
import sys
from datetime import datetime, timedelta

# Configuración
DB_CONFIG = {
    'database': 'minibank',
    'hostname': 'db',
    'port': 50000,
    'user': 'db2inst1',
    'password': 'password'
}

def load_sample_data():
    """Cargar datos de ejemplo en DB2"""
    try:
        print("🔌 Conectando a DB2...")
        dsn = (f"DATABASE={DB_CONFIG['database']};HOSTNAME={DB_CONFIG['hostname']};"
               f"PORT={DB_CONFIG['port']};UID={DB_CONFIG['user']};PWD={DB_CONFIG['password']};")
        conn = ibm_db.connect(dsn, "", "")
        print("✅ Conectado a DB2\n")

        # Limpiar datos anteriores
        print("🗑️  Limpiando datos anteriores...")
        try:
            ibm_db.exec_immediate(conn, "DELETE FROM TRANSACTIONS")
            ibm_db.exec_immediate(conn, "DELETE FROM ACCOUNTS")
            print("  ✓ Datos anteriores eliminados\n")
        except:
            pass

        # Insertar cuentas
        print("📊 Insertando cuentas...")
        accounts = [
            ('Cuenta Corriente Juan', 1000.00),
            ('Cuenta Ahorro María', 5000.00),
            ('Cuenta Inversión Pedro', 2500.00),
        ]

        account_ids = [1, 2, 3]  # Los IDs serán auto-generados pero sabemos cuáles son

        for idx, (account_name, balance) in enumerate(accounts):
            sql = f"""
            INSERT INTO ACCOUNTS (ACCOUNT_NAME, BALANCE)
            VALUES ('{account_name}', {balance})
            """
            ibm_db.exec_immediate(conn, sql)
            print(f"  ✓ {account_name} (Saldo: ${balance:,.2f})")

        print()

        # Insertar transacciones
        print("💳 Insertando transacciones...")

        base_date = datetime.now() - timedelta(days=30)
        transactions = [
            (1, base_date + timedelta(days=0), 'CREDIT', 500.00, 'Depósito inicial'),
            (1, base_date + timedelta(days=2), 'DEBIT', 150.25, 'Retiro cajero'),
            (1, base_date + timedelta(days=5), 'CREDIT', 200.00, 'Transferencia recibida'),
            (1, base_date + timedelta(days=8), 'DEBIT', 75.30, 'Pago servicios'),
            (2, base_date + timedelta(days=1), 'CREDIT', 1000.00, 'Depósito'),
            (2, base_date + timedelta(days=3), 'DEBIT', 50.75, 'Compra tienda'),
            (2, base_date + timedelta(days=6), 'CREDIT', 100.00, 'Reembolso'),
            (2, base_date + timedelta(days=10), 'DEBIT', 200.50, 'Pago factura'),
            (3, base_date + timedelta(days=2), 'CREDIT', 1500.00, 'Depósito inversión'),
            (3, base_date + timedelta(days=7), 'DEBIT', 350.50, 'Retiro parcial'),
            (3, base_date + timedelta(days=12), 'CREDIT', 450.00, 'Intereses ganados'),
        ]

        for account_id, trans_date, trans_type, amount, description in transactions:
            sql = f"""
            INSERT INTO TRANSACTIONS (ACCOUNT_ID, TRANSACTION_DATE, TRANSACTION_TYPE, AMOUNT)
            VALUES ({account_id}, '{trans_date.strftime("%Y-%m-%d")}', '{trans_type}', {amount})
            """
            ibm_db.exec_immediate(conn, sql)
            print(f"  ✓ Cuenta {account_id}: {trans_type:6} ${amount:>10.2f} ({description})")

        print()

        # Commit
        ibm_db.commit(conn)
        print("✅ Datos cargados exitosamente\n")

        # Mostrar resumen
        print("📋 RESUMEN DE CUENTAS:")
        print("─" * 60)
        sql = "SELECT ACCOUNT_ID, ACCOUNT_NAME, BALANCE FROM ACCOUNTS ORDER BY ACCOUNT_ID"
        stmt = ibm_db.exec_immediate(conn, sql)
        result = ibm_db.fetch_assoc(stmt)
        while result:
            acc_id = result['ACCOUNT_ID']
            print(f"  ID {acc_id}: {result['ACCOUNT_NAME']:30} - ${float(result['BALANCE']):>10.2f}")
            result = ibm_db.fetch_assoc(stmt)

        print()
        print("📊 TOTAL DE TRANSACCIONES:")
        print("─" * 60)
        sql = "SELECT COUNT(*) FROM TRANSACTIONS"
        stmt = ibm_db.exec_immediate(conn, sql)
        result = ibm_db.fetch_tuple(stmt)
        if result:
            print(f"  Total: {result[0]} transacciones cargadas")

        ibm_db.close(conn)
        print("\n✅ Operación completada exitosamente")
        return True

    except Exception as e:
        print(f"❌ Error: {e}")
        return False

if __name__ == "__main__":
    success = load_sample_data()
    sys.exit(0 if success else 1)
