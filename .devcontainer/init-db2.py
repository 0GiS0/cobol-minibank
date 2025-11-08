#!/usr/bin/env python3
"""
Script para inicializar tablas en DB2
"""

import ibm_db
import sys

# Configuración
DB_CONFIG = {
    'database': 'minibank',
    'hostname': 'localhost',
    'port': 50000,
    'user': 'db2inst1',
    'password': 'password'
}

def init_db2():
    """Inicializar DB2 con tablas"""
    try:
        print("🔌 Conectando a DB2...")
        dsn = (f"DATABASE={DB_CONFIG['database']};HOSTNAME={DB_CONFIG['hostname']};"
               f"PORT={DB_CONFIG['port']};UID={DB_CONFIG['user']};PWD={DB_CONFIG['password']};")
        conn = ibm_db.connect(dsn, "", "")
        print("✅ Conectado a DB2")

        # Eliminar tablas existentes (para comenzar limpio)
        try:
            print("🗑️  Eliminando tablas existentes...")
            ibm_db.exec_immediate(conn, "DROP TABLE TRANSACTIONS")
        except:
            pass

        try:
            ibm_db.exec_immediate(conn, "DROP TABLE ACCOUNTS")
        except:
            pass

        # Crear tabla ACCOUNTS
        print("📊 Creando tabla ACCOUNTS...")
        sql_accounts = """
        CREATE TABLE ACCOUNTS (
            ACCOUNT_ID INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
            ACCOUNT_NAME VARCHAR(30) NOT NULL,
            BALANCE DECIMAL(15,2) DEFAULT 0.00,
            CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UPDATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """
        ibm_db.exec_immediate(conn, sql_accounts)
        print("✅ Tabla ACCOUNTS creada")

        # Crear tabla TRANSACTIONS
        print("📝 Creando tabla TRANSACTIONS...")
        sql_transactions = """
        CREATE TABLE TRANSACTIONS (
            TRANSACTION_ID INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
            ACCOUNT_ID INT NOT NULL,
            TRANSACTION_DATE DATE NOT NULL,
            TRANSACTION_TYPE VARCHAR(10) NOT NULL,
            AMOUNT DECIMAL(15,2) NOT NULL,
            CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS(ACCOUNT_ID)
        )
        """
        ibm_db.exec_immediate(conn, sql_transactions)
        print("✅ Tabla TRANSACTIONS creada")

        # Crear índices
        print("📑 Creando índices...")
        ibm_db.exec_immediate(conn, "CREATE INDEX IDX_TRANS_ACCOUNT ON TRANSACTIONS(ACCOUNT_ID)")
        ibm_db.exec_immediate(conn, "CREATE INDEX IDX_TRANS_DATE ON TRANSACTIONS(TRANSACTION_DATE)")
        print("✅ Índices creados")

        # Commit
        ibm_db.commit(conn)
        print("✅ Cambios confirmados")

        # Listar tablas
        print("\n📋 Tablas disponibles:")
        sql = "SELECT TABNAME FROM SYSCAT.TABLES WHERE OWNER = 'DB2INST1'"
        stmt = ibm_db.exec_immediate(conn, sql)
        result = ibm_db.fetch_assoc(stmt)
        while result:
            print(f"  - {result['TABNAME']}")
            result = ibm_db.fetch_assoc(stmt)

        ibm_db.close(conn)
        print("\n✅ Inicialización completada exitosamente")
        return True

    except Exception as e:
        print(f"❌ Error: {e}")
        return False

if __name__ == "__main__":
    success = init_db2()
    sys.exit(0 if success else 1)
