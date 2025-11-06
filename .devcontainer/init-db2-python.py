#!/usr/bin/env python3
"""
🗄️ Inicializar DB2 con tablas para MiniBank
Crea las tablas ACCOUNTS y TRANSACTIONS si no existen
"""

import ibm_db
import sys

# Configuración de conexión
DB_CONFIG = {
    'database': 'minibank',
    'hostname': 'db',
    'port': 50000,
    'user': 'db2inst1',
    'password': 'password'
}

def connect_db2():
    """Conectar a DB2"""
    try:
        dsn = (f"DATABASE={DB_CONFIG['database']};HOSTNAME={DB_CONFIG['hostname']};"
               f"PORT={DB_CONFIG['port']};UID={DB_CONFIG['user']};PWD={DB_CONFIG['password']};")
        conn = ibm_db.connect(dsn, "", "")
        return conn
    except Exception as e:
        print(f"❌ Error conectando a DB2: {e}")
        return None

def create_tables(conn):
    """Crear las tablas si no existen"""
    try:
        print("📊 Creando tablas...")

        # Intentar eliminar tablas si existen (para empezar limpio)
        try:
            ibm_db.exec_immediate(conn, "DROP TABLE TRANSACTIONS")
            print("  ✓ Tabla TRANSACTIONS eliminada")
        except:
            pass

        try:
            ibm_db.exec_immediate(conn, "DROP TABLE ACCOUNTS")
            print("  ✓ Tabla ACCOUNTS eliminada")
        except:
            pass

        # Crear tabla ACCOUNTS
        sql_accounts = """
        CREATE TABLE ACCOUNTS (
            ACCOUNT_ID INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
            ACCOUNT_NAME VARCHAR(100) NOT NULL,
            BALANCE DECIMAL(15,2) DEFAULT 0.00,
            CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UPDATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """
        ibm_db.exec_immediate(conn, sql_accounts)
        print("  ✓ Tabla ACCOUNTS creada")

        # Crear tabla TRANSACTIONS
        sql_transactions = """
        CREATE TABLE TRANSACTIONS (
            TRANSACTION_ID INT NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
            ACCOUNT_ID INT NOT NULL,
            TRANSACTION_DATE DATE NOT NULL,
            TRANSACTION_TYPE VARCHAR(10) NOT NULL,
            AMOUNT DECIMAL(15,2) NOT NULL,
            CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS(ACCOUNT_ID) ON DELETE CASCADE
        )
        """
        ibm_db.exec_immediate(conn, sql_transactions)
        print("  ✓ Tabla TRANSACTIONS creada")

        # Crear índices
        try:
            ibm_db.exec_immediate(conn, "CREATE INDEX IDX_TRANS_ACCOUNT ON TRANSACTIONS(ACCOUNT_ID)")
            print("  ✓ Índice IDX_TRANS_ACCOUNT creado")
        except:
            pass

        try:
            ibm_db.exec_immediate(conn, "CREATE INDEX IDX_TRANS_DATE ON TRANSACTIONS(TRANSACTION_DATE)")
            print("  ✓ Índice IDX_TRANS_DATE creado")
        except:
            pass

        ibm_db.commit(conn)
        print("✅ Tablas creadas exitosamente\n")
        return True

    except Exception as e:
        print(f"❌ Error creando tablas: {e}\n")
        return False

def main():
    print("🗄️ ════════════════════════════════════════════════")
    print("   INICIALIZANDO DB2 PARA MINIBANK")
    print("════════════════════════════════════════════════\n")

    # Conectar
    conn = connect_db2()
    if not conn:
        sys.exit(1)

    print("✅ Conectado a DB2\n")

    # Crear tablas
    if not create_tables(conn):
        ibm_db.close(conn)
        sys.exit(1)

    # Cerrar conexión
    ibm_db.close(conn)
    print("✅ Inicialización completada")

if __name__ == "__main__":
    main()
