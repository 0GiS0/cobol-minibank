-- ==========================================
-- 🏦 MiniBank DB2 Schema Initialization
-- ==========================================
-- This file initializes the DB2 tables
-- for the COBOL MiniBank application

-- Drop existing tables (if any)
DROP TABLE IF EXISTS TRANSACTIONS;
DROP TABLE IF EXISTS ACCOUNTS;

-- ==========================================
-- ACCOUNTS Table - Stores account information
-- ==========================================
CREATE TABLE ACCOUNTS (
    ACCOUNT_ID VARCHAR(30) PRIMARY KEY,
    ACCOUNT_NAME VARCHAR(100) NOT NULL,
    BALANCE DECIMAL(15,2) DEFAULT 0.00,
    CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UPDATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE ACCOUNTS IS 'Bank accounts for the MiniBank system';
COMMENT ON COLUMN ACCOUNTS.ACCOUNT_ID IS 'Unique account identifier (e.g., ACC-001)';
COMMENT ON COLUMN ACCOUNTS.BALANCE IS 'Current account balance in decimal format';

-- ==========================================
-- TRANSACTIONS Table - Transaction history
-- ==========================================
CREATE TABLE TRANSACTIONS (
    TRANSACTION_ID INT GENERATED ALWAYS AS IDENTITY,
    ACCOUNT_ID VARCHAR(30) NOT NULL,
    TRANSACTION_DATE DATE NOT NULL,
    TRANSACTION_TYPE VARCHAR(10) NOT NULL,
    AMOUNT DECIMAL(15,2) NOT NULL,
    CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (TRANSACTION_ID),
    FOREIGN KEY (ACCOUNT_ID) REFERENCES ACCOUNTS(ACCOUNT_ID) ON DELETE CASCADE
);

COMMENT ON TABLE TRANSACTIONS IS 'Transaction history for all accounts';
COMMENT ON COLUMN TRANSACTIONS.TRANSACTION_TYPE IS 'Type: CREDIT or DEBIT';

-- ==========================================
-- Indexes for performance optimization
-- ==========================================
CREATE INDEX IDX_TRANS_ACCOUNT ON TRANSACTIONS(ACCOUNT_ID);
CREATE INDEX IDX_TRANS_DATE ON TRANSACTIONS(TRANSACTION_DATE);
CREATE INDEX IDX_ACCOUNTS_UPDATED ON ACCOUNTS(UPDATED_AT);

-- ==========================================
-- Grant permissions
-- ==========================================
GRANT SELECT, INSERT, UPDATE, DELETE ON ACCOUNTS TO db2inst1;
GRANT SELECT, INSERT, UPDATE, DELETE ON TRANSACTIONS TO db2inst1;

COMMIT;
