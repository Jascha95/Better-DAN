
-- Some useful short cut command for database insights:

-- \dt (shows all data tables)
-- \q (disconnects from database)
-- \i <path/to/your/script> (you can run scrips via command line)
-- \h (overview of SQL Commands and meaning)
-- \? (help for psql commands)




-- This allows you to run the script multiple times, as it always delets the table (if it exists, else it ignores this statement)
DROP TABLE IF EXISTS books;


-- Create a simple table with some columns and define the column values:
CREATE TABLE IF NOT EXISTS books (
    book_id SERIAL PRIMARY KEY,
    author TEXT NOT NULL,
    title TEXT NOT NULL,
    release_year INT,
    page_number INT,
    genre TEXT
);

-- Inserting a book with all information
INSERT INTO books (author, title, release_year, page_number, genre)
VALUES ('John Doe', 'The Great Novel', 2020, 400, 'Fiction');

-- Inserting a book with minimal information
INSERT INTO books (author, title)
VALUES ('Jane Smith', 'A Short Story');

-- Inserting a book with no release year and genre
INSERT INTO books (author, title, page_number)
VALUES ('Bob Johnson', 'Mystery Book', 300);

-- Inserting two books at once
INSERT INTO books (author, title, release_year, page_number, genre)
VALUES
    ('Alice Johnson', 'Adventure Story', 2019, 250, 'Adventure'),
    ('David Williams', 'Sci-Fi Novel', 2021, 350, 'Science Fiction');

-- Look at new Table Entries
SELECT * FROM books;

-- This step only works if you are currently logged in with your owner access:
-- manage your access for the webapp_user: Granting Access to specific table
GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE books TO webapp_user;

-- Get an overview of privilege types per table
SELECT table_name, privilege_type, grantee
FROM information_schema.role_table_grants
WHERE grantee = 'webapp_user' AND table_schema = 'public';



-- Remove Granted access from a table again
REVOKE SELECT, INSERT, UPDATE, DELETE ON TABLE books FROM webapp_user;

-- This allows webapp_user to CREATE tables in the public schema
GRANT CREATE ON SCHEMA public TO webapp_user;

-- Check the privileges for the overlying public schema:
SELECT
    grantee,
    privilege_type
FROM
    information_schema.table_privileges
WHERE
    table_schema = 'public'
    AND grantee = 'webapp_user'
    AND privilege_type = 'CREATE';
    
-- With this you can revoke this privilege again.
REVOKE CREATE ON SCHEMA public FROM webapp_user;