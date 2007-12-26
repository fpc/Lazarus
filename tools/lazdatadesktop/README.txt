This is the Lazarus database desktop, a database access tool for Lazarus

Current Features:
=================

- Connect to 
  DBase
  Firebird/Interbase
  MySQL 4.0, 4.1, 5.0
  PostGreSQL
  Oracle
  SQLite
  Any ODBC supported database

- Create data dictionaries for use in the Lazarus IDE or in your programs
  (make sure you install the lazdatadict package)
  - Create SQL queries from the data dictionary
  - Reverse engineer databases

- Export data to a variety of formats:
  - Text file with CSV (comma-separated values) 
  - Text file with Fixed-length fields 
  - DBF files
  - XML files
  - JSON files
  - RTF files
  - LaTeX tables
  - SQL insert/update statements

- Create Object Pascal code:
  - Transform a SQL statement to a pascal constant
  - Transform a SQL statement to a stringlist.
  - Create code to create a DBF file which can contain the result of your
    query 
  - Create a Class with properties corresponding to the fields in your query.
    Additionally, create a list corresponding with this class and code to
    load it.
  - Create a tiOPF class and associated visitors.

And more features to come.

Compilation currently requires fpc 2.3.1 (5 December 2007).
