TSQLScript
==========

This directory shows how to use TSQLScript to run a batch of SQL statements.

TSQLScript ican be used to run multiple SQL statements - terminated by ; - after each other.
It is provided by FPC's SQLDB database layer and available in Lazarus.

Notes:
- You must/should have created an empty database on your server/embedded database system first. The scripts will try to create tables and insert sample data.
- FPC 2.6.x versions currently have a bug that prevents running statements with : in them (e.g. Firebird stored procedure creation). FPC trunk/development version revision 26112 has fixed this, and it may be backported to 2.6.x; please check release notes and documentation.
- Firebird DDL (e.g. table creation) and DML (e.g. inserting data) must be separated by a COMMIT. This may also apply to other databases. FPC bug 17829 tracks this, but FPC 2.6.x or trunk currently contains no fix.
A workaround is to split the script into 2, see the sample program.
- The logon form is taken from SQLdb_Tutorial3.

Incidentally, it sets up a database with tables and sample data for the Lazarus wiki tutorials:
http://wiki.lazarus.freepascal.org/SQLdb_Tutorial0
http://wiki.lazarus.freepascal.org/SQLdb_Tutorial1
http://wiki.lazarus.freepascal.org/SQLdb_Tutorial2
http://wiki.lazarus.freepascal.org/SQLdb_Tutorial3

Please see the SQLdb_Tutorial0 article for instructions and requirements.
(You'll need database clients and a sample database; see the article)