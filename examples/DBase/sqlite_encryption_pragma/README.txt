////////////////////////////////////////////////////
//                                                //
// Simple SQLite3 Demo with Encryption and Pragma //
//                                                //
////////////////////////////////////////////////////
//                                                //
//    CONTENTS                                    //
//                                                //
//    1. OVERVIEW OF THIS DEMONSTRATION           //
//    2. REQUIREMENTS FOR SQLITE ENCRYPTION       //
//    3. SQLITE PRAGMA STATEMENTS                 //
//    4. ADDITIONAL SQLITE RESOURCES              //
//                                                //
////////////////////////////////////////////////////



1. OVERVIEW OF THIS DEMONSTRATION

This application very simply demonstrates the following capabilities:
- Creation of an SQLite3 Database
- Encrypting the database using a key
- Changing (or setting if not initially set) the encryption key for the 
  database after it has been created
- Creation of a database table
- Creating an Index
- Adding a row of data to the table
- Performing a very basic query
- Setting and reading various database metadata (Pragma)

The application makes a new database file "new.db" within the local 
directory.

I highly recommend using a third party SQLite Database Management Tool to 
verify the table and index creation and encryption of your database. 
You'll want to use one that supports SQLite 3.6.8 or later. I use SQLite2009 
Pro Enterprise Manager. This and other tools can be found at:
http://www.sqlite.org/cvstrac/wiki?p=ManagementTools




2. REQUIREMENTS FOR SQLITE ENCRYPTION

Since version 3.6.8 SQLite has supported the option of database encryption 
(though it must be supported specifically by the version of the sqlite3.dll 
you use for your application). The entire database, except for bytes 16 
through 23, will be encrypted. See the additional resources at the end of 
this document for more details about the SQLite Database Header, and these 
specific bytes.

Using the following link, you can find a few options for versions of SQLite 
that provide support for encryption. Since I'm working mainly on Windows, I 
opted for the Open Source System.Data.SQLite
http://wiki.freepascal.org/sqlite#Support_for_SQLite_encryption

I used the "Precompiled Binaries for 32-bit Windows (.NET Framework 3.5 SP1)" 
and renamed SQLite.Interop.dll to sqlite3.dll, but you should be able to use 
any version of applicable SQLite DLL you want as long as you have the 
required dependencies (.NET Framework and VC++ Redistributables.
http://system.data.sqlite.org/index.html/doc/trunk/www/downloads.wiki

When selecting the DLL to use, if you have the latest version of the .NET 
framework installed on your computer (4.5.1) and the Visual Studio 2013 
Redistributable Package, then you should be able to use this: 
http://system.data.sqlite.org/downloads/1.0.93.0/sqlite-netFx451-binary-x64-2013-1.0.93.0.zip
Just extract the SQLite.Interop.dll and rename it to sqlite3.dll and place 
it in the local directory. Again, you'll want to download a version that 
matches the version of .NET and Visual C++ Runtime on your computer (you 
may have many/all the versions of .NET and VC++ Redists installed).

You can download various versions of the .NET framework at:
http://msdn.microsoft.com/en-us/vstudio/aa496123.aspx

You can download various versions of the Visual C++ Redistributables at:
http://support.microsoft.com/kb/2019667


Make sure the sqlite3.dll is in the same directory as your application, 
or you *will* have errors, and your application will not work!




3. SQLITE PRAGMA STATEMENTS

SQLite Pragma are metadata variables and constants that are stored in the 
header of an SQLite Database. Most of these values are read-only or are not 
recommended to be changed, but a few of them can be set for various purposes 
in your application.

This demonstration application performs a few different Pragma operations:
- sets and reads the application_id Pragma
- sets and reads the user_version Pragma
- sets and re-sets the encryption key



Per the SQLite Documentation (edited for clarity):
The pragma user_version is used to set or get the value of the user-version.
The user-version is a big-endian 32-bit signed integer stored in the database 
header at offset 60.
The user-version is not used internally by SQLite. It may be used by 
applications for any purpose.
http://www.sqlite.org/pragma.html#pragma_schema_version

In the demo application, I've set the user_version to a constant value.
You can use any 32-bit Signed Integer value you want:
// must be a 32-bit Signed Integer (LongInt -2147483648 .. 2147483647)
  user_version = 23400001;

When we create the database, we set this value to the database:
  SQLite3Connection1.ExecuteDirect('PRAGMA user_version = ' + IntToStr(user_version) + ';');

To read the user_version from the database, we can do the following:
  SQLQuery1.SQL.Text := 'PRAGMA user_version;';
  SQLQuery1.Open;
  ShowMessage(SQLQuery1.fields[0].asString);



Per the SQLite Documentation:
The application_id PRAGMA is used to query or set the 32-bit unsigned big-endian
"Application ID" integer located at offset 68 into the database header.
Applications that use SQLite as their application file-format should set the
Application ID integer to a unique integer so that utilities such as file(1) can
determine the specific file type rather than just reporting "SQLite3 Database".
A list of assigned application IDs can be seen by consulting the magic.txt file
in the SQLite source repository. 
http://www.sqlite.org/pragma.html#pragma_application_id

In the demo application, I've set the application_id to a constant value.
You can use any 32-bit Unsigned Integer value you want. In one of my applications
I use this value to track differences in the database table structure between
different versions of my application, incrementing the application_id each time I
change the table structure with a new application version:
// must be a 32-bit Unsigned Integer (Longword 0 .. 4294967295)
  application_id = 1189021115; 

When we create the database, we set this value to the database:
  SQLite3Connection1.ExecuteDirect('PRAGMA application_id = ' + IntToStr(application_id) + ';');

To read the application_id from the database, we can do the following:
  SQLQuery1.SQL.Text := 'PRAGMA application_id;';
  SQLQuery1.Open;
  ShowMessage(SQLQuery1.fields[0].asString);



The key pragma is a little different. Using Lazarus' SQLiteConnection Component, 
we set the key with the 'password' parameter.
  SQLite3Connection1.Password := txtOld.Text;

We could also use a Pragma statement to set the key initially using the following 
when we create the database:
  SQLite3Connection1.ExecuteDirect('PRAGMA key = ' + QuotedStr(txtNew.Text) + ';');

The benefit of using the password parameter of the SQLiteConnection component 
is that it sets the key and if we want to use open and close the database 
multiple times while using the application, we don't have to keep specifying a key.

If you do not want to encrypt the database initially, simply do not provide an 
encryption key/password, or leave these values blank:
  SQLite3Connection1.Password := '';

In order to change the encryption or to remove all encryption (unencrypting the 
database) after the database has been created, we use the 'rekey' Pragma as follows:
  SQLite3Connection1.ExecuteDirect('PRAGMA rekey = ' + QuotedStr(txtNew.Text) + ';');

The double-quotes used here allow the user to leave txtNew empty, which sets the 
resulting SQL Statement to:
PRAGMA rekey = '';

Which removes all encryption (unencrypts the database).




4. ADDITIONAL SQLITE RESOURCES

To read more about the SQLite Encryption Extension (SEE), use the following URL 
(Section: How To Compile And Use SEE)
http://www.sqlite.org/see/doc/trunk/www/index.wiki

For specifics on the key and rekey Pragmas, read the section Using the "key" 
PRAGMA at the following URL:
http://www.sqlite.org/see/doc/trunk/www/readme.wiki

Details about the SQLite File Format (and particularly about the Database Header) 
can be found at:
http://www.sqlite.org/fileformat2.html#database_header

Information about the various standard database PRAGMA (metadata) statements can 
be found at:
http://www.sqlite.org/pragma.html

The methods of passing the key to the database used in this demonstration are very 
simplistic. Ideally, we would take a stronger cryptographic approach. 
Some helpful info on this topic can be found at:
https://www.owasp.org/index.php/Cheat_Sheets