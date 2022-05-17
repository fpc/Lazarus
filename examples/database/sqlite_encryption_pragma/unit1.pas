unit Unit1;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//   This is free and unencumbered software released into the public domain.  //
//                                                                            //
//   Anyone is free to copy, modify, publish, use, compile, sell, or          //
//   distribute this software, either in source code form or as a compiled    //
//   binary, for any purpose, commercial or non-commercial, and by any        //
//   means.                                                                   //
//                                                                            //
//   In jurisdictions that recognize copyright laws, the author or authors    //
//   of this software dedicate any and all copyright interest in the          //
//   software to the public domain. We make this dedication for the benefit   //
//   of the public at large and to the detriment of our heirs and             //
//   successors. We intend this dedication to be an overt act of              //
//   relinquishment in perpetuity of all present and future rights to this    //
//   software under copyright law.                                            //
//                                                                            //
//   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          //
//   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       //
//   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   //
//   IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR        //
//   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    //
//   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    //
//   OTHER DEALINGS IN THE SOFTWARE.                                          //
//                                                                            //
//   For more information, please refer to <http://unlicense.org/>            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////


// For this test application, I wanted to very simply try the following
// capabilities which I'll be using in a large application:
// - Creation of a SQLite3 Database
// - Creation of a database table
// - Setting various database metadata (PRAGMA)
// - Optionally encrypt the database using a key
// - Change (or set if not initially set) the encryption key for the database

// The application makes a new database file "new.db" within the local directory
// See readme.txt for installation instructions and details




{$mode objfpc}{$H+}

interface

uses
  SysUtils, db, sqldb, sqlite3conn, Forms, Dialogs, StdCtrls, ExtCtrls, DBGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnMakeNewDB: TButton;
    btnReKeyDB: TButton;
    btnViewAppID: TButton;
    btnSetAppID: TButton;
    btnViewUserVersion: TButton;
    btnSetUserVersion: TButton;
    btnAddToDB: TButton;
    btnUpdateGrid: TButton;
    btnCountRows: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Label12: TLabel;
    Shape4: TShape;
    Shape5: TShape;
    txtUser_Name: TEdit;
    txtInfo: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    txtNew: TEdit;
    txtApplication_ID: TEdit;
    Label2: TLabel;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    txtUser_Version: TEdit;
    txtPass: TEdit;
    procedure btnAddToDBClick(Sender: TObject);
    procedure btnMakeNewDBClick(Sender: TObject);
    procedure btnReKeyDBClick(Sender: TObject);
    procedure btnSetAppIDClick(Sender: TObject);
    procedure btnSetUserVersionClick(Sender: TObject);
    procedure btnViewAppIDClick(Sender: TObject);
    procedure btnViewUserVersionClick(Sender: TObject);
    procedure btnUpdateGridClick(Sender: TObject);
    procedure btnCountRowsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  const
    // More information on the use of these values is below.
    // They need not be set as constants in your application. They can be any valid value
    application_id = 1189021115; // must be a 32-bit Unsigned Integer (Longword 0 .. 4294967295)
    user_version = 23400001;  // must be a 32-bit Signed Integer (LongInt -2147483648 .. 2147483647)

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnMakeNewDBClick(Sender: TObject);
var
  newFile : Boolean;
begin

  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  // Set the password initially.
  // Could probably be done with a PRAGMA statement, but this is so much simpler
  // and once set, doesn't need to be reset every time we open the database.
  // txtPass can be left blank if you want an unencrypted database.
  SQLite3Connection1.Password := txtPass.Text;

  try

    // Since we're making this database for the first time,
    // check whether the file already exists
    newFile := not FileExists(SQLite3Connection1.DatabaseName);

    if newFile then
    begin

      // Make the database and the tables
      try
        SQLite3Connection1.Open;
        SQLTransaction1.Active := true;


        // Per the SQLite Documentation (edited for clarity):
        // The pragma user_version is used to set or get the value of the user-version.
        // The user-version is a big-endian 32-bit signed integer stored in the database header at offset 60.
        // The user-version is not used internally by SQLite. It may be used by applications for any purpose.
        // http://www.sqlite.org/pragma.html#pragma_schema_version
        SQLite3Connection1.ExecuteDirect('PRAGMA user_version = ' + IntToStr(user_version) + ';');


        // Per the SQLite Documentation:
        // The application_id PRAGMA is used to query or set the 32-bit unsigned big-endian
        // "Application ID" integer located at offset 68 into the database header.
        // Applications that use SQLite as their application file-format should set the
        // Application ID integer to a unique integer so that utilities such as file(1) can
        // determine the specific file type rather than just reporting "SQLite3 Database".
        // A list of assigned application IDs can be seen by consulting the magic.txt file
        // in the SQLite source repository. 
        // http://www.sqlite.org/pragma.html#pragma_application_id
        SQLite3Connection1.ExecuteDirect('PRAGMA application_id = ' + IntToStr(application_id) + ';');


        // Here we're setting up a table named "DATA" in the new database
        SQLite3Connection1.ExecuteDirect('CREATE TABLE "DATA"('+
                    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
                    ' "Current_Time" DateTime NOT NULL,'+
                    ' "User_Name" Char(128) NOT NULL,'+
                    ' "Info" Char(128) NOT NULL);');


        // Creating an index based upon id in the DATA Table
        SQLite3Connection1.ExecuteDirect('CREATE UNIQUE INDEX "Data_id_idx" ON "DATA"( "id" );');


        SQLTransaction1.Commit;

        ShowMessage('Succesfully created database.');

      except
        ShowMessage('Unable to Create new Database');
      end;

    end;

  except
    ShowMessage('Unable to check if database file exists');
  end;

end;

procedure TForm1.btnAddToDBClick(Sender: TObject);
begin
  SQLite3Connection1.Password := txtPass.Text; // The current password

  if (txtUser_Name.Text = '') OR (txtInfo.Text = '') then
  begin
    ShowMessage('Please enter both a Name and Info');
  end
  else
  begin

    // Attempt to add txtUser_Name and txtInfo to the database
    try
      SQLite3Connection1.Open;
      SQLTransaction1.Active := True;

      // Insert the values into the database
      // We're using ParamByName which prevents SQL Injection
      // http://wiki.freepascal.org/Working_With_TSQLQuery#Parameters_in_TSQLQuery.SQL
      SQLQuery1.SQL.Text := 'Insert into DATA (Current_Time,User_Name,Info) values (:Current_Time,:User_Name,:Info)';
      SQLQuery1.Params.ParamByName('Current_Time').AsDateTime := Now;
      SQLQuery1.Params.ParamByName('User_Name').AsString := txtUser_Name.Text;
      SQLQuery1.Params.ParamByName('Info').AsString := txtInfo.Text;
      SQLQuery1.ExecSQL;

      SQLTransaction1.Commit;

      // Clear Edit boxes
      txtUser_Name.Text := '';
      txtInfo.Text := '';

      // Now let's update the grid to show the new values to the user:
      btnUpdateGridClick(nil);
    except
      ShowMessage('Unable to add User_Name: ' + txtUser_Name.Text + ' and Info: ' + txtInfo.Text + ' to the database. Ensure database exists and password is correct.');
    end;

  end;

end;

procedure TForm1.btnReKeyDBClick(Sender: TObject);
begin

  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  SQLite3Connection1.Password := txtPass.Text; // The current password

  // Update the database key
  try
    SQLite3Connection1.Open;
    SQLTransaction1.Active := True;


    // Here we change the key.
    // We use double-quotes here so that a blank key (IE: "") can be provided if
    // you want to remove encryption from the database.
    // This is a very simplistic demonstration. Ideally, we would take a stronger cryptographic approach
    // Some helpful info on this topic can be found at:
    // https://www.owasp.org/index.php/Cheat_Sheets
    // Per SQLite Documentation:
    // Note that the hexkey, rekey and hexrekey pragmas only work with SQLite version 3.6.8 and later.
    // http://www.sqlite.org/see/doc/trunk/www/readme.wiki
    // Section: Using the "key" PRAGMA
    SQLite3Connection1.ExecuteDirect('PRAGMA rekey = ' + QuotedStr(txtNew.Text) + ';');


    SQLTransaction1.Commit;
    SQLite3Connection1.Close;

    // Transfer the password to txtPass and erase txtNew
    txtPass.Text := txtNew.Text;
    txtNew.Text := '';

    // ... and make sure we remember the new password in our sqlconnection ready
    // for reconnecting
    SQLite3Connection1.Password := txtPass.Text;

    ShowMessage('Password rekey succesful.');

  except
    ShowMessage('Unable to set the new key using: PRAGMA rekey = ' + txtNew.Text + ';');
  end;

end;

procedure TForm1.btnSetAppIDClick(Sender: TObject);
begin

  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  SQLite3Connection1.Password := txtPass.Text; // The current password

  // Try to set the application_id Pragma
  try
    SQLite3Connection1.Open;
    SQLTransaction1.Active := True;


    SQLQuery1.SQL.Text := 'PRAGMA application_id = ' + txtApplication_ID.Text + ';';
    SQLQuery1.ExecSQL;


    SQLTransaction1.Commit;
    SQLite3Connection1.Close;

    ShowMessage('SetAppID succesful');

  except
    ShowMessage('Unable to set new application_id: ' + txtApplication_ID.Text + ';');
  end;

end;

procedure TForm1.btnSetUserVersionClick(Sender: TObject);
begin

  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  SQLite3Connection1.Password := txtPass.Text; // The current password

  // Try to set the user_version Pragma
  try
    SQLite3Connection1.Open;
    SQLTransaction1.Active := True;


    SQLQuery1.SQL.Text := 'PRAGMA user_version = ' + txtUser_Version.Text + ';';
    SQLQuery1.ExecSQL;


    SQLTransaction1.Commit;
    SQLite3Connection1.Close;

    ShowMessage('SetUserVersion succesful.');

  except
    ShowMessage('Unable to set user_version: ' + txtUser_Version.Text + ';');
  end;

end;

procedure TForm1.btnViewAppIDClick(Sender: TObject);
begin

  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  SQLite3Connection1.Password := txtPass.Text; // The current password

  // Try to query database for application_id Pragma
  try
    SQLite3Connection1.Open;

    SQLQuery1.SQL.Text := 'PRAGMA application_id;';
    SQLQuery1.Open;

    // Display the resulting value
    ShowMessage('application_id is: '+SQLQuery1.fields[0].asString);

  except
    ShowMessage('Unable to display application_id');
  end;

end;

procedure TForm1.btnViewUserVersionClick(Sender: TObject);
begin

  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  SQLite3Connection1.Password := txtPass.Text; // The current password

  // Try to query database for user_version Pragma
  try
    SQLite3Connection1.Open;

    SQLQuery1.SQL.Text := 'PRAGMA user_version;';
    SQLQuery1.Open;

    // Display the resulting value
    ShowMessage('user_version is: '+SQLQuery1.fields[0].asString);

  except
    ShowMessage('Unable to display user_version');
  end;

end;

procedure TForm1.btnUpdateGridClick(Sender: TObject);
begin

  SQLite3Connection1.Password := txtPass.Text; // The current password

  // Try to perform query
  try
    SQLite3Connection1.Connected := True;

    // Set SQL text to select everything from the DATA table
    SQLQuery1.SQL.Clear;
    SQLQuery1.SQL.Text := 'Select * from DATA';
    SQLQuery1.Open;

    // Allow the DBGrid to view the results of our query
    DataSource1.DataSet := SQLQuery1;
    DBGrid1.DataSource := DataSource1;
    DBGrid1.AutoFillColumns := true;

  except
    ShowMessage('Unable to query the database');
  end;

end;

procedure TForm1.btnCountRowsClick(Sender: TObject);
begin

  SQLite3Connection1.Close; // Ensure the connection is closed when we start

  SQLite3Connection1.Password := txtPass.Text; // The current password

  // Try to perform query
  try
    SQLite3Connection1.Connected := True;

    // Set SQL text to count all rows from the DATA table
    SQLQuery1.SQL.Clear;
    SQLQuery1.SQL.Text := 'Select Count(*) from DATA';
    SQLQuery1.Open;

    // Allow the DBGrid to view the results of our query
    DataSource1.DataSet := SQLQuery1;
    DBGrid1.DataSource := DataSource1;
    DBGrid1.AutoFillColumns := true;

  except
    ShowMessage('Unable to query the database');
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  // Ensure we're using the local sqlite3.dll
  SQLiteLibraryName := 'sqlite3.dll';

  // Set the path to the database
  SQLite3Connection1.DatabaseName := 'new.db';

end;

end.

