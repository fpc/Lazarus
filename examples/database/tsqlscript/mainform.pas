unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, StdCtrls,
  dbconfiggui, dbconfig,
  {General db unit}sqldb,
  {For EDataBaseError}db,
  {Now we add all databases we want to support, otherwise their drivers won't be loaded}
  {$IFNDEF Solaris}IBConnection,{$ENDIF}pqconnection,sqlite3conn,
  mssqlconn,mysql50conn,mysql51conn,mysql55conn,odbcconn,oracleconnection,
  sqlscript {the unit that contains tsqlscript};

type

  { TForm1 }

  TForm1 = class(TForm)
    CmdCopyDDL: TButton;
    CmdOpenSQL: TButton;
    CmdCopyDML: TButton;
    CmdRunScript: TButton;
    OpenDialog1: TOpenDialog;
    ScriptMemo: TMemo;
    procedure CmdCopyDDLClick(Sender: TObject);
    procedure CmdCopyDMLClick(Sender: TObject);
    procedure CmdOpenSQLClick(Sender: TObject);
    procedure CmdRunScriptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FConn: TSQLConnector;
    FQuery: TSQLQuery;
    FTran: TSQLTransaction;
    // Run database connection test when asked
    function ConnectionTest(ChosenConfig: TDBConnectionConfig): boolean;
    // Display script error to the user
    procedure ShowScriptException(Sender: TObject; Statement: TStrings; TheException: Exception; var Continue: boolean);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  LoginForm: TDBConfigForm;
begin
  FConn:=TSQLConnector.Create(nil);
  FQuery:=TSQLQuery.Create(nil);
  FTran:=TSQLTransaction.Create(nil);
  FConn.Transaction:=FTran;
  FQuery.DataBase:=FConn;

  LoginForm:=TDBConfigForm.Create(self);
  try
    // The test button on dbconfiggui will link to this procedure:
    LoginForm.ConnectionTestCallback:=@ConnectionTest;
    LoginForm.ConnectorType.Clear; //remove any default connectors
    // Now add the dbs that you support - use the name of the *ConnectionDef.TypeName property
    {$IFNDEF Solaris}
    // Not available on Solaris
    LoginForm.ConnectorType.AddItem('Firebird', nil);
    {$ENDIF}
    LoginForm.ConnectorType.AddItem('MSSQLServer', nil);
    LoginForm.ConnectorType.AddItem('MySQL50', nil);
    LoginForm.ConnectorType.AddItem('MySQL51', nil);
    LoginForm.ConnectorType.AddItem('MySQL55', nil);
    LoginForm.ConnectorType.AddItem('ODBC', nil);
    LoginForm.ConnectorType.AddItem('Oracle', nil);
    LoginForm.ConnectorType.AddItem('PostGreSQL', nil);
    LoginForm.ConnectorType.AddItem('SQLite3', nil);
    LoginForm.ConnectorType.AddItem('Sybase', nil);
    case LoginForm.ShowModal of
    mrOK:
      begin
        // User wants to connect, so copy over db info
        FConn.ConnectorType:=LoginForm.Config.DBType;
        FConn.HostName:=LoginForm.Config.DBHost;
        FConn.DatabaseName:=LoginForm.Config.DBPath;
        FConn.UserName:=LoginForm.Config.DBUser;
        FConn.Password:=LoginForm.Config.DBPassword;
        FConn.Transaction:=FTran;
      end;
    mrCancel:
      begin
        ShowMessage('You canceled the database login. Application will terminate.');
        Application.Terminate;
      end;
    end;
  finally
    LoginForm.Free;
  end;
  // Get a script before the user's eyes:
  CmdCopyDDLClick(nil);
end;

procedure TForm1.CmdCopyDDLClick(Sender: TObject);
// Script that sets up tables as used in SQLdb_Tutorial1..3
// Also includes a photo blob used in the LazReport tutorial
// Notice we include 2 SQL statements, each terminated with ;
const ScriptText=
  'CREATE TABLE CUSTOMER '+LineEnding+
  '( '+LineEnding+
  '  CUST_NO INTEGER NOT NULL, '+LineEnding+
  '  CUSTOMER VARCHAR(25) NOT NULL, '+LineEnding+
  '  CITY VARCHAR(25), '+LineEnding+
  '  COUNTRY VARCHAR(15), '+LineEnding+
  '  CONSTRAINT CT_CUSTOMER_PK PRIMARY KEY (CUST_NO) '+LineEnding+
  '); '+LineEnding+
  'CREATE TABLE EMPLOYEE '+LineEnding+
  '( '+LineEnding+
  '  EMP_NO INTEGER NOT NULL, '+LineEnding+
  '  FIRST_NAME VARCHAR(15) NOT NULL, '+LineEnding+
  '  LAST_NAME VARCHAR(20) NOT NULL, '+LineEnding+
  '  PHONE_EXT VARCHAR(4), '+LineEnding+
  '  JOB_CODE VARCHAR(5) NOT NULL, '+LineEnding+
  '  JOB_GRADE INTEGER NOT NULL, '+LineEnding+
  '  JOB_COUNTRY VARCHAR(15) NOT NULL, '+LineEnding+
  '  SALARY NUMERIC(10,2) NOT NULL, '+LineEnding+
  '  PHOTO BLOB SUB_TYPE BINARY, '+LineEnding+
  '  CONSTRAINT CT_EMPLOYEE_PK PRIMARY KEY (EMP_NO) '+LineEnding+
  ');';
begin
  Scriptmemo.Lines.Text:=ScriptText;
end;

procedure TForm1.CmdCopyDMLClick(Sender: TObject);
// Script that fills the table with sample data as used in SQLdb_Tutorial1..3
// The double quotes inside the statements will be parsed by the Pascal compiler and
// end up as single quotes in the actual ScriptText string, like SQL expects it.
const ScriptText=
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (1, ''Michael Design'', ''San Diego'', ''USA''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (2, ''VC Technologies'', ''Dallas'', ''USA''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (3, ''Klämpfl, Van Canneyt'', ''Boston'', ''USA''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (4, ''Felipe Bank'', ''Manchester'', ''England''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (5, ''Joost Systems, LTD.'', ''Central Hong Kong'', ''Hong Kong''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (6, ''Van der Voort Int.'', ''Ottawa'', ''Canada''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (7, ''Mrs. Mauvais'', ''Pebble Beach'', ''USA''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (8, ''Asinine Vacation Rentals'', ''Lihue'', ''USA''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (9, ''Fax'', ''Turtle Island'', ''Fiji''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (10, ''FPC Corporation'', ''Tokyo'', ''Japan''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (11, ''Dynamic Intelligence Corp'', ''Zurich'', ''Switzerland''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (12, ''3D-Pad Corp.'', ''Paris'', ''France''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (13, ''Swen Export, Ltd.'', ''Milan'', ''Italy''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (14, ''Graeme Consulting'', ''Brussels'', ''Belgium''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (15, ''Klenin Inc.'', ''Den Haag'', ''Netherlands''); '+LineEnding+
  'INSERT INTO employee(emp_no, first_name, last_name, phone_ext, job_code, job_grade,  '+LineEnding+
  '  job_country, salary) '+LineEnding+
  '  VALUES (1,''William'',''Shatner'',''1702'',''CEO'',1,''USA'',48000); '+LineEnding+
  'INSERT INTO employee(emp_no, first_name, last_name, phone_ext, job_code, job_grade,  '+LineEnding+
  '  job_country, salary) '+LineEnding+
  '  VALUES (2,''Ivan'',''Rzeszow'',''9802'',''Eng'',2,''Russia'',38000); '+LineEnding+
  'INSERT INTO employee(emp_no, first_name, last_name, phone_ext, job_code, job_grade,  '+LineEnding+
  '  job_country, salary) '+LineEnding+
  '  VALUES (3,''Erin'',''Powell'',''1703'',''Admin'',2,''USA'',45368); ';
begin
  Scriptmemo.Lines.Text:=ScriptText;
end;

procedure TForm1.CmdOpenSQLClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ScriptMemo.Clear;
    ScriptMemo.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TForm1.CmdRunScriptClick(Sender: TObject);
// The heart of the program: runs the script in the memo
var
  OurScript: TSQLScript;
begin
  OurScript:=TSQLScript.Create(nil);
  try
    OurScript.Database:=FConn; //Indicate what db & ...
    OurScript.Transaction:=FTran; // ... transaction we actually want to run the script against
    OurScript.Script.Assign(ScriptMemo.Lines); //Copy over the script itself
    //Now set some options:
    OurScript.UseCommit:=true; //try process any COMMITs inside the script, instead of batching everything together. See readme.txt though
    //SET TERM is Firebird specific, used when creating stored procedures etc.
    if FConn.ConnectorType='Firebird' then
      OurScript.UseSetTerm:=true
    else
      OurScript.UseSetTerm:=false;
    OurScript.CommentsInSQL:=false; //Send commits to db server as well; could be useful to troubleshoot by monitoring all SQL statements at the server
    OurScript.OnException:=@ShowScriptException; //when errors occur, let this procedure handle the error display
    try
      if not(FTran.Active) then
        FTran.StartTransaction; //better safe than sorry
      OurScript.Execute;
      FTran.Commit; //Make sure entire script is committed to the db
      ShowMessage('Script was successfully run.');
    except
      on E: EDataBaseError do
      begin
        // Error was already displayed via ShowScriptException, so no need for this:
        //ShowMessage('Error running script: '+E.Message);
        FTran.Rollback;
      end;
    end;
  finally
    OurScript.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FQuery.Free;
  FTran.Free;
  FConn.Free;
end;

function TForm1.ConnectionTest(ChosenConfig: TDBConnectionConfig): boolean;
// Callback function that uses the info in dbconfiggui to test a connection
// and return the result of the test to dbconfiggui
var
  // Generic database connector...
  Conn: TSQLConnector;
begin
  result:=false;
  Conn:=TSQLConnector.Create(nil);
  Screen.BeginWaitCursor;
  try
    // ...actual connector type is determined by this property.
    // Make sure the ChosenConfig.DBType string matches
    // the connectortype (e.g. see the string in the
    // T*ConnectionDef.TypeName for that connector .
    Conn.ConnectorType:=ChosenConfig.DBType;
    Conn.HostName:=ChosenConfig.DBHost;
    Conn.DatabaseName:=ChosenConfig.DBPath;
    Conn.UserName:=ChosenConfig.DBUser;
    Conn.Password:=ChosenConfig.DBPassword;
    try
      Conn.Open;
      result:=Conn.Connected;
    except
      // Result is already false
    end;
    Conn.Close;
  finally
    Screen.EndWaitCursor;
    Conn.Free;
  end;
end;

procedure TForm1.ShowScriptException(Sender: TObject; Statement: TStrings;
  TheException: Exception; var Continue: boolean);
begin
  // Shows script execution error to user
  // todo: should really be a separate form with a memo big enough to display a large statement
  ShowMessage('Script error: '+TheException.Message+LineEnding+
    Statement.Text);
  Continue:=false; //indicate script should stop
end;

end.

