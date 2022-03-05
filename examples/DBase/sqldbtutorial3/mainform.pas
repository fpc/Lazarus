unit mainform;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Dialogs, Grids,
  dbconfiggui,dbconfig,
  {General db unit}sqldb,
  {For EDataBaseError}db,
  {Now we add all databases we want to support, otherwise their drivers won't be loaded}
  IBConnection,pqconnection,sqlite3conn;

type

  { TForm1 }

  TForm1 = class(TForm)
    SalaryGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SalaryGridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    FConn: TSQLConnector;
    FQuery: TSQLQuery;
    FTran: TSQLTransaction;
    function ConnectionTest(ChosenConfig: TDBConnectionConfig): boolean;
    procedure LoadSalaryGrid;
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
    LoginForm.ConnectorType.AddItem('Firebird', nil);
    LoginForm.ConnectorType.AddItem('PostGreSQL', nil);
    LoginForm.ConnectorType.AddItem('SQLite3', nil); //No connectiondef object yet in FPC2.6.0
    case LoginForm.ShowModal of
    mrOK:
      begin
        //user wants to connect, so copy over db info
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
        Close;
      end;
    end;
  finally
    LoginForm.Free;
  end;

  // Now load in our db details
  LoadSalaryGrid;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FQuery.Free;
  FTran.Free;
  FConn.Free;
end;

procedure TForm1.SalaryGridValidateEntry(sender: TObject; aCol, aRow: Integer;
  const OldValue: string; var NewValue: String);
begin
  if (aCol=3) and ((aRow=1) or (aRow=2)) then
  begin
    // Allow updates to min and max salary if positive numerical data is entered
    if StrToFloatDef(NewValue,-1)>0 then
    begin
      // Storing the primary key in e.g. a hidden cell in the grid and using that in our
      // update query would be cleaner, but we can do it the hard way as well:
      FQuery.SQL.Text:='update employee set salary=:newsalary '+
        ' where first_name=:firstname and last_name=:lastname and salary=:salary ';
      FQuery.Params.ParamByName('newsalary').AsFloat:=StrToFloatDef(NewValue,0);
      FQuery.Params.ParamByName('firstname').AsString:=SalaryGrid.Cells[1,aRow];
      FQuery.Params.ParamByName('lastname').AsString:=SalaryGrid.Cells[2,aRow];
      FQuery.Params.ParamByName('salary').AsFloat:=StrToFloatDef(OldValue,0);
      FTran.StartTransaction;
      FQuery.ExecSQL;
      FTran.Commit;
      LoadSalaryGrid; //reload standard deviation
    end
    else
    begin
      showmessage('Invalid salary entered.');
      NewValue:=OldValue;
    end;
  end
  else
  begin
    // Discard edits to any other cells
    NewValue:=OldValue;
  end;
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

procedure TForm1.LoadSalaryGrid;
var
  Average: double;
  DifferencesSquared: double=0;
  Count: integer=0;
begin
  // Clean out
  SalaryGrid.BeginUpdate;
  try
    SalaryGrid.ColCount:=4;
    SalaryGrid.RowCount:=4; //Header+3 detail rows
    SalaryGrid.Clean;
    SalaryGrid.Cells[1,0]:='First name';
    SalaryGrid.Cells[2,0]:='Surname';
    SalaryGrid.Cells[3,0]:='Salary';
    SalaryGrid.Cells[0,1]:='Min';
    SalaryGrid.Cells[0,2]:='Max';
    SalaryGrid.Cells[0,3]:='StdDev';
    // Load from DB
    try
      if not(FConn.Connected) then
        FConn.Open;
      {
      // This possible query works but is slow:
      // a nasty query if there are lots of rows in employee
      // because the subqueries in the where condition are run for each employee
      // in the table.
      // We use order by to make sure the lowest salary is presented first, then
      // the highest.
      SalaryQuery.SQL.Text:='select ' +
      '    first_name, ' +
      '    last_name, ' +
      '    salary ' +
      '  from employee  ' +
      '  where  ' +
      '    salary=(select min(salary) from employee) or  ' +
      '    salary=(select max(salary) from employee) ' +
      '  order by salary ' ;
      }
      if FConn.Connected=false then
      begin
        ShowMessage('Error connecting to the database. Aborting data loading.');
        exit;
      end;

      // Lowest salary
      // Note: we would like to only retrieve 1 row, but unfortunately the SQL
      // used differs for various dbs. As we'll deal with db dependent SQL later
      // in the tutorial, we leave this for now.
      // MS SQL: 'select top 1 '...
      FQuery.SQL.Text:='select ' +
        '    e.first_name, ' +
        '    e.last_name, ' +
        '    e.salary ' +
        'from employee e ' +
        'order by e.salary asc ';
        // ISO SQL+Firebird SQL: add
        //'rows 1 '; here and below... won't work on e.g. PostgreSQL though
      FTran.StartTransaction;
      FQuery.Open;
      SalaryGrid.Cells[1,1]:=FQuery.Fields[0].AsString;
      SalaryGrid.Cells[2,1]:=FQuery.Fields[1].AsString;
      SalaryGrid.Cells[3,1]:=FQuery.Fields[2].AsString;
      FQuery.Close;
      // Always commit(retain) an opened transaction, even if only reading
      // this will allow updates by others to be seen when reading again
      FTran.Commit;

      // Highest salary
      FQuery.SQL.Text:='select ' +
        '    e.first_name, ' +
        '    e.last_name, ' +
        '    e.salary ' +
        'from employee e ' +
        'order by e.salary desc ';
      FTran.StartTransaction;
      FQuery.Open;
      SalaryGrid.Cells[1,2]:=FQuery.Fields[0].AsString;
      SalaryGrid.Cells[2,2]:=FQuery.Fields[1].AsString;
      SalaryGrid.Cells[3,2]:=FQuery.Fields[2].AsString;
      FQuery.Close;
      // Always commit(retain) an opened transaction, even if only reading
      FTran.Commit;

      FTran.StartTransaction;
      if FConn.ConnectorType='PostGreSQL' then
      begin
        // For PostgreSQL, use a native SQL solution:
        FQuery.SQL.Text:='select stddev_pop(salary) from employee ';
        FTran.StartTransaction;
        FQuery.Open;
        if not(FQuery.EOF) then
          SalaryGrid.Cells[3,3]:=FQuery.Fields[0].AsString;
        FQuery.Close;
        // Always commit(retain) an opened transaction, even if only reading
      end
      else
      begin
        // For other databases, use the code approach:
        // 1. Get average of values
        FQuery.SQL.Text:='select avg(salary) from employee ';
        FQuery.Open;
        if (FQuery.EOF) then
          SalaryGrid.Cells[3,3]:='No data'
        else
        begin
          Average:=FQuery.Fields[0].AsFloat;
          FQuery.Close;
          // 2. For each value, calculate the square of (value-average), and add it up
          FQuery.SQL.Text:='select salary from employee where salary is not null ';
          FQuery.Open;
          while not(FQuery.EOF) do
          begin
            DifferencesSquared:=DifferencesSquared+Sqr(FQuery.Fields[0].AsFloat-Average);
            Count:=Count+1;
            FQuery.Next;
          end;
          // 3. Now calculate the average "squared difference" and take the square root
          SalaryGrid.Cells[3,3]:=FloatToStr(Sqrt(DifferencesSquared/Count));
        end;
        FQuery.Close;
      end;
      FTran.Commit;
    except
      on D: EDatabaseError do
      begin
        MessageDlg('Error', 'A database error has occurred. Technical error message: ' +
          D.Message, mtError, [mbOK], 0);
      end;
    end;
  finally
    SalaryGrid.EndUpdate;
  end;
end;

end.

