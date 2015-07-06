unit Unit1;
// J.P. August 2013
// Firebird embedded support added 2014
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3conn, ibconnection, fbadmin,
  FileUtil, Forms, Graphics, Dialogs, DbCtrls, Buttons, ExtCtrls, StdCtrls,
  DBGrids, Grids, LR_Class, LR_DBSet;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Bt_Print: TButton;
    Datasource1: TDatasource;
    DBEdit1: TDBEdit;
    DBGrid1: TDBGrid;
    DBImage1: TDBImage;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    DBText1: TDBText;
    frDBDataSet1: TfrDBDataSet;
    frReport1: TfrReport;
    IBConnection1: TIBConnection;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure Bt_PrintClick(Sender: TObject);
    procedure Datasource1DataChange(Sender: TObject; Field: TField);
    procedure DBGrid1PrepareCanvas(sender: TObject; DataCol: Integer;
      Column: TColumn; AState: TGridDrawState);
    procedure DBImage1DblClick(Sender: TObject);
    procedure DBNavigator1BeforeAction(Sender: TObject; Button: TDBNavButtonType);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure frReport1EnterRect(Memo: TStringList; View: TfrView);
  private
    FUsingFirebird: boolean; //indicates whether we're using Firebird (true) or SQLite3 (false)
    // Updates linked image field with image named in DBEdit1.Text
    // Shows image to user
    procedure UpdateImageLink;
    // Lets user change image stored in database blob field
    procedure ChangeImage;
    // Load image from file into database blob
    procedure LoadDbImage(aFileName: string);
    // Recreates mushroom database
    // for either SQLite3 (empty) or Firebird (filled)
    function RecreateDB: boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  FirebirdDB='ImageTest.fdb'; //database file for Firebird
  SQLiteDB='ImageTest.db3'; //database file for SQLite3


{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ChangeImage;
end;

procedure TForm1.Bt_PrintClick(Sender: TObject);
begin
  frReport1.LoadFromFile(ExtractFilePath(application.ExeName) +
    'Mushroom_Report.lrf');
  frReport1.ShowReport();
end;

procedure TForm1.Datasource1DataChange(Sender: TObject; Field: TField);
begin
  if Field=nil then
    UpdateImageLink;
end;

procedure TForm1.DBGrid1PrepareCanvas(sender: TObject; DataCol: Integer;
  Column: TColumn; AState: TGridDrawState);
  var MyTextStyle: TTextStyle;
begin
  if (DataCol =5) then
  begin
    MyTextStyle := DbGrid1.Canvas.TextStyle;
    MyTextStyle.SingleLine := false;
    DbGrid1.Canvas.TextStyle := MyTextStyle;
  end;
end;

procedure TForm1.DBImage1DblClick(Sender: TObject);
begin
  ChangeImage;
end;

procedure TForm1.DBNavigator1BeforeAction(Sender: TObject; Button: TDBNavButtonType);
begin
  if Button = nbRefresh then
  begin
    SQLQuery1.ApplyUpdates;
    SQLTransaction1.CommitRetaining;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SQLQuery1.ApplyUpdates;
  SQLTransaction1.Commit;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SQLQuery1.Close;

  // First try SQLite3 database in application directory
  // If no sqlite3 driver is found, try Firebird
  FUsingFirebird := false; //try sqlite first
  if not(FUsingFirebird) then
  begin
    SQLite3Connection1.DatabaseName:=ExtractFilePath(ParamStr(0)) + SQLiteDB;
    try
      SQLQuery1.Active := true;
    except
      on E:Exception do
      begin
        //ShowMessage('Exception: '+E.ClassName+': '+E.Message);
        if E.ClassNameIs('EInOutError') then
        // Assume 'Can not load SQLite client library "sqlite3.dll". Check your installation.' => but this text may change
        begin
          FUsingFirebird:=true;
        end
        else
        begin
          if RecreateDb then
          begin
            SQLQuery1.Active := true;
            ShowMessage('The SQLite3 database has been recreated');
          end
          else
          begin
            ShowMessage('Failure to create SQLite3 databse. Aborting');
            Application.Terminate;
          end;
        end;
      end;
    end;
    Caption := Caption + ' (SQLite3)';
  end;

  if FUsingFirebird then
  begin
    // Try Firebird. We'll need to reconnect some components as they were
    // set up for sqlite
    // Hostname was already empty in Object Inspector so we use embedded
    //IBConnection1.HostName:='';
    IBConnection1.UserName:='SYSDBA'; //for embedded, this should be an existing name
    IBConnection1.CharSet:='UTF8';
    IBConnection1.Params.Add('PAGE_SIZE=16384'); //enough space for indexes etc
    IBConnection1.DatabaseName:=ExtractFilePath(ParamStr(0)) + FirebirdDB;
    IBConnection1.Transaction:=SQLTransaction1;
    SQLTransaction1.Database:=IBConnection1;
    SQLQuery1.DataBase:=IBConnection1;
    try
      SQLQuery1.Active:=true;
    except
      on I: EIBDatabaseError do
      begin
        // Check for database file not found error message
        if (I.GDSErrorcode=335544344) and (pos('I/O error',i.message)>0) then
        begin
          if RecreateDb then
          begin
            SQLQuery1.Active := true;
            ShowMessage('The Firebird database has been recreated');
          end
          else
          begin
            ShowMessage('Failure to create Firebird databse. Aborting');
            Application.Terminate;
          end;
        end
        else
        begin
          ShowMessage('Have tried SQLite3 and Firebird dbs without success. '+LineEnding+
            'Exception: '+I.ClassName+': '+I.Message+LineEnding+
            '(Firebird GDS Eror code: '+inttostr(I.GDSErrorCode)+')'+LineEnding+
            'Aborting.');
          Application.Terminate;
        end;
      end;
      on E: Exception do
      begin
        ShowMessage('Have tried SQLite3 and Firebird dbs without success. '+LineEnding+
          'Exception: '+E.ClassName+': '+E.Message+LineEnding+
          'Aborting.');
        Application.Terminate;
      end;
    end;
    Caption := Caption + ' (Firebird embedded)';
  end;
end;

procedure TForm1.frReport1EnterRect(Memo: TStringList; View: TfrView);
begin
   if (View.Name = 'Picture2') AND
   (frDBDataSet1.DataSet.FieldByName('Image_Link').AsString <> '') then
     TFrPictureView(View).Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) +
    'images' + DirectorySeparator + frDBDataSet1.DataSet.FieldByName('Image_Link').AsString);
end;

procedure TForm1.UpdateImageLink;
var
  s: String;
begin
  if DBEdit1.Text <> '' then
  begin
    s := ExtractFilePath(ParamStr(0)) + 'images' + DirectorySeparator + DBEdit1.Text;
    Image1.Picture.LoadFromFile(s);
  end
  else
    Image1.Picture.Clear;
end;

procedure TForm1.ChangeImage;
var
  WasEditing: Boolean;
begin
  if SQLQuery1.Active then
  begin
    OpenDialog1.Filter :=
    'All image files (*.bmp,*.jpg,*.png,*.gif)|*.bmp;*.jpg;*.png;*.gif|' +
    'BMP files (*.bmp)|*.bmp|' +
    'JPEG files (*.jpg)|*.jpg|' +
    'PNG files (*.png)|*.png|' +
    'GIF files (*.gif)|*.gif' +
    'TIFF files (*.tiff)|*.tiff;*.tif';
    OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0)) + 'images';
    if OpenDialog1.Execute then
    begin
      WasEditing := (SQLQuery1.State = dsEdit);
      if not WasEditing then
        SQLQuery1.Edit;
      DBEdit1.Text := ExtractFileName(OpenDialog1.FileName);
      LoadDbImage(OpenDialog1.FileName);
      if not WasEditing then
        SQLQuery1.Post;
    end;
  end;
end;

procedure TForm1.LoadDbImage(aFileName: string);
begin
  // Disabling writeheader means the image is compatible with Delphi TDBImage applications
  // and other applications that only expect raw jpg data in the database blob.
  // However, it makes it a bit more difficult to mix various kinds of image types
  // e.g. jpg and tiff in the same database fields.
  // Fortunately, Lazarus can deal with that for us.
  DbImage1.WriteHeader:=false;
  DbImage1.Picture.LoadFromFile(aFileName);
end;

function TForm1.RecreateDB: boolean;
var
  BackupFile: string;
  DBFile: string;
  FBAdmin: TFBAdmin;
  SQLInstructions: string; //file with sql instructions to create tables etc
  Scripter: TSQLScript;
begin
  result:=false;
  if not(FUsingFirebird) then //not using Firebird, so using SQLite3
  begin
    try
      SQLite3Connection1.ExecuteDirect(
      'CREATE TABLE DeadlyMushrooms '+LineEnding+
      '-- This table created by Jurassic Pork '+LineEnding+
      '-- for Free Pascal Lazarus '+LineEnding+
      '-- Create date:2013-08-05 23:55:09 '+LineEnding+
      '( '+LineEnding+
      '       ID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, '+LineEnding+
      '       Scientific_Name VARCHAR NOT NULL, '+LineEnding+
      '       Common_Name VARCHAR, '+LineEnding+
      '       `Order`  VARCHAR, `Genus`  VARCHAR, `Notes`  TEXT, `Picture`  BLOB, `Image_Link`  VARCHAR)'
      );
      result := true;
    except
      result := false;
    end;
  end
  else
  begin //Using Firebird
    try
      DBFile:=ExtractFilePath(ParamStr(0)) + FirebirdDB;
      BackupFile:=ChangeFileExt(DBFile,'.fbk');
      // First try restoring backup file with full content
      if fileexists(BackupFile) then
      begin
        FBAdmin:=TFBAdmin.Create(nil);
        try
          try
            FBAdmin.UseExceptions:=true;
            FBAdmin.Host:=''; //embedded
            FBAdmin.Protocol:=IBSPLOCAL;
            FBAdmin.User:='SYSDBA';
            FBAdmin.Password:=''; //embedded: doesn't matter which password is used
            FBAdmin.Connect;
            // Now run the restore. It will not overwrite an existing file but will
            // create a new db
            if FBAdmin.Restore(DBFile,BackupFile,[IBResCreate]) then
            begin
              sleep(200); //at least needed on Windows: give the filesystem time to update so we don't get failures connecting
              exit(true) //we're done; exit the function
            end
            else
              result := false;
          finally
            FBAdmin.Free;
          end;
        except
          result := false; //we can continue with the next fallback step
        end;
      end;

      // If no backup was found or restore failed,
      // create an empty .fdb file
      if (FileExists(DBFile))=false then
      begin
        IBConnection1.CreateDB;
      end;
      if (FileExists(DBFile)=false) then
        raise Exception.CreateFmt('Firebird error: failure to create database %s',[DBFile]);
      // Now create needed tables etc:
      if not(IBConnection1.Connected) then IBConnection1.Connected := true;
      Scripter:=TSQLScript.Create(nil);
      try
        SQLInstructions := ExtractFilePath(ParamStr(0)) + 'mushrooms_firebird.sql';
        if not fileexists(SQLInstructions) then
          raise Exception.CreateFmt('Error creating db: could not load SQL definition file %s',[SQLInstructions]);
        Scripter.Script.LoadFromFile(SQLInstructions);
        Scripter.DataBase := IBConnection1;
        Scripter.Transaction := SQLTransaction1;
        Scripter.CommentsInSQL := false; //needed to circumvent bugs in TSQLScript
        Scripter.UseSetTerm := true; //needed because we have SET TERM statements in the SQL file
        // Now everything is loaded in, run all commands at once:
        Scripter.Execute;
        //... and then commit to make them stick and show them to the SQL that comes
        // after the commit
        SQLTransaction1.Commit;
        result := true;
      finally
        Scripter.Free;
      end;
    except
      result := false;
    end;
  end;
end;


end.

