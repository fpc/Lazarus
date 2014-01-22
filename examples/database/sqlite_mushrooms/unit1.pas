unit Unit1;
// J.P  August 2013
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_DBSet, Forms, Controls, Graphics,
  Dialogs, DbCtrls, Buttons, ExtCtrls, StdCtrls, DBGrids,
  db, sqldb, sqlite3conn, Grids;

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
    procedure DBNavigator1BeforeAction(Sender: TObject; Button: TDBNavButtonType
      );
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure frReport1EnterRect(Memo: TStringList; View: TfrView);
  private
    { private declarations }
    procedure UpdateImageLink;
    procedure ChangeImage;
    procedure LoadDbImage(aFileName: string);
    function RecreateDB: boolean;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

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

procedure TForm1.DBNavigator1BeforeAction(Sender: TObject;
  Button: TDBNavButtonType);
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
  SQLite3Connection1.DatabaseName:= ExtractFilePath(ParamStr(0)) + 'ImageTest.db3';
  try
    SQLQuery1.Active := true;
  except
    on E:Exception do begin
      //WriteLn(E.ClassName,': '+E.Message);
      if RecreateDb then begin
        SQLQuery1.Active := true;
        ShowMessage('The database has been recreated');
      end;
    end;
  end;
end;

procedure TForm1.frReport1EnterRect(Memo: TStringList; View: TfrView);
begin
   if  (View.Name = 'Picture2') AND
   (frDBDataSet1.DataSet.FieldByName('Image_Link').AsString <> '') then
    TFrPictureView(View).Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) +
  'images' +  DirectorySeparator +  frDBDataSet1.DataSet.FieldByName('Image_Link').AsString);
end;

procedure TForm1.UpdateImageLink;
var
  s: String;
begin
  if   DBEdit1.Text <> '' then begin
    s := ExtractFilePath(ParamStr(0)) + 'images' +  DirectorySeparator +  DBEdit1.Text;
    Image1.Picture.LoadFromFile(s);
  end else
    Image1.Picture.Clear;
end;

procedure TForm1.ChangeImage;
var
  WasEditing: Boolean;
begin
  if SQLQuery1.Active then begin
    OpenDialog1.Filter :=
    'All image files (*.bmp,*.jpg,*.png,*.gif)|*.bmp;*.jpg;*.png;*.gif|' +
    'BMP files (*.bmp)|*.bmp|' +
    'JPEG files (*.jpg)|*.jpg|' +
    'PNG files (*.png)|*.png|' +
    'GIF files (*.gif)|*.gif';
    OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0)) + 'images';
    if OpenDialog1.Execute then begin
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
  DbImage1.WriteHeader:=false;
  DbImage1.Picture.LoadFromFile(aFileName);
end;

function TForm1.RecreateDB: boolean;
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
end;


end.

