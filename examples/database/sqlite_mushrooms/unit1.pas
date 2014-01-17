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
    procedure DBEdit1Change(Sender: TObject);
    procedure DBGrid1PrepareCanvas(sender: TObject; DataCol: Integer;
      Column: TColumn; AState: TGridDrawState);
    procedure DBImage1DBImageRead(Sender: TObject; S: TStream;
      var GraphExt: string);
    procedure DBImage1DBImageWrite(Sender: TObject; S: TStream; GraphExt: string
      );
    procedure DBImage1DblClick(Sender: TObject);
    procedure DBNavigator1BeforeAction(Sender: TObject; Button: TDBNavButtonType
      );
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure frReport1DBImageRead(Sender: TObject; S: TStream;
      var GraphExt: string);
    procedure frReport1EnterRect(Memo: TStringList; View: TfrView);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.DBImage1DBImageRead(Sender: TObject; S: TStream;
  var GraphExt: string);
  var val1,val2:  WORD;
  begin
   S.Seek(0, soFromBeginning);
   S.Read(val1,2);
   S.Position := 2;
   S.Read(val2,2);
   if (val1 = $4D42) then  GraphExt := 'bmp';
   if (val1 = $4947) and (val2 = $3846) then  GraphExt := 'gif';
   if (val1 = $5089) and (val2 = $474E) then  GraphExt := 'png';
   if (val1 = $D8FF) and (val2 = $E0FF) then  GraphExt := 'jpg';
   S.Seek(0, soFromBeginning);
end;

procedure TForm1.Button1Click(Sender: TObject);
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
      if SQLQuery1.State <> dsEdit then
          SQLQuery1.Edit;
      Image1.Picture.LoadFromFile(OpenDialog1.FileName);
      DBEdit1.Text := ExtractFileName(OpenDialog1.FileName);
      SQLQuery1.Post;
    end;
  end;
end;

procedure TForm1.Bt_PrintClick(Sender: TObject);
begin
  frReport1.LoadFromFile(ExtractFilePath(application.ExeName) +
    'Mushroom_Report.lrf');
  frReport1.ShowReport();
end;
procedure TForm1.DBEdit1Change(Sender: TObject);
begin
 if   DBEdit1.Text <> '' then
 Image1.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) +
  'images' +  DirectorySeparator +  DBEdit1.Text) else
 Image1.Picture.Clear;
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



procedure TForm1.DBImage1DBImageWrite(Sender: TObject; S: TStream;
  GraphExt: string);
begin

end;

procedure TForm1.DBImage1DblClick(Sender: TObject);
var
  BlobStream: TStream;
  FileStream: TStream;
begin
  if SQLQuery1.Active then begin
     OpenDialog1.Filter :=
     'All image files (*.bmp,*.jpg,*.png,*.gif)|*.bmp;*.jpg;*.png;*.gif|' +
     'BMP files (*.bmp)|*.bmp|' +
     'JPEG files (*.jpg)|*.jpg|' +
     'PNG files (*.png)|*.png|' +
     'GIF files (*.gif)|*.gif';
    if OpenDialog1.Execute then begin
      begin
       if SQLQuery1.State <> dsEdit then
          SQLQuery1.Edit;

        try
          FileStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
          try
            BlobStream := SQLQuery1.CreateBlobStream(
            SQLQuery1.FieldByName('Picture'), bmWrite);
            BlobStream.CopyFrom(FileStream, FileStream.Size);
            SQLQuery1.Post;
          finally
           FileStream.Free;
          end;
        finally
          BlobStream.Free;
        end;
      end;
    end;
  end;

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
 SQLTransaction1.CommitRetaining;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   SQLQuery1.Close;
   SQLite3Connection1.DatabaseName:= ExtractFilePath(ParamStr(0)) + 'ImageTest.db3';
   SQLQuery1.Active := true;
end;

procedure TForm1.frReport1DBImageRead(Sender: TObject; S: TStream;
  var GraphExt: string);
var val1,val2:  WORD;
begin
    S.Seek(0, soFromBeginning);
   S.Read(val1,2);
   S.Position := 2;
   S.Read(val2,2);
   if (val1 = $4D42) then  GraphExt := 'bmp';
   if (val1 = $4947) and (val2 = $3846) then  GraphExt := 'gif';
   if (val1 = $5089) and (val2 = $474E) then  GraphExt := 'png';
   if (val1 = $D8FF) and (val2 = $E0FF) then  GraphExt := 'jpg';
   S.Seek(0, soFromBeginning);
end;

procedure TForm1.frReport1EnterRect(Memo: TStringList; View: TfrView);
begin
   if  (View.Name = 'Picture2') AND
   (frDBDataSet1.DataSet.FieldByName('Image_Link').AsString <> '') then
    TFrPictureView(View).Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) +
  'images' +  DirectorySeparator +  frDBDataSet1.DataSet.FieldByName('Image_Link').AsString);
end;


end.

