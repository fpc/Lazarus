unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, LR_Desgn, LR_IBConnection, lrTDbfData,
  lrOfficeImport, Forms, Controls, Graphics, Dialogs, StdCtrls, IBConnection,
  sqldb;

const
  RNSimple    = 'SimpleReport.lrf';
  RNWithSQLDB = 'ReportWithSQLDB.lrf';
  RNWithDBF   = 'ReportWithDBF.lrf';
  RNImport    = 'ReportImport.lrf';

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    IBConnection1: TIBConnection;
    lrOfficeImport1: TlrOfficeImport;
    lrTDbfData1: TlrTDbfData;
    LR_IBConnection1: TLR_IBConnection;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    procedure LoadReport(const AName:string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses fpsallformats, LazUtf8, LazFileUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  LoadReport(RNSimple);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  IBConnection1.Connected:=true;
  LoadReport(RNWithSQLDB);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  LoadReport(RNWithDBF);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  B:Boolean;
begin
  B:=CheckBox1.Checked;
  CheckBox1.Checked:=true;
  LoadReport(RNImport);
  CheckBox1.Checked:=B;
end;

procedure TForm1.LoadReport(const AName: string);
var
  S:string;
begin
  S:=AppendPathDelim(ExtractFileDir(ParamStrUTF8(0))) + AName;
  if FileExistsUTF8(S) then
    frReport1.LoadFromFile(S)
  else
    frReport1.FileName:=S;

  if CheckBox1.Checked then
    frReport1.DesignReport
  else
  begin
    frReport1.ShowReport;
  end;
end;

end.

