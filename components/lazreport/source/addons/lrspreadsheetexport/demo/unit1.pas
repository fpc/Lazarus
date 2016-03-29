unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, LR_Desgn, LR_BarC, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, le_e_spreadsheet,
  lrSpreadSheetExp;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button9: TButton;
    frBarCodeObject1: TfrBarCodeObject;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    Label1: TLabel;
    Label2: TLabel;
    lrSpreadSheetExport1: TlrSpreadSheetExport;
    RadioGroup1: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function RepName(Num:integer):string;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses LCLIntf, LazUTF8, LazFileUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  frReport1.Clear;
  if FileExistsUTF8(RepName((Sender as TComponent).Tag)) then
    frReport1.LoadFromFile(RepName((Sender as TComponent).Tag))
  else
    frReport1.FileName:=RepName((Sender as TComponent).Tag);
  frReport1.DesignReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if FileExistsUTF8(RepName((Sender as TComponent).Tag)) then
  begin
    frReport1.LoadFromFile(RepName((Sender as TComponent).Tag));
    frReport1.ShowReport;
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  FExportName: String;
  FExt:string;
begin
  case RadioGroup1.ItemIndex of
    1:FExt:='.xls';
    2:FExt:='.xlsx';
  else
    FExt:='.ods';
  end;
  FExportName:=AppendPathDelim(ExtractFileDir(ParamStrUTF8(0))) + AppendPathDelim('export') + 'test1'+FExt;
  frReport1.LoadFromFile(RepName((Sender as TComponent).Tag));
  frReport1.PrepareReport;
  frReport1.ExportTo(TlrSpreadSheetExportFilter, FExportName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  lrSpreadSheetExport1.ExportImages:=true;
end;

function TForm1.RepName(Num: integer): string;
begin
  Result:=AppendPathDelim(ExtractFileDir(ParamStrUTF8(0))) + AppendPathDelim('reports');
  case Num of
    0:Result:=Result + 'demo_report.lrf';
    1:Result:=Result + 'acc_pay.lrf';
  end;
end;

end.

