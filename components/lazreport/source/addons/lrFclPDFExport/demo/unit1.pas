unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_Desgn, LR_RRect,
  LR_BarC, LR_Shape, LR_ChBox, lr_CrossTab,
  lrPDFExport, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LazFileUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    frBarCodeObject1: TfrBarCodeObject;
    frCheckBoxObject1: TfrCheckBoxObject;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    frRoundRectObject1: TfrRoundRectObject;
    frShapeObject1: TfrShapeObject;
    lrCrossObject1: TlrCrossObject;
    lrPDFExport1: TlrPDFExport;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFileName:string;
  public

  end;

var
  Form1: TForm1;

implementation
uses lr_e_fclpdf, LCLIntf;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
var
  S: String;
begin
  S:=AppendPathDelim(ExtractFileDir(ParamStr(0))) + 'aaa1.pdf';
  frReport1.PrepareReport;
  frReport1.ExportTo(TlrPdfExportFilter, S);
  OpenDocument(S);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  frReport1.ShowReport;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFileName:=AppendPathDelim(ExtractFileDir(ParamStr(0))) + 'aaa.lrf';
  if FileExistsUTF8(FFileName) then
   frReport1.LoadFromFile(FFileName);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  frReport1.DesignReport;
end;

end.

