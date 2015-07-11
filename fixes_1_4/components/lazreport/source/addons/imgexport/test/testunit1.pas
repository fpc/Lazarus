unit testunit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_BarC, LR_RRect, LR_Shape, LR_ChBox,
  LR_Desgn, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ComCtrls,
  LR_e_htmldiv, LR_e_img;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ColorButton1: TColorButton;
    Edit1: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    frBarCodeObject1: TfrBarCodeObject;
    frCheckBoxObject1: TfrCheckBoxObject;
    frDesigner1: TfrDesigner;
    frHtmlDivExport1: TfrHtmlDivExport;
    frImageExport1: TfrImageExport;
    frReport1: TfrReport;
    frRoundRectObject1: TfrRoundRectObject;
    frShapeObject1: TfrShapeObject;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SaveDialog1: TSaveDialog;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure frReport1ExportFilterSetup(Sender: TfrExportFilter);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  LCLIntf;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  frReport1.ShowReport;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  frReport1.DesignReport;
end;

procedure TForm1.frReport1ExportFilterSetup(Sender: TfrExportFilter);
begin
  if Sender is TfrImageExportFilter then
    with TfrImageExportFilter(Sender) do
    begin
      Zoom := FloatSpinEdit1.Value;
      JPEGQuality := SpinEdit1.Value;
      BackgroundColor := ColorButton1.ButtonColor;
    end
  else
    if Sender is TfrHtmlDivExportFilter then
      with TfrHtmlDivExportFilter(Sender) do
      begin
        PageStyle := Edit1.Text;
        ExportImages := CheckBox1.Checked;
        EmbeddedImages := CheckBox2.Checked;
      end;
end;

end.

