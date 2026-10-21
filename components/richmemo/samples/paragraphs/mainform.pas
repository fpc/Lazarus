unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, Spin, RichMemo, RichMemoUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    RichMemo1: TRichMemo;
    btnLA: TSpeedButton;
    btnCA: TSpeedButton;
    btnRA: TSpeedButton;
    btnJA: TSpeedButton;
    edtFL: TSpinEdit;
    edtHI: TSpinEdit;
    edtTI: TSpinEdit;
    procedure btnCAClick(Sender: TObject);
    procedure btnJAClick(Sender: TObject);
    procedure btnLAClick(Sender: TObject);
    procedure btnRAClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure edtFLChange(Sender: TObject);
    procedure edtHIChange(Sender: TObject);
    procedure edtTIChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RichMemo1Click(Sender: TObject);
    procedure RichMemo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    procedure SelectionChanged;
    procedure SetRichAlign(a: TParaAlignment);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    LoadRTFFile( RichMemo1, OpenDialog1.FileName );
    SelectionChanged;
  end;
end;

procedure TForm1.btnLAClick(Sender: TObject);
begin
  SetRichAlign(paLeft);
end;

procedure TForm1.btnRAClick(Sender: TObject);
begin
  SetRichAlign(paRight);
end;

procedure TForm1.btnCAClick(Sender: TObject);
begin
  SetRichAlign(paCenter);
end;

procedure TForm1.btnJAClick(Sender: TObject);
begin
  SetRichAlign(paJustify);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  st, ln: Integer;
begin
  RichMemo1.GetParaRange(RichMemo1.SelStart+RichMemo1.SelLength, st, ln);
  Caption:=Format(' start: %d, len %d', [st, ln]);
  RichMemo1.SelStart:=st;
  RichMemo1.SelLength:=ln;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
end;

procedure TForm1.edtFLChange(Sender: TObject);
var
  m  : TParaMetric;
begin
  InitParaMetric(m);
  m.FirstLine:=edtFL.Value;
  RichMemo1.SetRangeParaParams(RichMemo1.SelStart, RichMemo1.SelLength,
    [pmm_FirstLine], m);
end;

procedure TForm1.edtHIChange(Sender: TObject);
var
  m  :TParaMetric;
begin
  InitParaMetric(m);
  m.HeadIndent:=edtHI.Value;
  RichMemo1.SetRangeParaParams(RichMemo1.SelStart, RichMemo1.SelLength,
    [pmm_HeadIndent], m);
end;

procedure TForm1.edtTIChange(Sender: TObject);
var
  m  :TParaMetric;
begin
  InitParaMetric(m);
  m.TailIndent:=edtTI.Value;
  RichMemo1.SetRangeParaParams(RichMemo1.SelStart, RichMemo1.SelLength,
    [pmm_TailIndent], m);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SelectionChanged;
end;

procedure TForm1.RichMemo1Click(Sender: TObject);
begin
  SelectionChanged;
end;

procedure TForm1.RichMemo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SelectionChanged;
end;

procedure TForm1.SelectionChanged;
var
  pa : TParaAlignment;
  m  : TParaMetric;
begin
  pa:=RichMemo1.GetParaAlignment(RichMemo1.SelStart);
  case pa of
    paLeft:  btnLA.Down:=true;
    paRight:  btnRA.Down:=true;
    paCenter:  btnCA.Down:=true;
    paJustify:  btnJA.Down:=true;
  end;
  edtFL.OnChange:=nil;
  edtHI.OnChange:=nil;
  edtTI.OnChange:=nil;
  RichMemo1.GetParaMetric(RichMemo1.SelStart, m);
  edtFL.Value:=round(m.FirstLine);
  edtHI.Value:=round(m.HeadIndent);
  edtTI.Value:=round(m.TailIndent);
  edtFL.OnChange:=@edtFLChange;
  edtHI.OnChange:=@edtHIChange;
  edtTI.OnChange:=@edtTIChange;
end;

procedure TForm1.SetRichAlign(a: TParaAlignment);
begin
  RichMemo1.SetParaAlignment(RichMemo1.SelStart, RichMemo1.SelLength, a);
end;

end.

