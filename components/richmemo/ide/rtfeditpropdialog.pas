unit RtfEditPropDialog;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Graphics, Dialogs, StdCtrls, Buttons, RichMemo, RichMemoUtils;

type

  { TRTFEditDialog }

  TRTFEditDialog = class(TForm)
    btnCA: TSpeedButton;
    btnJA: TSpeedButton;
    btnLA: TSpeedButton;
    btnRA: TSpeedButton;
    btnLoad: TButton;
    btnClear: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    btnSave: TButton;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    RtfOpenDialog: TOpenDialog;
    RichMemo1: TRichMemo;
    btnBold: TSpeedButton;
    btnItalic: TSpeedButton;
    btnUnderline: TSpeedButton;
    btnFont: TSpeedButton;
    btnColor: TSpeedButton;
    RtfSaveDialog: TSaveDialog;
    procedure btnCAClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnItalicClick(Sender: TObject);
    procedure btnJAClick(Sender: TObject);
    procedure btnLAClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnRAClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure btnUnderlineClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnColorClick(Sender: TObject);
    procedure RichMemo1Change(Sender: TObject);
  private
    procedure FontStyleModify(fs: TFontStyle);
  end;

var
  RTFEditDialog: TRTFEditDialog = nil;

implementation

{$R *.lfm}

{ TRTFEditDialog }

procedure TRTFEditDialog.btnLAClick(Sender: TObject);
begin
  RichMemo1.SetParaAlignment( RichMemo1.SelStart,
    RichMemo1.SelLength, paLeft);
end;

procedure TRTFEditDialog.btnCAClick(Sender: TObject);
begin
  RichMemo1.SetParaAlignment( RichMemo1.SelStart,
    RichMemo1.SelLength, paCenter);
end;

procedure TRTFEditDialog.btnClearClick(Sender: TObject);
begin
  RichMemo1.Clear;
  RichMemo1Change(Sender);
end;

procedure TRTFEditDialog.btnItalicClick(Sender: TObject);
begin
  FontStyleModify(fsItalic);
end;

procedure TRTFEditDialog.btnJAClick(Sender: TObject);
begin
  RichMemo1.SetParaAlignment( RichMemo1.SelStart,
    RichMemo1.SelLength, paJustify);
end;

procedure TRTFEditDialog.btnLoadClick(Sender: TObject);
begin
  if RtfOpenDialog.Execute then
    LoadRTFFile( RichMemo1, RtfOpenDialog.FileName);
end;

procedure TRTFEditDialog.btnRAClick(Sender: TObject);
begin
  RichMemo1.SetParaAlignment( RichMemo1.SelStart,
    RichMemo1.SelLength, paRight);
end;

procedure TRTFEditDialog.btnSaveClick(Sender: TObject);
begin
  if RtfSaveDialog.FileName = '' then
    RtfSaveDialog.FileName := 'New document.rtf';
  if RtfSaveDialog.Execute and (RtfSaveDialog.FileName <> '') then
    SaveRTFFile( RichMemo1, RtfSaveDialog.FileName);
end;

procedure TRTFEditDialog.btnBoldClick(Sender: TObject);
begin
  FontStyleModify(fsBold);
end;

procedure TRTFEditDialog.btnUnderlineClick(Sender: TObject);
begin
  FontStyleModify(fsUnderline);
end;

procedure TRTFEditDialog.btnFontClick(Sender: TObject);
var
  f : TFontParams;
begin
  RichMemo1.GetTextAttributes(RichMemo1.SelStart, f);
  FontDialog1.Font.Name:=f.Name;
  FontDialog1.Font.Size:=f.Size;
  FontDialog1.Font.Style:=f.Style;
  FontDialog1.Font.Color:=f.Color;
  if FontDialog1.Execute then begin
    RichMemo1.SetRangeParams(RichMemo1.SelStart, RichMemo1.SelLength
      , [tmm_Color, tmm_Size, tmm_Name]
      , FontDialog1.Font.Name
      , FontDialog1.Font.Size
      , FontDialog1.Font.Color, [], []);
  end;
end;

procedure TRTFEditDialog.btnColorClick(Sender: TObject);
var
  f : TFontParams;
begin
  RichMemo1.GetTextAttributes(RichMemo1.SelStart, f);
  ColorDialog1.Color:=f.Color;
  if ColorDialog1.Execute then begin
    RichMemo1.SetRangeColor( RichMemo1.SelStart, RichMemo1.SelLength,
      ColorDialog1.Color);
  end;
end;

procedure TRTFEditDialog.RichMemo1Change(Sender: TObject);
begin
  btnSave.Enabled := RichMemo1.Lines.Count > 0;
end;

procedure TRTFEditDialog.FontStyleModify(fs: TFontStyle);
var
  f : TFontParams;
  rm  : TFontStyles;
  add : TFontStyles;
begin
  RichMemo1.GetTextAttributes(RichMemo1.SelStart, f);
  if fs in f.Style then begin
    rm:=[fs]; add:=[];
  end else begin
    rm:=[]; add:=[fs];
  end;
  RichMemo1.SetRangeParams(RichMemo1.SelStart, RichMemo1.SelLength
    , [tmm_Styles] , '', 0, 0, add, rm);
end;

end.

