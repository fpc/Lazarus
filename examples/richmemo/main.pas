{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Željan Rikalo <zeljko@lazarus-ide.org>

  Abstract:
   Example application for TRichMemo: font name, size, style and colors,
   paragraph alignment and numbering, links, images, RTF loading and saving,
   clipboard and printing.
}

unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, Buttons, LCLType, LazUTF8,
  RichMemo, RichMemoUtils, RichMemoRTF, RichMemoHelpers;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSave: TButton;
    btnOpen: TButton;
    btnClose: TButton;
    cbFontName: TComboBox;
    cbFontSize: TComboBox;
    cbtnBackColor: TColorButton;
    cbtnFontColor: TColorButton;
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miSepFile1: TMenuItem;
    miPrint: TMenuItem;
    miSepFile2: TMenuItem;
    miClose: TMenuItem;
    miEdit: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miSepEdit1: TMenuItem;
    miSelectAll: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    RichMemo1: TRichMemo;
    SaveDialog1: TSaveDialog;
    sbBold: TSpeedButton;
    sbItalic: TSpeedButton;
    sbStrike: TSpeedButton;
    sbUnderline: TSpeedButton;
    ToolBar1: TToolBar;
    tbSep1: TToolButton;
    tbSep2: TToolButton;
    tbNoBackColor: TToolButton;
    tbSep3: TToolButton;
    tbAlignLeft: TToolButton;
    tbAlignCenter: TToolButton;
    tbAlignRight: TToolButton;
    tbAlignJustify: TToolButton;
    procedure cbtnBackColorColorChanged(Sender: TObject);
    procedure cbtnFontColorColorChanged(Sender: TObject);
    procedure cbFontNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbFontNameSelect(Sender: TObject);
    procedure cbFontSizeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbFontSizeSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miPrintClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure sbStyleClick(Sender: TObject);
    procedure tbAlignClick(Sender: TObject);
    procedure tbNoBackColorClick(Sender: TObject);
  private
    FFileName: string;
    FUpdatingToolbar: Boolean;
    FPending: Boolean;
    FPendingMask: TTextModifyMask;
    FPendingParams: TFontParams;
    FPendingAdd: TFontStyles;
    FPendingRemove: TFontStyles;
    FApplyingPending: Boolean;
    FLastTextLen: Integer;
    procedure LoadTestContent;
    procedure ApplyNumbering(const AFirst, ALast: string; const ANumber: TParaNumbering);
    procedure ApplyFontParams(const AMask: TTextModifyMask; const AParams: TFontParams;
      const AAddStyle, ARemoveStyle: TFontStyles);
    procedure ApplyFontName;
    procedure ApplyFontSize;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure UpdateCaption;
    procedure UpdateToolbar;
    procedure ClearPending;
    function TextLength: Integer;
    procedure RichMemoChange(Sender: TObject);
    procedure RichMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RichMemoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RichMemoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RichMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RichMemoLinkAction(Sender: TObject; ALinkAction: TLinkAction;
      const info: TLinkMouseInfo; LinkStart, LinkLen: Integer);
    procedure RichMemoSelectionChange(Sender: TObject);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  TestRtf =
    '{\rtf1\ansi\deff0' +
    '{\fonttbl{\f0\fswiss DejaVu Sans;}{\f1\froman DejaVu Serif;}{\f2\fmodern DejaVu Sans Mono;}}' +
    '{\colortbl;\red192\green0\blue0;\red0\green128\blue0;\red0\green0\blue192;' +
      '\red255\green255\blue0;\red128\green0\blue128;\red255\green200\blue200;\red200\green230\blue255;}' +

    '\pard\qc\sb120\sa240\f0\fs40\b RichMemo test\b0\par' +
    '\pard\qc\sa240\fs22\i gtk2 / gtk3 / qt5 / qt6 / cocoa / win32 - same RTF, same loader\i0\par' +

    '\pard\sb240\sa120\fs28\b\cf3 1. Character styles\cf0\b0\par' +
    '\pard\fs22 Plain text, \b bold\b0 , \i italic\i0 , \ul underlined\ulnone , ' +
      '\strike strikeout\strike0 , \b\i\ul all together\ulnone\i0\b0 .\par' +
    '\pard Croatian characters: \u268?a\u273?ava \u353?uma, \u382?uti \u263?uk - ' +
      '\u268?\u262?\u381?\u352?\u272? \u269?\u263?\u382?\u353?\u273?.\par' +
    '\pard Chinese (simplified): \u20320?\u22909?\u65292?\u19990?\u30028?\u65281?\u20013?\u25991?\u27721?\u23383?\u27979?\u35797?\par' +
    '\pard Japanese: \u12371?\u12435?\u12395?\u12385?\u12399? (hiragana), \u12459?\u12479?\u12459?\u12490? (katakana), \u19990?\u30028?\u12539?\u26085?\u26412?\u35486? (kanji)\par' +
    '\pard Russian: \u1055?\u1088?\u1080?\u1074?\u1077?\u1090?, \u1084?\u1080?\u1088?! \u1057?\u1098?\u1077?\u1096?\u1100? \u1078?\u1077? \u1077?\u1097?\u1105? \u1101?\u1090?\u1080?\u1093? \u1084?\u1103?\u1075?\u1082?\u1080?\u1093? \u1092?\u1088?\u1072?\u1085?\u1094?\u1091?\u1079?\u1089?\u1082?\u1080?\u1093? \u1073?\u1091?\u1083?\u1086?\u1082?.\par' +
    '\pard German: Gr\u252?\u223?e aus M\u252?nchen: \u196?pfel, \u214?l, \u220?bermut, Stra\u223?e - \u228? \u246? \u252? \u223? \u196? \u214? \u220? \u7838?\par' +

    '\pard\sb240\sa120\fs28\b\cf3 2. Fonts and sizes\cf0\b0\par' +
    '\pard\fs16 Small 8 pt, \fs22 normal 11 pt, \fs28 larger 14 pt, \fs36 large 18 pt\fs22 .\par' +
    '\pard\f1 Serif font: The quick brown fox jumps over the lazy dog.\f0\par' +
    '\pard\f2 Monospace: for i := 0 to Count - 1 do Inc(Sum, Items[i]);\f0\par' +

    '\pard\sb240\sa120\fs28\b\cf3 3. Colors and background\cf0\b0\par' +
    '\pard\fs22\cf1 Red\cf0 , \cf2 green\cf0 , \cf3 blue\cf0 , \cf5 purple\cf0 , ' +
      '\highlight4 yellow background\highlight0 , \cf1\highlight6 red on pink\highlight0\cf0 , ' +
      '\b\cf3\highlight7 bold blue on light blue\highlight0\cf0\b0 .\par' +

    '\pard\sb240\sa120\fs28\b\cf3 4. Subscript and superscript\cf0\b0\par' +
    '\pard\fs22 Water is H\sub 2\nosupersub O, and Einstein says E = mc\super 2\nosupersub . ' +
      'Footnote\super 1\nosupersub  and x\sub i\nosupersub\super 2\nosupersub .\par' +

    '\pard\sb240\sa120\fs28\b\cf3 5. Alignment\cf0\b0\par' +
    '\pard\ql\fs22 Left aligned paragraph.\par' +
    '\pard\qc Centered paragraph.\par' +
    '\pard\qr Right aligned paragraph.\par' +
    '\pard\qj Justified paragraph: Lorem ipsum dolor sit amet, consectetur adipiscing elit, ' +
      'sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, ' +
      'quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\par' +

    '\pard\sb240\sa120\fs28\b\cf3 6. Indents and spacing\cf0\b0\par' +
    '\pard\li720\fs22 Left indent of 0.5 inch.\par' +
    '\pard\fi720 First line indent of 0.5 inch: Duis aute irure dolor in reprehenderit in voluptate ' +
      'velit esse cillum dolore eu fugiat nulla pariatur.\par' +
    '\pard\li1080\fi-360 Hanging indent: the first line is on the left, the other lines are indented. Excepteur sint ' +
      'occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\par' +
    '\pard\li1440\ri1440\i Quote with left and right indent of one inch: Sed ut perspiciatis unde ' +
      'omnis iste natus error sit voluptatem accusantium doloremque laudantium.\i0\par' +
    '\pard\sb360\sa360 Paragraph with 18 pt spacing before and after.\par' +
    '\pard\sl360 Line spacing 1.5: Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, ' +
      'sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt.\par' +

    '\pard\sb240\sa120\fs28\b\cf3 7. Tab stops\cf0\b0\par' +
    '\pard\tx2160\tx4320\tx6480\fs22\b Item\tab Quantity\tab Price\tab Amount\b0\par' +
    '\pard\tx2160\tx4320\tx6480 Screw M6\tab 100\tab 0.12\tab 12.00\par' +
    '\pard\tx2160\tx4320\tx6480 Nut M6\tab 250\tab 0.05\tab 12.50\par' +

    '\pard\sb240\sa120\fs28\b\cf3 8. Lists (via API)\cf0\b0\par' +
    '\pard\fs22 Bullet item one\par' +
    '\pard Bullet item two\par' +
    '\pard Bullet item three\par' +
    '\pard Numbered item one\par' +
    '\pard Numbered item two\par' +
    '\pard Numbered item three\par' +
    '\pard Letter item one\par' +
    '\pard Letter item two\par' +
    '\pard Roman item one\par' +
    '\pard Roman item two\par' +
    '\pard Roman item three\par' +

    '\pard\sb240\sa120\fs28\b\cf3 9. Link and image (via API)\cf0\b0\par' +
    '\pard\fs22 Visit www.lazarus-ide.org for more information.\par' +
    '\pard PNG 48x48 px without resizing: [IMAGE1]\par' +
    '\pard PNG set to 24x24 pt: [IMAGE2]\par' +

    '\pard\sb240\sa120\fs28\b\cf3 10. Line break\cf0\b0\par' +
    '\pard\fs22 First line\line second line of the same paragraph\line third line of the same paragraph.\par' +
    '}';

  FontSizes: array[0..15] of Integer = (8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 28, 32, 36, 48, 72);

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RegisterRTFLoader;
  RegisterRTFSaver;
  cbFontName.Items.Assign(Screen.Fonts);
  for i := Low(FontSizes) to High(FontSizes) do
    cbFontSize.Items.Add(IntToStr(FontSizes[i]));
  RichMemo1.OnLinkAction := @RichMemoLinkAction;
  RichMemo1.OnSelectionChange := @RichMemoSelectionChange;
  RichMemo1.OnChange := @RichMemoChange;
  RichMemo1.OnKeyDown := @RichMemoKeyDown;
  RichMemo1.OnMouseDown := @RichMemoMouseDown;
  RichMemo1.OnMouseUp := @RichMemoMouseUp;
  RichMemo1.OnKeyUp := @RichMemoKeyUp;
  FFileName := '';
  LoadTestContent;
  ClearPending;
  UpdateCaption;
  UpdateToolbar;
end;

procedure TForm1.ApplyNumbering(const AFirst, ALast: string; const ANumber: TParaNumbering);
var
  AStart: Integer;
  AEnd: Integer;
begin
  AStart := RichMemo1.Search(AFirst, 0, -1, [soMatchCase]);
  if AStart < 0 then
    Exit;
  AEnd := RichMemo1.Search(ALast, AStart, -1, [soMatchCase]);
  if AEnd < 0 then
    Exit;
  RichMemo1.SetParaNumbering(AStart, AEnd - AStart + 1, ANumber);
end;

procedure TForm1.LoadTestContent;
var
  ss: TStringStream;
  num: TParaNumbering;
  p: Integer;
  img: string;
begin
  ss := TStringStream.Create(TestRtf);
  try
    RichMemo1.LoadRichText(ss);
  finally
    ss.Free;
  end;

  InitParaNumbering(num);
  num.Style := pnBullet;
  ApplyNumbering('Bullet item one', 'Bullet item three', num);

  InitParaNumber(num, SepDot, 1);
  ApplyNumbering('Numbered item one', 'Numbered item three', num);

  InitParaNumber(num, SepPar, 1);
  num.Style := pnLowLetter;
  ApplyNumbering('Letter item one', 'Letter item two', num);

  InitParaNumber(num, SepDot, 1);
  num.Style := pnUpRoman;
  ApplyNumbering('Roman item one', 'Roman item three', num);

  p := RichMemo1.Search('www.lazarus-ide.org', 0, -1, [soMatchCase]);
  if p >= 0 then
    RichMemo1.SetLink(p, Length('www.lazarus-ide.org'), True, 'https://www.lazarus-ide.org');

  img := ExtractFilePath(Application.ExeName) + 'test48.png';
  if FileExists(img) then
  begin
    p := RichMemo1.Search('[IMAGE1]', 0, -1, [soMatchCase]);
    if p >= 0 then
      InsertImageFromFileNoResize(RichMemo1, p + Length('[IMAGE1]'), img);
    p := RichMemo1.Search('[IMAGE2]', 0, -1, [soMatchCase]);
    if p >= 0 then
      InsertImageFromFile(RichMemo1, p + Length('[IMAGE2]'), img, Size(24, 24));
  end;

  RichMemo1.SelStart := 0;
end;

procedure TForm1.ApplyFontParams(const AMask: TTextModifyMask; const AParams: TFontParams;
  const AAddStyle, ARemoveStyle: TFontStyles);
begin
  if RichMemo1.SelLength > 0 then
    RichMemo1.SetRangeParams(RichMemo1.SelStart, RichMemo1.SelLength, AMask, AParams,
      AAddStyle, ARemoveStyle)
  else
  begin
    FPending := True;
    FPendingMask := FPendingMask + AMask;
    if tmm_Name in AMask then
      FPendingParams.Name := AParams.Name;
    if tmm_Size in AMask then
      FPendingParams.Size := AParams.Size;
    if tmm_Color in AMask then
      FPendingParams.Color := AParams.Color;
    if tmm_BackColor in AMask then
    begin
      FPendingParams.HasBkClr := AParams.HasBkClr;
      FPendingParams.BkColor := AParams.BkColor;
    end;
    if tmm_Styles in AMask then
    begin
      FPendingAdd := FPendingAdd + AAddStyle - ARemoveStyle;
      FPendingRemove := FPendingRemove + ARemoveStyle - AAddStyle;
    end;
    FLastTextLen := TextLength;
  end;
  if RichMemo1.CanFocus then
    RichMemo1.SetFocus;
end;

procedure TForm1.ClearPending;
begin
  FPending := False;
  FPendingMask := [];
  InitFontParams(FPendingParams);
  FPendingAdd := [];
  FPendingRemove := [];
  FLastTextLen := TextLength;
end;

function TForm1.TextLength: Integer;
begin
  Result := UTF8Length(RichMemo1.Text);
end;

procedure TForm1.RichMemoChange(Sender: TObject);
var
  ALen, ADelta, AStart: Integer;
begin
  if FApplyingPending then
    Exit;
  ALen := TextLength;
  if FPending and (ALen > FLastTextLen) then
  begin
    ADelta := ALen - FLastTextLen;
    AStart := RichMemo1.SelStart - ADelta;
    if AStart >= 0 then
    begin
      FApplyingPending := True;
      try
        RichMemo1.SetRangeParams(AStart, ADelta, FPendingMask, FPendingParams,
          FPendingAdd, FPendingRemove);
      finally
        FApplyingPending := False;
      end;
    end;
  end;
  FLastTextLen := TextLength;
end;

procedure TForm1.RichMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FPending and (Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT]) then
  begin
    ClearPending;
    UpdateToolbar;
  end;
end;

procedure TForm1.RichMemoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FPending then
    ClearPending;
end;

procedure TForm1.RichMemoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateToolbar;
end;

procedure TForm1.RichMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] then
    UpdateToolbar;
end;

procedure TForm1.ApplyFontName;
var
  fp: TFontParams;
begin
  if FUpdatingToolbar or (Trim(cbFontName.Text) = '') then
    Exit;
  InitFontParams(fp);
  fp.Name := Trim(cbFontName.Text);
  ApplyFontParams([tmm_Name], fp, [], []);
end;

procedure TForm1.ApplyFontSize;
var
  fp: TFontParams;
  ASize: Integer;
begin
  if FUpdatingToolbar then
    Exit;
  ASize := StrToIntDef(Trim(cbFontSize.Text), 0);
  if (ASize < 1) or (ASize > 1638) then
    Exit;
  InitFontParams(fp);
  fp.Size := ASize;
  ApplyFontParams([tmm_Size], fp, [], []);
end;

procedure TForm1.cbFontNameSelect(Sender: TObject);
begin
  ApplyFontName;
end;

procedure TForm1.cbFontNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    ApplyFontName;
  end;
end;

procedure TForm1.cbFontSizeSelect(Sender: TObject);
begin
  ApplyFontSize;
end;

procedure TForm1.cbFontSizeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    ApplyFontSize;
  end;
end;

procedure TForm1.sbStyleClick(Sender: TObject);
var
  fp: TFontParams;
  AStyle: TFontStyle;
begin
  if FUpdatingToolbar then
    Exit;
  if Sender = sbBold then
    AStyle := fsBold
  else
  if Sender = sbItalic then
    AStyle := fsItalic
  else
  if Sender = sbUnderline then
    AStyle := fsUnderline
  else
    AStyle := fsStrikeOut;
  InitFontParams(fp);
  if TSpeedButton(Sender).Down then
    ApplyFontParams([tmm_Styles], fp, [AStyle], [])
  else
    ApplyFontParams([tmm_Styles], fp, [], [AStyle]);
end;

procedure TForm1.cbtnFontColorColorChanged(Sender: TObject);
var
  fp: TFontParams;
begin
  if FUpdatingToolbar then
    Exit;
  InitFontParams(fp);
  fp.Color := cbtnFontColor.ButtonColor;
  ApplyFontParams([tmm_Color], fp, [], []);
end;

procedure TForm1.cbtnBackColorColorChanged(Sender: TObject);
var
  fp: TFontParams;
begin
  if FUpdatingToolbar then
    Exit;
  InitFontParams(fp);
  fp.HasBkClr := True;
  fp.BkColor := cbtnBackColor.ButtonColor;
  ApplyFontParams([tmm_BackColor], fp, [], []);
end;

procedure TForm1.tbNoBackColorClick(Sender: TObject);
var
  fp: TFontParams;
begin
  InitFontParams(fp);
  fp.HasBkClr := False;
  ApplyFontParams([tmm_BackColor], fp, [], []);
end;

procedure TForm1.tbAlignClick(Sender: TObject);
var
  AAlign: TParaAlignment;
  ALen: Integer;
begin
  if FUpdatingToolbar then
    Exit;
  if Sender = tbAlignCenter then
    AAlign := paCenter
  else
  if Sender = tbAlignRight then
    AAlign := paRight
  else
  if Sender = tbAlignJustify then
    AAlign := paJustify
  else
    AAlign := paLeft;
  ALen := RichMemo1.SelLength;
  if ALen < 1 then
    ALen := 1;
  RichMemo1.SetParaAlignment(RichMemo1.SelStart, ALen, AAlign);
  RichMemo1.SetFocus;
end;

procedure TForm1.UpdateToolbar;
var
  fp: TFontParams;
  AAlign: TParaAlignment;
  APos: Integer;
  APrev: string;
begin
  FUpdatingToolbar := True;
  try
    InitFontParams(fp);
    APos := RichMemo1.SelStart;
    if (RichMemo1.SelLength = 0) and (APos > 0) then
    begin
      APrev := RichMemo1.GetText(APos - 1, 1);
      if (APrev <> '') and not (APrev[1] in [' ', #9, #10, #13]) then
        Dec(APos);
    end;
    if RichMemo1.GetTextAttributes(APos, fp) then
    begin
      if FPending then
      begin
        if tmm_Name in FPendingMask then
          fp.Name := FPendingParams.Name;
        if tmm_Size in FPendingMask then
          fp.Size := FPendingParams.Size;
        if tmm_Color in FPendingMask then
          fp.Color := FPendingParams.Color;
        if tmm_BackColor in FPendingMask then
        begin
          fp.HasBkClr := FPendingParams.HasBkClr;
          fp.BkColor := FPendingParams.BkColor;
        end;
        if tmm_Styles in FPendingMask then
          fp.Style := fp.Style + FPendingAdd - FPendingRemove;
      end;
      cbFontName.Text := fp.Name;
      cbFontSize.Text := IntToStr(fp.Size);
      sbBold.Down := fsBold in fp.Style;
      sbItalic.Down := fsItalic in fp.Style;
      sbUnderline.Down := fsUnderline in fp.Style;
      sbStrike.Down := fsStrikeOut in fp.Style;
      cbtnFontColor.ButtonColor := fp.Color;
      if fp.HasBkClr then
        cbtnBackColor.ButtonColor := fp.BkColor
      else
      if RichMemo1.Color = clDefault then
        cbtnBackColor.ButtonColor := clWindow
      else
        cbtnBackColor.ButtonColor := RichMemo1.Color;
    end;
    AAlign := paLeft;
    if RichMemo1.GetParaAlignment(RichMemo1.SelStart, AAlign) then
    begin
      tbAlignLeft.Down := AAlign = paLeft;
      tbAlignCenter.Down := AAlign = paCenter;
      tbAlignRight.Down := AAlign = paRight;
      tbAlignJustify.Down := AAlign = paJustify;
    end;
  finally
    FUpdatingToolbar := False;
  end;
end;

procedure TForm1.UpdateCaption;
begin
  if FFileName = '' then
    Caption := 'RichMemo editor - untitled'
  else
    Caption := 'RichMemo editor - ' + ExtractFileName(FFileName);
end;

procedure TForm1.LoadFromFile(const AFileName: string);
var
  fs: TFileStream;
begin
  if SameText(ExtractFileExt(AFileName), '.rtf') then
  begin
    fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      RichMemo1.LoadRichText(fs);
    finally
      fs.Free;
    end;
  end else
    RichMemo1.Lines.LoadFromFile(AFileName);
  FFileName := AFileName;
  RichMemo1.SelStart := 0;
  ClearPending;
  UpdateCaption;
  UpdateToolbar;
end;

procedure TForm1.SaveToFile(const AFileName: string);
var
  fs: TFileStream;
begin
  if SameText(ExtractFileExt(AFileName), '.txt') then
    RichMemo1.Lines.SaveToFile(AFileName)
  else
  begin
    fs := TFileStream.Create(AFileName, fmCreate);
    try
      RichMemo1.SaveRichText(fs);
    finally
      fs.Free;
    end;
  end;
  FFileName := AFileName;
  UpdateCaption;
end;

procedure TForm1.miNewClick(Sender: TObject);
begin
  RichMemo1.Clear;
  FFileName := '';
  ClearPending;
  UpdateCaption;
  UpdateToolbar;
  if RichMemo1.CanFocus then
    RichMemo1.SetFocus;
end;

procedure TForm1.miOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.miSaveClick(Sender: TObject);
begin
  if FFileName = '' then
    miSaveAsClick(Sender)
  else
    SaveToFile(FFileName);
end;

procedure TForm1.miSaveAsClick(Sender: TObject);
begin
  if FFileName <> '' then
    SaveDialog1.FileName := FFileName;
  if SaveDialog1.Execute then
    SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.miPrintClick(Sender: TObject);
begin
  ShowMessage('Printing is not implemented yet.');
end;

procedure TForm1.miCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.miCutClick(Sender: TObject);
begin
  RichMemo1.CutToClipboard;
end;

procedure TForm1.miCopyClick(Sender: TObject);
begin
  RichMemo1.CopyToClipboard;
end;

procedure TForm1.miPasteClick(Sender: TObject);
begin
  RichMemo1.PasteFromClipboard;
end;

procedure TForm1.miSelectAllClick(Sender: TObject);
begin
  RichMemo1.SelectAll;
end;

procedure TForm1.RichMemoLinkAction(Sender: TObject; ALinkAction: TLinkAction;
  const info: TLinkMouseInfo; LinkStart, LinkLen: Integer);
begin
  Caption := Format('Link click: start=%d len=%d text="%s"',
    [LinkStart, LinkLen, RichMemo1.GetText(LinkStart, LinkLen)]);
end;

procedure TForm1.RichMemoSelectionChange(Sender: TObject);
begin
  if FPending and (RichMemo1.SelLength > 0) then
    ClearPending;
  UpdateToolbar;
end;

end.
