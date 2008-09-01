unit mainunit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LCLType, LCLIntf, StdCtrls, Buttons, LCLProc, ComCtrls, ExtCtrls, Grids,
  FileUtil, IniFiles;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    BtnFontDlg: TButton;
    cbCharset: TComboBox;
    cbPitch: TComboBox;
    chkStrike: TCheckBox;
    chkUnderLine: TCheckBox;
    FontDialog1: TFontDialog;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblCharset: TLabel;
    Label6: TLabel;
    Sizes: TLabel;
    lbFamily: TListBox;
    lbStyles: TListBox;
    lbSizes: TListBox;
    lbCharset: TListBox;
    grid: TStringGrid;
    procedure BtnFontDlgClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbFamilyClick(Sender: TObject);
    procedure lbCharsetClick(Sender: TObject);
    procedure lbSizesClick(Sender: TObject);
    procedure lbStylesClick(Sender: TObject);
  private
    { private declarations }
    FTime: LongWord;
    FIniTime: LongWord;
    FCurrentFamily,FCurrentStyle,FCurrentSize,FCurrentCharset: string;
    procedure StartTimer;
    Procedure EndTimer;
    function  GetCharSet: byte;
    function  GetPitch: integer;
    procedure EnableEvents(Ok: boolean; Lb: TListbox = nil);
    procedure SelectFont;
    procedure ResetSampleText;
    procedure SaveSelection;
    procedure RestoreSelection(Sender: TListbox);
    procedure LoadFontList;
    procedure LoadFamilyFonts(Charset: integer);
    procedure UpdateFont(F: TFont);
    
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation
{.$define Debug}

{ TfrmMain }
var
  LStyles,
  LSizes: TStringList;

function EnumFontsNoDups(
  var LogFont: TEnumLogFontEx;
  var Metric: TNewTextMetricEx;
  FontType: Longint;
  Data: LParam):LongInt; stdcall;
var
  L: TStringList;
  S: String;
begin
  L := TStringList(ptrint(Data));
  S := LogFont.elfLogFont.lfFaceName;
  if L.IndexOf(S)<0 then
    L.Add(S);
  result := 1;
end;

var
  NeedTTF: boolean;
  
function EnumFamilyFonts(
  var eLogFont: TEnumLogFontEx;
  var Metric:TNewTextMetricEx;
  FontType:longint;
  Data:LParam):longint; stdcall;
var
  s: string;
  n: integer;
  lcharsets: TStringList;
begin
  LCharSets := TStringList(ptrint(Data));
  if Lcharsets<>nil then begin
    // collect charsets
    // when collecting charsets no need to collect all other info
    s :=CharSetToString(eLogFont.elfLogFont.lfCharSet);
    if LCharsets.indexOf(s)<0 then
      LCharsets.AddObject(s, TObject(ptrint(eLogFont.elfLogFont.lfCharSet)));
    exit;
  end;
  // collect styles
  s :=eLogFont.elfStyle;
  if LStyles.IndexOf(s)<0 then begin
    // encode bold, italic
    n := eLogFont.elfLogFont.lfItalic;
    if eLogFont.elfLogFont.lfWeight > FW_MEDIUM then
      n := n or 2;
    LStyles.AddObject(eLogFont.elfStyle, TObject(ptrint(n)));
  end;
  // collect sizes
  if FontType=TRUETYPE_FONTTYPE then
    NeedTTF := True
  else
    with metric.ntmentm do
      if tmDigitizedAspectY <> 0 then begin
        n := (tmHeight-tmInternalLeading)*72+tmDigitizedAspectY shr 1;
        n := n div tmDigitizedAspectY;
        if n>0 then begin
          s := IntToStr(n)+'*'; // font sizes with * indicate raster fonts
          if LSizes.IndexOf(s)<0 then
            LSizes.AddObject(s, TObject(ptrint(n)));
        end;
      end;
  result := 1;
end;

procedure TfrmMain.BtnFontDlgClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    UpdateFont(FontDialog1.Font);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  ResetSampleText;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  LoadFontList;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Ini: TInifile;
begin
  SaveSelection;
  Ini := TIniFile.Create(UTF8ToSys(ChangeFileExt(Application.ExeName,'.ini')));
  try
    Ini.WriteString('General','CurrentFamily', FCurrentFamily);
    Ini.WriteString('General','CurrentCharset',FCurrentCharset);
    Ini.WriteString('General','CurrentStyle',  FCurrentStyle);
    Ini.WriteString('General','CurrentSize',   FCurrentSize);
  finally
    Ini.Free;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
  procedure Add(Charset: Integer);
  begin
    cbCharset.Items.AddObject(CharSetToString(CharSet), TObject(ptrint(Charset)));
  end;
var
  Ini: TIniFile;
begin
  // populate cbcharset
  cbCharset.Items.clear;
  Add(ANSI_CHARSET);
  Add(DEFAULT_CHARSET);
  Add(SYMBOL_CHARSET);
  Add(MAC_CHARSET);
  Add(SHIFTJIS_CHARSET);
  Add(HANGEUL_CHARSET);
  Add(JOHAB_CHARSET);
  Add(GB2312_CHARSET);
  Add(CHINESEBIG5_CHARSET);
  Add(GREEK_CHARSET);
  Add(TURKISH_CHARSET);
  Add(VIETNAMESE_CHARSET);
  Add(HEBREW_CHARSET);
  Add(ARABIC_CHARSET);
  Add(BALTIC_CHARSET);
  Add(RUSSIAN_CHARSET);
  Add(THAI_CHARSET);
  Add(EASTEUROPE_CHARSET);
  Add(OEM_CHARSET);
  Add(FCS_ISO_10646_1);
  Add(FCS_ISO_8859_1);
  Add(FCS_ISO_8859_2);
  Add(FCS_ISO_8859_3);
  Add(FCS_ISO_8859_4);
  Add(FCS_ISO_8859_5);
  Add(FCS_ISO_8859_6);
  Add(FCS_ISO_8859_7);
  Add(FCS_ISO_8859_8);
  Add(FCS_ISO_8859_9);
  Add(FCS_ISO_8859_10);
  Add(FCS_ISO_8859_15);
  ResetSampleText;
  
  Ini := TIniFile.Create(UTF8ToSys(ChangeFileExt(Application.ExeName,'.ini')));
  try
    FCurrentFamily  := Ini.ReadString('General','CurrentFamily', '');
    FCurrentCharset := Ini.ReadString('General','CurrentCharset','');
    FCurrentStyle   := Ini.ReadString('General','CurrentStyle',  '');
    FCurrentSize    := Ini.ReadString('General','CurrentSize',   '');
  finally
    Ini.Free;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  LoadFontlist;
end;

procedure TfrmMain.lbFamilyClick(Sender: TObject);
begin
  LoadFamilyFonts(-1);
end;

procedure TfrmMain.lbCharsetClick(Sender: TObject);
var
  i: Integer;
begin
  i := lbCharset.ItemIndex;
  if i<0 then exit;
  i := ptrint(lbCharSet.Items.Objects[i]);
  LoadFamilyFonts(byte(i));
end;

procedure TfrmMain.lbSizesClick(Sender: TObject);
begin
  SelectFont;
end;

procedure TfrmMain.lbStylesClick(Sender: TObject);
begin
  SelectFont;
end;

procedure TfrmMain.StartTimer;
begin
  FIniTime := GetTickCount;
end;

procedure TfrmMain.EndTimer;
begin
  FTime := GetTickCount-FIniTime;
end;

function TfrmMain.GetCharSet: Byte;
begin
  if cbCharSet.Itemindex<0 then
    result := ANSI_CHARSET
  else
    result := byte(ptrint(cbCharset.items.Objects[CbCharset.ItemIndex]));
end;

function TfrmMain.GetPitch: integer;
begin
  case cbPitch.ItemIndex of
    1: result := FIXED_PITCH;
    2: result := VARIABLE_PITCH;
    3: result := MONO_FONT;
    else
      result := DEFAULT_PITCH;
  end;
  Button2.Caption := IntToStr(result);
end;

procedure TfrmMain.EnableEvents(Ok: boolean; Lb: TListbox = nil);
  procedure SetEvent(L: TListbox);
  var
    Event: TNotifyEvent;
  begin
    Event := nil;
    if ok then begin
      if l=lbFamily then Event := @lbFamilyClick else
      if l=lbStyles then Event := @LbStylesClick else
      if l=lbCharset then Event := @lbCharsetClick else
      if l=lbSizes then Event := @lbSizesClick;
    end;
    L.OnClick := Event;
  end;
begin
  if Lb<>nil then
    SetEvent(Lb)
  else begin
    SetEvent(lbFamily);
    SetEvent(lbStyles);
    SetEvent(lbCharset);
    SetEvent(lbSizes);
  end;
end;

procedure TfrmMain.SelectFont;
var
  F: TFont;
  i: integer;
  function GetFontSize(s: string): Integer;
  begin
    i := pos('*',s);
    if i<>0 then
      result := StrToInt(Copy(S, 1, i-1))
    else
      result := StrToInt(s);
  end;
begin
  if lbFamily.ItemIndex>=0 then
    if lbCharSet.ItemIndex>=0 then
      if lbStyles.ItemIndex>=0 then
        if lbSizes.ItemIndex>=0 then
        begin
          F := TFont.Create;
          try
            F.Name := lbFamily.Items[lbFamily.ItemIndex];
            F.CharSet := TFontCharSet(ptrint(lbCharSet.Items.Objects[lbCharset.ItemIndex]));
            F.Size := GetFontSize(lbSizes.Items[lbSizes.ItemIndex]);
            i := ptrint(lbStyles.Items.Objects[lbStyles.ItemIndex]);
            F.Style := [];
            if i and 1 <> 0 then F.Style := F.Style + [fsItalic];
            if i and 2 <> 0 then F.Style := F.Style + [fsBold];
            UpdateFont(F);
            SaveSelection;
          finally
            F.Free;
          end;
        end;
end;

procedure TfrmMain.ResetSampleText;
var
  L: TStringList;
begin
  L := TStringList.Create;
  L.Add('abcdefhijklmnopqrstuvwxyz');
  L.Add('ABCDEFGHIJKLMNOPQRSTUVWXYZ');
  L.Add('01234567891   ¢£¤¥§');
  L.Add('àáâãäåæçèéêëìíîïðñòóôõöøùúûü');
  L.add('ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÕØÙÚÛÜÝß');
  grid.Cols[0] := L;
  l.Free;
end;

procedure TfrmMain.SaveSelection;
  function doGet(lb: TListbox): string;
  begin
    if lb.itemindex>=0 then
      result := lb.Items[lb.ItemIndex]
    else
      result := '';
  end;
begin
  FCurrentFamily := doGet(LbFamily);
  FCurrentCharset := doGet(LbCharset);
  FCurrentStyle := doGet(LbStyles);
  FCurrentSize := doGet(LbSizes);
end;

procedure TfrmMain.RestoreSelection(Sender: TListbox);
  function GetSelection: string;
  begin
    if Sender.itemindex>=0 then
      result := Sender.Items[Sender.ItemIndex]
    else
      result := '';
  end;
  function GetCurrent: string;
  begin
    if Sender=lbFamily then result := FCurrentFamily else
    if Sender=lbCharset then result := FCurrentCharset else
    if Sender=lbStyles then result := FCurrentStyle else
    if Sender=lbSizes then result := FCurrentSize;
  end;
var
  i: Integer;
  s: string;
begin
  s := GetCurrent;
  if GetSelection <> s then begin
    i := Sender.Items.IndexOf(s);
    if i>-1 then begin
      {$ifdef debug}
      WriteLn('RestoreSelection: listbox=',Sender.Name,' Old=',GetSelection,' New=',S);
      {$endif}
      if i<>Sender.ItemIndex then
        Sender.ItemIndex := i;
    end;
  end;
end;

procedure TfrmMain.LoadFontList;
var
  DC: HDC;
  lf: TLogFont;
  L: TStringList;
  i: Integer;
begin
  // this could be have done also with screen.fonts
  // but here, we have the list filtered by Charset
  lf.lfCharSet := GetCharSet;
  lf.lfFaceName := '';
  case cbPitch.ItemIndex of
    1: i:=FIXED_PITCH;
    2: i:=VARIABLE_PITCH;
    3: i:=MONO_FONT;
    else
      i:=DEFAULT_PITCH;
  end;
  lf.lfPitchAndFamily := i;

  {$ifdef debug}
  WriteLn('LoadFontList: for charset=',CharSetToString(lf.lfcharset));
  {$endif}

  L := TStringList.create;
  lbStyles.Clear;
  lbCharset.Clear;
  lbSizes.Clear;

  DC := GetDC(0);
  EnableEvents(False, lbFamily);
  try
    StartTimer;
    EnumFontFamiliesEX(DC, @lf, @EnumFontsNoDups, ptrint(L), 0);
    EndTimer;
    L.Sort;
    lbFamily.Items.Assign(L);
    lbFamily.Itemindex := -1;
    
    RestoreSelection(lbFamily);
    if lbFamily.ItemIndex<0 then begin
      if lbFamily.Items.Count>0 then
        lbFamily.ItemIndex := 0;
    end;
    LoadFamilyFonts(-1);
    
    Label4.Caption := format('Fontfaces, found %d, %d ms',[lbFamily.Items.Count, FTime]);
  finally
    EnableEvents(True, lbFamily);
    ReleaseDC(0, DC);
    L.Free;
  end;
end;

function CompareSizes(List: TStringList; Index1, Index2: Integer): Integer;
begin
  result := ptrint(List.Objects[Index1]) - ptrint(List.Objects[Index2]);
end;

procedure TfrmMain.LoadFamilyFonts(Charset: integer);
var
  LCharset: TStringList;
  dc: HDC;
  Lf: TLogFont;
  i: LongInt;
  LoadingCharsets: boolean;
  
  procedure AddScalableSizes;
    procedure Add(Sz: Integer);
    begin
      if LSizes.IndexOfObject(TObject(ptrint(Sz)))<0 then
        LSizes.AddObject(IntToStr(Sz), TObject(ptrint(Sz)));
    end;
  begin
    add(8);  add(9);  add(10); add(11); add(12); add(14); add(16); add(18);
    add(20); add(22); add(24); add(26); add(28); add(36); add(48); add(72);
  end;
begin
  i := lbFamily.ItemIndex;
  if i<0 then exit;
  
  LoadingCharsets := Charset<0;
  {$ifdef debug}
  Write('LoadFamilyFonts: for family=', lbFamily.Items[i],' and Charset=');
  if LoadingCharsets then
    WriteLn('ALL_CHARSETS')
  else
    WriteLn(CharsetToString(byte(Charset)));
  {$endif}

  // at the moment only global fonts are enumerated
  // ie. fonts selected in a device context are not enumerated
  DC := GetDC(0);
  // create global variables, EnumFamilyFonts use them
  if LoadingCharsets then begin
    // need to fill charset listbox too
    LCharset := TStringList.Create;
    CharSet := DEFAULT_CHARSET;
  end else begin
    // charset listbox is already filled, so fill styles and sizes
    LCharSet := nil;
    LStyles := TStringList.Create;
    LSizes  := TStringList.Create;
  end;
  try
    // enumerate fonts
    Lf.lfFaceName := lbFamily.Items[i];
    Lf.lfCharSet := byte(Charset);
    Lf.lfPitchAndFamily := 0;
    NeedTTF := False;
    EnumFontFamiliesEX(DC, @Lf, @EnumFamilyFonts, ptrint(LCharset), 0);
    // fill charset listbox if necessary
    if LCharset<>nil then begin
      LCharset.Sort;
      EnableEvents(False, LbCharset);
      LbCharset.Items.Assign(LCharset);
      LbCharset.ItemIndex := -1;
      EnableEvents(true, LbCharset);
    end else begin
      // fill styles listbox
      LStyles.Sort;
      EnableEvents(False, LbStyles);
      LbStyles.Items.Assign(LStyles);
      lbStyles.ItemIndex := -1;
      EnableEvents(true, LbStyles);
      RestoreSelection(lbStyles);
      if lbStyles.ItemIndex<0 then begin
       if LbStyles.Items.Count>0 then
          LbStyles.ItemIndex := 0;
      end;
      // fill sizes listbox
      // any raster font size is already there
      if NeedTTF then
        AddScalableSizes;
      LSizes.CustomSort(@CompareSizes);
      EnableEvents(False, lbSizes);
      lbSizes.Items.Assign(LSizes);
      lbSizes.ItemIndex := -1;
      EnableEvents(true, LbSizes);
      RestoreSelection(LbSizes);
      if lbSizes.ItemIndex<0 then begin
        if lbSizes.Items.Count>0 then
          LbSizes.ItemIndex := 0;
      end;
    end;
  finally
    if LCharset=nil then begin
      LSizes.Free;
      LStyles.Free;
    end else
      LCharset.Free;
    releaseDC(0, DC);
  end;
  
  if LoadingCharsets then begin
    // make an initial charset selection
    RestoreSelection(lbCharset);
    if lbCharset.ItemIndex<0 then begin
      if lbCharset.Items.Count>0 then
        lbCharset.ItemIndex := 0;
    end;
  end;
end;

procedure TfrmMain.UpdateFont(F: TFont);
begin
  grid.Font := F;
  grid.DefaultRowHeight := grid.canvas.textHeight('Áj') + 5;
end;

initialization
  {$I mainunit.lrs}

end.

