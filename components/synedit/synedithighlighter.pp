{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditHighlighter.pas, released 2000-04-07.

The Original Code is based on mwHighlighter.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditHighlighter;

{$I synedit.inc}

interface

uses
  SysUtils, Classes,
{$IFDEF SYN_CLX}
  kTextDrawer,
  Types,
  QGraphics,
{$ELSE}
  Graphics,
  {$IFDEF SYN_LAZARUS}
  FileUtil, LCLProc, LCLIntf, LCLType, AvgLvlTree,
  {$ELSE}
  Windows,
  {$ENDIF}
  Registry,
  IniFiles,
{$ENDIF}
  SynEditTypes,
  SynEditMiscClasses,
  SynEditTextBuffer, SynEditTextBase;

{$DEFINE _Gp_MustEnhanceRegistry}
{$IFDEF SYN_COMPILER_4_UP}
  {$UNDEF _Gp_MustEnhanceRegistry}
{$ENDIF}
type
  TBetterRegistry = class(TRegistry)
  {$IFDEF _Gp_MustEnhanceRegistry}
    function OpenKeyReadOnly(const Key: string): Boolean;
  {$ENDIF}
  end;

  { TSynHighlighterAttributes }

  TSynHighlighterAttributes = class(TPersistent)
  private
    fBackground: TColor;
    fBackgroundDefault: TColor;                                                 //mh 2000-10-08
    fForeground: TColor;
    fForegroundDefault: TColor;                                                 //mh 2000-10-08
    FFrameColor: TColor;
    FFrameColorDefault: TColor;
    fName: string;
    fStyle: TFontStyles;
    fStyleDefault: TFontStyles;                                                 //mh 2000-10-08
    {$IFDEF SYN_LAZARUS}
    fStyleMask: TFontStyles;
    fStyleMaskDefault: TFontStyles;                                                 //mh 2000-10-08
    {$ENDIF}
    fOnChange: TNotifyEvent;
    procedure Changed; virtual;
{begin}                                                                         //mh 2000-10-08
    function GetBackgroundColorStored: boolean;
    function GetFontStyleMaskStored : boolean;
    function GetForegroundColorStored: boolean;
    function GetFrameColorStored: boolean;
{end}                                                                           //mh 2000-10-08
    procedure SetBackground(Value: TColor);
    procedure SetForeground(Value: TColor);
    procedure SetFrameColor(const AValue: TColor);
    procedure SetStyle(Value: TFontStyles);
    function GetStyleFromInt: integer;
    procedure SetStyleFromInt(const Value: integer);
    function GetFontStyleStored: boolean;
    {$IFDEF SYN_LAZARUS}
    procedure SetStyleMask(const AValue : TFontStyles);
    function GetStyleMaskFromInt : integer;
    procedure SetStyleMaskFromInt(const Value : integer);
    {$ENDIF}
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(attribName: string);
    procedure InternalSaveDefaultValues;                                        //mh 2000-10-08
    function LoadFromBorlandRegistry(rootKey: HKEY; attrKey, attrName: string;
      oldStyle: boolean): boolean; virtual;
    function LoadFromRegistry(Reg: TBetterRegistry): boolean;
    function SaveToRegistry(Reg: TBetterRegistry): boolean;
    function LoadFromFile(Ini : TIniFile): boolean;                             //DDH 10/16/01
    function SaveToFile(Ini : TIniFile): boolean;                               //DDH 10/16/01
  public
    property IntegerStyle: integer read GetStyleFromInt write SetStyleFromInt;
    {$IFDEF SYN_LAZARUS}
    property IntegerStyleMask: integer read GetStyleMaskFromInt write SetStyleMaskFromInt;
    {$ENDIF}
    property Name: string read fName;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  published
    property Background: TColor read fBackground write SetBackground
      stored GetBackgroundColorStored;                                          //mh 2000-10-08
    property Foreground: TColor read fForeground write SetForeground
      stored GetForegroundColorStored;                                          //mh 2000-10-08
    property FrameColor: TColor read FFrameColor write SetFrameColor
      stored GetFrameColorStored;
    property Style: TFontStyles read fStyle write SetStyle //default [];
      stored GetFontStyleStored;                                                //mh 2000-10-08
    {$IFDEF SYN_LAZARUS}
    property StyleMask: TFontStyles read fStyleMask write SetStyleMask //default [];
      stored GetFontStyleMaskStored;                                                //mh 2000-10-08
    {$ENDIF}
  end;

  TSynHighlighterCapability = (
    hcUserSettings, // supports Enum/UseUserSettings
    hcRegistry      // supports LoadFrom/SaveToRegistry
    {$IFDEF SYN_LAZARUS}
    ,hcCodeFolding  // supports codefolding
    {$ENDIF}
  );

  TSynHighlighterCapabilities = set of TSynHighlighterCapability;

  {$IFDEF SYN_LAZARUS}
  { TSynCustomCodeFoldBlock }

  TSynCustomCodeFoldBlock = class
  private
    FBlockType: Pointer;
    FParent, FChildren: TSynCustomCodeFoldBlock;
    FRight, FLeft: TSynCustomCodeFoldBlock;
    FBalance: Integer;
    function GetChild(ABlockType: Pointer): TSynCustomCodeFoldBlock;
  protected
    function GetOrCreateSibling(ABlockType: Pointer): TSynCustomCodeFoldBlock;
    property Right: TSynCustomCodeFoldBlock read FRight;
    property Left: TSynCustomCodeFoldBlock read FLeft;
    property Children: TSynCustomCodeFoldBlock read FChildren;
  public
    destructor Destroy; override;
    function Compare(Block: TSynCustomCodeFoldBlock): integer; virtual;
    procedure Assign(Src: TSynCustomCodeFoldBlock); virtual;
  public
    property BlockType: Pointer read FBlockType write FBlockType;
    property Parent: TSynCustomCodeFoldBlock read FParent;
    property Child[ABlockType: Pointer]: TSynCustomCodeFoldBlock read GetChild;
  end;
  TSynCustomCodeFoldBlockClass = class of TSynCustomCodeFoldBlock;

  { TSynCustomHighlighterRange }

  TSynCustomHighlighterRange = class
  private
    FCodeFoldStackSize: integer;
    FRangeType: Pointer;
    FTop: TSynCustomCodeFoldBlock;
  public
    constructor Create(Template: TSynCustomHighlighterRange); virtual;
    destructor Destroy; override;
    function Compare(Range: TSynCustomHighlighterRange): integer; virtual;
    function Add(ABlockType: Pointer = nil; IncreaseLevel: Boolean = True):
        TSynCustomCodeFoldBlock;
    procedure Pop(DecreaseLevel: Boolean = True);
    procedure Clear; virtual;
    procedure Assign(Src: TSynCustomHighlighterRange); virtual;
    procedure WriteDebugReport;
    property FoldRoot: TSynCustomCodeFoldBlock read FTop write FTop;
  public
    property RangeType: Pointer read FRangeType write FRangeType;
    property CodeFoldStackSize: integer read FCodeFoldStackSize;
    property Top: TSynCustomCodeFoldBlock read FTop;
  end;
  TSynCustomHighlighterRangeClass = class of TSynCustomHighlighterRange;
  {$ENDIF}

const
  SYN_ATTR_COMMENT           =   0;
  SYN_ATTR_IDENTIFIER        =   1;
  SYN_ATTR_KEYWORD           =   2;
  SYN_ATTR_STRING            =   3;
  SYN_ATTR_WHITESPACE        =   4;
  SYN_ATTR_SYMBOL            =   5;                                             //mh 2001-09-13

type
  {$IFDEF SYN_LAZARUS}
  TSynCustomHighlighterRanges = class;
  {$ENDIF}

  { TSynCustomHighlighter }

  TSynCustomHighlighter = class(TComponent)
  private
    fAttributes: TStringList;
    fAttrChangeHooks: TSynNotifyEventChain;
    {$IFDEF SYN_LAZARUS}
    FCodeFoldRange: TSynCustomHighlighterRange;
    FCapabilities: TSynHighlighterCapabilities;
    protected
    FMinimumCodeFoldBlockLevel: integer;
    private
      FRootCodeFoldBlock: TSynCustomCodeFoldBlock;
    {$ENDIF}
    fUpdateCount: integer;                                                      //mh 2001-09-13
    fEnabled: Boolean;
    fWordBreakChars: TSynIdentChars;
    procedure SetEnabled(const Value: boolean);                                 //DDH 2001-10-23
  protected
    fDefaultFilter: string;
    fUpdateChange: boolean;                                                     //mh 2001-09-13
    {$IFDEF SYN_LAZARUS}
    fRanges: TSynCustomHighlighterRanges;
    {$ENDIF}
    function GetLastLineCodeFoldLevelFix: integer; virtual;
    procedure AddAttribute(AAttrib: TSynHighlighterAttributes);
    procedure FreeHighlighterAttributes;                                        //mh 2001-09-13
    function GetAttribCount: integer; virtual;
    function GetAttribute(idx: integer): TSynHighlighterAttributes; virtual;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      virtual; abstract;
    function GetDefaultFilter: string; virtual;
    function GetIdentChars: TSynIdentChars; virtual;
    procedure SetWordBreakChars(AChars: TSynIdentChars); virtual;
    function GetSampleSource: string; virtual;
    function IsFilterStored: boolean; virtual;
    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    procedure SetDefaultFilter(Value: string); virtual;
    procedure SetSampleSource(Value: string); virtual;
    {$IFDEF SYN_LAZARUS}
    // code fold - only valid if hcCodeFolding in Capabilities
    property CodeFoldRange: TSynCustomHighlighterRange read FCodeFoldRange;
    function GetRangeClass: TSynCustomHighlighterRangeClass; virtual;
    function TopCodeFoldBlockType: Pointer;
    function StartCodeFoldBlock(ABlockType: Pointer;
              IncreaseLevel: Boolean = true): TSynCustomCodeFoldBlock; virtual;
    procedure EndCodeFoldBlock(DecreaseLevel: Boolean = True); virtual;
    {$ENDIF}
    procedure CreateRootCodeFoldBlock;
    property RootCodeFoldBlock: TSynCustomCodeFoldBlock read FRootCodeFoldBlock
      write FRootCodeFoldBlock;
  public
    procedure DefHighlightChange(Sender: TObject);
{$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetCapabilities: TSynHighlighterCapabilities; virtual;
{$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF SYN_LAZARUS}
    procedure AddSpecialAttribute(const AttribName: string);
    {$ENDIF}
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetEol: Boolean; virtual; abstract;
    function GetRange: Pointer; virtual;
    function GetToken: String; virtual; abstract;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(var TokenStart: PChar; var TokenLength: integer); virtual; abstract;
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; virtual; abstract;
    function GetTokenKind: integer; virtual; abstract;
    function GetTokenPos: Integer; virtual; abstract;
    function IsKeyword(const AKeyword: string): boolean; virtual;               // DJLP 2000-08-09
    procedure Next; virtual; abstract;
    procedure NextToEol;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String;
                      LineNumber:Integer // 0 based
                      ); virtual; {$IFNDEF SYN_LAZARUS}abstract;{$ENDIF}
    procedure SetRange(Value: Pointer); virtual;
    procedure ResetRange; virtual;
    function UseUserSettings(settingIndex: integer): boolean; virtual;
    procedure EnumUserSettings(Settings: TStrings); virtual;
    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; virtual;
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; virtual;
    function LoadFromFile(AFileName: String): boolean;                          //DDH 10/16/01
    function SaveToFile(AFileName: String): boolean;                            //DDH 10/16/01
    procedure HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    procedure UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    Function GetWordTriplet(LogicalCaret: TPoint; Lines: TSynEditStrings;
         out Y1, XB1, XE1, Y2, XB2, XE2, Y3, XB3, XE3: Integer): Boolean; virtual;
    property IdentChars: TSynIdentChars read GetIdentChars;
    property WordBreakChars: TSynIdentChars read fWordBreakChars write SetWordBreakChars;
    property LanguageName: string read GetLanguageName;

    {$IFDEF SYN_LAZARUS}
    property MinimumCodeFoldBlockLevel: integer read FMinimumCodeFoldBlockLevel;
    function CurrentCodeFoldBlockLevel: integer;
    property LastLineCodeFoldLevelFix: integer read GetLastLineCodeFoldLevelFix;
    {$ENDIF}
  public
    property AttrCount: integer read GetAttribCount;
    property Attribute[idx: integer]: TSynHighlighterAttributes
      read GetAttribute;
    property Capabilities: TSynHighlighterCapabilities
       read {$IFDEF SYN_LAZARUS}FCapabilities{$ELSE}GetCapabilities{$ENDIF};
    property SampleSource: string read GetSampleSource write SetSampleSource;
    property CommentAttribute: TSynHighlighterAttributes
      index SYN_ATTR_COMMENT read GetDefaultAttribute;
    property IdentifierAttribute: TSynHighlighterAttributes
      index SYN_ATTR_IDENTIFIER read GetDefaultAttribute;
    property KeywordAttribute: TSynHighlighterAttributes
      index SYN_ATTR_KEYWORD read GetDefaultAttribute;
    property StringAttribute: TSynHighlighterAttributes
      index SYN_ATTR_STRING read GetDefaultAttribute;
    property SymbolAttribute: TSynHighlighterAttributes                         //mh 2001-09-13
      index SYN_ATTR_SYMBOL read GetDefaultAttribute;
    property WhitespaceAttribute: TSynHighlighterAttributes
      index SYN_ATTR_WHITESPACE read GetDefaultAttribute;
  published
    property DefaultFilter: string read GetDefaultFilter write SetDefaultFilter
      stored IsFilterStored;
    property Enabled: boolean read fEnabled write SetEnabled default TRUE;      //DDH 2001-10-23
  end;

  TSynCustomHighlighterClass = class of TSynCustomHighlighter;

{$IFDEF SYN_LAZARUS}
  { TSynCustomHighlighterRanges }

  TSynCustomHighlighterRanges = class
  private
    FAllocatedCount: integer;
    FHighlighterClass: TSynCustomHighlighterClass;
    FItems: TAvgLvlTree;
  public
    constructor Create(TheHighlighterClass: TSynCustomHighlighterClass);
    destructor Destroy; override;
    function GetEqual(Range: TSynCustomHighlighterRange
                      ): TSynCustomHighlighterRange;
    procedure Allocate;
    procedure Release;
    property HighlighterClass: TSynCustomHighlighterClass read FHighlighterClass;
    property AllocatedCount: integer read FAllocatedCount;
  end;
{$ENDIF}

{$IFNDEF SYN_CPPB_1}
  TSynHighlighterList = class(TList)
  private
    hlList: TList;
    function GetItem(idx: integer): TSynCustomHighlighterClass;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function FindByName(name: string): integer;
    function FindByClass(comp: TComponent): integer;
    property Items[idx: integer]: TSynCustomHighlighterClass
      read GetItem; default;
  end;

  procedure RegisterPlaceableHighlighter(highlighter:
    TSynCustomHighlighterClass);
  function GetPlaceableHighlighters: TSynHighlighterList;
{$ENDIF}

{$IFDEF SYN_LAZARUS}
function CompareSynHighlighterRanges(Data1, Data2: Pointer): integer;
function AllocateHighlighterRanges(
     HighlighterClass: TSynCustomHighlighterClass): TSynCustomHighlighterRanges;
{$ENDIF}


implementation

{$IFDEF _Gp_MustEnhanceRegistry}
  function IsRelative(const Value: string): Boolean;
  begin
    Result := not ((Value <> '') and (Value[1] = '\'));
  end;

  function TBetterRegistry.OpenKeyReadOnly(const Key: string): Boolean;
  var
    TempKey: HKey;
    S: string;
    Relative: Boolean;
  begin
    S := Key;
    Relative := IsRelative(S);

    if not Relative then Delete(S, 1, 1);
    TempKey := 0;
    Result := RegOpenKeyEx(GetBaseKey(Relative), PChar(S), 0,
        KEY_READ, TempKey) = ERROR_SUCCESS;
    if Result then
    begin
      if (CurrentKey <> 0) and Relative then S := CurrentPath + '\' + S;
      ChangeKey(TempKey, S);
    end;
  end; { TBetterRegistry.OpenKeyReadOnly }
{$ENDIF _Gp_MustEnhanceRegistry}

{$IFNDEF SYN_CPPB_1}
{ THighlighterList }

function TSynHighlighterList.Count: integer;
begin
  Result := hlList.Count;
end;

constructor TSynHighlighterList.Create;
begin
  inherited Create;
  hlList := TList.Create;
end;

destructor TSynHighlighterList.Destroy;
begin
  hlList.Free;
  inherited;
end;

function TSynHighlighterList.FindByClass(comp: TComponent): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do begin
    if comp is Items[i] then begin
      Result := i;
      Exit;
    end;
  end; //for
end;

function TSynHighlighterList.FindByName(name: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do begin
    if Items[i].GetLanguageName = name then begin
      Result := i;
      Exit;
    end;
  end; //for
end;

function TSynHighlighterList.GetItem(idx: integer): TSynCustomHighlighterClass;
begin
  Result := TSynCustomHighlighterClass(hlList[idx]);
end;

var
  G_PlaceableHighlighters: TSynHighlighterList;

function GetPlaceableHighlighters: TSynHighlighterList;
begin
  Result := G_PlaceableHighlighters;
end;

procedure RegisterPlaceableHighlighter(highlighter: TSynCustomHighlighterClass);
begin
  if G_PlaceableHighlighters.hlList.IndexOf(highlighter) < 0 then
    G_PlaceableHighlighters.hlList.Add(highlighter);
end;
{$ENDIF}

{$IFDEF SYN_LAZARUS}
function CompareSynHighlighterRanges(Data1, Data2: Pointer): integer;
var
  Range1: TSynCustomHighlighterRange;
  Range2: TSynCustomHighlighterRange;
begin
  Range1:=TSynCustomHighlighterRange(Data1);
  Range2:=TSynCustomHighlighterRange(Data2);
  Result:=Range1.Compare(Range2);
end;

var
  HighlighterRanges: TFPList = nil;

function IndexOfHighlighterRanges(
  HighlighterClass: TSynCustomHighlighterClass): integer;
begin
  if HighlighterRanges=nil then
    Result:=-1
  else begin
    Result:=HighlighterRanges.Count-1;
    while (Result>=0)
    and (TSynCustomHighlighterRanges(HighlighterRanges[Result]).HighlighterClass
      <>HighlighterClass)
    do
      dec(Result);
  end;
end;

function AllocateHighlighterRanges(
  HighlighterClass: TSynCustomHighlighterClass): TSynCustomHighlighterRanges;
var
  i: LongInt;
begin
  if HighlighterRanges=nil then HighlighterRanges:=TFPList.Create;
  i:=IndexOfHighlighterRanges(HighlighterClass);
  if i>=0 then begin
    Result:=TSynCustomHighlighterRanges(HighlighterRanges[i]);
    Result.Allocate;
  end else begin
    Result:=TSynCustomHighlighterRanges.Create(HighlighterClass);
    HighlighterRanges.Add(Result);
  end;
end;
{$ENDIF}

{ TSynHighlighterAttributes }

procedure TSynHighlighterAttributes.Assign(Source: TPersistent);
var
  src: TSynHighlighterAttributes;
  bChanged: boolean;
begin
  if Source is TSynHighlighterAttributes then begin
    bChanged := FALSE;
    src := Source as TSynHighlighterAttributes;
    fName := src.fName;
    if fBackground <> src.fBackground then begin
      fBackground := src.fBackground;
      bChanged := TRUE;
    end;
    if fForeground <> src.fForeground then begin
      fForeground := src.fForeground;
      bChanged := TRUE;
    end;
    if FFrameColor <> src.FFrameColor then begin
      FFrameColor := src.FFrameColor;
      bChanged := True;
    end;
    if fStyle <> src.fStyle then begin
      fStyle := src.fStyle;
      bChanged := TRUE;
    end;
    {$IFDEF SYN_LAZARUS}
    if fStyleMask <> src.fStyleMask then begin
      fStyleMask := src.fStyleMask;
      bChanged := TRUE;
    end;
    {$ENDIF}
    if bChanged then
      Changed;
  end else
    inherited Assign(Source);
end;

procedure TSynHighlighterAttributes.Changed;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

constructor TSynHighlighterAttributes.Create(attribName: string);
begin
  inherited Create;
  Background := clNone;
  Foreground := clNone;
  FFrameColor := clNone;
  fName := attribName;
end;

function TSynHighlighterAttributes.GetBackgroundColorStored: boolean;
begin
  Result := fBackground <> fBackgroundDefault;
end;

function TSynHighlighterAttributes.GetFontStyleMaskStored : boolean;
begin
  Result := fStyleMask <> fStyleMaskDefault;
end;

function TSynHighlighterAttributes.GetForegroundColorStored: boolean;
begin
  Result := fForeground <> fForegroundDefault;
end;

function TSynHighlighterAttributes.GetFrameColorStored: boolean;
begin
  Result := FFrameColor <> FFrameColorDefault;
end;

function TSynHighlighterAttributes.GetFontStyleStored: boolean;
begin
  Result := fStyle <> fStyleDefault;
end;

procedure TSynHighlighterAttributes.InternalSaveDefaultValues;
begin
  fForegroundDefault := fForeground;
  fBackgroundDefault := fBackground;
  FFrameColorDefault := FFrameColor;
  fStyleDefault := fStyle;
  {$IFDEF SYN_LAZARUS}
  fStyleMaskDefault := fStyleMask;
  {$ENDIF}
end;

function TSynHighlighterAttributes.LoadFromBorlandRegistry(rootKey: HKEY;
  attrKey, attrName: string; oldStyle: boolean): boolean;
  // How the highlighting information is stored:
  // Delphi 1.0:
  //   I don't know and I don't care.
  // Delphi 2.0 & 3.0:
  //   In the registry branch HKCU\Software\Borland\Delphi\x.0\Highlight
  //   where x=2 or x=3.
  //   Each entry is one string value, encoded as
  //     <foreground RGB>,<background RGB>,<font style>,<default fg>,<default Background>,<fg index>,<Background index>
  //   Example:
  //     0,16777215,BI,0,1,0,15
  //     foreground color (RGB): 0
  //     background color (RGB): 16777215 ($FFFFFF)
  //     font style: BI (bold italic), possible flags: B(old), I(talic), U(nderline)
  //     default foreground: no, specified color will be used (black (0) is used when this flag is 1)
  //     default background: yes, white ($FFFFFF, 15) will be used for background
  //     foreground index: 0 (foreground index (Pal16), corresponds to foreground RGB color)
  //     background index: 15 (background index (Pal16), corresponds to background RGB color)
  // Delphi 4.0 & 5.0:
  //   In the registry branch HKCU\Software\Borland\Delphi\4.0\Editor\Highlight.
  //   Each entry is subkey containing several values:
  //     Foreground Color: foreground index (Pal16), 0..15 (dword)
  //     Background Color: background index (Pal16), 0..15 (dword)
  //     Bold: fsBold yes/no, 0/True (string)
  //     Italic: fsItalic yes/no, 0/True (string)
  //     Underline: fsUnderline yes/no, 0/True (string)
  //     Default Foreground: use default foreground (clBlack) yes/no, False/-1 (string)
  //     Default Background: use default backround (clWhite) yes/no, False/-1 (string)
{$IFNDEF SYN_LAZARUS}
const
  Pal16: array [0..15] of TColor = (clBlack, clMaroon, clGreen, clOlive,
          clNavy, clPurple, clTeal, clLtGray, clDkGray, clRed, clLime,
          clYellow, clBlue, clFuchsia, clAqua, clWhite);
{$ENDIF}

  function LoadOldStyle(rootKey: HKEY; attrKey, attrName: string): boolean;
  var
    {$IFNDEF SYN_LAZARUS}
    descript : string;
    fgColRGB : string;
    bgColRGB : string;
    fontStyle: string;
    fgDefault: string;
    bgDefault: string;
    fgIndex16: string;
    bgIndex16: string;
    {$ENDIF}
    reg      : TBetterRegistry;

    function Get(var name: string): string;
    var
      p: integer;
    begin
      p := Pos(',',name);
      if p = 0 then p := Length(name)+1;
      Result := Copy(name,1,p-1);
      name := Copy(name,p+1,Length(name)-p);
    end; { Get }

  begin { LoadOldStyle }
    Result := false;
    try
      reg := TBetterRegistry.Create;
      reg.RootKey := rootKey;
      try
        with reg do begin
          {$IFNDEF SYN_LAZARUS}
          // ToDo Registry
          if OpenKeyReadOnly(attrKey) then begin
            try
              if ValueExists(attrName) then begin
                descript := ReadString(attrName);
                fgColRGB  := Get(descript);
                bgColRGB  := Get(descript);
                fontStyle := Get(descript);
                fgDefault := Get(descript);
                bgDefault := Get(descript);
                fgIndex16 := Get(descript);
                bgIndex16 := Get(descript);
                if bgDefault = '1'
                  then Background := clWindow
                  else Background := Pal16[StrToInt(bgIndex16)];
                if fgDefault = '1'
                  then Foreground := clWindowText
                  else Foreground := Pal16[StrToInt(fgIndex16)];
                Style := [];
                if Pos('B',fontStyle) > 0 then Style := Style + [fsBold];
                if Pos('I',fontStyle) > 0 then Style := Style + [fsItalic];
                if Pos('U',fontStyle) > 0 then Style := Style + [fsUnderline];
                Result := true;
              end;
            finally CloseKey; end;
          end; // if
          {$ENDIF}
        end; // with
      finally reg.Free; end;
    except end;
  end; { LoadOldStyle }

  function LoadNewStyle(rootKey: HKEY; attrKey, attrName: string): boolean;
  var
    {$IFNDEF SYN_LAZARUS}
    fgIndex16    : DWORD;
    bgIndex16    : DWORD;
    fontBold     : string;
    fontItalic   : string;
    fontUnderline: string;
    fgDefault    : string;
    bgDefault    : string;
    {$ENDIF}
    reg          : TBetterRegistry;

    function IsTrue(value: string): boolean;
    begin
      Result := not ((UpperCase(value) = 'FALSE') or (value = '0'));
    end; { IsTrue }

  begin
    Result := false;
    try
      reg := TBetterRegistry.Create;
      reg.RootKey := rootKey;
      try
        with reg do begin
          {$IFNDEF SYN_LAZARUS}
          // ToDo Registry
          if OpenKeyReadOnly(attrKey+'\'+attrName) then begin
            try
              if ValueExists('Foreground Color')
                then fgIndex16 := ReadInteger('Foreground Color')
                else Exit;
              if ValueExists('Background Color')
                then bgIndex16 := ReadInteger('Background Color')
                else Exit;
              if ValueExists('Bold')
                then fontBold := ReadString('Bold')
                else Exit;
              if ValueExists('Italic')
                then fontItalic := ReadString('Italic')
                else Exit;
              if ValueExists('Underline')
                then fontUnderline := ReadString('Underline')
                else Exit;
              if ValueExists('Default Foreground')
                then fgDefault := ReadString('Default Foreground')
                else Exit;
              if ValueExists('Default Background')
                then bgDefault := ReadString('Default Background')
                else Exit;
              if IsTrue(bgDefault)
                then Background := clWindow
                else Background := Pal16[bgIndex16];
              if IsTrue(fgDefault)
                then Foreground := clWindowText
                else Foreground := Pal16[fgIndex16];
              Style := [];
              if IsTrue(fontBold) then Style := Style + [fsBold];
              if IsTrue(fontItalic) then Style := Style + [fsItalic];
              if IsTrue(fontUnderline) then Style := Style + [fsUnderline];
              Result := true;
            finally CloseKey; end;
          end; // if
          {$ENDIF}
        end; // with
      finally reg.Free; end;
    except end;
  end; { LoadNewStyle }

begin
  if oldStyle then Result := LoadOldStyle(rootKey, attrKey, attrName)
              else Result := LoadNewStyle(rootKey, attrKey, attrName);
end; { TSynHighlighterAttributes.LoadFromBorlandRegistry }

procedure TSynHighlighterAttributes.SetBackground(Value: TColor);
begin
  if fBackGround <> Value then begin
    fBackGround := Value;
    Changed;
  end;
end;

procedure TSynHighlighterAttributes.SetForeground(Value: TColor);
begin
  if fForeGround <> Value then begin
    fForeGround := Value;
    Changed;
  end;
end;

procedure TSynHighlighterAttributes.SetFrameColor(const AValue: TColor);
begin
  if FFrameColor <> AValue then
  begin
    FFrameColor := AValue;
    Changed;
  end;
end;

procedure TSynHighlighterAttributes.SetStyle(Value: TFontStyles);
begin
  if fStyle <> Value then begin
    fStyle := Value;
    Changed;
  end;
end;

function TSynHighlighterAttributes.LoadFromRegistry(Reg: TBetterRegistry): boolean;
{$IFNDEF SYN_LAZARUS}
var
  key: string;
{$ENDIF}
begin
  {$IFNDEF SYN_LAZARUS}
  // ToDo  Registry
  key := Reg.CurrentPath;
  if Reg.OpenKeyReadOnly(Name) then begin
    if Reg.ValueExists('Background') then
      Background := Reg.ReadInteger('Background');
    if Reg.ValueExists('Foreground') then
      Foreground := Reg.ReadInteger('Foreground');
    if Reg.ValueExists('Style') then
      IntegerStyle := Reg.ReadInteger('Style');
    reg.OpenKeyReadOnly('\' + key);
    Result := true;
  end else
    Result := false;
  {$ELSE}
  Result:=false;
  {$ENDIF}
end;

function TSynHighlighterAttributes.SaveToRegistry(Reg: TBetterRegistry): boolean;
var
  key: string;
begin
  key := Reg.CurrentPath;
  if Reg.OpenKey(Name,true) then begin
    Reg.WriteInteger('Background', Background);
    Reg.WriteInteger('Foreground', Foreground);
    Reg.WriteInteger('Style', IntegerStyle);
    {$IFDEF SYN_LAZARUS}
    Reg.WriteInteger('StyleMask', IntegerStyleMask);
    {$ENDIF}
    reg.OpenKey('\' + key, false);
    Result := true;
  end else
    Result := false;
end;

function TSynHighlighterAttributes.LoadFromFile(Ini : TIniFile): boolean;       //DDH 10/16/01
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    Ini.ReadSection(Name, S);
    if S.Count > 0 then
    begin
      if S.IndexOf('Background') <> -1 then
        Background := Ini.ReadInteger(Name, 'Background', clWindow);
      if S.IndexOf('Foreground') <> -1 then
        Foreground := Ini.ReadInteger(Name, 'Foreground', clWindowText);
      if S.IndexOf('Style') <> -1 then
        IntegerStyle := Ini.ReadInteger(Name, 'Style', 0);
      {$IFDEF SYN_LAZARUS}
      if S.IndexOf('StyleMask') <> -1 then
        IntegerStyleMask := Ini.ReadInteger(Name, 'StyleMask', 0);
      {$ENDIF}
      Result := true;
    end else Result := false;
  finally
    S.Free;
  end;
end;

function TSynHighlighterAttributes.SaveToFile(Ini : TIniFile): boolean;         //DDH 10/16/01
begin
  Ini.WriteInteger(Name, 'Background', Background);
  Ini.WriteInteger(Name, 'Foreground', Foreground);
  Ini.WriteInteger(Name, 'Style', IntegerStyle);
  {$IFDEF SYN_LAZARUS}
  Ini.WriteInteger(Name, 'StyleMask', IntegerStyleMask);
  {$ENDIF}
  Result := true;
end;

function TSynHighlighterAttributes.GetStyleFromInt: integer;
begin
  if fsBold in Style then Result:= 1 else Result:= 0;
  if fsItalic in Style then Result:= Result + 2;
  if fsUnderline in Style then Result:= Result + 4;
  if fsStrikeout in Style then Result:= Result + 8;
end;

procedure TSynHighlighterAttributes.SetStyleFromInt(const Value: integer);
begin
  if Value and $1 = 0 then  Style:= [] else Style:= [fsBold];
  if Value and $2 <> 0 then Style:= Style + [fsItalic];
  if Value and $4 <> 0 then Style:= Style + [fsUnderline];
  if Value and $8 <> 0 then Style:= Style + [fsStrikeout];
end;

{$IFDEF SYN_LAZARUS}
procedure TSynHighlighterAttributes.SetStyleMask(const AValue : TFontStyles);
begin
  if fStyleMask <> AValue then begin
    fStyleMask := AValue;
    Changed;
  end;
end;

function TSynHighlighterAttributes.GetStyleMaskFromInt : integer;
begin
  if fsBold in StyleMask then Result:= 1 else Result:= 0;
  if fsItalic in StyleMask then Result:= Result + 2;
  if fsUnderline in StyleMask then Result:= Result + 4;
  if fsStrikeout in StyleMask then Result:= Result + 8;
end;

procedure TSynHighlighterAttributes.SetStyleMaskFromInt(const Value : integer);
begin
  if Value and $1 = 0 then  StyleMask:= [] else StyleMask:= [fsBold];
  if Value and $2 <> 0 then StyleMask:= StyleMask + [fsItalic];
  if Value and $4 <> 0 then StyleMask:= StyleMask + [fsUnderline];
  if Value and $8 <> 0 then StyleMask:= StyleMask + [fsStrikeout];
end;
{$ENDIF}

{ TSynCustomHighlighter }

constructor TSynCustomHighlighter.Create(AOwner: TComponent);
begin
  {$IFDEF SYN_LAZARUS}
  FCapabilities:=GetCapabilities;
  {$ENDIF}
  inherited Create(AOwner);
  fWordBreakChars := TSynWordBreakChars;
  fAttributes := TStringList.Create;
  fAttributes.Duplicates := dupError;
  fAttributes.Sorted := TRUE;
  fAttrChangeHooks := TSynNotifyEventChain.CreateEx(Self);
  fDefaultFilter := '';
  {$IFDEF SYN_LAZARUS}
  if hcCodeFolding in Capabilities then begin
    CreateRootCodeFoldBlock;
    FCodeFoldRange:=GetRangeClass.Create(nil);
    FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
    fRanges:=AllocateHighlighterRanges(TSynCustomHighlighterClass(ClassType));
  end;
  {$ENDIF}
end;

destructor TSynCustomHighlighter.Destroy;
begin
  {$IFDEF SYN_LAZARUS}
  if hcCodeFolding in Capabilities then begin
    FreeAndNil(FCodeFoldRange);
    fRanges.Release;
    FreeAndNil(FRootCodeFoldBlock);
  end;
  {$ENDIF}
  FreeHighlighterAttributes;
  fAttributes.Free;
  fAttrChangeHooks.Free;
  inherited Destroy;
end;

procedure TSynCustomHighlighter.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TSynCustomHighlighter.EndUpdate;
begin
  if fUpdateCount > 0 then begin
    Dec(fUpdateCount);
    if (fUpdateCount = 0) and fUpdateChange then begin
      fUpdateChange := FALSE;
      DefHighlightChange(Self);
    end;
  end;
end;

procedure TSynCustomHighlighter.FreeHighlighterAttributes;
var
  i: integer;
begin
  if fAttributes <> nil then begin
    for i := fAttributes.Count - 1 downto 0 do
      TSynHighlighterAttributes(fAttributes.Objects[i]).Free;
    fAttributes.Clear;
  end;
end;

procedure TSynCustomHighlighter.Assign(Source: TPersistent);
var
  Src: TSynCustomHighlighter;
  i, j: integer;
  AttriName: string;
  SrcAttri: TSynHighlighterAttributes;
begin
  if (Source <> nil) and (Source is TSynCustomHighlighter) then begin
    Src := TSynCustomHighlighter(Source);
    for i := 0 to AttrCount - 1 do begin
      // assign first attribute with the same name
      AttriName := Attribute[i].Name;
      for j := 0 to Src.AttrCount - 1 do begin
        SrcAttri := Src.Attribute[j];
        if AttriName = SrcAttri.Name then begin
          Attribute[i].Assign(SrcAttri);
          continue;
        end;
      end;
    end;
    // assign the sample source text only if same or descendant class
    if Src is ClassType then
      SampleSource := Src.SampleSource;
    fWordBreakChars := Src.WordBreakChars;
    DefaultFilter := Src.DefaultFilter;
    Enabled := Src.Enabled;
  end else
    inherited Assign(Source);
end;

procedure TSynCustomHighlighter.EnumUserSettings(Settings: TStrings);
begin
  Settings.Clear;
end;

function TSynCustomHighlighter.UseUserSettings(settingIndex: integer): boolean;
begin
  Result := false;
end;

function TSynCustomHighlighter.LoadFromRegistry(RootKey: HKEY;
  Key: string): boolean;
var
  r: TBetterRegistry;
  {$IFNDEF FPC}
  i: integer;
  {$ENDIF}
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    {$IFNDEF FPC}
    if r.OpenKeyReadOnly(Key) then begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Result and Attribute[i].LoadFromRegistry(r);
    end
    else
    {$ENDIF}
      Result := false;

  finally r.Free; end;
end;

function TSynCustomHighlighter.SaveToRegistry(RootKey: HKEY;
  Key: string): boolean;
var
  r: TBetterRegistry;
  i: integer;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,true) then begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Result and Attribute[i].SaveToRegistry(r);
    end
    else Result := false;
  finally r.Free; end;
end;

function TSynCustomHighlighter.LoadFromFile(AFileName : String): boolean;       //DDH 10/16/01
VAR AIni : TIniFile;
    i : Integer;
begin
  AIni := TIniFile.Create(UTF8ToSys(AFileName));
  try
    with AIni do
    begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Result and Attribute[i].LoadFromFile(AIni);
    end;
  finally
    AIni.Free;
  end;
end;

function TSynCustomHighlighter.SaveToFile(AFileName : String): boolean;         //DDH 10/16/01
var AIni : TIniFile;
    i: integer;
begin
  AIni := TIniFile.Create(UTF8ToSys(AFileName));
  try
    with AIni do
    begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Result and Attribute[i].SaveToFile(AIni);
    end;
  finally
    AIni.Free;
  end;
end;

procedure TSynCustomHighlighter.AddAttribute(AAttrib: TSynHighlighterAttributes);
begin
  fAttributes.AddObject(AAttrib.Name, AAttrib);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynCustomHighlighter.AddSpecialAttribute(const AttribName: string);
begin
  AddAttribute(TSynHighlighterAttributes.Create(AttribName));
end;
{$ENDIF}

procedure TSynCustomHighlighter.DefHighlightChange(Sender: TObject);
begin
  if fUpdateCount > 0 then
    fUpdateChange := TRUE
  else
    fAttrChangeHooks.Fire;
end;

function TSynCustomHighlighter.GetAttribCount: integer;
begin
  Result := fAttributes.Count;
end;

function TSynCustomHighlighter.GetAttribute(idx: integer):
  TSynHighlighterAttributes;
begin
  Result := nil;
  if (idx >= 0) and (idx < fAttributes.Count) then
    Result := TSynHighlighterAttributes(fAttributes.Objects[idx]);
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynCustomHighlighter.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := [hcRegistry]; //registry save/load supported by default
end;

function TSynCustomHighlighter.GetDefaultFilter: string;
begin
  Result := fDefaultFilter;
end;

function TSynCustomHighlighter.GetIdentChars: TSynIdentChars;
begin
  Result := [#33..#255];
end;

procedure TSynCustomHighlighter.SetWordBreakChars(AChars: TSynIdentChars);
begin
  fWordBreakChars := AChars;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynCustomHighlighter.GetLanguageName: string;
begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  raise Exception.CreateFmt('%s.GetLanguageName not implemented', [ClassName]);
{$ENDIF}
  Result := '<Unknown>';
end;

function TSynCustomHighlighter.GetRange: pointer;
begin
  Result := nil;
  {$IFDEF SYN_LAZARUS}
  if (hcCodeFolding in Capabilities) then begin
    // FCodeFoldRange is the working range and changed steadily
    // => return a fixed copy of the current CodeFoldRange instance,
    //    that can be stored by other classes (e.g. TSynEdit)
    Result:=fRanges.GetEqual(FCodeFoldRange);
  end;
  {$ENDIF}
end;

function TSynCustomHighlighter.GetSampleSource: string;
begin
  Result := '';
end;

procedure TSynCustomHighlighter.HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Add(ANotifyEvent);
end;

function TSynCustomHighlighter.IsFilterStored: boolean;
begin
  Result := TRUE;
end;

{begin}                                                                         // DJLP 2000-08-09
function TSynCustomHighlighter.IsKeyword(const AKeyword: string): boolean;
begin
  Result := FALSE;
end;
{end}                                                                           // DJLP 2000-08-09

procedure TSynCustomHighlighter.NextToEol;
begin
  while not GetEol do Next;
end;

procedure TSynCustomHighlighter.ResetRange;
begin
  {$IFDEF SYN_LAZARUS}
  if (hcCodeFolding in Capabilities) then begin
    FCodeFoldRange.Clear;
    FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
  end;
  {$ENDIF}
end;

procedure TSynCustomHighlighter.SetAttributesOnChange(AEvent: TNotifyEvent);
var
  i: integer;
  Attri: TSynHighlighterAttributes;
begin
  for i := fAttributes.Count - 1 downto 0 do begin
    Attri := TSynHighlighterAttributes(fAttributes.Objects[i]);
    if Attri <> nil then begin
      Attri.OnChange := AEvent;
      Attri.InternalSaveDefaultValues;
    end;
  end;
end;

procedure TSynCustomHighlighter.SetRange(Value: Pointer);
begin
  {$IFDEF SYN_LAZARUS}
  if (hcCodeFolding in Capabilities) then begin
    FCodeFoldRange.Assign(TSynCustomHighlighterRange(Value));
    // in case we asigned a null range
    if not assigned(FCodeFoldRange.FoldRoot) then
      FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
    FMinimumCodeFoldBlockLevel:=FCodeFoldRange.CodeFoldStackSize;
  end;
  {$ENDIF}
end;

procedure TSynCustomHighlighter.SetDefaultFilter(Value: string);
begin
  fDefaultFilter := Value;
end;

procedure TSynCustomHighlighter.SetSampleSource(Value: string);
begin
end;

procedure TSynCustomHighlighter.UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Remove(ANotifyEvent);
end;

function TSynCustomHighlighter.GetWordTriplet(LogicalCaret: TPoint;
  Lines: TSynEditStrings; out Y1, XB1, XE1, Y2, XB2, XE2, Y3, XB3, XE3: Integer
  ): Boolean;
begin
  Result := False;
end;

procedure TSynCustomHighlighter.SetEnabled(const Value: boolean);
begin
  if fEnabled <> Value then
  begin
    fEnabled := Value;
    //we need to notify any editor that we are attached to to repaint,
    //but a highlighter doesn't know what editor it is attached to.
    //Until this is resolved, you will have to manually call repaint
    //on the editor in question.
  end;
end;

function TSynCustomHighlighter.GetLastLineCodeFoldLevelFix: integer;
begin
  Result := 0;
end;

{$IFDEF SYN_LAZARUS}
procedure TSynCustomHighlighter.SetLine(const NewValue: String;
  LineNumber: Integer);
begin
  if (hcCodeFolding in Capabilities) then begin
    FMinimumCodeFoldBlockLevel:=CodeFoldRange.CodeFoldStackSize;
  end;
end;

function TSynCustomHighlighter.CurrentCodeFoldBlockLevel: integer;
begin
  if CodeFoldRange<>nil then
    Result:=CodeFoldRange.CodeFoldStackSize
  else
    Result:=0;
end;

function TSynCustomHighlighter.GetRangeClass: TSynCustomHighlighterRangeClass;
begin
  Result:=TSynCustomHighlighterRange;
end;

function TSynCustomHighlighter.TopCodeFoldBlockType: Pointer;
begin
  if (CodeFoldRange<>nil) and (CodeFoldRange.CodeFoldStackSize>0) then
    Result:=CodeFoldRange.Top.BlockType
  else
    Result:=nil;
end;

function TSynCustomHighlighter.StartCodeFoldBlock(ABlockType: Pointer;
  IncreaseLevel: Boolean = True): TSynCustomCodeFoldBlock;
begin
  Result:=CodeFoldRange.Add(ABlockType, IncreaseLevel);
end;

procedure TSynCustomHighlighter.EndCodeFoldBlock(DecreaseLevel: Boolean = True);
begin
  CodeFoldRange.Pop(DecreaseLevel);
  if FMinimumCodeFoldBlockLevel>CodeFoldRange.CodeFoldStackSize then
    FMinimumCodeFoldBlockLevel:=CodeFoldRange.CodeFoldStackSize;
end;

procedure TSynCustomHighlighter.CreateRootCodeFoldBlock;
begin
  FRootCodeFoldBlock := TSynCustomCodeFoldBlock.Create;
end;

{$ENDIF}

{$IFNDEF SYN_CPPB_1}

{$IFDEF SYN_LAZARUS}
{ TSynCustomCodeFoldBlock }

function TSynCustomCodeFoldBlock.GetChild(ABlockType: Pointer): TSynCustomCodeFoldBlock;
begin
  if assigned(FChildren) then
    Result := FChildren.GetOrCreateSibling(ABlockType)
  else begin
    Result := TSynCustomCodeFoldBlock(self.ClassType.Create);
    Result.FBlockType := ABlockType;
    Result.FParent := self;
    FChildren := Result;
  end;
end;

var
  CreateSiblingBalanceList: Array of TSynCustomCodeFoldBlock;

function TSynCustomCodeFoldBlock.GetOrCreateSibling(ABlockType: Pointer): TSynCustomCodeFoldBlock;
  procedure BalanceNode(TheNode: TSynCustomCodeFoldBlock);
  var
    i, l: Integer;
    t: Pointer;
    N, P, C: TSynCustomCodeFoldBlock;
  begin
    l := length(CreateSiblingBalanceList);
    i := 0;
    t := TheNode.FBlockType;
    N := self;
    while N.FBlockType <> t do begin
      if i >= l then begin
        inc(l, 20);
        SetLength(CreateSiblingBalanceList, l);
      end;
      CreateSiblingBalanceList[i] := N; // Record all parents
      inc(i);
      if t < N.FBlockType
      then N := N.FLeft
      else N := N.FRight;
    end;
    if i >= l then begin
      inc(l, 20);
      SetLength(CreateSiblingBalanceList, l);
    end;
    CreateSiblingBalanceList[i] := TheNode;
    while i >= 0 do begin
      if CreateSiblingBalanceList[i].FBalance = 0
        then exit;
      if (CreateSiblingBalanceList[i].FBalance = -1) or
         (CreateSiblingBalanceList[i].FBalance = 1) then begin
        if i = 0 then
          exit;
        dec(i);
        if CreateSiblingBalanceList[i+1] = CreateSiblingBalanceList[i].FLeft
        then dec(CreateSiblingBalanceList[i].FBalance)
        else inc(CreateSiblingBalanceList[i].FBalance);
        continue;
      end;
      // rotate
      P := CreateSiblingBalanceList[i];
      if P.FBalance = -2 then begin
        N := P.FLeft;
        if N.FBalance < 0 then begin
          (* ** single rotate ** *)
          (*  []\[]_     _C                []_      C_    _[]
                    N(-1)_     _[]    =>      []_    _P(0)
                          P(-2)                  N(0)           *)
          C := N.FRight;
          N.FRight := P;
          P.FLeft := C;
          N.FBalance := 0;
          P.FBalance := 0;
        end else begin
          (* ** double rotate ** *)
          (*          x1 x2
               []_     _C                  x1    x2
                  N(+1)_     _[]    =>    N _    _ P
                        P(-2)                 C           *)
          C := N.FRight;
          N.FRight := C.FLeft;
          P.FLeft  := C.FRight;
          C.FLeft  := N;
          C.FRight := P;
          // balance
          if (C.FBalance <= 0)
          then N.FBalance := 0
          else N.FBalance := -1;
          if (C.FBalance = -1)
          then P.FBalance := 1
          else P.FBalance := 0;
          C.FBalance := 0;
          N := C;
        end;
      end else begin // *******************
        N := P.FRight;
        if N.FBalance > 0 then begin
          (* ** single rotate ** *)
          C := N.FLeft;
          N.FLeft := P;
          P.FRight := C;
          N.FBalance := 0;
          P.FBalance := 0;
        end else begin
          (* ** double rotate ** *)
          C := N.FLeft;
          N.FLeft := C.FRight;
          P.FRight  := C.FLeft;
          C.FRight  := N;
          C.FLeft := P;
          // balance
          if (C.FBalance >= 0)
          then N.FBalance := 0
          else N.FBalance := +1;
          if (C.FBalance = +1)
          then P.FBalance := -1
          else P.FBalance := 0;
          C.FBalance := 0;
          N := C;
        end;
      end;
      // update parent
      dec(i);
      if i < 0 then begin
        if assigned(self.FParent) then
          self.FParent.FChildren := N
      end else
        if CreateSiblingBalanceList[i].FLeft = P
        then CreateSiblingBalanceList[i].FLeft := N
        else CreateSiblingBalanceList[i].FRight := N;
      break;
    end
  end;
var
  P: TSynCustomCodeFoldBlock;
begin
  Result := self;
  while (assigned(Result)) do begin
    if Result.FBlockType = ABlockType then
      exit;
    P := Result;
    if ABlockType < Result.FBlockType
    then Result := Result.FLeft
    else Result := Result.FRight;
  end;
  // Not Found
  Result := TSynCustomCodeFoldBlock(self.ClassType.Create);
  Result.FBlockType := ABlockType;
  Result.FParent := self.FParent;

  if ABlockType < P.FBlockType then begin
    P.FLeft := Result;
    dec(P.FBalance);
  end else begin
    P.FRight := Result;
    inc(P.FBalance);
  end;

  // Balance
  if P.FBalance <> 0 then
    BalanceNode(P);

end;

destructor TSynCustomCodeFoldBlock.Destroy;
begin
  FreeAndNil(FRight);
  FreeAndNil(FLeft);
  FreeAndNil(FChildren);
  inherited Destroy;
end;

function TSynCustomCodeFoldBlock.Compare(Block: TSynCustomCodeFoldBlock
  ): integer;
begin
  if BlockType>Block.BlockType then
    Result:=1
  else if BlockType<Block.BlockType then
    Result:=-1
  else
    Result:=0;
end;

procedure TSynCustomCodeFoldBlock.Assign(Src: TSynCustomCodeFoldBlock);
begin
  FBlockType:=Src.FBlockType;
end;

{ TSynCustomHighlighterRange }

constructor TSynCustomHighlighterRange.Create(
  Template: TSynCustomHighlighterRange);
begin
  if (Template<>nil) and (ClassType<>Template.ClassType) then
    RaiseGDBException('');
  if Template<>nil then
    Assign(Template);
end;

destructor TSynCustomHighlighterRange.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TSynCustomHighlighterRange.Compare(Range: TSynCustomHighlighterRange
  ): integer;
begin
  if RangeType<Range.RangeType then
    Result:=1
  else if RangeType>Range.RangeType then
    Result:=-1
  else
    Result := FTop.Compare(Range.FTop);
end;

function TSynCustomHighlighterRange.Add(ABlockType: Pointer;
  IncreaseLevel: Boolean = True): TSynCustomCodeFoldBlock;
begin
  Result := FTop.Child[ABlockType];
  if IncreaseLevel then
    inc(FCodeFoldStackSize);
  FTop:=Result;
end;

procedure TSynCustomHighlighterRange.Pop(DecreaseLevel: Boolean = True);
// can be called, even if there is no stack
// because it's normal that sources under development have unclosed blocks
begin
  //debugln('TSynCustomHighlighterRange.Pop');
  if assigned(FTop.Parent) then begin
    FTop := FTop.Parent;
    if DecreaseLevel then
      dec(FCodeFoldStackSize);
  end;
end;

procedure TSynCustomHighlighterRange.Clear;
begin
  FRangeType:=nil;
  FCodeFoldStackSize := 0;
  FTop:=nil;
end;

procedure TSynCustomHighlighterRange.Assign(Src: TSynCustomHighlighterRange);
begin
  if (Src<>nil) and (Src<>TSynCustomHighlighterRange(NullRange)) then begin
    FTop := Src.FTop;
    FCodeFoldStackSize := Src.FCodeFoldStackSize;
    FRangeType := Src.FRangeType;
  end
  else begin
    FTop := nil;
    FCodeFoldStackSize := 0;
    FRangeType := nil;
  end;
end;

procedure TSynCustomHighlighterRange.WriteDebugReport;
begin
  debugln('TSynCustomHighlighterRange.WriteDebugReport ',DbgSName(Self),
    ' RangeType=',dbgs(RangeType),' StackSize=',dbgs(CodeFoldStackSize));
  debugln(' BlockType=',dbgs(FTop.BlockType));
end;

{ TSynCustomHighlighterRanges }

constructor TSynCustomHighlighterRanges.Create(
  TheHighlighterClass: TSynCustomHighlighterClass);
begin
  Allocate;
  FItems:=TAvgLvlTree.Create(@CompareSynHighlighterRanges);
end;

destructor TSynCustomHighlighterRanges.Destroy;
begin
  if HighlighterRanges<>nil then begin
    HighlighterRanges.Remove(Self);
    if HighlighterRanges.Count=0 then
      FreeAndNil(HighlighterRanges);
  end;
  FItems.FreeAndClear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TSynCustomHighlighterRanges.GetEqual(Range: TSynCustomHighlighterRange
  ): TSynCustomHighlighterRange;
var
  Node: TAvgLvlTreeNode;
begin
  if Range=nil then exit(nil);
  Node:=FItems.Find(Range);
  if Node<>nil then begin
    Result:=TSynCustomHighlighterRange(Node.Data);
  end else begin
    // add a copy
    Result:=TSynCustomHighlighterRangeClass(Range.ClassType).Create(Range);
    FItems.Add(Result);
  end;
  //debugln('TSynCustomHighlighterRanges.GetEqual A ',dbgs(Node),' ',dbgs(Result.Compare(Range)),' ',dbgs(Result.CodeFoldStackSize));
end;

procedure TSynCustomHighlighterRanges.Allocate;
begin
  inc(FAllocatedCount);
end;

procedure TSynCustomHighlighterRanges.Release;
begin
  dec(FAllocatedCount);
  if FAllocatedCount=0 then Free;
end;
{$ENDIF}

initialization
  G_PlaceableHighlighters := TSynHighlighterList.Create;
finalization
  G_PlaceableHighlighters.Free;
  G_PlaceableHighlighters := nil;
{$ENDIF}
end.


