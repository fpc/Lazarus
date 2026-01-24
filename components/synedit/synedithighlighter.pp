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
  SysUtils, Classes, Registry, IniFiles, Contnrs,
  // LCL
  LCLType, Graphics,
  // LazUtils
  LazUTF8, LazMethodList,
  // SynEdit
  SynEditTypes, SynEditTextBase, SynEditMiscProcs, LazEditTextAttributes, LazEditHighlighterUtils,
  LazEditHighlighter;

type
  TSynHighlighterRangeList = TLazHighlighterLineRangeList deprecated 'use TLazHighlighterLineRangeList or TLazHighlighterLineRangeShiftList / to be removed in 5.99';

  TLazSynCustomTextAttributes = TLazEditTextAttribute deprecated 'use TLazEditTextAttribute // to be removed in 5.99';
  TSynHighlighterAttributes = TLazEditHighlighterAttributes;
  TSynHighlighterAttributesModifier = TLazEditHighlighterAttributesModifier;
  TSynHighlighterAttributes_Eol = TLazEditHighlighterAttributes_Eol;
  TSynHighlighterAttributesModifier_Eol = TLazEditHighlighterAttributesModifier_Eol;

  { TSynHighlighterAttributesHelper }

  TSynHighlighterAttributesHelper = class helper for TLazEditTextAttribute
  private
    function GetConstName: string;
    function GetStyleFromInt: integer;
    procedure SetStyleFromInt(const Value: integer);
    function GetStyleMaskFromInt : integer;
    procedure SetStyleMaskFromInt(const Value : integer);
  public
    function  LoadFromBorlandRegistry(rootKey: HKEY; attrKey, attrName: string;
                                      oldStyle: boolean): boolean;
    function  LoadFromRegistry(Reg: TRegistry): boolean;
    function  SaveToRegistry(Reg: TRegistry): boolean;
    function  LoadFromFile(Ini : TIniFile): boolean;
    function  SaveToFile(Ini : TIniFile): boolean;
  public
    property IntegerStyle: integer read GetStyleFromInt write SetStyleFromInt;
    property IntegerStyleMask: integer read GetStyleMaskFromInt write SetStyleMaskFromInt;
    property Name: string read GetConstName; deprecated 'use Caption'; // value of Caption at creation, use Caption instead, kept for compatibility
  end;


  TSynHighlighterCapability = (
    hcUserSettings, // supports Enum/UseUserSettings
    hcRegistry,     // supports LoadFrom/SaveToRegistry
    hcCodeFolding
  );

  TSynHighlighterCapabilities = set of TSynHighlighterCapability;

const
  { EXPERIMENTAL: A list of some typical attributes.
    This may be returned by a Highlighter via GetDefaultAttribute. Implementation
    is optional for each HL. So a HL may return nil even if it has an attribute
    of the requested type.
    This list does *not* aim to be complete. It may be replaced in future.
  }
  SYN_ATTR_COMMENT           =   0 deprecated 'Use TLazEditTokenClass / to be removed in 5.99';
  SYN_ATTR_IDENTIFIER        =   1 deprecated 'Use TLazEditTokenClass / to be removed in 5.99';
  SYN_ATTR_KEYWORD           =   2 deprecated 'Use TLazEditTokenClass / to be removed in 5.99';
  SYN_ATTR_STRING            =   3 deprecated 'Use TLazEditTokenClass / to be removed in 5.99';
  SYN_ATTR_WHITESPACE        =   4 deprecated 'Use TLazEditTokenClass / to be removed in 5.99';
  SYN_ATTR_SYMBOL            =   5 deprecated 'Use TLazEditTokenClass / to be removed in 5.99';
  SYN_ATTR_NUMBER            =   6 deprecated 'Use TLazEditTokenClass / to be removed in 5.99';
  SYN_ATTR_DIRECTIVE         =   7 deprecated 'Use TLazEditTokenClass / to be removed in 5.99';
  SYN_ATTR_ASM               =   8 deprecated 'Use TLazEditTokenClass / to be removed in 5.99';
  SYN_ATTR_VARIABLE          =   9 deprecated 'Use TLazEditTokenClass / to be removed in 5.99';

type

  TSynDividerDrawConfigSetting = TLazEditDividerDrawConfigSetting;
  TSynDividerDrawConfig = TLazEditDividerDrawConfig;

  { TSynCustomHighlighter }

  TSynCustomHighlighter = class(TLazEditCustomRangesHighlighter)
  private
    fAttrChangeHooks: TMethodList;
    FCapabilities: TSynHighlighterCapabilities deprecated;
    FDrawDividerLevel: Integer deprecated;
    fEnabled: Boolean deprecated;
    fWordBreakChars: TSynIdentChars deprecated;
    function GetKnownLines: TLazEditHighlighterAttachedLines; deprecated;
    procedure SetDrawDividerLevel(const AValue: Integer); deprecated;
    procedure SetEnabled(const Value: boolean);                                 //DDH 2001-10-23
  protected
    FAttributeChangeNeedScan: Boolean deprecated 'use RequestFullRescan // to be removed in 5.99';
    fDefaultFilter: string deprecated 'Use GetInitialDefaultFileFilterMask / to be removed in 5.99';
    fDefaultFilterInitialValue: string deprecated 'Use GetInitialDefaultFileFilterMask / to be removed in 5.99';
    fUpdateChange: boolean;                                                     //mh 2001-09-13
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      virtual; deprecated 'Use GetTokenClassAttribute / to be removed in 5.99';
    function GetDefaultFilter: string; virtual; deprecated'to be removed in 5.99';
    procedure SetWordBreakChars(AChars: TSynIdentChars); virtual;
    function IsFilterStored: boolean; virtual; deprecated'to be removed in 5.99';
    function __OLD_FileFilterDefaultMask: string; override; deprecated'to be removed in 5.99';
    procedure SetDefaultFilter(Value: string); virtual; deprecated'to be removed in 5.99';
    procedure SetSampleSource(Value: string); virtual;
    procedure AfterAttachedToRangeList(ARangeList: TLazHighlighterLineRangeList); virtual; deprecated 'use DoAttachedToLines // to be removed in 5.99';
    procedure BeforeDetachedFromRangeList(ARangeList: TLazHighlighterLineRangeList); virtual; deprecated 'use DoDetachingFromLines // to be removed in 5.99';
    // code fold - only valid if hcCodeFolding in Capabilities
    function PerformScan(StartIndex, EndIndex: Integer; ForceEndIndex: Boolean = False): Integer; virtual;  deprecated 'use DoPrepareLines / to be removed in 5.99';
    property KnownLines: TLazEditHighlighterAttachedLines read GetKnownLines; deprecated 'use AttachedLines // to be removed in 5.99';
    procedure RequestFullRescan; reintroduce; // deprecated 'to be removed in 5.99' // only needed to force a call to fAttrChangeHooks
    procedure SendAttributeChangeNotification; reintroduce; // deprecated 'to be removed in 5.99'
    procedure DoDefHighlightChanged; virtual;
    procedure DoEndUpdate; override;
    procedure DoAttachedToLines(Lines: TLazEditStringsBase; ARangeList: TLazHighlighterLineRangeList); override;
    procedure DoDetachingFromLines(Lines: TLazEditStringsBase; ARangeList: TLazHighlighterLineRangeList); override;
  public
    procedure DefHighlightChange(Sender: TObject);
    property  AttributeChangeNeedScan: Boolean read FAttributeChangeNeedScan; deprecated 'use RequestFullRescan // to be removed in 5.99';
    class function GetCapabilities: TSynHighlighterCapabilities; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddSpecialAttribute(const aCaption: string;
                     const aStoredName: String = ''): TLazEditTextAttribute;
    function AddSpecialAttribute(const aCaption: PString;
                     const aStoredName: String = ''): TLazEditTextAttribute;
    procedure Assign(Source: TPersistent); override;
  public
    procedure InitForScanningLine; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override; deprecated;
    procedure ResetRange; override; deprecated;
    //function GetTokenPos: Integer; override; // 0-based
    function GetTokenLen: Integer; override;
    function GetTokenClass: TLazEditTokenClass; override; deprecated 'Sub-class needs to implement this';

    property DrawDividerLevel: Integer read FDrawDividerLevel write SetDrawDividerLevel; deprecated;
  public
    procedure ScanRanges; deprecated 'use PrepareLines / to be removed in 5.99';
    (* IdleScanRanges
       Scan in small chunks during OnIdle; Return True, if more work avail
       This method is still under development. It may be changed, removed, un-virtualized, or anything.
       In future SynEdit & HL may have other IDLE tasks, and if and when that happens, there will be new ways to control this
    *)
    function  IdleScanRanges: Boolean; virtual; experimental;  deprecated 'use PrepareLines / to be removed in 5.99';
    function NeedScan: Boolean;  deprecated 'use FirstUnpreparedLine / to be removed in 5.99';
    procedure ScanAllRanges; deprecated 'use MarkUnprepared / to be removed in 5.99';
    procedure SetLine(const NewValue: String;
                      LineNumber:Integer // 0 based
                      ); virtual; deprecated 'Use InitForScanningLine // to be removed in 5.99';
  public
    function UseUserSettings(settingIndex: integer): boolean; virtual;
    procedure EnumUserSettings(Settings: TStrings); virtual;
    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; virtual;
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; virtual;
    function LoadFromFile(AFileName: String): boolean;                          //DDH 10/16/01
    function SaveToFile(AFileName: String): boolean;                            //DDH 10/16/01
    procedure HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);  deprecated 'use senrHighlightRescanNeeded // to be removed in 5.99';
    procedure UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);  deprecated 'use senrHighlightRescanNeeded // to be removed in 5.99';
    property WordBreakChars: TSynIdentChars read fWordBreakChars write SetWordBreakChars;  deprecated 'to become read-only in 5.99';
  public
    property Capabilities: TSynHighlighterCapabilities read FCapabilities;  deprecated 'to be removed in 5.99 / no replacement';
    property SampleSource: string read GetSampleSource write SetSampleSource; deprecated 'to become read-only in 5.99';
    // The below should be depricated and moved to those HL that actually implement them.
    property CommentAttribute: TSynHighlighterAttributes
      index SYN_ATTR_COMMENT read GetDefaultAttribute; deprecated 'Use GetTokenClassAttribute / to be removed in 5.99';
    property IdentifierAttribute: TSynHighlighterAttributes
      index SYN_ATTR_IDENTIFIER read GetDefaultAttribute; deprecated 'Use GetTokenClassAttribute / to be removed in 5.99';
    property KeywordAttribute: TSynHighlighterAttributes
      index SYN_ATTR_KEYWORD read GetDefaultAttribute; deprecated 'Use GetTokenClassAttribute / to be removed in 5.99';
    property StringAttribute: TSynHighlighterAttributes
      index SYN_ATTR_STRING read GetDefaultAttribute; deprecated 'Use GetTokenClassAttribute / to be removed in 5.99';
    property SymbolAttribute: TSynHighlighterAttributes                         //mh 2001-09-13
      index SYN_ATTR_SYMBOL read GetDefaultAttribute; deprecated 'Use GetTokenClassAttribute / to be removed in 5.99';
    property WhitespaceAttribute: TSynHighlighterAttributes
      index SYN_ATTR_WHITESPACE read GetDefaultAttribute; deprecated 'Use GetTokenClassAttribute / to be removed in 5.99';
  published
    property Enabled: boolean read fEnabled write SetEnabled default TRUE;      //DDH 2001-10-23
        deprecated 'to be removed in 5.99 / no replacement - was never implemented';
  end;

  TSynCustomHighlighterClass = class of TSynCustomHighlighter;

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

  procedure RegisterPlaceableHighlighter(highlighter: TSynCustomHighlighterClass);
  function GetPlaceableHighlighters: TSynHighlighterList;

implementation

const
  IDLE_SCAN_CHUNK_SIZE = 2500;

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

{ TSynHighlighterAttributesHelper }

function TSynHighlighterAttributesHelper.LoadFromBorlandRegistry(rootKey: HKEY;
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
    //fgColRGB : string;
    //bgColRGB : string;
    fontStyle: string;
    fgDefault: string;
    bgDefault: string;
    fgIndex16: string;
    bgIndex16: string;
    {$ENDIF}
    reg      : TRegistry;

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
      reg := TRegistry.Create;
      reg.RootKey := rootKey;
      try
        with reg do begin
          {$IFNDEF SYN_LAZARUS}
          // ToDo Registry
          if OpenKeyReadOnly(attrKey) then begin
            try
              if ValueExists(attrName) then begin
                descript := ReadString(attrName);
                //fgColRGB  := Get(descript);
                //bgColRGB  := Get(descript);
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
    reg          : TRegistry;

    function IsTrue(value: string): boolean;
    begin
      Result := not ((CompareText(value,'FALSE') = 0) or (value = '0'));
    end; { IsTrue }

  begin
    Result := false;
    try
      reg := TRegistry.Create;
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
end; { TSynHighlighterAttributesHelper.LoadFromBorlandRegistry }

function TSynHighlighterAttributesHelper.LoadFromRegistry(Reg: TRegistry): boolean;
{$IFNDEF SYN_LAZARUS}
var
  key: string;
{$ENDIF}
begin
  {$IFNDEF SYN_LAZARUS}
  // ToDo  Registry
  key := Reg.CurrentPath;
  if Reg.OpenKeyReadOnly(StoredName) then begin
    if Reg.ValueExists('Background') then
      Background := Reg.ReadInteger('Background');
    if Reg.ValueExists('Foreground') then
      Foreground := Reg.ReadInteger('Foreground');
    if Reg.ValueExists('Style') then
      IntegerStyle := Reg.ReadInteger('Style');
    if Reg.ValueExists('StyleMask') then
      IntegerStyle := Reg.ReadInteger('StyleMask');
    reg.OpenKeyReadOnly('\' + key);
    Result := true;
  end else
    Result := false;
  {$ELSE}
  Result:=false;
  {$ENDIF}
end;

function TSynHighlighterAttributesHelper.SaveToRegistry(Reg: TRegistry): boolean;
var
  key: string;
begin
  key := Reg.CurrentPath;
  if Reg.OpenKey(StoredName,true) then begin
    Reg.WriteInteger('Background', Background);
    Reg.WriteInteger('Foreground', Foreground);
    Reg.WriteInteger('Style', IntegerStyle);
    Reg.WriteInteger('StyleMask', IntegerStyleMask);
    reg.OpenKey('\' + key, false);
    Result := true;
  end else
    Result := false;
end;

function TSynHighlighterAttributesHelper.LoadFromFile(Ini : TIniFile): boolean;       //DDH 10/16/01
var
  S: TStringListUTF8Fast;
begin
  S := TStringListUTF8Fast.Create;
  try
    Ini.ReadSection(StoredName, S);
    if S.Count > 0 then
    begin
      if S.IndexOf('Background') <> -1 then
        Background := Ini.ReadInteger(StoredName, 'Background', clWindow);
      if S.IndexOf('Foreground') <> -1 then
        Foreground := Ini.ReadInteger(StoredName, 'Foreground', clWindowText);
      if S.IndexOf('Style') <> -1 then
        IntegerStyle := Ini.ReadInteger(StoredName, 'Style', 0);
      if S.IndexOf('StyleMask') <> -1 then
        IntegerStyleMask := Ini.ReadInteger(StoredName, 'StyleMask', 0);
      Result := true;
    end else Result := false;
  finally
    S.Free;
  end;
end;

function TSynHighlighterAttributesHelper.SaveToFile(Ini : TIniFile): boolean;         //DDH 10/16/01
begin
  Ini.WriteInteger(StoredName, 'Background', Background);
  Ini.WriteInteger(StoredName, 'Foreground', Foreground);
  Ini.WriteInteger(StoredName, 'Style', IntegerStyle);
  Ini.WriteInteger(StoredName, 'StyleMask', IntegerStyleMask);
  Result := true;
end;

function TSynHighlighterAttributesHelper.GetConstName: string;
begin
  Result := Caption^;
end;

function TSynHighlighterAttributesHelper.GetStyleFromInt: integer;
begin
  if fsBold in Style then Result:= 1 else Result:= 0;
  if fsItalic in Style then Result:= Result + 2;
  if fsUnderline in Style then Result:= Result + 4;
  if fsStrikeout in Style then Result:= Result + 8;
end;

procedure TSynHighlighterAttributesHelper.SetStyleFromInt(const Value: integer);
begin
  if Value and $1 = 0 then  Style:= [] else Style:= [fsBold];
  if Value and $2 <> 0 then Style:= Style + [fsItalic];
  if Value and $4 <> 0 then Style:= Style + [fsUnderline];
  if Value and $8 <> 0 then Style:= Style + [fsStrikeout];
end;

function TSynHighlighterAttributesHelper.GetStyleMaskFromInt : integer;
begin
  if fsBold in StyleMask then Result:= 1 else Result:= 0;
  if fsItalic in StyleMask then Result:= Result + 2;
  if fsUnderline in StyleMask then Result:= Result + 4;
  if fsStrikeout in StyleMask then Result:= Result + 8;
end;

procedure TSynHighlighterAttributesHelper.SetStyleMaskFromInt(const Value : integer);
begin
  if Value and $1 = 0 then  StyleMask:= [] else StyleMask:= [fsBold];
  if Value and $2 <> 0 then StyleMask:= StyleMask + [fsItalic];
  if Value and $4 <> 0 then StyleMask:= StyleMask + [fsUnderline];
  if Value and $8 <> 0 then StyleMask:= StyleMask + [fsStrikeout];
end;

{ TSynCustomHighlighter }

constructor TSynCustomHighlighter.Create(AOwner: TComponent);
begin
  FCapabilities:=GetCapabilities;
  inherited Create(AOwner);
  fWordBreakChars := TSynWordBreakChars;
  fAttrChangeHooks := TMethodList.Create;
  if (fDefaultFilter = '') and
     (TMethod(@GetDefaultFilter).Code <> pointer(@TSynCustomHighlighter.GetDefaultFilter))
  then
    fDefaultFilter := GetDefaultFilter;
end;

destructor TSynCustomHighlighter.Destroy;
begin
  fAttrChangeHooks.Free;
  inherited Destroy;
end;

procedure TSynCustomHighlighter.Assign(Source: TPersistent);
var
  Src: TSynCustomHighlighter;
  i: integer;
begin
  if Source is TSynCustomHighlighter then begin
    Src := TSynCustomHighlighter(Source);
    // assign the sample source text only if same or descendant class
    if Src is ClassType then
      SampleSource := Src.SampleSource;
    fWordBreakChars := Src.WordBreakChars;
    Enabled := Src.Enabled;
  end else
    inherited Assign(Source);
end;

procedure TSynCustomHighlighter.InitForScanningLine;
begin
  SetLine(CurrentLineText, LineIndex);
  inherited InitForScanningLine;
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
  r: TRegistry;
{  i: integer; }
begin
  r := TRegistry.Create;
  try
    r.RootKey := RootKey;
    {TODO:
    if r.OpenKeyReadOnly(Key) then begin
      Result := true;
      for i := 0 to AttrCount-1 do
        Result := Result and Attribute[i].LoadFromRegistry(r);
    end
    else
    }
      Result := false;

  finally r.Free; end;
end;

function TSynCustomHighlighter.SaveToRegistry(RootKey: HKEY;
  Key: string): boolean;
var
  r: TRegistry;
  i: integer;
begin
  r := TRegistry.Create;
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

function TSynCustomHighlighter.AddSpecialAttribute(const aCaption: string;
  const aStoredName: String): TLazEditTextAttribute;
begin
  result := TSynHighlighterAttributes.Create(aCaption,aStoredName);
  AddAttribute(result);
end;

function TSynCustomHighlighter.AddSpecialAttribute(const aCaption: PString;
  const aStoredName: String): TLazEditTextAttribute;
begin
  Result := TSynHighlighterAttributes.Create(aCaption,aStoredName);
  AddAttribute(result);
end;

procedure TSynCustomHighlighter.DefHighlightChange(Sender: TObject);
begin
  if IsUpdating then
    fUpdateChange := TRUE
  else begin
    if FAttributeChangeNeedScan then inherited RequestFullRescan;
    inherited SendAttributeChangeNotification;
    fAttrChangeHooks.CallNotifyEvents(self);
    FAttributeChangeNeedScan := False;
    DoDefHighlightChanged;
  end;
end;

class function TSynCustomHighlighter.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := [hcRegistry]; //registry save/load supported by default
end;

function TSynCustomHighlighter.GetDefaultFilter: string;
begin
  Result := fDefaultFilter;
end;

function TSynCustomHighlighter.GetTokenLen: Integer;
var
  x: PChar;
begin
  GetTokenEx(x, Result);
end;

function TSynCustomHighlighter.GetTokenClass: TLazEditTokenClass;
var
  a: TLazEditTextAttribute;
begin
  Result := inherited GetTokenClass;
  if Result <> tcUnknown then
    exit;
  a := GetTokenAttribute;
  if a = nil then
    exit;

  if      a = GetDefaultAttribute(SYN_ATTR_COMMENT) then    Result := tcComment
  else if a = GetDefaultAttribute(SYN_ATTR_IDENTIFIER) then Result := tcIdentifier
  else if a = GetDefaultAttribute(SYN_ATTR_KEYWORD) then    Result := tcKeyword
  else if a = GetDefaultAttribute(SYN_ATTR_STRING) then     Result := tcString
  else if a = GetDefaultAttribute(SYN_ATTR_WHITESPACE) then Result := tcWhiteSpace
  else if a = GetDefaultAttribute(SYN_ATTR_SYMBOL) then     Result := tcSymbol
  else if a = GetDefaultAttribute(SYN_ATTR_NUMBER) then     Result := tcNumber
  else if a = GetDefaultAttribute(SYN_ATTR_DIRECTIVE) then  Result := tcDirective
  else if a = GetDefaultAttribute(SYN_ATTR_ASM) then        Result := tcEmbedded
  else if a = GetDefaultAttribute(SYN_ATTR_VARIABLE) then   Result := tcVariable
  ;
end;

procedure TSynCustomHighlighter.SetWordBreakChars(AChars: TSynIdentChars);
begin
  fWordBreakChars := AChars;
end;

function TSynCustomHighlighter.GetRange: Pointer;
begin
  Result := nil;
end;

procedure TSynCustomHighlighter.SetRange(Value: Pointer);
begin
  //
end;

procedure TSynCustomHighlighter.ResetRange;
begin
  //
end;

procedure TSynCustomHighlighter.HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Add(TMethod(ANotifyEvent));
end;

function TSynCustomHighlighter.IsFilterStored: boolean;
begin
  Result := TRUE;
end;

function TSynCustomHighlighter.__OLD_FileFilterDefaultMask: string;
begin
  // set by old code, instead of overriding GetInitialDefaultFileFilterMask;
  Result := FDefaultFilterInitialValue;
  if FDefaultFilter <> '' then Result := FDefaultFilter;
end;

procedure TSynCustomHighlighter.SetLine(const NewValue: String; LineNumber: Integer);
begin
  if LineIndex <> LineNumber then begin
    //debugln(['TSynCustomHighlighter.SetLine - outdated call / deprecated']);
    SetAlternativeLineTextForGetTokens(NewValue, LineNumber);
  end;
end;

procedure TSynCustomHighlighter.SetDefaultFilter(Value: string);
begin
  fDefaultFilter := Value;
end;

procedure TSynCustomHighlighter.SetSampleSource(Value: string);
begin
end;

procedure TSynCustomHighlighter.AfterAttachedToRangeList(ARangeList: TLazHighlighterLineRangeList);
begin  // empty base
end;

procedure TSynCustomHighlighter.BeforeDetachedFromRangeList(ARangeList: TLazHighlighterLineRangeList);
begin  // empty base
end;

procedure TSynCustomHighlighter.UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Remove(TMethod(ANotifyEvent));
end;

procedure TSynCustomHighlighter.ScanRanges;
begin
  PrepareLines;
end;

function TSynCustomHighlighter.IdleScanRanges: Boolean;
begin
  Result := not PrepareLines(-1, 25);
end;

function TSynCustomHighlighter.NeedScan: Boolean;
begin
  Result := (CurrentRanges.FirstInvalidLine >= 0);
end;

function TSynCustomHighlighter.PerformScan(StartIndex, EndIndex: Integer;
  ForceEndIndex: Boolean = False): Integer;
begin
  if ForceEndIndex then
    DoPrepareLines(StartIndex, -1, 25)
  else
    DoPrepareLines(StartIndex);
end;

procedure TSynCustomHighlighter.RequestFullRescan;
begin
  //inherited RequestFullRescan;
  FAttributeChangeNeedScan := True;
  DefHighlightChange(nil); // will call RequestFullRescan;
end;

procedure TSynCustomHighlighter.SendAttributeChangeNotification;
begin
  //inherited SendAttributeChangeNotification;
  DefHighlightChange(nil); // will call SendAttributeChangeNotification;
end;

procedure TSynCustomHighlighter.DoDefHighlightChanged;
begin
  //
end;

procedure TSynCustomHighlighter.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if fUpdateChange then begin
    fUpdateChange := FALSE;
    DefHighlightChange(Self);
  end;
end;

procedure TSynCustomHighlighter.DoAttachedToLines(Lines: TLazEditStringsBase;
  ARangeList: TLazHighlighterLineRangeList);
begin
  inherited DoAttachedToLines(Lines, ARangeList);
  AfterAttachedToRangeList(ARangeList); // RefCount already increased
end;

procedure TSynCustomHighlighter.DoDetachingFromLines(Lines: TLazEditStringsBase;
  ARangeList: TLazHighlighterLineRangeList);
begin
  BeforeDetachedFromRangeList(ARangeList); // RefCount already decreased
  inherited DoDetachingFromLines(Lines, ARangeList);
end;

procedure TSynCustomHighlighter.ScanAllRanges;
begin
  CurrentRanges.InvalidateAll;
  PrepareLines;
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

procedure TSynCustomHighlighter.SetDrawDividerLevel(const AValue: Integer);
begin
  if FDrawDividerLevel = AValue then exit;
  FDrawDividerLevel := AValue;
  //DefHighlightChange(Self);
end;

function TSynCustomHighlighter.GetKnownLines: TLazEditHighlighterAttachedLines;
begin
  Result := AttachedLines;
end;

function TSynCustomHighlighter.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT:    Result := TSynHighlighterAttributes(GetTokenClassAttribute(tcComment));
    SYN_ATTR_IDENTIFIER: Result := TSynHighlighterAttributes(GetTokenClassAttribute(tcIdentifier));
    SYN_ATTR_KEYWORD:    Result := TSynHighlighterAttributes(GetTokenClassAttribute(tcKeyword));
    SYN_ATTR_STRING:     Result := TSynHighlighterAttributes(GetTokenClassAttribute(tcString));
    SYN_ATTR_WHITESPACE: Result := TSynHighlighterAttributes(GetTokenClassAttribute(tcWhiteSpace));
    SYN_ATTR_SYMBOL:     Result := TSynHighlighterAttributes(GetTokenClassAttribute(tcSymbol));
    SYN_ATTR_NUMBER:     Result := TSynHighlighterAttributes(GetTokenClassAttribute(tcNumber));
    SYN_ATTR_DIRECTIVE:  Result := TSynHighlighterAttributes(GetTokenClassAttribute(tcDirective));
    SYN_ATTR_ASM:        Result := TSynHighlighterAttributes(GetTokenClassAttribute(tcEmbedded));
    SYN_ATTR_VARIABLE:   Result := TSynHighlighterAttributes(GetTokenClassAttribute(tcVariable));
    otherwise Result := nil;
  end;
end;

initialization
  G_PlaceableHighlighters := TSynHighlighterList.Create;
finalization
  G_PlaceableHighlighters.Free;
  G_PlaceableHighlighters := nil;
end.


