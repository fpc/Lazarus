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
  SysUtils, Classes, Registry, IniFiles,
  // LCL
  LCLProc, LCLIntf, LCLType, Graphics,
  // LazUtils
  LazUTF8, LazMethodList,
  // SynEdit
  SynEditTypes, SynEditTextBase;

type
  { TSynHighlighterRangeList }

  TSynHighlighterRangeList = class(TSynManagedStorageMem)
  private
    FRefCount: Integer;
    FNeedsReScanStartIndex: Integer;
    FNeedsReScanEndIndex: Integer;
    FNeedsReScanRealStartIndex: Integer;
    function GetNeedsReScanRealStartIndex: Integer;
    function GetRange(Index: Integer): Pointer;
    procedure SetRange(Index: Integer; const AValue: Pointer);
  protected
    procedure LineTextChanged(AIndex: Integer; ACount: Integer = 1); override;
    procedure InsertedLines(AIndex, ACount: Integer); override;
    procedure DeletedLines(AIndex, ACount: Integer); override;
  public
    constructor Create;
    procedure ClearReScanNeeded;
    procedure AdjustReScanStart(ANewStart: Integer);
    procedure InvalidateAll;
    procedure IncRefCount;
    procedure DecRefCount;
    property Range[Index: Integer]: Pointer read GetRange write SetRange; default;
    property RefCount: Integer read FRefCount;
    property NeedsReScanStartIndex: Integer read FNeedsReScanStartIndex;
    property NeedsReScanEndIndex: Integer read FNeedsReScanEndIndex;
    property NeedsReScanRealStartIndex: Integer read GetNeedsReScanRealStartIndex;
  end;

  { TLazSynCustomTextAttributes }

  TLazSynCustomTextAttributes = class(TPersistent)
  {strict} private
    FUpdateCount: Integer;
    FWasChanged: Boolean;

    FBackground: TColor;
    FForeground: TColor;
    FFrameColor: TColor;
    FFrameEdges: TSynFrameEdges;
    FFrameStyle: TSynLineStyle;
    fStyle: TFontStyles;
    fStyleMask: TFontStyles;

    FBackPriority: integer;
    FForePriority: integer;
    FFramePriority: integer;
    FStylePriority: Array [TFontStyle] of integer;

    function GetStylePriority(AIndex: TFontStyle): integer;
    procedure SetBackground(AValue: TColor);
    procedure SetBackPriority(AValue: integer);
    procedure SetForeground(AValue: TColor);
    procedure SetForePriority(AValue: integer);
    procedure SetFrameColor(AValue: TColor);
    procedure SetFrameEdges(AValue: TSynFrameEdges);
    procedure SetFramePriority(AValue: integer);
    procedure SetFrameStyle(AValue: TSynLineStyle);
    procedure SetStyle(AValue: TFontStyles);
    procedure SetStyleMask(AValue: TFontStyles);
    procedure SetStylePriority(AIndex: TFontStyle; AValue: integer);
  protected
    procedure AssignFrom(Src: TLazSynCustomTextAttributes); virtual;
    procedure DoClear; virtual;
    procedure Changed;
    procedure DoChange; virtual;
    procedure Init; virtual;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(aSource: TPersistent); override;
    procedure SetAllPriorities(APriority: integer);
    procedure BeginUpdate;
    procedure EndUpdate;
  public
    property Background: TColor read FBackground write SetBackground;
    property Foreground: TColor read FForeground write SetForeground;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameStyle: TSynLineStyle  read FFrameStyle write SetFrameStyle;
    property FrameEdges: TSynFrameEdges read FFrameEdges write SetFrameEdges;
    property Style: TFontStyles     read fStyle write SetStyle;
    property StyleMask: TFontStyles read fStyleMask write SetStyleMask;

    property StylePriority[Index: TFontStyle]: integer read GetStylePriority write SetStylePriority;

    property BackPriority: integer read FBackPriority write SetBackPriority;
    property ForePriority: integer read FForePriority write SetForePriority;
    property FramePriority: integer read FFramePriority write SetFramePriority;
    property BoldPriority: integer index fsBold read GetStylePriority write SetStylePriority;
    property ItalicPriority: integer index fsItalic read GetStylePriority write SetStylePriority;
    property UnderlinePriority: integer index fsUnderline read GetStylePriority write SetStylePriority;
    property StrikeOutPriority: integer index fsStrikeOut read GetStylePriority write SetStylePriority;
  end;

  { TSynHighlighterAttributes }

  TSynHighlighterAttributes = class(TLazSynCustomTextAttributes)
  private
    FBackgroundDefault: TColor;
    FForegroundDefault: TColor;
    FFrameColorDefault: TColor;
    FFrameEdgesDefault: TSynFrameEdges;
    FFrameStyleDefault: TSynLineStyle;
    FStyleDefault: TFontStyles;
    FStyleMaskDefault: TFontStyles;
    FStoredName: string;
    FConstName: string;
    FCaption: PString;
    FOnChange: TNotifyEvent;
    function GetBackgroundColorStored: boolean;
    function GetFontStyleMaskStored : boolean;
    function GetForegroundColorStored: boolean;
    function GetFrameColorStored: boolean;
    function GetFrameEdgesStored: boolean;
    function GetFrameStyleStored: boolean;
    function GetStyleFromInt: integer;
    procedure SetStyleFromInt(const Value: integer);
    function GetFontStyleStored: boolean;
    function GetStyleMaskFromInt : integer;
    procedure SetStyleMaskFromInt(const Value : integer);
  protected
    function GetBackPriorityStored: Boolean; virtual;
    function GetForePriorityStored: Boolean; virtual;
    function GetFramePriorityStored: Boolean; virtual;
    function GetStylePriorityStored(AIndex: TFontStyle): Boolean; virtual;

    procedure AssignFrom(Src: TLazSynCustomTextAttributes); override;
    procedure DoChange; override;
  public
    constructor Create;
    constructor Create(aCaption: string; aStoredName: String = '');
    constructor Create(aCaption: PString; aStoredName: String = '');
    function  IsEnabled: boolean; virtual;
    procedure InternalSaveDefaultValues; virtual;
    function  LoadFromBorlandRegistry(rootKey: HKEY; attrKey, attrName: string;
                                      oldStyle: boolean): boolean; virtual;
    function  LoadFromRegistry(Reg: TRegistry): boolean;
    function  SaveToRegistry(Reg: TRegistry): boolean;
    function  LoadFromFile(Ini : TIniFile): boolean;
    function  SaveToFile(Ini : TIniFile): boolean;
  public
    property IntegerStyle: integer read GetStyleFromInt write SetStyleFromInt;
    property IntegerStyleMask: integer read GetStyleMaskFromInt write SetStyleMaskFromInt;
    property Name: string read FConstName; // value of Caption at creation, use Caption instead, kept for compatibility
    property Caption: PString read FCaption; // is never nil
    property StoredName: string read FStoredName write FStoredName; // the identifier
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  published
    property Background stored GetBackgroundColorStored;
    property Foreground stored GetForegroundColorStored;
    property FrameColor stored GetFrameColorStored;
    property FrameStyle stored GetFrameStyleStored;
    property FrameEdges stored GetFrameEdgesStored;
    property Style stored GetFontStyleStored;
    // TODO: StyleMask move to TSynHighlighterAttributesModifier
    property StyleMask stored GetFontStyleMaskStored;
    // FStyle = [],       FStyleMask = []        ==> no modification
    // FStyle = [fsBold], FStyleMask = []        ==> invert fsBold
    // FStyle = [],       FStyleMask = [fsBold]  ==> clear  fsBold
    // FStyle = [fsBold], FStyleMask = [fsBold]  ==> set    fsBold

    property BackPriority stored GetBackPriorityStored;
    property ForePriority stored GetForePriorityStored;
    property FramePriority stored GetFramePriorityStored;
    property BoldPriority stored GetStylePriorityStored;
    property ItalicPriority stored GetStylePriorityStored;
    property UnderlinePriority stored GetStylePriorityStored;
    property StrikeOutPriority stored GetStylePriorityStored;
  end;

  { TSynHighlighterAttributesModifier }

  TSynHighlighterAttributesModifier = class(TSynHighlighterAttributes)
  private
    FBackAlpha: byte;
    FForeAlpha: byte;
    FFrameAlpha: byte;

    FBackAlphaDefault: byte;
    FForeAlphaDefault: byte;
    FFrameAlphaDefault: byte;
    FBackPriorityDefault: integer;
    FForePriorityDefault: integer;
    FFramePriorityDefault: integer;
    FStylePriorityDefault: Array [TFontStyle] of integer;

    function GetBackAlphaStored: Boolean;
    function GetForeAlphaStored: Boolean;
    function GetFrameAlphaStored: Boolean;

    procedure SetBackAlpha(AValue: byte);
    procedure SetForeAlpha(AValue: byte);
    procedure SetFrameAlpha(AValue: byte);
  protected
    function GetBackPriorityStored: Boolean; override;
    function GetForePriorityStored: Boolean; override;
    function GetFramePriorityStored: Boolean; override;
    function GetStylePriorityStored(Index: TFontStyle): Boolean; override;

    procedure AssignFrom(Src: TLazSynCustomTextAttributes); override;
    procedure DoClear; override;
  public
    procedure InternalSaveDefaultValues; override;
  published
    // Alpha = 0 means solid // 1..255 means n of 256
    property BackAlpha: byte read FBackAlpha write SetBackAlpha stored GetBackAlphaStored;
    property ForeAlpha: byte read FForeAlpha write SetForeAlpha stored GetForeAlphaStored;
    property FrameAlpha: byte read FFrameAlpha write SetFrameAlpha stored GetFrameAlphaStored;
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
  SYN_ATTR_COMMENT           =   0;
  SYN_ATTR_IDENTIFIER        =   1;
  SYN_ATTR_KEYWORD           =   2;
  SYN_ATTR_STRING            =   3;
  SYN_ATTR_WHITESPACE        =   4;
  SYN_ATTR_SYMBOL            =   5;
  SYN_ATTR_NUMBER            =   6;
  SYN_ATTR_DIRECTIVE         =   7;
  SYN_ATTR_ASM               =   8;
  SYN_ATTR_VARIABLE          =   9;

type

  TSynDividerDrawConfigSetting = Record
    Color: TColor;
  end;

const
  SynEmptyDividerDrawConfigSetting: TSynDividerDrawConfigSetting =
    ( Color: clNone );

type
  { TSynDividerDrawConfig }

  TSynDividerDrawConfig = class
  private
    FDepth: Integer;
    FTopSetting, FNestSetting: TSynDividerDrawConfigSetting;
    fOnChange: TNotifyEvent;
    function GetNestColor: TColor; virtual;
    function GetTopColor: TColor; virtual;
    procedure SetNestColor(const AValue: TColor); virtual;
    procedure SetTopColor(const AValue: TColor); virtual;
  protected
    function GetMaxDrawDepth: Integer; virtual;
    procedure SetMaxDrawDepth(AValue: Integer); virtual;
    procedure Changed;
  public
    // Do not use to set values, or you skip the change notification
    property TopSetting: TSynDividerDrawConfigSetting read FTopSetting;
    property NestSetting: TSynDividerDrawConfigSetting read FNestSetting;
  public
    constructor Create;
    procedure Assign(Src: TSynDividerDrawConfig); virtual;
    property MaxDrawDepth: Integer read GetMaxDrawDepth write SetMaxDrawDepth;
    property TopColor: TColor read GetTopColor write SetTopColor;
    property NestColor: TColor read GetNestColor write SetNestColor;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  { TSynEditLinesList }

  TSynEditLinesList=class(TFPList)
  private
    function GetSynString(Index: Integer): TSynEditStringsBase;
    procedure PutSynStrings(Index: Integer; const AValue: TSynEditStringsBase);
  public
    property Items[Index: Integer]: TSynEditStringsBase
             read GetSynString write PutSynStrings; default;
  end;

  { TSynCustomHighlighter }

  TSynCustomHighlighter = class(TComponent)
  private
    fAttributes: TStringList;
    fAttrChangeHooks: TMethodList;
    FCapabilities: TSynHighlighterCapabilities;
    FKnownLines: TSynEditLinesList;
    FCurrentLines: TSynEditStringsBase;
    FCurrentRanges: TSynHighlighterRangeList;
    FDrawDividerLevel: Integer;
    FLineIndex: Integer;
    FLineText: String;
    fUpdateCount: integer;                                                      //mh 2001-09-13
    fEnabled: Boolean;
    fWordBreakChars: TSynIdentChars;
    FIsScanning: Boolean;
    function GetKnownRanges(Index: Integer): TSynHighlighterRangeList;
    procedure SetDrawDividerLevel(const AValue: Integer); deprecated;
    procedure SetEnabled(const Value: boolean);                                 //DDH 2001-10-23
  protected
    FAttributeChangeNeedScan: Boolean;
    fDefaultFilter: string;
    fUpdateChange: boolean;                                                     //mh 2001-09-13
    FIsInNextToEOL: Boolean;
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
    function GetRangeIdentifier: Pointer; virtual;
    function CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList; virtual;
    procedure AfterAttachedToRangeList(ARangeList: TSynHighlighterRangeList); virtual;
    procedure BeforeDetachedFromRangeList(ARangeList: TSynHighlighterRangeList); virtual;
    function UpdateRangeInfoAtLine(Index: Integer): Boolean; virtual; // Returns true if range changed
    // code fold - only valid if hcCodeFolding in Capabilities
    procedure SetCurrentLines(const AValue: TSynEditStringsBase); virtual; // todo remove virtual
    procedure DoCurrentLinesChanged; virtual;
    property  LineIndex: Integer read FLineIndex;
    property CurrentRanges: TSynHighlighterRangeList read FCurrentRanges;
    function GetDrawDivider(Index: integer): TSynDividerDrawConfigSetting; virtual;
    function GetDividerDrawConfig(Index: Integer): TSynDividerDrawConfig; virtual;
    function GetDividerDrawConfigCount: Integer; virtual;
    function PerformScan(StartIndex, EndIndex: Integer; ForceEndIndex: Boolean = False): Integer; virtual;
    property IsScanning: Boolean read FIsScanning;
    property KnownRanges[Index: Integer]: TSynHighlighterRangeList read GetKnownRanges;
    property KnownLines: TSynEditLinesList read FKnownLines;
  public
    procedure DefHighlightChange(Sender: TObject);
    property  AttributeChangeNeedScan: Boolean read FAttributeChangeNeedScan;
    class function GetCapabilities: TSynHighlighterCapabilities; virtual;
    class function GetLanguageName: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddSpecialAttribute(const aCaption: string;
                     const aStoredName: String = ''): TSynHighlighterAttributes;
    function AddSpecialAttribute(const aCaption: PString;
                     const aStoredName: String = ''): TSynHighlighterAttributes;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AttachToLines(Lines: TSynEditStringsBase);
    procedure DetachFromLines(Lines: TSynEditStringsBase);
  public
    function GetEol: Boolean; virtual; abstract;
    function GetRange: Pointer; virtual;
    function GetToken: String; virtual; abstract;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); virtual; abstract;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; virtual; // valid after line was scanned to EOL
    function GetTokenAttribute: TSynHighlighterAttributes; virtual; abstract;
    function GetTokenKind: integer; virtual; abstract;
    function GetTokenPos: Integer; virtual; abstract; // 0-based
    function IsKeyword(const AKeyword: string): boolean; virtual;               // DJLP 2000-08-09
    procedure Next; virtual; abstract;
    procedure NextToEol;

    property DrawDivider[Index: integer]: TSynDividerDrawConfigSetting
      read GetDrawDivider;
    property DrawDividerLevel: Integer read FDrawDividerLevel write SetDrawDividerLevel; deprecated;
  public
    property CurrentLines: TSynEditStringsBase read FCurrentLines write SetCurrentLines;

    procedure StartAtLineIndex(LineNumber:Integer); virtual; // 0 based
    procedure ContinueNextLine; // To be called at EOL; does not read the range

    procedure ScanRanges;
    (* IdleScanRanges
       Scan in small chunks during OnIdle; Return True, if more work avail
       This method is still under development. It may be changed, removed, un-virtualized, or anything.
       In future SynEdit & HL may have other IDLE tasks, and if and when that happens, there will be new ways to control this
    *)
    function  IdleScanRanges: Boolean; virtual; experimental;
    function NeedScan: Boolean;
    procedure ScanAllRanges;
    procedure SetRange(Value: Pointer); virtual;
    procedure ResetRange; virtual;
    procedure SetLine(const NewValue: String;
                      LineNumber:Integer // 0 based
                      ); virtual;

  public
    function UseUserSettings(settingIndex: integer): boolean; virtual;
    procedure EnumUserSettings(Settings: TStrings); virtual;
    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; virtual;
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; virtual;
    function LoadFromFile(AFileName: String): boolean;                          //DDH 10/16/01
    function SaveToFile(AFileName: String): boolean;                            //DDH 10/16/01
    procedure HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    procedure UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    property IdentChars: TSynIdentChars read GetIdentChars;
    property WordBreakChars: TSynIdentChars read fWordBreakChars write SetWordBreakChars;
    property LanguageName: string read GetLanguageName;
  public
    property AttrCount: integer read GetAttribCount;
    property Attribute[idx: integer]: TSynHighlighterAttributes
      read GetAttribute;
    property Capabilities: TSynHighlighterCapabilities
       read FCapabilities;
    property SampleSource: string read GetSampleSource write SetSampleSource;
    // The below should be depricated and moved to those HL that actually implement them.
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

    property DividerDrawConfig[Index: Integer]: TSynDividerDrawConfig
      read GetDividerDrawConfig;
    property DividerDrawConfigCount: Integer read GetDividerDrawConfigCount;
  published
    property DefaultFilter: string read GetDefaultFilter write SetDefaultFilter
      stored IsFilterStored;
    property Enabled: boolean read fEnabled write SetEnabled default TRUE;      //DDH 2001-10-23
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

{ TSynHighlighterAttributesModifier }

procedure TSynHighlighterAttributesModifier.SetBackAlpha(AValue: byte);
begin
  if FBackAlpha = AValue then Exit;
  FBackAlpha := AValue;
  Changed;
end;

function TSynHighlighterAttributesModifier.GetBackPriorityStored: Boolean;
begin
  Result := FBackPriority <> FBackPriorityDefault;
end;

function TSynHighlighterAttributesModifier.GetBackAlphaStored: Boolean;
begin
  Result := FBackAlpha <> FBackAlphaDefault;
end;

function TSynHighlighterAttributesModifier.GetForeAlphaStored: Boolean;
begin
  Result := FForeAlpha <> FForeAlphaDefault;
end;

function TSynHighlighterAttributesModifier.GetForePriorityStored: Boolean;
begin
  Result := FForePriority <> FForePriorityDefault;
end;

function TSynHighlighterAttributesModifier.GetFrameAlphaStored: Boolean;
begin
  Result := FFrameAlpha <> FForeAlphaDefault;
end;

function TSynHighlighterAttributesModifier.GetFramePriorityStored: Boolean;
begin
  Result := FFramePriority <> FFramePriorityDefault;
end;

function TSynHighlighterAttributesModifier.GetStylePriorityStored(Index: TFontStyle): Boolean;
begin
  Result := FStylePriority[Index] <> FStylePriorityDefault[Index];
end;

procedure TSynHighlighterAttributesModifier.SetForeAlpha(AValue: byte);
begin
  if FForeAlpha = AValue then Exit;
  FForeAlpha := AValue;
  Changed;
end;

procedure TSynHighlighterAttributesModifier.SetFrameAlpha(AValue: byte);
begin
  if FFrameAlpha = AValue then Exit;
  FFrameAlpha := AValue;
  Changed;
end;

procedure TSynHighlighterAttributesModifier.AssignFrom(Src: TLazSynCustomTextAttributes);
var
  Source: TSynHighlighterAttributesModifier;
begin
  inherited AssignFrom(Src);
  if Src is TSynHighlighterAttributesModifier then begin
    Source := TSynHighlighterAttributesModifier(Src);
    FBackAlpha  := Source.BackAlpha;
    FForeAlpha  := Source.ForeAlpha;
    FFrameAlpha := Source.FrameAlpha;
  end
  else begin
    FBackAlpha  := 0;
    FForeAlpha  := 0;
    FFrameAlpha := 0;
  end;
end;

procedure TSynHighlighterAttributesModifier.DoClear;
begin
  inherited DoClear;
  FBackAlpha  := 0;
  FForeAlpha  := 0;
  FFrameAlpha := 0;
end;

procedure TSynHighlighterAttributesModifier.InternalSaveDefaultValues;
begin
  inherited InternalSaveDefaultValues;
  FBackPriorityDefault  := FBackPriority;
  FForePriorityDefault  := FForePriority;
  FFramePriorityDefault := FFramePriority;
  FForeAlphaDefault     := FForeAlpha;
  FBackAlphaDefault     := FBackAlpha;
  FFrameAlphaDefault    := FFrameAlpha;
  FStylePriorityDefault := FStylePriority;
end;

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

{ TLazSynCustomTextAttributes }

procedure TLazSynCustomTextAttributes.SetBackground(AValue: TColor);
begin
  if FBackground = AValue then Exit;
  FBackground := AValue;
  Changed;
end;

function TLazSynCustomTextAttributes.GetStylePriority(AIndex: TFontStyle
  ): integer;
begin
  Result := FStylePriority[AIndex];
end;

procedure TLazSynCustomTextAttributes.SetBackPriority(AValue: integer);
begin
  if FBackPriority = AValue then Exit;
  FBackPriority := AValue;
  Changed;
end;

procedure TLazSynCustomTextAttributes.SetForeground(AValue: TColor);
begin
  if FForeground = AValue then Exit;
  FForeground := AValue;
  Changed;
end;

procedure TLazSynCustomTextAttributes.SetForePriority(AValue: integer);
begin
  if FForePriority = AValue then Exit;
  FForePriority := AValue;
  Changed;
end;

procedure TLazSynCustomTextAttributes.SetFrameColor(AValue: TColor);
begin
  if FFrameColor = AValue then Exit;
  FFrameColor := AValue;
  Changed;
end;

procedure TLazSynCustomTextAttributes.SetFrameEdges(AValue: TSynFrameEdges);
begin
  if FFrameEdges = AValue then Exit;
  FFrameEdges := AValue;
  Changed;
end;

procedure TLazSynCustomTextAttributes.SetFramePriority(AValue: integer);
begin
  if FFramePriority = AValue then Exit;
  FFramePriority := AValue;
  Changed;
end;

procedure TLazSynCustomTextAttributes.SetFrameStyle(AValue: TSynLineStyle);
begin
  if FFrameStyle = AValue then Exit;
  FFrameStyle := AValue;
  Changed;
end;

procedure TLazSynCustomTextAttributes.SetStyle(AValue: TFontStyles);
begin
  if fStyle = AValue then Exit;
  fStyle := AValue;
  Changed;
end;

procedure TLazSynCustomTextAttributes.SetStyleMask(AValue: TFontStyles);
begin
  if fStyleMask = AValue then Exit;
  fStyleMask := AValue;
  Changed;
end;

procedure TLazSynCustomTextAttributes.SetStylePriority(AIndex: TFontStyle;
  AValue: integer);
begin
  if FStylePriority[AIndex] = AValue then Exit;
  FStylePriority[AIndex] := AValue;
  Changed;
end;

procedure TLazSynCustomTextAttributes.Changed;
begin
  FWasChanged := True;
  if FUpdateCount > 0 then
    exit;
  FWasChanged := False;
  DoChange;
end;

procedure TLazSynCustomTextAttributes.AssignFrom(Src: TLazSynCustomTextAttributes);
begin
  //
end;

procedure TLazSynCustomTextAttributes.DoClear;
var
  i: TFontStyle;
begin
  Background := clNone;
  Foreground := clNone;
  FrameColor := clNone;
  FrameStyle := slsSolid;
  FrameEdges := sfeAround;
  Style := [];
  StyleMask := [];
  FForePriority := 0;
  FBackPriority := 0;
  FFramePriority := 0;
  for i := Low(TFontStyle) to High(TFontStyle) do
    FStylePriority[i] := 0;
end;

procedure TLazSynCustomTextAttributes.DoChange;
begin
  //
end;

procedure TLazSynCustomTextAttributes.Init;
begin
  // called by create
  Clear;
end;

constructor TLazSynCustomTextAttributes.Create;
begin
  inherited;
  FUpdateCount := 0;
  FWasChanged := False;
  Init;
end;

procedure TLazSynCustomTextAttributes.Clear;
begin
  BeginUpdate;
  DoClear;
  EndUpdate;
end;

procedure TLazSynCustomTextAttributes.Assign(aSource: TPersistent);
var
  Source : TLazSynCustomTextAttributes;
begin
  if aSource is TLazSynCustomTextAttributes then
  begin
    BeginUpdate;
    Source := TLazSynCustomTextAttributes(aSource);
    Foreground := Source.Foreground;
    Background := Source.Background;
    FrameColor := Source.FrameColor;
    FrameStyle := Source.FrameStyle;
    FrameEdges := Source.FrameEdges;
    Style      := Source.FStyle;
    StyleMask  := Source.FStyleMask;
    ForePriority   := Source.ForePriority;
    BackPriority   := Source.BackPriority;
    FramePriority  := Source.FramePriority;
    FStylePriority  := Source.FStylePriority;
    AssignFrom(Source);
    EndUpdate;
  end
  else
    inherited;
end;

procedure TLazSynCustomTextAttributes.SetAllPriorities(APriority: integer);
var
  i: TFontStyle;
begin
  BeginUpdate;
  ForePriority := APriority;
  BackPriority := APriority;
  FramePriority := APriority;
  for i := Low(TFontStyle) to High(TFontStyle) do
    StylePriority[i] := APriority;
  EndUpdate;
end;

procedure TLazSynCustomTextAttributes.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TLazSynCustomTextAttributes.EndUpdate;
begin
  dec(FUpdateCount);
  if (FUpdateCount = 0) and FWasChanged then
    Changed;
end;

{ TSynHighlighterAttributes }

constructor TSynHighlighterAttributes.Create(aCaption: string;
  aStoredName: String = '');
begin
  Create;
  FConstName := aCaption;
  FCaption := @FConstName;
  if aStoredName = '' then
    aStoredName := FConstName;
  FStoredName := aStoredName;;
end;

constructor TSynHighlighterAttributes.Create(aCaption: PString;
  aStoredName: String);
begin
  Create;
  if aCaption<>nil then begin
    FConstName := aCaption^;
    FCaption := aCaption;
  end
  else begin
    FConstName := '';
    FCaption := @FConstName;
  end;
  if aStoredName = '' then
    aStoredName := FConstName;
  FStoredName := aStoredName;;
end;

function TSynHighlighterAttributes.IsEnabled: boolean;
begin
  Result := (Background <> clNone) or (Foreground <> clNone) or
            ( (FrameColor <> clNone) and (FrameEdges <> sfeNone) ) or
            (Style <> []) or (StyleMask <> []);
end;

function TSynHighlighterAttributes.GetBackgroundColorStored: boolean;
begin
  Result := Background <> fBackgroundDefault;
end;

function TSynHighlighterAttributes.GetFontStyleMaskStored : boolean;
begin
  Result := StyleMask <> fStyleMaskDefault;
end;

function TSynHighlighterAttributes.GetForegroundColorStored: boolean;
begin
  Result := Foreground <> fForegroundDefault;
end;

function TSynHighlighterAttributes.GetFrameColorStored: boolean;
begin
  Result := FrameColor <> FFrameColorDefault;
end;

function TSynHighlighterAttributes.GetFrameEdgesStored: boolean;
begin
  Result := FrameEdges <> FFrameEdgesDefault;
end;

function TSynHighlighterAttributes.GetFrameStyleStored: boolean;
begin
  Result := FrameStyle <> FFrameStyleDefault;
end;

function TSynHighlighterAttributes.GetFontStyleStored: boolean;
begin
  Result := Style <> fStyleDefault;
end;

procedure TSynHighlighterAttributes.InternalSaveDefaultValues;
(* Called once from TSynCustomHighlighter.Create (and only from there),
   after all Attributes where created  *)
begin
  fForegroundDefault := Foreground;
  fBackgroundDefault := Background;
  FFrameColorDefault := FrameColor;
  FFrameStyleDefault := FrameStyle;
  FFrameEdgesDefault := FrameEdges;
  fStyleDefault      := Style;
  fStyleMaskDefault  := StyleMask;
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
      Result := not ((UpperCase(value) = 'FALSE') or (value = '0'));
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
end; { TSynHighlighterAttributes.LoadFromBorlandRegistry }

function TSynHighlighterAttributes.LoadFromRegistry(Reg: TRegistry): boolean;
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

function TSynHighlighterAttributes.SaveToRegistry(Reg: TRegistry): boolean;
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

function TSynHighlighterAttributes.LoadFromFile(Ini : TIniFile): boolean;       //DDH 10/16/01
var
  S: TStringList;
begin
  S := TStringList.Create;
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

function TSynHighlighterAttributes.SaveToFile(Ini : TIniFile): boolean;         //DDH 10/16/01
begin
  Ini.WriteInteger(StoredName, 'Background', Background);
  Ini.WriteInteger(StoredName, 'Foreground', Foreground);
  Ini.WriteInteger(StoredName, 'Style', IntegerStyle);
  Ini.WriteInteger(StoredName, 'StyleMask', IntegerStyleMask);
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

function TSynHighlighterAttributes.GetBackPriorityStored: Boolean;
begin
  Result := FBackPriority = 0;
end;

function TSynHighlighterAttributes.GetForePriorityStored: Boolean;
begin
  Result := FForePriority = 0;
end;

function TSynHighlighterAttributes.GetFramePriorityStored: Boolean;
begin
  Result := FFramePriority = 0;
end;

function TSynHighlighterAttributes.GetStylePriorityStored(AIndex: TFontStyle
  ): Boolean;
begin
  Result := FStylePriority[AIndex] = 0;
end;

procedure TSynHighlighterAttributes.AssignFrom(Src: TLazSynCustomTextAttributes);
begin
  inherited AssignFrom(Src);
  if not (Src is TSynHighlighterAttributes) then exit;
  FConstName      := TSynHighlighterAttributes(Src).FConstName;
  FStoredName:= TSynHighlighterAttributes(Src).FStoredName;
  Changed; {TODO: only if really changed}
end;

procedure TSynHighlighterAttributes.DoChange;
begin
  inherited DoChange;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

constructor TSynHighlighterAttributes.Create;
begin
  inherited Create;
  InternalSaveDefaultValues;
end;

{ TSynEditLinesList }

function TSynEditLinesList.GetSynString(Index: Integer): TSynEditStringsBase;
begin
  Result := TSynEditStringsBase(inherited Items[Index]);
end;

procedure TSynEditLinesList.PutSynStrings(Index: Integer; const AValue: TSynEditStringsBase);
begin
  inherited Items[Index] := AValue;
end;

{ TSynCustomHighlighter }

constructor TSynCustomHighlighter.Create(AOwner: TComponent);
begin
  FCapabilities:=GetCapabilities;
  FKnownLines := TSynEditLinesList.Create;
  inherited Create(AOwner);
  fWordBreakChars := TSynWordBreakChars;
  fAttributes := TStringList.Create;
  fAttributes.Duplicates := dupError;
  fAttributes.Sorted := TRUE;
  fAttrChangeHooks := TMethodList.Create;
  fDefaultFilter := '';
end;

destructor TSynCustomHighlighter.Destroy;
begin
  FreeHighlighterAttributes;
  fAttributes.Free;
  fAttrChangeHooks.Free;
  inherited Destroy;
  FreeAndNil(FKnownLines);
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
  SrcAttri: TSynHighlighterAttributes;
  StoredName: String;
begin
  if (Source <> nil) and (Source is TSynCustomHighlighter) then begin
    Src := TSynCustomHighlighter(Source);
    for i := 0 to AttrCount - 1 do begin
      // assign first attribute with the same name
      StoredName := Attribute[i].StoredName;
      for j := 0 to Src.AttrCount - 1 do begin
        SrcAttri := Src.Attribute[j];
        if StoredName = SrcAttri.StoredName then begin
          Attribute[i].Assign(SrcAttri);
          continue;
        end;
      end;
    end;
    for i := 0 to DividerDrawConfigCount - 1 do
      DividerDrawConfig[i].Assign(Src.DividerDrawConfig[i]);
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

procedure TSynCustomHighlighter.AddAttribute(AAttrib: TSynHighlighterAttributes);
begin
  fAttributes.AddObject(AAttrib.StoredName, AAttrib);
end;

function TSynCustomHighlighter.AddSpecialAttribute(const aCaption: string;
  const aStoredName: String): TSynHighlighterAttributes;
begin
  result := TSynHighlighterAttributes.Create(aCaption,aStoredName);
  AddAttribute(result);
end;

function TSynCustomHighlighter.AddSpecialAttribute(const aCaption: PString;
  const aStoredName: String): TSynHighlighterAttributes;
begin
  Result := TSynHighlighterAttributes.Create(aCaption,aStoredName);
  AddAttribute(result);
end;

procedure TSynCustomHighlighter.DefHighlightChange(Sender: TObject);
begin
  if fUpdateCount > 0 then
    fUpdateChange := TRUE
  else begin
    fAttrChangeHooks.CallNotifyEvents(self);
    FAttributeChangeNeedScan := False;
  end;
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

class function TSynCustomHighlighter.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := [hcRegistry]; //registry save/load supported by default
end;

function TSynCustomHighlighter.GetDefaultFilter: string;
begin
  Result := fDefaultFilter;
end;

function TSynCustomHighlighter.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'A'..'Z', 'a'..'z', '0'..'9'];
end;

function TSynCustomHighlighter.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  Result := nil;
end;

procedure TSynCustomHighlighter.SetWordBreakChars(AChars: TSynIdentChars);
begin
  fWordBreakChars := AChars;
end;

class function TSynCustomHighlighter.GetLanguageName: string;
begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  raise Exception.CreateFmt('%s.GetLanguageName not implemented', [ClassName]);
{$ENDIF}
  Result := '<Unknown>';
end;

function TSynCustomHighlighter.GetRange: Pointer;
begin
  Result := nil;
end;

function TSynCustomHighlighter.GetSampleSource: string;
begin
  Result := '';
end;

procedure TSynCustomHighlighter.HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Add(TMethod(ANotifyEvent));
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
  FIsInNextToEOL := True;
  while not GetEol do Next;
  FIsInNextToEOL := False;
end;

procedure TSynCustomHighlighter.ContinueNextLine;
begin
  inc(FLineIndex);
  SetLine(CurrentLines[FLineIndex], FLineIndex);
end;

procedure TSynCustomHighlighter.StartAtLineIndex(LineNumber: Integer);
begin
  FLineIndex := LineNumber;
  if LineNumber = 0 then
    ResetRange
  else
    SetRange(FCurrentRanges[LineNumber - 1]);
  SetLine(CurrentLines[LineNumber], LineNumber);
end;

procedure TSynCustomHighlighter.ResetRange;
begin
end;

procedure TSynCustomHighlighter.SetLine(const NewValue: String; LineNumber: Integer);
begin
  // Keep a copy of the line text, since some highlighters just use a PChar pointer to it.
  FLineText := NewValue;
  FIsInNextToEOL := False;
  FLineIndex := LineNumber;
end;

procedure TSynCustomHighlighter.SetAttributesOnChange(AEvent: TNotifyEvent);
(* Called once from TSynCustomHighlighter.Create (and only from there),
   after all Attributes where created  *)
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
end;

procedure TSynCustomHighlighter.SetDefaultFilter(Value: string);
begin
  fDefaultFilter := Value;
end;

procedure TSynCustomHighlighter.SetSampleSource(Value: string);
begin
end;

function TSynCustomHighlighter.GetRangeIdentifier: Pointer;
begin
  Result := self;
end;

function TSynCustomHighlighter.CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList;
begin
  Result := TSynHighlighterRangeList.Create;
end;

procedure TSynCustomHighlighter.AfterAttachedToRangeList(ARangeList: TSynHighlighterRangeList);
begin  // empty base
end;

procedure TSynCustomHighlighter.BeforeDetachedFromRangeList(ARangeList: TSynHighlighterRangeList);
begin  // empty base
end;

procedure TSynCustomHighlighter.UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Remove(TMethod(ANotifyEvent));
end;

function TSynCustomHighlighter.UpdateRangeInfoAtLine(Index: Integer): Boolean;
var
  r: Pointer;
begin
  r := GetRange;
  Result := r <> FCurrentRanges[Index];
  if Result then
    FCurrentRanges[Index] := r;
end;

procedure TSynCustomHighlighter.ScanRanges;
var
  StartIndex, EndIndex: Integer;
begin
  StartIndex := CurrentRanges.NeedsReScanStartIndex;
  if (StartIndex < 0) or (StartIndex >= CurrentRanges.Count) then
    exit;
  EndIndex := CurrentRanges.NeedsReScanEndIndex + 1;
  //debugln(['=== scan ',StartIndex,' - ',EndIndex]);
  FIsScanning := True;
  try
    EndIndex :=  PerformScan(StartIndex, EndIndex);
  finally
    FIsScanning := False;
  end;
  StartIndex := CurrentRanges.NeedsReScanRealStartIndex; // include idle scanned
  CurrentRanges.ClearReScanNeeded;
  // Invalidate one line above, since folds can change depending on next line
  // TODO: only classes with end-fold-last-line
  if StartIndex > 0
  then CurrentLines.SendHighlightChanged(StartIndex - 1, EndIndex - StartIndex + 1)
  else CurrentLines.SendHighlightChanged(StartIndex,     EndIndex - StartIndex    );
end;

function TSynCustomHighlighter.IdleScanRanges: Boolean;
var
  StartIndex, EndIndex, RealEndIndex: Integer;
begin
  Result := False;
  StartIndex := CurrentRanges.NeedsReScanStartIndex;
  if (StartIndex < 0) or (StartIndex >= CurrentRanges.Count) then
    exit;
  EndIndex := CurrentRanges.NeedsReScanEndIndex + 1;

  RealEndIndex := EndIndex;
  if EndIndex > StartIndex + IDLE_SCAN_CHUNK_SIZE then
    EndIndex := StartIndex + IDLE_SCAN_CHUNK_SIZE;
  FIsScanning := True;
  try
    EndIndex :=  PerformScan(StartIndex, EndIndex, True);
  finally
    FIsScanning := False;
  end;

  if EndIndex >= RealEndIndex then begin
    StartIndex := CurrentRanges.NeedsReScanRealStartIndex; // include idle scanned
//debugln(['=== IDLE SendHighlightChanged ',StartIndex,' - ',EndIndex]);
    CurrentRanges.ClearReScanNeeded;
  // Invalidate one line above, since folds can change depending on next line
    CurrentLines.SendHighlightChanged(StartIndex - 1, EndIndex - StartIndex + 1);
    exit;
  end
  else begin
    CurrentRanges.AdjustReScanStart(EndIndex);
    Result := True;
  end;
end;

function TSynCustomHighlighter.NeedScan: Boolean;
begin
  Result := (CurrentRanges.NeedsReScanStartIndex >= 0);
end;

function TSynCustomHighlighter.PerformScan(StartIndex, EndIndex: Integer;
  ForceEndIndex: Boolean = False): Integer;
var
  c: Integer;
begin
  Result := StartIndex;
  c := CurrentLines.Count;
  if (c = 0) or (Result >= c) then
    exit;
  StartAtLineIndex(Result);
  NextToEol;
  while UpdateRangeInfoAtLine(Result) or (Result <= EndIndex) do begin
    inc(Result);
    if (Result = c) or (ForceEndIndex and (Result > EndIndex)) then
      break;
    ContinueNextLine;
    NextToEol;
  end;
end;

procedure TSynCustomHighlighter.ScanAllRanges;
begin
  CurrentRanges.InvalidateAll;
  ScanRanges;
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

procedure TSynCustomHighlighter.SetCurrentLines(const AValue: TSynEditStringsBase);
begin
  if AValue = FCurrentLines then
    exit;
  FCurrentLines := AValue;
  if FCurrentLines <> nil
  then FCurrentRanges := TSynHighlighterRangeList(AValue.Ranges[GetRangeIdentifier])
  else FCurrentRanges := nil;
  DoCurrentLinesChanged;
end;

procedure TSynCustomHighlighter.DoCurrentLinesChanged;
begin
  //
end;

procedure TSynCustomHighlighter.AttachToLines(Lines: TSynEditStringsBase);
var
  r: TSynHighlighterRangeList;
begin
  r := TSynHighlighterRangeList(Lines.Ranges[GetRangeIdentifier]);
  if assigned(r) then
    r.IncRefCount
  else begin
    FKnownLines.Add(Lines);
    r := CreateRangeList(Lines);
    Lines.Ranges[GetRangeIdentifier] := r;
    r.InvalidateAll;
  end;
  AfterAttachedToRangeList(r); // RefCount already increased
  FCurrentLines := nil;
end;

procedure TSynCustomHighlighter.DetachFromLines(Lines: TSynEditStringsBase);
var
  r: TSynHighlighterRangeList;
begin
  r := TSynHighlighterRangeList(Lines.Ranges[GetRangeIdentifier]);
  if not assigned(r) then exit;
  r.DecRefCount;
  BeforeDetachedFromRangeList(r); // RefCount already decreased
  if r.RefCount = 0 then begin
    Lines.Ranges[GetRangeIdentifier] := nil;
    if FCurrentRanges = r then begin
      FCurrentRanges := nil;
      FCurrentLines := nil;
    end;
    r.Free;
  end;
  FKnownLines.Remove(Lines);
end;

procedure TSynCustomHighlighter.SetDrawDividerLevel(const AValue: Integer);
begin
  if FDrawDividerLevel = AValue then exit;
  FDrawDividerLevel := AValue;
  //DefHighlightChange(Self);
end;

function TSynCustomHighlighter.GetKnownRanges(Index: Integer): TSynHighlighterRangeList;
begin
  Result := TSynHighlighterRangeList(KnownLines[Index].Ranges[GetRangeIdentifier]);
end;

function TSynCustomHighlighter.GetDrawDivider(Index: integer): TSynDividerDrawConfigSetting;
begin
  result := SynEmptyDividerDrawConfigSetting;
end;

function TSynCustomHighlighter.GetDividerDrawConfig(Index: Integer): TSynDividerDrawConfig;
begin
  Result := nil;
end;

function TSynCustomHighlighter.GetDividerDrawConfigCount: Integer;
begin
  Result := 0;
end;

{ TSynHighlighterRangeList }

function TSynHighlighterRangeList.GetRange(Index: Integer): Pointer;
begin
  Result := Pointer(ItemPointer[Index]^);
end;

function TSynHighlighterRangeList.GetNeedsReScanRealStartIndex: Integer;
begin
  Result := FNeedsReScanRealStartIndex;
  if Result < 0 then
    Result := FNeedsReScanStartIndex;
end;

procedure TSynHighlighterRangeList.SetRange(Index: Integer; const AValue: Pointer);
begin
  Pointer(ItemPointer[Index]^) := AValue;
end;

procedure TSynHighlighterRangeList.LineTextChanged(AIndex: Integer; ACount: Integer = 1);
begin
  if FNeedsReScanStartIndex < 0 then begin
    FNeedsReScanStartIndex := AIndex;
    FNeedsReScanEndIndex := AIndex + ACount - 1;
  end
  else if AIndex < FNeedsReScanStartIndex then
    FNeedsReScanStartIndex := AIndex
  else if AIndex + ACount - 1 > FNeedsReScanEndIndex then
    FNeedsReScanEndIndex := AIndex + ACount - 1;
end;

procedure TSynHighlighterRangeList.InsertedLines(AIndex, ACount: Integer);
begin
  if (FNeedsReScanStartIndex < 0) or (AIndex < FNeedsReScanStartIndex) then
    FNeedsReScanStartIndex := AIndex;

  if (FNeedsReScanEndIndex < 0) or (FNeedsReScanEndIndex < AIndex) then
    FNeedsReScanEndIndex := AIndex + ACount
  else
    FNeedsReScanEndIndex := FNeedsReScanEndIndex + ACount
end;

procedure TSynHighlighterRangeList.DeletedLines(AIndex, ACount: Integer);
begin
  if AIndex >= Count then exit;
  if (FNeedsReScanStartIndex < 0) or (AIndex < FNeedsReScanStartIndex) then
    FNeedsReScanStartIndex := AIndex;
  if (FNeedsReScanEndIndex < 0) or (FNeedsReScanEndIndex < AIndex) then
    FNeedsReScanEndIndex := AIndex;
end;

procedure TSynHighlighterRangeList.ClearReScanNeeded;
begin
  FNeedsReScanStartIndex := -1;
  FNeedsReScanEndIndex := -1;
  FNeedsReScanRealStartIndex := -1;
end;

procedure TSynHighlighterRangeList.AdjustReScanStart(ANewStart: Integer);
begin
  if FNeedsReScanRealStartIndex < 0 then
    FNeedsReScanRealStartIndex := FNeedsReScanStartIndex;
  FNeedsReScanStartIndex := ANewStart;
end;

procedure TSynHighlighterRangeList.InvalidateAll;
begin
  FNeedsReScanStartIndex := 0;
  FNeedsReScanEndIndex := Count - 1;
end;

constructor TSynHighlighterRangeList.Create;
begin
  Inherited;
  ItemSize := SizeOf(Pointer);
  FRefCount := 1;
  ClearReScanNeeded;
end;

procedure TSynHighlighterRangeList.IncRefCount;
begin
  inc(FRefCount);
end;

procedure TSynHighlighterRangeList.DecRefCount;
begin
  dec(FRefCount);
end;

{ TSynDividerDrawConfig }

function TSynDividerDrawConfig.GetNestColor: TColor;
begin
  Result := FNestSetting.Color;
end;

function TSynDividerDrawConfig.GetTopColor: TColor;
begin
  Result := FTopSetting.Color;
end;

procedure TSynDividerDrawConfig.SetNestColor(const AValue: TColor);
begin
  if AValue = FNestSetting.Color then exit;
  FNestSetting.Color := AValue;
  Changed;
end;

procedure TSynDividerDrawConfig.SetTopColor(const AValue: TColor);
begin
  if AValue = FTopSetting.Color then exit;
  FTopSetting.Color := AValue;
  Changed;
end;

function TSynDividerDrawConfig.GetMaxDrawDepth: Integer;
begin
  Result := FDepth;
end;

procedure TSynDividerDrawConfig.SetMaxDrawDepth(AValue: Integer);
begin
  if FDepth = AValue then exit;
  FDepth := AValue;
  Changed;
end;

procedure TSynDividerDrawConfig.Changed;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

constructor TSynDividerDrawConfig.Create;
begin
  inherited;
  FDepth := 0;
  FTopSetting.Color := clDefault;
  FNestSetting.Color := clDefault;
end;

procedure TSynDividerDrawConfig.Assign(Src: TSynDividerDrawConfig);
begin
  fOnChange := src.fOnChange;
  FDepth := Src.FDepth;
end;

initialization
  G_PlaceableHighlighters := TSynHighlighterList.Create;
finalization
  G_PlaceableHighlighters.Free;
  G_PlaceableHighlighters := nil;
end.


