{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
 *****************************************************************************
}
unit LazEditTextAttributes;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics;

type
  // TODO: TLazEditDisplayTokenBound is not yet supporting wrapped text - The Physical value may change

  { TLazEditDisplayTokenBound }

  TLazEditDisplayTokenBound = record
    Physical: Integer;      // 1 based - May be in middle of char
    Logical: Integer;       // 1 based
    Offset: Integer;        // default 0. MultiWidth (e.g. Tab), if token starts in the middle of char

    function HasValue: boolean; inline;
    function Init(APhys, ALog: Integer; AnOffs: Integer = 0): boolean; inline;
  end;



  //TLazTextAttributeFeature = (
  //  lafForeColor, lafBackColor,
  //  lafFrameColor,
  //  lafAlpha, lafPrior,
  //  lafStyle, lafStyleMask
  //);

  TLazTextAttrLineStyle = (
    slsSolid,  // PS_SOLID pen
    slsDashed, // PS_DASH pen
    slsDotted, // PS_DOT
    slsWaved   // solid wave
  );

  TLazTextAttrFrameEdges = (
    sfeNone,
    sfeAround,      // frame around
    sfeBottom,      // bottom part of the frame
    sfeLeft         // left part of the frame
  );

  TLazTextAttrBorderSide = (
    bsLeft,
    bsTop,
    bsRight,
    bsBottom
  );
  TLazTextAttrBorderSides = set of TLazTextAttrBorderSide;

const
  LazTextFrameEdgeToSides: array [TLazTextAttrFrameEdges] of TLazTextAttrBorderSides =
  ( [],                                      // sfeNone
    [bsLeft, bsTop, bsRight, bsBottom],      // sfeAround
    [bsBottom],                              // sfeBottom
    [bsLeft]                                 // sfeLeft
  );

  LazTextFrameEdgePriorities: array [TLazTextAttrFrameEdges] of integer =
  ( 0,    // sfeNone
    1,   // sfeAround
    2,   // sfeBottom
    2    // sfeLeft
  );

type

  { TLazCustomEditTextAttribute }

  TLazCustomEditTextAttribute = class(TPersistent)
  protected type
    TLazTextAttributeColor = (lacForeColor, lacBackColor, lacFrameColor);
  private
    // 0 or -1 start/end before/after line // 1 first char
    FStartX, FEndX: TLazEditDisplayTokenBound;

    FColors:   array[TLazTextAttributeColor] of TColor;
    FFrameEdges: TLazTextAttrFrameEdges;
    FFrameStyle: TLazTextAttrLineStyle;
    FStyle: TFontStyles;

    FUpdateCount: integer;
    FHasUpdates: Boolean;

  protected
    function GetColor(AnIndex: TLazTextAttributeColor): TColor; inline;
    function GetAlpha({%H-}AnIndex: TLazTextAttributeColor): byte; virtual;
    function GetPriority({%H-}AnIndex: TLazTextAttributeColor): integer; virtual;
    function GetFrameStyle: TLazTextAttrLineStyle; inline;
    function GetFrameEdges: TLazTextAttrFrameEdges; inline;
    function GetFrameSideColors({%H-}Side: TLazTextAttrBorderSide): TColor; virtual;
    function GetFrameSidePriority({%H-}Side: TLazTextAttrBorderSide): integer; virtual;
    function GetFrameSideStyles({%H-}Side: TLazTextAttrBorderSide): TLazTextAttrLineStyle; virtual;
    function GetStyle: TFontStyles; inline;
    function GetStyleMask: TFontStyles; virtual;
    function GetStylePriority({%H-}AnIndex: TFontStyle): integer; virtual;
    procedure SetColor(AnIndex: TLazTextAttributeColor; AValue: TColor);
    procedure SetAlpha({%H-}AnIndex: TLazTextAttributeColor; {%H-}AValue: byte); virtual;
    procedure SetPriority({%H-}AnIndex: TLazTextAttributeColor; {%H-}AValue: integer); virtual;
    procedure SetFrameStyle(AValue: TLazTextAttrLineStyle);
    procedure SetFrameEdges(AValue: TLazTextAttrFrameEdges);
    procedure SetStyle(AValue: TFontStyles);
    procedure SetStyleMask({%H-}AValue: TFontStyles); virtual;
    procedure SetStylePriority({%H-}AnIndex: TFontStyle; {%H-}AValue: integer); virtual;

  protected
    procedure Changed;
    procedure DoChanged; virtual;
    procedure Init; virtual;
    procedure DoClear; virtual;
    procedure AssignColorsFrom(ASource: TLazCustomEditTextAttribute); virtual;
    procedure AssignFrom(ASource: TLazCustomEditTextAttribute); virtual;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
    procedure AssignColors(ASource: TLazCustomEditTextAttribute);
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    function  IsEnabled: boolean; virtual;
    procedure SetAllPriorities(APriority: integer); virtual;
    // boundaries of the frame
    procedure SetFrameBoundsPhys(AStart, AEnd: Integer);
    procedure SetFrameBoundsLog(AStart, AEnd: Integer; AStartOffs: Integer = 0; AEndOffs: Integer = 0);
    property StartX: TLazEditDisplayTokenBound read FStartX write FStartX;
    property EndX: TLazEditDisplayTokenBound read FEndX write FEndX;
  public
    property Foreground: TColor index lacForeColor read GetColor write SetColor;
    property Background: TColor index lacBackColor read GetColor write SetColor;
    property FrameColor: TColor index lacFrameColor read GetColor write SetColor;

    property BackAlpha:  byte index lacBackColor read GetAlpha write SetAlpha;
    property ForeAlpha:  byte index lacForeColor read GetAlpha write SetAlpha;
    property FrameAlpha: byte index lacFrameColor read GetAlpha write SetAlpha;

    property ForePriority:  integer index lacForeColor read GetPriority write SetPriority;
    property BackPriority:  integer index lacBackColor read GetPriority write SetPriority;
    property FramePriority: integer index lacFrameColor read GetPriority write SetPriority;

    property FrameStyle: TLazTextAttrLineStyle  read GetFrameStyle write SetFrameStyle;
    property FrameEdges: TLazTextAttrFrameEdges read GetFrameEdges write SetFrameEdges;

    property FrameSidePriority[Side: TLazTextAttrBorderSide]: integer read GetFrameSidePriority;
    property FrameSideColors[Side: TLazTextAttrBorderSide]: TColor read GetFrameSideColors;
    property FrameSideStyles[Side: TLazTextAttrBorderSide]: TLazTextAttrLineStyle read GetFrameSideStyles;

    property Style: TFontStyles     read GetStyle write SetStyle;
    property StyleMask: TFontStyles read GetStyleMask write SetStyleMask;
    // FStyle = [],       FStyleMask = []        ==> no modification
    // FStyle = [fsBold], FStyleMask = []        ==> invert fsBold
    // FStyle = [],       FStyleMask = [fsBold]  ==> clear  fsBold
    // FStyle = [fsBold], FStyleMask = [fsBold]  ==> set    fsBold

    property StylePriority[AnIndex: TFontStyle]: integer read GetStylePriority write SetStylePriority;
    property BoldPriority:      integer index fsBold read GetStylePriority write SetStylePriority;
    property ItalicPriority:    integer index fsItalic read GetStylePriority write SetStylePriority;
    property UnderlinePriority: integer index fsUnderline read GetStylePriority write SetStylePriority;
    property StrikeOutPriority: integer index fsStrikeOut read GetStylePriority write SetStylePriority;
  end;

  { TLazEditTextAttribute }

  TLazEditTextAttribute = class(TLazCustomEditTextAttribute)
  private
    FOnChange: TNotifyEvent;
    FStoredName, FFixedCaption: string;
    FCaption: PString;

    FPriority: array[TLazTextAttributeColor] of Integer;
    FStylePriority: array[TFontStyle] of Integer;

    FDefaultColors:   array[TLazTextAttributeColor] of TColor;
    FDefaultPriority: array[TLazTextAttributeColor] of Integer;
    FDefaultFrameEdges: TLazTextAttrFrameEdges;
    FDefaultFrameStyle: TLazTextAttrLineStyle;
    FDefaultStyle: TFontStyles;
    FDefaultStylePriority: array[TFontStyle] of Integer;

  protected
    function GetPriority(AnIndex: TLazTextAttributeColor): integer; override;
    function GetFrameSidePriority(Side: TLazTextAttrBorderSide): integer; override;
    function GetStylePriority(AnIndex: TFontStyle): integer; override;
    procedure SetPriority(AnIndex: TLazTextAttributeColor; AValue: integer); override;
    procedure SetStylePriority(AnIndex: TFontStyle; AValue: integer); override;

    function GetColorStored(AnIndex: TLazTextAttributeColor): Boolean;
    function GetPriorityStored(AnIndex: TLazTextAttributeColor): Boolean;
    function GetFrameStyleStored: Boolean;
    function GetFrameEdgesStored: Boolean;
    function GetStyleStored: Boolean;
    function GetStylePriorityStored(AnIndex: TFontStyle): Boolean;

    procedure DoChanged; override;
    procedure Init; override;
    procedure DoClearThis; inline;
    procedure DoClear; override;
    procedure AssignColorsFrom(ASource: TLazCustomEditTextAttribute); override;
    procedure AssignFrom(ASource: TLazCustomEditTextAttribute); override;
  public
    constructor Create;
    constructor Create(ACaption: string; AStoredName: String = '');
    constructor Create(ACaption: PString; AStoredName: String = ''); // e.g. pointer to resourcestring. (Must be global var/const)
    procedure SetCaption(ACaption: String);

    procedure InternalSaveDefaultValues; virtual;
    procedure SetAllPriorities(APriority: integer); override;

    property Caption: PString read FCaption;                        // will never be nil
    property StoredName: string read FStoredName write FStoredName; // name for storage (e.g. xml)

  published
    property Foreground stored GetColorStored;
    property Background stored GetColorStored;
    property FrameColor stored GetColorStored;

    property ForePriority  stored GetPriorityStored;
    property BackPriority  stored GetPriorityStored;
    property FramePriority stored GetPriorityStored;

    property FrameStyle stored GetFrameStyleStored;
    property FrameEdges stored GetFrameEdgesStored;

    property Style stored GetStyleStored;
    property BoldPriority      stored GetStylePriorityStored;
    property ItalicPriority    stored GetStylePriorityStored;
    property UnderlinePriority stored GetStylePriorityStored;
    property StrikeOutPriority stored GetStylePriorityStored;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TLazEditTextAttributeModifier }

  TLazEditTextAttributeModifier = class(TLazEditTextAttribute)
  private
    FAlpha:    array[TLazTextAttributeColor] of Byte;
    FStyleMask: TFontStyles;

    FDefaultAlpha:    array[TLazTextAttributeColor] of Byte;
    FDefaultStyleMask: TFontStyles;
  protected
    function GetAlpha(AnIndex: TLazTextAttributeColor): byte; override;
    function GetStyleMask: TFontStyles; override;
    procedure SetAlpha(AnIndex: TLazTextAttributeColor; AValue: byte); override;
    procedure SetStyleMask(AValue: TFontStyles); override;

    function GetAlphaStored(AnIndex: TLazTextAttributeColor): Boolean;
    function GetStyleMaskStored: Boolean;

    procedure DoClearThis; reintroduce; inline;
    procedure DoClear; override;
    procedure AssignColorsFrom(ASource: TLazCustomEditTextAttribute); override;
  public
    function IsEnabled: boolean; override;
    procedure InternalSaveDefaultValues; override;

  published
    property BackAlpha  stored GetAlphaStored;
    property ForeAlpha  stored GetAlphaStored;
    property FrameAlpha stored GetAlphaStored;

    property StyleMask stored GetStyleMaskStored;
  end;

  TLazEditCustomHighlighter = class(TComponent)
  protected
    procedure AddAttribute(AAttrib: TLazEditTextAttribute); virtual; abstract;
    procedure RemoveAttribute(AAttrib: TLazEditTextAttribute); virtual; abstract;
  end;

  { TLazEditTextAttributeModifierCollectionItem }

  TLazEditTextAttributeModifierCollectionItem = class(TCollectionItem)
  private
    FAttribute: TLazEditTextAttributeModifier;
    procedure SetAttribute(AValue: TLazEditTextAttributeModifier);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Attribute: TLazEditTextAttributeModifier read FAttribute write SetAttribute;
  end;

  { TLazEditTextAttributeModifierCollection }

  TLazEditTextAttributeModifierCollection = class(TCollection)
  private
    FBaseName: string;
    FBaseStoredName: string;
    FOnAttributeChange: TNotifyEvent;
    FOwner: TLazEditCustomHighlighter;
    procedure DoAttribChaged(Sender: TObject);
    function GetAttrib(Index: Integer): TLazEditTextAttributeModifier;
    function GetItem(Index: Integer): TLazEditTextAttributeModifierCollectionItem;
    procedure SetAttribs(Index: Integer; AValue: TLazEditTextAttributeModifier);
    procedure SetBaseName(AValue: string);
    procedure SetBaseStoredName(AValue: string);
    procedure SetItem(Index: Integer; Value: TLazEditTextAttributeModifierCollectionItem);
    procedure ResetNames;
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AnOwner: TLazEditCustomHighlighter);
    function Add: TLazEditTextAttributeModifierCollectionItem;
  public
    property BaseName: string read FBaseName write SetBaseName;
    property BaseStoredName: string read FBaseStoredName write SetBaseStoredName;

    property Items[Index: Integer]: TLazEditTextAttributeModifierCollectionItem read GetItem
      write SetItem; default;
    property Attribs[Index: Integer]: TLazEditTextAttributeModifier read GetAttrib
      write SetAttribs;
    property OnAttributeChange: TNotifyEvent read FOnAttributeChange write FOnAttributeChange;
  end;

implementation

{ TLazEditDisplayTokenBound }

function TLazEditDisplayTokenBound.HasValue: boolean;
begin
  Result := (Physical > 0) or (Logical > 0);
end;

function TLazEditDisplayTokenBound.Init(APhys, ALog: Integer; AnOffs: Integer): boolean;
begin
  Physical := APhys;
  Logical := ALog;
  Offset := AnOffs;
end;

{ TLazCustomEditTextAttribute }

function TLazCustomEditTextAttribute.GetColor(AnIndex: TLazTextAttributeColor): TColor;
begin
  Result := FColors[AnIndex];
end;

function TLazCustomEditTextAttribute.GetAlpha(AnIndex: TLazTextAttributeColor): byte;
begin
  Result := 0;
end;

function TLazCustomEditTextAttribute.GetPriority(AnIndex: TLazTextAttributeColor): integer;
begin
  Result := 0;
end;

function TLazCustomEditTextAttribute.GetFrameStyle: TLazTextAttrLineStyle;
begin
  Result := FFrameStyle;
end;

function TLazCustomEditTextAttribute.GetFrameEdges: TLazTextAttrFrameEdges;
begin
  Result := FFrameEdges;
end;

function TLazCustomEditTextAttribute.GetFrameSideColors(Side: TLazTextAttrBorderSide): TColor;
begin
  Result := FColors[lacFrameColor];
end;

function TLazCustomEditTextAttribute.GetFrameSidePriority(Side: TLazTextAttrBorderSide): integer;
begin
  Result := 0;
end;

function TLazCustomEditTextAttribute.GetFrameSideStyles(Side: TLazTextAttrBorderSide
  ): TLazTextAttrLineStyle;
begin
  Result := FFrameStyle;
end;

function TLazCustomEditTextAttribute.GetStyle: TFontStyles;
begin
  Result := FStyle;
end;

function TLazCustomEditTextAttribute.GetStyleMask: TFontStyles;
begin
  Result := [low(TFontStyle)..High(TFontStyle)];
end;

function TLazCustomEditTextAttribute.GetStylePriority(AnIndex: TFontStyle): integer;
begin
  Result := 0;
end;

procedure TLazCustomEditTextAttribute.SetColor(AnIndex: TLazTextAttributeColor; AValue: TColor);
begin
  if FColors[AnIndex] = AValue then
    exit;
  FColors[AnIndex] := AValue;
  Changed;
end;

procedure TLazCustomEditTextAttribute.SetAlpha(AnIndex: TLazTextAttributeColor; AValue: byte);
begin
  raise exception.Create('abstract');
end;

procedure TLazCustomEditTextAttribute.SetPriority(AnIndex: TLazTextAttributeColor; AValue: integer
  );
begin
  assert(false, 'TLazCustomEditTextAttribute.SetPriority: abstract');
  //raise exception.Create('abstract');
end;

procedure TLazCustomEditTextAttribute.SetFrameStyle(AValue: TLazTextAttrLineStyle);
begin
  if FFrameStyle = AValue then
    exit;
  FFrameStyle := AValue;
  Changed;
end;

procedure TLazCustomEditTextAttribute.SetFrameEdges(AValue: TLazTextAttrFrameEdges);
begin
  if FFrameEdges = AValue then
    exit;
  FFrameEdges := AValue;
  Changed;
end;

procedure TLazCustomEditTextAttribute.SetStyle(AValue: TFontStyles);
begin
  if FStyle = AValue then
    exit;
  FStyle := AValue;
  Changed;
end;

procedure TLazCustomEditTextAttribute.SetStyleMask(AValue: TFontStyles);
begin
  assert(false, 'TLazCustomEditTextAttribute.SetStyleMask: abstract');
  //raise exception.Create('abstract');
end;

procedure TLazCustomEditTextAttribute.SetStylePriority(AnIndex: TFontStyle; AValue: integer);
begin
  assert(false, 'TLazCustomEditTextAttribute.SetStylePriority: abstract');
  //raise exception.Create('abstract');
end;

procedure TLazCustomEditTextAttribute.Changed;
begin
  FHasUpdates := FUpdateCount > 0;
  if FHasUpdates then
    exit;
  DoChanged;
end;

procedure TLazCustomEditTextAttribute.DoChanged;
begin
  //
end;

procedure TLazCustomEditTextAttribute.Init;
begin
  Clear;
end;

procedure TLazCustomEditTextAttribute.DoClear;
var
  c: TLazTextAttributeColor;
begin
  for c := low(TLazTextAttributeColor) to high(TLazTextAttributeColor) do
    FColors[c] := clNone;
  FFrameEdges := sfeAround;
  FFrameStyle := slsSolid;
  FStyle      := [];

  FStartX.Physical := -1;
  FEndX.Physical   := -1;
  FStartX.Logical  := -1;
  FEndX.Logical    := -1;
  FStartX.Offset   := 0;
  FEndX.Offset     := 0;
end;

procedure TLazCustomEditTextAttribute.AssignColorsFrom(ASource: TLazCustomEditTextAttribute);
begin
  FColors     := ASource.FColors;
  FFrameEdges := ASource.FFrameEdges;
  FFrameStyle := ASource.FFrameStyle;
  FStyle      := ASource.FStyle;
end;

procedure TLazCustomEditTextAttribute.AssignFrom(ASource: TLazCustomEditTextAttribute);
begin
  FStartX     := ASource.FStartX;
  FEndX       := ASource.FEndX;
end;

constructor TLazCustomEditTextAttribute.Create;
begin
  Init;
end;

procedure TLazCustomEditTextAttribute.Assign(ASource: TPersistent);
var
  Src: TLazCustomEditTextAttribute absolute ASource;
begin
  if ASource is TLazCustomEditTextAttribute then begin
    BeginUpdate;
    AssignColorsFrom(Src);
    AssignFrom(Src);
    Changed;
    EndUpdate;
  end
  else
    inherited Assign(ASource);
end;

procedure TLazCustomEditTextAttribute.AssignColors(ASource: TLazCustomEditTextAttribute);
begin
  BeginUpdate;
  AssignColorsFrom(ASource);
  Changed;
  EndUpdate;
end;

procedure TLazCustomEditTextAttribute.Clear;
begin
  BeginUpdate;
  DoClear;
  Changed;
  EndUpdate;
end;

procedure TLazCustomEditTextAttribute.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TLazCustomEditTextAttribute.EndUpdate;
begin
  if FUpdateCount > 0 then begin
    dec(FUpdateCount);
    if FHasUpdates and (FUpdateCount = 0) then
      Changed;
  end;
end;

function TLazCustomEditTextAttribute.IsEnabled: boolean;
var
  c: TLazTextAttributeColor;
begin
  Result := True;
  if FStyle <> [] then
    exit;
  for c := low(TLazTextAttributeColor) to high(TLazTextAttributeColor) do
    if FColors[c] <> clNone then
      exit;
  Result := False;
end;

procedure TLazCustomEditTextAttribute.SetAllPriorities(APriority: integer);
begin
  assert(false, 'TLazCustomEditTextAttribute.SetAllPriorities: abstract');
  //raise exception.Create('abstract');
end;

procedure TLazCustomEditTextAttribute.SetFrameBoundsPhys(AStart, AEnd: Integer);
begin
  FStartX.Physical := AStart;
  FEndX.Physical   := AEnd;
  FStartX.Logical  := -1;
  FEndX.Logical    := -1;
  FStartX.Offset   := 0;
  FEndX.Offset     := 0;
end;

procedure TLazCustomEditTextAttribute.SetFrameBoundsLog(AStart, AEnd: Integer;
  AStartOffs: Integer; AEndOffs: Integer);
begin
  FStartX.Physical := -1;
  FEndX.Physical   := -1;
  FStartX.Logical  := AStart;
  FEndX.Logical    := AEnd;
  FStartX.Offset   := AStartOffs;
  FEndX.Offset     := AEndOffs;
end;

{ TLazEditTextAttribute }

function TLazEditTextAttribute.GetPriority(AnIndex: TLazTextAttributeColor): integer;
begin
  Result := FPriority[AnIndex];
end;

function TLazEditTextAttribute.GetFrameSidePriority(Side: TLazTextAttrBorderSide): integer;
begin
  Result := FPriority[lacFrameColor];
end;

function TLazEditTextAttribute.GetStylePriority(AnIndex: TFontStyle): integer;
begin
  Result := inherited GetStylePriority(AnIndex);
end;

procedure TLazEditTextAttribute.SetPriority(AnIndex: TLazTextAttributeColor; AValue: integer);
begin
  if FPriority[AnIndex] = AValue then
    exit;
  FPriority[AnIndex] := AValue;
  Changed;
end;

procedure TLazEditTextAttribute.SetStylePriority(AnIndex: TFontStyle; AValue: integer);
begin
  if FStylePriority[AnIndex] = AValue then
    exit;
  FStylePriority[AnIndex] := AValue;
  Changed;
end;

function TLazEditTextAttribute.GetColorStored(AnIndex: TLazTextAttributeColor): Boolean;
begin
  Result := FColors[AnIndex] <> FDefaultColors[AnIndex];
end;

function TLazEditTextAttribute.GetPriorityStored(AnIndex: TLazTextAttributeColor): Boolean;
begin
  Result := FPriority[AnIndex] <> FDefaultPriority[AnIndex];
end;

function TLazEditTextAttribute.GetFrameStyleStored: Boolean;
begin
  Result := FFrameStyle <> FDefaultFrameStyle;
end;

function TLazEditTextAttribute.GetFrameEdgesStored: Boolean;
begin
  Result := FFrameEdges <> FDefaultFrameEdges;
end;

function TLazEditTextAttribute.GetStyleStored: Boolean;
begin
  Result := FStyle <> FDefaultStyle;
end;

function TLazEditTextAttribute.GetStylePriorityStored(AnIndex: TFontStyle): Boolean;
begin
  Result := FStylePriority[AnIndex] <> FDefaultStylePriority[AnIndex];
end;

procedure TLazEditTextAttribute.SetCaption(ACaption: String);
begin
  FFixedCaption := ACaption;
  FCaption := @FFixedCaption;
end;

procedure TLazEditTextAttribute.DoChanged;
begin
  inherited DoChanged;
  if FOnChange <> nil then
    FOnChange(Self);
end;

procedure TLazEditTextAttribute.Init;
begin
  inherited Init;
  InternalSaveDefaultValues;
end;

procedure TLazEditTextAttribute.DoClearThis;
var
  c: TLazTextAttributeColor;
  f: TFontStyle;
begin
  for c := low(TLazTextAttributeColor) to high(TLazTextAttributeColor) do
    FPriority[c] := 0;
  for f := low(TFontStyle) to high(TFontStyle) do
    FStylePriority[f] := 0;
end;

procedure TLazEditTextAttribute.DoClear;
begin
  inherited DoClear;
  DoClearThis;
end;

procedure TLazEditTextAttribute.AssignColorsFrom(ASource: TLazCustomEditTextAttribute);
var
  Source: TLazEditTextAttribute absolute ASource;
begin
  inherited AssignColorsFrom(ASource);
  if ASource is TLazEditTextAttribute then begin
    FPriority      := Source.FPriority;
    FStylePriority := Source.FStylePriority;
  end
  else
    DoClearThis;
end;

procedure TLazEditTextAttribute.AssignFrom(ASource: TLazCustomEditTextAttribute);
var
  Source: TLazEditTextAttribute absolute ASource;
begin
  inherited AssignFrom(ASource);
  if ASource is TLazEditTextAttribute then begin
    FStoredName    := Source.FStoredName;
    FFixedCaption  := Source.FFixedCaption;
    FCaption       := Source.FCaption;
  end;
end;

constructor TLazEditTextAttribute.Create;
begin
  FCaption := @FFixedCaption;
  inherited Create;
end;

constructor TLazEditTextAttribute.Create(ACaption: string; AStoredName: String);
begin
  FFixedCaption := ACaption;
  Create(@FFixedCaption, AStoredName);
end;

constructor TLazEditTextAttribute.Create(ACaption: PString; AStoredName: String);
begin
  Create;
  if ACaption <> nil then
    FCaption := ACaption;

  FStoredName := AStoredName;
  if FStoredName = '' then
    FStoredName := FCaption^;
end;

procedure TLazEditTextAttribute.InternalSaveDefaultValues;
begin
  FDefaultColors        := FColors;
  FDefaultPriority      := FPriority;
  FDefaultFrameEdges    := FFrameEdges;
  FDefaultFrameStyle    := FFrameStyle;
  FDefaultStyle         := FStyle;
  FDefaultStylePriority := FStylePriority;
end;

procedure TLazEditTextAttribute.SetAllPriorities(APriority: integer);
var
  c: TLazTextAttributeColor;
  f: TFontStyle;
begin
  for c := low(TLazTextAttributeColor) to high(TLazTextAttributeColor) do
    FPriority[c] := APriority;
  for f := low(TFontStyle) to high(TFontStyle) do
    FStylePriority[f] := APriority;
end;

{ TLazEditTextAttributeModifier }

function TLazEditTextAttributeModifier.GetAlpha(AnIndex: TLazTextAttributeColor): byte;
begin
  Result := FAlpha[AnIndex];
end;

function TLazEditTextAttributeModifier.GetStyleMask: TFontStyles;
begin
  Result := FStyleMask;
end;

procedure TLazEditTextAttributeModifier.SetAlpha(AnIndex: TLazTextAttributeColor; AValue: byte);
begin
  if FAlpha[AnIndex] = AValue then
    exit;
  FAlpha[AnIndex] := AValue;
  Changed;
end;

procedure TLazEditTextAttributeModifier.SetStyleMask(AValue: TFontStyles);
begin
  if FStyleMask = AValue then
    exit;
  FStyleMask := AValue;
  Changed;
end;

function TLazEditTextAttributeModifier.GetAlphaStored(AnIndex: TLazTextAttributeColor): Boolean;
begin
  Result := FAlpha[AnIndex] <> FDefaultAlpha[AnIndex];
end;

function TLazEditTextAttributeModifier.GetStyleMaskStored: Boolean;
begin
  Result := FStyleMask <> FDefaultStyleMask;
end;

procedure TLazEditTextAttributeModifier.DoClearThis;
var
  c: TLazTextAttributeColor;
begin
  for c := low(TLazTextAttributeColor) to high(TLazTextAttributeColor) do
    FAlpha[c] := 0;
  FStyleMask := [];
end;

procedure TLazEditTextAttributeModifier.DoClear;
begin
  inherited DoClear;
  DoClearThis;
end;

procedure TLazEditTextAttributeModifier.AssignColorsFrom(ASource: TLazCustomEditTextAttribute);
var
  Source: TLazEditTextAttributeModifier absolute ASource;
begin
  inherited AssignColorsFrom(ASource);
  if ASource is TLazEditTextAttributeModifier then begin
    FAlpha     := Source.FAlpha;
    FStyleMask := Source.FStyleMask;
  end
  else
    DoClearThis;
end;

function TLazEditTextAttributeModifier.IsEnabled: boolean;
begin
  Result := inherited IsEnabled;
  if Result then
    exit;
  Result := FStyleMask <> [];
end;

procedure TLazEditTextAttributeModifier.InternalSaveDefaultValues;
begin
  inherited InternalSaveDefaultValues;
  FDefaultAlpha         := FAlpha;
  FDefaultStyleMask     := FStyleMask;
end;

{ TLazEditTextAttributeModifierCollectionItem }

procedure TLazEditTextAttributeModifierCollectionItem.SetAttribute(
  AValue: TLazEditTextAttributeModifier);
begin
  FAttribute.Assign(AValue);
end;

constructor TLazEditTextAttributeModifierCollectionItem.Create(ACollection: TCollection);
begin
  FAttribute := TLazEditTextAttributeModifier.Create('', '');
  inherited Create(ACollection);
end;

destructor TLazEditTextAttributeModifierCollectionItem.Destroy;
begin
  inherited Destroy;
  FAttribute.Destroy;
end;

{ TLazEditTextAttributeModifierCollection }

function TLazEditTextAttributeModifierCollection.GetItem(Index: Integer
  ): TLazEditTextAttributeModifierCollectionItem;
begin
  Result := TLazEditTextAttributeModifierCollectionItem(inherited GetItem(Index));
end;

function TLazEditTextAttributeModifierCollection.GetAttrib(Index: Integer
  ): TLazEditTextAttributeModifier;
begin
  Result := Items[Index].Attribute;
end;

procedure TLazEditTextAttributeModifierCollection.DoAttribChaged(Sender: TObject);
begin
  if FOnAttributeChange <> nil then
    FOnAttributeChange(Sender);
end;

procedure TLazEditTextAttributeModifierCollection.SetAttribs(Index: Integer;
  AValue: TLazEditTextAttributeModifier);
begin
  Items[Index].Attribute := AValue;
end;

procedure TLazEditTextAttributeModifierCollection.SetBaseName(AValue: string);
begin
  if FBaseName = AValue then Exit;
  FBaseName := AValue;
  if Format(FBaseName, [9]) = FBaseName then
    FBaseName := FBaseName + ' %d';

  ResetNames;
end;

procedure TLazEditTextAttributeModifierCollection.SetBaseStoredName(AValue: string);
begin
  if FBaseStoredName = AValue then Exit;
  FBaseStoredName := AValue;
  if Format(FBaseStoredName, [9]) = FBaseStoredName then
    FBaseStoredName := FBaseStoredName + '_%d';

  ResetNames;
end;

procedure TLazEditTextAttributeModifierCollection.SetItem(Index: Integer;
  Value: TLazEditTextAttributeModifierCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TLazEditTextAttributeModifierCollection.ResetNames;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    Attribs[i].SetCaption(Format(FBaseName, [i]));
    Attribs[i].StoredName := Format(FBaseStoredName, [i]);
  end;
end;

procedure TLazEditTextAttributeModifierCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
var
  TheItem: TLazEditTextAttributeModifierCollectionItem absolute Item;
  i: Integer;
begin
  inherited Notify(Item, Action);
  case Action of
    cnAdded: begin
      FOwner.AddAttribute(TheItem.Attribute);
      TheItem.Attribute.OnChange := @DoAttribChaged;
    end;
    cnExtracting, cnDeleting: begin
      FOwner.RemoveAttribute(TheItem.Attribute);
    end;
  end;
  ResetNames;
end;

constructor TLazEditTextAttributeModifierCollection.Create(AnOwner: TLazEditCustomHighlighter);
begin
  FOwner := AnOwner;
  inherited Create(TLazEditTextAttributeModifierCollectionItem);
end;

function TLazEditTextAttributeModifierCollection.Add: TLazEditTextAttributeModifierCollectionItem;
begin
  Result := TLazEditTextAttributeModifierCollectionItem(inherited Add);
end;

end.

