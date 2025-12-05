{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit LazEditTextAttributes;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, Graphics, LazMethodList;

type
  // TODO: TLazEditDisplayTokenBound is not yet supporting wrapped text - The Physical value may change

  { TLazEditDisplayTokenBound }

  TLazEditDisplayTokenBound = record
    Physical: Integer;      // 1 based - May be in middle of char
    Logical: Integer;       // 1 based
    Offset: Integer;        // default 0. MultiWidth (e.g. Tab), if token starts in the middle of char

    function HasValue: boolean; inline;
    procedure Init(APhys, ALog: Integer; AnOffs: Integer = 0); inline;
  end;



  TLazTextAttributeFeature = (
    lafPastEOL       // color extends past eol
  );
  TLazTextAttributeFeatures = set of TLazTextAttributeFeature;

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
  strict private
    FSupportedFeatures: TLazTextAttributeFeatures;
  private
    // 0 or -1 start/end before/after line // 1 first char
    FStartX, FEndX: TLazEditDisplayTokenBound;

    FColors:   array[TLazTextAttributeColor] of TColor;
    FFrameEdges: TLazTextAttrFrameEdges;
    FFrameStyle: TLazTextAttrLineStyle;
    FStyle: TFontStyles;
    FFeatures: TLazTextAttributeFeatures;

    FUpdateCount: integer;
    FHasUpdates: Boolean;
  protected
    function GetColor(AnIndex: TLazTextAttributeColor): TColor; inline;
    function GetAlpha({%H-}AnIndex: TLazTextAttributeColor): byte; virtual;
    function GetPriority({%H-}AnIndex: TLazTextAttributeColor): integer; virtual;
    function GetFrameStyle: TLazTextAttrLineStyle; inline;
    function GetFrameEdges: TLazTextAttrFrameEdges; virtual;
    function GetFrameSideColors({%H-}Side: TLazTextAttrBorderSide): TColor; virtual;
    function GetFrameSideAlpha({%H-}Side: TLazTextAttrBorderSide): Byte; virtual;
    function GetFrameSidePriority({%H-}Side: TLazTextAttrBorderSide): integer; virtual;
    function GetFrameSideStyles({%H-}Side: TLazTextAttrBorderSide): TLazTextAttrLineStyle; virtual;
    function GetStyle: TFontStyles; inline;
    function GetStyleMask: TFontStyles; virtual;
    function GetStylePriority({%H-}AnIndex: TFontStyle): integer; virtual;
    function GetFeature({%H-}AnIndex: TLazTextAttributeFeature): Boolean;
    procedure SetColor(AnIndex: TLazTextAttributeColor; AValue: TColor);
    procedure SetAlpha({%H-}AnIndex: TLazTextAttributeColor; {%H-}AValue: byte); virtual;
    procedure SetPriority({%H-}AnIndex: TLazTextAttributeColor; {%H-}AValue: integer); virtual;
    procedure SetFrameStyle(AValue: TLazTextAttrLineStyle);
    procedure SetFrameEdges(AValue: TLazTextAttrFrameEdges);
    procedure SetEndX(AValue: TLazEditDisplayTokenBound); virtual;
    procedure SetStartX(AValue: TLazEditDisplayTokenBound); virtual;
    procedure SetStyle(AValue: TFontStyles);
    procedure SetStyleMask({%H-}AValue: TFontStyles); virtual;
    procedure SetStylePriority({%H-}AnIndex: TFontStyle; {%H-}AValue: integer); virtual;
    procedure SetFeature(AnIndex: TLazTextAttributeFeature; AValue: Boolean);
    procedure SetFeatures(AValue: TLazTextAttributeFeatures);
  protected
    procedure Changed;
    procedure DoChanged; virtual;
    procedure Init; virtual;
    procedure DoClear; virtual;
    procedure AssignSupportedFeaturesFrom(ASource: TLazCustomEditTextAttribute); // Not called by assign, must be called explicitly
    procedure AssignColorsFrom(ASource: TLazCustomEditTextAttribute); virtual;
    procedure AssignFrom(ASource: TLazCustomEditTextAttribute); virtual;
    function  DefaultSupportedFeatures: TLazTextAttributeFeatures; virtual;

    property ColorValue [AnIndex: TLazTextAttributeColor]: TColor read GetColor write SetColor;
    property ColorAlpha [AnIndex: TLazTextAttributeColor]: byte read GetAlpha;
    property ColorPriority [AnIndex: TLazTextAttributeColor]: integer read GetPriority;

  public
    constructor Create; overload;
    constructor Create(ASupportedFeatures: TLazTextAttributeFeatures); overload;
    // Assign does not change SupportedFeatures
    procedure Assign(ASource: TPersistent); override;
    procedure AssignColors(ASource: TLazCustomEditTextAttribute);
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    function  IsEnabled: boolean; virtual;
    procedure UpdateSupportedFeatures(AnAddSupportedFeatures, ARemoveSupportedFeatures: TLazTextAttributeFeatures);
    procedure SetAllPriorities(APriority: integer); virtual;
    // boundaries of the frame
    procedure SetFrameBoundsPhys(AStart, AEnd: Integer); inline;
    procedure SetFrameBoundsLog(AStart, AEnd: Integer; AStartOffs: Integer = 0; AEndOffs: Integer = 0); inline;
    property StartX: TLazEditDisplayTokenBound read FStartX write SetStartX;
    property EndX: TLazEditDisplayTokenBound read FEndX write SetEndX;

    property SupportedFeatures: TLazTextAttributeFeatures read FSupportedFeatures;
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
    property FrameSideAlpha[Side: TLazTextAttrBorderSide]: Byte read GetFrameSideAlpha;
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

    property Features: TLazTextAttributeFeatures read FFeatures write SetFeatures;

    property ExtendPastEol: boolean index lafPastEOL read GetFeature write SetFeature;
  end;

  TLazCustomEditTextAttributeArray = array of TLazCustomEditTextAttribute;

  { TLazEditTextAttribute }

  TLazEditTextAttribute = class(TLazCustomEditTextAttribute)
  private
    FOnChange: TNotifyEvent;
    FChangeHandlers: TMethodList;
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
    FDefaultFeatures: TLazTextAttributeFeatures;

  protected
    function GetPriority(AnIndex: TLazTextAttributeColor): integer; override;
    function GetFrameSidePriority(Side: TLazTextAttrBorderSide): integer; override;
    function GetStylePriority(AnIndex: TFontStyle): integer; override;
    procedure SetPriority(AnIndex: TLazTextAttributeColor; AValue: integer); override;
    procedure SetStylePriority(AnIndex: TFontStyle; AValue: integer); override;

    function GetFeaturesStored: Boolean;
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
    constructor Create(ASupportedFeatures: TLazTextAttributeFeatures);
    constructor Create(ACaption: string; AStoredName: String = '');
    constructor Create(ACaption: string; AStoredName: String; ASupportedFeatures: TLazTextAttributeFeatures);
    constructor Create(ACaption: PString; AStoredName: String = ''); // e.g. pointer to resourcestring. (Must be global var/const)
    constructor Create(ACaption: PString; AStoredName: String; ASupportedFeatures: TLazTextAttributeFeatures); // e.g. pointer to resourcestring. (Must be global var/const)
    destructor Destroy; override;
    procedure SetCaption(ACaption: String);

    procedure InternalSaveDefaultValues; virtual;
    procedure SetAllPriorities(APriority: integer); override;

    procedure AddChangeHandler(AnHandler: TNotifyEvent);
    procedure RemoveChangeHandler(AnHandler: TNotifyEvent);

    property Caption: PString read FCaption;                        // will never be nil
    property StoredName: string read FStoredName write FStoredName; // name for storage (e.g. xml)

  public
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

    property Features stored GetFeaturesStored;

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

  public
    property BackAlpha  stored GetAlphaStored;
    property ForeAlpha  stored GetAlphaStored;
    property FrameAlpha stored GetAlphaStored;

    property StyleMask stored GetStyleMaskStored;
  end;

  TLazEditAttributeOwner = class(TComponent)
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
    FOwner: TLazEditAttributeOwner;
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
    constructor Create(AnOwner: TLazEditAttributeOwner);
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

  { TLazEditTextAttributeMergeResult }

  TLazEditTextAttributeMergeResult = class(TLazEditTextAttribute)
  private type
    TFontStylePriors = fsBold..fsUnderline; //fsBold, fsItalic, fsUnderline

    { TUnMergedList }

    generic TUnMergedList<T> = object
    public type
      PT = ^T;
    public
      Count: integer;
      List: array of T;
      procedure Clear;
      function Add: PT;
      procedure Shorten(NewBasePrior: integer);
      procedure AssignFrom(L: TUnMergedList);
    end;

    TUnMergedColor = object
      Prior: integer;
      Color: TColor;
      Alpha: byte;
    end;
    TUnMergedFrame = object(TUnMergedColor)
      Style: TLazTextAttrLineStyle;
    end;
    TUnMergedFontStyle = object
      Prior: integer;
      //Style,Mask: TFontStyles; // only invert goes into the list
    end;
    PUnMergedColor = ^TUnMergedColor;
    PUnMergedFrame = ^TUnMergedFrame;
    PUnMergedFontStyle = ^TUnMergedFontStyle;

  private
    FState: (mrsClear, mrsAssigning, mrsAssigningRes, mrsAssigned, mrsMerging, mrsMerged);
    FUnMergedColors:     array[TLazTextAttributeColor] of specialize TUnMergedList<TUnMergedColor>;
    FUnMergedFrameSides: array[TLazTextAttrBorderSide] of specialize TUnMergedList<TUnMergedFrame>;
    FUnMergedFontStyles: array [TFontStylePriors] of specialize TUnMergedList<TUnMergedFontStyle>;
    FFrameSides: array[TLazTextAttrBorderSide] of TUnMergedFrame;

    function IsMatching(const ABound1, ABound2: TLazEditDisplayTokenBound): Boolean; inline;
  protected
    function GetFrameEdges: TLazTextAttrFrameEdges; override; // ALL
    function GetFrameSideColors(Side: TLazTextAttrBorderSide): TColor; override;
    function GetFrameSideAlpha(Side: TLazTextAttrBorderSide): Byte; override;
    function GetFrameSideStyles(Side: TLazTextAttrBorderSide): TLazTextAttrLineStyle; override;
    procedure SetStartX(AValue: TLazEditDisplayTokenBound); override;
    procedure SetEndX(AValue: TLazEditDisplayTokenBound); override;
    procedure DoClear; override;
    procedure AssignFrom(ASource: TLazCustomEditTextAttribute); override;
  public
    procedure Assign(ASource: TPersistent); override;
    // procedure SetFrameBoundsLog(AStart, AEnd: Integer; AStartOffs: Integer = 0;
    //  AEndOffs: Integer = 0);
    procedure Merge(AnAttrib: TLazCustomEditTextAttribute);
    procedure FinishMerge;
  end;

implementation

{ TLazEditDisplayTokenBound }

function TLazEditDisplayTokenBound.HasValue: boolean;
begin
  Result := (Physical > 0) or (Logical > 0);
end;

procedure TLazEditDisplayTokenBound.Init(APhys, ALog: Integer; AnOffs: Integer);
begin
  Physical := APhys;
  Logical := ALog;
  Offset := AnOffs;
end;

{ TLazCustomEditTextAttribute }

procedure TLazCustomEditTextAttribute.SetEndX(AValue: TLazEditDisplayTokenBound);
begin
  FEndX := AValue;
end;

procedure TLazCustomEditTextAttribute.SetStartX(AValue: TLazEditDisplayTokenBound);
begin
  FStartX := AValue;
end;

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
  if Side in LazTextFrameEdgeToSides[FrameEdges] then
    Result := FColors[lacFrameColor]
  else
    Result := clNone;
end;

function TLazCustomEditTextAttribute.GetFrameSideAlpha(Side: TLazTextAttrBorderSide): Byte;
begin
  Result := FrameAlpha;
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

function TLazCustomEditTextAttribute.GetFeature(AnIndex: TLazTextAttributeFeature): Boolean;
begin
  Result := AnIndex in Features;
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

procedure TLazCustomEditTextAttribute.SetFeature(AnIndex: TLazTextAttributeFeature; AValue: Boolean
  );
begin
  if AValue then
    Features := Features + [AnIndex]
  else
    Features := Features - [AnIndex];
end;

procedure TLazCustomEditTextAttribute.SetFeatures(AValue: TLazTextAttributeFeatures);
begin
  AValue := AValue * FSupportedFeatures;
  if FFeatures = AValue then
    exit;
  FFeatures := AValue;
  Changed;
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

procedure TLazCustomEditTextAttribute.AssignSupportedFeaturesFrom(
  ASource: TLazCustomEditTextAttribute);
begin
  FSupportedFeatures := ASource.SupportedFeatures;
  FFeatures          := FFeatures * SupportedFeatures;
  Changed;
end;

procedure TLazCustomEditTextAttribute.AssignColorsFrom(ASource: TLazCustomEditTextAttribute);
begin
  FColors     := ASource.FColors;
  FFrameEdges := ASource.FFrameEdges;
  FFrameStyle := ASource.FFrameStyle;
  FStyle      := ASource.FStyle;
  FFeatures   := ASource.FFeatures * SupportedFeatures;
end;

procedure TLazCustomEditTextAttribute.AssignFrom(ASource: TLazCustomEditTextAttribute);
begin
  FStartX     := ASource.FStartX;
  FEndX       := ASource.FEndX;
end;

function TLazCustomEditTextAttribute.DefaultSupportedFeatures: TLazTextAttributeFeatures;
begin
  Result := [];
end;

constructor TLazCustomEditTextAttribute.Create;
begin
  Create(DefaultSupportedFeatures);
end;

constructor TLazCustomEditTextAttribute.Create(ASupportedFeatures: TLazTextAttributeFeatures);
begin
  FSupportedFeatures := ASupportedFeatures;
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

procedure TLazCustomEditTextAttribute.UpdateSupportedFeatures(AnAddSupportedFeatures,
  ARemoveSupportedFeatures: TLazTextAttributeFeatures);
begin
  FSupportedFeatures := FSupportedFeatures - ARemoveSupportedFeatures + AnAddSupportedFeatures;
end;

procedure TLazCustomEditTextAttribute.SetAllPriorities(APriority: integer);
begin
  assert(false, 'TLazCustomEditTextAttribute.SetAllPriorities: abstract');
  //raise exception.Create('abstract');
end;

procedure TLazCustomEditTextAttribute.SetFrameBoundsPhys(AStart, AEnd: Integer);
var
  b: TLazEditDisplayTokenBound;
begin
  b.Init(AStart, 0);
  StartX := b;
  b.Init(AEnd, 0);
  EndX := b;
end;

procedure TLazCustomEditTextAttribute.SetFrameBoundsLog(AStart, AEnd: Integer;
  AStartOffs: Integer; AEndOffs: Integer);
var
  b: TLazEditDisplayTokenBound;
begin
  b.Init(0, AStart);
  StartX := b;
  b.Init(0, AEnd);
  EndX := b;
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

function TLazEditTextAttribute.GetFeaturesStored: Boolean;
begin
  Result := FFeatures <> FDefaultFeatures;
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
  if FChangeHandlers <> nil then
    FChangeHandlers.CallNotifyEvents(Self);
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

constructor TLazEditTextAttribute.Create(ASupportedFeatures: TLazTextAttributeFeatures);
begin
  FCaption := @FFixedCaption;
  inherited Create(ASupportedFeatures);
end;

constructor TLazEditTextAttribute.Create(ACaption: string; AStoredName: String);
begin
  Create(ACaption, AStoredName, DefaultSupportedFeatures);
end;

constructor TLazEditTextAttribute.Create(ACaption: string; AStoredName: String;
  ASupportedFeatures: TLazTextAttributeFeatures);
begin
  FFixedCaption := ACaption;
  Create(@FFixedCaption, AStoredName, ASupportedFeatures);
end;

constructor TLazEditTextAttribute.Create(ACaption: PString; AStoredName: String);
begin
  Create(ACaption, AStoredName, DefaultSupportedFeatures);
end;

constructor TLazEditTextAttribute.Create(ACaption: PString; AStoredName: String;
  ASupportedFeatures: TLazTextAttributeFeatures);
begin
  Create(ASupportedFeatures);
  if ACaption <> nil then
    FCaption := ACaption;

  FStoredName := AStoredName;
  if FStoredName = '' then
    FStoredName := FCaption^;
end;

destructor TLazEditTextAttribute.Destroy;
begin
  inherited Destroy;
  FChangeHandlers.Free;
end;

procedure TLazEditTextAttribute.InternalSaveDefaultValues;
begin
  FDefaultColors        := FColors;
  FDefaultPriority      := FPriority;
  FDefaultFrameEdges    := FFrameEdges;
  FDefaultFrameStyle    := FFrameStyle;
  FDefaultStyle         := FStyle;
  FDefaultStylePriority := FStylePriority;
  FDefaultFeatures      := FFeatures;
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

procedure TLazEditTextAttribute.AddChangeHandler(AnHandler: TNotifyEvent);
begin
  if FChangeHandlers = nil then
    FChangeHandlers := TMethodList.Create;
  FChangeHandlers.Add(TMethod(AnHandler));
end;

procedure TLazEditTextAttribute.RemoveChangeHandler(AnHandler: TNotifyEvent);
begin
  if FChangeHandlers = nil then
    exit;
  FChangeHandlers.Remove(TMethod(AnHandler));
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
      TheItem.Attribute.AddChangeHandler(@DoAttribChaged);
    end;
    cnExtracting, cnDeleting: begin
      TheItem.Attribute.RemoveChangeHandler(@DoAttribChaged);
      FOwner.RemoveAttribute(TheItem.Attribute);
    end;
  end;
  ResetNames;
end;

constructor TLazEditTextAttributeModifierCollection.Create(AnOwner: TLazEditAttributeOwner);
begin
  FOwner := AnOwner;
  inherited Create(TLazEditTextAttributeModifierCollectionItem);
end;

function TLazEditTextAttributeModifierCollection.Add: TLazEditTextAttributeModifierCollectionItem;
begin
  Result := TLazEditTextAttributeModifierCollectionItem(inherited Add);
end;

{ TLazEditTextAttributeMergeResult.TUnMergedList }

procedure TLazEditTextAttributeMergeResult.TUnMergedList.Clear;
begin
  Count := 0;
  if Length(List) > 6 then
    SetLength(List, 6);
end;

function TLazEditTextAttributeMergeResult.TUnMergedList.Add: PT;
begin
  if Count >= Length(List) then
    SetLength(List, Count + 8);
  Result := @List[Count];
  inc(Count);
end;

procedure TLazEditTextAttributeMergeResult.TUnMergedList.Shorten(NewBasePrior: integer);
var
  c: Integer;
begin
  c := Count-1;
  while (c >= 0) and (List[c].Prior < NewBasePrior) do
    dec(c);
  Count := c+1;
end;

procedure TLazEditTextAttributeMergeResult.TUnMergedList.AssignFrom(L: TUnMergedList);
var
  i: Integer;
begin
  Count := L.Count;
  if (Length(List) < Count) or (Length(List) > Count + 12) then
    SetLength(List, Count);
  for i := 0 to Count - 1 do
    Add()^ := L.List[i];
end;

{ TLazEditTextAttributeMergeResult }

function TLazEditTextAttributeMergeResult.IsMatching(const ABound1,
  ABound2: TLazEditDisplayTokenBound): Boolean;
begin
  Result := ( (ABound1.Physical > 0) and
              (ABound1.Physical = ABound2.Physical)
            ) or
            ( (ABound1.Logical > 0) and
              (ABound1.Logical = ABound2.Logical) and (ABound1.Offset = ABound2.Offset)
            );
end;

function TLazEditTextAttributeMergeResult.GetFrameEdges: TLazTextAttrFrameEdges;
begin
  Result := sfeAround;
end;

function TLazEditTextAttributeMergeResult.GetFrameSideColors(Side: TLazTextAttrBorderSide): TColor;
begin
  Result := FFrameSides[Side].Color;
end;

function TLazEditTextAttributeMergeResult.GetFrameSideAlpha(Side: TLazTextAttrBorderSide): Byte;
begin
  Result := FFrameSides[Side].Alpha;
end;

function TLazEditTextAttributeMergeResult.GetFrameSideStyles(Side: TLazTextAttrBorderSide
  ): TLazTextAttrLineStyle;
begin
  Result := FFrameSides[Side].Style;
end;

procedure TLazEditTextAttributeMergeResult.SetStartX(AValue: TLazEditDisplayTokenBound);
begin
  if StartX.HasValue and not IsMatching(StartX, AValue) then begin
    FFrameSides[bsLeft].Color := clNone;
    FUnMergedFrameSides[bsLeft].Clear;
  end;

  inherited SetStartX(AValue);
end;

procedure TLazEditTextAttributeMergeResult.SetEndX(AValue: TLazEditDisplayTokenBound);
begin
  if EndX.HasValue and not IsMatching(EndX, AValue) then begin
    FFrameSides[bsRight].Color := clNone;
    FUnMergedFrameSides[bsRight].Clear;
  end;
  inherited SetEndX(AValue);
end;

procedure TLazEditTextAttributeMergeResult.DoClear;
var
  ColorIdx: TLazTextAttributeColor;
  BorderIdx: TLazTextAttrBorderSide;
  FontStyleIdx: TFontStyle;
begin
  inherited DoClear;
  if FState = mrsAssigningRes then
    exit;
  if FState <> mrsAssigning then begin
    FState := mrsClear;

    for BorderIdx in TLazTextAttrBorderSide do begin
      FFrameSides[BorderIdx].Prior := 0;
      FFrameSides[BorderIdx].Color := clNone;
    end;
  end;

  for ColorIdx in TLazTextAttributeColor do
    FUnMergedColors[ColorIdx].Clear;
  for BorderIdx in TLazTextAttrBorderSide do
    FUnMergedFrameSides[BorderIdx].Clear;
  for FontStyleIdx in TFontStylePriors do
    FUnMergedFontStyles[FontStyleIdx].Clear;
end;

procedure TLazEditTextAttributeMergeResult.Assign(ASource: TPersistent);
begin
  BeginUpdate;
  if (ASource is TLazEditTextAttributeMergeResult) and
     (TLazEditTextAttributeMergeResult(ASource).FState = mrsMerging)
  then
    FState := mrsAssigningRes
  else
    FState := mrsAssigning;

  Clear;
  inherited Assign(ASource);
  if FState in [mrsClear, mrsAssigning, mrsAssigningRes] then
    FState := mrsAssigned;
  EndUpdate;
end;

procedure TLazEditTextAttributeMergeResult.AssignFrom(ASource: TLazCustomEditTextAttribute);
var
  MrgResSource: TLazEditTextAttributeMergeResult absolute ASource;
  ColorIdx: TLazTextAttributeColor;
  BorderIdx: TLazTextAttrBorderSide;
  FontStyleIdx: TFontStyle;
begin
  inherited AssignFrom(ASource);

  if ASource is TLazEditTextAttributeMergeResult then begin
    FState := MrgResSource.FState;
    FFrameSides := MrgResSource.FFrameSides;

    if MrgResSource.FState = mrsMerging then begin
      for ColorIdx in TLazTextAttributeColor do
        FUnMergedColors[ColorIdx].AssignFrom(MrgResSource.FUnMergedColors[ColorIdx]);
      for BorderIdx in TLazTextAttrBorderSide do
        FUnMergedFrameSides[BorderIdx].AssignFrom(MrgResSource.FUnMergedFrameSides[BorderIdx]);
      for FontStyleIdx in TFontStylePriors do
        FUnMergedFontStyles[FontStyleIdx].AssignFrom(MrgResSource.FUnMergedFontStyles[FontStyleIdx]);
    end;
  end
  else begin
    // init frame sides
    for BorderIdx in TLazTextAttrBorderSide do begin
      FFrameSides[BorderIdx].Prior := ASource.FrameSidePriority[BorderIdx];
      FFrameSides[BorderIdx].Color := ASource.FrameSideColors[BorderIdx];
      FFrameSides[BorderIdx].Alpha := ASource.FrameSideAlpha[BorderIdx];
      FFrameSides[BorderIdx].Style := ASource.FrameSideStyles[BorderIdx];
    end;
  end;
end;

procedure TLazEditTextAttributeMergeResult.Merge(AnAttrib: TLazCustomEditTextAttribute);
var
  ColorIdx: TLazTextAttributeColor;
  BorderIdx: TLazTextAttrBorderSide;
  FontStyleIdx: TFontStylePriors;
  AttrPrior: integer;
  AttrCol: TColor;
  UnMergedColor: PUnMergedColor;
  UnMergedFrame: PUnMergedFrame;
  UnMergedStyle: PUnMergedFontStyle;
  edg: TLazTextAttrBorderSides;
  st, sm: TFontStyles;
begin
  BeginUpdate;
  FState := mrsMerging;

  for ColorIdx in TLazTextAttributeColor do begin
    if ColorIdx = lacFrameColor then continue;
    AttrPrior := AnAttrib.ColorPriority[ColorIdx];
    AttrCol   := AnAttrib.ColorValue[ColorIdx];
    if (AttrCol = clNone) or
       ((AttrPrior < ColorPriority[ColorIdx]) and (ColorValue[ColorIdx] <> clNone))
    then
      continue;
    if AnAttrib.ColorAlpha[ColorIdx] = 0 then begin
      if FPriority[ColorIdx] < AttrPrior then
        FUnMergedColors[ColorIdx].Shorten(AttrPrior);
      FPriority[ColorIdx] := AttrPrior;
      FColors[ColorIdx]   := AttrCol;
    end
    else begin
      UnMergedColor := FUnMergedColors[ColorIdx].Add;
      UnMergedColor^.Prior := AttrPrior;
      UnMergedColor^.Color := AttrCol;
      UnMergedColor^.Alpha := AnAttrib.ColorAlpha[ColorIdx];
    end;
  end;

  edg := LazTextFrameEdgeToSides[AnAttrib.FrameEdges];
  for BorderIdx in TLazTextAttrBorderSide do begin
    if not(BorderIdx in edg) then
      continue;
    case BorderIdx of
      bsLeft:  if AnAttrib.StartX.HasValue and not IsMatching(AnAttrib.StartX, StartX) then continue;
      bsRight: if AnAttrib.EndX.HasValue   and not IsMatching(AnAttrib.EndX, EndX) then continue;
    end;
    AttrPrior := AnAttrib.FrameSidePriority[BorderIdx];
    AttrCol   := AnAttrib.FrameSideColors[BorderIdx];
    if (AttrCol = clNone) or
       ((AttrPrior < FFrameSides[BorderIdx].Prior) and (FrameSideColors[BorderIdx] <> clNone))
    then
      continue;
    if AnAttrib.ColorAlpha[lacFrameColor] = 0 then begin
      if FFrameSides[BorderIdx].Prior < AttrPrior then
        FUnMergedColors[ColorIdx].Shorten(AttrPrior);
      FFrameSides[BorderIdx].Prior := AttrPrior;
      FFrameSides[BorderIdx].Color := AttrCol;
      FFrameSides[BorderIdx].Alpha := AnAttrib.FrameSideAlpha[BorderIdx];
      FFrameSides[BorderIdx].Style := AnAttrib.FrameSideStyles[BorderIdx];
    end
    else begin
      UnMergedFrame := FUnMergedFrameSides[BorderIdx].Add;
      UnMergedFrame^.Prior := AttrPrior;
      UnMergedFrame^.Color := AttrCol;
      UnMergedFrame^.Alpha := AnAttrib.ColorAlpha[lacFrameColor];
      UnMergedFrame^.Style := AnAttrib.FrameSideStyles[BorderIdx];
    end;
  end;

  st := AnAttrib.Style;
  sm := AnAttrib.StyleMask;
  for FontStyleIdx in TFontStylePriors do begin
    AttrPrior := AnAttrib.StylePriority[FontStyleIdx];
    if (AttrPrior < StylePriority[FontStyleIdx]) or
       not(FontStyleIdx in (st+sm))
    then
      continue;
    if ((FontStyleIdx in st) and not (FontStyleIdx in sm)) then begin
      // invert
      UnMergedStyle := FUnMergedFontStyles[FontStyleIdx].Add;
      UnMergedStyle^.Prior := AttrPrior;
    end
    else begin
      if StylePriority[FontStyleIdx] < AttrPrior then
        FUnMergedFontStyles[FontStyleIdx].Shorten(AttrPrior);
      StylePriority[FontStyleIdx] := AttrPrior;
      if (FontStyleIdx in st) then // set
        Include(FStyle, FontStyleIdx)
      else  // clear
        Exclude(FStyle, FontStyleIdx);
    end;
  end;
  EndUpdate;
end;

procedure TLazEditTextAttributeMergeResult.FinishMerge;
  procedure FadeColor(var R,G,B: Byte; ACol: TColor; AnAlpha: Byte);
  var
    R2,G2,B2: Byte;
  begin
    RedGreenBlue(ColorToRGB(ACol), R2, G2, B2);
    R := Byte(Min(Max(Integer(R) + (Integer(R2) - Integer(R)) * Integer(AnAlpha) div 256, 0), 255));
    G := Byte(Min(Max(Integer(G) + (Integer(G2) - Integer(G)) * Integer(AnAlpha) div 256, 0), 255));
    B := Byte(Min(Max(Integer(B) + (Integer(B2) - Integer(B)) * Integer(AnAlpha) div 256, 0), 255));
  end;
var
  i, c, CurPrior: Integer;
  f: boolean;
  ColorIdx: TLazTextAttributeColor;
  BorderIdx: TLazTextAttrBorderSide;
  FontStyleIdx: TFontStyle;
  CurCol: TColor;
  UnMergedColor: PUnMergedColor;
  UnMergedFrame: PUnMergedFrame;
  UnMergedStyle: PUnMergedFontStyle;
  R,G,B: Byte;
  st: TFontStyles;
begin
  if FState <> mrsMerging then begin
    FState := mrsMerged;
    exit;
  end;

  FState := mrsMerged;
  BeginUpdate;

  for ColorIdx in TLazTextAttributeColor do begin
    if ColorIdx = lacFrameColor then continue;
    c := FUnMergedColors[ColorIdx].Count;
    if c = 0 then continue;

    CurPrior := ColorPriority[ColorIdx];
    CurCol   := ColorValue[ColorIdx];
    if CurCol = clNone then
      CurCol := FUnMergedColors[ColorIdx].List[0].Color;
    RedGreenBlue(ColorToRGB(CurCol), R, G, B);
    for i := 0 to c - 1 do begin
      UnMergedColor := @FUnMergedColors[ColorIdx].List[i];
      if UnMergedColor^.Prior < CurPrior then continue;
      FadeColor(R, G, B, UnMergedColor^.Color, UnMergedColor^.Alpha);
    end;
    ColorValue[ColorIdx] := RGBToColor(R, G, B);
  end;

  for BorderIdx in TLazTextAttrBorderSide do begin
    c := FUnMergedFrameSides[BorderIdx].Count;
    if c = 0 then continue;

    CurPrior := FFrameSides[BorderIdx].Prior;
    CurCol   := FFrameSides[BorderIdx].Color;
    if CurCol = clNone then begin
      CurCol := Background;
      if CurCol = clNone then
        CurCol := FUnMergedFrameSides[BorderIdx].List[0].Color;
    end;
    RedGreenBlue(ColorToRGB(CurCol), R, G, B);
    for i := 0 to c - 1 do begin
      UnMergedFrame := @FUnMergedFrameSides[BorderIdx].List[i];
      if UnMergedFrame^.Prior < CurPrior then continue;
      FadeColor(R, G, B, UnMergedFrame^.Color, UnMergedFrame^.Alpha);
      FFrameSides[BorderIdx].Style := UnMergedFrame^.Style;  // always updating style
    end;
    FFrameSides[BorderIdx].Color := RGBToColor(R, G, B);
  end;

  st := Style;
  for FontStyleIdx in TFontStylePriors do begin
    c := FUnMergedFontStyles[FontStyleIdx].Count;
    if c = 0 then continue;
    CurPrior := StylePriority[FontStyleIdx];
    f := False;
    for i := 0 to c - 1 do begin
      UnMergedStyle := @FUnMergedFontStyles[FontStyleIdx].List[i];
      if UnMergedStyle^.Prior < CurPrior then continue;
      f := not f;
    end;
    if f then begin
      if FontStyleIdx in st then
        Exclude(st, FontStyleIdx)
      else
        Include(st, FontStyleIdx);
    end;
  end;
  Style := st;

  EndUpdate;
end;

end.

