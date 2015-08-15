(******************************************************
  AdvLed by Jurassic Pork 2013 for Lazarus
  created from TComled of ComPort Library ver. 3.00
  written by Dejan Crnila, 1998 - 2002
  email: dejancrn@yahoo.com
 ****************************************************************************
  This file is part of Lazarus

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 ****************************************************************************
  Unit: AdvLed.pas
 ******************************************************)

unit AdvLed;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, ExtCtrls, Controls, Graphics;

type

  // property types
  TLedBitmap = Graphics.TPicture;
//  TLedKind = (lkRedLight, lkGreenLight, lkBlueLight, lkYellowLight, lkPurpleLight, lkBulb, lkCustom);
  TLedKind = (lkRedLight, lkGreenLight, lkYellowLight, lkBulb, lkCustom);
  TLedState = (lsDisabled, lsOff, lsOn);
  TAdvLedGlyphs = array[TLedState] of TLedBitmap;
  TLedStateEvent = procedure(Sender: TObject; AState: TLedState) of object;

  // led control that shows the state of serial signals
  TAdvLed = class(TCustomImage)
  private
    FKind: TLedKind;
    FState: TLedState;
    FBlink: Boolean;
    FOnChange: TLedStateEvent;
    FGlyphs: TAdvLedGlyphs;
    FBlinkTimer: TTimer;
    function GetGlyph(const Index: Integer): TLedBitmap;
    function GetBlinkDuration: Integer;
    procedure SetKind(const Value: TLedKind);
    procedure SetState(const Value: TLedState);
    procedure SetGlyph(const Index: Integer; const Value: TLedBitmap);
    procedure SetBlinkDuration(const Value: Integer);
    procedure SetBlink(const Value: Boolean);
    function StoredGlyph(const Index: Integer): Boolean;
    procedure SelectLedBitmap(const LedKind: TLedKind);
    function BitmapToDraw: TLedBitmap;
    procedure BitmapNeeded;
    procedure DoTimer(Sender: TObject);
    procedure GlyphChanged(Sender: TObject);

  protected
    FlipFLop : Boolean;
    procedure DoChange(AState: TLedState); dynamic;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    // kind property must be published before GlyphOn, GlyphOff,GlyphDisable
    property Kind: TLedKind read FKind write SetKind default lkRedLight;
    property GlyphDisabled: TLedBitmap index 0 read GetGlyph
             write SetGlyph stored StoredGlyph;
    property GlyphOff: TLedBitmap index 1 read GetGlyph
             write SetGlyph   stored StoredGlyph;
    property GlyphOn: TLedBitmap index 2 read GetGlyph
             write SetGlyph stored StoredGlyph;
    property State: TLedState read FState write SetState;
    property Blink: Boolean read FBlink write SetBlink;
    property BlinkDuration: Integer read GetBlinkDuration write SetBlinkDuration default 1000;
    property Align;
    property AutoSize default true;
    property Center;
    property Constraints;
//  property Picture;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Stretch;
    property Showhint;
    property Transparent;
    property Proportional;
    property OnPictureChanged;
    property OnChange: TLedStateEvent read FOnChange write FOnChange;

    {   property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Anchors;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnResize;
    property OnStartDock;
    property OnContextPopup;  }
  end;

implementation

{$R ledbuttons.res}

(*****************************************
 * auxilary functions                    *
 *****************************************)

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;
(*****************************************
 * TAdvLed control                       *
 *****************************************)

// create control
constructor TAdvLed.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  AutoSize:=True;
  FGlyphs[lsOn] := TLedBitmap.Create;
  FGlyphs[lsOff] := TLedBitmap.Create;
  FGlyphs[lsDisabled] := TLedBitmap.Create;
  FGlyphs[lsOn].OnChange:= @GlyphChanged;
  FGlyphs[lsOff].OnChange:= @GlyphChanged;
  FGlyphs[lsDisabled].OnChange:= @GlyphChanged;
  FBlinkTimer := TTimer.Create(nil);
  FBlinkTimer.OnTimer := @DoTimer;
  FBlinkTimer.Enabled := false;
  if (csDesigning in ComponentState) then BitmapNeeded;
end;

// destroy control
destructor TAdvLed.Destroy;
begin
  FBlinkTimer.Free;
  FGlyphs[lsOn].Free;
  FGlyphs[lsOff].Free;
  FGlyphs[lsDisabled].Free;
  inherited Destroy;
end;

// loaded
procedure TAdvLed.Loaded;
  begin
   Try
      If (csDesigning in ComponentState) Then Exit ;
      // Load Bitmap if necessary
     BitmapNeeded;
   Finally
      inherited Loaded;
   End;
end;

// timer
procedure TAdvLed.DoTimer(Sender: TObject);
begin
  if FlipFlop then
    SetState(lsOn )
  else
    SetState(lsoff);
  FlipFlop := Not FlipFlop;
end;

// trigger OnChangeEvent
procedure TAdvLed.DoChange(AState: TLedState);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, AState);
  invalidate;
end;

// if bitmap is empty, load it
procedure TAdvLed.BitmapNeeded;
begin
  if (FGlyphs[lsOn].Bitmap.Empty) or (FGlyphs[lsOff].Bitmap.Empty) or
    (FGlyphs[lsDisabled].Bitmap.Empty) then
  begin
    SelectLedBitmap(FKind);
    Picture.Assign(BitmapToDraw);
  end;
end;

procedure TAdvLed.SelectLedBitmap(const LedKind: TLedKind);
const
{  OnBitmaps: array[TLedKind] of string = ('LEDREDON', 'LEDGREENON', 'LEDBLUEON',
    'LEDYELLOWON', 'LEDPURPLEON', 'LEDBULBON', '');
  OffBitmaps: array[TLedKind] of string = ('LEDREDOFF', 'LEDGREENOFF',
    'LEDBLUEOFF', 'LEDYELLOWOFF', 'LEDPURPLEOFF', 'LEDBULBOFF' ,'');
  DisabledBitmaps: array[TLedKind] of string = ('LEDREDOFF', 'LEDGREENOFF',
    'LEDBLUEOFF', 'LEDYELLOWOFF', 'LEDPURPLEOFF', 'LEDBULBOFF' ,'');   }
  OnBitmaps: array[TLedKind] of string = ('RED', 'GREEN', 'YELLOW', 'BULBON', '');
  OffBitmaps: array[TLedKind] of string = ('BLACK', 'BLACK', 'BLACK','BULBOFF', '');
  DisabledBitmaps: array[TLedKind] of string = ('BLACK', 'BLACK', 'BLACK','BULBOFF' ,'');
begin
  if LedKind <> lkCustom then
  begin
    FGlyphs[lsOn].LoadFromResourceName(HInstance, OnBitmaps[LedKind]);
    FGlyphs[lsOff].LoadFromResourceName(HInstance, OffBitmaps[LedKind]);
    FGlyphs[lsDisabled].LoadFromResourceName(HInstance, DisabledBitmaps[LedKind]);
  end;
end;

// set led kind
procedure TAdvLed.SetKind(const Value: TLedKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    SelectLedBitmap(FKind);
    Picture.Assign(BitmapToDraw);
  end;
end;

// set led state
procedure TAdvLed.SetState(const Value: TLedState);
begin
  FState := Value;
  if not (csLoading in ComponentState) then
    DoChange(FState);
  Picture.Assign(BitmapToDraw);
end;

function TAdvLed.GetGlyph(const Index: Integer): TLedBitmap;
begin
  case Index of
    0: Result := FGlyphs[lsDisabled];
    1: Result := FGlyphs[lsOff];
    2: Result := FGlyphs[lsOn];
  else
    Result := nil;
  end;
end;

procedure TAdvLed.GlyphChanged(Sender: TObject );
begin
//  if (csDesigning in ComponentState) then   Picture.Assign(Sender as TPicture);
   if (csDesigning in ComponentState) then
   begin
     if Sender = FGlyphs[lsDisabled] then FState := lsDisabled;
     if Sender = FGlyphs[lsOff] then FState := lsOff;
     if Sender = FGlyphs[lsOn] then FState := lsOn;
     Picture.Assign(Sender as TPicture);
   end;
end;

// set custom bitmap
procedure TAdvLed.SetGlyph(const Index: Integer; const Value: TLedBitmap);
begin
  if FKind = lkCustom then
  begin
    case Index of
      0: FGlyphs[lsDisabled].Assign(Value);
      1: FGlyphs[lsOff].Assign(Value);
      2: FGlyphs[lsOn].Assign(Value);
    end;
  end;
  Picture.Assign(BitmapToDraw);
end;

function TAdvLed.StoredGlyph(const Index: Integer): Boolean;
begin
  Result := FKind = lkCustom;
end;

// get bitmap for drawing
function TAdvLed.BitmapToDraw: TLedBitmap;
var
  ToDraw: TLedState;
begin
  if not Enabled then
    ToDraw := lsOff
  else
    ToDraw := FState;
  Result := FGlyphs[ToDraw];
end;

function TAdvLed.GetBlinkDuration: Integer;
begin
  Result := FBlinkTimer.Interval;
end;

procedure TAdvLed.SetBlinkDuration(const Value: Integer);
begin
  FBlinkTimer.Interval := Value;
end;

// set led blink
procedure TAdvLed.SetBlink(const Value: Boolean);
begin
   FBlink :=Value;
   if (csDesigning in ComponentState) then Exit;
   FBlinkTimer.Enabled :=  FBlink;
end;

end.
