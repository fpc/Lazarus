{
  Copyright (C) 2010 Felipe Monteiro de Carvalho

  License: The same modifying LGPL with static linking exception as the LCL

  This unit should be a repository for various custom drawn components,
  such as a custom drawn version of TButton, of TEdit, of TPageControl, etc,
  eventually forming a full set of custom drawn components.
}
unit customdrawnextras;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLType, LCLIntf, IntfGraphics,
  Math,
  // fpimage
  fpcanvas, fpimgcanv, fpimage
  {$ifdef CUSTOMDRAWN_USE_FREETYPE}
  // font support
  , ftfont
  {$endif}  ;

type

  // commented items are not yet supported
  TBitmappedButtonOption = (bboUseImageForSelection
    {bboUseImageForMouseOver, bboDrawFocusRectangle,}
    (*bboCheckable,*));

  TBitmappedButtonOptions = set of TBitmappedButtonOption;

  // commented items are not yet supported
  TBitmappedButtonState = (bbsNormal, bbsDown, bbsMouseOver, bbsFocused
    (* bbsChecked, bbsCheckedSelected, bbsCheckedDown { is going to be unchecked }*));

  TCDDrawStyle = (dsWinCE, dsAndroid, dsXPTaskBar, dsCustom);

  { TCustomBitmappedButton }

  TCustomBitmappedButton = class(TCustomControl)
  private
    FOnChange: TNotifyEvent;
    BmpBuffer: TBitmap;
    StrBuffer: TStringList;
  protected
    FImageBtn: TPicture;
    FImageBtnDown: TPicture;
    FImageBtnMouseOver: TPicture;
    FImageBtnFocused: TPicture;
    FImageBtnChecked: TPicture;
    FOptions: TBitmappedButtonOptions;
    FState: TBitmappedButtonState;
    // keyboard
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    // button state change
    procedure DoButtonDown();
    procedure DoButtonUp();
    // inherited
    procedure RealSetText(const Value: TCaption); override;
  protected
    // Properties
    property ImageBtn: TPicture read FImageBtn;
    property ImageBtnDown: TPicture read FImageBtnDown;
    property ImageBtnFocused: TPicture read FImageBtnFocused;
    property Options: TBitmappedButtonOptions read FOptions write FOptions;
    // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    function GetStateBitmap(): TBitmap;
  end;

  {@@
    TBitmappedButton is a simple custom drawn button which bases it's drawing
    on provided raster images. Currently the following states are supported:
    normal, down and focused.

    The Caption of this button may have multiple lines of text, separated by any
    line separator. The text is drawn centralized in the button.

    Some work was done trying to achieve alpha blending for the button, but this
    wasn't successfull. It would work like this: The button may be drawn flat
    or alpha blended using a separate image for the Alpha channel. While pixels
    in the alpha channel will result in the button pixel being fully drawn,
    while black pixels represent pixels which aren't drawn. grey pixels are
    alpha blended.
  }

  TBitmappedButton = class(TCustomBitmappedButton)
  published
    // LCL properties and events
    property Action;
    property Anchors;
    property AnchorSide;
    //    property BidiMode;
    //    property BorderSpacing;
    //    property Cancel;
    property Caption;
    property Constraints;
    //    property Default;
    //    property DragCursor;
    //    property DragKind;
    //    property DragMode;
    property Enabled;
    property Font;
    //    property ParentBidiMode;
    //    property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    // Specific properties
    property ImageBtn;
    property ImageBtnDown;
    property ImageBtnFocused;
    property Options;
  end;


  TCDButtonDrawer = class;
  TCDButtonDrawerWinCE = class;
  TCDButtonDrawerAndroid = class;

  TCDButton = class(TCustomControl)
  private
    FDrawStyle: TCDDrawStyle;
    FCurrentDrawer: TCDButtonDrawer;
    FDrawerWinCE: TCDButtonDrawerWinCE;
    FDrawerAndroid: TCDButtonDrawerAndroid;
    procedure PrepareCurrentDrawer();
    procedure SetDrawStyle(const AValue: TCDDrawStyle);
  protected
    FState: TBitmappedButtonState;
    // keyboard
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    // button state change
    procedure DoButtonDown();
    procedure DoButtonUp();
    procedure RealSetText(const Value: TCaption); override;
  public
    CustomDrawer: TCDButtonDrawer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Action;
    property Anchors;
    property Caption;
    property Color;
    property Constraints;
    property DrawStyle: TCDDrawStyle read FDrawStyle write SetDrawStyle;
    property Enabled;
    property Font;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

  { TCDButtonDrawer }

  TCDButtonDrawer = class
  public
    procedure SetClientRectPos(CDButton: TCDButton); virtual; abstract;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton);
      virtual; abstract;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TBitmappedButtonState); virtual; abstract;
  end;

  { TCDButtonDrawerWinCE }

  TCDButtonDrawerWinCE = class(TCDButtonDrawer)
  public
    procedure SetClientRectPos(CDButton: TCDButton); override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TBitmappedButtonState); override;
  end;

  { TCDButtonDrawerAndroid }
  TCDButtonDrawerAndroid = class(TCDButtonDrawer)
  public
    procedure SetClientRectPos(CDButton: TCDButton); override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TBitmappedButtonState); override;
  end;

  {@@
    TCDGroupBox is a custom-drawn group box control
  }

  TCDGroupBoxDrawer = class;
  TCDGroupBoxDrawerWinCE = class;

  { TCDGroupBox }

  TCDGroupBox = class(TCustomControl)
  private
    FDrawStyle: TCDDrawStyle;
    FCurrentDrawer: TCDGroupBoxDrawer;
    FDrawerWinCE: TCDGroupBoxDrawerWinCE;
    procedure PrepareCurrentDrawer();
    procedure SetDrawStyle(const AValue: TCDDrawStyle);
  public
    CustomDrawer: TCDGroupBoxDrawer; // Fill the field to use the dsCustom draw mode
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property DrawStyle: TCDDrawStyle read FDrawStyle write SetDrawStyle;
    property Caption;
    property TabStop default False;
  end;

  { TCDGroupBoxDrawer }

  TCDGroupBoxDrawer = class
  public
    procedure SetClientRectPos(CDGroupBox: TCDGroupBox); virtual; abstract;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDGroupBox: TCDGroupBox);
      virtual; abstract;
    procedure DrawToCanvas(ADest: TCanvas; CDGroupBox: TCDGroupBox); virtual; abstract;
  end;

  { TCDGroupBoxDrawerWinCE }

  TCDGroupBoxDrawerWinCE = class(TCDGroupBoxDrawer)
  public
    FCaptionMiddle: integer;
    procedure SetClientRectPos(CDGroupBox: TCDGroupBox); override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDGroupBox: TCDGroupBox); override;
    procedure DrawToCanvas(ADest: TCanvas; CDGroupBox: TCDGroupBox); override;
  end;

  {@@
    TCDTrackBar is a custom-drawn trackbar control
  }

//  TCDTrackBarDrawer = class;

{ TCDTrackBar }

{  TCDTrackBar = class(TCustomControl)
  private
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FOnChange: TNotifyEvent;
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
  protected
    procedure Changed; virtual;
    // keyboard
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Max: Integer read FMax write SetMax default 10;
    property Min: Integer read FMin write SetMin default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Position: Integer read FPosition write SetPosition;
    property TabStop default True;
  end;}

{ TCDTrackBarDrawer }

{  TCDTrackBarDrawer = class
  public
    procedure DrawToIntfImage(ADest: TLazIntfImage; CDTrackBar: TCDTrackBar); virtual; abstract;
  end;}

procedure Register;

implementation

const
  INT_BitmappedButton_LineSpacing = 2;

procedure Register;
begin
  RegisterComponents('Common Controls', [TCDButton]);
end;

{ TCustomBitmappedButton }

procedure TCustomBitmappedButton.DoEnter;
begin
  DoButtonUp();

  inherited DoEnter;
end;

procedure TCustomBitmappedButton.DoExit;
begin
  DoButtonUp();

  inherited DoExit;
end;

procedure TCustomBitmappedButton.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Key = VK_SPACE then
    DoButtonDown();
end;

procedure TCustomBitmappedButton.KeyUp(var Key: word; Shift: TShiftState);
begin
  DoButtonUp();

  inherited KeyUp(Key, Shift);
end;

procedure TCustomBitmappedButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DoButtonDown();

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomBitmappedButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DoButtonUp();

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomBitmappedButton.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TCustomBitmappedButton.MouseLeave;
begin
  inherited MouseLeave;
end;

procedure TCustomBitmappedButton.DoButtonDown();
var
  NewState: TBitmappedButtonState;
begin
  NewState := bbsDown;

  case FState of
    bbsNormal, bbsFocused: NewState := bbsDown;
    //  bbsChecked, bbsCheckedSelected: NewState := bbsCheckedDown;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TCustomBitmappedButton.DoButtonUp();
var
  NewState: TBitmappedButtonState;
begin
  if Focused then
    NewState := bbsFocused
  else
    NewState := bbsNormal;

{  case FState of
  bbsCheckedDown:
  begin
    if Focused then NewState := bbsCheckedSelected
    else NewState := bbsChecked;
  end;
  end;}

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TCustomBitmappedButton.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);

  // Makes sure that caption changes are drawn
  Invalidate;
end;

constructor TCustomBitmappedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BmpBuffer := TBitmap.Create;
  StrBuffer := TStringList.Create;

  FImageBtn := TPicture.Create;
  FImageBtnDown := TPicture.Create;
  FImageBtnMouseOver := TPicture.Create;
  FImageBtnFocused := TPicture.Create;
  FImageBtnChecked := TPicture.Create;

  FOptions := [{bboDrawSelectionRectangle}];

  TabStop := True;
end;

destructor TCustomBitmappedButton.Destroy;
begin
  if Assigned(FImageBtn) then
    FImageBtn.Free;
  if Assigned(FImageBtnDown) then
    FImageBtnDown.Free;
  if Assigned(FImageBtnMouseOver) then
    FImageBtnMouseOver.Free;
  if Assigned(FImageBtnFocused) then
    FImageBtnFocused.Free;
  if Assigned(FImageBtnChecked) then
    FImageBtnChecked.Free;

  BmpBuffer.Free;
  StrBuffer.Free;

  inherited Destroy;
end;

procedure TCustomBitmappedButton.EraseBackground(DC: HDC);
begin
  // The correct implementation is doing nothing
end;

procedure TCustomBitmappedButton.Paint;
var
  lTextX, lTextY, lTextCX, lTextCY, lTmp, lTextHeightPlusLineSpacing: integer;
  i: integer;
begin
  // First draw the button image
  BmpBuffer.Width := Width;
  BmpBuffer.Height := Height;
  BmpBuffer.Canvas.Draw(0, 0, GetStateBitmap());

  // Now measure the text position

  BmpBuffer.Canvas.Font.Assign(Self.Font);
  BmpBuffer.Canvas.Brush.Style := bsClear;

  StrBuffer.Text := Caption;

  lTextCX := 0;
  for i := 0 to StrBuffer.Count - 1 do
  begin
    lTmp := BmpBuffer.Canvas.TextWidth(StrBuffer.Strings[i]);
    lTextCX := Max(lTextCX, lTmp);
  end;

  lTextHeightPlusLineSpacing :=
    BmpBuffer.Canvas.TextHeight(Caption) + INT_BitmappedButton_LineSpacing;
  lTextCY := BmpBuffer.Canvas.TextHeight(Caption) * StrBuffer.Count +
    INT_BitmappedButton_LineSpacing * (StrBuffer.Count - 1);

  lTextX := Width div 2 - lTextCX div 2;
  lTextY := Height div 2 - lTextCY div 2;

  // Draw the text

  for i := 0 to StrBuffer.Count - 1 do
  begin
    BmpBuffer.Canvas.TextOut(lTextX, lTextY + lTextHeightPlusLineSpacing * i,
      StrBuffer.Strings[i]);
  end;

  // And flush the buffer to the screen
  Canvas.Draw(0, 0, BmpBuffer);
end;

function TCustomBitmappedButton.GetStateBitmap(): TBitmap;
begin
  case FState of
    bbsDown: Result := FImageBtnDown.Bitmap;
    //  bbsMouseOver: Result := FImageBtnMouseOver;
    bbsFocused:
    begin
      if bboUseImageForSelection in Options then
        Result := FImageBtnFocused.Bitmap
      else
        Result := FImageBtn.Bitmap;
    end;
      //  bbsChecked:   Result := FImageBtnChecked;
    else
      Result := FImageBtn.Bitmap;
  end;
end;

procedure GradientFill(Clr1, Clr2: TColor; TheBitmap: TBitmap);
var
  RGBFrom: array[0..2] of byte;
  RGBDiff: array[0..2] of integer;
  ColorBand: TRect; I: integer; R, G, B: byte;
begin
  RGBFrom[0] := GetRValue(ColorToRGB(Clr1));
  RGBFrom[1] := GetGValue(ColorToRGB(Clr1));
  RGBFrom[2] := GetBValue(ColorToRGB(Clr1));
  RGBDiff[0] := GetRValue(ColorToRGB(Clr2)) - RGBFrom[0];
  RGBDiff[1] := GetGValue(ColorToRGB(Clr2)) - RGBFrom[1];
  RGBDiff[2] := GetBValue(ColorToRGB(Clr2)) - RGBFrom[2];
  TheBitmap.Canvas.Pen.Style := psSolid;
  TheBitmap.Canvas.Pen.Mode := pmCopy;
  ColorBand.Left := 0;
  ColorBand.Right := TheBitmap.Width;
  for I := 0 to $ff do
  begin
    ColorBand.Top := MulDiv(I, TheBitmap.Height, $100);
    ColorBand.Bottom := MulDiv(I + 1, TheBitmap.Height, $100);
    R := RGBFrom[0] + MulDiv(I, RGBDiff[0], $ff);
    G := RGBFrom[1] + MulDiv(I, RGBDiff[1], $ff);
    B := RGBFrom[2] + MulDiv(I, RGBDiff[2], $ff);
    TheBitmap.Canvas.Brush.Color := RGB(R, G, B);
    TheBitmap.Canvas.FillRect(ColorBand);
  end;
end;

function GetNomalColor(a: byte): byte;
begin
  Result := a;
  if a < 1 then
    a := 1;
  if a > 255 then
    a := 255;
end;

procedure DrawAndroidButton(Canvas: TCanvas; Color: TColor);
const
  vedge = 12;
  rr = 3;
var
  i, xx, yy: integer;
  c2: TColor;
  r, g, b, r1, g1, b1: byte;
begin
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(0, 0, Canvas.Width, Canvas.Height);
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  r1 := GetRValue(Color);
  g1 := GetGValue(Color);
  b1 := GetBValue(Color);
  for yy := 0 to Canvas.Height do
  begin
    Canvas.MoveTo(0, yy);
 {   if yy < vedge then
    begin
      r := GetNomalColor(r1 - (vedge - yy) * rr);
      g := GetNomalColor(g1 - (vedge - yy) * rr);
      b := GetNomalColor(b1 - (vedge - yy) * rr);
      c2 := RGB(r, g, b);
      Canvas.Pen.Color := c2;
    end
    else       }
    if yy > Canvas.Height - vedge then
    begin
      r := GetNomalColor(r1 - (yy - Canvas.Height + vedge) * rr);
      g := GetNomalColor(g1 - (yy - Canvas.Height + vedge) * rr);
      b := GetNomalColor(b1 - (yy - Canvas.Height + vedge) * rr);
      c2 := RGB(r, g, b);
      Canvas.Pen.Color := c2;
    end
    else
      Canvas.Pen.Color := Color;
    Canvas.LineTo(Canvas.Width, yy);
  end;
end;

procedure TCDButton.DoEnter;
begin
  DoButtonUp();

  inherited DoEnter;
end;

procedure TCDButton.DoExit;
begin
  DoButtonUp();

  inherited DoExit;
end;

procedure TCDButton.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Key = VK_SPACE then
    DoButtonDown();
end;

procedure TCDButton.KeyUp(var Key: word; Shift: TShiftState);
begin
  DoButtonUp();

  inherited KeyUp(Key, Shift);
end;

procedure TCDButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if not Focused then
    SetFocus;
  DoButtonDown();

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  DoButtonUp();

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCDButton.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TCDButton.MouseLeave;
begin
  inherited MouseLeave;
end;

procedure TCDButton.DoButtonDown();
var
  NewState: TBitmappedButtonState;
begin
  NewState := bbsDown;

  case FState of
    bbsNormal, bbsFocused: NewState := bbsDown;
  end;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TCDButton.DoButtonUp();
var
  NewState: TBitmappedButtonState;
begin
  if Focused then
    NewState := bbsFocused
  else
    NewState := bbsNormal;

  if NewState <> FState then
  begin
    FState := NewState;
    Invalidate;
  end;
end;

procedure TCDButton.PrepareCurrentDrawer();
begin
  case DrawStyle of
    dsWince: FCurrentDrawer := FDrawerWinCE;
    dsCustom: FCurrentDrawer := CustomDrawer;
    dsAndroid: FCurrentDrawer := FDrawerAndroid;
  end;
end;

procedure TCDButton.SetDrawStyle(const AValue: TCDDrawStyle);
begin
  if FDrawStyle = AValue then
    exit;
  FDrawStyle := AValue;

  Invalidate;

  PrepareCurrentDrawer();
//  FCurrentDrawer.SetClientRectPos(Self); the button shouldn't receive controls inside it
end;

procedure TCDButton.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  Invalidate;
end;

constructor TCDButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  FDrawerWinCE := TCDButtonDrawerWinCE.Create;
  FDrawerAndroid := TCDButtonDrawerAndroid.Create;
  Width := 120;
  Height := 43;
  //Color := clTeal;
  ParentFont := True;
  FDrawStyle := dsAndroid;
  Color := clWhite;
end;

destructor TCDButton.Destroy;
begin
  inherited Destroy;
end;

procedure TCDButton.EraseBackground(DC: HDC);
begin

end;

procedure TCDButton.Paint;
var
  AImage: TLazIntfImage = nil;
  ABmp: TBitmap = nil;
  lCanvas: TFPImageCanvas = nil;
begin
//  inherited Paint;

  PrepareCurrentDrawer();

  ABmp := TBitmap.Create;
  try
    ABmp.Width := Width;
    ABmp.Height := Height;
    AImage := ABmp.CreateIntfImage;
    lCanvas := TFPImageCanvas.Create(AImage);
    // First step of the drawing: FCL TFPCustomCanvas for fast pixel access
    FCurrentDrawer.DrawToIntfImage(lCanvas, Self);
    ABmp.LoadFromIntfImage(AImage);
    // Second step of the drawing: LCL TCustomCanvas for easy font access
    FCurrentDrawer.DrawToCanvas(ABmp.Canvas, Self, FState);
    //
    Canvas.Draw(0, 0, ABmp);
  finally
    if lCanvas <> nil then
      lCanvas.Free;
    if AImage <> nil then
      AImage.Free;
    ABmp.Free;
  end;
end;

{ TCDButtonDrawerWinCE }

procedure TCDButtonDrawerWinCE.SetClientRectPos(CDButton: TCDButton);
var
  lRect: TRect;
begin
  lRect := Rect(1, 1, CDButton.Width - 1, CDButton.Height - 1);
  CDButton.AdjustClientRect(lRect);
end;

procedure TCDButtonDrawerWinCE.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

function GetEndColor(Color: TColor): TColor;
var
  r, g, b: byte;
begin
  r := GetRValue(ColorToRGB(Color));
  g := GetGValue(ColorToRGB(Color));
  b := GetBValue(ColorToRGB(Color));
  r := r * 50 div 100;
  g := g * 50 div 100;
  b := b * 50 div 100;
  if r <= 0 then
    r := 1;
  if g <= 0 then
    g := 1;
  if b <= 0 then
    b := 1;
  Result := RGB(r, g, b);
end;

function GetBeginColor(Color: TColor): TColor;
var
  r, g, b: byte;
begin
  r := GetRValue(ColorToRGB(Color));
  g := GetGValue(ColorToRGB(Color));
  b := GetBValue(ColorToRGB(Color));
  r := r * 100 div 60;
  g := g * 100 div 60;
  b := b * 100 div 60;
  if r >= 256 then
    r := 255;
  if g >= 256 then
    g := 255;
  if b >= 256 then
    b := 255;
  Result := RGB(r, g, b);
end;

procedure TCDButtonDrawerWinCE.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TBitmappedButtonState);
var
  TmpB: TBitmap;
  Str: String;
begin
  // Button shape -> This crashes in Gtk2
  TmpB := TBitmap.Create;
  TmpB.Width := CDButton.Width;
  TmpB.Height := CDButton.Height;
  TmpB.Canvas.Brush.Color := CDButton.Color;
  TmpB.Canvas.Brush.Style := bsSolid;
  TmpB.Canvas.RoundRect(0, 0, TmpB.Width, TmpB.Height, 8, 8);
  CDButton.SetShape(TmpB);
  ADest.Draw(0, 0, TmpB);
  TmpB.Free;

  // Button image
  case FState of
    bbsDown:
    begin

    end;
    bbsFocused:
      GradientFill(GetBeginColor(CDButton.Color), GetEndColor(CDButton.Color), TmpB);
    else
      GradientFill(GetBeginColor(CDButton.Color), GetEndColor(CDButton.Color), TmpB);
  end;

  // Button text
  {$ifndef CUSTOMDRAWN_USE_FREETYPE}
  ADest.Font.Assign(CDButton.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := CDButton.Caption;
  ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2,
    (CDButton.Height - ADest.TextHeight(Str)) div 2, Str);
  {$endif}
end;

procedure TCDButtonDrawerAndroid.SetClientRectPos(CDButton: TCDButton);
var
  lRect: TRect;
begin
  lRect := Rect(1, 1, CDButton.Width - 1, CDButton.Height - 1);
  CDButton.AdjustClientRect(lRect);
end;

procedure TCDButtonDrawerAndroid.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerAndroid.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TBitmappedButtonState);
var
  TmpB: TBitmap;
  Str: String;
begin
  // Button shape -> This crashes in Gtk2
{  TmpB.Canvas.Brush.Color := CDButton.Color;
  TmpB.Canvas.Brush.Style := bsSolid;
  TmpB.Canvas.RoundRect(0, 0, TmpB.Width, TmpB.Height, 8, 8);
  CDButton.SetShape(TmpB);
  ADest.Draw(0, 0, TmpB);
  TmpB.Free;
  }

  // Button image
  case FState of
    bbsDown:
    begin

    end;
    bbsFocused:
    begin
      DrawAndroidButton(ADest, CDButton.Color);
    end;
    else
      DrawAndroidButton(ADest, CDButton.Color);
  end;

  // Button text
  {$ifndef CUSTOMDRAWN_USE_FREETYPE}
  ADest.Font.Assign(CDButton.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := CDButton.Caption;
  ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2,
    (CDButton.Height - ADest.TextHeight(Str)) div 2, Str);
  {$endif}
end;

{ TCDGroupBox }

procedure TCDGroupBox.PrepareCurrentDrawer();
begin
  case DrawStyle of
    dsWince: FCurrentDrawer := FDrawerWinCE;
    dsCustom: FCurrentDrawer := CustomDrawer;
  end;
end;

procedure TCDGroupBox.SetDrawStyle(const AValue: TCDDrawStyle);
begin
  if FDrawStyle = AValue then
    exit;
  FDrawStyle := AValue;

  Invalidate;

  PrepareCurrentDrawer();
  FCurrentDrawer.SetClientRectPos(Self);
end;

constructor TCDGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TabStop := False;

  FDrawerWinCE := TCDGroupBoxDrawerWinCE.Create;
end;

destructor TCDGroupBox.Destroy;
begin
  inherited Destroy;
end;

procedure TCDGroupBox.EraseBackground(DC: HDC);
begin

end;

procedure TCDGroupBox.Paint;
var
  AImage: TLazIntfImage = nil;
  ABmp: TBitmap = nil;
  lCanvas: TFPImageCanvas = nil;
begin
  inherited Paint;

  PrepareCurrentDrawer();

  ABmp := TBitmap.Create;
  try
    ABmp.Width := Width;
    ABmp.Height := Height;
    AImage := ABmp.CreateIntfImage;
    lCanvas := TFPImageCanvas.Create(AImage);
    // First step of the drawing: FCL TFPCustomCanvas for fast pixel access
    FCurrentDrawer.DrawToIntfImage(lCanvas, Self);
    ABmp.LoadFromIntfImage(AImage);
    // Second step of the drawing: LCL TCustomCanvas for easy font access
    FCurrentDrawer.DrawToCanvas(ABmp.Canvas, Self);
    Canvas.Draw(0, 0, ABmp);
  finally
    if lCanvas <> nil then
      lCanvas.Free;
    if AImage <> nil then
      AImage.Free;
    ABmp.Free;
  end;
end;

{ TCDGroupBoxDrawerWinCE }

procedure TCDGroupBoxDrawerWinCE.SetClientRectPos(CDGroupBox: TCDGroupBox);
var
  lRect: TRect;
  lCaptionHeight: integer;
begin
  lCaptionHeight := 10;
  lRect := Rect(1, lCaptionHeight, CDGroupBox.Width - 1, CDGroupBox.Height - 1);
  CDGroupBox.AdjustClientRect(lRect);
end;

procedure TCDGroupBoxDrawerWinCE.DrawToIntfImage(ADest: TFPImageCanvas;
  CDGroupBox: TCDGroupBox);
{$ifdef CUSTOMDRAWN_USE_FREETYPE}
var
  AFont: TFreeTypeFont = nil;
{$endif}
begin
  FCaptionMiddle := CDGroupBox.Canvas.TextHeight('Ź') div 2;

  // Background
  if CDGroupBox.Parent = nil then
    ADest.Brush.FPColor := colLtGray
  else
    ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(CDGroupBox.Parent.Color));
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.Rectangle(0, 0, CDGroupBox.Width, CDGroupBox.Height);

  // frame
  ADest.Pen.FPColor := colBlack;
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Rectangle(0, FCaptionMiddle, CDGroupBox.Width - 1, CDGroupBox.Height - 1);

  {$ifdef CUSTOMDRAWN_USE_FREETYPE}
  // Caption background and caption

  // initialize free type font manager
  opcftfont.InitEngine;
  //  FontMgr.SearchPath:='/usr/share/fonts/truetype/';
  AFont := TFreeTypeFont.Create;
  try
    // Text background
    ADest.Pen.Style := psClear;
    ADest.Brush.Style := bsSolid;
    // The brush color was already set previously and is already correct
    //    ADest.Rectangle(5, 0, AFont.GetTextWidth(CDGroupBox.Caption) + 5, 10);

    // paint text
    ADest.Pen.Style := psSolid;
    ADest.Brush.Style := bsClear;
    ADest.Font := AFont;
    ADest.Font.Name := 'Arial';
    ADest.Font.Size := 10;
    ADest.TextOut(5, 10, CDGroupBox.Caption);
  finally
    AFont.Free;
  end;
  {$endif}
end;

procedure TCDGroupBoxDrawerWinCE.DrawToCanvas(ADest: TCanvas; CDGroupBox: TCDGroupBox);
begin
  {$ifndef CUSTOMDRAWN_USE_FREETYPE}
  if CDGroupBox.Parent = nil then
    ADest.Brush.Color := clLtGray
  else
    ADest.Brush.Color := ColorToRGB(CDGroupBox.Parent.Color);

  // Text background
  ADest.Pen.Style := psClear;
  ADest.Brush.Style := bsSolid;
  ADest.Rectangle(FCaptionMiddle, 0, ADest.GetTextWidth(CDGroupBox.Caption) +
    FCaptionMiddle, 10);

  // paint text
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Font.Size := 10;
  ADest.TextOut(FCaptionMiddle, 0, CDGroupBox.Caption);
  {$endif}
end;

{ TCDTrackBar }

{procedure TCDTrackBar.SetMax(Value: Integer);
begin
  if Value = FMax then Exit;
  FMax := Value;
  Invalidate;
end;

procedure TCDTrackBar.SetMin(Value: Integer);
begin
  if Value = FMin then Exit;
  FMin := Value;
  Invalidate;
end;

procedure TCDTrackBar.SetPosition(Value: Integer);
begin
  if Value = FPosition then Exit;
  FPosition := Value;
  Invalidate;
end;

procedure TCDTrackBar.Changed;
begin

end;

procedure TCDTrackBar.DoEnter;
begin
  inherited DoEnter;
end;

procedure TCDTrackBar.DoExit;
begin
  inherited DoExit;
end;

procedure TCDTrackBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

procedure TCDTrackBar.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure TCDTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCDTrackBar.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TCDTrackBar.MouseLeave;
begin
  inherited MouseLeave;
end;

constructor TCDTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCDTrackBar.Destroy;
begin
  inherited Destroy;
end;

procedure TCDTrackBar.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TCDTrackBar.Paint;
begin
  inherited Paint;
end;}

{ TCDTrackBarDrawer }

//procedure TCDTrackBarDrawer.DrawToIntfImage(ADest: TLazIntfImage; CDTrackBar: TCDTrackBar);
//begin
//  inherited DrawToIntfImage(ADest);
//end;

end.

