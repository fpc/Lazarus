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
  Math, customdrawnutils,
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
  TCDButtonDrawerXPTB = class;

  TCDButton = class(TCustomControl)
  private
    FDrawStyle: TCDDrawStyle;
    FCurrentDrawer: TCDButtonDrawer;
    FDrawerWinCE: TCDButtonDrawerWinCE;
    FDrawerAndroid: TCDButtonDrawerAndroid;
    FDrawerXPTB: TCDButtonDrawerXPTB;
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

  TCDButtonDrawerXPTB = class(TCDButtonDrawer)
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

  TCDTrackBarDrawer = class;

{ TCDTrackBar }

  TCDTrackBar = class(TCustomControl)
  private
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FOnChange: TNotifyEvent;
    FCurrentDrawer: TCDTrackBarDrawer;
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
  end;

{ TCDTrackBarDrawer }

  TCDTrackBarDrawer = class
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDTrackBar: TCDTrackBar); virtual; abstract;
  end;

  TCDTrackBarDrawerGraph = class(TCDTrackBarDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDTrackBar: TCDTrackBar); override;
  end;

procedure Register;

implementation

const
  INT_BitmappedButton_LineSpacing = 2;

procedure Register;
begin
  RegisterComponents('Common Controls', [TCDButton]);
  RegisterComponents('Common Controls', [TCDTrackBar]);
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
    dsXPTaskbar: FCurrentDrawer := FDrawerXPTB;
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
  TabStop := True;
  FDrawerWinCE := TCDButtonDrawerWinCE.Create;
  FDrawerAndroid := TCDButtonDrawerAndroid.Create;
  FDrawerXPTB := TCDButtonDrawerXPTB.Create;
  Width := 120;
  Height := 43;
  //Color := clTeal;
  ParentFont := True;
  FDrawStyle := dsAndroid;
  Color := $00F1F5F5;
end;

destructor TCDButton.Destroy;
begin
  inherited Destroy;
end;

procedure TCDButton.EraseBackground(DC: HDC);
begin

end;

procedure DrawCDButtonDown(Canvas: TCanvas; CDButton: TCDButton);
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := CDButton.Color;
    Pen.Color := Brush.Color;
    Rectangle(0, 0, Width, Height);
    FillRect(0, 0, Width, Height);
    Brush.Color := GetAColor(CDButton.Color, 93);
    Pen.Color := GetAColor(Brush.Color, 76);
    RoundRect(0, 0, Width, Height, 8, 8);
  end;
end;

procedure TCDButton.Paint;
var
  AImage: TLazIntfImage = nil;
  ABmp: TBitmap = nil;
  lCanvas: TFPImageCanvas = nil;
  pColor: TColor;
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

    with ABmp.Canvas do
    begin
      Brush.Style := bsClear;
      if FState <> bbsDown then
      begin
        Pen.Color := GetAColor(Color, 86);
        RoundRect(0, 0, Width, Height, 8, 8);
      end;
      Pen.Style := psSolid;
      Pen.Color := Parent.Color;
      Line(0, 2, 0, 0);
      Line(0, 0, 2, 0);
      Pixels[2, 0] := Pen.Color;
      Line(Width - 3, 0, Width - 1, 0);
      Line(Width - 1, 0, Width - 1, 2);
      Line(0, Height - 3, 0, Height - 1);
      Line(0, Height - 1, 2, Height - 1);
      Line(Width - 1, Height - 3, Width - 1, Height - 1);
      Line(Width - 1, Height - 1, Width - 3, Height - 1);
      Pixels[Width - 1, 2] := Pen.Color;
      Pixels[Width - 3, Height - 1] := Pen.Color;
      Pixels[2, Height - 1] := Pen.Color;
      pColor := Parent.Color; //GetAColor(Parent.Color, 96);
      Pixels[1, 1] := pColor;
      Pixels[Width - 2, 1] := pColor;
      Pixels[Width - 2, Height - 2] := pColor;
      Pixels[1, Height - 2] := pColor;
    end;
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

procedure TCDButtonDrawerWinCE.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TBitmappedButtonState);
var
  TmpB: TBitmap;
  Str: string;
begin
  // Button shape -> This crashes in Gtk2
  TmpB := TBitmap.Create;
  TmpB.Width := CDButton.Width;
  TmpB.Height := CDButton.Height;
  TmpB.Canvas.Brush.Color := CDButton.Color;
  TmpB.Canvas.Brush.Style := bsSolid;
  TmpB.Canvas.RoundRect(0, 0, TmpB.Width, TmpB.Height, 8, 8);
  //  CDButton.SetShape(TmpB);

  with TmpB.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := CDButton.Parent.Color;
    Pen.Color := Brush.Color;
    Rectangle(0, 0, Width, Height);
    FillRect(0, 0, Width, Height);
    Brush.Color := GetAColor(CDButton.Color, 90);
  end;

  // Button image
  case FState of
    bbsDown:
    begin
      DrawCDButtonDown(TmpB.Canvas, CDButton);
    end;
    bbsFocused:
      //GradientFill(GetUColor(CDButton.Color, 50), GetAColor(CDButton.Color, 60), TmpB.Canvas);
      GradientFill(clWhite, GetAColor(CDButton.Color, 96), TmpB.Canvas);
    else
      //GradientFill(GetUColor(CDButton.Color, 10), GetAColor(CDButton.Color, 20), TmpB.Canvas);
      GradientFill(clWhite, CDButton.Color, TmpB.Canvas);
  end;

  ADest.Draw(0, 0, TmpB);

  TmpB.Free;

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

procedure TCDButtonDrawerAndroid.DrawToCanvas(ADest: TCanvas;
  CDButton: TCDButton; FState: TBitmappedButtonState);
var
  //TmpB: TBitmap;
  Str: string;
begin
  // Button shape -> This crashes in Gtk2
{  TmpB.Canvas.Brush.Color := CDButton.Color;
  TmpB.Canvas.Brush.Style := bsSolid;
  TmpB.Canvas.RoundRect(0, 0, TmpB.Width, TmpB.Height, 8, 8);
  CDButton.SetShape(TmpB);
  ADest.Draw(0, 0, TmpB);
  TmpB.Free;
  }

  ADest.Brush.Color := CDButton.Parent.Color;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Color := ADest.Brush.Color;
  ADest.RecTangle(0, 0, CDButton.Width, CDButton.Height);

  // Button image
  case FState of
    bbsDown:
    begin
      DrawCDButtonDown(ADest, CDButton);
    end;
    bbsFocused:
    begin
      DrawAndroidButton(ADest, GetAColor(CDButton.Color, 98));
    end;
    else
      DrawAndroidButton(ADest, GetAColor(CDButton.Color, 96));
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

procedure TCDButtonDrawerXPTB.SetClientRectPos(CDButton: TCDButton);
var
  lRect: TRect;
begin
  lRect := Rect(1, 1, CDButton.Width - 1, CDButton.Height - 1);
  CDButton.AdjustClientRect(lRect);
end;

procedure TCDButtonDrawerXPTB.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure DrawXPTaskbarButton(Canvas: TCanvas; Color: TColor);
var
  aColor: TColor;
begin
  aColor := GetUColor(Color, 96);
  with Canvas do
  begin
    Brush.Color := Color;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
    Pen.Color := aColor;
    RecTangle(0, 0, Width, Height);
    Pen.Color := GetAColor(aColor, 86);
    RoundRect(0, 0, Width, Canvas.Height, 8, 8);
//    Pen.Color := aColor;
//    RecTangle(0, 6, Width, Height);
    Pen.Color := GetAColor(aColor, 86);
    Line(0, 3, 0, Height - 3);
    Line(Width, 3, Width, Height - 3);
    Line(3, Height - 1, Width - 3, Height - 1);
    Line(2, Height - 2, Width - 2, Height - 2);
    Pen.Color := GetAColor(aColor, 93);
    Line(1, Height - 4, Width - 1, Height - 4);
    Pen.Color := GetAColor(aColor, 91);
    Line(1, Height - 3, Width - 1, Height - 3);
    Pen.Color := GetAColor(aColor, 88);
    Line(Width - 2, 4, Width - 2, Height - 3);
    //Pen.Color := GetAColor(aColor, 94);
    //Line(2, 2, 6, 2);
  end;
end;

procedure TCDButtonDrawerXPTB.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TBitmappedButtonState);
var
  Str: string;
begin
  case FState of
    bbsDown:
    begin
      DrawCDButtonDown(ADest, CDButton);
    end;
    bbsFocused:
    begin
      DrawXPTaskbarButton(ADest, GetAColor(CDButton.Color, 98));
    end;
    else
      DrawXPTaskbarButton(ADest, CDButton.Color);
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

  Width := 100;
  Height := 100;

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

procedure TCDTrackBar.SetMax(Value: Integer);
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
  //if Key =
end;

procedure TCDTrackBar.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure TCDTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FPosition := FMin + (X - 8) * (FMax - FMin) div (Width - 15);
  invalidate;
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
  Height := 25;
  Width := 100;
  FCurrentDrawer := TCDTrackBarDrawerGraph.Create;
  Color := clBtnFace;
  FMax := 100;
  FMin := 0;
  TabStop := True;
end;

destructor TCDTrackBar.Destroy;
begin
  FCurrentDrawer.Free;
  inherited Destroy;
end;

procedure TCDTrackBar.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TCDTrackBar.Paint;
var
  AImage: TLazIntfImage = nil;
  ABmp: TBitmap = nil;
  lCanvas: TFPImageCanvas = nil;
begin
  inherited Paint;
  ABmp := TBitmap.Create;
  try
    ABmp.Width := Width;
    ABmp.Height := Height;
    AImage := ABmp.CreateIntfImage;
    lCanvas := TFPImageCanvas.Create(AImage);
    // First step of the drawing: FCL TFPCustomCanvas for fast pixel access
    FCurrentDrawer.DrawToIntfImage(lCanvas, Self);
    ABmp.LoadFromIntfImage(AImage);
    Canvas.Draw(0, 0, ABmp);
  finally
    if lCanvas <> nil then
      lCanvas.Free;
    if AImage <> nil then
      AImage.Free;
    ABmp.Free;
  end;
end;

{ TCDTrackBarDrawer }

procedure TCDTrackBarDrawerGraph.DrawToIntfImage(ADest: TFPImageCanvas; CDTrackBar: TCDTrackBar);
var aStart, RNum, i, pStart: integer;
begin
  // Background
  if CDTrackBar.Parent = nil then
    ADest.Brush.FPColor := colLtGray
  else
    ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(CDTrackBar.Color));
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.Rectangle(0, 0, CDTrackBar.Width, CDTrackBar.Height);
  ADest.Brush.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
  aStart := CDTrackBar.Height div 2 + 1;
  ADest.Pen.Style := psSolid;
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
  //ADest.Rectangle(0, aStart, CDTrackBar.Width, aStart);
  ADest.Line(0, aStart, CDTrackBar.Width, aStart);
  ADest.Line(3, aStart - 1, 6, aStart - 1);
  ADest.Line(5, aStart - 2, 6, aStart - 2);
  ADest.Line(3, aStart + 1, 6, aStart + 1);
  ADest.Line(5, aStart + 2, 6, aStart + 2);
  pStart := ((CDTrackBar.Position - CDTrackBar.Min) * (CDTrackBar.Width - 15)) div (CDTrackBar.Max - CDTrackBar.Min);
  ADest.Rectangle(8 + pStart, aStart + 1, 8 + pStart + 10, aStart + 11);
  ADest.Line(CDTrackBar.Width - 1 - 3, aStart - 1, CDTrackBar.Width - 1 - 6, aStart - 1);
  ADest.Line(CDTrackBar.Width - 1 - 5, aStart - 2, CDTrackBar.Width - 1 - 6, aStart - 2);
  ADest.Line(CDTrackBar.Width - 1 - 3, aStart + 1, CDTrackBar.Width - 1 - 6, aStart + 1);
  ADest.Line(CDTrackBar.Width - 1 - 5, aStart + 2, CDTrackBar.Width - 1 - 6, aStart + 2);
  RNum := (CDTrackBar.Width - 15) div 9;
  ADest.Pen.FPColor :=  TColorToFPColor(ColorToRGB(clGray));
  ADest.Brush.FPColor := TColorToFPColor(ColorToRGB($00F0F0F0));
  for i := 0 to RNum - 1 do
  begin
    ADest.RecTangle(10 + i * 9, aStart - 3 - i, 10 + i * 9 + 9 - 2, aStart - 1);
  end;
end;

end.

