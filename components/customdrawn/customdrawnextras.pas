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
  Math, customdrawnutils, contnrs, componenteditors, LMessages, Messages,
  LCLProc, PropEdits,
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
    FMDown: boolean;
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FOnChange: TNotifyEvent;
    FCurrentDrawer: TCDTrackBarDrawer;
    FFromColor, FToColor: TColor;
    FStepWidth: integer;
    procedure SetMax(Value: integer);
    procedure SetMin(Value: integer);
    procedure SetPosition(Value: integer);
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
    procedure SetStepWidth(Value: integer);
  protected
    procedure Changed; virtual;
    // keyboard
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    // mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    xPosition: integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Color;
    property Max: integer read FMax write SetMax default 10;
    property Min: integer read FMin write SetMin default 0;
    property ColorFrom: TColor read FFromColor write SetFromColor;
    property StepWidth: integer read FStepWidth write SetStepWidth;
    property ColorTo: TColor read FToColor write SetToColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Position: integer read FPosition write SetPosition;
    property TabStop default True;
  end;

  { TCDTrackBarDrawer }

  TCDTrackBarDrawer = class
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTrackBar: TCDTrackBar; FromColor, ToColor: TColor; pWidth: integer);
      virtual; abstract;
  end;

  TCDTrackBarDrawerGraph = class(TCDTrackBarDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTrackBar: TCDTrackBar; FromColor, ToColor: TColor; pWidth: integer); override;
  end;

  { TCDTabSheet }

  TCDTabSheetDrawerGraph = class;
  TCDPageControl = class;

  TCDTabSheet = class(TCustomControl)
  private
    FCurrentDrawer: TCDTabSheetDrawerGraph;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Caption;
    property Color;
    property Font;
  end;

  { TCDTabSheetDrawer }

  TCDTabSheetDrawer = class
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTrackBar: TCDTabSheet); virtual; abstract;
  end;

  TCDTabSheetDrawerGraph = class(TCDTabSheetDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTabSheet: TCDTabSheet); override;
  end;

  { TCDPageControl }

  TCDPageControlDrawer = class;
  TCDPageControlDrawerWinCE = class;

  TCDPageControlEditor = class(TDefaultComponentEditor)
  public
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;

  TCDPageControl = class(TCustomControl)
  private
    FDrawStyle: TCDDrawStyle;
    FCaptionHeight: integer;
    FActivePage: TCDTabSheet;
    FCurrentDrawer: TCDPageControlDrawer;
    FDrawerWinCE: TCDPageControlDrawerWinCE;
    FGrad: boolean;
    FShowTabs: boolean;
    FPages: TList;
    FMDownL, FMDownR: Boolean;
    FMEnterL, FMEnterR: Boolean;
    FPageIndex: integer;
    procedure SetMouseUP;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    //procedure CNNotify(var Message: TLMNotify); message CN_NOTIFY;
    procedure PrepareCurrentDrawer();
    procedure SetDrawStyle(const AValue: TCDDrawStyle);
    procedure SetCaptionHeight(Value: integer);
    procedure DrawCaptionBar(ADest: TFPImageCanvas; FPImg: TLazIntfImage; lRect: TRect);
    procedure SetPageGradient(Value: boolean);
    procedure SetShowTabs(Value: boolean);
    procedure SetActivePage(Value: TCDTabSheet);
    procedure SetPageIndex(Value: integer);
    procedure UpdateAllDesignerFlags;
    procedure UpdateDesignerFlags(APageIndex: integer);
  protected
    procedure Loaded; override;
  public
    CustomDrawer: TCDPageControlDrawer; // Fill the field to use the dsCustom draw mode
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure DoOnResize; override;
    function FindNextPage(CurPage: TCDTabSheet;
      GoForward, CheckTabVisible: boolean): TCDTabSheet;
    procedure SelectNextPage(GoForward: boolean; CheckTabVisible: boolean = True);
    procedure SetCDPages(Value: TList);
  published
    property ActivePage: TCDTabSheet read FActivePage write SetActivePage;
    property DrawStyle: TCDDrawStyle read FDrawStyle write SetDrawStyle;
    property Caption;
    property CaptionHeight: integer read FCaptionHeight write SetCaptionHeight;
    property Color;
    property Font;
    property Gradient: boolean read FGrad write SetPageGradient;
    property Pages: TList read FPages write SetCDPages;
    property ParentColor;
    property ParentFont;
    property ShowTabs: boolean read FShowTabs write SetShowTabs;
    property TabStop default True;
  end;

  { TCDTrackBarDrawer }

  TCDPageControlDrawer = class
  public
    procedure SetClientRectPos(CDPageControl: TCDPageControl); virtual; abstract;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDPageControl: TCDPageControl); virtual; abstract;
    procedure DrawToCanvas(ADest: TCanvas; CDPageControl: TCDPageControl);
      virtual; abstract;
  end;

  TCDPageControlDrawerWinCE = class(TCDPageControlDrawer)
  public
    procedure SetClientRectPos(CDPageControl: TCDPageControl); override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDPageControl: TCDPageControl); override;
    procedure DrawToCanvas(ADest: TCanvas; CDPageControl: TCDPageControl); override;
  end;

procedure Register;

implementation

const
  INT_BitmappedButton_LineSpacing = 2;

resourcestring
  sTABSHEET_DEFAULT_NAME = 'CTabSheet';
  sNEW_PAGE = 'Ne&w Page';
  sDEL_PAGE = '&Delete Page';
  sNEXT_PAGE = 'Ne&xt Page';
  sPREV_PAGE = '&Previouse Page';

procedure Register;
begin
  RegisterComponents('Common Controls', [TCDButton]);
  RegisterComponents('Common Controls', [TCDTrackBar]);
  RegisterComponents('Common Controls', [TCDPageControl]);
  RegisterComponents('Common Controls', [TCDGroupBox]);
  RegisterComponentEditor(TCDPageControl, TCDPageControlEditor);
  RegisterComponentEditor(TCDTabSheet, TCDPageControlEditor);
  RegisterNoIcon([TCDTabSheet]);
  RegisterClasses([TCDTabSheet]);
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

  if (Key = VK_SPACE) or (Key = VK_RETURN) then
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
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csReplicatable];
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

procedure TCDTrackBar.SetMax(Value: integer);
begin
  if Value = FMax then
    Exit;
  FMax := Value;
  Invalidate;
end;

procedure TCDTrackBar.SetMin(Value: integer);
begin
  if Value = FMin then
    Exit;
  FMin := Value;
  Invalidate;
end;

procedure TCDTrackBar.SetPosition(Value: integer);
begin
  if Value = FPosition then
    Exit;
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

procedure TCDTrackBar.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = 37) or (Key = 40) then
    Position := Position - (FMax - FMin) div 10;
  if (Key = 38) or (Key = 39) then
    Position := Position + (FMax - FMin) div 10;
  if Position > FMax then
    Position := FMax;
  if Position < FMin then
    Position := FMin;
end;

procedure TCDTrackBar.KeyUp(var Key: word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure TCDTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  SetFocus;
  FPosition := FMin + (X - 8) * (FMax - FMin) div (Width - 14);
  if X > Width - 14 then
    FPosition := FMax;
  if X < 13 then
    FPosition := 0;
  xPosition := X;
  if X > Width - 14 then
    xPosition := Width - 14;
  if X < 13 then
    xPosition := 13;
  invalidate;
  FMDown := True;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FMDown := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCDTrackBar.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if FMDown then
  begin
    FPosition := FMin + (X - 8) * (FMax - FMin) div (Width - 14);
    if X > Width - 14 then
      FPosition := FMax;
    if X < 13 then
      FPosition := 0;
    xPosition := X;
    if X > Width - 14 then
      xPosition := Width - 14;
    if X < 13 then
      xPosition := 13;
    invalidate;
  end;
  inherited MouseMove(Shift, X, Y);
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
  FFromColor := clWhite;
  FToColor := clGray;
  FStepWidth := 11;
  TabStop := True;
  xPosition := 13;
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
    FCurrentDrawer.DrawToIntfImage(lCanvas, AImage, Self, FFromColor,
      FToColor, FStepWidth);
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

procedure TCDTrackBar.SetFromColor(Value: TColor);
begin
  FFromColor := Value;
  invalidate;
end;

procedure TCDTrackBar.SetToColor(Value: TColor);
begin
  FToColor := Value;
  invalidate;
end;

procedure TCDTrackBar.SetStepWidth(Value: integer);
begin
  FStepWidth := Value;
  invalidate;
end;

{ TCDTrackBarDrawer }

procedure TCDTrackBarDrawerGraph.DrawToIntfImage(ADest: TFPImageCanvas;
  FPImg: TLazIntfImage; CDTrackBar: TCDTrackBar; FromColor, ToColor: TColor;
  pWidth: integer);
var
  aStart, RNum, i, pStart: integer;
  dRect: TRect;
  TempB: TLazIntfImage;
  BCanvas: TFPImageCanvas;
  ABmp: TBitmap = nil;
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
  //aStart := CDTrackBar.Height div 2 + 1;
  aStart := CDTrackBar.Height - 10;
  ADest.Pen.Style := psSolid;
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
  //ADest.Rectangle(0, aStart, CDTrackBar.Width, aStart);
  ADest.Line(0, aStart, CDTrackBar.Width, aStart);
  ADest.Line(3, aStart - 1, 6, aStart - 1);
  ADest.Line(5, aStart - 2, 6, aStart - 2);
  ADest.Line(3, aStart + 1, 6, aStart + 1);
  ADest.Line(5, aStart + 2, 6, aStart + 2);
  //pStart := ((CDTrackBar.Position - CDTrackBar.Min) * (CDTrackBar.Width - 32)) div
  //  (CDTrackBar.Max - CDTrackBar.Min) + 2;
  pStart := CDTrackBar.xPosition;
  ADest.Rectangle(pStart - 5, aStart + 1, pStart + 5, aStart + 6);
  ADest.Line(CDTrackBar.Width - 1 - 3, aStart - 1, CDTrackBar.Width - 1 - 6, aStart - 1);
  ADest.Line(CDTrackBar.Width - 1 - 5, aStart - 2, CDTrackBar.Width - 1 - 6, aStart - 2);
  ADest.Line(CDTrackBar.Width - 1 - 3, aStart + 1, CDTrackBar.Width - 1 - 6, aStart + 1);
  ADest.Line(CDTrackBar.Width - 1 - 5, aStart + 2, CDTrackBar.Width - 1 - 6, aStart + 2);
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($005BA6C6));
  ADest.RecTangle(pStart - 5, aStart + 2, pStart + 5, aStart + 7);
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
  ADest.RecTangle(pStart - 5, aStart, pStart + 5, aStart + 2);
  RNum := (CDTrackBar.Width - 15) div pWidth;
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB(clGray));
  ADest.Brush.FPColor := TColorToFPColor(ColorToRGB($00F0F0F0));
  ABmp := TBitmap.Create;
  ABmp.Width := CDTrackBar.Width;
  ABmp.Height := CDTrackBar.Height;
  TempB := ABmp.CreateIntfImage;
  //TempB := TLazIntfImage.Create(0, 0);
  //TempB.UsePalette := False;
  //TempB.Width := CDTrackBar.Width;
  //TempB.Height := CDTrackBar.Height;
  BCanvas := TFPImageCanvas.Create(TempB);
  //BCanvas.Brush.FPColor := TColorToFPColor(ColorToRGB(clRed));
  BCanvas.Brush.Style := bsSolid;
  //GradHFill(BCanvas, Rect(0, 0, CDTrackBar.Width, CDTrackBar.Height),
  //  GetAColor(FromColor, 70 + i), GetAColor(ToColor, 90 + i));
  GradCenterFill(BCanvas, Rect(0, 0, CDTrackBar.Width, CDTrackBar.Height),
    FromColor, ToColor, CDTrackBar.Position / (CDTrackBar.Max - CDTrackBar.Min));
  for i := 0 to RNum - 1 do
  begin
    dRect := Rect(10 + i * pWidth, aStart - 5 - i, 10 + i * pWidth +
      pWidth - 3, aStart - 1);
    ADest.Brush.Style := bsSolid;
    //  GradHFill(ADest, dRect, GetAColor(FromColor, 70 + i), GetAColor(ToColor, 90 + i));
    if aStart - 5 - i > 0 then
    begin
      FPImgCloneRect(TempB, FPImg, Rect(10 + i * pWidth, aStart - 5 -
        i, 10 + i * pWidth + pWidth - 3, aStart - 1), False);
      ADest.Brush.Style := bsClear;
      ADest.RecTangle(10 + i * pWidth, aStart - 5 - i, 10 + i * pWidth +
        pWidth - 3, aStart - 1);
    end;
  end;
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($007BC6F6));
  ADest.Line(7, aStart - 1, CDTrackBar.Width - 8, aStart - 1);
  ADest.Line(7, aStart + 1, CDTrackBar.Width - 8, aStart + 1);
  ADest.Colors[2, aStart - 1] := ADest.Pen.FPColor;
  ADest.Colors[4, aStart - 2] := ADest.Pen.FPColor;
  ADest.Colors[2, aStart + 1] := ADest.Pen.FPColor;
  ADest.Colors[4, aStart + 2] := ADest.Pen.FPColor;
  ADest.Colors[6, aStart - 3] := ADest.Pen.FPColor;
  ADest.Colors[6, aStart + 3] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 2, aStart - 1] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 4, aStart - 2] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 2, aStart + 1] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 4, aStart + 2] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 6, aStart - 3] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 6, aStart + 3] := ADest.Pen.FPColor;
  //ADest.Draw(0,0,TempB);
  //ADest.CopyRect(0, 0, BCanvas, Rect(0, 0, CDTrackBar.Width, CDTrackBar.Height));
  //FPIMGCopyRect(TempB, FPImg, Rect(10, 10, CDTrackBar.Width - 11, CDTrackBar.Height - 11));
  BCanvas.Free;
  TempB.Free;
  ABmp.Free;
end;

{ TCDTabSheet }

constructor TCDTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Parent := TCDPageControl(AOwner);
  TabStop := False;
  ParentColor := True;
  parentFont := True;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csReplicatable];
  FCurrentDrawer := TCDTabSheetDrawerGraph.Create;
end;

destructor TCDTabSheet.Destroy;
begin
  FCurrentDrawer.Free;
  inherited Destroy;
end;

procedure TCDTabSheet.EraseBackground(DC: HDC);
begin

end;

procedure TCDTabSheet.Paint;
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
    FCurrentDrawer.DrawToIntfImage(lCanvas, AImage, Self);
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

procedure TCDTabSheetDrawerGraph.DrawToIntfImage(ADest: TFPImageCanvas;
  FPImg: TLazIntfImage; CDTabSheet: TCDTabSheet);
begin
  if TCDPageControl(CDTabSheet.Parent).Gradient then
    GradientFillFP(clWhite, GetUColor(CDTabSheet.Color, 96), ADest,
      Rect(0, 0, CDTabSheet.Width - 1, CDTabSheet.Height - 1))
  else
    ADest.Rectangle(0, 0, CDTabSheet.Width - 1, CDTabSheet.Height - 1);
end;

{ TCDPageControlEditor }

procedure TCDPageControlEditor.ExecuteVerb(Index: integer);
var
  NewPage: TCDTabSheet;
  PControl: TCDPageControl;
  Hook: TPropertyEditorHook;
  PageComponent: TPersistent;
begin
  if Component is TCDPageControl then
    PControl := TCDPageControl(Component)
  else
    if Component is TCDTabSheet then
      PControl := TCDPageControl(TCDTabSheet(Component).Parent);
  Hook:=nil;
  case Index of
    0:
    begin  //  New Page
      if not GetHook(Hook) then exit;
      NewPage := TCDTabSheet.Create(PControl.Owner);
      NewPage.Parent := PControl;
      with NewPage do
      begin
        Name := Designer.CreateUniqueComponentName(ClassName);
        //GetUniqueName(sTABSHEET_DEFAULT_NAME, PControl);
        Caption := Name;
        {Left := 0;
        Top := PControl.CaptionHeight + 1;
        Width := PControl.Width;
        Height := PControl.Height - PControl.CaptionHeight + 1;  }
        SetBounds(1, PControl.CaptionHeight + 1, PControl.Width - 3, PControl.Height - PControl.CaptionHeight - 4);
        if PControl.ActivePage <> nil then
          PControl.ActivePage.Hide;
        PControl.ActivePage := NewPage;
        Show;
      end;
      PControl.FPages.Add(NewPage);
      Hook.PersistentAdded(NewPage,true);
      Designer.Modified;
    end;
    1:
    begin  //  Delete Page
      with PControl do
      begin
        NewPage := ActivePage;
        //FPages.Remove(NewPage);
        if not GetHook(Hook) then exit;
        PageComponent := TPersistent(NewPage);
        Hook.DeletePersistent(PageComponent);
        if NewPage <> nil then
          NewPage.Free;
      end;
    end;
    2:
    begin  //  Next Page
      PControl.FindNextPage(PControl.ActivePage, True, False);
    end;
    3:
    begin  //  Previous Page
      PControl.FindNextPage(PControl.ActivePage, False, False);
    end;
  end;
  if Designer <> nil then
    Designer.Modified;
end;

function TCDPageControlEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := sNEW_PAGE;
    1: Result := sDEL_PAGE;
    2: Result := sNEXT_PAGE;
    3: Result := sPREV_PAGE;
  end;
end;

function TCDPageControlEditor.GetVerbCount: integer;
begin
  Result := 4;
end;

{ TCDPageControl }

procedure TCDPageControl.PrepareCurrentDrawer();
begin
  case DrawStyle of
    dsWince: FCurrentDrawer := FDrawerWinCE;
    dsCustom: FCurrentDrawer := CustomDrawer;
  end;
end;

procedure TCDPageControl.SetDrawStyle(const AValue: TCDDrawStyle);
begin
  if FDrawStyle = AValue then
    exit;
  FDrawStyle := AValue;
  Invalidate;
  PrepareCurrentDrawer();
  FCurrentDrawer.SetClientRectPos(Self);
end;

function TCDPageControl.FindNextPage(CurPage: TCDTabSheet;
  GoForward, CheckTabVisible: boolean): TCDTabSheet;
var
  I, StartIndex: integer;
begin
  if FPages.Count <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then
      if GoForward then StartIndex := FPages.Count - 1 else StartIndex := 0;
    I := StartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FPages.Count then I := 0;
      end else
      begin
        if I = 0 then I := FPages.Count;
        Dec(I);
      end;
      Result := TCDTabSheet(FPages[I]);
      if not CheckTabVisible or Result.Visible then Exit;
    until I = StartIndex;
  end;
  Result := nil;
end;

procedure TCDPageControl.SelectNextPage(GoForward: boolean;
  CheckTabVisible: boolean = True);
var
  Page: TCDTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward, CheckTabVisible);
  if (Page <> nil) and (Page <> ActivePage) then
    SetActivePage(Page);
end;

constructor TCDPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 232;
  Height := 184;
  TabStop := False;
  FDrawerWinCE := TCDPageControlDrawerWinCE.Create;
  FCaptionHeight := 28;
  ParentColor := True;
  parentFont := True;
  FGrad := True;
  ControlStyle := [];
  FPages := TList.Create;
end;

destructor TCDPageControl.Destroy;
begin
  FPages.Free;
  inherited Destroy;
end;

procedure TCDPageControl.SetActivePage(Value: TCDTabSheet);
var i: integer;
begin
  for i := 0 to FPages.Count - 1 do
  begin
    if TCDTabSheet(FPages[i]) = Value then
      Show
    else
      Hide;
  end;
  FActivePage := Value;
  Value.BringToFront;
end;

procedure TCDPageControl.SetPageIndex(Value: integer);
begin
  FPageIndex := Value;
  ActivePage := TCDTabSheet(Pages[Value]);
  Invalidate;
end;

procedure TCDPageControl.EraseBackground(DC: HDC);
begin

end;

procedure TCDPageControl.Paint;
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
    FCurrentDrawer.DrawToIntfImage(lCanvas, AImage, Self);
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

procedure TCDPageControl.SetCaptionHeight(Value: integer);
begin
  FCaptionHeight := Value;
  invalidate;
end;

procedure TCDPageControl.SetPageGradient(Value: boolean);
begin
  FGrad := Value;
  invalidate;
end;

procedure TCDPageControl.SetShowTabs(Value: boolean);
begin
  FShowTabs := Value;
  invalidate;
end;

procedure TCDPageControl.DrawCaptionBar(ADest: TFPImageCanvas;
  FPImg: TLazIntfImage; lRect: TRect);
var
  aRect: TRect; i: integer; rWidth: integer; aText: string;
begin
  aRect := lRect;
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($009C9B91));
  aRect.Left := lRect.Left;
  aRect.Top := lRect.Top;
  aRect.Bottom := lRect.Bottom;
  aRect.Right := lRect.Right;
  if Owner.ComponentCount = 0 then
  begin
    ADest.RecTangle(aRect);
    Exit;
  end;
  for i := 0 to Owner.ComponentCount - 1 do
  begin
    if Owner.Components[i].GetParentComponent = Self then
    begin
      aText := TCDTabSheet(Owner.Components[i]).Caption;
      rWidth := 6 + Length(aText) * 9; //TCDTabSheet(FPages[i]).Font.Size;
      if aRect.Left + rWidth > lRect.Right - 6 then
        break
      else
        aRect.Right := aRect.Left + rWidth;
      ADest.RecTangle(aRect);
      //ADest.TextOut(aRect.Left + 3, aRect.Top + 3, aText);
      aRect.Left := aRect.Right;
    end;
  end;
  aRect.Left := lRect.Right - 51;
  aRect.Top := 1;
  aRect.Bottom := 25;
  aRect.Right := lRect.Right - 26;
  GradFill(ADest, aRect, $00FDD9CB, $00F2C9B8);
  aRect.Left := lRect.Right - 25;
  aRect.Top := 1;
  aRect.Bottom := 25;
  aRect.Right := lRect.Right;
  GradFill(ADest, aRect, $00FDD9CB, $00F2C9B8);
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB(clWhite));
  ADest.Line(lRect.Right - 51, 1, lRect.Right, 1);
  ADest.Line(lRect.Right, 1, lRect.Right, 25);
  ADest.Line(lRect.Right, 25, lRect.Right - 51, 25);
  ADest.Line(lRect.Right - 51, 25, lRect.Right - 51, 1);
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($00FFFFFF));
  //ADest.Line();
end;

procedure TCDPageControl.SetCDPages(Value: TList);
begin
  FPages.Assign(Value);
end;

procedure TCDPageControl.DoOnResize;
begin
  if ActivePage <> nil then
  begin
    ActivePage.Left := 1;
    ActivePage.Top := CaptionHeight + 1;
    ActivePage.Width := Width - 3;
    ActivePage.Height := Height - CaptionHeight - 4;
  end;
end;

procedure TCDPageControl.UpdateAllDesignerFlags;
var
  i: integer;
begin
  for i:=0 to FPages.Count-1 do
    UpdateDesignerFlags(i);
end;

procedure TCDPageControl.UpdateDesignerFlags(APageIndex: integer);
begin
  if APageIndex<>fPageIndex then
    TCDTabSheet(FPages[APageIndex]).ControlStyle:=
      TCDTabSheet(FPages[APageIndex]).ControlStyle+[csNoDesignVisible]
  else
    TCDTabSheet(FPages[APageIndex]).ControlStyle:=
      TCDTabSheet(FPages[APageIndex]).ControlStyle-[csNoDesignVisible];
end;

{procedure TCDPageControl.CNNotify(var Message: TLMNotify);
var
  OldPageIndex: LongInt;
begin
  with Message do
    case NMHdr^.code of
      TCN_SELCHANGE:
        begin
          // set the page from the NMHDR^.idfrom
          if not (csDestroyingHandle in ControlState) then
          begin
            OldPageIndex := FPageIndex;
            FPageIndex := PtrInt(NMHDR^.idfrom);
            if FPageIndex >= FPages.Count then
              FPageIndex := -1;
            //debugln(['TCDPageControl.CNNotify ',DbgSName(Self),' A Old=',OldPageIndex,' fPageIndex=',fPageIndex,' FLoadedPageIndex=',FLoadedPageIndex]);
            //if PageIndex>=0 then DebugLn(['TCDPageControl.CNNotify Page=',DbgSName(Page[PageIndex]),' Visible=',Page[PageIndex].Visible]);
            UpdateAllDesignerFlags;
            if ([csLoading,csDestroying]*ComponentState=[]) then
            begin
              if OldPageIndex <> FPageIndex then
              begin
                if csDesigning in ComponentState then
                  OwnerFormDesignerModified(Self);
                //DebugLn(['TCustomNotebook.CNNotify ',DbgSName(Page[PageIndex]),' ',Page[PageIndex].Visible]);
             //   Change;
              end;
            end;
          end;
        end;
      TCN_SELCHANGING:
        begin
        {  if CanChangePageIndex and not
          (csDestroyingHandle in ControlState) then
            Result := 0
          else
            Result := 1;       }
        end;
    else
      begin
        {$IFDEF NOTEBOOK_DEBUG}
        DebugLn(['[TCDPageControl.CNNotify] unhandled NMHdr code:', NMHdr^.code]);
        {$ENDIF}
      end;
    end;
end;       }

procedure TCDPageControl.SetMouseUP;
begin
  FMDownL := False;
  FMDownR := False;
end;

procedure TCDPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if (X > Width - 36) and (X < Width - 18) then
    FMDownL := True
  else
    if (X > Width - 36) and (X < Width - 18) then
     FMDownR := True
    else
      SetMouseUP;
  if (Y < 3) or (Y > 18) then
    SetMouseUP;
  if FMDownR or FMDownL then
    invalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  SetMouseUP;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCDPageControl.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if (X > Width - 36) and (X < Width - 18) then
    FMEnterL := True
  else
    if (X > Width - 36) and (X < Width - 18) then
      FMEnterR := True
    else
    begin
      FMEnterR := False;
      FMEnterL := False;
    end;
  if (Y < 3) or (Y > 18) then
  begin
    FMEnterR := False;
    FMEnterL := False;
  end;
  if FMEnterR or FMENterL then
    invalidate;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCDPageControl.Loaded;
begin
  inherited;
end;

procedure TCDPageControl.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TCDPageControl.MouseLeave;
begin
  inherited MouseLeave;
end;

{ TCDPageControlDrawerWinCE }

procedure TCDPageControlDrawerWinCE.SetClientRectPos(CDPageControl: TCDPageControl);
var
  lRect: TRect;
  lCaptionHeight: integer;
begin
  lCaptionHeight := CDPageControl.CaptionHeight;
  lRect := Rect(10, lCaptionHeight + 1, CDPageControl.Width - 10,
    CDPageControl.Height - 1);
  CDPageControl.AdjustClientRect(lRect);
end;

procedure TCDPageControlDrawerWinCE.DrawToIntfImage(ADest: TFPImageCanvas;
  FPImg: TLazIntfImage; CDPageControl: TCDPageControl);
begin
  ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(CDPageControl.Color));
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(CDPageControl.Color));
  ADest.Rectangle(0, 0, CDPageControl.Width, CDPageControl.Height);
  ADest.Font.Name := CDPageControl.Font.Name;
  ADest.Font.Size := CDPageControl.Font.Size;
  CDPageControl.DrawCaptionBar(ADest, FPImg, Rect(0, 0, CDPageControl.Width -
    2, CDPageControl.CaptionHeight + 1));
  if CDPageControl.Gradient then
    GradientFillFP(clWhite, GetUColor(CDPageControl.Color, 96), ADest,
      Rect(0, CDPageControl.CaptionHeight, CDPageControl.Width, CDPageControl.Height))
  else
    ADest.Rectangle(0, CDPageControl.CaptionHeight, CDPageControl.Width,
      CDPageControl.Height);
  ADest.Colors[CDPageControl.Width - 1, CDPageControl.CaptionHeight] :=
    TColorToFPColor(ColorToRGB(CDPageControl.Color));
  // frame
  //ADest.Pen.FPColor := colBlack;
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($009C9B91));
  //  ADest.Rectangle(0,0,CDPageControl.Width-2,CDPageControl.Height-2);
  ADest.Rectangle(0, CDPageControl.CaptionHeight, CDPageControl.Width -
    2, CDPageControl.Height - 2);
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($00BFCED0));
  ADest.Line(CDPageControl.Width - 1, CDPageControl.CaptionHeight + 1,
    CDPageControl.Width - 1, CDPageControl.Height - 1);
  ADest.Line(CDPageControl.Width - 1, CDPageControl.Height - 1, 1,
    CDPageControl.Height - 1);
end;

procedure TCDPageControlDrawerWinCE.DrawToCanvas(ADest: TCanvas;
  CDPageControl: TCDPageControl);
begin

end;

end.

