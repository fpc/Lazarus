{
  Copyright (C) 2011 Felipe Monteiro de Carvalho

  License: The same modifying LGPL with static linking exception as the LCL

  This unit should be a repository for various custom drawn components,
  such as a custom drawn version of TButton, of TEdit, of TPageControl, etc,
  eventually forming a full set of custom drawn components.
}
unit customdrawncontrols;

{$mode objfpc}{$H+}

interface

uses
  // FPC
  Classes, SysUtils, contnrs, Math, types,
  // LCL
  Graphics, Controls, LCLType, LCLIntf, IntfGraphics,
  customdrawnutils, LMessages, Messages,
  LCLProc, PropEdits, ExtCtrls, ImgList, Forms, Menus,
  // fpimage
  fpcanvas, fpimgcanv, fpimage
  {$ifdef CUSTOMDRAWN_USE_FREETYPE}
  // font support
  , ftfont
  {$endif}  ;

type

  TCDDrawStyle = (
    // Operating system styles
    dsWinCE, dsWin2000, dsAndroid, dsXPTaskBar,
    // Other special styles
    dsGrad,
    // Defined by the user
    dsCustom);

  // commented items are not yet supported
  TCDButtonState = (bbsNormal, bbsDown, bbsMouseOver, bbsFocused
    (* bbsChecked, bbsCheckedSelected, bbsCheckedDown { is going to be unchecked }*));

  TCDControlDrawer = class;

  { TCDControl }

  TCDControl = class(TCustomControl)
  protected
    FDrawStyle: TCDDrawStyle;
    FCurrentDrawer: TCDControlDrawer;
    //constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
    procedure PrepareCurrentDrawer(); virtual;
    procedure SetDrawStyle(const AValue: TCDDrawStyle);
    function GetClientRect: TRect; override;
    procedure EraseBackground(DC: HDC); override;
    property DrawStyle: TCDDrawStyle read FDrawStyle write SetDrawStyle;
  public
  end;

  TCDControlDrawer = class
  public
    function GetClientRect(AControl: TCDControl): TRect; virtual; abstract;
    //procedure DrawToIntfImage(ADest: TFPImageCanvas; AControl: TCDControl);
    //  virtual; abstract;
    //procedure DrawToCanvas(ADest: TCanvas; AControl: TCDControl); virtual; abstract;
  end;

  TCDButtonDrawer = class;
  TCDButtonDrawerWinCE = class;
  TCDButtonDrawerAndroid = class;
  TCDButtonDrawerXPTB = class;
  TCDButtonDrawerGrad = class;
  TCDButtonDrawerWin2k = class;

  { TCDButton }

  TCDButton = class(TCDControl)
  private
    //FCurrentDrawer: TCDButtonDrawer;
    FDrawerWinCE: TCDButtonDrawerWinCE;
    FDrawerAndroid: TCDButtonDrawerAndroid;
    FDrawerXPTB: TCDButtonDrawerXPTB;
    FDrawerGrad: TCDButtonDrawerGrad;
    FDrawerWin2k: TCDButtonDrawerWin2k;
    procedure PrepareCurrentDrawer(); override;
  protected
    FState: TCDButtonState;
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

  TCDButtonDrawer = class(TCDControlDrawer)
  public
    function GetClientRect(AControl: TCDControl): TRect; override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton);
      virtual; abstract;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TCDButtonState); virtual; abstract;
  end;

  { TCDButtonDrawerWinCE }

  TCDButtonDrawerWinCE = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TCDButtonState); override;
  end;

  { TCDButtonDrawerAndroid }
  TCDButtonDrawerAndroid = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TCDButtonState); override;
  end;

  TCDButtonDrawerXPTB = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TCDButtonState); override;
  end;

  TCDButtonDrawerGrad = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TCDButtonState); override;
  end;

  TCDButtonDrawerWin2k = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TCDButtonState); override;
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
  protected
    procedure RealSetText(const Value: TCaption); override; // to update on caption changes
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
    DragDropStarted: boolean;
    // fields
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FOnChange: TNotifyEvent;
    FCurrentDrawer: TCDTrackBarDrawer;
    procedure SetMax(Value: integer);
    procedure SetMin(Value: integer);
    procedure SetPosition(Value: integer);
    //
    function GetPositionFromMousePos(X, Y: Integer): integer;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Color;
    property Max: integer read FMax write SetMax default 10;
    property Min: integer read FMin write SetMin default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Position: integer read FPosition write SetPosition;
    property TabStop default True;
  end;

  { TCDTrackBarDrawer }

  TCDTrackBarDrawer = class
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTrackBar: TCDTrackBar); virtual; abstract;
    procedure GetGeometry(var ALeftBorder, ARightBorder: Integer); virtual; abstract;
  end;

  { TCDTrackBarDrawerGraph }

  TCDTrackBarDrawerGraph = class(TCDTrackBarDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTrackBar: TCDTrackBar); override;
    procedure GetGeometry(var ALeftBorder, ARightBorder: Integer); override;
  end;

  {TCDTabControl}

  { TCDCustomTabControl }

  TCDCustomTabControl = class;
  TCDCustomTabControlDrawer = class;
  TCDCustomTabControlDrawerWinCE = class;

  { TCDCustomTabSheet }

  TCDCustomTabSheet = class(TCustomControl)
  private
    CDTabControl: TCDCustomTabControl;
    FTabVisible: Boolean;
  protected
    procedure RealSetText(const Value: TCaption); override; // to update on caption changes
  public
    destructor Destroy; override;
    property TabVisible: Boolean read FTabVisible write FTabVisible;
  end;

  TCDCustomTabControl = class(TCDControl)
  private
    FTabIndex: Integer;
    FTabs: TStringList;
    FDrawerWinCE: TCDCustomTabControlDrawerWinCE;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    //procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    //procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    //procedure MouseEnter; override;
    //procedure MouseLeave; override;
    procedure PrepareCurrentDrawer(); override;
    procedure SetTabIndex(AValue: Integer); virtual;
    procedure SetTabs(AValue: TStringList);
  protected
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    function GetTabCount: Integer;
    procedure CorrectTabIndex();
  public
    CustomDrawer: TCDCustomTabControlDrawer; // Fill the field to use the dsCustom draw mode
    property Tabs: TStringList read FTabs write SetTabs;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property TabIndex: integer read FTabIndex write SetTabIndex;
  end;

  { TCDCustomTabControlDrawer }

  TCDCustomTabControlDrawer = class(TCDControlDrawer)
  public
    CDTabControl: TCDCustomTabControl;
    function GetPageIndexFromXY(x, y: integer): integer; virtual; abstract;
    function GetTabHeight(AIndex: Integer): Integer;  virtual; abstract;
    function GetTabWidth(ADest: TCanvas; AIndex: Integer): Integer; virtual; abstract;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage); virtual; abstract;
    procedure DrawToCanvas(ADest: TCanvas); virtual; abstract;
    procedure DrawTabSheet(ADest: TCanvas); virtual; abstract;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); virtual; abstract;
  end;

  { TCDCustomTabControlDrawerWinCE }

  TCDCustomTabControlDrawerWinCE = class(TCDCustomTabControlDrawer)
  private
    StartIndex: integer;       //FEndIndex
    LeftmostTabVisibleIndex: Integer;
    procedure DrawCaptionBar(ADest: TCanvas; lRect: TRect; CL: TColor);
    procedure DrawTabs(ADest: TCanvas);
    procedure DrawTab(ADest: TCanvas; AIndex: Integer; ACurStartLeftPos: Integer);
  public
    function GetPageIndexFromXY(x, y: integer): integer; override;
    function GetTabHeight(AIndex: Integer): Integer; override;
    function GetTabWidth(ADest: TCanvas; AIndex: Integer): Integer; override;
    //function GetClientRect(AControl: TCDControl): TRect; override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage); override;
    procedure DrawToCanvas(ADest: TCanvas); override;
    procedure DrawTabSheet(ADest: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
  end;

//  TTabSelectedEvent = procedure(Sender: TObject; ATab: TTabItem;
//    ASelected: boolean) of object;

  TCDTabControl = class(TCDCustomTabControl)
  published
    property Color;
    property Font;
    property Tabs;
    property TabIndex;
    property OnChanging;
    property OnChange;
  end;

  { TCDTabSheet }

  TCDPageControl = class;

  TCDTabSheet = class(TCDCustomTabSheet)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Caption;
    property Color;
    property Font;
    property TabVisible: Boolean;
  end;

  { TCDPageControl }

  TCDPageControl = class(TCDCustomTabControl)
  private
    function GetActivePage: TCDTabSheet;
    function GetPageCount: integer;
    function GetPageIndex: integer;
    procedure SetActivePage(Value: TCDTabSheet);
    procedure SetPageIndex(Value: integer);
    procedure UpdateAllDesignerFlags;
    procedure UpdateDesignerFlags(APageIndex: integer);
    procedure PositionTabSheet(ATabSheet: TCDTabSheet);
  protected
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function InsertPage(aIndex: integer; S: string): TCDTabSheet;
    procedure RemovePage(aIndex: integer);
    function AddPage(S: string): TCDTabSheet;
    function GetPage(aIndex: integer): TCDTabSheet;
    property PageCount: integer read GetPageCount;
    // Used by the property editor in customdrawnextras
    function FindNextPage(CurPage: TCDTabSheet;
      GoForward, CheckTabVisible: boolean): TCDTabSheet;
    procedure SelectNextPage(GoForward: boolean; CheckTabVisible: boolean = True);
  published
    property ActivePage: TCDTabSheet read GetActivePage write SetActivePage;
    property DrawStyle: TCDDrawStyle;
    property Caption;
    property Color;
    property Font;
    property PageIndex: integer read GetPageIndex write SetPageIndex;
    property ParentColor;
    property ParentFont;
    property TabStop default True;
    property TabIndex;
    property OnChanging;
    property OnChange;
  end;

implementation

resourcestring
  sTABSHEET_DEFAULT_NAME = 'CTabSheet';

{ TCDCustomTabSheet }

procedure TCDCustomTabSheet.RealSetText(const Value: TCaption);
var
  lIndex: Integer;
begin
  inherited RealSetText(Value);
  lIndex := CDTabControl.Tabs.IndexOfObject(Self);
  if lIndex >= 0 then
    CDTabControl.Tabs.Strings[lIndex] := Value;
  CDTabControl.Invalidate;
end;

destructor TCDCustomTabSheet.Destroy;
var
  lIndex: Integer;
begin
  // We should support deleting the tabsheet directly too,
  // and then it should update the tabcontrol
  // This is important mostly for the designer
  if CDTabControl <> nil then
  begin
    lIndex := CDTabControl.FTabs.IndexOfObject(Self);
    if lIndex >= 0 then
    begin
      CDTabControl.FTabs.Delete(lIndex);
      CDTabControl.CorrectTabIndex();
    end;
  end;

  inherited Destroy;
end;

{ TCDCustomTabControlDrawerWinCE }

procedure TCDCustomTabControlDrawerWinCE.DrawCaptionBar(ADest: TCanvas;
  lRect: TRect; CL: TColor);
begin
  {  CaptionHeight := GetTabHeight(CDPageControl.PageIndex, CDPageControl) - 4;
    RButtHeight := GetTabHeight(CDPageControl.PageIndex, CDPageControl);
    aRect := lRect;
    ADest.Pen.Style := psSolid;
    ADest.Brush.Style := bsSolid;
    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB(CL));
    //TColorToFPColor(ColorToRGB($009C9B91));
    ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(CL));
    aRect.Left := lRect.Left;
    aRect.Top := lRect.Top;
    aRect.Bottom := lRect.Bottom;
    aRect.Right := lRect.Right;
    ADest.RecTangle(lRect);
    if CDPageControl.FPages.Count = 0 then
    begin
      ADest.Brush.Color := clWhite;
      ADest.Pen.Color := $009C9B91;
      ADest.RecTangle(Rect(aRect.Left, aRect.Top, aRect.Right + 1, aRect.Bottom + 2));
      ADest.Pen.Color := clWhite;
      ADest.Line(aRect.Left + 1, aRect.Bottom + 1, aRect.Right, aRect.Bottom + 1);
      Exit;
    end;
    aRect.Left := lRect.Left + 2;
    aRect.Top := lRect.Top + 3;
    //ADest.TextStyle.Opaque :=false;
    //SetBkMode(ADest.Handle, TRANSPARENT);
    if ADest.Brush.Style = bsSolid then
      SetBkMode(ADest.Handle, OPAQUE)
    else
      SetBkMode(ADest.Handle, TRANSPARENT);

    for i := StartIndex to CDPageControl.FPages.Count - 1 do
    begin
      aText := CDPageControl.FPages[i].TabPage.Caption;
      rWidth := (CaptionHeight - ADest.TextHeight(aText)) + ADest.TextWidth(aText);
      CDPageControl.FPages[i].Width := rWidth;
      if aRect.Left + rWidth > lRect.Right - 6 then
        Break
      else
        aRect.Right := aRect.Left + rWidth;
      if CDPageControl.PageIndex = i then
      begin
        cRect := aRect;
        if i = StartIndex then
          cRect.Left := aRect.Left - 2
        else
          cRect.Left := aRect.Left - 4;
        cRect.Right := aRect.Right + 4;
        cRect.Top := cRect.Top - 2;
        bText := CDPageControl.FPages[i].TabPage.Caption;
      end
      else
        DrawTabHead(aDest, aRect, CDPageControl.Color, False);
      MaskColor := MaskBaseColor + i - StartIndex;
      //DrawTabHeadMask(MaskHeadBmp.Canvas, aRect, MaskColor, False);
      ADest.TextOut(aRect.Left + (aRect.Right - aRect.Left - ADest.TextWidth(aText)) div 2,
        aRect.Top + (aRect.Bottom - aRect.Top - ADest.TextHeight(aText)) div 2, aText);
      aRect.Left := aRect.Right + 3;
    end;
    ADest.Line(lRect.Left, lRect.Bottom - 1, cRect.Left, lRect.Bottom - 1);
    ADest.Line(cRect.Right, lRect.Bottom - 1, lRect.Right, lRect.Bottom - 1);
    DrawTabHead(aDest, cRect, clWhite, True);
    ADest.TextOut(cRect.Left + (cRect.Right - cRect.Left - ADest.TextWidth(bText)) div 2,
      cRect.Top + (cRect.Bottom - cRect.Top - ADest.TextHeight(bText)) div 2, bText);
    if not CheckTabButton(lRect.Right - lRect.Left, CDPageControl.FPages) then
      Exit;
    aRect.Left := lRect.Right - RButtHeight * 2 - 3;
    aRect.Top := 1;
    aRect.Bottom := RButtHeight + 1;
    aRect.Right := lRect.Right - RButtHeight;
    //if FMDownL then
    //  GradFill(ADest, aRect, $00F1A079, $00EFAF9B)
    //else
      GradFill(ADest, aRect, $00FDD9CB, $00F2C9B8);
    aRect.Left := lRect.Right - RButtHeight - 1;
    aRect.Top := 1;
    aRect.Bottom := RButtHeight + 1;
    aRect.Right := lRect.Right;

    GradFill(ADest, aRect, $00FDD9CB, $00F2C9B8);

    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($0085614D));
    bRect.Top := 1;
    bRect.Left := lRect.Right - RButtHeight * 2 - 3;
    bRect.Right := lRect.Right;
    bRect.Bottom := RButtHeight + 1;
    DrawArrow(ADest, bRect, True);
    DrawArrow(ADest, bRect, False);
    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB(clWhite));
    ADest.Line(lRect.Right - RButtHeight * 2 - 3, 1, lRect.Right, 1);
    ADest.Line(lRect.Right, 1, lRect.Right, RButtHeight + 1);
    ADest.Line(lRect.Right, RButtHeight + 1, lRect.Right - RButtHeight *
      2 - 3, RButtHeight + 1);
    ADest.Line(lRect.Right - RButtHeight * 2 - 3, RButtHeight + 1,
      lRect.Right - RButtHeight * 2 - 3, 1);
    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($00E5BAA7));
    ADest.Brush.Style := bsClear;
    ADest.Rectangle(lRect.Right - RButtHeight * 2 - 2, 2, lRect.Right -
      1, RButtHeight + 1);
    CornerColor := TColorToFPColor(ColorToRGB($00F6E3D9));
    ADest.Colors[lRect.Right - RButtHeight * 2 - 2, 2] := CornerColor;
    ADest.Colors[lRect.Right - RButtHeight * 2 - 2, RButtHeight] := CornerColor;
    ADest.Colors[lRect.Right - 1, 2] := CornerColor;
    ADest.Colors[lRect.Right - 1, RButtHeight] := CornerColor;
    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB(clWhite));
    ADest.Line(lRect.Right - 51, 1, lRect.Right, 1);
    ADest.Line(lRect.Right, 1, lRect.Right, 25);
    ADest.Line(lRect.Right, 25, lRect.Right - 51, 25);
    ADest.Line(lRect.Right - 51, 25, lRect.Right - 51, 1);
    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($00FFFFFF));}
end;

procedure TCDCustomTabControlDrawerWinCE.DrawTabs(ADest: TCanvas);
var
  IsPainting: Boolean = False;
  CurStartLeftPos: Integer = 0;
  i: Integer;
begin
  for i := 0 to CDTabControl.Tabs.Count - 1 do
  begin
    if i = LeftmostTabVisibleIndex then
      IsPainting := True;

    if IsPainting then
    begin
      DrawTab(ADest, i, CurStartLeftPos);
      CurStartLeftPos := CurStartLeftPos + GetTabWidth(ADest, i);
    end;
  end;
end;

procedure TCDCustomTabControlDrawerWinCE.DrawTab(ADest: TCanvas;
  AIndex: Integer; ACurStartLeftPos: Integer);
var
  IsSelected: Boolean;
  lTabWidth, lTabHeight, lTabTopPos: Integer;
  Points: array of TPoint;
  lCaption: String;
begin
  IsSelected := CDTabControl.FTabIndex = AIndex;

  if IsSelected then
  begin
    lTabTopPos := 0;
    lTabHeight := GetTabHeight(AIndex);
  end
  else
  begin
    lTabTopPos := 5;
    lTabHeight := GetTabHeight(AIndex)-5;
  end;

  lTabWidth := GetTabWidth(ADest, AIndex);

  // Fill the area inside the outer border
  ADest.Pen.Style := psClear;
  ADest.Brush.Style := bsSolid;
  ADest.Brush.Color := clWhite;
  SetLength(Points, 5);
  Points[0] := Point(ACurStartLeftPos, lTabTopPos);
  Points[1] := Point(ACurStartLeftPos+lTabWidth-5, lTabTopPos);
  Points[2] := Point(ACurStartLeftPos+lTabWidth, lTabTopPos+5);
  Points[3] := Point(ACurStartLeftPos+lTabWidth, lTabTopPos+lTabHeight);
  Points[4] := Point(ACurStartLeftPos, lTabTopPos+lTabHeight);
  ADest.Polygon(Points);

  // Draw the outer border only in the top and right sides,
  // and bottom if unselected
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := ColorToRGB($009C9B91);
  ADest.MoveTo(ACurStartLeftPos+1, lTabTopPos);
  ADest.LineTo(ACurStartLeftPos+lTabWidth-5, lTabTopPos);
  ADest.LineTo(ACurStartLeftPos+lTabWidth, lTabTopPos+5);
  ADest.LineTo(ACurStartLeftPos+lTabWidth, lTabTopPos+lTabHeight);

  // If it is selected, add a selection frame
  if IsSelected then
  begin
    ADest.Pen.Color := ColorToRGB($00D6C731);
    ADest.Pen.Style := psSolid;
    ADest.Brush.Style := bsClear;
    ADest.Rectangle(
      ACurStartLeftPos+3, lTabTopPos+3,
      ACurStartLeftPos+lTabWidth-5, lTabTopPos+lTabHeight-5
      );
  end;

  // Now the text
  lCaption := CDTabControl.Tabs.Strings[AIndex];
  ADest.TextOut(ACurStartLeftPos+5, lTabTopPos+5, lCaption);
end;

function TCDCustomTabControlDrawerWinCE.GetPageIndexFromXY(x, y: integer
  ): integer;
begin
  Result := 1;
end;

function TCDCustomTabControlDrawerWinCE.GetTabHeight(AIndex: Integer): Integer;
begin
  if CDTabControl.Font.Size = 0 then
    Result := 32
  else
    Result := CDTabControl.Font.Size + 22;
end;

function TCDCustomTabControlDrawerWinCE.GetTabWidth(ADest: TCanvas;
  AIndex: Integer): Integer;
const
  TCDTabControl_WinCE_TabCaptionExtraWidth = 20;
var
  lCaption: string;
begin
  lCaption := CDTabControl.Tabs.Strings[AIndex];

  Result := ADest.TextWidth(lCaption) + TCDTabControl_WinCE_TabCaptionExtraWidth;
end;

{function TCDCustomTabControlDrawerWinCE.GetClientRect(AControl: TCDControl
  ): TRect;
var
  lCaptionHeight: Integer;
begin
  lCaptionHeight := GetTabHeight(CDTabControl.FTabIndex) - 4;

  Result := Rect(5, lCaptionHeight + 1, CDTabControl.Width - 10,
    CDTabControl.Height - lCaptionHeight - 5);
end;}

procedure TCDCustomTabControlDrawerWinCE.DrawToIntfImage(ADest: TFPImageCanvas;
  FPImg: TLazIntfImage);
var
  lColor: TColor;
  lFPColor: TFPColor;
  x, y: Integer;
begin
  if CDTabControl.Color = clDefault then
    lColor := ColorToRGB(CDTabControl.GetDefaultColor(dctBrush))
  else lColor := ColorToRGB(CDTabControl.Color);

  // Background
  lFPColor := TColorToFPColor(lColor);
  FPImg.FillPixels(lFPColor);
end;

procedure TCDCustomTabControlDrawerWinCE.DrawToCanvas(ADest: TCanvas);
var
  CaptionHeight: Integer;
begin
  CaptionHeight := GetTabHeight(CDTabControl.FTabIndex);

  // frame
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := ColorToRGB($009C9B91);

  if CDTabControl.GetTabCount = 0 then
    ADest.Rectangle(0, 0, CDTabControl.Width - 2, CDTabControl.Height - 2)
  else
    ADest.Rectangle(0, CaptionHeight, CDTabControl.Width -  2, CDTabControl.Height - 2);

  ADest.Pen.Color := ColorToRGB($00BFCED0);
  ADest.Line(CDTabControl.Width - 1, CaptionHeight + 1,
    CDTabControl.Width - 1, CDTabControl.Height - 1);
  ADest.Line(CDTabControl.Width - 1, CDTabControl.Height - 1, 1,
    CDTabControl.Height - 1);

  // Tabs
  ADest.Font.Name := CDTabControl.Font.Name;
  ADest.Font.Size := CDTabControl.Font.Size;
//  DrawCaptionBar(ADest, Rect(0, 0, CDPageControl.Width -
//    2, CaptionHeight + 1), CDPageControl.Color, CDPageControl);
  DrawTabs(ADest);
end;

procedure TCDCustomTabControlDrawerWinCE.DrawTabSheet(ADest: TCanvas);
begin
  ADest.Brush.Color := CDTabControl.Color;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.Rectangle(0, 0, CDTabControl.Width, CDTabControl.Height);
end;

procedure TCDCustomTabControlDrawerWinCE.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i: Integer;
  CurPage: TCDTabSheet;
  CurStartLeftPos: Integer = 0;
  VisiblePagesStarted: Boolean = False;
  lTabWidth: Integer;
begin
  for i := 0 to CDTabControl.Tabs.Count - 1 do
  begin
    if i = LeftmostTabVisibleIndex then
      VisiblePagesStarted := True;

    if VisiblePagesStarted then
    begin
      lTabWidth := GetTabWidth(CDTabControl.Canvas, i);
      if (X > CurStartLeftPos) and
        (X < CurStartLeftPos + lTabWidth) and
        (Y < GetTabHeight(i)) then
      begin
        if CDTabControl is TCDPageControl then
          (CDTabControl as TCDPageControl).SetPageIndex(i)
        else
          CDTabControl.SetTabIndex(i);

        Exit;
      end;
      CurStartLeftPos := CurStartLeftPos + lTabWidth;
    end;
  end;
end;

{ TCDCustomTabControl }

procedure TCDCustomTabControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  TCDCustomTabControlDrawer(FCurrentDrawer).MouseDown(Button, Shift, X, Y);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDCustomTabControl.PrepareCurrentDrawer;
begin
  case FDrawStyle of
    dsWince: FCurrentDrawer := FDrawerWinCE;
    dsCustom: FCurrentDrawer := CustomDrawer;
  end;
end;

procedure TCDCustomTabControl.SetTabIndex(AValue: Integer);
begin
  if FTabIndex = AValue then Exit;
  if Assigned(OnChanging) then OnChanging(Self);
  FTabIndex := AValue;
  if Assigned(OnChange) then OnChange(Self);
  Invalidate;
end;

procedure TCDCustomTabControl.SetTabs(AValue: TStringList);
begin
  if FTabs=AValue then Exit;
  FTabs.Assign(AValue);
  CorrectTabIndex();
  Invalidate;
end;

constructor TCDCustomTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 232;
  Height := 184;
  TabStop := True;

  FDrawerWinCE := TCDCustomTabControlDrawerWinCE.Create;
  TCDCustomTabControlDrawerWinCE(FDrawerWinCE).CDTabControl := Self;
  CustomDrawer := FDrawerWinCE; // Dummy to avoid designer crashes
  FCurrentDrawer := FDrawerWinCE;
  FDrawStyle := dsWinCE;

  ParentColor := True;
  ParentFont := True;
  ControlStyle := ControlStyle + [csAcceptsControls, csDesignInteractive];

  // FTabs should hold only visible tabs
  FTabs := TStringList.Create;
end;

destructor TCDCustomTabControl.Destroy;
begin
  FTabs.Free;

  inherited Destroy;
end;

procedure TCDCustomTabControl.Paint;
var
  AImage: TLazIntfImage = nil;
  ABmp: TBitmap = nil;
  lCanvas: TFPImageCanvas = nil;
begin
  ABmp := TBitmap.Create;
  try
    ABmp.Width := Width;
    ABmp.Height := Height;
    AImage := ABmp.CreateIntfImage;
    lCanvas := TFPImageCanvas.Create(AImage);
    TCDCustomTabControlDrawer(FCurrentDrawer).DrawToIntfImage(lCanvas, AImage);
    ABmp.LoadFromIntfImage(AImage);
    ABmp.Canvas.Font.Assign(Font);
    TCDCustomTabControlDrawer(FCurrentDrawer).DrawToCanvas(ABmp.Canvas);
    Canvas.Draw(0, 0, ABmp);
  finally
    if lCanvas <> nil then
      lCanvas.Free;
    if AImage <> nil then
      AImage.Free;
    ABmp.Free;
  end;
end;

function TCDCustomTabControl.GetTabCount: Integer;
begin
  Result := 0;
  if FTabs <> nil then Result := FTabs.Count;
end;

procedure TCDCustomTabControl.CorrectTabIndex;
begin
  if FTabIndex >= FTabs.Count then SetTabIndex(FTabs.Count - 1);
end;

{ TCDControl }

procedure TCDControl.PrepareCurrentDrawer;
begin

end;

procedure TCDControl.SetDrawStyle(const AValue: TCDDrawStyle);
begin
  if FDrawStyle = AValue then exit;
  FDrawStyle := AValue;
  Invalidate;
  PrepareCurrentDrawer();

  //FCurrentDrawer.SetClientRectPos(Self);
end;

function TCDControl.GetClientRect: TRect;
begin
  // Disable this, since although it works in Win32, it doesn't seam to work in LCL-Carbon
  //if (FCurrentDrawer = nil) then
    Result := inherited GetClientRect()
  //else
    //Result := FCurrentDrawer.GetClientRect(Self);
end;

procedure TCDControl.EraseBackground(DC: HDC);
begin

end;

{ TCDButtonDrawer }

function TCDButtonDrawer.GetClientRect(AControl: TCDControl): TRect;
var
  CDButton: TCDButton absolute AControl;
begin
  Result := Rect(1, 1, CDButton.Width - 1, CDButton.Height - 1);
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
  NewState: TCDButtonState;
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
  NewState: TCDButtonState;
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

procedure TCDButton.PrepareCurrentDrawer;
begin
  case DrawStyle of
    dsWince: FCurrentDrawer := FDrawerWinCE;
    dsCustom: FCurrentDrawer := CustomDrawer;
    dsAndroid: FCurrentDrawer := FDrawerAndroid;
    dsXPTaskbar: FCurrentDrawer := FDrawerXPTB;
    dsGrad: FCurrentDrawer := FDrawerGrad;
    dsWin2000: FCurrentDrawer := FDrawerWin2k;
  end;
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
  CustomDrawer := FDrawerWinCE; // Dummy to avoid designer crashes
  FDrawerAndroid := TCDButtonDrawerAndroid.Create;
  FDrawerXPTB := TCDButtonDrawerXPTB.Create;
  FDrawerGrad := TCDButtonDrawerGrad.Create;
  FDrawerWin2k := TCDButtonDrawerWin2k.Create;
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
    TCDButtonDrawer(FCurrentDrawer).DrawToIntfImage(lCanvas, Self);
    ABmp.LoadFromIntfImage(AImage);
    // Second step of the drawing: LCL TCustomCanvas for easy font access
    TCDButtonDrawer(FCurrentDrawer).DrawToCanvas(ABmp.Canvas, Self, FState);

    Canvas.Draw(0, 0, ABmp);
  finally
    if lCanvas <> nil then
      lCanvas.Free;
    if AImage <> nil then
      AImage.Free;
    ABmp.Free;
  end;
end;

{ TCDButtonDrawerGrad }

procedure TCDButtonDrawerGrad.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerGrad.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TCDButtonState);
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

{ TCDButtonDrawerWinCE }

procedure TCDButtonDrawerWinCE.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerWinCE.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TCDButtonState);
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

  // Button image
  case FState of
    bbsDown:
    begin
      with TmpB.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := GetAColor(CDButton.Color, 90);
        Pen.Color := clBlack;
        Pen.Style := psSolid;
        Rectangle(0, 0, Width, Height);
      end;
    end;
    bbsFocused:
      with TmpB.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := GetAColor(CDButton.Color, 99);
        Pen.Color := clBlack;
        Pen.Style := psSolid;
        Rectangle(0, 0, Width, Height);
        Rectangle(1, 1, Width - 1, Height - 1); // The border is thicken when focused
      end;
    else
      with TmpB.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := CDButton.Color;
        Pen.Color := clBlack;
        Pen.Style := psSolid;
        Rectangle(0, 0, Width, Height);
      end;
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

procedure TCDButtonDrawerWin2k.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerWin2k.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TCDButtonState);
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

  with TmpB.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := CDButton.Color;
    Pen.Color := clWhite;
    Pen.Style := psSolid;
    Rectangle(0, 0, Width - 1, Height - 1);
    Pen.Color := clWhite;
    Line(0, 0, Width - 1, 0);
    Line(0, 0, 0, Height - 1);
    Pen.Color := clGray;
    Line(0, Height - 1, Width - 1, Height - 1);
    Line(Width - 1, Height - 1, Width - 1, -1);
    Pen.Color := $0099A8AC;
    Line(1, Height - 2, Width - 2, Height - 2);
    Line(Width - 2, Height - 2, Width - 2, 0);
    Pen.Color := $00E2EFF1;
    Line(1, 1, Width - 2, 1);
    Line(1, 1, 1, Height - 2);
  end;

  // Button image
  case FState of
    bbsDown:
    begin
      with TmpB.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := CDButton.Color;
        Pen.Color := clWhite;
        Pen.Style := psSolid;
        Rectangle(0, 0, Width - 1, Height - 1);
        Pen.Color := clGray;
        Line(0, 0, Width - 1, 0);
        Line(0, 0, 0, Height - 1);
        Pen.Color := clWhite;
        Line(0, Height - 1, Width - 1, Height - 1);
        Line(Width - 1, Height - 1, Width - 1, -1);
        Pen.Color := $00E2EFF1;
        Line(1, Height - 2, Width - 2, Height - 2);
        Line(Width - 2, Height - 2, Width - 2, 0);
        Pen.Color := $0099A8AC;
        Line(1, 1, Width - 2, 1);
        Line(1, 1, 1, Height - 2);
      end;
    end;
    bbsFocused:
      with TmpB.Canvas do
        DrawFocusRect(Rect(3, 3, Width - 4, Height - 4))
    else
    begin
    end;
  end;

  ADest.Draw(0, 0, TmpB);

  TmpB.Free;

  // Button text
  {$ifndef CUSTOMDRAWN_USE_FREETYPE}
  ADest.Font.Assign(CDButton.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := CDButton.Caption;
  if FState = bbsDown then
    ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2 + 1,
      (CDButton.Height - ADest.TextHeight(Str)) div 2 + 1, Str)
  else
    ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2,
      (CDButton.Height - ADest.TextHeight(Str)) div 2, Str);
  {$endif}
end;

procedure TCDButtonDrawerAndroid.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerAndroid.DrawToCanvas(ADest: TCanvas;
  CDButton: TCDButton; FState: TCDButtonState);
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

procedure TCDButtonDrawerXPTB.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerXPTB.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
  FState: TCDButtonState);
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

procedure TCDGroupBox.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  Invalidate;
end;

constructor TCDGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
  TabStop := False;
  FDrawerWinCE := TCDGroupBoxDrawerWinCE.Create;
  CustomDrawer := FDrawerWinCE; // Dummy to avoid designer crashes
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
  if FCaptionMiddle = 0 then FCaptionMiddle := CDGroupBox.Canvas.Font.Size div 2;
  if FCaptionMiddle = 0 then FCaptionMiddle := 5;

  // Background
  if CDGroupBox.Parent = nil then
    ADest.Brush.FPColor := colLtGray
  else if CDGroupBox.Parent.Color = clDefault then
    ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(clForm))
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
  else if CDGroupBox.Parent.Color = clDefault then
    ADest.Brush.Color := ColorToRGB(clForm)
  else
    ADest.Brush.Color := ColorToRGB(CDGroupBox.Parent.Color);

  // paint text
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsSolid; // This will fill the text background
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
  if Value = FPosition then Exit;
  FPosition := Value;
  Invalidate;
end;

function TCDTrackBar.GetPositionFromMousePos(X, Y: integer): integer;
var
  lLeftBorder, lRightBorder: Integer;
begin
  FCurrentDrawer.GetGeometry(lLeftBorder, lRightBorder);
  if X > Width - lRightBorder then Result := FMax
  else if X < lLeftBorder then Result := FMin
  else Result := FMin + (X - lLeftBorder) * (FMax - FMin + 1) div (Width - lRightBorder - lLeftBorder);

  // sanity check
  if Result > FMax then Result := FMax;
  if Result < FMin then Result := FMin;
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
var
  NewPosition: Integer;
begin
  inherited KeyDown(Key, Shift);
  if (Key = 37) or (Key = 40) then
    NewPosition := FPosition - (FMax - FMin) div 10;
  if (Key = 38) or (Key = 39) then
    NewPosition := FPosition + (FMax - FMin) div 10;

  // sanity check
  if NewPosition > FMax then NewPosition := FMax;
  if NewPosition < FMin then NewPosition := FMin;

  Position := NewPosition;
end;

procedure TCDTrackBar.KeyUp(var Key: word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure TCDTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  NewPosition: Integer;
begin
  SetFocus;

  NewPosition := GetPositionFromMousePos(X, Y);

  DragDropStarted := True;

  Position := NewPosition;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  DragDropStarted := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCDTrackBar.MouseMove(Shift: TShiftState; X, Y: integer);
var
  NewPosition: Integer;
begin
  if DragDropStarted then
  begin
    NewPosition := GetPositionFromMousePos(X, Y);
    Position := NewPosition;
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
  FMax := 10;
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
  //inherited EraseBackground(DC);
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

{ TCDTrackBarDrawer }

procedure TCDTrackBarDrawerGraph.DrawToIntfImage(ADest: TFPImageCanvas;
  FPImg: TLazIntfImage; CDTrackBar: TCDTrackBar);
const
  CDBarEdge = 18;
var
  lDrawingBottom, StepsCount, i: Integer;
  pStart, pEnd: integer; // for drawing the decorative bars
  dRect: TRect;
  pStepWidth, pHalfStepWidth: Integer;
begin
  // Sanity check
  if CDTrackBar.Max - CDTrackBar.Min <= 0 then
    raise Exception.Create('[TCDTrackBarDrawerGraph.DrawToIntfImage] Max-Min must be at least 1');

  // Preparations
  StepsCount := CDTrackBar.Max - CDTrackBar.Min + 1;
  pStepWidth := (CDTrackBar.Width - CDBarEdge) div StepsCount;
  pHalfStepWidth := (CDTrackBar.Width - CDBarEdge) div (StepsCount * 2);

  // The bottom part of the drawing
  lDrawingBottom := CDTrackBar.Height - 10;

  // Background

  if CDTrackBar.Parent = nil then
    ADest.Brush.FPColor := colLtGray
  else
    ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(CDTrackBar.Color));
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.Rectangle(0, 0, CDTrackBar.Width, CDTrackBar.Height);
  ADest.Brush.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));

  // Draws the double-sided arrow in the center of the slider

  ADest.Pen.Style := psSolid;
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
  ADest.Line(0, lDrawingBottom, CDTrackBar.Width, lDrawingBottom);
  ADest.Line(3, lDrawingBottom - 1, 6, lDrawingBottom - 1);
  ADest.Line(5, lDrawingBottom - 2, 6, lDrawingBottom - 2);
  ADest.Line(3, lDrawingBottom + 1, 6, lDrawingBottom + 1);
  ADest.Line(5, lDrawingBottom + 2, 6, lDrawingBottom + 2);
  ADest.Line(CDTrackBar.Width - 1 - 3, lDrawingBottom - 1, CDTrackBar.Width - 1 - 6, lDrawingBottom - 1);
  ADest.Line(CDTrackBar.Width - 1 - 5, lDrawingBottom - 2, CDTrackBar.Width - 1 - 6, lDrawingBottom - 2);
  ADest.Line(CDTrackBar.Width - 1 - 3, lDrawingBottom + 1, CDTrackBar.Width - 1 - 6, lDrawingBottom + 1);
  ADest.Line(CDTrackBar.Width - 1 - 5, lDrawingBottom + 2, CDTrackBar.Width - 1 - 6, lDrawingBottom + 2);
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB(clGray));
  ADest.Brush.FPColor := TColorToFPColor(ColorToRGB($00F0F0F0));

  // Draws the decorative bars and also the slider button

  pStart := 10 - 1;
  for i := 0 to StepsCount - 1 do
  begin
    // Draw the decorative bars
    dRect := Bounds(
      pStart + pHalfStepWidth,
      lDrawingBottom - 5 - i,
      Round(pStepWidth)-3,
      4 + i);

    ADest.Brush.Style := bsSolid;
    ADest.Pen.Style := psSolid;
    ADest.Pen.FPColor := colBlack;
    if i + CDTrackBar.Min <= CDTrackBar.Position then
      ADest.Brush.FPColor := colDkGray
    else
      ADest.Brush.FPColor := colWhite;

    ADest.Rectangle(dRect);

    // Draw the slider

    if i + CDTrackBar.Min = CDTrackBar.Position then
    begin
      ADest.Brush.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
      ADest.Brush.Style := bsSolid;
      ADest.Rectangle(pStart, lDrawingBottom + 1, pStart + 10, lDrawingBottom + 6);
      ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($005BA6C6));
      ADest.RecTangle(pStart, lDrawingBottom + 2, pStart + 10, lDrawingBottom + 7);
      ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($006BB6E6));
      ADest.RecTangle(pStart, lDrawingBottom, pStart + 10, lDrawingBottom + 2);
    end;
    pStart := pStart + pStepWidth;
  end;

  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($007BC6F6));
  ADest.Line(7, lDrawingBottom - 1, CDTrackBar.Width - 8, lDrawingBottom - 1);
  ADest.Line(7, lDrawingBottom + 1, CDTrackBar.Width - 8, lDrawingBottom + 1);
  ADest.Colors[2, lDrawingBottom - 1] := ADest.Pen.FPColor;
  ADest.Colors[4, lDrawingBottom - 2] := ADest.Pen.FPColor;
  ADest.Colors[2, lDrawingBottom + 1] := ADest.Pen.FPColor;
  ADest.Colors[4, lDrawingBottom + 2] := ADest.Pen.FPColor;
  ADest.Colors[6, lDrawingBottom - 3] := ADest.Pen.FPColor;
  ADest.Colors[6, lDrawingBottom + 3] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 2, lDrawingBottom - 1] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 4, lDrawingBottom - 2] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 2, lDrawingBottom + 1] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 4, lDrawingBottom + 2] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 6, lDrawingBottom - 3] := ADest.Pen.FPColor;
  ADest.Colors[CDTrackBar.Width - 1 - 6, lDrawingBottom + 3] := ADest.Pen.FPColor;
end;

procedure TCDTrackBarDrawerGraph.GetGeometry(var ALeftBorder,
  ARightBorder: Integer);
begin
  ALeftBorder := 9;
  ARightBorder := 9;
end;

{ TCDTabSheet }

constructor TCDTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TabStop := False;
  ParentColor := True;
  parentFont := True;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDesignFixedBounds, csDoubleClicks, csDesignInteractive];
  //ControlStyle := ControlStyle + [csAcceptsControls, csDesignFixedBounds,
  //  csNoDesignVisible, csNoFocus];
end;

destructor TCDTabSheet.Destroy;
begin
  inherited Destroy;
end;

procedure TCDTabSheet.EraseBackground(DC: HDC);
begin

end;

procedure TCDTabSheet.Paint;
begin
  if CDTabControl <> nil then
  begin
    TCDCustomTabControlDrawer(CDTabControl.FCurrentDrawer).DrawTabSheet(Canvas);
  end;
end;

{ TCDPageControl }

function TCDPageControl.AddPage(S: string): TCDTabSheet;
//  InsertPage(FPages.Count, S);
var
  NewPage: TCDTabSheet;
begin
  NewPage := TCDTabSheet.Create(Owner);
  NewPage.Parent := Self;
  NewPage.CDTabControl := Self;
  //Name := Designer.CreateUniqueComponentName(ClassName);
  NewPage.Name := GetUniqueName(sTABSHEET_DEFAULT_NAME, Self.Owner);
  if S = '' then
    NewPage.Caption := NewPage.Name
  else
    NewPage.Caption := S;

  PositionTabSheet(NewPage);

  FTabs.AddObject(S, NewPage);

  SetActivePage(NewPage);

  Result := NewPage;
end;

function TCDPageControl.GetPage(AIndex: integer): TCDTabSheet;
begin
  if (AIndex >= 0) and (AIndex < FTabs.Count) then
    Result := TCDTabSheet(FTabs.Objects[AIndex])
  else
    Result := nil;
end;

function TCDPageControl.InsertPage(aIndex: integer; S: string): TCDTabSheet;
var
  NewPage: TCDTabSheet;
begin
  NewPage := TCDTabSheet.Create(Owner);
  NewPage.Parent := Self;
  //Name := Designer.CreateUniqueComponentName(ClassName);
  NewPage.Name := GetUniqueName(sTABSHEET_DEFAULT_NAME, Self.Owner);
  if S = '' then
    NewPage.Caption := NewPage.Name
  else
    NewPage.Caption := S;

  PositionTabSheet(NewPage);

  FTabs.InsertObject(AIndex, S, NewPage);

  SetActivePage(NewPage);
  Result := NewPage;
end;

procedure TCDPageControl.RemovePage(aIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= FTabs.Count) then Exit;

  Application.ReleaseComponent(TComponent(FTabs.Objects[AIndex]));

  FTabs.Delete(aIndex);
  if FTabIndex >= FTabs.Count then SetTabIndex(FTabIndex-1);

  Invalidate;
end;

function TCDPageControl.FindNextPage(CurPage: TCDTabSheet;
  GoForward, CheckTabVisible: boolean): TCDTabSheet;
var
  I, TempStartIndex: integer;
begin
  if FTabs.Count <> 0 then
  begin
    //StartIndex := FPages.IndexOfObject(CurPage);
    TempStartIndex := FTabs.IndexOfObject(CurPage);
    if TempStartIndex = -1 then
      if GoForward then
        TempStartIndex := FTabs.Count - 1
      else
        TempStartIndex := 0;
    I := TempStartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FTabs.Count then
          I := 0;
      end
      else
      begin
        if I = 0 then
          I := FTabs.Count;
        Dec(I);
      end;
      Result := TCDTabSheet(FTabs.Objects[I]);
      if not CheckTabVisible or Result.Visible then
        Exit;
    until I = TempStartIndex;
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

  ControlStyle := ControlStyle - [csAcceptsControls];
end;

destructor TCDPageControl.Destroy;
begin
  inherited Destroy;
end;

procedure TCDPageControl.SetActivePage(Value: TCDTabSheet);
var
  i: integer;
  CurPage: TCDTabSheet;
begin
  for i := 0 to FTabs.Count - 1 do
  begin
    CurPage := TCDTabSheet(FTabs.Objects[i]);
    if CurPage = Value then
    begin
      PositionTabSheet(CurPage);
      CurPage.BringToFront;
      CurPage.Visible := True;

      // Check first, Tab is Visible?
      SetTabIndex(i);
    end
    else if CurPage <> nil then
    begin
      //CurPage.Align := alNone;
      //CurPage.Height := 0;
      CurPage.Visible := False;
    end;
  end;

  Invalidate;
end;

procedure TCDPageControl.SetPageIndex(Value: integer);
begin
  if (Value > -1) and (Value < FTabs.Count) then
  begin
    SetTabIndex(Value);
    ActivePage := GetPage(Value);
  end;
end;

procedure TCDPageControl.UpdateAllDesignerFlags;
var
  i: integer;
begin
  for i := 0 to FTabs.Count - 1 do
    UpdateDesignerFlags(i);
end;

procedure TCDPageControl.UpdateDesignerFlags(APageIndex: integer);
var
  CurPage: TCDTabSheet;
begin
  CurPage := GetPage(APageIndex);
  if APageIndex <> fTabIndex then
    CurPage.ControlStyle := CurPage.ControlStyle + [csNoDesignVisible]
  else
    CurPage.ControlStyle := CurPage.ControlStyle - [csNoDesignVisible];
end;

procedure TCDPageControl.PositionTabSheet(ATabSheet: TCDTabSheet);
var
  lTabHeight, lIndex: Integer;
begin
//  ATabSheet.SetBounds(1, 32 + 1, Width - 3, Height - 32 - 4);
  lIndex := FTabs.IndexOfObject(ATabSheet);;
  lTabHeight := TCDCustomTabControlDrawer(FCurrentDrawer).GetTabHeight(lIndex);
  ATabSheet.BorderSpacing.Top := lTabHeight;
  ATabSheet.BorderSpacing.Left := 2;
  ATabSheet.BorderSpacing.Right := 3;
  ATabSheet.BorderSpacing.Bottom := 3;
  ATabSheet.Align := alClient;
end;

function TCDPageControl.GetActivePage: TCDTabSheet;
begin
  Result := GetPage(FTabIndex);
end;

function TCDPageControl.GetPageCount: integer;
begin
  Result := FTabs.Count;
end;

function TCDPageControl.GetPageIndex: integer;
begin
  Result := FTabIndex;
end;

end.

