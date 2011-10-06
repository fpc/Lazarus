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
  LCLProc, PropEdits, ExtCtrls, ImgList, Forms, Menus,
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

  TCDDrawStyle = (
    // Operating system styles
    dsWinCE, dsWin2000, dsAndroid, dsXPTaskBar,
    // Other special styles
    dsGrad,
    // Defined by the user
    dsCustom);

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
  TCDButtonDrawerGrad = class;
  TCDButtonDrawerWin2k = class;

  TCDButton = class(TCustomControl)
  private
    FDrawStyle: TCDDrawStyle;
    FCurrentDrawer: TCDButtonDrawer;
    FDrawerWinCE: TCDButtonDrawerWinCE;
    FDrawerAndroid: TCDButtonDrawerAndroid;
    FDrawerXPTB: TCDButtonDrawerXPTB;
    FDrawerGrad: TCDButtonDrawerGrad;
    FDrawerWin2k: TCDButtonDrawerWin2k;
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

  TCDButtonDrawerGrad = class(TCDButtonDrawer)
  public
    procedure SetClientRectPos(CDButton: TCDButton); override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
      FState: TBitmappedButtonState); override;
  end;

  TCDButtonDrawerWin2k = class(TCDButtonDrawer)
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

  TCDTabSheet = class;

  TTabItem = class(TCollectionItem)
  private
    FTitle: string;
    FSelected: boolean;
    FData: TObject;
    FModified: boolean;
    FImageIndex: TImageIndex;
    FWidth: integer;
    FTabPage: TCDTabSheet;
    procedure SetSelected(Value: boolean);
    procedure SetModified(Value: boolean);
    procedure DoChange;
    procedure SetTitle(Value: string);
    procedure SetImageIndex(Value: TImageIndex);
  protected
    FStartPos: integer;
    FSize: integer;
    function GetDisplayName: string; override;
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    property Data: TObject read FData write FData;
  published
    property Width: integer read FWidth write FWidth;
    property Title: string read FTitle write SetTitle;
    property Selected: boolean read FSelected write SetSelected;
    property Modified: boolean read Fmodified write SetModified;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property TabPage: TCDTabSheet read FTabPage write FTabPage;
  end;

  TTabItemList = class(TCollection)
  protected
    procedure DoSelected(ATab: TTabItem; ASelected: boolean); dynamic;
    procedure DoChanged(ATab: TTabItem); dynamic;
    procedure SetItem(Index: integer; Value: TTabItem);
    function GetItem(Index: integer): TTabItem;
  public
    function IndexOf(ATab: TTabItem): integer;
    function Add: TTabItem;
    property Items[Index: integer]: TTabItem read GetItem write SetItem; default;
  end;

  TCDTabControlDrawer = class;
  TCDTabControlDrawerGraph = class;

  TTabSelectedEvent = procedure(Sender: TObject; ATab: TTabItem;
    ASelected: boolean) of object;

  TCDTabControl = class(TCustomControl)
  private
    FCurrentDrawer: TCDTabControlDrawerGraph;
    FTabIndex: integer;
    FTabs: TTabItemList;
    FOnTabSelected: TTabSelectedEvent;
    FMDownL, FMDownR: boolean;
    FMEnterL, FMEnterR: boolean;
    RButtHeight: integer;
    FStartIndex: integer;
    //FEndIndex: integer;
    MaskHeadBmp: TBitmap;
    procedure SetMouseUP;
    procedure SetTabIndex(Value: integer);
    procedure SetStartIndex(Value: integer);
    //procedure SetEndIndex(Value: integer);
  protected
    procedure Changed; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure TabSelected(ATab: TTabItem; ASelected: boolean); dynamic;
    procedure GetThisTab(ATab: TTabItem);
    procedure ClearSelection;
    procedure DrawCaptionBar(ADest: TCanvas; lRect: TRect; CL: TColor);
    procedure Loaded; override;
    procedure DoOnResize; override;
    procedure DoLeftButtonDown;
    procedure DoRightButtonDown;
    function GetPageIndexFromXY(x, y: integer): integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Caption;
    property Color;
    property Font;
    property StartIndex: integer read FStartIndex write SetStartIndex;
    //property EndIndex: integer read FEndIndex write SetEndIndex;
    property TabIndex: integer read fTabIndex write SetTabIndex;
    property Tabs: TTabItemList read FTabs write FTabs;
    property OnTabSelected: TTabSelectedEvent read fOnTabSelected write fOnTabSelected;
  end;

  TCDTabControlDrawer = class
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTabControl: TCDTabControl); virtual; abstract;
    procedure DrawToCanvas(ADest: TCanvas; CDTabControl: TCDTabControl);
      virtual; abstract;
  end;

  TCDTabControlDrawerGraph = class(TCDTabControlDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTabControl: TCDTabControl); override;
    procedure DrawToCanvas(ADest: TCanvas; CDTabControl: TCDTabControl); override;
  end;

  { TCDTabSheet }

  TCDTabSheetDrawerGraph = class;
  TCDPageControl = class;

  TCDTabSheet = class(TCustomControl)
  private
    FCurrentDrawer: TCDTabSheetDrawerGraph;
    FIndex: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
  published
    property Caption;
    property Color;
    property Font;
    property Index: integer read FIndex write FIndex;
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

  { TCDPageControlEditor }

  TCDPageControlEditor = class(TDefaultComponentEditor)
    procedure ShowPageMenuItemClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
    procedure PrepareItem(Index: integer; const AnItem: TMenuItem); override;
    procedure AddMenuItemsForPages(ParentMenuItem: TMenuItem); virtual;
    function PControl: TCDPageControl; virtual;
  end;

  TCDPageControl = class(TCustomControl)
  private
    FDrawStyle: TCDDrawStyle;
    FCaptionHeight: integer;
    FActivePage: TCDTabSheet;
    FCurrentDrawer: TCDPageControlDrawer;
    FDrawerWinCE: TCDPageControlDrawerWinCE;
    FStartIndex: integer;       //FEndIndex
    RButtHeight: integer;
    FPages: TTabItemList;
    FMDownL, FMDownR: boolean;
    FMEnterL, FMEnterR: boolean;
    FPageIndex: integer;  //FPageCount
    MaskHeadBmp: TBitmap;
    function GetPageCount: integer;
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
    procedure DrawCaptionBar(ADest: TCanvas; lRect: TRect; CL: TColor);
    procedure SetActivePage(Value: TCDTabSheet);
    procedure SetPageIndex(Value: integer);
    procedure UpdateAllDesignerFlags;
    procedure UpdateDesignerFlags(APageIndex: integer);
    procedure SetStartIndex(Value: integer);
    //procedure SetEndIndex(Value: integer);
  protected
    CustomDrawer: TCDPageControlDrawer; // Fill the field to use the dsCustom draw mode
    procedure Loaded; override;
    function GetPageIndexFromXY(x, y: integer): integer;
    procedure DoLeftButtonDown;
    procedure DoRightButtonDown;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure DoOnResize; override;
    function FindNextPage(CurPage: TCDTabSheet;
      GoForward, CheckTabVisible: boolean): TCDTabSheet;
    procedure SelectNextPage(GoForward: boolean; CheckTabVisible: boolean = True);
    procedure SetCDPages(Value: TTabItemList);
    procedure InsertPage(aIndex: integer; S: string);
    procedure RemovePage(aIndex: integer);
  public
    procedure AddPage(S: string);
  published
    property ActivePage: TCDTabSheet read FActivePage write SetActivePage;
    property DrawStyle: TCDDrawStyle read FDrawStyle write SetDrawStyle;
    property Caption;
    property CaptionHeight: integer read FCaptionHeight write SetCaptionHeight;
    property Color;
    property Font;
    //property PageCount: integer read FPageCount;
    property PageIndex: integer read FPageIndex write SetPageIndex;
    property Pages: TTabItemList read FPages write SetCDPages;
    property PageCount: integer read GetPageCount;
    property ParentColor;
    property ParentFont;
    property StartIndex: integer read FStartIndex write SetStartIndex;
    //property EndIndex: integer read FEndIndex write SetEndIndex;
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
function CheckTabButton(RWidth: integer; aItem: TTabItemList): boolean;

implementation

uses
  ObjInspStrConsts;

const
  INT_BitmappedButton_LineSpacing = 2;
  MaskBaseColor = $00111111;

resourcestring
  sTABSHEET_DEFAULT_NAME = 'CTabSheet';
  sNEXT_PAGE = 'Ne&xt Page';
  sPREV_PAGE = '&Previouse Page';

procedure Register;
begin
  RegisterComponents('Common Controls', [TCDButton]);
  RegisterComponents('Common Controls', [TCDTrackBar]);
  RegisterComponents('Common Controls', [TCDTabControl]);
  RegisterComponents('Common Controls', [TCDPageControl]);
  RegisterComponents('Common Controls', [TCDGroupBox]);
  //RegisterComponents('Common Controls', [TUntabbedNotebook]);
  RegisterComponentEditor(TCDPageControl, TCDPageControlEditor);
  RegisterComponentEditor(TCDTabSheet, TCDPageControlEditor);
  RegisterNoIcon([TCDTabSheet]);
  RegisterClasses([TCDTabSheet]);
end;

function CheckTabButton(RWidth: integer; aItem: TTabItemList): boolean;
var
  i, j: integer;
begin
  Result := False;
  j := 0;
  for i := 0 to aItem.Count - 1 do
  begin
    j := j + aItem[i].Width;
    if j > RWidth - 6 then
    begin
      Result := True;
      break;
    end;
  end;
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
    dsGrad: FCurrentDrawer := FDrawerGrad;
    dsWin2000: FCurrentDrawer := FDrawerWin2k;
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

procedure TCDButtonDrawerGrad.SetClientRectPos(CDButton: TCDButton);
var
  lRect: TRect;
begin
  lRect := Rect(1, 1, CDButton.Width - 1, CDButton.Height - 1);
  CDButton.AdjustClientRect(lRect);
end;

procedure TCDButtonDrawerGrad.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerGrad.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
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

procedure TCDButtonDrawerWin2k.SetClientRectPos(CDButton: TCDButton);
var
  lRect: TRect;
begin
  lRect := Rect(1, 1, CDButton.Width - 1, CDButton.Height - 1);
  CDButton.AdjustClientRect(lRect);
end;

procedure TCDButtonDrawerWin2k.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerWin2k.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton;
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

{TCDTabControlItemList}

constructor TTabItem.Create(aCollection: TCollection);
begin
  inherited Create(ACollection);
  fImageIndex := -1;
end;

destructor TTabItem.Destroy;
begin
  inherited;
end;

procedure TTabItem.SetModified(Value: boolean);
begin
  if fModified <> Value then
  begin
    fModified := Value;
    DoChange;
  end;
end;

procedure TTabItem.SetSelected(Value: boolean);
var
  i: integer;
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    if FSelected then
    begin
      with (GetOwner as TTabItemList) do
      begin
        for i := 0 to Count - 1 do
        begin
          if (Items[i] <> Self) and (Items[i].Selected) then
          begin
            Items[i].Selected := False;
          end;
        end;
      end;
    end;
    (Collection as TTabItemList).DoSelected(Self, FSelected);
  end;
end;

function TTabItem.GetDisplayName: string;
begin
  {if FCaption <> '' then
    Result := FCaption
  else     }
  Result := inherited GetDisplayName;
end;

procedure TTabItem.DoChange;
begin
  (Collection as TTabItemList).DoChanged(Self);
end;

procedure TTabItem.SetTitle(Value: string);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    //DoChange;
  end;
end;

procedure TTabItem.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    DoChange;
  end;
end;

{ TTabItemList }

procedure TTabItemList.DoSelected(ATab: TTabItem; ASelected: boolean);
begin
  (GetOwner as TCDTabControl).TabSelected(ATab, ASelected);
end;

procedure TTabItemList.DoChanged(ATab: TTabItem);
begin
  (GetOwner as TCDTabControl).Invalidate;
end;

procedure TTabItemList.SetItem(Index: integer; Value: TTabItem);
begin
  inherited SetItem(Index, Value);
end;

function TTabItemList.GetItem(Index: integer): TTabItem;
begin
  Result := inherited GetItem(Index) as TTabItem;
end;

function TTabItemList.IndexOf(ATab: TTabItem): integer;
var
  i, c: integer;
begin
  c := Count;
  for i := 0 to c - 1 do
    if Items[i] = ATab then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TTabItemList.Add: TTabItem;
begin
  Result := TTabItem(inherited add);
  Result.Title := 'Title' + IntToStr(Count - 1);
end;

{TCDTabControl}

constructor TCDTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := False;
  ParentColor := True;
  ParentFont := True;
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks,
    csDesignInteractive, csReplicatable];
  FCurrentDrawer := TCDTabControlDrawerGraph.Create;
  FTabs := TTabItemList.Create(TTabItem);
  Width := 200;
  Height := 28;
  FStartIndex := 0;
  MaskHeadBmp := TBitmap.Create;
end;

destructor TCDTabControl.Destroy;
begin
  MaskHeadBmp.Free;
  FCurrentDrawer.Free;
  inherited Destroy;
end;

procedure TCDTabControl.DrawCaptionBar(ADest: TCanvas; lRect: TRect; CL: TColor);
var
  aRect, bRect, cRect: TRect;
  i: integer;
  rWidth: integer;
  aText, bText: string;
  CornerColor: TFPColor;
  MaskColor: TColor;
begin
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
  MaskHeadBmp.Width := lRect.Right - lRect.Left;
  MaskHeadBmp.Height := lRect.Bottom - lRect.Top;
  MaskHeadBmp.Canvas.Brush.Style := bsSolid;
  MaskHeadBmp.Canvas.Pen.Style := psSolid;
  MaskHeadBmp.Canvas.Pen.Color := clWhite;
  MaskHeadBmp.Canvas.Brush.Color := MaskHeadBmp.Canvas.Pen.Color;
  MaskHeadBmp.Canvas.Rectangle(lRect);
  if FTabs.Count = 0 then
  begin
    ADest.RecTangle(aRect);
    Exit;
  end;
  aRect.Left := lRect.Left + 2;
  aRect.Top := lRect.Top + 3;
  //ADest.TextStyle.Opaque :=false;
  //SetBkMode(ADest.Handle, TRANSPARENT);
  if Brush.Style = bsSolid then
    SetBkMode(ADest.Handle, OPAQUE)
  else
    SetBkMode(ADest.Handle, TRANSPARENT);
  for i := StartIndex to FTabs.Count - 1 do
  begin
    aText := FTabs[i].Title;
    rWidth := (Height - ADest.TextHeight(aText)) + ADest.TextWidth(aText);
    FTabs[i].Width := rWidth;
    if aRect.Left + rWidth > lRect.Right - 6 then
      Break
    else
      aRect.Right := aRect.Left + rWidth;
    if TabIndex = i then
    begin
      cRect := aRect;
      if i = StartIndex then
        cRect.Left := aRect.Left - 2
      else
        cRect.Left := aRect.Left - 4;
      cRect.Right := aRect.Right + 4;
      cRect.Top := cRect.Top - 2;
      bText := FTabs[i].Title;
    end
    else
      DrawTabHead(aDest, aRect, Color, False);
    MaskColor := MaskBaseColor + i - StartIndex;
    DrawTabHeadMask(MaskHeadBmp.Canvas, aRect, MaskColor, False);
    ADest.TextOut(aRect.Left + (aRect.Right - aRect.Left - ADest.TextWidth(aText)) div 2,
      aRect.Top + (aRect.Bottom - aRect.Top - ADest.TextHeight(aText)) div 2, aText);
    aRect.Left := aRect.Right + 3;
  end;
  //ADest.Draw(0,0,MaskHeadBmp);           Exit;
  ADest.Line(lRect.Left, lRect.Bottom - 1, cRect.Left, lRect.Bottom - 1);
  ADest.Line(cRect.Right, lRect.Bottom - 1, lRect.Right, lRect.Bottom - 1);
  DrawTabHead(aDest, cRect, clWhite, True);
  ADest.TextOut(cRect.Left + (cRect.Right - cRect.Left - ADest.TextWidth(bText)) div 2,
    cRect.Top + (cRect.Bottom - cRect.Top - ADest.TextHeight(bText)) div 2, bText);
  if not CheckTabButton(lRect.Right - lRect.Left, FTabs) then
    Exit;
  aRect.Left := lRect.Right - RButtHeight * 2 - 3;
  aRect.Top := 1;
  aRect.Bottom := RButtHeight + 1;
  aRect.Right := lRect.Right - RButtHeight;
  if FMDownL then
    GradFill(ADest, aRect, $00F1A079, $00EFAF9B)
  else
    GradFill(ADest, aRect, $00FDD9CB, $00F2C9B8);
  aRect.Left := lRect.Right - RButtHeight - 1;
  aRect.Top := 1;
  aRect.Bottom := RButtHeight + 1;
  aRect.Right := lRect.Right;
  if FMDownR then
    GradFill(ADest, aRect, $00F1A079, $00EFAF9B)
  else
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
end;

procedure TCDTabControl.GetThisTab(ATab: TTabItem);
var
  Start, Stop: integer;
begin
{  if Assigned(ATab) then
  begin
    Start := 0;
    Stop := Width - Height * 2;
    if FShowNewTab then
      Dec(Stop,  fEdgeWidth + 10);
    CalcTabPositions(Start, Stop, Canvas, ATab);
    Invalidate;
  end;    }
end;

procedure TCDTabControl.TabSelected(ATab: TTabItem; ASelected: boolean);
begin
  if ASelected then
  begin
    GetThisTab(ATab);
    fTabIndex := ATab.Index;
  end;
  if Assigned(fOnTabSelected) then
    fOnTabSelected(Self, ATab, ASelected);
end;

procedure TCDTabControl.SetStartIndex(Value: integer);
begin
  if (Value < -1) or (Value >= FTabs.Count) then
    Exit;
  FStartIndex := Value;
end;

procedure TCDTabControl.SetTabIndex(Value: integer);
var
  t: TTabItem;
begin
  if (Value < -1) or (Value >= FTabs.Count) then
    Exit;
  {if Value <> -1 then
  begin
    t := FTabs.Items[Value];
    t.Selected := True;
  end
  else
    ClearSelection;    }
  if Value > -1 then
    FTabIndex := Value;
end;

procedure TCDTabControl.ClearSelection;
var
  I: integer;
begin
  for I := 0 to FTabs.Count - 1 do
    FTabs[I].Selected := False;
end;

procedure TCDTabControl.Changed;
begin

end;

procedure TCDTabControl.DoEnter;
begin
  inherited DoEnter;
end;

procedure TCDTabControl.DoExit;
begin
  inherited DoExit;
end;

procedure TCDTabControl.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

procedure TCDTabControl.KeyUp(var Key: word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

procedure TCDTabControl.SetMouseUP;
begin
  FMDownL := False;
  FMDownR := False;
end;

procedure TCDTabControl.DoLeftButtonDown;
begin
  if StartIndex > 0 then
    StartIndex := StartIndex - 1;
  invalidate;
end;

procedure TCDTabControl.DoRightButtonDown;
begin
  if StartIndex < FTabs.Count - 1 then
    StartIndex := StartIndex + 1;
  invalidate;
end;

procedure TCDTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if (X > Width - RButtHeight * 2 - 2) and (X < Width - RButtHeight) then
  begin
    FMDownL := True;
    DoLeftButtonDown;
  end
  else
  if (X > Width - RButtHeight - 1) and (X < Width - 1) then
  begin
    FMDownR := True;
    DoRightButtonDown;
  end
  else
    SetMouseUP;
  if (Y < 3) or (Y > RButtHeight + 1) then
    SetMouseUP;
  if (Y < Height) and (Y > 0) and (X > 0) and (X < Width) then
    GetPageIndexFromXY(X, Y);
  invalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDTabControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  SetMouseUP;
  invalidate;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TCDTabControl.GetPageIndexFromXY(x, y: integer): integer;
begin
  if MaskHeadBmp.Canvas.Pixels[x, y] = clWhite then
    Result := -1
  else
    Result := MaskHeadBmp.Canvas.Pixels[x, y] - MaskBaseColor + StartIndex;
  if (TabIndex <> Result) and (TabIndex > -1) then
    TabIndex := Result;
end;

procedure TCDTabControl.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if (X > Width - RButtHeight * 2 - 2) and (X < Width - RButtHeight) then
    FMEnterL := True
  else
  if (X > Width - RButtHeight - 1) and (X < Width - 1) then
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

procedure TCDTabControl.Loaded;
begin
  inherited;
end;

procedure TCDTabControl.DoOnResize;
begin
  RButtHeight := Height - 4;
  inherited;
end;

procedure TCDTabControl.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TCDTabControl.MouseLeave;
begin
  inherited MouseLeave;
  SetMouseUP;
  invalidate;
end;

procedure TCDTabControl.EraseBackground(DC: HDC);
begin

end;

procedure TCDTabControl.Paint;
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
    ABmp.Canvas.Font.Assign(Font);
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

procedure TCDTabControlDrawerGraph.DrawToIntfImage(ADest: TFPImageCanvas;
  FPImg: TLazIntfImage; CDTabControl: TCDTabControl);
begin
  ADest.Rectangle(0, 0, CDTabControl.Width - 1, CDTabControl.Height - 1);
end;

procedure TCDTabControlDrawerGraph.DrawToCanvas(ADest: TCanvas;
  CDTabControl: TCDTabControl);
begin
  //CDTabControl.DrawCaptionBar(ADest, Rect(0, 0, CDTabControl.Width -
  //  2, CDTabControl.Height + 1));
  //  ADest.Brush.Color := clRed;
  ADest.Rectangle(0, 0, CDTabControl.Width, CDTabControl.Height);
  CDTabControl.DrawCaptionBar(ADest, Rect(0, 0, CDTabControl.Width,
    CDTabControl.Height), CDTabControl.Color);
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
    csDesignFixedBounds, csDoubleClicks, csDesignInteractive];
  //ControlStyle := ControlStyle + [csAcceptsControls, csDesignFixedBounds,
  //  csNoDesignVisible, csNoFocus];
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
  ADest.Rectangle(0, 0, CDTabSheet.Width - 1, CDTabSheet.Height - 1);
end;

{ TCDPageControlEditor }

procedure TCDPageControlEditor.ShowPageMenuItemClick(Sender: TObject);
var
  AMenuItem: TMenuItem;
  NewPageIndex: integer;
begin
  AMenuItem := TMenuItem(Sender);
  if (AMenuItem = nil) or (not (AMenuItem is TMenuItem)) then
    exit;
  NewPageIndex := AMenuItem.MenuIndex;
  if (NewPageIndex < 0) or (NewPageIndex >= PControl.PageCount) then
    exit;
  PControl.PageIndex := NewPageIndex;
  GetDesigner.SelectOnlyThisComponent(PControl.Pages[PControl.PageIndex].TabPage);
end;

procedure TCDPageControlEditor.ExecuteVerb(Index: integer);
var
  NewPage: TCDTabSheet;
  Hook: TPropertyEditorHook;
  PageComponent: TPersistent;
  OldPage: longint;
begin
  if not GetHook(Hook) then
    exit;

  case Index of
    0:
    begin  //  New Page
      PControl.AddPage('');
      NewPage := PControl.ActivePage;
      Hook.PersistentAdded(NewPage, True);
      Designer.Modified;
      //Hook.PersistentAdded(NewPage,true);
      //Designer.Modified;
    end;
    1:
    begin // Insert Page
      PControl.InsertPage(PControl.PageIndex, '');
      Hook.PersistentAdded(PControl.ActivePage, True);
      Modified;
    end;
    2:
    begin  //  Delete Page
      with PControl do
      begin
        NewPage := ActivePage;
        if NewPage = nil then
          Exit;
        RemovePage(NewPage.Index);
      end;
    end;
    3:
    begin  //  Next Page
      PControl.ActivePage := PControl.FindNextPage(PControl.ActivePage, True, False);
    end;
    4:
    begin  //  Previous Page
      PControl.ActivePage := PControl.FindNextPage(PControl.ActivePage, False, False);
    end;
  end;
  Modified;
  if Designer <> nil then
    Designer.Modified;
  PControl.Invalidate;
end;

function TCDPageControlEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := nbcesAddPage;
    1: Result := nbcesInsertPage;
    2: Result := nbcesDeletePage;
    3: Result := sNEXT_PAGE;
    4: Result := sPREV_PAGE;
    5: Result := nbcesShowPage;
  end;
end;

function TCDPageControlEditor.GetVerbCount: integer;
begin
  Result := 6;
end;

procedure TCDPageControlEditor.PrepareItem(Index: integer; const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
  case Index of
    0: ;
    1: AnItem.Enabled := PControl.PageIndex >= 0;
    2: AnItem.Enabled := PControl.PageIndex >= 0;
    3: AnItem.Enabled := PControl.PageIndex < PControl.PageCount - 1;
    4: AnItem.Enabled := PControl.PageIndex > 0;
    5: AddMenuItemsForPages(AnItem);
  end;
end;

procedure TCDPageControlEditor.AddMenuItemsForPages(ParentMenuItem: TMenuItem);
var
  i: integer;
  NewMenuItem: TMenuItem;
  TabPage: TCDTabSheet;
begin
  ParentMenuItem.Enabled := PControl.PageCount > 0;
  for i := 0 to PControl.PageCount - 1 do
  begin
    TabPage := PControl.Pages.Items[i].TabPage;
    NewMenuItem := TMenuItem.Create(ParentMenuItem);
    NewMenuItem.Name := 'ShowPage' + IntToStr(i);
    NewMenuItem.Caption := TabPage.Name + ' "' + TabPage.Caption + '"';
    NewMenuItem.OnClick := @ShowPageMenuItemClick;
    ParentMenuItem.Add(NewMenuItem);
  end;
end;

function TCDPageControlEditor.PControl: TCDPageControl;
begin
  if Component is TCDPageControl then
    Result := TCDPageControl(Component)
  else if Component is TCDTabSheet then
    Result := TCDPageControl(TCDTabSheet(Component).Parent);
end;

{ TCDPageControl }

procedure TCDPageControl.AddPage(S: string);
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
  NewPage.SetBounds(1, CaptionHeight + 1, Width - 3, Height - CaptionHeight - 4);
  NewPage.BorderSpacing.Top := CaptionHeight + 2;
  NewPage.BorderSpacing.Left := 2;
  NewPage.BorderSpacing.Right := 3;
  NewPage.BorderSpacing.Bottom := 3;
  NewPage.Align := alClient;
  if ActivePage <> nil then
    ActivePage.Hide;
  ActivePage := NewPage;
  NewPage.Show;
  //FPages.AddObject(NewPage.Name, NewPage);
  FPages.Insert(FPages.Count);
  FPages.Items[FPages.Count - 1].DisplayName := NewPage.Name;
  FPages.Items[FPages.Count - 1].TabPage := NewPage;
  NewPage.Index := FPages.Count - 1;
  FPageIndex := FPages.Count - 1;
  //FPageCount := PageCount + 1;
end;

procedure TCDPageControl.InsertPage(aIndex: integer; S: string);
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
  NewPage.SetBounds(1, CaptionHeight + 1, Width - 3, Height - CaptionHeight - 4);
  NewPage.BorderSpacing.Top := CaptionHeight + 2;
  NewPage.BorderSpacing.Left := 2;
  NewPage.BorderSpacing.Right := 3;
  NewPage.BorderSpacing.Bottom := 3;
  NewPage.Align := alClient;
  if ActivePage <> nil then
    ActivePage.Hide;
  ActivePage := NewPage;
  NewPage.Show;
  //FPages.AddObject(NewPage.Name, NewPage);
  FPages.Insert(aIndex);
  FPages.Items[aIndex].DisplayName := NewPage.Name;
  FPages.Items[aIndex].TabPage := NewPage;
  NewPage.Index := FPages.Count - 1;
  FPageIndex := aIndex;
  //FPageCount := PageCount + 1;
end;

procedure TCDPageControl.RemovePage(aIndex: integer);
begin
  if aIndex > 0 then
    PageIndex := aIndex - 1
  else
    Exit;
  {if not GetHook(Hook) then
    exit;
  PageComponent := TPersistent(NewPage);
  Hook.DeletePersistent(PageComponent);   }
  //if NewPage <> nil then
  //  NewPage.Free;
  Application.ReleaseComponent(FPages[aIndex].TabPage);
  //Owner.RemoveComponent(FPages[aIndex].TabPage);
  //FPages[aIndex].TabPage := nil;
  FPages.Delete(aIndex);
  //Owner.RemoveComponent(NewPage);
  //if PControl.PageCount > 1 then
  //  PControl.FPageCount := PControl.PageCount - 1;
end;

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
  I, TempStartIndex: integer;
begin
  if FPages.Count <> 0 then
  begin
    //StartIndex := FPages.IndexOfObject(CurPage);
    TempStartIndex := CurPage.Index;
    if TempStartIndex = -1 then
      if GoForward then
        TempStartIndex := FPages.Count - 1
      else
        TempStartIndex := 0;
    I := TempStartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FPages.Count then
          I := 0;
      end
      else
      begin
        if I = 0 then
          I := FPages.Count;
        Dec(I);
      end;
      Result := FPages[I].TabPage;
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
  Width := 232;
  Height := 184;
  TabStop := False;
  FDrawerWinCE := TCDPageControlDrawerWinCE.Create;
  CustomDrawer := FDrawerWinCE; // Dummy to avoid designer crashes
  FCaptionHeight := 28;
  ParentColor := True;
  parentFont := True;
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks,
    csDesignInteractive];
  FPages := TTabItemList.Create(TTabItem);
  //FPageCount := 0;
  RButtHeight := CaptionHeight - 4;
  FStartIndex := 0;
  MaskHeadBmp := TBitmap.Create;
end;

destructor TCDPageControl.Destroy;
begin
  MaskHeadBmp.Free;
  FPages.Free;
  inherited Destroy;
end;

procedure TCDPageControl.SetStartIndex(Value: integer);
begin
  if (Value < -1) or (Value >= FPages.Count) then
    Exit;
  FStartIndex := Value;
end;

procedure TCDPageControl.SetActivePage(Value: TCDTabSheet);
var
  i: integer;
begin
  for i := 0 to FPages.Count - 1 do
  begin
    if FPages[i].TabPage = Value then
    begin
      Value.Show;

      // Check first, Tab is Visible?
      StartIndex := i;
      FPageIndex := i;
    end
    else
      Value.Hide;
  end;
  FActivePage := Value;
  Value.BringToFront;
  Invalidate;
end;

procedure TCDPageControl.SetPageIndex(Value: integer);
begin
  if (Value > -1) and (Value < FPages.Count) then
  begin
    FPageIndex := Value;
    ActivePage := FPages[Value].TabPage;
    Invalidate;
  end;
end;

procedure TCDPageControl.EraseBackground(DC: HDC);
begin

end;

function TCDPageControl.GetPageIndexFromXY(x, y: integer): integer;
begin
  if MaskHeadBmp.Canvas.Pixels[x, y] = clWhite then
    Result := -1
  else
    Result := MaskHeadBmp.Canvas.Pixels[x, y] - MaskBaseColor + StartIndex;
  if (PageIndex <> Result) and (PageIndex > -1) then
    PageIndex := Result;
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
    ABmp.Canvas.Font.Assign(Font);
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
  RButtHeight := Value - 4;
  invalidate;
end;

procedure TCDPageControl.DrawCaptionBar(ADest: TCanvas; lRect: TRect; CL: TColor);
var
  aRect, bRect, cRect: TRect;
  i: integer;
  rWidth: integer;
  aText, bText: string;
  CornerColor: TFPColor;
  MaskColor: TColor;
begin
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
  MaskHeadBmp.Width := lRect.Right - lRect.Left;
  MaskHeadBmp.Height := lRect.Bottom - lRect.Top;
  MaskHeadBmp.Canvas.Brush.Style := bsSolid;
  MaskHeadBmp.Canvas.Pen.Style := psSolid;
  MaskHeadBmp.Canvas.Pen.Color := clWhite;
  MaskHeadBmp.Canvas.Brush.Color := MaskHeadBmp.Canvas.Pen.Color;
  MaskHeadBmp.Canvas.Rectangle(lRect);
  if FPages.Count = 0 then
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
  if Brush.Style = bsSolid then
    SetBkMode(ADest.Handle, OPAQUE)
  else
    SetBkMode(ADest.Handle, TRANSPARENT);
  for i := StartIndex to FPages.Count - 1 do
  begin
    aText := FPages[i].TabPage.Caption;
    rWidth := (CaptionHeight - ADest.TextHeight(aText)) + ADest.TextWidth(aText);
    FPages[i].Width := rWidth;
    if aRect.Left + rWidth > lRect.Right - 6 then
      Break
    else
      aRect.Right := aRect.Left + rWidth;
    if PageIndex = i then
    begin
      cRect := aRect;
      if i = StartIndex then
        cRect.Left := aRect.Left - 2
      else
        cRect.Left := aRect.Left - 4;
      cRect.Right := aRect.Right + 4;
      cRect.Top := cRect.Top - 2;
      bText := FPages[i].TabPage.Caption;
    end
    else
      DrawTabHead(aDest, aRect, Color, False);
    MaskColor := MaskBaseColor + i - StartIndex;
    DrawTabHeadMask(MaskHeadBmp.Canvas, aRect, MaskColor, False);
    ADest.TextOut(aRect.Left + (aRect.Right - aRect.Left - ADest.TextWidth(aText)) div 2,
      aRect.Top + (aRect.Bottom - aRect.Top - ADest.TextHeight(aText)) div 2, aText);
    aRect.Left := aRect.Right + 3;
  end;
  //ADest.Draw(0,0,MaskHeadBmp);           Exit;
  ADest.Line(lRect.Left, lRect.Bottom - 1, cRect.Left, lRect.Bottom - 1);
  ADest.Line(cRect.Right, lRect.Bottom - 1, lRect.Right, lRect.Bottom - 1);
  DrawTabHead(aDest, cRect, clWhite, True);
  ADest.TextOut(cRect.Left + (cRect.Right - cRect.Left - ADest.TextWidth(bText)) div 2,
    cRect.Top + (cRect.Bottom - cRect.Top - ADest.TextHeight(bText)) div 2, bText);
  if not CheckTabButton(lRect.Right - lRect.Left, FPages) then
    Exit;
  aRect.Left := lRect.Right - RButtHeight * 2 - 3;
  aRect.Top := 1;
  aRect.Bottom := RButtHeight + 1;
  aRect.Right := lRect.Right - RButtHeight;
  if FMDownL then
    GradFill(ADest, aRect, $00F1A079, $00EFAF9B)
  else
    GradFill(ADest, aRect, $00FDD9CB, $00F2C9B8);
  aRect.Left := lRect.Right - RButtHeight - 1;
  aRect.Top := 1;
  aRect.Bottom := RButtHeight + 1;
  aRect.Right := lRect.Right;
  if FMDownR then
    GradFill(ADest, aRect, $00F1A079, $00EFAF9B)
  else
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
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($00FFFFFF));
  //ADest.Line();
end;

procedure TCDPageControl.SetCDPages(Value: TTabItemList);
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
    RButtHeight := CaptionHeight - 4;
  end;
  inherited;
end;

procedure TCDPageControl.UpdateAllDesignerFlags;
var
  i: integer;
begin
  for i := 0 to FPages.Count - 1 do
    UpdateDesignerFlags(i);
end;

procedure TCDPageControl.DoLeftButtonDown;
begin
  if StartIndex > 0 then
    StartIndex := StartIndex - 1;
  invalidate;
end;

procedure TCDPageControl.DoRightButtonDown;
begin
  if StartIndex < FPages.Count - 1 then
    StartIndex := StartIndex + 1;
  invalidate;
end;

procedure TCDPageControl.UpdateDesignerFlags(APageIndex: integer);
begin
  if APageIndex <> fPageIndex then
    FPages[APageIndex].TabPage.ControlStyle :=
      FPages[APageIndex].TabPage.ControlStyle + [csNoDesignVisible]
  else
    FPages[APageIndex].TabPage.ControlStyle :=
      FPages[APageIndex].TabPage.ControlStyle - [csNoDesignVisible];
end;

procedure TCDPageControl.SetMouseUP;
begin
  FMDownL := False;
  FMDownR := False;
end;

function TCDPageControl.GetPageCount: integer;
begin
  Result := FPages.Count;
end;

procedure TCDPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if (X > Width - RButtHeight * 2 - 2) and (X < Width - RButtHeight) then
  begin
    FMDownL := True;
    DoLeftButtonDown;
  end
  else
  if (X > Width - RButtHeight - 1) and (X < Width - 1) then
  begin
    FMDownR := True;
    DoRightButtonDown;
  end
  else
    SetMouseUP;
  if (Y < 3) or (Y > RButtHeight + 1) then
    SetMouseUP;
  if (Y < CaptionHeight) and (Y > 0) and (X > 0) and (X < Width) then
    GetPageIndexFromXY(X, Y);
  invalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCDPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  SetMouseUP;
  invalidate;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCDPageControl.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if (X > Width - RButtHeight * 2 - 2) and (X < Width - RButtHeight) then
    FMEnterL := True
  else
  if (X > Width - RButtHeight - 1) and (X < Width - 1) then
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
  SetMouseUP;
  invalidate;
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
  ADest.Rectangle(0, CDPageControl.CaptionHeight, CDPageControl.Width,
    CDPageControl.Height);
  ADest.Colors[CDPageControl.Width - 1, CDPageControl.CaptionHeight] :=
    TColorToFPColor(ColorToRGB(CDPageControl.Color));
  // frame
  //ADest.Pen.FPColor := colBlack;
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($009C9B91));
  if CDPageControl.Pages.Count = 0 then
    ADest.Rectangle(0, 0, CDPageControl.Width - 2, CDPageControl.Height - 2)
  else
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
  CDPageControl.DrawCaptionBar(ADest, Rect(0, 0, CDPageControl.Width -
    2, CDPageControl.CaptionHeight + 1), CDPageControl.Color);
end;

end.

