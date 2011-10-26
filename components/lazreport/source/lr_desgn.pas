
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Report Designer             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Desgn;

interface

{$I lr_vers.inc}

{.$Define ExtOI} // External Custom Object inspector (Christian)
{.$Define StdOI} // External Standard Object inspector (Jesus)
{$define sbod}  // status bar owner draw
uses
  Classes, SysUtils, FileUtil, LResources, LMessages,
  Forms, Controls, Graphics, Dialogs,ComCtrls,
  ExtCtrls, Buttons, StdCtrls,Menus,

  LCLType,LCLIntf,LCLProc,GraphType,Printers,

  ObjectInspector, PropEdits,TypInfo,
  
  LR_Class, LR_Color,LR_Edit;


const
  MaxUndoBuffer         = 100;
  crPencil              = 11;
  dtFastReportForm      = 1;
  dtFastReportTemplate  = 2;
  dtLazReportForm       = 3;

type
  TfrDesignerForm = class;
  TfrDesigner = class(TComponent)  // fake component
  private
    FTemplDir: String;
  public
    procedure Loaded; override;
  published
    property TemplateDir: String read FTemplDir write FTemplDir;
  end;

  TfrSelectionType = (ssBand, ssMemo, ssOther, ssMultiple, ssClipboardFull);
  TfrSelectionStatus = set of TfrSelectionType;
  TfrReportUnits = (ruPixels, ruMM, ruInches);
  TfrShapeMode = (smFrame, smAll);

  TfrUndoAction = (acInsert, acDelete, acEdit, acZOrder);
  PfrUndoObj = ^TfrUndoObj;
  TfrUndoObj = record
    Next: PfrUndoObj;
    ObjID: Integer;
    ObjPtr: TfrView;
    Int: Integer;
  end;

  TfrUndoRec = record
    Action: TfrUndoAction;
    Page: Integer;
    Objects: PfrUndoObj;
  end;

  PfrUndoRec1 = ^TfrUndoRec1;
  TfrUndoRec1 = record
    ObjPtr: TfrView;
    Int: Integer;
  end;

  PfrUndoBuffer = ^TfrUndoBuffer;
  TfrUndoBuffer = Array[0..MaxUndoBuffer - 1] of TfrUndoRec;

  TfrMenuItemInfo = class(TObject)
  private
    MenuItem: TMenuItem;
    Btn     : TSpeedButton;
  end;
  
  TfrDesignerDrawMode = (dmAll, dmSelection, dmShape);
  TfrCursorType       = (ctNone, ct1, ct2, ct3, ct4, ct5, ct6, ct7, ct8);
  TfrDesignMode       = (mdInsert, mdSelect);
  
  TfrSplitInfo = record
    SplRect: TRect;
    SplX   : Integer;
    View1,
    View2  : TfrView;
  end;

  { TfrObjectInspector }
  TfrObjectInspector = Class({$IFDEF EXTOI}TForm{$ELSE}TPanel{$ENDIF})
  private
    fPropertyGrid : TCustomPropertiesGrid;
    {$IFNDEF EXTOI}
    fcboxObjList  : TComboBox;
    fBtn,fBtn2    : TButton;
    fPanelHeader  : TPanel;
    fLastHeight   : Word;
    fDown         : Boolean;
    fPt           : TPoint;

    procedure BtnClick(Sender : TObject);
    procedure HeaderMDown(Sender: TOBject; Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
    procedure HeaderMMove(Sender: TObject; Shift: TShiftState; X,
                  Y: Integer);
    procedure HeaderMUp(Sender: TOBject; Button: TMouseButton;
                   Shift: TShiftState; X, Y: Integer);
    {$ENDIF}
  protected
    procedure CMVisibleChanged(var TheMessage: TLMessage); message CM_VISIBLECHANGED;
    {$IFDEF EXTOI}
    procedure DoHide; override;
    {$ENDIF}
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;

    procedure Select(Obj: TObject);
    procedure cboxObjListOnChanged(Sender: TObject);
    procedure SetModifiedEvent(AEvent: TNotifyEvent);
    procedure Refresh;
  end;
  
  { TfrDesignerPage }

  TfrDesignerPage = class(TCustomControl)
  private
    Down,                          // mouse button was pressed
    Moved,                         // mouse was moved (with pressed btn)
    DFlag,                         // was double click
    RFlag: Boolean;                // selecting objects by framing
    Mode : TfrDesignMode;          // current mode
    CT   : TfrCursorType;          // cursor type
    LastX, LastY: Integer;         // here stored last mouse coords
    SplitInfo: TfrSplitInfo;
    RightBottom: Integer;
    LeftTop: TPoint;
    FirstBandMove: Boolean;
    FDesigner: TfrDesignerForm;
    
    fOldFocusRect : TRect;
    fForm         : TfrDialogForm;
    
    procedure NormalizeRect(var r: TRect);
    procedure NormalizeCoord(t: TfrView);
    function FindNearestEdge(var x, y: Integer): Boolean;
    procedure RoundCoord(var x, y: Integer);
    procedure Draw(N: Integer; AClipRgn: HRGN);
    procedure DrawPage(DrawMode: TfrDesignerDrawMode);
    procedure DrawRectLine(Rect: TRect);
    procedure DrawFocusRect(aRect: TRect);
    procedure DrawHSplitter(Rect: TRect);
    procedure DrawSelection(t: TfrView);
    procedure DrawShape(t: TfrView);
    
    procedure DrawDialog;

    procedure MDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    procedure DClick(Sender: TObject);
    procedure MoveResize(Kx,Ky:Integer; UseFrames,AResize: boolean);
  protected
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Init;
    procedure SetPage;
    procedure GetMultipleSelected;
  end;

  { TfrDesignerForm }

  TfrDesignerForm = class(TfrReportDesigner)
    frSpeedButton1: TSpeedButton;
    frSpeedButton2: TSpeedButton;
    frSpeedButton3: TSpeedButton;
    frSpeedButton4: TSpeedButton;
    frSpeedButton5: TSpeedButton;
    frSpeedButton6: TSpeedButton;
    frTBSeparator16: TPanel;
    Image1: TImage;
    Image2: TImage;
    ImgIndic: TImageList;
    LinePanel: TPanel;
    OB7: TSpeedButton;
    panForDlg: TPanel;
    PgB4: TSpeedButton;
    Tab1: TTabControl;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    frDock1: TPanel;
    frDock2: TPanel;
    Popup1: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    EditMenu: TMenuItem;
    ToolMenu: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N19: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    N27: TMenuItem;
    N28: TMenuItem;
    N26: TMenuItem;
    N29: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    N33: TMenuItem;
    N36: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ImageList1: TImageList;
    Pan5: TMenuItem;
    N8: TMenuItem;
    ImageList2: TImageList;
    N38: TMenuItem;
    Pan6: TMenuItem;
    N39: TMenuItem;
    N40: TMenuItem;
    N42: TMenuItem;
    MastMenu: TMenuItem;
    N16: TMenuItem;
    Panel2: TPanel;
    FileBtn1: TSpeedButton;
    FileBtn2: TSpeedButton;
    FileBtn3: TSpeedButton;
    FileBtn4: TSpeedButton;
    CutB: TSpeedButton;
    CopyB: TSpeedButton;
    PstB: TSpeedButton;
    ZB1: TSpeedButton;
    ZB2: TSpeedButton;
    SelAllB: TSpeedButton;
    PgB1: TSpeedButton;
    PgB2: TSpeedButton;
    PgB3: TSpeedButton;
    GB1: TSpeedButton;
    GB2: TSpeedButton;
    ExitB: TSpeedButton;
    Panel3: TPanel;
    AlB1: TSpeedButton;
    AlB2: TSpeedButton;
    AlB3: TSpeedButton;
    AlB4: TSpeedButton;
    AlB5: TSpeedButton;
    FnB1: TSpeedButton;
    FnB2: TSpeedButton;
    FnB3: TSpeedButton;
    ClB2: TSpeedButton;
    HlB1: TSpeedButton;
    AlB6: TSpeedButton;
    AlB7: TSpeedButton;
    Panel1: TPanel;
    FrB1: TSpeedButton;
    FrB2: TSpeedButton;
    FrB3: TSpeedButton;
    FrB4: TSpeedButton;
    ClB1: TSpeedButton;
    ClB3: TSpeedButton;
    FrB5: TSpeedButton;
    FrB6: TSpeedButton;
    frTBSeparator1: TPanel;
    frTBSeparator2: TPanel;
    frTBSeparator3: TPanel;
    frTBSeparator4: TPanel;
    frTBSeparator5: TPanel;
    frTBPanel1: TPanel;
    C3: TComboBox;
    C2: TComboBox;
    frTBPanel2: TPanel;
    frTBSeparator6: TPanel;
    frTBSeparator7: TPanel;
    frTBSeparator8: TPanel;
    frTBSeparator9: TPanel;
    frTBSeparator10: TPanel;
    N37: TMenuItem;
    Pan2: TMenuItem;
    Pan3: TMenuItem;
    Pan1: TMenuItem;
    Pan4: TMenuItem;
    Panel4: TPanel;
    OB1: TSpeedButton;
    OB2: TSpeedButton;
    OB3: TSpeedButton;
    OB4: TSpeedButton;
    OB5: TSpeedButton;
    frTBSeparator12: TPanel;
    Panel5: TPanel;
    Align1: TSpeedButton;
    Align2: TSpeedButton;
    Align3: TSpeedButton;
    Align4: TSpeedButton;
    Align5: TSpeedButton;
    Align6: TSpeedButton;
    Align7: TSpeedButton;
    Align8: TSpeedButton;
    Align9: TSpeedButton;
    Align10: TSpeedButton;
    frTBSeparator13: TPanel;
//**    Tab1: TTabControl;
    frDock4: TPanel;
    HelpMenu: TMenuItem;
    N34: TMenuItem;
    GB3: TSpeedButton;
    N46: TMenuItem;
    N47: TMenuItem;
    UndoB: TSpeedButton;
    frTBSeparator14: TPanel;
    AlB8: TSpeedButton;
    RedoB: TSpeedButton;
    N48: TMenuItem;
    OB6: TSpeedButton;
    frTBSeparator15: TPanel;
    Panel6: TPanel;
    Pan7: TMenuItem;
    N14: TMenuItem;
    Panel7: TPanel;
    PBox1: TPaintBox;
    N17: TMenuItem;
    E1: TEdit;
    Panel8: TPanel;
    SB1: TSpeedButton;
    SB2: TSpeedButton;
    HelpBtn: TSpeedButton;
    frTBSeparator11: TPanel;
    N18: TMenuItem;
    N22: TMenuItem;
    N35: TMenuItem;
    Popup2: TPopupMenu;
    N41: TMenuItem;
    N43: TMenuItem;
    N44: TMenuItem;
    StB1: TSpeedButton;
    procedure C2GetItems(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
    procedure DoClick(Sender: TObject);
    procedure ClB1Click(Sender: TObject);
    procedure GB1Click(Sender: TObject);
    procedure ZB1Click(Sender: TObject);
    procedure ZB2Click(Sender: TObject);
    procedure PgB1Click(Sender: TObject);
    procedure PgB2Click(Sender: TObject);
    procedure OB2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OB1Click(Sender: TObject);
    procedure CutBClick(Sender: TObject);
    procedure CopyBClick(Sender: TObject);
    procedure PstBClick(Sender: TObject);
    procedure SelAllBClick(Sender: TObject);
    procedure ExitBClick(Sender: TObject);
    procedure PgB3Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure N5Click(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure GB2Click(Sender: TObject);
    procedure FileBtn1Click(Sender: TObject);
    procedure FileBtn2Click(Sender: TObject);
    procedure FileBtn3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure N8Click(Sender: TObject);
    procedure C2DrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure HlB1Click(Sender: TObject);
    procedure FileBtn4Click(Sender: TObject);
    procedure N42Click(Sender: TObject);
    procedure Popup1Popup(Sender: TObject);
    procedure N23Click(Sender: TObject);
    procedure N37Click(Sender: TObject);
    procedure Pan2Click(Sender: TObject);
    procedure N14Click(Sender: TObject);
    procedure Align1Click(Sender: TObject);
    procedure Align2Click(Sender: TObject);
    procedure Align3Click(Sender: TObject);
    procedure Align4Click(Sender: TObject);
    procedure Align5Click(Sender: TObject);
    procedure Align6Click(Sender: TObject);
    procedure Align7Click(Sender: TObject);
    procedure Align8Click(Sender: TObject);
    procedure Align9Click(Sender: TObject);
    procedure Align10Click(Sender: TObject);
    procedure Tab1Change(Sender: TObject);
    procedure N34Click(Sender: TObject);
    procedure GB3Click(Sender: TObject);
    procedure UndoBClick(Sender: TObject);
    procedure RedoBClick(Sender: TObject);
    procedure N20Click(Sender: TObject);
    procedure PBox1Paint(Sender: TObject);
    procedure SB1Click(Sender: TObject);
    procedure SB2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure N22Click(Sender: TObject);
    procedure Tab1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure frDesignerFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frDesignerFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure frSpeedButton1Click(Sender: TObject);
    procedure StB1Click(Sender: TObject);
  private
    { Private declarations }
    fInBuildPage : Boolean;
    
    PageView: TfrDesignerPage;
    EditorForm: TfrEditorForm;
    ColorSelector: TColorSelector;
    MenuItems: TFpList;
    ItemWidths: TStringList;
    FCurPage: Integer;
    FGridSize: Integer;
    FGridShow, FGridAlign: Boolean;
    FUnits: TfrReportUnits;
    FGrayedButtons: Boolean;
    FUndoBuffer, FRedoBuffer: TfrUndoBuffer;
    FUndoBufferLength, FRedoBufferLength: Integer;
    FirstTime: Boolean;
    MaxItemWidth, MaxShortCutWidth: Integer;
    FirstInstance: Boolean;
    EditAfterInsert: Boolean;
    FCurDocName, FCaption: String;
    fCurDocFileType: Integer;
    FileModified: Boolean;
    ShapeMode: TfrShapeMode;
    
    {$IFDEF StdOI}
    ObjInsp  : TObjectInspector;
    PropHook : TPropertyEditorHook;
    {$ELSE}
    ObjInsp  : TfrObjectInspector;
    {$ENDIF}
    procedure ObjInspSelect(Obj:TObject);
    procedure ObjInspRefresh;

    procedure GetFontList;
    procedure SetMenuBitmaps;
    procedure SetCurPage(Value: Integer);
    procedure SetGridSize(Value: Integer);
    procedure SetGridShow(Value: Boolean);
    procedure SetGridAlign(Value: Boolean);
    procedure SetUnits(Value: TfrReportUnits);
    procedure SetGrayedButtons(Value: Boolean);
    procedure SetCurDocName(Value: String);
    procedure SelectionChanged;
    procedure ShowPosition;
    procedure ShowContent;
    procedure EnableControls;
    procedure ResetSelection;
    procedure DeleteObjects;
    procedure AddPage(ClName : string);
    procedure RemovePage(n: Integer);
    procedure SetPageTitles;
//**    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure FillInspFields;
    function RectTypEnabled: Boolean;
    function FontTypEnabled: Boolean;
    function ZEnabled: Boolean;
    function CutEnabled: Boolean;
    function CopyEnabled: Boolean;
    function PasteEnabled: Boolean;
    function DelEnabled: Boolean;
    function EditEnabled: Boolean;
    procedure ColorSelected(Sender: TObject);
    procedure SelectAll;
    procedure Unselect;
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure SaveState;
    procedure RestoreState;
    procedure ClearBuffer(Buffer: TfrUndoBuffer; var BufferLength: Integer);
    procedure ClearUndoBuffer;
    procedure ClearRedoBuffer;
    procedure Undo(Buffer: PfrUndoBuffer);
    procedure ReleaseAction(ActionRec: TfrUndoRec);
    procedure AddAction(Buffer: PfrUndoBuffer; a: TfrUndoAction; List: TFpList);
    procedure AddUndoAction(a: TfrUndoAction);
    procedure DoDrawText(aCanvas: TCanvas; aCaption: string;
      Rect: TRect; Selected, aEnabled: Boolean; Flags: Longint);
    procedure MeasureItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
      var AWidth, AHeight: Integer);
    procedure DrawItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
      ARect: TRect; Selected: Boolean);
    procedure InsFieldsClick(Sender: TObject);
    function FindMenuItem(AMenuItem: TMenuItem): TfrMenuItemInfo;
    procedure SetMenuItemBitmap(AMenuItem: TMenuItem; ABtn:TSpeedButton);
    procedure FillMenuItems(MenuItem: TMenuItem);
    procedure DeleteMenuItems(MenuItem: TMenuItem);
    procedure OnActivateApp(Sender: TObject);
    procedure OnDeactivateApp(Sender: TObject);
    procedure GetDefaultSize(var dx, dy: Integer);
    function SelStatus: TfrSelectionStatus;
    procedure UpdScrollbars;
    procedure InsertFieldsFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure InsertDbFields;
    {$ifdef sbod}
    procedure DrawStatusPanel(const ACanvas:TCanvas; const rect:  TRect);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    {$endif}
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    
    procedure WndProc(var Message: TLMessage); override;
    procedure RegisterObject(ButtonBmp: TBitmap; const ButtonHint: String;
      ButtonTag: Integer); override;
    procedure RegisterTool(const MenuCaption: String; ButtonBmp: TBitmap;
      OnClickEvnt: TNotifyEvent); override;
    procedure BeforeChange; override;
    procedure AfterChange; override;
    procedure ShowMemoEditor;
    procedure ShowEditor;
    procedure RedrawPage; override;
    procedure OnModify(sender: TObject);
    function PointsToUnits(x: Integer): Double;  override;
    function UnitsToPoints(x: Double): Integer;  override;
    procedure MoveObjects(dx, dy: Integer; aResize: Boolean);
    procedure UpdateStatus;

    property CurDocName: String read FCurDocName write SetCurDocName;
    property CurPage: Integer read FCurPage write SetCurPage;
    property GridSize: Integer read FGridSize write SetGridSize;
    property ShowGrid: Boolean read FGridShow write SetGridShow;
    property GridAlign: Boolean read FGridAlign write SetGridAlign;
    property Units: TfrReportUnits read FUnits write SetUnits;
    property GrayedButtons: Boolean read FGrayedButtons write SetGrayedButtons;
  end;

procedure frSetGlyph(aColor: TColor; sb: TSpeedButton; n: Integer);
function frCheckBand(b: TfrBandType): Boolean;

var
  frTemplateDir: String;


implementation

{$R *.lfm}

uses
  LR_Pgopt, LR_GEdit, LR_Templ, LR_Newrp, LR_DsOpt, LR_Const,
  LR_Prntr, LR_Hilit, LR_Flds, LR_Dopt, LR_Ev_ed, LR_BndEd, LR_VBnd,
  LR_BTyp, LR_Utils, LR_GrpEd, LR_About, LR_IFlds, LR_DBRel,LR_DBSet,
  DB;

type
  THackView = class(TfrView)
  end;

function GetUnusedBand: TfrBandType; forward;
procedure SendBandsToDown; forward;
procedure ClearClipBoard; forward;
function Objects: TFpList; forward;
procedure GetRegion; forward;
function TopSelected: Integer; forward;

var
  FirstInst          : Boolean=True;// First instance
  FirstSelected      : TfrView;     // First Selected Object
  SelNum             : Integer;     // number of objects currently selected
  MRFlag             : Boolean;     // several objects was selected
  ObjRepeat          : Boolean;     // was pressed Shift + Insert Object
  WasOk              : Boolean;     // was Ok pressed in dialog
  OldRect,OldRect1   : TRect;       // object rect after mouse was clicked
  Busy               : Boolean;     // busy flag. need!
  ShowSizes          : Boolean;
  LastFontName       : String;
  LastFontSize       : Integer;
  LastAdjust         : Integer;
  LastFrameWidth     : Single;
  LastLineWidth      : Single;
  LastFrames         : TfrFrameBorders;
  LastFontStyle      : Word;
  LastFrameColor     : TColor;
  LastFillColor      : TColor;
  LastFontColor      : TColor;
  ClrButton          : TSpeedButton;
  FirstChange        : Boolean;
  ClipRgn            : HRGN;

// globals
  ClipBd             : TFpList;       // clipboard
  GridBitmap         : TBitmap;     // for drawing grid in design time
  ColorLocked        : Boolean;     // true to avoid unwished color change


{----------------------------------------------------------------------------}
procedure AddRgn(var HR: HRGN; T: TfrView);
var
  tr: HRGN;
begin
  tr := t.GetClipRgn(rtExtended);
  CombineRgn(HR, HR, TR, RGN_OR);
  DeleteObject(TR);
end;

{----------------------------------------------------------------------------}
procedure TfrDesigner.Loaded;
begin
  inherited Loaded;
  frTemplateDir := TemplateDir;
end;

{--------------------------------------------------}
constructor TfrDesignerPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent      := AOwner as TWinControl;
  Color       := clWhite;
  OnMouseDown := @MDown;
  OnMouseUp   := @MUp;
  OnMouseMove := @MMove;
  OnDblClick  := @DClick;
end;

procedure TfrDesignerPage.Init;
begin
  Down := False;
  DFlag:= False;
  RFlag := False;
  Cursor := crDefault;
  CT := ctNone;
end;

procedure TfrDesignerPage.SetPage;
var
  Pgw,Pgh: Integer;
begin
  if not Assigned(FDesigner.Page) then Exit;
  
  FDesigner.panForDlg.Visible:=(FDesigner.Page is TfrPageDialog);
  FDesigner.panel4.Visible   :=not FDesigner.panForDlg.Visible;
  
  if (FDesigner.Page is TfrPageDialog) then
  begin
    Color:=clBtnFace;
    SetBounds(10, 10,TfrPageDialog(FDesigner.Page).Width,TfrPageDialog(FDesigner.Page).Height);
  end
  else
  begin
    Pgw := FDesigner.Page.PrnInfo.Pgw;
    Pgh := FDesigner.Page.PrnInfo.Pgh;
    if Pgw > Parent.Width then
      SetBounds(10, 10, Pgw, Pgh)
    else
      SetBounds((Parent.Width - Pgw) div 2, 10, Pgw, Pgh);
  end;
end;

procedure TfrDesignerPage.Paint;
begin
  Draw(10000, 0);
end;

procedure TfrDesignerPage.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //do nothing to avoid flicker
end;

procedure TfrDesignerPage.NormalizeCoord(t: TfrView);
begin
  if t.dx < 0 then
  begin
    t.dx := -t.dx;
    t.x := t.x - t.dx;
  end;
  if t.dy < 0 then
  begin
    t.dy := -t.dy;
    t.y := t.y - t.dy;
  end;
end;

procedure TfrDesignerPage.NormalizeRect(var r: TRect);
var
  i: Integer;
begin
  with r do
  begin
    if Left > Right then
    begin
      i := Left;
      Left := Right;
      Right := i;
    end;
    if Top > Bottom then
    begin
      i := Top;
      Top := Bottom;
      Bottom := i;
    end;
  end;
end;

procedure TfrDesignerPage.DrawHSplitter(Rect: TRect);
begin
  with Canvas do
  begin
    Pen.Mode := pmXor;
    Pen.Color := clSilver;
    Pen.Width := 1;
    MoveTo(Rect.Left, Rect.Top);
    LineTo(Rect.Right, Rect.Bottom);
    Pen.Mode := pmCopy;
  end;
end;

procedure TfrDesignerPage.DrawRectLine(Rect: TRect);
begin
  with Canvas do
  begin
    Pen.Mode := pmNot;
    Pen.Style := psSolid;
    Pen.Width := Round(LastLineWidth);
    with Rect do
    begin
      if Abs(Right - Left) > Abs(Bottom - Top) then
      begin
        MoveTo(Left, Top);
        LineTo(Right, Top);
      end
      else
      begin
        MoveTo(Left, Top);
        LineTo(Left, Bottom);
      end;
    end;
    Pen.Mode := pmCopy;
  end;
end;

procedure DrawRubberRect(Canvas: TCanvas; aRect: TRect; Color: TColor);
  procedure DrawVertLine(X1,Y1,Y2: integer);
  Var Cl : TColor;
  begin
    Cl:=Canvas.Pen.Color;
    try
      if Y2<Y1 then
        while Y2<Y1 do
        begin
          Canvas.Pen.Color:=Color;
          Canvas.MoveTo(X1,Y1);
          Canvas.LineTo(X1,Y1+1);
          //Canvas.Pixels[X1, Y1] := Color;
          dec(Y1, 2);
        end
      else
        while Y1<Y2 do
        begin
          Canvas.Pen.Color:=Color;
          Canvas.MoveTo(X1,Y1);
          Canvas.LineTo(X1,Y1+1);
          //Canvas.Pixels[X1, Y1] := Color;
          inc(Y1, 2);
        end;
    finally
      Canvas.Pen.Color:=cl;
    end;
  end;
  
  procedure DrawHorzLine(X1,Y1,X2: integer);
  Var Cl : TColor;
  begin
    Cl:=Canvas.Pen.Color;
    try
      if X2<X1 then
        while X2<X1 do
        begin
          Canvas.Pen.Color:=Color;
          Canvas.MoveTo(X1,Y1);
          Canvas.LineTo(X1+1,Y1);
          //Canvas.Pixels[X1, Y1] := Color;
          dec(X1, 2);
        end
      else
        while X1<X2 do
        begin
          Canvas.Pen.Color:=Color;
          Canvas.MoveTo(X1,Y1);
          Canvas.LineTo(X1+1,Y1);
          //Canvas.Pixels[X1, Y1] := Color;
          inc(X1, 2);
        end;
    finally
      Canvas.Pen.Color:=cl;
    end;
  end;
begin
  with aRect do
  begin
    DrawHorzLine(Left, Top, Right-1);
    DrawVertLine(Right-1, Top, Bottom-1);
    DrawHorzLine(Right-1, Bottom-1, Left);
    DrawVertLine(Left, Bottom-1, Top);
  end;
end;

procedure TfrDesignerPage.DrawFocusRect(aRect: TRect);
var
  DCIndex: Integer;
begin
  with Canvas do
  begin
    DCIndex := SaveDC(Handle);
    Pen.Mode := pmXor;
    Pen.Color := clWhite;
    //DrawRubberRect(Canvas, aRect, clWhite);
    Pen.Width := 1;
    Pen.Style := psDot;
    MoveTo(aRect.Left, aRect.Top);
    LineTo(aRect.Right, aRect.Top);
    LineTo(aRect.Right, aRect.Bottom);
    LineTo(aRect.left, aRect.Bottom);
    LineTo(aRect.left, aRect.Top);
    //Brush.Style := bsClear;
    //Rectangle(aRect);
    RestoreDC(Handle, DCIndex);
    Pen.Mode := pmCopy;
    fOldFocusRect:=aRect;
  end;
end;

procedure TfrDesignerPage.DrawSelection(t: TfrView);
var
  px, py: Word;
  procedure DrawPoint(x, y: Word);
  begin
    Canvas.EllipseC(x,y,1,1);
    //Canvas.MoveTo(x, y);
    //Canvas.LineTo(x, y);
  end;
begin
  if t.Selected then
  with t, Self.Canvas do
  begin
    Pen.Width := 5;
    Pen.Mode := pmXor;
    Pen.Color := clWhite;
    px := x + dx div 2;
    py := y + dy div 2;

    DrawPoint(x, y); 

    if dx>0 then
      DrawPoint(x + dx, y);

    if dy>0 then
      DrawPoint(x, y + dy);

    if (dx>0) and (dy>0) then
    begin
      if Objects.IndexOf(t) = RightBottom then
        Pen.Color := clTeal;
      DrawPoint(x + dx, y + dy);
    end;

    Pen.Color := clWhite;
    if SelNum = 1 then
    begin
      if px>x then
        DrawPoint(px, y);

      if py>y then
        DrawPoint(x, py);

      if (py>y) and (px>x) then
      begin
        DrawPoint(px, y + dy);
        DrawPoint(x + dx, py);
      end;
    end;
    Pen.Mode := pmCopy;
    // NOTE: ROP mode under gtk is used not only to draw with pen but
    //       also any other filled graphics, the problem is that brush
    //       handle is not invalidated when pen has changed as result
    //       the ROP mode is not updated and next operation will use
    //       the old XOR mode.
    // TODO: Solve this problem in LCL-gtk, as workaround draw something
    //       using new pen.
    EllipseC(-100,-100,1,1);
  end;
end;

procedure TfrDesignerPage.DrawShape(t: TfrView);
begin
  if t.Selected then
  with t do
    DrawFocusRect(Rect(x, y, x + dx + 1, y + dy + 1));
end;

procedure TfrDesignerPage.DrawDialog;
Var Dlg : TfrPageDialog;
begin
  Dlg:=TfrPageDialog(FDesigner.Page);
  
  with Canvas do
  begin
    Brush.Color := clGray;
    FillRect(Rect(0,0,Width,Width));
    
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    Rectangle(Rect(0,0,FDesigner.Page.Width-1,FDesigner.Page.Height-1));
    Brush.Color := clBlue;
    Rectangle(Rect(0,0,FDesigner.Page.Width-1,20));
    
    Canvas.TextRect(Rect(0,0,FDesigner.Page.Width-1,20),1,5,Dlg.Caption);
  end;
end;

procedure TfrDesignerPage.Draw(N: Integer; AClipRgn: HRGN);
var
  i,iy      : Integer;
  t         : TfrView;
  R, R1     : HRGN;
  Objects   : TFpList;

  procedure DrawBackground;
  var
    i, j: Integer;
    Re: TRect;
  begin
    with Canvas do
    begin
      if FDesigner.ShowGrid and (FDesigner.GridSize <> 18) then
      begin
        with GridBitmap.Canvas do
        begin
          Brush.Color := clWhite;
          FillRect(Rect(0, 0, 8, 8));
          Pixels[0, 0] := clBlack;
          if FDesigner.GridSize = 4 then
          begin
            Pixels[4, 0] := clBlack;
            Pixels[0, 4] := clBlack;
            Pixels[4, 4] := clBlack;
          end;
        end;
        Brush.Bitmap := GridBitmap;
      end
      else
      begin
        Brush.Color := clWhite;
        Brush.Style := bsSolid;
        Brush.Bitmap:= nil;
      end;
      
      //FillRgn(Handle, R, Brush.Handle);
      GetRgnBox(R, @Re);
      FillRect(Re);

      if FDesigner.ShowGrid and (FDesigner.GridSize = 18) then
      begin
        i := 0;
        while i < Width do
        begin
          j := 0;
          while j < Height do
          begin
            if RectVisible(Handle, Rect(i, j, i + 1, j + 1)) then
              Pixels[i,j]:=clBlack;
            Inc(j, FDesigner.GridSize);
          end;
          Inc(i, FDesigner.GridSize);
        end;
      end;
      Brush.Style := bsClear;
      Pen.Width := 1;
      Pen.Color := clSilver;
      Pen.Style := psSolid;
      Pen.Mode := pmCopy;
      with FDesigner.Page do
      begin
        if UseMargins then
          Rectangle(LeftMargin, TopMargin, RightMargin, BottomMargin);
        if ColCount > 1 then
        begin
          ColWidth := (RightMargin - LeftMargin) div ColCount;
          Pen.Style := psDot;
          j := LeftMargin;
          for i := 1 to ColCount do
          begin
            Rectangle(j, -1, j + ColWidth + 1,  PrnInfo.Pgh + 1);
            Inc(j, ColWidth + ColGap);
          end;
          Pen.Style := psSolid;
        end;
      end;
    end;
  end;

  function ViewIsVisible(t: TfrView): Boolean;
  var
    Rn: HRGN;
  begin
    Rn := t.GetClipRgn(rtNormal);
    Result := CombineRgn(Rn, Rn, AClipRgn, RGN_AND) <> NULLREGION;
    DeleteObject(Rn);
  end;

begin
  if FDesigner.Page = nil then Exit;

  DocMode := dmDesigning;

  Objects := FDesigner.Page.Objects;

  if FDesigner.Page is TfrPageDialog then
  begin
    DrawDialog;
    Exit;
  end;

  if AClipRgn = 0 then
  begin
    with Canvas.ClipRect do
      AClipRgn := CreateRectRgn(Left, Top, Right, Bottom);
  end;


  {$IFDEF DebugLR}
  DebugLn('------------- Begin Draw() -------------');
  {$ENDIF}

  R:=CreateRectRgn(0, 0, Width, Height);
  for i:=Objects.Count-1 downto 0 do
  begin
    t := TfrView(Objects[i]);
    {$IFDEF DebugLR}
    DebugLn('Draw ',InttoStr(i),' ',t.Name);
    {$ENDIF}
    if i <= N then
    begin
      if t.selected then
        t.draw(canvas)
      else
      if ViewIsVisible(t) then
      begin
        R1 := CreateRectRgn(0, 0, 1, 1);
        CombineRgn(R1, AClipRgn, R, RGN_AND);
        SelectClipRgn(Canvas.Handle, R1);
        DeleteObject(R1);

        t.Draw(Canvas);

        iy:=1;
        //Show indicator if script it's not empty
        if t.Script.Count>0 then
        begin
          FDesigner.ImgIndic.Draw(Canvas, t.x+1, t.y+iy, 0);
          iy:=10;
        end;

        //Show indicator if hightlight it's not empty
        if (t is TfrMemoView) and (Trim(TfrmemoView(t).HighlightStr)<>'') then
          FDesigner.ImgIndic.Draw(Canvas, t.x+1, t.y+iy, 1);
      end;
    end;
    R1 := t.GetClipRgn(rtNormal);
    CombineRgn(R, R, R1, RGN_DIFF);
    DeleteObject(R1);
    SelectClipRgn(Canvas.Handle, R);
  end;

  {$IFDEF DebugLR}
  DebugLn('------------- End Draw() -------------');
  {$ENDIF}

  CombineRgn(R, R, AClipRgn, RGN_AND);

  DrawBackground;

  DeleteObject(R);
  DeleteObject(AClipRgn);
  if AClipRgn=ClipRgn then
    ClipRgn := 0;

  SelectClipRgn(Canvas.Handle, 0);

  if not Down then
    DrawPage(dmSelection);

end;

procedure TfrDesignerPage.DrawPage(DrawMode: TfrDesignerDrawMode);
var
  i: Integer;
  t: TfrView;
begin
  if DocMode <> dmDesigning then Exit;
  for i:=0 to Objects.Count-1 do
  begin
    t := TfrView(Objects[i]);
    case DrawMode of
      dmAll: t.Draw(Canvas);
      dmSelection: DrawSelection(t);
      dmShape: DrawShape(t);
    end;
  end;
end;

function TfrDesignerPage.FindNearestEdge(var x, y: Integer): Boolean;
var
  i: Integer;
  t: TfrView;
  min: Double;
  p: TPoint;
  
  function DoMin(a: Array of TPoint): Boolean;
  var
    i: Integer;
    d: Double;
  begin
    Result := False;
    for i := Low(a) to High(a) do
    begin
      d := sqrt((x - a[i].x) * (x - a[i].x) + (y - a[i].y) * (y - a[i].y));
      if d < min then
      begin
        min := d;
        p := a[i];
        Result := True;
      end;
    end;
  end;
  
begin
  Result := False;
  min := FDesigner.GridSize;
  p := Point(x, y);
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if DoMin([Point(t.x, t.y), Point(t.x + t.dx, t.y),
         Point(t.x + t.dx, t.y + t.dy),  Point(t.x, t.y + t.dy)]) then
      Result := True;
  end;

  x := p.x;
  y := p.y;
end;

procedure TfrDesignerPage.RoundCoord(var x, y: Integer);
begin
  with FDesigner do
    if GridAlign then
    begin
      x := x div GridSize * GridSize;
      y := y div GridSize * GridSize;
    end;
end;

procedure TfrDesignerPage.GetMultipleSelected;
var
  i, j, k: Integer;
  t: TfrView;
begin
  j := 0; k := 0;
  LeftTop := Point(10000, 10000);
  RightBottom := -1;
  MRFlag := False;
  if SelNum > 1 then                  {find right-bottom element}
  begin
    for i := 0 to Objects.Count-1 do
    begin
      t := TfrView(Objects[i]);
      if t.Selected then
      begin
        t.OriginalRect := Rect(t.x, t.y, t.dx, t.dy);
        if (t.x + t.dx > j) or ((t.x + t.dx = j) and (t.y + t.dy > k)) then
        begin
          j := t.x + t.dx;
          k := t.y + t.dy;
          RightBottom := i;
        end;
        if t.x < LeftTop.x then LeftTop.x := t.x;
        if t.y < LeftTop.y then LeftTop.y := t.y;
      end;
    end;
    t := TfrView(Objects[RightBottom]);
    OldRect := Rect(LeftTop.x, LeftTop.y, t.x + t.dx, t.y + t.dy);
    OldRect1 := OldRect;
    MRFlag := True;
  end;
end;

procedure TfrDesignerPage.MDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  f, DontChange, v: Boolean;
  t: TfrView;
  p: TPoint;
begin
  {$IFDEF DebugLR}
  DebugLn('TfrDesignerPage.MDown(X=',dbgs(x),',Y=',dbgs(y),'  INIT');
  DebugLn('  Down=', dbgs(Down),' FFlag=', dbgs(RFlag));
  {$ENDIF}
  if DFlag then
  begin
    DFlag := False;
    Exit;
  end;
  
  if (Button = mbRight) and Down and RFlag then
    DrawFocusRect(OldRect);
  
  RFlag := False;
  DrawPage(dmSelection);
  Down := True;
  DontChange := False;
  if Button = mbLeft then
  begin
    if (ssCtrl in Shift) or (Cursor = crCross) then
    begin
      RFlag := True;
      if Cursor = crCross then
      begin
        DrawFocusRect(OldRect);
        RoundCoord(x, y);
        OldRect1 := OldRect;
      end;
      OldRect := Rect(x, y, x, y);
      FDesigner.Unselect;
      SelNum := 0;
      RightBottom := -1;
      MRFlag := False;
      FirstSelected := nil;
      Exit;
    end
    else if Cursor = crPencil then
         begin
            with FDesigner do
            begin
              if GridAlign then
              begin
                if not FindNearestEdge(x, y) then
                begin
                  x := Round(x / GridSize) * GridSize;
                  y := Round(y / GridSize) * GridSize;
                end;
              end;
            end;
            OldRect := Rect(x, y, x, y);
            FDesigner.Unselect;
            SelNum := 0;
            RightBottom := -1;
            MRFlag := False;
            FirstSelected := nil;
            LastX := x;
            LastY := y;
            Exit;
         end;
  end;
  
  if Cursor = crDefault then
  begin
    f := False;
    for i := Objects.Count - 1 downto 0 do
    begin
      t := TfrView(Objects[i]);
      V:=t.PointInView(X,Y);
      {$IFDEF DebugLR}
      DebugLn(t.Name,' PointInView(Rgn, X, Y)=',dbgs(V),' Selected=',dbgs(t.selected));
      {$ENDIF}
      if v then
      begin
        if ssShift in Shift then
        begin
          t.Selected := not t.Selected;
          if t.Selected then
            Inc(SelNum)
          else
            Dec(SelNum);
        end
        else
        begin
          if not t.Selected then
          begin
            FDesigner.Unselect;
            SelNum := 1;
            t.Selected := True;
          end
          else DontChange := True;
        end;

        if SelNum = 0 then
          FirstSelected := nil
        else if SelNum = 1 then
          FirstSelected := t
        else if FirstSelected <> nil then
          if not FirstSelected.Selected then
            FirstSelected := nil;
        f := True;
        break;
      end;
    end;
    
    if not f then
    begin
      FDesigner.Unselect;
      SelNum := 0;
      FirstSelected := nil;
      if Button = mbLeft then
      begin
        RFlag := True;
        OldRect := Rect(x, y, x, y);
        Exit;
      end;
    end;
    
    GetMultipleSelected;
    if not DontChange then
       FDesigner.SelectionChanged;
  end;
  
  if SelNum = 0 then
  begin // reset multiple selection
    RightBottom := -1;
    MRFlag := False;
  end;
  
  LastX := x;
  LastY := y;
  Moved := False;
  FirstChange := True;
  FirstBandMove := True;
  
  if Button = mbRight then
  begin
    DrawPage(dmSelection);
    Down := False;
    GetCursorPos(p);
    //FDesigner.Popup1Popup(nil);
    
    FDesigner.Popup1.PopUp(p.X,p.Y);
    //**
    {TrackPopupMenu(FDesigner.Popup1.Handle,
      TPM_LEFTALIGN or TPM_RIGHTBUTTON, p.X, p.Y, 0, FDesigner.Handle, nil);
    }
  end
  else if FDesigner.ShapeMode = smFrame then
           DrawPage(dmShape);
           
  {$IFDEF DebugLR}
  DebugLn('TfrDesignerPage.MDown END');
  {$ENDIF}
end;

procedure TfrDesignerPage.MUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i, k, dx, dy: Integer;
  t: TfrView;
  ObjectInserted: Boolean;
  
  procedure AddObject(ot: Byte);
  begin
    Objects.Add(frCreateObject(ot, ''));
    t := TfrView(Objects.Last);
    if t is TfrMemoView then
      TfrMemoView(t).MonitorFontChanges;
  end;
  
  procedure CreateSection;
  var
    s: String;
  begin
    frBandTypesForm := TfrBandTypesForm.Create(FDesigner);
    try
      ObjectInserted := frBandTypesForm.ShowModal = mrOk;
      if ObjectInserted then
      begin
        Objects.Add(TfrBandView.Create);
        t := TfrView(Objects.Last);
        (t as TfrBandView).BandType := frBandTypesForm.SelectedTyp;
        s := frGetBandName(frBandTypesForm.SelectedTyp);
        THackView(t).BaseName := s;
        SendBandsToDown;
      end;
    finally
      frBandTypesForm.Free;
    end;
  end;
  
  procedure CreateSubReport;
  begin
    Objects.Add(TfrSubReportView.Create);
    t := TfrView(Objects.Last);
    (t as TfrSubReportView).SubPage := CurReport.Pages.Count;
    CurReport.Pages.Add;
  end;

begin
  if Button <> mbLeft then
  begin
    {$IFDEF DebugLR}
    DebugLn('MUp "Button <> mbLeft" cursor=',IntToStr(Cursor),' RFlag=',BoolToStr(RFlag));
    {$ENDIF}
    Exit;
  {$IFDEF DebugLR}
  end
  else DebugLn('MUp "Button = mbLeft" cursor=',IntToStr(Cursor),' RFlag=',BoolToStr(RFlag));
  {$ELSE}
  end;
  {$ENDIF}

  {$IFDEF LCLCarbon}Invalidate;{$endif}
  Down := False;
  if FDesigner.ShapeMode = smFrame then
    DrawPage(dmShape);

  //inserting a new object
  if Cursor = crCross then
  begin
    {$IFDEF DebugLR}
    DebugLn('inserting a new object');
    {$ENDIF}
    Mode := mdSelect;
    DrawFocusRect(OldRect);
    if (OldRect.Left = OldRect.Right) and (OldRect.Top = OldRect.Bottom) then
      OldRect := OldRect1;
    NormalizeRect(OldRect);
    RFlag := False;
    ObjectInserted := True;
    
    with FDesigner.Panel4 do
    begin
      for i := 0 to ControlCount - 1 do
      begin
        if Controls[i] is TSpeedButton then
        begin
          with Controls[i] as TSpeedButton do
          begin
            if Down then
            begin
              if Tag = gtBand then
              begin
                if GetUnusedBand <> btNone then
                  CreateSection
                else
                  Exit;
              end
              else if Tag = gtSubReport then
                       CreateSubReport
              else
              begin
                if Tag >= gtAddIn then
                begin
                  k := Tag - gtAddIn;
                  Objects.Add(frCreateObject(gtAddIn, frAddIns[k].ClassRef.ClassName));
                  t := TfrView(Objects.Last);
                end
                else
                  AddObject(Tag);
              end;
              break;
            end;
          end;
        end;
      end;
    end;
    
    if ObjectInserted then
    begin
      {$IFDEF DebugLR}
      debugLn('Object inserted begin');
      {$ENDIF}
      t.CreateUniqueName;
      t.Canvas:=Canvas;
      
      with OldRect do
      begin
        if (Left = Right) or (Top = Bottom) then
        begin
          dx := 40;
          dy := 40;
          if t is TfrMemoView then
            FDesigner.GetDefaultSize(dx, dy);
          OldRect := Rect(Left, Top, Left + dx, Top + dy);
        end;
      end;

      FDesigner.Unselect;
      t.x := OldRect.Left;
      t.y := OldRect.Top;
      t.dx := OldRect.Right - OldRect.Left;
      t.dy := OldRect.Bottom - OldRect.Top;
      
      if (t is TfrBandView) and
         (TfrBandView(t).BandType in [btCrossHeader..btCrossFooter]) and
         (t.dx > Width - 10) then
            t.dx := 40;
      t.FrameWidth := LastFrameWidth;
      t.FrameColor := LastFrameColor;
      t.FillColor  := LastFillColor;
      t.Selected   := True;
      
      if t.Typ <> gtBand then
        t.Frames:=LastFrames;
        
      if t is TfrMemoView then
      begin
        with t as TfrMemoView do
        begin
          Font.Name := LastFontName;
          Font.Size := LastFontSize;
          Font.Color := LastFontColor;
          Font.Style := frSetFontStyle(LastFontStyle);
          Adjust := LastAdjust;
        end;
      end;
      
      SelNum := 1;
      if t.Typ = gtBand then begin
        {$IFDEF DebugLR}
        DebugLn('A new band was inserted');
        {$ENDIF}
        Draw(10000, t.GetClipRgn(rtExtended))
      end else
      begin
        t.Draw(Canvas);
        DrawSelection(t);
      end;
      
      with FDesigner do
      begin
        SelectionChanged;
        AddUndoAction(acInsert);
        if EditAfterInsert then
          ShowEditor;
      end;
    end;
    if not ObjRepeat then
      FDesigner.OB1.Down := True
    else
      DrawFocusRect(OldRect);

    {$IFDEF DebugLR}
    DebugLn('Object inserted end');
    {$ENDIF}
    Exit;
  end;
  
  //line drawing
  if Cursor = crPencil then
  begin
    DrawRectLine(OldRect);
    AddObject(gtLine);
    t.CreateUniqueName;
    t.x := OldRect.Left; t.y := OldRect.Top;
    t.dx := OldRect.Right - OldRect.Left; t.dy := OldRect.Bottom - OldRect.Top;
    if t.dx < 0 then
    begin
      t.dx := -t.dx; if Abs(t.dx) > Abs(t.dy) then t.x := OldRect.Right;
    end;
    if t.dy < 0 then
    begin
      t.dy := -t.dy; if Abs(t.dy) > Abs(t.dx) then t.y := OldRect.Bottom;
    end;
    t.Selected := True;
    t.FrameWidth := LastLineWidth;
    t.FrameColor := LastFrameColor;
    SelNum := 1;
    t.Draw(Canvas);
    DrawSelection(t);
    FDesigner.SelectionChanged;
    FDesigner.AddUndoAction(acInsert);
    Exit;
  end;

  // calculating which objects contains in frame (if user select it with mouse+Ctrl key)
  if RFlag then
  begin
    DrawFocusRect(OldRect);
    RFlag := False;
    NormalizeRect(OldRect);
    for i := 0 to Objects.Count - 1 do
    begin
      t := TfrView(Objects[i]);
      with OldRect do
      begin
        if t.Typ <> gtBand then
        begin
          if not ((t.x > Right) or (t.x + t.dx < Left) or
                  (t.y > Bottom) or (t.y + t.dy < Top)) then
          begin
            t.Selected := True;
            Inc(SelNum);
          end;
        end;
      end;
    end;
    GetMultipleSelected;
    FDesigner.SelectionChanged;
    DrawPage(dmSelection);
    {$IFDEF DebugLR}
    DebugLn('calculating which objects contains in frame (if user select it with mouse+Ctrl key)');
    {$ENDIF}
    Exit;
  end;
  
  //splitting
  if Moved and MRFlag and (Cursor = crHSplit) then
  begin
    with SplitInfo do
    begin
      dx := SplRect.Left - SplX;
      if (View1.dx + dx > 0) and (View2.dx - dx > 0) then
      begin
        Inc(View1.dx, dx);
        Inc(View2.x, dx);
        Dec(View2.dx, dx);
      end;
    end;
    GetMultipleSelected;
    Draw(TopSelected, ClipRgn);
    {$IFDEF DebugLR}
    DebugLn('splitting');
    {$ENDIF}
    Exit;
  end;

  //resizing several objects
  if Moved and MRFlag and (Cursor <> crDefault) then
  begin
    Draw(TopSelected, ClipRgn);
    {$IFDEF DebugLR}
    DebugLn('resizing several objects');
    {$ENDIF}
    Exit;
  end;
  
  //redrawing all moved or resized objects
  if not Moved then
  begin
    DrawPage(dmSelection);
    {$IFDEF DebugLR}
    DebugLn('redrawing all moved or resized objects');
    {$ENDIF}
  end;

  if (SelNum >= 1) and Moved then
  begin
    if SelNum > 1 then
    begin
      //JRA DebugLn('HERE, ClipRgn', Dbgs(ClipRgn));
      Draw(TopSelected, ClipRgn);
      GetMultipleSelected;
      FDesigner.ShowPosition;
    end
    else
    begin
      t := TfrView(Objects[TopSelected]);
      NormalizeCoord(t);
      if Cursor <> crDefault then
        t.Resized;
      Draw(TopSelected, ClipRgn);
      FDesigner.ShowPosition;
    end;
  end;

  Moved := False;
  CT := ctNone;
end;

procedure TfrDesignerPage.MMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i, j, kx, ky, w, dx, dy: Integer;
  t, t1, Bnd: TfrView;
  nx, ny, x1, x2, y1, y2: Double;
  hr, hr1,Hr2: HRGN;

  function Cont(px, py, x, y: Integer): Boolean;
  begin
    Result := (x >= px - w) and (x <= px + w + 1) and
      (y >= py - w) and (y <= py + w + 1);
  end;
  
  function GridCheck:Boolean;
  begin
    with FDesigner do
    begin
      Result := (kx >= GridSize) or (kx <= -GridSize) or
                (ky >= GridSize) or (ky <= -GridSize);
      if Result then
      begin
        kx := kx - kx mod GridSize;
        ky := ky - ky mod GridSize;
      end;
    end;
  end;
begin
  {$IFDEF DebugLR}
  DebugLn('TfrDesignerPage.MMove(X=',dbgs(x),',Y=',dbgs(y),'  INIT');
  {$ENDIF}
  Moved := True;
  w := 2;

  {$ifdef LCLCarbon}if Down then Invalidate;{$endif}

  if FirstChange and Down and not RFlag then
  begin
    kx := x - LastX;
    ky := y - LastY;
    if not FDesigner.GridAlign or GridCheck then
    begin
      GetRegion; //JRA 1
      FDesigner.AddUndoAction(acEdit);
    end;
  end;

  if not Down then
  begin
    if FDesigner.OB6.Down then
    begin
      Mode := mdSelect;
      Cursor := crPencil;
    end
    else if FDesigner.OB1.Down then
    begin
      Mode := mdSelect;
      Cursor := crDefault;
    end
    else
    begin
      Mode := mdInsert;
      if Cursor <> crCross then
      begin
        RoundCoord(x, y);
        kx := Width; ky := 40;
        if not FDesigner.OB3.Down then
          FDesigner.GetDefaultSize(kx, ky);
        OldRect := Rect(x, y, x + kx, y + ky);
        DrawFocusRect(OldRect);
      end;
      Cursor := crCross;
    end;
  end;

  {$IFDEF DebugLR}
  if mode=mdInsert then
    DebugLn('  Mode=mdInsert Down=', dbgs(down))
  else
    DebugLn('  Mode=mdSelect Down=', dbgs(down));
  {$ENDIF}

  if (Mode = mdInsert) and not Down then
  begin
    DrawFocusRect(OldRect);
    RoundCoord(x, y);
    OffsetRect(OldRect, x - OldRect.Left, y - OldRect.Top);
    DrawFocusRect(OldRect);
    ShowSizes := True;
    FDesigner.UpdateStatus;
    ShowSizes := False;
    Exit;
  end;

  //cursor shapes
  if not Down and (SelNum = 1) and (Mode = mdSelect) and
    not FDesigner.OB6.Down then
  begin
    t := TfrView(Objects[TopSelected]);
    if Cont(t.x, t.y, x, y) or Cont(t.x + t.dx, t.y + t.dy, x, y) then
      Cursor := crSizeNWSE
    else if Cont(t.x + t.dx, t.y, x, y) or Cont(t.x, t.y + t.dy, x, y)then
      Cursor := crSizeNESW
    else if Cont(t.x + t.dx div 2, t.y, x, y) or Cont(t.x + t.dx div 2, t.y + t.dy, x, y) then
      Cursor := crSizeNS
    else if Cont(t.x, t.y + t.dy div 2, x, y) or Cont(t.x + t.dx, t.y + t.dy div 2, x, y) then
      Cursor := crSizeWE
    else
      Cursor := crDefault;
  end;
  
  //selecting a lot of objects
  if Down and RFlag then
  begin
    DrawFocusRect(OldRect);
    if Cursor = crCross then
      RoundCoord(x, y);
    OldRect := Rect(OldRect.Left, OldRect.Top, x, y);
    DrawFocusRect(OldRect);
    ShowSizes := True;
    if Cursor = crCross then
      FDesigner.UpdateStatus;
    ShowSizes := False;
    Exit;
  end;
  
  //line drawing
  if Down and (Cursor = crPencil) then
  begin
    kx := x - LastX;
    ky := y - LastY;
    if FDesigner.GridAlign and not GridCheck then Exit;
    DrawRectLine(OldRect);
    OldRect := Rect(OldRect.Left, OldRect.Top, OldRect.Right + kx, OldRect.Bottom + ky);
    DrawRectLine(OldRect);
    Inc(LastX, kx);
    Inc(LastY, ky);
    Exit;
  end;

  //check for multiple selected objects - right-bottom corner
  if not Down and (SelNum > 1) and (Mode = mdSelect) then
  begin
    t := TfrView(Objects[RightBottom]);
    if Cont(t.x + t.dx, t.y + t.dy, x, y) then
      Cursor := crSizeNWSE
  end;
  
  //split checking
  if not Down and (SelNum > 1) and (Mode = mdSelect) then
  begin
    for i := 0 to Objects.Count-1 do
    begin
      t := TfrView(Objects[i]);
      if (t.Typ <> gtBand) and t.Selected then
        if (x >= t.x) and (x <= t.x + t.dx) and (y >= t.y) and (y <= t.y + t.dy) then
        begin
          for j := 0 to Objects.Count - 1 do
          begin
            t1 := TfrView(Objects[j]);
            if (t1.Typ <> gtBand) and (t1 <> t) and t1.Selected then
              if ((t.x = t1.x + t1.dx) and ((x >= t.x) and (x <= t.x + 2))) or
              ((t1.x = t.x + t.dx) and ((x >= t1.x - 2) and (x <= t.x))) then
              begin
                Cursor := crHSplit;
                with SplitInfo do
                begin
                  SplRect := Rect(x, t.y, x, t.y + t.dy);
                  if t.x = t1.x + t1.dx then
                  begin
                    SplX := t.x;
                    View1 := t1;
                    View2 := t;
                  end
                  else
                  begin
                    SplX := t1.x;
                    View1 := t;
                    View2 := t1;
                  end;
                  SplRect.Left := SplX;
                  SplRect.Right := SplX;
                end;
              end;
          end;
        end;
    end;
  end;
  
  // splitting
  if Down and MRFlag and (Mode = mdSelect) and (Cursor = crHSplit) then
  begin
    kx := x - LastX;
    ky := 0;
    if FDesigner.GridAlign and not GridCheck then Exit;
    with SplitInfo do
    begin
      DrawHSplitter(SplRect);
      SplRect := Rect(SplRect.Left + kx, SplRect.Top, SplRect.Right + kx, SplRect.Bottom);
      DrawHSplitter(SplRect);
    end;
    Inc(LastX, kx);
    Exit;
  end;
  
  // sizing several objects
  if Down and MRFlag and (Mode = mdSelect) and (Cursor <> crDefault) then
  begin
    kx := x - LastX;
    ky := y - LastY;
    if FDesigner.GridAlign and not GridCheck then Exit;

    if FDesigner.ShapeMode = smFrame then
      DrawPage(dmShape)
    else
    begin
      hr := CreateRectRgn(0, 0, 0, 0);
      hr1 := CreateRectRgn(0, 0, 0, 0);
    end;
    
    OldRect := Rect(OldRect.Left, OldRect.Top, OldRect.Right + kx, OldRect.Bottom + ky);
    nx := (OldRect.Right - OldRect.Left) / (OldRect1.Right - OldRect1.Left);
    ny := (OldRect.Bottom - OldRect.Top) / (OldRect1.Bottom - OldRect1.Top);
    for i := 0 to Objects.Count - 1 do
    begin
      t := TfrView(Objects[i]);
      if t.Selected then
      begin
        if FDesigner.ShapeMode = smAll then
          AddRgn(hr, t);
        x1 := (t.OriginalRect.Left - LeftTop.x) * nx;
        x2 := t.OriginalRect.Right * nx;
        dx := Round(x1 + x2) - (Round(x1) + Round(x2));
        t.x := LeftTop.x + Round(x1); t.dx := Round(x2) + dx;

        y1 := (t.OriginalRect.Top - LeftTop.y) * ny;
        y2 := t.OriginalRect.Bottom * ny;
        dy := Round(y1 + y2) - (Round(y1) + Round(y2));
        t.y := LeftTop.y + Round(y1); t.dy := Round(y2) + dy;
        if FDesigner.ShapeMode = smAll then
          AddRgn(hr1, t);
      end;
    end;

    if FDesigner.ShapeMode = smFrame then
      DrawPage(dmShape)
    else
    begin
      Draw(10000, hr);
      Draw(10000, hr1);
    end;
    
    Inc(LastX, kx);
    Inc(LastY, ky);
    FDesigner.UpdateStatus;
    Exit;
  end;
  
  //moving
  if Down and (Mode = mdSelect) and (SelNum >= 1) and (Cursor = crDefault) then
  begin
    kx := x - LastX;
    ky := y - LastY;
    if FDesigner.GridAlign and not GridCheck then Exit;
    if FirstBandMove and (SelNum = 1) and ((kx <> 0) or (ky <> 0)) and
      not (ssAlt in Shift) then
    begin
      if Assigned(Objects[TopSelected]) and (TFrView(Objects[TopSelected]).Typ = gtBand) then
      begin
        Bnd := TfrView(Objects[TopSelected]);
        for i := 0 to Objects.Count-1 do
        begin
          t := TfrView(Objects[i]);
          if t.Typ <> gtBand then
          begin
          
            if (t.x >= Bnd.x) and (t.x + t.dx <= Bnd.x + Bnd.dx) and
               (t.y >= Bnd.y) and (t.y + t.dy <= Bnd.y + Bnd.dy) then
            begin
              t.Selected := True;
              Inc(SelNum);
            end;
          end;
        end;
        ColorLocked := True;
        FDesigner.SelectionChanged;
        GetMultipleSelected;
        ColorLocked := False;
      end;
    end;
    
    FirstBandMove := False;

    MoveResize(kx,ky,FDesigner.ShapeMode=smFrame, false);

    Inc(LastX, kx);
    Inc(LastY, ky);
    FDesigner.UpdateStatus;
  end;
{$IFDEF DebugLR}
//  else debugLn('Down=',BoolToStr(Down),' Mode=',IntToStr(Ord(Mode)),' SelNum=',IntToStr(Selnum),' Cursor=',IntToStr(Cursor));
{$ENDIF}

  //resizing
  if Down and (Mode = mdSelect) and (SelNum = 1) and (Cursor <> crDefault) then
  begin
    kx := x - LastX;
    ky := y - LastY;
    if FDesigner.GridAlign and not GridCheck then Exit;
    
    t := TfrView(Objects[TopSelected]);
    if FDesigner.ShapeMode = smFrame then
      DrawPage(dmShape)
    else
      hr:=t.GetClipRgn(rtExtended);
    w := 3;

    if Cursor = crSizeNWSE then
    begin
      if (CT <> ct2) and ((CT = ct1) or Cont(t.x, t.y, LastX, LastY)) then
      begin
        t.x := t.x + kx;
        t.dx := t.dx - kx;
        t.y := t.y + ky;
        t.dy := t.dy - ky;
        CT := ct1;
      end
      else
      begin
        t.dx := t.dx + kx;
        t.dy := t.dy + ky;
        CT := ct2;
      end;
    end;
    
    if Cursor = crSizeNESW then
    begin
      if (CT <> ct4) and ((CT = ct3) or Cont(t.x + t.dx, t.y, LastX, LastY)) then
      begin
        t.y := t.y + ky;
        t.dx := t.dx + kx;
        t.dy := t.dy - ky;
        CT := ct3;
      end
      else
      begin
        t.x := t.x + kx;
        t.dx := t.dx - kx;
        t.dy := t.dy + ky;
        CT := ct4;
      end;
    end;
    
    if Cursor = crSizeWE then
    begin
      if (CT <> ct6) and ((CT = ct5) or Cont(t.x, t.y + t.dy div 2, LastX, LastY)) then
      begin
        t.x := t.x + kx;
        t.dx := t.dx - kx;
        CT := ct5;
      end
      else
      begin
        t.dx := t.dx + kx;
        CT := ct6;
      end;
    end;
    
    if Cursor = crSizeNS then
    begin
      if (CT <> ct8) and ((CT = ct7) or Cont(t.x + t.dx div 2, t.y, LastX, LastY)) then
      begin
        t.y := t.y + ky;
        t.dy := t.dy - ky;
        CT := ct7;
      end
      else
      begin
        t.dy := t.dy + ky;
        CT := ct8;
      end;
    end;
    
    if FDesigner.ShapeMode = smFrame then
    begin
      DrawPage(dmShape);
      {$IFDEF DebugLR}
      DebugLn('MDown resizing 1');
      {$ENDIF}
    end
    else
    begin
      Hr1:=CreateRectRgn(0,0,0,0);
      Hr2:=t.GetClipRgn(rtExtended);
      CombineRgn(hr1, hr, hr2, RGN_OR);
      DeleteObject(Hr2);
      Draw(10000, hr1);
      DeleteObject(Hr);
      {$IFDEF DebugLR}
      DebugLn('MDown resizing 2');
      {$ENDIF}
    end;

    Inc(LastX, kx);
    Inc(LastY, ky);
  end;
  {$IFDEF DebugLR}
  DebugLn('TfrDesignerPage.MMove END');
  {$ENDIF}
end;

procedure TfrDesignerPage.DClick(Sender: TObject);
begin
  Down := False;
  if SelNum = 0 then
  begin
    FDesigner.PgB3Click(nil);
    DFlag := True;
  end
  else if SelNum = 1 then
  begin
    DFlag := True;
    FDesigner.ShowEditor;
  end
  else Exit;
end;

procedure TfrDesignerPage.MoveResize(Kx, Ky: Integer; UseFrames,AResize: boolean);
var
  hr,hr1: HRGN;
  i: Integer;
  t: TFrView;
begin
  If UseFrames then
    DrawPage(dmShape)
  else
  begin
    hr := CreateRectRgn(0, 0, 0, 0);
    hr1 := CreateRectRgn(0, 0, 0, 0);
  end;

  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if not t.Selected then continue;
    if FDesigner.ShapeMode = smAll then
      AddRgn(hr, t);
    if aResize then
    begin
      t.dx := t.dx + kx;
      t.dy := t.dy + ky;
    end
    else
    begin
      t.x := t.x + kx;
      t.y := t.y + ky;
    end;
    if FDesigner.ShapeMode = smAll then
      AddRgn(hr1, t);
  end;

  if UseFrames then
    DrawPage(dmShape)
  else
  begin
    CombineRgn(hr, hr, hr1, RGN_OR);
    DeleteObject(hr1);
    Draw(10000, hr);
  end;
end;

procedure TfrDesignerPage.CMMouseLeave(var Message: TLMessage);
begin
  if (Mode = mdInsert) and not Down then
  begin
    DrawFocusRect(OldRect);
    OffsetRect(OldRect, -10000, -10000);
  end;
end;

{-----------------------------------------------------------------------------}
procedure BDown(SB: TSpeedButton);
begin
  SB.Down := True;
end;

procedure BUp(SB: TSpeedButton);
begin
  SB.Down := False;
end;
{
function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TfrDesignerForm(frDesigner).C2.Items.AddObject(StrPas(LogFont.lfFaceName), TObject(FontType));
  Result := 1;
end;
}

function EnumFontsProc(
  var LogFont: TEnumLogFontEx;
  var Metric: TNewTextMetricEx;
  FontType: Longint;
  Data: LParam):LongInt; stdcall;
var
  S: String;
begin
  s := StrPas(LogFont.elfLogFont.lfFaceName);
  if TfrDesignerForm(frDesigner).C2.Items.IndexOf(S)<0 then
    TfrDesignerForm(frDesigner).C2.Items.AddObject(S, TObject(PtrInt(FontType)));
  Result := 1;
end;

constructor TfrDesignerForm.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fInBuildPage:=False;
  {$IFDEF STDOI}
  // create the ObjectInspector
  PropHook:= TPropertyEditorHook.Create;
  ObjInsp := TObjectInspector.Create(Self);
  ObjInsp.SetInitialBounds(10,10,220,400);
  ObjInsp.ShowComponentTree := False;
  ObjInsp.ShowFavouritePage := False;
  ObjInsp.PropertyEditorHook := PropHook;
  {$ELSE}
  ObjInsp := TFrObjectInspector.Create(Self);
  ObjInsp.SetModifiedEvent(@OnModify);
  {$ENDIF}
  {$ifdef sbod}
  StatusBar1.Panels[1].Style := psOwnerDraw;
  StatusBar1.OnDrawPanel := @StatusBar1Drawpanel;
  Panel7.Visible := false;
  {$endif}
end;

destructor TfrDesignerForm.Destroy;
begin
  {$IFDEF EXTOI}
  ObjInsp.Free;
  {$ENDIF}
  {$IFDEF STDOI}
  PropHook.Free;
  {$ENDIF}
  inherited Destroy;
end;

procedure TfrDesignerForm.GetFontList;
var
  DC: HDC;
  Lf: TLogFont;
begin
  C2.Items.Clear;
  DC := GetDC(0);
  try
    Lf.lfFaceName := '';
    Lf.lfCharSet := DEFAULT_CHARSET;
    Lf.lfPitchAndFamily := 0;
    EnumFontFamiliesEx(DC, @Lf, @EnumFontsProc, 0, 0);
  finally
    ReleaseDC(0, DC);
  end;
  if C2.Items.Count>0 then
    LastFontName := C2.Items[0]
  else
    LastFontName := '';
    
  if C2.Items.IndexOf('Arial') <> -1 then
    LastFontName := 'Arial'
  else if C2.Items.IndexOf('helvetica [urw]')<>-1 then
    LastFontName := 'helvetica [urw]'
  else if C2.Items.IndexOf('Arial Cyr') <> -1 then
    LastFontName := 'Arial Cyr';
  LastFontSize := 10;
end;

procedure TfrDesignerForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Busy := True;
  FirstTime := True;
  FirstInstance := FirstInst;

  PageView := TfrDesignerPage.Create(Self{ScrollBox1});
  PageView.Parent := ScrollBox1;
  PageView.FDesigner := Self;
  PageView.PopupMenu := Popup1;
  PageView.ShowHint := True;

  ColorSelector := TColorSelector.Create(Self);
  ColorSelector.OnColorSelected := @ColorSelected;
  ColorSelector.Hide;

  for i := 0 to frAddInsCount - 1 do
  with frAddIns[i] do
  begin
    RegisterObject(ButtonBMP, ButtonHint, Integer(gtAddIn) + i);
  end;

  if FirstInstance then
  begin
    frRegisterTool(sInsertFields, Image2.Picture.Bitmap, @InsFieldsClick);
    for i := 0 to frToolsCount - 1 do
    with frTools[i] do
    begin
      RegisterTool(Caption, ButtonBMP, OnClick);
    end;
  end;

  EditorForm := TfrEditorForm.Create(nil);

  MenuItems := TFpList.Create;
  ItemWidths := TStringlist.Create;
  if FirstInstance then
  begin
    //** Application.OnActivate := OnActivateApp;
    //** Application.OnDeactivate := OnDeactivateApp;
  end
  else
  begin
    PgB1.Enabled := False;
    PgB2.Enabled := False;
    N41.Enabled := False;
    N43.Enabled := False;
    N29.Enabled := False;
    N30.Enabled := False;
  end;
  FirstInst := False;

  FCaption :=         sFRDesignerFormCapt;
  //Panel1.Caption :=   sFRDesignerFormrect;
  //Panel2.Caption :=   sFRDesignerFormStd;
  //Panel3.Caption :=   sFRDesignerFormText;
  //Panel5.Caption :=   sFRDesignerFormAlign;
  //Panel6.Caption :=   sFRDesignerFormTools;
  FileBtn1.Hint :=    sFRDesignerFormNewRp;
  FileBtn2.Hint :=    sFRDesignerFormOpenRp;
  FileBtn3.Hint :=    sFRDesignerFormSaveRp;
  FileBtn4.Hint :=    sFRDesignerFormPreview;
  CutB.Hint :=        sFRDesignerFormCut;
  CopyB.Hint :=       sFRDesignerFormCopy;
  PstB.Hint :=        sFRDesignerFormPast;
  UndoB.Hint :=       sFRDesignerFormUndo;
  RedoB.Hint :=       sFRDesignerFormRedo;
  ZB1.Hint :=         sFRDesignerFormBring;
  ZB2.Hint :=         sFRDesignerFormBack;
  SelAllB.Hint :=     sFRDesignerFormSelectAll;
  PgB1.Hint :=        sFRDesignerFormAddPg;
  PgB2.Hint :=        sFRDesignerFormRemovePg;
  PgB3.Hint :=        sFRDesignerFormPgOption;
  GB1.Hint :=         sFRDesignerFormGrid;
  GB2.Hint :=         sFRDesignerFormGridAlign;
  GB3.Hint :=         sFRDesignerFormFitGrid;
  HelpBtn.Hint :=     sPreviewFormHelp;
  ExitB.Caption :=    sFRDesignerFormClose;
  ExitB.Hint :=       sFRDesignerFormCloseDesigner;
  AlB1.Hint :=        sFRDesignerFormLeftAlign;
  AlB2.Hint :=        sFRDesignerFormRightAlign;
  AlB3.Hint :=        sFRDesignerFormCenerAlign;
  AlB4.Hint :=        sFRDesignerFormNormalText;
  AlB5.Hint :=        sFRDesignerFormVertCenter;
  AlB6.Hint :=        sFRDesignerFormTopAlign;
  AlB7.Hint :=        sFRDesignerFormBottomAlign;
  AlB8.Hint :=        sFRDesignerFormWidthAlign;
  FnB1.Hint :=        sFRDesignerFormBold;
  FnB2.Hint :=        sFRDesignerFormItalic;
  FnB3.Hint :=        sFRDesignerFormUnderLine;
  ClB2.Hint :=        sFRDesignerFormFont;
  HlB1.Hint :=        sFRDesignerFormHightLight;
  C3.Hint :=          sFRDesignerFormFontSize;
  C2.Hint :=          sFRDesignerFormFontName;
  FrB1.Hint :=        sFRDesignerFormTopFrame;
  FrB2.Hint :=        sFRDesignerFormleftFrame;
  FrB3.Hint :=        sFRDesignerFormBottomFrame;
  FrB4.Hint :=        sFRDesignerFormRightFrame;
  FrB5.Hint :=        sFRDesignerFormAllFrame;
  FrB6.Hint :=        sFRDesignerFormNoFrame;
  ClB1.Hint :=        sFRDesignerFormBackColor;
  ClB3.Hint :=        sFRDesignerFormFrameColor;
  E1.Hint :=          sFRDesignerFormFrameWidth;
  OB1.Hint :=         sFRDesignerFormSelObj;
  OB2.Hint :=         sFRDesignerFormInsRect;
  OB3.Hint :=         sFRDesignerFormInsBand;
  OB4.Hint :=         sFRDesignerFormInsPict;
  OB5.Hint :=         sFRDesignerFormInsSub;
  OB6.Hint :=         sFRDesignerFormDrawLine;
  Align1.Hint :=      sFRDesignerFormAlignLeftedge;
  Align2.Hint :=      sFRDesignerFormAlignHorzCenter;
  Align3.Hint :=      sFRDesignerFormCenterHWind;
  Align4.Hint :=      sFRDesignerFormSpace;
  Align5.Hint :=      sFRDesignerFormAlignRightEdge;
  Align6.Hint :=      sFRDesignerFormAligneTop;
  Align7.Hint :=      sFRDesignerFormAlignVertCenter;
  Align8.Hint :=      sFRDesignerFormCenterVertWing;
  Align9.Hint :=      sFRDesignerFormSpaceEqVert;
  Align10.Hint :=     sFRDesignerFormAlignBottoms;
  N2.Caption :=       sFRDesignerForm_Cut;
  N1.Caption :=       sFRDesignerForm_Copy;
  N3.Caption :=       sFRDesignerForm_Paste;
  N5.Caption :=       sFRDesignerForm_Delete;
  N16.Caption :=      sFRDesignerForm_SelectAll;
  N6.Caption :=       sFRDesignerForm_Edit;
  FileMenu.Caption := sFRDesignerForm_File;
  N23.Caption :=      sFRDesignerForm_New;
  N19.Caption :=      sFRDesignerForm_Open;
  N20.Caption :=      sFRDesignerForm_Save;
  N42.Caption :=      sFRDesignerForm_Var;
  N8.Caption :=       sFRDesignerForm_RptOpt;
  N25.Caption :=      sFRDesignerForm_PgOpt;
  N39.Caption :=      sFRDesignerForm_preview;
  N10.Caption :=      sFRDesignerForm_Exit;
  EditMenu.Caption := sFRDesignerForm_Edit2;
  N46.Caption :=      sFRDesignerForm_Undo;
  N48.Caption :=      sFRDesignerForm_Redo;
  N11.Caption :=      sFRDesignerForm_Cut;
  N12.Caption :=      sFRDesignerForm_Copy;
  N13.Caption :=      sFRDesignerForm_Paste;
  N27.Caption :=      sFRDesignerForm_Delete;
  N28.Caption :=      sFRDesignerForm_SelectAll;
  N36.Caption :=      sFRDesignerForm_Editp;
  N29.Caption :=      sFRDesignerForm_AddPg;
  N30.Caption :=      sFRDesignerForm_RemovePg;
  N32.Caption :=      sFRDesignerForm_Bring;
  N33.Caption :=      sFRDesignerForm_Back;
  ToolMenu.Caption := sFRDesignerForm_Tools;
  N37.Caption :=      sFRDesignerForm_ToolBars;
  MastMenu.Caption := sFRDesignerForm_Tools2;
  N14.Caption :=      sFRDesignerForm_Opts;
  Pan1.Caption :=     sFRDesignerForm_Rect;
  Pan2.Caption :=     sFRDesignerForm_Std;
  Pan3.Caption :=     sFRDesignerForm_Text;
  Pan4.Caption :=     sFRDesignerForm_Obj;
  Pan5.Caption :=     sFRDesignerForm_Insp;
  Pan6.Caption :=     sFRDesignerForm_AlignPalette;
  Pan7.Caption :=     sFRDesignerForm_Tools3;
  N34.Caption :=      sFRDesignerForm_About;
  N17.Caption :=      sFRDesignerForm_SaveAs;
  N22.Caption :=      sFRDesignerForm_Help1;
  N35.Caption :=      sFRDesignerForm_Help2;
  StB1.Hint   :=      sFRDesignerForm_Line;
  //** FnB1.Glyph.Handle := LoadBitmap(hInstance, 'FR_BOLD');
  //** FnB2.Glyph.Handle := LoadBitmap(hInstance, 'FR_ITALIC');
  //** FnB3.Glyph.Handle := LoadBitmap(hInstance, 'FR_UNDRLINE');

  N41.Caption :=      N29.Caption;
  N41.OnClick :=      N29.OnClick;
  N43.Caption :=      N30.Caption;
  N43.OnClick :=      N30.OnClick;
  N44.Caption :=      N25.Caption;
  N44.OnClick :=      N25.OnClick;
end;

procedure TfrDesignerForm.C2GetItems(Sender: TObject);
var
  i: Integer;
begin
  if C2.Items.Count=0 then begin
    Screen.Cursor := crHourglass;
    GetFontList;
    i := C2.Items.IndexOf(LastFontName);
    if i<>-1 then
      C2.ItemIndex := i;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrDesignerForm.FormShow(Sender: TObject);
begin
  Screen.Cursors[crPencil] := LoadCursorFromLazarusREsource('FR_PENCIL');
  {$ifndef sbod}
  Panel7.Hide;
  {$endif}
  if FirstTime then
    SetMenuBitmaps;
  FirstTime := False;
  FileBtn1.Enabled := FirstInstance;
  FileBtn4.Enabled := FirstInstance and not (CurReport is TfrCompositeReport);
  N39.Enabled := FileBtn4.Enabled;
  N23.Enabled := FirstInstance;
  OB3.Enabled := FirstInstance;
  OB5.Enabled := FirstInstance;

  ClearUndoBuffer;
  ClearRedoBuffer;
  Modified := False;
  FileModified := False;
  Busy := True;
  DocMode := dmDesigning;
  
  //if C2.Items.Count=0 then
  //  GetFontList; // defered to speed loading
  LastFontSize := 10;
  {$IFDEF WIN32}
  LastFontName := 'Arial';
  {$ELSE}
  LastFontName := 'helvetica [urw]';
  {$ENDIF}

  //** C2.Perform(CB_SETDROPPEDWIDTH, 170, 0);
  CurPage := 0; // this cause page sizing
  CurDocName := CurReport.FileName;
  Unselect;

  PageView.Init;
  EnableControls;

  BDown(OB1);
  
  ColorLocked:=True;
  frSetGlyph(clNone, ClB1, 1);
  frSetGlyph(clNone, ClB2, 0);
  frSetGlyph(clNone, ClB3, 2);
  ColorLocked:=False;

  ColorSelector.Hide;

  LinePanel.Hide;

  ShowPosition;
  RestoreState;
  //FormResize(nil);
end;

procedure TfrDesignerForm.FormHide(Sender: TObject);
begin
  ClearUndoBuffer;
  ClearRedoBuffer;
  SaveState;
  if CurReport<>nil then
    CurReport.FileName := CurDocName;
end;

procedure TfrDesignerForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to MenuItems.Count - 1 do
    TfrMenuItemInfo(MenuItems[i]).Free;
  MenuItems.Free;
  ItemWidths.Free;
  PageView.Free;
  ColorSelector.Free;
  EditorForm.Free;
end;

procedure TfrDesignerForm.FormResize(Sender: TObject);
begin
  if csDestroying in ComponentState then Exit;

  //{$IFDEF WIN32}
  //if FirstTime then
  //  self.OnShow(self);
  //{$ENDIF}
    
  with ScrollBox1 do
  begin
    HorzScrollBar.Position := 0;
    VertScrollBar.Position := 0;
  end;
  if PageView<>nil then
    PageView.SetPage;
  StatusBar1.Top:=Height-StatusBar1.Height-3;
  {$ifndef sbod}
  Panel7.Top := StatusBar1.Top + 3;
  Panel7.Show;
  {$endif}
  UpdScrollbars;
end;

//**
{
procedure TfrDesignerForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin // for best view - not actual in Win98 :(
  with Msg.MinMaxInfo^ do
  begin
    ptMaxSize.x := Screen.Width;
    ptMaxSize.y := Screen.Height;
    ptMaxPosition.x := 0;
    ptMaxPosition.y := 0;
  end;
end;
}
procedure TfrDesignerForm.SetCurPage(Value: Integer);
begin // setting curpage and do all manipulation
  fInBuildPage:=True;
  try
    FCurPage := Value;
    Page := CurReport.Pages[CurPage];
    ScrollBox1.VertScrollBar.Position := 0;
    ScrollBox1.HorzScrollBar.Position := 0;
    PageView.SetPage;
    SetPageTitles;
    Tab1.TabIndex := Value;
    ResetSelection;
    SendBandsToDown;
    PageView.Invalidate;
  finally
    fInBuildPage:=False;
  end;
end;

procedure TfrDesignerForm.SetGridSize(Value: Integer);
begin
  if FGridSize = Value then Exit;
  FGridSize := Value;
  PageView.Invalidate;
end;

procedure TfrDesignerForm.SetGridShow(Value: Boolean);
begin
  if FGridShow = Value then Exit;
  FGridShow:= Value;
  GB1.Down := Value;
  PageView.Invalidate;
end;

procedure TfrDesignerForm.SetGridAlign(Value: Boolean);
begin
  if FGridAlign = Value then Exit;
  GB2.Down := Value;
  FGridAlign := Value;
end;

procedure TfrDesignerForm.SetUnits(Value: TfrReportUnits);
var
  s: String;
begin
  FUnits := Value;
  case Value of
    ruPixels: s := sPixels;
    ruMM:     s := sMM;
    ruInches: s := sInches;
  end;
  StatusBar1.Panels[0].Text := s;
  ShowPosition;
end;

procedure TfrDesignerForm.SetGrayedButtons(Value: Boolean);
  procedure DoButtons(t: Array of TControl);
  var
    i, j: Integer;
    c: TWinControl;
    c1: TControl;
  begin
    for i := Low(t) to High(t) do
    begin
      c := TWinControl(t[i]);
      for j := 0 to c.ControlCount - 1 do
      begin
        c1 := c.Controls[j];
        if c1 is TSpeedButton then
          TSpeedButton(c1).Enabled := FGrayedButtons; //** GrayedInactive := FGrayedButtons;
      end;
    end;
  end;
begin
  FGrayedButtons := Value;
  DoButtons([Panel1, Panel2, Panel3, Panel4, Panel5, Panel6]);
end;

procedure TfrDesignerForm.SetCurDocName(Value: String);
begin
  FCurDocName := Value;
  if FirstInstance then
    Caption := FCaption + ' - ' + ExtractFileName(Value)
  else
    Caption := FCaption;
end;

procedure TfrDesignerForm.RegisterObject(ButtonBmp: TBitmap;
  const ButtonHint: String; ButtonTag: Integer);
var
  b: TSpeedButton;
begin
  b := TSpeedButton.Create(Self);
  with b do
  begin
    Parent := Panel4;
    Glyph  := ButtonBmp;
    Hint   := ButtonHint;
    Flat   := True;
    GroupIndex := 1;
    Align:=alTop;
    SetBounds(1000, 1000, 22, 22);
    Visible:=True;
    Tag := ButtonTag;
    OnMouseDown := @OB2MouseDown;
  end;
end;

procedure TfrDesignerForm.RegisterTool(const MenuCaption: String; ButtonBmp: TBitmap;
  OnClickEvnt: TNotifyEvent);
var
  m: TMenuItem;
  b: TSpeedButton;
begin
  m := TMenuItem.Create(MastMenu);
  m.Caption := MenuCaption;
  m.OnClick := OnClickEvnt;
  MastMenu.Enabled := True;
  MastMenu.Add(m);
  Panel6.Height := 26;
  Panel6.Width := 26;
  b := TSpeedButton.Create(Self);
  with b do
  begin
    Parent := Panel6;
    Glyph := ButtonBmp;
    Hint := MenuCaption;
    Flat := True;
    Align:=alTop;
    SetBounds(1000, 1, 22, 22);
    Visible:=True;
    ShowHint:=True;
    Tag := 36;
  end;
  b.OnClick := OnClickEvnt;
  //** Panel6.AdjustBounds;
end;

procedure TfrDesignerForm.AddPage(ClName : string);
begin
  fInBuildPage:=True;
  try
    CurReport.Pages.Add(ClName);

    Page := CurReport.Pages[CurReport.Pages.Count - 1];
    if Page is TfrPageReport then
       PgB3Click(nil)
    else
       WasOk:=True;

    if WasOk then
    begin
      Modified := True;
      FileModified := True;
      CurPage := CurReport.Pages.Count - 1
    end
    else
    begin
      CurReport.Pages.Delete(CurReport.Pages.Count - 1);
      CurPage := CurPage;
    end;
  finally
    fInBuildPage:=False;
  end;
end;

procedure TfrDesignerForm.RemovePage(n: Integer);
  procedure AdjustSubReports;
  var
    i, j: Integer;
    t: TfrView;
  begin
    with CurReport do
      for i := 0 to Pages.Count - 1 do
      begin
        j := 0;
        while j < Pages[i].Objects.Count do
        begin
          t := TfrView(Pages[i].Objects[j]);
          if t.Typ = gtSubReport then
            if TfrSubReportView(t).SubPage = n then
            begin
              Pages[i].Delete(j);
              Dec(j);
            end
            else if TfrSubReportView(t).SubPage > n then
              Dec(TfrSubReportView(t).SubPage);
          Inc(j);
        end;
      end;
  end;
begin
  fInBuildPage:=True;
  try
    Modified := True;
    FileModified := True;
    with CurReport do
    begin
      if (n >= 0) and (n < Pages.Count) then
        if Pages.Count = 1 then
          Pages[n].Clear
        else
        begin
          CurReport.Pages.Delete(n);
          Tab1.Tabs.Delete(n);
          Tab1.TabIndex := 0;
          AdjustSubReports;
          CurPage := 0;
        end;
    end;
    ClearUndoBuffer;
    ClearRedoBuffer;
  finally
    fInBuildPage:=False;
  end;
end;

procedure TfrDesignerForm.SetPageTitles;
var
  i: Integer;
  s: String;
  
  function IsSubreport(PageN: Integer): Boolean;
  var
    i, j: Integer;
    t: TfrView;
  begin
    Result := False;
    with CurReport do
      for i := 0 to Pages.Count - 1 do
        for j := 0 to Pages[i].Objects.Count - 1 do
        begin
          t := TfrView(Pages[i].Objects[j]);
          if t.Typ = gtSubReport then
            if TfrSubReportView(t).SubPage = PageN then
            begin
              s := t.Name;
              Result := True;
              Exit;
            end;
        end;
  end;
  
begin
   if Tab1.Tabs.Count = CurReport.Pages.Count then
   begin
    for i := 0 to Tab1.Tabs.Count - 1 do
    begin
      if not IsSubreport(i) then
        s := sPg + IntToStr(i + 1);
      if Tab1.Tabs[i] <> s then
        Tab1.Tabs[i] := s;
    end;
  end
  else
  begin
    Tab1.Tabs.Clear;
    for i := 0 to CurReport.Pages.Count - 1 do
    begin
      if not IsSubreport(i) then
        s := sPg + IntToStr(i + 1);
      Tab1.Tabs.Add(s);
    end;
  end;
end;

procedure TfrDesignerForm.CutToClipboard;
var
  i: Integer;
  t: TfrView;
begin
  ClearClipBoard;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
    begin
      ClipBd.Add(frCreateObject(t.Typ, t.ClassName));
      TfrView(ClipBd.Last).Assign(t);
    end;
  end;
  for i := Objects.Count - 1 downto 0 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then Page.Delete(i);
  end;
  SelNum := 0;
end;

procedure TfrDesignerForm.CopyToClipboard;
var
  i: Integer;
  t: TfrView;
begin
  ClearClipBoard;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
    begin
      ClipBd.Add(frCreateObject(t.Typ, t.ClassName));
      TfrView(ClipBd.Last).Assign(t);
    end;
  end;
end;

procedure TfrDesignerForm.SelectAll;
var
  i: Integer;
begin
  SelNum := 0;
  for i := 0 to Objects.Count - 1 do
  begin
    TfrView(Objects[i]).Selected := True;
    Inc(SelNum);
  end;
end;

procedure TfrDesignerForm.Unselect;
var
  i: Integer;
begin
  SelNum := 0;
  for i := 0 to Objects.Count - 1 do
    TfrView(Objects[i]).Selected := False;
end;

procedure TfrDesignerForm.ResetSelection;
begin
  Unselect;
  EnableControls;
  ShowPosition;
end;

function TfrDesignerForm.PointsToUnits(x: Integer): Double;
begin
  Result := x;
  case FUnits of
    ruMM: Result := x / 18 * 5;
    ruInches: Result := x / 18 * 5 / 25.4;
  end;
end;

function TfrDesignerForm.UnitsToPoints(x: Double): Integer;
begin
  Result := Round(x);
  case FUnits of
    ruMM: Result := Round(x / 5 * 18);
    ruInches: Result := Round(x * 25.4 / 5 * 18);
  end;
end;

procedure TfrDesignerForm.RedrawPage;
begin
  PageView.Draw(10000, 0);
end;

procedure TfrDesignerForm.OnModify(sender: TObject);
begin
  SelectionChanged;
end;

procedure TfrDesignerForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  StepX, StepY: Integer;
  i, tx, ty, tx1, ty1, d, d1: Integer;
  t, t1: TfrView;

  procedure CheckStepFactor(var pStep: integer; aValue: integer);
  begin
    if (ssAlt in Shift) or (Shift = [ssShift,ssCtrl]) then
      pStep := aValue * 10
    else
      pStep := aValue;
  end;

begin
  {$IFNDEF EXTOI}
  if (ActiveControl<>nil) and (ActiveControl.Parent=ObjInsp.fPropertyGrid) then
    exit;
  {$ENDIF}
  StepX := 0; StepY := 0;
  if (Key=VK_F11) then
    ObjInsp.Visible:=not ObjInsp.Visible;

  if (Key = VK_RETURN) and (ActiveControl = C3) then
  begin
    Key := 0;
    DoClick(C3);
  end;
  if (Key = VK_RETURN) and (ActiveControl = E1) then
  begin
    Key := 0;
    DoClick(E1);
  end;
  if (Key = VK_DELETE) and DelEnabled then
  begin
    DeleteObjects;
    Key := 0;
  end;
  if (Key = VK_RETURN) and EditEnabled then
  begin
    if ssCtrl in Shift then
      ShowMemoEditor
    else
      ShowEditor;
  end;
  if (Chr(Key) in ['1'..'9']) and (ssCtrl in Shift) and DelEnabled then
  begin
    E1.Text := Chr(Key);
    DoClick(E1);
    Key := 0;
  end;
  if (Chr(Key) = 'F') and (ssCtrl in Shift) and DelEnabled then
  begin
    FrB5.Click;
    Key := 0;
  end;
  if (Chr(Key) = 'D') and (ssCtrl in Shift) and DelEnabled then
  begin
    FrB6.Click;
    Key := 0;
  end;
  if (Chr(Key) = 'G') and (ssCtrl in Shift) then
  begin
    ShowGrid := not ShowGrid;
    Key := 0;
  end;
  if (Chr(Key) = 'B') and (ssCtrl in Shift) then
  begin
    GridAlign := not GridAlign;
    Key := 0;
  end;
  if CutEnabled then
    if (Key = VK_DELETE) and (ssShift in Shift) then CutBClick(Self);
  if CopyEnabled then
    if (Key = VK_INSERT) and (ssCtrl in Shift) then CopyBClick(Self);
  if PasteEnabled then
    if (Key = VK_INSERT) and (ssShift in Shift) then PstBClick(Self);
    
  if Key = VK_PRIOR then
    with ScrollBox1.VertScrollBar do
    begin
      Position := Position - 200;
      Key := 0;
    end;
  if Key = VK_NEXT then
    with ScrollBox1.VertScrollBar do
    begin
      Position := Position + 200;
      Key := 0;
    end;
  if SelNum > 0 then
  begin
    if Key = vk_Up then CheckStepFactor(StepY, -1)
    else if Key = vk_Down then CheckStepFactor(StepY, 1)
    else if Key = vk_Left then CheckStepFactor(StepX, -1)
    else if Key = vk_Right then CheckStepFactor(StepX, 1);
    if (StepX <> 0) or (StepY <> 0) then
    begin
      if ssCtrl in Shift then
        MoveObjects(StepX, StepY, False)
      else if ssShift in Shift then
        MoveObjects(StepX, StepY, True)
      else if SelNum = 1 then
      begin
        t := TfrView(Objects[TopSelected]);
        tx := t.x; ty := t.y; tx1 := t.x + t.dx; ty1 := t.y + t.dy;
        d := 10000; t1 := nil;
        for i := 0 to Objects.Count-1 do
        begin
          t := TfrView(Objects[i]);
          if not t.Selected and (t.Typ <> gtBand) then
          begin
            d1 := 10000;
            if StepX <> 0 then
            begin
              if t.y + t.dy < ty then
                d1 := ty - (t.y + t.dy)
              else if t.y > ty1 then
                d1 := t.y - ty1
              else if (t.y <= ty) and (t.y + t.dy >= ty1) then
                d1 := 0
              else
                d1 := t.y - ty;
              if ((t.x <= tx) and (StepX = 1)) or
                 ((t.x + t.dx >= tx1) and (StepX = -1)) then
                d1 := 10000;
              if StepX = 1 then
                if t.x >= tx1 then
                  d1 := d1 + t.x - tx1 else
                  d1 := d1 + t.x - tx
              else if t.x + t.dx <= tx then
                  d1 := d1 + tx - (t.x + t.dx) else
                  d1 := d1 + tx1 - (t.x + t.dx);
            end
            else if StepY <> 0 then
            begin
              if t.x + t.dx < tx then
                d1 := tx - (t.x + t.dx)
              else if t.x > tx1 then
                d1 := t.x - tx1
              else if (t.x <= tx) and (t.x + t.dx >= tx1) then
                d1 := 0
              else
                d1 := t.x - tx;
              if ((t.y <= ty) and (StepY = 1)) or
                 ((t.y + t.dy >= ty1) and (StepY = -1)) then
                d1 := 10000;
              if StepY = 1 then
                if t.y >= ty1 then
                  d1 := d1 + t.y - ty1 else
                  d1 := d1 + t.y - ty
              else if t.y + t.dy <= ty then
                  d1 := d1 + ty - (t.y + t.dy) else
                  d1 := d1 + ty1 - (t.y + t.dy);
            end;
            if d1 < d then
            begin
              d := d1;
              t1 := t;
            end;
          end;
        end;
        if t1 <> nil then
        begin
          t := TfrView(Objects[TopSelected]);
          if not (ssAlt in Shift) then
          begin
            PageView.DrawPage(dmSelection);
            Unselect;
            SelNum := 1;
            t1.Selected := True;
            PageView.DrawPage(dmSelection);
          end
          else
          begin
            if (t1.x >= t.x + t.dx) and (Key = VK_RIGHT) then
              t.x := t1.x - t.dx
            else if (t1.y > t.y + t.dy) and (Key = VK_DOWN) then
              t.y := t1.y - t.dy
            else if (t1.x + t1.dx <= t.x) and (Key = VK_LEFT) then
              t.x := t1.x + t1.dx
            else if (t1.y + t1.dy <= t.y) and (Key = VK_UP) then
              t.y := t1.y + t1.dy;
            RedrawPage;
          end;
          SelectionChanged;
        end;
      end;
      Key := 0;
    end; // if (StepX <> 0) or (StepY <> 0)
  end; // if SelNum > 0 then
end;

procedure TfrDesignerForm.MoveObjects(dx, dy: Integer; aResize: Boolean);
begin
  AddUndoAction(acEdit);
  PageView.DrawPage(dmSelection);
  PageView.MoveResize(Dx,Dy, false, aResize);
  ShowPosition;
  PageView.GetMultipleSelected;
end;

procedure TfrDesignerForm.UpdateStatus;
begin
  {$ifdef sbod}
  StatusBar1.Update;
  {$else}
  PBox1Paint(nil);
  {$endif}
end;

procedure TfrDesignerForm.DeleteObjects;
var
  i: Integer;
  t: TfrView;
begin
  AddUndoAction(acDelete);
  GetRegion; // JRA 3
  PageView.DrawPage(dmSelection);
  for i := Objects.Count - 1 downto 0 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      Page.Delete(i);
  end;
  SetPageTitles;
  ResetSelection;
  FirstSelected := nil;
  PageView.Invalidate;
end;

function TfrDesignerForm.SelStatus: TfrSelectionStatus;
var
  t: TfrView;
begin
  Result := [];
  if SelNum = 1 then
  begin
    t := TfrView(Objects[TopSelected]);
    if t.Typ = gtBand then
      Result := [ssBand]
    else if t is TfrMemoView then
           Result := [ssMemo]
         else
           Result := [ssOther];
  end
  else if SelNum > 1 then
          Result := [ssMultiple];
          
  if ClipBd.Count > 0 then
    Result := Result + [ssClipboardFull];
end;

procedure TfrDesignerForm.UpdScrollbars;
begin
  ScrollBox1.Autoscroll := False;
  ScrollBox1.Autoscroll := True;
  ScrollBox1.VertScrollBar.Range := ScrollBox1.VertScrollBar.Range + 10;
  //ScrollBox1.VertScrollBar.Range := ScrollBox1.VertScrollBar.Range + 10;
end;

procedure TfrDesignerForm.InsertFieldsFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if (sender=frInsertFieldsForm) and (frInsertFieldsForm.ModalResult=mrOk) then
    InsertDbFields;
end;

{$HINTS OFF}
procedure TfrDesignerForm.InsertDbFields;
var
  i, x, y, dx, dy, pdx, adx: Integer;
  HeaderL, DataL: TFpList;
  t, t1: TfrView;
  b: TfrBandView;
  f: TfrTField;
  f1: TFieldDef;
  fSize: Integer;
  fName: String;

  function FindDataset(DataSet: TfrTDataSet): String;
  var
    i,j: Integer;

    function EnumComponents(f: TComponent): String;
    var
      i: Integer;
      c: TComponent;
      d: TfrDBDataSet;
    begin
      Result := '';
      for i := 0 to f.ComponentCount - 1 do
      begin
        c := f.Components[i];
        if c is TfrDBDataSet then
        begin
          d := c as TfrDBDataSet;
          if d.GetDataSet = DataSet then
          begin
            if d.Owner = CurReport.Owner then
              Result := d.Name else
              Result := d.Owner.Name + '.' + d.Name;
            break;
          end;
        end;
      end;
    end;

  begin
    Result := '';
    for i := 0 to Screen.FormCount - 1 do
    begin
      Result := EnumComponents(Screen.Forms[i]);
      if Result <> '' then Exit;
    end;

    with Screen do
    begin
      for i := 0 to CustomFormCount - 1 do
        with CustomForms[i] do
        if (ClassName = 'TDataModuleForm')  then
          for j := 0 to ComponentCount - 1 do
          begin
            if (Components[j] is TDataModule) then
              Result:=EnumComponents(Components[j]);
              if Result <> '' then Exit;
          end;
    end;
  end;
begin
  if frInsertFieldsForm=nil then
    exit;
    
  with frInsertFieldsForm do
  begin
    if (DataSet=nil) or (FieldsL.Items.Count = 0) or (FieldsL.SelCount = 0) then
      exit;
      
    HeaderL := TFpList.Create;
    DataL := TFpList.Create;
    try
      x := Page.LeftMargin; y := Page.TopMargin;
      Unselect;
      SelNum := 0;
      for i := 0 to FieldsL.Items.Count - 1 do
        if FieldsL.Selected[i] then
        begin
          f := TfrTField(DataSet.FindField(FieldsL.Items[i]));
          fSize := 0;
          if f <> nil then
          begin
            fSize := f.DisplayWidth;
            fName := f.DisplayName;
          end
          else
          begin
            f1 := DataSet.FieldDefs[i];
            fSize := f1.Size;
            fName := f1.Name;
          end;

          if (fSize = 0) or (fSize > 255) then
            fSize := 6;

          t := frCreateObject(gtMemo, '');
          t.CreateUniqueName;
          t.x := x;
          t.y := y;
          GetDefaultSize(t.dx, t.dy);
          with t as TfrMemoView do
          begin
            Font.Name := LastFontName;
            Font.Size := LastFontSize;
            if HeaderCB.Checked then
              Font.Style := [fsBold];
            MonitorFontChanges;
          end;
          PageView.Canvas.Font.Assign(TfrMemoView(t).Font);
          t.Selected := True;
          Inc(SelNum);
          if HeaderCB.Checked then
          begin
            t.Memo.Add(fName);
            t.dx := PageView.Canvas.TextWidth(fName + '   ') div GridSize * GridSize;
          end
          else
          begin
            t.Memo.Add('[' + DatasetCB.Items[DatasetCB.ItemIndex] +
              '."' + FieldsL.Items[i] + '"]');
            t.dx := (fSize * PageView.Canvas.TextWidth('=')) div GridSize * GridSize;
          end;
          dx := t.dx;
          Page.Objects.Add(t);
          if HeaderCB.Checked then
            HeaderL.Add(t) else
            DataL.Add(t);
          if HeaderCB.Checked then
          begin
            t := frCreateObject(gtMemo, '');
            t.CreateUniqueName;
            t.x := x;
            t.y := y;
            GetDefaultSize(t.dx, t.dy);
            if HorzRB.Checked then
              Inc(t.y, 72) else
              Inc(t.x, dx + GridSize * 2);
            with t as TfrMemoView do
            begin
              Font.Name := LastFontName;
              Font.Size := LastFontSize;
              MonitorFontChanges;
            end;
            t.Selected := True;
            Inc(SelNum);
            t.Memo.Add('[' + DatasetCB.Items[DatasetCB.ItemIndex] +
              '."' + FieldsL.Items[i] + '"]');
            t.dx := (fSize * PageView.Canvas.TextWidth('=')) div GridSize * GridSize;
            Page.Objects.Add(t);
            DataL.Add(t);
          end;
          if HorzRB.Checked then
            Inc(x, t.dx + GridSize) else
            Inc(y, t.dy + GridSize);
        end;

      if HorzRB.Checked then
      begin
        t := TfrView(DataL[DataL.Count - 1]);
        adx := t.x + t.dx;
        pdx := Page.RightMargin - Page.LeftMargin;
        x := Page.LeftMargin;
        if adx > pdx then
        begin
          for i := 0 to DataL.Count - 1 do
          begin
            t := TfrView(DataL[i]);
            t.x := Round((t.x - x) / (adx / pdx)) + x;
            t.dx := Round(t.dx / (adx / pdx));
          end;
          if HeaderCB.Checked then
            for i := 0 to DataL.Count - 1 do
            begin
              t := TfrView(HeaderL[i]);
              t1 := TfrView(DataL[i]);
              t.x := Round((t.x - x) / (adx / pdx)) + x;
              if t.dx > t1.dx then
                t.dx := t1.dx;
            end;
        end;
      end;

      if BandCB.Checked then
      begin
        if HeaderCB.Checked then
          t := TfrView(HeaderL[DataL.Count - 1])
        else
          t := TfrView(DataL[DataL.Count - 1]);
        dy := t.y + t.dy - Page.TopMargin;
        b := frCreateObject(gtBand, '') as TfrBandView;
        b.CreateUniqueName;
        b.y := Page.TopMargin;
        b.dy := dy;
        b.Selected := True;
        Inc(SelNum);
        if not HeaderCB.Checked or not HorzRB.Checked then
        begin
          Page.Objects.Add(b);
          b.BandType := btMasterData;
          b.DataSet := FindDataset(DataSet);
        end
        else
        begin
          if frCheckBand(btPageHeader) then
          begin
            Dec(SelNum);
            b.Free;
          end
          else
          begin
            b.BandType := btPageHeader;
            Page.Objects.Add(b);
          end;
          b := frCreateObject(gtBand, '') as TfrBandView;
          b.BandType := btMasterData;
          b.DataSet := FindDataset(DataSet);
          b.CreateUniqueName;
          b.y := Page.TopMargin + 72;
          b.dy := dy;
          b.Selected := True;
          Inc(SelNum);
          Page.Objects.Add(b);
        end;
      end;
      SelectionChanged;
      SendBandsToDown;
      PageView.GetMultipleSelected;
      RedrawPage;
      AddUndoAction(acInsert);
    finally
      HeaderL.Free;
      DataL.Free;
    end;
  end;
end;

{$ifdef sbod}
procedure TfrDesignerForm.DrawStatusPanel(const ACanvas: TCanvas;
  const rect: TRect);
var
  t: TfrView;
  s: String;
  nx, ny: Double;
  x, y, dx, dy: Integer;
begin
  with ACanvas do
  begin
    Brush.Color := StatusBar1.Color;
    FillRect(Rect);
    ImageList1.Draw(ACanvas, Rect.Left + 2, Rect.Top+2, 0);
    ImageList1.Draw(ACanvas, Rect.Left + 92, Rect.Top+2, 1);
    if (SelNum = 1) or ShowSizes then
    begin
      t := nil;
      if ShowSizes then
      begin
        x := OldRect.Left;
        y := OldRect.Top;
        dx := OldRect.Right - x;
        dy := OldRect.Bottom - y;
      end
      else
      begin
        t := TfrView(Objects[TopSelected]);
        x := t.x;
        y := t.y;
        dx := t.dx;
        dy := t.dy;
      end;

      if FUnits = ruPixels then
        s := IntToStr(x) + ';' + IntToStr(y)
      else
        s := FloatToStrF(PointsToUnits(x), ffFixed, 4, 2) + '; ' +
              FloatToStrF(PointsToUnits(y), ffFixed, 4, 2);

      TextOut(Rect.Left + 20, Rect.Top + 1, s);
      if FUnits = ruPixels then
        s := IntToStr(dx) + ';' + IntToStr(dy)
      else
        s := FloatToStrF(PointsToUnits(dx), ffFixed, 4, 2) + '; ' +
               FloatToStrF(PointsToUnits(dy), ffFixed, 4, 2);
      TextOut(Rect.Left + 110, Rect.Top + 1, s);

      if not ShowSizes and (t.Typ = gtPicture) then
      begin
        with t as TfrPictureView do
        begin
          if (Picture.Graphic <> nil) and not Picture.Graphic.Empty then
          begin
            s := IntToStr(dx * 100 div Picture.Width) + ',' +
                 IntToStr(dy * 100 div Picture.Height);
            TextOut(Rect.Left + 170, Rect.Top + 1, '% ' + s);
          end;
        end;
      end;
    end
    else if (SelNum > 0) and MRFlag then
         begin
            nx := 0;
            ny := 0;
            if OldRect1.Right - OldRect1.Left <> 0 then
              nx := (OldRect.Right - OldRect.Left) / (OldRect1.Right - OldRect1.Left);
            if OldRect1.Bottom - OldRect1.Top <> 0 then
              ny := (OldRect.Bottom - OldRect.Top) / (OldRect1.Bottom - OldRect1.Top);
            s := IntToStr(Round(nx * 100)) + ',' + IntToStr(Round(ny * 100));
            TextOut(Rect.left + 170, Rect.Top + 1, '% ' + s);
         end;
  end;
end;

procedure TfrDesignerForm.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if Panel.Index=1 then
    DrawStatusPanel(StatusBar.Canvas, Rect);
end;

{$endif}

{$HINTS ON}

function TfrDesignerForm.RectTypEnabled: Boolean;
begin
  Result := [ssMemo, ssOther, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.FontTypEnabled: Boolean;
begin
  Result := [ssMemo, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.ZEnabled: Boolean;
begin
  Result := [ssBand, ssMemo, ssOther, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.CutEnabled: Boolean;
begin
  Result := [ssBand, ssMemo, ssOther, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.CopyEnabled: Boolean;
begin
  Result := [ssBand, ssMemo, ssOther, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.PasteEnabled: Boolean;
begin
  Result := ssClipboardFull in SelStatus;
end;

function TfrDesignerForm.DelEnabled: Boolean;
begin
  Result := [ssBand, ssMemo, ssOther, ssMultiple] * SelStatus <> [];
end;

function TfrDesignerForm.EditEnabled: Boolean;
begin
  Result:=[ssBand,ssMemo,ssOther]*SelStatus <> [];
end;

procedure TfrDesignerForm.EnableControls;

  procedure SetCtrlEnabled(const Ar: Array of TObject; en: Boolean);
  var
    i: Integer;
  begin
    for i := Low(Ar) to High(Ar) do
      if Ar[i] is TControl then
        (Ar[i] as TControl).Enabled := en
      else if Ar[i] is TMenuItem then
        (Ar[i] as TMenuItem).Enabled := en;
  end;
  
begin
  SetCtrlEnabled([FrB1, FrB2, FrB3, FrB4, FrB5, FrB6, ClB1, ClB3, E1, SB1, SB2, StB1],
    RectTypEnabled);
  SetCtrlEnabled([ClB2, C2, C3, FnB1, FnB2, FnB3, AlB1, AlB2, AlB3, AlB4, AlB5, AlB6, AlB7, AlB8, HlB1],
    FontTypEnabled);
  SetCtrlEnabled([ZB1, ZB2, N32, N33, GB3], ZEnabled);
  SetCtrlEnabled([CutB, N11, N2], CutEnabled);
  SetCtrlEnabled([CopyB, N12, N1], CopyEnabled);
  SetCtrlEnabled([PstB, N13, N3], PasteEnabled);
  SetCtrlEnabled([N27, N5], DelEnabled);
  SetCtrlEnabled([N36, N6], EditEnabled);
  if not C2.Enabled then
  begin
    C2.ItemIndex := -1;
    C3.Text := '';
  end;

  StatusBar1.Repaint;
  {$ifndef sbod}
  PBox1.Invalidate;
  {$endif}
end;

procedure TfrDesignerForm.SelectionChanged;
var
  t: TfrView;
begin
  {$IFDEF DebugLR}
  debugLn('TfrDesignerForm.SelectionChanged INIT, SelNum=',dbgs(SelNum));
  {$ENDIF}
  Busy := True;
  ColorSelector.Hide;
  LinePanel.Hide;
  EnableControls;
  if SelNum = 1 then
  begin
    t := TfrView(Objects[TopSelected]);
    if t.Typ <> gtBand then
    with t do
    begin
      {$IFDEF DebugLR}
      DebugLn('selectionchanged 1');
      {$ENDIF}
      FrB1.Down := (frbTop in Frames);
      FrB2.Down := (frbLeft in Frames);
      FrB3.Down := (frbBottom in Frames);
      FrB4.Down := (frbRight  in Frames);
      E1.Text := FloatToStrF(FrameWidth, ffGeneral, 2, 2);
      frSetGlyph(FillColor, ClB1, 1);
      frSetGlyph(FrameColor, ClB3, 2);
      if t is TfrMemoView then
      with t as TfrMemoView do
      begin
        frSetGlyph(Font.Color, ClB2, 0);
        if C2.ItemIndex <> C2.Items.IndexOf(Font.Name) then
          C2.ItemIndex := C2.Items.IndexOf(Font.Name);
          
        if C3.Text <> IntToStr(Font.Size) then
          C3.Text := IntToStr(Font.Size);
          
        FnB1.Down := fsBold in Font.Style;
        FnB2.Down := fsItalic in Font.Style;
        FnB3.Down := fsUnderline in Font.Style;
        
        AlB4.Down := (Adjust and $4) <> 0;
        AlB5.Down := (Adjust and $18) = $8;
        AlB6.Down := (Adjust and $18) = 0;
        AlB7.Down := (Adjust and $18) = $10;
        case (Adjust and $3) of
          0: BDown(AlB1);
          1: BDown(AlB2);
          2: BDown(AlB3);
          3: BDown(AlB8);
        end;
      end;
    end;
  end
  else if SelNum > 1 then
  begin
    {$IFDEF DebugLR}
    DebugLn('selectionchanged 2');
    {$ENDIF}

    BUp(FrB1);
    BUp(FrB2);
    BUp(FrB3);
    BUp(FrB4);
    ColorLocked := True;
    frSetGlyph(0, ClB1, 1);
    ColorLocked := False;
    E1.Text := '1';
    C2.ItemIndex := -1;
    C3.Text := '';
    BUp(FnB1);
    BUp(FnB2);
    BUp(FnB3);
    BDown(AlB1);
    BUp(AlB4);
    BUp(AlB5);
  end;
  Busy := False;
  ShowPosition;
  ShowContent;
  ActiveControl := nil;
  {$IFDEF DebugLR}
  debugLn('TfrDesignerForm.SelectionChanged END, SelNum=',dbgs(SelNum));
  {$ENDIF}
end;

procedure TfrDesignerForm.ShowPosition;
begin
  FillInspFields;
  StatusBar1.Repaint;
  {$ifndef sbod}
  PBox1.Invalidate;
  {$endif}
end;

procedure TfrDesignerForm.ShowContent;
var
  t: TfrView;
  s: String;
begin
  s := '';
  if SelNum = 1 then
  begin
    t := TfrView(Objects[TopSelected]);
    s := t.Name;
    if t is TfrBandView then
      s := s + ': ' + frBandNames[TfrBandView(t).BandType]
    else if t.Memo.Count > 0 then
      s := s + ': ' + t.Memo[0];
  end;
  StatusBar1.Panels[2].Text := s;
end;

procedure TfrDesignerForm.DoClick(Sender: TObject);
var
  i, j, b: Integer;
  s      : String;
  t      : TfrView;
begin
  if Busy then
    Exit;
  AddUndoAction(acEdit);
  PageView.DrawPage(dmSelection);
  GetRegion;
  b:=(Sender as TControl).Tag;
  
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected and ((t.Typ <> gtBand) or (b = 16)) then
    with t do
    begin
      if t is TfrMemoView then
      with t as TfrMemoView do
        case b of
          7: if C2.ItemIndex <> 0 then
             begin
               Font.Name := C2.Text;
               LastFontName := Font.Name;
             end;
          8: begin
               Font.Size := StrToIntDef(C3.Text, LastFontSize);
               LastFontSize := Font.Size;
             end;
          9: begin
               LastFontStyle := frGetFontStyle(Font.Style);
               //SetBit(LastFontStyle, not FnB1.Down, 2);
               SetBit(LastFontStyle, FnB1.Down, 2);
               Font.Style := frSetFontStyle(LastFontStyle);
             end;
         10: begin
               LastFontStyle := frGetFontStyle(Font.Style);
               //SetBit(LastFontStyle, not FnB2.Down, 1);
               SetBit(LastFontStyle, FnB2.Down, 1);
               Font.Style := frSetFontStyle(LastFontStyle);
             end;
         11..13:
             begin
               Adjust := (Adjust and $FC) + (b-11);
               LastAdjust := Adjust;
             end;
         14: begin
               Adjust := (Adjust and $FB) + Word(AlB4.Down) * 4;
               LastAdjust := Adjust;
             end;
         15: begin
               Adjust := (Adjust and $E7) + Word(AlB5.Down) * 8 + Word(AlB7.Down) * $10;
               LastAdjust := Adjust;
             end;
         17: begin
               Font.Color := ColorSelector.Color;
               LastFontColor := Font.Color;
             end;
         18: begin
               LastFontStyle := frGetFontStyle(Font.Style);
//               SetBit(LastFontStyle, not FnB3.Down, 4);
               SetBit(LastFontStyle, FnB3.Down, 4);
               Font.Style := frSetFontStyle(LastFontStyle);
             end;
         22: begin
               //Alignment:=tafrJustify;
               Adjust := (Adjust and $FC) + 3;
               LastAdjust := Adjust;
             end;
        end;
        
      case b of
        1:
         begin //Top frame
           if (Sender=frB1) and frB1.Down then
             Frames:=Frames+[frbTop]
           else
             Frames:=Frames-[frbTop];
           DRect := Rect(t.x - 10, t.y - 10, t.x + t.dx + 10, t.y + 10)
         end;
        2: //Left frame
         begin
           if (Sender=FrB2) and frB2.Down then
             Frames:=Frames+[frbLeft]
           else
             Frames:=Frames-[frbLeft];
           DRect := Rect(t.x - 10, t.y - 10, t.x + 10, t.y + t.dy + 10)
         end;
        3: //Bottom Frame
         begin
           if (Sender=FrB3) and frB3.Down then
             Frames:=Frames+[frbBottom]
           else
             Frames:=Frames-[frbBottom];
           DRect := Rect(t.x - 10, t.y + t.dy - 10, t.x + t.dx + 10, t.y + t.dy + 10)
         end;
        4: //Right Frame
         begin
           if (Sender=FrB4) and frB4.Down then
             Frames:=Frames+[frbRight]
           else
             Frames:=Frames-[frbRight];
           DRect := Rect(t.x + t.dx - 10, t.y - 10, t.x + t.dx + 10, t.y + t.dy + 10)
         end;
        20:
         begin
           if (Sender=FrB5) then
             Frames:=[frbLeft, frbTop, frbRight, frbBottom];

           LastFrames:=Frames;
         end;
        21:
         begin
           if (Sender=FrB6) then
             Frames:=[];
           LastFrames:=[];
         end;
        5:
         begin
           FillColor:=ColorSelector.Color;
           LastFillColor := FillColor;
         end;
        6:
         begin
           s := E1.Text;
           for j := 1 to Length(s) do
             if s[j] in ['.', ','] then
               s[j] := DecimalSeparator;
           FrameWidth := StrToFloat(s);
           if t is TfrLineView then
             LastLineWidth := FrameWidth
           else
             LastFrameWidth := FrameWidth;
         end;
        19:
         begin
           FrameColor := ColorSelector.Color;
           LastFrameColor := FrameColor;
         end;
        25..30:
          FrameStyle:=TfrFrameStyle(b - 25);
      end;
    end;
  end;

  PageView.Draw(TopSelected, ClipRgn);

  ActiveControl := nil;
  if b in [20, 21] then
     SelectionChanged;
end;

procedure TfrDesignerForm.frSpeedButton1Click(Sender: TObject);
begin
  LinePanel.Hide;
  DoClick(Sender);
end;

procedure TfrDesignerForm.HlB1Click(Sender: TObject);
var
  t: TfrMemoView;
begin
  t := TfrMemoView(Objects[TopSelected]);
  frHilightForm := TfrHilightForm.Create(nil);
  with frHilightForm do
  begin
    FontColor := t.Highlight.FontColor;
    FillColor := t.Highlight.FillColor;
    CB1.Checked := (t.Highlight.FontStyle and $2) <> 0;
    CB2.Checked := (t.Highlight.FontStyle and $1) <> 0;
    CB3.Checked := (t.Highlight.FontStyle and $4) <> 0;
    Edit1.Text := t.HighlightStr;
    if ShowModal = mrOk then
    begin
      AddUndoAction(acEdit);
      t.HighlightStr := Edit1.Text;
      t.Highlight.FontColor := FontColor;
      t.Highlight.FillColor := FillColor;
      SetBit(t.Highlight.FontStyle, CB1.Checked, 2);
      SetBit(t.Highlight.FontStyle, CB2.Checked, 1);
      SetBit(t.Highlight.FontStyle, CB3.Checked, 4);
    end;
  end;
  frHilightForm.Free;
end;

procedure TfrDesignerForm.FillInspFields;
var
  t: TfrView;
begin
  ObjInspSelect(nil);
  if SelNum = 0 then
    ObjInspSelect(Page)
  else
  if SelNum = 1 then
  begin
    t := TfrView(Objects[TopSelected]);
    ObjInspSelect(t);
  end else
  if SelNum > 1 then
    ObjInspSelect(Objects);
  ObjInspRefresh;
end;

{
procedure TfrDesignerForm.OnModify(Item: Integer; var EditText: String);
var
  t: TfrView;
  i, k: Integer;
begin
  AddUndoAction(acEdit);
  if (Item = 0) and (SelNum = 1) then
  begin
    t := TfrView(Objects[TopSelected]);
    if CurReport.FindObject(fld[0]) = nil then
      t.Name := fld[0] else
      EditText := t.Name;
    SetPageTitles;
  end
  else if Item in [1..5] then
  begin
    EditText := frParser.Calc(fld[Item]);
    if Item <> 6 then
      k := UnitsToPoints(StrToFloat(EditText)) else
      k := StrToInt(EditText);
    for i := 0 to Objects.Count-1 do
    begin
      t := TfrView(Objects[i]);
      if t.Selected then
      with t do
        case Item of
          1: if (k > 0) and (k < Page.PrnInfo.Pgw) then
               x := k;
          2: if (k > 0) and (k < Page.PrnInfo.Pgh) then
             y := k;
          3: if (k > 0) and (k < Page.PrnInfo.Pgw) then
             dx := k;
          4: if (k > 0) and (k < Page.PrnInfo.Pgh) then
             dy := k;
          5: Visible := Boolean(k);
        end;
    end;
  end;
  FillInspFields;
  if Item in [1..5] then
    EditText := fld[Item];
  RedrawPage;
  StatusBar1.Repaint;
  PBox1.Invalidate;
end;
}
procedure TfrDesignerForm.StB1Click(Sender: TObject);
var
  p: TPoint;
begin
  if not LinePanel.Visible then
  begin
    LinePanel.Parent := Self;
    with (Sender as TControl) do
      p := Self.ScreenToClient(Parent.ClientToScreen(Point(Left, Top)));
    LinePanel.SetBounds(p.X,p.Y + 26,LinePanel.Width,LinePanel.Height);
  end;
  LinePanel.Visible := not LinePanel.Visible;
end;

procedure TfrDesignerForm.ObjInspSelect(Obj: TObject);
{$IFDEF STDOI}
var
  Selection: TPersistentSelectionList;
  i: Integer;
{$ENDIF}
begin
  {$IFDEF STDOI}
  Selection := TPersistentSelectionList.Create;
  PropHook.LookupRoot:=nil;
  if Obj is TPersistent then
  begin
    Selection.Add(TPersistent(Obj));
    PropHook.LookupRoot:=TPersistent(Obj);
  end else
  if Obj is TFpList then
    with frDesigner.page do
      for i:=0 to Objects.Count-1 do
        if TfrView(Objects[i]).Selected then
        begin
          if PropHook.LookupRoot=nil then
            PropHook.LookupRoot := TPersistent(Objects[i]);
          Selection.Add(TPersistent(Objects[i]));
        end;
  ObjInsp.Selection := Selection;
  Selection.Free;
  {$ELSE}
  ObjInsp.Select(Obj);
  {$ENDIF}
end;

procedure TfrDesignerForm.ObjInspRefresh;
begin
  {$IFDEF STDOI}
  //TODO: refresh
  {$ELSE}
  ObjInsp.Refresh;
  {$ENDIF}
end;

procedure TfrDesignerForm.ClB1Click(Sender: TObject);
var p  : TPoint;
    t  : TfrView;
    CL : TColor;
begin
  with (Sender as TControl) do
    p := Self.ScreenToClient(Parent.ClientToScreen(Point(Left, Top)));
  if ColorSelector.Left = p.X then
    ColorSelector.Visible := not ColorSelector.Visible
  else
  begin
    with ColorSelector do SetBounds(p.X,p.Y + 26,Width,Height);
    ColorSelector.Visible := True;
  end;
  ClrButton := Sender as TSpeedButton;
  t := TfrView(Objects[TopSelected]);
  CL:=clNone;
  if Sender=ClB1 then
    CL:=t.FillColor;
  if (Sender=ClB2) and (t is TfrMemoView) then
    CL:=TfrMemoView(t).Font.Color;
  if Sender=ClB3 then
    CL:=t.FrameColor;
  ColorSelector.Color:=CL;
end;

procedure TfrDesignerForm.ColorSelected(Sender: TObject);
var
  n: Integer;
begin
  n := 0;
  if ClrButton = ClB1 then
    n := 1
  else
    if ClrButton = ClB3 then
       n := 2;
  {$IFDEF DebugLR}
  DebugLn('ColorSelected');
  {$ENDIF}
  frSetGlyph(ColorSelector.Color, ClrButton, n);

  DoClick(ClrButton);
end;

procedure TfrDesignerForm.PBox1Paint(Sender: TObject);
var
  t: TfrView;
  s: String;
  nx, ny: Double;
  x, y, dx, dy: Integer;
begin
  with PBox1.Canvas do
  begin
    FillRect(Rect(0, 0, PBox1.Width, PBox1.Height));
    ImageList1.Draw(PBox1.Canvas, 2, 0, 0);
    ImageList1.Draw(PBox1.Canvas, 92, 0, 1);
    if (SelNum = 1) or ShowSizes then
    begin
      t := nil;
      if ShowSizes then
      begin
        x := OldRect.Left;
        y := OldRect.Top;
        dx := OldRect.Right - x;
        dy := OldRect.Bottom - y;
      end
      else
      begin
        t := TfrView(Objects[TopSelected]);
        x := t.x;
        y := t.y;
        dx := t.dx;
        dy := t.dy;
      end;
      
      if FUnits = ruPixels then
        s := IntToStr(x) + ';' + IntToStr(y)
      else
        s := FloatToStrF(PointsToUnits(x), ffFixed, 4, 2) + '; ' +
              FloatToStrF(PointsToUnits(y), ffFixed, 4, 2);
              
      TextOut(20, 1, s);
      if FUnits = ruPixels then
        s := IntToStr(dx) + ';' + IntToStr(dy)
      else
        s := FloatToStrF(PointsToUnits(dx), ffFixed, 4, 2) + '; ' +
               FloatToStrF(PointsToUnits(dy), ffFixed, 4, 2);
      TextOut(110, 1, s);

      if not ShowSizes and (t.Typ = gtPicture) then
      begin
        with t as TfrPictureView do
        begin
          if (Picture.Graphic <> nil) and not Picture.Graphic.Empty then
          begin
            s := IntToStr(dx * 100 div Picture.Width) + ',' +
                 IntToStr(dy * 100 div Picture.Height);
            TextOut(170, 1, '% ' + s);
          end;
        end;
      end;
    end
    else if (SelNum > 0) and MRFlag then
         begin
            nx := 0;
            ny := 0;
            if OldRect1.Right - OldRect1.Left <> 0 then
              nx := (OldRect.Right - OldRect.Left) / (OldRect1.Right - OldRect1.Left);
            if OldRect1.Bottom - OldRect1.Top <> 0 then
              ny := (OldRect.Bottom - OldRect.Top) / (OldRect1.Bottom - OldRect1.Top);
            s := IntToStr(Round(nx * 100)) + ',' + IntToStr(Round(ny * 100));
            TextOut(170, 1, '% ' + s);
         end;
  end;
end;

procedure TfrDesignerForm.C2DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with C2.Canvas do
  begin
    Font.Name := 'default';
    FillRect(Rect);
    if (PtrInt(C2.Items.Objects[Index]) and TRUETYPE_FONTTYPE) <> 0 then
      ImageList2.Draw(C2.Canvas, Rect.Left, Rect.Top + 1, 0);
    TextOut(Rect.Left + 20, Rect.Top + 1, C2.Items[Index]);
  end;
end;

procedure TfrDesignerForm.ShowMemoEditor;
begin
  with EditorForm do
  begin
    View := TfrView(Objects[TopSelected]);
    if ShowEditor = mrOk then
    begin
      PageView.DrawPage(dmSelection);
      PageView.Draw(TopSelected, View.GetClipRgn(rtExtended));
    end;
  end;
  ActiveControl := nil;
end;

procedure TfrDesignerForm.ShowEditor;
var
  t: TfrView;
  i: Integer;
  bt: TfrBandType;
begin
  SetCaptureControl(nil);
  t := TfrView(Objects[TopSelected]);
  if t.Typ = gtMemo then
    ShowMemoEditor
  else if t.Typ = gtPicture then
  begin
    frGEditorForm := TfrGEditorForm.Create(nil);
    with frGEditorForm do
    begin
      Image1.Picture.Assign((t as TfrPictureView).Picture);
      if ShowModal = mrOk then
      begin
        AddUndoAction(acEdit);
        PageView.DrawPage(dmSelection);
        (t as TfrPictureView).Picture.Assign(Image1.Picture);
        PageView.Draw(TopSelected, t.GetClipRgn(rtExtended));
      end;
    end;
    frGEditorForm.Free;
  end
  else if t.Typ = gtBand then
  begin
    PageView.DrawPage(dmSelection);
    bt := (t as TfrBandView).BandType;
    if bt in [btMasterData, btDetailData, btSubDetailData] then
    begin
      frBandEditorForm := TfrBandEditorForm.Create(nil);
      frBandEditorForm.ShowEditor(t);
      frBandEditorForm.Free;
    end
    else if bt = btGroupHeader then
    begin
      frGroupEditorForm := TfrGroupEditorForm.Create(nil);
      frGroupEditorForm.ShowEditor(t);
      frGroupEditorForm.Free;
    end
    else if bt = btCrossData then
    begin
      frVBandEditorForm := TfrVBandEditorForm.Create(nil);
      frVBandEditorForm.ShowEditor(t);
      frVBandEditorForm.Free;
    end
    else
      PageView.DFlag := False;
    PageView.Draw(TopSelected, t.GetClipRgn(rtExtended));
  end
  else if t.Typ = gtSubReport then
    CurPage := (t as TfrSubReportView).SubPage
  else if t.Typ = gtAddIn then
  begin
    for i := 0 to frAddInsCount - 1 do
      if frAddIns[i].ClassRef.ClassName = t.ClassName then
      begin
        if frAddIns[i].EditorForm <> nil then
        begin
          PageView.DrawPage(dmSelection);
          frAddIns[i].EditorForm.ShowEditor(t);
          PageView.Draw(TopSelected, t.GetClipRgn(rtExtended));
        end
        else
          ShowMemoEditor;
        break;
      end;
  end;
  ShowContent;
  ShowPosition;
  ActiveControl := nil;
end;

procedure TfrDesignerForm.ReleaseAction(ActionRec: TfrUndoRec);
var
  p, p1: PfrUndoObj;
begin
  p := ActionRec.Objects;
  while p <> nil do
  begin
    if ActionRec.Action in [acDelete, acEdit] then
      p^.ObjPtr.Free;
    p1 := p;
    p := p^.Next;
    FreeMem(p1, SizeOf(TfrUndoObj));
  end;
end;

procedure TfrDesignerForm.ClearBuffer(Buffer: TfrUndoBuffer; var BufferLength: Integer);
var
  i: Integer;
begin
  for i := 0 to BufferLength - 1 do
    ReleaseAction(Buffer[i]);
  BufferLength := 0;
end;

procedure TfrDesignerForm.ClearUndoBuffer;
begin
  ClearBuffer(FUndoBuffer, FUndoBufferLength);
  N46.Enabled := False;
  UndoB.Enabled := N46.Enabled;
end;

procedure TfrDesignerForm.ClearRedoBuffer;
begin
  ClearBuffer(FRedoBuffer, FRedoBufferLength);
  N48.Enabled := False;
  RedoB.Enabled := N48.Enabled;
end;

procedure TfrDesignerForm.Undo(Buffer: PfrUndoBuffer);
var
  p, p1: PfrUndoObj;
  r: PfrUndoRec1;
  BufferLength: Integer;
  List: TFpList;
  a: TfrUndoAction;
begin
  if Buffer = @FUndoBuffer then
    BufferLength := FUndoBufferLength
  else
    BufferLength := FRedoBufferLength;

  if (Buffer^[BufferLength - 1].Page <> CurPage) then Exit;
  List := TFpList.Create;
  a := Buffer^[BufferLength - 1].Action;
  p := Buffer^[BufferLength - 1].Objects;
  while p <> nil do
  begin
    GetMem(r, SizeOf(TfrUndoRec1));
    r^.ObjPtr := p^.ObjPtr;
    r^.Int := p^.Int;
    List.Add(r);
    case Buffer^[BufferLength - 1].Action of
      acInsert:
        begin
          r^.Int := Page.FindObjectByID(p^.ObjID);
          r^.ObjPtr := TfrView(Objects[r^.Int]);
          a := acDelete;
        end;
      acDelete: a := acInsert;
      acEdit:   r^.ObjPtr := TfrView(Objects[p^.Int]);
      acZOrder:
        begin
          r^.Int := Page.FindObjectByID(p^.ObjID);
          r^.ObjPtr := TfrView(Objects[r^.Int]);
          p^.ObjPtr := r^.ObjPtr;
        end;
    end;
    p := p^.Next;
  end;
  if Buffer = @FUndoBuffer then
    AddAction(@FRedoBuffer, a, List) else
    AddAction(@FUndoBuffer, a, List);
  List.Free;

  p := Buffer^[BufferLength - 1].Objects;
  while p <> nil do
  begin
    case Buffer^[BufferLength - 1].Action of
      acInsert: Page.Delete(Page.FindObjectByID(p^.ObjID));
      acDelete: Objects.Insert(p^.Int, p^.ObjPtr);
      acEdit:
        begin
          TfrView(Objects[p^.Int]).Assign(p^.ObjPtr);
          p^.ObjPtr.Free;
        end;
      acZOrder: Objects[p^.Int] := p^.ObjPtr;
    end;
    p1 := p;
    p := p^.Next;
    FreeMem(p1, SizeOf(TfrUndoObj));
  end;

  if Buffer = @FUndoBuffer then
    Dec(FUndoBufferLength)
  else
    Dec(FRedoBufferLength);

  ResetSelection;
  RedrawPage;
  N46.Enabled := FUndoBufferLength > 0;
  UndoB.Enabled := N46.Enabled;
  N48.Enabled := FRedoBufferLength > 0;
  RedoB.Enabled := N48.Enabled;
end;

procedure TfrDesignerForm.AddAction(Buffer: PfrUndoBuffer; a: TfrUndoAction; List: TFpList);
var
  i: Integer;
  p, p1: PfrUndoObj;
  r: PfrUndoRec1;
  t, t1: TfrView;
  BufferLength: Integer;
begin
  if Buffer = @FUndoBuffer then
    BufferLength := FUndoBufferLength
  else
    BufferLength := FRedoBufferLength;
  if BufferLength >= MaxUndoBuffer then
  begin
    ReleaseAction(Buffer^[0]);
    for i := 0 to MaxUndoBuffer - 2 do
      Buffer^[i] := Buffer^[i + 1];
    BufferLength := MaxUndoBuffer - 1;
  end;
  Buffer^[BufferLength].Action := a;
  Buffer^[BufferLength].Page := CurPage;
  Buffer^[BufferLength].Objects := nil;
  p := nil;
  for i := 0 to List.Count - 1 do
  begin
    r := List[i];
    t := r^.ObjPtr;
    GetMem(p1, SizeOf(TfrUndoObj));
    p1^.Next := nil;

    if Buffer^[BufferLength].Objects = nil then
      Buffer^[BufferLength].Objects := p1
    else
      p^.Next := p1;
      
    p := p1;
    case a of
      acInsert: p^.ObjID := t.ID;
      acDelete, acEdit:
        begin
          t1 := frCreateObject(t.Typ, t.ClassName);
          t1.Assign(t);
          t1.ID := t.ID;
          p^.ObjID := t.ID;
          p^.ObjPtr := t1;
          p^.Int := r^.Int;
        end;
      acZOrder:
        begin
          p^.ObjID := t.ID;
          p^.Int := r^.Int;
        end;
    end;
    FreeMem(r, SizeOf(TfrUndoRec1));
  end;
  if Buffer = @FUndoBuffer then
  begin
    FUndoBufferLength := BufferLength + 1;
    N46.Enabled := True;
    UndoB.Enabled := True;
  end
  else
  begin
    FRedoBufferLength := BufferLength + 1;
    N48.Enabled := True;
    RedoB.Enabled := True;
  end;
  Modified := True;
  FileModified := True;
end;

procedure TfrDesignerForm.AddUndoAction(a: TfrUndoAction);
var
  i: Integer;
  t: TfrView;
  List: TFpList;
  p: PfrUndoRec1;
begin
  ClearRedoBuffer;

  List := TFpList.Create;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected or (a = acZOrder) then
    begin
      GetMem(p, SizeOf(TfrUndoRec1));
      p^.ObjPtr := t;
      p^.Int := i;
      List.Add(p);
    end;
  end;
  AddAction(@FUndoBuffer, a, List);
  List.Free;
end;

procedure TfrDesignerForm.BeforeChange;
begin
  AddUndoAction(acEdit);
end;

procedure TfrDesignerForm.AfterChange;
begin
  PageView.DrawPage(dmSelection);
  PageView.Draw(TopSelected, 0);
  ObjInspRefresh;
end;

//Move selected object from front
procedure TfrDesignerForm.ZB1Click(Sender: TObject);   // go up
var
  i, j, n: Integer;
  t: TfrView;
begin
  AddUndoAction(acZOrder);
  n:=Objects.Count;
  i:=0;
  j:=0;
  while j < n do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
    begin
      Objects.Delete(i);
      Objects.Add(t);
    end
    else Inc(i);
    Inc(j);
  end;
  SendBandsToDown;
  RedrawPage;
end;

//Send selected object to back
procedure TfrDesignerForm.ZB2Click(Sender: TObject);    // go down
var
  t: TfrView;
  i, j, n: Integer;
begin
  AddUndoAction(acZOrder);
  n:=Objects.Count;
  j:=0;
  i:=n-1;
  while j < n do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
    begin
      Objects.Delete(i);
      Objects.Insert(0, t);
    end
    else Dec(i);
    Inc(j);
  end;
  SendBandsToDown;
  RedrawPage;
end;

procedure TfrDesignerForm.PgB1Click(Sender: TObject); // add page
begin
  ResetSelection;
  if Sender<>pgB4 then
     AddPage('TfrPageReport')
  else
     AddPage('TfrPageDialog');
end;

procedure TfrDesignerForm.PgB2Click(Sender: TObject); // remove page
begin
  if MessageDlg(sRemovePg,mtConfirmation,[mbYes,mbNo],0)=mrYes then
       RemovePage(CurPage);
end;

procedure TfrDesignerForm.OB1Click(Sender: TObject);
begin
  ObjRepeat := False;
end;

procedure TfrDesignerForm.OB2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ObjRepeat := ssShift in Shift;
  PageView.Cursor := crDefault;
end;

procedure TfrDesignerForm.CutBClick(Sender: TObject); //cut
begin
  AddUndoAction(acDelete);
  CutToClipboard;
  FirstSelected := nil;
  EnableControls;
  ShowPosition;
  RedrawPage;
end;

procedure TfrDesignerForm.CopyBClick(Sender: TObject); //copy
begin
  CopyToClipboard;
  EnableControls;
end;

procedure TfrDesignerForm.PstBClick(Sender: TObject); //paste
var
  i, minx, miny: Integer;
  t, t1: TfrView;
begin
  Unselect;
  SelNum := 0;
  minx := 32767; miny := 32767;
  with ClipBd do
  for i := 0 to Count-1 do
  begin
    t := TfrView(Items[i]);
    if t.x < minx then minx := t.x;
    if t.y < miny then miny := t.y;
  end;
  for i := 0 to ClipBd.Count - 1 do
  begin
    t := TfrView(ClipBd.Items[i]);
    if t.Typ = gtBand then
      if not (TfrBandView(t).BandType in [btMasterHeader..btSubDetailFooter,
                                          btGroupHeader, btGroupFooter]) and
        frCheckBand(TfrBandView(t).BandType) then
        continue;
    if PageView.Left < 0 then
      t.x := t.x - minx + ((-PageView.Left) div GridSize * GridSize) else
      t.x := t.x - minx;
    if PageView.Top < 0 then
      t.y := t.y - miny + ((-PageView.Top) div GridSize * GridSize) else
      t.y := t.y - miny;
    Inc(SelNum);
    t1 := frCreateObject(t.Typ, t.ClassName);
    t1.Assign(t);
    if CurReport.FindObject(t1.Name) <> nil then
      t1.CreateUniqueName;
    Objects.Add(t1);
  end;
  SelectionChanged;
  SendBandsToDown;
  PageView.GetMultipleSelected;
  RedrawPage;
  AddUndoAction(acInsert);
end;

procedure TfrDesignerForm.UndoBClick(Sender: TObject); // undo
begin
  Undo(@FUndoBuffer);
end;

procedure TfrDesignerForm.RedoBClick(Sender: TObject); // redo
begin
  Undo(@FRedoBuffer);
end;

procedure TfrDesignerForm.SelAllBClick(Sender: TObject); // select all
begin
  PageView.DrawPage(dmSelection);
  SelectAll;
  PageView.GetMultipleSelected;
  PageView.DrawPage(dmSelection);
  SelectionChanged;
end;

procedure TfrDesignerForm.ExitBClick(Sender: TObject);
begin
  {$IFDEF MODALDESIGNER}
  ModalResult := mrOk;
  {$ELSE}
  Close;
  {$ENDIF}
end;


procedure TfrDesignerForm.N5Click(Sender: TObject); // popup delete command
begin
  DeleteObjects;
end;

procedure TfrDesignerForm.N6Click(Sender: TObject); // popup edit command
begin
  ShowEditor;
end;

procedure TfrDesignerForm.FileBtn1Click(Sender: TObject); // create new
var
  w: Word;
begin
  if FileModified then
  begin
    w:=MessageDlg(sSaveChanges + ' ' + sTo + ' ' +
      ExtractFileName(CurDocName) + '?',mtConfirmation,
      [mbYes,mbNo,mbCancel],0);
      
    if w = mrCancel then Exit;
    if w = mrYes then
    begin
      FileBtn3Click(nil);
      if not WasOk then Exit;
    end;
  end;
  
  ClearUndoBuffer;
  CurReport.Pages.Clear;
  CurReport.Pages.Add;
  CurPage := 0;
  CurDocName := sUntitled;
  FileModified := False;
  FCurDocFileType := 3;
end;

procedure TfrDesignerForm.N23Click(Sender: TObject); // create new from template
begin
  frTemplForm := TfrTemplForm.Create(nil);
  with frTemplForm do
  if ShowModal = mrOk then
  begin
    ClearUndoBuffer;
    CurReport.LoadTemplate(TemplName, nil, nil, True);
    CurDocName := sUntitled;
    CurPage := 0; // do all
  end;
  frTemplForm.Free;
end;

procedure TfrDesignerForm.FileBtn2Click(Sender: TObject); // open
var
  w: Word;
begin
  w := mrNo;
  if FileModified then
    w:=MessageDlg(sSaveChanges + ' ' + sTo + ' ' +
          ExtractFileName(CurDocName) + '?',mtConfirmation,
          [mbYes,mbNo,mbCancel],0);
          
  if w=mrCancel then Exit;
  if w=mrYes then
  begin
    FileBtn3Click(nil);
    if not WasOk then Exit;
  end;

  with OpenDialog1 do
  begin
    Filter := sFormFile + ' (*.frf)|*.frf|' +
              sLazFormFile + ' (*.lrf)|*.lrf' +
              '';
    InitialDir:=ExtractFilePath(ParamStrUTF8(0));
    FileName := CurDocName;
    FilterIndex := 2;
    if Execute then
    begin
      ClearUndoBuffer;
      CurDocName := OpenDialog1.FileName;
      case FilterIndex of
        1: // fastreport form format
          begin
            CurReport.LoadFromFile(CurDocName);
            FCurDocFileType := dtFastReportForm;
          end;
        2: // lasreport form xml format
          begin
            CurReport.LoadFromXMLFile(CurDocName);
            FCurDocFileType := dtLazReportForm;
          end;
        else
          raise Exception.Create('Unrecognized file format');
      end;
      FileModified := False;
      CurPage := 0; // do all
    end;
  end;
end;

procedure TfrDesignerForm.N20Click(Sender: TObject); // save as
var
  s: String;
begin
  WasOk := False;
  with SaveDialog1 do
  begin
    Filter := sFormFile + ' (*.frf)|*.frf|' +
              sTemplFile + ' (*.frt)|*.frt|' +
              sLazFormFile + ' (*.lrf)|*.lrf' +
              '';
    InitialDir:=ExtractFilePath(ParamStrUTF8(0));
    FileName := CurDocName;
    FilterIndex := 3;
    if Execute then
       FCurDocFileType := FilterIndex;
      case FCurDocFileType of
        dtFastReportForm:
          begin
            s := ChangeFileExt(FileName, '.frf');
            CurReport.SaveToFile(s);
            CurDocName := s;
            WasOk := True;
          end;
        dtFastReportTemplate:
          begin
            s := ExtractFileName(ChangeFileExt(FileName, '.frt'));
            if frTemplateDir <> '' then
              s := frTemplateDir + PathDelim + s;
            frTemplNewForm := TfrTemplNewForm.Create(nil);
            with frTemplNewForm do
            if ShowModal = mrOk then
            begin
              CurReport.SaveTemplate(s, Memo1.Lines, Image1.Picture.Bitmap);
              WasOk := True;
            end;
            frTemplNewForm.Free;
          end;
        dtLazReportForm: // lasreport form xml format
          begin
            s := ChangeFileExt(FileName, '.lrf');
            CurReport.SaveToXMLFile(s);
            CurDocName := s;
            WasOk := True;
          end;
      end;
  end;
end;

procedure TfrDesignerForm.FileBtn3Click(Sender: TObject); // save
begin
  if CurDocName <> sUntitled then
  begin
    if FCurDocFileType=dtLazReportForm then
      CurReport.SaveToXMLFile(curDocName)
    else
      CurReport.SaveToFile(CurDocName);
    FileModified := False;
  end
  else
    N20Click(nil);
end;

procedure TfrDesignerForm.FileBtn4Click(Sender: TObject); // preview
var
  v1, v2: Boolean;
begin
  if CurReport is TfrCompositeReport then Exit;
  v1 := ObjInsp.Visible;
  v2 := CurReport.ModalPreview;
  ObjInsp.Visible:=False;
  Application.ProcessMessages;
  CurReport.ModalPreview := True;
  try
    CurReport.ShowReport;
  finally
    ObjInsp.Visible := v1;
    CurReport.ModalPreview := v2;
    SetFocus;
    DisableDrawing := False;
    CurPage := 0;
  end;
end;

procedure TfrDesignerForm.N42Click(Sender: TObject); // var editor
begin
  if ShowEvEditor(CurReport) then
  begin
    Modified := True;
    FileModified := True;
  end;
end;

procedure TfrDesignerForm.PgB3Click(Sender: TObject); // page setup
var
  w, h, p: Integer;
begin
  frPgoptForm := TfrPgoptForm.Create(nil);
  with frPgoptForm, Page do
  begin
    CB1.Checked := PrintToPrevPage;
    CB5.Checked := not UseMargins;
    if Orientation = poPortrait then
      RB1.Checked := True
    else
      RB2.Checked := True;
    ComB1.Items := Prn.PaperNames;
    ComB1.ItemIndex := Prn.GetArrayPos(pgSize);
    E1.Text := ''; E2.Text := '';

    if pgSize = $100 then
    begin
      E1.Text := IntToStr(Width div 10);
      E2.Text := IntToStr(Height div 10);
    end;
    
    E3.Text := IntToStr(Margins.Left * 5 div 18);
    E4.Text := IntToStr(Margins.Top * 5 div 18);
    E5.Text := IntToStr(Margins.Right * 5 div 18);
    E6.Text := IntToStr(Margins.Bottom * 5 div 18);
    E7.Text := IntToStr(ColGap * 5 div 18);
    ecolCount.Value := ColCount;
    if LayoutOrder = loColumns then
      RBColumns.Checked := true
    else
      RBRows.Checked := true;
    WasOk := False;
    if ShowModal = mrOk then
    begin
      Modified := True;
      FileModified := True;
      WasOk := True;
      PrintToPrevPage :=  CB1.Checked;
      UseMargins := not CB5.Checked;
      if RB1.Checked then
        Orientation := poPortrait
      else
        Orientation := poLandscape;
      if RBColumns.Checked then
        LayoutOrder := loColumns
      else
        LayoutOrder := loRows;
        
      p := Prn.PaperSizes[ComB1.ItemIndex];
      w := 0; h := 0;
      if p = $100 then
        try
          w := StrToInt(E1.Text) * 10;
          h := StrToInt(E2.Text) * 10;
        except
          on exception do p := 9; // A4
        end;
        
      try
        Margins.AsRect := Rect(StrToInt(E3.Text) * 18 div 5,
                          StrToInt(E4.Text) * 18 div 5,
                          StrToInt(E5.Text) * 18 div 5,
                          StrToInt(E6.Text) * 18 div 5);
        ColGap := StrToInt(E7.Text) * 18 div 5;
      except
        on exception do
        begin
          Margins.AsRect := Rect(0, 0, 0, 0);
          ColGap := 0;
        end;
      end;
      ColCount := ecolCount.Value;
      ChangePaper(p, w, h, Orientation);
      CurPage := CurPage; // for repaint and other
      UpdScrollbars;
    end;
  end;
  frPgoptForm.Free;
end;

procedure TfrDesignerForm.N8Click(Sender: TObject); // report setup
begin
  frDocOptForm := TfrDocOptForm.Create(nil);
  with frDocOptForm do
  begin
    CB1.Checked     := not CurReport.PrintToDefault;
    CB2.Checked     := CurReport.DoublePass;
    edTitle.Text    := CurReport.Title;
    edComments.Text := CurReport.Comments.Text;
    edKeyWords.Text := CurReport.KeyWords;
    edSubject.Text  := CurReport.Subject;
    edAutor.Text    := CurReport.ReportAutor;
    edtMaj.Text     := CurReport.ReportVersionMajor;
    edtMinor.Text   := CurReport.ReportVersionMinor;
    edtRelease.Text := CurReport.ReportVersionRelease;
    edtBuild.Text   := CurReport.ReportVersionBuild;
    if ShowModal = mrOk then
    begin
      CurReport.PrintToDefault := not CB1.Checked;
      CurReport.DoublePass := CB2.Checked;
      CurReport.ChangePrinter(Prn.PrinterIndex, ListBox1.ItemIndex);
      CurReport.Title:=edTitle.Text;
      CurReport.Subject:=edSubject.Text;
      CurReport.KeyWords:=edKeyWords.Text;
      CurReport.Comments.Text:=edComments.Text;
      CurReport.ReportVersionMajor:=edtMaj.Text;
      CurReport.ReportVersionMinor:=edtMinor.Text;
      CurReport.ReportVersionRelease:=edtRelease.Text;
      CurReport.ReportVersionBuild:=edtBuild.Text;
      CurReport.ReportAutor:=edAutor.Text;
      Modified := True;
      FileModified := True;
    end;
    CurPage := CurPage;
    Free;
  end;
end;

procedure TfrDesignerForm.N14Click(Sender: TObject); // grid menu
var
  DesOptionsForm: TfrDesOptionsForm;
begin
  DesOptionsForm := TfrDesOptionsForm.Create(nil);
  with DesOptionsForm do
  begin
    CB1.Checked := ShowGrid;
    CB2.Checked := GridAlign;
    case GridSize of
      4: RB1.Checked := True;
      8: RB2.Checked := True;
      18: RB3.Checked := True;
    end;
    if ShapeMode = smFrame then
      RB4.Checked := True
    else
      RB5.Checked := True;
      
    case Units of
      ruPixels: RB6.Checked := True;
      ruMM:     RB7.Checked := True;
      ruInches: RB8.Checked := True;
    end;
    
    CB3.Checked := not GrayedButtons;
    CB4.Checked := EditAfterInsert;
    CB5.Checked := ShowBandTitles;

    if ShowModal = mrOk then
    begin
      ShowGrid := CB1.Checked;
      GridAlign := CB2.Checked;
      if RB1.Checked then
        GridSize := 4
      else if RB2.Checked then
        GridSize := 8
      else
        GridSize := 18;
      if RB4.Checked then
        ShapeMode := smFrame
      else
        ShapeMode := smAll;
      if RB6.Checked then
        Units := ruPixels
      else if RB7.Checked then
        Units := ruMM
      else
        Units := ruInches;
      GrayedButtons := not CB3.Checked;
      EditAfterInsert := CB4.Checked;
      ShowBandTitles := CB5.Checked;
      RedrawPage;
    end;
    Free;
  end;
end;

procedure TfrDesignerForm.GB1Click(Sender: TObject);
begin
  ShowGrid := GB1.Down;
end;


procedure TfrDesignerForm.GB2Click(Sender: TObject);
begin
  GridAlign := GB2.Down;
end;

procedure TfrDesignerForm.GB3Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  AddUndoAction(acEdit);
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
    begin
      t.x := Round(t.x / GridSize) * GridSize;
      t.y := Round(t.y / GridSize) * GridSize;
      t.dx := Round(t.dx / GridSize) * GridSize;
      t.dy := Round(t.dy / GridSize) * GridSize;
      if t.dx = 0 then t.dx := GridSize;
      if t.dy = 0 then t.dy := GridSize;
    end;
  end;
  RedrawPage;
  ShowPosition;
  PageView.GetMultipleSelected;
end;

procedure TfrDesignerForm.Tab1Change(Sender: TObject);
begin
  if not fInBuildPage and (Tab1.TabIndex>=0) and (CurPage<>Tab1.TabIndex) then
    CurPage := Tab1.TabIndex;
end;

procedure TfrDesignerForm.Popup1Popup(Sender: TObject);
var
  i: Integer;
  t, t1: TfrView;
  fl: Boolean;
begin
  DeleteMenuItems(N2.Parent);
  EnableControls;

  while Popup1.Items.Count > 7 do
    Popup1.Items.Delete(7);
    
  if SelNum = 1 then
    TfrView(Objects[TopSelected]).DefinePopupMenu(Popup1)
  else
    if SelNum > 1 then
    begin
      t := TfrView(Objects[TopSelected]);
      fl := True;
      for i := 0 to Objects.Count - 1 do
      begin
        t1 := TfrView(Objects[i]);
        if t1.Selected then
          if not (((t is TfrMemoView) and (t1 is TfrMemoView)) or
             ((t.Typ <> gtAddIn) and (t.Typ = t1.Typ)) or
             ((t.Typ = gtAddIn) and (t.ClassName = t1.ClassName))) then
          begin
            fl := False;
            break;
          end;
      end;
      
      if fl and not (t.Typ = gtBand) then
        t.DefinePopupMenu(Popup1);
    end;

  FillMenuItems(N2.Parent);
  SetMenuItemBitmap(N2, CutB);
  SetMenuItemBitmap(N1, CopyB);
  SetMenuItemBitmap(N3, PstB);
  SetMenuItemBitmap(N16, SelAllB);
end;

procedure TfrDesignerForm.N37Click(Sender: TObject);
begin // toolbars
  Pan1.Checked := Panel1.IsVisible;
  Pan2.Checked := Panel2.IsVisible;
  Pan3.Checked := Panel3.IsVisible;
  Pan4.Checked := Panel4.IsVisible;
  Pan5.Checked := ObjInsp.Visible;
  Pan6.Checked := Panel5.Visible;
  Pan7.Checked := Panel6.Visible;
end;

procedure TfrDesignerForm.Pan2Click(Sender: TObject);

  procedure SetShow(c: Array of TWinControl; i: Integer; b: Boolean);
  begin
    if c[i] is TPanel then
    begin
      with c[i] as TPanel do
      begin
        Visible:=b;
        {if IsFloat then
          FloatWindow.Visible := b
        else
        begin
          if b then
            AddToDock(Parent as TPanel);
          Visible := b;
          (Parent as TPanel).AdjustBounds;
        end; }
      end;
    end
    else  TForm(c[i]).Visible:=b;
  end;

begin // each toolbar
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    SetShow([Panel1, Panel2, Panel3, Panel4, Panel5, ObjInsp, Panel6], Tag, Checked);
  end;
end;

procedure TfrDesignerForm.N34Click(Sender: TObject);
begin // about box
  frAboutForm := TfrAboutForm.Create(nil);
  frAboutForm.ShowModal;
  frAboutForm.Free;
end;

procedure TfrDesignerForm.InsFieldsClick(Sender: TObject);
begin
  frInsertFieldsForm := TfrInsertFieldsForm.Create(nil);
  frInsertFieldsForm.OnCloseQuery := @InsertFieldsFormCloseQuery;
  Try
    frInsertFieldsForm.ShowModal;
  finally
    frInsertFieldsForm.Free;
    frInsertFieldsForm:=nil;
  end;
end;

procedure TfrDesignerForm.Tab1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  GetCursorPos(p);
  
  if Button = mbRight then
    Popup2.PopUp(p.X,p.Y);
    
 //**
 {if Button = mbRight then
    TrackPopupMenu(Popup2.Handle,
      TPM_LEFTALIGN or TPM_RIGHTBUTTON, p.X, p.Y, 0, Handle, nil);
 }
end;

procedure TfrDesignerForm.frDesignerFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  ObjInsp.ShowHint := False;
end;

procedure TfrDesignerForm.frDesignerFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  Res:integer;
begin
  if FileModified and (CurReport<>nil) and
    (not ((csDesigning in CurReport.ComponentState) and CurReport.StoreInDFM)) then
  begin
    Res:=Application.MessageBox(PChar(sSaveChanges + ' ' + sTo + ' ' + ExtractFileName(CurDocName) + '?'),
      PChar(sConfirm), mb_IconQuestion + mb_YesNoCancel);
      
    case Res of
      mrNo:
        begin
          CanClose := True;
          FileModified := False; // no means don't want changes
          ModalResult := mrCancel;
        end;
      mrYes:
          begin
            FileBtn3Click(nil);
            CanClose := not FileModified;
          end;
    else
      CanClose := False;
    end;
  end;
end;

{----------------------------------------------------------------------------}
// state storing/retrieving
{const
  rsGridShow = 'GridShow';
  rsGridAlign = 'GridAlign';
  rsGridSize = 'GridSize';
  rsUnits = 'Units';
  rsButtons = 'GrayButtons';
  rsEdit = 'EditAfterInsert';
  rsSelection = 'Selection';

}
procedure TfrDesignerForm.SaveState;
{var
  Ini: TRegIniFile;
  Nm: String;

  procedure DoSaveToolbars(t: Array of TPanel);
  var
    i: Integer;
  begin
    for i := Low(t) to High(t) do
    begin
      if FirstInstance or (t[i] <> Panel6) then
        SaveToolbarPosition(t[i]);
      t[i].IsVisible := False;
    end;
  end;
}
begin
(*  Ini := TRegIniFile.Create(RegRootKey);
  Nm := rsForm + Name;
  Ini.WriteBool(Nm, rsGridShow, ShowGrid);
  Ini.WriteBool(Nm, rsGridAlign, GridAlign);
  Ini.WriteInteger(Nm, rsGridSize, GridSize);
  Ini.WriteInteger(Nm, rsUnits, Word(Units));
  Ini.WriteBool(Nm, rsButtons, GrayedButtons);
  Ini.WriteBool(Nm, rsEdit, EditAfterInsert);
  Ini.WriteInteger(Nm, rsSelection, Integer(ShapeMode));
  Ini.Free;
 //** DoSaveToolbars([Panel1, Panel2, Panel3, Panel4, Panel5, Panel6]);
  SaveFormPosition(InspForm);
*)
  //TODO: save ObjInsp position and size
  ObjInsp.Visible:=False;
end;

procedure TfrDesignerForm.RestoreState;
{var
  Ini: TRegIniFile;
  Nm: String;
  
//**  procedure DoRestoreToolbars(t: Array of TPanel);
  var
    i: Integer;
  begin
    for i := Low(t) to High(t) do
      RestoreToolbarPosition(t[i]);
  end;
}
begin
{  Ini := TRegIniFile.Create(RegRootKey);
  Nm := rsForm + Name;
  GridSize := Ini.ReadInteger(Nm, rsGridSize, 4);
  GridAlign := Ini.ReadBool(Nm, rsGridAlign, True);
  ShowGrid := Ini.ReadBool(Nm, rsGridShow, True);
  Units := TfrReportUnits(Ini.ReadInteger(Nm, rsUnits, 0));
  GrayedButtons := Ini.ReadBool(Nm, rsButtons, False);
  EditAfterInsert := Ini.ReadBool(Nm, rsEdit, True);
  ShapeMode := TfrShapeMode(Ini.ReadInteger(Nm, rsSelection, 1));
  Ini.Free;
//**  DoRestoreToolbars([Panel1, Panel2, Panel3, Panel4, Panel5, Panel6]);
  if Panel6.Height < 26 then
    Panel6.Height := 26;
  if Panel6.Width < 26 then
    Panel6.Width := 26;
  if Panel6.ControlCount < 2 then
    Panel6.Hide;
  frDock1.AdjustBounds;
  frDock2.AdjustBounds;
  frDock3.AdjustBounds;
  frDock4.AdjustBounds;
  RestoreFormPosition(InspForm);
}
  //TODO: restore ObjInsp position and size

  GridSize := 4;
  GridAlign := True;
  ShowGrid := False; //True;
  Units := TfrReportUnits(0);
  GrayedButtons := True; //False;
  EditAfterInsert := True;
  ShapeMode := TfrShapeMode(1);

  if Panel6.Height < 26 then
    Panel6.Height := 26;
  if Panel6.Width < 26 then
    Panel6.Width := 26;
  if Panel6.ControlCount < 2 then
    Panel6.Hide;
end;


{----------------------------------------------------------------------------}
// menu bitmaps
procedure TfrDesignerForm.SetMenuBitmaps;
var
  i: Integer;
begin
  MaxItemWidth := 0; MaxShortCutWidth := 0;

  FillMenuItems(FileMenu);
  FillMenuItems(EditMenu);
  FillMenuItems(ToolMenu);
  FillMenuItems(HelpMenu);

  SetMenuItemBitmap(N23, FileBtn1);
  SetMenuItemBitmap(N19, FileBtn2);
  SetMenuItemBitmap(N20, FileBtn3);
  SetMenuItemBitmap(N39, FileBtn4);
  SetMenuItemBitmap(N10, ExitB);

  SetMenuItemBitmap(N46, UndoB);
  SetMenuItemBitmap(N48, RedoB);
  SetMenuItemBitmap(N11, CutB);
  SetMenuItemBitmap(N12, CopyB);
  SetMenuItemBitmap(N13, PstB);
  SetMenuItemBitmap(N28, SelAllB);
  SetMenuItemBitmap(N29, PgB1);
  SetMenuItemBitmap(N30, PgB2);
  SetMenuItemBitmap(N32, ZB1);
  SetMenuItemBitmap(N33, ZB2);
  SetMenuItemBitmap(N35, HelpBtn);

  for i := 0 to  Panel6.ControlCount-1 - 1 do
  begin
    if Panel6.Controls[i] is TSpeedButton then
      SetMenuItemBitmap(MastMenu.Items[i], Panel6.Controls[i] as TSpeedButton);
  end;

  SetMenuItemBitmap(N41, PgB1);
  SetMenuItemBitmap(N43, PgB2);
  SetMenuItemBitmap(N44, PgB3);
end;

function TfrDesignerForm.FindMenuItem(AMenuItem: TMenuItem): TfrMenuItemInfo;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to MenuItems.Count - 1 do
    if TfrMenuItemInfo(MenuItems[i]).MenuItem = AMenuItem then
    begin
      Result := TfrMenuItemInfo(MenuItems[i]);
      Exit;
    end;
end;

procedure TfrDesignerForm.SetMenuItemBitmap(AMenuItem: TMenuItem; ABtn: TSpeedButton);
var
  m: TfrMenuItemInfo;
begin
  m := FindMenuItem(AMenuItem);
  if m = nil then
  begin
    m := TfrMenuItemInfo.Create;
    m.MenuItem := AMenuItem;
    MenuItems.Add(m);
  end;
  m.Btn := ABtn;
//**
{  ModifyMenu(AMenuItem.Parent.Handle, AMenuItem.MenuIndex,
    MF_BYPOSITION + MF_OWNERDRAW, AMenuItem.Command, nil);
}
end;

procedure TfrDesignerForm.FillMenuItems(MenuItem: TMenuItem);
var
  i: Integer;
  m: TMenuItem;
begin
  for i := 0 to MenuItem.Count - 1 do
  begin
    m := MenuItem.Items[i];
    SetMenuItemBitmap(m, nil);
    if m.Count > 0 then FillMenuItems(m);
  end;
end;

procedure TfrDesignerForm.DeleteMenuItems(MenuItem: TMenuItem);
var
  i, j: Integer;
  m: TMenuItem;
begin
  for i := 0 to MenuItem.Count - 1 do
  begin
    m := MenuItem.Items[i];
    for j := 0 to MenuItems.Count - 1 do
    if TfrMenuItemInfo(MenuItems[j]).MenuItem = m then
    begin
      TfrMenuItemInfo(MenuItems[j]).Free;
      MenuItems.Delete(j);
      break;
    end;
  end;
end;

procedure TfrDesignerForm.DoDrawText(aCanvas: TCanvas; aCaption: string;
  Rect: TRect; Selected, aEnabled: Boolean; Flags: Longint);
begin
  with aCanvas do
  begin
    Brush.Style := bsClear;
    if not aEnabled then
    begin
      if not Selected then
      begin
        OffsetRect(Rect, 1, 1);
        Font.Color := clBtnHighlight;
        DrawText(Handle, PChar(Caption), Length(Caption), Rect, Flags);
        OffsetRect(Rect, -1, -1);
      end;
      Font.Color := clBtnShadow;
    end;
    DrawText(Handle, PChar(aCaption), Length(aCaption), Rect, Flags);
    
    Brush.Style := bsSolid;
  end;
end;

procedure TfrDesignerForm.DrawItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
  ARect: TRect; Selected: Boolean);
var
  GlyphRect: TRect;
  Btn: TSpeedButton;
  Glyph: TBitmap;
begin
  MaxItemWidth := 0;
  MaxShortCutWidth := 0;
  with ACanvas do
  begin
    if Selected then
    begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText
    end
    else
    begin
      Brush.Color := clMenu;
      Font.Color := clMenuText;
    end;
    if AMenuItem.Caption <> '-' then
    begin
      FillRect(ARect);
      Btn := FindMenuItem(AMenuItem).Btn;
      GlyphRect := Bounds(ARect.Left + 1, ARect.Top + (ARect.Bottom - ARect.Top - 16) div 2, 16, 16);

      if AMenuItem.Checked then
      begin
        Glyph := TBitmap.Create;
        if AMenuItem.RadioItem then
        begin
          // todo
          //** Glyph.Handle := LoadBitmap(hInstance, 'FR_RADIO');
          //BrushCopy(GlyphRect, Glyph, Rect(0, 0, 16, 16), Glyph.TransparentColor);
        end
        else
        begin
          //** Glyph.Handle := LoadBitmap(hInstance, 'FR_CHECK');
          Draw(GlyphRect.Left, GlyphRect.Top, Glyph);
        end;
        Glyph.Free;
      end
      else if Btn <> nil then
      begin
        Glyph := TBitmap.Create;
        Glyph.Width := 16; Glyph.Height := 16;
        // todo
        //** Btn.DrawGlyph(Glyph.Canvas, 0, 0, AMenuItem.Enabled);
        //BrushCopy(GlyphRect, Glyph, Rect(0, 0, 16, 16), Glyph.TransparentColor);
        Glyph.Free;
      end;
      ARect.Left := GlyphRect.Right + 4;
    end;

    if AMenuItem.Caption <> '-' then
    begin
      OffsetRect(ARect, 0, 2);
      DoDrawText(ACanvas, AMenuItem.Caption, ARect, Selected, AMenuItem.Enabled, DT_LEFT);
      if AMenuItem.ShortCut <> 0 then
      begin
        ARect.Left := StrToInt(ItemWidths.Values[AMenuItem.Parent.Name]) + 6;
        DoDrawText(ACanvas, ShortCutToText(AMenuItem.ShortCut), ARect,
          Selected, AMenuItem.Enabled, DT_LEFT);
      end;
    end
    else
    begin
      Inc(ARect.Top, 4);
      DrawEdge(Handle, ARect, EDGE_ETCHED, BF_TOP);
    end;
  end;
end;

procedure TfrDesignerForm.MeasureItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
  var AWidth, AHeight: Integer);
var
  w: Integer;
begin
  w := ACanvas.TextWidth(AMenuItem.Caption) + 31;
  if MaxItemWidth < w then
    MaxItemWidth := w;
  ItemWidths.Values[AMenuItem.Parent.Name] := IntToStr(MaxItemWidth);

  if AMenuItem.ShortCut <> 0 then
  begin
    w := ACanvas.TextWidth(ShortCutToText(AMenuItem.ShortCut)) + 15;
    if MaxShortCutWidth < w then
      MaxShortCutWidth := w;
  end;

  if frGetWindowsVersion = '98' then
    AWidth := MaxItemWidth
  else
    AWidth := MaxItemWidth + MaxShortCutWidth;
  if AMenuItem.Caption <> '-' then
    AHeight := 19 else
    AHeight := 10;
end;

procedure TfrDesignerForm.WndProc(var Message: TLMessage);
var
  MenuItem: TMenuItem;
  CCanvas: TCanvas;

  function FindItem(ItemId: Integer): TMenuItem;
  begin
    Result := MainMenu1.FindItem(ItemID, fkCommand);
    if Result = nil then
      Result := Popup1.FindItem(ItemID, fkCommand);
    if Result = nil then
      Result := Popup2.FindItem(ItemID, fkCommand);
  end;

begin
  case Message.Msg of
    LM_COMMAND:
      if Popup1.DispatchCommand(Message.wParam) or
         Popup2.DispatchCommand(Message.wParam) then Exit;
//**
{    LM_INITMENUPOPUP:
      with TWMInitMenuPopup(Message) do
        if Popup1.DispatchPopup(MenuPopup) or
           Popup2.DispatchPopup(MenuPopup) then Exit;
}
(*
    LM_DRAWITEM:
      with PDrawItemStruct(Message.LParam)^ do
      begin
        if (CtlType = ODT_MENU) and (Message.WParam = 0) then
        begin
          MenuItem := FindItem(ItemId);
          if MenuItem <> nil then
          begin
            CCanvas := TControlCanvas.Create;
            with CCanvas do
            begin
              Handle := _hDC;
              DrawItem(MenuItem, CCanvas, rcItem, ItemState{//** and ODS_SELECTED} <> 0);
              Free;
            end;
            Exit;
          end;
        end;
      end;
    LM_MEASUREITEM:
      with PMeasureItemStruct(Message.LParam)^ do
      begin
        if (CtlType = ODT_MENU) and (Message.WParam = 0) then
        begin
          MenuItem := FindItem(ItemId);
          if MenuItem <> nil then
          begin
            MeasureItem(MenuItem, Canvas, Integer(ItemWidth), Integer(ItemHeight));
            Exit;
          end;
        end;
      end;
*)
  end;
  inherited WndProc(Message);
end;


{----------------------------------------------------------------------------}
// alignment palette
function GetFirstSelected: TfrView;
begin
  if FirstSelected <> nil then
    Result := FirstSelected
  else
    Result :=TfrView(Objects[TopSelected]);
end;

function GetLeftObject: Integer;
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  t := TfrView(Objects[TopSelected]);
  x := t.x;
  Result := TopSelected;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      if t.x < x then
      begin
        x := t.x;
        Result := i;
      end;
  end;
end;

function GetRightObject: Integer;
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  t :=TfrView(Objects[TopSelected]);
  x := t.x + t.dx;
  Result := TopSelected;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      if t.x + t.dx > x then
      begin
        x := t.x + t.dx;
        Result := i;
      end;
  end;
end;

function GetTopObject: Integer;
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  t := TfrView(Objects[TopSelected]);
  y := t.y;
  Result := TopSelected;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      if t.y < y then
      begin
        y := t.y;
        Result := i;
      end;
  end;
end;

function GetBottomObject: Integer;
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  t := TfrView(Objects[TopSelected]);
  y := t.y + t.dy;
  Result := TopSelected;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      if t.y + t.dy > y then
      begin
        y := t.y + t.dy;
        Result := i;
      end;
  end;
end;

procedure TfrDesignerForm.Align1Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  x := t.x;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      t.x := x;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align6Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  y := t.y;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      t.y := y;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align5Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  x := t.x+t.dx;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      t.x := x - t.dx;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align10Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  y := t.y + t.dy;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      t.y := y - t.dy;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align2Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  x := t.x + t.dx div 2;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      t.x := x - t.dx div 2;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align7Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  if SelNum < 2 then Exit;
  BeforeChange;
  t := GetFirstSelected;
  y := t.y + t.dy div 2;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
      t.y := y - t.dy div 2;
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align3Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  x: Integer;
begin
  if SelNum = 0 then Exit;
  BeforeChange;
  t := TfrView(Objects[GetLeftObject]);
  x := t.x;
  t := TfrView(Objects[GetRightObject]);
  x := x + (t.x + t.dx - x - Page.PrnInfo.Pgw) div 2;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then Dec(t.x, x);
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align8Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
  y: Integer;
begin
  if SelNum = 0 then Exit;
  BeforeChange;
  t := TfrView(Objects[GetTopObject]);
  y := t.y;
  t := TfrView(Objects[GetBottomObject]);
  y := y + (t.y + t.dy - y - Page.PrnInfo.Pgh) div 2;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then Dec(t.y, y);
  end;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align4Click(Sender: TObject);
var
  s: TStringList;
  i, dx: Integer;
  t: TfrView;
begin
  if SelNum < 3 then Exit;
  BeforeChange;
  s := TStringList.Create;
  s.Sorted := True;
  s.Duplicates := dupAccept;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then s.AddObject(Format('%4.4d', [t.x]), t);
  end;
  dx := (TfrView(s.Objects[s.Count - 1]).x - TfrView(s.Objects[0]).x) div (s.Count - 1);
  for i := 1 to s.Count - 2 do
    TfrView(s.Objects[i]).x := TfrView(s.Objects[i-1]).x + dx;
  s.Free;
  PageView.GetMultipleSelected;
  RedrawPage;
end;

procedure TfrDesignerForm.Align9Click(Sender: TObject);
var
  s: TStringList;
  i, dy: Integer;
  t: TfrView;
begin
  if SelNum < 3 then Exit;
  BeforeChange;
  s := TStringList.Create;
  s.Sorted := True;
  s.Duplicates := dupAccept;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then s.AddObject(Format('%4.4d', [t.y]), t);
  end;
  dy := (TfrView(s.Objects[s.Count - 1]).y - TfrView(s.Objects[0]).y) div (s.Count - 1);
  for i := 1 to s.Count - 2 do
    TfrView(s.Objects[i]).y := TfrView(s.Objects[i - 1]).y + dy;
  s.Free;
  PageView.GetMultipleSelected;
  RedrawPage;
end;


{----------------------------------------------------------------------------}
// miscellaneous
function Objects: TFpList;
begin
  Result := frDesigner.Page.Objects;
end;

procedure frSetGlyph(aColor: TColor; sb: TSpeedButton; n: Integer);
var
  b : TBitmap;
  s : TMemoryStream;
  r : TRect;
  t : TfrView;
  i : Integer;
begin
  {$IFDEF DebugLR}
  DebugLn('frSetGlyph(',colortostring(acolor),',',sb.name,',',IntToStr(n),')');
  DebugLn('ColorLocked=', dbgs(ColorLocked),' sb.tag=', dbgs(sb.tag));
  {$ENDIF}
  B:=sb.Glyph;
  b.Width := 32;
  b.Height:= 16;
  with b.Canvas do
  begin
    Brush.Color:=clWhite;
    FillRect(ClipRect);
    r := Rect(n * 32, 0, n * 32 + 32, 16);
    CopyRect(Rect(0, 0, 32, 16),
       TfrDesignerForm(frDesigner).Image1.Picture.Bitmap.Canvas, r);
    // JRA: workaround for copyrect not using transparency
    //      and bitmap using transparency only on reading stream
    S := TMemorystream.Create;
    B.SaveToStream(S);
    S.Position:=0;
    B.Transparent := True;
    B.LoadFromStream(S);
    S.Free;
    
    if aColor = clNone then
    begin
       Brush.Color:=clBtnFace;
       Pen.Color  :=clBtnFace;
    end
    else
    begin
       Brush.Color:=aColor;
       Pen.Color:=aColor;
    end;
    Rectangle(Rect(0,12,15,15));
  end;

  i:=TopSelected;
  if (i>-1) and not ColorLocked then
  begin
    t := TfrView(Objects[i]);
    {$IFDEF DebugLR}
    DebugLn('frSetGlyph: TopSelected=', t.Name);
    {$ENDIF}

    Case Sb.Tag of
      5 : t.FillColor:=aColor; {ClB1}
     17 : if (t is TfrMemoView) then {ClB2}
               TfrMemoView(t).Font.Color:=aColor;
     19 : t.FrameColor:=aColor; {ClB3}
    end;
  end;
end;

function TopSelected: Integer;
var
  i: Integer;
begin
  Result := Objects.Count - 1;
  for i := Objects.Count - 1 downto 0 do
    if TfrView(Objects[i]).Selected then
    begin
      Result := i;
      break;
    end;
end;

function frCheckBand(b: TfrBandType): Boolean;
var
  i: Integer;
  t: TfrView;
begin
  Result := False;
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Typ = gtBand then
      if b = TfrBandView(t).BandType then
      begin
        Result := True;
        break;
      end;
  end;
end;

function GetUnusedBand: TfrBandType;
var
  b: TfrBandType;
begin
  Result := btNone;
  for b := btReportTitle to btNone do
    if not frCheckBand(b) then
    begin
      Result := b;
      break;
    end;
  if Result = btNone then Result := btMasterData;
end;

procedure SendBandsToDown;
var
  i, j, n, k: Integer;
  t: TfrView;
begin
  n := Objects.Count; j := 0; i := n - 1;
  k := 0;
  while j < n do
  begin
    t := TfrView(Objects[i]);
    if t.Typ = gtBand then
    begin
      Objects.Delete(i);
      Objects.Insert(0, t);
      Inc(k);
    end
    else Dec(i);
    Inc(j);
  end;
  for i := 0 to n - 1 do // sends btOverlay to back
  begin
    t := TfrView(Objects[i]);
    if (t.Typ = gtBand) and (TfrBandView(t).BandType = btOverlay) then
    begin
      Objects.Delete(i);
      Objects.Insert(0, t);
      break;
    end;
  end;
  i := 0; j := 0;
  while j < n do // sends btCrossXXX to front
  begin
    t := TfrView(Objects[i]);
    if (t.Typ = gtBand) and
       (TfrBandView(t).BandType in [btCrossHeader..btCrossFooter]) then
    begin
      Objects.Delete(i);
      Objects.Insert(k - 1, t);
    end
    else Inc(i);
    Inc(j);
  end;
end;

procedure ClearClipBoard;
var
  t: TfrView;
begin
  if Assigned(ClipBd) then
    with ClipBd do
    while Count > 0 do
    begin
      t := TfrView(Items[0]);
      t.Free;
      Delete(0);
    end;
end;

procedure GetRegion;
var
  i: Integer;
  t: TfrView;
  R,R1: HRGN;
begin
  ClipRgn := CreateRectRgn(0, 0, 0, 0);
  for i := 0 to Objects.Count - 1 do
  begin
    t := TfrView(Objects[i]);
    if t.Selected then
    begin
      R := t.GetClipRgn(rtExtended);
      R1:=CreateRectRgn(0, 0, 0, 0);
      CombineRgn(ClipRgn, R1, R, RGN_OR);
      DeleteObject(R);
      DeleteObject(R1);
    end;
  end;
  FirstChange := False;
end;

procedure TfrDesignerForm.GetDefaultSize(var dx, dy: Integer);
begin
  dx := 96;
  if GridSize = 18 then dx := 18 * 6;
  dy := 18;
  if GridSize = 18 then dy := 18;
  if LastFontSize in [12, 13] then dy := 20;
  if LastFontSize in [14..16] then dy := 24;
end;


procedure TfrDesignerForm.SB1Click(Sender: TObject);
var
  d: Double;
begin
  d := StrToFloat(E1.Text);
  d := d + 1;
  E1.Text := FloatToStrF(d, ffGeneral, 2, 2);
  DoClick(E1);
end;

procedure TfrDesignerForm.SB2Click(Sender: TObject);
var
  d: Double;
begin
  d := StrToFloat(E1.Text);
  d := d - 1;
  if d <= 0 then d := 1;
  E1.Text := FloatToStrF(d, ffGeneral, 2, 2);
  DoClick(E1);
end;

{type
  THackBtn = class(TSpeedButton);
}

procedure TfrDesignerForm.HelpBtnClick(Sender: TObject);
begin
  HelpBtn.Down := True;
  Screen.Cursor := crHelp;
  SetCaptureControl(Self);
  //** THackBtn(HelpBtn).FMouseInControl := False;
  HelpBtn.Invalidate;
end;

procedure TfrDesignerForm.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  c: TControl;
  t: Integer;
begin
  if HelpBtn.Down and (GetCaptureControl=Self) then
    SetCaptureControl(nil);
  HelpBtn.Down := False;
  Screen.Cursor := crDefault;
  c := FindControlAtPosition(Mouse.CursorPos, true);
  if (c <> nil) and (c <> HelpBtn) then
  begin
    t := c.Tag;
    if (c.Parent = Panel4) and (t > 4) then
      t := 5;
    if c.Parent = Panel4 then
      Inc(t, 430) else
      Inc(t, 400);
    //DebugLn('TODO: HelpContext for tag=%d',[t]);
    //** Application.HelpCommand(HELP_CONTEXTPOPUP, t);
  end;
end;

procedure TfrDesignerForm.N22Click(Sender: TObject);
begin
  //** Application.HelpCommand(HELP_FINDER, 0);
end;

procedure TfrDesignerForm.OnActivateApp(Sender: TObject);

  procedure SetWinZOrder(Form: TForm);
  begin
    SetWindowPos(Form.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE);
  end;
begin
//  SetWinZOrder(InspForm);
{//**
  if Panel1.IsFloat then SetWinZOrder(Panel1.FloatWindow);
  if Panel2.IsFloat then SetWinZOrder(Panel2.FloatWindow);
  if Panel3.IsFloat then SetWinZOrder(Panel3.FloatWindow);
  if Panel4.IsFloat then SetWinZOrder(Panel4.FloatWindow);
  if Panel5.IsFloat then SetWinZOrder(Panel5.FloatWindow);
  if Panel6.IsFloat then SetWinZOrder(Panel6.FloatWindow);
}
end;

procedure TfrDesignerForm.OnDeactivateApp(Sender: TObject);

  procedure SetWinZOrder(Form: TForm);
  begin
    SetWindowPos(Form.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE);
  end;
  
begin
  if not Visible then Exit;
//  SetWinZOrder(InspForm);
{//**
  if Panel1.IsFloat then SetWinZOrder(Panel1.FloatWindow);
  if Panel2.IsFloat then SetWinZOrder(Panel2.FloatWindow);
  if Panel3.IsFloat then SetWinZOrder(Panel3.FloatWindow);
  if Panel4.IsFloat then SetWinZOrder(Panel4.FloatWindow);
  if Panel5.IsFloat then SetWinZOrder(Panel5.FloatWindow);
  if Panel6.IsFloat then SetWinZOrder(Panel6.FloatWindow);
}
end;

Procedure InitGlobalDesigner;
begin
  if Assigned(frDesigner) then
    Exit;
  frDesigner := TfrDesignerForm.Create(nil);
end;

{ TfrPanelObjectInspector }

{$IFNDEF EXTOI}
procedure TfrObjectInspector.BtnClick(Sender: TObject);
begin
  if Sender=fBtn then
  begin
    if fBtn.Caption='-' then
    begin
      fLastHeight:=Height;
      Height:=fPanelHeader.Height + 2*BorderWidth + 3;
      fBtn.Caption:='+';
    end
    else
    begin
      Height:=fLastHeight;
      fBtn.Caption:='-';
    end;
  end
  else Visible:=False;
end;

procedure TfrObjectInspector.HeaderMDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
  begin
    fDown:=True;
    if (x>4) and (x<fPanelHeader.Width-4) and (y<=16) then
    begin
      {$IFDEF DebugLR}
      debugLn('TfrObjectInspector.HeaderMDown()');
      {$ENDIF}
      fPanelHeader.Cursor:=crSize;
      // get absolute mouse position (X,Y can not be used, because they
      // are relative to what is moving)
      fPt:=Mouse.CursorPos;
      //DebugLn(['TfrObjectInspector.HeaderMDown ',dbgs(fPt)]);
    end;
  end;
end;

procedure TfrObjectInspector.HeaderMMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewPt: TPoint;
begin
  if fDown then
  begin
    {$IFDEF DebugLR}
    debugLn('TfrObjectInspector.HeaderMMove()');
    {$ENDIF}

    Case fPanelHeader.Cursor of
      crSize :
        begin
          NewPt:=Mouse.CursorPos;
          //DebugLn(['TfrObjectInspector.HeaderMDown ',dbgs(fPt),' New=',dbgs(NewPt)]);
          SetBounds(Left+NewPt.X-fPt.X,Top+NewPt.Y-fPt.Y,Width,Height);
          fPt:=NewPt;
        end;
    end;
  end
end;

procedure TfrObjectInspector.HeaderMUp(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF DebugLR}
  DebugLn('TfrObjectInspector.HeaderMUp()');
  {$ENDIF}
  fDown:=False;
  fPanelHeader.Cursor:=crDefault;
end;

{$ENDIF}

procedure TfrObjectInspector.CMVisibleChanged(var TheMessage: TLMessage);
begin
  Inherited CMVisibleChanged(TheMessage);
  
  if Visible then
  begin
    DoOnResize;
    BringToFront;
    Select(Objects);
  end;
  {$IFDEF DebugLR}
  debugLn('TfrObjectInspector.CMVisibleChanged: ', BooLTOStr(Visible));
  {$ENDIF}
end;

{$IFDEF EXTOI}
procedure TfrObjectInspector.DoHide;
begin
  //TODO Uncheck Menue Item
end;
{$ENDIF}

constructor TfrObjectInspector.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  {$IFDEF EXTOI}
  Width  :=220;
  Height :=300;
  Top    :=Screen.Height div 2;
  Left   :=40;
  Visible     :=False;
  Caption := 'Object Inspector';
  FormStyle := fsStayOnTop;
  // create the ObjectInspector
  fPropertyGrid:=TCustomPropertiesGrid.Create(aOwner);
  with fPropertyGrid do
  begin
    Name  :='PropertyGrid';
    Parent:=Self;
    align := alclient;
    ShowHint:=false; //cause problems in windows
  end;

  {$ELSE}

  Parent :=TWinControl(aOwner);
  Width  :=220;
  Height :=300;
  Top    :=120;
  Left   :=40;
  Borderstyle :=bsNone;
  BevelInner  :=bvLowered;
  BevelOuter  :=bvRaised;
  BorderWidth :=1;
  Visible     :=False;

  fDown       :=False;

  fPanelHeader:=TPanel.Create(self);
  with fPanelHeader do
  begin
    Parent:=Self;
    Color :=clSilver;
    BorderStyle:=bsNone;
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    Caption:=sObjectInspector;
    AnchorSideLeft.Control := self;
    AnchorSideTop.Control := self;
    AnchorSideRight.Control := self;
    AnchorSideRight.Side := asrBottom;
    Anchors := [akTop, akLeft, akRight];
    Top := 0;
    Height := 18;
    OnMouseDown:=@HeaderMDown;
    OnMouseMove:=@HeaderMMove;
    OnMouseUp  :=@HeaderMUp;
  end;

  fBtn2:=TButton.Create(fPanelHeader);
  with fBtn2 do
  begin
    Parent:=fPanelHeader;
    AnchorSideTop.Control := fPanelHeader;
    AnchorSideRight.Control := fPanelHeader;
    AnchorSideRight.Side := asrBottom;
    AnchorSideBottom.Control := fPanelHeader;
    AnchorSideBottom.Side := asrBottom;
    Anchors := [akTop, akRight, akBottom];
    BorderSpacing.Around := 1;
    Width := fPanelHeader.Height - 2*BorderSpacing.Around;
    Caption:='x';
    TabStop:=False;
    OnClick:=@BtnClick;
  end;

  fBtn:=TButton.Create(fPanelHeader);
  with fBtn do
  begin
    Parent:=fPanelHeader;
    AnchorSideTop.Control := fPanelHeader;
    AnchorSideRight.Control := fBtn2;
    AnchorSideBottom.Control := fPanelHeader;
    AnchorSideBottom.Side := asrBottom;
    Anchors := [akTop, akRight, akBottom];
    BorderSpacing.Around := 1;
    Width := fPanelHeader.Height - 2*BorderSpacing.Around;
    Caption:='-';
    TabStop:=False;
    OnClick:=@BtnClick;
  end;


  fcboxObjList  := TComboBox.Create(Self);
  with fcboxObjList do
  begin
    Parent:=Self;
    AnchorSideLeft.Control := Self;
    AnchorSideTop.Control := fPanelHeader;
    AnchorSideTop.Side := asrBottom;
    AnchorSideRight.Control := self;
    AnchorSideRight.Side := asrBottom;
    Anchors := [akTop, akLeft, akRight];
    ShowHint := false; //cause problems in windows
    Onchange := @cboxObjListOnChanged;
  end;

  // create the ObjectInspector
  fPropertyGrid:=TCustomPropertiesGrid.Create(aOwner);
  with fPropertyGrid do
  begin
    Name  :='PropertyGrid';
    Parent:=Self;
    AnchorSideLeft.Control := Self;
    AnchorSideTop.Control := fcboxObjList;
    AnchorSideTop.Side := asrBottom;
    AnchorSideRight.Control := Self;
    AnchorSideRight.Side := asrBottom;
    AnchorSideBottom.Control := Self;
    AnchorSideBottom.Side := asrBottom;
    Anchors := [akTop, akLeft, akRight, akBottom];
    ShowHint:=false; //cause problems in windows
    fPropertyGrid.SaveOnChangeTIObject:=false;
    DefaultItemHeight := fcboxObjList.Height-3;
  end;
  {$ENDIF}
end;

destructor TfrObjectInspector.Destroy;
begin
  //fPropertyGrid.Free; // it's owned by OI form/Panel
  inherited Destroy;
end;

procedure TfrObjectInspector.Select(Obj: TObject);
var
  i      : Integer;
  NewSel : TPersistentSelectionList;
  frDsg: TfrDesignerForm;
begin
  if Objects.Count <> fcboxObjList.Items.Count then
  begin
    fcboxObjList.Clear;
    for i:=0 to Objects.Count-1 do
       fcboxObjList.AddItem(TfrView(Objects[i]).Name, TObject(Objects[i]));
  end;

  if (Obj=nil) or (Obj is TPersistent) then
  begin
    fPropertyGrid.TIObject := TPersistent(Obj);
    if Obj <> nil then
      fcboxObjList.ItemIndex := fcboxObjList.Items.IndexOfObject(Obj);
  end
  else
  if Obj is TFpList then
    with TFpList(Obj) do
    begin
      NewSel:=TPersistentSelectionList.Create;
      try
        for i:=0 to Count-1 do
          if TfrView(Items[i]).Selected then
            NewSel.Add(TfrView(Items[i]));
        fPropertyGrid.Selection:=NewSel;
      finally
        NewSel.Free;
      end;
    end;
end;

procedure TfrObjectInspector.cboxObjListOnChanged(Sender: TObject);
var
  i: Integer;
  vObj: TObject;
begin
  if fcboxObjList.ItemIndex >= 0 then
  begin
    SelNum := 0;
    for i := 0 to Objects.Count - 1 do
      TfrView(Objects[i]).Selected := False;
    vObj := fcboxObjList.Items.Objects[fcboxObjList.ItemIndex];
    if vObj is TfrView then
    begin
      TfrView(vObj).Selected:=True;
      SelNum := 1;
      frDesigner.Invalidate;
    end;
    Select(vObj);
  end;
end;

procedure TfrObjectInspector.SetModifiedEvent(AEvent: TNotifyEvent);
begin
  fPropertyGrid.OnModified:=AEvent;
end;

procedure TfrObjectInspector.Refresh;
begin
  if not visible then
    exit;
  fPropertyGrid.RefreshPropertyValues;
end;

initialization

  {$I fr_pencil.lrs}
  
  frDesigner:=nil;
  ProcedureInitDesigner:=@InitGlobalDesigner;
  
  ClipBd := TFpList.Create;
  GridBitmap := TBitmap.Create;
  with GridBitmap do
  begin
    Width := 8; Height := 8;
  end;
  LastFrames:=[];
  LastFrameWidth := 1;
  LastLineWidth := 2;
  LastFillColor := clNone;
  LastFrameColor := clBlack;
  LastFontColor := clBlack;
  LastFontStyle := 0;
  LastAdjust := 0;
  //** RegRootKey := 'Software\FastReport\' + Application.Title;

finalization
  If Assigned(frDesigner) then begin
    {$IFNDEF MODALDESIGNER}
    if frDesigner.Visible then
      frDesigner.Hide;
    {$ENDIF}
    frDesigner.Free;
  end;
  ClearClipBoard;
  ClipBd.Free;
  GridBitmap.Free;

end.

