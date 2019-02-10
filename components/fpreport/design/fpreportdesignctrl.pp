{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Customcontrol which designs a page of a report instance.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportdesignctrl;

{$mode objfpc}{$H+}
{ $DEFINE DEBUGRD}

interface

uses
  Classes, SysUtils, controls, fpreport, graphics, lmessages, fpreportlclexport, lcltype, menus,
  fpreportdesignobjectlist, fpreportdrawruler, graphutil, ClipBrd, types;

Const
  clGrid  = TColor($E0E0E0);      // Default color for guide grid
  clSelectRect = clNavy;          // Pen color to draw selection rectangle
  psSelectRect = psDashDot;       // Pen style to draw selection rectangle with

Type
  TSelectResult = (srNone,srSelected,srDeselected);
  { TFPReportDesignerControl }
  TDesignerOption = (doGuideGrid, doShowRuler, doSnapToGrid);
  TDesignerOptions = Set of TDesignerOption;

  TDesignerState = (dsNeutral,dsReset,
                     dsSelect,dsRectangleSelect,dsRectangleSelectExtend,
                     dsStartAddControl,dsStartAddControlMulti,
                     dsAddControl,dsAddControlMulti,
                     dsMoving, dsResizing);
  TOnElementCreatedEvent = Procedure (Sender : TObject; AElement : TFPReportElement) of object;

  TFPReportDesignerControl = class(TCustomControl)
  private
    FDesignerOptions: TDesignerOptions;
    FDesignerState: TDesignerState;
    FGuideGridColor: TColor;
    FHRuler: TDrawRuler;
    FHRulerHeight: Integer;
    FMinControlHeight: Integer;
    FMinControlWidth: Integer;
    FObjects: TReportObjectList;
    FOnElementCreated: TOnElementCreatedEvent;
    FOnPaste: TNotifyEvent;
    FOnReportChanged: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FPage: TFPReportCustomPage;
    FPageRect : TRect;
    FSnapResolution: integer;
    FVRuler: TDrawRuler;
    FVRulerWidth: Integer;
    FZoom: Single;
    FCanvasExport : TFPReportExportCanvas;
    FLastMouseDown : TPoint;
    FLastMouseUp : TPoint;
    FLastMouseMove : TPoint;
    FFocusRect : TRect;
    FAddClass: TFPReportElementClass;
    // Things to be drawn during paint
    FResizeDirection : TResizeHandlePosition;
    FSelectionOffset : TPoint;
    FClearFocusRect,
    FDrawFocusRect : Trect; // Focus rect that must be painted
    function CheckPainting(Msg: String): Boolean;
    function DoAddControl(ABand: TFPReportCustomBand;
      AElement: TFPReportElement; ARect: TRect; IsMulti: Boolean
      ): TReportObject;
    procedure DoDrawCurrentFocusRect(var ARect : TRect);
    procedure DoneAddControl(IsMulti: Boolean);
    procedure DoneMoveSelection;
    procedure DoneResizeSelection;
    procedure DoneSelectRectangle(Extend: Boolean);
    procedure DoPagesizeChange(Sender: TObject);
    procedure DoReportChanged(Sender: TObject);
    procedure ExtendAddRectangle;
    procedure ExtendSelectRectangle;
    function GetCurrentDPI: Integer;
    function GetTopLeft: TPoint;
    procedure MoveSelection;
    procedure ResizeSelection;
    procedure SetDesignerOptions(AValue: TDesignerOptions);
    procedure SetEvents(EnableEvents: boolean);
    procedure SetGuideGridColor(AValue: TColor);
    procedure SetHRulerHeight(AValue: Integer);
    procedure SetTopLeft(AValue: TPoint);
    procedure SetVRulerWidth(AValue: Integer);
    procedure SetZoom(AValue: Single);
    procedure StartAddingElement;
    procedure StartResize(AStartPos: TPoint; ADirection: TResizeHandlePosition);
    procedure StartSelection(ExtendSelection : Boolean);
    procedure StartRectangleSelection(ExtendSelection : Boolean);
  protected
    procedure SetCursorFromHandlePos(AHandlePos: TResizeHandlePosition); virtual;
    Procedure SetDesignerState(aState : TDesignerState);
    procedure DrawCurrentFocusRect(IsClear : Boolean);
    procedure ClearFocusRect(EmptyRect: Boolean);
    Procedure SetRulerParams;
    Function DoAddControl(ABand : TFPReportCustomBand; ARect : TRect; IsMulti : Boolean) : TFPReportElement; virtual;
    Procedure DoSelectionChanged(Sender : TObject);
    procedure SetCanvasExportCoordinates; virtual;
    function CreateExportCanvas: TFPReportExportCanvas; virtual;
    Function CreateObjects : TReportObjectList; virtual;
    procedure SetPage(AValue: TFPReportCustomPage); virtual;
    procedure DrawGuideGrid(ARect: TRect; Interval: Integer); virtual;
    // Keyboard events
    procedure KKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // Mouse events
    procedure DClick(Sender: TObject);
    procedure MDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure MMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CMMouseLeave(var {%H-}Message: TLMessage); message CM_MOUSELEAVE;
    // Drag & Drop events
    procedure DDDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DDDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    // Drawing
    procedure WMEraseBkgnd(var {%H-}Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure PaintBackGround; virtual;
    procedure PaintObjects(ObjectSelection : TObjectSelection = osAll);virtual;
    procedure PaintSelection;virtual;
    Procedure PaintRulers; virtual;
    procedure Paint; override;
    Procedure Paste; virtual;
    Property VRuler : TDrawRuler Read FVRuler;
    Property HRuler : TDrawRuler Read FHRuler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure UpdatePageParams; virtual;
    procedure Reset;
    procedure CancelOperation;
    Procedure CopySelectionToClipBoard;
    Class Procedure CheckClipBoardFormat;
    Function GetBandForPaste : TFPReportCustomBand;
    function ShowEditorForElement(aElement: TFPReportElement): Boolean;
    Function AddBand(ABandClass : TFPReportBandClass) : TFPReportCustomBand;
    Procedure AddElement(AElementClass : TFPReportElementClass; Multi : Boolean = False);
    Function SelectObjectsInRectangle(ARect : TRect; ExtendSelection : Boolean) : TSelectResult;
    function SelectObjectAt(P: TPoint; ExtendSelection : Boolean): TSelectResult;
    Function GetObjectAt(P : TPoint; Aoptions : TGetObjectOptions) : TReportObject;
    Property DesignerState : TDesignerState Read FDesignerState;
    Property Margins : TPoint Read GetTopLeft Write SetTopLeft;
  Published
    Property CurrentDPI : Integer Read GetCurrentDPI;
    Property DesignerOptions : TDesignerOptions Read FDesignerOptions Write SetDesignerOptions;
    Property GuideGridColor : TColor Read FGuideGridColor Write SetGuideGridColor;
    Property HorzRulerHeight : Integer Read FHRulerHeight Write SetHRulerHeight;
    Property MinControlHeight : Integer Read FMinControlHeight Write FMinControlHeight;
    Property MinControlWidth : Integer Read FMinControlWidth Write FMinControlWidth;
    Property Objects : TReportObjectList Read FObjects;
    Property Page : TFPReportCustomPage Read FPage Write SetPage;
    property SnapResolution: integer read FSnapResolution write FSnapResolution default 8;
    Property VertRulerWidth : Integer Read FVRulerWidth Write SetVRulerWidth;
    Property Zoom : Single Read FZoom Write SetZoom;
    Property OnElementCreated : TOnElementCreatedEvent Read FOnElementCreated Write FOnElementCreated;
    Property OnSelectionChanged : TNotifyEvent Read FOnSelectionChanged Write FOnSelectionChanged;
    Property OnReportChanged : TNotifyEvent Read FOnReportChanged Write FOnReportChanged;
    Property OnStateChange : TNotifyEvent Read FOnStateChange Write FOnStateChange;
    Property OnPaste : TNotifyEvent Read FOnPaste Write FOnPaste;
  end;

Const
  DefaultDesignerOptions  = [doGuideGrid,doShowRuler]; // Default for designer options

Var
  ClipBoardFormat : TClipboardFormat;

implementation

uses
  lclintf,
  forms;

Resourcestring
  SErrFailedToCopyToClipboard = 'Failed to copy selection to clipboard.';

const
  cMoveStepSmall = 1;
  cMoveStepLarge = 8;
  ReportClipBoardFormatName = 'text/fpReport.Elements';


{ ---------------------------------------------------------------------
  TFPReportDesignerControl
  ---------------------------------------------------------------------}


constructor TFPReportDesignerControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csNoFocus];
  FZoom:=1.0;
  FGuideGridColor:=clGrid;
  FDesignerOptions:=DefaultDesignerOptions;
  Parent:= AOwner as TWinControl;
  Color:=clWhite;
  FSnapResolution := 8;
  FMinControlHeight:=FSnapResolution*2;
  FMinControlWidth:=FSnapResolution*2;
  FCanvasExport:=CreateExportCanvas;
  FCanvasExport.Canvas:=Self.Canvas;
  FCanvasExport.HDPI:=CurrentDPI;
  FCanvasExport.VDPI:=CurrentDPI;
  FCanvasExport.DrawMode:=dmDesign;
  FObjects:=CreateObjects;
  FObjects.OnSelectionChange:=@DoSelectionChanged;
  FObjects.OnReportChange:=@DoReportChanged;
  FObjects.CanvasExport:=FCanvasExport;
  FHRuler:=TDrawRuler.Create(Canvas);
  FHRuler.RulerType:=rtTop;
  FVRuler:=TDrawRuler.Create(Canvas);
  FVRuler.RulerType:=rtLeft;
  HorzRulerHeight:=Canvas.TextHeight('W')*2;
  VertRulerWidth:=Canvas.TextHeight('W')*2;
  SetRulerParams;
  // Must be after FCanvasExport and Rulers were created.
  Margins:=Point(10,10);
  Reset;
  SetEvents(True);
end;

destructor TFPReportDesignerControl.destroy;
begin
  FreeAndNil(FObjects);
  FreeAndNil(FHRuler);
  FreeAndNil(FVRuler);
  FreeAndNil(FObjects);
  FreeAndNil(FCanvasExport);
  inherited destroy;
end;

procedure TFPReportDesignerControl.Reset;
begin
  SetDesignerState(dsReset);
  FObjects.ClearSelection;
  Invalidate;
  SetDesignerState(dsNeutral);
end;

function TFPReportDesignerControl.SelectObjectsInRectangle(ARect: TRect;
  ExtendSelection: Boolean): TSelectResult;

Var
  l : TFPList;
  I : Integer;
  O : TReportObject;
  R : TRect;
  
begin
  Result:=srNone;
  R:=NormalizeRect(ARect);
  L:=FObjects.GetObjectsInRect(R,[]);
  try
    Objects.BeginSelectionUpdate;
    if not ExtendSelection then
      begin
      Objects.ClearSelection;
      Result:=srDeselected;
      end;
    For I:=0 to L.Count-1 do
      begin
      O:=TReportObject(L[i]);
      if Not O.Selected then
        begin
        O.Selected:=True;
        Result:=srSelected;
        end;
      end;
  finally
    L.Free;
    Objects.EndSelectionUpdate;
  end;
end;

function TFPReportDesignerControl.GetObjectAt(P: TPoint; Aoptions : TGetObjectOptions): TReportObject;
begin
  Result:=Objects.GetObjectAt(P,AOPtions);
end;

procedure TFPReportDesignerControl.UpdatePageParams;

Var
  W,H : Integer;

begin
  // Top left is default set
  if FPage.Orientation=poPortrait then
    begin
    W:=mmToPixels(FPage.PageSize.Width,CurrentDPI);
    H:=mmToPixels(FPage.PageSize.Height,CurrentDPI);
    end
  else
    begin
    W:=mmToPixels(FPage.PageSize.Height,CurrentDPI);
    H:=mmToPixels(FPage.PageSize.Width,CurrentDPI);
    end;
  FPageRect.Right:=FPageRect.Left+W;
  FPageRect.Bottom:=FPageRect.Top+H;
  {$IFDEF DEBUGRD}  Writeln('Page width',FPage.Layout.Width,' at ',CurrentDPI,' : ',FPageRect.Right);{$ENDIF}
  {$IFDEF DEBUGRD}  Writeln('Page height',FPage.Layout.Height,' at ',CurrentDPI,' : ',FPageRect.Bottom);{$ENDIF}
  if not WidthIsAnchored then
    begin
    W:=FPageRect.Right;
    Inc(W,FPageRect.Left);
    if doShowRuler in DesignerOptions then
      Inc(W,VertRulerWidth);
    Width:=W;
    end;
  if not HeightIsAnchored then
    begin
    H:=FPageRect.Bottom;
    Inc(H,FPageRect.Top);
    if doShowRuler in DesignerOptions then
      Inc(H,HorzRulerHeight);
    height:=H;
    end;
  SetCanvasExportCoordinates;
  SetRulerParams;
end;

procedure TFPReportDesignerControl.SetPage(AValue: TFPReportCustomPage);


begin
  If AValue=FPage then exit;
  FPage:=AValue;
  FPage.OnPageSizeChange:=@DoPagesizeChange;
  UpdatePageParams;
  FObjects.LoadFromPage(AValue);
  FObjects.OrderBands(Canvas,CurrentDPI);
  Invalidate;
end;

procedure TFPReportDesignerControl.DrawGuideGrid(ARect : TRect; Interval : Integer);

Var
  L : Integer;

begin
  Canvas.Pen.Color:=GuideGridColor;
  Canvas.Pen.Style:=psSolid;
  L:=ARect.Top+Interval;
  While L<=ARect.Bottom do
    begin
    Canvas.Line(ARect.Left,L,ARect.Right,L);
    L:=L+Interval;
    end;
  L:=ARect.Left+Interval;
  While L<=ARect.Right do
    begin
    Canvas.Line(L,ARect.Top,L,ARect.Bottom);
    L:=L+Interval;
    end;
end;

function TFPReportDesignerControl.CheckPainting(Msg: String): Boolean;

begin
  Result:=IsProcessingPaintMsg ;
{$IFDEF DEBUGRD}
  if not result then
    Writeln(Msg,'!!!!!!!!!!!!!!! not inside paint message !!!!!!!!!!!');
{$ENDIF}
end;

procedure TFPReportDesignerControl.DDDragDrop(Sender, Source: TObject; X,
  Y: Integer);

Var
  O : TReportObject;
  ABand : TFPReportCustomBand;
  E : TFPReportElement;
  C : String;
  R : TRect;
  S : TSize;
  Opts : TMemoDragDropOptions;

begin
  Opts:=[];
  O:=FObjects.GetBandObjectAt(Point(X,Y),[goBandHandle]);
  if O=Nil then
    exit;
  ABand:=O.AsBand;
  if ABand=Nil then
    exit;
  if Source is TMemoDragDrop then
    begin
    E:=TFPReportMemo.Create(ABand.Report);
    C:=(Source as TMemoDragDrop).Content;
    TFPReportMemo(E).Text:=C;
    R:=Default(TRect);
    OffSetRect(R,X,Y);
    S:=Canvas.TextExtent(C);
    R.Width:=Round(S.Width*1.2);
    R.Height:=Round(S.Height*1.2);
    Opts:=TMemoDragDrop(Source).Options;
    end;
  DoAddControl(ABand,E,R,False);
  FObjects.SelectElement(E);
  if mddShowEditor in Opts then
    ShowEditorForElement(E)
end;

procedure TFPReportDesignerControl.DDDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=Source is TReportDragDrop;
end;

procedure TFPReportDesignerControl.PaintBackGround;

Var
  R : Trect;

begin
  if not CheckPainting('PaintBackground') then
     exit;
  {$IFDEF DEBUGRD}
  With FPageRect do
    Writeln('Paintbackground (',Left,',',Top,',',Right,',',Bottom,')');
  {$ENDIF}
  Canvas.Brush.Color:=clWhite;
  R:=FPageRect;
  if doShowRuler in DesignerOptions then
    OffSetRect(R,FVRulerWidth,FHRulerHeight);
  Canvas.FillRect(R);
  Canvas.Pen.Color:=clRed;
  Canvas.Pen.style:=psSolid;
  Canvas.Rectangle(R);
  {$IFDEF DEBUGRD}
  With FPage.Margins do
    Writeln('Paintbackground, Margins: (',Left,',',Top,',',Right,',',Bottom,')');
  {$ENDIF}
  if doGuideGrid in DesignerOptions then
    DrawGuideGrid(R,mmToPixels(10,CurrentDPI));
  R.Left:=FPageRect.Left+mmToPixels(FPage.Margins.Left,CurrentDPI);
  R.Top:=FPageRect.Top+mmToPixels(FPage.Margins.Top,CurrentDPI);
  if doShowRuler in DesignerOptions then
    begin
    R.Left:=R.Left+FVRulerWidth;
    R.Top:=R.Top+FHRulerHeight;
    end;
  R.Right:=R.Right-mmToPixels(FPage.Margins.Right,CurrentDPI);
  R.Bottom:=R.Bottom-mmToPixels(FPage.Margins.Bottom,CurrentDPI);
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Style:=psDash;
  Canvas.Brush.style:=bsClear;
  Canvas.Rectangle(R);
end;

procedure TFPReportDesignerControl.PaintObjects(
  ObjectSelection: TObjectSelection);

Var
  B : TFPReportCustomBand;
  O : TReportObject;
  I,J : Integer;

begin
  if not CheckPainting('PaintObjects') then
     exit;
  For I:=0 to Objects.Count-1 do
    if Objects[i].IsBand then
      begin
      B:=Objects[i].AsBand;
      if (ObjectSelection=osAll) or Objects.BandHasSelection(B,ObjectSelection) then
        begin
{$IFDEF DEBUGRD}Writeln('PaintObjects(',ObjectSelection,'): Band has selection ',B.ClassName,' : ',Objects.BandHasSelection(B,ObjectSelection));{$ENDIF}
        FCanvasExport.RenderElement(Nil,B);
        For J:=0 to Objects.Count-1 do
          begin
          O:=Objects.Objects[J];
{$IFDEF DEBUGRD}Writeln('PaintObjects(',ObjectSelection,'): Checking element for draw: ',O.Element.ClassName,' s:',O.Selected,' ps: ',O.PreviousSelected);{$ENDIF}
          if (B=O.Element.Parent) and O.MatchSelection(ObjectSelection) Then
            begin
{$IFDEF DEBUGRD}Writeln('PaintObjects(',ObjectSelection,'): Element selected for draw: ',O.Element.ClassName);{$ENDIF}
            FCanvasExport.RenderElement(B,O.Element);
            end;
          end;
        end;
      end;
end;

procedure TFPReportDesignerControl.PaintSelection;

begin
  if not CheckPainting('PaintSelection') then
    exit;
  if FObjects.HaveSelection then
    FObjects.DrawSelectionHandles(FSelectionOffset,FResizeDirection);
end;

procedure TFPReportDesignerControl.PaintRulers;
begin
  if not CheckPainting('PaintRulers') then
    exit;
  If Not (doShowRuler in DesignerOptions) then exit;
  FHRuler.PaintRuler;
  FVRuler.PaintRuler;
end;

procedure TFPReportDesignerControl.Paint;
begin
  {$IFDEF DEBUGRD}Writeln('Paint, cliprect: ',RectToStr(Canvas.ClipRect));{$ENDIF}
  PaintRulers;
  PaintBackground;
  PaintObjects;
  PaintSelection;
  DoDrawCurrentFocusRect(FClearFocusRect);
  DoDrawCurrentFocusRect(FDrawFocusRect);
end;

procedure TFPReportDesignerControl.Paste;
begin
  If Assigned(FOnPaste) then
    FOnPaste(Self);
end;

Class procedure TFPReportDesignerControl.CheckClipBoardFormat;
begin
  If ClipBoardFormat=0 then
    ClipBoardFormat:=RegisterClipboardFormat(ReportClipBoardFormatName);
end;

function TFPReportDesignerControl.GetBandForPaste: TFPReportCustomBand;

Var
  I : Integer;
  A : TReportObjectArray;
  O : TReportObject;
  P : TPoint;

begin
  Result:=nil;
  // First, check selection;
  A:=Objects.GetSelection;
  I:=0;
  While (Result=Nil) and (I<Length(A)) do
    begin
    if A[i].IsBand then
      Result:=A[i].AsBand;
    Inc(I);
    end;
  If Assigned(Result) then
    exit;
  // Then, check band under cursor position
  P:=ScreenToControl(Mouse.CursorPos);
  O:=Objects.GetBandObjectAt(P,[goBandHandle]);
  if Assigned(O) then
    Result:=O.AsBand;
  If Assigned(Result) then
    Exit;
  // Lastly, first band...
  if Page.BandCount>0 then
    Result:=Page.Bands[0];
end;

procedure TFPReportDesignerControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  //do nothing to avoid flicker
end;

function TFPReportDesignerControl.CreateExportCanvas: TFPReportExportCanvas;

begin
  Result:=TFPReportExportCanvas.Create(Self);
end;

function TFPReportDesignerControl.CreateObjects: TReportObjectList;
begin
  Result:=TReportObjectList.Create(TReportObject);
end;

function TFPReportDesignerControl.SelectObjectAt(P: TPoint;
  ExtendSelection: Boolean): TSelectResult;

Var
  O : TReportObject;

begin
  Result:=srNone;
  O:=GetObjectAt(P,[goBandHandle]);
  if O=Nil then Exit;

  if O.Selected then
    begin
    O.Selected:=False;
    Result:=srDeselected;
    end
  else
    begin
    Objects.BeginSelectionUpdate;
    try
      if not ExtendSelection then
        Objects.ClearSelection;
      O.Selected:=True;
      Result:=srSelected;
    finally
      Objects.EndSelectionUpdate;
    end;
    end;
end;

procedure TFPReportDesignerControl.StartSelection(ExtendSelection: Boolean);

begin
  if (FObjects.GetObjectAt(FLastMouseDown,[goBandHandle])<>Nil) then
    SetDesignerState(dsSelect);
end;

procedure TFPReportDesignerControl.StartRectangleSelection(
  ExtendSelection: Boolean);

Const
  rsStates : Array[Boolean] of TDesignerState = (dsRectangleSelect,dsRectangleSelectExtend);

begin
  SetDesignerState(rsStates[ExtendSelection]);
  FFocusRect.TopLeft:=FLastMouseDown;
  FFocusRect.BottomRight:=FLastMouseDown;
end;

procedure TFPReportDesignerControl.SetDesignerState(aState: TDesignerState);
begin
  FDesignerState:=AState;
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TFPReportDesignerControl.SetRulerParams;
begin
  FHRuler.DPI:=CurrentDPI; // Takes into account zoom.
  FVRuler.DPI:=CurrentDPI;
  FHRuler.BoundsRect:=Rect(FPageRect.Left+FVRulerWidth,0,FPageRect.Right+FVRulerWidth,HorzRulerHeight);
  FVRuler.BoundsRect:=Rect(0,FPageRect.Top+FHRulerHeight,VertRulerWidth,FPageRect.Bottom+FHRulerHeight);
end;

function TFPReportDesignerControl.DoAddControl(ABand: TFPReportCustomBand; AElement : TFPReportElement;  ARect: TRect; IsMulti: Boolean) : TReportObject;

  Function MinSize (aSize : Integer) : Integer;

  begin
    Result:=ASize; // Should be handled in calling routine, actually
  end;


Var
  ERect,BRect : TRect;
  RRect : TFPReportRect;
  W,H : Integer;

begin
  AElement.Parent:=ABand;
  BRect:=FCanvasExport.GetBandRect(ABand,False);
  ERect.Left:=ARect.Left-BRect.Left;
  ERect.Top:=ARect.Top-BRect.Top;
  ERect.Right:=ARect.Right-BRect.Left;
  ERect.Bottom:=ARect.Bottom-BRect.Top;
  W:=MinSize(ERect.Right-ERect.Left);
  H:=MinSize(ERect.Bottom-ERect.Top);
  RRect.SetRect(PixelsToMM(ERect.Left,CurrentDPI),
                PixelsToMM(ERect.Top,CurrentDPI),
                PixelsToMM(W,CurrentDPI),
                PixelsToMM(H,CurrentDPI));
{$IFDEF DEBUGRD}  Writeln('Adding,',AElement.ClassName,' at absolute rect:',RectToStr(ARect),', band rect: ',RectToStr(BRect),' -> Relative rect ',RectToStr(ERect),' natural units: ',RRect.AsString);{$ENDIF}
  AElement.Layout.SetPosition(RRect);
  Result:=FObjects.AddElement(AElement);
  Result.Selected:=True;
  If (Not IsMulti) and Assigned(FOnElementCreated) then
    FOnElementCreated(Self,AElement);
  Invalidate;
end;

function TFPReportDesignerControl.DoAddControl(ABand: TFPReportCustomBand;
  ARect: TRect; IsMulti: Boolean): TFPReportElement;

begin
  Result:=FAddClass.Create(Page.Report);
  Result.Parent:=ABand;
  DoAddControl(ABand,Result,ARect,isMulti);
end;


function TFPReportDesignerControl.GetCurrentDPI: Integer;
begin
  if Zoom<>1.0 then
    Result:=Round(Screen.PixelsPerInch * Zoom)
  else
    Result:=Screen.PixelsPerInch;
end;

function TFPReportDesignerControl.GetTopLeft: TPoint;
begin
  Result:=Point(FPageRect.Left,FPageRect.Top);
end;

procedure TFPReportDesignerControl.KKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

Type
  TSizeMove = (smNone,smSize,smMove);

var
  p: TPoint;
  SMAction : TSizeMove;
  lStep: integer;
  lDirection: TResizeHandlePosition;

begin
//  Writeln('Key down: ',Key,', Shifted: ',Shift<>[]);
  lDirection := rhNone;
  SMAction:=smMove;
  if (ssCtrl in Shift)  then
    SMAction:=smSize;
  p.x:=0;
  p.y:=0;
  if (ssShift in Shift) then
    lStep := cMoveStepLarge
  else
    lStep := cMoveStepSmall;
//  writeln('Sized= ', lSized, '   Moved=', lMoved);
  Case key of
  VK_RIGHT :
    begin
    p.x := lStep;
    lDirection := rhRight;
    end;
  VK_LEFT:
    begin
    p.x := -lStep;
    lDirection := rhRight;
    end;
  VK_UP:
    begin
    p.y := -lStep;
    lDirection := rhBottom;
    end;
  VK_DOWN:
    begin
    p.y := lStep;
    lDirection := rhBottom;
    end;
  else
    smAction:=smNone;
  end;
  if (SMAction<>smNone) then
    begin
    Case SMAction of
      smSize : FObjects.ResizeSelection(p, CurrentDPI, lDirection);
      smMove : FObjects.MoveSelection(p, CurrentDPI);
    end;
    FSelectionOffset.X := 0;
    FSelectionOffset.Y := 0;
    Invalidate;
    Key:=0;
    end;
end;

procedure TFPReportDesignerControl.KKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

Const
{$IFDEF DARWIN}
  CtrlKey = ssMeta;
{$ELSE}
  CtrlKey = ssCtrl;
{$ENDIF}


begin
  {$IFDEF DEBUGRD}    Writeln('Key up: ',Key,', Shifted: ',Shift<>[]);{$ENDIF}
  if (Key=VK_DELETE) then
    begin
    Key:=0;
    if FObjects.DeleteSelection = odrBand then
      FObjects.OrderBands(Self.Canvas,CurrentDPI);
    end
  else if (Key=VK_C) and (Shift=[Ctrlkey]) then
    begin
    Key:=0;
    CopySelectionToClipBoard;
    end;
end;

procedure TFPReportDesignerControl.SetDesignerOptions(AValue: TDesignerOptions);
begin
  if FDesignerOptions=AValue then Exit;
  FDesignerOptions:=AValue;
  Invalidate;
end;


procedure TFPReportDesignerControl.SetTopLeft(AValue: TPoint);

Var
  W,H : Integer;
  P : TPoint;

begin
  W:=FPageRect.Right-FPageRect.Left;
  H:=FPageRect.Bottom-FPageRect.Top;
  FPageRect.TopLeft:=Avalue;
  FPageRect.Right:=FPageRect.Left+W;
  FPageRect.Bottom:=FPageRect.Bottom+H;
  SetRulerParams;
  P:=AValue;
  if doShowRuler in DesignerOptions then
    begin
    P.X:=P.X+FVRulerWidth;
    P.Y:=P.Y+FHRulerHeight;
    end;
  FObjects.PageOffset:=P;
  SetCanvasExportCoordinates;
  Invalidate;
end;

procedure TFPReportDesignerControl.SetVRulerWidth(AValue: Integer);
begin
  if FVRulerWidth=AValue then Exit;
  FVRulerWidth:=AValue;
  SetRulerParams;
  SetCanvasExportCoordinates;
end;

procedure TFPReportDesignerControl.SetCanvasExportCoordinates;

Var
  D : Integer;

begin
  FCanvasExport.HDPI:=CurrentDPI;
  FCanvasExport.VDPI:=CurrentDPI;
  D:=FPageRect.Left;
  if Assigned(Page) then
    D:=D+mmToPixels(Page.Margins.Left,CurrentDPI);
  if doShowRuler in designerOptions then
    D:=D+VertRulerWidth;
  FCanvasExport.HorzOffset:=D;
  D:=FPageRect.Top;
  if Assigned(Page) then
    D:=D+mmToPixels(Page.Margins.Top,CurrentDPI);
  if doShowRuler in designerOptions then
    D:=D+HorzRulerHeight;
  FCanvasExport.VertOffset:=D;
end;

procedure TFPReportDesignerControl.SetZoom(AValue: Single);
begin
  if FZoom=AValue then Exit;
  FZoom:=AValue;
  SetCanvasExportCoordinates;
  Invalidate;
end;

procedure TFPReportDesignerControl.DoSelectionChanged(Sender: TObject);
begin
{$IFDEF DEBUGRD}Writeln('Selection changed');{$ENDIF}
  if not (DesignerState=dsReset) then
    begin
{$IFDEF DEBUGRD}Writeln('Selection changed. DrawFocusRect ',RectToStr(FDrawFocusRect),' clearfocusrect',RectToStr(FDrawFocusRect));{$ENDIF}
    Invalidate;
    end;
  if Assigned(OnSelectionChanged) then
    OnSelectionChanged(Self);
end;

procedure TFPReportDesignerControl.SetGuideGridColor(AValue: TColor);
begin
  if FGuideGridColor=AValue then Exit;
  FGuideGridColor:=AValue;
  Invalidate;
end;

procedure TFPReportDesignerControl.SetHRulerHeight(AValue: Integer);
begin
  if FHRulerHeight=AValue then Exit;
  FHRulerHeight:=AValue;
  SetRulerParams;
  SetCanvasExportCoordinates;
end;


{ ---------------------------------------------------------------------
  Mouse events
  ---------------------------------------------------------------------}

procedure TFPReportDesignerControl.SetEvents(EnableEvents: boolean);
begin
  if EnableEvents then
    begin
    OnMouseDown := @MDown;
    OnMouseUp   := @MUp;
    OnMouseMove := @MMove;
    OnDblClick  := @DClick;
    OnKeyUp     := @KKeyUp;
    OnKeyDown   := @KKeyDown;
    OnDragOver  := @DDDragOver;
    OnDragDrop:=@DDDragDrop;
    end
  else
    begin
    OnMouseDown := nil;
    OnMouseUp   := nil;
    OnMouseMove := nil;
    OnDblClick  := nil;
    OnKeyUp     := Nil;
    OnKeyDown   := Nil;
    OnDragOver  := Nil;
    end;
end;

procedure TFPReportDesignerControl.StartAddingElement;

begin
  if FObjects.GetBandObjectAt(FLastMouseDown,[])=Nil then
    Exit; // Cannot add to report page.
  FFocusRect.TopLeft:=FLastMouseDown;
  FFocusRect.BottomRight:=FLastMouseDown;
  If DesignerState=dsStartAddControlMulti then
    SetDesignerState(dsAddControlMulti)
  else If DesignerState=dsStartAddControl then
    SetDesignerState(dsAddControl);
end;

procedure TFPReportDesignerControl.StartResize(AStartPos : TPoint; ADirection : TResizeHandlePosition);

begin
  FResizeDirection:=aDirection;
  SetDesignerState(dsResizing);
end;

procedure TFPReportDesignerControl.MMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  FLastMouseMove:=Point(X,Y);
  Case DesignerState of
    dsMoving,
    dsSelect:
      MoveSelection;
    dsRectangleSelect,
    dsRectangleSelectExtend:
      ExtendSelectRectangle;
    dsAddControl,
    dsAddControlMulti:
      ExtendAddRectangle;
    dsResizing :
      ResizeSelection;
  else
    SetCursorFromHandlePos(FObjects.PointToResizeHandlePos(FLastMouseMove));
  end;
end;

procedure TFPReportDesignerControl.MDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

Var
  RH : TResizeHandlePosition;

begin
  if (Button<>mbLeft) then
    exit;
  SetFocus;
  FLastMouseDown:=Point(X,Y);
  Case DesignerState of
    dsNeutral :
      begin
      RH:=FObjects.PointToResizeHandlePos(FLastMouseDown);
      if (RH<>rhNone) then
        StartResize(FLastMouseDown,RH)
      else
        begin
        if (ssCtrl in Shift) then
          StartRectangleSelection(ssShift in  Shift)
        else
          StartSelection(ssShift in  Shift);
        end
      end;
    dsStartAddControl,
    dsStartAddControlMulti :
      StartAddingElement;
  end;
end;


procedure TFPReportDesignerControl.MUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

begin
  if (Button<>mbLeft) then
    Exit;
  FLastMouseUp:=Point(X,Y);
{$IFDEF DEBUGRD}  Writeln('Mouse up, desigerstate : ',DesignerState);{$ENDIF}
  Case DesignerState of
    dsSelect:
      begin
      if SelectObjectAt(FLastMouseDown,ssShift in Shift)<>srNone then
        SetDesignerState(dsNeutral);
      end;
    dsMoving :
      DoneMoveSelection;
    dsRectangleSelect,
    dsRectangleSelectExtend :
      DoneSelectRectangle(DesignerState=dsRectangleSelectExtend);
    dsAddControlMulti,
    dsAddControl:
      DoneAddControl(DesignerState=dsAddControlMulti);
    dsResizing :
      DoneResizeSelection;
  else
    // Do nothing
  end;
end;

procedure TFPReportDesignerControl.DoneAddControl(IsMulti : Boolean);

Var
  CR : TRect;
  O : TReportObject;

begin
  {$IFDEF DEBUGRD}  Writeln('DoneAddControl: ',isMulti);{$ENDIF}
  O:=Objects.GetBandObjectAt(FLastMouseDown,[]);
  if O=Nil then exit;
  CR:=NormalizeRect(FFocusRect);
  ClearFocusRect(True);
  if (CR.Right-CR.Left)<MinControlWidth then
    CR.Right:=CR.Left+MinControlWidth;
  if (CR.Bottom-CR.Top)<MinControlHeight then
    CR.Bottom:=CR.Top+MinControlHeight;
  DoAddControl(O.AsBand,CR,isMulti);
  if not isMulti then
    begin
    SetDesignerState(dsNeutral);
    Cursor:=crDefault;
    end;
end;

procedure TFPReportDesignerControl.DoneMoveSelection;

Var
  D : TPoint;

begin
  d.x:=FLastMouseUp.x-FLastMouseDown.x;
  d.y:=FLastMouseUp.y-FLastMouseDown.y;
  {$IFDEF DEBUGRD}  Writeln('Donemoveselection ',PointToStr(D));{$ENDIF}
  FObjects.MoveSelection(d,CurrentDPI);
  SetDesignerState(dsNeutral);
  FSelectionOffset.X:=0;
  FSelectionOffset.Y:=0;
  Invalidate;
end;

procedure TFPReportDesignerControl.DoneResizeSelection;
Var
  D : TPoint;

begin
  d.x:=FLastMouseUp.x-FLastMouseDown.x;
  d.y:=FLastMouseUp.y-FLastMouseDown.y;
  {$IFDEF DEBUGRD}  Writeln('Donemoveselection ',PointToStr(D));{$ENDIF}
  FObjects.ResizeSelection(d,CurrentDPI,FResizeDirection);
  SetDesignerState(dsNeutral);
  FSelectionOffset.X:=0;
  FSelectionOffset.Y:=0;
  FResizeDirection:=rhNone;
  Invalidate;
end;

procedure TFPReportDesignerControl.ClearFocusRect(EmptyRect : Boolean);
var
  r: TRect;
begin
  r := FFocusRect;
  {$IFDEF DEBUGRD}  Writeln('ClearFocusRect : ',EmptyRect,' : ',RectToStr(R));{$ENDIF}
  if not IsRectEmpty(r) then
    begin
    DrawCurrentFocusRect(True);
    if EmptyRect then
      begin
      FFocusRect.TopLeft.X:=-1;
      FFocusRect.TopLeft.Y:=-1;
      FFocusRect.BottomRight.X:=-1;
      FFocusRect.BottomRight.Y:=-1;
      end;
    end;
end;

procedure TFPReportDesignerControl.DoneSelectRectangle(Extend : Boolean);

Var
  R : TRect;

begin
  R:=NormalizeRect(FFocusRect);
  ClearFocusRect(True);
  if SelectObjectsInRectangle(R,Extend)<>srNone then
    Invalidate;
  SetDesignerState(dsNeutral);
end;

procedure TFPReportDesignerControl.DoPagesizeChange(Sender: TObject);
begin
  UpdatePageParams;
  Invalidate;
end;

procedure TFPReportDesignerControl.DoReportChanged(Sender: TObject);
begin
  Invalidate;
  If Assigned(OnReportChanged) then
    OnReportChanged(Sender);
end;

procedure TFPReportDesignerControl.DrawCurrentFocusRect(IsClear : Boolean);
{
  This method does not actually draw anything.
  It prepares everything for the paint message:
  it copies the data to drawfocusrect and invalidates the region.
}
Var
  P : PRect;
begin
  if IsClear then
    P:=@FClearFocusRect
  else
    P:=@FDrawFocusRect;
  P^:=NormalizeRect(FFocusRect);
  {$IFDEF DEBUGRD}  Writeln('DrawCurrentFocsusRect : ',IsClear,' ', RectToStr(P^));{$ENDIF}
  InvalidateRect(Self.Handle,P,False);
 end;

procedure TFPReportDesignerControl.DoDrawCurrentFocusRect(var ARect: TRect);

begin
  if not CheckPainting('DoDrawCurrentFocusRect') then exit;
{$IFDEF DEBUGRD}  Writeln('DoDrawCurrentFocusRect : ',RectToStr(ARect));{$ENDIF}
  if IsRectEmpty(ARect) then exit;
{$IFDEF DEBUGRD}   Writeln('DoDrawCurrentFocusRect drawing: ',RectToStr(ARect));{$ENDIF}
{$IFDEF USEFOCUSRECT}
  Canvas.DrawFocusRect(FDrawFocusRect);
{$ELSE}
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psSelectRect;
  Canvas.Pen.Color := clSelectRect;
  Canvas.Rectangle(ARect);
{$ENDIF}
  ARect.Left:=-1;
  ARect.Top:=-1;
  ARect.Bottom:=-1;
  ARect.Right:=-1;
end;

procedure TFPReportDesignerControl.ExtendAddRectangle;

begin
  ClearFocusRect(False);
  FFocusRect.BottomRight:=FLastMouseMove;
  DrawCurrentFocusRect(False);
end;

procedure TFPReportDesignerControl.ExtendSelectRectangle;

begin
  ClearFocusRect(False);
  FFocusRect.BottomRight:=FLastMouseMove;
  DrawCurrentFocusRect(False);
end;

procedure TFPReportDesignerControl.ResizeSelection;

Var
  D : TPoint;

begin
  d.x:=FLastMouseMove.x-FLastMouseDown.x;
  d.y:=FLastMouseMove.y-FLastMouseDown.y;
  {$IFDEF DEBUGRD}Writeln('Moving ',PointToStr(d),'focusrect : ',RectToStr(FFocusRect));{$ENDIF}
  FSelectionOffset:=D;
  {$IFDEF DEBUGRD}Writeln('Moving ',PointToStr(d));{$ENDIF}
  Invalidate;
end;


procedure TFPReportDesignerControl.MoveSelection;

Const
  MinMoveX = 4;
  MinMoveY = 4;

Var
  D : TPoint;
  O : TReportObject;

begin
  d.x:=FLastMouseMove.x-FLastMouseDown.x;
  d.y:=FLastMouseMove.y-FLastMouseDown.y;
  if (DesignerState<>dsMoving) then
    if (Abs(d.x)>MinMoveX) or (Abs(d.y)>MinMoveY) then
      begin
      SetDesignerState(dsMoving);
      O:=FObjects.GetObjectAt(FLastMouseDown,[]);
      if not O.Selected then
        begin
        FObjects.BeginSelectionUpdate;
        try
          FObjects.ClearSelection;
          O.Selected:=True;
        finally
          FObjects.EndSelectionUpdate;
        end;
        end;
      end;
  if FDesignerState=dsMoving then
    begin
    {$IFDEF DEBUGRD}Writeln('Moving ',PointToStr(d),'focusrect : ',RectToStr(FFocusRect));{$ENDIF}
    // check if the end-user wants snap-to-grid or not.
    if (doSnapToGrid in DesignerOptions) and (FSnapResolution>1) then
      begin
      d.x := d.x - (d.x mod FSnapResolution);
      d.y := d.y - (d.y mod FSnapResolution);
      end;
    FSelectionOffset:=D;
    {$IFDEF DEBUGRD}Writeln('Moving ',PointToStr(d));{$ENDIF}
    Invalidate;
    end;
end;


procedure TFPReportDesignerControl.SetCursorFromHandlePos(AHandlePos : TResizeHandlePosition);

Const
  DefaultCursors : Array [TResizeHandlePosition] of TCursor =
  // rhNone,rhTopLeft,rhTop,rhTopRight,rhLeft,rhRight,rhBottomLeft, rhBottom,rhBottomRight
  (crDefault,crSizeNW,crSizeNS,crSizeNE,crSizeWE,crSizeWE,crSizeSW,crSizeNS,crSizeSE);

begin
  Screen.Cursor:=DefaultCursors[aHandlePos];
end;


Function TFPReportDesignerControl.ShowEditorForElement(aElement : TFPReportElement) : Boolean;

Var
  C : TFPReportElementEditorClass;
  E : TFPReportElementEditor;

begin
  C:=gElementFactory.FindEditorClassForInstance(AElement);
  if Assigned(C) then
    begin
    E:=C.Create(Self);
    try
      E.Element:=AElement;
      Result:=E.Execute;
      if Result then
        begin
        Objects.ReportChanged;
        Invalidate;
        end;
    finally
      E.Free;
    end;
    end;
end;

procedure TFPReportDesignerControl.DClick(Sender: TObject);

Var
  O : TReportObject;

begin
  O:=GetObjectAt(FLastMouseDown,[]);
  if Assigned(O) and O.IsPlainElement then
    ShowEditorForElement(O.Element);
end;

procedure TFPReportDesignerControl.CancelOperation;

Var
  ReturnToNeutral : Boolean;

begin
  ReturnToNeutral:=True;
  Case DesignerState of
    dsRectangleSelect,
    dsRectangleSelectExtend :
      ClearFocusRect(True);
    dsAddControlMulti,
    dsAddControl :
      begin
      FAddClass:=Nil;
      end
  end;
  if ReturnToNeutral then
    begin
    Cursor:=crDefault;
    SetDesignerState(dsNeutral);
    FSelectionOffset.X:=0;
    FSelectionOffset.Y:=0;
    FResizeDirection:=rhNone;
    end;
end;

procedure TFPReportDesignerControl.CopySelectionToClipBoard;

Var
  S : TMemoryStream;

begin
  CheckClipBoardFormat;
  S:=TMemoryStream.Create;
  try
    FObjects.SaveSelectionToStream(S);
    S.Position:=0;
    if not ClipBrd.Clipboard.AddFormat(ClipBoardFormat,S) then
      Raise EReportError.Create(SErrFailedToCopyToClipboard);
  finally
    S.Free;
  end;
end;



function TFPReportDesignerControl.AddBand(ABandClass: TFPReportBandClass
  ): TFPReportCustomBand;

Var
  O : TReportObject;

begin
  Result:=ABandClass.Create(Page.Report);
  Result.Layout.Height:=PixelsToMM(FMinControlHeight,CurrentDPI);
  Result.Parent:=Page;
  O:=FObjects.AddBand(Result);
  FObjects.OrderBands(Canvas,CurrentDPI);
  If  Assigned(FOnElementCreated) then
    FOnElementCreated(Self,Result);
  FObjects.SelectElement(O.AsBand);
end;

procedure TFPReportDesignerControl.AddElement(
  AElementClass: TFPReportElementClass; Multi: Boolean = False);
begin
  FAddClass:=AElementClass;
  if Multi then
    SetDesignerState(dsStartAddControlMulti)
  else
    SetDesignerState(dsStartAddControl);
  Cursor:=crCross;
  FObjects.ClearSelection;
  Invalidate;
end;

procedure TFPReportDesignerControl.CMMouseLeave(var Message: TLMessage);
begin
  CancelOperation;
end;

end.

