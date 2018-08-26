{                   -----------------------------------------
                    carbondef.pp  -  Type & Const definitions
                    -----------------------------------------

 @created(Wed Aug 26st WET 2005)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains type & const definitions needed in the Carbon <-> LCL interface

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}


unit CarbonDef;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
  // libs
  MacOSAll,
  Classes, SysUtils, Controls,
  Laz_AVL_Tree,
  // lcl
  LCLType, LCLProc, Graphics, Contnrs, LMessages, LCLMessageGlue,
  LazLoggerBase,
  // widgetset
  WSLCLClasses;

var
  LAZARUS_FOURCC: FourCharCode;    // = 'Laz ';
  WIDGETINFO_FOURCC: FourCharCode; // = 'WInf';
  MENU_FOURCC: FourCharCode;       // = 'Menu';
  CREATESHEETWINDOW: PtrUInt = 0; // used to create sheet inside modal form.issue #21459

type

  { TCarbonContext }

  TCarbonContext = class
  public
    CGContext : CGContextRef;
    procedure Reset; virtual; abstract;
    function GetLogicalOffset: TPoint; virtual;
  end;

  { TCarbonWidget }
  
  TCarbonWidget = class
  private
    FScrollOffset: TPoint;
    FEventProcCount: Integer;
    FProperties: TStringList;
    FCursor: HCURSOR;
    FHasCaret: Boolean;
    FHasPaint: Boolean;
    FResizing: Boolean;
    FBoundsReported: Boolean;
    function GetPainting: Boolean;
    function GetProperty(AIndex: String): Pointer;
    function GetScrollOffset: TPoint;
    procedure SetProperty(AIndex: String; const AValue: Pointer);
    procedure SetScrollOffset(AValue: TPoint);
  protected
    procedure RegisterEvents; virtual; abstract;
    procedure CreateWidget(const AParams: TCreateParams); virtual; abstract;
    procedure DestroyWidget; virtual; abstract;
    function GetContent: ControlRef; virtual; abstract;
    procedure UpdateLCLClientRect; virtual;
  public
    FPopupWin: WindowRef;
    FNeedFree: Boolean;
    procedure BeginEventProc;
    procedure EndEventProc;
    function isEventProcessing: Boolean;
    procedure FreeCarbonWidget;
  public
    LCLObject: TWinControl;  // LCL control which created this widget
    Context: TCarbonContext; // Carbon content area context
    Widget: HIViewRef;       // Reference to the Carbon control
  public
    procedure FocusSet; virtual;
    procedure FocusKilled; virtual;
    procedure BoundsChanged; virtual;
    procedure ControlAdded; virtual;
    function FilterKeyPress({%H-}SysKey: Boolean; const {%H-}Char: TUTF8Char): Boolean; virtual;
    procedure ProcessKeyEvent(const {%H-}msg: TLMKey); virtual;
    function NeedDeliverMouseEvent({%H-}Msg: Integer; const {%H-}AMessage): Boolean; virtual;
  public
    constructor Create(const AObject: TWinControl; const AParams: TCreateParams);
    destructor Destroy; override;
    procedure AddToWidget(AParent: TCarbonWidget); virtual; abstract;
    function GetClientRect(var ARect: TRect): Boolean; virtual; abstract;
    function GetPreferredSize: TPoint; virtual;
    function GetWindowRelativePos(winX, winY: Integer): TPoint; virtual; abstract;
    function GetMousePos: TPoint;
    function GetTopParentWindow: WindowRef; virtual; abstract;
    procedure Invalidate(Rect: PRect = nil); virtual; abstract;
    procedure InvalidateRgn(AShape: HISHapeRef);
    function IsDesignInteractive(const {%H-}P: TPoint): Boolean; virtual;
    function IsEnabled: Boolean; virtual; abstract;
    function IsVisible: Boolean; virtual; abstract;
    function Enable(AEnable: Boolean): Boolean; virtual; abstract;
    
    function GetNextFocus(Start: TCarbonWidget; Next: Boolean): ControlRef;
    procedure GetScrollInfo({%H-}SBStyle: Integer; var {%H-}ScrollInfo: TScrollInfo); virtual;
    function GetScrollbarVisible({%H-}SBStyle: Integer): Boolean; virtual;
    function GetBounds(var ARect: TRect): Boolean; virtual; abstract;
    function GetScreenBounds(var ARect: TRect): Boolean; virtual; abstract;
    function SetBounds(const ARect: TRect): Boolean; virtual; abstract;
    procedure SetChildZPosition(AChild: TCarbonWidget; const AOldPos, ANewPos: Integer; const AChildren: TFPList); virtual;
    procedure SetZOrder(AOrder: HIViewZOrderOp; ARefWidget: TCarbonWidget); virtual; abstract;
    procedure SetCursor(ACursor: HCURSOR); virtual;
    
    procedure ScrollBy(DX, DY: Integer); virtual;
    procedure ScrollRect(DX, DY: Integer; ARect: TRect); virtual;
    procedure SetFocus; virtual; abstract;
    procedure SetColor(const AColor: TColor); virtual; abstract;
    function SetScrollInfo({%H-}SBStyle: Integer; const {%H-}ScrollInfo: TScrollInfo): Integer; virtual;
    procedure SetFont(const AFont: TFont); virtual; abstract;
    procedure ShowHide(AVisible: Boolean); virtual; abstract;
    
    function GetText(var S: String): Boolean; virtual; abstract;
    function SetText(const S: String): Boolean; virtual; abstract;
    function Update: Boolean; virtual; abstract;
    
    function WidgetAtPos(const P: TPoint): ControlRef; virtual; abstract;
  public
    property BoundsReported: Boolean read FBoundsReported;
  { Content:
     = widget in controls without special client control
     - client area control of control or window
     - origin of local coordinates
     - area for embedding child controls
     - processes track and draw event                  }
    property Content: ControlRef read GetContent;
    property Cursor: HCURSOR read FCursor;
    property ScrollOffset: TPoint read GetScrollOffset write SetScrollOffset; // scrolled offset of  ScrollingWinControl
    property HasCaret: Boolean read FHasCaret write FHasCaret;
    property HasPaint: Boolean read FHasPaint write FHasPaint;
    property Painting: Boolean read GetPainting;
    property Properties[AIndex: String]: Pointer read GetProperty write SetProperty;
    property Resizing: Boolean read FResizing write FResizing;
  end;
  
type
  TCarbonObjectEventHandlerProc = function (ANextHandler: EventHandlerCallRef;
    AEvent: EventRef;
    AWidget: TObject): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}

  TCarbonEventHandlerProc = function (ANextHandler: EventHandlerCallRef;
    AEvent: EventRef;
    AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}

  TEventInt = packed record
    case Integer of
    1: (Chars: array[0..4] of Char);
    2: (Int: UInt32);
  end;
  
const
  LCLCarbonEventClass    = 'Laz ';
  LCLCarbonEventKindWake = 'Wake';
  LCLCarbonEventKindMain = 'Main';
  LCLCarbonEventKindUser = 'User';

function CheckHandle(const AWinControl: TWinControl; const AClass: TClass; const DbgText: String): Boolean;
function CheckWidget(const Handle: HWND; const AMethodName: String; AParamName: String = ''): Boolean;
function CheckWidget(const Handle: HWND; const AMethodName: String; AClass: TClass): Boolean;

function RegisterObjectEventHandler(AHandler: TCarbonObjectEventHandlerProc): EventHandlerUPP;
function RegisterEventHandler(AHandler: TCarbonEventHandlerProc): EventHandlerUPP;

procedure NeedFreeWidget(AWidget: TCarbonWidget);
procedure FreePendingWidgets;

implementation

uses
  CarbonProc, CarbonDbgConsts, CarbonUtils, CarbonCaret;

var
  WantFreeList : TFPList;

procedure NeedFreeWidget(AWidget: TCarbonWidget);
begin
  WantFreeList.Add(AWidget);
end;

procedure FreePendingWidgets;
var
  i : integer;
begin
  for i:=0 to WantFreeList.Count-1 do
    TCarbonWidget(WantFreeList[i]).Free;
  WantfreeList.Clear;
end;
{------------------------------------------------------------------------------
  Name:    CheckHandle
  Params:  AWinControl  - Handle of window
           AClass       - Class
           DbgText      - Text to output on invalid DC
  Returns: If the wincontrol handle is allocated and valid
 ------------------------------------------------------------------------------}
function CheckHandle(const AWinControl: TWinControl; const AClass: TClass;
  const DbgText: String): Boolean;
begin
  if AWinControl <> nil then
  begin
    if (AWinControl.HandleAllocated)
    and (TObject(AWinControl.Handle) is TCarbonWidget) then
    begin
      {$IFDEF VerboseWSClass}
        DebugLn(AClass.ClassName + '.' + DbgText + ' for ' + AWinControl.Name);
      {$ENDIF}

      Result := True;
    end
    else
    begin
      Result := False;
      debugln(['CheckHandle failed AWinControl=',DbgSName(AWinControl),' AClass=',DbgSName(AClass),' ',DbgText,' HandleAllocated=',AWinControl.HandleAllocated]);
      DumpStack;
    end;
  end
  else
  begin
    Result := False;
    DebugLn(AClass.ClassName + '.' + DbgText + ' for ' + AWinControl.Name +
      ' failed: WinControl is nil!');
  end;
end;

{------------------------------------------------------------------------------
  Name:    CheckWidget
  Params:  Handle      - Handle of window
           AMethodName - Method name
           AParamName  - Param name
  Returns: If the window is valid widget
 ------------------------------------------------------------------------------}
function CheckWidget(const Handle: HWND; const AMethodName: String;
  AParamName: String): Boolean;
begin
  if TObject(Handle) is TCarbonWidget then Result := True
  else
  begin
    Result := False;

    if Pos('.', AMethodName) = 0 then
      DebugLn(SCarbonWSPrefix + AMethodName + ' Error - invalid widget ' +
        AParamName + ' = ' + DbgS(Handle) + '!')
    else
      DebugLn(AMethodName + ' Error - invalid widget ' + AParamName + ' = ' +
        DbgS(Handle) + '!');
  end;
end;

{------------------------------------------------------------------------------
  Name:    CheckWidget
  Params:  Handle      - Handle of window
           AMethodName - Method name
           AClass      - Class
  Returns: If the window is valid widget and class
 ------------------------------------------------------------------------------}
function CheckWidget(const Handle: HWND; const AMethodName: String;
  AClass: TClass): Boolean;
var
  S: String;
begin
  if TObject(Handle) is TCarbonWidget then
  begin
    if TObject(Handle) is AClass then
    begin
      Result := True;
      Exit;
    end;
    
    S := ' Error - Widget ' + TObject(Handle).ClassName + ' is not ' +
      AClass.ClassName + '!';
  end
  else S := ' Error - Handle ' + DbgS(Handle) + ' is not valid widget!';
  
  Result := False;
  
  if Pos('.', AMethodName) = 0 then
    DebugLn(SCarbonWSPrefix + AMethodName + S)
  else
    DebugLn(AMethodName + S);
end;

//=====================================================
// UPP mamanger
//=====================================================
type
  TUPPAVLTreeNode = class(TAVLTreeNode)
  public
    UPP: EventHandlerUPP;
    procedure Clear; reintroduce; // not overridable, so reintroduce since we only will call this clear
    destructor Destroy; override;
  end;

var
  UPPTree: TAVLTree = nil;

{ TCarbonContext }

function TCarbonContext.GetLogicalOffset: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

procedure TUPPAVLTreeNode.Clear;
begin
  if UPP <> nil then
  begin
    DisposeEventHandlerUPP(UPP);
    UPP := nil;
  end;

  inherited Clear;
end;

destructor TUPPAVLTreeNode.Destroy;
begin
  if UPP <> nil then
  begin
    DisposeEventHandlerUPP(UPP);
    UPP := nil;
  end;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Name:    RegisterObjectEventHandler
  Params:  AHandler - Carbon object event handler procedure
  Returns: Event handler UPP

  Registers new carbon object event handler procedure
 ------------------------------------------------------------------------------}
function RegisterObjectEventHandler(AHandler: TCarbonObjectEventHandlerProc): EventHandlerUPP;
var
  Node: TUPPAVLTreeNode;
begin
  if UPPTree = nil then
  begin
    UPPTree := TAVLTree.Create;
    UPPTree.NodeClass:=TUPPAVLTreeNode;
  end;

  Node := TUPPAVLTreeNode(UPPTree.Find(AHandler));
  if Node = nil then
  begin
    Node := TUPPAVLTreeNode.Create;
    Node.Data := AHandler;
    Node.UPP := NewEventHandlerUPP(EventHandlerProcPtr(AHandler));
    UPPTree.Add(Node);
  end;

  Result := Node.UPP;
end;

{------------------------------------------------------------------------------
  Name:    RegisterEventHandler
  Params:  AHandler - Carbon event handler procedure
  Returns: Event handler UPP

  Registers new carbon event handler procedure
 ------------------------------------------------------------------------------}
function RegisterEventHandler(AHandler: TCarbonEventHandlerProc): EventHandlerUPP;
var
  Node: TUPPAVLTreeNode;
begin
  if UPPTree = nil then
  begin
    UPPTree := TAVLTree.Create;
    UPPTree.NodeClass:=TUPPAVLTreeNode;
  end;

  Node := TUPPAVLTreeNode(UPPTree.Find(AHandler));
  if Node = nil then
  begin
    Node := TUPPAVLTreeNode.Create;
    Node.Data := AHandler;
    Node.UPP := NewEventHandlerUPP(EventHandlerProcPtr(AHandler));
    UPPTree.Add(Node);
  end;

  Result := Node.UPP;
end;

{ TCarbonWidget }

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.GetProperty
  Params:  AIndex - Property name
  Returns: Property data, nil if the property is not listed

  Returns the specified property data or nil if the property is not listed
 ------------------------------------------------------------------------------}
function TCarbonWidget.GetProperty(AIndex: String): Pointer;
var
  I: Integer;
begin
  if FProperties <> nil then
  begin
    I := FProperties.IndexOf(AIndex);
    
    if I >= 0 then // the property is listed
    begin
      Result := FProperties.Objects[I];
      Exit;
    end;
  end;
  Result := nil;
end;

function TCarbonWidget.GetScrollOffset: TPoint;
begin
  Result := Point(-FScrollOffset.X, -FScrollOffset.Y);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.GetPainting
  Returns: If the widget is being repaint
 ------------------------------------------------------------------------------}
function TCarbonWidget.GetPainting: Boolean;
begin
  Result := Context <> nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.SetProperty
  Params:  AIndex - Property name
           AValue - Property data, nil means remove the property

  Sets the specified property data or removes the property
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.SetProperty(AIndex: String; const AValue: Pointer);
var
  I: Integer;
begin
  if FProperties = nil then
  begin
    if AValue = nil then Exit;
    // create string list for storing properties
    FProperties := TStringList.Create;
    FProperties.Sorted := True; // to enable binary searching
  end;
  
  I := FProperties.IndexOf(AIndex);
  if I >= 0 then // the property is listed -> update or remove if AValue = nil
  begin
    if AValue = nil then
    begin
      FProperties.Delete(I);
      if FProperties.Count = 0 then
      begin
        FProperties.Free; // free if the list is clear
        FProperties := nil;
      end;
    end
    else FProperties.Objects[I] := TObject(AValue);
  end
  else // the property is not listed -> add
  begin
    FProperties.AddObject(AIndex, TObject(AValue));
  end;
end;

procedure TCarbonWidget.SetScrollOffset(AValue: TPoint);
begin
  FScrollOffset := AValue;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.UpdateLCLClientRect

  Updates client rect of LCL object
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.UpdateLCLClientRect;
var
  R: TRect;
  ClientR: TRect;
  LCLR: TRect;
  LCLClientR: TRect;
  RChanged: Boolean;
  ClientChanged: Boolean;
begin
  if not Resizing then begin
    GetBounds(R{%H-});
    GetClientRect(ClientR{%H-});
    LCLR:=LCLObject.BoundsRect;
    LCLClientR:=LCLObject.ClientRect;
    RChanged:=not CompareRect(@R,@LCLR);
    ClientChanged:=not CompareRect(@ClientR,@LCLClientR);

    if not ClientChanged then
      LCLObject.InvalidateClientRectCache(False);
    if RChanged or ClientChanged then
      LCLSendSizeMsg(LCLObject, R.Right - R.Left, R.Bottom - R.Top, Size_SourceIsInterface);
  end;
end;

procedure TCarbonWidget.BeginEventProc;
begin
  inc(FEventProcCount);
end;

procedure TCarbonWidget.EndEventProc;
begin
  dec(FEventProcCount);
  if (FEventProcCount=0) and FNeedFree then
    NeedFreeWidget(Self)
end;

function TCarbonWidget.isEventProcessing: Boolean;
begin
  Result:=FEventProcCount>0;
end;

procedure TCarbonWidget.FreeCarbonWidget;
begin
  if isEventProcessing then
    FNeedFree:=True
  else
    Free;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.FocusSet

  Handles set focus
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.FocusSet;
begin
  {$IFDEF VerboseCommonEvent}
    DebugLn('TCarbonWidget.FocusSet: ', DbgSName(LCLObject));
  {$ENDIF}
  LCLSendSetFocusMsg(LCLObject);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.FocusKilled

  Handles kill focus
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.FocusKilled;
begin
  {$IFDEF VerboseCommonEvent}
    DebugLn('TCarbonWidget.FocusKilled: ', DbgSName(LCLObject));
  {$ENDIF}
  // the TCarbonWidget has already been freed, it cannot send any messages
  if not FNeedFree then
    LCLSendKillFocusMsg(LCLObject);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.BoundsChanged

  Handles bounds change
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.BoundsChanged;
var
{$IFDEF VerboseBounds}
  WidgetClient,
{$ENDIF}
  WidgetBounds, OldBounds: TRect;
  Resized, ClientResized, Moved: Boolean;
  PosMsg: TLMWindowPosChanged;
begin
  if FResizing then Exit;
  {$IFDEF VerboseBounds}
    DebugLn('TCarbonWidget.BoundsChanged ' + LCLObject.Name);
  {$ENDIF}

  GetBounds(WidgetBounds{%H-});
  OldBounds := LCLObject.BoundsRect;
  
  {$IFDEF VerboseBounds}
    GetClientRect(WidgetClient);
    DebugLn('TCarbonWidget.BoundsChanged Interface new bounds: ' + DbgS(WidgetBounds));
    DebugLn('TCarbonWidget.BoundsChanged LCL old bounds: ' + DbgS(OldBounds));
    DebugLn('TCarbonWidget.BoundsChanged Interface new client: ' + DbgS(WidgetClient));
    DebugLn('TCarbonWidget.BoundsChanged LCL old client: ' + DbgS(LCLObject.ClientRect));
  {$ENDIF}
  
  Resized :=
    (OldBounds.Right - OldBounds.Left <> WidgetBounds.Right - WidgetBounds.Left) or
    (OldBounds.Bottom - OldBounds.Top <> WidgetBounds.Bottom - WidgetBounds.Top) or
    not FBoundsReported;
  Moved :=
    (OldBounds.Left <> WidgetBounds.Left) or
    (OldBounds.Top <> WidgetBounds.Top) or
    not FBoundsReported;
  ClientResized := False;
  
  // send window pos changed
  if Resized or Moved then
  begin
    PosMsg.Msg := LM_WINDOWPOSCHANGED;
    PosMsg.Result := 0;
    New(PosMsg.WindowPos);
    try
      with PosMsg.WindowPos^ do
      begin
        hWndInsertAfter := 0;
        x := WidgetBounds.Left;
        y := WidgetBounds.Right;
        cx := WidgetBounds.Right - WidgetBounds.Left;
        cy := WidgetBounds.Bottom - WidgetBounds.Top;
        flags := 0;
      end;
      DeliverMessage(LCLObject, PosMsg);
    finally
      Dispose(PosMsg.WindowPos);
    end;
  end;
  
  // update client rect
  if Resized or LCLObject.ClientRectNeedsInterfaceUpdate then
  begin
    {$IFDEF VerboseBounds}
      DebugLn('TCarbonWidget.BoundsChanged Update client rects cache');
    {$ENDIF}
    LCLObject.InvalidateClientRectCache(False);
    ClientResized := True;
  end;
  
  // then send a LM_SIZE message
  if Resized or ClientResized then
  begin
    LCLSendSizeMsg(LCLObject, WidgetBounds.Right - WidgetBounds.Left,
      WidgetBounds.Bottom - WidgetBounds.Top, Size_SourceIsInterface);
  end;
  
  // then send a LM_MOVE message
  if Moved then
  begin
    LCLSendMoveMsg(LCLObject, WidgetBounds.Left,
      WidgetBounds.Top, Move_SourceIsInterface);
  end;

  // invalidate client area
  if ClientResized then Invalidate;

  // invalidate parent client area, previously covered by control
  if Resized and (LCLObject.Parent <> nil) and LCLObject.Parent.HandleAllocated then
  begin
    TCarbonWidget(LCLObject.Parent.Handle).Invalidate(@OldBounds);
  end;

  {$IFDEF VerboseBounds}
    DebugLn('TCarbonWidget.BoundsChanged LCL new bounds: ' + DbgS(LCLObject.BoundsRect));
    DebugLn('TCarbonWidget.BoundsChanged LCL new client: ' + DbgS(LCLObject.ClientRect));
  {$ENDIF}
  
  FBoundsReported := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.ControlAdded

  Notifies about control added
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.ControlAdded;
begin
  //
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.FilterKeyPress

  Filters key presses from being send to Carbon control
 ------------------------------------------------------------------------------}
function TCarbonWidget.FilterKeyPress(SysKey: Boolean; const Char: TUTF8Char): Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.ProcessKeyEvent
  Params:  msg    - LCL keyboard message
           Result - returned value, must be noErr if key is handled
  Returns: The Carbon widget

  Widget can perform it's own necessary actions if user has not processed the key.
  It's required to emulate Command driven Carbon controls
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.ProcessKeyEvent(const msg: TLMKey);
begin
end;

function TCarbonWidget.NeedDeliverMouseEvent(Msg: Integer; const AMessage): Boolean;
begin
  Result := true;
end;


{------------------------------------------------------------------------------
  Method:  TCarbonWidget.Create
  Params:  AObject - LCL conrol
           AParams - Creation parameters
  Returns: The Carbon widget

  Creates basic widget for the specified LCL control
 ------------------------------------------------------------------------------}
constructor TCarbonWidget.Create(const AObject: TWinControl;
  const AParams: TCreateParams);
begin
  FScrollOffset := Point(0, 0);
  LCLObject := AObject;
  FProperties := nil;
  Widget := nil;
  Context := nil;
  FHasCaret := False;
  FHasPaint := False;
  FResizing := False;
  FBoundsReported := False;
  
  CreateWidget(AParams);

  if (AParams.Style and WS_DISABLED) <> 0 then
    Enable(False);

  {$IFDEF VerboseWidget}
    DebugLn('TCarbonWidget.Create ', ClassName, ' ', LCLObject.Name, ': ',
      LCLObject.ClassName);
  {$ENDIF}
  
  RegisterEvents;
  
  {$IFDEF VerboseBounds}
    DebugLn('TCarbonWidget.Create LCL bounds: ' + DbgS(LCLObject.BoundsRect));
    DebugLn('TCarbonWidget.Create LCL client: ' + DbgS(LCLObject.ClientRect));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.Destroy

  Frees the widget
 ------------------------------------------------------------------------------}
destructor TCarbonWidget.Destroy;
begin
  {$IFDEF VerboseWidget}
    DebugLn('TCarbonWidget.Destroy ', ClassName, ' ', LCLObject.Name, ': ',
      LCLObject.ClassName);
  {$ENDIF}
  
  DestroyWidget;
  
  FProperties.Free;
  
  if HasCaret then DestroyCaret;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.GetPreferredSize
  Returns: The preffered size of widget for autosizing or (0, 0)
 ------------------------------------------------------------------------------}
function TCarbonWidget.GetPreferredSize: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.GetPreferredSize
  Returns: The current mouse position relative to the widgetset left-top pos
 ------------------------------------------------------------------------------}
function TCarbonWidget.GetMousePos: TPoint;
var
  P: MacOSAll.Point;
  R: MacOSAll.Rect;
const
  SName = 'GetMousePos';
begin
  GetGlobalMouse(P{%H-});

  OSError(GetWindowBounds(GetTopParentWindow, kWindowStructureRgn, R{%H-}),
    Self, SName, SGetWindowBounds);
  Result:=GetWindowRelativePos(P.h - R.left, P.v - R.top);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.InvalidateRgn
  Params:  AShape - HIShapeRef

  Invalidates the specified client region or entire area
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.InvalidateRgn(AShape: HISHapeRef);
begin
  if AShape = nil then Invalidate
  else
    OSError(HIViewSetNeedsDisplayInShape(Content, AShape, True),
      Self, 'InvalidateRgn', 'HIViewSetNeedsDisplayInShape');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.IsDesignInteractive
  Params:  P - Client pos
  Returns: If the pos is design interactive
 ------------------------------------------------------------------------------}
function TCarbonWidget.IsDesignInteractive(const P: TPoint): Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.GetNextFocus
  Params:  Start - Focus start
           Next  - Next or previous?
  Returns: Next control to focus
 ------------------------------------------------------------------------------}
function TCarbonWidget.GetNextFocus(Start: TCarbonWidget; Next: Boolean): ControlRef;
var
  StartControl, ParentControl, ResultControl: TWinControl;
  TabList: TFPObjectList;
  TabIndex: Integer;
  
begin
  Result := nil;
  ResultControl := nil;
  
  if Start <> nil then
    StartControl := Start.LCLObject
  else
    StartControl := nil;

  //DebugLn('TCarbonWidget.GetNextFocus ', LCLObject.Name, ' Start: ', DbgSName(StartControl), ' Next: ', DbgS(Next));
        
  ParentControl := LCLObject;
  TabList := TFPObjectList.Create(False);
  try
    while (ParentControl <> nil) and (ResultControl = nil) do
    begin
      TabList.Clear;
      ParentControl.GetTabOrderList(TabList.List);

      TabIndex := -1;
      if StartControl <> nil then
        TabIndex := TabList.IndexOf(StartControl);

      if (TabList.Count = 0) or
        (Next and (TabIndex > TabList.Count - 2)) or
        (not Next and (TabIndex < 1)) then
      begin
        StartControl := ParentControl;

        if ParentControl.Parent = nil then
          if Next then
            ResultControl := TabList.First as TWinControl
          else
            ResultControl := TabList.Last as TWinControl;
      end
      else
        if TabIndex = -1 then
        begin
          if Next then
            ResultControl := TabList.First as TWinControl
          else
            ResultControl := TabList.Last as TWinControl;
        end
        else
          if Next then
            ResultControl := TabList[TabIndex + 1] as TWinControl
          else
            ResultControl := TabList[TabIndex - 1] as TWinControl;
            
      ParentControl := ParentControl.Parent;
    end;
  finally
    TabList.Free;
  end;
  
  if ResultControl <> nil then
    Result := TCarbonWidget(ResultControl.Handle).Widget;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.GetScrollInfo
  Params:  SBStyle    - Scrollbar type (SB_VERT, SB_HORZ)
           ScrollInfo - Record fo scrolling info
  Returns: If the function suceeds

  Gets the scrolling info of the specified scroll bar
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.GetScrollInfo(SBStyle: Integer;
  var ScrollInfo: TScrollInfo);
begin
  // ToDo: TCarbonWidget.GetScrollInfo
  DebugLn(ClassName + '.GetScrollInfo unsupported or not implemented!');
end;

function TCarbonWidget.GetScrollbarVisible(SBStyle: Integer): Boolean;
begin
  Result := False;
  // ToDo TCarbonWidget.GetScrollbarVisible
  DebugLn(ClassName + '.GetScrollbarVisible unsupported or not implemented!');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonControl.SetChildZPosition
  Params:  AChild      - Child widget
           AOldPos     - Old z position
           ANewPos     - New z position
           AChildren   - List of all child controls

  Sets the child z position of Carbon widget
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.SetChildZPosition(AChild: TCarbonWidget; const AOldPos, ANewPos: Integer;
  const AChildren: TFPList);
var
  RefWidget: TCarbonWidget;
  Order: HIViewZOrderOp;
  I, StopPos: Integer;
  Child: TWinControl;
begin
  RefWidget := nil;

  if ANewPos <= 0 then // send behind all
    Order := kHIViewZOrderBelow
  else
    if ANewPos >= Pred(AChildren.Count) then // bring to front of all
      Order := kHIViewZOrderAbove
    else // custom position
    begin
      // Search for the first child above us with a handle.
      // The child list is reversed form the windows order.
      // If we don't find an allocated handle then exit.

      if AOldPos > ANewPos then
        StopPos := AOldPos // the child is moved to the bottom
      else
        StopPos := Pred(AChildren.Count); // the child is moved to the top

      for I := Succ(ANewPos) to StopPos do
      begin
        Child := TWinControl(AChildren[I]);

        if Child.HandleAllocated then
        begin
          RefWidget := TCarbonWidget(Child.Handle);
          Order := kHIViewZOrderBelow;
          Break;
        end;
      end;

      if RefWidget = nil then Exit;
    end;
    
  AChild.SetZOrder(Order, RefWidget);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.SetCursor
  Params:  ACursor - Handle of cursor to set

  Sets the cursor
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.SetCursor(ACursor: HCURSOR);
begin
  FCursor := ACursor;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.ScrollBy
  Params:  DX, DY

  Scrolls the content
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.ScrollBy(DX, DY: Integer);
var
  R: CGRect;
const
  SName = 'ScrollBy';
begin
  OSError(HIViewGetBounds(Content, R{%H-}),
    Self, SName, 'HIViewGetBounds');
  OSError(HIViewSetBoundsOrigin(Content, R.origin.x - DX, R.origin.y - DY),
    Self, SName, 'HIViewSetBoundsOrigin');
  with FScrollOffset do
  begin
    X := X + DX;
    Y := Y + DY;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.ScrollRect
  Params:  DX, DY

  Scrolls the content delimited by a bounding Rect
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.ScrollRect(DX, DY: Integer; ARect: TRect);
var
  R: CGRect;
const
  SName = 'ScrollRect';
begin
  R := RectToCGRect(ARect);
  OSError(HIViewScrollRect(Content, @R, DX, DY),
	Self, SName, 'HIViewScrollRect');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.SetScrollInfo
  Params:  SBStyle    - Scrollbar type (SB_VERT, SB_HORZ)
           ScrollInfo - Scrolling info
  Returns: The old scroll bar position

  Sets the scrolling info of the specified scroll bar
 ------------------------------------------------------------------------------}
function TCarbonWidget.SetScrollInfo(SBStyle: Integer;
  const ScrollInfo: TScrollInfo): Integer;
begin
  Result := 0;
  // ToDo: TCarbonWidget.SetScrollInfo
  DebugLn(ClassName + '.SetScrollInfo unsupported or not implemented!');
end;

initialization

  LAZARUS_FOURCC := MakeFourCC('Laz ');
  WIDGETINFO_FOURCC := MakeFourCC('WInf');
  MENU_FOURCC := MakeFourCC('Menu');
  WantFreeList:=TFPList.Create;
  
finalization

  if UPPTree <> nil then FreeAndNil(UPPTree);
  WantFreeList.Free;

end.
