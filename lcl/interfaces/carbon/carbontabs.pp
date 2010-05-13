{ $Id$
                  ----------------------------------------------
                  carbontabs.pp  -  Carbon tabs Control and tabs
                  ----------------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonTabs;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  MacOSAll,
 // widgetset
  WSControls, WSLCLClasses, WSProc,
 // LCL Carbon
  CarbonDef, CarbonPrivate, CarbonProc, CarbonDbgConsts, CarbonUtils, CarbonCanvas, CarbonGDIObjects,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, ExtCtrls;
  
type
  TCarbonTabsControl = class;

  { TCarbonTab }

  TCarbonTab = class(TCarbonCustomControl)
  private
    FParent: TCarbonTabsControl;
    FText: String;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
  public
    procedure Attach(AParent: TCarbonTabsControl);
    procedure UpdateTab;
    function SetText(const S: String): Boolean; override;
    procedure ShowHide(AVisible: Boolean); override;
  end;

  { TCarbonTabsControl }

  TCarbonTabsControl = class(TCarbonControl)
  private
    FUserPane: ControlRef;
    FTabPosition: TTabPosition;
    FTabs: TObjectList; // of TCarbonTab
    FTabIndex: Integer;
    FOldTabIndex: Integer;
    FFirstIndex: Integer; // index of first visible tab
    FLastIndex: Integer;  // index of last visible tab
    FPrevArrow: ControlRef;
    FNextArrow: ControlRef;
    FScrollingLeftTimer: TTimer;
    FScrollingRightTimer: TTimer;
    FLockChangeEvent: integer;
    FShowTabBar: Boolean;
    function GetPrevArrowBounds(const R: TRect): TRect;
    function GetNextArrowBounds(const R: TRect): TRect;
    procedure ScrollingLeftTimer(Sender: TObject);
    procedure ScrollingRightTimer(Sender: TObject);
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetContent: ControlRef; override;
    
    procedure ShowTab;
    procedure UpdateTabs(EnsureLastVisible: Boolean = False; UpdateIndex: Boolean = True);
    procedure UpdateTabIndex;
    procedure Remove(ATab: TCarbonTab);
    function GetControlTabIndex: Integer; // visible index, without hidden or scrolled tabs
    function GetTabIndex(APageIndex: Integer): Integer;
    function TabIndexToPageIndex(AIndex: Integer): Integer;
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure ValueChanged; override;
    procedure DisableChangeEvent;
    procedure EnableChangeEvent;
  public
    function SetText(const S: String): Boolean; override;
    function GetClientRect(var ARect: TRect): Boolean; override;
    function SetBounds(const ARect: TRect): Boolean; override;

    function GetPageIndexAtCursor(const AClientPos: TPoint): Integer;
    
    function IsDesignInteractive(const P: TPoint): Boolean; override;
    
    procedure ScrollTabsLeft;
    procedure ScrollTabsRight;
    procedure StartScrollingTabsLeft;
    procedure StartScrollingTabsRight;
    procedure StopScrollingTabsLeft;
    procedure StopScrollingTabsRight;

    procedure Add(ATab: TCarbonTab; AIndex: Integer);
    procedure Remove(AIndex: Integer);
    procedure SetPageIndex(AIndex: Integer);
    procedure ShowTabs(AShow: Boolean);
    procedure SetTabPosition(ATabPosition: TTabPosition);
  end;


implementation

{ TCarbonTab }

{------------------------------------------------------------------------------
  Method:  TCarbonTab.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon tab
 ------------------------------------------------------------------------------}
procedure TCarbonTab.CreateWidget(const AParams: TCreateParams);
begin
  inherited CreateWidget(AParams);
  
  ShowHide(False);
  
  FText := LCLObject.Caption;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTab.DestroyWidget

  Clean-up
 ------------------------------------------------------------------------------}
procedure TCarbonTab.DestroyWidget;
begin
  if FParent <> nil then FParent.Remove(Self);

  inherited DestroyWidget;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTab.Attach
  Params:  AParent - Tabs control

  Attaches Carbon tab to tabs control
 ------------------------------------------------------------------------------}
procedure TCarbonTab.Attach(AParent: TCarbonTabsControl);
begin
  FParent := AParent;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTab.UpdateTab

  Updates Carbon tab properties
 ------------------------------------------------------------------------------}
procedure TCarbonTab.UpdateTab;
begin
  if FParent = nil then Exit;
  
  FParent.UpdateTabs;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTab.SetText
  Params:  S - New text

  Changes Carbon tab caption
 ------------------------------------------------------------------------------}
function TCarbonTab.SetText(const S: String): Boolean;
begin
  FText := S;
  if FParent = nil then Exit;

  Result := False;
  FParent.UpdateTabs;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTab.ShowHide
  Params:  AVisible - if show

  Shows or hides control
 ------------------------------------------------------------------------------}
procedure TCarbonTab.ShowHide(AVisible: Boolean);
begin
  if not (csDesigning in LCLObject.ComponentState) then
    inherited ShowHide(AVisible)
  else
  begin
    if FParent <> nil then
      AVisible :=
        (LCLObject as TCustomPage).PageIndex = FParent.TabIndexToPageIndex(FParent.FTabIndex);

    OSError(HIViewSetVisible(Frames[0], AVisible),
      Self, 'ShowHide', SViewVisible);
  end;
end;

{ TCarbonTabsControl }

{------------------------------------------------------------------------------
  Name: CarbonTabsPrevArrow_Hit
 ------------------------------------------------------------------------------}
function CarbonTabsPrevArrow_Track(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonTabsPrevArrow_Track: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  (AWidget as TCarbonTabsControl).StartScrollingTabsLeft;
  try
    Result := CallNextEventHandler(ANextHandler, AEvent);
  finally
    (AWidget as TCarbonTabsControl).StopScrollingTabsLeft;
  end;
end;

{------------------------------------------------------------------------------
  Name: CarbonTabsNextArrow_Hit
 ------------------------------------------------------------------------------}
function CarbonTabsNextArrow_Track(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonTabsNextArrow_Track: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  (AWidget as TCarbonTabsControl).StartScrollingTabsRight;
  try
    Result := CallNextEventHandler(ANextHandler, AEvent);
  finally
    (AWidget as TCarbonTabsControl).StopScrollingTabsRight;
  end;
end;

const
  ArrowSize = 16;

{------------------------------------------------------------------------------
  Name: CarbonTabsPrevArrow_Reverse
  Reverses carbon arrow CGContext, so the right pointing arrow reversed to left
  It's required in Leopard only, there left arrow is suppressed by Apple.
 ------------------------------------------------------------------------------}
function CarbonTabsPrevArrow_Reverse(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  Context : CGContextRef;
  layer   : CGLayerRef;
  lCtx    : CGContextRef;
  sz      : CGSize;
  pnt     : CGPoint;
  w       : LongWord;
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonTabsPrevArrow_Reverse: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  Result := GetEventParameter(AEvent, kEventParamCGContextRef, typeCGContextRef, nil, sizeof(Context), nil, @Context );
  if Result <> 0 then begin
    CallNextEventHandler(ANextHandler, AEvent);
    Exit;
  end;

  sz.height := ArrowSize; sz.width := ArrowSize;
  layer := CGLayerCreateWithContext(Context, sz, nil);
  try
    lCtx := CGLayerGetContext(layer);
    SetEventParameter(AEvent, kEventParamCGContextRef, typeCGContextRef, sizeof(lCtx), @lCtx);

    Result := CallNextEventHandler(ANextHandler, AEvent);

    w := ArrowSize;
    pnt.x := w-0-ArrowSize; pnt.y := 1;
    CGContextTranslateCTM(Context, w, 0);
    CGContextScaleCTM(Context, -1, 1);
    CGContextDrawLayerAtPoint(Context, pnt, layer);
  finally
    CGLayerRelease(layer);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.GetPrevArrowBounds
  Returns: Bounds of prev arrow
 ------------------------------------------------------------------------------}
function TCarbonTabsControl.GetPrevArrowBounds(const R: TRect): TRect;
begin
  case FTabPosition of
    tpTop: Result := Classes.Bounds(R.Left, R.Top - ArrowSize, ArrowSize, ArrowSize);
    tpBottom: Result := Classes.Bounds(R.Left, R.Bottom, ArrowSize, ArrowSize);
    tpLeft: Result := Classes.Bounds(R.Left - ArrowSize, R.Top, ArrowSize, ArrowSize);
    tpRight: Result := Classes.Bounds(R.Right, R.Top, ArrowSize, ArrowSize);
  end;
end;


{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.GetNextArrowBounds
  Returns: Bounds of next arrow
 ------------------------------------------------------------------------------}
function TCarbonTabsControl.GetNextArrowBounds(const R: TRect): TRect;
begin
  case FTabPosition of
    tpTop: Result := Classes.Bounds(R.Right - ArrowSize, R.Top - ArrowSize, ArrowSize, ArrowSize);
    tpBottom: Result := Classes.Bounds(R.Right - ArrowSize, R.Bottom, ArrowSize, ArrowSize);
    tpLeft: Result := Classes.Bounds(R.Left - ArrowSize, R.Bottom - ArrowSize, ArrowSize, ArrowSize);
    tpRight: Result := Classes.Bounds(R.Right, R.Bottom - ArrowSize, ArrowSize, ArrowSize);
  end;
end;

procedure TCarbonTabsControl.ScrollingLeftTimer(Sender: TObject);
begin
  ScrollTabsLeft;
end;

procedure TCarbonTabsControl.ScrollingRightTimer(Sender: TObject);
begin
  ScrollTabsRight;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon tabs control
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.CreateWidget(const AParams: TCreateParams);
var
  Control: ControlRef;
  Direction: ControlTabDirection;
  TabEntry: ControlTabEntry;
  R: TRect;
  TmpSpec: EventTypeSpec;
  Err: OSStatus;
  Ver: SInt32;
begin
  FShowTabBar := (LCLObject as TCustomNotebook).ShowTabs;

  case (LCLObject as TCustomNotebook).TabPosition of
  tpTop: Direction := kControlTabDirectionNorth;
  tpBottom: Direction := kControlTabDirectionSouth;
  tpRight: Direction := kControlTabDirectionEast;
  tpLeft: Direction := kControlTabDirectionWest;
  end;

  if FShowTabBar then
  begin
    FillChar(TabEntry, SizeOf(TabEntry), 0);
    if OSError(
      CreateTabsControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
        kControlTabSizeLarge, Direction, 0, TabEntry, Control),
      Self, SCreateWidget, 'CreateTabsControl') then RaiseCreateWidgetError(LCLObject);
  end
  else
  begin
    if OSError(
      CreateGroupBoxControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
        nil, True, Control),
      Self, SCreateWidget, 'CreateGroupBoxControl') then RaiseCreateWidgetError(LCLObject);
  end;
  FOldTabIndex := -1;
  FTabPosition := (LCLObject as TCustomNotebook).TabPosition;
  FTabs := TObjectList.Create(False);

  Widget := Control;

  if not GetClientRect(R) then
  begin
    DebugLn('TCarbonTabsControl.CreateWidget Error - no content region!');
    Exit;
  end;

  if FShowTabBar then
  begin
    // create arrows for tabs scrolling
    OSError(
      CreateDisclosureTriangleControl(GetTopParentWindow,
        GetCarbonRect(GetPrevArrowBounds(R)),
        kControlDisclosureTrianglePointRight, nil, 0, False, False, FPrevArrow),
      Self, SCreateWidget, 'CreatePopupArrowControl');
    OSError(HIViewSetVisible(FPrevArrow, False), Self, SCreateWidget, SViewVisible);
    OSError(HIViewAddSubview(Widget, FPrevArrow), Self, SCreateWidget,
      SViewAddView);

    OSError(
      CreateDisclosureTriangleControl(GetTopParentWindow,
        GetCarbonRect(GetNextArrowBounds(R)),
        kControlDisclosureTrianglePointRight, nil, 0, False, False, FNextArrow),
      Self, SCreateWidget, 'CreatePopupArrowControl');
    OSError(HIViewSetVisible(FNextArrow, False), Self, SCreateWidget, SViewVisible);
    OSError(HIViewAddSubview(Widget, FNextArrow), Self, SCreateWidget,
      SViewAddView);

    if csDesigning in LCLObject.ComponentState then
      TmpSpec := MakeEventSpec(kEventClassControl, kEventControlHit)
    else
      TmpSpec := MakeEventSpec(kEventClassControl, kEventControlTrack);
    InstallControlEventHandler(FPrevArrow,
      RegisterEventHandler(@CarbonTabsPrevArrow_Track),
      1, @TmpSpec, Pointer(Self), nil);
    InstallControlEventHandler(FNextArrow,
      RegisterEventHandler(@CarbonTabsNextArrow_Track),
      1, @TmpSpec, Pointer(Self), nil);

    Err:=Gestalt(gestaltSystemVersion, Ver);
    if (Err <> 0) or (Ver >= $1040) then begin
      TmpSpec := MakeEventSpec(kEventClassControl, kEventControlDraw);
      InstallControlEventHandler(FPrevArrow,
        RegisterEventHandler(@CarbonTabsPrevArrow_Reverse),
        1, @TmpSpec, Pointer(Self), nil);
    end;
  end;

  FFirstIndex := 0;
  FLastIndex := 0;
  FTabIndex := -1;

  FUserPane := CreateCustomHIView(RectToCGRect(R));
  if FUserPane = nil then RaiseCreateWidgetError(LCLObject);

  OSError(HIViewSetVisible(FUserPane, True), Self, SCreateWidget, SViewVisible);

  if OSError(HIViewAddSubview(Control, FUserPane), Self, SCreateWidget,
    SViewAddView) then RaiseCreateWidgetError(LCLObject);
   
  inherited;
  
  FScrollingLeftTimer := TTimer.Create(nil);
  FScrollingLeftTimer.Interval := 200;
  FScrollingLeftTimer.Enabled := False;
  FScrollingLeftTimer.OnTimer := @ScrollingLeftTimer;
  
  FScrollingRightTimer := TTimer.Create(nil);
  FScrollingRightTimer.Interval := 200;
  FScrollingRightTimer.Enabled := False;
  FScrollingRightTimer.OnTimer := @ScrollingRightTimer;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.DestroyWidget

  Frees Carbon tabs control
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.DestroyWidget;
begin
  DisposeControl(FUserPane);
  FreeAndNil(FTabs);
  
  FScrollingLeftTimer.Free;
  FScrollingRightTimer.Free;

  inherited DestroyWidget;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.GetContent
  Returns: Content area control
 ------------------------------------------------------------------------------}
function TCarbonTabsControl.GetContent: ControlRef;
begin
  Result := FUserPane;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.ShowTab

  Shows the current tab and hides the others
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.ShowTab;
var
  I: Integer;
  R: TRect;
begin
  // show tab with FTabIndex, hide the others
  for I := 0 to FTabs.Count - 1 do
  begin
    if I = FTabIndex then // update tab bounds
    begin
      GetClientRect(R);
      OffsetRect(R, -R.Left, -R.Top);
      TCarbonTab(FTabs[I]).SetBounds(R);
    end;
    
    TCarbonTab(FTabs[I]).ShowHide(I = FTabIndex);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.UpdateTabs

  Updates tabs properties
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.UpdateTabs(EnsureLastVisible: Boolean; UpdateIndex: Boolean = True);
var
  I, L: Integer;
  TabSizes: Array of Integer;
  S: String;
  Size: TSize;
  ControlSize: Integer;
  TempFont: TCarbonFont;
  TabInfo: ControlTabInfoRecV1;
const
  SName = 'UpdateTabs';
begin
  try
    if not FShowTabBar or (FTabs.Count = 0) then
    begin
      FFirstIndex := 0;
      FLastIndex := 0;

      if not FShowTabBar then
        FLastIndex := FTabs.Count - 1;

      SetControl32BitMaximum(ControlRef(Widget), FTabs.Count);

      UpdateTabIndex;
      Exit;
    end;

    SetLength(TabSizes, FTabs.Count);
    TempFont := DefaultContext.CurrentFont;
    DefaultContext.CurrentFont := TCarbonFont(LCLObject.Font.Reference.Handle);
    try
      for I := 0 to High(TabSizes) do
      begin
        S := TCarbonTab(FTabs[I]).FText;
        DeleteAmpersands(S);
        if DefaultContext.GetTextExtentPoint(PChar(S), Length(S), Size) then
          TabSizes[I] := Size.cx + 24
        else
          TabSizes[I] := 24;
          
        //DebugLn(DbgS(I), '. ', S, ' ', DbgS(TabSizes[I]));
      end;
    finally
      DefaultContext.CurrentFont := TempFont;
    end;

    if FTabPosition in [tpTop, tpBottom] then ControlSize := LCLObject.Width
    else ControlSize := LCLObject.Height;

    //DebugLn('Size: ' + DbgS(ControlSize));
    ControlSize := ControlSize - 2 * ArrowSize - TabSizes[FFirstIndex];

    if EnsureLastVisible then
    begin
      if FLastIndex < 0 then FLastIndex := 0;
      if FLastIndex >= FTabs.Count then FLastIndex := FTabs.Count - 1;
      FFirstIndex := FLastIndex;
      
      L := FFirstIndex;
      // add tabs left from last
      for I := FLastIndex - 1 downto 0 do
      begin
        //DebugLn(DbgS(I), '. ', DbgS(ControlSize), ' >? ', DbgS(TabSizes[I]));
        if ControlSize >= TabSizes[I] then
        begin
          FFirstIndex := I;
          Dec(ControlSize, TabSizes[I]);
        end
        else Break;
      end;
      
      L := FLastIndex;
      // possibly add tabs right from last
      for I := L + 1 to FTabs.Count - 1 do
      begin
        //DebugLn(DbgS(I), '. ', DbgS(ControlSize), ' >? ', DbgS(TabSizes[I]));
        if ControlSize >= TabSizes[I] then
        begin
          FLastIndex := I;
          Dec(ControlSize, TabSizes[I]);
        end
        else Break;
      end;
    end
    else
    begin
      if FFirstIndex < 0 then FFirstIndex := 0;
      if FFirstIndex >= FTabs.Count then FFirstIndex := FTabs.Count - 1;
      FLastIndex := FFirstIndex;
      
      // add tabs right from first
      for I := FFirstIndex + 1 to FTabs.Count - 1 do
      begin
        //DebugLn(DbgS(I), '. ', DbgS(ControlSize), ' >? ', DbgS(TabSizes[I]));
        if ControlSize >= TabSizes[I] then
        begin
          FLastIndex := I;
          Dec(ControlSize, TabSizes[I]);
        end
        else Break;
      end;

      L := FFirstIndex;
      // possibly add tabs left from first
      for I := L - 1 downto 0 do
      begin
        //DebugLn(DbgS(I), '. ', DbgS(ControlSize), ' >? ', DbgS(TabSizes[I]));
        if ControlSize >= TabSizes[I] then
        begin
          FFirstIndex := I;
          Dec(ControlSize, TabSizes[I]);
        end
        else Break;
      end;
    end;

    // set tab count
    SetControl32BitMaximum(ControlRef(Widget), FLastIndex - FFirstIndex + 1);

    // update tabs
    TabInfo.version := kControlTabInfoVersionOne;
    TabInfo.iconSuiteID := 0;

    // TODO: imageindex
    for I := FFirstIndex to FLastIndex do
    begin
      S := TCarbonTab(FTabs[I]).FText;

      DeleteAmpersands(S);
      CreateCFString(S, TabInfo.name);
      try
        if OSError(SetControlData(ControlRef(Widget), I - FFirstIndex + 1, kControlTabInfoTag,
            SizeOf(ControlTabInfoRecV1), @TabInfo),
          Self, SName, SSetData) then Exit;
      finally
        FreeCFString(TabInfo.name);
      end;
    end;

  finally
    // update arrows visible
    if FShowTabBar then
    begin
      OSError(HIViewSetVisible(FPrevArrow, (FFirstIndex > 0)), Self, SName, SViewVisible);
      OSError(HIViewSetVisible(FNextArrow, (FLastIndex < FTabs.Count - 1)), Self, SName, SViewVisible);
    end;

    if UpdateIndex then UpdateTabIndex;
  end;
end;

procedure TCarbonTabsControl.UpdateTabIndex;
begin
  // set tab index
  //debugln(['TCarbonTabsControl.UpdateTabIndex FFirstIndex=',FFirstIndex,' FLastIndex=',FLastIndex,' TabIndex=',FTabIndex]);
  DisableChangeEvent;
  try
    SetControl32BitValue(ControlRef(Widget), GetControlTabIndex);
  finally
    EnableChangeEvent;
  end;
  Invalidate;
  ShowTab;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.Remove
  
  Removes the specified tab
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.Remove(ATab: TCarbonTab);
begin
  // FTabs is a TObjectLisy and Remove frees the ATab, which will
  // automatically call this proc again. Check if ATab is already removed.
  if FTabs.IndexOf(ATab)<0 then exit;
  FTabs.Remove(ATab);
  UpdateTabs(False, False);
  //debugln(['TCarbonTabsControl.Remove ',GetControlTabIndex,' FFirstIndex=',FFirstIndex,' FTabIndex=',FTabIndex,' Count=',ftabs.Count]);
end;

function TCarbonTabsControl.GetControlTabIndex: Integer;
begin
  Result := FTabIndex - FFirstIndex + 1;
end;

function TCarbonTabsControl.GetTabIndex(APageIndex: Integer): Integer;
// find the index in FTabs with TCustomPage.PageIndex=APageIndex
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FTabs.Count - 1 do
  begin
    if ((FTabs[I] as TCarbonTab).LCLObject as TCustomPage).PageIndex = APageIndex then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TCarbonTabsControl.TabIndexToPageIndex(AIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := AIndex;
  if csDesigning in LCLObject.ComponentState then Exit;
  I := 0;
  while (I < (LCLObject as TCustomNotebook).PageCount) and (I <= Result) do
  begin
    if not (LCLObject as TCustomNotebook).Page[I].TabVisible then Inc(Result);
    Inc(I);
  end;
end;

function TCarbonTabsControl.SetText(const S: String): Boolean;
begin
  // caption is not supported
  Result := True;
end;


{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.GetValidEvents
  Returns: Set of events with installed handlers
 ------------------------------------------------------------------------------}
class function TCarbonTabsControl.GetValidEvents: TCarbonControlEvents;
begin
  Result := [cceValueChanged];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.ValueChanged

  Value changed event handler
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.ValueChanged;
var
  Msg: TLMNotify;
  NMHdr: TNMHDR;
  Index, PIndex: Integer;
begin
  if FLockChangeEvent>0 then exit;
  Index := GetValue - 1;
  if Index >= 0 then Inc(Index, FFirstIndex);
  
  //comment
  //DebugLn('TCarbonTabsControl.ValueChanged Index: ', DbgS(Index),  ' Old ', DbgS(FOldTabIndex), ' Current ', DbgS(FTabIndex));
  if Index = FTabIndex then Exit;
  FOldTabIndex := FTabIndex;
  FTabIndex := Index;

  if (Index >= 0) and (Index < FTabs.Count) then
    PIndex := TabIndexToPageIndex(Index)
  else
  begin // select no tab
    SetPageIndex(-1);
    Exit;
  end;

  // send changing
  FillChar(Msg, SizeOf(TLMNotify), 0);
  Msg.Msg := LM_NOTIFY;

  FillChar(NMHdr, SizeOf(TNMHdr), 0);
  NMHdr.code := TCN_SELCHANGING;
  NMHdr.hwndFrom := LCLObject.Handle;
  NMHdr.idFrom := PIndex;

  Msg.NMHdr := @NMHdr;

  if DeliverMessage(LCLObject, Msg) <> 0 then
  begin // tab change cancelled
    SetPageIndex((LCLObject as TCustomNoteBook).PageIndex);
    Exit;
  end;

  SetPageIndex(PIndex); // we must use page index!

  // send change
  FillChar(Msg, SizeOf(TLMNotify), 0);
  Msg.Msg := LM_NOTIFY;
  
  FillChar(NMHdr, SizeOf(TNMHdr), 0);
  NMHdr.code := TCN_SELCHANGE;
  NMHdr.hwndFrom := LCLObject.Handle;
  NMHdr.idFrom := PIndex;
  
  Msg.NMHdr := @NMHdr;
  
  DeliverMessage(LCLObject, Msg);
end;

procedure TCarbonTabsControl.DisableChangeEvent;
begin
  inc(FLockChangeEvent);
end;

procedure TCarbonTabsControl.EnableChangeEvent;
begin
  dec(FLockChangeEvent);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.GetClientRect
  Params:  ARect - Record for client area coordinates
  Returns: If the function succeeds

  Returns the tabs control client rectangle relative to control origin
 ------------------------------------------------------------------------------}
function TCarbonTabsControl.GetClientRect(var ARect: TRect): Boolean;
var
  AClientRect: MacOSAll.Rect;
begin
  Result := False;

  if not FShowTabBar then
  begin
    Result := GetControlContentRect(ARect);
    Exit;
  end;
  //DebugLn('TCarbonTabsControl.GetClientRect, TabControl ', DbgS(Widget) );

  // it's normal sitation if GetControlData fails with error code.
  // (TabControl is not large enough to return client rect).
  // so there's no need to report the error.

  // if OSError(GetControlData(ControlRef(Widget), kControlEntireControl,
  //    kControlTabContentRectTag, SizeOf(MacOSAll.Rect), @AClientRect, nil),
  //    Self, 'GetClientRect', 'GetControlData') then begin

  if GetControlData(ControlRef(Widget), kControlEntireControl,
    kControlTabContentRectTag, SizeOf(MacOSAll.Rect), @AClientRect, nil) <> noErr then
    AClientRect := GetCarbonRect(0, 0, 0, 0);

  ARect := CarbonRectToRect(AClientRect);
  
  //DebugLn('TCarbonTabsControl.GetClientRect ' + DbgS(ARect));
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.SetBounds
  Params:  ARect - Record for control coordinates
  Returns: If function succeeds

  Sets the control bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCarbonTabsControl.SetBounds(const ARect: TRect): Boolean;
var
  R: TRect;
begin
  Result := False;
  if inherited SetBounds(ARect) then
  begin
    UpdateContentBounds;
    
    GetClientRect(R);

    if FShowTabBar then
    begin
      OSError(HIViewSetFrame(FPrevArrow, RectToCGRect(GetPrevArrowBounds(R))),
        Self, SSetBounds, SViewFrame);
      OSError(HIViewSetFrame(FNextArrow, RectToCGRect(GetNextArrowBounds(R))),
        Self, SSetBounds, SViewFrame);
    end;

    Result := True;
  end;
  
  UpdateTabs;
end;

function TCarbonTabsControl.GetPageIndexAtCursor(const AClientPos: TPoint): Integer;
var
  tabno  : ControlPartCode;
begin
  Result := -1;
  if not CarbonHitTest(Widget, AClientPos.X, AClientPos.Y, tabno) then Exit;

  if tabno = kControlNoPart then
  begin
    Result := TCustomNotebook(LCLObject).PageIndex
    //CarbonHitTest(FUserPane, AClientPos.X, AClientPos.Y-35, tabno);
    //Result := tabno;
  end
  else
    Result := FFirstIndex+tabno-1;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.IsDesignInteractive
  Params:  P
  Returns: If the pos is design interactive
 ------------------------------------------------------------------------------}
function TCarbonTabsControl.IsDesignInteractive(const P: TPoint): Boolean;
var
  R: TRect;
begin
  GetClientRect(R);
  Offsetrect(R, -R.Left, -R.Top);
  
  case FTabPosition of
    tpTop: Result := P.Y < R.Top;
    tpBottom: Result := P.Y > R.Bottom;
    tpLeft: Result := P.X < R.Left;
    tpRight: Result := P.X > R.Right;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.ScrollTabsLeft;
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.ScrollTabsLeft;
begin
  Dec(FFirstIndex);
  UpdateTabs;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.ScrollTabsRight;
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.ScrollTabsRight;
begin
  Inc(FLastIndex);
  UpdateTabs(True);
end;

procedure TCarbonTabsControl.StartScrollingTabsLeft;
begin
  ScrollTabsLeft;
  FScrollingLeftTimer.Enabled := True;
end;

procedure TCarbonTabsControl.StartScrollingTabsRight;
begin
  ScrollTabsRight;
  FScrollingRightTimer.Enabled := True;
end;

procedure TCarbonTabsControl.StopScrollingTabsLeft;
begin
  FScrollingLeftTimer.Enabled := False;
end;

procedure TCarbonTabsControl.StopScrollingTabsRight;
begin
  FScrollingRightTimer.Enabled := False;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.Add
  Params:  ATab   - Tab to add
           AIndex - At index

  Adds Carbon tab at the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.Add(ATab: TCarbonTab; AIndex: Integer);
begin
  //DebugLn('TCarbonTabsControl.Add ' + DbgS(AIndex));
  if FTabs.IndexOf(ATab) >= 0 then exit;
  FTabs.Insert(AIndex, ATab);
  ATab.Attach(Self);
  
  UpdateTabs;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.Remove
  Params:  AIndex - Index of tab to remove

  Removes Carbon tab with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.Remove(AIndex: Integer);
begin
  Remove(FTabs[AIndex] as TCarbonTab);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.SetPageIndex
  Params:  AIndex - New page index

  Changes the current Carbon page
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.SetPageIndex(AIndex: Integer);
var
  ATabIndex: Integer;
begin
  DisableChangeEvent;
  try
    ATabIndex := GetTabIndex(AIndex);

    //DebugLn('TCarbonTabsControl.SetPageIndex Page: ' + DbgS(AIndex) + ' Tab: ' + DbgS(ATabIndex));

    if (ATabIndex < 0) or (ATabIndex >= FTabs.Count) then
    begin
      // this PageIndex does not exist. This should only happen if AIndex<0
      {if AIndex>=0 then
      begin
        Debugln(['TCarbonTabsControl.SetPageIndex unknown pageindex: ',AIndex]);
      end;}
      ATabIndex := -1;
      SetControl32BitValue(ControlRef(Widget), 0);
      ShowTab;
      Exit;
    end;

    FTabIndex := ATabIndex;
    if (ATabIndex < FFirstIndex) or (ATabIndex > FLastIndex) then
    begin
      FFirstIndex := ATabIndex;
      UpdateTabs;
      ShowTab;
    end
    else
      UpdateTabIndex;
  finally
    EnableChangeEvent;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.ShowTabs
  Params:  AShow - Show/hide

  Shows/hides all Carbon tabs
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.ShowTabs(AShow: Boolean);
var
  I: Integer;
  Notebook: TCustomNotebook;
  Page: TCustomPage;
begin
  if FShowTabBar <> AShow then
  begin
    RecreateWnd(LCLObject);
    Exit;
  end
  else
    FShowTabBar := AShow;

  Notebook := LCLObject as TCustomNotebook;
  for I := 0 to Notebook.PageCount - 1 do
  begin
    Page := Notebook.Page[I];
    //DebugLn('TCarbonTabsControl.ShowTabs True ' + DbgS(I) + ' Handle ' +
    //  DbgS(Page.Handle) + ' TabVisible: ' + DbgS(Page.TabVisible));

    if Page.TabVisible or (csDesigning in Page.ComponentState) then
    begin
      if FTabs.IndexOf(TCarbonTab(Page.Handle)) < 0 then
      begin
        FTabs.Insert(Page.VisibleIndex, TCarbonTab(Page.Handle));
        TCarbonTab(Page.Handle).Attach(Self);
      end;
    end;
  end;

  UpdateTabs;
  ShowTab;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.SetTabPosition
  Params:  ATabPosition - New position of tabs

  Changes position of the tabs
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.SetTabPosition(ATabPosition: TTabPosition);
begin
  if FTabPosition <> ATabPosition then RecreateWnd(LCLObject);
end;

end.

