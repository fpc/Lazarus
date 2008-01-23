{ $Id$
                  ----------------------------------------------
                  carbontabs.pp  -  Carbon tabs Control and tabs
                  ----------------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  FPCMacOSAll,
 // widgetset
  WSControls, WSLCLClasses, WSProc,
 // LCL Carbon
  CarbonDef, CarbonPrivate,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, ExtCtrls;
  
type
  TCarbonTabsControl = class;

  { TCarbonTab }

  TCarbonTab = class(TCarbonCustomControl)
  private
    FParent: TCarbonTabsControl;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
  public
    procedure Attach(AParent: TCarbonTabsControl);
    procedure UpdateTab;
    function SetText(const S: String): Boolean; override;
  end;

  { TCarbonTabsControl }

  TCarbonTabsControl = class(TCarbonControl)
  private
    FUserPane: ControlRef;
    FTabPosition: TTabPosition;
    FTabs: TObjectList; // of TCarbonTab
    FTabIndex: Integer;
    FFirstIndex: Integer; // index of first visible tab
    FLastIndex: Integer;  // index of last visible tab
    FPrevArrow: ControlRef;
    FNextArrow: ControlRef;
    function GetPrevArrowBounds(const R: TRect): TRect;
    function GetNextArrowBounds(const R: TRect): TRect;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetContent: ControlRef; override;
    
    procedure ShowTab;
    procedure UpdateTabs;
    procedure Remove(ATab: TCarbonTab);
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure ValueChanged; override;
  public
    function GetClientRect(var ARect: TRect): Boolean; override;
    function SetBounds(const ARect: TRect): Boolean; override;
    
    procedure ScrollTabsLeft;
    procedure ScrollTabsRight;

    procedure Add(ATab: TCarbonTab; AIndex: Integer);
    procedure Remove(AIndex: Integer);
    procedure SetTabIndex(AIndex: Integer);
    procedure ShowTabs(AShow: Boolean);
    procedure SetTabPosition(ATabPosition: TTabPosition);
  end;


implementation

uses CarbonProc, CarbonDbgConsts, CarbonUtils, CarbonCanvas, CarbonGDIObjects;

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
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTab.DestroyWidget

  Clean-up
 ------------------------------------------------------------------------------}
procedure TCarbonTab.DestroyWidget;
begin
  FParent.Remove(Self);

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
  if FParent = nil then Exit;
  
  Result := False;
  FParent.UpdateTabs;
  Result := True;
end;

{ TCarbonTabsControl }

{------------------------------------------------------------------------------
  Name: CarbonTabsPrevArrow_Hit
 ------------------------------------------------------------------------------}
function CarbonTabsPrevArrow_Hit(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonTabsPrevArrow_Hit: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  Result := CallNextEventHandler(ANextHandler, AEvent);

  (AWidget as TCarbonTabsControl).ScrollTabsLeft;
end;

{------------------------------------------------------------------------------
  Name: CarbonTabsNextArrow_Hit
 ------------------------------------------------------------------------------}
function CarbonTabsNextArrow_Hit(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseControlEvent}
    DebugLn('CarbonTabsNextArrow_Hit: ', DbgSName(AWidget.LCLObject));
  {$ENDIF}

  Result := CallNextEventHandler(ANextHandler, AEvent);

  (AWidget as TCarbonTabsControl).ScrollTabsRight;
end;

const
  ArrowSize = 16;
  TabHeight = 10;
  TabWidth = 8;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.GetPrevArrowBounds
  Returns: Bounds of prev arrow
 ------------------------------------------------------------------------------}
function TCarbonTabsControl.GetPrevArrowBounds(const R: TRect): TRect;
var
  P: TPoint;
begin
  P := Classes.Point(R.Left, R.Top + TabHeight);
  Result := Classes.Rect(P.X, P.Y,
    P.X + ArrowSize, P.Y + ArrowSize);
end;


{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.GetNextArrowBounds
  Returns: Bounds of next arrow
 ------------------------------------------------------------------------------}
function TCarbonTabsControl.GetNextArrowBounds(const R: TRect): TRect;
var
  P: TPoint;
begin
  P := Classes.Point(R.Right - ArrowSize - TabWidth, R.Top + TabHeight);
  Result := Classes.Rect(P.X, P.Y,
    P.X + ArrowSize, P.Y + ArrowSize);
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
begin
  case (LCLObject as TCustomNotebook).TabPosition of
  tpTop: Direction := kControlTabDirectionNorth;
  tpBottom: Direction := kControlTabDirectionSouth;
  tpRight: Direction := kControlTabDirectionEast;
  tpLeft: Direction := kControlTabDirectionWest;
  end;
  if OSError(
    CreateTabsControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      kControlTabSizeLarge, Direction, 0, TabEntry, Control),
    Self, SCreateWidget, 'CreateTabsControl') then RaiseCreateWidgetError(LCLObject);
    
  FTabPosition := (LCLObject as TCustomNotebook).TabPosition;
  FTabs := TObjectList.Create(False);

  Widget := Control;
  
  OSError(
    CreateDisclosureTriangleControl(GetTopParentWindow,
      GetCarbonRect(GetPrevArrowBounds(ParamsToRect(AParams))),
      kControlDisclosureTrianglePointLeft, nil, 0, False, False, FPrevArrow),
    Self, SCreateWidget, 'CreatePopupArrowControl');
  OSError(HIViewSetVisible(FPrevArrow, False), Self, SCreateWidget, SViewVisible);
  OSError(HIViewAddSubview(Widget, FPrevArrow), Self, SCreateWidget,
    SViewAddView);
    
  OSError(
    CreateDisclosureTriangleControl(GetTopParentWindow,
      GetCarbonRect(GetNextArrowBounds(ParamsToRect(AParams))),
      kControlDisclosureTrianglePointRight, nil, 0, False, False, FNextArrow),
    Self, SCreateWidget, 'CreatePopupArrowControl');
  OSError(HIViewSetVisible(FNextArrow, False), Self, SCreateWidget, SViewVisible);
  OSError(HIViewAddSubview(Widget, FNextArrow), Self, SCreateWidget,
    SViewAddView);
    
  AddControlPart(FPrevArrow);
  AddControlPart(FNextArrow);
  
  TmpSpec := MakeEventSpec(kEventClassControl, kEventControlHit);
  InstallControlEventHandler(FPrevArrow,
    RegisterEventHandler(@CarbonTabsPrevArrow_Hit),
    1, @TmpSpec, Pointer(Self), nil);
  InstallControlEventHandler(FNextArrow,
    RegisterEventHandler(@CarbonTabsNextArrow_Hit),
    1, @TmpSpec, Pointer(Self), nil);

  if not GetClientRect(R) then
  begin
    DebugLn('TCarbonTabsControl.CreateWidget Error - no content region!');
    Exit;
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
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.DestroyWidget

  Frees Carbon tabs control
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.DestroyWidget;
begin
  DisposeControl(FUserPane);
  FreeAndNil(FTabs);

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
  Page: TCustomPage;
begin
  // show tab with FTabIndex, hide the others
  for I := 0 to FTabs.Count - 1 do
  begin
    Page := TCarbonTab(FTabs[I]).LCLObject as TCustomPage;
    if Page.PageIndex = FTabIndex then // update tab bounds
    begin
      GetClientRect(R);
      OffsetRect(R, -R.Left, -R.Top);
      TCarbonTab(FTabs[I]).SetBounds(R);
    end;
    
    TCarbonTab(FTabs[I]).ShowHide(Page.PageIndex = FTabIndex);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.UpdateTabs

  Updates tabs properties
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.UpdateTabs;
var
  I: Integer;
  TabSizes: Array of Integer;
  S: String;
  Size: TSize;
  ControlSize: Integer;
  TempFont: TCarbonFont;
  TabInfo: ControlTabInfoRecV1;
  ControlIndex: Integer;
const
  SName = 'UpdateTabs';
begin
  try
    if FTabs.Count = 0 then
    begin
      FFirstIndex := 0;
      FLastIndex := 0;

      // set tab count
      SetControl32BitMaximum(ControlRef(Widget), 0);
      ControlIndex := 0;
      Exit;
    end;

    SetLength(TabSizes, FTabs.Count);
    TempFont := DefaultContext.CurrentFont;
    DefaultContext.CurrentFont := TCarbonFont(LCLObject.Font.Reference.Handle);
    try
      for I := 0 to High(TabSizes) do
      begin
        S := TCarbonTab(FTabs[I]).LCLObject.Caption;
        DeleteAmpersands(S);
        if DefaultContext.GetTextExtentPoint(PChar(S), Length(S), Size) then
          TabSizes[I] := Size.cx + 20
        else
          TabSizes[I] := 20;
      end;
    finally
      DefaultContext.CurrentFont := TempFont;
    end;

    if FFirstIndex < 0 then FFirstIndex := 0;
    if FFirstIndex >= FTabs.Count then FFirstIndex := FTabs.Count - 1;
    FLastIndex := FFirstIndex;

    if FTabPosition in [tpTop, tpBottom] then ControlSize := LCLObject.Width
    else ControlSize := LCLObject.Height;
    ControlSize := ControlSize - 2 * ArrowSize - TabSizes[FFirstIndex];

    // add tabs right from first
    for I := FFirstIndex + 1 to FTabs.Count - 1 do
    begin
      if ControlSize >= TabSizes[I] then
      begin
        FLastIndex := I;
        Dec(ControlSize, TabSizes[I]);
      end;
    end;

    // possibly add tabs left from first
    for I := FFirstIndex - 1 downto 0 do
    begin
      if ControlSize >= TabSizes[I] then
      begin
        FFirstIndex := I;
        Dec(ControlSize, TabSizes[I]);
      end;
    end;

    // set tab count
    SetControl32BitMaximum(ControlRef(Widget), FLastIndex - FFirstIndex + 1);

    ControlIndex := 0;
    // update tabs
    TabInfo.version := kControlTabInfoVersionOne;
    TabInfo.iconSuiteID := 0;

    // TODO: imageindex
    for I := FFirstIndex to FLastIndex do
    begin
      S := TCarbonTab(FTabs[I]).LCLObject.Caption;
      if (TCarbonTab(FTabs[I]).LCLObject as TCustomPage).PageIndex = FTabIndex then
        ControlIndex := I - FFirstIndex + 1;

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
    // set tab index
    SetControl32BitValue(ControlRef(Widget), ControlIndex);
    // update arrows visible
    OSError(HIViewSetVisible(FPrevArrow, (FFirstIndex > 0) and (FTabPosition = tpTop)), Self, SName, SViewVisible);
    OSError(HIViewSetVisible(FNextArrow, (FLastIndex < FTabs.Count - 1) and (FTabPosition = tpTop)), Self, SName, SViewVisible);
    
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.Remove
  
  Removes the specified tab
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.Remove(ATab: TCarbonTab);
begin
  FTabs.Remove(ATab);
  UpdateTabs;
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
  Index := GetControl32BitValue(ControlRef(Widget)) - 1 + FFirstIndex;

  if (Index >= 0) and (Index < FTabs.Count) then
    PIndex := (TCarbonTab(FTabs[Index]).LCLObject as TCustomPage).PageIndex
  else
    PIndex := -1;

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
    SetTabIndex((LCLObject as TCustomNoteBook).PageIndex);
    Exit;
  end;

  SetTabIndex(Index);

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

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.GetClientRect
  Params:  ARect - Record for client area coordinates
  Returns: If the function succeeds

  Returns the tabs control client rectangle relative to control origin
 ------------------------------------------------------------------------------}
function TCarbonTabsControl.GetClientRect(var ARect: TRect): Boolean;
var
  AClientRect: FPCMacOSAll.Rect;
begin
  Result := False;
  
  //DebugLn('TCarbonTabsControl.GetClientRect');

  if OSError(GetControlData(ControlRef(Widget), kControlEntireControl,
      kControlTabContentRectTag, SizeOf(FPCMacOSAll.Rect), @AClientRect, nil),
    Self, 'GetClientRect', 'GetControlData') then Exit;

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
begin
  Result := False;
  if inherited SetBounds(ARect) then
  begin
    UpdateContentBounds;
    
    OSError(HIViewSetFrame(FPrevArrow, RectToCGRect(GetPrevArrowBounds(ARect))),
      Self, SSetBounds, SViewFrame);
    OSError(HIViewSetFrame(FNextArrow, RectToCGRect(GetNextArrowBounds(ARect))),
      Self, SSetBounds, SViewFrame);
    
    Result := True;
  end;
  
  UpdateTabs;
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
  Inc(FFirstIndex);
  UpdateTabs;
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
  if FTabs.IndexOf(ATab) < 0 then
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
  //DebugLn('TCarbonTabsControl.Remove ' + DbgS(AIndex));
  FTabs.Delete(AIndex);
  
  UpdateTabs;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.SetTabIndex
  Params:  AIndex - New index

  Changes the current Carbon tab
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.SetTabIndex(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FTabs.Count) then
  begin
    SetControl32BitValue(ControlRef(Widget), 0);
    ShowTab;
    Exit;
  end;
  
  //DebugLn('TCarbonTabsControl.SetTabIndex ' + DbgS(AIndex) + ' ' + DbgS((LCLObject as TCustomNotebook).PageCount));
  
  FTabIndex := AIndex;
  if (AIndex < FFirstIndex) or (AIndex > FLastIndex) then
  begin
    FFirstIndex := AIndex;
    UpdateTabs;
  end
  else
  begin
    SetControl32BitValue(ControlRef(Widget), FTabIndex - FFirstIndex + 1);
    Invalidate;
  end;

  
  ShowTab;
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
  if AShow then // add all tabs
  begin
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
  end
  else FTabs.Clear; // remove all tabs
  
  UpdateTabs;
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

