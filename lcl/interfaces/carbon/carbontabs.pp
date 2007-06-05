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
    function GetIndex: Integer;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
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
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    function GetContent: ControlRef; override;
    
    procedure ShowTab;
    procedure UpdateTabs(AIndex: Integer; TillEnd: Boolean = False);
    procedure SetTabCaption(AIndex: Integer; const S: String);
  public
    class function GetValidEvents: TCarbonControlEvents; override;
    procedure ValueChanged; override;
  public
    function GetClientRect(var ARect: TRect): Boolean; override;
    function SetBounds(const ARect: TRect): Boolean; override;
    procedure Add(ATab: TCarbonTab; AIndex: Integer);
    procedure Remove(AIndex: Integer);
    procedure SetTabIndex(AIndex: Integer);
    procedure ShowTabs(AShow: Boolean);
    procedure SetTabPosition(ATabPosition: TTabPosition);
  end;


implementation

uses CarbonProc, CarbonDbgConsts, CarbonUtils;

{ TCarbonTab }

{------------------------------------------------------------------------------
  Method:  TCarbonTab.GetIndex
  Returns: The real index of tab
 ------------------------------------------------------------------------------}
function TCarbonTab.GetIndex: Integer;
begin
  if FParent <> nil then Result := FParent.FTabs.IndexOf(Self)
  else
  begin
    Result := -1;
    DebugLn('TCarbonTab.GetIndex Error - tab ' + LCLObject.Name +
      ' is not attached to parent!');
  end;
end;

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
  
  FParent.UpdateTabs(GetIndex);
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
  FParent.SetTabCaption(GetIndex, S);
  Result := True;
end;

{ TCarbonTabsControl }

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
begin
  case (LCLObject as TCustomNotebook).TabPosition of
  tpTop:    Direction := kControlTabDirectionNorth;
  tpBottom: Direction := kControlTabDirectionSouth;
  tpLeft:   Direction := kControlTabDirectionWest;
  tpRight:  Direction := kControlTabDirectionEast;
  end;

  if OSError(
    CreateTabsControl(GetTopParentWindow, ParamsToCarbonRect(AParams),
      kControlTabSizeLarge, Direction, 0, TabEntry, Control),
    Self, SCreateWidget, 'CreateTabsControl') then RaiseCreateWidgetError(LCLObject);
    
  Widget := Control;
  
  if not GetClientRect(R) then
  begin
    DebugLn('TCarbonTabsControl.CreateWidget Error - no content region!');
    Exit;
  end;

  FUserPane := CreateCustomHIView(RectToCGRect(R));
  if FUserPane = nil then RaiseCreateWidgetError(LCLObject);

  OSError(HIViewSetVisible(FUserPane, True), Self, SCreateWidget, SViewVisible);

  if OSError(HIViewAddSubview(Control, FUserPane), Self, SCreateWidget,
    SViewAddView) then RaiseCreateWidgetError(LCLObject);

  inherited;

  FTabPosition := (LCLObject as TCustomNotebook).TabPosition;
  
  FTabs := TObjectList.Create(False);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.DestroyWidget

  Frees Carbon tabs control
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.DestroyWidget;
begin
  FTabs.Free;
  
  DisposeControl(FUserPane);

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
  I, VIndex: Integer;
begin
  VIndex := GetControl32BitValue(ControlRef(Widget)) - 1;

  // show tab with VIndex, hide the others
  for I := 0 to FTabs.Count - 1 do
    TCarbonTab(FTabs[I]).ShowHide(I = VIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.UpdateTabs
  Params:  AIndex  - Start index
           TillEnd - Till end

  Updates tabs properties
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.UpdateTabs(AIndex: Integer; TillEnd: Boolean);
begin
  // update tabs count
  SetControl32BitMaximum(ControlRef(Widget), FTabs.Count);
  
  // TODO imageindex
  while AIndex < FTabs.Count do
  begin
    //DebugLn('TCarbonTabsControl.UpdateTabs ' + DbgS(AIndex) + ' Caption: ' +
    //  TCarbonTab(FTabs[AIndex]).LCLObject.Caption);
      
    SetTabCaption(AIndex, TCarbonTab(FTabs[AIndex]).LCLObject.Caption);
    
    if not TillEnd then Exit;
    
    Inc(AIndex);
  end;
  
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.SetTabCaption
  Params:  AIndex - Start index
           S      - New caption

  Changes the specified tab caption
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.SetTabCaption(AIndex: Integer; const S: String);
var
  Info: ControlTabInfoRecV1;
  T: String;
begin
  Info.version := kControlTabInfoVersionOne;
  Info.iconSuiteID := 0;

  T := S;
  DeleteAmpersands(T);
  
  CreateCFString(T, Info.name);
  try
    if OSError(SetControlData(ControlRef(Widget), AIndex + 1, kControlTabInfoTag,
        SizeOf(ControlTabInfoRecV1), @Info),
      Self, 'SetTabCaption', 'SetControlData') then Exit;
  finally
    FreeCFString(Info.name);
  end;
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
  VIndex, PIndex: Integer;
begin
  VIndex := GetControl32BitValue(ControlRef(Widget)) - 1;
  if (VIndex >= 0) and (VIndex < FTabs.Count) then
    PIndex := (TCarbonTab(FTabs[VIndex]).LCLObject as TCustomPage).PageIndex
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

  ShowTab;

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
    Result := True;
  end;
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
  
  UpdateTabs(AIndex, True);
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
  
  UpdateTabs(AIndex, True);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonTabsControl.SetTabIndex
  Params:  AIndex - New index

  Changes the current Carbon tab
 ------------------------------------------------------------------------------}
procedure TCarbonTabsControl.SetTabIndex(AIndex: Integer);
var
  VIndex: Integer;
begin
  if (AIndex < 0) or (AIndex >= (LCLObject as TCustomNotebook).PageCount) then
  begin
    SetControl32BitValue(ControlRef(Widget), 0);
    ShowTab;
    Exit;
  end;
  
  //DebugLn('TCarbonTabsControl.SetTabIndex ' + DbgS(AIndex) + ' ' + DbgS((LCLObject as TCustomNotebook).PageCount));
  
  VIndex := (LCLObject as TCustomNotebook).Page[AIndex].VisibleIndex;
  SetControl32BitValue(ControlRef(Widget), VIndex + 1);
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
  
  UpdateTabs(0, True);
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

