unit CocoaWSComCtrls;

interface

{$mode delphi}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

{.$DEFINE COCOA_DEBUG_TABCONTROL}
{.$DEFINE COCOA_DEBUG_LISTVIEW}

uses
  Classes, Math, SysUtils,
  LCLType, Controls, ComCtrls, LCLMessageGlue, LMessages,
  WSComCtrls,
  MacOSAll, CocoaAll,
  CocoaPrivate, CocoaCallback, CocoaWSCommon, CocoaGDIObjects, CocoaUtils,
  CocoaTabControls, CocoaButtons, CocoaStatusBar;

type

  { TCocoaWSStatusBar }

  TCocoaWSStatusBar = class(TWSStatusBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    //
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TCocoaWSTabSheet }

  TCocoaWSTabSheet = class(TWSTabSheet)
  published
  end;

  { TLCLTabControlCallback }

  TLCLTabControlCallback = class(TLCLCommonCallback, ITabControlCallback)
    function shouldSelectTabViewItem(aTabIndex: Integer): Boolean;
    procedure willSelectTabViewItem(aTabIndex: Integer);
    procedure didSelectTabViewItem(aTabIndex: Integer);
  private
    procedure sengNotifyMsg(aTabIndex:Integer; aCode:Integer);
  end;

  { TCocoaWSCustomPage }

  TCocoaWSCustomPage = class(TWSCustomPage)
  public
    class function  GetCocoaTabPageFromHandle(AHandle: HWND): TCocoaTabPage;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetProperties(const ACustomPage: TCustomPage; ACocoaControl: NSTabViewItem);
    //
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
  end;

  { TCocoaWSCustomTabControl }

  TCocoaWSCustomTabControl = class(TWSCustomTabControl)
  private
    class function LCLTabPosToNSTabStyle(AShowTabs: Boolean; ABorderWidth: Integer; ATabPos: TTabPosition): NSTabViewType;
  public
    class function  GetCocoaTabControlHandle(ATabControl: TCustomTabControl): TCocoaTabControl;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;

    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;

    class procedure AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ATabControl: TCustomTabControl; const AIndex: integer); override;

    //class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    //class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    //class function GetPageRealIndex(const ATabControl: TCustomTabControl; AIndex: Integer): Integer; override;
    class function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; override;
    class function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect; override;
    class procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); override;
    class procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); override;

    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
      const AOldPos, ANewPos: Integer; const AChildren: TFPList); override;
  end;

  { TCocoaWSPageControl }

  TCocoaWSPageControl = class(TWSPageControl)
  published
  end;

  { TCocoaWSProgressBar }

  TCocoaWSProgressBar = class(TWSProgressBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); override;
  end;

  { TCocoaWSCustomUpDown }

  TCocoaWSCustomUpDown = class(TWSCustomUpDown)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetIncrement(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetMaxPosition(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetMinPosition(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetPosition(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetWrap(const AUpDown: TCustomUpDown; ADoWrap: Boolean); override;
  end;

  { TCarbonWSUpDown }

  TCarbonWSUpDown = class(TWSUpDown)
  published
  end;

  { TCocoaWSToolButton }

  TCocoaWSToolButton = class(TWSToolButton)
  published
  end;

  { TCarbonWSToolBar }

  TCarbonWSToolBar = class(TWSToolBar)
  published
    //class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
  end;

  { TCocoaWSTrackBar }

  TCocoaWSTrackBar = class(TWSTrackBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const {%H-}NewPosition: integer); override;
    class procedure SetOrientation(const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation); override;
    class procedure SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TCocoaWSCustomTreeView }

  TCocoaWSCustomTreeView = class(TWSCustomTreeView)
  published
  end;

  { TCocoaWSTreeView }

  TCocoaWSTreeView = class(TWSTreeView)
  published
  end;

implementation

type

  { TUpdownCommonCallback }

  TUpdownCommonCallback = class(TLCLCommonCallback, IStepperCallback)
    procedure BeforeChange(var Allowed: Boolean);
    procedure Change(NewValue: Double; isUpPressed: Boolean; var Allowed: Boolean);
    procedure UpdownClick(isUpPressed: Boolean);
  end;

type
  TAccessUpDown = class(TCustomUpDown);

{ TUpdownCommonCallback }

procedure TUpdownCommonCallback.BeforeChange(var Allowed: Boolean);
begin
  if Assigned( TAccessUpDown(Target).OnChanging ) then
    TAccessUpDown(Target).OnChanging(Target, Allowed);
end;

procedure TUpdownCommonCallback.Change(NewValue: Double; isUpPressed: Boolean;
  var Allowed: Boolean);
const
  UpDownDir : array [Boolean] of TUpDownDirection = (updUp, updDown);
begin
  if Assigned( TAccessUpDown(Target).OnChangingEx ) then
    TAccessUpDown(Target).OnChangingEx(Target, Allowed,
      Round(NewValue), UpDownDir[isUpPressed]);
end;

procedure TUpdownCommonCallback.UpdownClick(isUpPressed: Boolean);
const
  UpDownBtn : array [Boolean] of TUDBtnType = (btPrev, btNext);
begin
  TAccessUpDown(Target).Position := NSStepper(Owner).intValue;
  if Assigned( TAccessUpDown(Target).OnClick ) then
    TAccessUpDown(Target).OnClick( Target, UpDownBtn[isUpPressed]);
end;

{ TCocoaWSCustomUpDown }

class function TCocoaWSCustomUpDown.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaStepper;
begin
  lResult := TCocoaStepper.alloc.lclInitWithCreateParams(AParams);
  if Assigned(lResult) then
  begin
    lResult.callback := TUpdownCommonCallback.Create(lResult, AWinControl);
    //small constrol size looks like carbon
    //lResult.setControlSize(NSSmallControlSize);
    lResult.setTarget(lResult);
    lResult.setAction(objcselector('stepperAction:'));
    lResult.setMinValue( 0 );
    lResult.setMaxValue( 100 );
    lResult.setValueWraps( False );
    lResult.setAutorepeat( True );
  end;
  Result := TLCLHandle(lResult);
end;

class procedure TCocoaWSCustomUpDown.SetMinPosition(
  const AUpDown: TCustomUpDown; AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setMinValue(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetMaxPosition(
  const AUpDown: TCustomUpDown; AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setMaxValue(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetPosition(const AUpDown: TCustomUpDown;
  AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).lastValue := AValue;
  TCocoaStepper(AUpDown.Handle).setDoubleValue(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetIncrement(const AUpDown: TCustomUpDown;
  AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setIncrement(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetWrap(const AUpDown: TCustomUpDown;
  ADoWrap: Boolean);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setValueWraps(ADoWrap);
end;

{ TLCLTabControlCallback }

function TLCLTabControlCallback.shouldSelectTabViewItem(aTabIndex: Integer): Boolean;
begin
  Result:= NOT TTabControl(Target).Dragging;
end;

procedure TLCLTabControlCallback.sengNotifyMsg(aTabIndex:Integer; aCode:Integer);
var
  Msg: TLMNotify;
  Hdr: TNmHdr;
begin
  if aTabIndex<0 then exit;

  FillChar(Msg, SizeOf(Msg), 0);
  Msg.Msg := LM_NOTIFY;
  FillChar(Hdr, SizeOf(Hdr), 0);

  Hdr.hwndFrom := Target.Handle;
  Hdr.Code := aCode;
  Hdr.idFrom := TTabControl(Target).TabToPageIndex(ATabIndex);
  Msg.NMHdr := @Hdr;
  Msg.Result := 0;
  LCLMessageGlue.DeliverMessage(Target, Msg);
end;

procedure TLCLTabControlCallback.willSelectTabViewItem(aTabIndex: Integer);
begin
  sengNotifyMsg(aTabIndex, TCN_SELCHANGING);
end;

procedure TLCLTabControlCallback.didSelectTabViewItem(aTabIndex: Integer);
begin
  sengNotifyMsg(aTabIndex, TCN_SELCHANGE);
end;

{ TCocoaWSStatusBar }

class function TCocoaWSStatusBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaStatusBar;
  cell   : NSButtonCell;
  cb : TStatusBarCallback;
begin
  Result := 0;
  lResult := TCocoaStatusBar.alloc.lclInitWithCreateParams(AParams);
  if not Assigned(lResult) then Exit;
  Result := TLCLHandle(lResult);

  cb := TStatusBarCallback.Create(lResult, AWinControl);
  lResult.callback := cb;
  lResult.barcallback := cb;
  cb.BlockCocoaUpDown := true;
  //lResult.StatusBar := TStatusBar(AWinControl);

  //todo: get rid of Cells and replace them with views!
  cell:=NSButtonCell(NSButtonCell.alloc).initTextCell(nil);
  // NSSmallSquareBezelStyle aka "Gradient button", is the best looking
  // candidate for the status bar panel. Could be changed to any NSCell class
  // since CocoaStatusBar doesn't suspect any specific cell type.
  cell.setBezelStyle(NSSmallSquareBezelStyle);
  cell.setFont( NSFont.systemFontOfSize( NSFont.smallSystemFontSize ));

  cell.setLineBreakMode(NSLineBreakByClipping);
  //cell.setLineBreakMode(NSLineBreakByTruncatingTail);

  lResult.panelCell := cell;
end;

class procedure TCocoaWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  // todo: can make more effecient
  Update(AStatusBar);
end;

class procedure TCocoaWSStatusBar.SetPanelText(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  Update(AStatusBar);
end;

class procedure TCocoaWSStatusBar.Update(const AStatusBar: TStatusBar);
begin
  if not Assigned(AStatusBar) or not (AStatusBar.HandleAllocated) then Exit;
  {$ifdef BOOLFIX}
  TCocoaStatusBar(AStatusBar.Handle).setNeedsDisplay__(Ord(true));
  {$else}
  TCocoaStatusBar(AStatusBar.Handle).setNeedsDisplay_(true);
  {$endif}
end;

class procedure TCocoaWSStatusBar.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;
  PreferredHeight := STATUSBAR_DEFAULT_HEIGHT;
end;

{ TCocoaWSCustomPage }

class function  TCocoaWSCustomPage.GetCocoaTabPageFromHandle(AHandle: HWND): TCocoaTabPage;
var
  lHandle: TCocoaTabPageView;
begin
  lHandle := TCocoaTabPageView(AHandle);
  Result := lHandle.tabPage;
end;

class function TCocoaWSCustomPage.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  lControl: TCocoaTabPage;
  tv: TCocoaTabPageView;
  tabview: TCocoaTabControl;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomPage.CreateHandle]');
  {$ENDIF}
  lControl := TCocoaTabPage.alloc().init();
  Result := TLCLHandle(lControl);
  if Result <> 0 then
  begin
    //lControl.callback := TLCLCommonCallback.Create(lControl, AWinControl);
    SetProperties(TCustomPage(AWinControl), lControl);

    // Set a special view for the page
    // based on http://stackoverflow.com/questions/14892218/adding-a-nstextview-subview-to-nstabviewitem
    tabview := TCocoaTabControl(AWinControl.Parent.Handle);
    tabview.setAllowsTruncatedLabels(false);
    tv := TCocoaTabPageView.alloc.initWithFrame(NSZeroRect);
    tv.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
    {tv.setHasVerticalScroller(True);
    tv.setHasHorizontalScroller(True);
    tv.setAutohidesScrollers(True);
    tv.setBorderType(NSNoBorder);}
    tv.tabView := tabview;
    tv.tabPage := lControl;
    tv.callback := TLCLCommonCallback.Create(tv, AWinControl);
    TLCLCommonCallback(tv.callback.GetCallbackObject).BlockCocoaUpDown := true;
    lControl.callback := tv.callback;
    lControl.setView(tv);
    UpdateControlFocusRing( tabview, AWinControl );

    Result := TLCLHandle(tv);
  end;
end;

class procedure TCocoaWSCustomPage.DestroyHandle(const AWinControl: TWinControl);
var
  tv: TCocoaTabPageView;
  ndx: NSInteger;
begin
  tv := TCocoaTabPageView(AWinControl.Handle);
  ndx := tv.tabView.exttabIndexOfTabViewItem(tv.tabPage);
  if (ndx >= 0) and (ndx < tv.tabView.fulltabs.count) then
    tv.tabview.exttabRemoveTabViewItem(tv.tabPage);
  TCocoaWSWinControl.DestroyHandle(AWinControl);
end;

class procedure TCocoaWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
var
  lTabPage: TCocoaTabPage;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.UpdateProperties] ACustomPage='+IntToStr(PtrInt(ACustomPage)));
  {$ENDIF}
  if not Assigned(ACustomPage) or not ACustomPage.HandleAllocated then Exit;
  lTabPage := GetCocoaTabPageFromHandle(ACustomPage.Handle);
  if Assigned(lTabPage) then SetProperties(ACustomPage, lTabPage);
end;

class procedure TCocoaWSCustomPage.SetProperties(
  const ACustomPage: TCustomPage; ACocoaControl: NSTabViewItem);
var
  lHintStr: string;
begin
  // title
  ACocoaControl.setLabel(ControlTitleToNSStr(ACustomPage.Caption));

  // hint
  if ACustomPage.ShowHint then lHintStr := ACustomPage.Hint
  else lHintStr := '';
  ACocoaControl.setToolTip(StrToNSString(lHintStr));
end;

class procedure TCocoaWSCustomPage.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  // Pages should be fixed into their PageControl owner,
  // allowing the TCocoaWSWinControl.SetBounds function to operate here
  // was causing bug 28489
end;

class procedure TCocoaWSCustomPage.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  lTitle: String;
  page  : TCocoaTabPage;
begin
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then Exit;

  page := GetCocoaTabPageFromHandle(AWinControl.Handle);
  if not Assigned(page) then Exit;
  page.setLabel(ControlTitleToNSStr(AText));

  if (AWinControl.Parent <> nil)
    and (AWinControl.Parent is TCustomTabControl)
    and (AWinControl.HandleAllocated)
  then
    UpdateTabAndArrowVisibility( TCocoaTabControl(AWinControl.Parent.Handle) );
end;

class function TCocoaWSCustomPage.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  page  : TCocoaTabPage;
begin
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then
  begin
    Result := false;
    Exit;
  end;

  page := GetCocoaTabPageFromHandle(AWinControl.Handle);
  AText := NSStringToString( page.label_ );
  Result := true;
end;

{ TCocoaWSCustomTabControl }

class function TCocoaWSCustomTabControl.LCLTabPosToNSTabStyle(AShowTabs: Boolean; ABorderWidth: Integer; ATabPos: TTabPosition): NSTabViewType;
begin
  Result := NSTopTabsBezelBorder;
  if AShowTabs then
  begin
    case ATabPos of
    tpTop:    Result := NSTopTabsBezelBorder;
    tpBottom: Result := NSBottomTabsBezelBorder;
    tpLeft:   Result := NSLeftTabsBezelBorder;
    tpRight:  Result := NSRightTabsBezelBorder;
    end;
  end
  else
  begin
    if ABorderWidth = 0 then
      Result := NSNoTabsNoBorder
    else if ABorderWidth = 1 then
      Result := NSNoTabsLineBorder
    else
      Result := NSNoTabsBezelBorder;
  end;
end;

class function TCocoaWSCustomTabControl.GetCocoaTabControlHandle(ATabControl: TCustomTabControl): TCocoaTabControl;
begin
  Result := nil;
  if ATabControl = nil then Exit;
  if not ATabControl.HandleAllocated then Exit;
  Result := TCocoaTabControl(ATabControl.Handle);
end;

class function TCocoaWSCustomTabControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
  function CreateTabControl: TLCLHandle;
  var
    lControl: TCocoaTabControl;
    lTabControl: TCustomTabControl = nil;
    lTabStyle: NSTabViewType = NSTopTabsBezelBorder;
  begin
    lTabControl := TCustomTabControl(AWinControl);
    lControl := TCocoaTabControl.alloc.lclInitWithCreateParams(AParams);
    lTabStyle := LCLTabPosToNSTabStyle(lTabControl.ShowTabs, lTabControl.BorderWidth, lTabControl.TabPosition);
    lControl.setTabViewType(lTabStyle);
    lControl.lclEnabled := AWinControl.Enabled;
    Result := TLCLHandle(lControl);
    if Result <> 0 then
    begin
      lControl.callback := TLCLTabControlCallback.Create(lControl, AWinControl);
      lControl.setDelegate(lControl);
    end;
  end;

  function CreateContainer: TLCLHandle;
  var
    contianer: TCocoaTabControlContainer;
  begin
    contianer:= TCocoaTabControlContainer.new;
    contianer.lclInitWithCreateParams( AParams );
    Result:= TLCLHandle( contianer );
  end;

begin
  if AWinControl is TTabControl then
    Result:= CreateContainer
  else
    Result:= CreateTabControl;
end;

class procedure TCocoaWSCustomTabControl.SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer);
begin
  // because TCocoaWSCustomPage.SetBounds() is disabled
  // all Pages should be invalidated in TCocoaWSCustomTabControl.SetBounds()
  // see also: https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40296
  TCocoaWSWinControl.SetBounds( AWinControl, ALeft, ATop, AWidth, AHeight );
  AWinControl.InvalidateClientRectCache(true);
end;

class procedure TCocoaWSCustomTabControl.AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: TCocoaTabPage;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.AddPage] AChild='+IntToStr(PtrInt(AChild)));
  {$ENDIF}
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  AChild.HandleNeeded();
  if not Assigned(AChild) or not AChild.HandleAllocated then Exit;
  lTabPage := TCocoaWSCustomPage.GetCocoaTabPageFromHandle(AChild.Handle);

  lTabControl.exttabInsertTabViewItem_atIndex(lTabPage, AIndex);

  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.AddPage] END');
  {$ENDIF}
end;

class procedure TCocoaWSCustomTabControl.MovePage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const NewIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: TCocoaTabPage;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  AChild.HandleNeeded();
  if not Assigned(AChild) or not AChild.HandleAllocated then Exit;
  lTabPage := TCocoaWSCustomPage.GetCocoaTabPageFromHandle(AChild.Handle);

  lTabControl.exttabMoveTabViewItem_toIndex( lTabPage, NewIndex );
end;

class procedure TCocoaWSCustomTabControl.RemovePage(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: NSTabViewItem;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);

  lTabPage := NSTabViewItem(lTabControl.fulltabs.objectAtIndex(AIndex));
  lTabControl.exttabremoveTabViewItem(lTabPage);
end;

class function TCocoaWSCustomTabControl.GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer;
var
  lTabControl: TCocoaTabControl;
  lTabPage: NSTabViewItem;
  lClientPos: NSPoint;
  pt : TPoint;
begin
  Result := -1;
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);

  pt.x := Round(AClientPos.x + lTabControl.contentRect.origin.x);
  pt.y := Round(AClientPos.y + lTabControl.contentRect.origin.y);

  if lTabControl.isFlipped then
  begin
    lClientPos.x := pt.X;
    lClientPos.y := pt.Y;
  end
  else
    lClientPos := LCLToNSPoint(pt, lTabControl.frame.size.height);

  lTabPage := lTabControl.tabViewItemAtPoint(lClientPos);
  if not Assigned(lTabPage) then
    Exit;
  Result := lTabControl.exttabIndexOfTabViewItem(lTabPage);
end;

class function TCocoaWSCustomTabControl.GetTabRect(
  const ATabControl: TCustomTabControl; const AIndex: Integer): TRect;
var
  lTabControl: TCocoaTabControl;
  lTabPage: NSTabViewItem;
  tb : TCocoaTabPageView;
  i   : integer;
  idx : NSUInteger;
  tr  : TRect;
  w   : array of Double;
  mw  : Double;
  ofs : Double; // aprx offset between label and the text (from both sides)
  x   : Double;
  vt  : NSTabViewType;
begin
  Result:=inherited GetTabRect(ATabControl, AIndex);
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  // unable to determine the rectangle view

  if (AIndex<0) or (AIndex>=ATabControl.PageCount) then Exit;
  tb := TCocoaTabPageView(ATabControl.Page[AIndex].Handle);
  if not Assigned(tb) then Exit;

  idx := lTabControl.tabViewItems.indexOfObject( tb.tabPage );
  if idx = NSNotFound then Exit;

  if not GetTabsRect(lTabControl, tr) then Exit;

  SetLength(w, lTabControl.tabViewItems.count);
  if (length(w) = 0) then Exit; // no tabs!

  vt := lTabControl.tabViewType;
  if (vt = NSTopTabsBezelBorder) or (vt = NSBottomTabsBezelBorder) then
  begin
    mw := 0;
    for i := 0 to Integer(lTabControl.tabViewItems.count)-1 do
    begin
      lTabPage := lTabControl.tabViewItemAtIndex(i);
      w[i] := lTabPage.sizeOfLabel(false).width;
      mw := mw + w[i];
    end;
    if (mw = 0) then Exit; // 0 for the total tabs width?

    ofs := (tr.Right - tr.Left - mw) / length(w);

    x := tr.Left;
    for i := 0 to Integer(idx)-1 do
      x := x+ofs+w[i];

    Result.Left := Round(x);
    Result.Right := Round(Result.Left + w[idx]);
    Result.Top := tr.Top;
    Result.Bottom := tr.Bottom;
  end
  else
  begin
    mw := 0;
    for i := 0 to Integer(lTabControl.tabViewItems.count)-1 do
    begin
      lTabPage := lTabControl.tabViewItemAtIndex(i);
      w[i] := lTabPage.sizeOfLabel(false).height;
      mw := mw + w[i];
    end;
    if (mw = 0) then Exit; // 0 for the total tabs width?

    ofs := (tr.Bottom - tr.Top - mw) / length(w);

    x := tr.Top;
    for i := 0 to Integer(idx)-1 do
      x := x+ofs+w[i];

    Result.Left := tr.Left;
    Result.Right := tr.Right;
    Result.Top := Round(x);
    Result.Bottom := Round(Result.Top + w[idx]);
  end;
end;

class procedure TCocoaWSCustomTabControl.SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  i  : NSInteger;
  tb : TCocoaTabPageView;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.SetPageIndex]');
  {$ENDIF}
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  if (AIndex<0) or (AIndex>=ATabControl.PageCount) then Exit;
  tb := TCocoaTabPageView(ATabControl.Page[AIndex].Handle);
  if not Assigned(tb) then Exit;

  i := TCocoaTabControl(ATabControl.Handle).exttabIndexOfTabViewItem(tb.tabPage);
  if i < 0 then
    Exit;

  TCocoaTabControl(ATabControl.Handle).extselectTabViewItemAtIndex(i);
end;

class procedure TCocoaWSCustomTabControl.SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition);
var
  lTabControl: TCocoaTabControl = nil;
  lTabStyle: NSTabViewType;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);

  lTabStyle := LCLTabPosToNSTabStyle(ATabControl.ShowTabs, ATabControl.BorderWidth, ATabPosition);
  lTabControl.setTabViewType(lTabStyle);
end;

class procedure TCocoaWSCustomTabControl.ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean);
var
  lTabControl: TCocoaTabControl = nil;
  lTabStyle: NSTabViewType;
var
  pr : TRect;
  ar : TRect;
  fr : NSRect;
  dx, dy : double;
  cb: ICommonCallback;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then
    Exit;
  if ATabControl is TTabControl then
    Exit;

  lTabControl := TCocoaTabControl(ATabControl.Handle);
  lTabStyle := LCLTabPosToNSTabStyle(AShowTabs, ATabControl.BorderWidth, ATabControl.TabPosition);
  pr := lTabControl.lclGetFrameToLayoutDelta;
  lTabControl.setTabViewType(lTabStyle);
  ar := lTabControl.lclGetFrameToLayoutDelta;
  // switching ShowTabs actually changes layout to frame
  // this needs to be compenstated
  if (ar.Top<>pr.Top) or (ar.Left<>pr.Left) then
  begin
    fr := lTabControl.frame;
    dx := pr.Left - ar.left;
    dy := pr.Top - ar.Top;
    fr.origin.x := fr.origin.x + dx;
    fr.origin.y := fr.origin.y + dy;
    fr.size.width := fr.size.width - dx - (ar.Right - pr.Right);
    fr.size.height := fr.size.height - dy - (ar.Bottom - pr.Bottom);
    lTabControl.setFrame(fr);
    cb := lTabControl.lclGetCallback;
    if Assigned(cb) then cb.frameDidChange(lTabControl);
  end;
end;

class procedure TCocoaWSCustomTabControl.SetChildZPosition(const AWinControl, AChild: TWinControl;
  const AOldPos, ANewPos: Integer; const AChildren: TFPList);
begin
  // subviews of NSTabView do not need to be resorted, Cocoa will take of it.
  // avoid unnecessary performance loss.
end;

{ TCocoaWSProgressBar }

class function TCocoaWSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaProgressIndicator;
begin
  lResult := TCocoaProgressIndicator.alloc.lclInitWithCreateParams(AParams);
  if Assigned(lResult) then
  begin
    lResult.callback := TLCLCommonCallback.Create(lResult, AWinControl);
    lResult.startAnimation(nil);
    //small constrol size looks like carbon
    //lResult.setControlSize(NSSmallControlSize);
  end;
  Result := TLCLHandle(lResult);
end;

class procedure TCocoaWSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
var
  ind : NSProgressIndicator;
begin
  if not Assigned(AProgressBar) or not AProgressBar.HandleAllocated then Exit;
  ind:=NSProgressIndicator(AProgressBAr.Handle);
  ind.setMaxValue(AProgressBar.Max);
  ind.setMinValue(AProgressBar.Min);
  ind.setDoubleValue(AProgressBar.Position);
  SetStyle(AProgressBar, AProgressBar.Style);
end;

class procedure TCocoaWSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  if AProgressBar.HandleAllocated then
    NSProgressIndicator(AProgressBar.Handle).setDoubleValue(NewPosition);
end;

class procedure TCocoaWSProgressBar.SetStyle(
  const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle);
begin
  if AProgressBar.HandleAllocated then
  begin
    NSProgressIndicator(AProgressBar.Handle).setIndeterminate(NewStyle = pbstMarquee);
    NSProgressIndicator(AProgressBar.Handle).startAnimation(nil);
  end;
end;

{ TCocoaTabPage }

(*function TCocoaTabPage.lclGetCallback: ICommonCallback;
begin
  Result:=callback;
end;

procedure TCocoaTabPage.lclClearCallback;
begin
  callback:=nil;
end;

{ TCocoaTabControl }

function TCocoaTabControl.lclGetCallback: ICommonCallback;
begin
  Result:=callback;
end;

procedure TCocoaTabControl.lclClearCallback;
begin
  callback:=nil;
end; *)

{ TCocoaWSTrackBar }

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new track bar with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSTrackBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaSlider;
begin
  lResult := TCocoaSlider.alloc.lclInitWithCreateParams(AParams);
  if Assigned(lResult) then
  begin
    lResult.callback := TLCLCommonCallback.Create(lResult, AWinControl);
    lResult.setTarget(lResult);
    lResult.setAction(objcselector('sliderAction:'));
  end;
  Result := TLCLHandle(lResult);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.ApplyChanges
  Params:  ATrackBar - LCL custom track bar

  Sets the parameters (Min, Max, Position, Ticks) of slider
 ------------------------------------------------------------------------------}
class procedure TCocoaWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  lSlider: TCocoaSlider;
  lTickCount, lTrackBarLength: Integer;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  lSlider.setMaxValue(ATrackBar.Max);
  lSlider.setMinValue(ATrackBar.Min);
  lSlider.setIntValue(ATrackBar.Position);
  lSlider.intval := ATrackBar.Position;
  lSlider.setContinuous(true);
  lSlider.setAltIncrementValue(1); // forcing the slider to switch by 1 by the keyboard

  // Ticks
  if ATrackBar.TickStyle = tsAuto then
  begin
    // this should only apply to Auto
    // and for Manual it should drawn manually
    if ATrackBar.Frequency <> 0 then
      lTickCount := (ATrackBar.Max-ATrackBar.Min) div ATrackBar.Frequency + 1
    else
      lTickCount := (ATrackBar.Max-ATrackBar.Min);

    // Protection from too frequent ticks.
    // 1024 is a number of "too much" ticks, based on a common
    // screen resolution 1024 x 768
    // Protects ticks from "disappearing" when trackbar is resized
    // and is temporary too narrow to fit the trackbar
    if TickCount > 1024 then
    begin
      if ATrackBar.Orientation = trHorizontal then
        lTrackBarLength := ATrackBar.Width
      else
        lTrackBarLength := ATrackBar.Height;

      lTickCount := Min(lTickCount, lTrackBarLength);
    end;
  end else if ATrackBar.TickStyle = tsManual then
  begin
    lTickCount := 2;
  end else
    lTickCount := 0;

  lSlider.lclSetManTickDraw(ATrackBar.TickStyle = tsManual);

  lSlider.setNumberOfTickMarks(lTickCount);

  if ATrackBar.TickMarks = tmTopLeft then
    lSlider.setTickMarkPosition(NSTickMarkAbove)
  else
    lSlider.setTickMarkPosition(NSTickMarkBelow);
  lSlider.setNeedsDisplay_(true);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.GetPosition
  Params:  ATrackBar - LCL custom track bar
  Returns: Position of slider
 ------------------------------------------------------------------------------}
class function TCocoaWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar
  ): integer;
var
  lSlider: TCocoaSlider;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then
  begin
    Result := 0;
    Exit;
  end;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  Result := lSlider.intValue();
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.SetPosition
  Params:  ATrackBar - LCL custom track bar
           NewPosition  - New position

  Sets the position of slider
 ------------------------------------------------------------------------------}
class procedure TCocoaWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar;
  const NewPosition: integer);
var
  lSlider: TCocoaSlider;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  lSlider.setIntValue(ATrackBar.Position);
end;

// Cocoa auto-detects the orientation based on width/height and there seams
// to be no way to force it
class procedure TCocoaWSTrackBar.SetOrientation(const ATrackBar: TCustomTrackBar;
  const AOrientation: TTrackBarOrientation);
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  if (AOrientation = trHorizontal) and (ATrackBar.Height >= ATrackBar.Width) then
    ATrackBar.Width := ATrackBar.Height + 1
  else if (AOrientation = trVertical) and (ATrackBar.Width >= ATrackBar.Height) then
    ATrackBar.Height := ATrackBar.Width + 1;
end;

class procedure TCocoaWSTrackBar.SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer);
var
  lSlider: TCocoaSlider;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  lSlider.lclAddManTick(ATick);
end;

class procedure TCocoaWSTrackBar.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  lSlider : TCocoaSlider;
  trk     : TCustomTrackBar;
  frm     : NSRect;
begin
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then Exit;
  trk := TCustomTrackBar(AWinControl);
  lSlider := TCocoaSlider(AWinControl.Handle);
  frm := lSlider.frame;
  try
    if trk.Orientation = trVertical then
      lSlider.setFrame(NSMakeRect(0,0,5,10))
    else
      lSlider.setFrame(NSMakeRect(0,0,10,5));

    TCocoaWSWinControl.GetPreferredSize(AWinControl,PreferredWidth, PreferredHeight, WithThemeSpace);
    if trk.Orientation = trVertical then
      PreferredHeight := 0
    else
      PreferredWidth := 0;
  finally
    lSlider.setFrame(frm);
  end;
end;

end.
