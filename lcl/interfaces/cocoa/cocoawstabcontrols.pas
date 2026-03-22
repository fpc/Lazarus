unit CocoaWSTabControls;

interface

{$mode delphi}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

{.$DEFINE COCOA_DEBUG_TABCONTROL}

uses
  Classes, SysUtils,
  LCLType, Controls, ComCtrls, LCLMessageGlue, LMessages,
  WSComCtrls,
  CocoaAll,
  CocoaPrivate, CocoaCallback, CocoaCommonCallback, CocoaWSPrivate, CocoaConfig,
  CocoaUtils, CocoaTabControls;

type

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

implementation

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
    TCocoaViewUtil.updateFocusRing( tabview, AWinControl );

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
  ACocoaControl.setLabel(TCocoaControlUtil.toMacOSTitle(ACustomPage.Caption));

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
  page.setLabel(TCocoaControlUtil.toMacOSTitle(AText));

  if (AWinControl.Parent <> nil)
    and (AWinControl.Parent is TCustomTabControl)
    and (AWinControl.HandleAllocated)
  then
    TCocoaTabControlUtil.updateTabAndArrowVisibility( TCocoaTabControl(AWinControl.Parent.Handle) );
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
    lClientPos := TCocoaTypeUtil.toPoint(pt, lTabControl.frame.size.height);

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
  origin: NSPoint;
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

  if not TCocoaTabControlUtil.getTabsRect(lTabControl, tr) then Exit;

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
    Result.Right := Round(x + w[idx] + ofs);
    Result.Top := tr.Top;
    Result.Bottom := tr.Bottom;
  end
  else
  begin
    mw := 0;
    for i := 0 to Integer(lTabControl.tabViewItems.count)-1 do
    begin
      lTabPage := lTabControl.tabViewItemAtIndex(i);
      w[i] := lTabPage.sizeOfLabel(false).width;
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
    Result.Bottom := Round(x + w[idx] + ofs);
  end;

  origin:= lTabControl.contentRect.origin;
  Result.Offset( -Round(origin.x), -Round(origin.y) );
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

end.
