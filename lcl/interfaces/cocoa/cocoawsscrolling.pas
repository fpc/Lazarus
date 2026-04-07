unit CocoaWSScrolling;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  LCLType, Controls, Forms,
  WSForms,
  CocoaAll, CocoaPrivate, CocoaCommonCallback, CocoaCustomControl,
  CocoaScrolling, CocoaScrollerImpl;

type

  { TCocoaWSScrollingWinControl }

  TCocoaWSScrollingWinControl = class(TWSScrollingWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
  end;

  { TCocoaWSScrollingUtil }

  TCocoaWSScrollingUtil = class
  public
    class function embedInScrollView(
      const AView: NSView;
      const AReleaseView: Boolean = True ): TCocoaScrollView;
    class function embedInManualScrollView(
      const AView: NSView ): TCocoaManualScrollView;
    class function embedInManualScrollHost(
      const AView: TCocoaManualScrollView ): TCocoaManualScrollHost;

    class function createLegacyScroller: TCocoaScrollBar;

    class function getFromScrollInfo(
      const bar: TCocoaScrollBar;
      const scrollInfo: TScrollInfo ): Integer;
    class function setToScrollInfo(
      const bar: TCocoaScrollBar;
      var scrollInfo: TScrollInfo ): Boolean;

    class procedure asyncAdjustSize(const control: TWinControl);
  end;

implementation

type

  { TASyncLCLControlAdjustSizer }

  TASyncLCLControlAdjustSizer = class
  private
    _control: TWinControl;
    _doing: Boolean;
    procedure doAdjustSize(data: PtrInt);
  public
    procedure adjustSize(control: TWinControl);
  end;

var
  ASyncLCLControlAdjustSizer: TASyncLCLControlAdjustSizer;

{ TCocoaWSScrollingUtil }

class function TCocoaWSScrollingUtil.embedInScrollView(
  const AView: NSView;
  const AReleaseView: Boolean ): TCocoaScrollView;
var
  r: TRect;
  p: NSView;
begin
  if not Assigned(AView) then
    Exit(nil);
  r := AView.lclFrame;
  p := AView.superview;
  Result := TCocoaScrollView.alloc.initWithFrame(NSZeroRect);
  if Assigned(p) then p.addSubView(Result);
  Result.lclSetFrame(r);
  {$ifdef BOOLFIX}
  Result.setHidden_(Ord(AView.isHidden));
  {$else}
  Result.setHidden(AView.isHidden);
  {$endif}
  Result.setDocumentView(AView);
  Result.setDrawsBackground(false); // everything is covered anyway
  {$ifdef BOOLFIX}
  AView.setHidden_(Ord(false));
  {$else}
  AView.setHidden(false);
  {$endif}
  if AReleaseView then AView.release;
  TCocoaViewUtil.setDefaultMargin(Result);
end;

class function TCocoaWSScrollingUtil.embedInManualScrollView(
  const AView: NSView ): TCocoaManualScrollView;
var
  r: TRect;
  p: NSView;
begin
  if not Assigned(AView) then
  begin
    Result:=nil;
    Exit;
  end;
  r := AView.lclFrame;
  p := AView.superview;
  p.setAutoresizingMask( NSViewWidthSizable or NSViewHeightSizable);
  Result := TCocoaManualScrollView.alloc.initWithFrame(NSZeroRect);
  Result.setAcceptsTouchEvents(true);
  Result.setAutoresizesSubviews(false);
  if Assigned(p) then p.addSubView(Result);
  Result.lclSetFrame(r);
  {$ifdef BOOLFIX}
  Result.setHidden_(Ord(AView.isHidden));
  {$else}
  Result.setHidden(AView.isHidden);
  {$endif}
  Result.setDocumentView(AView);
  {$ifdef BOOLFIX}
  AView.setHidden_(Ord(false));
  {$else}
  AView.setHidden(false);
  {$endif}
  AView.release;
  TCocoaViewUtil.setDefaultMargin(Result);
  if AView.isKindOfClass(TCocoaCustomControl) then
    TCocoaCustomControl(AView).auxMouseByParent := true;
end;

class function TCocoaWSScrollingUtil.embedInManualScrollHost(
  const AView: TCocoaManualScrollView ): TCocoaManualScrollHost;
var
  r: TRect;
  p: NSView;
begin
  if not Assigned(AView) then
    Exit(nil);
  r := AView.lclFrame;
  p := AView.superview;
  Result := TCocoaManualScrollHost.alloc.initWithFrame(NSZeroRect);
  if Assigned(p) then p.addSubView(Result);
  Result.lclSetFrame(r);
  {$ifdef BOOLFIX}
  Result.setHidden_(Ord(AView.isHidden));
  {$else}
  Result.setHidden(AView.isHidden);
  {$endif}
  Result.setDocumentView(AView);
  Result.setDrawsBackground(false); // everything is covered anyway
  Result.contentView.setAutoresizesSubviews(false);
  AView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);

  AView.release;
  {$ifdef BOOLFIX}
  AView.setHidden_(Ord(false));
  {$else}
  AView.setHidden(false);
  {$endif}
  TCocoaViewUtil.setDefaultMargin(Result);
end;

// for the Scroller created separately through TCocoaWSScrollBar.CreateHandle(),
// due to the lack of the control over the Layout by TCocoaManualScrollView,
// only the Legacy Style can be used for compatibility.
// it's the same logical relationship as NSScrollView and NSScroller.
class function TCocoaWSScrollingUtil.createLegacyScroller: TCocoaScrollBar;
var
  scrollBar: TCocoaScrollBar Absolute Result;
  manager: TCocoaScrollStyleManager;
begin
  scrollBar:= TCocoaScrollBar(TCocoaScrollBar.alloc);
  manager:= CocoaScrollStyleManagerCreator( nil, NSScrollerStyleLegacy );
  scrollBar.setManager( manager );
end;

class function TCocoaWSScrollingUtil.getFromScrollInfo(
  const bar: TCocoaScrollBar;
  const scrollInfo: TScrollInfo ): Integer;
var
  pg  : Integer;
  mn  : Integer;
  mx  : Integer;
  dl  : Integer;
begin
  if not Assigned(bar) then
  begin
    Result := 0;
    Exit;
  end;

  if scrollInfo.fMask and SIF_PAGE>0 then
  begin
    pg:=scrollInfo.nPage;
  end
  else pg:=bar.pageInt;

  if scrollInfo.fMask and SIF_RANGE>0 then
  begin
    mn:=scrollInfo.nMin;
    mx:=scrollInfo.nMax;
  end
  else
  begin
    mn:=bar.minInt;
    mx:=bar.maxInt;
  end;

  dl:=mx-mn;
  {$ifdef BOOLFIX}
  bar.setEnabled_(Ord(dl<>0));
  {$else}
  bar.SetEnabled(dl<>0);
  {$endif}

  // if changed page or range, the knob changes
  if scrollInfo.fMask and (SIF_RANGE or SIF_PAGE)>0 then
  begin
    if dl<>0 then
      bar.setKnobProportion(pg/dl)
    else
      bar.setKnobProportion(1);
    bar.pageInt:=pg;
    bar.minInt:=mn;
    bar.maxInt:=mx;
  end;

  if scrollInfo.fMask and SIF_POS > 0 then
    bar.lclSetPos( scrollInfo.nPos );

  Result:=bar.lclPos;
end;

class function TCocoaWSScrollingUtil.setToScrollInfo(
  const bar: TCocoaScrollBar;
  var scrollInfo: TScrollInfo ): Boolean;
var
  l : integer;
begin
  Result:=Assigned(bar);
  if not Result then Exit;

  FillChar(scrollInfo, sizeof(scrollInfo), 0);
  scrollInfo.cbSize:=sizeof(scrollInfo);
  scrollInfo.fMask:=SIF_ALL;
  scrollInfo.nMin:=bar.minInt;
  scrollInfo.nMax:=bar.maxInt;
  scrollInfo.nPage:=bar.pageInt;
  scrollInfo.nPos:=bar.lclPos;
  scrollInfo.nTrackPos:=scrollInfo.nPos;
  Result:=true;
end;

class procedure TCocoaWSScrollingUtil.asyncAdjustSize(
  const control: TWinControl);
begin
  if NSScroller.preferredScrollerStyle = NSScrollerStyleOverlay then
    Exit;
  ASyncLCLControlAdjustSizer.adjustSize(control);
end;

{ TASyncLCLControlAdjustSizer }

procedure TASyncLCLControlAdjustSizer.doAdjustSize(data: PtrInt);
begin
  _control.AdjustSize;
  _doing:= False;
end;

procedure TASyncLCLControlAdjustSizer.adjustSize(control: TWinControl);
begin
  _control:= control;
  _control.InvalidateClientRectCache(true);
  if NOT _doing then
    Application.QueueAsyncCall(@doAdjustSize, 0);
  _doing:= True;
end;

{ TCocoaWSScrollingWinControl}

class function  TCocoaWSScrollingWinControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  scrollcon: TCocoaScrollView;
  docview: TCocoaCustomControl;
  lcl : TLCLCommonCallback;
begin
  scrollcon:= TCocoaScrollView.alloc.lclInitWithCreateParams(AParams);
  TCocoaScrollingUtil.setBorderStyle(scrollcon, TScrollingWinControl(AWincontrol).BorderStyle);
  scrollcon.setDrawsBackground(false); // everything is covered anyway
  scrollcon.setBackgroundColor(NSColor.windowBackgroundColor);
  scrollcon.setAutohidesScrollers(True);
  scrollcon.isCustomRange := true;

  docview:= TCocoaCustomControl.alloc.init;
  docview.setFrameSize( scrollcon.contentSize );
  scrollcon.setDocumentView(docview);

  lcl := TLCLCommonCallback.Create(docview, AWinControl, scrollcon);
  lcl.BlockCocoaUpDown := true;
  scrollcon.callback := lcl;
  docview.callback := lcl;

  Result := TLCLHandle(scrollcon);
end;

class procedure TCocoaWSScrollingWinControl.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  if not Assigned(AWinControl) or not AWincontrol.HandleAllocated then Exit;
  TCocoaScrollingUtil.setBorderStyle( NSScrollView(AWinControl.Handle), ABorderStyle);
end;

initialization
  CocoaScrollStyleManagerCreator:= @CocoaScrollStyleManagerCreatorImpl;
  ASyncLCLControlAdjustSizer:= TASyncLCLControlAdjustSizer.Create;

finalization
  FreeAndNil( ASyncLCLControlAdjustSizer );

end.

