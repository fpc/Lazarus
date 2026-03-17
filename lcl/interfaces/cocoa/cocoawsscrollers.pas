unit CocoaWSScrollers;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, LCLType, Controls, Forms,
  CocoaAll, CocoaPrivate, CocoaCustomControl, CocoaScrollers;

type

  { TCocoaWSScrollerUtil }

  TCocoaWSScrollerUtil = class
  public
    class function embedInScrollView(
      const AView: NSView;
      const AReleaseView: Boolean = True ): TCocoaScrollView;
    class function embedInManualScrollView(
      const AView: NSView ): TCocoaManualScrollView;
    class function embedInManualScrollHost(
      const AView: TCocoaManualScrollView ): TCocoaManualScrollHost;

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

{ TCocoaWSScrollerUtil }

class function TCocoaWSScrollerUtil.embedInScrollView(
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

class function TCocoaWSScrollerUtil.embedInManualScrollView(
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

class function TCocoaWSScrollerUtil.embedInManualScrollHost(
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

class procedure TCocoaWSScrollerUtil.asyncAdjustSize(
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

initialization
  ASyncLCLControlAdjustSizer:= TASyncLCLControlAdjustSizer.Create;

finalization
  FreeAndNil( ASyncLCLControlAdjustSizer );

end.

