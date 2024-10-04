unit CocoaWSScrollers;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, LCLType, Controls, Forms,
  CocoaAll, CocoaPrivate, CocoaCustomControl, CocoaScrollers, CocoaUtils;

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

function EmbedInScrollView(AView: NSView; AReleaseView: Boolean = true): TCocoaScrollView;
function EmbedInManualScrollView(AView: NSView): TCocoaManualScrollView;
function EmbedInManualScrollHost(AView: TCocoaManualScrollView): TCocoaManualScrollHost;

procedure LCLScrollViewAdjustSize(control: TWinControl);

var
  ASyncLCLControlAdjustSizer: TASyncLCLControlAdjustSizer;

implementation

function EmbedInScrollView(AView: NSView; AReleaseView: Boolean): TCocoaScrollView;
var
  r: TRect;
  p: NSView;
begin
  if not Assigned(AView) then
    Exit(nil);
  r := AView.lclFrame;
  p := AView.superview;
  Result := TCocoaScrollView.alloc.initWithFrame(NSNullRect);
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
  SetViewDefaults(Result);
end;

function EmbedInManualScrollView(AView: NSView): TCocoaManualScrollView;
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
  Result := TCocoaManualScrollView.alloc.initWithFrame(NSNullRect);
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
  SetViewDefaults(Result);
  if AView.isKindOfClass(TCocoaCustomControl) then
    TCocoaCustomControl(AView).auxMouseByParent := true;
end;

function EmbedInManualScrollHost(AView: TCocoaManualScrollView
  ): TCocoaManualScrollHost;
var
  r: TRect;
  p: NSView;
begin
  if not Assigned(AView) then
    Exit(nil);
  r := AView.lclFrame;
  p := AView.superview;
  Result := TCocoaManualScrollHost.alloc.initWithFrame(NSNullRect);
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
  SetViewDefaults(Result);
end;

procedure LCLScrollViewAdjustSize(control: TWinControl);
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

