unit CocoaDatePicker;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils,
  CocoaAll, CocoaUtils, CocoaPrivate, cocoa_extra;


type
  { TCocoaDatePicker }

  TCocoaDatePicker = objcclass(NSDatePicker)
  public
    callback: ICommonCallback;
    autoResize: boolean;
    retainAspectRatio: boolean;

    function lclGetCallback: ICommonCallback; override;

    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure rightMouseDragged(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure otherMouseDragged(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;

    function acceptsFirstResponder: LCLObjCBoolean; override;

    procedure setFrame(aframe: NSRect); override;
  end;

implementation

procedure TCocoaDatePicker.mouseDown(event: NSEvent);
Var
  oldDate, newDate: TDateTime;
begin
  if assigned(callback) then
  begin
    // Save Date BEFORE mouse click/down event
    oldDate:= NSDateToDateTime(Self.dateValue);

    if not callback.MouseUpDownEvent(event) then
    begin
      // Without this, Cocoa will not update our NSDatePicker date...
      inherited mouseDown(event);

      // After mouse event, has our date changed
      newDate:= NSDateToDateTime(Self.dateValue);
      if oldDate <> newDate then
        callback.SendOnChange;

      // This also calls OnClick....
      if Assigned(Callback) then
        callback.MouseUpDownEvent(event, true);
      end;
  end;
end;

procedure TCocoaDatePicker.mouseMoved(event: NSEvent);
begin
  if not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;


function TCocoaDatePicker.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaDatePicker.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaDatePicker.setFrame(aframe: NSRect);
var
  fsz : NSSize;
  sz  : NSSize;
  rt  : double;
  wr  : double;
  hr  : double;
begin
  inherited setFrame(aframe);
  if not autoResize then Exit;

  if Self.respondsToSelector(objcselector('fittingSize')) then
    fsz:=fittingSize
  else
  begin
    // hardcoded size of a Calendar for MacOSX 10.6
    // as can be seen in Interface Builder
    fsz.width := 139;
    fsz.height := 148;
  end;

  if (fsz.width=0) or (fsz.height=0) then Exit;
  sz:=frame.size;
  // resize even if too small already

  if retainAspectRatio and (fsz.height>0) and (fsz.width>0)then
  begin
    rt:=fsz.width/fsz.height;
    wr:=sz.width / fsz.width;
    hr:=sz.height / fsz.height;
    if wr > hr then
      fsz.width:=fsz.width * sz.width / (sz.height*rt)
    else
      fsz.height:=fsz.height * sz.height / (sz.width / rt);
  end;
  setBoundsSize(fsz);
end;

procedure TCocoaDatePicker.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaDatePicker.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaDatePicker.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaDatePicker.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaDatePicker.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaDatePicker.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaDatePicker.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaDatePicker.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaDatePicker.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

end.

