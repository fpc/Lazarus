unit CocoaDatePicker;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils,
  CocoaAll, CocoaUtils, CocoaPrivate, cocoa_extra;


type
  IDatePickerCallback = interface(ICommonCallback)
    Procedure MouseBtnUp;
  end;

  { TCocoaDatePicker }

  TCocoaDatePicker = objcclass(NSDatePicker)
  public
    callback: IDatePickerCallback;
    autoResize: boolean;
    retainAspectRatio: boolean;

    function lclGetCallback: ICommonCallback; override;

    procedure mouseDown(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;

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
      // Without this, Cocoa will not update our NSDatePicker date...
      inherited mouseDown(event);

    // After mouse event, has our date changed
    newDate:= NSDateToDateTime(Self.dateValue);
    if oldDate <> newDate then
      callback.SendOnChange;

    // This also calls OnClick....
    callback.MouseBtnUp;
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
  //don't resize if too small already
  if (sz.width<fsz.width) or (sz.height<fsz.height) then Exit;

  if retainAspectRatio and (fsz.height>0) then
  begin
    rt:=fsz.width/fsz.height;
    fsz.width:=fsz.width * sz.width / (sz.height*rt);
  end;
  setBoundsSize(fsz);
end;

end.

