unit CocoaGroupBox;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll,
  CocoaPrivate, CocoaUtils;

type

  { TCocoaGroupBox }

  TCocoaGroupBox = objcclass(NSBox)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclClientFrame: TRect; override;
    function lclContentView: NSView; override;
    function lclGetFrameToLayoutDelta: TRect; override;
  end;

implementation

{ TCocoaGroupBox }

function TCocoaGroupBox.lclClientFrame: TRect;
var
  v : NSView;
begin
  v:=contentView;
  if not Assigned(v) then
    Result := inherited lclClientFrame
  else
    if v.isFlipped then
      Result:= TCocoaTypeUtil.toRect( v.frame )
    else
      Result:= TCocoaTypeUtil.toRect( v.frame, self.frame.size.height );
end;

function TCocoaGroupBox.lclContentView: NSView;
begin
  Result := NSView(contentView);
end;

function TCocoaGroupBox.lclGetFrameToLayoutDelta: TRect;
begin
  Result.Left := 3;
  Result.Right := -3;
  Result.Top := 0;
  Result.Bottom := -4;
end;

function TCocoaGroupBox.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaGroupBox.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaGroupBox.lclClearCallback;
begin
  callback := nil;
end;

end.

