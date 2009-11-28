unit CocoaWSCommon;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  MacOSAll, CocoaAll,
  Controls, {todo: remove controls?}
  WSControls,
  CocoaPrivate, CocoaUtils, LCLMessageGlue;

type

  { TLCLCommonCallback }

  TLCLCommonCallback = class(TCommonCallback)
  public
    Target  : TControl;
    constructor Create(AOwner: NSObject; ATarget: TControl);
    procedure MouseDown(x,y: Integer); override;
    procedure MouseUp(x,y: Integer); override;
    procedure MouseClick(clickCount: Integer); override;
    procedure MouseMove(x,y: Integer); override;
  end;

  { TCocoaWSWinControl }

  TCocoaWSWinControl=class(TWSWinControl)
  published
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;
  end;


implementation

{ TLCLCommonCallback }

constructor TLCLCommonCallback.Create(AOwner: NSObject; ATarget: TControl);
begin
  inherited Create(AOwner);
  Target:=ATarget;
end;

procedure TLCLCommonCallback.MouseDown(x, y: Integer);
begin
  LCLSendMouseDownMsg(Target,x,y,mbLeft, []);
end;

procedure TLCLCommonCallback.MouseUp(x, y: Integer);
begin
  LCLSendMouseUpMsg(Target,x,y,mbLeft, []);
end;

procedure TLCLCommonCallback.MouseClick(clickCount: Integer);
begin
  LCLSendClickedMsg(Target);
end;

procedure TLCLCommonCallback.MouseMove(x, y: Integer);
begin
  LCLSendMouseMoveMsg(Target, x,y, []);
end;

{ TCocoaWSWinControl }

class procedure TCocoaWSWinControl.SetText(const AWinControl: TWinControl; const AText: String);
var
  obj : NSObject;
begin
  // sanity check
  obj:=NSObject(AWinControl.Handle);
  if not Assigned(obj) or not obj.isKindOfClass_(NSControl) then Exit;

  SetNSControlValue(NSControl(obj), AText);
end;

class function TCocoaWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  obj   : NSObject;
begin
  Result:=false;

  // sanity check
  obj:=NSObject(AWinControl.Handle);
  Result:=Assigned(obj) and obj.isKindOfClass_(NSControl);
  if not Result then Exit;

  AText:=GetNSControlValue(NSControl(obj));
  Result:=true;
end;

class function TCocoaWSWinControl.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  obj : NSObject;
  s   : NSString;
begin
  Result:=false;

  // sanity check
  obj:=NSObject(AWinControl.Handle);
  Result:= Assigned(obj) and obj.isKindOfClass_(NSControl);
  if not Result then Exit;

  s:=NSControl(obj).stringValue;
  if Assigned(s) then ALength:=s.length
  else ALength:=0
end;

end.

