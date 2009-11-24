unit CocoaWSCommon;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Controls,
  CocoaPrivate, LCLMessageGlue;

type

  { TControlCallback }

  TControlCallback = class(TCommonCallback)
  public
    Target  : TControl;
    constructor Create(AOwner: NSObject; ATarget: TControl);
    procedure MouseDown(x,y: Integer); override;
    procedure MouseUp(x,y: Integer); override;
  end;

implementation

{ TControlCallback }

constructor TControlCallback.Create(AOwner: NSObject; ATarget: TControl);
begin
  inherited Create(AOwner);
  Target:=ATarget;
end;

procedure TControlCallback.MouseDown(x, y: Integer);
begin
  LCLSendMouseDownMsg(Target,x,y,mbLeft, []);
end;

procedure TControlCallback.MouseUp(x, y: Integer);
begin
  LCLSendMouseUpMsg(Target,x,y,mbLeft, []);
end;

end.

