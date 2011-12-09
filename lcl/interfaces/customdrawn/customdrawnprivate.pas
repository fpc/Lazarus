unit customdrawnprivate;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  // LCL
  Controls, Graphics, stdctrls, extctrls, comctrls,
  customdrawnproc, customdrawncontrols;

type
  { TCDIntfButton }

  TCDIntfButton = class(TCDButton)
  public
    LCLControl: TButton;
    constructor Create(AOwner: TComponent); override;
    procedure HandleOnClick(Sender: TObject);
  end;

// These are default message handlers which backends might use to simplify their code
// They convert a message sent to the form into a message to the correct sub-control
procedure CallbackMouseUp(AWindowHandle: TCDForm; x, y: Integer; Button: TMouseButton; ShiftState: TShiftState = []);
procedure CallbackMouseDown(AWindowHandle: TCDForm; x, y: Integer; Button: TMouseButton; ShiftState: TShiftState = []);
procedure CallbackMouseMove(AWindowHandle: TCDForm; x, y: Integer; ShiftState: TShiftState = []);

implementation

uses customdrawnint, LCLMessageGlue;

procedure CallbackMouseUp(AWindowHandle: TCDForm; x, y: Integer; Button: TMouseButton; ShiftState: TShiftState = []);
var
  lTarget: TWinControl;
  lEventPos: TPoint;
begin
  lTarget := AWindowHandle.LastMouseDownControl;
  AWindowHandle.LastMouseDownControl := nil;
  if lTarget = nil then lTarget := FindControlWhichReceivedEvent(
    AWindowHandle.LCLForm, AWindowHandle.Children, x, y);
  lEventPos := FormPosToControlPos(lTarget, x, y);
  LCLSendMouseUpMsg(lTarget, lEventPos.x, lEventPos.y, Button, ShiftState);
  LCLSendClickedMsg(lTarget);
end;

procedure CallbackMouseDown(AWindowHandle: TCDForm; x, y: Integer; Button: TMouseButton; ShiftState: TShiftState = []);
var
  lTarget: TWinControl;
  lEventPos: TPoint;
begin
  lTarget := FindControlWhichReceivedEvent(AWindowHandle.LCLForm, AWindowHandle.Children, x, y);
  AWindowHandle.LastMouseDownControl := lTarget;
  lEventPos := FormPosToControlPos(lTarget, x, y);

  // If the target is focusable, a mouse down will give it focus
  CDWidgetset.CDSetFocusToControl(lTarget);

  LCLSendMouseDownMsg(lTarget, lEventPos.x, lEventPos.y, Button, ShiftState);
end;

procedure CallbackMouseMove(AWindowHandle: TCDForm; x, y: Integer; ShiftState: TShiftState = []);
var
  lTarget: TWinControl;
  lEventPos: TPoint;
begin
  if AWindowHandle.LastMouseDownControl = nil then
    lTarget := FindControlWhichReceivedEvent(AWindowHandle.LCLForm, AWindowHandle.Children, x, y)
  else
    lTarget := AWindowHandle.LastMouseDownControl;

  lEventPos := FormPosToControlPos(lTarget, x, y);
  LCLSendMouseMoveMsg(lTarget, lEventPos.x, lEventPos.y, ShiftState);
end;

{ TCDIntfButton }

constructor TCDIntfButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnClick := @HandleOnClick;
end;

procedure TCDIntfButton.HandleOnClick(Sender: TObject);
begin
  LCLControl.OnClick(LCLControl);
end;

end.

