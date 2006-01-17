{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit TrayIcon;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, WSTrayIcon, Menus, Graphics, Forms, Controls;

type
  { TCustomTrayIcon }

  TCustomTrayIcon = class(TComponent)
    private
      vPopUpMenu: TPopupMenu;
      vIcon: TIcon;
      vToolTip: string;
      vVisible, vShowToolTip: Boolean;
      vOnPaint, vOnClick, vOnDblClick: TNotifyEvent;
      vOnMouseDown, vOnMouseUp: TMouseEvent;
      vOnMouseMove: TMouseMoveEvent;
      function GetCanvas: TCanvas;
      procedure UpdateWS;
      procedure SetVisible(Value: Boolean);
    protected
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Hide: Boolean;
      function Show: Boolean;
      property Canvas: TCanvas read GetCanvas;
      property PopUpMenu: TPopupMenu read vPopUpMenu write vPopUpMenu;
      property Icon: TIcon read vIcon write vIcon;
      property ToolTip: string read vToolTip write vToolTip;
      property ShowToolTip: Boolean read vShowToolTip write vShowToolTip;
      property Visible: Boolean read vVisible write SetVisible;
      property OnClick: TNotifyEvent read vOnClick write vOnClick;
      property OnDblClick: TNotifyEvent read vOnDblClick write vOnDblClick;
      property OnMouseDown: TMouseEvent read vOnMouseDown write vOnMouseDown;
      property OnMouseUp: TMouseEvent read vOnMouseUp write vOnMouseUp;
      property OnMouseMove: TMouseMoveEvent read vOnMouseMove write vOnMouseMove;
      property OnPaint: TNotifyEvent read vOnPaint write vOnPaint;
  end;
  
  { TTrayIcon }

  TTrayIcon = class(TCustomTrayIcon)
    published
      property PopUpMenu;
      property Icon;
      property ToolTip;
      property ShowToolTip;
      property Visible;
      property OnClick;
      property OnDblClick;
      property OnMouseDown;
      property OnMouseUp;
      property OnMouseMove;
      property OnPaint;
  end;

var
  SystrayIcon: TCustomTrayIcon;
  
procedure Register;
  
implementation

procedure Register;
begin
  RegisterComponents('Misc',[TTrayIcon]);
end;

{ TCustomTrayIcon }

{*******************************************************************
*  TCustomTrayIcon.Create ()
*
*  DESCRIPTION:    Creates a object from the TAplicativo class
*
*  PARAMETERS:     AOwner  - The owner of the component (this may be nil)
*
*  RETURNS:        A pointer to the newly created object
*
*******************************************************************}
constructor TCustomTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  vIcon := TIcon.Create;
end;

{*******************************************************************
*  TCustomTrayIcon.Destroy ()
*
*  DESCRIPTION:    Destroys a object derived from the TAplicativo class
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
destructor TCustomTrayIcon.Destroy;
begin
  vIcon.Free;

  inherited Destroy;
end;

{*******************************************************************
*  TGlass.Hide ()
*
*  DESCRIPTION:    Hides the Icon
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function TCustomTrayIcon.Hide: Boolean;
begin
  UpdateWS;

  Result := vwsTrayIcon.Hide;
end;

function TCustomTrayIcon.Show: Boolean;
begin
  UpdateWS;

  Result := vwsTrayIcon.Show;
end;

procedure TCustomTrayIcon.SetVisible(Value: Boolean);
begin
  vVisible := Value;

  if vVisible then Show
  else Hide;
end;

procedure TCustomTrayIcon.UpdateWS;
begin
  vwsTrayIcon.Icon.Assign(vIcon);
  vwsTrayIcon.PopUpMenu := vPopUpMenu;
  vwsTrayIcon.ShowToolTip := vShowToolTip;
  StrCopy(PChar(vwsTrayIcon.ToolTip), PChar(vToolTip));

  // Update events
  vwsTrayIcon.OnClick := vOnClick;
  vwsTrayIcon.OnPaint := vOnPaint;
  vwsTrayIcon.OnDblClick := vOnDblClick;
  vwsTrayIcon.OnMouseDown := vOnMouseDown;
  vwsTrayIcon.OnMouseUp := vOnMouseUp;
  vwsTrayIcon.OnMouseMove := vOnMouseMove;
end;

function TCustomTrayIcon.GetCanvas: TCanvas;
begin
  Result := vwsTrayIcon.Canvas;
end;

initialization

  SystrayIcon := TTrayIcon.Create(nil);

finalization

  SystrayIcon.Free;

end.

