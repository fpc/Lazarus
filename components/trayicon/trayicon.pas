{
 trayicon.pas

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

 Authors: Felipe Monteiro de Carvalho and Andrew Haines

 This unit contains the SystrayIcon object.
 
 Documentation for the component can be found here:
 http://wiki.lazarus.freepascal.org/index.php/TrayIcon
}
{Version 0.2}
unit TrayIcon;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, WSTrayIcon, Menus, Graphics, Forms, Controls;

type

  { TTrayIconClass }

  TTrayIconClass = class(TComponent)
    private
      vPopUpMenu: TPopupMenu;
      vIcon: TIcon;
      vHint: string;
      vVisible, vShowIcon, vShowHint: Boolean;
      vOnPaint, vOnClick, vOnDblClick: TNotifyEvent;
      vOnMouseDown, vOnMouseUp: TMouseEvent;
      vOnMouseMove: TMouseMoveEvent;
      function GetCanvas: TCanvas;
      procedure SetVisible(Value: Boolean);
    protected
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function Hide: Boolean;
      function Show: Boolean;
      procedure UpdateWS;
      property Canvas: TCanvas read GetCanvas;
    published
      property PopUpMenu: TPopupMenu read vPopUpMenu write vPopUpMenu;
      property Icon: TIcon read vIcon write vIcon;
      property Hint: string read vHint write vHint;
      property ShowHint: Boolean read vShowHint write vShowHint;
      property ShowIcon: Boolean read vShowIcon write vShowIcon;
      property Visible: Boolean read vVisible write SetVisible;
      property OnClick: TNotifyEvent read vOnClick write vOnClick;
      property OnDblClick: TNotifyEvent read vOnDblClick write vOnDblClick;
      property OnMouseDown: TMouseEvent read vOnMouseDown write vOnMouseDown;
      property OnMouseUp: TMouseEvent read vOnMouseUp write vOnMouseUp;
      property OnMouseMove: TMouseMoveEvent read vOnMouseMove write vOnMouseMove;
      property OnPaint: TNotifyEvent read vOnPaint write vOnPaint;
  end;

var
  SystrayIcon: TTrayIconClass;
  
implementation

{ TTrayIconClass }

{*******************************************************************
*  TTrayIconClass.Create ()
*
*  DESCRIPTION:    Creates a object from the TAplicativo class
*
*  PARAMETERS:     AOwner  - The owner of the component (this may be nil)
*
*  RETURNS:        A pointer to the newly created object
*
*******************************************************************}
constructor TTrayIconClass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  vIcon := TIcon.Create;
  
  vShowIcon := True;
end;

{*******************************************************************
*  TTrayIconClass.Destroy ()
*
*  DESCRIPTION:    Destroys a object derived from the TAplicativo class
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
destructor TTrayIconClass.Destroy;
begin
  vIcon.Free;

  inherited Destroy;
end;

{*******************************************************************
*  TTrayIconClass.Hide ()
*
*  DESCRIPTION:    Hides the Icon
*
*  PARAMETERS:     None
*
*  RETURNS:        If successfull
*
*******************************************************************}
function TTrayIconClass.Hide: Boolean;
begin
  vVisible := False;

  UpdateWS;

  Result := vwsTrayIcon.Hide;
end;

{*******************************************************************
*  TTrayIconClass.Show ()
*
*  DESCRIPTION:    Shows the Icon
*
*  PARAMETERS:     None
*
*  RETURNS:        If successfull
*
*******************************************************************}
function TTrayIconClass.Show: Boolean;
begin
  vVisible := True;

  UpdateWS;

  Result := vwsTrayIcon.Show;
end;

{*******************************************************************
*  TTrayIconClass.SetVisible ()
*
*  DESCRIPTION:    Setter method of the Visible property
*
*  PARAMETERS:     None
*
*  RETURNS:        If successfull
*
*******************************************************************}
procedure TTrayIconClass.SetVisible(Value: Boolean);
begin
  if Value then Show
  else Hide;
end;

{*******************************************************************
*  TTrayIconClass.UpdateWS ()
*
*  DESCRIPTION:    Updates the widgetset object
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TTrayIconClass.UpdateWS;
begin
  vwsTrayIcon.Icon.Assign(vIcon);
  vwsTrayIcon.PopUpMenu := vPopUpMenu;
  vwsTrayIcon.ShowHint := vShowHint;
  vwsTrayIcon.ShowIcon := vShowIcon;
  vwsTrayIcon.Hint := vHint;

  // Update events
  vwsTrayIcon.OnClick := vOnClick;
  vwsTrayIcon.OnPaint := vOnPaint;
  vwsTrayIcon.OnDblClick := vOnDblClick;
  vwsTrayIcon.OnMouseDown := vOnMouseDown;
  vwsTrayIcon.OnMouseUp := vOnMouseUp;
  vwsTrayIcon.OnMouseMove := vOnMouseMove;
  
  // Allows the widgetset to update itself internally
  vwsTrayIcon.InternalUpdate;
end;

{*******************************************************************
*  TTrayIconClass.GetCanvas ()
*
*  DESCRIPTION:    Getter method of the Canvas property
*
*  PARAMETERS:     None
*
*  RETURNS:        The canvas of the underlaying Widgetset component
*
*******************************************************************}
function TTrayIconClass.GetCanvas: TCanvas;
begin
  Result := vwsTrayIcon.Canvas;
end;

initialization

  SystrayIcon := TTrayIconClass.Create(nil);

finalization

  SystrayIcon.Free;

end.

