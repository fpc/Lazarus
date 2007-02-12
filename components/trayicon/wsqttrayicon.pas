{
 wsqttrayicon.pas

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

 Special thanks for: Danny Milosavljevic and the Lazarus Team

 Qt specific code.
}
unit wsqttrayicon;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Menus, Controls, Lclintf,
  wscommontrayicon, qt4, qtobjects, qtwidgets;

type

  { TWidgetTrayIcon }

  TWidgetTrayIcon = class(TCustomWidgetTrayIcon)
    private
      IconHandle: QIconH;
      SystemTrayIcon: TQtSystemTrayIcon;
      function CreateIcon: QIconH;
      function GetCanvas: TCanvas;
    protected
    public
      hIcon, hSmallIcon: Cardinal;
      function Hide: Boolean; override;
      function Show: Boolean; override;
      property Canvas: TCanvas read GetCanvas;
      procedure InternalUpdate; override;
      function GetPosition: TPoint; override;
    published
  end;

implementation

uses WSTrayIcon;

{ TWidgetTrayIcon }

{*******************************************************************
*  TWidgetTrayIcon.CreateIcon ()
*
*  DESCRIPTION:    Converts a TIcon to a QIconH
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function TWidgetTrayIcon.CreateIcon: QIconH;
var
  Pixmap: QPixmapH;
begin
{  if Self.Icon.Handle <> 0 then
  begin
    QPixmap_fromImage(Pixmap, TQtImage(Self.Icon.Handle).Handle);
    
    Result := QIcon_create(Pixmap);
  end
  else }
    Result := QIcon_create();
end;

{*******************************************************************
*  TWidgetTrayIcon.GetCanvas ()
*
*  DESCRIPTION:
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function TWidgetTrayIcon.GetCanvas: TCanvas;
begin
  Result := Icon.Canvas;
end;

{*******************************************************************
*  TWidgetTrayIcon.Hide ()
*
*  DESCRIPTION:    Hides the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
function TWidgetTrayIcon.Hide: Boolean;
begin
  Result := False;

  if not vVisible then Exit;

  SystemTrayIcon.hide;

  SystemTrayIcon.Free;

  QIcon_destroy(IconHandle);

  vVisible := False;

  Result := True;
end;

{*******************************************************************
*  TWidgetTrayIcon.Show ()
*
*  DESCRIPTION:    Shows the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
function TWidgetTrayIcon.Show: Boolean;
var
  Text: WideString;
begin
  Result := False;

  if vVisible then Exit;
  
  IconHandle := CreateIcon;
  
  SystemTrayIcon := TQtSystemTrayIcon.create(IconHandle);

  Text := UTF8Decode(Hint);
  SystemTrayIcon.setToolTip(Text);

  if Assigned(PopUpMenu) then
   if TQtMenu(PopUpMenu.Handle).Widget <> nil then
    SystemTrayIcon.setContextMenu(QMenuH(TQtMenu(PopUpMenu.Handle).Widget));

  SystemTrayIcon.show;
  
  vVisible := True;

  Result := True;
end;

{*******************************************************************
*  TWidgetTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TWidgetTrayIcon.InternalUpdate;
begin
  { PopUpMenu }
  if Assigned(PopUpMenu) then
   if TQtMenu(PopUpMenu.Handle).Widget <> nil then
    SystemTrayIcon.setContextMenu(QMenuH(TQtMenu(PopUpMenu.Handle).Widget));
end;

{*******************************************************************
*  TWidgetTrayIcon.GetPosition ()
*
*  DESCRIPTION:    Returns the position of the tray icon on the display.
*                  This function is utilized to show message boxes near
*                  the icon
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function TWidgetTrayIcon.GetPosition: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

end.

