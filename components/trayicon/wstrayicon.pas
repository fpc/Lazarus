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
unit WSTrayIcon;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

{*******************************************************************
*  Compatibility code for Delphi for Windows.
*******************************************************************}
{$ifndef FPC}
  {$define Win32}
{$endif}


uses
{$ifdef LCLWin32}
  wswin32trayicon,
{$endif}
{$ifdef LCLGtk}
  wsgtktrayicon,
{$endif}
{$ifdef LCLGtk2}
  wsgtk2trayicon,
{$endif}
{$ifdef LCLGnome}
  wsgtktrayicon,
{$endif}
  Classes, SysUtils;

type

  { TWSTrayIcon }

  TWSTrayIcon = class(TWidgetTrayIcon)
    private
    protected
    public
    published
  end;

var
  vwsTrayIcon: TWidgetTrayIcon;
  vwsTrayIconCreated: Boolean;

implementation

initialization

  vwsTrayIconCreated := False;
  vwsTrayIcon := TWidgetTrayIcon.Create;
  vwsTrayIconCreated := True;

finalization

  vwsTrayIcon.Free;

end.

