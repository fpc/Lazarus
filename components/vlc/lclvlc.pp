{ <description>

  Copyright (C) 2012 Michael Van Canneyt (michael@freepascal.org)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit lclvlc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libvlc, vlc, controls;

Type

  { TLCLVLCPlayer }

  TLCLVLCPlayer = Class(TVLCMediaPlayer)
  private
    FParentWindow: TWinControl;
    procedure SetLCLParentWindow(AValue: TWinControl);
  Protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure SetParentWindow; override;
    Procedure SetParentWindowSize(AWidth,AHeight : Cardinal); override;
  Published
    Property ParentWindow : TWinControl Read FParentWindow Write SetLCLParentWindow;
  end;

implementation

{$ifdef Unix}
{$ifdef lclgtk2}
{$I vlcgtk2.inc}
{$endif}
{$ifdef lclqt}
{$I vlcqt.inc}
{$endif}
{$endif}

{ TLCLVLCPlayer }

procedure TLCLVLCPlayer.SetLCLParentWindow(AValue: TWinControl);
begin
  if FParentWindow=AValue then Exit;
  If Assigned(FParentWindow) then
    FParentWindow.RemoveFreeNotification(Self);
  FParentWindow:=AValue;
  If Assigned(FParentWindow) then
    FParentWindow.FreeNotification(Self);
end;

procedure TLCLVLCPlayer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FParentWindow) then
    FParentWindow:=Nil;
end;

procedure TLCLVLCPlayer.SetParentWindow;
begin
  if Assigned(ParentWindow) then
    begin
    {$IFDEF UNIX}
    libvlc_media_player_set_xwindow(Instance, GetXHandle(ParentWindow));
    {$else}
    {$IFDEF MSWINDOWS}
    libvlc_media_player_set_hwnd(Instance, Pointer(ParentWindow.Handle));
    {$else}
    {$ERROR This platform is currently not supported by the VLC player component}
    {$ENDIF}
    {$endif}
    end;
end;

procedure TLCLVLCPlayer.SetParentWindowSize(AWidth, AHeight: Cardinal);
begin
end;

end.
