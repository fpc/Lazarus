{
***************************************************************************
*                                                                         *
*   This source is free software; you can redistribute it and/or modify   *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
*   This code is distributed in the hope that it will be useful, but      *
*   WITHOUT ANY WARRANTY; without even the implied warranty of            *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
*   General Public License for more details.                              *
*                                                                         *
*   A copy of the GNU General Public License is available on the World    *
*   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
*   obtain it by writing to the Free Software Foundation,                 *
*   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
*                                                                         *
***************************************************************************

Author: Balázs Székely
}
unit opkman_timer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
   { TThreadTimer }
   TThreadTimer = class(TThread)
   private
     FTime: QWORD;
     FInterval: Cardinal;
     FOnTimer: TNotifyEvent;
     FEnabled: Boolean;
     procedure DoOnTimer;
   protected
     procedure Execute; override;
   public
     constructor Create;
     destructor Destroy; override;
   public
     property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
     property Interval: Cardinal read FInterval write FInterval;
     property Enabled: Boolean read FEnabled write FEnabled;
     procedure StopTimer;
     procedure StartTimer;
   end;

implementation

{ TThreadTimer }

constructor TThreadTimer.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FInterval := 10000;
  FEnabled := False;
end;

destructor TThreadTimer.Destroy;
begin
  //
  inherited Destroy;
end;

procedure TThreadTimer.DoOnTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TThreadTimer.Execute;
begin
  while not Terminated do
  begin
    Sleep(1000);
    if (GetTickCount64 - FTime > FInterval) and FEnabled and not Terminated then
    begin
      FTime := GetTickCount64;
      DoOnTimer;
    end;
  end;
end;

procedure TThreadTimer.StopTimer;
begin
  FEnabled := False;
end;

procedure TThreadTimer.StartTimer;
begin
  FTime := GetTickCount64;
  FEnabled := True;
  if Self.Suspended then
    Start;
end;

end.

