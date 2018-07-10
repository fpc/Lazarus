{            ----------------------------------------------------
              DbgIntfPsuedoTerminal.pp  -  Debugger helper class
             ----------------------------------------------------

  This unit contains a helper class for a console containing a program being debugged.


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
}

unit DbgIntfPseudoTerminal;

{$mode objfpc}{$H+}
{$IFDEF linux} {$DEFINE DBG_ENABLE_TERMINAL} {$ENDIF}

interface

uses
  Classes, SysUtils
{$IFDEF DBG_ENABLE_TERMINAL}
  , IDEMiniLibC, BaseUnix
{$ENDIF}
;

type

{ TPseudoTerminal }

TPseudoTerminal = class
{$IFDEF DBG_ENABLE_TERMINAL}
private
  FDeviceName: string;
  FOnCanRead: TNotifyEvent;
  FPTy: Integer;
  FReadBuf: String;
  procedure CloseInp;
public
  constructor Create;
  destructor  Destroy; override;
  procedure Open;
  procedure Close;
  function Write(s: string): Integer;
  function Read: String;
  procedure CheckCanRead;
  property OnCanRead: TNotifyEvent read FOnCanRead write FOnCanRead;
  property DevicePtyMaster: integer read FPty;
  property Devicename: string read FDeviceName;
{$ENDIF}
end;


implementation

{$IFDEF DBG_ENABLE_TERMINAL}

{ TPseudoTerminal }

procedure TPseudoTerminal.CloseInp;
var
  ios: termios;
begin
  // Based on MSEGui
  if FPTy = InvalHandle then exit;
  tcgetattr(FPty, @ios);
  ios.c_lflag:= (ios.c_lflag and not (icanon)) or echo;
  ios.c_cc[vmin]:= 0;
  ios.c_cc[vtime]:= 0;
  tcsetattr(FPty, tcsanow, @ios);
    //foutput.writeln('');
end;

constructor TPseudoTerminal.Create;
begin
  FPTy := InvalHandle;
end;

destructor TPseudoTerminal.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TPseudoTerminal.Close;
begin
  CloseInp;
  if FPTy <> InvalHandle
  then __Close(FPTy);
  FPTy := InvalHandle;
end;

procedure TPseudoTerminal.Open;
const
  BufLen = 100;
var
  ios: termios;
  int1: integer;

  procedure Error;
  begin
    if FPTy <> InvalHandle
    then __Close(FPTy);
    FPTy := InvalHandle;
    FDeviceName := '';
  end;

begin
  Close;
  FPTy := getpt;
  if FPTy < 0 then Error;
  if (grantpt(FPTy) < 0) or (unlockpt(FPTy) < 0) then begin
    Error;
    exit;
  end;
  setlength(FDeviceName, BufLen);
  if ptsname_r(FPTy, @FDeviceName[1], BufLen) < 0 then begin
    Error;
    exit;
  end;
  setlength(FDeviceName,length(pchar(FDeviceName)));
  if tcgetattr(FPTy, @ios) <> 0 then begin
    Error;
    exit;
  end;
  ios.c_lflag:= ios.c_lflag and not (icanon); // or echo);
  ios.c_cc[vmin]:= 1;
  ios.c_cc[vtime]:= 0;
  if tcsetattr(FPTy, tcsanow, @ios) <> 0 then begin
    Error;
    exit;
  end;

  int1 := fcntl(FPTy, f_getfl, 0);
  if int1 = InvalHandle then begin
    Error;
    exit;
  end;
  if fcntl(FPTy, f_setfl, int1 or o_nonblock) = InvalHandle then Error;
end;

function TPseudoTerminal.Write(s: string): Integer;
var
  int1, nbytes: Integer;
  p: PChar;
begin
  nbytes := length(s);
  if (FPTy = InvalHandle) or (nbytes = 0) then exit(0);
  Result:= nbytes;
  p := @s[1];
  repeat
    int1 := __write(FPTy, p^, nbytes);
    if int1 = -1 then begin
      if errno <> eintr then begin
        Result:= int1;
        break;
      end;
      continue;
    end;
    inc(p, int1);
    dec(nbytes, int1);
  until integer(nbytes) <= 0;
end;

function TPseudoTerminal.Read: String;
const
  BufLen = 1024;
var
  buf: String;
  i: Integer;
begin
  if (FPTy = InvalHandle) then exit('');

  SetLength(buf, BufLen + 1);
  Result := FReadBuf;
  FReadBuf := '';
  repeat
    i := __read(FPTy, buf[1], BufLen);
    if i > 0 then Result := Result + copy(buf, 1, i);
  until i <= 0;
end;

procedure TPseudoTerminal.CheckCanRead;
begin
  FReadBuf := Read;
  if (FReadBuf <> '') and assigned(FOnCanRead)
  then FOnCanRead(self);
end;

{$ENDIF}


end.

