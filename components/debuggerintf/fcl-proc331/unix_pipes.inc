{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt

    Linux specific part of pipe stream.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFDEF FPC_DOTTEDUNITS}
Uses
  UnixApi.Base, UnixApi.Unix, UnixApi.TermIO;
{$ELSE FPC_DOTTEDUNITS}
Uses
  BaseUnix, Unix, TermIO;
{$ENDIF FPC_DOTTEDUNITS}

Function CreatePipeHandles (Var Inhandle,OutHandle : Longint; APipeBufferSize : Cardinal = 1024) : Boolean;

begin
  Result := (AssignPipe (Inhandle,OutHandle)<>-1);
end;


Function TInputPipeStream.GetNumBytesAvailable: DWord;

begin
  if fpioctl(Handle, FIONREAD, @Result)<0 then
    Result := 0;
end;

function TInputPipeStream.GetPosition: Int64;
begin
  Result:=FPos;
end;

procedure TInputPipeStream.InvalidSeek;
begin
  Raise EPipeSeek.Create (ENoSeekMsg);
end;

procedure PipeClose (const FHandle: THandle); inline;
begin
  FileClose(FHandle);
end;
