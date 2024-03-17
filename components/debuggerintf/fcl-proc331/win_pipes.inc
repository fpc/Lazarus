{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by Michael Van Canneyt

    Win part of pipe stream.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFDEF FPC_DOTTEDUNITS}
uses WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
uses windows;
{$ENDIF FPC_DOTTEDUNITS}

Const piInheritablePipe : TSecurityAttributes = (
                           nlength:SizeOF(TSecurityAttributes);
                           lpSecurityDescriptor:Nil;
                           Binherithandle:True);
      piNonInheritablePipe : TSecurityAttributes = (
                             nlength:SizeOF(TSecurityAttributes);
                             lpSecurityDescriptor:Nil;
                             Binherithandle:False);


      PipeBufSize = 1024;


Function CreatePipeHandles (Var Inhandle,OutHandle : THandle; APipeBufferSize : Cardinal = PipeBufSize) : Boolean;

begin
  Result := CreatePipe (@Inhandle,@OutHandle,@piNonInheritablePipe,APipeBufferSize);
end;


Function TInputPipeStream.GetNumBytesAvailable: DWord;
begin
  if not PeekNamedPipe(Handle, nil, 0, nil, @Result, nil) then
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
