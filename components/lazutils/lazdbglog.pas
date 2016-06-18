{
 **********************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************
}
unit LazDbgLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function MemSizeString(const s: string): PtrUInt;
function MemSizeFPList(const List: TFPList): PtrUInt;
function GetStringRefCount(const s: string): PtrInt;

implementation

function MemSizeString(const s: string): PtrUInt;
begin
  Result:=length(s);
  if s<>'' then
    inc(Result,SizeOf(Pointer)*4);
end;

function MemSizeFPList(const List: TFPList): PtrUInt;
begin
  if List=nil then exit(0);
  Result:=PtrUInt(List.InstanceSize)
    +PtrUInt(List.Capacity)*SizeOf(Pointer);
end;

function GetStringRefCount(const s: string): PtrInt;
begin
  if s='' then
    Result:=-1
  else
    Result:=PPtrInt(s)[-2];
end;

end.

