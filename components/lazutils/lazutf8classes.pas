{
 **********************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************
}
unit LazUTF8Classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8;

type

  { TFileStreamUTF8 }

  TFileStreamUTF8 = TFileStream;

  { TStringListUTF8 }

  TStringListUTF8 = class(TStringList)
  protected
    function DoCompareText(const s1,s2 : string) : PtrInt; override;
  public
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  end;

  { TMemoryStreamUTF8 }

  TMemoryStreamUTF8 = class(TMemoryStream)
  public
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  end;

procedure LoadStringsFromFileUTF8(List: TStrings; const FileName: string);
procedure SaveStringsToFileUTF8(List: TStrings; const FileName: string);

function CompareStringListItemsUTF8LowerCase(List: TStringList; Index1, Index2: Integer): Integer;

implementation

uses
  LazFileUtils; //avoid circular reference with LazFileUtils

procedure LoadStringsFromFileUTF8(List: TStrings; const FileName: string);
var
  uList: TStringListUTF8;
begin
  if List is TStringListUTF8 then
  begin
    List.LoadFromFile(FileName);
    exit;
  end;
  uList:=TStringListUTF8.Create;
  try
    uList.LoadFromFile(FileName);
    List.Assign(uList);
  finally
    uList.Free;
  end;
end;

procedure SaveStringsToFileUTF8(List: TStrings; const FileName: string);
var
  uList: TStringListUTF8;
begin
  if List is TStringListUTF8 then
  begin
    List.SaveToFile(FileName);
    exit;
  end;
  uList:=TStringListUTF8.Create;
  try
    uList.Assign(List);
    uList.SaveToFile(FileName);
  finally
    uList.Free;
  end;
end;

function CompareStringListItemsUTF8LowerCase(List: TStringList; Index1,
  Index2: Integer): Integer;
begin
  Result:=CompareStr(UTF8LowerCase(List[Index1]),UTF8LowerCase(List[Index2]));
end;

{ TMemoryStreamUTF8 }

procedure TMemoryStreamUTF8.LoadFromFile(const FileName: string);
var
  S: TFileStreamUTF8;
begin
  S:=TFileStreamUTF8.Create (FileName,fmOpenRead or fmShareDenyWrite);
  Try
    LoadFromStream(S);
  finally
    S.free;
  end;
end;

procedure TMemoryStreamUTF8.SaveToFile(const FileName: string);
var
  S: TFileStreamUTF8;
begin
  S:=TFileStreamUTF8.Create (FileName,fmCreate);
  Try
    SaveToStream(S);
  finally
    S.free;
  end;
end;

function TStringListUTF8.DoCompareText(const s1, s2: string): PtrInt;
begin
  if CaseSensitive then
    Result:= UTF8CompareStr(s1,s2)
  else
    Result:= UTF8CompareText(s1,s2);
end;

procedure TStringListUTF8.LoadFromFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:= TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure TStringListUTF8.SaveToFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:=TFileStreamUTF8.Create(FileName,fmCreate);
  try
    SaveToStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

end.

