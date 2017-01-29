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
}
unit ProcessList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, UTF8Process;
  
type
  { The TProcessList is used by the IDE to store all running programs and
    external tools, that are not watched. From time to time the IDE checks,
    if the processes has terminated and will free them cleanly to avoid
    zombies. }
  TProcessList = class
  private
    FItems: TList; // list of TProcessUTF8
    FFreeing: Boolean; // set wehn freeing stopped processes
    function GetCount: integer;
    function GetItems(Index: integer): TProcessUTF8;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(NewProcess: TProcessUTF8): integer;
    procedure Clear;
    procedure FreeStoppedProcesses;
  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TProcessUTF8 read GetItems; default;
  end;
  
function GetDefaultProcessList: TProcessList;

implementation

var
  DefaultProcessList: TProcessList;

function GetDefaultProcessList: TProcessList;
begin
  if DefaultProcessList=nil then DefaultProcessList:=TProcessList.Create;
  Result:=DefaultProcessList;
end;

{ TProcessList }

function TProcessList.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TProcessList.GetItems(Index: integer): TProcessUTF8;
begin
  Result:=TProcessUTF8(FItems[Index]);
end;

constructor TProcessList.Create;
begin
  FItems:=TList.Create;
end;

destructor TProcessList.Destroy;
begin
  FreeStoppedProcesses;
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TProcessList.Add(NewProcess: TProcessUTF8): integer;
begin
  Result:=FItems.Add(NewProcess);
end;

procedure TProcessList.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    Items[i].Free;
  FItems.Clear;
end;

procedure TProcessList.FreeStoppedProcesses;
var
  AProcess: TProcessUTF8;
  i: Integer;
begin
  // waitonexit or free may trigger another idle
  if FFreeing then Exit;
  FFreeing := True;
  try
    for i:=FItems.Count-1 downto 0 do begin
      AProcess:=Items[i];
      if AProcess.Running then continue;
      try
        try
          AProcess.WaitOnExit;
          AProcess.Free;
        finally
          FItems.Delete(i);
        end;
      except
        on E: Exception do begin
          DebugLn('Error freeing stopped process: ',E.Message);
        end;
      end;
    end;
  finally
    FFreeing := False;
  end;
end;

initialization
  DefaultProcessList:=nil;

finalization
  DefaultProcessList.Free;
  DefaultProcessList:=nil;

end.

