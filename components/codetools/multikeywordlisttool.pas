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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TMultiKeyWordListCodeTool enhances the TCustomCodeTool with the ability
    to switch the KeyWord list and keep a list of KeyWord lists.
}
unit MultiKeyWordListTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CustomCodeTool, KeywordFuncLists;

type

  { TMultiKeyWordListCodeTool }

  TMultiKeyWordListCodeTool = class(TCustomCodeTool)
  private
    FKeyWordLists: TFPList; // list of TKeyWordFunctionList
    FCurKeyWordListID: integer;
    function GetKeyWordFuncList: TKeyWordFunctionList;
    procedure SetCurKeyWordFuncList(AKeyWordFuncList: TKeyWordFunctionList);
  protected
    procedure SetKeyWordListID(NewID: integer);
  public
    DefaultKeyWordFuncList: TKeyWordFunctionList;
    property KeyWordListID: integer read FCurKeyWordListID write SetKeyWordListID;
    property CurKeyWordFuncList: TKeyWordFunctionList
       read GetKeyWordFuncList write SetCurKeyWordFuncList;
    function AddKeyWordFuncList(AKeyWordFuncList: TKeyWordFunctionList): integer;
    procedure ClearKeyWordFuncLists;

    constructor Create;
    destructor Destroy; override;
    procedure CalcMemSize(Stats: TCTMemStats); override;
  end;


implementation

{ TMultiKeyWordListCodeTool }

constructor TMultiKeyWordListCodeTool.Create;
begin
  inherited Create;
  FKeyWordLists:=TFPList.Create; // list of TKeyWordFunctionList
  AddKeyWordFuncList(KeyWordFuncList);
  FCurKeyWordListID:=0;
  DefaultKeyWordFuncList:=KeyWordFuncList;
end;

destructor TMultiKeyWordListCodeTool.Destroy;
begin
  ClearKeyWordFuncLists;
  FKeyWordLists.Free;
  inherited Destroy;
end;

procedure TMultiKeyWordListCodeTool.CalcMemSize(Stats: TCTMemStats);
begin
  inherited CalcMemSize(Stats);
  Stats.Add('TMultiKeyWordListCodeTool',
     PtrUInt(FKeyWordLists.InstanceSize)
     +PtrUInt(FKeyWordLists.Capacity)*SizeOf(Pointer));
end;

procedure TMultiKeyWordListCodeTool.SetKeyWordListID(NewID: integer);
begin
  if FCurKeyWordListID=NewID then exit;
  FCurKeyWordListID:=NewID;
  KeyWordFuncList:=TKeyWordFunctionList(FKeyWordLists[NewID]);
end;

procedure TMultiKeyWordListCodeTool.SetCurKeyWordFuncList(
  AKeyWordFuncList: TKeyWordFunctionList);
var i: integer;
begin
  i:=0;
  while i<FKeyWordLists.Count do begin
    if TKeyWordFunctionList(FKeyWordLists[i])=AKeyWordFuncList then begin
      SetKeyWordListID(i);
      exit;
    end;
    inc(i);
  end;
  SaveRaiseException(
    '[TMultiKeyWordListCodeTool.SetCurKeyWordFuncList] unknown list',true);
end;

function TMultiKeyWordListCodeTool.GetKeyWordFuncList: TKeyWordFunctionList;
begin
  Result:=KeyWordFuncList;
end;

function TMultiKeyWordListCodeTool.AddKeyWordFuncList(
  AKeyWordFuncList: TKeyWordFunctionList): integer;
begin
  Result:=FKeyWordLists.Add(AKeyWordFuncList);
end;

procedure TMultiKeyWordListCodeTool.ClearKeyWordFuncLists;
var i: integer;
begin
  KeyWordListID:=0;
  for i:=FKeyWordLists.Count-1 downto 1 do begin
    TKeyWordFunctionList(FKeyWordLists[i]).Free;
    FKeyWordLists.Delete(i);
  end;
  KeyWordFuncList.Clear;
end;


end.

