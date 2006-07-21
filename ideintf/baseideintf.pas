{ Copyright (C) 2004

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

  Author: Mattias Gaertner

  Abstract: Base classes of the IDEIntf.
}
unit BaseIDEIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazConfigStorage, Dialogs;
  
type
  TGetIDEConfigStorage = function(const Filename: string; LoadFromDisk: Boolean
                                  ): TConfigStorage;

var
  // will be set by the IDE
  DefaultConfigClass: TConfigStorageClass = nil;
  GetIDEConfigStorage: TGetIDEConfigStorage = nil;

function EnvironmentAsStringList: TStringList;
procedure AssignEnvironmentTo(DestStrings, Overrides: TStrings);

implementation

function EnvironmentAsStringList: TStringList;
var
  i, SysVarCount, e: integer;
  Variable, Value: string;
Begin
  Result:=TStringList.Create;
  SysVarCount:=GetEnvironmentVariableCount;
  for i:=0 to SysVarCount-1 do begin
    Variable:=GetEnvironmentString(i+1);
    e:=1;
    while (e<=length(Variable)) and (Variable[e]<>'=') do inc(e);
    Value:=copy(Variable,e+1,length(Variable)-e);
    Variable:=LeftStr(Variable,e-1);
    Result.Values[Variable]:=Value;
  end;
end;

procedure AssignEnvironmentTo(DestStrings, Overrides: TStrings);
var
  EnvList: TStringList;
  i: integer;
  Variable, Value: string;
begin
  // get system environment
  EnvList:=EnvironmentAsStringList;
  try
    if Overrides<>nil then begin
      // merge overrides
      for i:=0 to Overrides.Count-1 do begin
        Variable:=Overrides.Names[i];
        Value:=Overrides.Values[Variable];
        EnvList.Values[Variable]:=Value;
      end;
    end;
    DestStrings.Assign(EnvList);
  finally
    EnvList.Free;
  end;
end;

end.

