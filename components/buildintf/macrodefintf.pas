{ Copyright (C) 2012

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Abstract:
    Interface to the IDE macros.
}

unit MacroDefIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes;

Type
  TTransferMacro = class;

  TOnSubstitution = procedure(TheMacro: TTransferMacro; const MacroName: string;
    var s:string; const Data: PtrInt; var Handled, Abort: boolean;
    Depth: integer) of object;

  TMacroFunction = function(const s: string; const Data: PtrInt;
                            var Abort: boolean): string of object;

  TTransferMacroFlag = (
    tmfInteractive
    );
  TTransferMacroFlags = set of TTransferMacroFlag;

  TTransferMacro = class
  public
    Name: string;
    Value: string;
    Description: string;
    MacroFunction: TMacroFunction;
    Flags: TTransferMacroFlags;
    constructor Create(AName, AValue, ADescription:string;
      AMacroFunction: TMacroFunction; TheFlags: TTransferMacroFlags);
  end;


implementation

{ TTransferMacro }

constructor TTransferMacro.Create(AName, AValue, ADescription:string;
  AMacroFunction: TMacroFunction; TheFlags: TTransferMacroFlags);
begin
  Name:=AName;
  Value:=AValue;
  Description:=ADescription;
  MacroFunction:=AMacroFunction;
  Flags:=TheFlags;
end;

end.
