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

  Abstract:
    Interface to the IDE macros.
}
unit MacroIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

type
  { TIDEMacros - macros for paths and compiler settings }

  TIDEMacros = class
  protected
    FBaseTimeStamp: integer;
    FGraphTimeStamp: integer;
  public
    property BaseTimeStamp: integer read FBaseTimeStamp;
    property GraphTimeStamp: integer read FGraphTimeStamp;
    procedure IncreaseBaseStamp;
    procedure IncreaseGraphStamp;
    function StrHasMacros(const s: string): boolean; virtual;
    function SubstituteMacros(var s: string): boolean; virtual;
    // file utility functions
    function CreateAbsoluteSearchPath(var SearchPath: string;
                                      const BaseDirectory: string): boolean;
  end;
  
var
  // the global IDE values
  IDEMacros: TIDEMacros = nil; // set by the IDE


implementation

const
  MaxStamp = $7fffffff;
  MinStamp = -$7fffffff;
  InvalidStamp = MinStamp-1;

{ TIDEMacros }

procedure TIDEMacros.IncreaseBaseStamp;
begin
  if FBaseTimeStamp<MaxStamp then
    inc(FBaseTimeStamp)
  else
    FBaseTimeStamp:=MinStamp;
end;

procedure TIDEMacros.IncreaseGraphStamp;
begin
  if FGraphTimeStamp<MaxStamp then
    inc(FGraphTimeStamp)
  else
    FGraphTimeStamp:=MinStamp;
end;

function TIDEMacros.StrHasMacros(const s: string): boolean;
begin
  Result:=false;
end;

function TIDEMacros.SubstituteMacros(var s: string): boolean;
begin
  Result:=true;
end;

function TIDEMacros.CreateAbsoluteSearchPath(var SearchPath: string;
  const BaseDirectory: string): boolean;
begin
  if SearchPath='' then exit(true);
  Result:=SubstituteMacros(SearchPath);
  SearchPath:=FileUtil.CreateAbsoluteSearchPath(SearchPath,BaseDirectory);
end;

end.

