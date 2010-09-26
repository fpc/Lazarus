{ Copyright (C) 2004

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
    function IsMacro(const Name: string): boolean; virtual;
    // file utility functions
    function CreateAbsoluteSearchPath(var SearchPath: string;
                                      const BaseDirectory: string): boolean;
  end;
  
var
  // the global IDE values
  IDEMacros: TIDEMacros = nil; // set by the IDE

procedure RenameIDEMacroInString(var s: string; const OldName, NewName: string);

implementation

const
  MaxStamp = $7fffffff;
  MinStamp = -$7fffffff;
  InvalidStamp = MinStamp-1;

procedure RenameIDEMacroInString(var s: string; const OldName, NewName: string);
var
  p: Integer;
  Macro1: String;
  Macro2: String;

  procedure Replace(const OldValue, NewValue: string);
  begin
    s:=copy(s,1,p-1)+NewValue+copy(s,p+length(OldValue),length(s));
    inc(p,length(NewValue));
  end;

begin
  Macro1:='$('+OldName+')';
  Macro2:='$'+OldName+'(';
  p:=1;
  while (p<length(s)) do
  begin
    if (s[p]<>'$') then
      inc(p)  // skip normal character
    else if (s[p+1]='$') then
      inc(p,2) // skip $$
    else begin
      // macro at p found
      if SysUtils.CompareText(Macro1,copy(s,p,length(Macro1)))=0 then
        Replace(Macro1,'$('+NewName+')')
      else if SysUtils.CompareText(Macro2,copy(s,p,length(Macro1)))=0 then
        Replace(Macro2,'$'+NewName+'(')
      else
        inc(p);
    end;
  end;
end;

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

function TIDEMacros.IsMacro(const Name: string): boolean;
begin
  if Name='' then ;
  Result:=true;
end;

function TIDEMacros.CreateAbsoluteSearchPath(var SearchPath: string;
  const BaseDirectory: string): boolean;
var
  BaseDir: String;
begin
  if SearchPath='' then exit(true);
  BaseDir:=BaseDirectory;
  if not SubstituteMacros(BaseDir) then exit(false);
  Result:=SubstituteMacros(SearchPath);
  SearchPath:=FileUtil.CreateAbsoluteSearchPath(SearchPath,BaseDir);
end;

end.

