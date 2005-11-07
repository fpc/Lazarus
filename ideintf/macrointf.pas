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
  Classes, SysUtils;

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
  end;
  
var
  // the global IDE values
  IDEMacros: TIDEMacros; // set by the IDE


type

  { TIDEInteractiveStringValue }

  TIDEInteractiveStringValue = class(TPersistent)
  private
    FValue: string;
  published
    property Value: string read FValue write FValue;
  end;
  
  
  TIDECodeMacroGetValueProc = function(const Parameter: string;
                      InteractiveValue: TPersistent;
                      var Value, ErrorMsg: string): boolean;
  TIDECodeMacroGetValueMethod = function(const Parameter: string;
                      InteractiveValue: TPersistent;
                      var Value, ErrorMsg: string): boolean of object;

  { TIDECodeMacro }

  TIDECodeMacro = class
  private
    FInteractive: boolean;
    FInteractiveValueClass: TPersistentClass;
    FLongDescription: string;
    FName: string;
    FOnGetValueMethod: TIDECodeMacroGetValueMethod;
    FOnGetValueProc: TIDECodeMacroGetValueProc;
    FShortDescription: string;
  protected
    procedure Init; virtual;
  public
    constructor Create(const TheName: string);
    property Name: string read FName;
    property ShortDescription: string read FShortDescription write FShortDescription;
    property LongDescription: string read FLongDescription write FLongDescription;
    property OnGetValueProc: TIDECodeMacroGetValueProc read FOnGetValueProc
                                                       write FOnGetValueProc;
    property OnGetValueMethod: TIDECodeMacroGetValueMethod read FOnGetValueMethod
                                                       write FOnGetValueMethod;
    function GetValue(const Parameter: string; InteractiveValue: TPersistent;
                      out Value, ErrorMsg: string): boolean; virtual;
    property Interactive: boolean read FInteractive write FInteractive;
    property InteractiveValueClass: TPersistentClass read FInteractiveValueClass
                                                   write FInteractiveValueClass;
  end;
  
  
  { TIDECodeMacros }

  TIDECodeMacros = class
  protected
    function GetItems(Index: integer): TIDECodeMacro; virtual; abstract;
  public
    property Items[Index: integer]: TIDECodeMacro read GetItems; default;
    function Count: integer; virtual; abstract;
    function Add(Macro: TIDECodeMacro): integer; virtual; abstract;
    function FindByName(const AName: string): TIDECodeMacro; virtual; abstract;
    function CreateUniqueName(const AName: string): string; virtual; abstract;
  end;
  
var
  IDECodeMacros: TIDECodeMacros = nil; // set by the IDE

const
  CodeTemplateMakroMagic = '$(EnableMakros)';


function RegisterCodeMacro(const Name: string;
  const ShortDescription, LongDescription: string;
  OnGetValueProc: TIDECodeMacroGetValueProc;
  OnGetValueMethod: TIDECodeMacroGetValueMethod): TIDECodeMacro;

implementation

const
  MaxStamp = $7fffffff;
  MinStamp = -$7fffffff;
  InvalidStamp = MinStamp-1;

function RegisterCodeMacro(const Name: string; const ShortDescription,
  LongDescription: string; OnGetValueProc: TIDECodeMacroGetValueProc;
  OnGetValueMethod: TIDECodeMacroGetValueMethod): TIDECodeMacro;
var
  NewName: String;
begin
  NewName:=IDECodeMacros.CreateUniqueName(Name);
  Result:=TIDECodeMacro.Create(NewName);
  Result.ShortDescription:=ShortDescription;
  Result.LongDescription:=LongDescription;
  Result.OnGetValueProc:=OnGetValueProc;
  Result.OnGetValueMethod:=OnGetValueMethod;
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

{ TIDECodeMacro }

procedure TIDECodeMacro.Init;
begin
  FInteractiveValueClass:=TIDEInteractiveStringValue;
end;

constructor TIDECodeMacro.Create(const TheName: string);
begin
  FName:=TheName;
  FShortDescription:=FName;
  FLongDescription:=FName;
end;

function TIDECodeMacro.GetValue(const Parameter: string;
  InteractiveValue: TPersistent; out Value, ErrorMsg: string): boolean;
begin
  Value:=Parameter;
  ErrorMsg:='';
  if Assigned(OnGetValueProc) then
    Result:=OnGetValueProc(Parameter,InteractiveValue,Value,ErrorMsg)
  else if Assigned(OnGetValueMethod) then
    Result:=OnGetValueMethod(Parameter,InteractiveValue,Value,ErrorMsg)
  else
    Result:=true;
end;

initialization
  IDEMacros:=nil;

end.

