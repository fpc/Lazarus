{ Copyright (C) 2007 Mattias Gaertner

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

  Abstract:
  
}

unit SimpleIDE;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TextTools, SynRegExpr, FileProcs,
  IDETextConverter;

type
  { TLazyTextConverterToolClasses }

  TLazyTextConverterToolClasses = class(TTextConverterToolClasses)
  public
    function SupportsType(aTextType: TTextConverterType): boolean; override;
    
    function GetTempFilename: string; override;
    function LoadFromFile(Converter: TIDETextConverter; const AFilename: string;
                          UpdateFromDisk, Revert: Boolean): Boolean; override;

    function SaveCodeBufferToFile(Converter: TIDETextConverter;
                           const AFilename: string): Boolean; override;
    function GetCodeBufferSource(Converter: TIDETextConverter;
                                out Source: string): boolean; override;
    function CreateCodeBuffer(Converter: TIDETextConverter;
                              const Filename, NewSource: string;
                              out CodeBuffer: Pointer): boolean; override;
    function LoadCodeBufferFromFile(Converter: TIDETextConverter;
                                 const Filename: string;
                                 UpdateFromDisk, Revert: Boolean;
                                 out CodeBuffer: Pointer): boolean; override;
  end;

procedure SetupTextConverters;
procedure FreeTextConverters;

implementation

var
  SynREEngine: TRegExpr;

procedure InitSynREEngine;
begin
  if SynREEngine=nil then
    SynREEngine:=TRegExpr.Create;
end;

function SynREMatches(const TheText, RegExpr, ModifierStr: string;
  StartPos: integer): boolean;
begin
  InitSynREEngine;
  SynREEngine.ModifierStr:=ModifierStr;
  SynREEngine.Expression:=RegExpr;
  SynREEngine.InputString:=TheText;
  Result:=SynREEngine.ExecPos(StartPos);
end;

function SynREVar(Index: Integer): string;
begin
  if SynREEngine<>nil then
    Result:=SynREEngine.Match[Index]
  else
    Result:='';
end;

procedure SynREVarPos(Index: Integer; out MatchStart, MatchLength: integer);
begin
  if SynREEngine<>nil then begin
    MatchStart:=SynREEngine.MatchPos[Index];
    MatchLength:=SynREEngine.MatchLen[Index];
  end else begin
    MatchStart:=-1;
    MatchLength:=-1;
  end;
end;

function SynREVarCount: Integer;
begin
  if SynREEngine<>nil then
    Result:=SynREEngine.SubExprMatchCount
  else
    Result:=0;
end;

function SynREReplace(const TheText, FindRegExpr, ReplaceRegExpr: string;
  UseSubstutition: boolean; const ModifierStr: string): string;
begin
  InitSynREEngine;
  SynREEngine.ModifierStr:=ModifierStr;
  SynREEngine.Expression:=FindRegExpr;
  Result:=SynREEngine.Replace(TheText,ReplaceRegExpr,UseSubstutition);
end;

procedure SynRESplit(const TheText, SeparatorRegExpr: string; Pieces: TStrings;
  const ModifierStr: string);
begin
  InitSynREEngine;
  SynREEngine.ModifierStr:=ModifierStr;
  SynREEngine.Expression:=SeparatorRegExpr;
  SynREEngine.Split(TheText,Pieces);
end;

procedure SetupTextConverters;
begin
  if TextConverterToolClasses<>nil then
    raise Exception.Create('SimpleIDE: TextConverterToolClasses<>nil');
  TextConverterToolClasses:=TLazyTextConverterToolClasses.Create;
  TextConverterToolClasses.RegisterClass(TTextReplaceTool);
end;

procedure FreeTextConverters;
begin
  FreeAndNil(TextConverterToolClasses);
end;

{ TLazyTextConverterToolClasses }

function TLazyTextConverterToolClasses.SupportsType(
  aTextType: TTextConverterType): boolean;
begin
  Result:=aTextType in [tctSource,tctFile,tctStrings];
end;

function TLazyTextConverterToolClasses.GetTempFilename: string;
var
  BaseDir: String;
begin
  BaseDir:=GetCurrentDirUTF8;
  Result:=FileProcs.GetTempFilename(BaseDir,'convert_');
end;

function TLazyTextConverterToolClasses.LoadFromFile(
  Converter: TIDETextConverter; const AFilename: string; UpdateFromDisk,
  Revert: Boolean): Boolean;
begin
  Result:=Converter.LoadFromFile(AFilename,false,UpdateFromDisk,Revert);
end;

function TLazyTextConverterToolClasses.SaveCodeBufferToFile(
  Converter: TIDETextConverter; const AFilename: string): Boolean;
begin
  raise Exception.Create('SaveCodeBufferToFile not supported');
  if (Converter=nil) and (aFilename='') then;
  Result:=false;
end;

function TLazyTextConverterToolClasses.GetCodeBufferSource(
  Converter: TIDETextConverter; out Source: string): boolean;
begin
  raise Exception.Create('GetCodeBufferSource not supported');
  Source:='';
  if Converter=nil then;
  Result:=false;
end;

function TLazyTextConverterToolClasses.CreateCodeBuffer(
  Converter: TIDETextConverter; const Filename, NewSource: string; out
  CodeBuffer: Pointer): boolean;
begin
  raise Exception.Create('CreateCodeBuffer not supported');
  CodeBuffer:=nil;
  if (Converter=nil) and (Filename='') and (NewSource='') then;
  Result:=false;
end;

function TLazyTextConverterToolClasses.LoadCodeBufferFromFile(
  Converter: TIDETextConverter; const Filename: string; UpdateFromDisk,
  Revert: Boolean; out CodeBuffer: Pointer): boolean;
begin
  raise Exception.Create('LoadCodeBufferFromFile not supported');
  CodeBuffer:=nil;
  if (Converter=nil) and (Filename='') and UpdateFromDisk and Revert then;
  Result:=false;
end;

initialization
  REException:=ERegExpr;
  REMatchesFunction:=@SynREMatches;
  REVarFunction:=@SynREVar;
  REVarPosProcedure:=@SynREVarPos;
  REVarCountFunction:=@SynREVarCount;
  REReplaceProcedure:=@SynREReplace;
  RESplitFunction:=@SynRESplit;
  SetupTextConverters;

finalization
  FreeTextConverters;
  FreeAndNil(SynREEngine);

end.

