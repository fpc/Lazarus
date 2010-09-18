{ /***************************************************************************
                    compoptsmodes.pas  -  Lazarus IDE unit
                    ---------------------------------------
                Conditional compiler options and build modes.

 ***************************************************************************/

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
    This unit contains the implementation of the conditional compiler options
    that depend on build macros/modes and environment.
}
unit CompOptsModes;

{$mode objfpc}{$H+}

{$i ide.inc}

interface

uses
  Classes, SysUtils, LCLProc, ExprEval,
  IDEProcs, ProjectIntf;

type

  { TCompilerDiffTool
    A tool to create the difference between two option sets }

  TCompilerDiffTool = class
  private
    FDiff: TStrings;
    FDiffer: boolean;
    FPath: string;
    procedure SetDiff(const AValue: TStrings);
    procedure SetDiffer(const AValue: boolean);
    procedure SetPath(const AValue: string);
  public
    constructor Create(DiffList: TStrings);
    procedure AddDiffItem(const PropertyName, Value: string);
    procedure AddDiff(const PropertyName: string; const Old, New: string);
    procedure AddDiff(const PropertyName: string; const Old, New: integer);
    procedure AddDiff(const PropertyName: string; const Old, New: boolean);
    procedure AddStringsDiff(const PropertyName: string; const OldList, NewList: TStrings);
    procedure AddPathsDiff(const PropertyName: string; const Old, New: string);
    procedure AddSetDiff(const PropertyName: string; const Old, New: integer;
                         const EnumNames: PString);
    property Diff: TStrings read FDiff write SetDiff;
    property Path: string read FPath write SetPath;
    property Differ: boolean read FDiffer write SetDiffer;
  end;

implementation

{ TCompilerDiffTool }

procedure TCompilerDiffTool.SetDiff(const AValue: TStrings);
begin
  if FDiff=AValue then exit;
  FDiff:=AValue;
end;

procedure TCompilerDiffTool.SetDiffer(const AValue: boolean);
begin
  if FDiffer=AValue then exit;
  FDiffer:=AValue;
end;

procedure TCompilerDiffTool.SetPath(const AValue: string);
begin
  if FPath=AValue then exit;
  FPath:=AValue;
  // ! config path, not file path. Always /, not PathDelim
  if (FPath<>'') and (Path[length(Path)]<>'/') then FPath:=FPath+'/';
end;

constructor TCompilerDiffTool.Create(DiffList: TStrings);
begin
  FDiff:=DiffList;
  if Diff<>nil then
    Diff.Clear;
end;

procedure TCompilerDiffTool.AddDiffItem(const PropertyName, Value: string);
begin
  Differ:=true;
  if Diff<>nil then
    Diff.Add(Path+PropertyName+'='+Value);
end;

procedure TCompilerDiffTool.AddDiff(const PropertyName: string; const Old,
  New: string);
begin
  if Old=New then exit;
  AddDiffItem(PropertyName,New);
end;

procedure TCompilerDiffTool.AddDiff(const PropertyName: string; const Old,
  New: integer);
begin
  if Old=New then exit;
  AddDiffItem(PropertyName,IntToStr(New));
end;

procedure TCompilerDiffTool.AddDiff(const PropertyName: string; const Old,
  New: boolean);
begin
  if Old=New then exit;
  AddDiffItem(PropertyName,dbgs(New));
end;

procedure TCompilerDiffTool.AddStringsDiff(const PropertyName: string;
  const OldList, NewList: TStrings);
var
  i: Integer;
  OldCnt: Integer;
  NewCnt: Integer;
begin
  OldCnt:=0;
  if OldList<>nil then
    OldCnt:=OldList.Count;
  NewCnt:=0;
  if NewList<>nil then
    NewCnt:=NewList.Count;
  AddDiff(PropertyName+'/Count',OldCnt,NewCnt);
  for i:=0 to OldCnt-1 do begin
    if (i>=NewCnt) then
      AddDiffItem(PropertyName+'/Item'+IntToStr(i),'deleted='+OldList[i])
    else if (OldList[i]<>NewList[i]) then
      AddDiffItem(PropertyName+'/Item'+IntToStr(i),NewList[i]);
  end;
end;

procedure TCompilerDiffTool.AddPathsDiff(const PropertyName: string; const Old,
  New: string);
begin
  if Old=New then exit;
  AddDiff(PropertyName,Old,New);
end;

procedure TCompilerDiffTool.AddSetDiff(const PropertyName: string; const Old,
  New: integer; const EnumNames: PString);
var
  i: Integer;
  Mask: LongInt;
  s: String;
begin
  if Old=New then exit;
  Mask := 1;
  s:='';
  for i := 0 to 31 do begin
    if (New and Mask) <> (Old and Mask) then begin
      if s<>'' then s:=s+',';
      if (New and Mask) <> 0 then
        s:=s+'+'
      else
        s:=s+'-';
      s:=s+EnumNames[i];
    end;
    Mask := Mask shl 1;
  end;
  AddDiffItem(PropertyName,s);
end;

end.

