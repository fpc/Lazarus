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

 Author: Joost van der Sluis

 Abstract:
   Registers the lfm resource format of forms.
}
unit lfmUnitResource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // packages
  LResources, Forms, CodeCache, CodeToolManager,
  // IDEIntf
  UnitResources, SrcEditorIntf, LazFileCache, LazFileUtils, AvgLvlTree,
  // IDE
  CheckLFMDlg;

type

  { TLFMUnitResourcefileFormat }

  TLFMUnitResourcefileFormat = class(TCustomLFMUnitResourceFileFormat)
  public
    class function FindResourceDirective(Source: TObject): boolean; override;
    class function GetUnitResourceFilename(AUnitFilename: string; {%H-}Loading: boolean): string; override;
    class function QuickCheckResourceBuffer(PascalBuffer, LFMBuffer: TObject; out
      LFMType, LFMComponentName, LFMClassName: string; out LCLVersion: string;
      out MissingClasses: TStrings): TModalResult; override;
  end;

implementation

type
  TLFMUnitResCacheItem = class
  public
    UnitFilename: string;
    CodeBufStamp: integer;
    ResourceDirective: string; // '*.lfm' or '*.dfm'
  end;

var
  LFMUnitResCache: TAvgLvlTree;

function CompareLFMUnitResCacheItems(Cache1, Cache2: Pointer): integer;
var
  Unit1: TLFMUnitResCacheItem absolute Cache1;
  Unit2: TLFMUnitResCacheItem absolute Cache2;
begin
  Result:=CompareFilenames(Unit1.UnitFilename,Unit2.UnitFilename);
end;

function CompareFilenameWithLFMUnitResCacheItem(aFilename, aCache: Pointer
  ): integer;
var
  Unit1Filename: String;
  Unit2: TLFMUnitResCacheItem absolute aCache;
begin
  Unit1Filename:=AnsiString(aFilename);
  Result:=CompareFilenames(Unit1Filename,Unit2.UnitFilename);
end;

function GetLFMUnitResCache(UnitFilename: string; AutoCreate: boolean
  ): TLFMUnitResCacheItem;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=LFMUnitResCache.FindKey(Pointer(UnitFilename),@CompareFilenameWithLFMUnitResCacheItem);
  if Node<>nil then begin
    Result:=TLFMUnitResCacheItem(Node.Data);
  end else if AutoCreate then begin
    Result:=TLFMUnitResCacheItem.Create;
    Result.UnitFilename:=UnitFilename;
    LFMUnitResCache.Add(Result);
  end else
    Result:=nil;
end;

{ TLFMUnitResourcefileFormat }

class function TLFMUnitResourcefileFormat.FindResourceDirective(Source: TObject): boolean;
var
  NewCode: TCodeBuffer;
  NewX,NewY,NewTopLine: integer;
  CodeBuf: TCodeBuffer;
  Cache: TLFMUnitResCacheItem;
begin
  CodeBuf:=Source as TCodeBuffer;
  Cache:=GetLFMUnitResCache(CodeBuf.Filename,true);
  if Cache.CodeBufStamp<>CodeBuf.ChangeStep then begin
    Cache.ResourceDirective:='';
    Cache.CodeBufStamp:=CodeBuf.ChangeStep;
    if CodeToolBoss.FindResourceDirective(CodeBuf,1,1,
      NewCode,NewX,NewY,NewTopLine, ResourceDirectiveFilename,false)
    then
      Cache.ResourceDirective:=ResourceDirectiveFilename
    else if (ResourceDirectiveFilename<>'*.dfm')
    and CodeToolBoss.FindResourceDirective(CodeBuf,1,1,
                     NewCode,NewX,NewY,NewTopLine, '*.dfm',false)
    then
      Cache.ResourceDirective:='*.dfm';
  end;
  Result:=Cache.ResourceDirective<>'';
end;

class function TLFMUnitResourcefileFormat.GetUnitResourceFilename(
  AUnitFilename: string; Loading: boolean): string;
var
  DFMFilename: String;
begin
  Result := ChangeFileExt(AUnitFilename,'.lfm');
  if not FileExistsCached(Result)
  and (SourceEditorManagerIntf.SourceEditorIntfWithFilename(Result)=nil)
  then begin
    DFMFilename:=ChangeFileExt(AUnitFilename,'.dfm');
    if FileExistsCached(DFMFilename) then
      Result:=DFMFilename;
  end;
end;

class function TLFMUnitResourcefileFormat.QuickCheckResourceBuffer(PascalBuffer,
  LFMBuffer: TObject; out LFMType, LFMComponentName, LFMClassName: string; out
  LCLVersion: string; out MissingClasses: TStrings): TModalResult;
begin
  Result := QuickCheckLFMBuffer(PascalBuffer as TCodeBuffer,
    LFMBuffer as TCodeBuffer, LFMType, LFMComponentName, LFMClassName,
    LCLVersion, MissingClasses);
end;

initialization
  RegisterUnitResourcefileFormat(TLFMUnitResourcefileFormat);
  LFMUnitResourceFileFormat:=TLFMUnitResourcefileFormat;
  LFMUnitResCache:=TAvgLvlTree.Create(@CompareLFMUnitResCacheItems);
finalization
  LFMUnitResCache.FreeAndClear;
  FreeAndNil(LFMUnitResCache);
end.

