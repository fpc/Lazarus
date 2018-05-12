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

  Author: Mattias Gaertner

  Abstract:
    Parser for Free Pascal Compiler output.
}
unit etPas2jsMsgParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IDEExternToolIntf, LazFileUtils,
  etFPCMsgParser, EnvironmentOpts;

type

  { TPas2jsMsgFilePool }

  TPas2jsMsgFilePool = class(TFPCMsgFilePool)
  public
    destructor Destroy; override;
    function LoadCurrentEnglishFile(UpdateFromDisk: boolean; AThread: TThread
      ): TFPCMsgFilePoolItem; override;
    procedure GetMsgFileNames({%H-}CompilerFilename, {%H-}TargetOS, {%H-}TargetCPU: string;
      out anEnglishFile, aTranslationFile: string); override;
  end;

  { TIDEPas2jsParser }

  TIDEPas2jsParser = class(TIDEFPCParser)
  public
    class function CanParseSubTool(const SubTool: string): boolean; override;
    class function DefaultSubTool: string; override;
    class function Priority: integer; override;
    class function MsgFilePool: TFPCMsgFilePool; override;
  end;

var
  Pas2jsMsgFilePool: TFPCMsgFilePool = nil;

procedure RegisterPas2jsParser;

implementation

procedure RegisterPas2jsParser;
begin
  ExternalToolList.RegisterParser(TIDEPas2jsParser);
end;

{ TPas2jsMsgFilePool }

destructor TPas2jsMsgFilePool.Destroy;
begin
  inherited Destroy;
  if Pas2jsMsgFilePool=Self then
    Pas2jsMsgFilePool:=nil;
end;

function TPas2jsMsgFilePool.LoadCurrentEnglishFile(UpdateFromDisk: boolean;
  AThread: TThread): TFPCMsgFilePoolItem;
var
  Filename: String;
begin
  Result:=nil;
  Filename:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)+'ide'+PathDelim+'pas2jsmsg.txt';
  if not FilenameIsAbsolute(Filename) then exit;
  Result:=LoadFile(Filename,UpdateFromDisk,AThread);
end;

procedure TPas2jsMsgFilePool.GetMsgFileNames(CompilerFilename, TargetOS,
  TargetCPU: string; out anEnglishFile, aTranslationFile: string);
begin
  anEnglishFile:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)+'ide'+PathDelim+'pas2jsmsg.txt';
  aTranslationFile:='';
end;

{ TIDEPas2jsParser }

class function TIDEPas2jsParser.CanParseSubTool(const SubTool: string): boolean;
begin
  Result:=(CompareText(SubTool,SubToolPas2js)=0);
end;

class function TIDEPas2jsParser.DefaultSubTool: string;
begin
  Result:=SubToolPas2js;
end;

class function TIDEPas2jsParser.Priority: integer;
begin
  Result:=SubToolPas2jsPriority;
end;

class function TIDEPas2jsParser.MsgFilePool: TFPCMsgFilePool;
begin
  Result:=Pas2jsMsgFilePool;
end;

initialization
  IDEPas2jsParser:=TIDEPas2jsParser;
finalization
  FreeAndNil(Pas2jsMsgFilePool);

end.

