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
    Parser for GNU 'make' output.
}
unit etMakeMsgParser;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils,
  // CodeTools
  KeywordFuncLists,
  // LazUtils
  FileUtil, LazFileUtils, AvgLvlTree,
  // IDEIntf
  IDEExternToolIntf;

const
  MakeMsgIDEnteringDirectory = 1;
  MakeMsgIDLeavingDirectory = 2;

type

  { TIDEMakeParser
    Parse lines of 'make' tool. Finding the current directory is needed by other parsers. }

  TIDEMakeParser = class(TMakeParser)
  protected
    fIsFileExecutable: TFilenameToPointerTree;
    function IsFileExecutable(const Filename: string): boolean;
  public
    DirectoryStack: TStrings;
    DefaultDirectory: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitReading; override;
    procedure ReadLine(Line: string; OutputIndex: integer; IsStdErr: boolean;
      var Handled: boolean); override;
    class function DefaultSubTool: string; override;
    class function Priority: integer; override;
  end;

function CompStr(const SubStr: string; p: PChar): boolean;
function CompStrI(const SubStr: string; p: PChar): boolean;
function FindSubStrI(const SubStr: string; p: PChar): PChar;
function GetString(p: PChar; MaxLen: integer): string;
function Str2Integer(p: PChar; const Default: integer): integer;
function ReadDecimal(var p: PChar): boolean;
function ReadNumberWithThousandSep(var p: PChar): boolean;
function ReadChar(var p: PChar; c: char): boolean; inline;
function ReadString(var p: PChar; const Find: string): boolean; inline;

procedure RegisterMakeParser;

implementation

function CompStr(const SubStr: string; p: PChar): boolean;
var
  s: PChar;
begin
  Result:=false;
  if (SubStr='') or (p=nil) then exit;
  s:=PChar(SubStr);
  while (s^<>#0) and (p^=s^) do begin
    inc(p);
    inc(s);
  end;
  Result:=s^=#0;
end;

function CompStrI(const SubStr: string; p: PChar): boolean;
var
  s: PChar;
begin
  Result:=false;
  if (SubStr='') or (p=nil) then exit;
  s:=PChar(SubStr);
  while (s^<>#0) and (UpChars[p^]=UpChars[s^]) do begin
    inc(p);
    inc(s);
  end;
  Result:=s^=#0;
end;

function FindSubStrI(const SubStr: string; p: PChar): PChar;
var
  s: PChar;
begin
  Result:=nil;
  if (SubStr='') or (p=nil) then exit;
  s:=PChar(SubStr);
  while p^<>#0 do begin
    if (UpChars[p^]=UpChars[s^]) and CompStrI(SubStr,p) then begin
      Result:=p;
      exit;
    end;
    inc(p);
  end;
end;

function GetString(p: PChar; MaxLen: integer): string;
var
  e: PChar;
  len: Integer;
begin
  e:=p;
  len:=0;
  while (e^<>#0) and (len<MaxLen) do begin
    inc(e);
    inc(len);
  end;
  SetLength(Result,len);
  if len>0 then
    System.Move(p^,Result[1],len);
end;

function Str2Integer(p: PChar; const Default: integer): integer;
var
  Negated: Boolean;
  i: int64;
begin
  i:=0;
  if p^='-' then begin
    Negated:=true;
    inc(p);
  end else
    Negated:=false;
  while p^ in ['0'..'9'] do begin
    i:=i*10+ord(p^)-ord('0');
    if (i>High(Result)) then begin
      Result:=Default;
      exit;
    end;
    inc(p);
  end;
  Result:=i;
  if Negated then
    Result:=-Result;
end;

function ReadDecimal(var p: PChar): boolean;
var
  OldP: PChar;
begin
  OldP:=p;
  while p^ in ['0'..'9'] do inc(p);
  Result:=(OldP<p) and (p-OldP<10);
end;

function ReadNumberWithThousandSep(var p: PChar): boolean;
var
  OldP: PChar;
begin
  OldP:=p;
  repeat
    case p^ of
    '0'..'9': ;
    '.':
      if p=OldP then exit(false)
      else if p[1]='.' then exit(false);
    else break;
    end;
    inc(p);
  until false;
  Result:=(OldP<p) and (p-OldP<20);
end;

function ReadChar(var p: PChar; c: char): boolean;
begin
  Result:=p^=c;
  if Result then inc(p);
end;

function ReadString(var p: PChar; const Find: string): boolean;
begin
  Result:=CompStr(Find,p);
  if Result then inc(p,length(Find));
end;

procedure RegisterMakeParser;
begin
  ExternalToolList.RegisterParser(TIDEMakeParser);
end;

{ TIDEMakeParser }

function TIDEMakeParser.IsFileExecutable(const Filename: string): boolean;
var
  p: Pointer;
begin
  p:=fIsFileExecutable[Filename];
  if p=Pointer(Self) then
    Result:=true
  else if p=Pointer(fIsFileExecutable) then
    Result:=false
  else begin
    Result:=FileIsExecutable(Filename);
    if Result then
      fIsFileExecutable[Filename]:=Pointer(Self)
    else
      fIsFileExecutable[Filename]:=Pointer(fIsFileExecutable);
  end;
end;

constructor TIDEMakeParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fIsFileExecutable:=TFilenameToPointerTree.Create(false);
end;

destructor TIDEMakeParser.Destroy;
begin
  FreeAndNil(fIsFileExecutable);
  FreeAndNil(DirectoryStack);
  inherited Destroy;
end;

procedure TIDEMakeParser.InitReading;
begin
  DefaultDirectory:=Tool.WorkerDirectory;
  inherited InitReading;
end;

procedure TIDEMakeParser.ReadLine(Line: string; OutputIndex: integer;
  IsStdErr: boolean; var Handled: boolean);
{ returns true, if it is a make/gmake message
   Examples for make messages:
     make[1]: Entering directory `<filename>'
     make[1]: Leaving directory `<filename>'
     make[1]: *** [<filename>] Killed
     make <command>
     make[2]: *** [lazarus] Error 1
     make[1]: *** [idepkg] Error 2
     make: *** [idepkg] Error 2
     /bin/cp <options>
}
const
  EnterDirPattern = ': Entering directory `';
  LeavingDirPattern = ': Leaving directory `';
  MakeMsgPattern = ': *** [';
var
  MsgLine: TMessageLine;
  p: PChar;
  Filename, Dir: string;
  Run, OldP: PChar;
begin
  if Line='' then exit;
  p:=PChar(Line);
  OldP:=p;
  if ReadString(p,'make.exe') then
    inc(p,8)
  else if ReadString(p,'make') then
    inc(p,4)
  else if ReadString(p,'gmake') then
    inc(p,5);

  if (p>OldP) and (p^ in ['[',':']) then begin
    // e.g. make[2]: *** [lazarus] Error 1
    Handled:=true;

    MsgLine:=CreateMsgLine(OutputIndex);
    MsgLine.SubTool:=SubToolMake;
    if IsStdErr then
    begin
      MsgLine.Urgency:=mluImportant;
      MsgLine.Flags:=MsgLine.Flags+[mlfStdErr];
    end else begin
      MsgLine.Urgency:=mluVerbose;
    end;
    MsgLine.Msg:=Line;

    if p^='[' then
    begin
      while not (p^ in [']',#0]) do inc(p);
      if p^=']' then inc(p);
    end;
    if ReadString(p,EnterDirPattern) then begin
      // entering directory
      MsgLine.MsgID:=MakeMsgIDEnteringDirectory;
      if DefaultDirectory='' then DefaultDirectory:=Tool.WorkerDirectory;
      if (Tool.WorkerDirectory<>'') then begin
        if (DirectoryStack=nil) then DirectoryStack:=TStringList.Create;
        DirectoryStack.Add(Tool.WorkerDirectory);
      end;
      Dir:=p;
      if (Dir<>'') and (Dir[length(Dir)]='''') then
        Dir:=copy(Dir,1,length(Dir)-1);
      Tool.WorkerDirectory:=Dir;
    end else if ReadString(p,LeavingDirPattern) then begin
      // leaving directory
      MsgLine.MsgID:=MakeMsgIDLeavingDirectory;
      if (DirectoryStack<>nil) and (DirectoryStack.Count>0) then begin
        Tool.WorkerDirectory:=DirectoryStack[DirectoryStack.Count-1];
        DirectoryStack.Delete(DirectoryStack.Count-1);
      end else begin
        // leaving what directory?
        Tool.WorkerDirectory:=DefaultDirectory;
      end;
    end else if ReadString(p,MakeMsgPattern) then begin
      MsgLine.Msg:=p-1;
    end;
    AddMsgLine(MsgLine);
    exit;
  end else if (p>OldP) and (p^=' ') then begin
    // e.g. make --assume-new=lazbuild.lpr lazbuild
    Handled:=true;

    MsgLine:=CreateMsgLine(OutputIndex);
    MsgLine.SubTool:=SubToolMake;
    if IsStdErr then begin
      MsgLine.Urgency:=mluImportant;
      MsgLine.Flags:=MsgLine.Flags+[mlfStdErr];
    end else begin
      MsgLine.Urgency:=mluVerbose;
    end;
    MsgLine.Msg:=Line;
    AddMsgLine(MsgLine);
    exit;
  end;

  p:=OldP;
  if not (p^ in [#0,' ',#9]) then begin
    // check for command <option>
    Run:=p;
    while not (Run^ in [' ',#9,#0]) do inc(Run);
    if Run^<>#0 then begin
      SetLength(Filename,Run-p);
      System.Move(p^,Filename[1],length(Filename));
      Filename:=TrimFilename(Filename);
      if FilenameIsAbsolute(Filename)
      and ((GetExeExt='') or (ExtractFileExt(Filename)=GetExeExt))
      and IsFileExecutable(Filename) then begin
        Handled:=true;
        MsgLine:=CreateMsgLine(OutputIndex);
        MsgLine.SubTool:=SubToolMake;
        if IsStdErr then begin
          MsgLine.Urgency:=mluImportant;
          MsgLine.Flags:=MsgLine.Flags+[mlfStdErr];
        end else begin
          MsgLine.Urgency:=mluVerbose;
        end;
        MsgLine.Msg:=Line;
        AddMsgLine(MsgLine);
      end;
    end;
  end;
end;

class function TIDEMakeParser.DefaultSubTool: string;
begin
  Result:=SubToolMake;
end;

class function TIDEMakeParser.Priority: integer;
begin
  Result:=SubToolMakePriority;
end;

end.

