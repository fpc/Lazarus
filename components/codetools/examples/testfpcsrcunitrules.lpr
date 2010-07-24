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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Write all duplicate ppu files and all duplicate unit source files.
}
program TestFPCSrcUnitRules;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, CodeToolManager, DefineTemplates, FileProcs;

const
  ConfigFilename = 'codetools.config';
type

  { TTestFPCSourceUnitRules }

  TTestFPCSourceUnitRules = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure Error(Msg: string; DoWriteHelp: Boolean);
    procedure WriteCompilerInfo(ConfigCache: TFPCTargetConfigCache);
  end;

{ TMyApplication }

procedure TTestFPCSourceUnitRules.DoRun;
var
  ErrorMsg: String;
  CompilerFilename: String;
  TargetOS: String;
  TargetCPU: String;
  FPCSrcDir: String;
  UnitSet: TFPCUnitSetCache;
  ConfigCache: TFPCTargetConfigCache;
  SourceCache: TFPCSourceCache;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hcTPF','help compiler targetos targetcpu fpcsrcdir');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Halt;
  end;

  if not HasOption('F','fpcsrcdir') then
    Error('fpc source directory missing',true);

  if HasOption('c','compiler') then begin
    CompilerFilename:=GetOptionValue('c','compiler');
    CompilerFilename:=CleanAndExpandFilename(CompilerFilename);
  end else begin
    CompilerFilename:=GetDefaultCompilerFilename;
    CompilerFilename:=SearchFileInPath(CompilerFilename,'',
                    GetEnvironmentVariable('PATH'), PathSeparator,ctsfcDefault);
  end;
  TargetOS:=GetOptionValue('T','targetos');
  TargetCPU:=GetOptionValue('P','targetcpu');
  FPCSrcDir:=GetOptionValue('F','fpcsrcdir');
  FPCSrcDir:=CleanAndExpandDirectory(FPCSrcDir);

  if not FileExistsUTF8(CompilerFilename) then
    Error('compiler file not found: '+CompilerFilename,false);
  if not DirPathExists(FPCSrcDir) then
    Error('FPC source directory not found: '+FPCSrcDir,false);

  CodeToolBoss.SimpleInit(ConfigFilename);

  UnitSet:=CodeToolBoss.FPCDefinesCache.FindUnitSet(CompilerFilename,
                                          TargetOS,TargetCPU,'',FPCSrcDir,true);
  ConfigCache:=UnitSet.GetConfigCache(true);

  WriteCompilerInfo(ConfigCache);


  SourceCache:=UnitSet.GetSourceCache(true);

  // stop program loop
  Terminate;
end;

constructor TTestFPCSourceUnitRules.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestFPCSourceUnitRules.Destroy;
begin
  inherited Destroy;
end;

procedure TTestFPCSourceUnitRules.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln;
  writeln('  -c <compiler file name>');
  writeln('  --compiler=<compiler file name>');
  writeln('  -T <target OS>');
  writeln('  --targetos=<target OS>');
  writeln('  -P <target CPU>');
  writeln('  --targetcpu=<target CPU>');
  writeln('  -F <FPC source directory>');
  writeln('  --fpcsrcdir=<FPC source directory>');
end;

procedure TTestFPCSourceUnitRules.Error(Msg: string; DoWriteHelp: Boolean);
begin
  writeln('Error: ',Msg);
  if DoWriteHelp then begin
    writeln;
    WriteHelp;
  end;
  Halt;
end;

procedure TTestFPCSourceUnitRules.WriteCompilerInfo(
  ConfigCache: TFPCTargetConfigCache);
var
  i: Integer;
  CfgFile: TFPCConfigFileState;
begin
  writeln('Compiler=',ConfigCache.Compiler);
  writeln('TargetOS=',ConfigCache.TargetOS);
  writeln('TargetCPU=',ConfigCache.TargetCPU);
  writeln('Options=',ConfigCache.CompilerOptions);
  writeln('RealCompiler=',ConfigCache.RealCompiler);
  writeln('RealTargetOS=',ConfigCache.RealTargetOS);
  writeln('RealTargetCPU=',ConfigCache.RealTargetCPU);
  if ConfigCache.ConfigFiles<>nil then begin
    for i:=0 to ConfigCache.ConfigFiles.Count-1 do begin
      CfgFile:=ConfigCache.ConfigFiles[i];
      writeln('Config=',CfgFile.Filename,' Exists=',CfgFile.FileExists);
    end;
  end;
end;

var
  Application: TTestFPCSourceUnitRules;
begin
  Application:=TTestFPCSourceUnitRules.Create(nil);
  Application.Title:='TestFPCSrcUnitRules';
  Application.Run;
  Application.Free;
end.

