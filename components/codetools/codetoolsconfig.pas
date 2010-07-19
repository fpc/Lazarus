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
    This unit helps to setup and configure the codetools.

  Example:
    Create an empty unit empty.pas and do
  
    Options:=TCodeToolsOptions.Create;
    Options.LoadFromFile('config.xml');
    Options.FPCPath:='/usr/bin/ppc386';
    Options.FPCSrcDir:='/home/username/freepascal/fpc';
    Options.LazarusSrcDir:='/home/username/pascal/lazarus';
    Options.ProjectDir:='/home/username/pascal/project1/';
    Options.TestPascalFile:=Options.ProjectDir+'empty.pas';
    CodeToolBoss.Init(Options);
    Options.SaveToFile('config.xml');
    Options.Free;
    
    .. use CodeToolBoss ..
    
}
unit CodeToolsConfig;

{$mode objfpc}{$H+}

{$I codetools.inc}

interface

uses
  Classes, SysUtils, Laz_XMLCfg,
  {$IFDEF NewXMLCfg}
  Laz2_XMLRead, Laz2_XMLWrite, Laz2_DOM,
  {$ELSE}
  Laz_XMLRead, Laz_XMLWrite, Laz_DOM,
  {$ENDIF}
  FileProcs, CodeCache, DefineTemplates;
  
type

  { TCodeBufXMLConfig }

  TCodeBufXMLConfig = class(TXMLConfig)
  private
    FCodeCache: TCodeCache;
  protected
    fKeepFileAttributes: boolean;
    procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String
            ); override;
    procedure WriteXMLFile(ADoc: TXMLDocument; const AFileName: String);
           override;
    function GetCache: TCodeCache;
  public
    constructor CreateWithCache(AFilename: string;
      LoadContent: boolean = true;        // init/load from disk
      LoadFileAttributes: boolean = true; // load lineending and encoding
      ASource: string = '';               // init with this source
      ACache: TCodeCache = nil);
    property CodeCache: TCodeCache read FCodeCache write FCodeCache;
    property KeepFileAttributes: boolean read fKeepFileAttributes write fKeepFileAttributes;
  end;

var
  DefaultConfigCodeCache: TCodeCache = nil; // set by CodeToolBoss

type

  { TCodeToolsOptions }

  TCodeToolsOptions = class
  private
    FConfigCaches: TFPCTargetConfigCaches;
    FFPCOptions: string;
    FFPCPath: string;
    FFPCSrcDir: string;
    FFPCUnitPath: string;
    FLazarusSrcDir: string;
    FLazarusSrcOptions: string;
    FLCLWidgetType: string;
    FModified: boolean;
    FPPUExt: string;
    FProjectDir: string;
    FSourceCaches: TFPCSourceCaches;
    FTargetOS: string;
    FTargetProcessor: string;
    FTestPascalFile: string;
    FUnitLinkList: string;
    FUnitLinkListValid: boolean;
    procedure SetFPCOptions(const AValue: string);
    procedure SetFPCPath(const AValue: string);
    procedure SetFPCSrcDir(const AValue: string);
    procedure SetFPCUnitPath(const AValue: string);
    procedure SetLazarusSrcDir(const AValue: string);
    procedure SetLCLWidgetType(const AValue: string);
    procedure SetLazarusSrcOptions(const AValue: string);
    procedure SetModified(const AValue: boolean);
    procedure SetPPUExt(const AValue: string);
    procedure SetProjectDir(const AValue: string);
    procedure SetTargetOS(const AValue: string);
    procedure SetTargetProcessor(const AValue: string);
    procedure SetTestPascalFile(const AValue: string);
    procedure SetUnitLinkList(const AValue: string);
    procedure SetUnitLinkListValid(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitWithEnvironmentVariables;
    function FindDefaultCompilerFilename: string;

    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);

    property Modified: boolean read FModified write SetModified;

    // FPC
    property FPCSrcDir: string read FFPCSrcDir write SetFPCSrcDir; // e.g. /usr/share/fpcsrc
    property FPCPath: string read FFPCPath write SetFPCPath; // e.g. /usr/bin/fpc or /usr/bin/ppc386
    property FPCOptions: string read FFPCOptions write SetFPCOptions; // extra options for fpc
    property TargetOS: string read FTargetOS write SetTargetOS;
    property TargetProcessor: string read FTargetProcessor write SetTargetProcessor;
    property TestPascalFile: string read FTestPascalFile write SetTestPascalFile; // points to an empty unit
    property FPCUnitPath: string read FFPCUnitPath write SetFPCUnitPath;
    property PPUExt: string read FPPUExt write SetPPUExt;
    property SourceCaches: TFPCSourceCaches read FSourceCaches;
    property ConfigCaches: TFPCTargetConfigCaches read FConfigCaches;
    property UnitLinkListValid: boolean read FUnitLinkListValid write SetUnitLinkListValid;
    property UnitLinkList: string read FUnitLinkList write SetUnitLinkList;

    // Project
    property ProjectDir: string read FProjectDir write SetProjectDir;
    
    // Lazarus
    property LazarusSrcDir: string read FLazarusSrcDir write SetLazarusSrcDir;
    property LCLWidgetType: string read FLCLWidgetType write SetLCLWidgetType;
    property LazarusSrcOptions: string read FLazarusSrcOptions write SetLazarusSrcOptions;
  end;

implementation

{ TCodeToolsOptions }

procedure TCodeToolsOptions.SetFPCOptions(const AValue: string);
begin
  if FFPCOptions=AValue then exit;
  FFPCOptions:=AValue;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetFPCPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ExpandFileNameUTF8(AValue);
  if FFPCPath=NewValue then exit;
  FFPCPath:=NewValue;
  FUnitLinkListValid:=false;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetFPCSrcDir(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ExpandFileNameUTF8(AValue);
  if FFPCSrcDir=NewValue then exit;
  FFPCSrcDir:=NewValue;
  FUnitLinkListValid:=false;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetFPCUnitPath(const AValue: string);
begin
  if FFPCUnitPath=AValue then exit;
  FFPCUnitPath:=AValue;
  FUnitLinkListValid:=false;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetLazarusSrcDir(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=ExpandFileNameUTF8(AValue);
  if FLazarusSrcDir=NewValue then exit;
  FLazarusSrcDir:=NewValue;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetLCLWidgetType(const AValue: string);
begin
  if FLCLWidgetType=AValue then exit;
  FLCLWidgetType:=AValue;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetLazarusSrcOptions(const AValue: string);
begin
  if FLazarusSrcOptions=AValue then exit;
  FLazarusSrcOptions:=AValue;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
end;

procedure TCodeToolsOptions.SetPPUExt(const AValue: string);
begin
  if FPPUExt=AValue then exit;
  FPPUExt:=AValue;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetProjectDir(const AValue: string);
begin
  if FProjectDir=AValue then exit;
  FProjectDir:=AppendPathDelim(AValue);
  Modified:=true;
end;

procedure TCodeToolsOptions.SetTargetOS(const AValue: string);
begin
  if FTargetOS=AValue then exit;
  FTargetOS:=AValue;
  FUnitLinkListValid:=false;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetTargetProcessor(const AValue: string);
begin
  if FTargetProcessor=AValue then exit;
  FTargetProcessor:=AValue;
  FUnitLinkListValid:=false;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetTestPascalFile(const AValue: string);
begin
  if FTestPascalFile=AValue then exit;
  FTestPascalFile:=AValue;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetUnitLinkList(const AValue: string);
begin
  if FUnitLinkList=AValue then exit;
  FUnitLinkList:=AValue;
  Modified:=true;
end;

procedure TCodeToolsOptions.SetUnitLinkListValid(const AValue: boolean);
begin
  if FUnitLinkListValid=AValue then exit;
  FUnitLinkListValid:=AValue;
  Modified:=true;
end;

constructor TCodeToolsOptions.Create;
begin
  FPPUExt:='.ppu';
  FLCLWidgetType:='gtk2';
  FConfigCaches:=TFPCTargetConfigCaches.Create(nil);
  FSourceCaches:=TFPCSourceCaches.Create(nil);
end;

destructor TCodeToolsOptions.Destroy;
begin
  FreeAndNil(FConfigCaches);
  FreeAndNil(FSourceCaches);
  inherited Destroy;
end;

procedure TCodeToolsOptions.InitWithEnvironmentVariables;
begin
  if GetEnvironmentVariableUTF8('PP')<>'' then
    FPCPath:=GetEnvironmentVariableUTF8('PP')
  else if (FPCPath='') or not FileExistsCached(FPCPath) then
    FPCPath:=FindDefaultCompilerFilename;
  if GetEnvironmentVariableUTF8('FPCDIR')<>'' then
    FPCSrcDir:=GetEnvironmentVariableUTF8('FPCDIR');
  if GetEnvironmentVariableUTF8('LAZARUSDIR')<>'' then
    LazarusSrcDir:=GetEnvironmentVariableUTF8('LAZARUSDIR');
  if GetEnvironmentVariableUTF8('FPCTARGET')<>'' then
    TargetOS:=GetEnvironmentVariableUTF8('FPCTARGET');
  if GetEnvironmentVariableUTF8('FPCTARGETCPU')<>'' then
    TargetProcessor:=GetEnvironmentVariableUTF8('FPCTARGETCPU');
end;

function TCodeToolsOptions.FindDefaultCompilerFilename: string;
begin
  Result:=SearchFileInPath(GetDefaultCompilerFilename,'',
                           GetEnvironmentVariableUTF8('PATH'),':',ctsfcDefault);
end;

procedure TCodeToolsOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'FPC/Options/Value',FPCOptions,'');
  XMLConfig.SetDeleteValue(Path+'FPC/CompilerPath/Value',FPCPath,'');
  XMLConfig.SetDeleteValue(Path+'FPC/SrcDir/Value',FPCSrcDir,'');
  XMLConfig.SetDeleteValue(Path+'FPC/UnitPath/Value',FPCUnitPath,'');
  XMLConfig.SetDeleteValue(Path+'FPC/TargetOS/Value',TargetOS,'');
  XMLConfig.SetDeleteValue(Path+'FPC/TargetProcessor/Value',TargetProcessor,'');
  XMLConfig.SetDeleteValue(Path+'FPC/PPUExt/Value',PPUExt,'.ppu');
  XMLConfig.SetDeleteValue(Path+'FPC/TestPascalFile/Value',TestPascalFile,'');
  XMLConfig.SetDeleteValue(Path+'FPC/UnitLinkList/Value',UnitLinkList,'');
  XMLConfig.SetDeleteValue(Path+'FPC/UnitLinkList/Valid',UnitLinkListValid,false);
  XMLConfig.SetDeleteValue(Path+'Lazarus/SrcDir/Value',LazarusSrcDir,'');
  XMLConfig.SetDeleteValue(Path+'Lazarus/SrcDirOptions/Value',LazarusSrcOptions,'');
  XMLConfig.SetDeleteValue(Path+'Lazarus/LCLWidgetType/Value',LCLWidgetType,'');
  XMLConfig.SetDeleteValue(Path+'Project/Dir/Value',ProjectDir,'');
  FConfigCaches.SaveToXMLConfig(XMLConfig,Path+'FPCConfigCaches/');
  FSourceCaches.SaveToXMLConfig(XMLConfig,Path+'FPCSrcDirCaches/');
  Modified:=false;
end;

procedure TCodeToolsOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  i: Integer;
  UnitPath: string;
begin
  FPCOptions:=XMLConfig.GetValue(Path+'FPC/Options/Value','');
  FPCPath:=XMLConfig.GetValue(Path+'FPC/CompilerPath/Value','');
  FPCSrcDir:=XMLConfig.GetValue(Path+'FPC/SrcDir/Value','');
  UnitPath:=XMLConfig.GetValue(Path+'FPC/UnitPath/Value','');
  for i:=1 to length(UnitPath) do
    if (UnitPath[i] in [#0..#8,#10..#31]) then
      UnitPath[i]:=';';
  FPCUnitPath:=UnitPath;
  TargetOS:=XMLConfig.GetValue(Path+'FPC/TargetOS/Value','');
  TargetProcessor:=XMLConfig.GetValue(Path+'FPC/TargetProcessor/Value','');
  PPUExt:=XMLConfig.GetValue(Path+'FPC/PPUExt/Value','.ppu');
  TestPascalFile:=XMLConfig.GetValue(Path+'FPC/TestPascalFile/Value','');
  UnitLinkList:=XMLConfig.GetValue(Path+'FPC/UnitLinkList/Value','');
  // UnitLinkListValid must be set as last
  UnitLinkListValid:=XMLConfig.GetValue(Path+'FPC/UnitLinkList/Valid',false);
  FConfigCaches.LoadFromXMLConfig(XMLConfig,Path+'FPCConfigCaches/');
  FSourceCaches.LoadFromXMLConfig(XMLConfig,Path+'FPCSrcDirCaches/');

  LazarusSrcDir:=XMLConfig.GetValue(Path+'Lazarus/SrcDir/Value','');
  LazarusSrcOptions:=XMLConfig.GetValue(Path+'Lazarus/SrcDirOptions/Value','');
  LCLWidgetType:=XMLConfig.GetValue(Path+'Lazarus/LCLWidgetType/Value','');
  ProjectDir:=XMLConfig.GetValue(Path+'Project/Dir/Value','');
  Modified:=false;
end;

procedure TCodeToolsOptions.SaveToFile(const Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.CreateClean(Filename);
  try
    SaveToXMLConfig(XMLConfig,'CodeToolsOptions/');
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
end;

procedure TCodeToolsOptions.LoadFromFile(const Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    LoadFromXMLConfig(XMLConfig,'CodeToolsOptions/');
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
end;

{ TCodeBufXMLConfig }

procedure TCodeBufXMLConfig.ReadXMLFile(out ADoc: TXMLDocument;
  const AFilename: String);
var
  Buf: TCodeBuffer;
  ms: TMemoryStream;
  Cache: TCodeCache;
begin
  Cache:=GetCache;
  if Cache<>nil then begin
    Buf:=Cache.LoadFile(AFilename);
    if Buf<>nil then begin
      fKeepFileAttributes:=true;
      ms:=TMemoryStream.Create;
      try
        Buf.SaveToStream(ms);
        ms.Position:=0;
        {$IFDEF NewXMLCfg}
        Laz2_XMLRead.ReadXMLFile(ADoc, ms);
        {$ELSE}
        Laz_XMLRead.ReadXMLFile(ADoc, ms);
        {$ENDIF}
        exit; // success
      finally
        ms.Free;
      end;
    end;
  end;
  // try default (this will create the normal exceptions)
  inherited ReadXMLFile(ADoc, AFilename);
end;

procedure TCodeBufXMLConfig.WriteXMLFile(ADoc: TXMLDocument;
  const AFileName: String);
var
  Buf: TCodeBuffer;
  ms: TMemoryStream;
  Cache: TCodeCache;
begin
  Cache:=GetCache;
  if Cache<>nil then begin
    Buf:=nil;
    if (not fKeepFileAttributes) or (not FileExistsCached(AFileName)) then
      Buf:=Cache.CreateFile(AFilename)
    else
      Buf:=Cache.LoadFile(AFilename);
    if Buf<>nil then begin
      fKeepFileAttributes:=true;
      ms:=TMemoryStream.Create;
      try
        {$IFDEF NewXMLCfg}
        Laz2_XMLWrite.WriteXMLFile(ADoc, ms);
        {$ELSE}
        Laz_XMLWrite.WriteXMLFile(ADoc, ms);
        {$ENDIF}
        ms.Position:=0;
        Buf.LoadFromStream(ms);
        if Buf.Save then exit; // success
      finally
        ms.Free;
      end;
    end;
  end;
  // try default (this will create the normal exceptions)
  inherited WriteXMLFile(ADoc, AFileName);
end;

function TCodeBufXMLConfig.GetCache: TCodeCache;
begin
  Result:=CodeCache;
  if Result=nil then
    Result:=DefaultConfigCodeCache;
end;

constructor TCodeBufXMLConfig.CreateWithCache(AFilename: string;
  LoadContent: boolean; LoadFileAttributes: boolean; ASource: string;
  ACache: TCodeCache);
begin
  CodeCache:=ACache;
  fKeepFileAttributes:=LoadFileAttributes;
  if (ASource<>'') then
    inherited CreateWithSource(AFilename,ASource)
  else if LoadContent then
    inherited Create(AFilename)
  else
    inherited CreateClean(AFilename);
end;

end.

