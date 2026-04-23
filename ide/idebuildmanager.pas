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
}
unit IdeBuildManager;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, AVL_Tree, System.UITypes,
  // LazUtils
  LazUTF8, LazMethodList, LazLoggerBase,
  // CodeTools
  CodeToolManager, DefineTemplates,
  // IdeIntf
  IDEDialogs, LazIDEIntf, SrcEditorIntf,
  // IdeConfig
  EnvironmentOpts,
  // IdeUtils
  InputHistory,
  // IdeProject
  Project, BuildManager,
  // IDE
  LazarusIDEStrConsts, EditableProject, FPCSrcScan;

type

  { TIdeBuildManager }

  TIdeBuildManager = class(TBuildManager)
  private
    FFPCSrcScans: TFPCSrcScans;
    procedure DoRescanFPCDirectoryCache(Sender: TObject);
  protected
    function EditorUnitInfoModified(AnUnitInfo: TUnitInfo): boolean; override;
    procedure MaybeAddIgnorePath(const aPath: string); override;
    procedure ScanFPCSource(Directory: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFPCFrontEndOptions: string; override;
    procedure SetupInputHistories(aInputHist: TInputHistories);
    procedure SetupTransferMacros; override;
  end;

procedure RunBootHandlers(ht: TLazarusIDEBootHandlerType); external name 'ideintf_LazIDEIntf_RunBootHandlers';


implementation

{ TIdeBuildManager }

constructor TIdeBuildManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CodeToolBoss.OnRescanFPCDirectoryCache:=@DoRescanFPCDirectoryCache;
end;

destructor TIdeBuildManager.Destroy;
begin
  FreeAndNil(FFPCSrcScans);
  if SameMethod(TMethod(CodeToolBoss.OnRescanFPCDirectoryCache),
                TMethod(@DoRescanFPCDirectoryCache))
  then
    CodeToolBoss.OnRescanFPCDirectoryCache:=nil;
  inherited Destroy;
end;

procedure TIdeBuildManager.DoRescanFPCDirectoryCache(Sender: TObject);
var
  Files: TStringList;
  FPCSrcDir: string;
begin
  FPCSrcDir := EnvironmentOptions.GetParsedFPCSourceDirectory;
  Files := GatherFilesInFPCSources(FPCSrcDir, nil);
  if Files<>nil then
    try
      ApplyFPCSrcFiles(FPCSrcDir, Files);
    finally
      Files.Free;
    end;
end;

function TIdeBuildManager.EditorUnitInfoModified(AnUnitInfo: TUnitInfo): boolean;
var
  EditComp: TSourceEditorInterface;
begin
  Result:=false;
  if AnUnitInfo=nil then exit;
  if TEditableUnitInfo(AnUnitInfo).EditorInfoCount=0 then exit;
  EditComp:=TEditableUnitInfo(AnUnitInfo).EditorInfo[0].EditorComponent;
  Result:=(EditComp<>nil) and EditComp.Modified;
end;

procedure TIdeBuildManager.MaybeAddIgnorePath(const aPath: string);
var
  MsgResult: Integer;
begin
  Assert(Assigned(InputHistories), 'TIdeBuildManager.MaybeAddIgnorePath: InputHistories=Nil');
  if InputHistories.Ignores.Find(aPath)=nil then begin
    MsgResult:=IDEMessageDialog(lisCCOWarningCaption,
      lisTheCurrentFPCHasNoConfigFileItWillProbablyMissSome,mtWarning,[mbOk,mbIgnore]);
    if MsgResult=mrIgnore then
      InputHistories.Ignores.Add(aPath,iiidIDERestart);
  end;
end;

procedure TIdeBuildManager.ScanFPCSource(Directory: string);
begin
  // start scanning the fpc source directory in the background
  {$IFDEF VerboseFPCSrcScan}
  debugln(['TBuildManager.RescanCompilerDefines scanning fpc sources:',Directory]);
  {$ENDIF}
  if FFPCSrcScans=nil then
    FFPCSrcScans:=TFPCSrcScans.Create(Self);
  FFPCSrcScans.Scan(Directory);
end;

function TIdeBuildManager.GetFPCFrontEndOptions: string;
begin
  Result:=inherited GetFPCFrontEndOptions;
  if LazarusIDE<>nil then
    if not LazarusIDE.CallHandlerGetFPCFrontEndParams(Self,Result) then
      DebugLn(['Warning: TIdeBuildManager.GetFPCFrontEndOptions: '
              +'LazarusIDE.CallHandlerGetFPCFrontEndParams failed Result="',Result,'"']);
  //Result:=UTF8Trim(Result);
  Assert(Result=UTF8Trim(Result), 'TIdeBuildManager.GetFPCFrontEndOptions: Trim Result.');
end;

procedure TIdeBuildManager.SetupInputHistories(aInputHist: TInputHistories);
begin
  aInputHist.SetLazarusDefaultFilename;
  aInputHist.Load;
end;

procedure TIdeBuildManager.SetupTransferMacros;
begin
  inherited SetupTransferMacros;
  RunBootHandlers(libhTransferMacrosCreated);
end;

end.

