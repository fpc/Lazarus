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
unit IDEOptionsIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  Laz2_XMLCfg, LazMethodList;

const
  NoParent = -1;

type
  TIDEOptionsHandler = (
    iohBeforeRead,
    iohAfterRead,
    iohBeforeWrite,
    iohAfterWrite,
    iohDestroy
    );
  TIDEOptionsHandlers = set of TIDEOptionsHandler;

  TIDEOptionsEditorSetting = (
    ioesReadOnly      // open options editor read only
  );
  TIDEOptionsEditorSettings = set of TIDEOptionsEditorSetting;

  TIDEOptionsWriteEvent = procedure(Sender: TObject; Restore: boolean) of object;

  TAbstractDesktopDockingOpt = class
  public
    constructor Create; virtual;
    procedure StoreWindowPositions; virtual; abstract;
    procedure LoadDefaults; virtual; abstract;
    procedure Load(Path: String; aXMLCfg: TRttiXMLConfig); virtual; abstract;
    procedure Save(Path: String; aXMLCfg: TRttiXMLConfig); virtual; abstract;
    procedure ImportSettingsFromIDE; virtual; abstract;
    procedure ExportSettingsToIDE; virtual; abstract;
    function RestoreDesktop: Boolean; virtual; abstract;
    procedure Assign(Source: TAbstractDesktopDockingOpt); virtual; abstract;
  end;
  TAbstractDesktopDockingOptClass = class of TAbstractDesktopDockingOpt;

  { TAbstractIDEOptions base class for all option containers }

  TAbstractIDEOptions = class(TPersistent)
  private
    fHandlers: array[TIDEOptionsHandler] of TMethodList;
    FOnAfterRead: TNotifyEvent;
    FOnAfterWrite: TIDEOptionsWriteEvent;
    FOnBeforeRead: TNotifyEvent;
    FOnBeforeWrite: TIDEOptionsWriteEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    class function GetGroupCaption: string; virtual; abstract;
    class function GetInstance: TAbstractIDEOptions; virtual; abstract;

    procedure DoBeforeRead; virtual;
    procedure DoAfterRead; virtual;
    procedure DoBeforeWrite(Restore: boolean); virtual;
    procedure DoAfterWrite(Restore: boolean); virtual;

    procedure AddHandlerBeforeRead(const Handler: TNotifyEvent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerBeforeRead(const Handler: TNotifyEvent);
    procedure AddHandlerAfterRead(const Handler: TNotifyEvent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerAfterRead(const Handler: TNotifyEvent);
    procedure AddHandlerBeforeWrite(const Handler: TIDEOptionsWriteEvent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerBeforeWrite(const Handler: TIDEOptionsWriteEvent);
    procedure AddHandlerAfterWrite(const Handler: TIDEOptionsWriteEvent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerAfterWrite(const Handler: TIDEOptionsWriteEvent);
    procedure AddHandlerDestroy(const Handler: TNotifyEvent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerDestroy(const Handler: TNotifyEvent);

    property OnBeforeRead: TNotifyEvent read FOnBeforeRead write FOnBeforeRead;
    property OnAfterRead: TNotifyEvent read FOnAfterRead write FOnAfterRead;
    property OnBeforeWrite: TIDEOptionsWriteEvent read FOnBeforeWrite write FOnBeforeWrite;
    property OnAfterWrite: TIDEOptionsWriteEvent read FOnAfterWrite write FOnAfterWrite;
  end;
  TAbstractIDEOptionsClass = class of TAbstractIDEOptions;

  TAbstractIDEEnvironmentOptions = class(TAbstractIDEOptions);
  TAbstractIDEHelpOptions = class(TAbstractIDEEnvironmentOptions);

  TOnLoadIDEOptions = procedure(Sender: TObject; AOptions: TAbstractIDEOptions) of object;
  TOnSaveIDEOptions = procedure(Sender: TObject; AOptions: TAbstractIDEOptions) of object;

  TOnAddToRecent = procedure(Sender: TObject; AFileName: string; var AAllow: Boolean) of object;
  TIDERecentHandler = (irhOpenFiles, irhProjectFiles, irhPackageFiles);

  TIDEEnvironmentOptions = class(TAbstractIDEEnvironmentOptions)
  private
    fRecentHandlers: array[TIDERecentHandler] of TMethodList;

    procedure DoCallRecentHandlers(AHandler: TIDERecentHandler;
      const AFileName: string; var AAllow: Boolean);
  protected
    procedure DoAddToRecentOpenFiles(const AFileName: string; var AAllow: Boolean);
    procedure DoAddToRecentProjectFiles(const AFileName: string; var AAllow: Boolean);
    procedure DoAddToRecentPackageFiles(const AFileName: string; var AAllow: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure AddToRecentOpenFiles(const AFilename: string); virtual; abstract;
    procedure RemoveFromRecentOpenFiles(const AFilename: string); virtual; abstract;
    procedure AddToRecentProjectFiles(const AFilename: string); virtual; abstract;
    procedure RemoveFromRecentProjectFiles(const AFilename: string); virtual; abstract;
    Procedure GetRecentFiles(aType: TIDERecentHandler; aList : TStrings); virtual; abstract;
    procedure AddToRecentPackageFiles(const AFilename: string); virtual; abstract;
    procedure RemoveFromRecentPackageFiles(const AFilename: string); virtual; abstract;


    procedure AddHandlerAddToRecentOpenFiles(Handler: TOnAddToRecent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerAddToRecentOpenFiles(Handler: TOnAddToRecent);
    procedure AddHandlerAddToRecentProjectFiles(Handler: TOnAddToRecent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerAddToRecentProjectFiles(Handler: TOnAddToRecent);
    procedure AddHandlerAddToRecentPackageFiles(Handler: TOnAddToRecent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerAddToRecentPackageFiles(Handler: TOnAddToRecent);
  end;

var
  IDEEnvironmentOptions: TIDEEnvironmentOptions;

const
  // predefined environment options groups
  GroupEnvironment  = 100;
    EnvOptionsFiles         = 100;
    EnvOptionsDesktop       = 200;
    EnvOptionsWindow        = 300;
    EnvOptionsToolbar       = 320;
    EnvOptionsEditorToolbar = 330;
    EnvOptionsCompPalette   = 350;
    EnvOptionsFormEd        = 400;
    EnvOptionsOI            = 500;
    EnvOptionsMessages      = 550;
    EnvOptionsFpDoc         = 600;
    EnvOptionsBackup        = 700;
    EnvOptionsNaming        = 800;
    EnvOptionsFileFilters   = 900;

  GroupEditor       = 200;
    EdtOptionsGeneral     = 100;
      EdtOptionsIndent      = 110;
      EdtOptionsGeneralMisc = 120;
    EdtOptionsDisplay     = 200;
      EdtOptionsColors      = 500;
      EdtOptionsMarkup      = 502;
      EdtOptionsUserDefined = 504;
    EdtOptionsKeys        = 300;
    EdtOptionsMouse       = 400;
    EdtOptionsMouseAdv    = 401;
    EdtOptionsCodetools   = 600;
    EdtOptionsCodeFolding = 700;
      EdtOptionsCodeFoldingMouse = 701;
    EdtOptionsDrawDivider = 800;
    EdtOptionsMultiWindow = 900;

  GroupCodetools    = 300;
    CdtOptionsGeneral         = 100;
    CdtOptionsClassCompletion = 200;
    CdtOptionsCodeCreation    = 300;
    CdtOptionsWords           = 400;
    CdtOptionsLineSplitting   = 500;
    CdtOptionsSpace           = 600;
    CdtOptionsIdentCompletion = 700;

  GroupCodeExplorer = 350;
    cdeOptionsUpdate     = 100;
    cdeOptionsCategories = 200;
    cdeOptionsFigures    = 300;

  GroupDebugger     = 400;
    DbgOptionsGeneral            = 100;
    DbgOptionsEventLog           = 200;
    DbgOptionsLanguageExceptions = 300;
    DbgOptionsSignals            = 400;

  GroupHelp         = 500;
    HlpOptionsGeneral = 100;

  // predefined project options groups
  GroupProject      = 100100;
    ProjectOptionsApplication = 100;
    ProjectOptionsForms       = 200;
    ProjectOptionsLazDoc      = 300;
    ProjectOptionsSave        = 400;
    ProjectOptionsVersionInfo = 500;
    ProjectOptionsResources   = 550;
    ProjectOptionsI18N        = 600;
    ProjectOptionsMisc        = 700;

  GroupPackage      = 200100;
    PackageOptionsUsage        = 100;
    PackageOptionsDescription  = 200;
    PackageOptionsIntegration  = 300;
    PackageOptionsProvides     = 400;
    PackageOptionsI18N         = 500;

  GroupCompiler     = 100200;
    CompilerOptionsBuildModes            = 0100;
    CompilerOptionsSearchPaths           = 0200;
    CompilerOptionsConfigTarget          = 0250;
    CompilerOptionsParsing               = 0300;
    CompilerOptionsCodeGeneration        = 0400;
    CompilerOptionsDebugging             = 0500;
    CompilerOptionsVerbosity             = 0600;
    CompilerOptionsMessages              = 0700;
    CompilerOptionsOther                 = 0800;
    CompilerOptionsConditional           = 0900; // IDE Macros
    CompilerOptionsAdditionsAndOverrides = 1000;
    CompilerOptionsInherited             = 1100;
    CompilerOptionsCompilation           = 1200;

  GroupPkgCompiler  = 200200;

  GroupPackageFile  = 200300;

var
  HasGUI: boolean = true; // lazbuild sets this to false

implementation

{ TIDEEnvironmentOptions }

constructor TIDEEnvironmentOptions.Create;
var
  I: TIDERecentHandler;
begin
  inherited Create;

  for I := Low(fRecentHandlers) to High(fRecentHandlers) do
    fRecentHandlers[I] := TMethodList.Create;
end;

procedure TIDEEnvironmentOptions.AddHandlerAddToRecentOpenFiles(
  Handler: TOnAddToRecent; const AsFirst: boolean);
begin
  fRecentHandlers[irhOpenFiles].Add(TMethod(Handler), AsFirst);
end;

procedure TIDEEnvironmentOptions.AddHandlerAddToRecentPackageFiles(
  Handler: TOnAddToRecent; const AsFirst: boolean);
begin
  fRecentHandlers[irhPackageFiles].Add(TMethod(Handler), AsFirst);
end;

procedure TIDEEnvironmentOptions.AddHandlerAddToRecentProjectFiles(
  Handler: TOnAddToRecent; const AsFirst: boolean);
begin
  fRecentHandlers[irhProjectFiles].Add(TMethod(Handler), AsFirst);
end;

destructor TIDEEnvironmentOptions.Destroy;
var
  I: TIDERecentHandler;
begin
  for I := Low(fRecentHandlers) to High(fRecentHandlers) do
    fRecentHandlers[I].Free;

  inherited Destroy;
end;

procedure TIDEEnvironmentOptions.DoAddToRecentOpenFiles(
  const AFileName: string; var AAllow: Boolean);
begin
  DoCallRecentHandlers(irhOpenFiles, AFileName, AAllow);
end;

procedure TIDEEnvironmentOptions.DoAddToRecentPackageFiles(
  const AFileName: string; var AAllow: Boolean);
begin
  DoCallRecentHandlers(irhPackageFiles, AFileName, AAllow);
end;

procedure TIDEEnvironmentOptions.DoAddToRecentProjectFiles(
  const AFileName: string; var AAllow: Boolean);
begin
  DoCallRecentHandlers(irhProjectFiles, AFileName, AAllow);
end;

procedure TIDEEnvironmentOptions.DoCallRecentHandlers(
  AHandler: TIDERecentHandler; const AFileName: string; var AAllow: Boolean);
var
  xMethod: TOnAddToRecent;
  I: Integer;
begin
  for I := 0 to fRecentHandlers[AHandler].Count-1 do
  begin
    xMethod := TOnAddToRecent(fRecentHandlers[AHandler][I]);
    xMethod(Self, AFileName, AAllow);
  end;
end;

procedure TIDEEnvironmentOptions.RemoveHandlerAddToRecentOpenFiles(
  Handler: TOnAddToRecent);
begin
  fRecentHandlers[irhOpenFiles].Remove(TMethod(Handler));
end;

procedure TIDEEnvironmentOptions.RemoveHandlerAddToRecentPackageFiles(
  Handler: TOnAddToRecent);
begin
  fRecentHandlers[irhPackageFiles].Remove(TMethod(Handler));
end;

procedure TIDEEnvironmentOptions.RemoveHandlerAddToRecentProjectFiles(
  Handler: TOnAddToRecent);
begin
  fRecentHandlers[irhProjectFiles].Remove(TMethod(Handler));
end;

{ TAbstractDesktopDockingOpt }

constructor TAbstractDesktopDockingOpt.Create;
begin
  inherited Create;
end;

{ TAbstractIDEOptions }

constructor TAbstractIDEOptions.Create;
var
  h: TIDEOptionsHandler;
begin
  for h:=low(TIDEOptionsHandler) to high(TIDEOptionsHandler) do
    fHandlers[h]:=TMethodList.Create;
end;

destructor TAbstractIDEOptions.Destroy;
var
  h: TIDEOptionsHandler;
begin
  for h:=low(TIDEOptionsHandler) to high(TIDEOptionsHandler) do
    FreeAndNil(fHandlers[h]);
  inherited Destroy;
end;

procedure TAbstractIDEOptions.BeforeDestruction;
begin
  inherited BeforeDestruction;
  fHandlers[iohDestroy].CallNotifyEvents(Self);
end;

procedure TAbstractIDEOptions.DoBeforeRead;
begin
  if Assigned(FOnBeforeRead) then
    FOnBeforeRead(Self);
  fHandlers[iohBeforeRead].CallNotifyEvents(Self);
end;

procedure TAbstractIDEOptions.DoAfterRead;
begin
  if Assigned(FOnAfterRead) then
    FOnAfterRead(Self);
  fHandlers[iohAfterRead].CallNotifyEvents(Self);
end;

procedure TAbstractIDEOptions.DoBeforeWrite(Restore: boolean);
var
  i: LongInt;
begin
  if Assigned(FOnBeforeWrite) then
    FOnBeforeWrite(Self,Restore);
  i:=fHandlers[iohBeforeWrite].Count;
  while fHandlers[iohBeforeWrite].NextDownIndex(i) do
    TIDEOptionsWriteEvent(fHandlers[iohBeforeWrite][i])(Self,Restore);
end;

procedure TAbstractIDEOptions.DoAfterWrite(Restore: boolean);
var
  i: LongInt;
begin
  if Assigned(FOnAfterWrite) then
    FOnAfterWrite(Self,Restore);
  i:=fHandlers[iohAfterWrite].Count;
  while fHandlers[iohAfterWrite].NextDownIndex(i) do
    TIDEOptionsWriteEvent(fHandlers[iohAfterWrite][i])(Self,Restore);
end;

procedure TAbstractIDEOptions.AddHandlerBeforeRead(const Handler: TNotifyEvent;
  const AsFirst: boolean);
begin
  fHandlers[iohBeforeRead].Add(TMethod(Handler),AsFirst);
end;

procedure TAbstractIDEOptions.RemoveHandlerBeforeRead(const Handler: TNotifyEvent);
begin
  fHandlers[iohBeforeRead].Remove(TMethod(Handler));
end;

procedure TAbstractIDEOptions.AddHandlerAfterRead(const Handler: TNotifyEvent;
  const AsFirst: boolean);
begin
  fHandlers[iohAfterRead].Add(TMethod(Handler),AsFirst);
end;

procedure TAbstractIDEOptions.RemoveHandlerAfterRead(const Handler: TNotifyEvent);
begin
  fHandlers[iohAfterRead].Remove(TMethod(Handler));
end;

procedure TAbstractIDEOptions.AddHandlerBeforeWrite(
  const Handler: TIDEOptionsWriteEvent; const AsFirst: boolean);
begin
  fHandlers[iohBeforeWrite].Add(TMethod(Handler),AsFirst);
end;

procedure TAbstractIDEOptions.RemoveHandlerBeforeWrite(
  const Handler: TIDEOptionsWriteEvent);
begin
  fHandlers[iohBeforeWrite].Remove(TMethod(Handler));
end;

procedure TAbstractIDEOptions.AddHandlerAfterWrite(
  const Handler: TIDEOptionsWriteEvent; const AsFirst: boolean);
begin
  fHandlers[iohAfterWrite].Add(TMethod(Handler),AsFirst);
end;

procedure TAbstractIDEOptions.RemoveHandlerAfterWrite(
  const Handler: TIDEOptionsWriteEvent);
begin
  fHandlers[iohAfterWrite].Remove(TMethod(Handler));
end;

procedure TAbstractIDEOptions.AddHandlerDestroy(const Handler: TNotifyEvent;
  const AsFirst: boolean);
begin
  fHandlers[iohDestroy].Add(TMethod(Handler),AsFirst);
end;

procedure TAbstractIDEOptions.RemoveHandlerDestroy(const Handler: TNotifyEvent);
begin
  fHandlers[iohDestroy].Remove(TMethod(Handler));
end;

end.
