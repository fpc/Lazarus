{  $Id$  }
{
 /***************************************************************************
                    mainbase.pas  -  the "integrated" in IDE
                    ----------------------------------------
  TMainIDEInterface is the ancestor of TMainIDEBase.
  TMainIDEInterface is used by functions/units, that uses several different
  parts of the IDE (designer, source editor, codetools), so they can't be
  assigned to a specific boss and which are yet too small to become a boss of
  their own.


  main.pp      - TMainIDE = class(TMainIDEBase)
                   The highest manager/boss of the IDE. Only lazarus.pp uses
                   this unit.
  mainbase.pas - TMainIDEBase = class(TMainIDEInterface)
                   The ancestor class used by (and only by) the other
                   bosses/managers like debugmanager, pkgmanager.
  mainintf.pas - TMainIDEInterface = class
                   The interface class of the top level functions of the IDE.
                   TMainIDEInterface is used by functions/units, that uses
                   several different parts of the IDE (designer, source editor,
                   codetools), so they can't be assigned to a specific boss and
                   which are yet too small to become a boss of their own.


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
}
unit MainIntf;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, LCLType, LCLIntf, StdCtrls, Buttons, Menus, ComCtrls, SysUtils,
  Controls, Graphics, ExtCtrls, Dialogs, FileCtrl, Forms, CodeToolManager,
  CodeCache, AVL_Tree, SynEditKeyCmds,
  // IDE
  LazConf, LazarusIDEStrConsts,
  ProjectDefs, Project, PublishModule, BuildLazDialog, Compiler,
  {$IFDEF DisablePkgs}
  CompReg, IDEComp,
  {$ELSE}
  ComponentReg,
  {$ENDIF}
  TransferMacros, ObjectInspector, PropEdits, OutputFilter, IDEDefs, MsgView,
  EnvironmentOpts, EditorOptions, CompilerOptions, KeyMapping, IDEProcs,
  Debugger, IDEOptionDefs, CodeToolsDefines, SrcEditorIntf;

type
  // The IDE is at anytime in a specific state:
  TIDEToolStatus = (
    itNone,      // The default mode. All editing allowed.
    itExiting,   // the ide is shutting down
    itBuilder,   // compiling (the project, a package, an external tool)
                 //    Loading/Saving/Debugging is not allowed.
    itDebugger,  // debugging the project.
                 //    Loading/Saving/Compiling is not allowed.
    itCodeTools, // the CodeToolBoss is working and has called the progress
                 //    event.
    itCodeToolAborting,// the CodeToolBoss is working and is about to abort
    itCustom     // this state is not used yet.
    );

  // window in front
  TDisplayState = (
    dsSource,     // focussing sourcenotebook
    dsInspector,  // focussing object inspector after Source
    dsForm,       // focussing designer form
    dsInspector2  // focussing object inspector after form
    );

  // new file flags
  TNewFlag = (
    nfIsPartOfProject, // force IsPartOfProject,
                       //   default is to use a heuristic
    nfIsNotPartOfProject,// forbid IsPartOfProject
    nfOpenInEditor,    // open in editor
    nfSave,            // save file instantly
    nfAddToRecent,     // add file to recent files
    nfQuiet,           // less messages
    nfConvertMacros,   // replace macros in filename
    nfBeautifySrc,     // beautify custom source
    nfCreateDefaultSrc // create initial source based on the type
    );
  TNewFlags = set of TNewFlag;

  // save file flags
  TSaveFlag = (
    sfSaveAs,
    sfSaveToTestDir,
    sfProjectSaving,
    sfCheckAmbigiousFiles
    );
  TSaveFlags = set of TSaveFlag;

  // open file flags
  TOpenFlag = (
    ofProjectLoading,// this open is part of opening a whole project
    ofOnlyIfExists,  // do not auto create non existing files
    ofRevert,        // reload file if already open
    ofQuiet,         // less messages
    ofAddToRecent,   // add file to recent files
    ofRegularFile,   // open as regular file (e.g. do not open projects)
    ofVirtualFile,   // open the virtual file
    ofConvertMacros, // replace macros in filename
    ofUseCache,      // do not update file from disk
    ofMultiOpen      // set during loading multiple files
    );
  TOpenFlags = set of TOpenFlag;

  // revert file flags
  TRevertFlag = (
    rfQuiet
    );
  TRevertFlags = set of TRevertFlag;

  // close file flags
  TCloseFlag = (
    cfSaveFirst, // check if modified and save
    cfProjectClosing
    );
  TCloseFlags = set of TCloseFlag;

  // codetools flags
  TCodeToolsFlag = (
    ctfSwitchToFormSource, // bring source notebook to front and show source of
                           //   current designed form
    ctfActivateAbortMode   // activate the CodeToolBoss.Abortable mode
    );
  TCodeToolsFlags = set of TCodeToolsFlag;

  // find source flags
  TFindSourceFlag = (
    fsfSearchForProject,
    fsfUseIncludePaths,
    fsfUseDebugPath
    );
  TFindSourceFlags = set of TFindSourceFlag;

  { TMainIDEInterface }

  TMainIDEInterface = class(TComponent)
  protected
    function GetToolStatus: TIDEToolStatus; virtual; abstract;
  public
    MacroList: TTransferMacroList;
    HiddenWindowsOnRun: TList; // list of forms, that were automatically hidden
                               // and will be shown when debugged program stops

    property ToolStatus: TIDEToolStatus read GetToolStatus;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateCaption; virtual; abstract;
    procedure HideIDE; virtual; abstract;
    procedure UnhideIDE; virtual; abstract;

    function FindUnitFile(const AFilename: string): string; virtual; abstract;
    function FindSourceFile(const AFilename, BaseDirectory: string;
                            Flags: TFindSourceFlags): string; virtual; abstract;
    procedure GetCurrentUnitInfo(var ActiveSourceEditor: TSourceEditorInterface;
                              var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetUnitInfoForDesigner(ADesigner: TIDesigner;
                              var ActiveSourceEditor: TSourceEditorInterface;
                              var ActiveUnitInfo: TUnitInfo); virtual; abstract;

    function GetTestBuildDir: string; virtual; abstract;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; virtual; abstract;
    function IsTestUnitFilename(const AFilename: string): boolean; virtual; abstract;
    function GetRunCommandLine: string; virtual; abstract;
    procedure GetIDEFileState(Sender: TObject; const AFilename: string;
                        NeededFlags: TIDEFileStateFlags;
                        var ResultFlags: TIDEFileStateFlags); virtual; abstract;

    function DoNewEditorFile(NewUnitType: TNewUnitType;
        NewFilename: string; const NewSource: string;
        NewFlags: TNewFlags): TModalResult; virtual; abstract;
    function DoOpenEditorFile(AFileName:string; PageIndex: integer;
        Flags: TOpenFlags): TModalResult; virtual; abstract;
    function DoInitProjectRun: TModalResult; virtual; abstract;
    function DoOpenMacroFile(Sender: TObject;
        const AFilename: string): TModalResult; virtual; abstract;

    function DoShowProjectInspector: TModalResult; virtual; abstract;
    function DoImExportCompilerOptions(Sender: TObject): TModalResult; virtual; abstract;

    function PrepareForCompile: TModalResult; virtual; abstract;
    function DoSaveBuildIDEConfigs(Flags: TBuildLazarusFlags): TModalResult; virtual; abstract;
    function DoBuildLazarus(Flags: TBuildLazarusFlags): TModalResult; virtual; abstract;
    function DoExecuteCompilationTool(Tool: TCompilationTool;
                                      const WorkingDir, ToolTitle: string
                                      ): TModalResult; virtual; abstract;
    function DoSaveForBuild: TModalResult; virtual; abstract;
    function DoCheckFilesOnDisk: TModalResult; virtual; abstract;
    function DoPublishModule(Options: TPublishModuleOptions;
                             const SrcDirectory, DestDirectory: string
                             ): TModalResult; virtual; abstract;
    function DoCheckAmbigiousSources(const AFilename: string;
                                     Compiling: boolean): TModalResult; virtual; abstract;
    function DoCheckCreatingFile(const AFilename: string;
                                 CheckReadable: boolean): TModalResult; virtual; abstract;
    function DoSaveStringToFile(const Filename, Src,
                                FileDescription: string): TModalResult; virtual; abstract;
    function DoSaveCodeBufferToFile(ABuffer: TCodeBuffer;
                                    const AFilename: string;
                                    IsPartOfProject:boolean): TModalResult; virtual; abstract;
    function DoBackupFile(const Filename:string;
                      IsPartOfProject:boolean): TModalResult; virtual; abstract;
    function DoDeleteAmbigiousFiles(const Filename:string
                                    ): TModalResult; virtual; abstract;
    function DoCheckUnitPathForAmbigiousPascalFiles(const BaseDir, TheUnitPath,
                                    CompiledExt, ContextDescription: string
                                    ): TModalResult; virtual; abstract;

    procedure UpdateWindowsMenu; virtual; abstract;
    procedure SaveEnvironment; virtual; abstract;
    procedure SetRecentSubMenu(ParentMenuItem: TMenuItem; FileList: TStringList;
                               OnClickEvent: TNotifyEvent); virtual; abstract;
    function DoJumpToSourcePosition(const Filename: string;
                               NewX, NewY, NewTopLine: integer;
                               AddJumpPoint: boolean): TModalResult; virtual; abstract;
    function DoJumpToCodePosition(
                        ActiveSrcEdit: TSourceEditorInterface;
                        ActiveUnitInfo: TUnitInfo;
                        NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
                        AddJumpPoint: boolean): TModalResult; virtual; abstract;
    procedure DoJumpToCodeToolBossError; virtual; abstract;
    procedure SaveSourceEditorChangesToCodeCache(PageIndex: integer); virtual; abstract;

    procedure FindInFilesPerDialog(AProject: TProject); virtual; abstract;
    procedure FindInFiles(AProject: TProject; const FindText: string); virtual; abstract;
  end;

var
  MainIDEInterface: TMainIDEInterface;

  ObjectInspector1: TObjectInspector;
  Project1: TProject;

const
  OpenFlagNames: array[TOpenFlag] of string = (
     'ofProjectLoading',
     'ofOnlyIfExists',
     'ofRevert',
     'ofQuiet',
     'ofAddToRecent',
     'ofRegularFile',
     'ofVirtualFile',
     'ofConvertMacros',
     'ofUseCache',
     'ofMultiOpen'
    );

  SaveFlagNames: array[TSaveFlag] of string = (
     'sfSaveAs',
     'sfSaveToTestDir',
     'sfProjectSaving',
     'sfCheckAmbigiousFiles'
    );

function OpenFlagsToString(Flags: TOpenFlags): string;
function SaveFlagsToString(Flags: TSaveFlags): string;

implementation


function OpenFlagsToString(Flags: TOpenFlags): string;
var
  Flag: TOpenFlag;
begin
  Result:='';
  for Flag:=Low(TOpenFlag) to High(TOpenFlag) do begin
    if Flag in Flags then begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+OpenFlagNames[Flag];
    end;
  end;
  Result:='['+Result+']';
end;

function SaveFlagsToString(Flags: TSaveFlags): string;
var
  Flag: TSaveFlag;
begin
  Result:='';
  for Flag:=Low(TSaveFlag) to High(TSaveFlag) do begin
    if Flag in Flags then begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+SaveFlagNames[Flag];
    end;
  end;
  Result:='['+Result+']';
end;

{ TMainIDEInterface }

constructor TMainIDEInterface.Create(TheOwner: TComponent);
begin
  MainIDEInterface:=Self;
  inherited Create(TheOwner);
end;

destructor TMainIDEInterface.Destroy;
begin
  inherited Destroy;
  MainIDEInterface:=nil;
end;

end.

