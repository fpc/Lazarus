{  $Id$  }
{
 /***************************************************************************
                    mainintf.pas  -  the "integrated" in IDE
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
  mainintf.pas - TMainIDEInterface = class(TLazIDEInterface)
                   The interface class of the top level functions of the IDE.
                   TMainIDEInterface is used by functions/units, that uses
                   several different parts of the IDE (designer, source editor,
                   codetools), so they can't be added to a specific boss and
                   which are yet too small to become a boss of their own.
  lazideintf.pas - TLazIDEInterface = class(TComponent)
                   For designtime packages, this is the interface class of the
                   top level functions of the IDE.


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
  Controls, Graphics, ExtCtrls, Dialogs, FileUtil, Forms, CodeToolManager,
  CodeCache, AVL_Tree, SynEditKeyCmds,
  // IDE
  PropEdits, ObjectInspector, MenuIntf, SrcEditorIntf, ProjectIntf, MacroIntf,
  LazIDEIntf,
  LazConf, LazarusIDEStrConsts,
  ProjectDefs, Project, PublishModule, BuildLazDialog, Compiler,
  ComponentReg,
  TransferMacros, OutputFilter, IDEDefs, MsgView, ProgressDlg,
  EnvironmentOpts, EditorOptions, CompilerOptions, KeyMapping, IDEProcs,
  IDEOptionDefs, CodeToolsDefines;

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

  // revert file flags
  TRevertFlag = (
    rfQuiet
    );
  TRevertFlags = set of TRevertFlag;

  // codetools flags
  TCodeToolsFlag = (
    ctfSwitchToFormSource, // bring source notebook to front and show source of
                           //   current designed form
    ctfActivateAbortMode,  // activate the CodeToolBoss.Abortable mode
    ctfSourceEditorNotNeeded // do not check, if the source editor has a file open
    );
  TCodeToolsFlags = set of TCodeToolsFlag;

  { TMainIDEInterface }

  TMainIDEInterface = class(TLazIDEInterface)
  protected
    function GetToolStatus: TIDEToolStatus; virtual; abstract;
    function GetActiveProject: TLazProject; override;
  public
    HiddenWindowsOnRun: TList; // list of forms, that were automatically hidden
                               // and will be shown when debugged program stops

    property ToolStatus: TIDEToolStatus read GetToolStatus;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateCaption; virtual; abstract;
    procedure HideIDE; virtual; abstract;
    procedure UnhideIDE; virtual; abstract;

    procedure GetCurrentUnitInfo(var ActiveSourceEditor: TSourceEditorInterface;
                              var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetUnitInfoForDesigner(ADesigner: TIDesigner;
                              var ActiveSourceEditor: TSourceEditorInterface;
                              var ActiveUnitInfo: TUnitInfo); virtual; abstract;

    procedure DoCommand(EditorCommand: integer); virtual; abstract;

    function GetProjectTargetFilename: string; virtual; abstract;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; virtual; abstract;
    function IsTestUnitFilename(const AFilename: string): boolean; virtual; abstract;
    function GetRunCommandLine: string; virtual; abstract;
    procedure GetIDEFileState(Sender: TObject; const AFilename: string;
                        NeededFlags: TIDEFileStateFlags;
                        var ResultFlags: TIDEFileStateFlags); virtual; abstract;

    function DoInitProjectRun: TModalResult; virtual; abstract;
    function DoOpenMacroFile(Sender: TObject;
        const AFilename: string): TModalResult; virtual; abstract;

    function DoShowProjectInspector: TModalResult; virtual; abstract;
    function DoImExportCompilerOptions(Sender: TObject): TModalResult; virtual; abstract;

    function CreateProjectObject(ProjectDesc,
                             FallbackProjectDesc: TProjectDescriptor): TProject; virtual; abstract;
    function PrepareForCompile: TModalResult; virtual; abstract;
    function DoSaveBuildIDEConfigs(Flags: TBuildLazarusFlags): TModalResult; virtual; abstract;
    function DoBuildLazarus(Flags: TBuildLazarusFlags): TModalResult; virtual; abstract;
    function DoExecuteCompilationTool(Tool: TCompilationToolOptions;
                                      const WorkingDir, ToolTitle: string
                                      ): TModalResult; virtual; abstract;
    function DoSaveForBuild: TModalResult; virtual; abstract;
    function DoCheckFilesOnDisk(Instantaneous: boolean = false): TModalResult; virtual; abstract;
    function DoPublishModule(Options: TPublishModuleOptions;
                             const SrcDirectory, DestDirectory: string
                             ): TModalResult; virtual; abstract;
    function DoCheckAmbiguousSources(const AFilename: string;
                                     Compiling: boolean): TModalResult; virtual; abstract;
    function DoSaveStringToFile(const Filename, Src,
                                FileDescription: string): TModalResult; virtual; abstract;
    function DoSaveCodeBufferToFile(ABuffer: TCodeBuffer;
                                    const AFilename: string;
                                    IsPartOfProject:boolean): TModalResult; virtual; abstract;
    function DoBackupFile(const Filename:string;
                      IsPartOfProject:boolean): TModalResult; virtual; abstract;
    function DoDeleteAmbiguousFiles(const Filename:string
                                    ): TModalResult; virtual; abstract;
    function DoCheckUnitPathForAmbiguousPascalFiles(const BaseDir, TheUnitPath,
                                    CompiledExt, ContextDescription: string
                                    ): TModalResult; virtual; abstract;

    procedure UpdateWindowsMenu; virtual; abstract;
    procedure SaveEnvironment; virtual; abstract;
    procedure SetRecentSubMenu(Section: TIDEMenuSection; FileList: TStringList;
                               OnClickEvent: TNotifyEvent); virtual; abstract;
    function DoJumpToSourcePosition(const Filename: string;
                               NewX, NewY, NewTopLine: integer;
                               AddJumpPoint: boolean): TModalResult; virtual; abstract;
    function DoJumpToCodePosition(
                        ActiveSrcEdit: TSourceEditorInterface;
                        ActiveUnitInfo: TUnitInfo;
                        NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
                        AddJumpPoint: boolean): TModalResult; virtual; abstract;

    procedure FindInFilesPerDialog(AProject: TProject); virtual; abstract;
    procedure FindInFiles(AProject: TProject; const FindText: string); virtual; abstract;

    function GetPrimaryConfigPath: String; override;
    function GetSecondaryConfigPath: String; override;
    procedure CopySecondaryConfigFile(const AFilename: String); override;

    function ShowProgress(const SomeText: string;
                          Step, MaxStep: integer): boolean; override;
  end;

var
  MainIDEInterface: TMainIDEInterface;

  ObjectInspector1: TObjectInspector;

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
     'ofMultiOpen',
     'ofDoNotLoadResource',
     'ofDoLoadResource',
     'ofAddToProject'
    );

  SaveFlagNames: array[TSaveFlag] of string = (
     'sfSaveAs',
     'sfSaveToTestDir',
     'sfProjectSaving',
     'sfCheckAmbiguousFiles',
     'sfSaveNonProjectFiles'
    );

function OpenFlagsToString(Flags: TOpenFlags): string;
function SaveFlagsToString(Flags: TSaveFlags): string;


//==============================================================================
type
  { TFileDescPascalUnitWithForm }

  TFileDescPascalUnitWithForm = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;


  { TFileDescPascalUnitWithDataModule }

  TFileDescPascalUnitWithDataModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;


  { TFileDescSimplePascalProgram }

  TFileDescSimplePascalProgram = class(TFileDescPascalUnit)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function CreateSource(const Filename, SourceName,
                          ResourceName: string): string; override;
  end;


  { TFileDescText }

  TFileDescText = class(TProjectFileDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;
  

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

function TMainIDEInterface.GetActiveProject: TLazProject;
begin
  Result:=Project1;
end;

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

function TMainIDEInterface.GetPrimaryConfigPath: String;
begin
  Result:=LazConf.GetPrimaryConfigPath;
end;

function TMainIDEInterface.GetSecondaryConfigPath: String;
begin
  Result:=LazConf.GetSecondaryConfigPath;
end;

procedure TMainIDEInterface.CopySecondaryConfigFile(const AFilename: String);
begin
  LazConf.CopySecondaryConfigFile(AFilename);
end;

function TMainIDEInterface.ShowProgress(const SomeText: string; Step,
  MaxStep: integer): boolean;
begin
  Result:=ProgressDlg.ShowProgress(SomeText,Step,MaxStep);
end;

{ TFileDescPascalUnitWithForm }

constructor TFileDescPascalUnitWithForm.Create;
begin
  inherited Create;
  Name:=FileDescNameLCLForm;
  ResourceClass:=TForm;
  UseCreateFormStatements:=true;
end;

function TFileDescPascalUnitWithForm.GetInterfaceUsesSection: string;
begin
  Result:='Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs';
end;

function TFileDescPascalUnitWithForm.GetLocalizedName: string;
begin
  Result:='Form';
end;

function TFileDescPascalUnitWithForm.GetLocalizedDescription: string;
begin
  Result:=lisNewDlgCreateANewUnitWithALCLForm;
end;

{ TFileDescPascalUnitWithDataModule }

constructor TFileDescPascalUnitWithDataModule.Create;
begin
  inherited Create;
  Name:=FileDescNameDatamodule;
  ResourceClass:=TDataModule;
  UseCreateFormStatements:=true;
end;

function TFileDescPascalUnitWithDataModule.GetInterfaceUsesSection: string;
begin
  Result:='Classes, SysUtils, LResources, Forms, Controls, Dialogs';
end;

function TFileDescPascalUnitWithDataModule.GetLocalizedName: string;
begin
  Result:='Data Module';
end;

function TFileDescPascalUnitWithDataModule.GetLocalizedDescription: string;
begin
  Result:=lisNewDlgCreateANewUnitWithADataModule;
end;

{ TFileDescText }

constructor TFileDescText.Create;
begin
  inherited Create;
  Name:=FileDescNameText;
  DefaultFilename:='text.txt';
  AddToProject:=false;
end;

function TFileDescText.GetLocalizedName: string;
begin
  Result:='Text';
end;

function TFileDescText.GetLocalizedDescription: string;
begin
  Result:=lisNewDlgCreateANewEmptyTextFile;
end;

{ TFileDescSimplePascalProgram }

constructor TFileDescSimplePascalProgram.Create;
begin
  inherited Create;
  Name:='custom program';
  DefaultFilename:='project.pas';
end;

function TFileDescSimplePascalProgram.GetLocalizedName: string;
begin
  Result:='Custom Program';
end;

function TFileDescSimplePascalProgram.GetLocalizedDescription: string;
begin
  Result:=Format(lisASimplePascalProgramFileThisCanBeUsedForQuickAndDi, [#13,
    #13]);
end;

function TFileDescSimplePascalProgram.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
var
  LE: String;
begin
  LE:=LineEnding;
  Result:='program '+SourceName+';'+LE
         +LE
         +'{$mode objfpc}{$H+}'+LE
         +LE
         +'uses'+LE
         +'  Classes, SysUtils;'+LE
         +LE
         +'begin'+LE
         +'end.'+LE
         +LE;
end;

end.

