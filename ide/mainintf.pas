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
  CompOptsIntf, LazIDEIntf,
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
    ctfSourceEditorNotNeeded, // do not check, if the source editor has a file open
    ctfUseGivenSourceEditor
    );
  TCodeToolsFlags = set of TCodeToolsFlag;

  // import/export compiler options result
  TImportExportOptionsResult = (
    ieorCancel,
    ieorImport,
    ieorExport
    );

  TJumpToCodePosFlag = (
    jfAddJumpPoint,
    jfFocusEditor,
    jfMarkLine,
    jfMapLineFromDebug,
    jfDoNotExpandFilename,
    jfSearchVirtualFullPath
  );
  TJumpToCodePosFlags = set of TJumpToCodePosFlag;

  { TMainIDEInterface }

  TMainIDEInterface = class(TLazIDEInterface)
  protected
    function GetToolStatus: TIDEToolStatus; virtual; abstract;
    function GetActiveProject: TLazProject; override;

    function CreateProjectObject(ProjectDesc,
                             FallbackProjectDesc: TProjectDescriptor): TProject; virtual; abstract;
  public
    HiddenWindowsOnRun: TList; // list of forms, that were automatically hidden
                               // and will be shown when debugged program stops

    property ToolStatus: TIDEToolStatus read GetToolStatus;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetMainBarSubTitle(const AValue: string); override;
    procedure UpdateCaption; virtual; abstract;
    procedure HideIDE; virtual; abstract;
    procedure UnhideIDE; virtual; abstract;

    procedure GetCurrentUnitInfo(out ActiveSourceEditor: TSourceEditorInterface;
                              out ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetUnitInfoForDesigner(ADesigner: TIDesigner;
                              out ActiveSourceEditor: TSourceEditorInterface;
                              out ActiveUnitInfo: TUnitInfo); virtual; abstract;

    procedure DoCommand(EditorCommand: integer); virtual; abstract;

    procedure GetIDEFileState(Sender: TObject; const AFilename: string;
                        NeededFlags: TIDEFileStateFlags;
                        out ResultFlags: TIDEFileStateFlags); virtual; abstract;

    function DoInitProjectRun: TModalResult; virtual; abstract;
    function DoOpenMacroFile(Sender: TObject;
        const AFilename: string): TModalResult; virtual; abstract;

    procedure DoShowProjectInspector(Show: boolean); virtual; abstract;
    function DoImExportCompilerOptions(Sender: TObject; out ImportExportResult: TImportExportOptionsResult): TModalResult; virtual; abstract;

    function PrepareForCompile: TModalResult; virtual; abstract; // stop things that interfere with compilation, like debugging
    function DoSaveBuildIDEConfigs(Flags: TBuildLazarusFlags): TModalResult; virtual; abstract;
    function DoManageExamples(): TModalResult; virtual; abstract;
    function DoBuildLazarus(Flags: TBuildLazarusFlags): TModalResult; virtual; abstract;
    function DoSaveForBuild(AReason: TCompileReason): TModalResult; virtual; abstract;
    function DoPublishModule(Options: TPublishModuleOptions;
                             const SrcDirectory, DestDirectory: string
                             ): TModalResult; virtual; abstract;

    function DoFixupComponentReferences(RootComponent: TComponent;
                        OpenFlags: TOpenFlags): TModalResult; virtual; abstract;

    procedure UpdateWindowMenu(Immediately: boolean = false); virtual; abstract;
    procedure SaveEnvironment; virtual; abstract;
    procedure UpdateHighlighters(Immediately: boolean = false); virtual; abstract;
    procedure SetRecentSubMenu(Section: TIDEMenuSection; FileList: TStringList;
                               OnClickEvent: TNotifyEvent); virtual; abstract;
    function DoJumpToSourcePosition(const Filename: string;
                               NewX, NewY, NewTopLine: integer;
                               AddJumpPoint: boolean;
                               MarkLine: Boolean = False): TModalResult;
    function DoJumpToSourcePosition(const Filename: string;
                               NewX, NewY, NewTopLine: integer;
                               Flags: TJumpToCodePosFlags = [jfFocusEditor]): TModalResult; virtual; abstract;
    function DoJumpToCodePosition(
                        ActiveSrcEdit: TSourceEditorInterface;
                        ActiveUnitInfo: TUnitInfo;
                        NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
                        AddJumpPoint: boolean;
                        MarkLine: Boolean = False): TModalResult;
    function DoJumpToCodePosition(
                        ActiveSrcEdit: TSourceEditorInterface;
                        ActiveUnitInfo: TUnitInfo;
                        NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
                        Flags: TJumpToCodePosFlags = [jfFocusEditor]): TModalResult; virtual; abstract;

    procedure FindInFilesPerDialog(AProject: TProject); virtual; abstract;
    procedure FindInFiles(AProject: TProject; const FindText: string); virtual; abstract;

    class function GetPrimaryConfigPath: String; override;
    class function GetSecondaryConfigPath: String; override;
    procedure CopySecondaryConfigFile(const AFilename: String); override;

    function ShowProgress(const SomeText: string;
                          Step, MaxStep: integer): boolean; override;
  end;

var
  MainIDEInterface: TMainIDEInterface = nil;
  ObjectInspector1: TObjectInspectorDlg = nil; // created by the IDE

function OpenFlagsToString(Flags: TOpenFlags): string;
function SaveFlagsToString(Flags: TSaveFlags): string;


//==============================================================================
type

  { TFileDescPascalUnitWithProjectResource }

  TFileDescPascalUnitWithProjectResource = class(TFileDescPascalUnitWithResource)
  protected
    function GetResourceType: TResourceType; override;
  end;
  { TFileDescPascalUnitWithForm }

  TFileDescPascalUnitWithForm = class(TFileDescPascalUnitWithProjectResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TFileDescPascalUnitWithDataModule }

  TFileDescPascalUnitWithDataModule = class(TFileDescPascalUnitWithProjectResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TFileDescPascalUnitWithFrame }

  TFileDescPascalUnitWithFrame = class(TFileDescPascalUnitWithProjectResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TFileDescInheritedItem }

  TFileDescInheritedItem = class(TFileDescPascalUnitWithProjectResource)
  private
    FInheritedUnits: string;
  public
    function GetResourceSource(const ResourceName: string): string; override;
    function GetInterfaceSource(const Filename, SourceName,
                                ResourceName: string): string; override;
    property InheritedUnits: string read FInheritedUnits write FInheritedUnits;
  end;

  { TFileDescInheritedComponent }

  TFileDescInheritedComponent = class(TFileDescInheritedItem)
  private
    FInheritedUnit: TUnitInfo;
    procedure SetInheritedUnit(const AValue: TUnitInfo);
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    property InheritedUnit: TUnitInfo read FInheritedUnit write SetInheritedUnit;
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
  s: string;
begin
  Result:='';
  for Flag:=Low(TOpenFlag) to High(TOpenFlag) do begin
    if Flag in Flags then begin
      if Result<>'' then
        Result:=Result+',';
      WriteStr(s, Flag);
      Result:=Result+s;
    end;
  end;
  Result:='['+Result+']';
end;

function SaveFlagsToString(Flags: TSaveFlags): string;
var
  Flag: TSaveFlag;
  s: string;
begin
  Result:='';
  for Flag:=Low(TSaveFlag) to High(TSaveFlag) do begin
    if Flag in Flags then begin
      if Result<>'' then
        Result:=Result+',';
      WriteStr(s, Flag);
      Result:=Result+s;
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

procedure TMainIDEInterface.SetMainBarSubTitle(const AValue: string);
begin
  if MainBarSubTitle=AValue then exit;
  inherited SetMainBarSubTitle(AValue);
  UpdateCaption;
end;

function TMainIDEInterface.DoJumpToSourcePosition(const Filename: string; NewX, NewY,
  NewTopLine: integer; AddJumpPoint: boolean; MarkLine: Boolean): TModalResult;
var
  Flags: TJumpToCodePosFlags;
begin
  Flags := [jfFocusEditor];
  if AddJumpPoint then Include(Flags, jfAddJumpPoint);
  if MarkLine then Include(Flags, jfMarkLine);
  Result := DoJumpToSourcePosition(Filename, NewX, NewY, NewTopLine, Flags);
end;

function TMainIDEInterface.DoJumpToCodePosition(ActiveSrcEdit: TSourceEditorInterface;
  ActiveUnitInfo: TUnitInfo; NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
  AddJumpPoint: boolean; MarkLine: Boolean): TModalResult;
var
  Flags: TJumpToCodePosFlags;
begin
  Flags := [jfFocusEditor];
  if AddJumpPoint then Include(Flags, jfAddJumpPoint);
  if MarkLine then Include(Flags, jfMarkLine);
  Result := DoJumpToCodePosition(ActiveSrcEdit, ActiveUnitInfo, NewSource, NewX, NewY, NewTopLine,
    Flags);
end;

class function TMainIDEInterface.GetPrimaryConfigPath: String;
begin
  Result:=LazConf.GetPrimaryConfigPath;
end;

class function TMainIDEInterface.GetSecondaryConfigPath: String;
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
  Result := inherited GetInterfaceUsesSection + ', Forms, Controls, Graphics, Dialogs';
end;

function TFileDescPascalUnitWithForm.GetLocalizedName: string;
begin
  Result:=lisForm;
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
  Result := inherited GetInterfaceUsesSection;
end;

function TFileDescPascalUnitWithDataModule.GetLocalizedName: string;
begin
  Result:=lisDataModule;
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
  Result:=dlgMouseOptNodeMain;
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
  IsPascalUnit:=false;
end;

function TFileDescSimplePascalProgram.GetLocalizedName: string;
begin
  Result:=lisCustomProgram;
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

{ TFileDescPascalUnitWithFrame }

constructor TFileDescPascalUnitWithFrame.Create;
begin
  inherited Create;
  Name := FileDescNameFrame;
  ResourceClass := TFrame;
  UseCreateFormStatements := False;
  DeclareClassVariable := False;
end;

function TFileDescPascalUnitWithFrame.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection + ', Forms, Controls';
end;

function TFileDescPascalUnitWithFrame.GetLocalizedName: string;
begin
  Result:=lisFrame;
end;

function TFileDescPascalUnitWithFrame.GetLocalizedDescription: string;
begin
  Result := lisNewDlgCreateANewUnitWithAFrame;
end;

{ TFileDescInheritedComponent }

procedure TFileDescInheritedComponent.SetInheritedUnit(const AValue: TUnitInfo);
begin
  if FInheritedUnit=AValue then exit;
  FInheritedUnit:=AValue;
  InheritedUnits:=FInheritedUnit.Unit_Name;
end;

constructor TFileDescInheritedComponent.Create;
begin
  inherited Create;
  Name := FileDescNameLCLInheritedComponent;
  ResourceClass := TForm;// will be adjusted on the fly
  UseCreateFormStatements := true;
end;

function TFileDescInheritedComponent.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result := Result+', Forms, Controls, Graphics, Dialogs';
  if InheritedUnits<>'' then
    Result := Result+', '+InheritedUnits;
end;

function TFileDescInheritedComponent.GetLocalizedName: string;
begin
  Result:=lisInheritedProjectComponent;
end;

function TFileDescInheritedComponent.GetLocalizedDescription: string;
begin
  Result:=lisNewDlgInheritFromAProjectFormComponent;
end;

{ TFileDescInheritedItem }

function TFileDescInheritedItem.GetResourceSource(const ResourceName: string): string;
begin
  Result := 'inherited '+ ResourceName+': T'+ResourceName+LineEnding+
            'end';
end;

function TFileDescInheritedItem.GetInterfaceSource(const Filename, SourceName,
  ResourceName: string): string;
var
  LE: string;
begin
  LE:=LineEnding;
  Result:=
     'type'+LE
    +'  T'+ResourceName+' = class('+ResourceClass.ClassName+')'+LE
    +'  private'+LE
    +'    { private declarations }'+LE
    +'  public'+LE
    +'    { public declarations }'+LE
    +'  end;'+LE
    +LE;

  if DeclareClassVariable then
    Result := Result +
     'var'+LE
    +'  '+ResourceName+': T'+ResourceName+';'+LE
    +LE;
end;

{ TFileDescPascalUnitWithProjectResource }

function TFileDescPascalUnitWithProjectResource.GetResourceType: TResourceType;
begin
  Result := Project1.ProjResources.ResourceType;
end;

end.


