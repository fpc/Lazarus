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

  Abstract:
    Class TLazSourceFileManager has methods to deal with source files.
    It deals with projects, packages and the IDE features when needed.
    The code is copied and refactored from the huge main.pp. The goal was to not
    call methods defined in TMainIDE but there are still some calls doing it.
}
unit SourceFileManager;

{$mode objfpc}{$H+}

interface

uses
  AVL_Tree, typinfo, math, Classes, SysUtils, Controls, Forms, Dialogs, LCLIntf,
  LCLType, LCLProc, FileProcs, FileUtil, IDEProcs, DialogProcs, IDEDialogs,
  LConvEncoding, LResources, PropEdits, DefineTemplates, IDEMsgIntf,
  IDEProtocol, LazarusIDEStrConsts, NewDialog, NewProjectDlg, LazIDEIntf,
  MainBase, MainBar, MainIntf, MenuIntf, NewItemIntf, ProjectIntf, Project,
  ProjectDefs, ProjectInspector, CompilerOptions, BasePkgManager, PackageIntf,
  PackageDefs, PackageSystem, SrcEditorIntf, IDEWindowIntf, ComponentReg,
  SourceEditor, EditorOptions, CustomFormEditor, FormEditor, EmptyMethodsDlg,
  BaseDebugManager, ControlSelection, TransferMacros, EnvironmentOpts,
  BuildManager, Designer, EditorMacroListViewer, KeywordFuncLists,
  FindRenameIdentifier, GenericCheckList,
  {$IFDEF EnableNewExtTools}
  etMessagesWnd,
  {$ELSE}
  MsgView,
  {$ENDIF}
  InputHistory, CheckLFMDlg, LCLMemManager, CodeToolManager, CodeToolsStructs,
  ConvCodeTool, CodeCache, CodeTree, FindDeclarationTool, BasicCodeTools,
  SynEdit, UnitResources, IDEExternToolIntf;


type

  { TLazSourceFileManager }

  TLazSourceFileManager = class
  private
    function CheckMainSrcLCLInterfaces(Silent: boolean): TModalResult;
    function FileExistsInIDE(const Filename: string;
                             SearchFlags: TProjectFileSearchFlags): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRecentProjectFileToEnvironment(const AFilename: string);
    procedure UpdateSourceNames;
    function CheckEditorNeedsSave(AEditor: TSourceEditorInterface;
        IgnoreSharedEdits: Boolean): Boolean;
    procedure ArrangeSourceEditorAndMessageView(PutOnTop: boolean);

    // files/units/projects
    function GetAvailableUnitEditorInfo(AnUnitInfo: TUnitInfo;
      ACaretPoint: TPoint; WantedTopLine: integer = -1): TUnitEditorInfo;
    function SomethingOfProjectIsModified(Verbose: boolean = false): boolean;
    function NewFile(NewFileDescriptor: TProjectFileDescriptor;
      var NewFilename: string; NewSource: string;
      NewFlags: TNewFlags; NewOwner: TObject): TModalResult;
    function NewOther: TModalResult;
    function NewUnitOrForm(Template: TNewIDEItemTemplate;
      DefaultDesc: TProjectFileDescriptor): TModalResult;
    procedure CreateFileDialogFilterForSourceEditorFiles(Filter: string;
        out AllEditorMask, AllMask: string);
    function SaveEditorFile(AEditor: TSourceEditorInterface;
                              Flags: TSaveFlags): TModalResult;
    function SaveEditorFile(const Filename: string; Flags: TSaveFlags): TModalResult;
    function CloseEditorFile(AEditor: TSourceEditorInterface;
                               Flags: TCloseFlags):TModalResult;
    function CloseEditorFile(const Filename: string; Flags: TCloseFlags): TModalResult;
    function OpenEditorFile(AFileName:string; PageIndex, WindowIndex: integer;
      AEditorInfo: TUnitEditorInfo; Flags: TOpenFlags;
      UseWindowID: Boolean = False): TModalResult;  // WindowIndex is WindowID
    function OpenFileAtCursor(ActiveSrcEdit: TSourceEditor;
      ActiveUnitInfo: TUnitInfo): TModalResult;
    function InitNewProject(ProjectDesc: TProjectDescriptor): TModalResult;
    function InitOpenedProjectFile(AFileName: string; Flags: TOpenFlags): TModalResult;
    procedure NewProjectFromFile;
    function CreateProjectForProgram(ProgramBuf: TCodeBuffer): TModalResult;
    function InitProjectForProgram(ProgramBuf: TCodeBuffer): TModalResult;
    function SaveProject(Flags: TSaveFlags): TModalResult;
    function SaveProjectIfChanged: TModalResult;
    function CloseProject: TModalResult;
    procedure OpenProject(aMenuItem: TIDEMenuItem);
    procedure CloseAll;
    // project inspector
    // Checks if the UnitDirectory is part of the Unit Search Paths, if not,
    // ask the user if he wants to extend dependencies or the Unit Search Paths.
    function CheckDirIsInSearchPath(UnitInfo: TUnitInfo; AllowAddingDependencies, IsIncludeFile: Boolean): Boolean;

    // methods for 'new unit'
    function CreateNewCodeBuffer(Descriptor: TProjectFileDescriptor;
        NewOwner: TObject; NewFilename: string; var NewCodeBuffer: TCodeBuffer;
        var NewUnitName: string): TModalResult;
    function CreateNewForm(NewUnitInfo: TUnitInfo;
        AncestorType: TPersistentClass; ResourceCode: TCodeBuffer;
        UseCreateFormStatements, DisableAutoSize: Boolean): TModalResult;
    function NewUniqueComponentName(Prefix: string): string;

    // methods for 'save unit'
    function ShowSaveFileAsDialog(var AFilename: string; AnUnitInfo: TUnitInfo;
        var LFMCode, LRSCode: TCodeBuffer; CanAbort: boolean): TModalResult;
    function SaveUnitComponent(AnUnitInfo: TUnitInfo;
        LRSCode, LFMCode: TCodeBuffer; Flags: TSaveFlags): TModalResult;
    function RemoveLooseEvents(AnUnitInfo: TUnitInfo;
        OkOnCodeErrors: boolean): TModalResult;
    function RenameUnit(AnUnitInfo: TUnitInfo; NewFilename, NewUnitName: string;
        var LFMCode, LRSCode: TCodeBuffer): TModalResult;

    // methods for 'open unit' and 'open main unit'
  private
    function OpenFileInSourceEditor(AnEditorInfo: TUnitEditorInfo;
      PageIndex, WindowIndex: integer; Flags: TOpenFlags;
      UseWindowID: Boolean = False): TModalResult;  // WindowIndex is WindowID
    function OpenUnknownFile(const AFileName:string; Flags: TOpenFlags;
        var NewUnitInfo: TUnitInfo; var Handled: boolean): TModalResult;
    function OpenNotExistingFile(const AFileName:string;
        Flags: TOpenFlags): TModalResult;
    function LoadResourceFile(AnUnitInfo: TUnitInfo; var LFMCode, LRSCode: TCodeBuffer;
        IgnoreSourceErrors, AutoCreateResourceCode, ShowAbort: boolean): TModalResult;
    function FindBaseComponentClass(const AComponentClassName,
        DescendantClassName: string; out AComponentClass: TComponentClass): boolean;
    function LoadAncestorDependencyHidden(AnUnitInfo: TUnitInfo;
        const DescendantClassName: string; OpenFlags: TOpenFlags;
        out AncestorClass: TComponentClass; out AncestorUnitInfo: TUnitInfo): TModalResult;
    function FindComponentClass(AnUnitInfo: TUnitInfo;
        const AComponentClassName: string; Quiet: boolean;
        out ComponentUnitInfo: TUnitInfo; out AComponentClass: TComponentClass;
        out LFMFilename: string;  out AncestorClass: TComponentClass): TModalResult;
    function LoadComponentDependencyHidden(AnUnitInfo: TUnitInfo;
        const AComponentClassName: string; Flags: TOpenFlags; MustHaveLFM: boolean;
        out AComponentClass: TComponentClass; out ComponentUnitInfo: TUnitInfo;
        out AncestorClass: TComponentClass): TModalResult;
    function LoadIDECodeBuffer(var ACodeBuffer: TCodeBuffer;
        const AFilename: string; Flags: TLoadBufferFlags; ShowAbort: boolean): TModalResult;
  public
    function OpenMainUnit(PageIndex, WindowIndex: integer;
                          Flags: TOpenFlags;
                          UseWindowID: Boolean = False // WindowIndex is WindowID
                         ): TModalResult;
    function RevertMainUnit: TModalResult;
    function CheckLFMInEditor(LFMUnitInfo: TUnitInfo; Quiet: boolean): TModalResult;
    function LoadLFM(AnUnitInfo: TUnitInfo; OpenFlags: TOpenFlags;
                       CloseFlags: TCloseFlags): TModalResult;
    function LoadLFM(AnUnitInfo: TUnitInfo; LFMBuf: TCodeBuffer;
                       OpenFlags: TOpenFlags;
                       CloseFlags: TCloseFlags): TModalResult;
    function OpenComponent(const UnitFilename: string; OpenFlags: TOpenFlags;
        CloseFlags: TCloseFlags; out Component: TComponent): TModalResult;

    // methods for 'close unit'
    function CloseUnitComponent(AnUnitInfo: TUnitInfo; Flags: TCloseFlags): TModalResult;
    function CloseDependingUnitComponents(AnUnitInfo: TUnitInfo;
                                          Flags: TCloseFlags): TModalResult;
    function UnitComponentIsUsed(AnUnitInfo: TUnitInfo;
                                 CheckHasDesigner: boolean): boolean;
    function RemoveFilesFromProject(AProject: TProject; UnitInfos: TFPList): TModalResult;
    procedure InvertedFileClose(PageIndex: LongInt; SrcNoteBook: TSourceNotebook);

    // methods for open project, create project from source
    function CompleteLoadingProjectInfo: TModalResult;

    // methods for 'save project'
  private
    function ShowSaveProjectAsDialog(UseMainSourceFile: boolean): TModalResult;
    function SaveProjectInfo(var Flags: TSaveFlags): TModalResult;
    procedure GetMainUnit(var MainUnitInfo: TUnitInfo;
        var MainUnitSrcEdit: TSourceEditor; UpdateModified: boolean);
    procedure SaveSrcEditorProjectSpecificSettings(AnEditorInfo: TUnitEditorInfo);
    procedure SaveSourceEditorProjectSpecificSettings;
    procedure UpdateProjectResourceInfo;
  public
    function AskSaveProject(const ContinueText, ContinueBtn: string): TModalResult;
    function SaveSourceEditorChangesToCodeCache(AEditor: TSourceEditorInterface): boolean;
    function ReplaceUnitUse(OldFilename, OldUnitName,
                              NewFilename, NewUnitName: string;
                              IgnoreErrors, Quiet, Confirm: boolean): TModalResult;
    // related to Designer
    function DesignerUnitIsVirtual(aLookupRoot: TComponent): Boolean;
  end;


  function SourceFileMgr: TLazSourceFileManager;

  function CreateSrcEditPageName(const AnUnitName, AFilename: string;
    IgnoreEditor: TSourceEditor): string;
  procedure UpdateDefaultPasFileExt;


implementation

var
  SourceFileMgrSingleton: TLazSourceFileManager = nil;

function SourceFileMgr: TLazSourceFileManager;
// Return always the same instance of SourceFileManager. Create at the first time.
begin
  if SourceFileMgrSingleton = nil then
    SourceFileMgrSingleton := TLazSourceFileManager.Create;
  Result := SourceFileMgrSingleton;
end;

function CreateSrcEditPageName(const AnUnitName, AFilename: string;
  IgnoreEditor: TSourceEditor): string;
begin
  Result:=AnUnitName;
  if Result='' then
    Result:=AFilename;
  if FilenameIsPascalUnit(Result) then
    Result:=ExtractFileNameOnly(Result)
  else
    Result:=ExtractFileName(Result);
  Result:=SourceEditorManager.FindUniquePageName(Result,IgnoreEditor);
end;

procedure UpdateDefaultPasFileExt;
var
  DefPasExt: string;
begin
  // change default pascal file extensions
  DefPasExt:=PascalExtension[EnvironmentOptions.PascalFileExtension];
  if LazProjectFileDescriptors<>nil then
    LazProjectFileDescriptors.DefaultPascalFileExt:=DefPasExt;
end;


{ TLazSourceFileManager }

constructor TLazSourceFileManager.Create;
begin

end;

destructor TLazSourceFileManager.Destroy;
begin
  inherited Destroy;
end;


function TLazSourceFileManager.CheckMainSrcLCLInterfaces(Silent: boolean): TModalResult;
var
  MainUnitInfo: TUnitInfo;
  MainUsesSection,ImplementationUsesSection: TStrings;
  MsgResult: TModalResult;
begin
  Result:=mrOk;
  if (Project1=nil) then exit;
  if Project1.SkipCheckLCLInterfaces then exit;
  MainUnitInfo:=Project1.MainUnitInfo;
  if (MainUnitInfo=nil) or (MainUnitInfo.Source=nil) then exit;
  if PackageGraph.FindDependencyRecursively(Project1.FirstRequiredDependency,
    PackageGraph.LCLBasePackage)=nil
  then
    exit; // project does not use LCLBase
  // project uses LCLBase
  MainUsesSection:=nil;
  ImplementationUsesSection:=nil;
  try
    if not CodeToolBoss.FindUsedUnitNames(MainUnitInfo.Source,
      MainUsesSection,ImplementationUsesSection) then exit;
    if (UTF8SearchInStringList(MainUsesSection,'forms')<0)
    and (UTF8SearchInStringList(ImplementationUsesSection,'forms')<0) then
      exit;
    // project uses lcl unit Forms
    if (UTF8SearchInStringList(MainUsesSection,'interfaces')>=0)
    or (UTF8SearchInStringList(ImplementationUsesSection,'interfaces')>=0) then
      exit;
    // project uses lcl unit Forms, but not unit interfaces
    // this will result in strange linker error
    if not Silent then
    begin
      MsgResult:=IDEQuestionDialog(lisCCOWarningCaption,
        Format(lisTheProjectDoesNotUseTheLCLUnitInterfacesButItSeems, [LineEnding]),
        mtWarning, [mrYes, lisAddUnitInterfaces, mrNo, dlgIgnoreVerb,
                    mrNoToAll, lisAlwaysIgnore, mrCancel]);
      case MsgResult of
        mrNo: exit;
        mrNoToAll: begin Project1.SkipCheckLCLInterfaces:=true; exit; end;
        mrCancel: exit(mrCancel);
      end;
    end;
    CodeToolBoss.AddUnitToMainUsesSection(MainUnitInfo.Source,'Interfaces','');
  finally
    MainUsesSection.Free;
    ImplementationUsesSection.Free;
  end;
end;

procedure TLazSourceFileManager.AddRecentProjectFileToEnvironment(const AFilename: string);
begin
  EnvironmentOptions.AddToRecentProjectFiles(AFilename);
  MainIDE.SetRecentProjectFilesMenu;
  MainIDE.SaveEnvironment;
end;

procedure TLazSourceFileManager.UpdateSourceNames;
var
  i: integer;
  AnUnitInfo: TUnitInfo;
  SourceName, PageName: string;
  AEditor: TSourceEditor;
begin
  for i:=0 to SourceEditorManager.SourceEditorCount-1 do begin
    AEditor := SourceEditorManager.SourceEditors[i];
    AnUnitInfo := Project1.UnitWithEditorComponent(AEditor);
    if AnUnitInfo=nil then continue;
    if FilenameIsPascalUnit(AnUnitInfo.Filename) then begin
      SourceName:=CodeToolBoss.GetCachedSourceName(AnUnitInfo.Source);
      if SourceName<>'' then
        AnUnitInfo.ReadUnitNameFromSource(true);
    end else
      SourceName:='';
    PageName:=CreateSrcEditPageName(SourceName, AnUnitInfo.Filename, AEditor);
    AEditor.PageName := PageName;
  end;
end;

function TLazSourceFileManager.CheckEditorNeedsSave(AEditor: TSourceEditorInterface;
  IgnoreSharedEdits: Boolean): Boolean;
var
  AnEditorInfo: TUnitEditorInfo;
  AnUnitInfo: TUnitInfo;
begin
  Result := False;
  if AEditor = nil then exit;
  AnEditorInfo := Project1.EditorInfoWithEditorComponent(AEditor);
  if AnEditorInfo = nil then exit;

  AnUnitInfo := AnEditorInfo.UnitInfo;
  if (AnUnitInfo.OpenEditorInfoCount > 1) and IgnoreSharedEdits then
    exit;

  // save some meta data of the source
  SaveSrcEditorProjectSpecificSettings(AnEditorInfo);

  Result := (AEditor.Modified) or (AnUnitInfo.Modified);
end;

procedure TLazSourceFileManager.ArrangeSourceEditorAndMessageView(PutOnTop: boolean);
var
  SrcNoteBook: TSourceNotebook;
  Layout: TSimpleWindowLayout;
begin
  MainIDE.DoShowMessagesView(PutOnTop);
  if SourceEditorManager.SourceWindowCount = 0 then exit;
  SrcNoteBook := SourceEditorManager.SourceWindows[0];

  Layout:=IDEWindowCreators.SimpleLayoutStorage.ItemByFormID(SrcNoteBook.Name);
  if (Layout<>nil) and (Layout.WindowPlacement=iwpDefault)
  and ((SrcNoteBook.Top + SrcNoteBook.Height) > MessagesView.Top)
  and (MessagesView.Parent = nil) then
    SrcNoteBook.Height := Max(50,Min(SrcNoteBook.Height,
       MessagesView.Top-SrcNoteBook.Top));
  if PutOnTop then
  begin
    IDEWindowCreators.ShowForm(MessagesView,true);
    SourceEditorManager.ShowActiveWindowOnTop(False);
  end;
end;

function TLazSourceFileManager.GetAvailableUnitEditorInfo(AnUnitInfo: TUnitInfo;
  ACaretPoint: TPoint; WantedTopLine: integer): TUnitEditorInfo;

  function EditorMatches(AEditInfo: TUnitEditorInfo;
     AAccess: TEditorOptionsEditAccessOrderEntry; ALockRun: Integer = 0): Boolean;
  var
    AEdit: TSourceEditor;
  begin
    AEdit := TSourceEditor(AEditInfo.EditorComponent);
    Result := False;
    case AAccess.SearchLocked of
      eoeaIgnoreLock: ;
      eoeaLockedOnly:   if not AEdit.IsLocked then exit;
      eoeaUnlockedOnly: if AEdit.IsLocked then exit;
      eoeaLockedFirst:  if (not AEdit.IsLocked) and (ALockRun = 0) then exit;
      eoeaLockedLast:   if (AEdit.IsLocked) and (ALockRun = 0) then exit;
    end;
    case AAccess.SearchInView of
      eoeaIgnoreInView: ;
      eoeaInViewOnly:   if not AEdit.IsCaretOnScreen(ACaretPoint, False) then exit;
      eoeaInViewSoftCenterOnly: if not AEdit.IsCaretOnScreen(ACaretPoint, True) then exit;
    end;
    Result := True;
  end;

  function AvailSrcWindowIndex: Integer;
  var
    i: Integer;
  begin
    Result := -1;
    i := 0;
    if AnUnitInfo.OpenEditorInfoCount > 0 then
      while (i < SourceEditorManager.SourceWindowCount) and
            (SourceEditorManager.SourceWindowByLastFocused[i].IndexOfEditorInShareWith
               (TSourceEditor(AnUnitInfo.OpenEditorInfo[0].EditorComponent)) >= 0)
      do
        inc(i);
    if i < SourceEditorManager.SourceWindowCount then
      Result := SourceEditorManager.IndexOfSourceWindow(SourceEditorManager.SourceWindowByLastFocused[i]);
  end;

var
  i, j, w, LockRun: Integer;
  Access: TEditorOptionsEditAccessOrderEntry;
begin
  Result := nil;
  // Check for already open Editor. If there is none, then it must be opened in DoOpenEditorFile
  if AnUnitInfo.OpenEditorInfoCount = 0 then exit;
  for i := 0 to EditorOpts.MultiWinEditAccessOrder.Count - 1 do begin
    Access := EditorOpts.MultiWinEditAccessOrder[i];
    if not Access.Enabled then continue;
    LockRun := 1;
    if Access.SearchLocked in [eoeaLockedFirst, eoeaLockedLast] then LockRun := 0;
    repeat
      case Access.RealSearchOrder of
        eoeaOrderByEditFocus, eoeaOrderByListPref:
          begin
            for j := 0 to AnUnitInfo.OpenEditorInfoCount - 1 do
              if EditorMatches(AnUnitInfo.OpenEditorInfo[j], Access) then begin
                Result := AnUnitInfo.OpenEditorInfo[j];
                break;
              end;
          end;
        eoeaOrderByWindowFocus:
          begin
            for w := 0 to SourceEditorManager.SourceWindowCount - 1 do begin
              for j := 0 to AnUnitInfo.OpenEditorInfoCount - 1 do
                if (TSourceEditor(AnUnitInfo.OpenEditorInfo[j].EditorComponent).SourceNotebook
                    = SourceEditorManager.SourceWindowByLastFocused[w])
                and EditorMatches(AnUnitInfo.OpenEditorInfo[j], Access) then begin
                  Result := AnUnitInfo.OpenEditorInfo[j];
                  break;
                end;
              if Result <> nil then break;
            end;
          end;
        eoeaOrderByOldestEditFocus:
          begin
            for j := AnUnitInfo.OpenEditorInfoCount - 1 downto 0 do
              if EditorMatches(AnUnitInfo.OpenEditorInfo[j], Access) then begin
                Result := AnUnitInfo.OpenEditorInfo[j];
                break;
              end;
          end;
        eoeaOrderByOldestWindowFocus:
          begin
            for w := SourceEditorManager.SourceWindowCount - 1 downto 0 do begin
              for j := 0 to AnUnitInfo.OpenEditorInfoCount - 1 do
                if (TSourceEditor(AnUnitInfo.OpenEditorInfo[j].EditorComponent).SourceNotebook
                    = SourceEditorManager.SourceWindowByLastFocused[w])
                and EditorMatches(AnUnitInfo.OpenEditorInfo[j], Access) then begin
                  Result := AnUnitInfo.OpenEditorInfo[j];
                  break;
                end;
              if Result <> nil then break;
            end;
          end;
        eoeaOnlyCurrentEdit:
          begin
            LockRun := 1;
            for j := 0 to AnUnitInfo.OpenEditorInfoCount - 1 do
              if (AnUnitInfo.OpenEditorInfo[j].EditorComponent = SourceEditorManager.ActiveEditor)
              and EditorMatches(AnUnitInfo.OpenEditorInfo[j], Access) then begin
                Result := AnUnitInfo.OpenEditorInfo[j];
                break;
              end;
          end;
        eoeaOnlyCurrentWindow:
          begin
            LockRun := 1;
            for j := 0 to AnUnitInfo.OpenEditorInfoCount - 1 do
              if (TSourceEditor(AnUnitInfo.OpenEditorInfo[j].EditorComponent).SourceNotebook
                  = SourceEditorManager.ActiveSourceWindow)
              and EditorMatches(AnUnitInfo.OpenEditorInfo[j], Access) then begin
                Result := AnUnitInfo.OpenEditorInfo[j];
                break;
              end;
          end;
      end;
      inc(LockRun);
    until (LockRun > 1) or (Result <> nil);
    if (Result = nil) then
      case Access.SearchOpenNew of
        eoeaNoNewTab: ;
        eoeaNewTabInExistingWindowOnly:
          begin
            w := AvailSrcWindowIndex;
            if w >= 0 then
              if OpenFileInSourceEditor(AnUnitInfo.GetClosedOrNewEditorInfo, -1, w, []) = mrOk then
                Result := AnUnitInfo.OpenEditorInfo[0]; // newly opened will be last focused
          end;
        eoeaNewTabInNewWindowOnly:
          begin
            if OpenFileInSourceEditor(AnUnitInfo.GetClosedOrNewEditorInfo,
                                      -1, SourceEditorManager.SourceWindowCount, []) = mrOk then
              Result := AnUnitInfo.OpenEditorInfo[0]; // newly opened will be last focused
          end;
        eoeaNewTabInExistingOrNewWindow:
          begin
            w := AvailSrcWindowIndex;
            if w < 0 then w := SourceEditorManager.SourceWindowCount;
            if OpenFileInSourceEditor(AnUnitInfo.GetClosedOrNewEditorInfo, -1, w, []) = mrOk then
              Result := AnUnitInfo.OpenEditorInfo[0]; // newly opened will be last focused
          end;
      end;
    if Result <> nil then
      break;
  end;
  if Result = nil then // should never happen
    Result := AnUnitInfo.OpenEditorInfo[0];
end;

function TLazSourceFileManager.SomethingOfProjectIsModified(Verbose: boolean): boolean;
begin
  Result:=(Project1<>nil)
      and (Project1.SomethingModified(true,true,Verbose)
           or SourceEditorManager.SomethingModified(Verbose));
end;

function TLazSourceFileManager.FileExistsInIDE(const Filename: string;
  SearchFlags: TProjectFileSearchFlags): boolean;
begin
  Result:=FileExistsCached(Filename)
          or ((Project1<>nil) and (Project1.UnitInfoWithFilename(Filename,SearchFlags)<>nil));
end;

function TLazSourceFileManager.NewFile(NewFileDescriptor: TProjectFileDescriptor;
  var NewFilename: string; NewSource: string;
  NewFlags: TNewFlags; NewOwner: TObject): TModalResult;

  function BeautifySrc(const s: string): string;
  begin
    Result:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(s,0);
  end;

var
  NewUnitInfo:TUnitInfo;
  NewSrcEdit: TSourceEditor;
  NewUnitName: string;
  NewBuffer: TCodeBuffer;
  OldUnitIndex: Integer;
  AncestorType: TPersistentClass;
  LFMFilename: String;
  SearchFlags: TProjectFileSearchFlags;
  LFMSourceText: String;
  LFMCode: TCodeBuffer;
  AProject: TProject;
  LRSFilename: String;
  ResType: TResourceType;
  SrcNoteBook: TSourceNotebook;
  AShareEditor: TSourceEditor;
  DisableAutoSize: Boolean;
  APackage: TLazPackage;
  IsPartOfProject: Boolean;
  RequiredPackages: String;
  Src: String;
begin
  //debugln('TLazSourceFileManager.NewEditorFile A NewFilename=',NewFilename);
  // empty NewFilename is ok, it will be auto generated
  SaveSourceEditorChangesToCodeCache(nil);

  // convert macros in filename
  if nfConvertMacros in NewFlags then begin
    if not GlobalMacroList.SubstituteStr(NewFilename) then begin
      Result:=mrCancel;
      exit;
    end;
  end;

  Result:=NewFileDescriptor.Init(NewFilename,NewOwner,NewSource,nfQuiet in NewFlags);
  if Result<>mrOk then exit;

  if FilenameIsAbsolute(NewFilename) and DirectoryExistsUTF8(NewFilename) then
  begin
    IDEMessageDialog(lisFileIsDirectory,
      lisUnableToCreateNewFileBecauseThereIsAlreadyADirecto,
      mtError,[mbCancel]);
    exit(mrCancel);
  end;

  if NewOwner is TProject then
    AProject:=TProject(NewOwner)
  else
    AProject:=Project1;
  if NewOwner is TLazPackage then
    APackage:=TLazPackage(NewOwner)
  else
    APackage:=nil;

  OldUnitIndex:=AProject.IndexOfFilename(NewFilename);
  if OldUnitIndex>=0 then begin
    // the file is not really new
    // => close form
    Result:=CloseUnitComponent(AProject.Units[OldUnitIndex],
                               [cfCloseDependencies,cfSaveDependencies]);
    if Result<>mrOk then
    begin
      debugln(['TLazSourceFileManager.NewFile CloseUnitComponent failed']);
      exit;
    end;
  end;

  IsPartOfProject:=(nfIsPartOfProject in NewFlags)
                   or (NewOwner is TProject)
                   or (AProject.FileIsInProjectDir(NewFilename)
                       and (not (nfIsNotPartOfProject in NewFlags)));

  // add required packages
  //debugln(['TLazSourceFileManager.NewFile NewFileDescriptor.RequiredPackages="',NewFileDescriptor.RequiredPackages,'" ',DbgSName(NewFileDescriptor)]);
  RequiredPackages:=NewFileDescriptor.RequiredPackages;
  if (RequiredPackages='') and (NewFileDescriptor.ResourceClass<>nil) then
  begin
    if (NewFileDescriptor.ResourceClass.InheritsFrom(TForm))
    or (NewFileDescriptor.ResourceClass.InheritsFrom(TFrame)) then
      RequiredPackages:='LCL';
  end;
  if RequiredPackages<>'' then
  begin
    if IsPartOfProject then begin
      Result:=PkgBoss.AddProjectDependencies(Project1,RequiredPackages);
      if Result<>mrOk then
      begin
        debugln(['TLazSourceFileManager.NewFile PkgBoss.AddProjectDependencies failed RequiredPackages="',RequiredPackages,'"']);
        exit;
      end;
    end;
    if APackage<>nil then
    begin
      Result:=PkgBoss.AddPackageDependency(APackage,RequiredPackages);
      if Result<>mrOk then
      begin
        debugln(['TLazSourceFileManager.NewFile PkgBoss.AddPackageDependency failed RequiredPackages="',RequiredPackages,'"']);
        exit;
      end;
    end;
  end;

  // check if the new file fits
  Result:=NewFileDescriptor.CheckOwner(nfQuiet in NewFlags);
  if Result<>mrOk then
  begin
    debugln(['TLazSourceFileManager.NewFile NewFileDescriptor.CheckOwner failed NewFilename="',NewFilename,'"']);
    exit;
  end;

  // create new codebuffer and apply naming conventions
  NewBuffer:=nil;
  NewUnitName:='';
  Result:=CreateNewCodeBuffer(NewFileDescriptor,NewOwner,NewFilename,NewBuffer,NewUnitName);
  if Result<>mrOk then
  begin
    debugln(['TLazSourceFileManager.NewFile CreateNewCodeBuffer failed NewFilename="',NewFilename,'"']);
    exit;
  end;
  NewFilename:=NewBuffer.Filename;

  if OldUnitIndex>=0 then begin
    // the file is not really new
    NewUnitInfo:=AProject.Units[OldUnitIndex];
    // assign source
    NewUnitInfo.Source:=NewBuffer;
  end else
    NewUnitInfo:=TUnitInfo.Create(NewBuffer);
  //debugln(['TLazSourceFileManager.NewFile ',NewUnitInfo.Filename,' ',NewFilename]);
  NewUnitInfo.ImproveUnitNameCache(NewUnitName);
  NewUnitInfo.BuildFileIfActive:=NewFileDescriptor.BuildFileIfActive;
  NewUnitInfo.RunFileIfActive:=NewFileDescriptor.RunFileIfActive;

  // create source code
  //debugln('TLazSourceFileManager.NewEditorFile A nfCreateDefaultSrc=',nfCreateDefaultSrc in NewFlags,' ResourceClass=',dbgs(NewFileDescriptor.ResourceClass));
  if nfCreateDefaultSrc in NewFlags then begin
    if (NewFileDescriptor.ResourceClass<>nil) then begin
      NewUnitInfo.ComponentName:=NewUniqueComponentName(NewFileDescriptor.DefaultResourceName);
      NewUnitInfo.ComponentResourceName:='';
    end;
    Src:=NewFileDescriptor.CreateSource(NewUnitInfo.Filename,NewUnitName,NewUnitInfo.ComponentName);
    Src:=SourceEditorManager.ReIndent(Src);
    //debugln(['TLazSourceFileManager.NewFile ',dbgtext(Src)]);
    Src:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(Src,0);
    NewUnitInfo.Source.Source:=Src;
  end else begin
    if nfBeautifySrc in NewFlags then
      NewBuffer.Source:=BeautifySrc(NewSource)
    else
      NewBuffer.Source:=NewSource;
  end;
  NewUnitInfo.Modified:=true;

  // add to project
  NewUnitInfo.Loaded:=true;
  NewUnitInfo.IsPartOfProject:=IsPartOfProject;
  if OldUnitIndex<0 then begin
    AProject.AddFile(NewUnitInfo,
                     NewFileDescriptor.AddToProject
                     and NewFileDescriptor.IsPascalUnit
                     and NewUnitInfo.IsPartOfProject
                     and (pfMainUnitHasUsesSectionForAllUnits in AProject.Flags));
  end;

  // syntax highlighter type
  NewUnitInfo.DefaultSyntaxHighlighter := FilenameToLazSyntaxHighlighter(NewFilename);

  if nfOpenInEditor in NewFlags then begin
    // open a new sourceeditor
    SrcNoteBook := SourceEditorManager.ActiveOrNewSourceWindow;
    AShareEditor := nil;
    if NewUnitInfo.OpenEditorInfoCount > 0 then
      AShareEditor := TSourceEditor(NewUnitInfo.OpenEditorInfo[0].EditorComponent);
    NewSrcEdit := SrcNoteBook.NewFile(
      CreateSrcEditPageName(NewUnitInfo.Unit_Name, NewUnitInfo.Filename, AShareEditor),
      NewUnitInfo.Source, True, AShareEditor);
    MainIDEBar.itmFileClose.Enabled:=True;
    MainIDEBar.itmFileCloseAll.Enabled:=True;
    NewSrcEdit.SyntaxHighlighterType:=NewUnitInfo.EditorInfo[0].SyntaxHighlighter;
    NewUnitInfo.GetClosedOrNewEditorInfo.EditorComponent := NewSrcEdit;
    NewSrcEdit.EditorComponent.CaretXY := Point(1,1);

    // create component
    AncestorType:=NewFileDescriptor.ResourceClass;
    //DebugLn(['TLazSourceFileManager.NewFile AncestorType=',dbgsName(AncestorType),' ComponentName',NewUnitInfo.ComponentName]);
    if AncestorType<>nil then begin
      ResType:=MainBuildBoss.GetResourceType(NewUnitInfo);
      LFMSourceText:=NewFileDescriptor.GetResourceSource(NewUnitInfo.ComponentName);
      //DebugLn(['TLazSourceFileManager.NewFile LFMSourceText=',LFMSourceText]);
      if LFMSourceText<>'' then begin
        // the NewFileDescriptor provides a custom .lfm source
        // -> put it into a new .lfm buffer and load it
        LFMFilename:=ChangeFileExt(NewUnitInfo.Filename,'.lfm');
        LFMCode:=CodeToolBoss.CreateFile(LFMFilename);
        LFMCode.Source:=LFMSourceText;
        //debugln('TLazSourceFileManager.NewEditorFile A ',LFMFilename);
        Result:=LoadLFM(NewUnitInfo,LFMCode,[],[]);
        //DebugLn(['TLazSourceFileManager.NewFile ',dbgsName(NewUnitInfo.Component),' ',dbgsName(NewUnitInfo.Component.ClassParent)]);
        // make sure the .lrs file exists
        if (ResType=rtLRS) and NewUnitInfo.IsVirtual then begin
          LRSFilename:=ChangeFileExt(NewUnitInfo.Filename,'.lrs');
          CodeToolBoss.CreateFile(LRSFilename);
        end;
        if (NewUnitInfo.Component<>nil)
        and NewFileDescriptor.UseCreateFormStatements
        and NewUnitInfo.IsPartOfProject
        and AProject.AutoCreateForms
        and (pfMainUnitHasCreateFormStatements in AProject.Flags) then
        begin
          AProject.AddCreateFormToProjectFile(NewUnitInfo.Component.ClassName,
                                              NewUnitInfo.Component.Name);
        end;
      end else begin
        // create a designer form for a form/datamodule/frame
        //DebugLn(['TLazSourceFileManager.NewFile Name=',NewFileDescriptor.Name,' Class=',NewFileDescriptor.ClassName]);
        DisableAutoSize:=true;
        Result := CreateNewForm(NewUnitInfo, AncestorType, nil,
                                NewFileDescriptor.UseCreateFormStatements,
                                DisableAutoSize);
        if DisableAutoSize and (NewUnitInfo.Component<>nil)
        and (NewUnitInfo.Component is TControl) then
          TControl(NewUnitInfo.Component).EnableAutoSizing;
      end;
      if Result<>mrOk then
      begin
        debugln(['TLazSourceFileManager.NewFile create designer form failed ',NewUnitInfo.Filename]);
        exit;
      end;
    end;

    // show form and select form
    if NewUnitInfo.Component<>nil then begin
      // show form
      MainIDE.CreateObjectInspector;
      MainIDE.DoShowDesignerFormOfCurrentSrc;
    end else begin
      MainIDE.DisplayState:= dsSource;
    end;
  end else begin
    // do not open in editor
  end;

  // Update HasResources property (if the .lfm file was created separately)
  if (not NewUnitInfo.HasResources)
  and FilenameIsPascalUnit(NewUnitInfo.Filename) then begin
    //debugln('TLazSourceFileManager.NewFile no HasResources ',NewUnitInfo.Filename);
    LFMFilename:=ChangeFileExt(NewUnitInfo.Filename,'.lfm');
    SearchFlags:=[];
    if NewUnitInfo.IsPartOfProject then
      Include(SearchFlags,pfsfOnlyProjectFiles);
    if NewUnitInfo.IsVirtual then
      Include(SearchFlags,pfsfOnlyVirtualFiles);
    if (AProject.UnitInfoWithFilename(LFMFilename,SearchFlags)<>nil) then begin
      //debugln('TLazSourceFileManager.NewFile no HasResources ',NewUnitInfo.Filename,' ResourceFile exists');
      NewUnitInfo.HasResources:=true;
    end;
  end;

  if (nfAskForFilename in NewFlags) then begin
    // save and ask for filename
    NewUnitInfo.Modified:=true;
    Result:=SaveEditorFile(NewSrcEdit,[sfCheckAmbiguousFiles,sfSaveAs]);
    if Result<>mrOk then
    begin
      debugln(['TLazSourceFileManager.NewFile SaveEditorFile failed ',NewFilename]);
      exit;
    end;
  end else if nfSave in NewFlags then begin
    if (nfOpenInEditor in NewFlags) or NewBuffer.IsVirtual then begin
      // save and ask for filename if needed
      NewUnitInfo.Modified:=true;
      Result:=SaveEditorFile(NewSrcEdit,[sfCheckAmbiguousFiles]);
      if Result<>mrOk then
      begin
        debugln(['TLazSourceFileManager.NewFile SaveEditorFile SaveAs failed ',NewFilename]);
        exit;
      end;
    end else begin
      // save quietly
      NewBuffer.Save;
    end;
  end;

  Result:=mrOk;
  //DebugLn('TLazSourceFileManager.NewEditorFile END ',NewUnitInfo.Filename);
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.NewUnit end');{$ENDIF}
end;

function TLazSourceFileManager.NewOther: TModalResult;
var
  NewIDEItem: TNewIDEItemTemplate;
  NewProjFile: TNewItemProjectFile;
begin
  Result:=ShowNewIDEItemDialog(NewIDEItem);
  if Result<>mrOk then exit;
  if NewIDEItem is TNewItemProjectFile then begin
    // file
    NewProjFile:=TNewItemProjectFile(NewIDEItem);
    if NewProjFile.Descriptor<>nil then
      NewProjFile.Descriptor.Owner:=Project1;
    try
      Result:=MainIDE.DoNewEditorFile(NewProjFile.Descriptor,
                                      '','',[nfOpenInEditor,nfCreateDefaultSrc]);
    finally
      if NewProjFile.Descriptor<>nil then
        NewProjFile.Descriptor.Owner:=nil;
    end;
  end else if NewIDEItem is TNewItemProject then       // project
    Result:=MainIDE.DoNewProject(TNewItemProject(NewIDEItem).Descriptor)
  else if NewIDEItem is TNewItemPackage then           // packages
    PkgBoss.DoNewPackage
  else
    IDEMessageDialog(ueNotImplCap, lisSorryThisTypeIsNotYetImplemented, mtInformation,[mbOk]);
end;

function TLazSourceFileManager.NewUnitOrForm(Template: TNewIDEItemTemplate;
  DefaultDesc: TProjectFileDescriptor): TModalResult;
var
  Desc: TProjectFileDescriptor;
  Flags: TNewFlags;
begin
  if (Template is TNewItemProjectFile) and Template.VisibleInNewDialog then
    Desc:=TNewItemProjectFile(Template).Descriptor
  else
    Desc:=DefaultDesc;
  Flags:=[nfOpenInEditor,nfCreateDefaultSrc];
  if (not Project1.IsVirtual) and EnvironmentOptions.AskForFilenameOnNewFile then
    Flags:=Flags+[nfAskForFilename,nfSave];
  Desc.Owner:=Project1;
  try
    Result := MainIDE.DoNewEditorFile(Desc,'','',Flags);
  finally
    Desc.Owner:=nil;
  end;
end;

procedure TLazSourceFileManager.CreateFileDialogFilterForSourceEditorFiles(Filter: string;
  out AllEditorMask, AllMask: string);
// Filter: a TFileDialog filter, e.g. Pascal|*.pas;*.pp|Text|*.txt
// AllEditorExt: a mask for all open files in the source editor, that are not
//               in Filter, e.g. '*.txt;*.xml'
// AllFilter: all masks of Filter and AllEditorExt, e.g. '*.pas;*.pp;*.inc'
var
  i: Integer;
  SrcEdit: TSourceEditor;
  Ext: String;
begin
  AllMask:='|'+TFileDialog.ExtractAllFilterMasks(Filter);
  AllEditorMask:='|';
  for i:=0 to SourceEditorManager.SourceEditorCount-1 do begin
    SrcEdit:=SourceEditorManager.SourceEditors[i];
    Ext:=ExtractFileExt(SrcEdit.FileName);
    if Ext<>'' then begin
      Ext:='*'+Ext;
      if (TFileDialog.FindMaskInFilter(AllMask,Ext)>0)
      or (TFileDialog.FindMaskInFilter(AllEditorMask,Ext)>0) then continue;
      if AllEditorMask<>'|' then
        AllEditorMask:=AllEditorMask+';';
      AllEditorMask:=AllEditorMask+Ext;
    end;
  end;
  System.Delete(AllMask,1,1);
  System.Delete(AllEditorMask,1,1);
  if AllEditorMask<>'' then begin
    if AllMask<>'' then
      AllMask:=AllMask+';';
    AllMask:=AllMask+AllEditorMask;
  end;
end;

function TLazSourceFileManager.SaveEditorFile(AEditor: TSourceEditorInterface;
  Flags: TSaveFlags): TModalResult;
var
  AnUnitInfo, MainUnitInfo: TUnitInfo;
  TestFilename, DestFilename: string;
  LRSCode, LFMCode: TCodeBuffer;
  OldUnitName, OldFilename: String;
  NewUnitName, NewFilename: String;
  WasVirtual, WasPascalSource, CanAbort, Confirm: Boolean;
  SaveProjectFlags: TSaveFlags;
  EMacro: TEditorMacro;
begin
  Result:=mrCancel;
  CanAbort:=[sfCanAbort,sfProjectSaving]*Flags<>[];
  //writeln('TLazSourceFileManager.SaveEditorFile A PageIndex=',PageIndex,' Flags=',SaveFlagsToString(Flags));
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.SaveEditorFile A');{$ENDIF}
  if not (MainIDE.ToolStatus in [itNone,itDebugger]) then
    exit(mrAbort);
  if AEditor=nil then exit(mrCancel);
  AnUnitInfo := Project1.UnitWithEditorComponent(AEditor);
  if AnUnitInfo=nil then exit(mrCancel);

  // do not save a unit which is currently reverting
  if AnUnitInfo.IsReverting then
    exit(mrOk);

  WasVirtual:=AnUnitInfo.IsVirtual;
  WasPascalSource:=FilenameIsPascalSource(AnUnitInfo.Filename);

  // if this file is part of the project and the project is virtual then save
  // project first
  if (not (sfProjectSaving in Flags)) and Project1.IsVirtual
  and AnUnitInfo.IsPartOfProject then
  begin
    SaveProjectFlags:=Flags*[sfSaveToTestDir];
    if AnUnitInfo=Project1.MainUnitInfo then
      Include(SaveProjectFlags,sfSaveMainSourceAs);
    Result:=SaveProject(SaveProjectFlags);
    exit;
  end;

  // update codetools cache and collect Modified flags
  if not (sfProjectSaving in Flags) then
    SaveSourceEditorChangesToCodeCache(nil);

  if (uifInternalFile in AnUnitInfo.Flags) then
  begin
    if (copy(AnUnitInfo.Filename, 1, length(EditorMacroVirtualDrive)) = EditorMacroVirtualDrive)
    then begin
      // save to macros
      EMacro := MacroListViewer.MacroByFullName(AnUnitInfo.Filename);
      if EMacro <> nil then begin
        EMacro.SetFromSource(AEditor.SourceText);
        if EMacro.IsInvalid and (EMacro.ErrorMsg <> '') then
          {$IFDEF EnableNewExtTools}
          IDEMessagesWindow.AddCustomMessage(mluError,EMacro.ErrorMsg);
          {$ELSE}
          MessagesView.AddMsg(EMacro.ErrorMsg, '', -1);
          {$ENDIF}
      end;
      MacroListViewer.UpdateDisplay;
      AnUnitInfo.ClearModifieds;
      AEditor.Modified:=false;
      Result := mrOK;
      exit;
    end;
    // unknown internal file => skip
    exit(mrOk);
  end;

  // if this is a new unit then a simple Save becomes a SaveAs
  if (not (sfSaveToTestDir in Flags)) and (AnUnitInfo.IsVirtual) then
    Include(Flags,sfSaveAs);

  // if this is the main source and has the same name as the lpi
  // rename the project
  // Note:
  //   Changing the main source file without the .lpi is possible only by
  //   manually editing the lpi file, because this is only needed in
  //   special cases (rare functions don't need front ends).
  MainUnitInfo:=AnUnitInfo.Project.MainUnitInfo;
  if (sfSaveAs in Flags) and (not (sfProjectSaving in Flags))
  and (AnUnitInfo=MainUnitInfo)
  then begin
    Result:=SaveProject([sfSaveAs,sfSaveMainSourceAs]);
    exit;
  end;

  // if nothing modified then a simple Save can be skipped
  //debugln(['TLazSourceFileManager.SaveEditorFile A ',AnUnitInfo.Filename,' ',AnUnitInfo.NeedsSaveToDisk]);
  if ([sfSaveToTestDir,sfSaveAs]*Flags=[])
  and (not AnUnitInfo.NeedsSaveToDisk) then
  begin
    if AEditor.Modified then
    begin
      AnUnitInfo.SessionModified:=true;
      AEditor.Modified:=false;
    end;
    exit(mrOk);
  end;

  // check if file is writable on disk
  if (not AnUnitInfo.IsVirtual)
  and FileExistsUTF8(AnUnitInfo.Filename) then
    AnUnitInfo.FileReadOnly:=not FileIsWritable(AnUnitInfo.Filename)
  else
    AnUnitInfo.FileReadOnly:=false;

  // if file is readonly then a simple Save is skipped
  if (AnUnitInfo.ReadOnly) and ([sfSaveToTestDir,sfSaveAs]*Flags=[]) then
    exit(mrOk);

  // load old resource file
  LFMCode:=nil;
  LRSCode:=nil;
  if WasPascalSource then
  begin
    Result:=LoadResourceFile(AnUnitInfo,LFMCode,LRSCode,
                               not (sfSaveAs in Flags),true,CanAbort);
    if not (Result in [mrIgnore,mrOk]) then
      exit;
  end;

  OldUnitName:='';
  if WasPascalSource then
    OldUnitName:=AnUnitInfo.ParseUnitNameFromSource(true);
  OldFilename:=AnUnitInfo.Filename;

  if [sfSaveAs,sfSaveToTestDir]*Flags=[sfSaveAs] then begin
    // let user choose a filename
    NewFilename:=OldFilename;
    Result:=ShowSaveFileAsDialog(NewFilename,AnUnitInfo,LFMCode,LRSCode,CanAbort);
    if not (Result in [mrIgnore,mrOk]) then
      exit;
  end;

  // save source

  // a) do before save events
  if EditorOpts.AutoRemoveEmptyMethods and (AnUnitInfo.Component<>nil) then begin
    // Note: When removing published methods, the source, the lfm, the lrs
    //       and the form must be changed. At the moment editing the lfm without
    //       the component is not yet implemented.
    Result:=RemoveEmptyMethods(AnUnitInfo.Source,
                   AnUnitInfo.Component.ClassName,0,0,false,[pcsPublished]);
    if Result=mrAbort then exit;
  end;

  // b) do actual save
  if (sfSaveToTestDir in Flags) or AnUnitInfo.IsVirtual then
  begin
    // save source to test directory
    TestFilename := MainBuildBoss.GetTestUnitFilename(AnUnitInfo);
    if TestFilename <> '' then
    begin
      //DebugLn(['TLazSourceFileManager.SaveEditorFile TestFilename="',TestFilename,'" Size=',AnUnitInfo.Source.SourceLength]);
      Result := AnUnitInfo.WriteUnitSourceToFile(TestFilename);
      if Result <> mrOk then
        Exit;
      DestFilename := TestFilename;
    end
    else
      exit(mrCancel);
  end else
  begin
    if AnUnitInfo.Modified or AnUnitInfo.NeedsSaveToDisk then
    begin
      // save source to file
      Result := AnUnitInfo.WriteUnitSource;
      if Result <> mrOK then
        exit;
      DestFilename := AnUnitInfo.Filename;
    end;
  end;

  if sfCheckAmbiguousFiles in Flags then
    MainBuildBoss.CheckAmbiguousSources(DestFilename,false);

  {$IFDEF IDE_DEBUG}
  writeln('*** HasResources=',AnUnitInfo.HasResources);
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.SaveEditorFile B');{$ENDIF}
  // save resource file and lfm file
  if (LRSCode<>nil) or (AnUnitInfo.Component<>nil) then begin
    Result:=SaveUnitComponent(AnUnitInfo,LRSCode,LFMCode,Flags);
    if not (Result in [mrIgnore, mrOk]) then
      exit;
  end;

  // unset all modified flags
  if not (sfSaveToTestDir in Flags) then begin
    AnUnitInfo.ClearModifieds;
    AEditor.Modified:=false;
    MainIDE.UpdateSaveMenuItemsAndButtons(not (sfProjectSaving in Flags));
  end;
  TSourceEditor(AEditor).SourceNotebook.UpdateStatusBar;

  // fix all references
  NewUnitName:='';
  if FilenameIsPascalSource(AnUnitInfo.Filename) then
    NewUnitName:=AnUnitInfo.ParseUnitNameFromSource(true);
  NewFilename:=AnUnitInfo.Filename;
  if (NewUnitName<>'')
  and  ((OldUnitName<>NewUnitName)
        or (CompareFilenames(OldFilename,NewFilename)<>0))
  then begin
    if EnvironmentOptions.UnitRenameReferencesAction<>urraNever then
    begin
      // silently update references of new units (references were auto created
      // and keeping old references makes no sense)
      Confirm:=(EnvironmentOptions.UnitRenameReferencesAction=urraAsk)
               and (not WasVirtual);
      Result:=ReplaceUnitUse(OldFilename,OldUnitName,NewFilename,NewUnitName,
               true,true,Confirm);
      if Result<>mrOk then exit;
    end;
  end;

  {$IFDEF IDE_VERBOSE}
  debugln(['TLazSourceFileManager.SaveEditorFile END ',NewFilename,' AnUnitInfo.Modified=',AnUnitInfo.Modified,' AEditor.Modified=',AEditor.Modified]);
  {$ENDIF}
  Result:=mrOk;
end;

function TLazSourceFileManager.SaveEditorFile(const Filename: string;
  Flags: TSaveFlags): TModalResult;
var
  UnitIndex: Integer;
  AnUnitInfo: TUnitInfo;
  i: Integer;
begin
  Result:=mrOk;
  if Filename='' then exit;
  UnitIndex:=Project1.IndexOfFilename(TrimFilename(Filename),[pfsfOnlyEditorFiles]);
  if UnitIndex<0 then exit;
  AnUnitInfo:=Project1.Units[UnitIndex];
  for i := 0 to AnUnitInfo.OpenEditorInfoCount-1 do begin
    Result:=SaveEditorFile(AnUnitInfo.OpenEditorInfo[i].EditorComponent, Flags);
    if Result <> mrOK then Break;
  end;
end;

function TLazSourceFileManager.CloseEditorFile(AEditor: TSourceEditorInterface;
  Flags: TCloseFlags): TModalResult;
var
  AnUnitInfo: TUnitInfo;
  ACaption, AText: string;
  i: integer;
  AnEditorInfo: TUnitEditorInfo;
  SrcEditWasFocused: Boolean;
  SrcEdit: TSourceEditor;
begin
  {$IFDEF IDE_DEBUG}
  //debugln('TLazSourceFileManager.CloseEditorFile A PageIndex=',IntToStr(AnUnitInfo.PageIndex));
  {$ENDIF}
  Result:=mrCancel;
  if AEditor = nil then exit;
  AnEditorInfo := Project1.EditorInfoWithEditorComponent(AEditor);
  //AnUnitInfo := Project1.UnitWithEditorComponent(AEditor);
  if AnEditorInfo = nil then begin
    // we need to close the page anyway or else we might enter a loop
    DebugLn('TLazSourceFileManager.CloseEditorFile INCONSISTENCY: NO AnUnitInfo');
    SourceEditorManager.CloseFile(AEditor);
    Result:=mrOk;
    exit;
  end;
  AnUnitInfo := AnEditorInfo.UnitInfo;
  AnUnitInfo.SessionModified:=true;
  SrcEditWasFocused:=(AnEditorInfo.EditorComponent<>nil)
     and (AnEditorInfo.EditorComponent.EditorControl<>nil)
     and AnEditorInfo.EditorComponent.EditorControl.Focused;
  //debugln(['TLazSourceFileManager.CloseEditorFile File=',AnUnitInfo.Filename,' WasFocused=',SrcEditWasFocused]);
  try
    //debugln(['TLazSourceFileManager.CloseEditorFile File=',AnUnitInfo.Filename,' UnitSession=',AnUnitInfo.SessionModified,' ProjSession=',project1.SessionModified]);
    if AnUnitInfo.OpenEditorInfoCount > 1 then begin
      // opened multiple times => close one instance
      SourceEditorManager.CloseFile(AEditor);
      Result:=mrOk;
      exit;
    end;

    if (AnUnitInfo.Component<>nil) and (MainIDE.LastFormActivated<>nil)
    and (TDesigner(MainIDE.LastFormActivated.Designer).LookupRoot=AnUnitInfo.Component) then
      MainIDE.LastFormActivated:=nil;

    // save some meta data of the source
    SaveSrcEditorProjectSpecificSettings(AnEditorInfo);

    // if SaveFirst then save the source
    if (cfSaveFirst in Flags) and (not AnUnitInfo.ReadOnly)
    and ((AEditor.Modified) or (AnUnitInfo.Modified)) then begin
      if not (cfQuiet in Flags) then begin
        // ask user
        if AnUnitInfo.Filename<>'' then
          AText:=Format(lisFileHasChangedSave, ['"', AnUnitInfo.Filename, '"'])
        else if AnUnitInfo.Unit_Name<>'' then
          AText:=Format(lisUnitHasChangedSave, ['"', AnUnitInfo.Unit_name, '"'])
        else
          AText:=Format(lisSourceOfPageHasChangedSave, ['"',
            TSourceEditor(AEditor).PageName, '"']);
        ACaption:=lisSourceModified;
        Result:=IDEQuestionDialog(ACaption, AText,
            mtConfirmation, [mrYes, lisMenuSave, mrNo, lisDiscardChanges, mrAbort
              ]);
      end else
        Result:=mrYes;
      if Result=mrYes then begin
        Result:=SaveEditorFile(AnEditorInfo.EditorComponent,[sfCheckAmbiguousFiles]);
      end;
      if Result=mrAbort then exit;
      Result:=mrOk;
    end;

    // add to recent file list
    if (not AnUnitInfo.IsVirtual)
    and (not (cfProjectClosing in Flags)) then
    begin
      EnvironmentOptions.AddToRecentOpenFiles(AnUnitInfo.Filename);
      MainIDE.SetRecentFilesMenu;
    end;

    // close form soft (keep it if used by another component)
    CloseUnitComponent(AnUnitInfo,[]);

    // close source editor
    SourceEditorManager.CloseFile(AnEditorInfo.EditorComponent);
    MainIDEBar.itmFileClose.Enabled:=SourceEditorManager.SourceEditorCount > 0;
    MainIDEBar.itmFileCloseAll.Enabled:=MainIDEBar.itmFileClose.Enabled;

    // free sources, forget changes
    if (AnUnitInfo.Source<>nil) then begin
      if (Project1.MainUnitInfo=AnUnitInfo)
      and (not (cfProjectClosing in Flags)) then begin
        AnUnitInfo.Source.Revert;
      end else begin
        AnUnitInfo.Source.IsDeleted:=true;
      end;
    end;

    // close file in project
    AnUnitInfo.Loaded:=false;
    if AnUnitInfo<>Project1.MainUnitInfo then
      AnUnitInfo.Source:=nil;
    if not (cfProjectClosing in Flags) then begin
      i:=Project1.IndexOf(AnUnitInfo);
      if (i<>Project1.MainUnitID) and AnUnitInfo.IsVirtual then begin
        Project1.RemoveUnit(i);
      end;
    end;

  finally
    if SrcEditWasFocused then begin
      // before closing the syendit was focused. Focus the current synedit.
      SrcEdit := SourceEditorManager.ActiveEditor;
      if (SrcEdit<>nil)
      and (SrcEdit.EditorControl<>nil)
      and (SrcEdit.EditorControl.CanFocus) then
        SrcEdit.EditorControl.SetFocus;
      //debugln(['TLazSourceFileManager.CloseEditorFile Focus=',SrcEdit.FileName,' Editor=',DbgSName(SrcEdit.EditorControl),' Focused=',(SrcEdit.EditorControl<>nil) and (SrcEdit.EditorControl.Focused)]);
    end;
  end;
  {$IFDEF IDE_DEBUG}
  DebugLn('TLazSourceFileManager.CloseEditorFile end');
  {$ENDIF}
  Result:=mrOk;
end;

function TLazSourceFileManager.CloseEditorFile(const Filename: string;
  Flags: TCloseFlags): TModalResult;
var
  UnitIndex: Integer;
  AnUnitInfo: TUnitInfo;
begin
  Result:=mrOk;
  if Filename='' then exit;
  UnitIndex:=Project1.IndexOfFilename(TrimFilename(Filename),[pfsfOnlyEditorFiles]);
  if UnitIndex<0 then exit;
  AnUnitInfo:=Project1.Units[UnitIndex];
  while (AnUnitInfo.OpenEditorInfoCount > 0) and (Result = mrOK) do
    Result:=CloseEditorFile(AnUnitInfo.OpenEditorInfo[0].EditorComponent, Flags);
end;

function TLazSourceFileManager.OpenEditorFile(AFileName: string; PageIndex,
  WindowIndex: integer; AEditorInfo: TUnitEditorInfo; Flags: TOpenFlags;
  UseWindowID: Boolean): TModalResult;
var
  UnitIndex: integer;
  UnknownFile, Handled: boolean;
  NewUnitInfo:TUnitInfo;
  NewBuf: TCodeBuffer;
  FilenameNoPath: String;
  LoadBufferFlags: TLoadBufferFlags;
  DiskFilename: String;
  Reverting: Boolean;
  CanAbort: boolean;
  NewEditorInfo: TUnitEditorInfo;

  function OpenResource: TModalResult;
  var
    CloseFlags: TCloseFlags;
  begin
    // read form data
    if FilenameIsPascalUnit(AFilename) then begin
      // this could be a unit with a form
      //debugln('TLazSourceFileManager.OpenEditorFile ',AFilename,' ',OpenFlagsToString(Flags));
      if ([ofDoNotLoadResource]*Flags=[])
      and ( (ofDoLoadResource in Flags)
         or ((ofProjectLoading in Flags)
             and NewUnitInfo.LoadedDesigner
             and (not Project1.AutoOpenDesignerFormsDisabled)
             and EnvironmentOptions.AutoCreateFormsOnOpen))
      then begin
        // -> try to (re)load the lfm file
        //debugln(['TLazSourceFileManager.OpenEditorFile Loading LFM for ',NewUnitInfo.Filename,' LoadedDesigner=',NewUnitInfo.LoadedDesigner]);
        CloseFlags:=[cfSaveDependencies];
        if ofRevert in Flags then
          Include(CloseFlags,cfCloseDependencies);
        Result:=LoadLFM(NewUnitInfo,Flags,CloseFlags);
        if Result<>mrOk then begin
          DebugLn(['OpenResource DoLoadLFM failed']);
          exit;
        end;
      end else begin
        Result:=mrOk;
      end;
    end else if NewUnitInfo.Component<>nil then begin
      // this is no pascal source and there is a designer form
      // This can be the case, when the file is renamed and/or reverted
      // -> close form
      Result:=CloseUnitComponent(NewUnitInfo,
                                 [cfCloseDependencies,cfSaveDependencies]);
      if Result<>mrOk then begin
        DebugLn(['OpenResource CloseUnitComponent failed']);
      end;
    end else begin
      Result:=mrOk;
    end;
    if NewUnitInfo.Component=nil then
      NewUnitInfo.LoadedDesigner:=false;
  end;

begin
  {$IFDEF IDE_VERBOSE}
  DebugLn('');
  DebugLn(['*** TLazSourceFileManager.OpenEditorFile START "',AFilename,'" ',OpenFlagsToString(Flags),' Window=',WindowIndex,' Page=',PageIndex]);
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.OpenEditorFile START');{$ENDIF}
  Result:=mrCancel;

  CanAbort:=[ofProjectLoading,ofMultiOpen]*Flags<>[];

  // replace macros
  if ofConvertMacros in Flags then begin
    if not GlobalMacroList.SubstituteStr(AFilename) then exit;
    AFilename:=ExpandFileNameUTF8(AFilename);
  end;

  if (ofRevert in Flags) then begin
    // revert: use source editor filename
    if (PageIndex>=0) then begin
      if UseWindowID then
        WindowIndex := SourceEditorManager.IndexOfSourceWindowWithID(WindowIndex); // Revert must have a valid ID
      UseWindowID := False;
      assert((WindowIndex >= 0) and (WindowIndex < SourceEditorManager.SourceWindowCount), 'WindowIndex for revert');
      AFilename := SourceEditorManager.SourceEditorsByPage[WindowIndex, PageIndex].FileName;
    end;

    UnitIndex:=Project1.IndexOfFilename(AFilename);
    if (UnitIndex > 0) then begin
      NewUnitInfo:=Project1.Units[UnitIndex];
      if (uifInternalFile in NewUnitInfo.Flags) then
      begin
        if (NewUnitInfo.OpenEditorInfoCount > 0) then begin
          NewEditorInfo := NewUnitInfo.OpenEditorInfo[0];
          if MacroListViewer.MacroByFullName(AFileName) <> nil then
            NewUnitInfo.Source.Source := MacroListViewer.MacroByFullName(AFileName).GetAsSource;
          Result:=OpenFileInSourceEditor(NewEditorInfo, NewEditorInfo.PageIndex,
            NewEditorInfo.WindowID, Flags, True);
          exit;
        end;
        // unknown internal file
        exit(mrOk);
      end;
    end;
  end;

  if (ofInternalFile in Flags) then begin
    if (copy(AFileName, 1, length(EditorMacroVirtualDrive)) = EditorMacroVirtualDrive)
    then begin
      FilenameNoPath := AFileName;

      UnitIndex:=Project1.IndexOfFilename(AFilename);
      if (UnitIndex < 0) then begin
        NewBuf := CodeToolBoss.SourceCache.CreateFile(AFileName);
        if MacroListViewer.MacroByFullName(AFileName) <> nil then
          NewBuf.Source := MacroListViewer.MacroByFullName(AFileName).GetAsSource;
        NewUnitInfo:=TUnitInfo.Create(NewBuf);
        NewUnitInfo.DefaultSyntaxHighlighter := lshFreePascal;
        Project1.AddFile(NewUnitInfo,false);
      end
      else begin
        NewUnitInfo:=Project1.Units[UnitIndex];
      end;
      NewUnitInfo.Flags := NewUnitInfo.Flags + [uifInternalFile];

      if NewUnitInfo.OpenEditorInfoCount > 0 then begin
        NewEditorInfo := NewUnitInfo.OpenEditorInfo[0];
        SourceEditorManager.ActiveSourceWindowIndex := SourceEditorManager.IndexOfSourceWindowWithID(NewEditorInfo.WindowID);
        SourceEditorManager.ActiveSourceWindow.PageIndex:= NewEditorInfo.PageIndex;
      end
      else begin
        NewEditorInfo := NewUnitInfo.GetClosedOrNewEditorInfo;
        Result:=OpenFileInSourceEditor(NewEditorInfo, PageIndex, WindowIndex, Flags, UseWindowID);
      end;
      Result:=mrOK;
      exit;
    end;
    // unknown internal file => ignore
    exit(mrOK);
  end;

  // normalize filename
  AFilename:=TrimFilename(AFilename);
  DiskFilename:=CodeToolBoss.DirectoryCachePool.FindDiskFilename(AFilename);
  if DiskFilename<>AFilename then begin
    // the case is different
    DebugLn(['TLazSourceFileManager.OpenEditorFile Fixing file name: ',AFilename,' -> ',DiskFilename]);
    AFilename:=DiskFilename;
  end;

  // check if symlink and ask user open the real file instead
  ChooseSymlink(AFilename);

  FilenameNoPath:=ExtractFilename(AFilename);

  // check to not open directories
  if ((FilenameNoPath='') or (FilenameNoPath='.') or (FilenameNoPath='..')) then
  begin
    DebugLn(['TLazSourceFileManager.OpenEditorFile ignoring special file: ',AFilename]);
    exit;
  end;
  if DirectoryExistsUTF8(AFileName) then begin
    debugln(['TLazSourceFileManager.OpenEditorFile skipping directory ',AFileName]);
    exit(mrCancel);
  end;

  if ([ofAddToRecent,ofRevert,ofVirtualFile]*Flags=[ofAddToRecent])
  and (AFilename<>'') and FilenameIsAbsolute(AFilename) then
    EnvironmentOptions.AddToRecentOpenFiles(AFilename);

  // check if this is a hidden unit:
  // if this is the main unit, it is already
  // loaded and needs only to be shown in the sourceeditor/formeditor
  if (not (ofRevert in Flags))
  and (CompareFilenames(Project1.MainFilename,AFilename, not (ofVirtualFile in Flags))=0)
  then begin
    Result:=OpenMainUnit(PageIndex,WindowIndex,Flags,UseWindowID);
    exit;
  end;

  // check for special files
  if ([ofRegularFile,ofRevert,ofProjectLoading]*Flags=[])
  and FilenameIsAbsolute(AFilename) and FileExistsUTF8(AFilename) then begin
    // check if file is a lazarus project (.lpi)
    if (CompareFileExt(AFilename,'.lpi',false)=0) then
    begin
      case
        IDEQuestionDialog(
          lisOpenProject, Format(lisOpenTheProject, [AFilename]), mtConfirmation,
          [mrYes, lisOpenProject2, mrNoToAll, lisOpenAsXmlFile, mrCancel])
      of
        mrYes: begin
          Result:=MainIDE.DoOpenProjectFile(AFilename,[ofAddToRecent]);
          exit;
        end;
        mrNoToAll: include(Flags, ofRegularFile);
        mrCancel: exit(mrCancel);
      end;
    end;

    // check if file is a lazarus package (.lpk)
    if (CompareFileExt(AFilename,'.lpk',false)=0) then
    begin
      case
        IDEQuestionDialog(
          lisOpenPackage, Format(lisOpenThePackage, [AFilename]), mtConfirmation,
          [mrYes, lisCompPalOpenPackage, mrNoToAll, lisOpenAsXmlFile, mrCancel])
      of
        mrYes: begin
          Result:=PkgBoss.DoOpenPackageFile(AFilename,[pofAddToRecent],CanAbort);
          exit;
        end;
        mrCancel: exit(mrCancel);
      end;
    end;
  end;

  // check if the project knows this file
  if (ofRevert in Flags) then begin
    // revert
    UnknownFile := False;
    if UseWindowID then
      NewEditorInfo := Project1.EditorInfoWithEditorComponent(
        SourceEditorManager.SourceEditorsByPage[SourceEditorManager.IndexOfSourceWindowWithID(WindowIndex), PageIndex])
    else
      NewEditorInfo := Project1.EditorInfoWithEditorComponent(
        SourceEditorManager.SourceEditorsByPage[WindowIndex, PageIndex]);
    NewUnitInfo := NewEditorInfo.UnitInfo;
    UnitIndex:=Project1.IndexOf(NewUnitInfo);
    AFilename:=NewUnitInfo.Filename;
    if CompareFilenames(AFileName,DiskFilename)=0 then
      AFileName:=DiskFilename;
    if NewUnitInfo.IsVirtual then begin
      if (not (ofQuiet in Flags)) then begin
        IDEMessageDialog(lisRevertFailed, Format(lisFileIsVirtual, ['"', AFilename,
          '"']),
          mtInformation,[mbCancel]);
      end;
      Result:=mrCancel;
      exit;
    end;
  end else begin
    UnitIndex:=Project1.IndexOfFilename(AFilename);
    UnknownFile := (UnitIndex < 0);
    NewEditorInfo := nil;
    if not UnknownFile then begin
      NewUnitInfo:=Project1.Units[UnitIndex];
      if AEditorInfo <> nil then
        NewEditorInfo := AEditorInfo
      else if (ofProjectLoading in Flags) then
        NewEditorInfo := NewUnitInfo.GetClosedOrNewEditorInfo
      else
        NewEditorInfo := NewUnitInfo.EditorInfo[0];
    end;
  end;

  if (NewEditorInfo <> nil) and (ofAddToProject in Flags) and (not NewUnitInfo.IsPartOfProject) then
  begin
    NewUnitInfo.IsPartOfProject:=true;
    Project1.Modified:=true;
  end;

  if (NewEditorInfo <> nil) and (Flags * [ofProjectLoading, ofRevert] = []) and (NewEditorInfo.EditorComponent <> nil) then
  begin
    //DebugLn(['TLazSourceFileManager.OpenEditorFile file already open ',NewUnitInfo.Filename,' WindowIndex=',NewEditorInfo.WindowID,' PageIndex=',NewEditorInfo.PageIndex]);
    // file already open -> change source notebook page
    SourceEditorManager.ActiveSourceWindowIndex := SourceEditorManager.IndexOfSourceWindowWithID(NewEditorInfo.WindowID);
    SourceEditorManager.ActiveSourceWindow.PageIndex:= NewEditorInfo.PageIndex;
    if ofDoLoadResource in Flags then
      Result:=OpenResource
    else
      Result:=mrOk;
    exit;
  end;

  Reverting:=false;
  if ofRevert in Flags then begin
    Reverting:=true;
    Project1.BeginRevertUnit(NewUnitInfo);
  end;
  try

    // check if file exists
    if FilenameIsAbsolute(AFilename) and (not FileExistsUTF8(AFilename)) then begin
      // file does not exist
      if (ofRevert in Flags) then begin
        // revert failed, due to missing file
        if not (ofQuiet in Flags) then begin
          IDEMessageDialog(lisRevertFailed, Format(lisPkgMangFileNotFound, ['"',
            AFilename, '"']),
            mtError,[mbCancel]);
        end;
        Result:=mrCancel;
        exit;
      end else begin
        Result:=OpenNotExistingFile(AFilename,Flags);
        exit;
      end;
    end;

    // load the source
    if UnknownFile then begin
      // open unknown file, Never happens if ofRevert
      Handled:=false;
      Result:=OpenUnknownFile(AFilename,Flags,NewUnitInfo,Handled);
      if (Result<>mrOk) or Handled then exit;
      // the file was previously unknown, use the default EditorInfo
      if AEditorInfo <> nil then
        NewEditorInfo := AEditorInfo
      else
      if NewUnitInfo <> nil then
        NewEditorInfo := NewUnitInfo.GetClosedOrNewEditorInfo
      else
        NewEditorInfo := nil;
    end else begin
      // project knows this file => all the meta data is known
      // -> just load the source
      NewUnitInfo:=Project1.Units[UnitIndex];
      LoadBufferFlags:=[lbfCheckIfText];
      if FilenameIsAbsolute(AFilename) then begin
        if (not (ofUseCache in Flags)) then
          Include(LoadBufferFlags,lbfUpdateFromDisk);
        if ofRevert in Flags then
          Include(LoadBufferFlags,lbfRevert);
      end;
      Result:=LoadCodeBuffer(NewBuf,AFileName,LoadBufferFlags,CanAbort);
      if Result<>mrOk then begin
        DebugLn(['TLazSourceFileManager.OpenEditorFile failed LoadCodeBuffer: ',AFilename]);
        exit;
      end;

      NewUnitInfo.Source:=NewBuf;
      if FilenameIsPascalUnit(NewUnitInfo.Filename) then
        NewUnitInfo.ReadUnitNameFromSource(false);
      NewUnitInfo.Modified:=NewUnitInfo.Source.FileOnDiskNeedsUpdate;
    end;

    // check readonly
    NewUnitInfo.FileReadOnly:=FileExistsUTF8(NewUnitInfo.Filename)
                              and (not FileIsWritable(NewUnitInfo.Filename));
    //writeln('[TLazSourceFileManager.OpenEditorFile] B');
    // open file in source notebook
    Result:=OpenFileInSourceEditor(NewEditorInfo, PageIndex, WindowIndex, Flags, UseWindowID);
    if Result<>mrOk then begin
      DebugLn(['TLazSourceFileManager.OpenEditorFile failed DoOpenFileInSourceEditor: ',AFilename]);
      exit;
    end;
    //writeln('[TLazSourceFileManager.OpenEditorFile] C');
    // open resource component (designer, form, datamodule, ...)
    if NewUnitInfo.OpenEditorInfoCount = 1 then
      Result:=OpenResource;
    if Result<>mrOk then begin
      DebugLn(['TLazSourceFileManager.OpenEditorFile failed OpenResource: ',AFilename]);
      exit;
    end;
  finally
    if Reverting then
      Project1.EndRevertUnit(NewUnitInfo);
  end;

  Result:=mrOk;
  //writeln('TLazSourceFileManager.OpenEditorFile END "',AFilename,'"');
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.OpenEditorFile END');{$ENDIF}
end;

function TLazSourceFileManager.OpenFileAtCursor(ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo): TModalResult;
var
  FileName,SearchPath: String;

  function FindFile(var CurFileName: String; CurSearchPath: String): Boolean;
  //  Searches for FileName in SearchPath
  //  If FileName is not found, we'll check extensions pp and pas too
  //  Returns true if found. FName contains the full file+path in that case
  var TempFile,TempPath,CurPath,FinalFile, Ext: String;
      p,c: Integer;
      PasExt: TPascalExtType;
  begin
    if CurSearchPath='' then CurSearchPath:='.';
    Result:=true;
    TempPath:=CurSearchPath;
    while TempPath<>'' do begin
      p:=pos(';',TempPath);
      if p=0 then p:=length(TempPath)+1;
      CurPath:=copy(TempPath,1,p-1);
      Delete(TempPath,1,p);
      if CurPath='' then continue;
      CurPath:=AppendPathDelim(CurPath);
      if not FilenameIsAbsolute(CurPath) then begin
        if ActiveUnitInfo.IsVirtual then
          CurPath:=AppendPathDelim(Project1.ProjectDirectory)+CurPath
        else
          CurPath:=AppendPathDelim(ExtractFilePath(ActiveUnitInfo.Filename))
                   +CurPath;
      end;
      for c:=0 to 2 do begin
        // FPC searches first lowercase, then keeping case, then uppercase
        case c of
          0: TempFile:=LowerCase(CurFileName);
          1: TempFile:=CurFileName;
          2: TempFile:=UpperCase(CurFileName);
        end;
        if ExtractFileExt(TempFile)='' then begin
          for PasExt:=Low(TPascalExtType) to High(TPascalExtType) do begin
            Ext:=PascalExtension[PasExt];
            FinalFile:=ExpandFileNameUTF8(CurPath+TempFile+Ext);
            if FileExistsUTF8(FinalFile) then begin
              CurFileName:=FinalFile;
              exit;
            end;
          end;
        end else begin
          FinalFile:=ExpandFileNameUTF8(CurPath+TempFile);
          if FileExistsUTF8(FinalFile) then begin
            CurFileName:=FinalFile;
            exit;
          end;
        end;
      end;
    end;
    Result:=false;
  end;

  function CheckIfIncludeDirectiveInFront(const Line: string;
    X: integer): boolean;
  var
    DirectiveEnd, DirectiveStart: integer;
    Directive: string;
  begin
    Result:=false;
    DirectiveEnd:=X;
    while (DirectiveEnd>1) and (Line[DirectiveEnd-1] in [' ',#9]) do
      dec(DirectiveEnd);
    DirectiveStart:=DirectiveEnd-1;
    while (DirectiveStart>0) and (Line[DirectiveStart]<>'$') do
      dec(DirectiveStart);
    Directive:=uppercase(copy(Line,DirectiveStart,DirectiveEnd-DirectiveStart));
    if (Directive='$INCLUDE') or (Directive='$I') then begin
      if ((DirectiveStart>1) and (Line[DirectiveStart-1]='{'))
      or ((DirectiveStart>2)
        and (Line[DirectiveStart-2]='(') and (Line[DirectiveStart-1]='*'))
      then begin
        Result:=true;
      end;
    end;
  end;

  function GetFilenameAtRowCol(XY: TPoint; var IsIncludeDirective: boolean): string;
  var
    Line: string;
    Len, Stop: integer;
    StopChars: set of char;
  begin
    Result := '';
    IsIncludeDirective:=false;
    if (XY.Y >= 1) and (XY.Y <= ActiveSrcEdit.EditorComponent.Lines.Count) then
    begin
      Line := ActiveSrcEdit.EditorComponent.Lines.Strings[XY.Y - 1];
      Len := Length(Line);
      if (XY.X >= 1) and (XY.X <= Len + 1) then begin
        StopChars := [',',';',':','[',']','{','}','(',')','''','"','`'
                     ,'#','%','=','>'];
        Stop := XY.X;
        if Stop>Len then Stop:=Len;
        while (Stop >= 1) and (not (Line[Stop] in ['''','"','`'])) do
          dec(Stop);
        if Stop<1 then
          StopChars:=StopChars+[' ',#9]; // no quotes in front => use spaces as boundaries
        Stop := XY.X;
        while (Stop <= Len) and (not (Line[Stop] in StopChars)) do
          Inc(Stop);
        while (XY.X > 1) and (not (Line[XY.X - 1] in StopChars)) do
          Dec(XY.X);
        if Stop > XY.X then begin
          Result := Copy(Line, XY.X, Stop - XY.X);
          IsIncludeDirective:=CheckIfIncludeDirectiveInFront(Line,XY.X);
        end;
      end;
    end;
  end;

var
  IsIncludeDirective: boolean;
  BaseDir: String;
  NewFilename: string;
  Found: Boolean;
  AUnitName: String;
  InFilename: String;
begin
  Result:=mrCancel;
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) then exit;
  BaseDir:=ExtractFilePath(ActiveUnitInfo.Filename);

  // parse filename at cursor
  IsIncludeDirective:=false;
  Found:=false;
  FileName:=GetFilenameAtRowCol(ActiveSrcEdit.EditorComponent.LogicalCaretXY,
                             IsIncludeDirective);
  if FileName='' then exit;

  // check if absolute filename
  if FilenameIsAbsolute(FileName) then begin
    if FileExistsUTF8(FileName) then
      Found:=true
    else
      exit;
  end;

  if (not Found) then begin
    if IsIncludeDirective then begin
      // search include file
      SearchPath:='.;'+CodeToolBoss.DefineTree.GetIncludePathForDirectory(BaseDir);
      if FindFile(FileName,SearchPath) then
        Found:=true;
    end else if FilenameIsPascalSource(FileName) or (ExtractFileExt(FileName)='') then
    begin
      // search pascal unit
      AUnitName:=ExtractFileNameOnly(FileName);
      InFilename:=FileName;
      if ExtractFileExt(FileName)='' then InFilename:='';
      NewFilename:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
                           BaseDir,AUnitName,InFilename,true);
      if NewFilename<>'' then begin
        Found:=true;
        FileName:=NewFilename;
      end;
    end;
  end;

  if (not Found) then begin
    // simple search relative to current unit
    InFilename:=AppendPathDelim(BaseDir)+FileName;
    if FileExistsCached(InFilename) then begin
      Found:=true;
      FileName:=InFilename;
    end;
  end;

  if (not Found) and (System.Pos('.',FileName)>0) and (not IsIncludeDirective) then
  begin
    // for example 'SysUtils.CompareText'
    FileName:=ActiveSrcEdit.EditorComponent.GetWordAtRowCol(
      ActiveSrcEdit.EditorComponent.LogicalCaretXY);
    if (FileName<>'') and IsValidIdent(FileName) then begin
      // search pascal unit
      AUnitName:=FileName;
      InFilename:='';
      NewFilename:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
                           BaseDir,AUnitName,InFilename,true);
      if NewFilename<>'' then begin
        Found:=true;
        FileName:=NewFilename;
      end;
    end;
  end;

  if Found then begin
    // open
    InputHistories.SetFileDialogSettingsInitialDir(ExtractFilePath(FileName));
    Result:=OpenEditorFile(FileName, -1, -1, nil, [ofAddToRecent]);
  end;
end;

function TLazSourceFileManager.InitNewProject(ProjectDesc: TProjectDescriptor): TModalResult;
var
  i:integer;
  HandlerResult: TModalResult;
begin
  try
    Project1.BeginUpdate(true);
    try
      Project1.CompilerOptions.CompilerPath:='$(CompPath)';
      if pfUseDefaultCompilerOptions in Project1.Flags then begin
        MainIDE.DoMergeDefaultProjectOptions(Project1);
        Project1.Flags:=Project1.Flags-[pfUseDefaultCompilerOptions];
      end;
      Project1.AutoAddOutputDirToIncPath;
      MainIDE.UpdateCaption;
      if ProjInspector<>nil then ProjInspector.LazProject:=Project1;
      // add and load default required packages
      PkgBoss.OpenProjectDependencies(Project1,true);
      // rebuild codetools defines
      MainBuildBoss.SetBuildTargetProject1(false);
      // (i.e. remove old project specific things and create new)
      IncreaseCompilerParseStamp;
      Project1.DefineTemplates.AllChanged;
      Project1.DefineTemplates.Active:=true;
      DebugBoss.Reset;
    finally
      Project1.EndUpdate;
    end;
    Project1.BeginUpdate(true);
    try
      // create files
      if ProjectDesc.CreateStartFiles(Project1)<>mrOk then begin
        debugln('TLazSourceFileManager.NewProject ProjectDesc.CreateStartFiles failed');
      end;
      if (Project1.MainUnitInfo<>nil)
      and ((Project1.FirstUnitWithEditorIndex=nil)
       or ([pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement]*Project1.Flags=[]))
      then begin
        // the project has not created any secondary files
        // or the project main source is not auto updated by the IDE
        OpenMainUnit(-1,-1,[]);
      end;

      // init resource files
      if not Project1.ProjResources.Regenerate(Project1.MainFilename, True, False,'') then
        DebugLn('TLazSourceFileManager.NewProject Project1.Resources.Regenerate failed');
    finally
      Project1.EndUpdate;
    end;
    Result:=mrOk;
  finally
    // set all modified to false
    Project1.UpdateAllVisibleUnits;
    for i:=0 to Project1.UnitCount-1 do
      Project1.Units[i].ClearModifieds;
    Project1.Modified:=false;
    // call handlers
    HandlerResult:=MainIDE.DoCallProjectChangedHandler(lihtProjectOpened, Project1);
    if not (HandlerResult in [mrOk,mrCancel,mrAbort]) then
      HandlerResult:=mrCancel;
    if (Result=mrOk) then
      Result:=HandlerResult;
  end;
end;

function TLazSourceFileManager.InitOpenedProjectFile(AFileName: string;
  Flags: TOpenFlags): TModalResult;
var
  EditorInfoIndex, i, j: Integer;
  NewBuf: TCodeBuffer;
  LastDesigner: TDesigner;
  AnUnitInfo: TUnitInfo;
  HandlerResult: TModalResult;
  AnEditorInfo: TUnitEditorInfo;
begin
  EditorInfoIndex := 0;
  SourceEditorManager.IncUpdateLock;
  try
    Project1.BeginUpdate(true);
    try
      if ProjInspector<>nil then ProjInspector.LazProject:=Project1;

      // read project info file
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.OpenProjectFile B3');{$ENDIF}
      Project1.ReadProject(AFilename,EnvironmentOptions.BuildMatrixOptions);
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.OpenProjectFile B4');{$ENDIF}
      Result:=CompleteLoadingProjectInfo;
    finally
      Project1.EndUpdate;
    end;
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.OpenProjectFile B5');{$ENDIF}
    if Result<>mrOk then exit;

    if Project1.MainUnitID>=0 then begin
      // read MainUnit Source
      Result:=LoadCodeBuffer(NewBuf,Project1.MainFilename,
                             [lbfUpdateFromDisk,lbfRevert],false);// do not check if source is text
      case Result of
      mrOk: Project1.MainUnitInfo.Source:=NewBuf;
      mrIgnore: Project1.MainUnitInfo.Source:=CodeToolBoss.CreateFile(Project1.MainFilename);
      else exit(mrCancel);
      end;
    end;
    //debugln('TLazSourceFileManager.OpenProjectFile C');
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.OpenProjectFile C');{$ENDIF}
    IncreaseCompilerParseStamp;

    // restore files
    while EditorInfoIndex < Project1.AllEditorsInfoCount do begin
      // TProject.ReadProject sorts alle UnitEditorInfos
      AnEditorInfo := Project1.AllEditorsInfo[EditorInfoIndex];
      AnUnitInfo := AnEditorInfo.UnitInfo;
      if (not AnUnitInfo.Loaded) or (AnEditorInfo.PageIndex < 0) then begin
        inc(EditorInfoIndex);
        Continue;
      end;

      // reopen file
      if (not AnUnitInfo.IsPartOfProject)
      and (not FileExistsCached(AnUnitInfo.Filename)) then begin
        // this file does not exist, but is not important => silently ignore
      end
      else begin
        // reopen file
        // This will adjust Page/WindowIndex if they are not continous
        Result:=OpenEditorFile(AnUnitInfo.Filename, -1, AnEditorInfo.WindowID,
                      AnEditorInfo, [ofProjectLoading,ofMultiOpen,ofOnlyIfExists], True);
        if Result=mrAbort then
          exit;
      end;
      if not ((AnUnitInfo.Filename<>'') and (AnEditorInfo.EditorComponent <> nil))
      then begin
        // failed to open
        AnEditorInfo.PageIndex := -1;
        // if failed entirely -> mark as unloaded, so that next time it will not be tried again
        if AnUnitInfo.OpenEditorInfoCount = 0 then
          AnUnitInfo.Loaded := False;
      end;
      inc(EditorInfoIndex);
    end; // while EditorInfoIndex < Project1.AllEditorsInfoCount
    Result:=mrCancel;
    //debugln('TLazSourceFileManager.OpenProjectFile D');

    // set active editor source editor
    for i := 0 to Project1.AllEditorsInfoCount - 1 do begin
      AnEditorInfo := Project1.AllEditorsInfo[i];
      if AnEditorInfo.IsVisibleTab then
      begin
        if (AnEditorInfo.WindowID < 0) then continue;
        j := SourceEditorManager.IndexOfSourceWindowWithID(AnEditorInfo.WindowID);
        if j < 0
        then begin
          // session info is invalid (buggy lps file?) => auto fix
          AnEditorInfo.IsVisibleTab:=false;
          AnEditorInfo.WindowID:=-1;
          Continue;
        end;
        if (SourceEditorManager.SourceWindows[j] <> nil)
        then
          SourceEditorManager.SourceWindows[j].PageIndex :=
            AnEditorInfo.PageIndex;
      end;
    end;
    if (Project1.ActiveWindowIndexAtStart<0)
    or (Project1.ActiveWindowIndexAtStart >= SourceEditorManager.SourceWindowCount)
    then begin
      // session info is invalid (buggy lps file?) => auto fix
      Project1.ActiveWindowIndexAtStart := 0;
    end;
    if (Project1.ActiveWindowIndexAtStart >= 0) and
       (Project1.ActiveWindowIndexAtStart < SourceEditorManager.SourceWindowCount)
    then begin
      SourceEditorManager.ActiveSourceWindow :=
        SourceEditorManager.SourceWindows[Project1.ActiveWindowIndexAtStart];
      SourceEditorManager.ShowActiveWindowOnTop(True);
    end;

    if ([ofDoNotLoadResource]*Flags=[])
    and ( (not Project1.AutoOpenDesignerFormsDisabled)
           and EnvironmentOptions.AutoCreateFormsOnOpen
           and (SourceEditorManager.ActiveEditor<>nil) )
    then begin
      // auto open form of active unit
      AnUnitInfo:=Project1.UnitWithEditorComponent(SourceEditorManager.ActiveEditor);
      if AnUnitInfo<>nil then
        Result:=LoadLFM(AnUnitInfo,[ofProjectLoading,ofMultiOpen,ofOnlyIfExists],
                          [cfSaveDependencies]);
    end;

    // select a form (object inspector, formeditor, control selection)
    if MainIDE.LastFormActivated<>nil then begin
      LastDesigner:=TDesigner(MainIDE.LastFormActivated.Designer);
      debugln(['TLazSourceFileManager.OpenProjectFile select form in designer: ',
               DbgSName(MainIDE.LastFormActivated),' ',DbgSName(MainIDE.LastFormActivated.Designer)]);
      LastDesigner.SelectOnlyThisComponent(LastDesigner.LookupRoot);
    end;

    // set all modified to false
    Project1.UpdateAllVisibleUnits;
    Project1.ClearModifieds(true);

    IncreaseCompilerParseStamp;
    IDEProtocolOpts.LastProjectLoadingCrashed := False;
    Result:=mrOk;
  finally
    SourceEditorManager.DecUpdateLock;
    if (Result<>mrOk) and (Project1<>nil) then begin
      // mark all files, that are left to open as unloaded:
      for i := EditorInfoIndex to Project1.AllEditorsInfoCount - 1 do begin
        AnEditorInfo := Project1.AllEditorsInfo[i];
        AnEditorInfo.PageIndex := -1;
        AnUnitInfo := AnEditorInfo.UnitInfo;
        if AnUnitInfo.Loaded and (AnUnitInfo.OpenEditorInfoCount = 0) then
          AnUnitInfo.Loaded := false;
      end;
    end;
    // call handlers
    HandlerResult:=MainIDE.DoCallProjectChangedHandler(lihtProjectOpened, Project1);
    if not (HandlerResult in [mrOk,mrCancel,mrAbort]) then
      HandlerResult:=mrCancel;
    if (Result=mrOk) then
      Result:=HandlerResult;
  end;
  if Result=mrAbort then exit;
  //debugln('TLazSourceFileManager.OpenProjectFile end  CodeToolBoss.ConsistencyCheck=',IntToStr(CodeToolBoss.ConsistencyCheck));
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.OpenProjectFile end');{$ENDIF}
end;

procedure TLazSourceFileManager.NewProjectFromFile;
var
  OpenDialog:TOpenDialog;
  AFilename: string;
  PreReadBuf: TCodeBuffer;
  Filter: String;
Begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisChooseProgramSourcePpPasLpr;
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist,ofFileMustExist];
    Filter := lisLazarusUnit + ' (*.pas;*.pp;*.p)|*.pas;*.pp;*.p'
      + '|' + lisLazarusProjectSource + ' (*.lpr)|*.lpr';
    Filter:=Filter+ '|' + dlgAllFiles + ' (' + GetAllFilesMask + ')|' + GetAllFilesMask;
    OpenDialog.Filter := Filter;
    if OpenDialog.Execute then begin
      AFilename:=ExpandFileNameUTF8(OpenDialog.Filename);
      if not FilenameIsPascalSource(AFilename) then begin
        IDEMessageDialog(lisPkgMangInvalidFileExtension,
          lisProgramSourceMustHaveAPascalExtensionLikePasPpOrLp,
          mtError,[mbOk],'');
        exit;
      end;
      if mrOk<>LoadCodeBuffer(PreReadBuf,AFileName,
                              [lbfCheckIfText,lbfUpdateFromDisk,lbfRevert],false)
      then
        exit;
      if CreateProjectForProgram(PreReadBuf)=mrOk then begin
        exit;
      end;
    end;
  finally
    InputHistories.StoreFileDialogSettings(OpenDialog);
    OpenDialog.Free;
  end;
end;

function TLazSourceFileManager.CreateProjectForProgram(ProgramBuf: TCodeBuffer): TModalResult;
var
  NewProjectDesc: TProjectDescriptor;
begin
  //writeln('[TLazSourceFileManager.DoCreateProjectForProgram] A ',ProgramBuf.Filename);
  if (Project1 <> nil)
  and (not MainIDE.DoResetToolStatus([rfInteractive, rfSuccessOnTrigger])) then exit;

  Result:=SaveProjectIfChanged;
  if Result=mrAbort then exit;

  // let user choose the program type
  NewProjectDesc:=nil;
  if ChooseNewProject(NewProjectDesc)<>mrOk then exit;

  // close old project
  If Project1<>nil then begin
    if CloseProject=mrAbort then begin
      Result:=mrAbort;
      exit;
    end;
  end;

  // reload file (if the file was open in the IDE, closeproject unloaded it)
  ProgramBuf.Reload;

  // switch codetools to new project directory
  CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'ProjPath']:=
    ExpandFileNameUTF8(ExtractFilePath(ProgramBuf.Filename));

  // create a new project
  Project1:=MainIDE.CreateProjectObject(NewProjectDesc,ProjectDescriptorProgram);
  Result:=InitProjectForProgram(ProgramBuf);
  //writeln('[TLazSourceFileManager.DoCreateProjectForProgram] END');
end;

function TLazSourceFileManager.InitProjectForProgram(ProgramBuf: TCodeBuffer): TModalResult;
var
  MainUnitInfo: TUnitInfo;
begin
  Project1.BeginUpdate(true);
  try
    if ProjInspector<>nil then ProjInspector.LazProject:=Project1;
    MainUnitInfo:=Project1.MainUnitInfo;
    MainUnitInfo.Source:=ProgramBuf;
    Project1.ProjectInfoFile:=ChangeFileExt(ProgramBuf.Filename,'.lpi');
    Project1.CompilerOptions.TargetFilename:=ExtractFileNameOnly(ProgramBuf.Filename);
    MainIDE.DoMergeDefaultProjectOptions(Project1);
    MainIDE.UpdateCaption;
    IncreaseCompilerParseStamp;
    // add and load default required packages
    PkgBoss.OpenProjectDependencies(Project1,true);
    Result:=CompleteLoadingProjectInfo;
    if Result<>mrOk then exit;
  finally
    Project1.EndUpdate;
  end;
  // show program unit
  Result:=OpenEditorFile(ProgramBuf.Filename,-1,-1, nil, [ofAddToRecent,ofRegularFile]);
  if Result=mrAbort then exit;
  Result:=mrOk;
end;

function TLazSourceFileManager.SaveProject(Flags: TSaveFlags):TModalResult;
var
  i: integer;
  AnUnitInfo: TUnitInfo;
  SaveFileFlags: TSaveFlags;
  SrcEdit: TSourceEditor;
  j: Integer;
begin
  Result:=mrCancel;
  if not (MainIDE.ToolStatus in [itNone,itDebugger]) then begin
    Result:=mrAbort;
    exit;
  end;
  SaveSourceEditorChangesToCodeCache(nil);
  //DebugLn('TLazSourceFileManager.SaveProject A SaveAs=',dbgs(sfSaveAs in Flags),' SaveToTestDir=',dbgs(sfSaveToTestDir in Flags),' ProjectInfoFile=',Project1.ProjectInfoFile);
  Result:=MainIDE.DoCheckFilesOnDisk(true);
  if Result in [mrCancel,mrAbort] then exit;

  if CheckMainSrcLCLInterfaces(sfQuietUnitCheck in Flags)<>mrOk then
    exit(mrCancel);

  // if this is a virtual project then save first the project info file
  // to get a project directory
  if Project1.IsVirtual and ([sfSaveToTestDir,sfDoNotSaveVirtualFiles]*Flags=[])
  then begin
    Result:=SaveProjectInfo(Flags);
    if Result in [mrCancel,mrAbort] then exit;
  end;

  if (not (sfDoNotSaveVirtualFiles in Flags)) then
  begin
    // check that all new units are saved first to get valid filenames
    // (this can alter the mainunit: e.g. used unit names)
    for i:=0 to Project1.UnitCount-1 do begin
      AnUnitInfo:=Project1.Units[i];
      if (AnUnitInfo.Loaded) and AnUnitInfo.IsVirtual
      and AnUnitInfo.IsPartOfProject
      and (Project1.MainUnitID<>i)
      and (AnUnitInfo.OpenEditorInfoCount > 0) then begin
        SaveFileFlags:=[sfSaveAs,sfProjectSaving]+[sfCheckAmbiguousFiles]*Flags;
        if sfSaveToTestDir in Flags then begin
          if AnUnitInfo.IsPartOfProject or AnUnitInfo.IsVirtual then
            Include(SaveFileFlags,sfSaveToTestDir);
        end;
        Result:=SaveEditorFile(AnUnitInfo.OpenEditorInfo[0].EditorComponent, SaveFileFlags);
        if Result in [mrCancel,mrAbort] then exit;
      end;
    end;
  end;

  Result:=SaveProjectInfo(Flags);
  if Result in [mrCancel,mrAbort] then exit;

  // save all editor files
  for i:=0 to SourceEditorManager.SourceEditorCount-1 do begin
    SrcEdit:=SourceEditorManager.SourceEditors[i];
    AnUnitInfo:=Project1.UnitWithEditorComponent(SrcEdit);
    if (Project1.MainUnitID>=0) and (Project1.MainUnitInfo = AnUnitInfo) then
      continue;
    SaveFileFlags:=[sfProjectSaving]+Flags*[sfCheckAmbiguousFiles];
    if AnUnitInfo = nil
    then begin
      // inconsistency detected, write debug info
      DebugLn(['TLazSourceFileManager.SaveProject - unit not found for page ',i,' File="',SrcEdit.FileName,'" SrcEdit=',dbgsname(SrcEdit),'=',dbgs(Pointer(SrcEdit))]);
      DumpStack;
      debugln(['TLazSourceFileManager.SaveProject Project1 has the following information about the source editor:']);
      AnUnitInfo:=Project1.FirstUnitWithEditorIndex;
      while AnUnitInfo<>nil do begin
        for j:=0 to AnUnitInfo.EditorInfoCount-1 do begin
          dbgout(['  ',AnUnitInfo.Filename,' ',j,'/',AnUnitInfo.EditorInfoCount,' Component=',dbgsname(AnUnitInfo.EditorInfo[j].EditorComponent),'=',dbgs(Pointer(AnUnitInfo.EditorInfo[j].EditorComponent))]);
          if AnUnitInfo.EditorInfo[j].EditorComponent<>nil then
            dbgout(AnUnitInfo.EditorInfo[j].EditorComponent.FileName);
          debugln;
        end;
        debugln(['  ',AnUnitInfo.EditorInfoCount]);
        AnUnitInfo:=AnUnitInfo.NextUnitWithEditorIndex;
      end;
    end else begin
      if AnUnitInfo.IsVirtual
      then begin
        if (sfSaveToTestDir in Flags) then
          Include(SaveFileFlags,sfSaveToTestDir)
        else
          continue;
      end;
    end;
    Result:=SaveEditorFile(SourceEditorManager.SourceEditors[i], SaveFileFlags);
    if Result=mrAbort then exit;
    // mrCancel: continue saving other files
  end;

  // update all lrs files
  if sfSaveToTestDir in Flags then
    MainBuildBoss.UpdateProjectAutomaticFiles(EnvironmentOptions.GetParsedTestBuildDirectory)
  else
    MainBuildBoss.UpdateProjectAutomaticFiles('');

  // everything went well => clear all modified flags
  Project1.ClearModifieds(true);
  // update menu and buttons state
  MainIDE.UpdateSaveMenuItemsAndButtons(true);
  //DebugLn('TLazSourceFileManager.SaveProject End');
  Result:=mrOk;
end;

function TLazSourceFileManager.SaveProjectIfChanged: TModalResult;
begin
  if SomethingOfProjectIsModified then begin
    if IDEMessageDialog(lisProjectChanged, Format(lisSaveChangesToProject,
      [Project1.GetTitleOrName]),
      mtconfirmation, [mbYes, mbNo, mbCancel])=mrYes then
    begin
      if SaveProject([])=mrAbort then begin
        Result:=mrAbort;
        exit;
      end;
    end;
  end;
  Result:=mrOk;
end;

function TLazSourceFileManager.CloseProject: TModalResult;
begin
  //writeln('TLazSourceFileManager.CloseProject A');
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.CloseProject A');{$ENDIF}
  Result:=DebugBoss.DoStopProject;
  if Result<>mrOk then begin
    debugln('TLazSourceFileManager.CloseProject DebugBoss.DoStopProject failed');
    exit;
  end;

  // call handlers
  Result:=MainIDE.DoCallProjectChangedHandler(lihtProjectClose, Project1);
  if Result=mrAbort then exit;

    // close all loaded files
  SourceEditorManager.IncUpdateLock;
  try
    while SourceEditorManager.SourceEditorCount > 0 do begin
      Result:=CloseEditorFile(
        SourceEditorManager.SourceEditors[SourceEditorManager.SourceEditorCount-1],
        [cfProjectClosing]);
      if Result=mrAbort then exit;
    end;
  finally
    SourceEditorManager.DecUpdateLock;
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.CloseProject B');{$ENDIF}
  IncreaseCompilerParseStamp;

  // close Project
  if ProjInspector<>nil then
    ProjInspector.LazProject:=nil;
  FreeThenNil(Project1);
  if IDEMessagesWindow<>nil then IDEMessagesWindow.Clear;

  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TLazSourceFileManager.CloseProject C');{$ENDIF}
  Result:=mrOk;
  //writeln('TLazSourceFileManager.CloseProject end ',CodeToolBoss.ConsistencyCheck);
end;

procedure TLazSourceFileManager.OpenProject(aMenuItem: TIDEMenuItem);
var
  OpenDialog:TOpenDialog;
  AFileName: string;
  LoadFlags: TLoadBufferFlags;
  PreReadBuf: TCodeBuffer;
  SourceType: String;
  LPIFilename: String;
begin
  if Assigned(aMenuItem) and (aMenuItem.Section=itmProjectRecentOpen) then begin
    AFileName:=ExpandFileNameUTF8(aMenuItem.Caption);
    if MainIDE.DoOpenProjectFile(AFilename,[ofAddToRecent])=mrOk then begin
      AddRecentProjectFileToEnvironment(AFilename);
    end else begin
      // open failed
      if not FileExistsUTF8(AFilename) then begin
        EnvironmentOptions.RemoveFromRecentProjectFiles(AFilename);
      end else
        AddRecentProjectFileToEnvironment(AFilename);
    end;
  end
  else begin
    OpenDialog:=TOpenDialog.Create(nil);
    try
      InputHistories.ApplyFileDialogSettings(OpenDialog);
      OpenDialog.Title:=lisOpenProjectFile+' (*.lpi)';
      OpenDialog.Filter := lisLazarusProjectInfoFile+' (*.lpi)|*.lpi|'
                          +lisAllFiles+'|'+GetAllFilesMask;
      if OpenDialog.Execute then begin
        AFilename:=ExpandFileNameUTF8(OpenDialog.Filename);
        if FileUtil.CompareFileExt(AFilename,'.lpi')<>0 then begin
          // not a lpi file
          // check if it is a program source

          // load the source
          LoadFlags := [lbfCheckIfText,lbfUpdateFromDisk,lbfRevert];
          if LoadCodeBuffer(PreReadBuf,AFileName,LoadFlags,true)<>mrOk then exit;

          // check if unit is a program
          SourceType:=CodeToolBoss.GetSourceType(PreReadBuf,false);
          if (SysUtils.CompareText(SourceType,'PROGRAM')=0)
          or (SysUtils.CompareText(SourceType,'LIBRARY')=0)
          then begin
            // source is a program
            // either this is a lazarus project
            // or it is not yet a lazarus project ;)
            LPIFilename:=ChangeFileExt(AFilename,'.lpi');
            if FileExistsUTF8(LPIFilename) then begin
              if IDEQuestionDialog(lisProjectInfoFileDetected,
                  Format(lisTheFileSeemsToBeTheProgramFileOfAnExistingLazarusP, [
                  AFilename]), mtConfirmation,
                  [mrOk, lisOpenProject2, mrCancel])
                <>mrOk
              then
                exit;
              AFilename:=LPIFilename;
            end else begin
              if IDEQuestionDialog(lisFileHasNoProject,
                Format(lisTheFileIsNotALazarusProjectCreateANewProjectForThi, [
                  '"', AFilename, '"', LineEnding, '"'+lowercase(SourceType)+'"']),
                mtConfirmation, [mrYes, lisCreateProject, mrCancel])<>mrYes
              then
                exit;
              CreateProjectForProgram(PreReadBuf);
              exit;
            end;
          end;
        end;
        MainIDE.DoOpenProjectFile(AFilename,[ofAddToRecent]);
      end;
      InputHistories.StoreFileDialogSettings(OpenDialog);
    finally
      OpenDialog.Free;
    end;
  end;
end;

procedure TLazSourceFileManager.CloseAll;
var
  i, NeedSave, Idx: Integer;
  r: TModalResult;
  Ed: TSourceEditor;
begin
  // Close editor files
  NeedSave := 0;
  for i := 0 to SourceEditorManager.UniqueSourceEditorCount - 1 do begin
    if CheckEditorNeedsSave(SourceEditorManager.UniqueSourceEditors[i], False) then begin
      inc(NeedSave);
      if NeedSave = 1 then Idx := i;
    end;
  end;
  if NeedSave = 1 then begin
    Ed := TSourceEditor(SourceEditorManager.UniqueSourceEditors[Idx]);
    r := IDEQuestionDialog(lisSourceModified,
                     Format(lisSourceOfPageHasChangedSave, ['"', Ed.PageName, '"']),
                     mtConfirmation,
                     [mrYes, lisMenuSave, mrNo, lisDiscardChanges, mrAbort]);
    case r of
      mrYes: SaveEditorFile(Ed, [sfCheckAmbiguousFiles]);
      mrNo: ; // don't save
      mrAbort: exit;
    end;
  end
  else if NeedSave > 1 then begin
    for i := 0 to SourceEditorManager.UniqueSourceEditorCount - 1 do begin
      if CheckEditorNeedsSave(SourceEditorManager.UniqueSourceEditors[i], False) then begin
        dec(NeedSave);
        Ed := TSourceEditor(SourceEditorManager.UniqueSourceEditors[i]);
        r := IDEQuestionDialog(lisSourceModified,
                         Format(lisSourceOfPageHasChangedSaveExtended, ['"', Ed.PageName, '"', NeedSave]),
                         mtConfirmation,
                         [mrYes, lisMenuSave, mrAll, lisSaveAll,
                          mrNo, lisDiscardChanges, mrIgnore, lisDiscardChangesAll,
                          mrAbort]);
        case r of
          mrYes: SaveEditorFile(Ed, [sfCheckAmbiguousFiles]);
          mrNo: ; // don't save
          mrAll: begin
              MainIDE.DoSaveAll([]);
              break
            end;
          mrIgnore: break; // don't save anymore
          mrAbort: exit;
        end;
      end;
    end;
  end;
  SourceEditorManager.IncUpdateLock;
  try
    while (SourceEditorManager.SourceEditorCount > 0) and
      (CloseEditorFile(SourceEditorManager.SourceEditors[0], []) = mrOk)
    do ;
  finally
    SourceEditorManager.DecUpdateLock;
  end;

  // Close packages
  PkgBoss.DoCloseAllPackageEditors;
end;

function TLazSourceFileManager.CheckDirIsInSearchPath(UnitInfo: TUnitInfo;
  AllowAddingDependencies, IsIncludeFile: Boolean): Boolean;
// Check if the given unit's path is on Unit- or Include-search path.
// Returns true if it is OK to add the unit to current project.
var
  CurDirectory, CurPath, ShortDir: String;
  DlgMsg: String;
  Owners: TFPList;
  APackage: TLazPackage;
  ListForm: TGenericCheckListForm;
  i: Integer;
begin
  Result:=True;
  if UnitInfo.IsVirtual then exit;
  if IsIncludeFile then begin
    CurPath:=Project1.CompilerOptions.GetIncludePath(false);
    DlgMsg:=lisTheNewIncludeFileIsNotYetInTheIncludeSearchPathAdd;
  end
  else begin
    CurPath:=Project1.CompilerOptions.GetUnitPath(false);
    DlgMsg:=lisTheNewUnitIsNotYetInTheUnitSearchPathAddDirectory;
  end;
  CurDirectory:=AppendPathDelim(UnitInfo.GetDirectory);
  if SearchDirectoryInSearchPath(CurPath,CurDirectory)<1 then
  begin
    if AllowAddingDependencies then begin
      Owners:=PkgBoss.GetPossibleOwnersOfUnit(UnitInfo.Filename,[]);
      try
        if (Owners<>nil) then begin
          for i:=0 to Owners.Count-1 do begin
            if TObject(Owners[i]) is TLazPackage then begin
              APackage:=TLazPackage(Owners[i]);
              if IDEMessageDialog(lisAddPackageRequirement,
                Format(lisAddPackageToProject, [APackage.IDAsString]),
                mtConfirmation,[mbYes,mbCancel],'')<>mrYes
              then
                Exit(True);
              PkgBoss.AddProjectDependency(Project1,APackage);
              Exit(False);
            end;
          end;
        end;
      finally
        Owners.Free;
      end;
    end;
    // unit is not in a package => extend unit path
    ShortDir:=CurDirectory;
    if (not Project1.IsVirtual) then
      ShortDir:=CreateRelativePath(ShortDir,Project1.ProjectDirectory);
    ListForm:=TGenericCheckListForm.Create(Nil);
    try
      //lisApplyForBuildModes = 'Apply for build modes:';
      ListForm.Caption:=lisAvailableProjectBuildModes;
      ListForm.InfoLabel.Caption:=Format(DlgMsg,[LineEnding,CurDirectory]);
      for i:=0 to Project1.BuildModes.Count-1 do begin
        ListForm.CheckListBox1.Items.Add(Project1.BuildModes[i].Identifier);
        ListForm.CheckListBox1.Checked[i]:=True;
      end;
      if ListForm.ShowModal<>mrOK then Exit(False);
      for i:=0 to Project1.BuildModes.Count-1 do
        if ListForm.CheckListBox1.Checked[i] then
          with Project1.BuildModes[i].CompilerOptions do
            if IsIncludeFile then
              IncludePath:=MergeSearchPaths(IncludePath,ShortDir)
            else
              OtherUnitFiles:=MergeSearchPaths(OtherUnitFiles,ShortDir);
    finally
      ListForm.Free;
    end;
  end;
end;

function TLazSourceFileManager.CreateNewCodeBuffer(Descriptor: TProjectFileDescriptor;
  NewOwner: TObject; NewFilename: string;
  var NewCodeBuffer: TCodeBuffer; var NewUnitName: string): TModalResult;
var
  NewShortFilename: String;
  NewFileExt: String;
  SearchFlags: TSearchIDEFileFlags;
begin
  //debugln('TLazSourceFileManager.CreateNewCodeBuffer START NewFilename=',NewFilename,' ',Descriptor.DefaultFilename,' ',Descriptor.ClassName);
  NewUnitName:='';
  NewCodeBuffer:=nil;
  if NewFilename='' then begin
    // create a new unique filename
    SearchFlags:=[siffCheckAllProjects];
    if Descriptor.IsPascalUnit then begin
      if NewUnitName='' then
        NewUnitName:=Descriptor.DefaultSourceName;
      NewShortFilename:=lowercase(NewUnitName);
      NewFileExt:=Descriptor.DefaultFileExt;
      SearchFlags:=SearchFlags+[siffIgnoreExtension];
    end else begin
      NewFilename:=ExtractFilename(Descriptor.DefaultFilename);
      NewShortFilename:=ExtractFilenameOnly(NewFilename);
      NewFileExt:=ExtractFileExt(NewFilename);
      SearchFlags:=[];
    end;
    NewFilename:=MainIDE.CreateNewUniqueFilename(NewShortFilename,
                                           NewFileExt,NewOwner,SearchFlags,true);
    if NewFilename='' then
      RaiseException('');
    NewShortFilename:=ExtractFilenameOnly(NewFilename);
    // use as unitname the NewShortFilename, but with the case of the
    // original unitname. e.g. 'unit12.pas' becomes 'Unit12.pas'
    if Descriptor.IsPascalUnit then begin
      NewUnitName:=ChompEndNumber(NewUnitName);
      NewUnitName:=NewUnitName+copy(NewShortFilename,length(NewUnitName)+1,
                                    length(NewShortFilename));
    end;
  end;
  //debugln('TLazSourceFileManager.CreateNewCodeBuffer NewFilename=',NewFilename,' NewUnitName=',NewUnitName);

  if FilenameIsPascalUnit(NewFilename) then begin
    if NewUnitName='' then
      NewUnitName:=ExtractFileNameOnly(NewFilename);
    if EnvironmentOptions.CharcaseFileAction in [ccfaAsk, ccfaAutoRename] then
      NewFilename:=ExtractFilePath(NewFilename)
                   +lowercase(ExtractFileName(NewFilename));
  end;

  NewCodeBuffer:=CodeToolBoss.CreateFile(NewFilename);
  if NewCodeBuffer=nil then
    exit(mrCancel);

  Result:=mrOk;
end;

function TLazSourceFileManager.CreateNewForm(NewUnitInfo: TUnitInfo;
  AncestorType: TPersistentClass; ResourceCode: TCodeBuffer;
  UseCreateFormStatements, DisableAutoSize: Boolean): TModalResult;
var
  NewComponent: TComponent;
  new_x, new_y: integer;
  p: TPoint;
  r: TRect;
begin
  if not AncestorType.InheritsFrom(TComponent) then
    RaiseException('TLazSourceFileManager.CreateNewForm invalid AncestorType');

  //debugln('TLazSourceFileManager.CreateNewForm START ',NewUnitInfo.Filename,' ',AncestorType.ClassName,' ',dbgs(ResourceCode<>nil));
  // create a buffer for the new resource file and for the LFM file
  if ResourceCode=nil then begin
    ResourceCode:=
      CodeToolBoss.CreateFile(ChangeFileExt(NewUnitInfo.Filename,
                              ResourceFileExt));
  end;
  //debugln('TLazSourceFileManager.CreateNewForm B ',ResourceCode.Filename);
  ResourceCode.Source:='{ '+LRSComment+' }';
  CodeToolBoss.CreateFile(ChangeFileExt(NewUnitInfo.Filename,'.lfm'));

  // clear formeditor
  FormEditor1.ClearSelection;

  // Figure out where we want to put the new form
  // if there is more place left of the OI put it left, otherwise right
  p:=Point(0,0);
  if ObjectInspector1<>nil then begin
    p:=ObjectInspector1.ClientOrigin;
    new_x:=p.x;
    new_y:=p.Y+10;
  end else begin
    new_x:=200;
    new_y:=100;
  end;
  if new_x>Screen.Width div 2 then
    new_x:=new_x-500
  else if ObjectInspector1<>nil then
    new_x:=new_x+ObjectInspector1.Width;
  r:=Screen.PrimaryMonitor.WorkareaRect;
  new_x:=Max(r.Left,Min(new_x,r.Right-400));
  new_y:=Max(r.Top,Min(new_y,r.Bottom-400));

  // create jit component
  NewComponent := FormEditor1.CreateComponent(nil,TComponentClass(AncestorType),
      NewUnitInfo.CreateUnitName, new_x, new_y, 0,0,DisableAutoSize);
  if NewComponent=nil then begin
    DebugLn(['TLazSourceFileManager.CreateNewForm FormEditor1.CreateComponent failed ',dbgsName(TComponentClass(AncestorType))]);
    exit(mrCancel);
  end;
  FormEditor1.SetComponentNameAndClass(NewComponent,
    NewUnitInfo.ComponentName,'T'+NewUnitInfo.ComponentName);
  if NewComponent is TCustomForm then
    TControl(NewComponent).Visible := False;
  if (NewComponent is TControl)
  and (csSetCaption in TControl(NewComponent).ControlStyle) then
    TControl(NewComponent).Caption:=NewComponent.Name;
  NewUnitInfo.Component := NewComponent;
  MainIDE.CreateDesignerForComponent(NewUnitInfo,NewComponent);

  NewUnitInfo.ComponentName:=NewComponent.Name;
  NewUnitInfo.ComponentResourceName:=NewUnitInfo.ComponentName;
  if UseCreateFormStatements and
     NewUnitInfo.IsPartOfProject and
     Project1.AutoCreateForms and
     (pfMainUnitHasCreateFormStatements in Project1.Flags) then
  begin
    Project1.AddCreateFormToProjectFile(NewComponent.ClassName,
                                        NewComponent.Name);
  end;

  Result:=mrOk;
end;

function TLazSourceFileManager.NewUniqueComponentName(Prefix: string): string;

  function SearchProject(AProject: TProject; const Identifier: string): boolean;
  var
    i: Integer;
    AnUnitInfo: TUnitInfo;
  begin
    if AProject=nil then exit(false);
    Result:=true;
    for i:=0 to AProject.UnitCount-1 do
    begin
      AnUnitInfo:=AProject.Units[i];
      if (AnUnitInfo.Component<>nil) then begin
        if CompareText(AnUnitInfo.Component.Name,Identifier)=0 then exit;
        if CompareText(AnUnitInfo.Component.ClassName,Identifier)=0 then exit;
      end else if (AnUnitInfo.ComponentName<>'')
      and ((AnUnitInfo.IsPartOfProject) or AnUnitInfo.Loaded) then begin
        if SysUtils.CompareText(AnUnitInfo.Unit_Name,Identifier)=0 then exit;
        if SysUtils.CompareText(AnUnitInfo.ComponentName,Identifier)=0 then exit;
      end;
    end;
    Result:=false;
  end;

  function SearchPackage(APackage: TLazPackage; const Identifier: string): boolean;
  var
    i: Integer;
    PkgFile: TPkgFile;
  begin
    if APackage=nil then exit(false);
    Result:=true;
    if SysUtils.CompareText(APackage.Name,Identifier)=0 then exit;
    for i:=0 to APackage.FileCount-1 do
    begin
      PkgFile:=APackage.Files[i];
      if SysUtils.CompareText(PkgFile.Unit_Name,Identifier)=0 then exit;
    end;
    Result:=false;
  end;

  function IdentifierExists(Identifier: string): boolean;
  var
    i: Integer;
  begin
    Result:=true;
    if GetClass(Identifier)<>nil then exit;
    if SearchProject(Project1,Identifier) then exit;
    for i:=0 to PackageGraph.Count-1 do
      if SearchPackage(PackageGraph[i],Identifier) then exit;
    Result:=false;
  end;

  function IdentifierIsOk(Identifier: string): boolean;
  begin
    Result:=false;
    if (Identifier='') or not IsValidIdent(Identifier) then exit;
    if AllKeyWords.DoIdentifier(PChar(Identifier)) then exit;
    if IdentifierExists(Identifier) then exit;
    if IdentifierExists('T'+Identifier) then exit;
    Result:=true;
  end;

var
  i: Integer;
begin
  if IdentifierIsOk(Prefix) then
    exit(Prefix);
  while (Prefix<>'') and (Prefix[length(Prefix)] in ['0'..'9']) do
    System.Delete(Prefix,length(Prefix),1);
  if (Prefix='') or (not IsValidIdent(Prefix)) then
    Prefix:='Resource';
  i:=0;
  repeat
    inc(i);
    Result:=Prefix+IntToStr(i);
  until IdentifierIsOk(Result);
end;

function TLazSourceFileManager.ShowSaveFileAsDialog(var AFilename: string;
  AnUnitInfo: TUnitInfo; var LFMCode, LRSCode: TCodeBuffer; CanAbort: boolean): TModalResult;
var
  SaveDialog: TSaveDialog;
  SaveAsFilename, SaveAsFileExt, NewFilename, NewUnitName, NewFilePath,
  AlternativeUnitName: string;
  ACaption, AText: string;
  SrcEdit: TSourceEditor;
  FileWithoutPath: String;
  PkgDefaultDirectory: String;
  OldUnitName: String;
  IsPascal: Boolean;
  Filter: String;
  AllEditorExt: string;
  AllFilter: string;
begin
  if (AnUnitInfo<>nil) and (AnUnitInfo.OpenEditorInfoCount>0) then
    SrcEdit := TSourceEditor(AnUnitInfo.OpenEditorInfo[0].EditorComponent)
  else
    SrcEdit:=nil;
  //debugln('TLazSourceFileManager.ShowSaveFileAsDialog ',AnUnitInfo.Filename);

  // try to keep the old filename and extension
  SaveAsFileExt:=ExtractFileExt(AFileName);
  if (SaveAsFileExt='') and (SrcEdit<>nil) then begin
    if (SrcEdit.SyntaxHighlighterType in [lshFreePascal, lshDelphi])
    then
      SaveAsFileExt:=PascalExtension[EnvironmentOptions.PascalFileExtension]
    else
      SaveAsFileExt:=EditorOpts.HighlighterList.GetDefaultFilextension(
                         SrcEdit.SyntaxHighlighterType);
  end;
  IsPascal:=FilenameIsPascalSource(AFilename);
  if IsPascal then begin
    if AnUnitInfo<>nil then
      OldUnitName:=AnUnitInfo.ParseUnitNameFromSource(false)
    else
      OldUnitName:=ExtractFileNameOnly(AFilename);
  end else
    OldUnitName:='';
  //debugln('TLazSourceFileManager.ShowSaveFileAsDialog sourceunitname=',OldUnitName);
  SaveAsFilename:=OldUnitName;
  if SaveAsFilename='' then
    SaveAsFilename:=ExtractFileNameOnly(AFilename);
  if SaveAsFilename='' then
    SaveAsFilename:=lisnoname;

  //suggest lowercased name if user wants so
  if EnvironmentOptions.LowercaseDefaultFilename = true then
    SaveAsFilename:=LowerCase(SaveAsFilename);

  // let user choose a filename
  SaveDialog:=TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title:=lisSaveSpace+SaveAsFilename+' (*'+SaveAsFileExt+')';
    SaveDialog.FileName:=SaveAsFilename+SaveAsFileExt;

    Filter := lisLazarusUnit + ' (*.pas;*.pp)|*.pas;*.pp';
    if (SaveAsFileExt='.lpi') then
      Filter:=Filter+ '|' + lisLazarusProject + ' (*.lpi)|*.lpi';
    if (SaveAsFileExt='.lfm') or (SaveAsFileExt='.dfm') then
      Filter:=Filter+ '|' + lisLazarusForm + ' (*.lfm;*.dfm)|*.lfm;*.dfm';
    if (SaveAsFileExt='.lpk') then
      Filter:=Filter+ '|' + lisLazarusPackage + ' (*.lpk)|*.lpk';
    if (SaveAsFileExt='.lpr') then
      Filter:=Filter+ '|' + lisLazarusProjectSource + ' (*.lpr)|*.lpr';
    // append a filter for all editor files
    CreateFileDialogFilterForSourceEditorFiles(Filter,AllEditorExt,AllFilter);
    if AllEditorExt<>'' then
      Filter:=Filter+ '|' + lisEditorFileTypes + ' (' + AllEditorExt + ')|' + AllEditorExt;

    // append an any file filter *.*
    Filter:=Filter+ '|' + dlgAllFiles + ' (' + GetAllFilesMask + ')|' + GetAllFilesMask;

    // prepend an all filter
    Filter:=  lisLazarusFile + ' ('+AllFilter+')|' + AllFilter + '|' + Filter;
    SaveDialog.Filter := Filter;

    // if this is a project file, start in project directory
    if (AnUnitInfo=nil)
    or (AnUnitInfo.IsPartOfProject and (not Project1.IsVirtual)
        and (not FileIsInPath(SaveDialog.InitialDir,Project1.ProjectDirectory)))
    then begin
      SaveDialog.InitialDir:=Project1.ProjectDirectory;
    end;
    // if this is a package file, then start in package directory
    PkgDefaultDirectory:=
      PkgBoss.GetDefaultSaveDirectoryForFile(AFilename);
    if (PkgDefaultDirectory<>'')
    and (not FileIsInPath(SaveDialog.InitialDir,PkgDefaultDirectory)) then
      SaveDialog.InitialDir:=PkgDefaultDirectory;
    // show save dialog
    if (not SaveDialog.Execute) or (ExtractFileName(SaveDialog.Filename)='')
    then begin
      // user cancels
      Result:=mrCancel;
      exit;
    end;
    NewFilename:=ExpandFileNameUTF8(SaveDialog.Filename);
    //debugln(['TLazSourceFileManager.ShowSaveFileAsDialog SaveDialog.Filename="',SaveDialog.Filename,'" NewFilename="',NewFilename,'"']);
  finally
    InputHistories.StoreFileDialogSettings(SaveDialog);
    SaveDialog.Free;
  end;

  // check file extension
  if ExtractFileExt(NewFilename)='' then begin
    NewFilename:=NewFilename+SaveAsFileExt;
  end;

  // check file path
  NewFilePath:=ExtractFilePath(NewFilename);
  if not DirPathExists(NewFilePath) then begin
    ACaption:=lisEnvOptDlgDirectoryNotFound;
    AText:=Format(lisTheDestinationDirectoryDoesNotExist,
                  [LineEnding, '"', NewFilePath, '"']);
    Result:=IDEMessageDialogAb(ACaption, AText, mtConfirmation,[mbCancel],CanAbort);
    exit;
  end;

  // check unitname
  if FilenameIsPascalUnit(NewFilename) then begin
    NewUnitName:=ExtractFileNameOnly(NewFilename);
    // do not rename the unit if new filename differs from its name only in case
    if LowerCase(OldUnitName)=NewUnitName then
      NewUnitName:=OldUnitName;
    if NewUnitName='' then begin
      Result:=mrCancel;
      exit;
    end;
    if not IsValidUnitName(NewUnitName) then begin
      AlternativeUnitName:=NameToValidIdentifier(NewUnitName);
      Result:=IDEMessageDialogAb(lisInvalidPascalIdentifierCap,
        Format(lisInvalidPascalIdentifierText,[NewUnitName,AlternativeUnitName]),
        mtWarning,[mbIgnore,mbCancel],CanAbort);
      if Result in [mrCancel,mrAbort] then exit;
      NewUnitName:=AlternativeUnitName;
    end;
    if Project1.IndexOfUnitWithName(NewUnitName,true,AnUnitInfo)>=0 then
    begin
      Result:=IDEQuestionDialogAb(lisUnitNameAlreadyExistsCap,
         Format(lisTheUnitAlreadyExists, ['"', NewUnitName, '"']),
          mtConfirmation, [mrIgnore, lisForceRenaming,
                          mrCancel, lisCancelRenaming,
                          mrAbort, lisAbortAll], not CanAbort);
      if Result=mrIgnore then
        Result:=mrCancel
      else
        exit;
    end;
  end else begin
    NewUnitName:='';
  end;

  // check filename
  if FilenameIsPascalUnit(NewFilename) then begin
    FileWithoutPath:=ExtractFileName(NewFilename);
    // check if file should be auto renamed

    if EnvironmentOptions.CharcaseFileAction = ccfaAsk then begin
      if lowercase(FileWithoutPath)<>FileWithoutPath
      then begin
        Result:=IDEQuestionDialogAb(lisRenameFile,
          Format(lisThisLooksLikeAPascalFileItIsRecommendedToUseLowerC,
                 [LineEnding, LineEnding]),
          mtWarning, [mrYes, lisRenameToLowercase, mrNoToAll, lisKeepName,
                      mrAbort, lisAbortAll], not CanAbort);
        if Result=mrYes then
          NewFileName:=ExtractFilePath(NewFilename)+lowercase(FileWithoutPath);
        Result:=mrOk;
      end;
    end else begin
      if EnvironmentOptions.CharcaseFileAction = ccfaAutoRename then
        NewFileName:=ExtractFilePath(NewFilename)+lowercase(FileWithoutPath);
    end;
  end;

  // check overwrite existing file
  if ((not FilenameIsAbsolute(AFilename))
      or (CompareFilenames(NewFilename,AFilename)<>0))
  and FileExistsUTF8(NewFilename) then begin
    ACaption:=lisOverwriteFile;
    AText:=Format(lisAFileAlreadyExistsReplaceIt, ['"', NewFilename, '"', LineEnding]);
    Result:=IDEQuestionDialogAb(ACaption, AText, mtConfirmation,
      [mrYes, lisOverwriteFileOnDisk, mrCancel,
       mrAbort, lisAbortAll], not CanAbort);
    if Result=mrCancel then exit;
  end;

  if AnUnitInfo<>nil then begin
    // rename unit
    Result:=RenameUnit(AnUnitInfo,NewFilename,NewUnitName,LFMCode,LRSCode);
    AFilename:=AnUnitInfo.Filename;
    if Result<>mrOk then exit;
  end else begin
    Result:=mrOk;
    AFilename:=NewFilename;
  end;
end;

type
  TLRTGrubber = class(TObject)
  private
    FGrubbed: TStrings;
    FWriter: TWriter;
  public
    constructor Create(TheWriter: TWriter);
    destructor Destroy; override;
    procedure Grub(Sender: TObject; const Instance: TPersistent;
                   PropInfo: PPropInfo; var Content: string);
    property Grubbed: TStrings read FGrubbed;
    property Writer: TWriter read FWriter write FWriter;
  end;

constructor TLRTGrubber.Create(TheWriter: TWriter);
begin
  inherited Create;
  FGrubbed:=TStringList.Create;
  FWriter:=TheWriter;
  FWriter.OnWriteStringProperty:=@Grub;
end;

destructor TLRTGrubber.Destroy;
begin
  FGrubbed.Free;
  inherited Destroy;
end;

procedure TLRTGrubber.Grub(Sender: TObject; const Instance: TPersistent;
  PropInfo: PPropInfo; var Content: string);
var
  LRSWriter: TLRSObjectWriter;
  Path: String;
begin
  if not Assigned(Instance) then exit;
  if not Assigned(PropInfo) then exit;
  if SysUtils.CompareText(PropInfo^.PropType^.Name,'TTRANSLATESTRING')<>0 then exit;
  if Writer.Driver is TLRSObjectWriter then begin
    LRSWriter:=TLRSObjectWriter(Writer.Driver);
    Path:=LRSWriter.GetStackPath;
  end else begin
    Path:=Instance.ClassName+'.'+PropInfo^.Name;
  end;
  FGrubbed.Add(Uppercase(Path)+'='+Content);
  //DebugLn(['TLRTGrubber.Grub "',FGrubbed[FGrubbed.Count-1],'"']);
end;

function TLazSourceFileManager.SaveUnitComponent(AnUnitInfo: TUnitInfo;
  LRSCode, LFMCode: TCodeBuffer; Flags: TSaveFlags): TModalResult;

  function IsI18NEnabled(UnitOwners: TFPList): boolean;
  var
    i: Integer;
    APackage: TLazPackage;
    PkgFile: TPkgFile;
  begin
    if AnUnitInfo.IsPartOfProject then begin
      // a project unit
      Result:=AnUnitInfo.Project.EnableI18N and AnUnitInfo.Project.EnableI18NForLFM
         and (not AnUnitInfo.DisableI18NForLFM);
      exit;
    end;
    if (UnitOwners<>nil) then begin
      for i:=0 to UnitOwners.Count-1 do begin
        if TObject(UnitOwners[i]) is TLazPackage then begin
          // a package unit
          APackage:=TLazPackage(UnitOwners[i]);
          Result:=false;
          if APackage.EnableI18N and APackage.EnableI18NForLFM then begin
            PkgFile:=APackage.FindPkgFile(AnUnitInfo.Filename,true,true);
            Result:=(PkgFile<>nil) and (not PkgFile.DisableI18NForLFM);
          end;
          exit;
        end;
      end;
    end;
    // a rogue unit
    Result:=false;
  end;

var
  ComponentSavingOk: boolean;
  MemStream, BinCompStream, TxtCompStream: TExtMemoryStream;
  DestroyDriver: Boolean;
  Writer: TWriter;
  ACaption, AText: string;
  CompResourceCode, LFMFilename, TestFilename: string;
  ADesigner: TDesigner;
  Grubber: TLRTGrubber;
  LRTFilename: String;
  AncestorUnit: TUnitInfo;
  Ancestor: TComponent;
  HasI18N: Boolean;
  UnitOwners: TFPList;
  LRSFilename: String;
  PropPath: String;
  ResType: TResourceType;
begin
  Result:=mrCancel;

  // save lrs - lazarus resource file and lfm - lazarus form text file
  // Note: When there is a bug in the source, the include directive of the
  //       resource code can not be found, therefore the LFM file should always
  //       be saved first.
  //       And therefore each TUnitInfo stores the resource filename (.lrs).

  // the lfm file is saved before the lrs file, because the IDE only needs the
  // lfm file to recreate the lrs file.
  // by VVI - now a LRT file is saved in addition to LFM and LRS
  // LRT file format (in present) are lines
  // <ClassName>.<PropertyName>=<PropertyValue>
  LRSFilename:='';
  ResType:=MainBuildBoss.GetResourceType(AnUnitInfo);
  LRSCode:=nil;

  if (AnUnitInfo.Component<>nil) then begin
    // stream component to resource code and to lfm file
    ComponentSavingOk:=true;

    // clean up component
    Result:=RemoveLooseEvents(AnUnitInfo,true);
    if Result<>mrOk then exit;

    // save designer form properties to the component
    FormEditor1.SaveHiddenDesignerFormProperties(AnUnitInfo.Component);

    if ResType=rtLRS then begin
      if (sfSaveToTestDir in Flags) then
        LRSFilename:=MainBuildBoss.GetDefaultLRSFilename(AnUnitInfo)
      else
        LRSFilename:=MainBuildBoss.FindLRSFilename(AnUnitInfo,true);
    end;

    // stream component to binary stream
    BinCompStream:=TExtMemoryStream.Create;
    if AnUnitInfo.ComponentLastBinStreamSize>0 then
      BinCompStream.Capacity:=AnUnitInfo.ComponentLastBinStreamSize+LRSStreamChunkSize;
    Writer:=nil;
    DestroyDriver:=false;
    Grubber:=nil;
    UnitOwners:=nil;
    try
      UnitOwners:=PkgBoss.GetOwnersOfUnit(AnUnitInfo.Filename);
      Result:=mrOk;
      repeat
        try
          BinCompStream.Position:=0;
          Writer:=AnUnitInfo.UnitResourceFileformat.CreateWriter(BinCompStream,DestroyDriver);
          // used to save lrt files
          HasI18N:=IsI18NEnabled(UnitOwners);
          if HasI18N then
            Grubber:=TLRTGrubber.Create(Writer);
          Writer.OnWriteMethodProperty:=@FormEditor1.WriteMethodPropertyEvent;
          //DebugLn(['TLazSourceFileManager.SaveUnitComponent AncestorInstance=',dbgsName(AncestorInstance)]);
          Writer.OnFindAncestor:=@FormEditor1.WriterFindAncestor;
          AncestorUnit:=AnUnitInfo.FindAncestorUnit;
          Ancestor:=nil;
          if AncestorUnit<>nil then
            Ancestor:=AncestorUnit.Component;
          //DebugLn(['TLazSourceFileManager.SaveUnitComponent Writer.WriteDescendent ARoot=',AnUnitInfo.Component,' Ancestor=',DbgSName(Ancestor)]);
          Writer.WriteDescendent(AnUnitInfo.Component,Ancestor);
          if DestroyDriver then
            Writer.Driver.Free;
          FreeAndNil(Writer);
          AnUnitInfo.ComponentLastBinStreamSize:=BinCompStream.Size;
        except
          on E: Exception do begin
            PropPath:='';
            if Writer.Driver is TLRSObjectWriter then
              PropPath:=TLRSObjectWriter(Writer.Driver).GetStackPath;
            DumpExceptionBackTrace;
            ACaption:=lisStreamingError;
            AText:=Format(lisUnableToStreamT, [AnUnitInfo.ComponentName,
                          AnUnitInfo.ComponentName]) + LineEnding + E.Message;
            if PropPath<>'' then
              AText := Atext + LineEnding + LineEnding + lisPathToInstance
                     + LineEnding + PropPath;
            Result:=IDEMessageDialog(ACaption, AText, mtError,
                       [mbAbort, mbRetry, mbIgnore]);
            if Result=mrAbort then exit;
            if Result=mrIgnore then Result:=mrOk;
            ComponentSavingOk:=false;
          end;
        end;
      until Result<>mrRetry;

      // create lazarus form resource code
      if ComponentSavingOk and (LRSFilename<>'') then begin
        if LRSCode=nil then begin
          LRSCode:=CodeToolBoss.CreateFile(LRSFilename);
          ComponentSavingOk:=(LRSCode<>nil);
        end;
        if ComponentSavingOk then begin
          // there is no bug in the source, so the resource code should be changed too
          MemStream:=TExtMemoryStream.Create;
          if AnUnitInfo.ComponentLastLRSStreamSize>0 then
            MemStream.Capacity:=AnUnitInfo.ComponentLastLRSStreamSize+LRSStreamChunkSize;
          try
            BinCompStream.Position:=0;
            BinaryToLazarusResourceCode(BinCompStream,MemStream
              ,'T'+AnUnitInfo.ComponentName,'FORMDATA');
            AnUnitInfo.ComponentLastLRSStreamSize:=MemStream.Size;
            MemStream.Position:=0;
            SetLength(CompResourceCode,MemStream.Size);
            MemStream.Read(CompResourceCode[1],length(CompResourceCode));
          finally
            MemStream.Free;
          end;
        end;
        if ComponentSavingOk then begin
          {$IFDEF IDE_DEBUG}
          writeln('TLazSourceFileManager.SaveFileResources E ',CompResourceCode);
          {$ENDIF}
          // replace lazarus form resource code in include file (.lrs)
          if not (sfSaveToTestDir in Flags) then begin
            // if resource name has changed, delete old resource
            if (AnUnitInfo.ComponentName<>AnUnitInfo.ComponentResourceName)
            and (AnUnitInfo.ComponentResourceName<>'') then begin
              CodeToolBoss.RemoveLazarusResource(LRSCode,
                                          'T'+AnUnitInfo.ComponentResourceName);
            end;
            // add comment to resource file (if not already exists)
            if (not CodeToolBoss.AddLazarusResourceHeaderComment(LRSCode,LRSComment)) then
            begin
              ACaption:=lisResourceSaveError;
              AText:=Format(lisUnableToAddResourceHeaderCommentToResourceFile, [
                LineEnding, '"', LRSCode.FileName, '"', LineEnding]);
              Result:=IDEMessageDialog(ACaption,AText,mtError,[mbIgnore,mbAbort]);
              if Result<>mrIgnore then exit;
            end;
            // add resource to resource file
            if (not CodeToolBoss.AddLazarusResource(LRSCode,
               'T'+AnUnitInfo.ComponentName,CompResourceCode)) then
            begin
              ACaption:=lisResourceSaveError;
              AText:=Format(lisUnableToAddResourceTFORMDATAToResourceFileProbably,
                [AnUnitInfo.ComponentName,
                 LineEnding, '"', LRSCode.FileName, '"', LineEnding] );
              Result:=IDEMessageDialog(ACaption, AText, mtError, [mbIgnore, mbAbort]);
              if Result<>mrIgnore then exit;
            end else begin
              AnUnitInfo.ComponentResourceName:=AnUnitInfo.ComponentName;
            end;
          end else begin
            LRSCode.Source:=CompResourceCode;
          end;
        end;
      end;
      if ComponentSavingOk then begin
        if (not AnUnitInfo.IsVirtual) or (sfSaveToTestDir in Flags) then
        begin
          // save lfm file
          LFMFilename:=AnUnitInfo.UnitResourceFileformat.GetUnitResourceFilename(AnUnitInfo.Filename,false);
          if AnUnitInfo.IsVirtual then
            LFMFilename:=AppendPathDelim(MainBuildBoss.GetTestBuildDirectory)+LFMFilename;
          if LFMCode=nil then begin
            LFMCode:=CodeToolBoss.CreateFile(LFMFilename);
            if LFMCode=nil then begin
              Result:=IDEQuestionDialog(lisUnableToCreateFile,
                Format(lisUnableToCreateFile2, ['"', LFMFilename, '"']),
                mtWarning, [mrIgnore, lisContinueWithoutLoadingForm,
                           mrCancel, lisCancelLoadingUnit,
                           mrAbort, lisAbortAllLoading]);
              if Result<>mrIgnore then exit;
            end;
          end;
          if (LFMCode<>nil) then begin
            {$IFDEF IDE_DEBUG}
            writeln('TLazSourceFileManager.SaveFileResources E2 LFM=',LFMCode.Filename);
            {$ENDIF}
            if (ResType=rtRes) and (LFMCode.DiskEncoding<>EncodingUTF8) then
            begin
              // the .lfm file is used by fpcres, which only supports UTF8 without BOM
              DebugLn(['TLazSourceFileManager.SaveUnitComponent fixing encoding of ',LFMCode.Filename,' from ',LFMCode.DiskEncoding,' to ',EncodingUTF8]);
              LFMCode.DiskEncoding:=EncodingUTF8;
            end;

            Result:=mrOk;
            repeat
              try
                // transform binary to text
                TxtCompStream:=TExtMemoryStream.Create;
                if AnUnitInfo.ComponentLastLFMStreamSize>0 then
                  TxtCompStream.Capacity:=AnUnitInfo.ComponentLastLFMStreamSize
                                          +LRSStreamChunkSize;
                try
                  BinCompStream.Position:=0;
                  AnUnitInfo.UnitResourceFileformat.BinStreamToTextStream(BinCompStream,TxtCompStream);
                  AnUnitInfo.ComponentLastLFMStreamSize:=TxtCompStream.Size;
                  // stream text to file
                  TxtCompStream.Position:=0;
                  LFMCode.LoadFromStream(TxtCompStream);
                  Result:=SaveCodeBufferToFile(LFMCode,LFMCode.Filename,true);
                  if not Result=mrOk then exit;
                  Result:=mrCancel;
                finally
                  TxtCompStream.Free;
                end;
              except
                on E: Exception do begin
                  // added to get more feedback on issue 7009
                  Debugln('TLazSourceFileManager.SaveFileResources E3: ', E.Message);
                  DumpExceptionBackTrace;
                  ACaption:=lisStreamingError;
                  AText:=Format(
                    lisUnableToTransformBinaryComponentStreamOfTIntoText, [
                    AnUnitInfo.ComponentName, AnUnitInfo.ComponentName])
                    +LineEnding+E.Message;
                  Result:=IDEMessageDialog(ACaption, AText, mtError,
                                     [mbAbort, mbRetry, mbIgnore]);
                  if Result=mrAbort then exit;
                  if Result=mrIgnore then Result:=mrOk;
                end;
              end;
            until Result<>mrRetry;
          end;
        end;
      end;
      // Now the most important file (.lfm) is saved.
      // Now save the secondary files

      // save the .lrt file containing the list of all translatable strings of
      // the component
      if ComponentSavingOk
      and (Grubber<>nil) and (Grubber.Grubbed.Count>0)
      and (not (sfSaveToTestDir in Flags))
      and (not AnUnitInfo.IsVirtual) then begin
        LRTFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lrt');
        DebugLn(['TLazSourceFileManager.SaveUnitComponent save lrt: ',LRTFilename]);
        Result:=SaveStringToFile(LRTFilename,Grubber.Grubbed.Text,
                                 [mbIgnore,mbAbort],AnUnitInfo.Filename);
        if (Result<>mrOk) and (Result<>mrIgnore) then exit;
      end;

    finally
      try
        FreeAndNil(BinCompStream);
        if DestroyDriver and (Writer<>nil) then Writer.Driver.Free;
        FreeAndNil(Writer);
        FreeAndNil(Grubber);
        FreeAndNil(UnitOwners);
      except
        on E: Exception do begin
          debugln('TLazSourceFileManager.SaveFileResources Error cleaning up: ',E.Message);
        end;
      end;
    end;
  end;
  {$IFDEF IDE_DEBUG}
  if ResourceCode<>nil then
    writeln('TLazSourceFileManager.SaveFileResources F ',ResourceCode.Modified);
  {$ENDIF}
  // save binary stream (.lrs)
  if LRSCode<>nil then begin
    if (not (sfSaveToTestDir in Flags)) then
    begin
      if (LRSCode.Modified) then begin
        if FilenameIsAbsolute(LRSCode.Filename) then
          LRSFilename:=LRSCode.Filename
        else if LRSFilename='' then
          LRSFilename:=MainBuildBoss.FindLRSFilename(AnUnitInfo,true);
        if (LRSFilename<>'') and FilenameIsAbsolute(LRSFilename) then
        begin
          Result:=ForceDirectoryInteractive(ExtractFilePath(LRSFilename),[mbRetry]);
          if not Result=mrOk then exit;
          Result:=SaveCodeBufferToFile(LRSCode,LRSFilename);
          if not Result=mrOk then exit;
        end;
      end;
    end else begin
      TestFilename:=MainBuildBoss.GetTestUnitFilename(AnUnitInfo);
      LRSFilename:=ChangeFileExt(TestFilename,ExtractFileExt(LRSCode.Filename));
      Result:=SaveCodeBufferToFile(LRSCode,LRSFilename);
      if not Result=mrOk then exit;
    end;
  end;
  // mark designer unmodified
  ADesigner:=FindRootDesigner(AnUnitInfo.Component) as TDesigner;
  if ADesigner<>nil then
    ADesigner.DefaultFormBoundsValid:=false;

  Result:=mrOk;
  {$IFDEF IDE_DEBUG}
  writeln('TLazSourceFileManager.SaveFileResources G ',LFMCode<>nil);
  {$ENDIF}
end;

function TLazSourceFileManager.RemoveLooseEvents(AnUnitInfo: TUnitInfo;
  OkOnCodeErrors: boolean): TModalResult;
var
  ComponentModified: boolean;
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  Result:=mrOk;
  if (AnUnitInfo.Component=nil) then exit;
  if not MainIDE.BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  // unselect methods in ObjectInspector1
  if (ObjectInspector1<>nil)
  and (ObjectInspector1.PropertyEditorHook.LookupRoot=AnUnitInfo.Component) then
  begin
    ObjectInspector1.EventGrid.ItemIndex:=-1;
    ObjectInspector1.FavoriteGrid.ItemIndex:=-1;
  end;
  //debugln('TLazSourceFileManager.RemoveDanglingEvents ',AnUnitInfo.Filename,' ',dbgsName(AnUnitInfo.Component));
  // remove dangling methods
  Result:=RemoveDanglingEvents(AnUnitInfo.Component, AnUnitInfo.Source, True,
                               ComponentModified);
  // update ObjectInspector1
  if ComponentModified
  and (ObjectInspector1<>nil)
  and (ObjectInspector1.PropertyEditorHook.LookupRoot=AnUnitInfo.Component) then
  begin
    ObjectInspector1.EventGrid.RefreshPropertyValues;
    ObjectInspector1.FavoriteGrid.RefreshPropertyValues;
  end;
end;

function TLazSourceFileManager.RenameUnit(AnUnitInfo: TUnitInfo; NewFilename,
  NewUnitName: string; var LFMCode, LRSCode: TCodeBuffer): TModalResult;
var
  NewLFMFilename: String;
  OldSourceCode: String;
  NewSource: TCodeBuffer;
  NewFilePath: String;
  NewLRSFilePath: String;
  OldFilePath: String;
  OldLRSFilePath: String;
  OldFilename: String;
  NewLRSFilename: String;
  NewHighlighter: TLazSyntaxHighlighter;
  AmbiguousFiles: TStringList;
  AmbiguousText: string;
  i: Integer;
  AmbiguousFilename: String;
  OldUnitPath: String;
  OldLFMFilename: String;
  OldLRSFilename: String;
  OldPPUFilename: String;
  OutDir: string;
  Owners: TFPList;
  OldFileExisted: Boolean;
  ConvTool: TConvDelphiCodeTool;
begin
  Project1.BeginUpdate(false);
  try
    OldFilename:=AnUnitInfo.Filename;
    OldFilePath:=ExtractFilePath(OldFilename);
    OldLFMFilename:='';
    // ToDo: use UnitResources
    if FilenameIsPascalUnit(OldFilename) then begin
      OldLFMFilename:=ChangeFileExt(OldFilename,'.lfm');
      if not FileExistsUTF8(OldLFMFilename) then
        OldLFMFilename:=ChangeFileExt(OldFilename,'.dfm');
    end;
    if NewUnitName='' then
      NewUnitName:=AnUnitInfo.Unit_Name;
    debugln(['TLazSourceFileManager.RenameUnit ',AnUnitInfo.Filename,' NewUnitName=',NewUnitName,' OldUnitName=',AnUnitInfo.Unit_Name,' LFMCode=',LFMCode<>nil,' LRSCode=',LRSCode<>nil,' NewFilename="',NewFilename,'"']);

    // check new resource file
    NewLFMFilename:='';
    if FilenameIsPascalUnit(NewFilename) then
       NewLFMFilename:=ChangeFileExt(NewFilename,'.lfm');
    if AnUnitInfo.ComponentName='' then begin
      // unit has no component
      // -> remove lfm file, so that it will not be auto loaded on next open
      if (FileExistsUTF8(NewLFMFilename))
      and (not DeleteFileUTF8(NewLFMFilename))
      and (IDEMessageDialog(lisPkgMangDeleteFailed, Format(lisDeletingOfFileFailed, [
        '"', NewLFMFilename, '"']), mtError, [mbIgnore, mbCancel])=mrCancel)
        then
      begin
        Result:=mrCancel;
        exit;
      end;
    end;

    // create new source with the new filename
    OldSourceCode:=AnUnitInfo.Source.Source;
    NewSource:=CodeToolBoss.CreateFile(NewFilename);
    NewSource.Source:=OldSourceCode;
    if NewSource=nil then begin
      Result:=IDEMessageDialog(lisUnableToCreateFile,
        Format(lisCanNotCreateFile, ['"', NewFilename, '"']),
        mtError,[mbCancel,mbAbort]);
      exit;
    end;
    // get final filename
    NewFilename:=NewSource.Filename;
    NewFilePath:=ExtractFilePath(NewFilename);
    EnvironmentOptions.RemoveFromRecentOpenFiles(OldFilename);
    EnvironmentOptions.AddToRecentOpenFiles(NewFilename);
    MainIDE.SetRecentFilesMenu;

    // add new path to unit path
    if AnUnitInfo.IsPartOfProject
    and (not Project1.IsVirtual)
    and (FilenameIsPascalUnit(NewFilename))
    and (CompareFilenames(NewFilePath,Project1.ProjectDirectory)<>0) then begin
      OldUnitPath:=Project1.CompilerOptions.GetUnitPath(false);

      if SearchDirectoryInSearchPath(OldUnitPath,NewFilePath,1)<1 then begin
        //DebugLn('TLazSourceFileManager.RenameUnit NewFilePath="',NewFilePath,'" OldUnitPath="',OldUnitPath,'"');
        if IDEMessageDialog(lisExtendUnitPath,
          Format(lisTheDirectoryIsNotYetInTheUnitPathAddIt,
                 ['"', NewFilePath, '"', LineEnding]),
          mtConfirmation,[mbYes,mbNo])=mrYes then
        begin
          Project1.CompilerOptions.OtherUnitFiles:=
                       Project1.CompilerOptions.OtherUnitFiles+';'
                       +CreateRelativePath(NewFilePath,Project1.ProjectDirectory);
        end;
      end;
    end;

    // rename lfm file
    if FilenameIsAbsolute(NewLFMFilename) then begin
      if (LFMCode=nil)
      and (OldLFMFilename<>'')
      and FilenameIsAbsolute(OldLFMFilename) and FileExistsUTF8(OldLFMFilename) then
        LFMCode:=CodeToolBoss.LoadFile(OldLFMFilename,false,false);
      if (LFMCode<>nil) then begin
        Result:=SaveCodeBufferToFile(LFMCode,NewLFMFilename,true);
        if not (Result in [mrOk,mrIgnore]) then begin
          DebugLn(['TLazSourceFileManager.RenameUnit SaveCodeBufferToFile failed for "',NewLFMFilename,'"']);
          exit;
        end;
        LFMCode:=CodeToolBoss.LoadFile(NewLFMFilename,true,false);
        if LFMCode<>nil then
          NewLFMFilename:=LFMCode.Filename;
        ConvTool:=TConvDelphiCodeTool.Create(NewSource);
        try
          if not ConvTool.RenameResourceDirectives then
            debugln(['TLazSourceFileManager.RenameUnit WARNING: unable to rename resource directive in "',NewSource.Filename,'"']);
        finally
          ConvTool.Free;
        end;
      end;
    end;

    // rename Resource file (.lrs)
    if (LRSCode<>nil) then begin
      // the resource include line in the code will be changed later after
      // changing the unitname
      if AnUnitInfo.IsPartOfProject
      and (not Project1.IsVirtual)
      and (pfLRSFilesInOutputDirectory in Project1.Flags) then begin
        NewLRSFilename:=MainBuildBoss.GetDefaultLRSFilename(AnUnitInfo);
        NewLRSFilename:=AppendPathDelim(ExtractFilePath(NewLRSFilename))
          +ExtractFileNameOnly(NewFilename)+ResourceFileExt;
      end else begin
        OldLRSFilePath:=ExtractFilePath(LRSCode.Filename);
        NewLRSFilePath:=OldLRSFilePath;
        if FilenameIsAbsolute(OldFilePath)
        and FileIsInPath(OldLRSFilePath,OldFilePath) then begin
          // resource code was in the same or in a sub directory of source
          // -> try to keep this relationship
          NewLRSFilePath:=NewFilePath
                           +copy(LRSCode.Filename,length(OldFilePath)+1,
                             length(LRSCode.Filename));
          if not DirPathExists(NewLRSFilePath) then
            NewLRSFilePath:=NewFilePath;
        end else begin
          // resource code was not in the same or in a sub directory of source
          // copy resource into the same directory as the source
          NewLRSFilePath:=NewFilePath;
        end;
        NewLRSFilename:=NewLRSFilePath
                        +ExtractFileNameOnly(NewFilename)+ResourceFileExt;
      end;
      Result:=ForceDirectoryInteractive(ExtractFilePath(NewLRSFilename),[mbRetry,mbIgnore]);
      if Result=mrCancel then exit;
      if Result=mrOk then begin
        if not CodeToolBoss.SaveBufferAs(LRSCode,NewLRSFilename,LRSCode)
        then
          DebugLn(['TLazSourceFileManager.RenameUnit CodeToolBoss.SaveBufferAs failed: NewResFilename="',NewLRSFilename,'"']);
      end;

      {$IFDEF IDE_DEBUG}
      debugln(['TLazSourceFileManager.RenameUnit C ',ResourceCode<>nil]);
      debugln(['   NewResFilePath="',NewResFilePath,'" NewResFilename="',NewResFilename,'"']);
      if ResourceCode<>nil then debugln('*** ResourceFileName ',ResourceCode.Filename);
      {$ENDIF}
    end else begin
      NewLRSFilename:='';
    end;
    // rename unit name of jit class
    if (AnUnitInfo.Component<>nil) then
      FormEditor1.RenameJITComponentUnitname(AnUnitInfo.Component,NewUnitName);
    {$IFDEF IDE_DEBUG}
    if AnUnitInfo.Component<>nil then debugln('*** AnUnitInfo.Component ',dbgsName(AnUnitInfo.Component),' ClassUnitname=',GetClassUnitName(AnUnitInfo.Component.ClassType));
    debugln(['TLazSourceFileManager.RenameUnit D ',ResourceCode<>nil]);
    {$ENDIF}

    // set new codebuffer in unitinfo and sourceeditor
    AnUnitInfo.Source:=NewSource;
    AnUnitInfo.ClearModifieds;
    for i := 0 to AnUnitInfo.EditorInfoCount -1 do
      if AnUnitInfo.EditorInfo[i].EditorComponent <> nil then
        TSourceEditor(AnUnitInfo.EditorInfo[i].EditorComponent).CodeBuffer := NewSource;
        // the code is not changed, therefore the marks are kept

    // change unitname in lpi and in main source file
    AnUnitInfo.Unit_Name:=NewUnitName;
    if LRSCode<>nil then begin
      // change resource filename in the source include directive
      if not CodeToolBoss.RenameMainInclude(AnUnitInfo.Source,
        ExtractFilename(LRSCode.Filename),false)
      then
        DebugLn(['TLazSourceFileManager.RenameUnit CodeToolBoss.RenameMainInclude failed: AnUnitInfo.Source="',AnUnitInfo.Source,'" ResourceCode="',ExtractFilename(LRSCode.Filename),'"']);
    end;

    // change unitname on SourceNotebook
    if AnUnitInfo.OpenEditorInfoCount > 0 then
      UpdateSourceNames;

    // change syntax highlighter
    NewHighlighter:=FilenameToLazSyntaxHighlighter(NewFilename);
    AnUnitInfo.UpdateDefaultHighlighter(NewHighlighter);
    for i := 0 to AnUnitInfo.EditorInfoCount - 1 do
      if (AnUnitInfo.EditorInfo[i].EditorComponent <> nil) and
         (not AnUnitInfo.EditorInfo[i].CustomHighlighter)
      then
        TSourceEditor(AnUnitInfo.EditorInfo[i].EditorComponent).SyntaxHighlighterType :=
          AnUnitInfo.EditorInfo[i].SyntaxHighlighter;

    // save file
    if not NewSource.IsVirtual then begin
      Result:=AnUnitInfo.WriteUnitSource;
      if Result<>mrOk then exit;
      AnUnitInfo.Modified:=false;
    end;

    // change lpks containing the file
    Result:=PkgBoss.OnRenameFile(OldFilename,AnUnitInfo.Filename,
                                 AnUnitInfo.IsPartOfProject);
    if Result=mrAbort then exit;

    OldFileExisted:=FilenameIsAbsolute(OldFilename) and FileExistsUTF8(OldFilename);

    // delete ambiguous files
    NewFilePath:=ExtractFilePath(NewFilename);
    AmbiguousFiles:=
      FindFilesCaseInsensitive(NewFilePath,ExtractFilename(NewFilename),true);
    if AmbiguousFiles<>nil then begin
      try
        if (AmbiguousFiles.Count=1)
        and (CompareFilenames(OldFilePath,NewFilePath)=0)
        and (CompareFilenames(AmbiguousFiles[0],ExtractFilename(OldFilename))=0)
        then
          AmbiguousText:=Format(lisDeleteOldFile, ['"', ExtractFilename(
            OldFilename), '"'])
        else
          AmbiguousText:=
            Format(lisThereAreOtherFilesInTheDirectoryWithTheSameName,
                   [LineEnding, LineEnding, AmbiguousFiles.Text, LineEnding]);
        Result:=IDEMessageDialog(lisAmbiguousFilesFound, AmbiguousText,
          mtWarning,[mbYes,mbNo,mbAbort]);
        if Result=mrAbort then exit;
        if Result=mrYes then begin
          NewFilePath:=AppendPathDelim(ExtractFilePath(NewFilename));
          for i:=0 to AmbiguousFiles.Count-1 do begin
            AmbiguousFilename:=NewFilePath+AmbiguousFiles[i];
            if (FileExistsUTF8(AmbiguousFilename))
            and (not DeleteFileUTF8(AmbiguousFilename))
            and (IDEMessageDialog(lisPkgMangDeleteFailed, Format(lisDeletingOfFileFailed,
              ['"', AmbiguousFilename, '"']), mtError, [mbIgnore, mbCancel])=
              mrCancel) then
            begin
              Result:=mrCancel;
              exit;
            end;
          end;
        end;
      finally
        AmbiguousFiles.Free;
      end;
    end;

    // remove old path from unit path
    if AnUnitInfo.IsPartOfProject
    and (FilenameIsPascalUnit(OldFilename))
    and (OldFilePath<>'') then begin
      //DebugLn('TLazSourceFileManager.RenameUnit OldFilePath="',OldFilePath,'" SourceDirs="',Project1.SourceDirectories.CreateSearchPathFromAllFiles,'"');
      if (SearchDirectoryInSearchPath(
        Project1.SourceDirectories.CreateSearchPathFromAllFiles,OldFilePath,1)<1)
      then begin
        //DebugLn('TLazSourceFileManager.RenameUnit OldFilePath="',OldFilePath,'" UnitPath="',Project1.CompilerOptions.GetUnitPath(false),'"');
        if (SearchDirectoryInSearchPath(
                     Project1.CompilerOptions.GetUnitPath(false),OldFilePath,1)<1)
        then begin
          if IDEMessageDialog(lisCleanUpUnitPath,
              Format(lisTheDirectoryIsNoLongerNeededInTheUnitPathRemoveIt,
                     ['"', OldFilePath, '"', LineEnding]),
              mtConfirmation,[mbYes,mbNo])=mrYes then
          begin
            Project1.CompilerOptions.OtherUnitFiles:=
                        RemoveSearchPaths(Project1.CompilerOptions.OtherUnitFiles,
                                          OldUnitPath);
          end;
        end;
      end;
    end;

    // delete old pas, .pp, .ppu
    if (CompareFilenames(NewFilename,OldFilename)<>0)
    and OldFileExisted then begin
      if IDEMessageDialog(lisDeleteOldFile2,
        Format(lisDeleteOldFile, ['"', OldFilename, '"']),
        mtConfirmation,[mbYes,mbNo])=mrYes then
      begin
        Result:=DeleteFileInteractive(OldFilename,[mbAbort]);
        if Result=mrAbort then exit;
        // delete old lfm
        //debugln(['TLazSourceFileManager.RenameUnit NewLFMFilename=',NewLFMFilename,' exists=',FileExistsUTF8(NewLFMFilename),' Old=',OldLFMFilename,' exists=',FileExistsUTF8(OldLFMFilename)]);
        if FileExistsUTF8(NewLFMFilename) then begin
          // the new file has a lfm, so it is safe to delete the old
          // (if NewLFMFilename does not exist, it didn't belong to the unit
          //  or there was an error during delete. Never delete files in doubt.)
          OldLFMFilename:=ChangeFileExt(OldFilename,'.lfm');
          if FileExistsUTF8(OldLFMFilename) then begin
            Result:=DeleteFileInteractive(OldLFMFilename,[mbAbort]);
            if Result=mrAbort then exit;
          end;
        end;
        // delete old lrs
        if (LRSCode<>nil) and FileExistsUTF8(LRSCode.Filename) then begin
          // the new file has a lrs, so it is safe to delete the old
          // (if the new lrs does not exist, it didn't belong to the unit
          //  or there was an error during delete. Never delete files in doubt.)
          OldLRSFilename:=ChangeFileExt(OldFilename,ResourceFileExt);
          if FileExistsUTF8(OldLRSFilename) then begin
            Result:=DeleteFileInteractive(OldLRSFilename,[mbAbort]);
            if Result=mrAbort then exit;
          end;
        end;
        // delete ppu in source directory
        OldPPUFilename:=ChangeFileExt(OldFilename,'.ppu');
        if FileExistsUTF8(OldPPUFilename) then begin
          Result:=DeleteFileInteractive(OldPPUFilename,[mbAbort]);
          if Result=mrAbort then exit;
        end;
        OldPPUFilename:=ChangeFileExt(OldPPUFilename,'.o');
        if FileExistsUTF8(OldPPUFilename) then begin
          Result:=DeleteFileInteractive(OldPPUFilename,[mbAbort]);
          if Result=mrAbort then exit;
        end;
        Owners:=PkgBoss.GetOwnersOfUnit(NewFilename);
        try
          if Owners<>nil then begin
            for i:=0 to Owners.Count-1 do begin
              OutDir:='';
              if TObject(Owners[i]) is TProject then begin
                // delete old files in project output directory
                OutDir:=TProject(Owners[i]).CompilerOptions.GetUnitOutPath(false);
              end else if TObject(Owners[i]) is TLazPackage then begin
                // delete old files in package output directory
                OutDir:=TLazPackage(Owners[i]).CompilerOptions.GetUnitOutPath(false);
              end;
              if (OutDir<>'') and FilenameIsAbsolute(OutDir) then begin
                OldPPUFilename:=AppendPathDelim(OutDir)+ChangeFileExt(ExtractFilenameOnly(OldFilename),'.ppu');
                if FileExistsUTF8(OldPPUFilename) then begin
                  Result:=DeleteFileInteractive(OldPPUFilename,[mbAbort]);
                  if Result=mrAbort then exit;
                end;
                OldPPUFilename:=ChangeFileExt(OldPPUFilename,'.o');
                if FileExistsUTF8(OldPPUFilename) then begin
                  Result:=DeleteFileInteractive(OldPPUFilename,[mbAbort]);
                  if Result=mrAbort then exit;
                end;
                OldLRSFilename:=ChangeFileExt(OldPPUFilename,ResourceFileExt);
                if FileExistsUTF8(OldLRSFilename) then begin
                  Result:=DeleteFileInteractive(OldLRSFilename,[mbAbort]);
                  if Result=mrAbort then exit;
                end;
              end;
            end;
          end;
        finally
          Owners.Free;
        end;
      end;
    end;

  finally
    Project1.EndUpdate;
  end;
  Result:=mrOk;
end;

function TLazSourceFileManager.OpenNotExistingFile(const AFileName: string;
  Flags: TOpenFlags): TModalResult;
var
  NewFlags: TNewFlags;
begin
  if ofProjectLoading in Flags then begin
    // this is a file, that was loaded last time, but was removed from disk
    Result:=IDEQuestionDialog(lisFileNotFound,
      Format(lisTheFileWasNotFoundIgnoreWillGoOnLoadingTheProject,
             ['"', AFilename, '"', LineEnding, LineEnding, LineEnding]),
      mtError, [mrIgnore, lisSkipFileAndContinueLoading,
                mrAbort, lisAbortLoadingProject]);
    exit;
  end;

  // Default to cancel
  Result:=mrCancel;
  if ofQuiet in Flags then Exit;

  if ofOnlyIfExists in Flags
  then begin
    IDEMessageDialog(lisFileNotFound,
      Format(lisFileNotFound2, ['"', AFilename, '"', LineEnding]),
      mtInformation,[mbCancel]);
    // cancel loading file
    Exit;
  end;

  if IDEMessageDialog(lisFileNotFound,
    Format(lisFileNotFoundDoYouWantToCreateIt, ['"',AFilename,'"',LineEnding,LineEnding]),
    mtInformation,[mbYes,mbNo])=mrYes then
  begin
    // create new file
    NewFlags:=[nfOpenInEditor,nfCreateDefaultSrc];
    if ofAddToProject in Flags then
      Include(NewFlags,nfIsPartOfProject);
    if FilenameIsPascalSource(AFilename) then
      Result:=MainIDE.DoNewEditorFile(FileDescriptorUnit,AFilename,'',NewFlags)
    else
      Result:=MainIDE.DoNewEditorFile(FileDescriptorText,AFilename,'',NewFlags);
  end;
end;

function TLazSourceFileManager.OpenUnknownFile(const AFileName: string; Flags: TOpenFlags;
  var NewUnitInfo: TUnitInfo; var Handled: boolean): TModalResult;
var
  Ext, NewProgramName, LPIFilename, ACaption, AText: string;
  PreReadBuf: TCodeBuffer;
  LoadFlags: TLoadBufferFlags;
  SourceType: String;
begin
  Handled:=false;
  Ext:=lowercase(ExtractFileExt(AFilename));

  if ([ofProjectLoading,ofRegularFile]*Flags=[]) and (MainIDE.ToolStatus=itNone)
  and (Ext='.lpi') then begin
    // this is a project info file -> load whole project
    Handled:=true;
    Result:=MainIDE.DoOpenProjectFile(AFilename,[ofAddToRecent]);
    exit;
  end;

  // load the source
  LoadFlags := [lbfCheckIfText,lbfUpdateFromDisk,lbfRevert];
  if ofQuiet in Flags then Include(LoadFlags, lbfQuiet);
  Result:=LoadCodeBuffer(PreReadBuf,AFileName,LoadFlags,true);
  if Result<>mrOk then exit;
  NewUnitInfo:=nil;

  // check if unit is a program
  if ([ofProjectLoading,ofRegularFile]*Flags=[])
  and FilenameIsPascalSource(AFilename) then begin
    SourceType:=CodeToolBoss.GetSourceType(PreReadBuf,false);
    if (SysUtils.CompareText(SourceType,'PROGRAM')=0)
    or (SysUtils.CompareText(SourceType,'LIBRARY')=0)
    then begin
      NewProgramName:=CodeToolBoss.GetSourceName(PreReadBuf,false);
      if NewProgramName<>'' then begin
        // source is a program
        // either this is a lazarus project or it is not yet a lazarus project ;)
        LPIFilename:=ChangeFileExt(AFilename,'.lpi');
        if FileExistsUTF8(LPIFilename) then begin
          if IDEQuestionDialog(lisProjectInfoFileDetected,
            Format(lisTheFileSeemsToBeTheProgramFileOfAnExistingLazarusP,
                   [AFilename]), mtConfirmation,
              [mrOk, lisOpenProject2, mrCancel, lisOpenTheFileAsNormalSource])=mrOk then
          begin
            Handled:=true;
            Result:=MainIDE.DoOpenProjectFile(LPIFilename,[ofAddToRecent]);
            exit;
          end;
        end else begin
          AText:=Format(lisTheFileSeemsToBeAProgramCloseCurrentProject,
                        ['"', AFilename, '"', LineEnding, LineEnding]);
          ACaption:=lisProgramDetected;
          if IDEMessageDialog(ACaption, AText, mtConfirmation,
              [mbYes, mbNo])=mrYes then
          begin
            Handled:=true;
            Result:=CreateProjectForProgram(PreReadBuf);
            exit;
          end;
        end;
      end;
    end;
  end;
  NewUnitInfo:=TUnitInfo.Create(PreReadBuf);
  if FilenameIsPascalSource(NewUnitInfo.Filename) then
    NewUnitInfo.ReadUnitNameFromSource(true);
  Project1.AddFile(NewUnitInfo,false);
  if (ofAddToProject in Flags) and (not NewUnitInfo.IsPartOfProject) then
  begin
    NewUnitInfo.IsPartOfProject:=true;
    Project1.Modified:=true;
  end;
  Result:=mrOk;
end;

function TLazSourceFileManager.OpenMainUnit(PageIndex, WindowIndex: integer;
  Flags: TOpenFlags; UseWindowID: Boolean): TModalResult;
var
  MainUnitInfo: TUnitInfo;
begin
  {$IFDEF IDE_VERBOSE}
  debugln(['[TLazSourceFileManager.DoOpenMainUnit] A ProjectLoading=',ofProjectLoading in Flags,' MainUnitID=',Project1.MainUnitID]);
  {$ENDIF}
  Result:=mrCancel;
  if (Project1=nil) or (Project1.MainUnitID<0) then exit;
  MainUnitInfo:=Project1.MainUnitInfo;

  // check if main unit is already open in source editor
  if (MainUnitInfo.OpenEditorInfoCount > 0) and (not (ofProjectLoading in Flags)) then
  begin
    // already loaded -> switch to source editor
    SourceEditorManager.ActiveEditor := TSourceEditor(MainUnitInfo.OpenEditorInfo[0].EditorComponent);
    SourceEditorManager.ShowActiveWindowOnTop(True);
    Result:=mrOk;
    exit;
  end;

  // open file in source notebook
  Result:=OpenFileInSourceEditor(MainUnitInfo.GetClosedOrNewEditorInfo,
                                 PageIndex,WindowIndex,Flags,UseWindowID);
  if Result<>mrOk then exit;

  Result:=mrOk;
  {$IFDEF IDE_VERBOSE}
  writeln('[TLazSourceFileManager.DoOpenMainUnit] END');
  {$ENDIF}
end;

function TLazSourceFileManager.RevertMainUnit: TModalResult;
begin
  Result:=mrOk;
  if Project1.MainUnitID<0 then exit;
  if Project1.MainUnitInfo.OpenEditorInfoCount > 0 then
    // main unit is loaded, so we can just revert
    Result:=OpenEditorFile('',Project1.MainUnitInfo.EditorInfo[0].PageIndex,
      Project1.MainUnitInfo.EditorInfo[0].WindowID, nil, [ofRevert], True)
  else begin
    // main unit is only loaded in background
    // -> just reload the source and update the source name
    Result:=Project1.MainUnitInfo.ReadUnitSource(true,true);
  end;
end;

function TLazSourceFileManager.CheckLFMInEditor(
  LFMUnitInfo: TUnitInfo; Quiet: boolean): TModalResult;
var
  LFMChecker: TLFMChecker;
  UnitFilename: String;
  PascalBuf: TCodeBuffer;
  i: integer;
  LFMFilename: String;
  SrcEdit: TSourceEditor;
begin
  if (LFMUnitInfo<>nil)
  and FilenameIsPascalUnit(LFMUnitInfo.Filename) then begin
    LFMFilename:=ChangeFileExt(LFMUnitInfo.Filename,'.lfm');
    if FileExistsInIDE(LFMFilename,[])
    and (OpenEditorFile(LFMFilename,-1,-1,nil,[])=mrOk)
    and (SourceEditorManager.ActiveEditor<>nil)
    then begin
      SrcEdit:=SourceEditorManager.ActiveEditor;
      LFMUnitInfo:=Project1.UnitInfoWithFilename(SrcEdit.FileName);
    end;
  end;

  // check, if a .lfm file is opened in the source editor
  if (LFMUnitInfo=nil) or
    ((CompareFileExt(LFMUnitInfo.Filename,'.lfm',false)<>0) and
     (CompareFileExt(LFMUnitInfo.Filename,'.dfm',false)<>0)) then
  begin
    if not Quiet then
    begin
      IDEMessageDialog(lisNoLFMFile,
        lisThisFunctionNeedsAnOpenLfmFileInTheSourceEditor,
        mtError,[mbCancel]);
    end;
    Result:=mrCancel;
    exit;
  end;
  // try to find the pascal unit
  for i:=Low(PascalFileExt) to High(PascalFileExt) do begin
    UnitFilename:=ChangeFileExt(LFMUnitInfo.Filename,PascalFileExt[i]);
    if FileExistsUTF8(UnitFilename) then
      break
    else
      UnitFilename:='';
  end;
  if UnitFilename='' then begin
    IDEMessageDialog(lisNoPascalFile,
      Format(lisUnableToFindPascalUnitPasPpForLfmFile,
             [LineEnding, '"', LFMUnitInfo.Filename, '"']),
      mtError,[mbCancel]);
    Result:=mrCancel;
    exit;
  end;

  if MainIDE.ToolStatus<>itNone then begin
    DebugLn(['TLazSourceFileManager.DoCheckLFMInEditor ToolStatus<>itNone']);
    Result:=mrCancel;
    exit;
  end;
  // load the pascal unit
  SaveSourceEditorChangesToCodeCache(nil);
  Result:=LoadCodeBuffer(PascalBuf,UnitFilename,[],false);
  if Result<>mrOk then exit;

  // open messages window
  SourceEditorManager.ClearErrorLines;
  if MessagesView<>nil then
    MessagesView.Clear;
  ArrangeSourceEditorAndMessageView(false);

  // parse the LFM file and the pascal unit
  LFMChecker:=TLFMChecker.Create(PascalBuf,LFMUnitInfo.Source);
  try
    LFMChecker.ShowMessages:=true;
    LFMChecker.RootMustBeClassInUnit:=true;
    LFMChecker.RootMustBeClassInIntf:=true;
    LFMChecker.ObjectsMustExist:=true;
    if LFMChecker.Repair=mrOk then begin
      if not Quiet then begin
        IDEMessageDialog(lisLFMIsOk,
          lisClassesAndPropertiesExistValuesWereNotChecked,
          mtInformation,[mbOk],'');
      end;
    end else begin
      MainIDE.DoJumpToCompilerMessage(true);
      Result:=mrAbort;
      exit;
    end;
  finally
    LFMChecker.Free;
  end;
  Result:=mrOk;
end;

function TLazSourceFileManager.OpenFileInSourceEditor(AnEditorInfo: TUnitEditorInfo;
  PageIndex, WindowIndex: integer; Flags: TOpenFlags; UseWindowID: Boolean): TModalResult;
var
  NewSrcEdit: TSourceEditor;
  AFilename: string;
  NewCaretXY: TPoint;
  NewTopLine: LongInt;
  NewLeftChar: LongInt;
  NewErrorLine: LongInt;
  NewExecutionLine: LongInt;
  FoldState: String;
  SrcNotebook: TSourceNotebook;
  AnUnitInfo: TUnitInfo;
  AShareEditor: TSourceEditor;
begin
  //debugln(['TLazSourceFileManager.OpenFileInSourceEditor ',AnEditorInfo.UnitInfo.Filename,' Window=',WindowIndex,'/',SourceEditorManager.SourceWindowCount,' Page=',PageIndex]);
  AnUnitInfo := AnEditorInfo.UnitInfo;
  AFilename:=AnUnitInfo.Filename;
  if (WindowIndex < 0) then
    SrcNotebook := SourceEditorManager.ActiveOrNewSourceWindow
  else
  if UseWindowID then begin
    SrcNotebook := SourceEditorManager.SourceWindowWithID(WindowIndex);
    WindowIndex := SourceEditorManager.IndexOfSourceWindow(SrcNotebook);
  end
  else
  if (WindowIndex >= SourceEditorManager.SourceWindowCount) then begin
    SrcNotebook := SourceEditorManager.NewSourceWindow;
  end
  else
    SrcNotebook := SourceEditorManager.SourceWindows[WindowIndex];

  // get syntax highlighter type
  if (uifInternalFile in AnUnitInfo.Flags) then
    AnUnitInfo.UpdateDefaultHighlighter(lshFreePascal)
  else
    AnUnitInfo.UpdateDefaultHighlighter(FilenameToLazSyntaxHighlighter(AFilename));

  SrcNotebook.IncUpdateLock;
  try
    //DebugLn(['TLazSourceFileManager.OpenFileInSourceEditor Revert=',ofRevert in Flags,' ',AnUnitInfo.Filename,' PageIndex=',PageIndex]);
    if (not (ofRevert in Flags)) or (PageIndex<0) then begin
      // create a new source editor

      // update marks and cursor positions in Project1, so that merging the old
      // settings during restoration will work
      SaveSourceEditorProjectSpecificSettings;
      AShareEditor := nil;
      if AnUnitInfo.OpenEditorInfoCount > 0 then
        AShareEditor := TSourceEditor(AnUnitInfo.OpenEditorInfo[0].EditorComponent);
      NewSrcEdit:=SrcNotebook.NewFile(
        CreateSrcEditPageName(AnUnitInfo.Unit_Name, AFilename, AShareEditor),
        AnUnitInfo.Source, False, AShareEditor);
      NewSrcEdit.EditorComponent.BeginUpdate;
      MainIDEBar.itmFileClose.Enabled:=True;
      MainIDEBar.itmFileCloseAll.Enabled:=True;
      NewCaretXY := AnEditorInfo.CursorPos;
      NewTopLine := AnEditorInfo.TopLine;
      FoldState := AnEditorInfo.FoldState;
      NewLeftChar:=1;
      NewErrorLine:=-1;
      NewExecutionLine:=-1;
    end else begin
      // revert code in existing source editor
      NewSrcEdit:=SourceEditorManager.SourceEditorsByPage[WindowIndex, PageIndex];
      NewCaretXY:=NewSrcEdit.EditorComponent.CaretXY;
      NewTopLine:=NewSrcEdit.EditorComponent.TopLine;
      FoldState := NewSrcEdit.EditorComponent.FoldState;
      NewLeftChar:=NewSrcEdit.EditorComponent.LeftChar;
      NewErrorLine:=NewSrcEdit.ErrorLine;
      NewExecutionLine:=NewSrcEdit.ExecutionLine;
      NewSrcEdit.EditorComponent.BeginUpdate;
      if NewSrcEdit.CodeBuffer=AnUnitInfo.Source then begin
        AnUnitInfo.Source.AssignTo(NewSrcEdit.EditorComponent.Lines,true);
      end else
        NewSrcEdit.CodeBuffer:=AnUnitInfo.Source;
      AnUnitInfo.ClearModifieds;
      //DebugLn(['TLazSourceFileManager.OpenFileInSourceEditor NewCaretXY=',dbgs(NewCaretXY),' NewTopLine=',NewTopLine]);
    end;

    NewSrcEdit.IsLocked := AnEditorInfo.IsLocked;
    AnEditorInfo.EditorComponent := NewSrcEdit;
    //debugln(['TLazSourceFileManager.OpenFileInSourceEditor ',AnUnitInfo.Filename,' ',AnUnitInfo.EditorIndex]);

    // restore source editor settings
    DebugBoss.DoRestoreDebuggerMarks(AnUnitInfo);
    NewSrcEdit.SyntaxHighlighterType := AnEditorInfo.SyntaxHighlighter;
    NewSrcEdit.EditorComponent.AfterLoadFromFile;
    try
      NewSrcEdit.EditorComponent.FoldState := FoldState;
    except
      IDEMessageDialog(lisError, lisFailedToLoadFoldStat, mtError, [mbOK]);
    end;

    NewSrcEdit.EditorComponent.CaretXY:=NewCaretXY;
    NewSrcEdit.EditorComponent.TopLine:=NewTopLine;
    NewSrcEdit.EditorComponent.LeftChar:=NewLeftChar;
    NewSrcEdit.ErrorLine:=NewErrorLine;
    NewSrcEdit.ExecutionLine:=NewExecutionLine;
    NewSrcEdit.ReadOnly:=AnUnitInfo.ReadOnly;
    NewSrcEdit.Modified:=false;

    // mark unit as loaded
    NewSrcEdit.EditorComponent.EndUpdate;
    AnUnitInfo.Loaded:=true;
  finally
    SrcNotebook.DecUpdateLock;
  end;

  // update statusbar and focus editor
  if (not (ofProjectLoading in Flags)) then begin
    SourceEditorManager.ActiveEditor := NewSrcEdit;
    SourceEditorManager.ShowActiveWindowOnTop(True);
  end;
  SrcNoteBook.UpdateStatusBar;
  SrcNotebook.BringToFront;

  Result:=mrOk;
end;

function TLazSourceFileManager.LoadResourceFile(AnUnitInfo: TUnitInfo;
  var LFMCode, LRSCode: TCodeBuffer;
  IgnoreSourceErrors, AutoCreateResourceCode, ShowAbort: boolean): TModalResult;
var
  LFMFilename: string;
  LRSFilename: String;
  ResType: TResourceType;
begin
  LFMCode:=nil;
  LRSCode:=nil;
  //DebugLn(['TLazSourceFileManager.LoadResourceFile ',AnUnitInfo.Filename,' HasResources=',AnUnitInfo.HasResources,' IgnoreSourceErrors=',IgnoreSourceErrors,' AutoCreateResourceCode=',AutoCreateResourceCode]);
  // Load the lfm file (without parsing)
  if not AnUnitInfo.IsVirtual then begin  // and (AnUnitInfo.Component<>nil)
    LFMFilename:=AnUnitInfo.UnitResourceFileformat.GetUnitResourceFilename(AnUnitInfo.Filename,true);
    if (FileExistsCached(LFMFilename)) then begin
      Result:=LoadCodeBuffer(LFMCode,LFMFilename,[lbfCheckIfText],ShowAbort);
      if not (Result in [mrOk,mrIgnore]) then
        exit;
    end;
  end;
  if AnUnitInfo.HasResources then begin
    //writeln('TLazSourceFileManager.LoadResourceFile A "',AnUnitInfo.Filename,'" "',AnUnitInfo.ResourceFileName,'"');
    ResType:=MainBuildBoss.GetResourceType(AnUnitInfo);
    if ResType=rtLRS then begin
      LRSFilename:=MainBuildBoss.FindLRSFilename(AnUnitInfo,false);
      if LRSFilename<>'' then begin
        Result:=LoadCodeBuffer(LRSCode,LRSFilename,[lbfUpdateFromDisk],ShowAbort);
        if Result<>mrOk then exit;
      end else begin
        LRSFilename:=MainBuildBoss.GetDefaultLRSFilename(AnUnitInfo);
        if AutoCreateResourceCode then begin
          LRSCode:=CodeToolBoss.CreateFile(LRSFilename);
        end else begin
          DebugLn(['TLazSourceFileManager.LoadResourceFile .lrs file not found of unit ',AnUnitInfo.Filename]);
          exit(mrCancel);
        end;
      end;
    end else begin
      LRSFilename:='';
      LRSCode:=nil;
    end;
    // if no resource file found (i.e. normally the .lrs file)
    // don't bother the user, because it is created automatically anyway
  end;
  Result:=mrOk;
end;

function TLazSourceFileManager.LoadLFM(AnUnitInfo: TUnitInfo;
  OpenFlags: TOpenFlags; CloseFlags: TCloseFlags): TModalResult;
// if there is a .lfm file, open the resource
var
  UnitResourceFilename: string;
  UnitResourceFileformat: TUnitResourcefileFormatClass;
  LFMBuf: TCodeBuffer;
  CanAbort: boolean;
begin
  CanAbort:=[ofProjectLoading,ofMultiOpen]*OpenFlags<>[];

  UnitResourceFileformat:=AnUnitInfo.UnitResourceFileformat;
  // Note: think about virtual and normal .lfm files.
  UnitResourceFilename:=UnitResourceFileformat.GetUnitResourceFilename(AnUnitInfo.Filename,true);
  LFMBuf:=nil;
  if not FileExistsInIDE(UnitResourceFilename,[pfsfOnlyEditorFiles]) then begin
    // there is no LFM file -> ok
    {$IFDEF IDE_DEBUG}
    debugln('TLazSourceFileManager.LoadLFM there is no LFM file for "',AnUnitInfo.Filename,'"');
    {$ENDIF}
    Result:=mrOk;
    exit;
  end;

  // there is a lazarus form text file -> load it
  Result:=LoadIDECodeBuffer(LFMBuf,UnitResourceFilename,[lbfUpdateFromDisk],CanAbort);
  if Result<>mrOk then begin
    DebugLn(['TLazSourceFileManager.LoadLFM LoadIDECodeBuffer failed']);
    exit;
  end;

  Result:=LoadLFM(AnUnitInfo,LFMBuf,OpenFlags,CloseFlags);
end;

function TLazSourceFileManager.LoadLFM(AnUnitInfo: TUnitInfo; LFMBuf: TCodeBuffer;
  OpenFlags: TOpenFlags; CloseFlags: TCloseFlags): TModalResult;
const
  BufSize = 4096; // allocating mem in 4k chunks helps many mem managers

  ShowCommands: array[TWindowState] of Integer =
    (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED, SW_SHOWFULLSCREEN);

var
  TxtLFMStream, BinStream: TExtMemoryStream;
  NewComponent: TComponent;
  AncestorType: TComponentClass;
  DesignerForm: TCustomForm;
  NewClassName: String;
  LFMType: String;
  ACaption, AText: String;
  NewUnitName: String;
  AncestorUnitInfo, NestedUnitInfo, LFMUnitInfo: TUnitInfo;
  ReferencesLocked: Boolean;
  LCLVersion: string;
  MissingClasses: TStrings;
  LFMComponentName: string;
  i: Integer;
  NestedClassName: string;
  NestedClass: TComponentClass;
  DisableAutoSize: Boolean;
  NewControl: TControl;
  ARestoreVisible: Boolean;
  AncestorClass: TComponentClass;
begin
  {$IFDEF IDE_DEBUG}
  debugln('TLazSourceFileManager.LoadLFM A ',AnUnitInfo.Filename,' IsPartOfProject=',dbgs(AnUnitInfo.IsPartOfProject),' ');
  {$ENDIF}

  ReferencesLocked:=false;
  MissingClasses:=nil;
  NewComponent:=nil;
  try
    if (ofRevert in OpenFlags) and (AnUnitInfo.Component<>nil) then begin
      // the component must be destroyed and recreated => store references
      ReferencesLocked:=true;
      Project1.LockUnitComponentDependencies;
      Project1.UpdateUnitComponentDependencies;

      // close old designer form
      Result:=CloseUnitComponent(AnUnitInfo,CloseFlags);
      if Result<>mrOk then begin
        DebugLn(['TLazSourceFileManager.LoadLFM CloseUnitComponent failed']);
        exit;
      end;
    end;

    // check installed packages
    if EnvironmentOptions.CheckPackagesOnFormCreate and
       (AnUnitInfo.Component = nil) and
        AnUnitInfo.IsPartOfProject and
       (not (ofProjectLoading in OpenFlags)) then
    begin
      // opening a form of the project -> check installed packages
      Result := PkgBoss.CheckProjectHasInstalledPackages(Project1,
                                       OpenFlags * [ofProjectLoading, ofQuiet] = []);
      if not (Result in [mrOk, mrIgnore]) then
      begin
        DebugLn(['TLazSourceFileManager.LoadLFM PkgBoss.CheckProjectHasInstalledPackages failed']);
        exit;
      end;
    end;
    {$IFDEF VerboseLFMSearch}
    debugln('TLazSourceFileManager.LoadLFM LFM file loaded, parsing "',LFMBuf.Filename,'" ...');
    {$ENDIF}

    // someone created a .lfm file -> Update HasResources
    AnUnitInfo.HasResources:=true;

    // find the classname of the LFM, and check for inherited form
    AnUnitInfo.UnitResourceFileformat.QuickCheckResourceBuffer(AnUnitInfo.Source,LFMBuf,LFMType,LFMComponentName,
                        NewClassName,LCLVersion,MissingClasses);

    {$IFDEF VerboseLFMSearch}
    debugln('TLazSourceFileManager.LoadLFM LFM="',LFMBuf.Source,'"');
    {$ENDIF}
    if AnUnitInfo.Component=nil then begin
      // load/create new instance

      if (NewClassName='') or (LFMType='') then begin
        DebugLn(['TLazSourceFileManager.LoadLFM LFM file corrupt']);
        Result:=IDEMessageDialog(lisLFMFileCorrupt,
          Format(lisUnableToFindAValidClassnameIn, ['"', LFMBuf.Filename, '"']),
          mtError,[mbIgnore,mbCancel,mbAbort]);
        exit;
      end;

      // load missing component classes (e.g. ancestor and frames)
      Result:=LoadAncestorDependencyHidden(AnUnitInfo,NewClassName,OpenFlags,
                                             AncestorType,AncestorUnitInfo);
      if Result<>mrOk then begin
        DebugLn(['TLazSourceFileManager.LoadLFM DoLoadAncestorDependencyHidden failed for ',AnUnitInfo.Filename]);
        exit;
      end;

      if MissingClasses<>nil then begin
        {$IFDEF VerboseLFMSearch}
        DebugLn(['TLazSourceFileManager.LoadLFM has nested: ',AnUnitInfo.Filename]);
        {$ENDIF}
        for i:=MissingClasses.Count-1 downto 0 do begin
          NestedClassName:=MissingClasses[i];
          {$IFDEF VerboseLFMSearch}
          DebugLn(['TLazSourceFileManager.LoadLFM nested ',i,' ',MissingClasses.Count,': ',NestedClassName]);
          {$ENDIF}
          if SysUtils.CompareText(NestedClassName,AncestorType.ClassName)=0 then
          begin
            MissingClasses.Delete(i);
          end else begin
            DebugLn(['TLazSourceFileManager.LoadLFM loading nested class ',NestedClassName,' needed by ',AnUnitInfo.Filename]);
            NestedClass:=nil;
            NestedUnitInfo:=nil;
            Result:=LoadComponentDependencyHidden(AnUnitInfo,NestedClassName,
                      OpenFlags,
                      {$IFDEF EnableNestedComponentsWithoutLFM}
                      false,
                      {$ELSE}
                      true,
                      {$ENDIF}
                      NestedClass,NestedUnitInfo,AncestorClass);
            if Result<>mrOk then begin
              DebugLn(['TLazSourceFileManager.LoadLFM DoLoadComponentDependencyHidden NestedClassName=',NestedClassName,' failed for ',AnUnitInfo.Filename]);
              exit;
            end;
            if NestedClass<>nil then
              MissingClasses.Objects[i]:=TObject(Pointer(NestedClass))
            else if AncestorClass<>nil then
              MissingClasses.Objects[i]:=TObject(Pointer(AncestorClass));
          end;
        end;
        //DebugLn(['TLazSourceFileManager.LoadLFM had nested: ',AnUnitInfo.Filename]);
        if AnUnitInfo.ComponentFallbackClasses<>nil then begin
          AnUnitInfo.ComponentFallbackClasses.Free;
          AnUnitInfo.ComponentFallbackClasses:=nil;
        end;
        AnUnitInfo.ComponentFallbackClasses:=MissingClasses;
        MissingClasses:=nil;
      end;

      BinStream:=nil;
      try
        // convert text to binary format
        BinStream:=TExtMemoryStream.Create;
        TxtLFMStream:=TExtMemoryStream.Create;
        try
          {$IFDEF VerboseIDELFMConversion}
          DebugLn(['TLazSourceFileManager.LoadLFM LFMBuf START =======================================']);
          DebugLn(LFMBuf.Source);
          DebugLn(['TLazSourceFileManager.LoadLFM LFMBuf END   =======================================']);
          {$ENDIF}
          LFMBuf.SaveToStream(TxtLFMStream);
          AnUnitInfo.ComponentLastLFMStreamSize:=TxtLFMStream.Size;
          TxtLFMStream.Position:=0;

          try
            if AnUnitInfo.ComponentLastBinStreamSize>0 then
              BinStream.Capacity:=AnUnitInfo.ComponentLastBinStreamSize+BufSize;
            AnUnitInfo.UnitResourceFileformat.TextStreamToBinStream(TxtLFMStream, BinStream);
            AnUnitInfo.ComponentLastBinStreamSize:=BinStream.Size;
            BinStream.Position:=0;

            {$IFDEF VerboseIDELFMConversion}
            DebugLn(['TLazSourceFileManager.LoadLFM Binary START =======================================']);
            debugln(dbgMemStream(BinStream,BinStream.Size));
            DebugLn(['TLazSourceFileManager.LoadLFM Binary END   =======================================']);
            BinStream.Position:=0;
            {$ENDIF}

            Result:=mrOk;
          except
            on E: Exception do begin
              DumpExceptionBackTrace;
              ACaption:=lisFormatError;
              AText:=Format(lisUnableToConvertTextFormDataOfFileIntoBinaryStream,
                [LineEnding, '"', LFMBuf.Filename, '"', LineEnding, E.Message]);
              Result:=IDEMessageDialog(ACaption, AText, mtError, [mbOk, mbCancel]);
              if Result=mrCancel then Result:=mrAbort;
              exit;
            end;
          end;
        finally
          TxtLFMStream.Free;
        end;
        if ([ofProjectLoading,ofLoadHiddenResource]*OpenFlags=[]) then
          FormEditor1.ClearSelection;

        // create JIT component
        NewUnitName:=AnUnitInfo.Unit_Name;
        if NewUnitName='' then
          NewUnitName:=ExtractFileNameOnly(AnUnitInfo.Filename);
        DisableAutoSize:=true;
        NewComponent:=FormEditor1.CreateRawComponentFromStream(BinStream, AnUnitInfo.UnitResourceFileformat,
                   AncestorType,copy(NewUnitName,1,255),true,true,DisableAutoSize,AnUnitInfo);
        if (NewComponent is TControl) then begin
          NewControl:=TControl(NewComponent);
          if ofLoadHiddenResource in OpenFlags then
            NewControl.ControlStyle:=NewControl.ControlStyle+[csNoDesignVisible];
          if DisableAutoSize then
            NewControl.EnableAutoSizing;
          if NewComponent is TFrame then
            AnUnitInfo.ResourceBaseClass:=pfcbcFrame
          else if NewComponent is TDataModule then
            AnUnitInfo.ResourceBaseClass:=pfcbcDataModule
          else if NewComponent is TForm then
            AnUnitInfo.ResourceBaseClass:=pfcbcForm;
        end;
        Project1.InvalidateUnitComponentDesignerDependencies;
        AnUnitInfo.Component:=NewComponent;
        if (AncestorUnitInfo<>nil) then
          AnUnitInfo.AddRequiresComponentDependency(AncestorUnitInfo,[ucdtAncestor]);
        if NewComponent<>nil then begin
          // component loaded, now load the referenced units
          Result:=MainIDE.DoFixupComponentReferences(AnUnitInfo.Component,OpenFlags);
          if Result<>mrOk then begin
            DebugLn(['TLazSourceFileManager.LoadLFM DoFixupComponentReferences failed']);
            exit;
          end;
        end else begin
          // error streaming component -> examine lfm file
          DebugLn('ERROR: streaming failed lfm="',LFMBuf.Filename,'"');
          // open lfm file in editor
          if AnUnitInfo.OpenEditorInfoCount > 0 then
            Result:=OpenEditorFile(LFMBuf.Filename,
              AnUnitInfo.OpenEditorInfo[0].PageIndex+1,
              AnUnitInfo.OpenEditorInfo[0].WindowID, Nil,
              OpenFlags+[ofOnlyIfExists,ofQuiet,ofRegularFile], True)
          else
            Result:=OpenEditorFile(LFMBuf.Filename, -1, -1, nil,
              OpenFlags+[ofOnlyIfExists,ofQuiet,ofRegularFile]);
          if Result<>mrOk then begin
            DebugLn(['TLazSourceFileManager.LoadLFM DoOpenEditorFile failed']);
            exit;
          end;
          LFMUnitInfo:=Project1.UnitWithEditorComponent(SourceEditorManager.ActiveEditor);
          Result:=CheckLFMInEditor(LFMUnitInfo, true);
          if Result=mrOk then Result:=mrCancel;
          exit;
        end;
      finally
        BinStream.Free;
      end;
    end else if SysUtils.CompareText(AnUnitInfo.Component.ClassName,NewClassName)<>0
    then begin
      // lfm and current designer are about different classes
      debugln(['TLazSourceFileManager.LoadLFM unit="',AnUnitInfo.Filename,'": loaded component has class "',AnUnitInfo.Component.ClassName,'", lfm has class "',NewClassName,'"']);
      // keep old instance, add a designer, so user can see current component
    end else begin
      // make hidden component visible, keep old instance, add a designer
      DebugLn(['TLazSourceFileManager.LoadLFM Creating designer for hidden component of ',AnUnitInfo.Filename]);
    end;
  finally
    MissingClasses.Free;
    if ReferencesLocked then begin
      if Project1<>nil then
        Project1.UnlockUnitComponentDependencies;
    end;
  end;

  NewComponent:=AnUnitInfo.Component;
  // create the designer (if not already done)
  if ([ofProjectLoading,ofLoadHiddenResource]*OpenFlags=[]) then
    FormEditor1.ClearSelection;
  {$IFDEF IDE_DEBUG}
  DebugLn('SUCCESS: streaming lfm="',LFMBuf.Filename,'"');
  {$ENDIF}
  AnUnitInfo.ComponentName:=NewComponent.Name;
  AnUnitInfo.ComponentResourceName:=AnUnitInfo.ComponentName;
  DesignerForm := nil;
  MainIDE.LastFormActivated := nil;
  if not (ofLoadHiddenResource in OpenFlags) then
  begin
    DesignerForm := FormEditor1.GetDesignerForm(NewComponent);
    if (DesignerForm=nil) or (DesignerForm.Designer=nil) then
      DesignerForm := MainIDE.CreateDesignerForComponent(AnUnitInfo,NewComponent);
  end;

  // select the new form (object inspector, formeditor, control selection)
  if (DesignerForm <> nil)
  and ([ofProjectLoading,ofLoadHiddenResource] * OpenFlags=[]) then
  begin
    MainIDE.DisplayState := dsForm;
    GlobalDesignHook.LookupRoot := NewComponent;
    TheControlSelection.AssignPersistent(NewComponent);
    MainIDE.CreateObjectInspector;
  end;

  // show new form
  if DesignerForm <> nil then
  begin
    DesignerForm.ControlStyle := DesignerForm.ControlStyle - [csNoDesignVisible];
    if NewComponent is TControl then
      TControl(NewComponent).ControlStyle:= TControl(NewComponent).ControlStyle - [csNoDesignVisible];
    if (DesignerForm.WindowState in [wsMinimized]) then
    begin
      ARestoreVisible := DesignerForm.Visible;
      DesignerForm.Visible := False;
      DesignerForm.ShowOnTop;
      DesignerForm.Visible := ARestoreVisible;
      DesignerForm.WindowState := wsMinimized;
    end else
      LCLIntf.ShowWindow(DesignerForm.Handle, ShowCommands[AnUnitInfo.ComponentState]);
    MainIDE.LastFormActivated := DesignerForm;
  end;

  {$IFDEF IDE_DEBUG}
  debugln('[TLazSourceFileManager.LoadLFM] LFM end');
  {$ENDIF}
  Result:=mrOk;
end;

function TLazSourceFileManager.OpenComponent(const UnitFilename: string;
  OpenFlags: TOpenFlags; CloseFlags: TCloseFlags; out Component: TComponent): TModalResult;
var
  AnUnitInfo: TUnitInfo;
  LFMFilename: String;
  UnitCode: TCodeBuffer;
  LFMCode: TCodeBuffer;
  AFilename: String;
begin
  if Project1=nil then exit(mrCancel);
  // try to find a unit name without expaning the path. this is required if unit is virtual
  // in other case file name will be expanded with the wrong path
  AFilename := UnitFilename;
  AnUnitInfo:=Project1.UnitInfoWithFilename(AFilename);
  if AnUnitInfo = nil then
  begin
    AFilename:=TrimAndExpandFilename(UnitFilename);
    if (AFilename='') or (not FileExistsInIDE(AFilename,[])) then begin
      DebugLn(['TLazSourceFileManager.DoOpenComponent file not found ',AFilename]);
      exit(mrCancel);
    end;
    AnUnitInfo:=Project1.UnitInfoWithFilename(AFilename);
  end;
  if (not (ofRevert in OpenFlags))
  and (AnUnitInfo<>nil) and (AnUnitInfo.Component<>nil) then begin
    // already open
    Component:=AnUnitInfo.Component;
    Result:=mrOk;
    exit;
  end;

  // ToDo: use UnitResources
  LFMFilename:=ChangeFileExt(AFilename,'.lfm');
  if not FileExistsInIDE(LFMFilename,[]) then
    LFMFilename:=ChangeFileExt(AFilename,'.dfm');
  if not FileExistsInIDE(LFMFilename,[]) then begin
    DebugLn(['TLazSourceFileManager.DoOpenComponent file not found ',LFMFilename]);
    exit(mrCancel);
  end;

  // load unit source
  Result:=LoadCodeBuffer(UnitCode,AFilename,[lbfCheckIfText],true);
  if Result<>mrOk then begin
    debugln('TLazSourceFileManager.DoOpenComponent Failed loading ',AFilename);
    exit;
  end;

  // create unit info
  if AnUnitInfo=nil then begin
    AnUnitInfo:=TUnitInfo.Create(UnitCode);
    AnUnitInfo.ReadUnitNameFromSource(true);
    Project1.AddFile(AnUnitInfo,false);
  end;

  // load lfm source
  Result:=LoadCodeBuffer(LFMCode,LFMFilename,[lbfCheckIfText],true);
  if Result<>mrOk then begin
    debugln('TLazSourceFileManager.DoOpenComponent Failed loading ',LFMFilename);
    exit;
  end;

  // load resource
  Result:=LoadLFM(AnUnitInfo,LFMCode,OpenFlags,CloseFlags);
  if Result<>mrOk then begin
    debugln('TLazSourceFileManager.DoOpenComponent DoLoadLFM failed ',LFMFilename);
    exit;
  end;

  Component:=AnUnitInfo.Component;
  if Component<>nil then
    Result:=mrOk
  else
    Result:=mrCancel;
end;

function TLazSourceFileManager.FindBaseComponentClass(const AComponentClassName,
  DescendantClassName: string;
  out AComponentClass: TComponentClass): boolean;
// returns false if an error occured
// Important: returns true even if AComponentClass=nil
begin
  // find the ancestor class
  if AComponentClassName<>'' then begin
    if (DescendantClassName<>'')
    and (SysUtils.CompareText(AComponentClassName,'TCustomForm')=0) then begin
      // this is a common user mistake
      IDEMessageDialog(lisCodeTemplError, Format(
        lisTheResourceClassDescendsFromProbablyThisIsATypoFor, ['"',
        DescendantClassName, '"', '"', AComponentClassName, '"']),
        mtError,[mbCancel]);
      Result:=false;
      exit;
    end else if (DescendantClassName<>'')
    and (SysUtils.CompareText(AComponentClassName,'TComponent')=0) then begin
      // this is not yet implemented
      IDEMessageDialog(lisCodeTemplError,
        Format(lisUnableToOpenDesignerTheClassDoesNotDescendFromADes,
               [LineEnding, DescendantClassName]),
        mtError,[mbCancel]);
      Result:=false;
      exit;
    end else begin
      // search in the registered base classes
      AComponentClass:=FormEditor1.FindDesignerBaseClassByName(AComponentClassName,true);
    end;
  end else begin
    // default is TForm
    AComponentClass:=TForm;
  end;
  Result:=true;
end;

function TLazSourceFileManager.LoadAncestorDependencyHidden(AnUnitInfo: TUnitInfo;
  const DescendantClassName: string;
  OpenFlags: TOpenFlags;
  out AncestorClass: TComponentClass;
  out AncestorUnitInfo: TUnitInfo): TModalResult;
var
  AncestorClassName: String;
  CodeBuf: TCodeBuffer;
  GrandAncestorClass: TComponentClass;
begin
  AncestorClassName:='';
  AncestorClass:=nil;
  AncestorUnitInfo:=nil;

  // find the ancestor type in the source
  if AnUnitInfo.Source=nil then begin
    Result:=LoadCodeBuffer(CodeBuf,AnUnitInfo.Filename,
                           [lbfUpdateFromDisk,lbfCheckIfText],true);
    if Result<>mrOk then exit;
    AnUnitInfo.Source:=CodeBuf;
  end;
  if not CodeToolBoss.FindFormAncestor(AnUnitInfo.Source,DescendantClassName,
                                       AncestorClassName,true)
  then begin
    DebugLn('TLazSourceFileManager.LoadAncestorDependencyHidden Filename="',AnUnitInfo.Filename,'" ClassName=',DescendantClassName,'. Unable to find ancestor class: ',CodeToolBoss.ErrorMessage);
  end;

  // try the base designer classes
  if not FindBaseComponentClass(AncestorClassName,DescendantClassName,
    AncestorClass) then
  begin
    DebugLn(['TLazSourceFileManager.LoadAncestorDependencyHidden FindUnitComponentClass failed for AncestorClassName=',AncestorClassName]);
    exit(mrCancel);
  end;

  // try loading the ancestor first (unit, lfm and component instance)
  if (AncestorClass=nil) then begin
    Result:=LoadComponentDependencyHidden(AnUnitInfo,AncestorClassName,
             OpenFlags,false,AncestorClass,AncestorUnitInfo,GrandAncestorClass);
    if Result<>mrOk then begin
      DebugLn(['TLazSourceFileManager.LoadAncestorDependencyHidden DoLoadComponentDependencyHidden failed AnUnitInfo=',AnUnitInfo.Filename]);
    end;
    case  Result of
    mrAbort: exit;
    mrOk: ;
    mrIgnore:
      begin
        // use TForm as default
        AncestorClass:=TForm;
        AncestorUnitInfo:=nil;
      end;
    else
      // cancel
      Result:=mrCancel;
      exit;
    end;
  end;

  // use TForm as default ancestor
  if AncestorClass=nil then
    AncestorClass:=TForm;
  //DebugLn('TLazSourceFileManager.LoadAncestorDependencyHidden Filename="',AnUnitInfo.Filename,'" AncestorClassName=',AncestorClassName,' AncestorClass=',dbgsName(AncestorClass));
  Result:=mrOk;
end;

function TLazSourceFileManager.FindComponentClass(AnUnitInfo: TUnitInfo;
  const AComponentClassName: string; Quiet: boolean;
  out ComponentUnitInfo: TUnitInfo; out AComponentClass: TComponentClass; out
  LFMFilename: string; out AncestorClass: TComponentClass): TModalResult;
{ Possible results:
  mrOk:
   - AComponentClass<>nil and ComponentUnitInfo<>nil
      designer component
   - AComponentClass<>nil and ComponentUnitInfo=nil
      registered componentclass
   - LFMFilename<>''
      lfm of an used unit
   - AncestorClass<>nil
      componentclass does not exist, but the ancestor is a registered class
  mrCancel:
    not found
  mrAbort:
    not found, error already shown
}
var
  CTErrorMsg: String;
  CTErrorCode: TCodeBuffer;
  CTErrorLine: Integer;
  CTErrorCol: Integer;

  procedure StoreCodetoolsError;
  begin
    {$IFDEF VerboseLFMSearch}
    debugln(['  StoreCodetoolsError: ',CodeToolBoss.ErrorMessage]);
    {$ENDIF}
    if CTErrorMsg<>'' then exit;
    if CodeToolBoss.ErrorMessage<>'' then begin
      CTErrorMsg:=CodeToolBoss.ErrorMessage;
      CTErrorCode:=CodeToolBoss.ErrorCode;
      CTErrorLine:=CodeToolBoss.ErrorLine;
      CTErrorCol:=CodeToolBoss.ErrorColumn;
    end;
  end;

  function TryUnitComponent(const UnitFilename: string;
    out TheModalResult: TModalResult): boolean;
  // returns true if the unit contains the component class and sets
  // TheModalResult to the result of the loading
  var
    CurUnitInfo: TUnitInfo;
  begin
    {$IFDEF VerboseLFMSearch}
    debugln(['  TryUnitComponent UnitFilename="',UnitFilename,'"']);
    {$ENDIF}
    Result:=false;
    TheModalResult:=mrCancel;
    if not FilenameIsPascalUnit(UnitFilename) then exit;

    CurUnitInfo:=Project1.UnitInfoWithFilename(UnitFilename);
    if (CurUnitInfo=nil) or (CurUnitInfo.Component=nil) then exit;
    // unit with loaded component found -> check if it is the right one
    //DebugLn(['TLazSourceFileManager.FindComponentClass unit with a component found CurUnitInfo=',CurUnitInfo.Filename,' ',dbgsName(CurUnitInfo.Component)]);
    if SysUtils.CompareText(CurUnitInfo.Component.ClassName,AComponentClassName)<>0
    then exit;
    // component found (it was already loaded)
    ComponentUnitInfo:=CurUnitInfo;
    AComponentClass:=TComponentClass(ComponentUnitInfo.Component.ClassType);
    TheModalResult:=mrOk;
    Result:=true;
  end;

  function TryRegisteredClasses(aClassName: string;
    out FoundComponentClass: TComponentClass;
    out TheModalResult: TModalResult): boolean;
  var
    RegComp: TRegisteredComponent;
  begin
    {$IFDEF VerboseLFMSearch}
    debugln(['  TryRegisteredClasses aClassName="',aClassName,'"']);
    {$ENDIF}
    Result:=false;
    TheModalResult:=mrCancel;
    RegComp:=IDEComponentPalette.FindComponent(aClassName);
    if RegComp<>nil then
      FoundComponentClass:=RegComp.ComponentClass
    else
      FoundComponentClass:=FormEditor1.FindDesignerBaseClassByName(aClassName,true);
    if FoundComponentClass<>nil then begin
      DebugLn(['TLazSourceFileManager.FindComponentClass.TryRegisteredClasses found: ',FoundComponentClass.ClassName]);
      TheModalResult:=mrOk;
      Result:=true;
    end;
  end;

  function TryLFM(const UnitFilename, AClassName: string;
    out TheModalResult: TModalResult): boolean;
  var
    CurLFMFilename: String;
    LFMCode: TCodeBuffer;
    LFMClassName: String;
    LFMType: String;
  begin
    {$IFDEF VerboseLFMSearch}
    debugln(['  TryLFM UnitFilename="',UnitFilename,'" AClassName=',AClassName]);
    {$ENDIF}
    Result:=false;
    TheModalResult:=mrCancel;
    if not FilenameIsPascalSource(UnitFilename) then
    begin
      {$IFDEF VerboseLFMSearch}
      debugln(['  TryLFM UnitFilename="',UnitFilename,'" is not a unit']);
      {$ENDIF}
      exit;
    end;
    // ToDo: use UnitResources
    CurLFMFilename:=ChangeFileExt(UnitFilename,'.lfm');
    if not FileExistsCached(CurLFMFilename) then
    begin
      {$IFDEF VerboseLFMSearch}
      debugln(['  TryLFM CurLFMFilename="',CurLFMFilename,'" does not exist']);
      {$ENDIF}
      CurLFMFilename:=ChangeFileExt(UnitFilename,'.dfm');
      if not FileExistsCached(CurLFMFilename) then
      begin
        {$IFDEF VerboseLFMSearch}
        debugln(['  TryLFM CurLFMFilename="',CurLFMFilename,'" does not exist']);
        {$ENDIF}
        exit;
      end;
    end;
    // load the lfm file
    TheModalResult:=LoadCodeBuffer(LFMCode,CurLFMFilename,[lbfCheckIfText],true);
    if TheModalResult<>mrOk then
    begin
      debugln('TLazSourceFileManager.FindComponentClass Failed loading ',CurLFMFilename);
      exit;
    end;
    // read the LFM classname
    ReadLFMHeader(LFMCode.Source,LFMClassName,LFMType);
    if LFMType='' then ;
    if SysUtils.CompareText(LFMClassName,AClassName)<>0 then
    begin
      {$IFDEF VerboseLFMSearch}
      debugln(['  TryLFM CurLFMFilename="',CurLFMFilename,'" LFMClassName="',LFMClassName,'" does not match']);
      {$ENDIF}
      exit;
    end;

    // .lfm found
    LFMFilename:=CurLFMFilename;
    Result:=true;
  end;

  function TryFindDeclaration(out TheModalResult: TModalResult): boolean;
  var
    Tool: TCodeTool;

    function FindTypeNode(Node: TCodeTreeNode; Level: integer): TCodeTreeNode;
    var
      TypeNode: TCodeTreeNode;
      Child: TCodeTreeNode;
    begin
      Result:=nil;
      if Node=nil then exit;
      if Node.Desc=ctnVarDefinition then begin
        TypeNode:=Tool.FindTypeNodeOfDefinition(Node);
        if (TypeNode=nil) or (TypeNode.Desc<>ctnIdentifier) then exit;
        if Tool.CompareSrcIdentifiers(TypeNode.StartPos,PChar(AComponentClassName))
        then exit(TypeNode);
      end else if Node.Desc=ctnTypeDefinition then begin
        if Tool.CompareSrcIdentifiers(Node.StartPos,PChar(AComponentClassName))
        then exit(Node);
      end;
      // increase level on identifier nodes
      if Node.Desc in AllIdentifierDefinitions then begin
        if Level=1 then exit; // ignore nested vars
        inc(Level);
      end;
      Child:=Node.FirstChild;
      while Child<>nil do begin
        Result:=FindTypeNode(Child,Level);
        if Result<>nil then exit;
        Child:=Child.NextBrother;
      end;
    end;

  var
    Code: TCodeBuffer;
    Params: TFindDeclarationParams;
    NewNode: TCodeTreeNode;
    NewTool: TFindDeclarationTool;
    InheritedNode: TCodeTreeNode;
    ClassNode: TCodeTreeNode;
    AncestorNode: TCodeTreeNode;
    AncestorClassName: String;
    Node: TCodeTreeNode;
    ok: Boolean;
  begin
    Result:=false;
    TheModalResult:=mrCancel;
    // parse interface current unit
    Code:=CodeToolBoss.LoadFile(AnUnitInfo.Filename,false,false);
    if Code=nil then begin
      debugln(['TLazSourceFileManager.FindComponentClass unable to load ',AnUnitInfo.Filename]);
      exit;
    end;
    if not CodeToolBoss.Explore(Code,Tool,false,true) then begin
      {$IFDEF VerboseLFMSearch}
      debugln(['  CodeToolBoss.Explore failed: ',Code.Filename]);
      {$ENDIF}
      StoreCodetoolsError;
      exit;
    end;
    Params:=TFindDeclarationParams.Create;
    try
      ok:=false;
      try
        // search a class reference in the unit
        Node:=Tool.FindInterfaceNode;
        if Node=nil then
          Node:=Tool.Tree.Root;
        Node:=FindTypeNode(Node,0);
        if Node=nil then begin
          debugln('TLazSourceFileManager.FindComponentClass Failed finding reference of ',AComponentClassName,' in ',Code.Filename);
          exit;
        end;
        if Node.Desc=ctnIdentifier then begin
          //debugln(['TryFindDeclaration found reference of ',AComponentClassName,' at ',Tool.CleanPosToStr(Node.StartPos)]);
          Params.ContextNode:=Node;
          Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound,
                         fdfExceptionOnPredefinedIdent,
                         fdfTopLvlResolving,fdfSearchInAncestors,
                         fdfIgnoreCurContextNode];
          Params.SetIdentifier(Tool,@Tool.Src[Node.StartPos],nil);
          if not Tool.FindIdentifierInContext(Params) then begin
            debugln(['TLazSourceFileManager.FindComponentClass find declaration failed at ',Tool.CleanPosToStr(Node.StartPos,true)]);
            exit;
          end;
          NewNode:=Params.NewNode;
          NewTool:=Params.NewCodeTool;
        end else begin
          NewNode:=Node;
          NewTool:=Tool;
        end;
        ok:=true;
      except
        on E: Exception do
          CodeToolBoss.HandleException(E);
      end;
      if not ok then begin
        {$IFDEF VerboseLFMSearch}
        debugln(['  find declaration failed.']);
        {$ENDIF}
        StoreCodetoolsError;
        exit;
      end;
      // declaration found
      ClassNode:=NewNode.FirstChild;
      if (NewNode.Desc<>ctnTypeDefinition)
      or (ClassNode=nil) or (ClassNode.Desc<>ctnClass) then
      begin
        debugln(['TLazSourceFileManager.FindComponentClass ',AComponentClassName,' is not a class at ',NewTool.CleanPosToStr(NewNode.StartPos,true)]);
        exit;
      end;
      // find inheritance list
      InheritedNode:=ClassNode.FirstChild;
      while (InheritedNode<>nil) and (InheritedNode.Desc<>ctnClassInheritance) do
        InheritedNode:=InheritedNode.NextBrother;
      if (InheritedNode=nil) or (InheritedNode.FirstChild=nil) then begin
        debugln(['TLazSourceFileManager.FindComponentClass ',AComponentClassName,' is not a TComponent at ',NewTool.CleanPosToStr(NewNode.StartPos,true)]);
        exit;
      end;
      AncestorNode:=InheritedNode.FirstChild;
      AncestorClassName:=GetIdentifier(@NewTool.Src[AncestorNode.StartPos]);
      //debugln(['TryFindDeclaration declaration of ',AComponentClassName,' found at ',NewTool.CleanPosToStr(NewNode.StartPos),' ancestor="',AncestorClassName,'"']);

      // try unit component
      if TryUnitComponent(NewTool.MainFilename,TheModalResult) then
        exit(true);

      // try lfm
      if TryLFM(NewTool.MainFilename,AComponentClassName,TheModalResult) then
        exit(true);

      // search ancestor in registered classes
      if TryRegisteredClasses(AncestorClassName,AncestorClass,TheModalResult) then
        exit(true);

      {$IFDEF VerboseLFMSearch}
      debugln(['TryFindDeclaration declaration of ',AComponentClassName,' found at ',NewTool.CleanPosToStr(NewNode.StartPos),' Ancestor="',AncestorClassName,'", but no lfm and no registered class found']);
      {$ENDIF}
    finally
      Params.Free;
    end;
  end;

  function TryUsedUnitInterface(UnitFilename: string;
    out TheModalResult: TModalResult): boolean;
  var
    Code: TCodeBuffer;
    AncestorClassName: string;
  begin
    {$IFDEF VerboseLFMSearch}
    debugln(['  TryUsedUnitInterface UnitFilename="',UnitFilename,'"']);
    {$ENDIF}
    Result:=false;
    TheModalResult:=mrCancel;
    if not FilenameIsPascalSource(UnitFilename) then
    begin
      {$IFDEF VerboseLFMSearch}
      debugln(['  TryUsedUnitInterface UnitFilename="',UnitFilename,'" is not a unit']);
      {$ENDIF}
      exit;
    end;
    AncestorClassName:='';
    Code:=CodeToolBoss.LoadFile(UnitFilename,true,false);
    if Code=nil then begin
      debugln(['TLazSourceFileManager.FindComponentClass unable to load ',AnUnitInfo.Filename]);
      exit;
    end;
    if not CodeToolBoss.FindFormAncestor(Code,AComponentClassName,
      AncestorClassName,true) then
    begin
      {$IFDEF VerboseLFMSearch}
      debugln(['  TryUsedUnitInterface FindFormAncestor failed for "',AComponentClassName,'"']);
      {$ENDIF}
      StoreCodetoolsError;
      exit;
    end;
    if AncestorClassName='' then begin
      {$IFDEF VerboseLFMSearch}
      debugln(['  TryUsedUnitInterface FindFormAncestor failed silently for "',AComponentClassName,'"']);
      {$ENDIF}
      exit;
    end;
    if TryRegisteredClasses(AncestorClassName,AncestorClass,TheModalResult) then
      exit(true);
  end;

var
  UsedUnitFilenames: TStrings;
  i: Integer;
begin
  Result:=mrCancel;
  AComponentClass:=nil;
  ComponentUnitInfo:=nil;
  AncestorClass:=nil;
  LFMFilename:='';
  CTErrorMsg:='';
  CTErrorCode:=nil;
  CTErrorLine:=0;
  CTErrorCol:=0;

  if (AComponentClassName='') or (not IsValidIdent(AComponentClassName)) then
  begin
    DebugLn(['TLazSourceFileManager.FindComponentClass invalid component class name "',AComponentClassName,'"']);
    exit(mrCancel);
  end;

  // search component lfm
  {$ifdef VerboseFormEditor}
  debugln('TLazSourceFileManager.FindComponentClass START ',AnUnitInfo.Filename,' AComponentClassName=',AComponentClassName);
  {$endif}
  // first search the resource of ComponentUnitInfo
  if AnUnitInfo<>nil then begin
    if TryUnitComponent(AnUnitInfo.Filename,Result) then exit;
  end;

  // then try registered global classes
  if TryRegisteredClasses(AComponentClassName,AComponentClass,Result) then exit;

  // search in used units
  UsedUnitFilenames:=nil;
  try
    if not CodeToolBoss.FindUsedUnitFiles(AnUnitInfo.Source,UsedUnitFilenames)
    then begin
      MainIDE.DoJumpToCodeToolBossError;
      Result:=mrCancel;
      exit;
    end;

    {$IFDEF VerboseLFMSearch}
    if (UsedUnitFilenames=nil) or (UsedUnitFilenames.Count=0) then
      debugln(['TLazSourceFileManager.FindComponentClass unit has no main uses']);
    {$ENDIF}

    if (UsedUnitFilenames<>nil) then begin
      // search for every used unit the .lfm file
      for i:=UsedUnitFilenames.Count-1 downto 0 do begin
        if TryLFM(UsedUnitFilenames[i],AComponentClassName,Result) then exit;
      end;
      // search class via codetools
      if TryFindDeclaration(Result) then exit;
      // search the class in every used unit
      for i:=UsedUnitFilenames.Count-1 downto 0 do begin
        if TryUsedUnitInterface(UsedUnitFilenames[i],Result) then exit;
      end;
    end;
  finally
    UsedUnitFilenames.Free;
  end;

  // not found
  if Quiet then exit(mrCancel);

  // show codetool error
  if (CTErrorMsg<>'') and (not Quiet) then begin
    CodeToolBoss.SetError(CTErrorCode,CTErrorLine,CTErrorCol,CTErrorMsg);
    MainIDE.DoJumpToCodeToolBossError;
    Result:=mrAbort;
    exit;
  end;

  // just not found
  Result:=mrCancel;
end;

function TLazSourceFileManager.LoadComponentDependencyHidden(
  AnUnitInfo: TUnitInfo; const AComponentClassName: string; Flags: TOpenFlags;
  MustHaveLFM: boolean; out AComponentClass: TComponentClass; out
  ComponentUnitInfo: TUnitInfo; out AncestorClass: TComponentClass
  ): TModalResult;
{ Possible results:
  mrOk:
   - AComponentClass<>nil and ComponentUnitInfo<>nil
      designer component
   - AComponentClass<>nil and ComponentUnitInfo=nil
      registered componentclass
   - Only for MustHaveLFM=false: AncestorClass<>nil
      componentclass does not exist, but the ancestor is a registered class
  mrCancel:
    not found, skip this form
  mrAbort:
    not found, user wants to stop all pending operations
  mrIgnore:
    not found, user wants to skip this step and continue
}

  function TryLFM(LFMFilename: string; out TheModalResult: TModalResult): boolean;
  var
    UnitFilename: String;
    CurUnitInfo: TUnitInfo;
    LFMCode: TCodeBuffer;
    LFMClassName: String;
    LFMType: String;
    UnitCode: TCodeBuffer;
  begin
    Result:=false;
    TheModalResult:=mrCancel;
    // load lfm
    TheModalResult:=LoadCodeBuffer(LFMCode,LFMFilename,[lbfCheckIfText],true);
    if TheModalResult<>mrOk then begin
      {$IFDEF VerboseLFMSearch}
      debugln(['  TryLFM LoadCodeBuffer failed ',LFMFilename]);
      {$ENDIF}
      exit(TheModalResult=mrAbort);
    end;
    // check if the unit component is already loaded
    UnitFilename:=ChangeFileExt(LFMFilename,'.pas');
    CurUnitInfo:=Project1.UnitInfoWithFilename(UnitFilename);
    if CurUnitInfo=nil then begin
      UnitFilename:=ChangeFileExt(LFMFilename,'.pp');
      CurUnitInfo:=Project1.UnitInfoWithFilename(UnitFilename);
    end;
    ReadLFMHeader(LFMCode.Source,LFMClassName,LFMType);
    if CurUnitInfo<>nil then begin
      if (CurUnitInfo.Component<>nil) then begin
        // component already loaded
        if SysUtils.CompareText(CurUnitInfo.Component.ClassName,LFMClassName)<>0
        then begin
          {$IFDEF VerboseLFMSearch}
          debugln(['  TryLFM ERROR lfmclass=',LFMClassName,' unit.component=',DbgSName(CurUnitInfo.Component)]);
          {$ENDIF}
          IDEMessageDialog('Error','Unable to load "'+LFMFilename+'".'
            +' The component '+DbgSName(CurUnitInfo.Component)
            +' is already loaded for unit "'+CurUnitInfo.Filename+'"'#13
            +'LFM contains a different class name "'+LFMClassName+'".',
            mtError,[mbCancel]);
          TheModalResult:=mrAbort;
          exit(true);
        end;
        ComponentUnitInfo:=CurUnitInfo;
        AComponentClass:=TComponentClass(ComponentUnitInfo.Component.ClassType);
        TheModalResult:=mrOK;
        exit(true);
      end;
    end else begin
      // load unit source
      UnitFilename:=ChangeFileExt(LFMFilename,'.pas');
      if not FileExistsCached(UnitFilename) then
        UnitFilename:=ChangeFileExt(LFMFilename,'.pp');
      TheModalResult:=LoadCodeBuffer(UnitCode,UnitFilename,[lbfCheckIfText],true);
      if TheModalResult<>mrOk then exit(TheModalResult=mrAbort);
      // create unit info
      CurUnitInfo:=TUnitInfo.Create(UnitCode);
      CurUnitInfo.ReadUnitNameFromSource(true);
      Project1.AddFile(CurUnitInfo,false);
    end;

    // load resource hidden
    TheModalResult:=LoadLFM(CurUnitInfo,LFMCode,
                              Flags+[ofLoadHiddenResource],[]);
    if (TheModalResult=mrOk) then begin
      ComponentUnitInfo:=CurUnitInfo;
      AComponentClass:=TComponentClass(ComponentUnitInfo.Component.ClassType);
      {$if defined(VerboseFormEditor) or defined(VerboseLFMSearch)}
      debugln('TLazSourceFileManager.LoadComponentDependencyHidden Wanted=',AComponentClassName,' Class=',AComponentClass.ClassName);
      {$endif}
      TheModalResult:=mrOk;
      exit(true);
    end else begin
      debugln('TLazSourceFileManager.LoadComponentDependencyHidden Failed to load component ',AComponentClassName);
      TheModalResult:=mrCancel;
    end;
  end;

var
  Quiet: Boolean;
  LFMFilename: string;
begin
  Result:=mrCancel;
  AComponentClass:=nil;
  Quiet:=([ofProjectLoading,ofQuiet]*Flags<>[]);

  if (AComponentClassName='') or (not IsValidIdent(AComponentClassName)) then
  begin
    DebugLn(['TLazSourceFileManager.LoadComponentDependencyHidden invalid component class name "',AComponentClassName,'"']);
    exit(mrCancel);
  end;

  // check for cycles
  if AnUnitInfo.LoadingComponent then begin
    Result:=IDEQuestionDialog(lisCodeTemplError, Format(
      lisUnableToLoadTheComponentClassBecauseItDependsOnIts, ['"',
      AComponentClassName, '"']),
      mtError, [mrCancel, lisCancelLoadingThisComponent,
               mrAbort, lisAbortWholeLoading]);
    exit;
  end;

  AnUnitInfo.LoadingComponent:=true;
  try
    // search component lfm
    {$if defined(VerboseFormEditor) or defined(VerboseLFMSearch)}
    debugln('TLazSourceFileManager.LoadComponentDependencyHidden ',AnUnitInfo.Filename,' AComponentClassName=',AComponentClassName,' AComponentClass=',dbgsName(AComponentClass));
    {$endif}
    Result:=FindComponentClass(AnUnitInfo,AComponentClassName,Quiet,
      ComponentUnitInfo,AComponentClass,LFMFilename,AncestorClass);
    {$if defined(VerboseFormEditor) or defined(VerboseLFMSearch)}
    debugln('TLazSourceFileManager.LoadComponentDependencyHidden ',AnUnitInfo.Filename,' AComponentClassName=',AComponentClassName,' AComponentClass=',dbgsName(AComponentClass),' AncestorClass=',DbgSName(AncestorClass),' LFMFilename=',LFMFilename);
    {$endif}

    //- AComponentClass<>nil and ComponentUnitInfo<>nil
    //   designer component
    //- AComponentClass<>nil and ComponentUnitInfo=nil
    //   registered componentclass
    //- LFMFilename<>''
    //   lfm of an used unit
    //- AncestorClass<>nil
    //   componentclass does not exist, but the ancestor is a registered class

    if (Result=mrOk) and (AComponentClass=nil) and (LFMFilename<>'') then begin
      TryLFM(LFMFilename,Result);
      exit;
    end;

    if MustHaveLFM and (AComponentClass=nil) then
      Result:=mrCancel;
    if Result=mrAbort then exit;
    if Result<>mrOk then begin
      Result:=IDEQuestionDialog(lisCodeTemplError,
        Format(lisUnableToFindTheComponentClassItIsNotRegisteredViaR, [
          AComponentClassName, LineEnding, LineEnding, LineEnding, AnUnitInfo.Filename]),
        mtError, [mrCancel, lisCancelLoadingThisComponent,
                 mrAbort, lisAbortWholeLoading,
                 mrIgnore, lisIgnoreUseTFormAsAncestor]);
    end;
  finally
    AnUnitInfo.LoadingComponent:=false;
  end;
end;

function TLazSourceFileManager.LoadIDECodeBuffer(var ACodeBuffer: TCodeBuffer;
  const AFilename: string; Flags: TLoadBufferFlags; ShowAbort: boolean): TModalResult;
begin
  if (Project1<>nil)
  and (Project1.UnitInfoWithFilename(AFilename,[pfsfOnlyEditorFiles])<>nil) then
    Exclude(Flags,lbfUpdateFromDisk);
  Result:=LoadCodeBuffer(ACodeBuffer,AFilename,Flags,ShowAbort);
end;

function TLazSourceFileManager.CloseUnitComponent(AnUnitInfo: TUnitInfo;
  Flags: TCloseFlags): TModalResult;
var
  OldDesigner: TDesigner;

  procedure FreeDesigner(AFreeComponent: boolean);
  begin
    AnUnitInfo.LoadedDesigner:=false;
    OldDesigner.PrepareFreeDesigner(AFreeComponent);
    OldDesigner.FinalizeFreeDesigner;
  end;

  procedure FreeUnusedComponents;
  var
    CompUnitInfo: TUnitInfo;
  begin
    CompUnitInfo:=Project1.FirstUnitWithComponent;
    Project1.UpdateUnitComponentDependencies;
    while CompUnitInfo<>nil do begin
      //DebugLn(['FreeUnusedComponents ',CompUnitInfo.Filename,' ',dbgsName(CompUnitInfo.Component),' UnitComponentIsUsed=',UnitComponentIsUsed(CompUnitInfo,true)]);
      if not UnitComponentIsUsed(CompUnitInfo,true) then begin
        // close the unit component
        CloseUnitComponent(CompUnitInfo,Flags);
        // this has recursively freed all components, so exit here
        exit;
      end;
      CompUnitInfo:=CompUnitInfo.NextUnitWithComponent;
    end;
  end;

var
  AForm: TCustomForm;
  LookupRoot: TComponent;
  ComponentStillUsed: Boolean;
begin
  LookupRoot:=AnUnitInfo.Component;
  if LookupRoot=nil then exit(mrOk);
  {$IFDEF VerboseIDEMultiForm}
  DebugLn(['TLazSourceFileManager.CloseUnitComponent ',AnUnitInfo.Filename,' ',dbgsName(LookupRoot)]);
  {$ENDIF}

  Project1.LockUnitComponentDependencies; // avoid circles
  try
    // save
    if (cfSaveFirst in Flags) and (AnUnitInfo.OpenEditorInfoCount > 0)
    and (not AnUnitInfo.IsReverting) then begin
      Result:=SaveEditorFile(AnUnitInfo.OpenEditorInfo[0].EditorComponent,[sfCheckAmbiguousFiles]);
      if Result<>mrOk then begin
        DebugLn(['TLazSourceFileManager.CloseUnitComponent DoSaveEditorFile failed']);
        exit;
      end;
    end;

    // close dependencies
    if cfCloseDependencies in Flags then begin
      {$IFDEF VerboseIDEMultiForm}
      DebugLn(['TLazSourceFileManager.CloseUnitComponent cfCloseDependencies ',AnUnitInfo.Filename,' ',dbgsName(LookupRoot)]);
      {$ENDIF}
      Result:=CloseDependingUnitComponents(AnUnitInfo,Flags);
      if Result<>mrOk then begin
        DebugLn(['TLazSourceFileManager.CloseUnitComponent CloseDependingUnitComponents failed']);
        exit;
      end;
      // now only soft dependencies are left. The component can be freed.
    end;

    AForm:=FormEditor1.GetDesignerForm(LookupRoot);
    OldDesigner:=nil;
    if AForm<>nil then
      OldDesigner:=TDesigner(AForm.Designer);
    if MainIDE.LastFormActivated=AForm then
      MainIDE.LastFormActivated:=nil;
    ComponentStillUsed:=(not (cfCloseDependencies in Flags))
                        and UnitComponentIsUsed(AnUnitInfo,false);
    {$IFDEF VerboseTFrame}
    DebugLn(['TLazSourceFileManager.CloseUnitComponent ',AnUnitInfo.Filename,' ComponentStillUsed=',ComponentStillUsed,' UnitComponentIsUsed=',UnitComponentIsUsed(AnUnitInfo,false),' ',dbgs(AnUnitInfo.Flags),' DepAncestor=',AnUnitInfo.FindUsedByComponentDependency([ucdtAncestor])<>nil,' DepInline=',AnUnitInfo.FindUsedByComponentDependency([ucdtInlineClass])<>nil]);
    {$ENDIF}
    if (OldDesigner=nil) then begin
      // hidden component
      //DebugLn(['TLazSourceFileManager.CloseUnitComponent freeing hidden component without designer: ',AnUnitInfo.Filename,' ',DbgSName(AnUnitInfo.Component)]);
      if ComponentStillUsed then begin
        // hidden component is still used => keep it
        {$IFDEF VerboseIDEMultiForm}
        DebugLn(['TLazSourceFileManager.CloseUnitComponent hidden component is still used => keep it ',AnUnitInfo.Filename,' ',DbgSName(AnUnitInfo.Component)]);
        {$ENDIF}
      end else begin
        // hidden component is not used => free it
        {$IFDEF VerboseIDEMultiForm}
        DebugLn(['TLazSourceFileManager.CloseUnitComponent hidden component is not used => free it ',AnUnitInfo.Filename,' ',DbgSName(AnUnitInfo.Component)]);
        {$ENDIF}
        try
          FormEditor1.DeleteComponent(LookupRoot,true);
        finally
          AnUnitInfo.Component:=nil;
        end;
        FreeUnusedComponents;
      end;
    end else begin
      // component with designer
      if ComponentStillUsed then begin
        // free designer, keep component hidden
        {$IFDEF VerboseIDEMultiForm}
        DebugLn(['TLazSourceFileManager.CloseUnitComponent hiding component and freeing designer: ',AnUnitInfo.Filename,' ',DbgSName(AnUnitInfo.Component)]);
        {$ENDIF}
        FreeDesigner(false);
      end else begin
        // free designer and design form
        {$IFDEF VerboseIDEMultiForm}
        DebugLn(['TLazSourceFileManager.CloseUnitComponent freeing component and designer: ',AnUnitInfo.Filename,' ',DbgSName(AnUnitInfo.Component)]);
        {$ENDIF}
        try
          FreeDesigner(true);
        finally
          AnUnitInfo.Component:=nil;
        end;
      end;
      Project1.InvalidateUnitComponentDesignerDependencies;
      FreeUnusedComponents;
    end;
  finally
    Project1.UnlockUnitComponentDependencies;
  end;

  Result:=mrOk;
end;

function TLazSourceFileManager.CloseDependingUnitComponents(AnUnitInfo: TUnitInfo;
  Flags: TCloseFlags): TModalResult;
var
  UserAsked: Boolean;

  function CloseNext(var ModResult: TModalresult;
    Types: TUnitCompDependencyTypes): boolean;
  var
    DependingUnitInfo: TUnitInfo;
    DependenciesFlags: TCloseFlags;
  begin
    repeat
      DependingUnitInfo:=Project1.UnitUsingComponentUnit(AnUnitInfo,Types);
      if DependingUnitInfo=nil then break;
      if (not UserAsked) and (not (cfQuiet in Flags))
      and (not DependingUnitInfo.IsReverting) then begin
        // ToDo: collect in advance all components to close and show user the list
        ModResult:=IDEQuestionDialog('Close component?',
          'Close component '+dbgsName(DependingUnitInfo.Component)+'?',
          mtConfirmation,[mrYes,mrAbort]);
        if ModResult<>mrYes then exit(false);
        UserAsked:=true;
      end;
      // close recursively
      DependenciesFlags:=Flags+[cfCloseDependencies];
      if cfSaveDependencies in Flags then
        Include(DependenciesFlags,cfSaveFirst);
      ModResult:=CloseUnitComponent(DependingUnitInfo,DependenciesFlags);
      if ModResult<>mrOk then exit(false);
    until false;
    Result:=true;
  end;

begin
  UserAsked:=false;
  Project1.LockUnitComponentDependencies;
  try
    // Important:
    // This function is called recursively.
    // It is important that first the hard, non cyclic dependencies
    // are freed in the correct order.
    // After that the soft, cyclic dependencies can be freed in any order.

    // first close all descendants recursively
    // This must happen in the right order (descendants before ancestor)
    if not CloseNext(Result,[ucdtAncestor]) then exit;

    // then close all nested descendants recursively
    // This must happen in the right order (nested descendants before ancestor)
    if not CloseNext(Result,[ucdtInlineClass]) then exit;

    // then close all referring components
    // These can build circles and can be freed in any order.
    if not CloseNext(Result,[ucdtProperty]) then exit;
  finally
    Project1.UnlockUnitComponentDependencies;
  end;
  Result:=mrOk;
end;

function TLazSourceFileManager.UnitComponentIsUsed(AnUnitInfo: TUnitInfo;
  CheckHasDesigner: boolean): boolean;
// if CheckHasDesigner=true and AnUnitInfo has a designer (visible) return true
// otherwise check if another unit needs AnUnitInfo
var
  LookupRoot: TComponent;
begin
  Result:=false;
  LookupRoot:=AnUnitInfo.Component;
  if LookupRoot=nil then exit;
  // check if a designer or another component uses this component
  Project1.UpdateUnitComponentDependencies;
  if Project1.UnitComponentIsUsed(AnUnitInfo,CheckHasDesigner) then
    exit(true);
  //DebugLn(['TLazSourceFileManager.UnitComponentIsUsed ',AnUnitInfo.Filename,' ',dbgs(AnUnitInfo.Flags)]);
end;

function TLazSourceFileManager.RemoveFilesFromProject(AProject: TProject;
  UnitInfos: TFPList): TModalResult;
var
  i: Integer;
  AnUnitInfo: TUnitInfo;
  ShortUnitName: String;
  Dummy: Boolean;
  ObsoleteUnitPaths: String;
  ObsoleteIncPaths: String;
  p: Integer;
  ProjUnitPaths: String;
  CurDir: String;
  ResolvedDir: String;
  OldP: LongInt;
  ProjIncPaths: String;
begin
  Result:=mrOk;
  if UnitInfos=nil then exit;
  // check if something will change
  i:=UnitInfos.Count-1;
  while (i>=0) and (not TUnitInfo(UnitInfos[i]).IsPartOfProject) do dec(i);
  if i<0 then exit(mrOk);
  // check ToolStatus
  if (MainIDE.ToolStatus in [itCodeTools,itCodeToolAborting]) then begin
    debugln('TLazSourceFileManager.RemoveUnitsFromProject wrong ToolStatus ',dbgs(ord(MainIDE.ToolStatus)));
    exit;
  end;
  // commit changes from source editor to codetools
  SaveSourceEditorChangesToCodeCache(nil);

  ObsoleteUnitPaths:='';
  ObsoleteIncPaths:='';
  AProject.BeginUpdate(true);
  try
    for i:=0 to UnitInfos.Count-1 do begin
      AnUnitInfo:=TUnitInfo(UnitInfos[i]);
      //debugln(['TLazSourceFileManager.RemoveUnitsFromProject Unit ',AnUnitInfo.Filename]);
      if not AnUnitInfo.IsPartOfProject then continue;
      AnUnitInfo.IsPartOfProject:=false;
      AProject.Modified:=true;
      if FilenameIsPascalUnit(AnUnitInfo.Filename) then begin
        if FilenameIsAbsolute(AnUnitInfo.Filename) then
          ObsoleteUnitPaths:=MergeSearchPaths(ObsoleteUnitPaths,
                          ChompPathDelim(ExtractFilePath(AnUnitInfo.Filename)));
        // remove from project's unit section
        if (AProject.MainUnitID>=0)
        and (pfMainUnitHasUsesSectionForAllUnits in AProject.Flags)
        then begin
          ShortUnitName:=ExtractFileNameOnly(AnUnitInfo.Filename);
          //debugln(['TLazSourceFileManager.RemoveUnitsFromProject UnitName=',ShortUnitName]);
          if (ShortUnitName<>'') then begin
            Dummy:=CodeToolBoss.RemoveUnitFromAllUsesSections(
                                      AProject.MainUnitInfo.Source,ShortUnitName);
            if not Dummy then begin
              MainIDE.DoJumpToCodeToolBossError;
              Result:=mrCancel;
              exit;
            end;
          end;
        end;
        // remove CreateForm statement from project
        if (AProject.MainUnitID>=0)
        and (pfMainUnitHasCreateFormStatements in AProject.Flags)
        and (AnUnitInfo.ComponentName<>'') then begin
          Dummy:=AProject.RemoveCreateFormFromProjectFile(
              'T'+AnUnitInfo.ComponentName,AnUnitInfo.ComponentName);
          if not Dummy then begin
            MainIDE.DoJumpToCodeToolBossError;
            Result:=mrCancel;
            exit;
          end;
        end;
      end;
      if CompareFileExt(AnUnitInfo.Filename,'.inc',false)=0 then begin
        // include file
        if FilenameIsAbsolute(AnUnitInfo.Filename) then
          ObsoleteIncPaths:=MergeSearchPaths(ObsoleteIncPaths,
                        ChompPathDelim(ExtractFilePath(AnUnitInfo.Filename)));
      end;
    end;

    // removed directories still used fomr ObsoleteUnitPaths, ObsoleteIncPaths
    AnUnitInfo:=AProject.FirstPartOfProject;
    while AnUnitInfo<>nil do begin
      if FilenameIsAbsolute(AnUnitInfo.Filename) then begin
        if FilenameIsPascalUnit(AnUnitInfo.Filename) then
          ObsoleteUnitPaths:=RemoveSearchPaths(ObsoleteUnitPaths,
                        ChompPathDelim(ExtractFilePath(AnUnitInfo.Filename)));
        if CompareFileExt(AnUnitInfo.Filename,'.inc',false)=0 then
          ObsoleteIncPaths:=RemoveSearchPaths(ObsoleteIncPaths,
                        ChompPathDelim(ExtractFilePath(AnUnitInfo.Filename)));
      end;
      AnUnitInfo:=AnUnitInfo.NextPartOfProject;
    end;

    // check if compiler options contain paths of ObsoleteUnitPaths
    if ObsoleteUnitPaths<>'' then begin
      ProjUnitPaths:=AProject.CompilerOptions.OtherUnitFiles;
      p:=1;
      repeat
        OldP:=p;
        CurDir:=GetNextDirectoryInSearchPath(ProjUnitPaths,p);
        if CurDir='' then break;
        ResolvedDir:=AProject.CompilerOptions.ParsedOpts.DoParseOption(CurDir,
                                                            pcosUnitPath,false);
        if (ResolvedDir<>'')
        and (SearchDirectoryInSearchPath(ObsoleteUnitPaths,ResolvedDir)>0) then begin
          if IDEQuestionDialog(lisRemoveUnitPath,
            Format(lisTheDirectoryContainsNoProjectUnitsAnyMoreRemoveThi, [CurDir]),
            mtConfirmation, [mrYes, lisRemove, mrNo, lisKeep2], '')=mrYes
          then begin
            // remove
            ProjUnitPaths:=RemoveSearchPaths(ProjUnitPaths,CurDir);
            p:=OldP;
          end;
        end;
      until false;
      AProject.CompilerOptions.OtherUnitFiles:=ProjUnitPaths;
    end;

    // check if compiler options contain paths of ObsoleteIncPaths
    if ObsoleteIncPaths<>'' then begin
      ProjIncPaths:=AProject.CompilerOptions.IncludePath;
      p:=1;
      repeat
        OldP:=p;
        CurDir:=GetNextDirectoryInSearchPath(ProjIncPaths,p);
        if CurDir='' then break;
        ResolvedDir:=AProject.CompilerOptions.ParsedOpts.DoParseOption(CurDir,
                                                         pcosIncludePath,false);
        if (ResolvedDir<>'')
        and (SearchDirectoryInSearchPath(ObsoleteIncPaths,ResolvedDir)>0) then begin
          if IDEQuestionDialog(lisRemoveIncludePath,
            Format(lisTheDirectoryContainsNoProjectIncludeFilesAnyMoreRe, [CurDir]),
            mtConfirmation, [mrYes, lisRemove, mrNo, lisKeep2], '')=mrYes
          then begin
            // remove
            ProjIncPaths:=RemoveSearchPaths(ProjIncPaths,CurDir);
            p:=OldP;
          end;
        end;
      until false;
      AProject.CompilerOptions.IncludePath:=ProjIncPaths;
    end;

  finally
    // all changes were handled automatically by events, just clear the logs
    CodeToolBoss.SourceCache.ClearAllSourceLogEntries;
    AProject.EndUpdate;
  end;
end;

procedure TLazSourceFileManager.InvertedFileClose(PageIndex: LongInt; SrcNoteBook: TSourceNotebook);
// close all source editors except the clicked
var
  i, NeedSave, Idx: Integer;
  Ed: TSourceEditor;
  r: TModalResult;
begin
  NeedSave := 0;
  for i := 0 to SrcNoteBook.EditorCount - 1 do begin
    //no need for CheckEditorNeedsSave ActiveSourcenoteBook (i = PageIndex)
    if (i <> PageIndex) and CheckEditorNeedsSave(SrcNoteBook.Editors[i], True) then begin
      inc(NeedSave);
      if NeedSave = 1 then Idx := i;
    end;
  end;
  if NeedSave = 1 then begin
    Ed := SrcNoteBook.Editors[Idx];
    r := IDEQuestionDialog(lisSourceModified,
                     Format(lisSourceOfPageHasChangedSave, ['"', Ed.PageName, '"']),
                     mtConfirmation,
                     [mrYes, lisMenuSave, mrNo, lisDiscardChanges, mrAbort]);
    case r of
      mrYes: SaveEditorFile(Ed, [sfCheckAmbiguousFiles]);
      mrNo: ; // don't save
      mrAbort: exit;
    end;
  end
  else if NeedSave > 1 then begin
    for i := 0 to SrcNoteBook.EditorCount - 1 do begin
      if CheckEditorNeedsSave(SrcNoteBook.Editors[i], True) then begin
        dec(NeedSave);
        Ed := SrcNoteBook.Editors[i];
        r := IDEQuestionDialog(lisSourceModified,
                         Format(lisSourceOfPageHasChangedSaveExtended, ['"', Ed.PageName, '"', NeedSave]),
                         mtConfirmation,
                         [mrYes, lisMenuSave, mrAll, lisSaveAll,
                          mrNo, lisDiscardChanges, mrIgnore, lisDiscardChangesAll,
                          mrAbort]);
        case r of
          mrYes: SaveEditorFile(Ed, [sfCheckAmbiguousFiles]);
          mrNo: ; // don't save
          mrAll: begin
              MainIDE.DoSaveAll([]);
              break
            end;
          mrIgnore: break; // don't save anymore
          mrAbort: exit;
        end;
      end;
    end;
  end;
  SourceEditorManager.IncUpdateLock;
  try
    repeat
      i:=SrcNoteBook.PageCount-1;
      if i=PageIndex then dec(i);
      if i<0 then break;
      if CloseEditorFile(SrcNoteBook.FindSourceEditorWithPageIndex(i),[])<>mrOk then exit;
      if i<PageIndex then PageIndex:=i;
    until false;
  finally
    SourceEditorManager.DecUpdateLock;
  end;
end;

// methods for open project, create project from source

function TLazSourceFileManager.CompleteLoadingProjectInfo: TModalResult;
begin
  MainIDE.UpdateCaption;
  EnvironmentOptions.LastSavedProjectFile:=Project1.ProjectInfoFile;
  MainIDE.SaveEnvironment;

  MainBuildBoss.SetBuildTargetProject1(false);

  // load required packages
  PkgBoss.OpenProjectDependencies(Project1,true);

  Project1.DefineTemplates.AllChanged;
  //DebugLn('TLazSourceFileManager.CompleteLoadingProjectInfo ',Project1.IDAsString);
  Project1.DefineTemplates.Active:=true;

  Result:=mrOk;
end;

// Methods for 'save project'

function TLazSourceFileManager.SaveProjectInfo(var Flags: TSaveFlags): TModalResult;
var
  MainUnitInfo: TUnitInfo;
  MainUnitSrcEdit: TSourceEditor;
  DestFilename: String;
  SkipSavingMainSource: Boolean;
begin
  Project1.ActiveWindowIndexAtStart := SourceEditorManager.ActiveSourceWindowIndex;

  // update source notebook page names
  UpdateSourceNames;

  // find mainunit
  GetMainUnit(MainUnitInfo,MainUnitSrcEdit,true);

  // save project specific settings of the source editor
  SaveSourceEditorProjectSpecificSettings;

  if Project1.IsVirtual
  and (not (sfDoNotSaveVirtualFiles in Flags)) then
    Include(Flags,sfSaveAs);
  if ([sfSaveAs,sfSaveToTestDir]*Flags=[sfSaveAs]) then begin
    // let user choose a filename
    Result:=ShowSaveProjectAsDialog(sfSaveMainSourceAs in Flags);
    if Result<>mrOk then exit;
    Flags:=Flags-[sfSaveAs,sfSaveMainSourceAs];
  end;

  // update HasResources information
  UpdateProjectResourceInfo;

  // save project info file
  if (not (sfSaveToTestDir in Flags))
  and (not Project1.IsVirtual) then begin
    Result:=Project1.WriteProject([],'',EnvironmentOptions.BuildMatrixOptions);
    if Result=mrAbort then exit;
    EnvironmentOptions.LastSavedProjectFile:=Project1.ProjectInfoFile;
    IDEProtocolOpts.LastProjectLoadingCrashed := False;
    AddRecentProjectFileToEnvironment(Project1.ProjectInfoFile);
    MainIDE.SaveIncludeLinks;
    MainIDE.UpdateCaption;
    if Result=mrAbort then exit;
  end;

  // save main source
  if (MainUnitInfo<>nil) and (not (sfDoNotSaveVirtualFiles in flags)) then
  begin
    if not (sfSaveToTestDir in Flags) then
      DestFilename := MainUnitInfo.Filename
    else
      DestFilename := MainBuildBoss.GetTestUnitFilename(MainUnitInfo);

    if MainUnitInfo.OpenEditorInfoCount > 0 then
    begin
      // loaded in source editor
      Result:=SaveEditorFile(MainUnitInfo.OpenEditorInfo[0].EditorComponent,
               [sfProjectSaving]+[sfSaveToTestDir,sfCheckAmbiguousFiles]*Flags);
      if Result=mrAbort then exit;
    end else
    begin
      // not loaded in source editor (hidden)
      SkipSavingMainSource := false;
      if not (sfSaveToTestDir in Flags) and not MainUnitInfo.NeedsSaveToDisk then
        SkipSavingMainSource := true;
      if (not SkipSavingMainSource) and (MainUnitInfo.Source<>nil) then
      begin
        Result:=SaveCodeBufferToFile(MainUnitInfo.Source, DestFilename);
        if Result=mrAbort then exit;
      end;
    end;

    // clear modified flags
    if not (sfSaveToTestDir in Flags) then
    begin
      if (Result=mrOk) then begin
        if MainUnitInfo<>nil then MainUnitInfo.ClearModifieds;
        if MainUnitSrcEdit<>nil then MainUnitSrcEdit.Modified:=false;
      end;
    end;
  end;

  Result:=mrOk;
end;

procedure TLazSourceFileManager.GetMainUnit(var MainUnitInfo: TUnitInfo;
  var MainUnitSrcEdit: TSourceEditor; UpdateModified: boolean);
begin
  MainUnitSrcEdit:=nil;
  if Project1.MainUnitID>=0 then begin
    MainUnitInfo:=Project1.MainUnitInfo;
    if MainUnitInfo.OpenEditorInfoCount > 0 then begin
      MainUnitSrcEdit := TSourceEditor(MainUnitInfo.OpenEditorInfo[0].EditorComponent);
      if UpdateModified and MainUnitSrcEdit.Modified
      then begin
        MainUnitSrcEdit.UpdateCodeBuffer;
      end;
    end;
  end else
    MainUnitInfo:=nil;
end;

procedure TLazSourceFileManager.SaveSrcEditorProjectSpecificSettings(AnEditorInfo: TUnitEditorInfo);
var
  ASrcEdit: TSourceEditor;
begin
  ASrcEdit := TSourceEditor(AnEditorInfo.EditorComponent);
  if ASrcEdit=nil then exit;
  AnEditorInfo.TopLine:=ASrcEdit.EditorComponent.TopLine;
  AnEditorInfo.CursorPos:=ASrcEdit.EditorComponent.CaretXY;
  AnEditorInfo.FoldState := ASrcEdit.EditorComponent.FoldState;
end;

procedure TLazSourceFileManager.SaveSourceEditorProjectSpecificSettings;
var
  i: Integer;
begin
  for i := 0 to Project1.AllEditorsInfoCount - 1 do
    SaveSrcEditorProjectSpecificSettings(Project1.AllEditorsInfo[i]);
end;

procedure TLazSourceFileManager.UpdateProjectResourceInfo;
var
  AnUnitInfo: TUnitInfo;
  LFMFilename: String;
begin
  AnUnitInfo:=Project1.FirstPartOfProject;
  while AnUnitInfo<>nil do begin
    if (not AnUnitInfo.HasResources)
    and (not AnUnitInfo.IsVirtual) and FilenameIsPascalUnit(AnUnitInfo.Filename)
    then begin
      LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
      if not FileExistsUTF8(LFMFilename) then
        LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.dfm');
      AnUnitInfo.HasResources:=FileExistsUTF8(LFMFilename);
    end;
    AnUnitInfo:=AnUnitInfo.NextPartOfProject;
  end;
end;

function TLazSourceFileManager.ShowSaveProjectAsDialog(UseMainSourceFile: boolean): TModalResult;
var
  MainUnitSrcEdit: TSourceEditor;
  MainUnitInfo: TUnitInfo;
  SaveDialog: TSaveDialog;
  NewLPIFilename, NewProgramFilename, NewProgramName, AText, ACaption,
  Ext: string;
  NewBuf: TCodeBuffer;
  OldProjectDir: string;
  TitleWasDefault: Boolean;
  OldSource: String;
  AFilename: String;
  NewTargetFilename: String;
begin
  Project1.BeginUpdate(false);
  try
    OldProjectDir:=Project1.ProjectDirectory;

    if Project1.MainUnitInfo = nil then
      UseMainSourceFile := False;

    SaveDialog:=TSaveDialog.Create(nil);
    try
      InputHistories.ApplyFileDialogSettings(SaveDialog);
      AFilename:='';
      // build a nice project info filename suggestion
      if UseMainSourceFile and (Project1.MainUnitID>=0) then
        AFilename:=Project1.MainUnitInfo.Unit_Name;
      if AFilename='' then
        AFilename:=ExtractFileName(Project1.ProjectInfoFile);
      if AFilename='' then
        AFilename:=ExtractFileName(Project1.MainFilename);
      if AFilename='' then
        AFilename:=Trim(Project1.GetTitle);
      if AFilename='' then
        AFilename:='project1';
      Ext := LowerCase(ExtractFileExt(AFilename));
      if UseMainSourceFile then
      begin
        if (Ext = '') or (not FilenameIsPascalSource(AFilename)) then
          AFilename := ChangeFileExt(AFilename, '.pas');
      end else
      begin
        if (Ext = '') or FilenameIsPascalSource(AFilename) then
          AFilename := ChangeFileExt(AFilename, '.lpi');
      end;
      Ext := ExtractFileExt(AFilename);
      SaveDialog.Title := Format(lisSaveProject, [Project1.GetTitleOrName, Ext]);
      SaveDialog.FileName := AFilename;
      SaveDialog.Filter := '*' + Ext + '|' + '*' + Ext;
      SaveDialog.DefaultExt := ExtractFileExt(AFilename);
      if not Project1.IsVirtual then
        SaveDialog.InitialDir := Project1.ProjectDirectory;

      repeat
        Result:=mrCancel;
        NewLPIFilename:='';     // the project info file name
        NewProgramName:='';     // the pascal program identifier
        NewProgramFilename:=''; // the program source filename

        if not SaveDialog.Execute then begin
          // user cancels
          Result:=mrCancel;
          exit;
        end;
        AFilename:=ExpandFileNameUTF8(SaveDialog.FileName);
        if not FilenameIsAbsolute(AFilename) then
          RaiseException('TLazSourceFileManager.ShowSaveProjectAsDialog: buggy ExpandFileNameUTF8');

        // check program name
        NewProgramName:=ExtractFileNameOnly(AFilename);
        if (NewProgramName='') or (not IsValidUnitName(NewProgramName)) then begin
          Result:=IDEMessageDialog(lisInvalidProjectFilename,
            Format(lisisAnInvalidProjectNamePleaseChooseAnotherEGProject, ['"',
              SaveDialog.Filename, '"', LineEnding]),
            mtInformation,[mbRetry,mbAbort]);
          if Result=mrAbort then exit;
          continue; // try again
        end;

        // append default extension
        if UseMainSourceFile then
        begin
          NewLPIFilename:=ChangeFileExt(AFilename,'.lpi');
        end else
        begin
          NewLPIFilename:=AFilename;
          if ExtractFileExt(NewLPIFilename)='' then
            NewLPIFilename:=NewLPIFilename+'.lpi';
        end;

        // apply naming conventions
        // rename to lowercase is not needed for main source

        if Project1.MainUnitID >= 0 then
        begin
          // check mainunit filename
          Ext := ExtractFileExt(Project1.MainUnitInfo.Filename);
          if Ext = '' then Ext := '.pas';
          if UseMainSourceFile then
            NewProgramFilename := ExtractFileName(AFilename)
          else
            NewProgramFilename := ExtractFileNameWithoutExt(NewProgramName) + Ext;
          NewProgramFilename := ExtractFilePath(NewLPIFilename) + NewProgramFilename;
          if (CompareFilenames(NewLPIFilename, NewProgramFilename) = 0) then
          begin
            ACaption:=lisChooseADifferentName;
            AText:=Format(lisTheProjectInfoFileIsEqualToTheProjectMainSource, [
              '"', NewLPIFilename, '"', LineEnding]);
            Result:=IDEMessageDialog(ACaption, AText, mtError, [mbAbort,mbRetry]);
            if Result=mrAbort then exit;
            continue; // try again
          end;
          // check programname
          if FilenameIsPascalUnit(NewProgramFilename)
          and (Project1.IndexOfUnitWithName(NewProgramName,true,
                                         Project1.MainUnitInfo)>=0) then
          begin
            ACaption:=lisUnitIdentifierExists;
            AText:=Format(lisThereIsAUnitWithTheNameInTheProjectPleaseChoose, ['"',
              NewProgramName, '"', LineEnding]);
            Result:=IDEMessageDialog(ACaption,AText,mtError,[mbRetry,mbAbort]);
            if Result=mrAbort then exit;
            continue; // try again
          end;
          Result:=mrOk;
        end else begin
          NewProgramFilename:='';
          Result:=mrOk;
        end;
      until Result<>mrRetry;
    finally
      InputHistories.StoreFileDialogSettings(SaveDialog);
      SaveDialog.Free;
    end;

    //DebugLn(['TLazSourceFileManager.ShowSaveProjectAsDialog NewLPI=',NewLPIFilename,' NewProgramName=',NewProgramName,' NewMainSource=',NewProgramFilename]);

    // check if info file or source file already exists
    if FileExistsUTF8(NewLPIFilename) then
    begin
      ACaption:=lisOverwriteFile;
      AText:=Format(lisAFileAlreadyExistsReplaceIt, ['"', NewLPIFilename, '"', LineEnding]);
      Result:=IDEMessageDialog(ACaption, AText, mtConfirmation, [mbOk, mbCancel]);
      if Result=mrCancel then exit;
    end
    else
    begin
      if FileExistsUTF8(NewProgramFilename) then
      begin
        ACaption:=lisOverwriteFile;
        AText:=Format(lisAFileAlreadyExistsReplaceIt,
                      ['"', NewProgramFilename, '"', LineEnding]);
        Result:=IDEMessageDialog(ACaption, AText, mtConfirmation,[mbOk,mbCancel]);
        if Result=mrCancel then exit;
      end;
    end;

    TitleWasDefault := Project1.TitleIsDefault(true);

    // set new project target filename
    if (Project1.TargetFilename<>'')
    and ((SysUtils.CompareText(ExtractFileNameOnly(Project1.TargetFilename),
                               ExtractFileNameOnly(Project1.ProjectInfoFile))=0)
        or (Project1.ProjectInfoFile='')) then
    begin
      // target file is default => change, but keep sub directories
      // Note: Extension is appended automatically => do not add it
      NewTargetFilename:=ExtractFilePath(Project1.TargetFilename)
                                         +ExtractFileNameOnly(NewProgramFilename);
      Project1.TargetFilename:=NewTargetFilename;
      //DebugLn(['TLazSourceFileManager.ShowSaveProjectAsDialog changed targetfilename to ',Project1.TargetFilename]);
    end;

    // set new project filename
    Project1.ProjectInfoFile:=NewLPIFilename;
    EnvironmentOptions.AddToRecentProjectFiles(NewLPIFilename);
    MainIDE.SetRecentProjectFilesMenu;

    // change main source
    if (Project1.MainUnitID >= 0) then
    begin
      GetMainUnit(MainUnitInfo, MainUnitSrcEdit, true);

      if not Project1.ProjResources.RenameDirectives(MainUnitInfo.Filename,NewProgramFilename)
      then begin
        DebugLn(['TLazSourceFileManager.ShowSaveProjectAsDialog failed renaming directives Old="',MainUnitInfo.Filename,'" New="',NewProgramFilename,'"']);
        // silently ignore
      end;

      // Save old source code, to prevent overwriting it,
      // if the file name didn't actually change.
      OldSource := MainUnitInfo.Source.Source;

      // switch MainUnitInfo.Source to new code
      NewBuf := CodeToolBoss.CreateFile(NewProgramFilename);
      if NewBuf=nil then begin
        Result:=IDEMessageDialog(lisErrorCreatingFile, Format(lisUnableToCreateFile3, [
          LineEnding, '"', NewProgramFilename, '"']), mtError, [mbCancel]);
        exit;
      end;

      // copy the source to the new buffer
      NewBuf.Source:=OldSource;

      // assign the new buffer to the MainUnit
      MainUnitInfo.Source:=NewBuf;
      if MainUnitSrcEdit<>nil then
        MainUnitSrcEdit.CodeBuffer:=NewBuf;

      // change program name
      MainUnitInfo.Unit_Name:=NewProgramName;
      MainUnitInfo.Modified:=true;

      // update source notebook page names
      UpdateSourceNames;
    end;

    // update paths
    Project1.CompilerOptions.OtherUnitFiles:=
      RebaseSearchPath(Project1.CompilerOptions.OtherUnitFiles,OldProjectDir,
                       Project1.ProjectDirectory,true);
    Project1.CompilerOptions.IncludePath:=
      RebaseSearchPath(Project1.CompilerOptions.IncludePath,OldProjectDir,
                       Project1.ProjectDirectory,true);
    Project1.CompilerOptions.Libraries:=
      RebaseSearchPath(Project1.CompilerOptions.Libraries,OldProjectDir,
                       Project1.ProjectDirectory,true);
    Project1.CompilerOptions.ObjectPath:=
      RebaseSearchPath(Project1.CompilerOptions.ObjectPath,OldProjectDir,
                       Project1.ProjectDirectory,true);
    Project1.CompilerOptions.SrcPath:=
      RebaseSearchPath(Project1.CompilerOptions.SrcPath,OldProjectDir,
                       Project1.ProjectDirectory,true);
    Project1.CompilerOptions.DebugPath:=
      RebaseSearchPath(Project1.CompilerOptions.DebugPath,OldProjectDir,
                       Project1.ProjectDirectory,true);

    // change title
    if TitleWasDefault then begin
      Project1.Title:=Project1.GetDefaultTitle;
      // title does not need to be removed from source, because it was default
    end;

    // invalidate cached substituted macros
    IncreaseCompilerParseStamp;
  finally
    Project1.EndUpdate;
  end;
  Result:=mrOk;
  //DebugLn(['TLazSourceFileManager.ShowSaveProjectAsDialog END OK']);
end;

function TLazSourceFileManager.AskSaveProject(const ContinueText, ContinueBtn: string): TModalResult;
var
  DataModified: Boolean;
  SrcModified: Boolean;
begin
  if Project1=nil then exit(mrOk);
  if not SomethingOfProjectIsModified then exit(mrOk);

  DataModified:=Project1.SomeDataModified(false);
  SrcModified:=SourceEditorManager.SomethingModified(false);

  if Project1.IsVirtual
  and (not DataModified)
  and (not SrcModified) then begin
    // only session changed of a new project => ignore
    exit(mrOk)
  end;

  if (Project1.SessionStorage=pssInProjectInfo)
  or DataModified
  then begin
    // lpi file will change => ask
    Result:=IDEQuestionDialog(lisProjectChanged,
      Format(lisSaveChangesToProject, [Project1.GetTitleOrName]),
      mtConfirmation, [mrYes, mrNoToAll, lisNo, mbCancel], '');
    if Result=mrNoToAll then exit(mrOk);
    if Result<>mrYes then exit(mrCancel);
  end
  else if SrcModified then
  begin
    // some non project files were changes in the source editor
    Result:=IDEQuestionDialog(lisSaveChangedFiles,lisSaveChangedFiles,
      mtConfirmation, [mrYes, mrNoToAll, lisNo, mbCancel], '');
    if Result=mrNoToAll then exit(mrOk);
    if Result<>mrYes then exit(mrCancel);
  end
  else begin
    // only session data changed
    if Project1.SessionStorage=pssNone then
      // session is not saved => skip
      exit(mrOk)
    else if not SomethingOfProjectIsModified then
      // no change
      exit(mrOk)
    else begin
      // session is saved separately
      if EnvironmentOptions.AskSaveSessionOnly then begin
        Result:=IDEQuestionDialog(lisProjectSessionChanged,
          Format(lisSaveSessionChangesToProject, [Project1.GetTitleOrName]),
          mtConfirmation, [mrYes, mrNoToAll, lisNo, mbCancel], '');
        if Result=mrNoToAll then exit(mrOk);
        if Result<>mrYes then exit(mrCancel);
      end;
    end;
  end;
  Result:=SaveProject([sfCanAbort]);
  if Result=mrAbort then exit;
  if Result<>mrOk then begin
    Result:=IDEQuestionDialog(lisChangesWereNotSaved,
      ContinueText,
      mtConfirmation, [mrOk, ContinueBtn, mrAbort]);
    if Result<>mrOk then exit(mrCancel);
  end;
end;

function TLazSourceFileManager.SaveSourceEditorChangesToCodeCache(
  AEditor: TSourceEditorInterface): boolean;
// save all open sources to code tools cache

  procedure SaveChanges(SaveEditor: TSourceEditorInterface);
  var
    AnUnitInfo: TUnitInfo;
  begin
    AnUnitInfo := Project1.UnitWithEditorComponent(SaveEditor);
    if (AnUnitInfo<>nil) then
    begin
      //debugln(['SaveChanges ',AnUnitInfo.Filename,' ',SaveEditor.NeedsUpdateCodeBuffer]);
      if SaveEditor.NeedsUpdateCodeBuffer then
      begin
        SaveSourceEditorChangesToCodeCache:=true;
        SaveEditor.UpdateCodeBuffer;
        //debugln(['TLazSourceFileManager.SaveSourceEditorChangesToCodeCache.SaveChanges ',AnUnitInfo.Filename,' Step=',TCodeBuffer(SaveEditor.CodeToolsBuffer).ChangeStep]);
      end;
    end;
  end;

var
  i: integer;
begin
  Result:=false;
  //debugln(['TLazSourceFileManager.SaveSourceEditorChangesToCodeCache ']);
  if AEditor = nil then begin
    for i:=0 to SourceEditorManager.SourceEditorCount - 1 do
      SaveChanges(SourceEditorManager.SourceEditors[i]);
  end else begin
    SaveChanges(AEditor);
  end;
end;

function TLazSourceFileManager.ReplaceUnitUse(OldFilename, OldUnitName, NewFilename,
  NewUnitName: string; IgnoreErrors, Quiet, Confirm: boolean): TModalResult;
// Replaces all references to a unit
var
  OwnerList: TFPList;
  ExtraFiles: TStrings;
  Files: TStringList;
  OldCode: TCodeBuffer;
  OldCodeCreated: Boolean;
  PascalReferences: TAVLTree;
  i: Integer;
  MsgResult: TModalResult;
  OnlyEditorFiles: Boolean;
  aFilename: String;
begin
  if (CompareFilenames(OldFilename,NewFilename)=0)
  and (OldUnitName=NewUnitName) then // compare unitnames case sensitive, maybe only the case changed
    exit(mrOk);
  OnlyEditorFiles:=not FilenameIsAbsolute(OldFilename); // this was a new file, files on disk can not refer to it

  OwnerList:=nil;
  OldCode:=nil;
  OldCodeCreated:=false;
  PascalReferences:=nil;
  Files:=TStringList.Create;
  try
    if OnlyEditorFiles then begin
      // search only in open files
      for i:=0 to SourceEditorManagerIntf.UniqueSourceEditorCount-1 do begin
        aFilename:=SourceEditorManagerIntf.UniqueSourceEditors[i].FileName;
        if not FilenameIsPascalSource(aFilename) then continue;
        Files.Add(aFileName);
      end;
      // add project's main source file
      if (Project1<>nil) and (Project1.MainUnitID>=0) then
        Files.Add(Project1.MainFilename);
    end else begin
      // get owners of unit
      OwnerList:=PkgBoss.GetOwnersOfUnit(NewFilename);
      if OwnerList=nil then exit(mrOk);
      PkgBoss.ExtendOwnerListWithUsedByOwners(OwnerList);
      ReverseList(OwnerList);

      // get source files of packages and projects
      ExtraFiles:=PkgBoss.GetSourceFilesOfOwners(OwnerList);
      try
        if ExtraFiles<>nil then
          Files.AddStrings(ExtraFiles);
      finally
        ExtraFiles.Free;
      end;
    end;
    for i:=Files.Count-1 downto 0 do begin
      if (CompareFilenames(Files[i],OldFilename)=0)
      or (CompareFilenames(Files[i],NewFilename)=0) then
        Files.Delete(i);
    end;

    //DebugLn(['TLazSourceFileManager.ReplaceUnitUse ',Files.Text]);

    // commit source editor to codetools
    SaveSourceEditorChangesToCodeCache(nil);

    // load or create old unit
    OldCode:=CodeToolBoss.LoadFile(OldFilename,true,false);
    if OldCode=nil then begin
      // create old file in memory so that unit search can find it
      OldCode:=CodeToolBoss.CreateFile(OldFilename);
      OldCodeCreated:=true;
    end;

    // search pascal source references
    Result:=GatherUnitReferences(Files,OldCode,false,IgnoreErrors,true,
                                 PascalReferences);
    if (not IgnoreErrors) and (not Quiet) and (CodeToolBoss.ErrorMessage<>'')
    then
      MainIDE.DoJumpToCodeToolBossError;
    if Result<>mrOk then begin
      debugln('TLazSourceFileManager.ReplaceUnitUse GatherUnitReferences failed');
      exit;
    end;

    // replace
    if (PascalReferences<>nil) and (PascalReferences.Count>0) then begin
      if Confirm then begin
        MsgResult:=IDEQuestionDialog(lisUpdateReferences,
          Format(lisTheUnitIsUsedByOtherFilesUpdateReferencesAutomatic,
                 [OldUnitName, LineEnding]),
          mtConfirmation, [mrYes,mrNo,mrYesToAll,mrNoToAll],'');
        case MsgResult of
        mrYes: ;
        mrYesToAll: EnvironmentOptions.UnitRenameReferencesAction:=urraAlways;
        mrNoToAll:
          begin
            EnvironmentOptions.UnitRenameReferencesAction:=urraNever;
            Result:=mrOk;
            exit;
          end;
        else
          Result:=mrOk;
          exit;
        end;
      end;
      if not CodeToolBoss.RenameIdentifier(PascalReferences,
        OldUnitName,NewUnitName)
      then begin
        if (not IgnoreErrors) and (not Quiet) then
          MainIDE.DoJumpToCodeToolBossError;
        debugln('TLazSourceFileManager.ReplaceUnitUse unable to commit');
        if not IgnoreErrors then begin
          Result:=mrCancel;
          exit;
        end;
      end;
    end;

  finally
    if OldCodeCreated then
      OldCode.IsDeleted:=true;
    CodeToolBoss.FreeTreeOfPCodeXYPosition(PascalReferences);
    OwnerList.Free;
    Files.Free;
  end;
  //PkgBoss.GetOwnersOfUnit(NewFilename);
  Result:=mrOk;
end;

function TLazSourceFileManager.DesignerUnitIsVirtual(aLookupRoot: TComponent): Boolean;
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  Assert(Assigned(aLookupRoot),'SourceFileMgr.DesignerUnitIsVirtual: aLookupRoot is not assigned');
  MainIDE.GetUnitWithPersistent(aLookupRoot, ActiveSourceEditor, ActiveUnitInfo);
  Result := ActiveUnitInfo.IsVirtual;
end;

finalization
  SourceFileMgrSingleton.Free;

end.

