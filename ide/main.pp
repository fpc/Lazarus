{  $Id$  }
{
 /***************************************************************************
                          main.pp  -  Toolbar
                          -------------------
                   TMainIDE is the application toolbar window.


                 Initial Revision  : Sun Mar 28 23:15:32 CST 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit Main;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, LclLinux, Compiler, StdCtrls, Forms, Buttons, Menus, ComCtrls, Spin,
  Project, Sysutils, FileCtrl, Controls, Graphics, ExtCtrls, Dialogs, LazConf,
  CompReg, CodeToolManager, CodeCache, DefineTemplates, MsgView, NewProjectDlg,
  IDEComp, AbstractFormEditor, FormEditor, CustomFormEditor, ObjectInspector,
  PropEdits, ControlSelection, UnitEditor, CompilerOptions, EditorOptions,
  EnvironmentOpts, TransferMacros, KeyMapping, ProjectOpts, IDEProcs, Process,
  UnitInfoDlg, Debugger;

const
  Version_String = '0.8 alpha';

type
  TIDEToolStatus = (itNone, itBuilder, itDebugger, itCustom);

  TMainIDE = class(TForm)
    ViewUnitsSpeedBtn  : TSpeedButton;
    ViewFormsSpeedBtn  : TSpeedButton;
    NewUnitSpeedBtn    : TSpeedButton;
    OpenFileSpeedBtn   : TSpeedButton;
    OpenFileArrowSpeedBtn : TSpeedButton;
    SaveSpeedBtn       : TSpeedButton;
    SaveAllSpeedBtn    : TSpeedButton;
    ToggleFormSpeedBtn : TSpeedButton;
    NewFormSpeedBtn    : TSpeedButton;
    RunSpeedButton     : TSpeedButton;
    PauseSpeedButton     : TSpeedButton;
    StepIntoSpeedButton     : TSpeedButton;
    StepOverSpeedButton     : TSpeedButton;
    OpenFilePopUpMenu : TPopupMenu;
    Toolbutton1  : TToolButton;
    Toolbutton2  : TToolButton;
    Toolbutton3  : TToolButton;
    Toolbutton4  : TToolButton;
    GlobalMouseSpeedButton : TSpeedButton;

    ComboBox1 : TComboBox;
    Edit1: TEdit;
    SpinEdit1 : TSpinEdit;
    ListBox1 : TListBox;
    mnuMain: TMainMenu;

    mnuFile: TMenuItem;
    mnuEdit: TMenuItem; 
    mnuSearch: TMenuItem;
    mnuView: TMenuItem; 
    mnuProject: TMenuItem; 
    mnuEnvironment:TMenuItem;

    itmSeperator: TMenuItem;

    itmFileNew : TMenuItem;
    itmFileNewForm : TMenuItem;
    itmFileOpen: TMenuItem;
    itmFileRecentOpen: TMenuItem;
    itmFileSave: TMenuItem; 
    itmFileSaveAs: TMenuItem; 
    itmFileSaveAll: TMenuItem; 
    itmFileClose: TMenuItem; 
    itmFileQuit: TMenuItem; 

    itmProjectNew: TMenuItem;
    itmProjectOpen: TMenuItem;
    itmProjectRecentOpen: TMenuItem;
    itmProjectSave: TMenuItem;
    itmProjectSaveAs: TMenuItem;
    itmProjectAddTo: TMenuItem;
    itmProjectRemoveFrom: TMenuItem;
    itmProjectViewSource: TMenuItem;
    itmProjectBuild: TMenuItem;
    itmProjectRun: TMenuItem;
    itmProjectPause: TMenuItem;
    itmProjectStepInto: TMenuItem;
    itmProjectStepOver: TMenuItem;
    itmProjectRunToCursor: TMenuItem;
    itmProjectStop: TMenuItem;
    itmProjectOptions: TMenuItem;
    itmProjectCompilerSettings: TMenuItem;

    itmEditUndo: TMenuItem; 
    itmEditRedo: TMenuItem; 
    itmEditCut: TMenuItem; 
    itmEditCopy: TMenuItem; 
    itmEditPaste: TMenuItem; 

    itmSearchFind: TMenuItem;
    itmSearchFindNext: TMenuItem;
    itmSearchFindPrevious: TMenuItem;
    itmSearchFindInFiles: TMenuItem;
    itmSearchReplace: TMenuItem;
    itmGotoLineNumber: TMenuItem;

    itmViewInspector: TMenuItem;
    itmViewProject: TMenuItem; 
    itmViewUnits : TMenuItem;
    itmViewCodeExplorer : TMenuItem;
    itmViewForms : TMenuItem;
    itmViewFile : TMenuItem;
    itmViewMessage : TMenuItem;

    itmEnvGeneralOptions: TMenuItem; 
    itmEnvEditorOptions: TMenuItem; 
    
    CheckBox1 : TCheckBox; 
    ComponentNotebook : TNotebook;
    cmdTest: TButton;
    cmdTest2: TButton;
    Label2 : TLabel;

    // event handlers
    procedure FormShow(Sender : TObject);
    procedure FormClose(Sender : TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose: boolean);
    procedure FormPaint(Sender : TObject);

    procedure mnuNewUnitClicked(Sender : TObject);
    procedure mnuNewFormClicked(Sender : TObject);
    procedure mnuOpenClicked(Sender : TObject);
    procedure mnuOpenFileAtCursorClicked(Sender : TObject);
    procedure mnuSaveClicked(Sender : TObject);
    procedure mnuSaveAsClicked(Sender : TObject);
    procedure mnuSaveAllClicked(Sender : TObject);
    procedure mnuCloseClicked(Sender : TObject);
    procedure mnuQuitClicked(Sender : TObject);

    procedure mnuViewInspectorClicked(Sender : TObject);
    Procedure mnuViewUnitsClicked(Sender : TObject);
    Procedure mnuViewFormsClicked(Sender : TObject);

    procedure mnuToggleFormUnitClicked(Sender : TObject);
    procedure mnuNewProjectClicked(Sender : TObject);
    procedure mnuOpenProjectClicked(Sender : TObject);
    procedure mnuSaveProjectClicked(Sender : TObject);
    procedure mnuSaveProjectAsClicked(Sender : TObject);
    procedure mnuAddToProjectClicked(Sender : TObject);
    procedure mnuRemoveFromProjectClicked(Sender : TObject);
    procedure mnuViewProjectSourceClicked(Sender : TObject);
    procedure mnuBuildProjectClicked(Sender : TObject);
    procedure mnuRunProjectClicked(Sender : TObject);
    procedure mnuPauseProjectClicked(Sender : TObject);
    procedure mnuStepIntoProjectClicked(Sender : TObject);
    procedure mnuStepOverProjectClicked(Sender : TObject);
    procedure mnuRunToCursorProjectClicked(Sender : TObject);
    procedure mnuStopProjectClicked(Sender : TObject);
    procedure mnuProjectCompilerSettingsClicked(Sender : TObject);
    procedure mnuProjectOptionsClicked(Sender : TObject);

    procedure mnuViewCodeExplorerClick(Sender : TObject);
    procedure mnuViewMessagesClick(Sender : TObject);
    procedure MessageViewDblClick(Sender : TObject);

    procedure mnuEnvGeneralOptionsClicked(Sender : TObject);
    procedure mnuEnvEditorOptionsClicked(Sender : TObject);

    Procedure OpenFileDownArrowClicked(Sender : TObject);
    Procedure ControlClick(Sender : TObject);

    // SourceNotebook events
    Procedure OnSrcNotebookFileNew(Sender : TObject);
    Procedure OnSrcNotebookFileOpen(Sender : TObject);
    Procedure OnSrcNotebookFileOpenAtCursor(Sender : TObject);
    Procedure OnSrcNotebookFileSave(Sender : TObject);
    Procedure OnSrcNotebookFileSaveAs(Sender : TObject);
    Procedure OnSrcNotebookFileClose(Sender : TObject);
    Procedure OnSrcNotebookSaveAll(Sender : TObject);
    Procedure OnSrcNotebookToggleFormUnit(Sender : TObject);
    Procedure OnSrcNotebookProcessCommand(Sender: TObject; Command: integer;
        var Handled: boolean);
    procedure OnSrcNoteBookShowUnitInfo(Sender: TObject);

    // ObjectInspector events
    procedure OIOnAddAvailableComponent(AComponent:TComponent;
       var Allowed:boolean);
    procedure OIOnSelectComponent(AComponent:TComponent);

    // Environment options dialog events
    procedure OnLoadEnvironmentSettings(Sender: TObject; 
       TheEnvironmentOptions: TEnvironmentOptions);
    procedure OnSaveEnvironmentSettings(Sender: TObject; 
       TheEnvironmentOptions: TEnvironmentOptions);
       
    // CodeToolBoss events
    procedure OnBeforeCodeToolBossApplyChanges(Manager: TCodeToolManager;
                                    var Abort: boolean);
    procedure OnAfterCodeToolBossApplyChanges(Manager: TCodeToolManager);
    
    // Debugger Events
    procedure OnDebuggerChangeState(Sender: TObject);
    procedure OnDebuggerCurrentLine(Sender: TObject; const AFilename: String;
                               const ALine: Integer);
  private
    FCodeLastActivated : Boolean; //used for toggling between code and forms
    FSelectedComponent : TRegisteredComponent;
    fProject: TProject;
    MacroList: TTransferMacroList;
    FMessagesViewBoundsRectValid: boolean;
    FOpenEditorsOnCodeToolChange: boolean;
    TheDebugger: TDebugger;

    Function CreateSeperator : TMenuItem;
    Procedure SetDefaultsForForm(aForm : TCustomForm);

  protected
    procedure ToolButtonClick(Sender : TObject);

  public
    ToolStatus: TIDEToolStatus;
 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Function SearchPaths : String;

    // files/units
    function DoNewEditorUnit(NewUnitType:TNewUnitType):TModalResult;
    function DoSaveEditorUnit(PageIndex:integer;
                   SaveAs, SaveToTestDir:boolean):TModalResult;
    function DoCloseEditorUnit(PageIndex:integer;
        SaveFirst: boolean):TModalResult;
    function DoOpenEditorFile(const AFileName:string;
        ProjectLoading:boolean):TModalResult;
    function DoOpenFileAtCursor(Sender: TObject):TModalResult;
    function DoSaveAll: TModalResult;
    function DoOpenMainUnit(ProjectLoading: boolean): TModalResult;
    function DoViewUnitsAndForms(OnlyForms: boolean): TModalResult;
    
    // project(s)
    property Project: TProject read fProject write fProject;
    function DoNewProject(NewProjectType:TProjectType):TModalResult;
    function DoSaveProject(SaveAs,SaveToTestDir:boolean):TModalResult;
    function DoCloseProject:TModalResult;
    function DoOpenProjectFile(AFileName:string):TModalResult;
    function DoAddActiveUnitToProject: TModalResult;
    function DoRemoveFromProjectDialog: TModalResult;
    function DoBuildProject: TModalResult;
    function DoRunProject: TModalResult;
    function DoPauseProject: TModalResult;
    function DoStepIntoProject: TModalResult;
    function DoStepOverProject: TModalResult;
    function DoRunToCursor: TModalResult;
    function DoStopProject: TModalResult;
    function SomethingOfProjectIsModified: boolean;
    function DoCreateProjectForProgram(ProgramBuf: TCodeBuffer): TModalResult;
    function DoSaveProjectToTestDirectory: TModalResult;

    // useful methods
    procedure GetCurrentUnit(var ActiveSourceEditor:TSourceEditor; 
      var ActiveUnitInfo:TUnitInfo);
    procedure GetUnitWithPageIndex(PageIndex:integer; 
      var ActiveSourceEditor:TSourceEditor; var ActiveUnitInfo:TUnitInfo);
    function DoSaveStreamToFile(AStream:TStream; const Filename:string; 
      IsPartOfProject:boolean): TModalResult;
    function DoLoadMemoryStreamFromFile(MemStream: TMemoryStream; 
      const AFilename:string): TModalResult;
    function DoSaveCodeBufferToFile(ABuffer: TCodeBuffer;
      const AFilename: string; IsPartOfProject:boolean): TModalResult;
    function DoLoadCodeBuffer(var ACodeBuffer: TCodeBuffer; 
      const AFilename: string): TModalResult;
    function DoBackupFile(const Filename:string; 
      IsPartOfProject:boolean): TModalResult;
    procedure UpdateCaption;
    procedure DoBringToFrontFormOrUnit;
    procedure OnMacroSubstitution(TheMacro: TTransferMacro; var s:string;
      var Handled, Abort: boolean);
    procedure OnCmdLineCreate(var CmdLine: string; var Abort:boolean);
    function DoJumpToCompilerMessage(Index:integer;
      FocusEditor: boolean): boolean;
    procedure DoShowMessagesView;
    function GetTestProjectFilename: string;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string;
    procedure SaveSourceEditorChangesToCodeCache;
    procedure ApplyCodeToolChanges;
    procedure DoJumpToProcedureSection;
    procedure DoCompleteCodeAtCursor;
    function DoInitDebugger: TModalResult;

    procedure LoadMainMenu;
    procedure LoadSpeedbuttons;
    Procedure SetDesigning(Control : TComponent; Value : Boolean);

    // form editor and designer
    property SelectedComponent : TRegisteredComponent 
      read FSelectedComponent write FSelectedComponent;
    procedure OnDesignerGetSelectedComponentClass(Sender: TObject;
      var RegisteredComponent: TRegisteredComponent);
    procedure OnDesignerUnselectComponentClass(Sender: TObject);
    procedure OnDesignerSetDesigning(Sender: TObject; Component: TComponent;
      Value: boolean);
    procedure OnDesignerComponentListChanged(Sender: TObject);
    procedure OnDesignerPropertiesChanged(Sender: TObject);
    procedure OnDesignerAddComponent(Sender: TObject; Component: TComponent;
      ComponentClass: TRegisteredComponent);
    procedure OnDesignerRemoveComponent(Sender: TObject; Component: TComponent);
    procedure OnDesignerModified(Sender: TObject);
    procedure OnControlSelectionChanged(Sender: TObject);

    procedure SaveEnvironment;
    procedure SaveDesktopSettings(TheEnvironmentOptions: TEnvironmentOptions);
    procedure LoadDesktopSettings(TheEnvironmentOptions: TEnvironmentOptions);
    procedure InitCodeToolBoss;
  end;



const
  CapLetters = ['A'..'Z'];
  SmallLetters = ['a'..'z'];
  Numbers = ['0'..'1'];

var
  MainIDE : TMainIDE;

  ObjectInspector1 : TObjectInspector;
  PropertyEditorHook1 : TPropertyEditorHook;
  SourceNotebook : TSourceNotebook;


implementation

uses
  ViewUnit_dlg, Math, LResources, Designer;


function LoadPixmapRes(const ResourceName:string; PixMap:TPixMap):boolean;
var
  ms:TMemoryStream;
  res:TLResource;
begin
  Result:=false;
  res:=LazarusResources.Find(ResourceName);
  if (res<>nil) and (res.Value<>'') and (res.ValueType='XPM') then begin
    ms:=TMemoryStream.Create;
    try
      ms.Write(res.Value[1],length(res.Value));
      ms.Position:=0;
      Pixmap.LoadFromStream(ms);
      Result:=true;
    finally
      ms.Free;
    end;
  end;
end;

function LoadSpeedBtnPixMap(const ResourceName:string):TPixmap;
begin
  Result:=TPixmap.Create;
  Result.TransparentColor:=clBtnFace;
  if not LoadPixmapRes(ResourceName,Result) then
    LoadPixmapRes('default',Result);
end;


{ TMainIDE }


constructor TMainIDE.Create(AOwner: TComponent);
const
  PrimaryConfPathOpt='--primary-config-path=';
  SecondaryConfPathOpt='--secondary-config-path=';
var
  i,x : Integer;
  PageCount : Integer;
  RegComp     : TRegisteredComponent;
  RegCompPage : TRegisteredComponentPage;
  IDEComponent : TIdeComponent;
  SelectionPointerPixmap: TPixmap;
begin
  inherited Create(AOwner);

  // parse command line options
  for i:=1 to ParamCount do begin
    if copy(ParamStr(i),1,length(PrimaryConfPathOpt))=PrimaryConfPathOpt then
    begin
      SetPrimaryConfigPath(copy(ParamStr(i),length(PrimaryConfPathOpt)+1,
               length(ParamStr(i))));
    end;
    if copy(ParamStr(i),1,length(SecondaryConfPathOpt))=SecondaryConfPathOpt
    then begin
      SetSecondaryConfigPath(copy(ParamStr(i),length(SecondaryConfPathOpt)+1,
               length(ParamStr(i))));
    end;
  end;

  // load environment and editor options
  CreatePrimaryConfigPath;

  EnvironmentOptions:=TEnvironmentOptions.Create;
  with EnvironmentOptions do begin
    SetLazarusDefaultFilename;
    Load(false);
  end;

  EditorOpts:=TEditorOptions.Create;
  EditorOpts.Load;

  // set the IDE mode to none (= editing mode)
  ToolStatus:=itNone;

  // setup the code tools
  InitCodeToolBoss;

  // build and position the MainIDE form
  Name := 'MainIDE';
  if (EnvironmentOptions.SaveWindowPositions) 
  and (EnvironmentOptions.WindowPositionsValid) 
  then begin
    BoundsRect := EnvironmentOptions.MainWindowBounds;
  end 
  else begin
    Left := 0;
    Top := 0;
    Width := Screen.Width - 10;
    Height := 125;
  end;
  Position:= poDesigned;

  if LazarusResources.Find(ClassName)=nil then begin
    LoadMainMenu;
    LoadSpeedbuttons;
  end;

  // Component Notebook
  ComponentNotebook := TNotebook.Create(Self);
  with ComponentNotebook do begin
    Parent := Self;
    Name := 'ComponentNotebook';
//    Align := alBottom;
    Left := ToggleFormSpeedBtn.Left + ToggleFormSpeedBtn.Width + 4;
//    Top :=50+ 2;
    Top := 0;
    Width := Self.ClientWidth - Left;
    Height := 60; //Self.ClientHeight - ComponentNotebook.Top;
  end;

  PageCount := 0;
  for I := 0 to RegCompList.PageCount-1 do begin
    // Component Notebook Pages
    RegCompPage := RegCompList.Pages[i];
    if RegCompPage.Name <> '' then
    Begin
      if (PageCount = 0) then
        ComponentNotebook.Pages.Strings[PageCount] := RegCompPage.Name
      else
        ComponentNotebook.Pages.Add(RegCompPage.Name);
      GlobalMouseSpeedButton := TSpeedButton.Create(Self);
      SelectionPointerPixmap:=LoadSpeedBtnPixMap('tmouse');
      with GlobalMouseSpeedButton do
      Begin
        Parent := ComponentNotebook.Page[PageCount];
        Enabled := True;
        Width := 25;
        Height := 25;
        OnClick := @ControlClick;
        Glyph := SelectionPointerPixmap;
        Visible := True;
        Flat := True;
        Down := True;
        Name := 'GlobalMouseSpeedButton'+IntToStr(PageCount);
      end;
      for x := 0 to RegCompPage.Count-1 do //for every component on the page....
      begin
        RegComp := RegCompPage.Items[x];
        IDEComponent := TIDEComponent.Create;
        IDEComponent.RegisteredComponent := RegComp;
        IDEComponent._SpeedButton(Self,ComponentNotebook.Page[PageCount]);
        IDEComponent.SpeedButton.OnClick := @ControlClick;
        IDEComponent.SpeedButton.Hint := RegComp.ComponentClass.ClassName;
        IDEComponent.SpeedButton.Name := IDEComponent.SpeedButton.Hint;
        IDEComponent.SpeedButton.ShowHint := True;
        IDECompList.Add(IDEComponent);
      end;
      inc(PageCount);
    end;
  end;
  ComponentNotebook.PageIndex := 0;   // Set it to the first page
  ComponentNotebook.OnPageChanged := @ControlClick;
  ComponentNotebook.Show;

  // compiler interface
  Compiler1 := TCompiler.Create;
  with Compiler1 do begin
    OnCommandLineCreate:=@OnCmdLineCreate;
  end;

  // MainIDE form events
  OnShow := @FormShow;
  OnClose := @FormClose;
  OnCloseQuery := @FormCloseQuery;

  // object inspector
  ObjectInspector1 := TObjectInspector.Create(Self);
  if (EnvironmentOptions.SaveWindowPositions) 
  and (EnvironmentOptions.WindowPositionsValid) then begin
    with EnvironmentOptions.ObjectInspectorOptions do
      ObjectInspector1.SetBounds(Left,Top,Width,Height);
  end else begin
    ObjectInspector1.SetBounds(
      0,Top+Height+30,230,Max(Screen.Height-Top-Height-120,50));
  end;
  ObjectInspector1.OnAddAvailComponent:=@OIOnAddAvailableComponent;
  ObjectInspector1.OnSelectComponentInOI:=@OIOnSelectComponent;
  PropertyEditorHook1:=TPropertyEditorHook.Create;
  ObjectInspector1.PropertyEditorHook:=PropertyEditorHook1;
  ObjectInspector1.Show;

  // create formeditor
  FormEditor1 := TFormEditor.Create;
  FormEditor1.Obj_Inspector := ObjectInspector1;

  // source editor / notebook
  SourceNotebook := TSourceNotebook.Create(Self);
  SourceNotebook.OnNewClicked := @OnSrcNotebookFileNew;
  SourceNotebook.OnOpenClicked := @ OnSrcNotebookFileOpen;
  SourceNotebook.OnOpenFileAtCursorClicked := @OnSrcNotebookFileOpenAtCursor;
  SourceNotebook.OnSaveClicked := @OnSrcNotebookFileSave;
  SourceNotebook.OnSaveAsClicked := @OnSrcNotebookFileSaveAs;
  SourceNotebook.OnCloseClicked := @OnSrcNotebookFileClose;
  SourceNotebook.OnSaveAllClicked := @OnSrcNotebookSaveAll;
  SourceNotebook.OnToggleFormUnitClicked := @OnSrcNotebookToggleFormUnit;
  SourceNotebook.OnProcessUserCommand := @OnSrcNotebookProcessCommand;
  SourceNotebook.OnShowUnitInfo := @OnSrcNoteBookShowUnitInfo;

  // find / replace dialog
  itmSearchFind.OnClick := @SourceNotebook.FindClicked;
  itmSearchFindNext.OnClick := @SourceNotebook.FindNextClicked;
  itmSearchFindPrevious.OnClick := @SourceNotebook.FindPreviousClicked;
  itmSearchFindInFiles.OnClick := @SourceNotebook.FindInFilesClicked;
  itmSearchReplace.OnClick := @SourceNotebook.ReplaceClicked;

  // message view
  FMessagesViewBoundsRectValid:=false;

  // macros
  MacroList:=TTransferMacroList.Create;
  MacroList.Add(TTransferMacro.Create('Col','',nil));
  MacroList.Add(TTransferMacro.Create('Row','',nil));
  MacroList.Add(TTransferMacro.Create('EdFile','',nil));
  MacroList.Add(TTransferMacro.Create('CurToken','',nil));
  MacroList.Add(TTransferMacro.Create('ProjFile','',nil));
  MacroList.Add(TTransferMacro.Create('ProjPath','',nil));
  MacroList.Add(TTransferMacro.Create('Save','',nil));
  MacroList.Add(TTransferMacro.Create('SaveAll','',nil));
  MacroList.Add(TTransferMacro.Create('Params','',nil));
  MacroList.Add(TTransferMacro.Create('TargetFile','',nil));
  MacroList.Add(TTransferMacro.Create('CompPath','',nil));
  MacroList.Add(TTransferMacro.Create('FPCSrcDir','',nil));
  MacroList.Add(TTransferMacro.Create('LazarusDir','',nil));
  MacroList.OnSubstitution:=@OnMacroSubstitution;

  // control selection (selected components on edited form)
  TheControlSelection:=TControlSelection.Create;
  TheControlSelection.OnChange:=@OnControlSelectionChanged;

writeln('TMainIDE.Create A ***********');
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  // load command line project or last project or create a new project
  if (ParamCount>0) and (ParamStr(ParamCount)[1]<>'-')
  and (ExtractFileExt(ParamStr(ParamCount))='.lpi')
  and (DoOpenProjectFile(ParamStr(ParamCount))=mrOk) then
    // command line project loaded
  else if (EnvironmentOptions.OpenLastprojectAtStart)
  and (FileExists(EnvironmentOptions.LastSavedProjectFile)) 
  and (DoOpenProjectFile(EnvironmentOptions.LastSavedProjectFile)=mrOk) then
  begin
    // last project loaded
writeln('TMainIDE.Create last project loaded successfully');
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  end else
    // create new project
    DoNewProject(ptApplication);
    
writeln('TMainIDE.Create B');
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
end;

destructor TMainIDE.Destroy;
begin
writeln('[TMainIDE.Destroy] A');
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  if Project<>nil then begin
    Project.Free;
    Project:=nil;
  end;
  TheControlSelection.OnChange:=nil;
  TheControlSelection.Free;
  TheControlSelection:=nil;
  FormEditor1.Free;
  FormEditor1:=nil;
  PropertyEditorHook1.Free;
  Compiler1.Free;
  MacroList.Free;
  EditorOpts.Free;
  EditorOpts:=nil;
  EnvironmentOptions.Free;
  EnvironmentOptions:=nil;
writeln('[TMainIDE.Destroy] B  -> inherited Destroy...');
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  inherited Destroy;
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
writeln('[TMainIDE.Destroy] END');
end;

procedure TMainIDE.OIOnAddAvailableComponent(AComponent:TComponent;
var Allowed:boolean);
begin
  //Allowed:=(not (AComponent is TGrabber));
end;

procedure TMainIDE.OIOnSelectComponent(AComponent:TComponent);
begin
  with TheControlSelection do begin
    BeginUpdate;
    Clear;
    Add(AComponent);
    EndUpdate;
  end;
  if AComponent.Owner is TControl then
    TControl(AComponent.Owner).Invalidate;
end;

Procedure TMainIDE.ToolButtonClick(Sender : TObject);
Begin
  Assert(False, 'Trace:TOOL BUTTON CLICK!');

end;

Procedure TMainIDE.FormPaint(Sender : TObject);
begin

end;

{------------------------------------------------------------------------------}
procedure TMainIDE.FormShow(Sender : TObject);
Begin

end;

procedure TMainIDE.FormClose(Sender : TObject; var Action: TCloseAction);
begin
  SaveEnvironment;
  if TheControlSelection<>nil then TheControlSelection.Clear;
  if SourceNoteBook<>nil then SourceNoteBook.ClearUnUsedEditorComponents(true);
end;

procedure TMainIDE.FormCloseQuery(Sender : TObject; var CanClose: boolean);
Begin
writeln('[TMainIDE.FormCloseQuery]');
  CanClose:=true;

  if SomethingOfProjectIsModified then begin
    if (MessageDlg('Project changed', 'Save changes to project?', 
      mtconfirmation, [mbOK, mbcancel], 0))=mrOK then
    begin
      CanClose:=DoSaveProject(false,false)<>mrAbort;
      if CanClose=false then exit;
    end;
  end;

  CanClose:=(DoCloseProject<>mrAbort);
End;

{------------------------------------------------------------------------------}
type 
  TMoveFlags = set of (mfTop, mfLeft);

procedure TMainIDE.LoadSpeedbuttons;
  
  function CreateButton(const AName, APixName: String; ANumGlyphs: Integer;
    var ALeft, ATop: Integer; const AMoveFlags: TMoveFlags;
    const AOnClick: TNotifyEvent): TSpeedButton;
  begin
    Result := TSpeedButton.Create(Self);
    with Result do
    begin
      Name := AName;
//      Parent := pnlSpeedButtons;
      Parent := Self;
      Enabled := True;
      Top := ATop;
      Left := ALeft;
      OnClick := AOnClick;
      Glyph := LoadSpeedBtnPixMap(APixName);
      NumGlyphs := ANumGlyphs;
      Flat := True;
      //Transparent:=True;
      if mfTop in AMoveFlags then Inc(ATop, Height + 1);
      if mfLeft in AMoveFlags then Inc(ALeft, Width + 1);
//writeln('---- W=',Width,',',Height,' Transparent=',Transparent);
      Visible := True;
    end;
  end;
var
  ButtonTop, ButtonLeft, n: Integer;

begin
(*
  pnlSpeedButtons := TPanel.Create(Self);
  with pnlSpeedButtons do
  begin
    Parent := Self;
    Visible := True;
    Name := 'pnlSpeedButtons';
    Top := 0;
    Caption := '';
  end;
*)

  ButtonTop := 1;
  ButtonLeft := 1;
  NewUnitSpeedBtn       := CreateButton('NewUnitSpeedBtn'      , 'btn_newunit'   , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuNewUnitClicked);
  OpenFileSpeedBtn      := CreateButton('OpenFileSpeedBtn'     , 'btn_openfile'  , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuOpenClicked);

  // store left
  n := ButtonLeft;
  OpenFileArrowSpeedBtn := CreateButton('OpenFileArrowSpeedBtn', 'btn_downarrow' , 1, ButtonLeft, ButtonTop, [mfLeft], @OpenFileDownArrowClicked);
  OpenFileArrowSpeedBtn.Width := 12;
  ButtonLeft := n+12+1;
  
  SaveSpeedBtn          := CreateButton('SaveSpeedBtn'         , 'btn_save'      , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuSaveClicked);
  SaveAllSpeedBtn       := CreateButton('SaveAllSpeedBtn'      , 'btn_saveall'   , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuSaveAllClicked);
  NewFormSpeedBtn       := CreateButton('NewFormSpeedBtn'      , 'btn_newform'   , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuNewFormClicked);
  ToggleFormSpeedBtn    := CreateButton('ToggleFormSpeedBtn'   , 'btn_toggleform', 1, ButtonLeft, ButtonTop, [mfLeft, mfTop], @mnuToggleFormUnitCLicked);

// new row
  ButtonLeft := 1;
  ViewUnitsSpeedBtn     := CreateButton('ViewUnitsSpeedBtn'    , 'btn_viewunits' , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuViewUnitsClicked);
  ViewFormsSpeedBtn     := CreateButton('ViewFormsSpeedBtn'    , 'btn_viewforms' , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuViewFormsClicked);   
  inc(ButtonLeft,12);
  RunSpeedButton        := CreateButton('RunSpeedButton'       , 'btn_run'       , 2, ButtonLeft, ButtonTop, [mfLeft], @mnuRunProjectClicked);
  PauseSpeedButton      := CreateButton('PauseSpeedButton'     , 'btn_pause'       , 2, ButtonLeft, ButtonTop, [mfLeft], @mnuPauseProjectClicked);
  PauseSpeedButton.Enabled:=false;
  StepIntoSpeedButton  := CreateButton('StepIntoSpeedButton'   , 'btn_stepinto'       , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuStepIntoProjectClicked);
  StepOverSpeedButton  := CreateButton('StepOverpeedButton'   , 'btn_stepover'       , 1, ButtonLeft, ButtonTop, [mfLeft, mfTop], @mnuStepOverProjectClicked);
  
//  pnlSpeedButtons.Width := ButtonLeft;
//  pnlSpeedButtons.Height := ButtonTop;
  

  // create the popupmenu for the OpenFileArrowSpeedBtn
  OpenFilePopUpMenu := TPopupMenu.Create(self);
  OpenFilePopupMenu.Name:='OpenFilePopupMenu';
  OpenFilePopupMenu.AutoPopup := False;
{ 
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := 'No files have been opened';
  MenuItem.OnClick := nil;
  OpenFilePopupMenu.Items.Add(MenuItem);
}
end;


{------------------------------------------------------------------------------}
procedure TMainIDE.LoadMainMenu;

  procedure AddRecentSubMenu(ParentMenuItem: TMenuItem; FileList: TStringList;
    OnClickEvent: TNotifyEvent);
  var i: integer;
    NewMenuItem: TMenuItem;
  begin
    for i:=0 to FileList.Count-1 do begin
      NewMenuItem:= TMenuItem.Create(Self);
      NewMenuItem.Name:=ParentMenuItem.Name+'Recent'+IntToStr(i);
      NewMenuItem.Caption := FileList[i];
      NewMenuItem.OnClick := OnClickEvent;
      ParentMenuItem.Add(NewMenuItem);
    end;
  end;

begin

//--------------
// The Menu
//--------------

  mnuMain := TMainMenu.Create(Self);
  mnuMain.Name:='mnuMainMenu';
  Menu := mnuMain;

//--------------
// Main menu
//--------------

  mnuFile := TMenuItem.Create(Self);
  mnuFile.Name:='mnuFile';
  mnuFile.Caption := '&File';
  mnuMain.Items.Add(mnuFile);

  mnuEdit := TMenuItem.Create(Self);
  mnuEdit.Name:='mnuEdit';
  mnuEdit.Caption := '&Edit';
  mnuMain.Items.Add(mnuEdit);

  mnuSearch := TMenuItem.Create(Self);
  mnuSearch.Name:='mnuSearch';
  mnuSearch.Caption := '&Search';
  mnuMain.Items.Add(mnuSearch);

  mnuView := TMenuItem.Create(Self);
  mnuView.Name:='mnuView';
  mnuView.Caption := '&View';
  mnuMain.Items.Add(mnuView);

  mnuProject := TMenuItem.Create(Self);
  mnuProject.Name:='mnuProject';
  mnuProject.Caption := '&Project';
  mnuMain.Items.Add(mnuProject);

  mnuEnvironment := TMenuItem.Create(Self);
  mnuEnvironment.Name:='mnuEnvironment';
  mnuEnvironment.Caption := 'E&nvironment';
  mnuMain.Items.Add(mnuEnvironment);

//--------------
// File
//--------------
  
  itmFileNew := TMenuItem.Create(Self);
  itmFileNew.Name:='itmFileNew';
  itmFileNew.Caption := 'New Unit';
  itmFileNew.OnClick := @mnuNewUnitClicked; // ToDo:  new dialog
  mnuFile.Add(itmFileNew);

  itmFileNewForm := TMenuItem.Create(Self);
  itmFileNewForm.Name:='itmFileNewForm';
  itmFileNewForm.Caption := 'New Form';
  itmFileNewForm.OnClick := @mnuNewFormClicked;
  mnuFile.Add(itmFileNewForm);

  mnuFile.Add(CreateSeperator);

  itmFileOpen := TMenuItem.Create(Self);
  itmFileOpen.Name:='itmFileOpen';
  itmFileOpen.Caption := 'Open';
  itmFileOpen.OnClick := @mnuOpenClicked;
  mnuFile.Add(itmFileOpen);

  itmFileRecentOpen := TMenuItem.Create(Self);
  itmFileRecentOpen.Name:='itmFileRecentOpen';
  itmFileRecentOpen.Caption := 'Open Recent';
  mnuFile.Add(itmFileRecentOpen);
  
  AddRecentSubMenu(itmFileRecentOpen,EnvironmentOptions.RecentOpenFiles,
                    @mnuOpenClicked);

  itmFileSave := TMenuItem.Create(Self);
  itmFileSave.Name:='itmFileSave';
  itmFileSave.Caption := 'Save';
  itmFileSave.OnClick := @mnuSaveClicked;
  mnuFile.Add(itmFileSave);

  itmFileSaveAs := TMenuItem.Create(Self);
  itmFileSaveAs.Name:='itmFileSaveAs';
  itmFileSaveAs.Caption := 'Save As';
  itmFileSaveAs.OnClick := @mnuSaveAsClicked;
  mnuFile.Add(itmFileSaveAs);

  itmFileSaveAll := TMenuItem.Create(Self);
  itmFileSaveAll.Name:='itmFileSaveAll';
  itmFileSaveAll.Caption := 'Save All';
  itmFileSaveAll.OnClick := @mnuSaveAllClicked;
  mnuFile.Add(itmFileSaveAll);

  itmFileClose := TMenuItem.Create(Self);
  itmFileClose.Name:='itmFileClose';
  itmFileClose.Caption := 'Close';
  itmFileClose.Enabled := False;
  itmFileClose.OnClick := @mnuCloseClicked;
  mnuFile.Add(itmFileClose);

  mnuFile.Add(CreateSeperator);

  itmFileQuit := TMenuItem.Create(Self);
  itmFileQuit.Name:='itmFileQuit';
  itmFileQuit.Caption := 'Quit';
  itmFileQuit.OnClick := @mnuQuitClicked;
  mnuFile.Add(itmFileQuit);

//--------------
// Edit
//--------------

  itmEditUndo := TMenuItem.Create(nil);
  itmEditUndo.Name:='itmEditUndo';
  itmEditUndo.Caption := 'Undo';
  mnuEdit.Add(itmEditUndo);

  itmEditRedo := TMenuItem.Create(nil);
  itmEditRedo.Name:='itmEditRedo';
  itmEditRedo.Caption := 'Redo';
  mnuEdit.Add(itmEditRedo);

  mnuEdit.Add(CreateSeperator);

  itmEditCut  := TMenuItem.Create(nil);
  itmEditCut.Name:='itmEditCut';
  itmEditCut.Caption := 'Cut';
  mnuEdit.Add(itmEditCut);

  itmEditCopy := TMenuItem.Create(nil);
  itmEditCopy.Name:='itmEditCopy';
  itmEditCopy.Caption := 'Copy';
  mnuEdit.Add(itmEditCopy);

  itmEditPaste := TMenuItem.Create(nil);
  itmEditPaste.Name:='itmEditPaste';
  itmEditPaste.Caption := 'Paste';
  mnuEdit.Add(itmEditPaste);

//--------------
// Search
//--------------

  itmSearchFind := TMenuItem.Create(nil);
  itmSearchFind.Name:='itmSearchFind';
  itmSearchFind.Caption := 'Find';
  mnuSearch.add(itmSearchFind);

  itmSearchFindNext := TMenuItem.Create(nil);
  itmSearchFindNext.Name:='itmSearchFindNext';
  itmSearchFindNext.Caption := 'Find &Next';
  itmSearchFindNext.Enabled := False;
  mnuSearch.add(itmSearchFindNext);

  itmSearchFindPrevious := TMenuItem.Create(nil);
  itmSearchFindPrevious.Name:='itmSearchFindPrevious';
  itmSearchFindPrevious.Caption := 'Find &Previous';
  itmSearchFindPrevious.Enabled := False;
  mnuSearch.add(itmSearchFindPrevious);

  itmSearchFindInFiles := TMenuItem.Create(nil);
  itmSearchFindInFiles.Name:='itmSearchFindInFiles';
  itmSearchFindInFiles.Caption := 'Find &in files';
  itmSearchFindInFiles.Enabled := False;
  mnuSearch.add(itmSearchFindInFiles);

  itmSearchReplace := TMenuItem.Create(nil);
  itmSearchReplace.Name:='itmSearchReplace';
  itmSearchReplace.Caption := 'Replace';
  mnuSearch.add(itmSearchReplace);

//--------------
// View
//--------------

  itmViewInspector := TMenuItem.Create(Self);
  itmViewInspector.Name:='itmViewInspector';
  itmViewInspector.Caption := 'Object Inspector';
  itmViewInspector.OnClick := @mnuViewInspectorClicked;
  mnuView.Add(itmViewInspector);

  itmViewProject  := TMenuItem.Create(Self);
  itmViewProject.Name:='itmViewProject';
  itmViewProject.Caption := 'Project Explorer';
  mnuView.Add(itmViewProject);

  mnuView.Add(CreateSeperator);

  itmViewCodeExplorer := TMenuItem.Create(Self);
  itmViewCodeExplorer.Name:='itmViewCodeExplorer';
  itmViewCodeExplorer.Caption := 'Code Explorer';
  itmViewCodeExplorer.OnClick := @mnuViewCodeExplorerClick;
  mnuView.Add(itmViewCodeExplorer);

  mnuView.Add(CreateSeperator);

  itmViewUnits := TMenuItem.Create(Self);
  itmViewUnits.Name:='itmViewUnits';
  itmViewUnits.Caption := 'Units...';
  itmViewUnits.OnClick := @mnuViewUnitsClicked;
  mnuView.Add(itmViewUnits);

  itmViewForms := TMenuItem.Create(Self);
  itmViewForms.Name:='itmViewForms';
  itmViewForms.Caption := 'Forms...';
  itmViewForms.OnClick := @mnuViewFormsClicked;
  mnuView.Add(itmViewForms);

  mnuView.Add(CreateSeperator);

  itmViewMessage := TMenuItem.Create(Self);
  itmViewMessage.Name:='itmViewMessage';
  itmViewMessage.Caption := 'Messages';
  itmViewMessage.OnClick := @mnuViewMessagesClick;
  mnuView.Add(itmViewMessage);

//--------------
// Project
//--------------

  itmProjectNew := TMenuItem.Create(Self);
  itmProjectNew.Name:='itmProjectNew';
  itmProjectNew.Caption := 'New Project';
  itmProjectNew.OnClick := @mnuNewProjectClicked;
  mnuProject.Add(itmProjectNew);

  itmProjectOpen := TMenuItem.Create(Self);
  itmProjectOpen.Name:='itmProjectOpen';
  itmProjectOpen.Caption := 'Open Project';
  itmProjectOpen.OnClick := @mnuOpenProjectClicked;
  mnuProject.Add(itmProjectOpen);

  itmProjectRecentOpen := TMenuItem.Create(Self);
  itmProjectRecentOpen.Name:='itmProjectRecentOpen';
  itmProjectRecentOpen.Caption := 'Open Recent Project';
  mnuProject.Add(itmProjectRecentOpen);
  
  AddRecentSubMenu(itmProjectRecentOpen,EnvironmentOptions.RecentProjectFiles,
                   @mnuOpenProjectClicked);

  itmProjectSave := TMenuItem.Create(Self);
  itmProjectSave.Name:='itmProjectSave';
  itmProjectSave.Caption := 'Save Project';
  itmProjectSave.OnClick := @mnuSaveProjectClicked;
  mnuProject.Add(itmProjectSave);

  itmProjectSaveAs := TMenuItem.Create(Self);
  itmProjectSaveAs.Name:='itmProjectSaveAs';
  itmProjectSaveAs.Caption := 'Save Project As...';
  itmProjectSaveAs.OnClick := @mnuSaveProjectAsClicked;
  mnuProject.Add(itmProjectSaveAs);

  mnuProject.Add(CreateSeperator);

  itmProjectAddTo := TMenuItem.Create(Self);
  itmProjectAddTo.Name:='itmProjectAddTo';
  itmProjectAddTo.Caption := 'Add active unit to Project';
  itmProjectAddTo.OnClick := @mnuAddToProjectClicked;
  mnuProject.Add(itmProjectAddTo);

  itmProjectRemoveFrom := TMenuItem.Create(Self);
  itmProjectRemoveFrom.Name:='itmProjectRemoveFrom';
  itmProjectRemoveFrom.Caption := 'Remove from Project';
  itmProjectRemoveFrom.OnClick := @mnuRemoveFromProjectClicked;
  mnuProject.Add(itmProjectRemoveFrom);

  mnuProject.Add(CreateSeperator);

  itmProjectViewSource := TMenuItem.Create(Self);
  itmProjectViewSource.Name:='itmProjectViewSource';
  itmProjectViewSource.Caption := 'View Source';
  itmProjectViewSource.OnClick := @mnuViewProjectSourceClicked;
  mnuProject.Add(itmProjectViewSource);

  mnuProject.Add(CreateSeperator);

  itmProjectBuild := TMenuItem.Create(Self);
  itmProjectBuild.Name:='itmProjectBuild';
  itmProjectBuild.Caption := 'Build';
  itmProjectBuild.OnClick := @mnuBuildProjectClicked;
  mnuProject.Add(itmProjectBuild);

  itmProjectRun := TMenuItem.Create(Self);
  itmProjectRun.Name:='itmProjectRun';
  itmProjectRun.Caption := 'Run';
  itmProjectRun.OnClick := @mnuRunProjectClicked;
  mnuProject.Add(itmProjectRun);

  itmProjectPause := TMenuItem.Create(Self);
  itmProjectPause.Name:='itmProjectPause';
  itmProjectPause.Caption := 'Pause';
  itmProjectPause.OnClick := @mnuPauseProjectClicked;
  itmProjectPause.Enabled := false;
  mnuProject.Add(itmProjectPause);

  itmProjectStepInto := TMenuItem.Create(Self);
  itmProjectStepInto.Name:='itmProjectStepInto';
  itmProjectStepInto.Caption := 'Step into';
  itmProjectStepInto.OnClick := @mnuStepIntoProjectClicked;
  mnuProject.Add(itmProjectStepInto);

  itmProjectStepOver := TMenuItem.Create(Self);
  itmProjectStepOver.Name:='itmProjectStepOver';
  itmProjectStepOver.Caption := 'Step over';
  itmProjectStepOver.OnClick := @mnuStepOverProjectClicked;
  mnuProject.Add(itmProjectStepOver);

  itmProjectRunToCursor := TMenuItem.Create(Self);
  itmProjectRunToCursor.Name:='itmProjectRunToCursor';
  itmProjectRunToCursor.Caption := 'Run to cursor';
  itmProjectRunToCursor.OnClick := @mnuRunToCursorProjectClicked;
  mnuProject.Add(itmProjectRunToCursor);

  itmProjectStop := TMenuItem.Create(Self);
  itmProjectStop.Name:='itmProjectStop';
  itmProjectStop.Caption := 'Stop';
  itmProjectStop.OnClick := @mnuStopProjectClicked;
  mnuProject.Add(itmProjectStop);

  mnuProject.Add(CreateSeperator);

  itmProjectCompilerSettings := TMenuItem.Create(Self);
  itmProjectCompilerSettings.Name:='itmProjectCompilerSettings';
  itmProjectCompilerSettings.Caption := 'Compiler Options...';
  itmProjectCompilerSettings.OnClick := @mnuProjectCompilerSettingsClicked;
  mnuProject.Add(itmProjectCompilerSettings);
  
  itmProjectOptions := TMenuItem.Create(Self);
  itmProjectOptions.Name:='itmProjectOptions';
  itmProjectOptions.Caption := 'Project Options...';
  itmProjectOptions.OnClick := @mnuProjectOptionsClicked;
  mnuProject.Add(itmProjectOptions);

//--------------
// Environment
//--------------

  itmEnvGeneralOptions := TMenuItem.Create(nil);
  itmEnvGeneralOptions.Name:='itmEnvGeneralOptions';
  itmEnvGeneralOptions.Caption := 'General options';
  itmEnvGeneralOptions.OnCLick := @mnuEnvGeneralOptionsClicked;
  mnuEnvironment.Add(itmEnvGeneralOptions);

  itmEnvEditorOptions := TMenuItem.Create(nil);
  itmEnvEditorOptions.Name:='itmEnvEditorOptions';
  itmEnvEditorOptions.Caption := 'Editor options';
  itmEnvEditorOptions.OnCLick := @mnuEnvEditorOptionsClicked;
  mnuEnvironment.Add(itmEnvEditorOptions);

end;
{------------------------------------------------------------------------------}

function TMainIDE.CreateSeperator : TMenuItem;
begin
  itmSeperator := TMenuItem.Create(Self);
  itmSeperator.Caption := '-';
  Result := itmSeperator;
end;

{------------------------------------------------------------------------------}

Procedure TMainIDE.mnuToggleFormUnitClicked(Sender : TObject);
Begin
  writeln('Toggle form clicked');
  FCodeLastActivated:=not FCodeLastActivated;
  DoBringToFrontFormOrUnit;
end;

Procedure TMainIDE.SetDesigning(Control : TComponent; Value : Boolean);
Begin
  Writeln('Setting designing');
  Control.SetDesigning(Value);
  Writeln('Set');
end;


Function TMainIDE.SearchPaths : String;
Begin
  Result :=  Project.CompilerOptions.OtherUnitFiles;
End;

{
------------------------------------------------------------------------
-------------------ControlClick-----------------------------------------
------------------------------------------------------------------------
}

Procedure TMainIDE.ControlClick(Sender : TObject);
var
  I : Integer;
  IDECOmp : TIDEComponent;
  Speedbutton : TSpeedbutton;
  Temp : TControl;
begin
  if Sender is TSpeedButton then
  Begin
//    Writeln('sender is a speedbutton');
//    Writeln('The name is '+TSpeedbutton(sender).name);
    SpeedButton := TSpeedButton(Sender);
//    Writeln('Speedbutton s Name is '+SpeedButton.name);
    //find the IDECOmponent that has this speedbutton
    IDEComp := IDECompList.FindCompBySpeedButton(SpeedButton);
    if SelectedComponent <> nil then
      TIDeComponent(
       IdeCompList.FindCompByRegComponent(SelectedComponent)).SpeedButton.Down
         := False
    else begin
      Temp := nil;
      for i := 0 to 
              ComponentNotebook.Page[ComponentNotebook.Pageindex].ControlCount-1
      do begin
        if CompareText(
            TControl(ComponentNotebook.
                Page[ComponentNotebook.Pageindex].Controls[I]).Name
            ,'GlobalMouseSpeedButton'
              +IntToStr(ComponentNotebook.Pageindex)) = 0 then
        begin
          temp := TControl(ComponentNotebook.
                              Page[ComponentNotebook.Pageindex].Controls[i]);
          Break;
        end;
      end;
      if temp <> nil then
        TSpeedButton(Temp).down := False
      else begin
        Writeln('[TMainIDE.ControlClick] ERROR - Control ',
           'GlobalMouseSpeedButton',
           IntToStr(ComponentNotebook.PageIndex),' not found');
        Halt;
      end;
    end;
    if IDECOmp <> nil then Begin
      //draw this button down
      SpeedButton.Down := True;
      SelectedComponent := IDEComp.RegisteredComponent;
    end else begin
      SelectedComponent := nil;
      Temp := nil;
      for i := 0 to 
          ComponentNotebook.Page[ComponentNotebook.Pageindex].ControlCount-1 do
      begin
        if CompareText(
          TControl(ComponentNotebook.
               Page[ComponentNotebook.Pageindex].Controls[I]).Name
           ,'GlobalMouseSpeedButton'
             +IntToStr(ComponentNotebook.Pageindex)) = 0 then
        begin
          temp := TControl(ComponentNotebook.
                                 Page[ComponentNotebook.Pageindex].Controls[i]);
          Break;
        end;
      end;
      if temp <> nil then
        TSpeedButton(Temp).down := True
      else begin
        Writeln('[TMainIDE.ControlClick] ERROR - Control '
           +'GlobalMouseSpeedButton'
           +IntToStr(ComponentNotebook.Pageindex)+' not found');
        Halt;
      end;
    end;
  end
  else
  Begin
//    Writeln('must be nil');
    //draw old speedbutton up
    if SelectedComponent <> nil then
      TIDeComponent(
        IdeCompList.FindCompByRegComponent(SelectedComponent)).SpeedButton.Down
           := False;
    SelectedComponent := nil;
    Temp := nil;
    for i := 0 to 
          ComponentNotebook.Page[ComponentNotebook.Pageindex].ControlCount-1 do
    begin
      if CompareText(
         TControl(ComponentNotebook.
            Page[ComponentNotebook.Pageindex].Controls[I]).Name
         ,'GlobalMouseSpeedButton'
           +IntToStr(ComponentNotebook.Pageindex)) = 0 then
      begin
        temp := TControl(ComponentNotebook.
                              Page[ComponentNotebook.Pageindex].Controls[i]);
        Break;
      end;
    end;
    if temp <> nil then
      TSpeedButton(Temp).down := True
    else begin
      Writeln('[TMainIDE.ControlClick] ERROR - Control '
        +'GlobalMouseSpeedButton'
        +IntToStr(ComponentNotebook.Pageindex)+' not found');
      Halt;
    end;
  end;
//  Writeln('Exiting ControlClick');
end;



{------------------------------------------------------------------------------}
procedure TMainIDE.mnuNewUnitClicked(Sender : TObject);
begin
  DoNewEditorUnit(nuUnit);
end;

procedure TMainIDE.mnuNewFormClicked(Sender : TObject);
begin
  DoNewEditorUnit(nuForm);
end;

procedure TMainIDE.mnuOpenClicked(Sender : TObject);
var OpenDialog:TOpenDialog;
  AFilename: string;
begin
  if (Sender=itmFileOpen) or (Sender=OpenFileSpeedBtn) then begin
    OpenDialog:=TOpenDialog.Create(Application);
    try
      OpenDialog.Title:='Open file';
      OpenDialog.InitialDir:=EnvironmentOptions.LastOpenDialogDir;
      if OpenDialog.Execute then begin
        AFilename:=ExpandFilename(OpenDialog.Filename);
        EnvironmentOptions.LastOpenDialogDir:=ExtractFilePath(AFilename);
        if DoOpenEditorFile(AFilename,false)=mrOk then begin
          EnvironmentOptions.AddToRecentOpenFiles(AFilename);
          SaveEnvironment;
        end;
      end;
    finally
      OpenDialog.Free;
    end;
  end else if Sender is TMenuItem then begin
    AFileName:=ExpandFilename(TMenuItem(Sender).Caption);
    if DoOpenEditorFile(AFilename,false)=mrOk then begin
      EnvironmentOptions.AddToRecentOpenFiles(AFilename);
      SaveEnvironment;
    end;
  end;
end;

procedure TMainIDE.mnuOpenFileAtCursorClicked(Sender : TObject);
begin
  if SourceNoteBook.NoteBook=nil then exit;
  DoOpenFileAtCursor(Sender);  
end;

procedure TMainIDE.mnuSaveClicked(Sender : TObject);
begin
  if SourceNoteBook.NoteBook=nil then exit;
  DoSaveEditorUnit(SourceNoteBook.NoteBook.PageIndex,false,false);
end;

procedure TMainIDE.mnuSaveAsClicked(Sender : TObject);
begin
  if SourceNoteBook.NoteBook=nil then exit;
  DoSaveEditorUnit(SourceNoteBook.NoteBook.PageIndex,true,false);
end;

procedure TMainIDE.mnuSaveAllClicked(Sender : TObject);
begin
  if SourceNoteBook.NoteBook=nil then exit;
  DoSaveAll;  
end;

procedure TMainIDE.mnuCloseClicked(Sender : TObject);
begin
  if SourceNoteBook.NoteBook=nil then exit;
  DoCloseEditorUnit(SourceNoteBook.NoteBook.PageIndex,true);
end;

Procedure TMainIDE.OnSrcNotebookFileNew(Sender : TObject);
begin
  mnuNewFormClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookFileClose(Sender : TObject);
begin
  mnuCloseClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookFileOpen(Sender : TObject);
begin
  mnuOpenClicked(Sender);
end;

Procedure TMainIDE.OnSrcNoteBookFileOpenAtCursor(Sender : TObject);
begin
  mnuOpenFileAtCursorClicked(Sender);  
end;

Procedure TMainIDE.OnSrcNotebookFileSave(Sender : TObject);
begin
  mnuSaveClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookFileSaveAs(Sender : TObject);
begin
  mnuSaveAsClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookSaveAll(Sender : TObject);
begin
  mnuSaveAllClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookToggleFormUnit(Sender : TObject);
begin
  mnuToggleFormUnitClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookProcessCommand(Sender: TObject;
  Command: integer;  var Handled: boolean);
begin
  Handled:=true;
  case Command of
   ecBuild:
    begin
      DoBuildProject;
    end;
   ecRun:
    begin
      if ToolStatus=itNone then
        if DoBuildProject<>mrOk then begin
          Handled:=false;
          exit;
        end;
      DoRunProject;
    end;
   ecPause:
    begin
      DoPauseProject;
    end;
   ecStepInto:
    begin
      if ToolStatus=itNone then
        if DoBuildProject<>mrOk then begin
          Handled:=false;
          exit;
        end;
      DoStepIntoProject;
    end;
   ecStepOver:
    begin
      if ToolStatus=itNone then
        if DoBuildProject<>mrOk then begin
          Handled:=false;
          exit;
        end;
      DoStepOverProject;
    end;
   ecRunToCursor:
    begin
      if ToolStatus=itNone then
        if DoBuildProject<>mrOk then begin
          Handled:=false;
          exit;
        end;
      DoRunToCursor;
    end;
   ecStopProgram:
    begin
      DoStopProject;
    end;
   ecFindProcedureDefinition,ecFindProcedureMethod:
    begin
      DoJumpToProcedureSection;
    end;
   ecCompleteCode:
    begin
      DoCompleteCodeAtCursor;
    end;
  else
    Handled:=false;
  end;
end;

procedure TMainIDE.OnSrcNoteBookShowUnitInfo(Sender: TObject);
var ActiveSrcEdit:TSourceEditor;
  ActiveUnitInfo:TUnitInfo;
  ShortUnitName, AFilename: string;
begin
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) then exit;
  ShortUnitName:=ExtractFileName(ActiveUnitInfo.Filename);
  if ShortUnitName='' then
    ShortUnitName:='(unsaved)';
  AFilename:=ActiveUnitInfo.Filename;
  if AFileName='' then
    AFileName:='(unsaved)';
  ShowUnitInfoDlg(ShortUnitName,
    LazSyntaxHighlighterNames[ActiveUnitInfo.SyntaxHighlighter],
    ActiveUnitInfo.IsPartOfProject, length(ActiveSrcEdit.Source.Text),
    ActiveSrcEdit.Source.Count,AFilename);
end;

{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{----------------OpenFileDownArrowClicked--------------------------------------}
{------------------------------------------------------------------------------}

Procedure TMainIDE.OpenFileDownArrowClicked(Sender : TObject);
Begin
  //display the PopupMenu
  if OpenFilePopupMenu.Items.Count > 0 then
    OpenFilePopupMenu.Popup(0,0);
end;

//==============================================================================
{
  This function creates a LFM file from any form.
  To create the resource file use the program lazres or the
  LFMtoLFCfile function.
}
function CreateLFM(AForm:TCustomForm):integer;
// 0 = ok
// -1 = error while streaming AForm to binary stream
// -2 = error while streaming binary stream to text file
var BinStream,TxtMemStream:TMemoryStream;
  Driver: TAbstractObjectWriter;
  Writer:TWriter;
  TxtFileStream:TFileStream;
begin
  Result:=0;
  BinStream:=TMemoryStream.Create;
  try
    try
      Driver:=TBinaryObjectWriter.Create(BinStream,4096);
      try
        Writer:=TWriter.Create(Driver);
        try
          Writer.WriteDescendent(AForm,nil);
        finally
          Writer.Free;
        end;
      finally
        Driver.Free;
      end;
    except
      Result:=-1;
      exit;
    end;
    try
      // transform binary to text and save LFM file
      TxtMemStream:=TMemoryStream.Create;
      TxtFileStream:=TFileStream.Create(lowercase(AForm.ClassName)+'.lfm'
                           ,fmCreate);
      try
        BinStream.Position:=0;
        ObjectBinaryToText(BinStream,TxtMemStream);
        TxtMemStream.Position:=0;
        TxtFileStream.CopyFrom(TxtMemStream,TxtMemStream.Size);
      finally
        TxtMemStream.Free;
        TxtFileStream.Free;
      end;
    except
      Result:=-2;
      exit;
    end;
  finally
    BinStream.Free;
  end;
end;

//==============================================================================

Procedure TMainIDE.SetDefaultsforForm(aForm : TCustomForm);
Begin
{$IFDEF IDE_DEBUG}
writeln('[TMainIDE.SetDefaultsforForm] A');
{$ENDIF}
  aForm.Designer := TDesigner.Create(aForm, TheControlSelection);
{$IFDEF IDE_DEBUG}
writeln('[TMainIDE.SetDefaultsforForm] B');
{$ENDIF}
  with TDesigner(aForm.Designer) do begin
    FormEditor := FormEditor1;
    OnGetSelectedComponentClass:=@OnDesignerGetSelectedComponentClass;
    OnUnselectComponentClass:=@OnDesignerUnselectComponentClass;
    OnSetDesigning:=@OnDesignerSetDesigning;
    OnComponentListChanged:=@OnDesignerComponentListChanged;
    OnPropertiesChanged:=@OnDesignerPropertiesChanged;
    OnAddComponent:=@OnDesignerAddComponent;
    OnRemoveComponent:=@OnDesignerRemoveComponent;
    OnGetNonVisualCompIconCanvas:=@IDECompList.OnGetNonVisualCompIconCanvas;
    OnModified:=@OnDesignerModified;
  end;
end;


{------------------------------------------------------------------------------}

procedure TMainIDE.mnuQuitClicked(Sender : TObject);
var CanClose: boolean;
begin
  CanClose:=true;
  OnCloseQuery(Sender, CanClose);
{$IFDEF IDE_DEBUG}
writeln('TMainIDE.mnuQuitClicked 1');
{$ENDIF}
  if CanClose then Close;
{$IFDEF IDE_DEBUG}
writeln('TMainIDE.mnuQuitClicked 2');
{$ENDIF}
end;

{------------------------------------------------------------------------------}
procedure TMainIDE.mnuViewInspectorClicked(Sender : TObject);
begin
  ObjectInspector1.Show;
end;

{------------------------------------------------------------------------------}

Procedure TMainIDE.mnuViewUnitsClicked(Sender : TObject);
begin
  DoViewUnitsAndForms(false);
end;

Procedure TMainIDE.mnuViewFormsClicked(Sender : TObject);
Begin
  DoViewUnitsAndForms(true);
end;

Procedure TMainIDE.mnuViewCodeExplorerClick(Sender : TObject);
begin
  SourceNotebook.Show;
end;

Procedure TMainIDE.mnuViewMessagesClick(Sender : TObject);
Begin
  MessagesView.Show;
End;


{------------------------------------------------------------------------------}

Procedure TMainIDE.mnuNewProjectClicked(Sender : TObject);
var
  NewProjectType: TProjectType;
Begin
  if ChooseNewProject(NewProjectType)=mrCancel then exit;
  DoNewProject(NewprojectType);
end;

Procedure TMainIDE.mnuOpenProjectClicked(Sender : TObject);
var OpenDialog:TOpenDialog;
  AFileName: string;
begin
  if Sender=itmProjectOpen then begin
    OpenDialog:=TOpenDialog.Create(Application);
    try
      OpenDialog.Title:='Open Project File (*.lpi)';
      OpenDialog.InitialDir:=EnvironmentOptions.LastOpenDialogDir;
      if OpenDialog.Execute then begin
        AFilename:=ExpandFilename(OpenDialog.Filename);
        EnvironmentOptions.LastOpenDialogDir:=ExtractFilePath(AFilename);
        if DoOpenProjectFile(AFilename)=mrOk then begin
          EnvironmentOptions.AddToRecentProjectFiles(AFilename);
          SaveEnvironment;
        end;
      end;
    finally
      OpenDialog.Free;
    end;
  end else if Sender is TMenuItem then begin
    AFileName:=ExpandFilename(TMenuItem(Sender).Caption);
    if DoOpenProjectFile(AFilename)=mrOk then begin
      EnvironmentOptions.AddToRecentProjectFiles(AFilename);
      SaveEnvironment;
    end;
  end;
end;

Procedure TMainIDE.mnuSaveProjectClicked(Sender : TObject);
Begin
  DoSaveProject(false,false);
end;

procedure TMainIDE.mnuSaveProjectAsClicked(Sender : TObject);
begin
  DoSaveProject(true,false);
end;

procedure TMainIDE.mnuAddToProjectClicked(Sender : TObject);
begin
  DoAddActiveUnitToProject;
end;

procedure TMainIDE.mnuRemoveFromProjectClicked(Sender : TObject);
begin
  DoRemoveFromProjectDialog;
end;

procedure TMainIDE.mnuViewProjectSourceClicked(Sender : TObject);
begin
  DoOpenMainUnit(false);
end;

Procedure TMainIDE.mnuBuildProjectClicked(Sender : TObject);
Begin
  DoBuildProject;
end;

Procedure TMainIDE.mnuRunProjectClicked(Sender : TObject);
begin
  DoRunProject;
end;

Procedure TMainIDE.mnuPauseProjectClicked(Sender : TObject);
begin
  DoPauseProject;
end;

Procedure TMainIDE.mnuStepIntoProjectClicked(Sender : TObject);
begin
  DoStepIntoProject;
end;

Procedure TMainIDE.mnuStepOverProjectClicked(Sender : TObject);
begin
  DoStepOverProject;
end;

Procedure TMainIDE.mnuRunToCursorProjectClicked(Sender : TObject);
begin
  DoRunToCursor;
end;

Procedure TMainIDE.mnuStopProjectClicked(Sender : TObject);
begin
  DoStopProject;
end;

procedure TMainIDE.mnuProjectCompilerSettingsClicked(Sender : TObject);
var frmCompilerOptions:TfrmCompilerOptions;
begin
  frmCompilerOptions:=TfrmCompilerOptions.Create(Application);
  try
    frmCompilerOptions.CompilerOpts:=Project.CompilerOptions;
    frmCompilerOptions.GetCompilerOptions;
    if frmCompilerOptions.ShowModal=mrOk then begin
      SourceNoteBook.SearchPaths:=SearchPaths;
    end;
  finally
    frmCompilerOptions.Free;
  end;
end;

procedure TMainIDE.mnuProjectOptionsClicked(Sender : TObject);
begin
  if ShowProjectOptionsDialog(Project)=mrOk then begin
    
  end;
end;

//------------------------------------------------------------------------------

procedure TMainIDE.SaveDesktopSettings(
  TheEnvironmentOptions: TEnvironmentOptions);
begin
  with TheEnvironmentOptions do begin
    MainWindowBounds:=BoundsRect;
    SourceEditorBounds:=SourceNoteBook.BoundsRect;
    MessagesViewBoundsValid:=FMessagesViewBoundsRectValid;
    MessagesViewBounds:=MessagesView.BoundsRect;
    ObjectInspectorOptions.Assign(ObjectInspector1);
    WindowPositionsValid:=true;
  end;
end;

procedure TMainIDE.LoadDesktopSettings(
  TheEnvironmentOptions: TEnvironmentOptions);
begin
  with TheEnvironmentOptions do begin
    if WindowPositionsValid then begin
      // set window positions
      BoundsRect:=MainWindowBounds;
      SourceNoteBook.BoundsRect:=SourceEditorBounds;
      if MessagesViewBoundsValid then begin
        MessagesView.BoundsRect:=MessagesViewBounds;
        FMessagesViewBoundsRectValid:=true;
      end;
      ObjectInspectorOptions.AssignTo(ObjectInspector1);
    end;
  end;
  // set global variables
  with CodeToolBoss.GlobalValues do begin
    Variables[ExternalMacroStart+'LazarusSrcDir']:=
      TheEnvironmentOptions.LazarusDirectory;
    Variables[ExternalMacroStart+'FPCSrcDir']:=
      TheEnvironmentOptions.FPCSourceDirectory;
  end;
end;

procedure TMainIDE.OnLoadEnvironmentSettings(Sender: TObject; 
  TheEnvironmentOptions: TEnvironmentOptions);
begin
  LoadDesktopSettings(TheEnvironmentOptions);
end;

procedure TMainIDE.OnSaveEnvironmentSettings(Sender: TObject; 
  TheEnvironmentOptions: TEnvironmentOptions);
begin
  SaveDesktopSettings(TheEnvironmentOptions);
end;

procedure TMainIDE.mnuEnvGeneralOptionsClicked(Sender : TObject);
var EnvironmentOptionsDialog: TEnvironmentOptionsDialog;
Begin
  EnvironmentOptionsDialog:=TEnvironmentOptionsDialog.Create(Application);
  try
    with EnvironmentOptionsDialog do begin
      SaveDesktopSettings(EnvironmentOptions);
      OnLoadEnvironmentSettings:=@Self.OnLoadEnvironmentSettings;
      OnSaveEnvironmentSettings:=@Self.OnSaveEnvironmentSettings;
      ReadSettings(EnvironmentOptions);
      if ShowModal=mrOk then begin
        WriteSettings(EnvironmentOptions);
        EnvironmentOptions.Save(false);
      end;
    end;
  finally
    EnvironmentOptionsDialog.Free;
  end;
End;

procedure TMainIDE.mnuEnvEditorOptionsClicked(Sender : TObject);
var EditorOptionsForm: TEditorOptionsForm;
Begin
  EditorOptionsForm:=TEditorOptionsForm.Create(Application);
  try
    if EditorOptionsForm.ShowModal=mrOk then
      SourceNotebook.ReloadEditorOptions;
  finally
    EditorOptionsForm.Free;
  end;
End;

procedure TMainIDE.SaveEnvironment;
begin
  SaveDesktopSettings(EnvironmentOptions);
  EnvironmentOptions.Save(false);
end;
//------------------------------------------------------------------------------

Procedure TMainIDE.MessageViewDblClick(Sender : TObject);
Begin

end;

//==============================================================================

function TMainIDE.DoNewEditorUnit(NewUnitType:TNewUnitType):TModalResult;
var NewUnitInfo:TUnitInfo;
  TempForm : TCustomForm;
  CInterface : TComponentInterface;
  NewSrcEdit: TSourceEditor;
  NewUnitName: string;
  NewBuffer, ResourceCode: TCodeBuffer;
begin
writeln('TMainIDE.DoNewEditorUnit A');
  Result:=mrCancel;
  NewUnitName:=Project.NewUniqueUnitName(NewUnitType);
  NewBuffer:=CodeToolBoss.CreateFile(
                                   NewUnitName+UnitTypeDefaultExt[NewUnitType]);
  if NewBuffer=nil then exit;
  NewUnitInfo:=TUnitInfo.Create(NewBuffer);

  // create source code
  if NewUnitType in [nuForm] then
    NewUnitInfo.FormName:=Project.NewUniqueFormName(NewUnitType);
  NewUnitInfo.CreateStartCode(NewUnitType,NewUnitName);
  
  // add to project
  with NewUnitInfo do begin
    Loaded:=true;
    IsPartOfProject:=true;
  end;
  Project.AddUnit(NewUnitInfo,true);
  if NewUnitType in [nuForm, nuUnit] then begin
    NewUnitInfo.SyntaxHighlighter:=lshFreePascal;
  end;

  if NewUnitType in [nuForm] then begin
    // create a buffer for the new resource file and for the LFM file
    ResourceCode:=
      CodeToolBoss.CreateFile(ChangeFileExt(NewUnitName,ResourceFileExt));
    ResourceCode.Source:=
      '{ This is an automatically generated lazarus resource file }';
    CodeToolBoss.CreateFile(ChangeFileExt(NewUnitName,'.lfm'));
    
    // clear formeditor
    if not Assigned(FormEditor1) then
      FormEditor1 := TFormEditor.Create;
    FormEditor1.ClearSelected;
{$IFDEF IDE_MEM_CHECK}
CheckHeap('TMainIDE.DoNewEditorUnit G '+IntToStr(GetMem_Cnt));
{$ENDIF}

    // create jitform
    CInterface := TComponentInterface(
      FormEditor1.CreateComponent(nil,TForm,
        ObjectInspector1.Left+ObjectInspector1.Width+40,Top+Height+50,400,300));
{$IFDEF IDE_MEM_CHECK}
CheckHeap('TMainIDE.DoNewEditorUnit H '+IntToStr(GetMem_Cnt));
{$ENDIF}
    FormEditor1.SetFormNameAndClass(CInterface,
      NewUnitInfo.FormName,'T'+NewUnitInfo.FormName);
{$IFDEF IDE_MEM_CHECK}
CheckHeap('TMainIDE.DoNewEditorUnit I '+IntToStr(GetMem_Cnt));
{$ENDIF}
    TempForm:=TForm(CInterface.Control);
    NewUnitInfo.Form:=TempForm;
    SetDefaultsForForm(TempForm);

    NewUnitInfo.FormName:=TempForm.Name;
{$IFDEF IDE_MEM_CHECK}
CheckHeap('TMainIDE.DoNewEditorUnit J '+IntToStr(GetMem_Cnt));
{$ENDIF}
    Project.AddCreateFormToProjectFile(TempForm.ClassName,TempForm.Name);
{$IFDEF IDE_MEM_CHECK}
CheckHeap('TMainIDE.DoNewEditorUnit K '+IntToStr(GetMem_Cnt));
{$ENDIF}
  end;

  // create a new sourceeditor
  SourceNotebook.NewFile(NewUnitInfo.UnitName,NewUnitInfo.Source);
{$IFDEF IDE_MEM_CHECK}
CheckHeap('TMainIDE.DoNewEditorUnit L '+IntToStr(GetMem_Cnt));
{$ENDIF}
  NewSrcEdit:=SourceNotebook.GetActiveSE;
  NewSrcEdit.SyntaxHighlighterType:=NewUnitInfo.SyntaxHighlighter;
  Project.InsertEditorIndex(SourceNotebook.NoteBook.PageIndex);
  NewUnitInfo.EditorIndex:=SourceNotebook.NoteBook.PageIndex;

  if NewUnitType in [nuForm] then begin
    // show form
    TDesigner(TempForm.Designer).SourceEditor := SourceNoteBook.GetActiveSE;

    TempForm.Show;
    SetDesigning(TempForm,True);

    // select the new form (object inspector, formeditor, control selection)
    PropertyEditorHook1.LookupRoot := TForm(CInterface.Control);
    TDesigner(TempForm.Designer).SelectOnlyThisComponent(TempForm);
  end;

  FCodeLastActivated:=not (NewUnitType in [nuForm]);
writeln('TMainIDE.DoNewUnit end');
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
end;

function TMainIDE.DoSaveEditorUnit(PageIndex:integer; 
  SaveAs, SaveToTestDir:boolean):TModalResult;
var ActiveSrcEdit:TSourceEditor;
  ActiveUnitInfo:TUnitInfo;
  SaveDialog:TSaveDialog;
  NewUnitName,NewFilename,NewPageName:string;
  AText,ACaption,CompResourceCode,TestFilename: string;
  MemStream,BinCompStream,TxtCompStream:TMemoryStream;
  Driver: TAbstractObjectWriter;
  Writer:TWriter;
  ResourceCode, LFMCode, NewSource: TCodeBuffer;
  LinkIndex: integer;
begin
writeln('TMainIDE.DoSaveEditorUnit A PageIndex=',PageIndex,' SaveAs=',SaveAs,' SaveToTestDir=',SaveToTestDir);
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  Result:=mrCancel;
  if ToolStatus<>itNone then begin
    Result:=mrAbort;
    exit;
  end;
  GetUnitWithPageIndex(PageIndex,ActiveSrcEdit,ActiveUnitInfo);
  if ActiveUnitInfo=nil then exit;
  
  if (not SaveToTestDir) and (Project.MainUnit>=0)
  and (Project.Units[Project.MainUnit]=ActiveUnitInfo)
  and (ActiveUnitInfo.IsVirtual) then begin
    Result:=DoSaveProject(false,SaveToTestDir);
    exit;
  end;

  ActiveUnitInfo.ReadOnly:=ActiveSrcEdit.ReadOnly;
  if (ActiveUnitInfo.ReadOnly) and (not SaveToTestDir) then begin
    Result:=mrOk;
    exit;
  end;
  if ActiveSrcEdit.Modified then begin
    ActiveSrcEdit.UpdateCodeBuffer;
    ActiveUnitInfo.Modified:=true;
  end;
  if (not SaveToTestDir) and (not ActiveUnitInfo.Modified) and (not SaveAs) then
  begin
    Result:=mrOk;
    exit;
  end;
  if (not SaveToTestDir) and (ActiveUnitInfo.IsVirtual) then
    SaveAs:=true;
  
  if ActiveUnitInfo.HasResources then begin
    LinkIndex:=-1;
{$IFDEF IDE_DEBUG}
writeln('TMainIDE.DoSaveEditorUnit B');
CodeToolBoss.SourceCache.WriteAllFileNames;
{$ENDIF}
    ResourceCode:=CodeToolBoss.FindNextResourceFile(
      ActiveUnitInfo.Source,LinkIndex);
{$IFDEF IDE_DEBUG}
writeln('TMainIDE.DoSaveEditorUnit B2 ',ResourceCode<>nil);
{$ENDIF}
    if ResourceCode<>nil then
      LFMCode:=
        CodeToolBoss.LoadFile(ChangeFileExt(ResourceCode.Filename,'.lfm'))
    else
      LFMCode:=nil;
  end else
    ResourceCode:=nil;
    
  if SaveAs and (not SaveToTestDir) then begin
    // let user choose a filename
    SaveDialog:=TSaveDialog.Create(Application);
    try
      SaveDialog.Title:='Save '+ActiveUnitInfo.UnitName+' (*.pas)';
      SaveDialog.FileName:=lowercase(ActiveUnitInfo.UnitName)+'.pas';
      SaveDialog.InitialDir:=EnvironmentOptions.LastOpenDialogDir;
      if SaveDialog.Execute then begin
        NewFilename:=ExpandFilename(SaveDialog.Filename);
        EnvironmentOptions.LastOpenDialogDir:=ExtractFilePath(NewFilename);
        if ExtractFileExt(NewFilename)='' then
          NewFilename:=NewFilename+'.pas';
        if FileExists(NewFilename) then begin
          ACaption:='Overwrite file?';
          AText:='A file "'+NewFilename+'" already exists.'#13'Replace it?';
          if MessageDlg(ACaption, AText, mtconfirmation,[mbok,mbCancel],0)
             =mrCancel then exit;
        end;
        EnvironmentOptions.AddToRecentOpenFiles(NewFilename);
        if not CodeToolBoss.SaveBufferAs(ActiveUnitInfo.Source,NewFilename,
               NewSource) then exit;
        if ResourceCode<>nil then begin
          // rename Resource file and form text file
          // the resource include line in the code will be changed later
          CodeToolBoss.SaveBufferAs(ResourceCode,
            ChangeFileExt(NewFilename,ResourceFileExt),ResourceCode);
          LinkIndex:=-1;
          ResourceCode:=CodeToolBoss.FindNextResourceFile(NewSource,LinkIndex);
{$IFDEF IDE_DEBUG}
writeln('TMainIDE.DoSaveEditorUnit D ',ResourceCode<>nil);
if ResourceCode<>nil then writeln('*** ResourceFileName ',ResourceCode.Filename);
{$ENDIF}
          if LFMCode<>nil then begin
            if not CodeToolBoss.SaveBufferAs(LFMCode,
              ChangeFileExt(NewFilename,'.lfm'),LFMCode) then
                LFMCode:=nil;
          end;
        end;
{$IFDEF IDE_DEBUG}
writeln('TMainIDE.DoSaveEditorUnit C ',ResourceCode<>nil);
{$ENDIF}
        ActiveUnitInfo.Source:=NewSource;
        ActiveUnitInfo.Modified:=false;
        ActiveSrcEdit.CodeBuffer:=NewSource; // the code is not changed, thus the marks are kept
        NewUnitName:=ExtractFileNameOnly(ActiveUnitInfo.Filename);
        // change unitname in source (resource filename is also changed)
        ActiveUnitInfo.UnitName:=NewUnitName;
        // change unitname on SourceNotebook
        NewPageName:=SourceNoteBook.FindUniquePageName(
            ActiveUnitInfo.Filename,SourceNoteBook.NoteBook.PageIndex);
        SourceNoteBook.NoteBook.Pages[SourceNoteBook.NoteBook.PageIndex]:=
            NewPageName;
        ActiveSrcEdit.ShortName:=NewPageName;
      end else begin
        // user cancels
        Result:=mrCancel;
        exit;
      end;
    finally
      SaveDialog.Free;
    end;
  end;
  TestFilename:='';
  if not SaveToTestDir then begin
    if ActiveUnitInfo.Modified and not SaveAs then begin
      // save source
      Result:=ActiveUnitInfo.WriteUnitSource;
      if Result=mrAbort then exit;
    end;
  end else begin
    // save source to test directory
    TestFilename:=GetTestUnitFilename(ActiveUnitInfo);
    if TestFilename<>'' then begin
      Result:=ActiveUnitInfo.WriteUnitSourceToFile(TestFilename);
      if Result<>mrOk then exit;
      Result:=mrCancel;
    end else
      exit;
  end;

{$IFDEF IDE_DEBUG}
writeln('*** HasResources=',ActiveUnitInfo.HasResources);
{$ENDIF}
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  if ResourceCode<>nil then begin
    // save lrs - lazarus resource file and lfm - lazarus form text file

    // stream component to binary stream
    BinCompStream:=TMemoryStream.Create;
    try
      repeat
        try
          BinCompStream.Position:=0;
          Driver:=TBinaryObjectWriter.Create(BinCompStream,4096);
          try
            Writer:=TWriter.Create(Driver);
            try
              Writer.WriteDescendent(ActiveUnitInfo.Form,nil);
            finally
              Writer.Free;
            end;
          finally
            Driver.Free;
          end;
        except
          ACaption:='Streaming error';
          AText:='Unable to stream '
              +ActiveUnitInfo.FormName+':T'+ActiveUnitInfo.FormName+'.';
          Result:=MessageDlg(ACaption, AText, mterror,
                     [mbabort, mbretry, mbignore], 0);
          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
        end;
      until Result<>mrRetry;
      // create lazarus form resource code
      MemStream:=TMemoryStream.Create;
      try
        BinCompStream.Position:=0;
        BinaryToLazarusResourceCode(BinCompStream,MemStream
          ,'T'+ActiveUnitInfo.FormName,'FORMDATA');
        MemStream.Position:=0;
        SetLength(CompResourceCode,MemStream.Size);
        MemStream.Read(CompResourceCode[1],length(CompResourceCode));
      finally
        MemStream.Free;
      end;
{$IFDEF IDE_DEBUG}
writeln('TMainIDE.DoSaveEditorUnit E ',CompResourceCode);
{$ENDIF}
      // replace lazarus form resource code
      if not CodeToolBoss.AddLazarusResource(ResourceCode,
         'T'+ActiveUnitInfo.FormName,CompResourceCode) then
      begin
        ACaption:='Resource error';
        AText:='Unable to add resource '
          +'T'+ActiveUnitInfo.FormName+':FORMDATA to resource file '#13
          +'"'+ResourceCode.FileName+'".'#13
          +'Probably a syntax error.';
        Result:=MessageDlg(ACaption, AText, mterror, [mbok, mbcancel], 0);
        if Result=mrCancel then Result:=mrAbort;
        exit;
      end;
{$IFDEF IDE_DEBUG}
writeln('TMainIDE.DoSaveEditorUnit F ',ResourceCode.Modified);
{$ENDIF}
      if not SaveToTestDir then begin
        if ResourceCode.Modified then begin
          Result:=DoSaveCodeBufferToFile(ResourceCode,ResourceCode.Filename,
              ActiveUnitInfo.IsPartOfProject);
          if not Result=mrOk then exit;
          Result:=mrCancel;
        end;
      end else begin
        // ToDo: calculate a better resource filename
writeln('>>>>>>>>>>>>> ',TestFilename,' ',ChangeFileExt(TestFilename,ResourceFileExt));
        Result:=DoSaveCodeBufferToFile(ResourceCode,
                    ChangeFileExt(TestFilename,ResourceFileExt),false);
        if not Result=mrOk then exit;
        Result:=mrCancel;
      end;
{$IFDEF IDE_DEBUG}
writeln('TMainIDE.DoSaveEditorUnit G ',LFMCode<>nil);
{$ENDIF}
      if (not SaveToTestDir) and (LFMCode<>nil) then begin
        repeat
          try
            // transform binary to text
            TxtCompStream:=TMemoryStream.Create;
            try
              BinCompStream.Position:=0;
              ObjectBinaryToText(BinCompStream,TxtCompStream);
              TxtCompStream.Position:=0;
              LFMCode.LoadFromStream(TxtCompStream);
              Result:=DoSaveCodeBufferToFile(LFMCode,LFMCode.Filename,
                               ActiveUnitInfo.IsPartOfProject);
              if not Result=mrOk then exit;
              Result:=mrCancel;
            finally
              TxtCompStream.Free;
            end;
          except
            ACaption:='Streaming error';
            AText:='Unable to transform binary component stream of '
               +ActiveUnitInfo.FormName+':T'+ActiveUnitInfo.FormName
               +' into text.';
            Result:=MessageDlg(ACaption, AText, mterror,
                                [mbabort, mbretry, mbignore], 0);
            if Result=mrAbort then exit;
            if Result=mrIgnore then Result:=mrOk;
          end;
        until Result<>mrRetry;
        Result:=mrCancel;
      end;
    finally
      BinCompStream.Free;
    end;
  end;
  if not SaveToTestDir then begin
    ActiveUnitInfo.Modified:=false;
    ActiveSrcEdit.Modified:=false;
  end;
  SourceNoteBook.UpdateStatusBar;
writeln('TMainIDE.DoSaveEditorUnit END');
  Result:=mrOk;
end;

function TMainIDE.DoCloseEditorUnit(PageIndex:integer; 
  SaveFirst: boolean):TModalResult;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  ACaption,AText: string;
  i:integer;
  OldDesigner: TDesigner;
begin
writeln('TMainIDE.DoCloseEditorUnit A PageIndex=',PageIndex);
  Result:=mrCancel;
  GetUnitWithPageIndex(PageIndex,ActiveSrcEdit,ActiveUnitInfo);
  if ActiveUnitInfo=nil then exit;
  ActiveUnitInfo.ReadOnly:=ActiveSrcEdit.ReadOnly;
  ActiveUnitInfo.TopLine:=ActiveSrcEdit.EditorComponent.TopLine;
  ActiveUnitInfo.CursorPos:=ActiveSrcEdit.EditorComponent.CaretXY;
  if SaveFirst and (not ActiveUnitInfo.ReadOnly) 
  and ((ActiveSrcEdit.Modified) or (ActiveUnitInfo.Modified)) then begin
    if ActiveUnitInfo.Filename<>'' then
      AText:='File "'+ActiveUnitInfo.Filename+'" has changed. Save?'
    else if ActiveUnitInfo.UnitName<>'' then
      AText:='Unit "'+ActiveUnitInfo.Unitname+'" has changed. Save?'
    else
      AText:='Source of page "'+
        SourceNotebook.NoteBook.Pages[SourceNotebook.NoteBook.PageIndex]
        +'" has changed. Save?';
    ACaption:='Source mofified';
    if Messagedlg(ACaption, AText, mtconfirmation, [mbyes, mbno], 0)=mryes then
    begin
      Result:=DoSaveEditorUnit(PageIndex,false,false);
      if Result=mrAbort then exit;
    end;
    Result:=mrOk;
  end;
  // close form
  if ActiveUnitInfo.Form<>nil then begin
    for i:=TWinControl(ActiveUnitInfo.Form).ComponentCount-1 downto 0 do
      TheControlSelection.Remove(
        TWinControl(ActiveUnitInfo.Form).Components[i]);
    TheControlSelection.Remove(TControl(ActiveUnitInfo.Form));
    OldDesigner:=TDesigner(TCustomForm(ActiveUnitInfo.Form).Designer);
    FormEditor1.DeleteControl(ActiveUnitInfo.Form);
    OldDesigner.Free;
    ActiveUnitInfo.Form:=nil;
  end;
  // close source editor
  SourceNoteBook.CloseFile(PageIndex);
  // close project file (not remove)
  Project.CloseEditorIndex(ActiveUnitInfo.EditorIndex);
  ActiveUnitInfo.Loaded:=false;
  i:=Project.IndexOf(ActiveUnitInfo);
  if (i<>Project.MainUnit) and (ActiveUnitInfo.Source.IsVirtual) then begin
    Project.RemoveUnit(i);
  end;
writeln('TMainIDE.DoCloseEditorUnit end');
  Result:=mrOk;
end;

function TMainIDE.DoOpenEditorFile(const AFileName:string; 
  ProjectLoading:boolean):TModalResult;
var Ext,ACaption,AText:string;
  i,BookmarkID:integer;
  ReOpen:boolean;
  NewUnitInfo:TUnitInfo;
  NewPageName, NewProgramName, LFMFilename: string;
  NewSrcEdit: TSourceEditor;
  TxtLFMStream, BinLFMStream:TMemoryStream;
  CInterface: TComponentInterface;
  TempForm: TCustomForm;
  PreReadBuf, NewBuf: TCodeBuffer;
begin
writeln('');
writeln('*** TMainIDE.DoOpenEditorFile START "',AFilename,'"');
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  Result:=mrCancel;
  if AFileName='' then exit;
  Ext:=lowercase(ExtractFileExt(AFilename));
  // check if the project knows this file
  i:=Project.UnitCount-1;
  while (i>=0) and (Project.Units[i].Filename<>AFileName) do dec(i);
  ReOpen:=(i>=0);
  if ReOpen then begin
    NewUnitInfo:=Project.Units[i];
    if (not ProjectLoading) and NewUnitInfo.Loaded then begin
      // file already open -> change to page
      SourceNoteBook.NoteBook.PageIndex:=NewUnitInfo.EditorIndex;
      Result:=mrOk;
      exit;
    end;
    Result:=DoLoadCodeBuffer(NewBuf,AFileName);
    if Result<>mrOk then exit;
    NewUnitInfo.Source:=NewBuf;
    if (Ext='.pp') or (Ext='.pas') then
      NewUnitInfo.ReadUnitNameFromSource;
  end else begin
    if (not ProjectLoading) and (ToolStatus=itNone)
    and ((Ext='.lpi') or (Ext='.lpr')) then begin
      // load program file and project info file
      Result:=DoOpenProjectFile(AFilename);
      exit;
    end;
    Result:=DoLoadCodeBuffer(PreReadBuf,AFileName);
    if Result<>mrOk then exit;
    Result:=mrCancel;
    // check if unit is a program
    if (not ProjectLoading) and (not ReOpen)
    and ((Ext='.pp') or (Ext='.pas') or (Ext='.dpr') or (Ext='.lpr'))
    and (CodeToolBoss.GetSourceType(PreReadBuf)='PROGRAM') then begin
      NewProgramName:=CodeToolBoss.GetSourceName(PreReadBuf);
      if NewProgramName<>'' then begin
        if FileExists(ChangeFileExt(AFilename,'.lpi')) then begin
          AText:='The file "'+AFilename+'"'#13
              +'seems to be the program file of an existing lazarus project.'#13
              +'Open project?'#13
              +'Cancel will load the source.';
          ACaption:='Project info file detected';
          if MessageDlg(ACaption, AText, mtconfirmation, 
               [mbok, mbcancel], 0)=mrOk then
          begin
            Result:=DoOpenProjectFile(ChangeFileExt(AFilename,'.lpi'));
            exit;
          end;
        end else begin
          AText:='The file "'+AFilename+'"'#13
              +'seems to be a program. Close current project'
              +' and create a new lazarus project for this program?'#13
              +'Cancel will load the source.';
          ACaption:='Program detected';
          if MessageDlg(ACaption, AText, mtconfirmation,
              [mbok, mbcancel], 0)=mrOk then 
          begin
            Result:=DoCreateProjectForProgram(PreReadBuf);
            exit;
          end;
        end;
      end;
    end;
    NewUnitInfo:=TUnitInfo.Create(PreReadBuf);
    if (Ext='.pp') or (Ext='.pas') then
      NewUnitInfo.ReadUnitNameFromSource;
    Project.AddUnit(NewUnitInfo,false);
  end;
{$IFDEF IDE_DEBUG}
writeln('[TMainIDE.DoOpenEditorFile] B');
{$ENDIF}
  // create a new source editor
  NewUnitInfo.SyntaxHighlighter:=ExtensionToLazSyntaxHighlighter(Ext);
writeln('[TMainIDE.DoOpenEditorFile] B2');
  NewPageName:=NewUnitInfo.UnitName;
  if NewPageName='' then begin
    NewPageName:=ExtractFileName(AFilename);
    if (Ext='.pas') or (Ext='.pp') then
      NewPageName:=copy(NewPageName,1,length(NewPageName)-length(Ext));
    if NewpageName='' then NewPageName:='file';
  end;
writeln('[TMainIDE.DoOpenEditorFile] B3');
  SourceNotebook.NewFile(NewPageName,NewUnitInfo.Source);
writeln('*** TMainIDE.DoOpenEditorFile C');
  NewSrcEdit:=SourceNotebook.GetActiveSE;
  if not ProjectLoading then
    Project.InsertEditorIndex(SourceNotebook.NoteBook.PageIndex)
  else begin
    for BookmarkID:=0 to 9 do begin
      i:=Project.Bookmarks.IndexOfID(BookmarkID);
      if (i>=0) and (Project.Bookmarks[i].EditorIndex=NewUnitInfo.EditorIndex)
      then begin
        NewSrcEdit.EditorComponent.SetBookmark(BookmarkID,
           Project.Bookmarks[i].CursorPos.X,Project.Bookmarks[i].CursorPos.Y);
        while i>=0 do begin
          Project.Bookmarks.Delete(i);
          i:=Project.Bookmarks.IndexOfID(BookmarkID);
        end;
      end;
    end;
  end;
  NewUnitInfo.EditorIndex:=SourceNotebook.NoteBook.PageIndex;
  NewSrcEdit.SyntaxHighlighterType:=NewUnitInfo.SyntaxHighlighter;
  NewSrcEdit.EditorComponent.CaretXY:=NewUnitInfo.CursorPos;
  NewSrcEdit.EditorComponent.TopLine:=NewUnitInfo.TopLine;
  NewSrcEdit.EditorComponent.LeftChar:=1;
  
{$IFDEF IDE_DEBUG}
writeln('[TMainIDE.DoOpenEditorFile] C');
{$ENDIF}
  NewUnitInfo.Loaded:=true;
  // read form data
  if (NewUnitInfo.Unitname<>'') then begin
    // this is a unit -> try to find the lfm file
    LFMFilename:=ChangeFileExt(NewUnitInfo.Filename,'.lfm');
    NewBuf:=nil;
    if FileExists(LFMFilename) then
      NewBuf:=CodeToolBoss.LoadFile(LFMFilename)
    else begin
      i:=-1;
      NewBuf:=CodeToolBoss.FindNextResourceFile(NewUnitInfo.Source,i);
      if NewBuf<>nil then begin
        LFMFilename:=ChangeFileExt(NewBuf.Filename,'.lfm');
        if FileExists(LFMFilename) then
          NewBuf:=CodeToolBoss.LoadFile(LFMFilename);
      end;
    end;
    
    if NewBuf<>nil then begin
      // there is a lazarus form text file -> load it
      BinLFMStream:=TMemoryStream.Create;
      try
        TxtLFMStream:=TMemoryStream.Create;
        try
          NewBuf.SaveToStream(TxtLFMStream);
          TxtLFMStream.Position:=0;
          // convert text to binary format
          try
            ObjectTextToBinary(TxtLFMStream,BinLFMStream);
            BinLFMStream.Position:=0;
            Result:=mrOk;
          except
            on E: Exception do begin
              ACaption:='Format error';
              AText:='Unable to convert text form data of file '#13
                +'"'+NewBuf.Filename+'"'#13
                +'into binary stream. ('+E.Message+')';
              Result:=MessageDlg(ACaption, AText, mterror, [mbok, mbcancel], 0);
              if Result=mrCancel then Result:=mrAbort;
              if Result<>mrOk then exit;
            end;
          end;
        finally
          TxtLFMStream.Free;
        end;
        if not Assigned(FormEditor1) then
          FormEditor1 := TFormEditor.Create;
        if not ProjectLoading then FormEditor1.ClearSelected;

        // create jitform
        CInterface := TComponentInterface(
          FormEditor1.CreateFormFromStream(BinLFMStream));
        if CInterface=nil then begin
          ACaption:='Form load error';
          AText:='Unable to build form from file '#13
                      +'"'+NewBuf.Filename+'".';
          Result:=MessageDlg(ACaption, AText, mterror, [mbok, mbcancel], 0);
          if Result=mrCancel then Result:=mrAbort;
          if Result<>mrOk then exit;
        end;
        TempForm:=TForm(CInterface.Control);
        NewUnitInfo.Form:=TempForm;
        SetDefaultsForForm(TempForm);
        NewUnitInfo.FormName:=TempForm.Name;
        // show form
        TDesigner(TempForm.Designer).SourceEditor := SourceNoteBook.GetActiveSE;

        if not ProjectLoading then begin
          TempForm.Show;
          FCodeLastActivated:=false;
        end;
        SetDesigning(TempForm,True);
        
        // select the new form (object inspector, formeditor, control selection)
        if not ProjectLoading then begin
          PropertyEditorHook1.LookupRoot := TForm(CInterface.Control);
          TDesigner(TempForm.Designer).SelectOnlyThisComponent(TempForm);
        end;
{$IFDEF IDE_DEBUG}
writeln('[TMainIDE.DoOpenEditorFile] LFM end');
{$ENDIF}
      finally
        BinLFMStream.Free;
      end;
    end;
  end;
  SourceNoteBook.UpdateStatusBar;
  Result:=mrOk;
writeln('TMainIDE.DoOpenEditorFile END "',AFilename,'" NewSrcEdit.Filename=',NewSrcEdit.Filename);
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
end;

function TMainIDE.DoOpenMainUnit(ProjectLoading: boolean): TModalResult;
var MainUnitInfo: TUnitInfo;
  NewPageName, Ext: string;
  NewSrcEdit: TSourceEditor;
begin
writeln('[TMainIDE.DoOpenMainUnit] A');
  Result:=mrCancel;
  if Project.MainUnit<0 then exit;
  MainUnitInfo:=Project.Units[Project.MainUnit];
//writeln('TMainIDE.DoOpenMainUnit B1 ',MainUnitInfo.Source.SourceLength);
  if MainUnitInfo.Loaded then begin
    // already loaded switch to source editor
    SourceNotebook.NoteBook.PageIndex:=MainUnitInfo.EditorIndex;
    Result:=mrOk;
    exit;
  end;
  // MainUnit not loaded -> create source editor
  if MainUnitInfo.Source.IsVirtual then
    NewPageName:=CodeToolBoss.GetSourceName(MainUnitInfo.Source)
  else begin
    NewPageName:=ExtractFileName(MainUnitInfo.Filename);
    Ext:=uppercase(ExtractFileExt(MainUnitInfo.Filename));
    if (Ext='.PAS') or (Ext='.PP') then
      NewPageName:=copy(NewPageName,1,length(NewPageName)-length(Ext));
  end;
//writeln('TMainIDE.DoOpenMainUnit B ',NewPageName,'  ',MainUnitInfo.Source.SourceLength);
  if NewPageName='' then
    NewPageName:='mainunit';
//writeln('TMainIDE.DoOpenMainUnit C ',NewPageName);
  SourceNotebook.NewFile(NewPageName,MainUnitInfo.Source);
  if not ProjectLoading then
    Project.InsertEditorIndex(SourceNotebook.NoteBook.PageIndex);
  MainUnitInfo.EditorIndex:=SourceNotebook.NoteBook.PageIndex;
  MainUnitInfo.Loaded:=true;
  NewSrcEdit:=SourceNotebook.GetActiveSE;
  NewSrcEdit.SyntaxHighlighterType:=MainUnitInfo.SyntaxHighlighter;
  NewSrcEdit.EditorComponent.CaretXY:=MainUnitInfo.CursorPos;
  NewSrcEdit.EditorComponent.TopLine:=MainUnitInfo.TopLine;
  Result:=mrOk;
writeln('[TMainIDE.DoOpenMainUnit] END');
end;

function TMainIDE.DoViewUnitsAndForms(OnlyForms: boolean): TModalResult;
var UnitList: TList;
  i: integer;
  MainUnitName, Ext, DlgCaption: string;
  MainUnitInfo, AnUnitInfo: TUnitInfo;
  MainUnitIndex: integer;
Begin
  UnitList:=TList.Create;
  try
    MainUnitIndex:=-1;
    for i:=0 to Project.UnitCount-1 do begin
      if Project.Units[i].IsPartOfProject then begin
        if OnlyForms then begin
          if Project.MainUnit=i then MainUnitIndex:=i;
          if Project.Units[i].FormName<>'' then
            UnitList.Add(TViewUnitsEntry.Create(
              Project.Units[i].FormName,i,false));
        end else begin
          if Project.Units[i].UnitName<>'' then begin
            if Project.MainUnit=i then MainUnitIndex:=i;
            UnitList.Add(TViewUnitsEntry.Create(
              Project.Units[i].UnitName,i,false));
          end else if Project.MainUnit=i then begin
            MainUnitInfo:=Project.Units[Project.MainUnit];
            if Project.ProjectType in [ptProgram,ptApplication,ptCustomProgram]
            then begin
              if (MainUnitInfo.Loaded) then
                MainUnitName:=SourceNoteBook.NoteBook.Pages[
                  MainUnitInfo.EditorIndex];
              if MainUnitName='' then begin
                MainUnitName:=CodeToolBoss.GetSourceName(MainUnitInfo.Source);
              end;
              if MainUnitName='' then begin
                MainUnitName:=ExtractFileName(MainUnitInfo.Filename);
                Ext:=ExtractFileExt(MainUnitName);
                MainUnitName:=copy(MainUnitName,1,length(MainUnitName)-length(Ext));
              end;
              if MainUnitName<>'' then begin
                MainUnitIndex:=UnitList.Count;
                UnitList.Add(TViewUnitsEntry.Create(
                  MainUnitName,i,false));
              end;
            end;
          end;
        end;
      end;
    end;
    if OnlyForms then
      DlgCaption:='View forms'
    else
      DlgCaption:='View units';
    if ShowViewUnitsDlg(UnitList,true,DlgCaption)=mrOk then begin
      for i:=0 to UnitList.Count-1 do begin
        if TViewUnitsEntry(UnitList[i]).Selected then begin
          AnUnitInfo:=Project.Units[TViewUnitsEntry(UnitList[i]).ID];
          if AnUnitInfo.Loaded then
            SourceNoteBook.NoteBook.PageIndex:=AnUnitInfo.EditorIndex
          else begin
            if MainUnitIndex=i then
              Result:=DoOpenMainUnit(false)
            else
              Result:=DoOpenEditorFile(AnUnitInfo.Filename,false);
            if Result=mrAbort then exit;
          end;
        end;
      end;
      FCodeLastActivated:=not OnlyForms;
      DoBringToFrontFormOrUnit;
    end;
  finally
    UnitList.Free;
  end;
  Result:=mrOk;
end;

function TMainIDE.DoOpenFileAtCursor(Sender: TObject):TModalResult;
begin
writeln('TMainIDE.DoOpenFileAtCursor');
  Result:=mrCancel;
  // ToDo
  // check if include, unit, or simply a filename (in a string or comment)
end;

function TMainIDE.DoNewProject(NewProjectType:TProjectType):TModalResult;
var i:integer;
Begin
writeln('TMainIDE.DoNewProject A');
  Result:=mrCancel;

  If Project<>nil then begin
    if SomethingOfProjectIsModified then begin
        if MessageDlg('Project changed', 'Save changes to project?', 
          mtconfirmation, [mbyes, mbno], 0)=mryes then begin
        if DoSaveProject(false,false)=mrAbort then begin
          Result:=mrAbort;
          exit;
        end;
      end;
    end;
    if DoCloseProject=mrAbort then begin
      Result:=mrAbort;
      exit;
    end;
  end;

  CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'ProjectDir']:=
    '(unknown Project Directory)';

  Project:=TProject.Create(NewProjectType);
  Project.OnFileBackup:=@DoBackupFile;
  Project.Title := 'project1';
  Project.CompilerOptions.CompilerPath:='$(CompPath)';
  SourceNotebook.SearchPaths:=Project.CompilerOptions.OtherUnitFiles;

  case NewProjectType of
   ptApplication:
    begin
      // create a first form unit
      Project.CompilerOptions.OtherUnitFiles:=
         '$(LazarusDir)'+OSDirSeparator+'lcl'+OSDirSeparator+'units'
        +';'+
         '$(LazarusDir)'+OSDirSeparator+'lcl'+OSDirSeparator+'units'
         +OSDirSeparator
         +CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'LCLWidgetType'];
      DoNewEditorUnit(nuForm);
    end;
   ptProgram,ptCustomProgram:
    begin
      // show program unit
      DoOpenMainUnit(false);
    end;
  end;
 
  // set all modified to false
  for i:=0 to Project.UnitCount-1 do
    Project.Units[i].Modified:=false;
  Project.Modified:=false;

writeln('TMainIDE.DoNewProject end ',CodeToolBoss.ConsistencyCheck);
  UpdateCaption;
  Result:=mrOk;
end;

function TMainIDE.DoSaveProject(SaveAs, SaveToTestDir:boolean):TModalResult;
var MainUnitSrcEdit, ASrcEdit: TSourceEditor;
  MainUnitInfo, AnUnitInfo: TUnitInfo;
  SaveDialog: TSaveDialog;
  NewFilename, NewProgramFilename, NewPageName, NewProgramName, AText, ACaption,
  Ext: string;
  i, BookmarkID, BookmarkX, BookmarkY :integer;
  NewBuf: TCodeBuffer;
begin
  Result:=mrCancel;
  if ToolStatus<>itNone then begin
    Result:=mrAbort;
    exit;
  end;
writeln('TMainIDE.DoSaveProject A SaveAs=',SaveAs,' SaveToTestDir=',SaveToTestDir);
  // check that all new units are saved first to get valid filenames
  for i:=0 to Project.UnitCount-1 do begin
    if (Project.Units[i].Loaded) and (Project.Units[i].IsVirtual)
    and (Project.MainUnit<>i) then begin
      Result:=DoSaveEditorUnit(Project.Units[i].EditorIndex,false,
                               SaveToTestDir);
      if (Result=mrAbort) or (Result=mrCancel) then exit;
    end;
  end;

  if SourceNotebook.Notebook=nil then
    Project.ActiveEditorIndexAtStart:=-1
  else
    Project.ActiveEditorIndexAtStart:=SourceNotebook.Notebook.PageIndex;
  MainUnitSrcEdit:=nil;
  if Project.MainUnit>=0 then begin
    MainUnitInfo:=Project.Units[Project.MainUnit];
    if MainUnitInfo.Loaded then begin
      MainUnitSrcEdit:=SourceNoteBook.FindSourceEditorWithPageIndex(
        MainUnitInfo.EditorIndex);
      if MainUnitSrcEdit.Modified then begin
        MainUnitSrcEdit.UpdateCodeBuffer;
        MainUnitInfo.Modified:=true;
      end;
    end;
  end else
    MainUnitInfo:=nil;

  // save some information of the loaded files
  Project.Bookmarks.Clear;
  for i:=0 to Project.UnitCount-1 do begin
    AnUnitInfo:=Project.Units[i];
    if AnUnitInfo.Loaded then begin
      ASrcEdit:=SourceNoteBook.FindSourceEditorWithPageIndex(
         AnUnitInfo.EditorIndex);
      AnUnitInfo.TopLine:=ASrcEdit.EditorComponent.TopLine;
      AnUnitInfo.CursorPos:=ASrcEdit.EditorComponent.CaretXY;
      for BookmarkID:=0 to 9 do begin
        if (ASrcEdit.EditorComponent.GetBookMark(
             BookmarkID,BookmarkX,BookmarkY))
        and (Project.Bookmarks.IndexOfID(BookmarkID)<0) then begin
          Project.Bookmarks.Add(TProjectBookmark.Create(BookmarkX,BookmarkY,
              AnUnitInfo.EditorIndex,BookmarkID));
        end;
      end;
    end;
  end;

  SaveAs:=SaveAs or (Project.ProjectFile='');
  if SaveAs and (not SaveToTestDir) then begin
    // let user choose a filename
    SaveDialog:=TSaveDialog.Create(Application);
    try
      SaveDialog.Title:='Save Project '+Project.Title+' (*.lpi)';
      if ExtractFileName(Project.ProjectFile)<>'' then
        SaveDialog.FileName:=ExtractFileName(Project.ProjectFile)
      else if Project.Title<>'' then
        SaveDialog.Filename:=ChangeFileExt(Project.Title,'.lpi')
      else if SaveDialog.Filename='' then
        SaveDialog.Filename:='project1.lpi';
      repeat
        SaveDialog.InitialDir:=EnvironmentOptions.LastOpenDialogDir;
        if SaveDialog.Execute then begin
          NewFilename:=ExpandFilename(SaveDialog.Filename);
          EnvironmentOptions.LastOpenDialogDir:=ExtractFilePath(NewFilename);
          if ExtractFileExt(NewFilename)='' then
            NewFilename:=NewFilename+'.lpi';
          NewProgramFilename:=ChangeFileExt(
            NewFilename,ProjectDefaultExt[Project.ProjectType]);
          if NewFilename=NewProgramFilename then begin
            ACaption:='Choose a different name';
            AText:='The project info file "'+NewFilename+'"'#13'is equal '
               +'to the project source file!';
            Result:=MessageDlg(ACaption, AText, mtconfirmation, [mbabort, mbretry], 0);
            if Result=mrAbort then exit;
          end else
            Result:=mrOk;
        end else begin
          // user cancels
          Result:=mrCancel;
          exit;
        end;
      until Result<>mrRetry;

      if FileExists(NewFilename) then begin
        ACaption:='Overwrite file?';
        AText:='A file "'+NewFilename+'" already exists.'#13'Replace it?';
        Result:=MessageDlg(ACaption, AText, mtconfirmation, [mbok, mbcancel], 0);
        if Result=mrCancel then exit;
      end else if Project.ProjectType in [ptProgram, ptApplication] then begin
        if FileExists(NewProgramFilename) then begin
          ACaption:='Overwrite file?';
          AText:='A file "'+NewProgramFilename+'" already exists.'#13
                          +'Replace it?';
          Result:=MessageDlg(ACaption, AText, mtconfirmation,[mbOk,mbCancel],0);
          if Result=mrCancel then exit;
        end;
      end;
      Project.ProjectFile:=NewFilename;
      EnvironmentOptions.AddToRecentProjectFiles(NewFilename);
      if (MainUnitInfo<>nil) and (MainUnitInfo.Loaded) then begin
        // sitch MainUnitInfo to new code
        NewBuf:=CodeToolBoss.CreateFile(NewProgramFilename);
        if NewBuf=nil then begin
          Result:=MessageDlg('Error creating file','Unable to create file'#13
               +'"'+NewProgramFilename+'"',mtError,[mbCancel],0);
          exit;
        end;
        NewBuf.Source:=MainUnitInfo.Source.Source;
        MainUnitInfo.Source:=NewBuf;
        MainUnitSrcEdit.CodeBuffer:=NewBuf;
        // change program name
        NewProgramName:=ExtractFileNameOnly(NewProgramFilename);
        CodeToolBoss.RenameSource(MainUnitInfo.Source,NewProgramName);
        // update source editor of main unit
        MainUnitInfo.Modified:=true;
        NewPageName:=ExtractFileName(MainUnitInfo.Filename);
        Ext:=ExtractFileExt(NewPagename);
        if (Ext='.pp') or (Ext='.pas') then
          NewPageName:=copy(NewpageName,1,length(NewPageName)-length(Ext));
        NewPageName:=SourceNoteBook.FindUniquePageName(
          NewPageName,MainUnitInfo.EditorIndex);
        SourceNoteBook.NoteBook.Pages[MainUnitInfo.EditorIndex]:=
          NewPageName;
      end;
    finally
      SaveDialog.Free;
    end;
  end;
  if not SaveToTestDir then begin
    Result:=Project.WriteProject;
    if Result=mrAbort then exit;
  end;
  // save main source
  if MainUnitInfo<>nil then begin
    if MainUnitInfo.Loaded then begin
      // shown in source editor
      Result:=DoSaveEditorUnit(MainUnitInfo.EditorIndex,false,SaveToTestDir);
      if Result=mrAbort then exit;
    end else begin
      // not shown in source editor, but code internally loaded
      if not SaveToTestDir then begin
        Result:=DoSaveCodeBufferToFile(MainUnitInfo.Source,
                                       MainUnitInfo.Filename,true);
      end else begin
        Result:=DoSaveCodeBufferToFile(MainUnitInfo.Source,
                                       GetTestUnitFilename(MainUnitInfo),false);
      end;
      if Result=mrAbort then exit;
    end;
  end;
  if not SaveToTestDir then begin
    EnvironmentOptions.LastSavedProjectFile:=Project.ProjectInfoFile;
    EnvironmentOptions.Save(false);
    if (Result=mrOk) then begin
      if MainUnitInfo<>nil then MainUnitInfo.Modified:=false;
      if MainUnitSrcEdit<>nil then MainUnitSrcEdit.Modified:=false;
    end;
    UpdateCaption;
  end;

  // save editor files
  if (SourceNoteBook.Notebook<>nil) and (not SaveToTestDir) then begin
    for i:=0 to SourceNoteBook.Notebook.Pages.Count-1 do begin
      if (Project.MainUnit<0)
      or (Project.Units[Project.MainUnit].EditorIndex<>i) then begin
        Result:=DoSaveEditorUnit(i,false,SaveToTestDir);
        if Result=mrAbort then exit;
      end;
    end;
  end;
writeln('TMainIDE.DoSaveProject End');
end;

function TMainIDE.DoCloseProject:TModalResult;
begin
writeln('TMainIDE.DoCloseProject A');
  // close all loaded files
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  while SourceNotebook.NoteBook<>nil do begin
    Result:=DoCloseEditorUnit(SourceNotebook.Notebook.Pages.Count-1,false);
    if Result=mrAbort then exit;
  end;
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  // close Project
  Project.Free;
  Project:=nil;
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  Result:=mrOk;
writeln('TMainIDE.DoCloseProject end ',CodeToolBoss.ConsistencyCheck);
end;

function TMainIDE.DoOpenProjectFile(AFileName:string):TModalResult;
var Ext,AText,ACaption,LPIFilename:string;
  LowestEditorIndex,LowestUnitIndex,LastEditorIndex,i:integer;
  NewBuf: TCodeBuffer;
begin
writeln('TMainIDE.DoOpenProjectFile A "'+AFileName+'"');
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  Result:=mrCancel;
  if AFileName='' then exit;
  AFilename:=ExpandFileName(AFilename);
  Ext:=lowercase(ExtractFileExt(AFilename));
  if (Ext='.lpr') and (FileExists(ChangeFileExt(AFileName,'.lpi'))) then begin
    // load instead of lazarus program file the project info file
    AFileName:=ChangeFileExt(AFileName,'.lpi');
    Ext:='.lpi';
  end;
  repeat
    if not FileExists(AFilename) then begin
      ACaption:='File not found';
      AText:='File "'+AFilename+'" not found.';
      Result:=MessageDlg(ACaption, AText, mterror, [mbabort, mbretry], 0);
      if Result=mrAbort then exit;
    end;
  until Result<>mrRetry;
  // close the old project
  if SomethingOfProjectIsModified then begin
    if MessageDlg('Project changed', 'Save changes to project?',
      mtconfirmation,[mbok, mbcancel],0)=mrOK then 
    begin
      if DoSaveProject(false,false)=mrAbort then begin
        Result:=mrAbort;
        exit;
      end;
    end;
  end;
  Result:=DoCloseProject;
  if Result=mrAbort then exit;
  // create a new one
writeln('TMainIDE.DoOpenProjectFile B');
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  LPIFilename:=ChangeFileExt(AFilename,'.lpi');
  Project:=TProject.Create(ptProgram);
  Project.ReadProject(LPIFilename);
  if Project.MainUnit>=0 then begin
    // read MainUnit Source
    Result:=DoLoadCodeBuffer(NewBuf,Project.Units[Project.MainUnit].Filename);
    if Result in [mrAbort,mrIgnore] then exit;
    Project.Units[Project.MainUnit].Source:=NewBuf;
  end;
writeln('TMainIDE.DoOpenProjectFile C');
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  UpdateCaption;
  // restore files
  LastEditorIndex:=-1;
  repeat
    LowestUnitIndex:=-1;
    LowestEditorIndex:=-1;
    for i:=0 to Project.UnitCount-1 do begin
      if (Project.Units[i].Loaded) then begin
        if (Project.Units[i].EditorIndex>LastEditorIndex)
        and ((Project.Units[i].EditorIndex<LowestEditorIndex)
             or (LowestEditorIndex<0)) then
        begin
          LowestEditorIndex:=Project.Units[i].EditorIndex;
          LowestUnitIndex:=i;
        end;
      end;
    end;
    if LowestEditorIndex>=0 then begin
      // reopen file
      Result:=DoOpenEditorFile(Project.Units[LowestUnitIndex].Filename,true);
      if Result=mrAbort then exit;
      if Project.ActiveEditorIndexAtStart=LowestEditorIndex then
        Project.ActiveEditorIndexAtStart:=SourceNoteBook.NoteBook.PageIndex;
      LastEditorIndex:=LowestEditorIndex;
    end;
  until LowestEditorIndex<0;
writeln('TMainIDE.DoOpenProjectFile D');
  // set active editor source editor
  if (SourceNoteBook.NoteBook<>nil) and (Project.ActiveEditorIndexAtStart>=0)
  and (Project.ActiveEditorIndexAtStart<SourceNoteBook.NoteBook.Pages.Count)
  then
    SourceNoteBook.Notebook.PageIndex:=Project.ActiveEditorIndexAtStart;

  // set all modified to false
  for i:=0 to Project.UnitCount-1 do begin
    Project.Units[i].Modified:=false;
  end;
  Project.Modified:=false;
  EnvironmentOptions.LastSavedProjectFile:=Project.ProjectInfoFile;
  EnvironmentOptions.Save(false);
  Result:=mrOk;
writeln('TMainIDE.DoOpenProjectFile end  CodeToolBoss.ConsistencyCheck=',CodeToolBoss.ConsistencyCheck);
{$IFDEF IDE_MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
end;

function TMainIDE.DoCreateProjectForProgram(
  ProgramBuf: TCodeBuffer): TModalResult;
var NewProjectType:TProjectType;
  ProgramTitle, Ext: string;
  MainUnitInfo: TUnitInfo;
begin
//writeln('[TMainIDE.DoCreateProjectForProgram] A');
  Result:=mrCancel;

  if SomethingOfProjectIsModified then begin
    if MessageDlg('Project changed','Save changes to project?',
      mtconfirmation,[mbok, mbcancel],0)=mrOK then 
    begin
      if DoSaveProject(false,false)=mrAbort then begin
        Result:=mrAbort;
        exit;
      end;
    end;
  end;

  // let user choose the program type
  if ChooseNewProject(NewProjectType)=mrCancel then exit;

  // close old project
  If Project<>nil then begin
    if DoCloseProject=mrAbort then begin
      Result:=mrAbort;
      exit;
    end;
  end;

  // create a new project
  Project:=TProject.Create(NewProjectType);
  Project.OnFileBackup:=@DoBackupFile;
  ProgramTitle:=ExtractFileName(ProgramBuf.Filename);
  Ext:=ExtractFileExt(ProgramTitle);
  ProgramTitle:=copy(ProgramTitle,1,length(ProgramTitle)-length(Ext));
  Project.Title:=ProgramTitle;
  SourceNotebook.SearchPaths:=Project.CompilerOptions.OtherUnitFiles;
  MainUnitInfo:=Project.Units[Project.MainUnit];
  MainUnitInfo.Source:=ProgramBuf;
  Project.ProjectFile:=ProgramBuf.Filename;
  Project.CompilerOptions.CompilerPath:='$(CompPath)';
  if NewProjectType=ptApplication then begin
    Project.CompilerOptions.OtherUnitFiles:=
       '$(LazarusDir)'+OSDirSeparator+'lcl'+OSDirSeparator+'units'
      +';'+
       '$(LazarusDir)'+OSDirSeparator+'lcl'+OSDirSeparator+'units'
       +OSDirSeparator
       +CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'LCLWidgetType'];
  end;

  // show program unit
  Result:=DoOpenMainUnit(false);
  if Result=mrAbort then exit;
 
  UpdateCaption;

//writeln('[TMainIDE.DoCreateProjectForProgram] END');
  Result:=mrOk;
end;

function TMainIDE.DoAddActiveUnitToProject: TModalResult;
var
  ActiveSourceEditor:TSourceEditor; 
  ActiveUnitInfo:TUnitInfo;
  s, ShortUnitName: string;
begin
  Result:=mrCancel;
  GetCurrentUnit(ActiveSourceEditor,ActiveUnitInfo);
  if ActiveUnitInfo<>nil then begin
    if ActiveUnitInfo.IsPartOfProject=false then begin
      if not ActiveUnitInfo.IsVirtual then
        s:='"'+ActiveUnitInfo.Filename+'"'
      else
        s:='"'+SourceNotebook.Notebook.Pages[SourceNotebook.Notebook.PageIndex]
          +'"';
      if (Project.ProjectType in [ptProgram, ptApplication])
      and (ActiveUnitInfo.UnitName<>'')
      and (Project.IndexOfUnitWithName(ActiveUnitInfo.UnitName,true)>=0) then
      begin
        MessageDlg('Unable to add '+s+' to project, because there is already a '
           +'unit with the same name in the project.',mtInformation,[mbOk],0);
      end else begin
        if MessageDlg('Add '+s+' to project?',mtConfirmation,[mbOk,mbCancel],0)
          =mrOk then
        begin
          ActiveUnitInfo.IsPartOfProject:=true;
          if (ActiveUnitInfo.UnitName<>'')
          and (Project.ProjectType in [ptProgram, ptApplication]) then begin
            ShortUnitName:=CodeToolBoss.GetSourceName(ActiveUnitInfo.Source);
            if ShortUnitName='' then ShortUnitName:=ActiveUnitInfo.UnitName;
            if (ShortUnitName<>'') then
              CodeToolBoss.AddUnitToMainUsesSection(
                 Project.Units[Project.MainUnit].Source,ShortUnitName,'');
          end;
          Project.Modified:=true;
        end;
      end;
    end else begin
      if not ActiveUnitInfo.IsVirtual then
        s:='The file "'+ActiveUnitInfo.Filename+'"'
      else
        s:='The file "'
          +SourceNotebook.Notebook.Pages[SourceNotebook.Notebook.PageIndex]
          +'"';
      s:=s+' is already part of the project.';
      MessageDlg(s,mtInformation,[mbOk],0);
    end;
  end else begin
    Result:=mrOk;
  end;
end;

function TMainIDE.DoRemoveFromProjectDialog: TModalResult;
var UnitList: TList;
  i:integer;
  AName: string;
  AnUnitInfo: TUnitInfo;
Begin
  UnitList:=TList.Create;
  try
    for i:=0 to Project.UnitCount-1 do begin
      AnUnitInfo:=Project.Units[i];
      if (AnUnitInfo.IsPartOfProject) and (i<>Project.MainUnit) then begin
        AName:=AnUnitInfo.FileName;
        if (AnUnitInfo.IsVirtual) and (AnUnitInfo.Loaded) then begin
          AName:=SourceNotebook.NoteBook.Pages[AnUnitInfo.EditorIndex];
        end;
        if not AnUnitInfo.IsVirtual then
          UnitList.Add(TViewUnitsEntry.Create(AName,i,false));
      end;
    end;
    if ShowViewUnitsDlg(UnitList,true,'Remove from project')=mrOk then begin
      for i:=0 to UnitList.Count-1 do begin
        if TViewUnitsEntry(UnitList[i]).Selected then begin
          AnUnitInfo:=Project.Units[TViewUnitsEntry(UnitList[i]).ID];
          AnUnitInfo.IsPartOfProject:=false;
          if (Project.MainUnit>=0)
          and (Project.ProjectType in [ptProgram, ptApplication]) then begin
            if (AnUnitInfo.UnitName<>'') then
              CodeToolBoss.RemoveUnitFromAllUsesSections(
                Project.Units[Project.MainUnit].Source,AnUnitInfo.UnitName);
            if (AnUnitInfo.FormName<>'') then
              Project.RemoveCreateFormFromProjectFile(
                  'T'+AnUnitInfo.FormName,AnUnitInfo.FormName);
          end;
        end;
      end;
    end;
  finally
    UnitList.Free;
  end;
  Result:=mrOk;
end;

function TMainIDE.DoBuildProject: TModalResult;
var ActiveSrcEdit: TSourceEditor;
  DefaultFilename: string;
begin
  Result:=mrCancel;
  if ToolStatus<>itNone then begin
    Result:=mrAbort;
    exit;
  end;
  if Project=nil then Begin
    MessageDlg('Create a project first!',mterror,[mbok],0);
    Exit;
  end;
  try
    if not (Project.ProjectType in [ptProgram, ptApplication, ptCustomProgram])
    then exit;
    if Project.ProjectFile<>'' then
      Result:=DoSaveAll
    else
      Result:=DoSaveProjectToTestDirectory;
    if Result<>mrOk then exit;
    if Project.ProjectFile<>'' then
      DefaultFilename:=''
    else
      DefaultFilename:=GetTestUnitFilename(Project.Units[Project.MainUnit]);

    ActiveSrcEdit:=SourceNotebook.GetActiveSE;
    if ActiveSrcEdit<>nil then ActiveSrcEdit.ErrorLine:=-1;

    ToolStatus:=itBuilder;
    MessagesView.Clear;
    DoShowMessagesView;

    if (SourceNotebook.Top+SourceNotebook.Height) > MessagesView.Top then
      SourceNotebook.Height := Max(50,Min(SourceNotebook.Height,
         MessagesView.Top-SourceNotebook.Top));
    Compiler1.OnOutputString:=@MessagesView.Add;
    Result:=Compiler1.Compile(Project,DefaultFilename);
    if Result=mrOk then begin
      MessagesView.MessageView.Items.Add(
        'Project "'+Project.Title+'" successfully built. :)');
    end else begin
      DoJumpToCompilerMessage(-1,true);
    end;
  finally
    ToolStatus:=itNone;
  end;
end;

function TMainIDE.DoSaveProjectToTestDirectory: TModalResult;
begin
  Result:=mrCancel;
  if (EnvironmentOptions.TestBuildDirectory='')
  or (not DirectoryExists(EnvironmentOptions.TestBuildDirectory)) then begin
    Result:=DoSaveAll;
    exit;
  end;
  Result:=DoSaveProject(false,true);
end;

function TMainIDE.DoRunProject: TModalResult;
// quick hack to start programs
// ToDo:
//  -switch the IDE mode to running and free the process when program terminates
//  -implement a better messages form for vast amount of output
//  -target filename
//  -command line parameters
//  -connect program to debugger
var
  TheProcess : TProcess;
  ProgramFilename, AText : String;
  MainUnitInfo: TUnitInfo;
begin
  Result:=mrCancel;
writeln('[TMainIDE.DoRunProject] A');
  if not (ToolStatus in [itNone,itDebugger]) then begin
    Result:=mrAbort;
    exit;
  end;
  if not (Project.ProjectType in [ptProgram, ptApplication, ptCustomProgram])
  or (Project.MainUnit<0) then
    exit;

  MainUnitInfo:=Project.Units[Project.MainUnit];
  if MainUnitInfo.IsVirtual then
    ProgramFilename:=GetTestProjectFilename
  else
    ProgramFilename:=ChangeFileExt(MainUnitInfo.Filename,Project.TargetFileExt);

  if not FileExists(ProgramFilename) then begin
    AText:='No program file "'+ProgramFilename+'" found!';
    MessageDlg('File not found',AText,mtError,[mbCancel],0);
    exit;
  end;

  case EnvironmentOptions.DebuggerType of
    dtGnuDebugger:
      begin
        if TheDebugger=nil then begin
          Result:=DoInitDebugger;
          if Result<>mrOk then exit;
          Result:=mrCancel;
        end;
        ToolStatus:=itDebugger;
        TheDebugger.Run;
      end;
  else
      begin
        try
          TheProcess:=TProcess.Create(ProgramFilename,
             [poRunSuspended,poUsePipes,poNoConsole]);
          TheProcess.Execute;
        except
          on e: Exception do begin
            AText:='Error running program "'+ProgramFilename+'": '+e.Message;
            MessageDlg(AText,mterror,[mbok], 0);
          end;
        end;
      end;
  end;   
  Result:=mrOk;
writeln('[TMainIDE.DoRunProject] END');
end;

function TMainIDE.DoPauseProject: TModalResult;
begin
  Result:=mrCancel;
  if (ToolStatus<>itDebugger) or (TheDebugger=nil) then exit;
  TheDebugger.Pause;
  Result:=mrOk;
end;

function TMainIDE.DoStepIntoProject: TModalResult;
begin
  Result:=mrCancel;
  if ToolStatus=itNone then begin
    Result:=DoInitDebugger;
    if Result<>mrOk then exit;
    Result:=mrCancel;
    ToolStatus:=itDebugger;
  end;
  if (ToolStatus<>itDebugger) or (TheDebugger=nil) then
    exit
  else begin
    TheDebugger.StepInto;
    Result:=mrOk;
  end;
end;

function TMainIDE.DoStepOverProject: TModalResult;
begin
  Result:=mrCancel;
  if ToolStatus=itNone then begin
    Result:=DoInitDebugger;
    if Result<>mrOk then exit;
    Result:=mrCancel;
    ToolStatus:=itDebugger;
  end;
  if (ToolStatus<>itDebugger) or (TheDebugger=nil) then
    exit
  else begin
    TheDebugger.StepOver;
    Result:=mrOk;
  end;
end;

function TMainIDE.DoStopProject: TModalResult;
begin
  Result:=mrCancel;
  if (ToolStatus<>itDebugger) or (TheDebugger=nil) then exit;
  TheDebugger.Stop;
  Result:=mrOk;
end;

function TMainIDE.DoRunToCursor: TModalResult;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  UnitFilename: string;
begin
  Result:=mrCancel;
  if ToolStatus=itNone then begin
    Result:=DoInitDebugger;
    if Result<>mrOk then exit;
    Result:=mrCancel;
    ToolStatus:=itDebugger;
  end;
  if ToolStatus<>itDebugger then exit;
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) then begin
    MessageDlg('Run to failed','Please open a unit before run.',mtError,
      [mbCancel],0);
    exit;
  end;
  if not ActiveUnitInfo.Source.IsVirtual then
    UnitFilename:=ActiveUnitInfo.Filename
  else
    UnitFilename:=GetTestUnitFilename(ActiveUnitInfo);
  TheDebugger.RunTo(UnitFilename,ActiveSrcEdit.EditorComponent.CaretY);
end;

function TMainIDE.DoInitDebugger: TModalResult;
var ProgramFilename: string;
  MainUnitInfo: TUnitInfo;
begin
  Result:=mrCancel;
  if Project.MainUnit<0 then exit;
  
  case EnvironmentOptions.DebuggerType of
    dtGnuDebugger:
      begin
        MessageDlg('Sorry, not implemented yet',
           'The GNU debugger support is not yet implemented.'#13
           +'The IDE can already handle the abstract debugger'#13
           +'(see directory debugger), so that anyone can write a unit for their'#13
           +'favourite debugger.'#13
           +'Please set the debugger in the environment options to none to'#13
           +'just start the program without debugging.',mtInformation,[mbOk],0);
        exit;
      { ToDo: GnuDebugger
        if (TheDebugger<>nil) and (not (TheDebugger is TGnuDebugger)) then begin
          TheDebugger.Free;
          TheDebugger:=nil;
        end;
        TheDebugger:=TGnuDebugger.Create;}
      end;
  else
    begin
      TheDebugger.Free;
      TheDebugger:=nil;
      exit;
    end;
  end;
  MainUnitInfo:=Project.Units[Project.MainUnit];
  if MainUnitInfo.IsVirtual then
    ProgramFilename:=GetTestProjectFilename
  else
    ProgramFilename:=ChangeFileExt(MainUnitInfo.Filename,Project.TargetFileExt);
  TheDebugger.Filename:=ProgramFilename;
  TheDebugger.OnState:=@OnDebuggerChangeState;
  TheDebugger.OnCurrent:=@OnDebuggerCurrentLine;
    
  // property BreakPointGroups: TDBGBreakPointGroups read FBreakPointGroups; // list of all breakpoints
  // property Watches: TDBGWatches read FWatches;   // list of all watches localvars etc
  
  Result:=mrOk;
end;

procedure TMainIDE.OnDebuggerChangeState(Sender: TObject);
begin
  if (Sender<>TheDebugger) or (Sender=nil) then exit;
  RunSpeedButton.Enabled:=(TheDebugger.State in [dsStop,dsPause,dsError]);
  PauseSpeedButton.Enabled:=(TheDebugger.State in [dsRun]);
  itmProjectRun.Enabled:=RunSpeedButton.Enabled;
  itmProjectPause.Enabled:=PauseSpeedButton.Enabled;
  case TheDebugger.State of
  dsStop:
    begin
      // program stopped -> end debugging session
      TheDebugger.Free;
      TheDebugger:=nil;
      ToolStatus:=itNone;
    end;
  dsPause:
    begin
      // program paused
      ToolStatus:=itDebugger;
    end;
  dsRun:
    begin
      // program is running
      ToolStatus:=itDebugger;
    end;
  dsError:
    begin
      // ???
      ToolStatus:=itDebugger;
    end;
  end;
end;

procedure TMainIDE.OnDebuggerCurrentLine(Sender: TObject; 
  const AFilename: String;  const ALine: Integer);
// debugger paused program due to pause or error
// -> show the current execution line in editor
var ActiveSrcEdit: TSourceEditor;
begin
  if (Sender<>TheDebugger) or (Sender=nil) then exit;
  if DoOpenEditorFile(AFilename,false)<>mrOk then exit;
  ActiveSrcEdit:=SourceNoteBook.GetActiveSE;
  if ActiveSrcEdit=nil then exit;
  ActiveSrcEdit.EditorComponent.CaretXY:=Point(1,ALine);
  ActiveSrcEdit.EditorComponent.TopLine:=
    ALine-(ActiveSrcEdit.EditorComponent.LinesInWindow div 2);
  ActiveSrcEdit.ErrorLine:=ALine;
end;

function TMainIDE.SomethingOfProjectIsModified: boolean;
begin
  Result:=(Project<>nil) 
      and (Project.SomethingModified or SourceNotebook.SomethingModified);
end;

function TMainIDE.DoSaveAll: TModalResult;
begin
writeln('TMainIDE.DoSaveAll');
  Result:=DoSaveProject(false,false);
  // ToDo: save package, cvs settings, ...
end;

procedure TMainIDE.GetCurrentUnit(var ActiveSourceEditor:TSourceEditor;
      var ActiveUnitInfo:TUnitInfo);
begin
  if SourceNoteBook.NoteBook=nil then begin
    ActiveSourceEditor:=nil;
    ActiveUnitInfo:=nil;
  end else begin
    GetUnitWithPageIndex(SourceNotebook.NoteBook.PageIndex,ActiveSourceEditor,
       ActiveUnitInfo);
  end;
end;

procedure TMainIDE.GetUnitWithPageIndex(PageIndex:integer; 
  var ActiveSourceEditor:TSourceEditor; var ActiveUnitInfo:TUnitInfo);
begin
  if SourceNoteBook.NoteBook=nil then begin
    ActiveSourceEditor:=nil;
    ActiveUnitInfo:=nil;
  end else begin
    ActiveSourceEditor:=SourceNoteBook.FindSourceEditorWithPageIndex(PageIndex);
    if ActiveSourceEditor=nil then
      ActiveUnitInfo:=nil
    else 
      ActiveUnitInfo:=Project.UnitWithEditorIndex(PageIndex);
  end;
end;

function TMainIDE.DoSaveStreamToFile(AStream:TStream; 
  const Filename:string; IsPartOfProject:boolean):TModalResult;
// save to file with backup and user interaction
var AText,ACaption:string;
  NewBuf: TCodeBuffer;
begin
  Result:=DoBackupFile(Filename,IsPartOfProject);
  if Result<>mrOk then exit;
  repeat
    NewBuf:=CodeToolBoss.CreateFile(FileName);
    if (NewBuf<>nil) or (not NewBuf.SaveToFile(Filename)) then begin
      ACaption:='Write error';
      AText:='Unable to save file "'+Filename+'"';
      Result:=MessageDlg(ACaption,AText,mterror, [mbabort, mbretry, mbignore],0);
      if Result=mrIgnore then Result:=mrOk;
      if Result=mrAbort then exit;
    end;
  until Result<>mrRetry;
end;

function TMainIDE.DoLoadMemoryStreamFromFile(MemStream: TMemoryStream; 
  const AFilename:string): TModalResult;
var FileStream: TFileStream;
  ACaption,AText:string;
begin
  repeat
    try
      FileStream:=TFileStream.Create(AFilename,fmOpenRead);
      try
        FileStream.Position:=0;
        MemStream.CopyFrom(FileStream,FileStream.Size);
        MemStream.Position:=0;
      finally
        FileStream.Free;
      end;
      Result:=mrOk;
    except
      ACaption:='Read Error';
      AText:='Unable to read file "'+AFilename+'"!';
      Result:=MessageDlg(ACaption, AText,mterror, [mbabort, mbretry, mbignore], 0);
      if Result=mrAbort then exit;
    end;
  until Result<>mrRetry;
end;

function TMainIDE.DoSaveCodeBufferToFile(ABuffer: TCodeBuffer;
  const AFilename: string; IsPartOfProject:boolean): TModalResult;
var
  ACaption,AText:string;
begin
  Result:=DoBackupFile(AFilename,IsPartOfProject);
  if Result<>mrOk then exit;
  repeat
    if ABuffer.SaveToFile(AFilename) then begin
      Result:=mrOk;
    end else begin
      ACaption:='Write Error';
      AText:='Unable to write to file "'+AFilename+'"!';
      Result:=MessageDlg(ACaption,AText,mtError,[mbAbort, mbRetry, mbIgnore],0);
      if Result=mrAbort then exit;
      if Result=mrIgnore then Result:=mrOk;
    end;
  until Result<>mrRetry;
end;

function TMainIDE.DoLoadCodeBuffer(var ACodeBuffer: TCodeBuffer; 
  const AFilename: string): TModalResult;
var
  ACaption,AText:string;
begin
  repeat
    ACodeBuffer:=CodeToolBoss.LoadFile(AFilename);
    if ACodeBuffer<>nil then begin
      Result:=mrOk;
writeln('[TMainIDE.DoLoadCodeBuffer] ',ACodeBuffer.SourceLength,' ',ACodeBuffer.Filename);
    end else begin
      ACaption:='Read Error';
      AText:='Unable to read file "'+AFilename+'"!';
      Result:=MessageDlg(ACaption,AText,mterror,[mbabort, mbretry, mbignore],0);
      if Result=mrAbort then exit;
    end;
  until Result<>mrRetry;
end;

function TMainIDE.DoBackupFile(const Filename:string; 
  IsPartOfProject:boolean): TModalResult;
var BackupFilename, CounterFilename: string;
  AText,ACaption:string;
  BackupInfo: TBackupInfo;
  FilePath, FileNameOnly, FileExt, SubDir: string;
  i: integer;
begin
  Result:=mrOk;
  if not (FileExists(Filename)) then exit;
  if IsPartOfProject then
    BackupInfo:=EnvironmentOptions.BackupInfoProjectFiles
  else
    BackupInfo:=EnvironmentOptions.BackupInfoOtherFiles;
  if (BackupInfo.BackupType=bakNone)
  or ((BackupInfo.BackupType=bakSameName) and (BackupInfo.SubDirectory='')) then
    exit;
  FilePath:=ExtractFilePath(Filename);
  FileExt:=ExtractFileExt(Filename);
  FileNameOnly:=ExtractFilename(Filename);
  FileNameOnly:=copy(FilenameOnly,1,length(FilenameOnly)-length(FileExt));
  if BackupInfo.SubDirectory<>'' then begin
    SubDir:=FilePath+BackupInfo.SubDirectory;
    repeat
      if not DirectoryExists(SubDir) then begin
        if not CreateDir(SubDir) then begin
          Result:=MessageDlg('Unable to create backup directory "'+SubDir+'".'
                ,mtWarning,[mbAbort,mbRetry,mbIgnore],0);
          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
        end;
      end;
    until Result<>mrRetry;
  end;
  if BackupInfo.BackupType in
   [bakSymbolInFront,bakSymbolBehind,bakUserDefinedAddExt,bakSameName] then
  begin
    case BackupInfo.BackupType of
      bakSymbolInFront:
        BackupFilename:=FileNameOnly+'.~'+copy(FileExt,2,length(FileExt)-1);
      bakSymbolBehind:
        BackupFilename:=FileNameOnly+FileExt+'~';
      bakUserDefinedAddExt:
        BackupFilename:=FileNameOnly+FileExt+'.'+BackupInfo.AdditionalExtension;
      bakSameName:
        BackupFilename:=FileNameOnly+FileExt;
    end;
    if BackupInfo.SubDirectory<>'' then
      BackupFilename:=SubDir+OSDirSeparator+BackupFilename
    else
      BackupFilename:=FilePath+BackupFilename;
    // remove old backup file
    repeat
      if FileExists(BackupFilename) then begin
        if not DeleteFile(BackupFilename) then begin
          ACaption:='Delete file failed';
          AText:='Unable to remove old backup file "'+BackupFilename+'"!';

//          Result:=Application.MessageBox(PChar(AText),PChar(ACaption),MB_ABORTRETRYIGNORE);
          Result:=MessageDlg(ACaption,AText,mterror,[mbabort,mbretry,mbignore],0);

          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
        end;
      end;
    until Result<>mrRetry;
  end else begin
    // backup with counter
    if BackupInfo.SubDirectory<>'' then
      BackupFilename:=SubDir+OSDirSeparator+FileNameOnly+FileExt+';'
    else
      BackupFilename:=Filename+';';
    if BackupInfo.MaxCounter<=0 then begin
      // search first non existing backup filename
      i:=1;
      while FileExists(BackupFilename+IntToStr(i)) do inc(i);
      BackupFilename:=BackupFilename+IntToStr(i);
    end else begin
      // rename all backup files (increase number)
      i:=1;
      while FileExists(BackupFilename+IntToStr(i))
      and (i<=BackupInfo.MaxCounter) do inc(i);
      if i>BackupInfo.MaxCounter then begin
        dec(i);
        CounterFilename:=BackupFilename+IntToStr(BackupInfo.MaxCounter);
        // remove old backup file
        repeat
          if FileExists(CounterFilename) then begin
            if not DeleteFile(CounterFilename) then begin
              ACaption:='Delete file failed';
              AText:='Unable to remove old backup file "'+CounterFilename+'"!';
              Result:=MessageDlg(ACaption,AText,mterror,[mbabort,mbretry,mbignore],0);
              if Result=mrAbort then exit;
              if Result=mrIgnore then Result:=mrOk;
            end;
          end;
        until Result<>mrRetry;
      end;
      // rename all old backup files
      dec(i);
      while i>=1 do begin
        repeat
          if not RenameFile(BackupFilename+IntToStr(i),
             BackupFilename+IntToStr(i+1)) then
          begin
            ACaption:='Rename file failed';
            AText:='Unable to rename file "'+BackupFilename+IntToStr(i)
                  +'" to "'+BackupFilename+IntToStr(i+1)+'"!';
            Result:=MessageDlg(ACaption,AText,mterror,[mbabort,mbretry,mbignore],0);
            if Result=mrAbort then exit;
            if Result=mrIgnore then Result:=mrOk;
          end;
        until Result<>mrRetry;
        dec(i);
      end;
      BackupFilename:=BackupFilename+'1';
    end;
  end;
  // backup file
  repeat
    if not RenameFile(Filename,BackupFilename) then begin
      ACaption:='Rename file failed';
      AText:='Unable to rename file "'+Filename+'" to "'+BackupFilename+'"!';
      Result:=MessageDlg(ACaption,AText,mterror,[mbabort,mbretry,mbignore],0);
      if Result=mrAbort then exit;
      if Result=mrIgnore then Result:=mrOk;
    end;
  until Result<>mrRetry;
end;

procedure TMainIDE.UpdateCaption;
var NewCaption:string;
begin
  NewCaption := 'Lazarus Editor v'+Version_String;
  if Project<>nil then begin
    if Project.Title<>'' then
      NewCaption:=NewCaption +' - '+Project.Title
    else if Project.ProjectFile<>'' then
      NewCaption:=NewCaption+' - '+ExtractFileName(Project.ProjectFile)
    else
      NewCaption:=NewCaption+' - (new project)'
  end;
  Caption:=NewCaption;
end;

procedure TMainIDE.DoBringToFrontFormOrUnit;
var AForm: TCustomForm;
  ActiveUnitInfo: TUnitInfo;
begin
  if FCodeLastActivated then begin
    if SourceNoteBook.NoteBook<>nil then AForm:=SourceNotebook
    else AForm:=nil;
  end else begin
    if (SourceNoteBook.NoteBook<>nil) then begin
      ActiveUnitInfo:=Project.UnitWithEditorIndex(
        SourceNoteBook.NoteBook.PageIndex);
      if (ActiveUnitInfo<>nil) then
        AForm:=TCustomForm(ActiveUnitInfo.Form);
    end;
  end;
  if AForm<>nil then begin
    AForm.Hide;
    AForm.Show;
  end;
end;

procedure TMainIDE.OnMacroSubstitution(TheMacro: TTransferMacro; var s:string;
  var Handled, Abort: boolean);
var MacroName:string;
begin
  if TheMacro=nil then exit;
  MacroName:=lowercase(TheMacro.Name);
  if MacroName='save' then begin
    Handled:=true;
    if SourceNoteBook.NoteBook<>nil then
      Abort:=(DoSaveEditorUnit(SourceNoteBook.NoteBook.PageIndex,false,false)
              <>mrOk);
    s:='';
  end else if MacroName='saveall' then begin
    Handled:=true;
    Abort:=(DoSaveAll<>mrOk);
    s:='';
  end else if MacroName='edfile' then begin
    Handled:=true;
    if SourceNoteBook.NoteBook<>nil then
      s:=Project.UnitWithEditorIndex(SourceNoteBook.NoteBook.PageIndex).Filename
    else
      s:='';
  end else if MacroName='col' then begin
    Handled:=true;
    if SourceNoteBook.NoteBook<>nil then
      s:=IntToStr(SourceNoteBook.GetActiveSE.EditorComponent.CaretX);
  end else if MacroName='row' then begin
    Handled:=true;
    if SourceNoteBook.NoteBook<>nil then
      s:=IntToStr(SourceNoteBook.GetActiveSE.EditorComponent.CaretY);
  end else if MacroName='projfile' then begin
    Handled:=true;
    s:=Project.ProjectFile;
  end else if MacroName='projpath' then begin
    Handled:=true;
    s:=ExtractFilePath(Project.ProjectFile);
  end else if MacroName='curtoken' then begin
    Handled:=true;
    if SourceNoteBook.NoteBook<>nil then
      s:=IntToStr(SourceNoteBook.GetActiveSE.EditorComponent.CaretY);
  end else if MacroName='lazarusdir' then begin
    Handled:=true;
    s:=EnvironmentOptions.LazarusDirectory;
    if s='' then s:=ExtractFilePath(ParamStr(0));
  end else if MacroName='fpcsrcdir' then begin
    Handled:=true;
    s:=EnvironmentOptions.FPCSourceDirectory;
  end else if MacroName='comppath' then begin
    Handled:=true;
    s:=EnvironmentOptions.CompilerFilename;
  end;
  // ToDo:
  //MacroList.Add(TIDEMacro.Create('CurToken','',nil));
  //MacroList.Add(TIDEMacro.Create('Params','',nil));
  //MacroList.Add(TIDEMacro.Create('TargetFile','',nil));

end;

procedure TMainIDE.OnCmdLineCreate(var CmdLine: string; var Abort:boolean);
// replace all transfer macros in command line
begin
  Abort:=not MacroList.SubstituteStr(CmdLine);
end;

function TMainIDE.DoJumpToCompilerMessage(Index:integer;
  FocusEditor: boolean): boolean;
  
  function SearchFile(const AFilename: string): string;
  var OldCurrDir, SearchPath, Delimiter, ProjectDir: string;
    PathStart, PathEnd: integer;
  begin
    if FilenameIsAbsolute(AFilename) then begin
      Result:=AFileName;
      exit;
    end;
    // search file in project directory
    ProjectDir:=ExtractFilePath(Project.ProjectFile);
    Result:=ProjectDir+AFilename;
    if FileExists(Result) then exit;
    // search file with unit search path
    OldCurrDir:=GetCurrentDir;
    try
      SetCurrentDir(ProjectDir);
      SearchPath:=Project.CompilerOptions.OtherUnitFiles;
      Delimiter:=';';
      PathStart:=1;
      while (PathStart<=length(SearchPath)) do begin
        while (PathStart<=length(SearchPath)) 
        and (Pos(SearchPath[PathStart],Delimiter)>0) do
          inc(PathStart);
        PathEnd:=PathStart;
        while (PathEnd<=length(SearchPath)) 
        and (Pos(SearchPath[PathEnd],Delimiter)<1) do
          inc(PathEnd);
        if PathEnd>PathStart then begin
          Result:=ExpandFileName(copy(SearchPath,PathStart,PathEnd-PathStart));
          if Result<>'' then begin
            if Result[length(Result)]<>OSDirSeparator then
              Result:=Result+OSDirSeparator;
            Result:=Result+AFileName;
            if FileExists(Result) then exit;
          end;
        end;
        PathStart:=PathEnd;
      end;
    finally
      SetCurrentDir(OldCurrDir);
    end;
    Result:='';
  end;
  
var MaxMessages: integer;
  Filename, Ext, SearchedFilename: string;
  CaretXY: TPoint;
  TopLine: integer;
  MsgType: TErrorType;
  SrcEdit: TSourceEditor;
begin
  Result:=false;
  MaxMessages:=MessagesView.MessageView.Items.Count;
  if Index>=MaxMessages then exit;
  if (Index<0) then begin
    // search relevant message (first error, first fatal)
    Index:=0;
    while (Index<MaxMessages) do begin
      if (Compiler1.GetSourcePosition(MessagesView.MessageView.Items[Index],
        Filename,CaretXY,MsgType)) then begin
        if MsgType in [etError,etFatal] then break;
      end;
      inc(Index);
    end;
    if Index>=MaxMessages then exit;
    MessagesView.MessageView.ItemIndex:=Index;
  end;
  if Compiler1.GetSourcePosition(MessagesView.MessageView.Items[Index],
        Filename,CaretXY,MsgType) then begin
    SearchedFilename:=SearchFile(Filename);
    if SearchedFilename<>'' then begin
      // open the file in the source editor
      Ext:=lowercase(ExtractFileExt(SearchedFilename));
      if (Ext<>'.lfm') or (Ext='.lpi') then begin
        Result:=(DoOpenEditorFile(SearchedFilename,false)=mrOk);
        if Result then begin
          // set caret position
          SrcEdit:=SourceNoteBook.GetActiveSE;
          TopLine:=CaretXY.Y-(SrcEdit.EditorComponent.LinesInWindow div 2);
          if TopLine<1 then TopLine:=1;
          SrcEdit.EditorComponent.CaretXY:=CaretXY;
          SrcEdit.EditorComponent.TopLine:=TopLine;
          SrcEdit.ErrorLine:=CaretXY.Y;
          if FocusEditor then begin
//writeln('[TMainIDE.DoJumpToCompilerMessage] A');
            SourceNotebook.BringToFront;
//writeln('[TMainIDE.DoJumpToCompilerMessage] B');
            SrcEdit.EditorComponent.SetFocus;
          end;
        end;
      end;
    end else begin
      MessageDlg('Unable to find file "'+Filename+'".'
         +' Check search path in'
         +' Project->Compiler Options...->Search Paths->Other Unit Files',
         mtInformation,[mbOk],0);
    end;
  end;
end;

procedure TMainIDE.DoShowMessagesView;
var WasVisible: boolean;
begin
  if (EnvironmentOptions.SaveWindowPositions) 
  and (EnvironmentOptions.MessagesViewBoundsValid) then begin
    MessagesView.BoundsRect:=EnvironmentOptions.MessagesViewBounds;
  end else begin
    MessagesView.Top := Screen.Height - 100 - 100;
    MessagesView.Height := 100;
    MessagesView.Left := SourceNotebook.Left;
    MessagesView.Width := SourceNotebook.Width;
  end;
  FMessagesViewBoundsRectValid:=true;
  WasVisible:=MessagesView.Visible;
  MessagesView.Show;
  if not WasVisible then begin
    // quick hack, till BringToFront works correctly
    SourceNotebook.Hide;
    SourceNotebook.Show;
  end;
end;

function TMainIDE.GetTestProjectFilename: string;
var TestDir: string;
begin
  Result:='';
  if (Project.MainUnit<0) then exit;
  Result:=lowercase(
               CodeToolBoss.GetSourceName(Project.Units[Project.MainUnit].Source));
  if (Result='') then exit;
  TestDir:=EnvironmentOptions.TestBuildDirectory;
  if (TestDir='') then exit;
  if TestDir[length(TestDir)]<>OSDirSeparator then
    TestDir:=TestDir+OSDirSeparator;
  Result:=TestDir+Result;
end;

function TMainIDE.GetTestUnitFilename(AnUnitInfo: TUnitInfo): string;
var TestDir: string;
begin
  Result:='';
  if AnUnitInfo=nil then exit;
  TestDir:=EnvironmentOptions.TestBuildDirectory;
  if (TestDir='') then exit;
  if TestDir[length(TestDir)]<>OSDirSeparator then
    TestDir:=TestDir+OSDirSeparator;
  Result:=ExtractFilename(AnUnitInfo.Filename);
  if Result='' then exit;
  Result:=TestDir+Result;
end;

//------------------------------------------------------------------------------

procedure TMainIDE.OnDesignerGetSelectedComponentClass(Sender: TObject; 
  var RegisteredComponent: TRegisteredComponent);
begin
  RegisteredComponent:=SelectedComponent;
end;

procedure TMainIDE.OnDesignerUnselectComponentClass(Sender: TObject);
begin
  ControlClick(ComponentNoteBook);
end;

procedure TMainIDE.OnDesignerSetDesigning(Sender: TObject; 
  Component: TComponent;  Value: boolean);
begin
  SetDesigning(Component,Value);
end;

procedure TMainIDE.OnDesignerComponentListChanged(Sender: TObject);
begin
  ObjectInspector1.FillComponentComboBox;
end;

procedure TMainIDE.OnDesignerPropertiesChanged(Sender: TObject);
begin
  ObjectInspector1.RefreshPropertyValues;
end;

procedure TMainIDE.OnDesignerAddComponent(Sender: TObject; 
  Component: TComponent; ComponentClass: TRegisteredComponent);
var i: integer;
  ActiveForm: TCustomForm;
  ActiveUnitInfo: TUnitInfo;
  FormClassName: string;
begin
  ActiveForm:=TDesigner(Sender).Form;
  if ActiveForm=nil then begin
    writeln('[TMainIDE.OnDesignerAddComponent] Error: TDesigner without a form');
    halt;
  end;
  // find source for form
  i:=Project.UnitCount-1;
  while (i>=0) do begin
    if (Project.Units[i].Loaded) 
    and (Project.Units[i].Form=ActiveForm) then break;
    dec(i);
  end;
  if i<0 then begin
    writeln('[TMainIDE.OnDesignerAddComponent] Error: form without source');
    halt;
  end;
  ActiveUnitInfo:=Project.Units[i];
  // add needed unit to source
  CodeToolBoss.AddUnitToMainUsesSection(ActiveUnitInfo.Source,
            ComponentClass.UnitName,'');
  // add component definition to form source
  FormClassName:=ActiveForm.ClassName;
  if CodeToolBoss.PublishedVariableExists(ActiveUnitInfo.Source,'*',
    FormClassName) then begin
    CodeToolBoss.AddPublishedVariable(ActiveUnitInfo.Source,FormClassName,
      Component.Name, Component.ClassName);
  end;
end;

procedure TMainIDE.OnDesignerRemoveComponent(Sender: TObject;
  Component: TComponent);
var i: integer;
  ActiveForm: TCustomForm;
  ActiveUnitInfo: TUnitInfo;
  FormClassName: string;
begin
  ActiveForm:=TDesigner(Sender).Form;
  if ActiveForm=nil then begin
    writeln('[TMainIDE.OnDesignerAddComponent] Error: TDesigner without a form');
    halt;
  end;
  // find source for form
  i:=Project.UnitCount-1;
  while (i>=0) do begin
    if (Project.Units[i].Loaded) 
    and (Project.Units[i].Form=ActiveForm) then break;
    dec(i);
  end;
  if i<0 then begin
    writeln('[TMainIDE.OnDesignerAddComponent] Error: form without source');
    halt;
  end;
  ActiveUnitInfo:=Project.Units[i];
  // remove component definition to form source
  FormClassName:=ActiveForm.ClassName;
  if CodeToolBoss.RemovePublishedVariable(ActiveUnitInfo.Source,FormClassName,
    Component.Name) then begin
    ActiveUnitInfo.Modified:=true;
  end;
end;

procedure TMainIDE.OnDesignerModified(Sender: TObject);
var i: integer;
begin
  i:=Project.IndexOfUnitWithForm(TDesigner(Sender).Form,false);
  if i>=0 then begin
    Project.Units[i].Modified:=true;
    if Project.Units[i].Loaded then
      SourceNotebook.FindSourceEditorWithPageIndex(
        Project.Units[i].EditorIndex).EditorComponent.Modified:=true;
  end;
end;

procedure TMainIDE.OnControlSelectionChanged(Sender: TObject);
var NewSelectedComponents : TComponentSelectionList;
  i: integer;
begin
{$IFDEF IDE_DEBUG}
writeln('[TMainIDE.OnControlSelectionChanged]');
{$ENDIF}
  if (TheControlSelection=nil) or (FormEditor1=nil) then exit;
  NewSelectedComponents:=TComponentSelectionList.Create;
  for i:=0 to TheControlSelection.Count-1 do begin
    NewSelectedComponents.Add(TheControlSelection[i].Component);
  end;
  FormEditor1.SelectedComponents:=NewSelectedComponents;
  NewSelectedComponents.Free;
{$IFDEF IDE_DEBUG}
writeln('[TMainIDE.OnControlSelectionChanged] END');
{$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TMainIDE.InitCodeToolBoss;
// initialize the CodeToolBoss, which is the frontend for the codetools.
// - sets a basic set of compiler macros
// ToDo: build a frontend for the codetools and save the settings
var CompilerUnitSearchPath: string;
  ADefTempl: TDefineTemplate;
  c: integer;
begin
  FOpenEditorsOnCodeToolChange:=false;
  
  if (not FileExists(EnvironmentOptions.CompilerFilename)) then begin
    writeln('');
    writeln('Warning *: Compiler Filename not set! (see Environment Options)');
  end;
  if (EnvironmentOptions.LazarusDirectory='') then begin
    writeln('');
    writeln(
      'Warning *: Lazarus Source Directory not set!  (see Environment Options)');
  end;
  if (EnvironmentOptions.FPCSourceDirectory='') then begin
    writeln('');
    writeln(
      'Warning: FPC Source Directory not set!  (see Environment Options)');
  end;
  
  // set global variables
  with CodeToolBoss.GlobalValues do begin
    Variables[ExternalMacroStart+'LazarusSrcDir']:=
      EnvironmentOptions.LazarusDirectory;
    Variables[ExternalMacroStart+'FPCSrcDir']:=
      EnvironmentOptions.FPCSourceDirectory;
    Variables[ExternalMacroStart+'LCLWidgetType']:='gtk';
    Variables[ExternalMacroStart+'ProjectDir']:='';
  end;
  
  // build DefinePool and Define Tree
  with CodeToolBoss.DefinePool do begin
    // start the compiler and ask for his settings
    ADefTempl:=CreateFPCTemplate(EnvironmentOptions.CompilerFilename,
                          CompilerUnitSearchPath);
    if ADefTempl=nil then begin
      writeln('');
      writeln(
        'Warning: Could not create Define Template for Free Pascal Compiler');
    end;
    Add(ADefTempl);
    CodeToolBoss.DefineTree.Add(ADefTempl.CreateCopy);
    // create compiler macros to simulate the Makefiles of the FPC sources
    ADefTempl:=CreateFPCSrcTemplate(EnvironmentOptions.FPCSourceDirectory,
                          CompilerUnitSearchPath);
    if ADefTempl=nil then begin
      writeln('');
      writeln(
        'Warning: Could not create Define Template for Free Pascal Sources');
    end;
    Add(ADefTempl);
    CodeToolBoss.DefineTree.Add(ADefTempl.CreateCopy);
    // create compilr macros for the lazarus sources 
    ADefTempl:=CreateLazarusSrcTemplate('$(#LazarusSrcDir)','$(#LCLWidgetType)');
    if ADefTempl=nil then begin
      writeln('');
      writeln(
        'Warning: Could not create Define Template for Lazarus Sources');
    end;
    Add(ADefTempl);
    CodeToolBoss.DefineTree.Add(ADefTempl.CreateCopy);
  end;  
  // build define tree
  with CodeToolBoss do begin
    DefineTree.Add(DefinePool.CreateLCLProjectTemplate(
                     '$(#LazarusSrcDir)','$(#LCLWidgetType)','$(#ProjectDir)'));
    //DefineTree.WriteDebugReport;
  end;
  c:=CodeToolBoss.ConsistencyCheck;
  if c<>0 then begin
    writeln('CodeToolBoss.ConsistencyCheck=',c);
    Halt;
  end;
  
  with CodeToolBoss do begin
    WriteExceptions:=true;
    CatchExceptions:=true;
    OnBeforeApplyChanges:=@OnBeforeCodeToolBossApplyChanges;
    OnAfterApplyChanges:=@OnAfterCodeToolBossApplyChanges;
  end;
end;

procedure TMainIDE.OnBeforeCodeToolBossApplyChanges(Manager: TCodeToolManager;
  var Abort: boolean);
// the CodeToolBoss built a list of Sources that will be modified
// 1. open all of them in the source notebook
// 2. lock the editors to reduce repaints and undo steps
var i: integer;
begin
  if FOpenEditorsOnCodeToolChange then begin
    // open all sources in editor
    for i:=0 to Manager.SourceChangeCache.BuffersToModifyCount-1 do begin
      if DoOpenEditorFile(Manager.SourceChangeCache.BuffersToModify[i].Filename,
        false)<>mrOk then
      begin
        Abort:=true;
        exit;
      end;
    end;
  end;
  // lock all editors
  SourceNoteBook.LockAllEditorsInSourceChangeCache;
end;

procedure TMainIDE.OnAfterCodeToolBossApplyChanges(Manager: TCodeToolManager);
begin
  SourceNoteBook.UnlockAllEditorsInSourceChangeCache;
end;

procedure TMainIDE.SaveSourceEditorChangesToCodeCache;
// save all open sources to code tools cache
var i: integer;
  CurUnitInfo: TUnitInfo;
  SrcEdit: TSourceEditor;
begin
  for i:=0 to Project.UnitCount-1 do begin
    CurUnitInfo:=Project.Units[i];
    if CurUnitInfo.EditorIndex>=0 then begin
      SrcEdit:=SourceNotebook.FindSourceEditorWithPageIndex(
        CurUnitInfo.EditorIndex);
      if SrcEdit.Modified then begin
        SrcEdit.UpdateCodeBuffer;
        CurUnitInfo.Modified:=true;
      end;
    end;
  end;
end;

procedure TMainIDE.ApplyCodeToolChanges;
begin
  // all changes were handled automatically by events
  // just clear the logs
  CodeToolBoss.SourceCache.ClearAllSourleLogEntries;
end;

procedure TMainIDE.DoJumpToProcedureSection;
var ActiveSrcEdit, NewSrcEdit: TSourceEditor;
  ActiveUnitInfo, NewUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  if SourceNoteBook.NoteBook=nil then exit;
  GetUnitWithPageIndex(SourceNoteBook.NoteBook.PageIndex,ActiveSrcEdit,
    ActiveUnitInfo);
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) then exit;
  SaveSourceEditorChangesToCodeCache;
  CodeToolBoss.VisibleEditorLines:=ActiveSrcEdit.EditorComponent.LinesInWindow;
{$IFDEF IDE_DEBUG}
writeln('');
writeln('[TMainIDE.DoJumpToProcedureSection] ************');
{$ENDIF}
  if CodeToolBoss.JumpToMethod(ActiveUnitInfo.Source,
    ActiveSrcEdit.EditorComponent.CaretX,
    ActiveSrcEdit.EditorComponent.CaretY,
    NewSource,NewX,NewY,NewTopLine) then
  begin
    if NewSource<>ActiveUnitInfo.Source then begin
      // jump to other file -> open it
      if DoOpenEditorFile(NewSource.Filename,false)<>mrOk then exit;
      GetUnitWithPageIndex(SourceNoteBook.NoteBook.PageIndex,NewSrcEdit,
        NewUnitInfo);
    end else begin
      NewSrcEdit:=ActiveSrcEdit;
    end;
//writeln('[TMainIDE.DoJumpToProcedureSection] ',NewX,',',NewY,',',NewTopLine);
    NewSrcEdit.EditorComponent.CaretXY:=Point(NewX,NewY);
    NewSrcEdit.EditorComponent.TopLine:=NewTopLine;
  end else begin
    // probably a syntax error or just not in a procedure head/body -> ignore
  end;
end;

procedure TMainIDE.DoCompleteCodeAtCursor;
var ActiveSrcEdit, NewSrcEdit: TSourceEditor;
  ActiveUnitInfo, NewUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  if SourceNoteBook.NoteBook=nil then exit;
  GetUnitWithPageIndex(SourceNoteBook.NoteBook.PageIndex,ActiveSrcEdit,
    ActiveUnitInfo);
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) then exit;
  FOpenEditorsOnCodeToolChange:=true;
  SaveSourceEditorChangesToCodeCache;
  CodeToolBoss.VisibleEditorLines:=ActiveSrcEdit.EditorComponent.LinesInWindow;
{$IFDEF IDE_DEBUG}
writeln('');
writeln('[TMainIDE.DoCompleteCodeAtCursor] ************');
{$ENDIF}
  if CodeToolBoss.CompleteCode(ActiveUnitInfo.Source,
    ActiveSrcEdit.EditorComponent.CaretX,
    ActiveSrcEdit.EditorComponent.CaretY,
    NewSource,NewX,NewY,NewTopLine) then
  begin
    ApplyCodeToolChanges;
    if NewSource<>ActiveUnitInfo.Source then begin
      // jump to other file -> open it
      if DoOpenEditorFile(NewSource.Filename,false)<>mrOk then exit;
      GetUnitWithPageIndex(SourceNoteBook.NoteBook.PageIndex,NewSrcEdit,
        NewUnitInfo);
    end else begin
      NewSrcEdit:=ActiveSrcEdit;
    end;
//writeln('[TMainIDE.DoJumpToProcedureSection] ',NewX,',',NewY,',',NewTopLine);
    NewSrcEdit.EditorComponent.CaretXY:=Point(NewX,NewY);
    NewSrcEdit.EditorComponent.TopLine:=NewTopLine;
  end else begin
    // error: probably a syntax error or just not in a procedure head/body
    // or not in a class
    // -> there are enough events to handle everything, so it is ignored here
    ApplyCodeToolChanges;
  end;
  FOpenEditorsOnCodeToolChange:=false;
end;

initialization
  { $I mainide.lrs}
  {$I images/laz_images.lrs}
  {$I images/mainicon.lrs}


end.


{ =============================================================================

  $Log$
  Revision 1.126  2001/10/23 09:13:51  lazarus
  MG: fixed TestProject

  Revision 1.125  2001/10/18 13:34:03  lazarus
  MG: keys for debugging

  Revision 1.123  2001/10/17 13:43:15  lazarus
  MG: added find previous to source editor

  Revision 1.122  2001/10/16 14:19:10  lazarus
  MG: added nvidia opengl support and a new opengl example from satan

  Revision 1.121  2001/10/15 17:41:30  lazarus
  MG: fixed splashform showing

  Revision 1.115  2001/10/09 09:46:49  lazarus
  MG: added codetools, fixed synedit unindent, fixed MCatureHandle

  Revision 1.113  2001/07/31 18:57:48  lazarus
  MG: fixed source ediotr statusbar filename

  Revision 1.111  2001/07/29 20:33:23  lazarus
  MG: bugfixed event propeditor, DoJumpToMethod with searchpath

  Revision 1.110  2001/07/10 10:44:15  lazarus
  MG: save unit only if modified

  Revision 1.105  2001/07/01 15:55:43  lazarus
  MG: JumpToCompilerMessage now centered in source editor

  Revision 1.104  2001/06/27 21:43:23  lazarus
  MG: added project bookmark support

  Revision 1.103  2001/06/26 00:08:35  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.102  2001/06/06 12:30:40  lazarus
  MG: bugfixes

  Revision 1.100  2001/06/05 10:27:50  lazarus
  MG: saving recent file lists

  Revision 1.98  2001/05/29 08:16:26  lazarus
  MG: bugfixes + starting programs

  Revision 1.97  2001/05/28 10:00:54  lazarus
  MG: removed unused code. fixed editor name bug.

  Revision 1.96  2001/05/27 11:52:00  lazarus
  MG: added --primary-config-path=<filename> cmd line option

  Revision 1.92  2001/04/21 14:50:21  lazarus
  MG: bugfix for mainunits ext <> .lpr

  Revision 1.91  2001/04/13 17:56:16  lazarus
  MWE:
  * Moved menubar outside clientarea
  * Played a bit with the IDE layout
  * Moved the creation of the toolbarspeedbuttons to a separate function

  Revision 1.90  2001/04/04 13:58:50  lazarus
  Added some changes to compreg.pp

  Revision 1.89  2001/04/04 13:55:34  lazarus
  MG: finished TComponentPropertyEditor, added OnModified to oi, cfe and designer

  Revision 1.88  2001/04/04 12:20:34  lazarus
  MG: added  add to/remove from project, small bugfixes

  Revision 1.86  2001/03/31 13:35:22  lazarus
  MG: added non-visual-component code to IDE and LCL

  Revision 1.85  2001/03/29 13:11:33  lazarus
  MG: fixed loading program file bug

  Revision 1.84  2001/03/29 12:38:59  lazarus
  MG: new environment opts, ptApplication bugfixes

  Revision 1.83  2001/03/28 14:08:45  lazarus
  MG: added backup code and fixed removing controls

  Revision 1.82  2001/03/27 11:11:13  lazarus
  MG: fixed mouse msg, added filedialog initialdir

  Revision 1.81  2001/03/26 14:52:30  lazarus
  MG: TSourceLog + compiling bugfixes

  Revision 1.75  2001/03/19 14:00:46  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.74  2001/03/12 18:57:31  lazarus
  MG: new designer and controlselection code

  Revision 1.68  2001/03/03 11:06:15  lazarus
  added project support, codetools

  Revision 1.62  2001/02/22 17:04:57  lazarus
  added environment options + killed ide unit circles

  Revision 1.61  2001/02/21 22:55:24  lazarus
  small bugfixes + added TOIOptions

  Revision 1.60  2001/02/20 16:53:24  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.59  2001/02/16 19:13:29  lazarus
  Added some functions
  Shane

  Revision 1.58  2001/02/08 06:09:25  lazarus
  Partially implemented Save Project As menu selection.               CAW

  Revision 1.57  2001/02/06 13:38:57  lazarus
  Fixes from Mattias for EditorOPtions
  Fixes to COmpiler that should allow people to compile if their path is set up.
  Changes to code completion.
  Shane

  Revision 1.56  2001/02/04 04:18:11  lazarus
  Code cleanup and JITFOrms bug fix.
  Shane

  Revision 1.55  2001/02/02 14:23:37  lazarus
  Start of code completion code.
  Shane

  Revision 1.54  2001/02/01 16:45:19  lazarus
  Started the code completion.
  Shane

  Revision 1.52  2001/01/31 13:03:33  lazarus
  Commitng source with new editor.
  Shane

  Revision 1.51  2001/01/31 06:25:35  lazarus
  Removed global unit.
  Removed and commented all references to TUnitInfo.

  Revision 1.50  2001/01/29 05:46:30  lazarus
  Moved Project Options and Compiler Options menus to the Project menu.
  Added Project property to TMainIDE class to allow the project to be
  accessed from other units.                                            CAW

  Revision 1.49  2001/01/18 13:27:30  lazarus
  Minor changees
  Shane

  Revision 1.48  2001/01/16 23:30:45  lazarus
  trying to determine what's crashing LAzarus on load.
  Shane

  Revision 1.45  2001/01/15 20:55:44  lazarus
  Changes for loading filesa
  Shane

  Revision 1.44  2001/01/15 18:25:51  lazarus
  Fixed a stupid error I caused by using a variable as an index in main.pp and this variable sometimes caused an exception because the index was out of range.
  Shane

  Revision 1.43  2001/01/14 03:56:57  lazarus
  Shane

  Revision 1.42  2001/01/13 06:11:06  lazarus
  Minor fixes
  Shane

  Revision 1.41  2001/01/13 03:09:37  lazarus
  Minor changes
  Shane

  Revision 1.40  2001/01/12 18:46:49  lazarus
  Named the speedbuttons in MAINIDE and took out some writelns.
  Shane

  Revision 1.39  2001/01/12 18:10:53  lazarus
  Changes for keyevents in the editor.
  Shane

  Revision 1.38  2001/01/09 21:06:06  lazarus
  Started taking KeyDown messages in TDesigner
  Shane

  Revision 1.37  2001/01/09 18:23:20  lazarus
  Worked on moving controls.  It's just not working with the X and Y coord's I'm getting.
  Shane

  Revision 1.36  2001/01/08 23:48:33  lazarus
  MWE:
    ~ Changed makefiles
    ~ Removed testform from lararus and changed it into program
    * some formatting

  Revision 1.35  2001/01/06 06:28:47  lazarus
  Made Designer control the control movement and such.  I am now using ISDesignMsg to move the controls.
  Shane

  Revision 1.32  2001/01/04 20:33:53  lazarus
  Moved lresources.
  Moved CreateLFM to Main.pp
  Changed Form1 and TFOrm1 to MainIDE and TMainIDE
  Shane

  Revision 1.30  2001/01/03 18:44:54  lazarus
  The Speedbutton now has a numglyphs setting.
  I started the TStringPropertyEditor

  Revision 1.29  2000/12/29 20:43:17  lazarus
  I added the run button with an Enable and disable icon

  Revision 1.25  2000/12/29 13:35:50  lazarus
  Mattias submitted new lresources.pp and lazres.pp files.
  Shane

  Revision 1.23  2000/12/21 20:28:33  lazarus
  Project - RUN will run the program IF the program is the active unit in the Editor.
  Shane

  Revision 1.22  2000/12/20 20:04:30  lazarus
  Made PRoject Build compile the active unit.  This way we can actually play with it by compiling units.

  Revision 1.19  2000/12/19 18:43:12  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.18  2000/12/15 18:25:16  lazarus
  Changes from Mattias and I.
  Shane

  Revision 1.16  2000/12/01 20:23:34  lazarus
  renamed Object_Inspector and Prop_edits by removing the underline.
  Shane

  Revision 1.5  2000/08/10 13:22:51  lazarus
  Additions for the FIND dialog
  Shane

  Revision 1.4  2000/08/09 18:32:10  lazarus
  Added more code for the find function.
  Shane

  Revision 1.2  2000/08/07 19:15:05  lazarus
  Added the Search menu to the IDE.
  Shane

  Revision 1.1  2000/07/13 10:27:47  michael
  + Initial import

}
