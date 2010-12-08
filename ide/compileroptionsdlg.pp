{ /***************************************************************************
                    compileroptionsdlg.pp  -  Lazarus IDE unit
                      ---------------------------------------
                   Compiler options form sets the switches for the project
                   file for the FPC compiler.


                   Initial Revision  : Sat May 10 23:15:32 CST 1999


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

unit CompilerOptionsDlg;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
  Forms, Classes, Math, LCLProc, SysUtils, InterfaceBase,
  ComCtrls, Buttons, StdCtrls, ExtCtrls, Graphics, FileUtil,
  Dialogs, Controls, GraphType, LCLType,
  LinkScanner,
  MacroIntf, ProjectIntf, IDEWindowIntf, IDEContextHelpEdit, MainIntf,
  TransferMacros, PathEditorDlg, LazarusIDEStrConsts, IDEOptionDefs, LazConf,
  IDEProcs, IDEImagesIntf, ShowCompilerOpts, Project, PackageDefs, IDEMsgIntf,
  CompilerOptions, CheckCompilerOpts, CompOptsModes, BuildModesEditor,
  Compiler_BuildMacro_Options, CheckLst;

type
  { TfrmCompilerOptions }

  TfrmCompilerOptions = class(TForm)
    CategoryTreeView: TTreeView;
    chkUseAsDefault: TCheckBox;
    MainNoteBook: TPageControl;

    { Search Paths Controls }
    PathPage: TTabSheet;
    
    lblOtherUnits: TLabel;
    edtOtherUnits: TEdit;
    OtherUnitsPathEditBtn: TPathEditorButton;

    lblIncludeFiles: TLabel;
    edtIncludeFiles: TEdit;
    IncludeFilesPathEditBtn: TPathEditorButton;

    lblOtherSources: TLabel;
    edtOtherSources: TEdit;
    OtherSourcesPathEditBtn: TPathEditorButton;

    lblLibraries: TLabel;
    edtLibraries: TEdit;
    LibrariesPathEditBtn: TPathEditorButton;

    lblUnitOutputDir: TLabel;
    edtUnitOutputDir: TEdit;
    btnUnitOutputDir: TButton;

    lblDebugPath: TLabel;
    edtDebugPath: TEdit;
    DebugPathEditBtn: TPathEditorButton;

    LCLWidgetTypeLabel: TLabel;
    LCLWidgetTypeComboBox: TComboBox;

    { Build modes }
    BuildModesPage: TTabSheet;

    { Parsing Controls }
    ParsingPage: TTabSheet;
    grpAsmStyle: TRadioGroup;
    grpSyntaxOptions: TCheckGroup;
    grpSyntaxMode: TGroupBox;
    cmbSyntaxMode: TComboBox;

    { Code Generation Controls }
    CodeGenPage: TTabSheet;

    grpSmartLinkUnit: TGroupBox;
    chkSmartLinkUnit: TCheckBox;

    grpChecks: TGroupBox;
    chkChecksIO: TCheckBox;
    chkChecksRange: TCheckBox;
    chkChecksOverflow: TCheckBox;
    chkChecksStack: TCheckBox;
    chkVerifyObjMethodCall: TCheckBox;

    grpHeapSize: TGroupBox;
    edtHeapSize: TEdit;

    grpTargetPlatform: TGroupBox;
    lblTargetOS : TLabel;
    CategorySplitter: TSplitter;
    TargetOSComboBox: TComboBox;
    lblTargetCPU : TLabel;
    TargetCPUComboBox: TComboBox;
    lblTargetProcessorProc : TLabel;
    TargetProcessorProcComboBox: TComboBox;

    grpOptimizations: TGroupBox;
    radOptLevelNone: TRadioButton;
    radOptLevel1: TRadioButton;
    radOptLevel2: TRadioButton;
    radOptLevel3: TRadioButton;
    chkOptVarsInReg: TCheckBox;
    lblOptMiddle: TLabel;
    chkOptUncertain: TCheckBox;
    chkOptSmaller: TCheckBox;

    { Linking Controls }
    LinkingPage: TTabSheet;
    grpDebugging: TGroupBox;
    chkDebugGDB: TCheckBox;
    chkUseLineInfoUnit: TCheckBox;
    chkGenerateDwarf: TCheckBox;
    chkUseHeaptrc: TCheckBox;
    chkUseValgrind: TCheckBox;
    chkGenGProfCode: TCheckBox;
    chkSymbolsStrip: TCheckBox;
    chkUseExternalDbgSyms: TCheckBox;

    grpLinkLibraries: TGroupBox;
    chkLinkSmart: TCheckBox;

    grpOptions: TGroupBox;
    chkOptionsLinkOpt: TCheckBox;
    edtOptionsLinkOpt: TEdit;
    
    TargetSpecificsGrpBox: TGroupBox;
    chkWin32GraphicApp: TCheckBox;

    { Messages Controls }
    MsgPage: TTabSheet;
    grpVerbosity: TCheckGroup;
    grpErrorCnt: TGroupBox;

    { Compiler Messages Controls }
    edtErrorCnt: TEdit;
    btnBrowseMsg: TButton;
    chklistCompMsg: TCheckListBox;
    chkUseMsgFile: TCheckBox;
    editMsgFileName: TEdit;
    grpCompilerMessages: TGroupBox;
    CfgCmpMsgPage: TTabSheet;


    { 'Other' Controls }
    OtherPage: TTabSheet;
    grpConfigFile: TGroupBox;
    chkConfigFile: TCheckBox;
    chkCustomConfigFile: TCheckBox;
    edtConfigPath: TEdit;
    grpCustomOptions: TGroupBox;
    memCustomOptions: TMemo;

    { Build Macros and Conditionals }
    MacrosPage: TTabSheet;
    BuildMacrosFrame: TCompOptBuildMacrosFrame;

    { Inherited Options }
    InheritedPage: TTabSheet;
    InhSplitter: TSplitter;
    InhNoteLabel: TLabel;
    InhTreeView: TTreeView;
    InhItemMemo: TMemo;

    { Compilation }
    CompilationPage: TTabSheet;

    chkCreateMakefile: TCheckBox;

    ExecuteBeforeGroupBox: TGroupBox;
    lblRunIfExecBefore: TLabel;
    chkExecBeforeCompile: TCheckBox;
    chkExecBeforeBuild: TCheckBox;
    chkExecBeforeRun: TCheckBox;
    ExecuteBeforeCommandLabel: TLabel;
    ExecuteBeforeCommandEdit: TEdit;
    ExecuteBeforeScanFPCCheckBox: TCheckBox;
    ExecuteBeforeScanMakeCheckBox: TCheckBox;
    ExecuteBeforeShowAllCheckBox: TCheckBox;

    grpCompiler: TGroupBox;
    lblRunIfCompiler: TLabel;
    chkCompilerCompile: TCheckBox;
    chkCompilerBuild: TCheckBox;
    chkCompilerRun: TCheckBox;
    lblCompiler: TLabel;
    edtCompiler: TEdit;

    ExecuteAfterGroupBox: TGroupBox;
    lblRunIfExecAfter: TLabel;
    chkExecAfterCompile: TCheckBox;
    chkExecAfterBuild: TCheckBox;
    chkExecAfterRun: TCheckBox;
    ExecuteAfterCommandLabel: TLabel;
    ExecuteAfterCommandEdit: TEdit;
    ExecuteAfterScanFPCCheckBox: TCheckBox;
    ExecuteAfterScanMakeCheckBox: TCheckBox;
    ExecuteAfterShowAllCheckBox: TCheckBox;

    { Buttons }
    BtnPanel: TPanel;
    btnShowOptions: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnCheck: TBitBtn;
    btnLoadSave: TBitBtn;
    HelpButton: TBitBtn;

    procedure ButtonOKClicked(Sender: TObject);
    procedure btnTestClicked(Sender: TObject);
    procedure ButtonLoadSaveClick(Sender: TObject);
    procedure ButtonShowOptionsClicked(Sender: TObject);
    procedure CategoryTreeViewSelectionChanged(Sender: TObject);
    procedure FileBrowseBtnClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure InhTreeViewSelectionChanged(Sender: TObject);
    procedure chkCustomConfigFileClick(Sender: TObject);
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    procedure frmCompilerOptionsClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure btnBrowseMsgClick(Sender: TObject);
    procedure chklistCompMsgClick(Sender: TObject);
    procedure chkUseMsgFileChange(Sender: TObject);
  private
    fPathsTVNode: TTreeNode;
    FBuildModesTVNode: TTreeNode;
    fBuildModeFrame: TBuildModesEditorFrame;
    procedure SetupSearchPathsTab(Page: integer);
    procedure SetupBuildModesTab(Page: integer);
    procedure SetupParsingTab(Page: integer);
    procedure SetupCodeGenerationTab(Page: integer);
    procedure SetupLinkingTab(Page: integer);
    procedure SetupVerbosityTab(Page: integer);
    procedure SetupConfigMsgTab(Page: integer);
    procedure SetupOtherTab(Page: integer);
    procedure SetupMacrosTab(Page: integer);
    procedure SetupInheritedTab(Page: integer);
    procedure SetupCompilationTab(Page: integer);
    procedure SetupButtonBar;
    function CheckSearchPath(const Context, ExpandedPath: string;
                             Level: TCheckCompileOptionsMsgLvl): boolean;
  private
    FReadOnly: boolean;
    ImageIndexPackage: integer;
    ImageIndexRequired: integer;
    ImageIndexInherited: integer;
    InheritedChildDatas: TList; // list of PInheritedNodeData
    TempMessages: TCompilerMessagesList;
    procedure SetReadOnly(const AValue: boolean);
    procedure UpdateInheritedTab;
    procedure ClearInheritedTree;
    procedure SetCompilerMessages;
    function GetUseAsDefault: Boolean;
  public
    CompilerOpts: TBaseCompilerOptions; // real options
    OldCompOpts: TBaseCompilerOptions; // set on loading, used for revert

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadOptionsToForm;
    procedure LoadOptionsToForm(SrcCompilerOptions: TBaseCompilerOptions);
    function SaveFormToOptions(CheckAndWarn: TCheckCompileOptionsMsgLvl): boolean;
    function SaveFormToOptions(CheckAndWarn: TCheckCompileOptionsMsgLvl;
                               DestCompilerOptions: TBaseCompilerOptions): boolean;
  public
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property UseAsDefault: boolean read GetUseAsDefault;
  end;

function SyntaxModeToCaption(const Mode: string): string;
function CaptionToSyntaxMode(const Caption: string): string;
function ProcessorToCaption(const Processor: string): string;
function CaptionToProcessor(const Caption: string): string;

function CheckCompileReasons(Reason: TCompileReason;
  Options: TProjectCompilerOptions; Quiet: boolean): TModalResult; // check if Reason is handled at least once

implementation

{$R *.lfm}

type
  TInheritedNodeData = record
    FullText: string;
    Option: TInheritedCompilerOption;
  end;
  PInheritedNodeData = ^TInheritedNodeData;

function SyntaxModeToCaption(const Mode: string): string;
begin
  if SysUtils.CompareText(Mode,'ObjFPC')=0 then
    Result:=lisObjectPascalDefault+' (-Mobjfpc)'
  else if SysUtils.CompareText(Mode,'Delphi')=0 then
    Result:=lisDelphi+' (-Mdelphi)'
  else if SysUtils.CompareText(Mode,'tp')=0 then
    Result:=lisTurboPascal+' (-Mtp)'
  else if SysUtils.CompareText(Mode,'fpc')=0 then
    Result:=lisFreePascal+' (-Mfpc)'
  else if SysUtils.CompareText(Mode,'macpas')=0 then
    Result:=lisMacPascal+' (-Mmacpas)'
  else
    Result:='';
end;

function CaptionToSyntaxMode(const Caption: string): string;
begin
  if System.Pos('-Mdelphi',Caption)>0 then
    Result:='Delphi'
  else if System.Pos('-Mtp',Caption)>0 then
    Result:='tp'
  else if System.Pos('-Mmacpas',Caption)>0 then
    Result:='macpas'
  else if System.Pos('-Mfpc',Caption)>0 then
    Result:='fpc'
  else
    Result:='ObjFPC';
end;

function ProcessorToCaption(const Processor: string): string;
begin
  if SysUtils.CompareText(Processor,'386')=0 then
    Result:='386/486'+' (-Op386)'
  else if SysUtils.CompareText(Processor,'pentium')=0 then
    Result:='Pentium/Pentium MMX (-OpPENTIUM)'
  else if SysUtils.CompareText(Processor,'pentium2')=0 then
    Result:='Pentium Pro/Pentium II/C6x86/K6 (-OpPENTIUM2)'
  else if SysUtils.CompareText(Processor,'pentium3')=0 then
    Result:='Pentium III (-OpPENTIUM3)'
  else if SysUtils.CompareText(Processor,'pentium4')=0 then
    Result:='Pentium IV (-OpPENTIUM4)'
  else if SysUtils.CompareText(Processor,'pentiumm')=0 then
    Result:='Pentium M (-OpPENTIUMM)'
  else
    Result:='('+rsiwpDefault+')';
end;

function CaptionToProcessor(const Caption: string): string;
begin
  if System.Pos('-Op386',Caption)>0 then
    Result:='386'
  else if System.Pos('-OpPENTIUMM',Caption)>0 then
    Result:='pentiumm'
  else if System.Pos('-OpPENTIUM4',Caption)>0 then
    Result:='pentium4'
  else if System.Pos('-OpPENTIUM3',Caption)>0 then
    Result:='pentium3'
  else if System.Pos('-OpPENTIUM2',Caption)>0 then
    Result:='pentium2'
  else if System.Pos('-OpPENTIUM',Caption)>0 then
    Result:='pentium'
  else
    Result:='';
end;

function CheckCompileReasons(Reason: TCompileReason;
  Options: TProjectCompilerOptions; Quiet: boolean): TModalResult;
var
  ProjToolOpts: TProjectCompilationToolOptions;
begin
  if (Reason in Options.CompileReasons)
  and (Options.CompilerPath<>'') then
    exit(mrOk);
  if Options.ExecuteBefore is TProjectCompilationToolOptions then begin
    ProjToolOpts:=TProjectCompilationToolOptions(Options.ExecuteBefore);
    if (Reason in ProjToolOpts.CompileReasons) and (ProjToolOpts.Command<>'') then
      exit(mrOk);
  end;
  if Options.ExecuteAfter is TProjectCompilationToolOptions then begin
    ProjToolOpts:=TProjectCompilationToolOptions(Options.ExecuteAfter);
    if (Reason in ProjToolOpts.CompileReasons) and (ProjToolOpts.Command<>'') then
      exit(mrOk);
  end;
  // reason is not handled
  if Quiet then exit(mrCancel);
  Result:=MessageDlg('Nothing to do',
    'The project''s compiler options has no compile command.'#13
    +'See Project / Compiler Options ... / Compilation',mtInformation,
    [mbCancel,mbIgnore],0);
  if Result=mrIgnore then
    Result:=mrOk;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions Constructor
------------------------------------------------------------------------------}
constructor TfrmCompilerOptions.Create(TheOwner: TComponent);
var 
  Page: integer;
begin
  DisableAlign;
  try
    inherited Create(TheOwner);
    Caption := dlgCompilerOptions;

    TempMessages := TCompilerMessagesList.Create;
    IDEDialogLayoutList.ApplyLayout(Self,Width,Height);

    ImageIndexPackage := IDEImages.LoadImage(16, 'item_package');
    ImageIndexRequired := IDEImages.LoadImage(16, 'pkg_required');
    ImageIndexInherited := IDEImages.LoadImage(16, 'pkg_inherited');

    MainNotebook.PageIndex:=0;
    MainNoteBook.ShowTabs:=false;
    Page:=0;

    SetupSearchPathsTab(Page);
    inc(Page);
    SetupBuildModesTab(Page);
    inc(Page);
    SetupParsingTab(Page);
    inc(Page);
    SetupCodeGenerationTab(Page);
    inc(Page);
    SetupLinkingTab(Page);
    inc(Page);
    SetupVerbosityTab(Page);
    inc(Page);
    SetupConfigMsgTab(Page);
    inc(Page);
    SetupOtherTab(Page);
    inc(Page);
    SetupMacrosTab(Page);
    inc(Page);
    SetupInheritedTab(Page);
    inc(Page);
    SetupCompilationTab(Page);
    inc(Page);
    SetupButtonBar;
    chkUseAsDefault.Caption := dlgCOUseAsDefault;
    chkUseAsDefault.ShowHint:=true;
    chkUseAsDefault.Hint:=lisWhenEnabledTheCurrentOptionsAreSavedToTheTemplateW;

    CategoryTreeView.Selected:=CategoryTreeView.Items.GetFirstNode;
    CategoryTreeViewSelectionChanged(nil);
  finally
    EnableAlign;
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions Destructor
------------------------------------------------------------------------------}
destructor TfrmCompilerOptions.Destroy;
begin
  FreeAndNil(OldCompOpts);
  ClearInheritedTree;
  TempMessages.Free;
  inherited Destroy;
end;

procedure TfrmCompilerOptions.LoadOptionsToForm;
begin
  LoadOptionsToForm(nil);
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions ButtonOKClicked
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonOKClicked(Sender: TObject);
begin
  // Accept any changes
  Assert(False, 'Trace:Accept compiler options changes');
  
  { Save the options and hide the dialog }
  if not SaveFormToOptions(ccomlErrors) then exit;
  ModalResult:=mrOk;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions ButtonCheckClicked
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.btnTestClicked(Sender: TObject);
begin
  // Apply any changes and test
  if not SaveFormToOptions(ccomlHints) then exit;
  if Assigned(TestCompilerOptions) then begin
    btnCheck.Enabled:=false;
    try
      TestCompilerOptions(CompilerOpts);
    finally
      btnCheck.Enabled:=true;
    end;
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions ButtonShowOptionsClicked
     This function is for testing the MakeOptionsString function only. Remove
     this function and its button when the function is working correctly.
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonShowOptionsClicked(Sender: TObject);
begin
  // Test MakeOptionsString function
  if SaveFormToOptions(ccomlWarning) then
    ShowCompilerOptionsDialog(Self, CompilerOpts);
end;

procedure TfrmCompilerOptions.CategoryTreeViewSelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node:=CategoryTreeView.Selected;
  if Node=nil then exit;
  if TObject(Node.Data) is TTabSheet then begin
    MainNoteBook.ActivePage:=TTabSheet(Node.Data);
  end;
end;

procedure TfrmCompilerOptions.FileBrowseBtnClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  DefaultFilename: String;
  NewFilename: String;
begin
  OpenDialog:=TSelectDirectoryDialog.Create(Self);
  try
    DefaultFilename:='';
    if Sender=btnUnitOutputDir then begin
      OpenDialog.Title:=lisUnitOutputDirectory;
      OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    end else
      exit;
    OpenDialog.Filename:=ExtractFilename(DefaultFilename);
    if DefaultFilename<>'' then
      OpenDialog.InitialDir:=ExtractFilePath(DefaultFilename)
    else
      OpenDialog.InitialDir:=CompilerOpts.BaseDirectory;
    if OpenDialog.Execute then begin
      NewFilename:=TrimFilename(OpenDialog.Filename);
      if CompilerOpts<>nil then
        NewFilename:=CompilerOpts.ShortenPath(NewFilename,false);
      if Sender=btnUnitOutputDir then begin
        edtUnitOutputDir.Text:=OpenDialog.Filename;
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TfrmCompilerOptions.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TfrmCompilerOptions.InhTreeViewSelectionChanged(Sender: TObject);
var
  ANode: TTreeNode;
  ChildData: PInheritedNodeData;
  sl: TStrings;
begin
  ANode:=InhTreeView.Selected;
  if (ANode=nil) or (ANode.Data=nil) then begin
    InhItemMemo.Lines.Text:=lisSelectANode;
  end else begin
    ChildData:=PInheritedNodeData(ANode.Data);
    if ChildData^.Option in icoAllSearchPaths then begin
      sl:=SplitString(ChildData^.FullText,';');
      InhItemMemo.Lines.Assign(sl);
      sl.Free;
    end else
      InhItemMemo.Lines.Text:=ChildData^.FullText;
  end;
end;

procedure TfrmCompilerOptions.ButtonLoadSaveClick(Sender: TObject);
var
  ImExpCompilerOptionsResult: TImportExportOptionsResult;
begin
  MainIDEInterface.DoImExportCompilerOptions(Self, ImExpCompilerOptionsResult);
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions GetCompilerOptions
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.LoadOptionsToForm(
  SrcCompilerOptions: TBaseCompilerOptions);
var
  i: integer;
  LCLPlatform: TLCLPlatform;
  EnabledLinkerOpts: Boolean;
  Options: TBaseCompilerOptions;
  HasBuildModes: boolean;
begin
  if SrcCompilerOptions<>nil then
    Options:=SrcCompilerOptions
  else
    Options:=CompilerOpts;

  FreeAndNil(OldCompOpts);
  OldCompOpts := TBaseCompilerOptionsClass(Options.ClassType).Create(nil);
  OldCompOpts.Assign(Options);

  DisableAlign;
  try
    EnabledLinkerOpts:=Options.NeedsLinkerOpts;
    chkUseAsDefault.Visible:=Options.CanBeDefaulForProject;

    { Get the compiler options and apply them to the dialog }

    // paths
    edtOtherUnits.Text := Options.OtherUnitFiles;
    edtIncludeFiles.Text := Options.IncludePath;
    edtLibraries.Text := Options.Libraries;
    lblLibraries.Enabled:=EnabledLinkerOpts;
    edtOtherSources.Text := Options.SrcPath;
    edtUnitOutputDir.Text := Options.UnitOutputDirectory;
    edtDebugPath.Text := Options.DebugPath;

    LCLPlatform := DirNameToLCLPlatform(Options.LCLWidgetType);
    if CompareText(Options.LCLWidgetType,LCLPlatformDirNames[LCLPlatform])=0 then
      LCLWidgetTypeComboBox.ItemIndex := ord(LCLPlatform)+1
    else
      LCLWidgetTypeComboBox.ItemIndex := 0;

    // build modes
    HasBuildModes:=(Options is TProjectCompilerOptions);
    if HasBuildModes then begin
      // show build modes
      if FBuildModesTVNode=nil then begin
        FBuildModesTVNode:=CategoryTreeView.Items.AddObject(fPathsTVNode,
                                         BuildModesPage.Caption,BuildModesPage);
      end;
    end else begin
      // hide build modes
      if FBuildModesTVNode<>nil then begin
        CategoryTreeView.Items.Delete(FBuildModesTVNode);
        FBuildModesTVNode:=nil;
      end;
    end;

    // parsing
    if (Options.AssemblerStyle in [1,2,3])  then
      grpAsmStyle.ItemIndex:=Options.AssemblerStyle
    else
      grpAsmStyle.ItemIndex:=0;

    with grpSyntaxOptions do
    begin
      Checked[0] := Options.CStyleOperators;
      Checked[1] := Options.IncludeAssertionCode;
      Checked[2] := Options.AllowLabel;
      Checked[3] := Options.CPPInline;
      Checked[4] := Options.CStyleMacros;
      Checked[5] := Options.InitConstructor;
      Checked[6] := Options.StaticKeyword;
      Checked[7] := Options.UseAnsiStrings;
    end;

    cmbSyntaxMode.Text:=SyntaxModeToCaption(Options.SyntaxMode);

    // code generation
    chkSmartLinkUnit.Checked := Options.SmartLinkUnit;

    chkChecksIO.Checked := Options.IOChecks;
    chkChecksRange.Checked := Options.RangeChecks;
    chkChecksOverflow.Checked := Options.OverflowChecks;
    chkChecksStack.Checked := Options.StackChecks;
    chkVerifyObjMethodCall.Checked := Options.VerifyObjMethodCall;

    grpHeapSize.Enabled:=EnabledLinkerOpts;
    edtHeapSize.Text := IntToStr(Options.HeapSize);

    i:=TargetOSComboBox.Items.IndexOf(Options.TargetOS);
    if i<0 then i:=0;  // 0 is default
    TargetOSComboBox.ItemIndex:=i;
    TargetOSComboBox.Text:=Options.TargetOS;
    i:=TargetCPUComboBox.Items.IndexOf(Options.TargetCPU);
    if i<0 then i:=0;  // 0 is default
    TargetCPUComboBox.ItemIndex:=i;
    TargetCPUComboBox.Text:=Options.TargetCPU;

    TargetProcessorProcComboBox.Text:=ProcessorToCaption(Options.TargetProcessor);

    chkOptVarsInReg.Checked := Options.VariablesInRegisters;
    chkOptUncertain.Checked := Options.UncertainOptimizations;
    chkOptSmaller.Checked := Options.SmallerCode;

    case Options.OptimizationLevel of
      1: radOptLevel1.Checked := true;
      2: radOptLevel2.Checked := true;
      3: radOptLevel3.Checked := true;
    else
      radOptLevelNone.Checked := true;
    end;

    // linking
    chkDebugGDB.Checked := Options.GenerateDebugInfo;
    chkUseLineInfoUnit.Checked := Options.UseLineInfoUnit;
    chkGenerateDwarf.Checked := Options.GenerateDwarf;
    chkUseHeaptrc.Checked := Options.UseHeaptrc;
    chkUseValgrind.Checked := Options.UseValgrind;
    chkGenGProfCode.Checked := Options.GenGProfCode;
    chkSymbolsStrip.Checked := Options.StripSymbols;
    chkSymbolsStrip.Enabled:=EnabledLinkerOpts;
    chkUseExternalDbgSyms.Checked := Options.UseExternalDbgSyms;

    chkLinkSmart.Checked := Options.LinkSmart;
    grpLinkLibraries.Enabled:=EnabledLinkerOpts;

    chkOptionsLinkOpt.Checked := Options.PassLinkerOptions;
    edtOptionsLinkOpt.Text := Options.LinkerOptions;
    chkWin32GraphicApp.Checked := Options.Win32GraphicApp;
    chkWin32GraphicApp.Enabled:=EnabledLinkerOpts;
    grpOptions.Enabled:=EnabledLinkerOpts;

    // messages
    with grpVerbosity do
    begin
      Checked[0] := Options.ShowErrors;
      Checked[1] := Options.ShowHintsForSenderNotUsed;
      Checked[2] := Options.ShowWarn;
      Checked[3] := Options.ShowDebugInfo;
      Checked[4] := Options.ShowNotes;
      Checked[5] := Options.ShowUsedFiles;
      Checked[6] := Options.ShowHints;
      Checked[7] := Options.ShowTriedFiles;
      Checked[8] := Options.ShowGenInfo;
      Checked[9] := Options.ShowDefMacros;
      Checked[10] := Options.ShowLineNum;
      Checked[11] := Options.ShowCompProc;
      Checked[12] := Options.ShowAllProcsOnError;
      Checked[13] := Options.ShowCond;
      Checked[14] := Options.ShowAll;
      Checked[15] := Options.ShowExecInfo;
      Checked[16] := Options.ShowSummary;
      Checked[17] := Options.ShowNothing;
      Checked[18] := Options.ShowHintsForUnusedUnitsInMainSrc;
      Checked[19] := Options.WriteFPCLogo;
    end;

    // compiler messages
    chkUseMsgFile.OnChange := nil;
    try
      chkUseMsgFile.Checked := Options.UseMsgFile;
      editMsgFileName.Caption := Options.MsgFileName;
      TempMessages.Assign(Options.CompilerMessages);
      SetCompilerMessages;
    finally
      chkUseMsgFile.OnChange := @chkUseMsgFileChange;
    end;

    // other
    chkConfigFile.Checked := not Options.DontUseConfigFile;
    chkCustomConfigFile.Checked := Options.CustomConfigFile;
    edtConfigPath.Enabled := chkCustomConfigFile.Checked;
    edtConfigPath.Text := Options.ConfigFilePath;
    memCustomOptions.Text := Options.CustomOptions;

    edtErrorCnt.Text := IntToStr(Options.StopAfterErrCount);

    // conditionals + build macros
    BuildMacrosFrame.LoadFromOptions(Options);

    // inherited tab
    UpdateInheritedTab;

    // compilation
    chkCreateMakefile.Checked:=Options.CreateMakefileOnBuild;

    ExecuteBeforeCommandEdit.Text:=Options.ExecuteBefore.Command;
    ExecuteBeforeScanFPCCheckBox.Checked:=Options.ExecuteBefore.ScanForFPCMessages;
    ExecuteBeforeScanMakeCheckBox.Checked:=
                                      Options.ExecuteBefore.ScanForMakeMessages;
    ExecuteBeforeShowAllCheckBox.Checked:=Options.ExecuteBefore.ShowAllMessages;
    if Options.ExecuteBefore is TProjectCompilationToolOptions
    then with TProjectCompilationToolOptions(Options.ExecuteBefore) do begin
      chkExecBeforeCompile.Checked := crCompile in CompileReasons;
      chkExecBeforeBuild.Checked := crBuild in CompileReasons;
      chkExecBeforeRun.Checked := crRun in CompileReasons;
      lblRunIfExecBefore.Visible := True;
      chkExecBeforeCompile.Visible := True;
      chkExecBeforeBuild.Visible := True;
      chkExecBeforeRun.Visible := True;
    end
    else begin
      lblRunIfExecBefore.Visible := False;
      chkExecBeforeCompile.Visible := False;
      chkExecBeforeBuild.Visible := False;
      chkExecBeforeRun.Visible := False;
    end;

    edtCompiler.Text := Options.CompilerPath;
    if Options is TProjectCompilerOptions
    then with TProjectCompilerOptions(Options) do begin
      lblRunIfCompiler.Visible := True;
      chkCompilerCompile.AnchorToNeighbour(akLeft,30,lblRunIfCompiler);
      chkCompilerCompile.Checked := crCompile in CompileReasons;
      chkCompilerBuild.Checked := crBuild in CompileReasons;
      chkCompilerRun.Checked := crRun in CompileReasons;
      chkCompilerCompile.Caption := lisCOCallOnCompile;
      chkCompilerCompile.Visible := True;
      chkCompilerBuild.Visible := True;
      chkCompilerRun.Visible := True;
      edtCompiler.AnchorToNeighbour(akTop,0,chkCompilerCompile);
    end
    else if Options is TPkgCompilerOptions
    then begin
      lblRunIfCompiler.Visible := False;
      chkCompilerCompile.AnchorParallel(akTop,6,chkCompilerCompile.Parent);
      chkCompilerCompile.AnchorParallel(akLeft,6,chkCompilerCompile.Parent);
      chkCompilerCompile.Visible := True;
      chkCompilerCompile.Caption := lisCOSkipCallingCompiler;
      chkCompilerCompile.Checked := TPkgCompilerOptions(Options).SkipCompiler;
      chkCompilerBuild.Visible := False;
      chkCompilerRun.Visible := False;
      edtCompiler.AnchorToNeighbour(akTop,0,chkCompilerCompile);
    end
    else begin
      lblRunIfCompiler.Visible := False;
      chkCompilerCompile.Visible := False;
      chkCompilerBuild.Visible := False;
      chkCompilerRun.Visible := False;
      edtCompiler.AnchorParallel(akTop,0,lblCompiler.Parent);
    end;

    ExecuteAfterCommandEdit.Text:=Options.ExecuteAfter.Command;
    ExecuteAfterScanFPCCheckBox.Checked:=Options.ExecuteAfter.ScanForFPCMessages;
    ExecuteAfterScanMakeCheckBox.Checked:=Options.ExecuteAfter.ScanForMakeMessages;
    ExecuteAfterShowAllCheckBox.Checked:=Options.ExecuteAfter.ShowAllMessages;
    if Options.ExecuteAfter is TProjectCompilationToolOptions
    then with TProjectCompilationToolOptions(Options.ExecuteAfter) do begin
      chkExecAfterCompile.Checked := crCompile in CompileReasons;
      chkExecAfterBuild.Checked := crBuild in CompileReasons;
      chkExecAfterRun.Checked := crRun in CompileReasons;
      lblRunIfExecAfter.Visible := True;
      chkExecAfterCompile.Visible := True;
      chkExecAfterBuild.Visible := True;
      chkExecAfterRun.Visible := True;
    end
    else begin
      lblRunIfExecAfter.Visible := False;
      chkExecAfterCompile.Visible := False;
      chkExecAfterBuild.Visible := False;
      chkExecAfterRun.Visible := False;
    end;
  finally
    EnableAlign;
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions PutCompilerOptions
------------------------------------------------------------------------------}
function TfrmCompilerOptions.SaveFormToOptions(
  CheckAndWarn: TCheckCompileOptionsMsgLvl;
  DestCompilerOptions: TBaseCompilerOptions): boolean;

  function MakeCompileReasons(const ACompile, ABuild, ARun: TCheckBox): TCompileReasons;
  begin
    Result := [];
    if ACompile.Checked then Include(Result, crCompile);
    if ABuild.Checked then Include(Result, crBuild);
    if ARun.Checked then Include(Result, crRun);
  end;
  
  function CheckPutSearchPath(const Context, OldExpandedPath,
    NewExpandedPath: string): boolean;
  var
    Level: TCheckCompileOptionsMsgLvl;
  begin
    Level:=CheckAndWarn;
    if OldExpandedPath<>NewExpandedPath then
      Level:=ccomlHints;
    Result:=CheckSearchPath(Context,NewExpandedPath,Level);
  end;
  
var
  code: LongInt;
  hs: LongInt;
  i: integer;
  NewTargetOS,
  NewTargetCPU: String;
  Options: TBaseCompilerOptions;
  NewDontUseConfigFile: Boolean;
  NewCustomConfigFile: Boolean;
  NewConfigFilePath: String;
  AdditionalConfig: String;
  OldPath: String;
  //Diff: TStringList;
begin
  Result:=false;

  // Put the compiler options from the dialog into the TCompilerOptions class
  if DestCompilerOptions<>nil then
    Options:=DestCompilerOptions
  else
    Options:=CompilerOpts;
  if ReadOnly and (Options=CompilerOpts) then exit(true);

  NewDontUseConfigFile:=not chkConfigFile.Checked;
  NewCustomConfigFile:=chkCustomConfigFile.Checked;
  NewConfigFilePath:=edtConfigPath.Text;

  if ord(CheckAndWarn)<=ord(ccomlWarning) then begin
    if ((NewDontUseConfigFile<>Options.DontUseConfigFile)
        or (NewCustomConfigFile<>Options.CustomConfigFile)
        or (NewConfigFilePath<>Options.ConfigFilePath))
    and (not NewDontUseConfigFile) and NewCustomConfigFile
    then begin
      // config file options changed
      // and both additional and standard config files are used
      AdditionalConfig:=ExtractFilename(edtConfigPath.Text);
      if (CompareFileNames(AdditionalConfig,'fpc.cfg')=0)
      or (CompareFileNames(AdditionalConfig,'ppc386.cfg')=0)
      then begin
        if MessageDlg(lisCOAmbiguousAdditionalCompilerConfigFile,
          Format(lisCOClickOKIfAreSureToDoThat, [BreakString(
            lisCOWarningTheAdditionalCompilerConfigFileHasTheSameNa,
            60, 0), #13#13]),
          mtWarning,[mbOk,mbCancel],0)<>mrOk
        then begin
          Result:=false;
          exit;
        end;
      end;
    end;
  end;

  try

    // paths
    OldPath:=Options.GetIncludePath(false);
    Options.IncludePath := edtIncludeFiles.Text;
    if not CheckPutSearchPath('include search path',OldPath,Options.GetIncludePath(false)) then
      exit(false);
    OldPath:=Options.GetLibraryPath(false);
    Options.Libraries := edtLibraries.Text;
    if not CheckPutSearchPath('library search path',OldPath,Options.GetLibraryPath(false)) then
      exit(false);
    OldPath:=Options.GetUnitPath(false);
    Options.OtherUnitFiles := edtOtherUnits.Text;
    if not CheckPutSearchPath('unit search path',OldPath,Options.GetUnitPath(false)) then
      exit(false);
    OldPath:=Options.GetSrcPath(false);
    Options.SrcPath := edtOtherSources.Text;
    if not CheckPutSearchPath('source search path',OldPath,Options.GetSrcPath(false)) then
      exit(false);
    Options.UnitOutputDirectory := edtUnitOutputDir.Text;
    OldPath:=Options.GetDebugPath(false);
    Options.DebugPath := edtDebugPath.Text;
    if not CheckPutSearchPath('debugger search path',OldPath,Options.GetDebugPath(false)) then
      exit(false);

    // ToDo: will be replaced by build macro
    i:=LCLWidgetTypeComboBox.Itemindex;
    if i<=0 then
      Options.LCLWidgetType:=''
    else
      Options.LCLWidgetType:= LCLPlatformDirNames[TLCLPlatform(i-1)];

    // parsing
    Options.AssemblerStyle := grpAsmStyle.ItemIndex;

    with grpSyntaxOptions do
    begin
      Options.CStyleOperators := Checked[0];
      Options.IncludeAssertionCode := Checked[1];
      Options.AllowLabel := Checked[2];
      Options.CPPInline := Checked[3];
      Options.CStyleMacros := Checked[4];
      Options.InitConstructor := Checked[5];
      Options.StaticKeyword := Checked[6];
      Options.UseAnsiStrings := Checked[7];
    end;

    Options.SyntaxMode:=CaptionToSyntaxMode(cmbSyntaxMode.Text);

    // code generation
    Options.SmartLinkUnit := chkSmartLinkUnit.Checked;

    Options.IOChecks := chkChecksIO.Checked;
    Options.RangeChecks := chkChecksRange.Checked;
    Options.OverflowChecks := chkChecksOverflow.Checked;
    Options.StackChecks := chkChecksStack.Checked;
    Options.VerifyObjMethodCall := chkVerifyObjMethodCall.Checked;

    Val(edtHeapSize.Text, hs, code);
    if (code <> 0) then
      Options.HeapSize := 0
    else
      Options.HeapSize := hs;


    NewTargetOS:=TargetOSComboBox.Text;
    if TargetOSComboBox.Items.IndexOf(NewTargetOS)<=0 then
      NewTargetOS:='';
    Options.TargetOS:=NewTargetOS;

    NewTargetCPU:=TargetCPUComboBox.Text;
    if TargetCPUComboBox.Items.IndexOf(NewTargetCPU)<=0 then
      NewTargetCPU:='';
    Options.TargetCPU:=NewTargetCPU;

    Options.TargetProcessor := CaptionToProcessor(TargetProcessorProcComboBox.Text);
    Options.VariablesInRegisters := chkOptVarsInReg.Checked;
    Options.UncertainOptimizations := chkOptUncertain.Checked;
    Options.SmallerCode := chkOptSmaller.Checked;

    if (radOptLevel1.Checked) then
      Options.OptimizationLevel := 1
    else if (radOptLevel2.Checked) then
      Options.OptimizationLevel := 2
    else if (radOptLevel3.Checked) then
      Options.OptimizationLevel := 3
    else
      Options.OptimizationLevel := 0;

    // linking
    Options.GenerateDebugInfo := chkDebugGDB.Checked;
    Options.UseLineInfoUnit := chkUseLineInfoUnit.Checked;
    Options.GenerateDwarf := chkGenerateDwarf.Checked;
    Options.UseHeaptrc := chkUseHeaptrc.Checked;
    Options.UseValgrind := chkUseValgrind.Checked;
    Options.GenGProfCode := chkGenGProfCode.Checked;
    Options.StripSymbols := chkSymbolsStrip.Checked;
    Options.UseExternalDbgSyms := chkUseExternalDbgSyms.Checked;

    Options.PassLinkerOptions := chkOptionsLinkOpt.Checked;
    Options.LinkerOptions := edtOptionsLinkOpt.Text;
    Options.Win32GraphicApp := chkWin32GraphicApp.Checked;
    Options.LinkSmart := chkLinkSmart.Checked;

    // messages
    with grpVerbosity do
    begin
      Options.ShowErrors := Checked[0];
      Options.ShowHintsForSenderNotUsed := Checked[1];
      Options.ShowWarn := Checked[2];
      Options.ShowDebugInfo := Checked[3];
      Options.ShowNotes := Checked[4];
      Options.ShowUsedFiles := Checked[5];
      Options.ShowHints := Checked[6];
      Options.ShowTriedFiles := Checked[7];
      Options.ShowGenInfo := Checked[8];
      Options.ShowDefMacros := Checked[9];
      Options.ShowLineNum := Checked[10];
      Options.ShowCompProc := Checked[11];
      Options.ShowAllProcsOnError := Checked[12];
      Options.ShowCond := Checked[13];
      Options.ShowAll := Checked[14];
      Options.ShowExecInfo := Checked[15];
      Options.ShowSummary := Checked[16];
      Options.ShowNothing := Checked[17];
      Options.ShowHintsForUnusedUnitsInMainSrc := Checked[18];
      Options.WriteFPCLogo := Checked[19];
    end;

    // compiler messages
    Options.UseMsgFile := chkUseMsgFile.Checked;
    Options.MsgFileName := editMsgFileName.Caption;
    Options.CompilerMessages.Assign(TempMessages);

    // conditionals
    BuildMacrosFrame.SaveToOptions(Options);

    // other
    Options.DontUseConfigFile := not chkConfigFile.Checked;
    Options.CustomConfigFile := chkCustomConfigFile.Checked;
    Options.ConfigFilePath := edtConfigPath.Text;
    Options.CustomOptions := memCustomOptions.Text;

    Options.StopAfterErrCount := StrToIntDef(edtErrorCnt.Text,1);

    // compilation
    Options.CreateMakefileOnBuild := chkCreateMakefile.Checked;

    Options.ExecuteBefore.Command := ExecuteBeforeCommandEdit.Text;
    Options.ExecuteBefore.ScanForFPCMessages :=
                                             ExecuteBeforeScanFPCCheckBox.Checked;
    Options.ExecuteBefore.ScanForMakeMessages :=
                                            ExecuteBeforeScanMakeCheckBox.Checked;
    Options.ExecuteBefore.ShowAllMessages:=ExecuteBeforeShowAllCheckBox.Checked;
    if Options.ExecuteBefore is TProjectCompilationToolOptions
    then begin
      TProjectCompilationToolOptions(Options.ExecuteBefore).CompileReasons :=
        MakeCompileReasons(
          chkExecBeforeCompile,
          chkExecBeforeBuild,
          chkExecBeforeRun
        );
    end;

    Options.CompilerPath := edtCompiler.Text;
    if Options is TProjectCompilerOptions
    then begin
      TProjectCompilerOptions(Options).CompileReasons := MakeCompileReasons(
        chkCompilerCompile,
        chkCompilerBuild,
        chkCompilerRun
      );
    end
    else if Options is TPkgCompilerOptions
    then begin
      TPkgCompilerOptions(Options).SkipCompiler := chkCompilerCompile.Checked;
    end;

    Options.ExecuteAfter.Command := ExecuteAfterCommandEdit.Text;
    Options.ExecuteAfter.ScanForFPCMessages :=
                                              ExecuteAfterScanFPCCheckBox.Checked;
    Options.ExecuteAfter.ScanForMakeMessages :=
                                             ExecuteAfterScanMakeCheckBox.Checked;
    Options.ExecuteAfter.ShowAllMessages:=ExecuteAfterShowAllCheckBox.Checked;
    if Options.ExecuteAfter is TProjectCompilationToolOptions
    then begin
      TProjectCompilationToolOptions(Options.ExecuteAfter).CompileReasons :=
        MakeCompileReasons(
          chkExecAfterCompile,
          chkExecAfterBuild,
          chkExecAfterRun
        );
    end;

    // check for change and mark as modified
    //debugln(['TfrmCompilerOptions.SaveFormToOptions "',OldCompOpts.CustomOptions,'" "',Options.CustomOptions,'"']);
    if not OldCompOpts.IsEqual(Options) then begin
      //Diff:=TStringList.Create;
      //Options.CreateDiffAsText(OldCompOpts,Diff);
      //debugln('DIFF:',Diff.Text);
      //Diff.Free;
      Options.Modified:=true;
      //debugln(['TfrmCompilerOptions.SaveFormToOptions AAA1 ',Options.Modified,' ',dbgs(Options.ChangeStamp),' ',Options.ChangeStamp=Options.InvalidChangeStamp]);
      IncreaseCompilerParseStamp;
    end;
    Result:=true;
  finally
    if not Result then begin
      Options.Assign(OldCompOpts);
      Options.Modified:=OldCompOpts.Modified;
    end;
  end;
  //debugln(['TfrmCompilerOptions.SaveFormToOptions ',Options.Modified]);
end;

function TfrmCompilerOptions.SaveFormToOptions(
  CheckAndWarn: TCheckCompileOptionsMsgLvl): boolean;
begin
  Result:=SaveFormToOptions(CheckAndWarn,nil);
end;

procedure TfrmCompilerOptions.UpdateInheritedTab;
var
  OptionsList: TFPList;
  i: Integer;
  AncestorOptions: TAdditionalCompilerOptions;
  AncestorNode: TTreeNode;

  procedure AddChildNode(const NewNodeName, Value: string;
    Option: TInheritedCompilerOption);
  var
    VisibleValue: String;
    ChildNode: TTreeNode;
    ChildData: PInheritedNodeData;
  begin
    if Value='' then exit;
    New(ChildData);
    ChildData^.FullText:=Value;
    ChildData^.Option:=Option;
    if InheritedChildDatas=nil then InheritedChildDatas:=TList.Create;
    InheritedChildDatas.Add(ChildData);

    if length(Value)>100 then
      VisibleValue:=copy(Value,1,100)+'[...]'
    else
      VisibleValue:=Value;
    ChildNode:=InhTreeView.Items.AddChildObject(AncestorNode,
                                 NewNodeName+' = "'+VisibleValue+'"',ChildData);
    ChildNode.ImageIndex:=ImageIndexRequired;
    ChildNode.SelectedIndex:=ChildNode.ImageIndex;
  end;

begin
  OptionsList:=nil;
  CompilerOpts.GetInheritedCompilerOptions(OptionsList);
  InhTreeView.BeginUpdate;
  ClearInheritedTree;
  if OptionsList<>nil then begin
    // add All node
    AncestorNode:=InhTreeView.Items.Add(nil,lisAllInheritedOptions);
    AncestorNode.ImageIndex:=ImageIndexInherited;
    AncestorNode.SelectedIndex:=AncestorNode.ImageIndex;
    with CompilerOpts do begin
      AddChildNode(lisunitPath,
        GetInheritedOption(icoUnitPath,true),icoUnitPath);
      AddChildNode(lisincludePath,
        GetInheritedOption(icoIncludePath,true),icoIncludePath);
      AddChildNode(lisobjectPath,
        GetInheritedOption(icoObjectPath,true),icoObjectPath);
      AddChildNode(lislibraryPath,
        GetInheritedOption(icoLibraryPath,true),icoLibraryPath);
      AddChildNode(lislinkerOptions,GetInheritedOption(icoLinkerOptions,true),
        icoLinkerOptions);
      AddChildNode(liscustomOptions,GetInheritedOption(icoCustomOptions,true),
        icoCustomOptions);
    end;
    AncestorNode.Expanded:=true;
    // add detail nodes
    for i:=0 to OptionsList.Count-1 do begin
      AncestorOptions:=TAdditionalCompilerOptions(OptionsList[i]);
      AncestorNode:=InhTreeView.Items.Add(nil,'');
      AncestorNode.Text:=AncestorOptions.GetOwnerName;
      AncestorNode.ImageIndex:=ImageIndexPackage;
      AncestorNode.SelectedIndex:=AncestorNode.ImageIndex;
      with AncestorOptions.ParsedOpts do begin
        AddChildNode(lisunitPath,
          CreateRelativeSearchPath(GetParsedValue(pcosUnitPath),
                                   CompilerOpts.BaseDirectory),
          icoUnitPath);
        AddChildNode(lisincludePath,
          CreateRelativeSearchPath(GetParsedValue(pcosIncludePath),
                                   CompilerOpts.BaseDirectory),
          icoIncludePath);
        AddChildNode(lisobjectPath,
          CreateRelativeSearchPath(GetParsedValue(pcosObjectPath),
                                   CompilerOpts.BaseDirectory),
          icoObjectPath);
        AddChildNode(lislibraryPath,
          CreateRelativeSearchPath(GetParsedValue(pcosLibraryPath),
                                   CompilerOpts.BaseDirectory),
          icoLibraryPath);
        AddChildNode(lislinkerOptions, GetParsedValue(pcosLinkerOptions),
          icoLinkerOptions);
        AddChildNode(liscustomOptions, GetParsedValue(pcosCustomOptions),
          icoCustomOptions);
      end;
      AncestorNode.Expanded:=true;
    end;
    OptionsList.Free;
  end else begin
    InhTreeView.Items.Add(nil, lisNoCompilerOptionsInherited);
  end;
  InhTreeView.EndUpdate;
end;

procedure TfrmCompilerOptions.ClearInheritedTree;
var
  i: Integer;
  ChildData: PInheritedNodeData;
begin
  if InhTreeView=nil then exit;
  InhTreeView.BeginUpdate;
  // dispose all child data
  if InheritedChildDatas<>nil then begin
    for i:=0 to InheritedChildDatas.Count-1 do begin
      ChildData:=PInheritedNodeData(InheritedChildDatas[i]);
      Dispose(ChildData);
    end;
    InheritedChildDatas.Free;
    InheritedChildDatas:=nil;
  end;
  InhTreeView.Items.Clear;
  InhTreeView.EndUpdate;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupParsingTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupParsingTab(Page: integer);
var
  m: TCompilerMode;
  s: String;
begin
  MainNoteBook.Page[Page].Caption:= dlgCOParsing;
  CategoryTreeView.Items.AddObject(nil,MainNoteBook.Page[Page].Caption,MainNoteBook.Page[Page]);

  // Setup the Parsing Tab
  with grpAsmStyle do begin
    Caption := dlgCOAsmStyle+' (-R)';

    with Items do begin
      BeginUpdate;
      Add(dlgAssemblerDefault);
      Add('Intel');
      Add('AT&&T');
      EndUpdate;
    end;
  end;

  with grpSyntaxOptions do begin
    AutoSize:=true;
    Caption := dlgSyntaxOptions;

    Items.Add(dlgCOCOps+' (-Sc)');
    Items.Add(dlgAssertCode+' (-Sa)');
    Items.Add(dlgLabelGoto+' (-Sg)');
    Items.Add(dlgCppInline+' (-Si)');
    Items.Add(dlgCMacro+' (-Sm)');
    Items.Add(dlgInitDoneOnly+' (-Ss)');
    Items.Add(dlgStaticKeyword+' (-St)');
    Items.Add(dlgCOAnsiStr+' (-Sh)');
  end;

  grpSyntaxMode.Caption:=lisSyntaxMode+' (-M)';
  cmbSyntaxMode.Items.BeginUpdate;
  cmbSyntaxMode.Items.Clear;
  for m:=Low(TCompilerMode) to High(TCompilerMode) do begin
    s:=SyntaxModeToCaption(CompilerModeNames[m]);
    if s<>'' then
      cmbSyntaxMode.Items.Add(s);
  end;
  cmbSyntaxMode.Items.EndUpdate;
end;


{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupCodeGenerationTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupCodeGenerationTab(Page: integer);
begin
  // Setup the Code Generation Tab
  MainNoteBook.Page[Page].Caption:= dlgCodeGeneration;
  CategoryTreeView.Items.AddObject(nil,MainNoteBook.Page[Page].Caption,MainNoteBook.Page[Page]);

  grpSmartLinkUnit.Caption := dlgCOUnitStyle;
  chkSmartLinkUnit.Caption := dlgCOSmartLinkable + ' (-CX)';

  grpChecks.Caption := dlgCOChecks;
  chkChecksIO.Caption := 'I/O (-Ci)';
  chkChecksRange.Caption := dlgCORange+' (-Cr)';
  chkChecksOverflow.Caption := dlgCOOverflow+' (-Co)';
  chkChecksStack.Caption := dlgCOStack+' (-Ct)';
  chkVerifyObjMethodCall.Caption := lisVerifyMethodCalls+' (-CR)';

  grpHeapSize.Caption := dlgHeapSize +' (-Ch):';
  edtHeapSize.Text := '';

  grpTargetPlatform.Caption := dlgTargetPlatform;
  lblTargetOS.Caption := dlgTargetOS+' (-T)';

  with TargetOSComboBox do
  begin
    with Items do begin
      Add('('+rsiwpDefault+')');
      Add('Darwin');
      Add('FreeBSD');
      Add('Linux');
      Add('NetBSD');
      Add('OpenBSD');
      Add('Solaris');
      Add('Win32');
      Add('Win64');
      Add('WinCE');
      Add('go32v2');
      Add('os2');
      Add('beos');
      Add('haiku');
      Add('qnx');
      Add('netware');
      Add('wdosx');
      Add('emx');
      Add('watcom');
      Add('netwlibc');
      Add('amiga');
      Add('atari');
      Add('palmos');
      Add('gba');
      Add('nds');
      Add('macos');
      Add('morphos');
      Add('embedded');
      Add('symbian');
    end;
    ItemIndex:=0;
  end;

  lblTargetCPU.Caption :=dlgTargetCPUFamily+' (-P)';

  with TargetCPUComboBox do begin
    with Items do begin
      Add('('+rsiwpDefault+')');
      Add('arm');
      Add('i386');
      Add('m68k');
      Add('powerpc');
      Add('sparc');
      Add('x86_64');
    end;
    ItemIndex:=0;
  end;

  lblTargetProcessorProc.Caption := dlgTargetProc;

  with TargetProcessorProcComboBox do begin
    with Items do begin
      Clear;
      Add(ProcessorToCaption(''));
      Add(ProcessorToCaption('386'));
      Add(ProcessorToCaption('Pentium'));
      Add(ProcessorToCaption('Pentium2'));
      Add(ProcessorToCaption('Pentium3'));
      Add(ProcessorToCaption('Pentium4'));
      Add(ProcessorToCaption('PentiumM'));
    end;
    ItemIndex:=0;
  end;

  grpOptimizations.Caption := dlgOptimiz;
  radOptLevelNone.Caption :=  dlgLevelNoneOpt+' (none)';
  radOptLevel1.Caption :=  dlgLevel1Opt+' (-O1)';
  radOptLevel2.Caption := dlgLevel2Opt+' (-O2)';
  radOptLevel3.Caption := dlgLevel3Opt+' (-O3)';
  chkOptVarsInReg.Caption := dlgCOKeepVarsReg+' (-Or)';
  chkOptUncertain.Caption := dlgUncertOpt+' (-Ou)';
  chkOptSmaller.Caption := lisSmallerRatherThanFaster+' (-Os)';
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupLinkingTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupLinkingTab(Page: integer);
begin
  MainNoteBook.Page[Page].Caption:= dlgCOLinking;
  CategoryTreeView.Items.AddObject(nil,MainNoteBook.Page[Page].Caption,MainNoteBook.Page[Page]);

  // Setup the Linking Tab
  with grpDebugging do begin
    AutoSize:=true;
    Caption := dlgCODebugging;
  end;

  chkDebugGDB.Caption := dlgCOGDB+' (-g)';
  chkUseLineInfoUnit.Caption := dlgLNumsBct+' (-gl)';
  chkGenerateDwarf.Caption := dlgGenerateDwarf + '(-gw)';
  chkUseHeaptrc.Caption := dlgCOHeaptrc+' (-gh)';
  chkUseValgrind.Caption := dlgCOValgrind+' (-gv)';
  chkGenGProfCode.Caption := dlgGPROF+' (-pg)';
  chkSymbolsStrip.Caption := dlgCOStrip+' (-Xs)';
  chkUseExternalDbgSyms.Caption := dlgExtSymb +' (-Xg)';

  grpLinkLibraries.Caption := dlgLinkLibraries;
  chkLinkSmart.Caption := dlgLinkSmart+' (-XX)';

  TargetSpecificsGrpBox.Caption := lisCOTargetOSSpecificOptions;
  chkWin32GraphicApp.Caption := dlgWin32GUIApp+' (-WG)';

  grpOptions.Caption := dlgCOOpts+' (-k)';
  chkOptionsLinkOpt.Caption := dlgPassOptsLinker;
  edtOptionsLinkOpt.Text := '';
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupVerbosityTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupVerbosityTab(Page: integer);
begin
  // Setup the Messages Tab
  MainNoteBook.Page[Page].Caption:= dlgCOVerbosity;
  CategoryTreeView.Items.AddObject(nil,MainNoteBook.Page[Page].Caption,MainNoteBook.Page[Page]);

  with grpVerbosity do begin
    Caption := dlgVerbosity;
    AutoSize:=true;

    Items.Add(dlgCOShowErr+' (-ve)');
    Items.Add(dlgHintsParameterSenderNotUsed+' (none)');
    Items.Add(dlgShowWarnings+' (-vw)');
    Items.Add(dlgShowDebugInfo+' (-vd)');
    Items.Add(dlgShowNotes+' (-vn)');
    Items.Add(dlgShowUsedFiles+' (-vu)');
    Items.Add(dlgShowHint+' (-vh)');
    Items.Add(dlgShowTriedFiles+' (-vt)');
    Items.Add(dlgShowGeneralInfo+' (-vi)');
    Items.Add(dlgShowDefinedMacros+' (-vm)');
    Items.Add(dlgShowCompilingLineNumbers+' (-vl)');
    Items.Add(dlgShowCompiledProcedures+' (-vp)');
    Items.Add(dlgShowProcsError+' (-vb)');
    Items.Add(dlgShowConditionals+' (-vc)');
    Items.Add(dlgShowEverything+' (-va)');
    Items.Add(dlgShowExecutableInfo+' (-vx)');
    Items.Add(dlgShowSummary+' (none)');
    Items.Add(dlgShowNothing+' (-v0)');
    Items.Add(dlgHintsUnused+' (none)');
    Items.Add(dlgWriteFPCLogo+' (-l)');
  end;

  grpErrorCnt.Caption := dlgStopAfterNrErr+' (-Se)';
  edtErrorCnt.Text := '';
end;

procedure TfrmCompilerOptions.SetupOtherTab(Page: integer);
begin
  MainNoteBook.Page[Page].Caption:= dlgCOOther;
  CategoryTreeView.Items.AddObject(nil,MainNoteBook.Page[Page].Caption,MainNoteBook.Page[Page]);

  grpConfigFile.Caption := dlgConfigFiles;
  chkConfigFile.Caption := dlgUseFpcCfg+' (If not checked: -n)';
  chkCustomConfigFile.Caption := dlgUseCustomConfig+' (@)';
  edtConfigPath.Text := '';

  grpCustomOptions.Caption:=lisCustomOptions2;
end;

procedure TfrmCompilerOptions.SetupMacrosTab(Page: integer);
begin
  MacrosPage:=MainNoteBook.Pages[Page];
  MacrosPage.Caption:=lisBuildMacros;
  CategoryTreeView.Items.AddObject(nil,MacrosPage.Caption,MacrosPage);
end;

{------------------------------------------------------------------------------
  procedure TfrmCompilerOptions.SetupInheritedTab(Page: integer);
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupInheritedTab(Page: integer);
begin
  MainNoteBook.Page[Page].Caption:= dlgCOInherited;
  CategoryTreeView.Items.AddObject(nil,MainNoteBook.Page[Page].Caption,MainNoteBook.Page[Page]);

  InhNoteLabel.Caption:=lisAdditionalCompilerOptionsInheritedFromPackages;

  with InhTreeView do begin
    Options:=Options+[tvoReadOnly, tvoRightClickSelect, tvoShowRoot,
                      tvoKeepCollapsedNodes];
    Images := IDEImages.Images_16;
  end;

  InhItemMemo.Text:=lisSelectANode;
end;

procedure TfrmCompilerOptions.SetupCompilationTab(Page: integer);
// the real edit width is calculated in the groupbox resize
// here the controls are given initial sizes to avoid jumpy controls

begin
  MainNoteBook.Page[Page].Caption:= dlgCOCompilation;
  CategoryTreeView.Items.AddObject(nil,MainNoteBook.Page[Page].Caption,MainNoteBook.Page[Page]);

  chkCreateMakefile.Caption := dlgCOCreateMakefile;

  ExecuteBeforeGroupBox.Caption := lisCOExecuteBefore;
  chkExecBeforeBuild.Caption := lisCOCallOnBuild;
  chkExecBeforeCompile.Caption := lisCOCallOnCompile;
  chkExecBeforeRun.Caption := lisCOCallOnRun;
  ExecuteBeforeCommandEdit.Text:='';
  ExecuteBeforeCommandLabel.Caption:=lisCOCommand;
  ExecuteBeforeScanFPCCheckBox.Caption := lisCOScanForFPCMessages;
  ExecuteBeforeScanMakeCheckBox.Caption := lisCOScanForMakeMessages;
  ExecuteBeforeShowAllCheckBox.Caption := lisCOShowAllMessages;
  lblRunIfExecBefore.Caption := lisCOCallOn;

  grpCompiler.Caption := lisCompiler;
  chkCompilerBuild.Caption := lisCOCallOnBuild;
  chkCompilerBuild.Checked := True;
  chkCompilerCompile.Caption := lisCOCallOnCompile;
  chkCompilerCompile.Checked := True;
  chkCompilerRun.Caption := lisCOCallOnRun;
  chkCompilerRun.Checked := True;
  edtCompiler.Text:='';
  lblCompiler.Caption:=lisCOCommand;
  lblRunIfCompiler.Caption := lisCOCallOn;

  ExecuteAfterGroupBox.Caption := lisCOExecuteAfter;
  chkExecAfterBuild.Caption := lisCOCallOnBuild;
  chkExecAfterCompile.Caption := lisCOCallOnCompile;
  chkExecAfterRun.Caption := lisCOCallOnRun;
  ExecuteAfterCommandEdit.Text:='';
  ExecuteAfterCommandLabel.Caption:=lisCOCommand;
  ExecuteAfterScanFPCCheckBox.Caption := lisCOScanForFPCMessages;
  ExecuteAfterScanMakeCheckBox.Caption := lisCOScanForMakeMessages;
  ExecuteAfterShowAllCheckBox.Caption := lisCOShowAllMessages;
  lblRunIfExecAfter.Caption := lisCOCallOn;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupSearchPathsTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupSearchPathsTab(Page: integer);
var
  LCLInterface: TLCLPlatform;
  s: String;
begin
  // Setup the Search Paths Tab
  MainNoteBook.Page[Page].Caption:= dlgSearchPaths;
  fPathsTVNode:=CategoryTreeView.Items.AddObject(nil,MainNoteBook.Page[Page].Caption,MainNoteBook.Page[Page]);

  lblOtherUnits.Caption := dlgOtherUnitFiles;
  OtherUnitsPathEditBtn:=TPathEditorButton.Create(Self);
  with OtherUnitsPathEditBtn do begin
    Name:='OtherUnitsPathEditBtn';
    Caption:='...';
    Anchors:=[akRight,akTop,akBottom];
    AnchorParallel(akTop,0,edtOtherUnits);
    AnchorParallel(akBottom,0,edtOtherUnits);
    AnchorParallel(akRight,6,PathPage);
    AutoSize:=true;
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
    Parent:=PathPage;
  end;
  edtOtherUnits.AnchorToNeighbour(akRight,0,OtherUnitsPathEditBtn);

  {------------------------------------------------------------}

  lblIncludeFiles.Caption := dlgCOIncFiles;
  IncludeFilesPathEditBtn:=TPathEditorButton.Create(Self);
  with IncludeFilesPathEditBtn do begin
    Name:='IncludeFilesPathEditBtn';
    Anchors:=[akRight,akTop,akBottom];
    AnchorParallel(akTop,0,edtIncludeFiles);
    AnchorParallel(akBottom,0,edtIncludeFiles);
    AnchorParallel(akRight,6,PathPage);
    AutoSize:=true;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
    Parent:=PathPage;
  end;
  edtIncludeFiles.AnchorToNeighbour(akRight,0,IncludeFilesPathEditBtn);

  {------------------------------------------------------------}

  lblOtherSources.Caption := dlgCOSources;
  OtherSourcesPathEditBtn:=TPathEditorButton.Create(Self);
  with OtherSourcesPathEditBtn do begin
    Name:='OtherSourcesPathEditBtn';
    Anchors:=[akRight,akTop,akBottom];
    AnchorParallel(akTop,0,edtOtherSources);
    AnchorParallel(akBottom,0,edtOtherSources);
    AnchorParallel(akRight,6,PathPage);
    AutoSize:=true;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
    Parent:=PathPage;
  end;
  edtOtherSources.AnchorToNeighbour(akRight,0,OtherSourcesPathEditBtn);

  {------------------------------------------------------------}

  lblLibraries.Caption := dlgCOLibraries;
  LibrariesPathEditBtn:=TPathEditorButton.Create(Self);
  with LibrariesPathEditBtn do begin
    Name:='LibrariesPathEditBtn';
    Anchors:=[akRight,akTop,akBottom];
    AnchorParallel(akTop,0,edtLibraries);
    AnchorParallel(akBottom,0,edtLibraries);
    AnchorParallel(akRight,6,PathPage);
    AutoSize:=true;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
    Parent:=PathPage;
  end;
  edtLibraries.AnchorToNeighbour(akRight,0,LibrariesPathEditBtn);

  {------------------------------------------------------------}

  lblUnitOutputDir.Caption := dlgUnitOutp;
  btnUnitOutputDir:=TButton.Create(Self);
  with btnUnitOutputDir do begin
    Name:='btnUnitOutputDir';
    Anchors:=[akRight,akTop,akBottom];
    AnchorParallel(akTop,0,edtUnitOutputDir);
    AnchorParallel(akBottom,0,edtUnitOutputDir);
    AnchorParallel(akRight,6,PathPage);
    AutoSize:=true;
    Caption:='...';
    OnClick:=@FileBrowseBtnClick;
    Parent:=PathPage;
  end;
  edtUnitOutputDir.AnchorToNeighbour(akRight,0,btnUnitOutputDir);

  {------------------------------------------------------------}

  lblDebugPath.Caption := dlgCODebugPath;
  DebugPathEditBtn:=TPathEditorButton.Create(Self);
  with DebugPathEditBtn do begin
    Name:='DebugPathEditBtn';
    Anchors:=[akRight,akTop,akBottom];
    AnchorParallel(akTop,0,edtDebugPath);
    AnchorParallel(akBottom,0,edtDebugPath);
    AnchorParallel(akRight,6,PathPage);
    AutoSize:=true;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
    Parent:=PathPage;
  end;
  edtDebugPath.AnchorToNeighbour(akRight,0,DebugPathEditBtn);

  {------------------------------------------------------------}

  LCLWidgetTypeLabel.Caption:=Format(lisCOVarious, [lisLCLWidgetType]);
  with LCLWidgetTypeComboBox do begin
    with Items do begin
      BeginUpdate;
      s:=LCLPlatformDisplayNames[GetDefaultLCLWidgetType];
      Add(Format(lisCOdefault,[s]));
      for LCLInterface:=Low(TLCLPlatform) to High(TLCLPlatform) do begin
        Items.Add(LCLPlatformDisplayNames[LCLInterface]);
      end;
      EndUpdate;
    end;
    ItemIndex:=1;
    Constraints.MinWidth:=150;
  end;
end;

procedure TfrmCompilerOptions.SetupBuildModesTab(Page: integer);
begin
  // Setup the Build Modes Tab
  MainNoteBook.Page[Page].Caption:='Build modes';
  fBuildModesTVNode:=CategoryTreeView.Items.AddObject(nil,MainNoteBook.Page[Page].Caption,MainNoteBook.Page[Page]);

  fBuildModeFrame:=TBuildModesEditorFrame.Create(Self);
  fBuildModeFrame.Setup(nil);
  with fBuildModeFrame do begin
    Name:='fBuildModeFrame';
    Align:=alClient;
    Parent:=BuildModesPage;
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupButtonBar
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupButtonBar;
begin
  // Setup the Button Bar
  btnShowOptions.Caption := dlgCOShowOptions;
  btnCheck.Caption := lisCompTest;
  btnOK.Caption := lisOk;
  btnCancel.Caption := dlgCancel;
  btnCheck.Caption := dlgCCOTest;
  HelpButton.Caption := lisMenuHelp;
  btnLoadSave.Hint := dlgCOLoadSave;
  btnLoadSave.Caption := '...';
  btnLoadSave.LoadGlyphFromStock(idButtonSave);
  if btnLoadSave.Glyph.Empty then
    btnLoadSave.LoadGlyphFromLazarusResource('laz_save');
end;

function TfrmCompilerOptions.CheckSearchPath(const Context,
  ExpandedPath: string; Level: TCheckCompileOptionsMsgLvl): boolean;
var
  CurPath: String;
  p: Integer;
  HasChars: TCCOSpecialChars;
  ErrorMsg: String;
begin
  Result:=false;
  
  //DebugLn(['TfrmCompilerOptions.CheckSearchPath Context=',Context,' ExpandedPath=',ExpandedPath,' Level=',ord(Level)]);

  // check for *
  if ord(Level)<=ord(ccomlHints) then begin
    if System.Pos('*',ExpandedPath)>0 then begin
      if MessageDlg('Hint','The '+Context+' contains a star * character.'#13
        +'Lazarus uses this as normal character and does not expand this as file mask.',
        mtWarning,[mbOk,mbCancel],0) <> mrOk then exit;
    end;
  end;

  // check for non existing directories
  if ord(Level)<=ord(ccomlWarning) then begin
    p:=1;
    repeat
      //DebugLn(['CheckSearchPath ',ExpandedPath,' ',p,' ',length(ExpandedPath)]);
      CurPath:=GetNextDirectoryInSearchPath(ExpandedPath,p);
      if (CurPath<>'') and (not IDEMacros.StrHasMacros(CurPath))
      and (FilenameIsAbsolute(CurPath)) then begin
        if not DirPathExistsCached(CurPath) then begin
          if MessageDlg('Warning','The '+Context+' contains a not existing directory:'#13
            +CurPath,
            mtWarning,[mbIgnore,mbCancel],0) <> mrIgnore then exit;
        end;
      end;
    until p>length(ExpandedPath);
  end;

  // check for special characters
  if (not IDEMacros.StrHasMacros(CurPath)) then begin
    FindSpecialCharsInPath(ExpandedPath,HasChars);
    if ord(Level)<=ord(ccomlWarning) then begin
      if ord(Level)>=ord(ccomlErrors) then begin
        ErrorMsg:=SpecialCharsToStr(HasChars*[ccoscSpecialChars,ccoscNewLine]);
      end else begin
        ErrorMsg:=SpecialCharsToStr(HasChars);
      end;
      if ErrorMsg<>'' then begin
        if MessageDlg('Warning',Context+#13+ErrorMsg,
          mtWarning,[mbOk,mbCancel],0) <> mrOk then exit;
      end;
    end;
  end;

  Result:=true;
end;

procedure TfrmCompilerOptions.chkCustomConfigFileClick(Sender: TObject);
begin
  edtConfigPath.Enabled:=chkCustomConfigFile.Checked;
end;

procedure TfrmCompilerOptions.PathEditBtnClick(Sender: TObject);
var AButton: TPathEditorButton;
  OldPath, Templates: string;
begin
  if Sender is TPathEditorButton then begin
    AButton:=TPathEditorButton(Sender);
    if AButton=OtherUnitsPathEditBtn then begin
      OldPath:=edtOtherUnits.Text;
      Templates:=SetDirSeparators(
            '$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)'
          +';$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/$(LCLWidgetType)'
          +';$(LazarusDir)/components/codetools/units/$(TargetCPU)-$(TargetOS)'
          +';$(LazarusDir)/components/custom'
          +';$(LazarusDir)/packager/units/$(TargetCPU)-$(TargetOS)'
          );
    end else
    if AButton=IncludeFilesPathEditBtn then begin
      OldPath:=edtIncludeFiles.Text;
      Templates:='include'
                +';inc';
    end else
    if AButton=OtherSourcesPathEditBtn then begin
      OldPath:=edtOtherSources.Text;
      Templates:=SetDirSeparators(
            '$(LazarusDir)/lcl'
          +';$(LazarusDir)/lcl/interfaces/$(LCLWidgetType)'
          +';$(LazarusDir)/components/synedit'
          +';$(LazarusDir)/components/codetools'
          );
    end else
    if AButton=LibrariesPathEditBtn then begin
      OldPath:=edtLibraries.Text;
      Templates:=SetDirSeparators('/usr/X11R6/lib;/sw/lib');
    end else
    if AButton=DebugPathEditBtn then begin
      OldPath:=edtDebugPath.Text;
      Templates:=SetDirSeparators(
            '$(LazarusDir)/lcl/include'
          +';$(LazarusDir)/lcl/interfaces/$(LCLWidgetType)'
          +';$(LazarusDir)/include'
          );
    end else
      exit;
    AButton.CurrentPathEditor.BaseDirectory:=CompilerOpts.BaseDirectory;
    AButton.CurrentPathEditor.Path:=OldPath;
    AButton.CurrentPathEditor.Templates:=SetDirSeparators(Templates);
  end;
end;

procedure TfrmCompilerOptions.PathEditBtnExecuted(Sender: TObject);

  function CheckPath(const Context, NewPath: string): boolean;
  var
    ExpandedPath: String;
    BaseDir: String;
  begin
    BaseDir:=CompilerOpts.BaseDirectory;
    ExpandedPath:=TrimSearchPath(NewPath,BaseDir,true);
    Result:=CheckSearchPath(Context,ExpandedPath,ccomlHints);
  end;

var AButton: TPathEditorButton;
  NewPath: string;
begin
  if Sender is TPathEditorButton then begin
    AButton:=TPathEditorButton(Sender);
    if AButton.CurrentPathEditor.ModalResult<>mrOk then exit;
    NewPath:=AButton.CurrentPathEditor.Path;
    if CompilerOpts<>nil then
      NewPath:=CompilerOpts.ShortenPath(NewPath,false);
    if AButton=OtherUnitsPathEditBtn then begin
      if CheckPath(lblOtherUnits.Caption,NewPath) then
        edtOtherUnits.Text:=NewPath;
    end else
    if AButton=IncludeFilesPathEditBtn then begin
      if CheckPath(lblIncludeFiles.Caption,NewPath) then
        edtIncludeFiles.Text:=NewPath;
    end else
    if AButton=OtherSourcesPathEditBtn then begin
      if CheckPath(lblOtherSources.Caption,NewPath) then
        edtOtherSources.Text:=NewPath;
    end else
    if AButton=LibrariesPathEditBtn then begin
      if CheckPath(lblLibraries.Caption,NewPath) then
        edtLibraries.Text:=NewPath;
    end else
    if AButton=DebugPathEditBtn then begin
      if CheckPath(lblDebugPath.Caption,NewPath) then
        edtDebugPath.Text:=NewPath;
    end;
  end;
end;

procedure TfrmCompilerOptions.frmCompilerOptionsClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TfrmCompilerOptions.SetReadOnly(const AValue: boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
  btnOk.Enabled:=not FReadOnly;
  btnCheck.Enabled:=not FReadOnly;
end;

procedure TfrmCompilerOptions.SetupConfigMsgTab(Page: integer);
begin
  MainNotebook.Page[Page].Caption := dlgCOCfgCmpMessages;
  CategoryTreeView.Items.AddObject(nil,MainNoteBook.Page[Page].Caption,MainNoteBook.Page[Page]);

  grpCompilerMessages.Caption := dlgCompilerMessage;
  chkUseMsgFile.Caption := dlgUseMsgFile;
  editMsgFileName.Caption := '';
end;

procedure TfrmCompilerOptions.chklistCompMsgClick(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to chklistCompMsg.Count - 1 do
    TCompilerMessageConfig(chklistCompMsg.Items.Objects[i]).Ignored := not chklistCompMsg.Checked[i];
end;

procedure TfrmCompilerOptions.chkUseMsgFileChange(Sender: TObject);
begin
  SetCompilerMessages;
end;

procedure TfrmCompilerOptions.btnBrowseMsgClick(Sender: TObject);
var
  dlg : TOpenDialog;
begin
  dlg := TOpenDialog.Create(Self);
  try
    dlg.Filter := dlgBrowseMsgFilter;
    if not dlg.Execute then Exit;
    editMsgFileName.Caption := dlg.FileName;
    SetCompilerMessages;
  finally
    dlg.Free
  end;
end;

procedure TfrmCompilerOptions.SetCompilerMessages;
const
  MaxIndexLen = 5;
var
  i : Integer;
  j : Integer;
  topidx  : Integer;
  m : TCompilerMessageConfig;

const
  //todo: should be translated
  MsgTypeStr : array [TFPCErrorType] of String = ('-','H','N','W','E','F','P');

  function IntToStrLen(idx, strlen: integer): string;
  var
    s : string;
  begin
    Result := IntToStr(idx);
    if length(Result) < strlen then  begin
      SetLength(s, strlen - length(Result));
      FillChar(s[1], length(s), '0');
      Result := s + Result;
    end;
  end;

begin
  topidx := chklistCompMsg.TopIndex;
  chklistCompMsg.Items.BeginUpdate;
  try
    if chkUseMsgFile.Checked and FileExistsUTF8(editMsgFileName.Caption) and (editMsgFileName.Caption <> '') then begin
      try
        // FPC messages file is expected to be UTF8 encoded, no matter for the current code page is
        TempMessages.LoadMsgFile(editMsgFileName.Caption);
      except
        TempMessages.SetDefault;
      end;
    end else
      TempMessages.SetDefault;

    chklistCompMsg.Clear;
    chklistCompMsg.Items.Clear;
    for i := 0 to TempMessages.Count - 1 do
    begin
      m := TempMessages.Msg[i];
      if m.MsgType in [etNote, etHint, etWarning] then
      begin
        j := chklistCompMsg.Items.AddObject( Format('(%s) %s', [MsgTypeStr[m.MsgType], m.GetUserText]), m);
        chklistCompMsg.Checked[j] := not m.Ignored;
      end;
    end;

  finally
    chklistCompMsg.Items.EndUpdate;
    chkListCompMsg.TopIndex := topidx;
  end;
end;

function TfrmCompilerOptions.GetUseAsDefault: Boolean;
begin
  Result := chkUseAsDefault.Checked;
end;

end.
