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

uses
  Forms, Classes, LCLProc, SysUtils, InterfaceBase,
  ComCtrls, Buttons, StdCtrls, ExtCtrls,
  Graphics, LResources, FileUtil, Dialogs, Controls, GraphType,
  ProjectIntf, IDEWindowIntf,
  PathEditorDlg, LazarusIDEStrConsts, IDEOptionDefs, LazConf, IDEProcs,
  CompilerOptions, ShowCompilerOpts, Project, PackageDefs;

type
  { Compiler options form }
  
  TCheckCompileOptionsMsgLvl = (
    ccomlHints,
    ccomlWarning,
    ccomlErrors,
    ccomlNone
    );
  

  { TfrmCompilerOptions }

  TfrmCompilerOptions = class(TForm)
    nbMain: TNotebook;
    { Search Paths Controls }
    PathPage: TPage;
    
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

    { Parsing Controls }
    ParsingPage: TPage;
    grpStyle: TRadioGroup;

    grpSyntaxOptions: TGroupBox;
    chkSymD2Ext: TCheckBox;
    chkSymCOper: TCheckBox;
    chkSymIncludeAssertions: TCheckBox;
    chkSymAllowLab: TCheckBox;
    chkSymUseAnsiStrings: TCheckBox;
    chkSymCPPInline: TCheckBox;
    chkSymCMacros: TCheckBox;
    chkSymDelphiCompat: TCheckBox;
    chkSymTP7Compat: TCheckBox;
    chkSymGPCCompat: TCheckBox;
    chkSymConstInit: TCheckBox;
    chkSymStaticKwd: TCheckBox;

    { Code Generation Controls }
    CodeGenPage: TPage;
    grpSmartLinkUnit: TGroupBox;
    chkSmartLinkUnit: TCheckBox;

    grpChecks: TGroupBox;
    chkChecksIO: TCheckBox;
    chkChecksRange: TCheckBox;
    chkChecksOverflow: TCheckBox;
    chkChecksStack: TCheckBox;

    grpHeapSize: TGroupBox;
    edtHeapSize: TEdit;

    grpGenerate: TGroupBox;
    radGenNormal: TRadioButton;
    radGenFaster: TRadioButton;
    radGenSmaller: TRadioButton;

    grpTargetPlatform: TGroupBox;
    lblTargetOS : TLabel;
    TargetOSComboBox: TComboBox;
    lblTargetCPU : TLabel;
    TargetCPUComboBox: TComboBox;
    lblTargeti386Proc : TLabel;
    Targeti386ProcComboBox: TComboBox;
    TargetOSGroupBox: TGroupBox;

    grpOptimizations: TGroupBox;
    chkOptVarsInReg: TCheckBox;
    chkOptUncertain: TCheckBox;
    radOptLevelNone: TRadioButton;
    radOptLevel1: TRadioButton;
    radOptLevel2: TRadioButton;
    radOptLevel3: TRadioButton;


    { Linking Controls }
    LinkingPage: TPage;
    grpDebugging: TGroupBox;
    chkDebugGDB: TCheckBox;
    chkDebugDBX: TCheckBox;
    chkUseLineInfoUnit: TCheckBox;
    chkUseHeaptrc: TCheckBox;
    chkUseValgrind: TCheckBox;
    chkGenGProfCode: TCheckBox;
    chkSymbolsStrip: TCheckBox;

    grpLinkLibraries: TGroupBox;
    chkLinkSmart: TCheckBox;

    grpOptions: TGroupBox;
    chkOptionsLinkOpt: TCheckBox;
    edtOptionsLinkOpt: TEdit;
    TargetSpecificsGrpBox: TGroupBox;
    chkWin32GraphicApp: TCheckBox;

    { Messages Controls }
    MsgPage: TPage;
    grpVerbosity: TGroupBox;
    chkErrors: TCheckBox;
    chkWarnings: TCheckBox;
    chkNotes: TCheckBox;
    chkHints: TCheckBox;
    chkGeneralInfo: TCheckBox;
    chkLineNumbers: TCheckBox;
    chkEverything: TCheckBox;
    chkShowSummary: TCheckBox;
    chkAllProcsOnError: TCheckBox;
    chkDebugInfo: TCheckBox;
    chkUsedFiles: TCheckBox;
    chkTriedFiles: TCheckBox;
    chkDefinedMacros: TCheckBox;
    chkCompiledProc: TCheckBox;
    chkConditionals: TCheckBox;
    chkExecutableInfo: TCheckBox;
    chkNothing: TCheckBox;
    chkHintsForUnusedUnitsInMainSrc: TCheckBox;
    chkHintsForSenderNotUsed: TCheckBox;
    chkFPCLogo: TCheckBox;

    grpErrorCnt: TGroupBox;
    edtErrorCnt: TEdit;

    { 'Other' Controls }
    OtherPage: TPage;
    grpConfigFile: TGroupBox;
    chkConfigFile: TCheckBox;
    chkCustomConfigFile: TCheckBox;
    edtConfigPath: TEdit;
    grpCustomOptions: TGroupBox;
    memCustomOptions: TMemo;

    { Inherited Options }
    InheritedPage: TPage;
    InhNoteLabel: TLabel;
    InhTreeView: TTreeView;
    InhItemMemo: TMemo;

    { Compilation }
    CompilationPage: TPage;

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
    btnCompiler: TButton;

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
    btnShowOptions: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnCheck: TButton;
    btnLoadSave: TButton;

    procedure ButtonOKClicked(Sender: TObject);
    procedure btnTestClicked(Sender: TObject);
    procedure ButtonLoadSaveClick(Sender: TObject);
    procedure ButtonShowOptionsClicked(Sender: TObject);
    procedure FileBrowseBtnClick(Sender: TObject);
    procedure InhTreeViewSelectionChanged(Sender: TObject);
    procedure chkCustomConfigFileClick(Sender: TObject);
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    procedure frmCompilerOptionsClose(Sender: TObject;
                                      var CloseAction: TCloseAction);
    procedure grpVerbosityResize(Sender: TObject);
  private
    procedure SetupSearchPathsTab(Page: integer);
    procedure SetupParsingTab(Page: integer);
    procedure SetupCodeGenerationTab(Page: integer);
    procedure SetupLinkingTab(Page: integer);
    procedure SetupMessagesTab(Page: integer);
    procedure SetupOtherTab(Page: integer);
    procedure SetupInheritedTab(Page: integer);
    procedure SetupCompilationTab(Page: integer);
    procedure SetupButtonBar;
  private
    FOnImExportCompilerOptions: TNotifyEvent;
    FOnTest: TNotifyEvent;
    FReadOnly: boolean;
    ImageIndexPackage: integer;
    ImageIndexRequired: integer;
    ImageIndexInherited: integer;
    InheritedChildDatas: TList; // list of PInheritedNodeData
    procedure SetReadOnly(const AValue: boolean);
    procedure UpdateInheritedTab;
    procedure ClearInheritedTree;
  public
    CompilerOpts: TBaseCompilerOptions;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetCompilerOptions;
    procedure GetCompilerOptions(SrcCompilerOptions: TBaseCompilerOptions);
    function PutCompilerOptions(CheckAndWarn: TCheckCompileOptionsMsgLvl): boolean;
    function PutCompilerOptions(CheckAndWarn: TCheckCompileOptionsMsgLvl;
                            DestCompilerOptions: TBaseCompilerOptions): boolean;
  public
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property OnTest: TNotifyEvent read FOnTest write FOnTest;
    property OnImExportCompilerOptions: TNotifyEvent
               read FOnImExportCompilerOptions write FOnImExportCompilerOptions;
  end;


implementation

uses
  IDEImagesIntf;

const
  XMARGIN = 6;
  YMARGIN = 6;

  WCOLABEL  = 90; // the with of the labels on the Compiler tab


type
  TInheritedNodeData = record
    FullText: string;
    Option: TInheritedCompilerOption;
  end;
  PInheritedNodeData = ^TInheritedNodeData;



{------------------------------------------------------------------------------
  TfrmCompilerOptions Constructor
------------------------------------------------------------------------------}
constructor TfrmCompilerOptions.Create(TheOwner: TComponent);
var 
  Page: integer;
begin
  inherited Create(TheOwner);
  Name:='CompilerOptionsDlg';
  Caption := dlgCompilerOptions;

  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,550,450);

  ImageIndexPackage := IDEImages.LoadImage(16, 'pkg_package');
  ImageIndexRequired := IDEImages.LoadImage(16, 'pkg_required');
  ImageIndexInherited := IDEImages.LoadImage(16, 'pkg_inherited');

  DisableAlign;
  try
    nbMain := TNotebook.Create(Self);
    with nbMain do begin
      Name:='MainNotebook';
      Align:=alTop;
      Parent:=Self;
    end;

    // Add the pages
    with nbMain.Pages do begin
      Add(dlgSearchPaths);
      Add(dlgCOParsing);
      Add(dlgCodeGeneration);
      Add(dlgCOLinking);
      Add(dlgCOMessages);
      Add(dlgCOOther);
      Add(dlgCOInherited);
      Add(dlgCOCompilation);
    end;
    nbMain.PageIndex:=0;

    Page:=0;

    SetupSearchPathsTab(Page);
    inc(Page);
    SetupParsingTab(Page);
    inc(Page);
    SetupCodeGenerationTab(Page);
    inc(Page);
    SetupLinkingTab(Page);
    inc(Page);
    SetupMessagesTab(Page);
    inc(Page);
    SetupOtherTab(Page);
    inc(Page);
    SetupInheritedTab(Page);
    inc(Page);
    SetupCompilationTab(Page);
    inc(Page);
    SetupButtonBar;
    nbMain.AnchorToNeighbour(akBottom,6,btnLoadSave);
  finally
    EnableAlign;
  end;

  //TODO: MWE: Are these still needed ?
  // can't we just use a Doxxx portected method ?
  OnClose:=@frmCompilerOptionsClose;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions Destructor                                              }
{------------------------------------------------------------------------------}
destructor TfrmCompilerOptions.Destroy;
begin
  ClearInheritedTree;
  inherited Destroy;
end;

procedure TfrmCompilerOptions.GetCompilerOptions;
begin
  GetCompilerOptions(nil);
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions ButtonOKClicked                                         }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.ButtonOKClicked(Sender: TObject);
begin
  // Accept any changes
  Assert(False, 'Trace:Accept compiler options changes');
  
  { Save the options and hide the dialog }
  if not PutCompilerOptions(ccomlErrors) then exit;
  ModalResult:=mrOk;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions ButtonCheckClicked
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.btnTestClicked(Sender: TObject);
begin
  // Apply any changes and test
  if not PutCompilerOptions(ccomlHints) then exit;
  if Assigned(OnTest) then begin
    btnCheck.Enabled:=false;
    try
      OnTest(CompilerOpts);
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
  if not PutCompilerOptions(ccomlWarning) then exit;
  ShowCompilerOptionsDialog(CompilerOpts);
end;

procedure TfrmCompilerOptions.FileBrowseBtnClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  DefaultFilename: String;
  NewFilename: String;
begin
  if Sender=btnCompiler then
    OpenDialog:=TOpenDialog.Create(Self)
  else
    OpenDialog:=TSelectDirectoryDialog.Create(Self);
  try
    if Sender=btnCompiler then begin
      OpenDialog.Title:=Format(lisBrowseForCompiler, [GetDefaultCompilerFilename
        ]);
      DefaultFilename:=FindDefaultCompilerPath;
      OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
    end else if Sender=btnUnitOutputDir then begin
      OpenDialog.Title:=lisUnitOutputDirectory;
      DefaultFilename:='';
      OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    end else
      exit;
    OpenDialog.Filename:=ExtractFilename(DefaultFilename);
    if DefaultFilename<>'' then
      OpenDialog.InitialDir:=ExtractFilePath(DefaultFilename);
    if OpenDialog.Execute then begin
      NewFilename:=TrimFilename(OpenDialog.Filename);
      if CompilerOpts<>nil then
        NewFilename:=CompilerOpts.ShortenPath(NewFilename,false);
      if Sender=btnCompiler then begin
        edtCompiler.Text:=OpenDialog.Filename;
      end else if Sender=btnUnitOutputDir then begin
        edtUnitOutputDir.Text:=OpenDialog.Filename;
      end;
    end;
  finally
    OpenDialog.Free;
  end;
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
begin
  if Assigned(OnImExportCompilerOptions) then
    OnImExportCompilerOptions(Self);
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions GetCompilerOptions
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.GetCompilerOptions(
  SrcCompilerOptions: TBaseCompilerOptions);
var
  i: integer;
  LCLPlatform: TLCLPlatform;
  EnabledLinkerOpts: Boolean;
  Options: TBaseCompilerOptions;
begin
  if SrcCompilerOptions<>nil then
    Options:=SrcCompilerOptions
  else
    Options:=CompilerOpts;

  EnabledLinkerOpts:=Options.NeedsLinkerOpts;

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

  // parsing
  if (Options.AssemblerStyle in [1,2,3])  then
    grpStyle.ItemIndex:=Options.AssemblerStyle
  else
    grpStyle.ItemIndex:=0;

  chkSymD2Ext.Checked := Options.Delphi2Extensions;
  chkSymCOper.Checked := Options.CStyleOperators;
  chkSymIncludeAssertions.Checked := Options.IncludeAssertionCode;
  chkSymAllowLab.Checked := Options.AllowLabel;
  chkSymCPPInline.Checked := Options.CPPInline;
  chkSymCMacros.Checked := Options.CStyleMacros;
  chkSymTP7Compat.Checked := Options.TPCompatible;
  chkSymConstInit.Checked := Options.InitConstructor;
  chkSymStaticKwd.Checked := Options.StaticKeyword;
  chkSymDelphiCompat.Checked := Options.DelphiCompat;
  chkSymUseAnsiStrings.Checked := Options.UseAnsiStrings;
  chkSymGPCCompat.Checked := Options.GPCCompat;

  // code generation
  chkSmartLinkUnit.Checked := Options.SmartLinkUnit;

  chkChecksIO.Checked := Options.IOChecks;
  chkChecksRange.Checked := Options.RangeChecks;
  chkChecksOverflow.Checked := Options.OverflowChecks;
  chkChecksStack.Checked := Options.StackChecks;

  grpHeapSize.Enabled:=EnabledLinkerOpts;
  edtHeapSize.Text := IntToStr(Options.HeapSize);

  case Options.Generate of
    cgcNormalCode:  radGenNormal.Checked := true;
    cgcFasterCode:  radGenFaster.Checked := true;
    cgcSmallerCode: radGenSmaller.Checked := true;
  end;

  i:=TargetOSComboBox.Items.IndexOf(Options.TargetOS);
  if i<0 then i:=0;  // 0 is default
  TargetOSComboBox.ItemIndex:=i;
  TargetOSComboBox.Text:=Options.TargetOS;
  i:=TargetCPUComboBox.Items.IndexOf(Options.TargetCPU);
  if i<0 then i:=0;  // 0 is default
  TargetCPUComboBox.ItemIndex:=i;
  TargetCPUComboBox.Text:=Options.TargetCPU;

  case Options.TargetProcessor of
    1..3: Targeti386ProcComboBox.ItemIndex:=Options.TargetProcessor;
  else
    Targeti386ProcComboBox.ItemIndex := 0;
  end;

  chkOptVarsInReg.Checked := Options.VariablesInRegisters;
  chkOptUncertain.Checked := Options.UncertainOptimizations;

  case Options.OptimizationLevel of
    1: radOptLevel1.Checked := true;
    2: radOptLevel2.Checked := true;
    3: radOptLevel3.Checked := true;
  else
    radOptLevelNone.Checked := true;
  end;

  // linking
  chkDebugGDB.Checked := Options.GenerateDebugInfo;
  chkDebugDBX.Checked := Options.GenerateDebugDBX;
  chkUseLineInfoUnit.Checked := Options.UseLineInfoUnit;
  chkUseHeaptrc.Checked := Options.UseHeaptrc;
  chkUseValgrind.Checked := Options.UseValgrind;
  chkGenGProfCode.Checked := Options.GenGProfCode;
  chkSymbolsStrip.Checked := Options.StripSymbols;
  chkSymbolsStrip.Enabled:=EnabledLinkerOpts;

  chkLinkSmart.Checked := Options.LinkSmart;
  grpLinkLibraries.Enabled:=EnabledLinkerOpts;

  chkOptionsLinkOpt.Checked := Options.PassLinkerOptions;
  edtOptionsLinkOpt.Text := Options.LinkerOptions;
  chkWin32GraphicApp.Checked := Options.Win32GraphicApp;
  chkWin32GraphicApp.Enabled:=EnabledLinkerOpts;
  grpOptions.Enabled:=EnabledLinkerOpts;

  // messages
  chkErrors.Checked := Options.ShowErrors;
  chkWarnings.Checked := Options.ShowWarn;
  chkNotes.Checked := Options.ShowNotes;
  chkHints.Checked := Options.ShowHints;
  chkGeneralInfo.Checked := Options.ShowGenInfo;
  chkLineNumbers.Checked := Options.ShowLineNum;
  chkEverything.Checked := Options.ShowAll;
  chkAllProcsOnError.Checked := Options.ShowAllProcsOnError;
  chkDebugInfo.Checked := Options.ShowDebugInfo;
  chkUsedFiles.Checked := Options.ShowUsedFiles;
  chkTriedFiles.Checked := Options.ShowTriedFiles;
  chkDefinedMacros.Checked := Options.ShowDefMacros;
  chkCompiledProc.Checked := Options.ShowCompProc;
  chkConditionals.Checked := Options.ShowCond;
  chkExecutableInfo.Checked := Options.ShowExecInfo;
  chkNothing.Checked := Options.ShowNothing;
  chkShowSummary.Checked := Options.ShowSummary;
  chkHintsForUnusedUnitsInMainSrc.Checked :=
                                       Options.ShowHintsForUnusedUnitsInMainSrc;
  chkHintsForSenderNotUsed.Checked := Options.ShowHintsForSenderNotUsed;

  chkFPCLogo.Checked := Options.WriteFPCLogo;

  // other
  chkConfigFile.Checked := not Options.DontUseConfigFile;
  chkCustomConfigFile.Checked := Options.CustomConfigFile;
  edtConfigPath.Enabled := chkCustomConfigFile.Checked;
  edtConfigPath.Text := Options.ConfigFilePath;
  memCustomOptions.Text := Options.CustomOptions;

  edtErrorCnt.Text := IntToStr(Options.StopAfterErrCount);

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
    chkCompilerCompile.AnchorToNeighbour(akLeft,6,lblRunIfCompiler);
    chkCompilerCompile.Checked := crCompile in CompileReasons;
    chkCompilerBuild.Checked := crBuild in CompileReasons;
    chkCompilerRun.Checked := crRun in CompileReasons;
    chkCompilerCompile.Caption := lisCOCallOnCompile;
    chkCompilerCompile.Visible := True;
    chkCompilerBuild.Visible := True;
    chkCompilerRun.Visible := True;
    lblCompiler.AnchorParallel(akLeft,0,lblRunIfCompiler);
  end
  else if Options is TPkgCompilerOptions
  then begin
    lblRunIfCompiler.Visible := False;
    chkCompilerCompile.AnchorParallel(akLeft,6,chkCompilerCompile.Parent);
    chkCompilerCompile.Visible := True;
    chkCompilerCompile.Caption := lisCOSkipCallingCompiler;
    chkCompilerCompile.Checked := TPkgCompilerOptions(Options).SkipCompiler;
    chkCompilerBuild.Visible := False;
    chkCompilerRun.Visible := False;
    lblCompiler.AnchorParallel(akLeft,6,lblCompiler.Parent);
  end
  else begin
    lblRunIfCompiler.Visible := False;
    chkCompilerCompile.Visible := False;
    chkCompilerBuild.Visible := False;
    chkCompilerRun.Visible := False;
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
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions PutCompilerOptions
------------------------------------------------------------------------------}
function TfrmCompilerOptions.PutCompilerOptions(
  CheckAndWarn: TCheckCompileOptionsMsgLvl;
  DestCompilerOptions: TBaseCompilerOptions): boolean;

  function MakeCompileReasons(const ACompile, ABuild, ARun: TCheckBox): TCompileReasons;
  begin
    Result := [];
    if ACompile.Checked then Include(Result, crCompile);
    if ABuild.Checked then Include(Result, crBuild);
    if ARun.Checked then Include(Result, crRun);
  end;
  
  function CheckSearchPath(const Context, ExpandedPath: string): boolean;
  var
    CurPath: String;
    p: Integer;
  begin
    Result:=false;
    if ord(CheckAndWarn)<=ord(ccomlHints) then begin
      if System.Pos('*',ExpandedPath)>0 then begin
        if MessageDlg('Warning','The '+Context+' contains a star * character.'#13
          +'Lazarus uses this as normal character and does not expand them as file mask.',
          mtWarning,[mbOk,mbCancel],0) <> mrOk then exit;
      end;
      p:=0;
      repeat
        DebugLn(['CheckSearchPath ',ExpandedPath,' ',p,' ',length(ExpandedPath)]);
        CurPath:=GetNextDirectoryInSearchPath(ExpandedPath,p);
        if CurPath<>'' then begin
          if not DirPathExistsCached(CurPath) then begin
            if MessageDlg('Warning','The '+Context+' contains a not existing directory:'#13
              +CurPath,
              mtWarning,[mbIgnore,mbCancel],0) <> mrIgnore then exit;
          end;
        end;
      until p>length(ExpandedPath);
    end;
    Result:=true;
  end;

var
  code: LongInt;
  hs: LongInt;
  i: integer;
  OldCompOpts: TBaseCompilerOptions;
  NewTargetOS,
  NewTargetCPU: String;
  Options: TBaseCompilerOptions;
  NewDontUseConfigFile: Boolean;
  NewCustomConfigFile: Boolean;
  NewConfigFilePath: String;
  AdditionalConfig: String;
begin
  Result:=true;
  
  { Put the compiler options into the TCompilerOptions class to be saved }
  if DestCompilerOptions<>nil then
    Options:=DestCompilerOptions
  else
    Options:=CompilerOpts;
  if ReadOnly and (Options=CompilerOpts) then exit;

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

  OldCompOpts := TBaseCompilerOptionsClass(Options.ClassType).Create(nil);
  OldCompOpts.Assign(Options);

  // paths
  Options.IncludePath := edtIncludeFiles.Text;
  if not CheckSearchPath('include search path',Options.GetIncludePath(false)) then
    exit(false);
  Options.Libraries := edtLibraries.Text;
  if not CheckSearchPath('library search path',Options.GetLibraryPath(false)) then
    exit(false);
  Options.OtherUnitFiles := edtOtherUnits.Text;
  if not CheckSearchPath('unit search path',Options.GetUnitPath(false)) then
    exit(false);
  Options.SrcPath := edtOtherSources.Text;
  if not CheckSearchPath('source search path',Options.GetSrcPath(false)) then
    exit(false);
  Options.UnitOutputDirectory := edtUnitOutputDir.Text;
  Options.DebugPath := edtDebugPath.Text;
  if not CheckSearchPath('debugger search path',
    Options.GetParsedPath(pcosDebugPath,icoNone,false))
  then
    exit(false);

  i:=LCLWidgetTypeComboBox.Itemindex;
  if i<=0 then
    Options.LCLWidgetType:=''
  else
    Options.LCLWidgetType:= LCLPlatformDirNames[TLCLPlatform(i-1)];

  // parsing;
  Options.AssemblerStyle := grpStyle.ItemIndex;
  Options.Delphi2Extensions := chkSymD2Ext.Checked;
  Options.CStyleOperators := chkSymCOper.Checked;
  Options.IncludeAssertionCode := chkSymIncludeAssertions.Checked;
  Options.AllowLabel := chkSymAllowLab.Checked;
  Options.CPPInline := chkSymCPPInline.Checked;
  Options.CStyleMacros := chkSymCMacros.Checked;
  Options.TPCompatible := chkSymTP7Compat.Checked;
  Options.InitConstructor := chkSymConstInit.Checked;
  Options.StaticKeyword := chkSymStaticKwd.Checked;
  Options.DelphiCompat := chkSymDelphiCompat.Checked;
  Options.UseAnsiStrings := chkSymUseAnsiStrings.Checked;
  Options.GPCCompat := chkSymGPCCompat.Checked;

  // code generation
  Options.SmartLinkUnit := chkSmartLinkUnit.Checked;

  Options.IOChecks := chkChecksIO.Checked;
  Options.RangeChecks := chkChecksRange.Checked;
  Options.OverflowChecks := chkChecksOverflow.Checked;
  Options.StackChecks := chkChecksStack.Checked;

  Val(edtHeapSize.Text, hs, code);
  if (code <> 0) then
    Options.HeapSize := 0
  else
    Options.HeapSize := hs;

  if (radGenFaster.Checked) then
    Options.Generate := cgcFasterCode
  else if (radGenSmaller.Checked) then
    Options.Generate := cgcSmallerCode
  else
    Options.Generate := cgcNormalCode;

  NewTargetOS:=TargetOSComboBox.Text;
  if TargetOSComboBox.Items.IndexOf(NewTargetOS)<=0 then
    NewTargetOS:='';
  Options.TargetOS:=NewTargetOS;

  NewTargetCPU:=TargetCPUComboBox.Text;
  if TargetCPUComboBox.Items.IndexOf(NewTargetCPU)<=0 then
    NewTargetCPU:='';
  Options.TargetCPU:=NewTargetCPU;

  Options.TargetProcessor := Targeti386ProcComboBox.ItemIndex;
  Options.VariablesInRegisters := chkOptVarsInReg.Checked;
  Options.UncertainOptimizations := chkOptUncertain.Checked;

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
  Options.GenerateDebugDBX := chkDebugDBX.Checked;
  Options.UseLineInfoUnit := chkUseLineInfoUnit.Checked;
  Options.UseHeaptrc := chkUseHeaptrc.Checked;
  Options.UseValgrind := chkUseValgrind.Checked;
  Options.GenGProfCode := chkGenGProfCode.Checked;
  Options.StripSymbols := chkSymbolsStrip.Checked;

  Options.PassLinkerOptions := chkOptionsLinkOpt.Checked;
  Options.LinkerOptions := edtOptionsLinkOpt.Text;
  Options.Win32GraphicApp := chkWin32GraphicApp.Checked;
  Options.LinkSmart := chkLinkSmart.Checked;

  // messages
  Options.ShowErrors := chkErrors.Checked;
  Options.ShowWarn := chkWarnings.Checked;
  Options.ShowNotes := chkNotes.Checked;
  Options.ShowHints := chkHints.Checked;
  Options.ShowGenInfo := chkGeneralInfo.Checked;
  Options.ShowLineNum := chkLineNumbers.Checked;
  Options.ShowAll := chkEverything.Checked;
  Options.ShowAllProcsOnError := chkAllProcsOnError.Checked;
  Options.ShowDebugInfo := chkDebugInfo.Checked;
  Options.ShowUsedFiles := chkUsedFiles.Checked;
  Options.ShowTriedFiles := chkTriedFiles.Checked;
  Options.ShowDefMacros := chkDefinedMacros.Checked;
  Options.ShowCompProc := chkCompiledProc.Checked;
  Options.ShowCond := chkConditionals.Checked;
  Options.ShowExecInfo := chkExecutableInfo.Checked;
  Options.ShowNothing := chkNothing.Checked;
  Options.ShowSummary := chkShowSummary.Checked;
  Options.ShowHintsForUnusedUnitsInMainSrc :=
                                        chkHintsForUnusedUnitsInMainSrc.Checked;
  Options.ShowHintsForSenderNotUsed := chkHintsForSenderNotUsed.Checked;

  Options.WriteFPCLogo := chkFPCLogo.Checked;

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

  // check for change and save
  if not OldCompOpts.IsEqual(Options) then begin
    Options.Modified:=true;
    IncreaseCompilerParseStamp;
  end;
  OldCompOpts.Free;
end;

function TfrmCompilerOptions.PutCompilerOptions(
  CheckAndWarn: TCheckCompileOptionsMsgLvl): boolean;
begin
  Result:=PutCompilerOptions(CheckAndWarn,nil);
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
    AncestorNode:=InhTreeView.Items.Add(nil,'All inherited options');
    AncestorNode.ImageIndex:=ImageIndexInherited;
    AncestorNode.SelectedIndex:=AncestorNode.ImageIndex;
    with CompilerOpts do begin
      AddChildNode('unit path',
        GetInheritedOption(icoUnitPath,true),icoUnitPath);
      AddChildNode('include path',
        GetInheritedOption(icoIncludePath,true),icoIncludePath);
      AddChildNode('object path',
        GetInheritedOption(icoObjectPath,true),icoObjectPath);
      AddChildNode('library path',
        GetInheritedOption(icoLibraryPath,true),icoLibraryPath);
      AddChildNode('linker options',GetInheritedOption(icoLinkerOptions,true),
        icoLinkerOptions);
      AddChildNode('custom options',GetInheritedOption(icoCustomOptions,true),
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
begin
  // Setup the Parsing Tab
  ParsingPage:=nbMain.Page[Page];
  ParsingPage.Name:='ParsingPage';

  grpStyle := TRadioGroup.Create(Self);
  with grpStyle do begin
    Name:='grpStyle';
    Caption := dlgCOStyle+' (-R)';
    with Items do begin
      BeginUpdate;
      Items.Add('Default');
      Items.Add('Intel');
      Items.Add('AT&&T');
      EndUpdate;
    end;
    Columns:=3;
    Parent := ParsingPage;
    AnchorAsAlign(alTop,6);
    Height:=40;
  end;

  grpSyntaxOptions := TGroupBox.Create(Self);
  with grpSyntaxOptions do begin
    Name:='grpSyntaxOptions';
    ChildSizing.SetGridSpacing(6);
    Parent := ParsingPage;
    AnchorToCompanion(akTop,6,grpStyle);
    AutoSize:=true;
    Caption := dlgSyntaxOptions;
  end;

  chkSymD2Ext := TCheckBox.Create(Self);
  with chkSymD2Ext do begin
    Name:='chkSymD2Ext';
    Parent := grpSyntaxOptions;
    Caption := dlgDelphi2Ext+' (-S2)';
  end;

  chkSymCOper := TCheckBox.Create(Self);
  with chkSymCOper do begin
    Name:='chkSymCOper';
    AnchorToNeighbour(akTop,6,chkSymD2Ext);
    Parent := grpSyntaxOptions;
    Caption := dlgCOCOps+' (-Sc)';
  end;

  chkSymIncludeAssertions := TCheckBox.Create(Self);
  with chkSymIncludeAssertions do begin
    Name:='chkSymIncludeAssertions';
    AnchorToNeighbour(akTop,6,chkSymCOper);
    Parent := grpSyntaxOptions;
    Caption := dlgAssertCode+' (-Sa)';
  end;

  chkSymAllowLab := TCheckBox.Create(Self);
  with chkSymAllowLab do begin
    Name:='chkSymAllowLab';
    AnchorToNeighbour(akTop,6,chkSymIncludeAssertions);
    Parent := grpSyntaxOptions;
    Caption := dlgLabelGoto+' (-Sg)';
  end;

  chkSymCPPInline := TCheckBox.Create(Self);
  with chkSymCPPInline do begin
    Name:='chkSymCPPInline';
    AnchorToNeighbour(akTop,6,chkSymAllowLab);
    Parent := grpSyntaxOptions;
    Caption := dlgCppInline+' (-Si)';
  end;

  chkSymCMacros := TCheckBox.Create(Self);
  with chkSymCMacros do begin
    Name:='chkSymCMacros';
    AnchorToNeighbour(akTop,6,chkSymCPPInline);
    Parent := grpSyntaxOptions;
    Caption := dlgCMacro+' (-Sm)';
  end;

  chkSymTP7Compat := TCheckBox.Create(Self);
  with chkSymTP7Compat do begin
    Name:='chkSymTP7Compat';
    AnchorToNeighbour(akTop,6,chkSymCMacros);
    Parent := grpSyntaxOptions;
    Caption := dlgBP7Cptb+' (-So)';
  end;

  chkSymConstInit := TCheckBox.Create(Self);
  with chkSymConstInit do begin
    Name:='chkSymConstInit';
    AnchorToNeighbour(akTop,6,chkSymTP7Compat);
    Parent := grpSyntaxOptions;
    Caption := dlgInitDoneOnly+' (-Ss)';
  end;

  chkSymStaticKwd := TCheckBox.Create(Self);
  with chkSymStaticKwd do begin
    Name:='chkSymStaticKwd';
    AnchorToNeighbour(akTop,6,chkSymConstInit);
    Parent := grpSyntaxOptions;
    Caption := dlgStaticKeyword+' (-St)';
  end;

  chkSymDelphiCompat := TCheckBox.Create(Self);
  with chkSymDelphiCompat do begin
    Name:='chkSymDelphiCompat';
    AnchorToNeighbour(akTop,6,chkSymStaticKwd);
    Parent := grpSyntaxOptions;
    Caption := dlgDeplhiComp+' (-Sd)';
  end;

  chkSymUseAnsiStrings := TCheckBox.Create(Self);
  with chkSymUseAnsiStrings do begin
    Name:='chkSymUseAnsiStrings';
    AnchorToNeighbour(akTop,6,chkSymDelphiCompat);
    Parent := grpSyntaxOptions;
    Caption := dlgCOAnsiStr+' (-Sh)';
  end;

  chkSymGPCCompat := TCheckBox.Create(Self);
  with chkSymGPCCompat do begin
    Name:='chkSymGPCCompat';
    AnchorToNeighbour(akTop,6,chkSymUseAnsiStrings);
    Parent := grpSyntaxOptions;
    Caption := dlgGPCComp+' (-Sp)';
  end;
end;


{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupCodeGenerationTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupCodeGenerationTab(Page: integer);
begin
  // Setup the Code Generation Tab
  CodeGenPage:=nbMain.Page[Page];
  CodeGenPage.Name:='CodePage';

  grpSmartLinkUnit := TGroupBox.Create(Self);
  with grpSmartLinkUnit do begin
    Name:='grpSmartLinkUnit';
    Top := 6;
    Left := 6;
    Caption := dlgCOUnitStyle;
    AutoSize:=true;
    ChildSizing.SetGridSpacing(6);
    Parent := CodeGenPage;
  end;

  chkSmartLinkUnit := TCheckBox.Create(Self);
  with chkSmartLinkUnit do begin
    Name:='chkSmartLinkUnit';
    Caption := dlgCOSmartLinkable + ' (-CX)';
    Parent := grpSmartLinkUnit;
  end;

  {------------------------------------------------------------}
  grpChecks := TGroupBox.Create(Self);
  with grpChecks do begin
    Name:='grpChecks';
    Top := 6;
    AnchorToNeighbour(akLeft,6,grpSmartLinkUnit);
    Caption := dlgCOChecks;
    ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
    ChildSizing.SetGridSpacing(6);
    ChildSizing.ControlsPerLine:=2;
    AutoSize:=true;
    Parent := CodeGenPage;
  end;

  chkChecksIO := TCheckBox.Create(Self);
  with chkChecksIO do begin
    Name:='chkChecksIO';
    Caption := 'I/O (-Ci)';
    Parent := grpChecks;
  end;

  chkChecksRange := TCheckBox.Create(Self);
  with chkChecksRange do begin
    Name:='chkChecksRange';
    Caption := dlgCORange+' (-Cr)';
    Parent := grpChecks;
  end;

  chkChecksOverflow := TCheckBox.Create(Self);
  with chkChecksOverflow do begin
    Name:='chkChecksOverflow';
    Caption := dlgCOOverflow+' (-Co)';
    Parent := grpChecks;
  end;

  chkChecksStack := TCheckBox.Create(Self);
  with chkChecksStack do begin
    Name:='chkChecksStack';
    Caption := dlgCOStack+' (-Ct)';
    Parent := grpChecks;
  end;

  {------------------------------------------------------------}

  grpHeapSize := TGroupBox.Create(Self);
  with grpHeapSize do begin
    Name:='grpHeapSize';
    Top := 6;
    AnchorToNeighbour(akLeft,6,grpChecks);
    Caption := dlgHeapSize +' (-Ch):';
    ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
    ChildSizing.SetGridSpacing(6);
    AutoSize:=true;
    Parent := CodeGenPage;
  end;

  edtHeapSize := TEdit.Create(grpHeapSize);
  with edtHeapSize do begin
    Name:='edtHeapSize';
    Caption := dlgHeapSize;
    Text := '';
    Parent := grpHeapSize;
  end;

  {------------------------------------------------------------}

  grpGenerate := TGroupBox.Create(Self);
  with grpGenerate do begin
    Name:='grpGenerate';
    AnchorToNeighbour(akTop,6,grpChecks);
    Caption := dlgCOGenerate;
    ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    ChildSizing.SetGridSpacing(6);
    AutoSize:=true;
    Parent := CodeGenPage;
  end;

  radGenNormal := TRadioButton.Create(grpGenerate);
  with radGenNormal do begin
    Name:='radGenNormal';
    Caption := dlgCONormal+' (none)';
    Parent := grpGenerate;
  end;

  radGenFaster := TRadioButton.Create(grpGenerate);
  with radGenFaster do begin
    Name:='radGenFaster';
    Caption := dlgCOFast+' (-OG)';
    Parent := grpGenerate;
  end;

  radGenSmaller := TRadioButton.Create(grpGenerate);
  with radGenSmaller do begin
    Name:='radGenSmaller';
    Caption := dlgCOSmaller+' (-Og)';
    Parent := grpGenerate;
  end;


  {------------------------------------------------------------}

  grpTargetPlatform := TGroupBox.Create(Self);
  with grpTargetPlatform do begin
    Name := 'grpTargetPlatform';
    AnchorToNeighbour(akTop,6,grpChecks);
    AnchorToNeighbour(akLeft,6,grpGenerate);
    AnchorParallel(akRight,6,CodeGenPage);
    Caption := dlgTargetPlatform;
    ChildSizing.SetGridSpacing(6);
    AutoSize:=true;
    Parent := CodeGenPage;
  end;

  lblTargetOS := TLabel.Create(Self);
  with lblTargetOS do begin
    Name := 'lblTargetOS';
    Parent := grpTargetPlatform;
    Caption := dlgTargetOS+' (-T)';
  end;

  TargetOSComboBox:=TComboBox.Create(Self);
  with TargetOSComboBox do begin
    Name:='TargetOSComboBox';
    Parent := grpTargetPlatform;
    Left := 100;
    Top := 0;
    Constraints.MinWidth:=100;
    AnchorParallel(akRight,6,Parent);
    AnchorToNeighbour(akLeft,6,lblTargetOS);
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
      //Add('go32v2');
      //Add('os2'); 
      //Add('beos'); 
      //Add('qnx'); 
      //Add('netware'); 
      //Add('wdosx'); 
      //Add('emx'); 
      //Add('watcom'); 
      //Add('netwlibc');
      //Add('amiga');
      //Add('atari');
      //Add('palmos'); 
      //Add('macos'); 
      //Add('morphos'); 
    end;
    ItemIndex:=0;
  end;

  lblTargetOS.AnchorVerticalCenterTo(TargetOSComboBox);

  lblTargetCPU := TLabel.Create(Self);
  with lblTargetCPU do begin
    Name := 'lblTargetCPU';
    Parent := grpTargetPlatform;
    Caption :=dlgTargetCPU+' (-P)';
  end;

  TargetCPUComboBox:=TComboBox.Create(Self);
  with TargetCPUComboBox do begin
    Name:='TargetCPUComboBox';
    Parent := grpTargetPlatform;
    AnchorToCompanion(akTop,1,TargetOSComboBox);
    AutoSize:=true;
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
  lblTargetCPU.AnchorVerticalCenterTo(TargetCPUComboBox);

  lblTargeti386Proc := TLabel.Create(Self);
  with lblTargeti386Proc do begin
    Name := 'lblTargeti386Proc';
    Parent := grpTargetPlatform;
    Caption := dlgTargetProc;
  end;

  Targeti386ProcComboBox:=TComboBox.Create(Self);
  with Targeti386ProcComboBox do begin
    Name:='Targeti386ProcComboBox';
    Parent := grpTargetPlatform;
    AnchorToCompanion(akTop,1,TargetCPUComboBox);
    AutoSize:=true;
    with Items do begin
      Add('('+rsiwpDefault+')');
      Add('386/486 (-Op1)');
      Add('Pentium/Pentium MMX (-Op2)');
      Add('Pentium Pro/Pentium II/C6x86/K6 (-Op3)');
    end;
    ItemIndex:=0;
  end;
  lblTargeti386Proc.AnchorVerticalCenterTo(Targeti386ProcComboBox);

  {------------------------------------------------------------}

  grpOptimizations := TGroupBox.Create(Self);
  with grpOptimizations do begin
    Name:='grpOptimizations';
    AnchorParallel(akLeft,6,Parent);
    AnchorToNeighbour(akTop,6,grpTargetPlatform);
    Caption := dlgOptimiz;
    ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    ChildSizing.SetGridSpacing(6);
    AutoSize:=true;
    Parent := CodeGenPage;
  end;

  radOptLevelNone := TRadioButton.Create(grpOptimizations);
  with radOptLevelNone do begin
    Name:='radOptLevelNone';
    Parent := grpOptimizations;
    Caption :=  dlgLevelNoneOpt+' (none)';
  end;

  radOptLevel1 := TRadioButton.Create(grpOptimizations);
  with radOptLevel1 do begin
    Name:='radOptLevel1';
    Parent := grpOptimizations;
    Caption :=  dlgLevel1Opt+' (-O1)';
  end;

  radOptLevel2 := TRadioButton.Create(grpOptimizations);
  with radOptLevel2 do begin
    Name:='radOptLevel2';
    Parent := grpOptimizations;
    Caption := dlgLevel2Opt+' (-O2)';
  end;

  radOptLevel3 := TRadioButton.Create(grpOptimizations);
  with radOptLevel3 do begin
    Name:='radOptLevel3';
    Parent := grpOptimizations;
    Caption := dlgLevel3Opt+' (-O3)';
  end;

  chkOptVarsInReg := TCheckBox.Create(Self);
  with chkOptVarsInReg do begin
    Name:='chkOptVarsInReg';
    Parent := grpOptimizations;
    Caption := dlgCOKeepVarsReg+' (-Or)';
  end;

  chkOptUncertain := TCheckBox.Create(Self);
  with chkOptUncertain do begin
    Name:='chkOptUncertain';
    Parent := grpOptimizations;
    Caption := dlgUncertOpt+' (-Ou)';
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupLinkingTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupLinkingTab(Page: integer);
begin
  // Setup the Linking Tab
  LinkingPage:=nbMain.Page[Page];
  LinkingPage.Name:='LinkingPage';

  grpDebugging := TGroupBox.Create(Self);
  with grpDebugging do begin
    Name:='grpDebugging';
    AutoSize:=true;
    Caption := dlgCODebugging;
    Parent := LinkingPage;
    ChildSizing.SetGridSpacing(6);
    ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    AnchorAsAlign(alTop,6);
  end;

  chkDebugGDB := TCheckBox.Create(Self);
  with chkDebugGDB do begin
    Name:='chkDebugGDB';
    Caption := dlgCOGDB+' (-g)';
    Parent := grpDebugging;
  end;

  chkDebugDBX := TCheckBox.Create(Self);
  with chkDebugDBX do begin
    Name:='chkDebugDBX';
    Caption := dlgCODBX+' (-gd)';
    Parent := grpDebugging;
  end;

  chkUseLineInfoUnit := TCheckBox.Create(Self);
  with chkUseLineInfoUnit do begin
    Name:='chkUseLineInfoUnit';
    Caption := dlgLNumsBct+' (-gl)';
    Parent := grpDebugging;
  end;

  chkUseHeaptrc := TCheckBox.Create(Self);
  with chkUseHeaptrc do begin
    Name:='chkUseHeaptrc';
    Caption := dlgCOHeaptrc+' (-gh)';
    Parent := grpDebugging;
  end;

  chkUseValgrind := TCheckBox.Create(Self);
  with chkUseValgrind do begin
    Name:='chkUseValgrind';
    Caption := dlgCOValgrind+' (-gv)';
    Parent := grpDebugging;
  end;

  chkGenGProfCode := TCheckBox.Create(Self);
  with chkGenGProfCode do begin
    Name:='chkGenGProfCode';
    Caption := dlgGPROF+' (-pg)';
    Parent := grpDebugging;
  end;

  chkSymbolsStrip := TCheckBox.Create(Self);
  with chkSymbolsStrip do begin
    Name:='chkSymbolsStrip';
    Caption := dlgCOStrip+' (-Xs)';
    Parent := grpDebugging;
  end;

  {------------------------------------------------------------}

  grpLinkLibraries := TGroupBox.Create(Self);
  with grpLinkLibraries do begin
    Name:='grpLinkLibraries';
    Left:=6;
    AnchorToNeighbour(akTop,6,grpDebugging);
    AutoSize:=true;
    Caption := dlgLinkLibraries;
    ChildSizing.SetGridSpacing(6);
    Parent := LinkingPage;
  end;

  chkLinkSmart := TCheckBox.Create(Self);
  with chkLinkSmart do begin
    Name:='chkLinkSmart';
    Caption := dlgLinkSmart+' (-XX)';
    Parent := grpLinkLibraries;
  end;

  {------------------------------------------------------------}

  TargetSpecificsGrpBox := TGroupBox.Create(Self);
  with TargetSpecificsGrpBox do begin
    Name:='TargetSpecificsGrpBox';
    AnchorToNeighbour(akLeft,6,grpLinkLibraries);
    AnchorToNeighbour(akTop,6,grpDebugging);
    AnchorParallel(akRight,6,LinkingPage);
    Caption := lisCOTargetOSSpecificOptions;
    AutoSize:=true;
    ChildSizing.SetGridSpacing(6);
    Parent := LinkingPage;
  end;

  chkWin32GraphicApp := TCheckBox.Create(Self);
  with chkWin32GraphicApp do begin
    Name:='chkWin32GraphicApp';
    Caption := 'Win32 gui application (-WG)';
    Parent := TargetSpecificsGrpBox;
  end;

  {------------------------------------------------------------}

  grpOptions := TGroupBox.Create(Self);
  with grpOptions do begin
    Name:='grpOptions';
    Left:=6;
    AnchorToNeighbour(akTop,6,grpLinkLibraries);
    Caption := dlgCOOpts+' (-k)';
    AutoSize:=true;
    ChildSizing.SetGridSpacing(6);
    ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    Parent := LinkingPage;
  end;

  chkOptionsLinkOpt := TCheckBox.Create(Self);
  with chkOptionsLinkOpt do begin
    Name:='chkOptionsLinkOpt';
    Caption := dlgPassOptsLinker;
    Parent := grpOptions;
  end;

  edtOptionsLinkOpt := TEdit.Create(grpOptions);
  with edtOptionsLinkOpt do begin
    Name:='edtOptionsLinkOpt';
    Text := '';
    Parent := grpOptions;
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupMessagesTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupMessagesTab(Page: integer);
begin
  // Setup the Messages Tab
  MsgPage:=nbMain.Page[Page];
  MsgPage.Name:='MessagesPage';

  grpVerbosity := TGroupBox.Create(Self);
  with grpVerbosity do begin
    Name:='grpVerbosity';
    Left:=6;
    Top:=6;
    AnchorParallel(akRight,6,MsgPage);
    Caption := dlgVerbosity;
    AutoSize:=true;
    ChildSizing.SetGridSpacing(6);
    Parent := MsgPage;
    OnResize:=@grpVerbosityResize;
  end;

  chkErrors := TCheckBox.Create(Self);
  with chkErrors do begin
    Name:='chkErrors';
    Caption := dlgCOShowErr+' (-ve)';
    Parent := grpVerbosity;
  end;

  chkWarnings := TCheckBox.Create(Self);
  with chkWarnings do begin
    Name:='chkWarnings';
    AnchorToNeighbour(akTop,6,chkErrors);
    Caption := dlgShowWarnings+' (-vw)';
    Parent := grpVerbosity;
  end;

  chkNotes := TCheckBox.Create(Self);
  with chkNotes do begin
    Name:='chkNotes';
    Caption := dlgShowNotes+' (-vn)';
    AnchorToNeighbour(akTop,6,chkWarnings);
    Parent := grpVerbosity;
  end;

  chkHints := TCheckBox.Create(Self);
  with chkHints do begin
    Name:='chkHints';
    Caption := dlgShowHint+' (-vh)';
    AnchorToNeighbour(akTop,6,chkNotes);
    Parent := grpVerbosity;
  end;

  chkGeneralInfo := TCheckBox.Create(Self);
  with chkGeneralInfo do begin
    Name:='chkGeneralInfo';
    Caption := dlgShowGeneralInfo+' (-vi)';
    AnchorToNeighbour(akTop,6,chkHints);
    Parent := grpVerbosity;
  end;

  chkLineNumbers := TCheckBox.Create(Self);
  with chkLineNumbers do begin
    Name:='chkLineNumbers';
    Caption := dlgShowCompilingLineNumbers+' (-vl)';
    AnchorToNeighbour(akTop,6,chkGeneralInfo);
    Parent := grpVerbosity;
  end;

  chkAllProcsOnError := TCheckBox.Create(Self);
  with chkAllProcsOnError do begin
    Name:='chkAllProcsOnError';
    Caption := dlgShowProcsError+' (-vb)';
    AnchorToNeighbour(akTop,6,chkLineNumbers);
    Parent := grpVerbosity;
  end;

  chkEverything := TCheckBox.Create(Self);
  with chkEverything do begin
    Name:='chkEverything';
    Caption := dlgShowEverything+' (-va)';
    AnchorToNeighbour(akTop,6,chkAllProcsOnError);
    Parent := grpVerbosity;
  end;

  chkShowSummary := TCheckBox.Create(Self);
  with chkShowSummary do begin
    Name:='chkShowSummary';
    Caption := dlgShowSummary+' (none)';
    AnchorToNeighbour(akTop,6,chkEverything);
    Parent := grpVerbosity;
  end;

  chkHintsForUnusedUnitsInMainSrc := TCheckBox.Create(Self);
  with chkHintsForUnusedUnitsInMainSrc do begin
    Name:='chkHintsForUnusedUnitsInMainSrc';
    Caption := dlgHintsUnused+' (none)';
    AnchorToNeighbour(akTop,6,chkShowSummary);
    Parent := grpVerbosity;
  end;

  chkHintsForSenderNotUsed := TCheckBox.Create(Self);
  with chkHintsForSenderNotUsed do begin
    Name:='chkHintsForSenderNotUsed';
    Caption := dlgHintsParameterSenderNotUsed+' (none)';
    AnchorToNeighbour(akTop,6,chkHintsForUnusedUnitsInMainSrc);
    Parent := grpVerbosity;
  end;

  // right column
  
  chkDebugInfo := TCheckBox.Create(Self);
  with chkDebugInfo do begin
    Name:='chkDebugInfo';
    Caption := dlgShowDebugInfo+' (-vd)';
    Left := (grpVerbosity.ClientWidth div 2)+4;
    Parent := grpVerbosity;
  end;

  chkUsedFiles := TCheckBox.Create(Self);
  with chkUsedFiles do begin
    Name:='chkUsedFiles';
    Caption := dlgShowUsedFiles+' (-vu)';
    AnchorToNeighbour(akTop,6,chkDebugInfo);
    AnchorParallel(akLeft,0,chkDebugInfo);
    Parent := grpVerbosity;
  end;

  chkTriedFiles := TCheckBox.Create(Self);
  with chkTriedFiles do begin
    Name:='chkTriedFiles';
    Caption := dlgShowTriedFiles+' (-vt)';
    AnchorToNeighbour(akTop,6,chkUsedFiles);
    AnchorParallel(akLeft,0,chkDebugInfo);
    Parent := grpVerbosity;
  end;

  chkDefinedMacros := TCheckBox.Create(Self);
  with chkDefinedMacros do begin
    Name:='chkDefinedMacros';
    Caption := dlgShowDefinedMacros+' (-vm)';
    AnchorToNeighbour(akTop,6,chkTriedFiles);
    AnchorParallel(akLeft,0,chkDebugInfo);
    Parent := grpVerbosity;
  end;

  chkCompiledProc := TCheckBox.Create(Self);
  with chkCompiledProc do begin
    Name:='chkCompiledProc';
    Caption := dlgShowCompiledProcedures+' (-vp)';
    AnchorToNeighbour(akTop,6,chkDefinedMacros);
    AnchorParallel(akLeft,0,chkDebugInfo);
    Parent := grpVerbosity;
  end;

  chkConditionals := TCheckBox.Create(Self);
  with chkConditionals do begin
    Name:='chkConditionals';
    Caption := dlgShowConditionals+' (-vc)';
    AnchorToNeighbour(akTop,6,chkCompiledProc);
    AnchorParallel(akLeft,0,chkDebugInfo);
    Parent := grpVerbosity;
  end;

  chkExecutableInfo := TCheckBox.Create(Self);
  with chkExecutableInfo do begin
    Name:='chkExecutableInfo';
    Caption := dlgShowExecutableInfo+' (-vx)';
    AnchorToNeighbour(akTop,6,chkConditionals);
    AnchorParallel(akLeft,0,chkDebugInfo);
    Parent := grpVerbosity;
  end;

  chkNothing := TCheckBox.Create(Self);
  with chkNothing do begin
    Name:='chkNothing';
    Caption := dlgShowNothing+' (-v0)';
    AnchorToNeighbour(akTop,6,chkExecutableInfo);
    AnchorParallel(akLeft,0,chkDebugInfo);
    Parent := grpVerbosity;
  end;

  chkFPCLogo := TCheckBox.Create(Self);
  with chkFPCLogo do begin
    Name:='chkFPCLogo';
    Caption := dlgWriteFPCLogo+' (-l)';
    AnchorToNeighbour(akTop,6,chkNothing);
    AnchorParallel(akLeft,0,chkDebugInfo);
    Parent := grpVerbosity;
  end;

  {------------------------------------------------------------}
  grpErrorCnt := TGroupBox.Create(Self);
  with grpErrorCnt do begin
    Name:='grpErrorCnt';
    Left:=6;
    AnchorToNeighbour(akTop,6,grpVerbosity);
    AutoSize:=true;
    Caption := dlgStopAfterNrErr+' (-Se)';
    ChildSizing.SetGridSpacing(6);
    Parent := MsgPage;
  end;

  edtErrorCnt := TEdit.Create(grpErrorCnt);
  with edtErrorCnt do begin
    Name:='edtErrorCnt';
    Text := '';
    Constraints.MinWidth:=100;
    Parent := grpErrorCnt;
    AnchorAsAlign(alTop,0);
  end;
end;

procedure TfrmCompilerOptions.SetupOtherTab(Page: integer);
begin
  OtherPage:=nbMain.Page[Page];
  OtherPage.Name:='OtherPage';

  grpConfigFile := TGroupBox.Create(Self);
  with grpConfigFile do begin
    Name:='grpConfigFile';
    ChildSizing.SetGridSpacing(6);
    AutoSize:=true;
    Caption := dlgConfigFiles;
    Parent := OtherPage;
    AnchorAsAlign(alTop,6);
  end;

  chkConfigFile := TCheckBox.Create(Self);
  with chkConfigFile do begin
    Name:='chkConfigFile';
    Caption := dlgUseFpcCfg+' (If not checked: -n)';
    Left:=6;
    Parent := grpConfigFile;
  end;

  chkCustomConfigFile := TCheckBox.Create(Self);
  with chkCustomConfigFile do begin
    Name:='chkCustomConfigFile';
    Caption := dlgUseCustomConfig+' (@)';
    AnchorToNeighbour(akTop,6,chkConfigFile);
    Left:=6;
    Parent := grpConfigFile;
    OnClick:=@chkCustomConfigFileClick;
  end;

  edtConfigPath := TEdit.Create(grpConfigFile);
  with edtConfigPath do begin
    Name:='edtConfigPath';
    Constraints.MinWidth:=200;
    Left:=6;
    AnchorToNeighbour(akTop,6,chkCustomConfigFile);
    Text := '';
    AnchorParallel(akRight,6,grpConfigFile);
    AutoSize:=true;
    Parent := grpConfigFile;
  end;

  grpCustomOptions := TGroupBox.Create(Self);
  with grpCustomOptions do begin
    Name:='grpCustomOptions';
    AnchorToCompanion(akTop,6,grpConfigFile);
    Caption:=lisCustomOptions2;
    Height:=200;
    Parent := OtherPage;
  end;

  memCustomOptions := TMemo.Create(Self);
  with memCustomOptions do begin
    Name:='memCustomOptions';
    Parent:=grpCustomOptions;
    Align:=alClient;
  end;
end;

{------------------------------------------------------------------------------
  procedure TfrmCompilerOptions.SetupInheritedTab(Page: integer);
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupInheritedTab(Page: integer);
begin
  InheritedPage:=nbMain.Page[Page];
  InheritedPage.Name:='InheritedPage';

  InhNoteLabel:=TLabel.Create(Self);
  with InhNoteLabel do begin
    Name:='InhNoteLabel';
    Caption:=lisAdditionalCompilerOptionsInheritedFromPackages;
    Parent:=InheritedPage;
  end;

  InhTreeView:=TTreeView.Create(Self);
  with InhTreeView do begin
    Name:='InhTreeView';
    Options:=Options+[tvoReadOnly, tvoRightClickSelect, tvoShowRoot,
                      tvoKeepCollapsedNodes];
    Images := IDEImages.Images_16;
    AnchorToNeighbour(akTop,6,InhNoteLabel);
    AnchorParallel(akLeft,0,InheritedPage);
    AnchorParallel(akRight,0,InheritedPage);
    Parent:=InheritedPage;
    OnSelectionChanged:=@InhTreeViewSelectionChanged;
  end;

  InhItemMemo:=TMemo.Create(Self);
  with InhItemMemo do begin
    Name:='InhItemMemo';
    ReadOnly:=true;
    WordWrap:=true;
    ScrollBars:=ssAutoVertical;
    Text:=lisSelectANode;
    Anchors:=[akLeft,akRight,akBottom];
    Height:=100;
    Parent:=InheritedPage;
    AnchorAsAlign(alBottom,0);
  end;
  InhTreeView.AnchorToNeighbour(akBottom,0,InhItemMemo);
end;

procedure TfrmCompilerOptions.SetupCompilationTab(Page: integer);
// the real edit width is calculated in the groupbox resize
// here the controls are given initial sizes to avoid jumpy controls

  procedure CreateGroup(AGroupBox: TGroupBox;
    var lblRunIf: TLabel;
    var chkCompile: TCheckBox;
    var chkBuild: TCheckBox;
    var chkRun: TCheckBox;
    var lblCommand: TLabel;
    var edtCommand: TEdit;
    WithScanFlags: boolean;
    var chkScanFPC: TCheckBox;
    var chkScanMake: TCheckBox;
    var chkShowAll: TCheckBox);
  begin
    lblRunIf := TLabel.Create(Self);
    with lblRunIf do begin
      Caption := lisCOCallOn;
      Parent:=AGroupBox;
    end;

    chkCompile := TCheckBox.Create(Self);
    with chkCompile do begin
      Caption := lisCOCallOnCompile;
      AnchorToNeighbour(akLeft,6,lblRunIf);
      Constraints.MinWidth:=WCOLABEL;
      Parent:=AGroupBox;
    end;
    lblRunIf.AnchorVerticalCenterTo(chkCompile);

    chkBuild := TCheckBox.Create(Self);
    with chkBuild do begin
      Caption := lisCOCallOnBuild;
      AnchorToCompanion(akLeft,6,chkCompile);
      Constraints.MinWidth:=WCOLABEL;
      Parent:=AGroupBox;
    end;

    chkRun := TCheckBox.Create(Self);
    with chkRun do begin
      Caption := lisCOCallOnRun;
      AnchorToCompanion(akLeft,6,chkBuild);
      Constraints.MinWidth:=WCOLABEL;
      Parent:=AGroupBox;
    end;
    
    lblCommand := TLabel.Create(Self);
    with lblCommand do begin
      Caption:=lisCOCommand;
      Parent:=AGroupBox;
    end;
    
    edtCommand := TEdit.Create(Self);
    with edtCommand do begin
      AnchorToNeighbour(akTop,6,chkCompile);
      AnchorToNeighbour(akLeft,6,lblCommand);
      AnchorParallel(akRight,6,AGroupBox);
      Text:='';
      Parent:=AGroupBox;
    end;
    lblCommand.AnchorVerticalCenterTo(edtCommand);

    if WithScanFlags then begin
      chkScanFPC := TCheckBox.Create(Self);
      with chkScanFPC do begin
        Caption := lisCOScanForFPCMessages;
        AnchorToNeighbour(akTop,6,edtCommand);
        Constraints.MinWidth:=WCOLABEL;
        Parent:=AGroupBox;
      end;

      chkScanMake := TCheckBox.Create(Self);
      with chkScanMake do begin
        Caption := lisCOScanForMakeMessages;
        AnchorToNeighbour(akTop,6,chkScanFPC);
        Constraints.MinWidth:=WCOLABEL;
        Parent:=AGroupBox;
      end;

      chkShowAll := TCheckBox.Create(Self);
      with chkShowAll do begin
        Caption := lisCOShowAllMessages;
        AnchorToCompanion(akLeft,15,chkScanFPC);
        Constraints.MinWidth:=WCOLABEL;
        Parent:=AGroupBox;
      end;
    end;
  end;

begin
  CompilationPage := nbMain.Page[Page];
  CompilationPage.Name:='CompilationPage';

  chkCreateMakefile:=TCheckBox.Create(Self);
  with chkCreateMakefile do begin
    Name := 'chkCreateMakefile';
    Caption := 'Create Makefile';
    Left:=6;
    Top:=6;
    Parent := CompilationPage;
  end;

  {------------------------------------------------------------}

  ExecuteBeforeGroupBox:=TGroupBox.Create(Self);
  with ExecuteBeforeGroupBox do begin
    Name := 'ExecuteBeforeGroupBox';
    Left:=6;
    AnchorToNeighbour(akTop,6,chkCreateMakefile);
    AnchorParallel(akRight,6,CompilationPage);
    Caption := lisCOExecuteBefore;
    ChildSizing.SetGridSpacing(6);
    AutoSize:=true;
    Parent := CompilationPage;
  end;

  CreateGroup(ExecuteBeforeGroupBox,
    lblRunIfExecBefore,chkExecBeforeCompile,chkExecBeforeBuild,chkExecBeforeRun,
    ExecuteBeforeCommandLabel,ExecuteBeforeCommandEdit,
    true,
    ExecuteBeforeScanFPCCheckBox,ExecuteBeforeScanMakeCheckBox,
    ExecuteBeforeShowAllCheckBox
  );

  {------------------------------------------------------------}

  grpCompiler := TGroupBox.Create(Self);
  with grpCompiler do begin
    Name := 'grpCompiler';
    AnchorToCompanion(akTop,6,ExecuteBeforeGroupBox);
    Caption := lisCompiler;
    ChildSizing.SetGridSpacing(6);
    AutoSize:=true;
    Parent := CompilationPage;
  end;

  CreateGroup(grpCompiler,
    lblRunIfCompiler,chkCompilerCompile,chkCompilerBuild,chkCompilerRun,
    lblCompiler,edtCompiler,
    false,
    ExecuteBeforeScanFPCCheckBox,ExecuteBeforeScanMakeCheckBox,
    ExecuteBeforeShowAllCheckBox
  );
  chkCompilerCompile.Checked := True;
  chkCompilerBuild.Checked := True;
  chkCompilerRun.Checked := True;

  {------------------------------------------------------------}

  ExecuteAfterGroupBox:=TGroupBox.Create(Self);
  with ExecuteAfterGroupBox do begin
    Name := 'ExecuteAfterGroupBox';
    Left:=6;
    AnchorToCompanion(akTop,6,grpCompiler);
    Caption := lisCOExecuteAfter;
    ChildSizing.SetGridSpacing(6);
    AutoSize:=true;
    Parent := CompilationPage;
  end;

  CreateGroup(ExecuteAfterGroupBox,
    lblRunIfExecAfter,chkExecAfterCompile,chkExecAfterBuild,chkExecAfterRun,
    ExecuteAfterCommandLabel,ExecuteAfterCommandEdit,
    true,
    ExecuteAfterScanFPCCheckBox,ExecuteAfterScanMakeCheckBox,
    ExecuteAfterShowAllCheckBox
  );
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupSearchPathsTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupSearchPathsTab(Page: integer);
var
  LCLInterface: TLCLPlatform;
begin
  // Setup the Search Paths Tab
  PathPage:=nbMain.Page[Page];
  PathPage.Name:='PathsPage';
  PathPage.ChildSizing.SetGridSpacing(6);

  lblOtherUnits := TLabel.Create(Self);
  with lblOtherUnits do begin
    Name:='lblOtherUnits';
    Parent := PathPage;
    Left:=6;
    Top:=6;
    Caption := dlgOtherUnitFiles;
  end;

  edtOtherUnits := TEdit.Create(Self);
  with edtOtherUnits do begin
    Name:='edtOtherUnits';
    Text := '';
    Left:=6;
    AnchorToNeighbour(akTop,6,lblOtherUnits);
    Constraints.MinWidth:=200;
    AutoSize:=true;
    Parent := PathPage;
  end;

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
  edtOtherUnits.AnchorToNeighbour(akRight,2,OtherUnitsPathEditBtn);

  {------------------------------------------------------------}

  lblIncludeFiles := TLabel.Create(Self);
  with lblIncludeFiles do begin
    Name:='grpIncludeFiles';
    Left:=6;
    AnchorToNeighbour(akTop,10,edtOtherUnits);
    Caption := dlgCOIncFiles;
    Parent := PathPage;
  end;

  edtIncludeFiles := TEdit.Create(Self);
  with edtIncludeFiles do begin
    Name:='edtIncludeFiles';
    Text := '';
    Left:=6;
    AnchorToNeighbour(akTop,2,lblIncludeFiles);
    Constraints.MinWidth:=200;
    AutoSize:=true;
    Parent := PathPage;
  end;

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
  edtIncludeFiles.AnchorToNeighbour(akRight,2,IncludeFilesPathEditBtn);

  {------------------------------------------------------------}

  lblOtherSources := TLabel.Create(Self);
  with lblOtherSources do begin
    Name:='lblOtherSources';
    Left:=6;
    AnchorToNeighbour(akTop,10,edtIncludeFiles);
    Caption := dlgCOSources;
    Parent := PathPage;
  end;

  edtOtherSources := TEdit.Create(Self);
  with edtOtherSources do begin
    Name:='edtOtherSources';
    Text := '';
    Left:=6;
    AnchorToNeighbour(akTop,2,lblOtherSources);
    Constraints.MinWidth:=200;
    AutoSize:=true;
    Parent := PathPage;
  end;

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
  edtOtherSources.AnchorToNeighbour(akRight,2,OtherSourcesPathEditBtn);

  {------------------------------------------------------------}

  lblLibraries := TLabel.Create(Self);
  with lblLibraries do begin
    Name:='lblLibraries';
    Left:=6;
    AnchorToNeighbour(akTop,10,edtOtherSources);
    Caption := dlgCOLibraries;
    Parent := PathPage;
  end;

  edtLibraries := TEdit.Create(Self);
  with edtLibraries do begin
    Name:='edtLibraries';
    Text := '';
    Constraints.MinWidth:=200;
    Left:=6;
    AnchorToNeighbour(akTop,2,lblLibraries);
    AutoSize:=true;
    Parent := PathPage;
  end;

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
  edtLibraries.AnchorToNeighbour(akRight,2,LibrariesPathEditBtn);

  {------------------------------------------------------------}

  lblUnitOutputDir := TLabel.Create(Self);
  with lblUnitOutputDir do begin
    Name:='lblUnitOutputDir';
    Left:=6;
    AnchorToNeighbour(akTop,10,edtLibraries);
    Caption := dlgUnitOutp;
    Parent := PathPage;
  end;

  edtUnitOutputDir := TEdit.Create(Self);
  with edtUnitOutputDir do begin
    Name:='edtUnitOutputDir';
    Text := '';
    Left:=6;
    AnchorToNeighbour(akTop,2,lblUnitOutputDir);
    Constraints.MinWidth:=200;
    AutoSize:=true;
    Parent := PathPage;
  end;

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
  edtUnitOutputDir.AnchorToNeighbour(akRight,2,btnUnitOutputDir);

  {------------------------------------------------------------}

  lblDebugPath := TLabel.Create(Self);
  with lblDebugPath do begin
    Name:='lblDebugPath';
    Left:=6;
    AnchorToNeighbour(akTop,10,edtUnitOutputDir);
    Caption := dlgCODebugPath;
    Parent := PathPage;
  end;

  edtDebugPath := TEdit.Create(Self);
  with edtDebugPath do begin
    Name:='edtDebugPath';
    Text := '';
    Left:=6;
    AnchorToNeighbour(akTop,2,lblDebugPath);
    Constraints.MinWidth:=200;
    AutoSize:=true;
    Parent := PathPage;
  end;

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
  edtDebugPath.AnchorToNeighbour(akRight,2,DebugPathEditBtn);

  {------------------------------------------------------------}

  LCLWidgetTypeLabel:=TLabel.Create(Self);
  with LCLWidgetTypeLabel do begin
    Name:='LCLWidgetTypeLabel';
    Left:=6;
    Caption:=Format(lisCOVarious, [lisLCLWidgetType]);
    Parent := PathPage;
  end;

  LCLWidgetTypeComboBox:=TComboBox.Create(Self);
  with LCLWidgetTypeComboBox do begin
    Name:='LCLWidgetTypeComboBox';
    AnchorToNeighbour(akLeft,6,LCLWidgetTypeLabel);
    AnchorToNeighbour(akTop,10,edtDebugPath);
    with Items do begin
      BeginUpdate;
      Add(Format(lisCOdefault,
                   [LCLPlatformDisplayNames[GetDefaultLCLWidgetType]]));
      for LCLInterface:=Low(TLCLPlatform) to High(TLCLPlatform) do begin
        Items.Add(LCLPlatformDisplayNames[LCLInterface]);
      end;
      EndUpdate;
    end;
    ItemIndex:=1;
    Constraints.MinWidth:=150;
    AutoSize:=true;
    Parent := PathPage;
  end;
  LCLWidgetTypeLabel.AnchorVerticalCenterTo(LCLWidgetTypeComboBox);
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupButtonBar
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupButtonBar;
begin
  // Setup the Button Bar
  btnOK := TButton.Create(Self);
  with btnOK do begin
    Caption := lisOkBtn;
    AutoSize:=true;
    OnClick := @ButtonOKClicked;
    Constraints.MinWidth:=60;
    Parent := Self;
  end;

  btnCancel := TButton.Create(Self);
  with btnCancel do begin
    Caption := dlgCancel;
    AutoSize:=true;
    ModalResult := mrCancel;
    Constraints.MinWidth:=60;
    Parent := Self;
  end;
  CancelControl:=btnCancel;

  btnShowOptions := TButton.Create(Self);
  with btnShowOptions do begin
    Caption := dlgCOShowOptions;
    AutoSize:=true;
    OnClick := @ButtonShowOptionsClicked;
    Constraints.MinWidth:=60;
    Parent := Self;
  end;

  btnCheck := TButton.Create(Self);
  with btnCheck do begin
    Caption := lisCompTest;
    AutoSize:=true;
    OnClick := @btnTestClicked;
    Constraints.MinWidth:=60;
    Parent := Self;
  end;

  btnLoadSave := TButton.Create(Self);
  with btnLoadSave do begin
    Caption := dlgCOLoadSave;
    AutoSize:=true;
    OnClick := @ButtonLoadSaveClick;
    Constraints.MinWidth:=60;
    Parent := Self;
  end;
  
  btnLoadSave.Anchors:=[akRight,akBottom];
  btnLoadSave.AnchorParallel(akRight,6,Self);
  btnLoadSave.AnchorParallel(akBottom,6,Self);
  btnCheck.AnchorToCompanion(akRight,10,btnLoadSave);
  btnShowOptions.AnchorToCompanion(akRight,10,btnCheck);
  btnCancel.AnchorToCompanion(akRight,10,btnShowOptions);
  btnOK.AnchorToCompanion(akRight,10,btnCancel);
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
      Templates:='include';
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
    AButton.CurrentPathEditor.Path:=OldPath;
    AButton.CurrentPathEditor.Templates:=SetDirSeparators(Templates);
  end;
end;

procedure TfrmCompilerOptions.PathEditBtnExecuted(Sender: TObject);
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
      edtOtherUnits.Text:=NewPath;
    end else
    if AButton=IncludeFilesPathEditBtn then begin
      edtIncludeFiles.Text:=NewPath;
    end else
    if AButton=OtherSourcesPathEditBtn then begin
      edtOtherSources.Text:=NewPath;
    end else
    if AButton=LibrariesPathEditBtn then begin
      edtLibraries.Text:=NewPath;
    end else
    if AButton=DebugPathEditBtn then begin
      edtDebugPath.Text:=NewPath;
    end;
  end;
end;

procedure TfrmCompilerOptions.frmCompilerOptionsClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TfrmCompilerOptions.grpVerbosityResize(Sender: TObject);
begin
  chkDebugInfo.Left := (grpVerbosity.ClientWidth div 2)+4;
end;

procedure TfrmCompilerOptions.SetReadOnly(const AValue: boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
  btnOk.Enabled:=not FReadOnly;
  btnCheck.Enabled:=not FReadOnly;
end;

end.
