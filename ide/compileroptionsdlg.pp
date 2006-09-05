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
  Forms, Classes, LCLProc, SysUtils, ComCtrls, Buttons, StdCtrls, ExtCtrls,
  Graphics, LResources, FileUtil, Dialogs, Controls, GraphType,
  ProjectIntf, IDEWindowIntf,
  PathEditorDlg, LazarusIDEStrConsts, IDEOptionDefs, LazConf, IDEProcs,
  CompilerOptions, ShowCompilerOpts, Project, PackageDefs;

type
  { Compiler options form }

  TfrmCompilerOptions = class(TForm)
    nbMain: TNotebook;
    ImageList: TImageList;

    { Search Paths Controls }
    PathPage: TPage;
    grpOtherUnits: TGroupBox;
    edtOtherUnits: TEdit;
    OtherUnitsPathEditBtn: TPathEditorButton;

    grpIncludeFiles: TGroupBox;
    edtIncludeFiles: TEdit;
    IncludeFilesPathEditBtn: TPathEditorButton;

    grpOtherSources: TGroupBox;
    edtOtherSources: TEdit;
    OtherSourcesPathEditBtn: TPathEditorButton;

    grpLibraries: TGroupBox;
    edtLibraries: TEdit;
    LibrariesPathEditBtn: TPathEditorButton;

    grpUnitOutputDir: TGroupBox;
    edtUnitOutputDir: TEdit;
    btnUnitOutputDir: TButton;

    grpDebugPath: TGroupBox;
    edtDebugPath: TEdit;
    DebugPathEditBtn: TPathEditorButton;

    LCLWidgetTypeGroupBox: TGroupBox;
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
    edtCompiler: TEdit;
    btnCompiler: TButton;
    lblCompiler: TLabel;

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
    procedure ExecuteAfterGroupBoxResize(Sender: TObject);
    procedure ExecuteBeforeGroupBoxResize(Sender: TObject);
    procedure grpCompilerResize(Sender: TObject);
    procedure FileBrowseBtnClick(Sender: TObject);
    procedure InhTreeViewSelectionChanged(Sender: TObject);
    procedure InheritedPageResize(Sender: TObject);
    procedure chkCustomConfigFileClick(Sender: TObject);
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    procedure frmCompilerOptionsClose(Sender: TObject;
                                       var CloseAction: TCloseAction);
    procedure frmCompilerOptionsResize(Sender: TObject);
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
    function PutCompilerOptions(CheckAndWarn: boolean): boolean;
    function PutCompilerOptions(CheckAndWarn: boolean;
                            DestCompilerOptions: TBaseCompilerOptions): boolean;
  public
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property OnTest: TNotifyEvent read FOnTest write FOnTest;
    property OnImExportCompilerOptions: TNotifyEvent
               read FOnImExportCompilerOptions write FOnImExportCompilerOptions;
  end;


implementation

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



{------------------------------------------------------------------------------}
{  TfrmCompilerOptions Constructor                                             }
{------------------------------------------------------------------------------}
constructor TfrmCompilerOptions.Create(TheOwner: TComponent);

  procedure AddResImg(const ResName: string);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    Pixmap.TransparentColor:=clWhite;
    Pixmap.LoadFromLazarusResource(ResName);
    ImageList.AddDirect(Pixmap,nil)
  end;

var Page: integer;
begin
  inherited Create(TheOwner);
  Name:='CompilerOptionsDlg';
  Caption := dlgCompilerOptions;

  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,550,450);

  ImageList:=TImageList.Create(Self);
  with ImageList do begin
    Width:=17;
    Height:=17;
    Name:='ImageList';
    ImageIndexPackage:=Count;
    AddResImg('pkg_package');
    ImageIndexRequired:=Count;
    AddResImg('pkg_required');
    ImageIndexInherited:=Count;
    AddResImg('pkg_inherited');
  end;

  nbMain := TNotebook.Create(Self);
  nbMain.Name:='MainNotebook';
  nbMain.Parent := Self;
  nbMain.Height := Height - 50;
  nbMain.Width := Width - 4;
  nbMain.Top := 0;
  nbMain.Left := 0;

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

  DisableAlign;
  try
    { Search Paths Tab }
    SetupSearchPathsTab(Page);
    inc(Page);

    { Parsing Tab }
    SetupParsingTab(Page);
    inc(Page);

    { Code Generation Tab }
    SetupCodeGenerationTab(Page);
    inc(Page);

    { Linking Tab }
    SetupLinkingTab(Page);
    inc(Page);

    { Messages Tab }
    SetupMessagesTab(Page);
    inc(Page);

    { Other Tab }
    SetupOtherTab(Page);
    inc(Page);

    { Inherited Tab }
    SetupInheritedTab(Page);
    inc(Page);

    { Compilation Tab }
    SetupCompilationTab(Page);
    inc(Page);

    { Bottom Buttons }
    SetupButtonBar;
  finally
    EnableAlign;
  end;

  //TODO: MWE: Are these still needed ?
  // can't we just use a Doxxx portected method ?
  OnResize:=@frmCompilerOptionsResize;
  OnResize(Self);
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
  if not PutCompilerOptions(true) then exit;
  ModalResult:=mrOk;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions ButtonCheckClicked
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.btnTestClicked(Sender: TObject);
begin
  // Apply any changes and test
  PutCompilerOptions(true);
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
  PutCompilerOptions(true);
  ShowCompilerOptionsDialog(CompilerOpts);
end;

procedure TfrmCompilerOptions.ExecuteAfterGroupBoxResize(Sender: TObject);
var
  m, w: Integer;
begin
  w := ExecuteAfterGroupBox.ClientWidth;
  m := w div 2;

  with ExecuteAfterCommandEdit do
    Width := w - Left - XMARGIN;

  chkExecAfterBuild.Left := (chkExecAfterCompile.Left + m) div 2;
  chkExecAfterRun.Left := m;

end;

procedure TfrmCompilerOptions.ExecuteBeforeGroupBoxResize(Sender: TObject);
var
  m, w: Integer;
begin
  w := ExecuteBeforeGroupBox.ClientWidth;
  m := w div 2;

  with ExecuteBeforeCommandEdit do
    Width := w - Left - XMARGIN;

  chkExecBeforeBuild.Left := (chkExecBeforeCompile.Left + m) div 2;
  chkExecBeforeRun.Left := m;
end;

procedure TfrmCompilerOptions.grpCompilerResize(Sender: TObject);
var
  m, w: Integer;
begin
  w := ExecuteBeforeGroupBox.ClientWidth;
  m := w div 2;

  btnCompiler.Left := w - btnCompiler.Width - XMARGIN;
  edtCompiler.Width := btnCompiler.Left - edtCompiler.Left - 2;

  chkCompilerBuild.Left := (chkCompilerCompile.Left + m) div 2;
  chkCompilerRun.Left := m;
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

{------------------------------------------------------------------------------
  procedure TfrmCompilerOptions.InheritedPageResize(Sender: TObject);
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.InheritedPageResize(Sender: TObject);
var
  y: Integer;
begin
  InhNoteLabel.SetBounds(3,3,InheritedPage.ClientWidth-6,20);
  InhTreeView.SetBounds(0,25,
                      InheritedPage.ClientWidth,InheritedPage.ClientHeight-100);
  y:=InhTreeView.Top+InhTreeView.Height;
  InhItemMemo.SetBounds(0,y,
                        InheritedPage.ClientWidth,InheritedPage.ClientHeight-y);
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
  grpLibraries.Enabled:=EnabledLinkerOpts;
  edtOtherSources.Text := Options.SrcPath;
  edtUnitOutputDir.Text := Options.UnitOutputDirectory;
  edtDebugPath.Text := Options.DebugPath;

  i:=LCLWidgetTypeComboBox.Items.IndexOf(Options.LCLWidgetType);
  if i<0 then i:=0;
  LCLWidgetTypeComboBox.ItemIndex:=i;

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
    chkCompilerCompile.Checked := crCompile in CompileReasons;
    chkCompilerBuild.Checked := crBuild in CompileReasons;
    chkCompilerRun.Checked := crRun in CompileReasons;
    lblRunIfCompiler.Visible := True;
    chkCompilerCompile.Visible := True;
    chkCompilerCompile.Caption := lisCOCallOnCompile;
    chkCompilerCompile.Width := WCOLABEL;
    chkCompilerBuild.Visible := True;
    chkCompilerRun.Visible := True;
  end
  else if Options is TPkgCompilerOptions
  then begin
    lblRunIfCompiler.Visible := False;
    chkCompilerCompile.Visible := True;
    chkCompilerCompile.Caption := lisCOSkipCallingCompiler;
    chkCompilerCompile.Width := 2 * WCOLABEL;
    chkCompilerCompile.Checked := TPkgCompilerOptions(Options).SkipCompiler;
    chkCompilerBuild.Visible := False;
    chkCompilerRun.Visible := False;
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

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions PutCompilerOptions                                      }
{------------------------------------------------------------------------------}
function TfrmCompilerOptions.PutCompilerOptions(CheckAndWarn: boolean;
  DestCompilerOptions: TBaseCompilerOptions): boolean;

  function MakeCompileReasons(const ACompile, ABuild, ARun: TCheckBox): TCompileReasons;
  begin
    Result := [];
    if ACompile.Checked then Include(Result, crCompile);
    if ABuild.Checked then Include(Result, crBuild);
    if ARun.Checked then Include(Result, crRun);
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

  if CheckAndWarn then begin
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
  Options.Libraries := edtLibraries.Text;
  Options.OtherUnitFiles := edtOtherUnits.Text;
  Options.SrcPath := edtOtherSources.Text;
  Options.UnitOutputDirectory := edtUnitOutputDir.Text;
  Options.DebugPath := edtDebugPath.Text;

  i:=LCLWidgetTypeComboBox.Itemindex;
  if i<=0 then
    Options.LCLWidgetType:=''
  else
    Options.LCLWidgetType:= LCLWidgetTypeComboBox.Items[i];

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

function TfrmCompilerOptions.PutCompilerOptions(CheckAndWarn: boolean): boolean;
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
var
  y: Integer;
  yDiff: Integer;
begin
  // Setup the Parsing Tab
  ParsingPage:=nbMain.Page[Page];
  ParsingPage.Name:='ParsingPage';

  grpStyle := TRadioGroup.Create(Self);
  with grpStyle do
  begin
    Parent := ParsingPage;
    Top := 5;
    Left := 5;
    Height := 42;
    Width := 350;
    Caption := dlgCOStyle+' (-R)';
    with Items do begin
      BeginUpdate;
      Items.Add('Default');
      Items.Add('Intel');
      Items.Add('AT&&T');
      EndUpdate;
    end;
    Columns:=3;
  end;

  yDiff:=22;

  grpSyntaxOptions := TGroupBox.Create(Self);
  with grpSyntaxOptions do
  begin
    Parent := ParsingPage;
    Top := grpStyle.Top+grpStyle.Height+5;
    Left := grpStyle.Left;
    Height := 25+12*yDiff;
    Width := Self.ClientWidth-28;
    Caption := dlgSyntaxOptions;
  end;

  y:=2;

  chkSymD2Ext := TCheckBox.Create(Self);
  with chkSymD2Ext do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgDelphi2Ext+' (-S2)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymCOper := TCheckBox.Create(Self);
  with chkSymCOper do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgCOCOps+' (-Sc)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymIncludeAssertions := TCheckBox.Create(Self);
  with chkSymIncludeAssertions do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgAssertCode+' (-Sa)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymAllowLab := TCheckBox.Create(Self);
  with chkSymAllowLab do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgLabelGoto+' (-Sg)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymCPPInline := TCheckBox.Create(Self);
  with chkSymCPPInline do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgCppInline+' (-Si)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymCMacros := TCheckBox.Create(Self);
  with chkSymCMacros do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgCMacro+' (-Sm)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymTP7Compat := TCheckBox.Create(Self);
  with chkSymTP7Compat do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgBP7Cptb+' (-So)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymConstInit := TCheckBox.Create(Self);
  with chkSymConstInit do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgInitDoneOnly+' (-Ss)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymStaticKwd := TCheckBox.Create(Self);
  with chkSymStaticKwd do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgStaticKeyword+' (-St)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymDelphiCompat := TCheckBox.Create(Self);
  with chkSymDelphiCompat do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgDeplhiComp+' (-Sd)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymUseAnsiStrings := TCheckBox.Create(Self);
  with chkSymUseAnsiStrings do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgCOAnsiStr+' (-Sh)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
  end;

  inc(y,yDiff);
  chkSymGPCCompat := TCheckBox.Create(Self);
  with chkSymGPCCompat do
  begin
    Parent := grpSyntaxOptions;
    Caption := dlgGPCComp+' (-Sp)';
    Top := y;
    Left := 5;
    Width := Parent.ClientWidth-20;
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
  with grpSmartLinkUnit do
  begin
    Parent := CodeGenPage;
    Top := 5;
    Left := 5;
    Height := 80;
    Width := 150;
    Caption := dlgCOUnitStyle;
    ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
    ChildSizing.SetGridSpacing(6);
  end;

  chkSmartLinkUnit := TCheckBox.Create(Self);
  with chkSmartLinkUnit do
  begin
    Parent := grpSmartLinkUnit;
    Caption := dlgCOSmartLinkable + ' (-CX)';
  end;

  {------------------------------------------------------------}
  grpChecks := TGroupBox.Create(Self);
  with grpChecks do
  begin
    Parent := CodeGenPage;
    Top := 5;
    Left := grpSmartLinkUnit.Left + grpSmartLinkUnit.Width + 10;
    Height := 80;
    Width := 220;
    Caption := dlgCOChecks;
    ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
    ChildSizing.SetGridSpacing(6);
    ChildSizing.ControlsPerLine:=2;
  end;

  chkChecksIO := TCheckBox.Create(Self);
  with chkChecksIO do
  begin
    Parent := grpChecks;
    Caption := 'I/O (-Ci)';
  end;

  chkChecksRange := TCheckBox.Create(Self);
  with chkChecksRange do
  begin
    Parent := grpChecks;
    Caption := dlgCORange+' (-Cr)';
  end;

  chkChecksOverflow := TCheckBox.Create(Self);
  with chkChecksOverflow do
  begin
    Parent := grpChecks;
    Caption := dlgCOOverflow+' (-Co)';
  end;

  chkChecksStack := TCheckBox.Create(Self);
  with chkChecksStack do
  begin
    Parent := grpChecks;
    Caption := dlgCOStack+' (-Ct)';
  end;

  {------------------------------------------------------------}

  grpHeapSize := TGroupBox.Create(Self);
  with grpHeapSize do
  begin
    Parent := CodeGenPage;
    Top := 10;
    Left := grpChecks.Left + grpChecks.Width + 10;
    Height := 55;
    Width := 100;
    Caption := dlgHeapSize +' (-Ch):';
    ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
    ChildSizing.SetGridSpacing(6);
  end;

  edtHeapSize := TEdit.Create(grpHeapSize);
  with edtHeapSize do
  begin
    Parent := grpHeapSize;
    Caption := dlgHeapSize;
    Text := '';
  end;

  {------------------------------------------------------------}

  grpGenerate := TGroupBox.Create(Self);
  with grpGenerate do
  begin
    Parent := CodeGenPage;
    Top := grpSmartLinkUnit.Top + grpSmartLinkUnit.Height + 6;
    Left := 5;
    Height := 100;
    Width := 150;
    Caption := dlgCOGenerate;
    ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    ChildSizing.SetGridSpacing(6);
  end;

  radGenNormal := TRadioButton.Create(grpGenerate);
  with radGenNormal do
  begin
    Parent := grpGenerate;
    Caption := dlgCONormal+' (none)';
  end;

  radGenFaster := TRadioButton.Create(grpGenerate);
  with radGenFaster do
  begin
    Parent := grpGenerate;
    Caption := dlgCOFast+' (-OG)';
  end;

  radGenSmaller := TRadioButton.Create(grpGenerate);
  with radGenSmaller do
  begin
    Parent := grpGenerate;
    Caption := dlgCOSmaller+' (-Og)';
  end;


  {------------------------------------------------------------}

  grpTargetPlatform := TGroupBox.Create(Self);
  with grpTargetPlatform do
  begin
    Name := 'grpTargetPlatform';
    Parent := CodeGenPage;
    Top := grpGenerate.Top;
    Left := grpGenerate.Left + grpGenerate.Width + 10;
    Height := 100;
    Width := 300;
    Caption := dlgTargetPlatform;
  end;

  TargetOSComboBox:=TComboBox.Create(Self);
  with TargetOSComboBox do begin
    Name:='TargetOSComboBox';
    Parent := grpTargetPlatform;
    Left := 100;
    Top := 0;
    Width := grpTargetPlatform.ClientWidth-Left-10;
    with Items do begin
      Add('('+rsiwpDefault+')');
      Add('Darwin');
      Add('FreeBSD');
      Add('Linux');
      Add('NetBSD');
      Add('OpenBSD');
      Add('Solaris');
      Add('Win32');
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

  lblTargetOS := TLabel.Create(Self);
  with lblTargetOS do begin
    Name := 'lblTargetOS';
    Parent := grpTargetPlatform;
    Left := 6;
    AnchorVerticalCenterTo(TargetOSComboBox);
    Caption :=dlgTargetOS+' (-T)';
  end;

  TargetCPUComboBox:=TComboBox.Create(Self);
  with TargetCPUComboBox do begin
    Name:='TargetCPUComboBox';
    Parent := grpTargetPlatform;
    AnchorToCompanion(akTop,1,TargetOSComboBox);
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

  lblTargetCPU := TLabel.Create(Self);
  with lblTargetCPU do begin
    Name := 'lblTargetCPU';
    Parent := grpTargetPlatform;
    Left := 6;
    AnchorVerticalCenterTo(TargetCPUComboBox);
    Caption :=dlgTargetCPU+' (-P)';
  end;

  Targeti386ProcComboBox:=TComboBox.Create(Self);
  with Targeti386ProcComboBox do begin
    Name:='Targeti386ProcComboBox';
    Parent := grpTargetPlatform;
    AnchorToCompanion(akTop,1,TargetCPUComboBox);
    with Items do begin
      Add('('+rsiwpDefault+')');
      Add('386/486 (-Op1)');
      Add('Pentium/Pentium MMX (-Op2)');
      Add('Pentium Pro/Pentium II/C6x86/K6 (-Op3)');
    end;
    ItemIndex:=0;
  end;

  lblTargeti386Proc := TLabel.Create(Self);
  with lblTargeti386Proc do begin
    Name := 'lblTargeti386Proc';
    Parent := grpTargetPlatform;
    Left := 6;
    AnchorVerticalCenterTo(Targeti386ProcComboBox);
    Caption := dlgTargetProc;
  end;

  {------------------------------------------------------------}

  grpOptimizations := TGroupBox.Create(Self);
  with grpOptimizations do
  begin
    Parent := CodeGenPage;
    Top := grpTargetPlatform.Top + grpTargetPlatform.Height + 6;
    Left := 5;
    Height := 150;
    Width := 360;
    Caption := dlgOptimiz;
    ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    ChildSizing.SetGridSpacing(6);
    AutoSize:=true;
  end;

  radOptLevelNone := TRadioButton.Create(grpOptimizations);
  with radOptLevelNone do
  begin
    Parent := grpOptimizations;
    Caption :=  dlgLevelNoneOpt+' (none)';
  end;

  radOptLevel1 := TRadioButton.Create(grpOptimizations);
  with radOptLevel1 do
  begin
    Parent := grpOptimizations;
    Caption :=  dlgLevel1Opt+' (-O1)';
  end;

  radOptLevel2 := TRadioButton.Create(grpOptimizations);
  with radOptLevel2 do
  begin
    Parent := grpOptimizations;
    Caption := dlgLevel2Opt+' (-O2)';
  end;

  radOptLevel3 := TRadioButton.Create(grpOptimizations);
  with radOptLevel3 do
  begin
    Parent := grpOptimizations;
    Caption := dlgLevel3Opt+' (-O3)';
  end;

  chkOptVarsInReg := TCheckBox.Create(Self);
  with chkOptVarsInReg do
  begin
    Parent := grpOptimizations;
    Caption := dlgCOKeepVarsReg+' (-Or)';
  end;

  chkOptUncertain := TCheckBox.Create(Self);
  with chkOptUncertain do
  begin
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
  with grpDebugging do
  begin
    Parent := LinkingPage;
    Top := 10;
    Left := 10;
    Height := 172;
    Width := Self.ClientWidth-28;
    Caption := dlgCODebugging;
  end;

  chkDebugGDB := TCheckBox.Create(Self);
  with chkDebugGDB do
  begin
    Parent := grpDebugging;
    Caption := dlgCOGDB+' (-g)';
    Top := 6;
    Left := 8;
    Width := 360;
  end;

  chkDebugDBX := TCheckBox.Create(Self);
  with chkDebugDBX do
  begin
    Parent := grpDebugging;
    Caption := dlgCODBX+' (-gd)';
    Top := 27;
    Left := 8;
    Width := 360;
  end;

  chkUseLineInfoUnit := TCheckBox.Create(Self);
  with chkUseLineInfoUnit do
  begin
    Parent := grpDebugging;
    Caption := dlgLNumsBct+' (-gl)';
    Top := 48;
    Left := 8;
    Width := 360;
  end;

  chkUseHeaptrc := TCheckBox.Create(Self);
  with chkUseHeaptrc do
  begin
    Parent := grpDebugging;
    Caption := dlgCOHeaptrc+' (-gh)';
    Top := 69;
    Left := 8;
    Width := 360;
  end;

  chkUseValgrind := TCheckBox.Create(Self);
  with chkUseValgrind do
  begin
    Parent := grpDebugging;
    Caption := dlgCOValgrind+' (-gv)';
    Top := 90;
    Left := 8;
    Width := 360;
  end;

  chkGenGProfCode := TCheckBox.Create(Self);
  with chkGenGProfCode do
  begin
    Parent := grpDebugging;
    Caption := dlgGPROF+' (-pg)';
    Top := 111;
    Left := 8;
    Width := 360;
  end;

  chkSymbolsStrip := TCheckBox.Create(Self);
  with chkSymbolsStrip do
  begin
    Parent := grpDebugging;
    Caption := dlgCOStrip+' (-Xs)';
    Top := 132;
    Left := 8;
    Width := 360;
  end;

  {------------------------------------------------------------}

  grpLinkLibraries := TGroupBox.Create(Self);
  with grpLinkLibraries do
  begin
    Parent := LinkingPage;
    Top := grpDebugging.Top + grpDebugging.Height + 10;
    Left := 10;
    Height := 50;
    Width := (Self.ClientWidth-30) div 2;
    Caption := dlgLinkLibraries;
  end;

  chkLinkSmart := TCheckBox.Create(Self);
  with chkLinkSmart do
  begin
    Parent := grpLinkLibraries;
    Caption := dlgLinkSmart+' (-XX)';
    Top := 6;
    Left := 8;
    Height := 22;
    Width := Parent.ClientWidth-13;
  end;

  {------------------------------------------------------------}

  TargetSpecificsGrpBox := TGroupBox.Create(Self);
  with TargetSpecificsGrpBox do begin
    Parent := LinkingPage;
    Top := grpLinkLibraries.Top;
    Left := grpLinkLibraries.Left+grpLinkLibraries.Width+10;
    Height := 50;
    Width := (Self.ClientWidth-30) div 2;
    Caption := lisCOTargetOSSpecificOptions;
  end;

  chkWin32GraphicApp := TCheckBox.Create(Self);
  with chkWin32GraphicApp do
  begin
    Parent := TargetSpecificsGrpBox;
    Caption := 'Win32 gui application (-WG)';
    Top := 8;
    Left := 2;
    Height := 22;
    Width := Parent.Width-13;
  end;

  {------------------------------------------------------------}

  grpOptions := TGroupBox.Create(Self);
  with grpOptions do
  begin
    Parent := LinkingPage;
    Top := grpLinkLibraries.Top + grpLinkLibraries.Height + 10;
    Left := 10;
    Height := 75;
    Width := (Self.ClientWidth-20);
    Caption := dlgCOOpts+' (-k)';
  end;

  chkOptionsLinkOpt := TCheckBox.Create(Self);
  with chkOptionsLinkOpt do
  begin
    Parent := grpOptions;
    Caption := dlgPassOptsLinker;
    Top := 6;
    Left := 8;
    Width := 330;
  end;

  edtOptionsLinkOpt := TEdit.Create(grpOptions);
  with edtOptionsLinkOpt do
  begin
    Parent := grpOptions;
    Top := 27;
    Left := 8;
    Height := 23;
    Width := Parent.ClientWidth-20;
    Text := '';
  end;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupMessagesTab                                           }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupMessagesTab(Page: integer);
begin
  // Setup the Messages Tab
  MsgPage:=nbMain.Page[Page];
  MsgPage.Name:='MessagesPage';

  grpVerbosity := TGroupBox.Create(Self);
  with grpVerbosity do
  begin
    Parent := MsgPage;
    Top := 10;
    Left := 10;
    Height := 256;
    Width := Self.ClientWidth-28;
    Caption := dlgVerbosity;
  end;

  chkErrors := TCheckBox.Create(Self);
  with chkErrors do
  begin
    Parent := grpVerbosity;
    Caption := dlgCOShowErr+' (-ve)';
    Top := 6;
    Left := 8;
    Width := (grpVerbosity.ClientWidth div 2)-12;
  end;

  chkWarnings := TCheckBox.Create(Self);
  with chkWarnings do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowWarnings+' (-vw)';
    Top := 27;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkNotes := TCheckBox.Create(Self);
  with chkNotes do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowNotes+' (-vn)';
    Top := 48;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkHints := TCheckBox.Create(Self);
  with chkHints do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowHint+' (-vh)';
    Top := 69;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkGeneralInfo := TCheckBox.Create(Self);
  with chkGeneralInfo do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowGeneralInfo+' (-vi)';
    Top := 90;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkLineNumbers := TCheckBox.Create(Self);
  with chkLineNumbers do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowCompilingLineNumbers+' (-vl)';
    Top := 111;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkAllProcsOnError := TCheckBox.Create(Self);
  with chkAllProcsOnError do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowProcsError+' (-vb)';
    Top := 132;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkEverything := TCheckBox.Create(Self);
  with chkEverything do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowEverything+' (-va)';
    Top := 153;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkShowSummary := TCheckBox.Create(Self);
  with chkShowSummary do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowSummary+' (none)';
    Top := 174;
    Left := chkErrors.Left;
    Height := chkErrors.Height;
    Width := chkErrors.Width;
  end;

  chkDebugInfo := TCheckBox.Create(Self);
  with chkDebugInfo do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowDebugInfo+' (-vd)';
    Top := 6;
    Left := (grpVerbosity.ClientWidth div 2)+4;
    Width := (grpVerbosity.ClientWidth div 2)-12;
  end;

  chkUsedFiles := TCheckBox.Create(Self);
  with chkUsedFiles do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowUsedFiles+' (-vu)';
    Top := 27;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkTriedFiles := TCheckBox.Create(Self);
  with chkTriedFiles do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowTriedFiles+' (-vt)';
    Top := 48;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkDefinedMacros := TCheckBox.Create(Self);
  with chkDefinedMacros do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowDefinedMacros+' (-vm)';
    Top := 69;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkCompiledProc := TCheckBox.Create(Self);
  with chkCompiledProc do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowCompiledProcedures+' (-vp)';
    Top := 90;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkConditionals := TCheckBox.Create(Self);
  with chkConditionals do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowConditionals+' (-vc)';
    Top := 111;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkExecutableInfo := TCheckBox.Create(Self);
  with chkExecutableInfo do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowExecutableInfo+' (-vx)';
    Top := 132;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkNothing := TCheckBox.Create(Self);
  with chkNothing do
  begin
    Parent := grpVerbosity;
    Caption := dlgShowNothing+' (-v0)';
    Top := 153;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkFPCLogo := TCheckBox.Create(Self);
  with chkFPCLogo do
  begin
    Parent := grpVerbosity;
    Caption := dlgWriteFPCLogo+' (-l)';
    Top := 174;
    Left := chkDebugInfo.Left;
    Height := chkDebugInfo.Height;
    Width := chkDebugInfo.Width;
  end;

  chkHintsForUnusedUnitsInMainSrc := TCheckBox.Create(Self);
  with chkHintsForUnusedUnitsInMainSrc do
  begin
    Parent := grpVerbosity;
    Caption := dlgHintsUnused+' (none)';
    Top := 195;
    Left := ChkErrors.Left;
    Height := ChkErrors.Height;
    Width := chkDebugInfo.Width*2;
  end;

  chkHintsForSenderNotUsed := TCheckBox.Create(Self);
  with chkHintsForSenderNotUsed do
  begin
    Parent := grpVerbosity;
    Caption := dlgHintsParameterSenderNotUsed+' (none)';
    Top := 216;
    Left := ChkErrors.Left;
    Height := ChkErrors.Height;
    Width := chkDebugInfo.Width*2;
  end;

  {------------------------------------------------------------}
  grpErrorCnt := TGroupBox.Create(Self);
  with grpErrorCnt do
  begin
    Parent := MsgPage;
    Top := grpVerbosity.Top + grpVerbosity.Height + 10;
    Left := 10;
    Height := 50;
    Width := 200;
    Caption := dlgStopAfterNrErr+' (-Se)';
  end;

  edtErrorCnt := TEdit.Create(grpErrorCnt);
  with edtErrorCnt do
  begin
    Parent := grpErrorCnt;
    Top := 6;
    Left := 8;
    Height := 23;
    Width := grpErrorCnt.ClientWidth-2*Left-4;
    Text := '';
  end;
end;

procedure TfrmCompilerOptions.SetupOtherTab(Page: integer);
begin
  {------------------------------------------------------------}
  OtherPage:=nbMain.Page[Page];
  OtherPage.Name:='OtherPage';

  grpConfigFile := TGroupBox.Create(Self);
  with grpConfigFile do
  begin
    Parent := OtherPage;
    Top := 10;
    Left := 10;
    Height := 95;
    Width := Self.ClientWidth-28;
    Caption := dlgConfigFiles;
  end;

  chkConfigFile := TCheckBox.Create(Self);
  with chkConfigFile do
  begin
    Parent := grpConfigFile;
    Caption := dlgUseFpcCfg+' (If not checked: -n)';
    Top := 6;
    Left := 8;
    Width := 330;
  end;

  chkCustomConfigFile := TCheckBox.Create(Self);
  with chkCustomConfigFile do
  begin
    Parent := grpConfigFile;
    Caption := dlgUseCustomConfig+' (@)';
    Top := 27;
    Left := 8;
    Width := 330;
    OnClick:=@chkCustomConfigFileClick;
  end;

  edtConfigPath := TEdit.Create(grpConfigFile);
  with edtConfigPath do
  begin
    Parent := grpConfigFile;
    Top := 48;
    Left := 8;
    Height := 23;
    Width := 330;
    Text := '';
  end;

  grpCustomOptions := TGroupBox.Create(Self);
  with grpCustomOptions do begin
    Name:='grpCustomOptions';
    Parent := OtherPage;
    Left:=grpConfigFile.Left;
    Top:=grpConfigFile.Top+grpConfigFile.Height+10;
    Width:=grpConfigFile.Width;
    Height:=200;
    Caption:=lisCustomOptions2;
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
  InheritedPage.OnResize:=@InheritedPageResize;
  InheritedPage.Name:='InheritedPage';

  InhNoteLabel:=TLabel.Create(Self);
  with InhNoteLabel do begin
    Name:='InhNoteLabel';
    Parent:=InheritedPage;
    Caption:=lisAdditionalCompilerOptionsInheritedFromPackages;
  end;

  InhTreeView:=TTreeView.Create(Self);
  with InhTreeView do begin
    Name:='InhTreeView';
    Parent:=InheritedPage;
    Options:=Options+[tvoReadOnly, tvoRightClickSelect, tvoShowRoot,
                      tvoKeepCollapsedNodes];
    Images:=ImageList;
    OnSelectionChanged:=@InhTreeViewSelectionChanged;
  end;

  InhItemMemo:=TMemo.Create(Self);
  with InhItemMemo do begin
    Name:='InhItemMemo';
    Parent:=InheritedPage;
    ReadOnly:=true;
    WordWrap:=true;
    ScrollBars:=ssAutoVertical;
    Text:=lisSelectANode;
  end;
end;

procedure TfrmCompilerOptions.SetupCompilationTab(Page: integer);
// the real edit widht is calculated in the groupbox resize
// here the controls are given initial sizes to avoid jumpy controls

  function CreateGroupHead(const AParent: TWinControl;
    var ALabel: TLabel; var ACheck1, ACheck2, ACheck3: TCheckBox): Integer;
  begin
    ALabel := TLabel.Create(Self);
    ALabel.Parent := AParent;
    ALabel.Caption := lisCOCallOn;
    ALabel.Top := 3;
    ALabel.Left := XMARGIN;
    ALabel.Width := WCOLABEL;

    ACheck1 := TCheckBox.Create(Self);
    ACheck1.Parent := AParent;
    ACheck1.Caption := lisCOCallOnCompile;
    ACheck1.Top := 0;
    ACheck1.Left := XMARGIN + 1 * WCOLABEL;
    ACheck1.Width := WCOLABEL;

    ACheck2 := TCheckBox.Create(Self);
    ACheck2.Parent := AParent;
    ACheck2.Caption := lisCOCallOnBuild;
    ACheck2.Top := 0;
    ACheck2.Left := XMARGIN + 2 * WCOLABEL;
    ACheck2.Width := WCOLABEL;

    ACheck3 := TCheckBox.Create(Self);
    ACheck3.Parent := AParent;
    ACheck3.Caption := lisCOCallOnRun;
    ACheck3.Top := 0;
    ACheck3.Left := XMARGIN + 3 * WCOLABEL;
    ACheck3.Width := WCOLABEL;

    Result := ACheck1.Height + YMARGIN;
  end;

var
  y: Integer;  // Top of groups
  w: Integer;  // Width of groups
  cy: Integer; // Top of controls
  cm: Integer; // Mid for controls
begin
  CompilationPage := nbMain.Page[Page];
  CompilationPage.Name:='CompilationPage';

  y := YMARGIN;
  w := nbMain.ClientWidth - 2 * XMARGIN-4;
  cm := w div 2 - 2 * XMARGIN;

  chkCreateMakefile:=TCheckBox.Create(Self);
  with chkCreateMakefile do begin
    Name := 'chkCreateMakefile';
    Caption := 'Create Makefile';
    SetBounds(XMARGIN, y, Width, Height);
    Parent := CompilationPage;
    Inc(y, Height + YMARGIN);
  end;

  {------------------------------------------------------------}

  ExecuteBeforeGroupBox:=TGroupBox.Create(Self);
  with ExecuteBeforeGroupBox do
  begin
    Name := 'ExecuteBeforeGroupBox';
    SetBounds(XMARGIN, y, w, 125);
    Caption := lisCOExecuteBefore;
    OnResize := @ExecuteBeforeGroupBoxResize;
    Inc(y, Height + YMARGIN);
    Parent := CompilationPage;
  end;

  cy := CreateGroupHead(ExecuteBeforeGroupBox,
    lblRunIfExecBefore,
    chkExecBeforeCompile,
    chkExecBeforeBuild,
    chkExecBeforeRun
  );

  ExecuteBeforeCommandLabel:=TLabel.Create(Self);
  with ExecuteBeforeCommandLabel do begin
    Name:='ExecuteBeforeCommandLabel';
    Caption:=lisCOCommand;
    SetBounds(XMARGIN, cy + 2, WCOLABEL, Height);
    Parent:=ExecuteBeforeGroupBox;
//    Inc(cy, Height + YMARGIN);
  end;

  ExecuteBeforeCommandEdit:=TEdit.Create(Self);
  with ExecuteBeforeCommandEdit do begin
    Name:='ExecuteBeforeCommandEdit';
    Parent:=ExecuteBeforeGroupBox;
    Text:='';
    SetBounds(XMARGIN + WCOLABEL, cy, w - 2 * XMARGIN - WCOLABEL, Height);
    inc(cy, Height+YMARGIN);
  end;

  ExecuteBeforeScanFPCCheckBox:=TCheckBox.Create(Self);
  with ExecuteBeforeScanFPCCheckBox do begin
    Name:='ExecuteBeforeScanFPCCheckBox';
    Parent:=ExecuteBeforeGroupBox;
    Caption:=lisCOScanForFPCMessages;
    SetBounds(XMARGIN, cy, cm, Height);
  end;

  ExecuteBeforeScanMakeCheckBox:=TCheckBox.Create(Self);
  with ExecuteBeforeScanMakeCheckBox do begin
    Name:='ExecuteBeforeScanMakeCheckBox';
    Parent:=ExecuteBeforeGroupBox;
    Caption:=lisCOScanForMakeMessages;
    SetBounds(XMARGIN + cm, cy, cm, Height);
    inc(cy, Height);
  end;

  ExecuteBeforeShowAllCheckBox:=TCheckBox.Create(Self);
  with ExecuteBeforeShowAllCheckBox do begin
    Name:='ExecuteBeforeShowAllCheckBox';
    Parent:=ExecuteBeforeGroupBox;
    Caption:=lisCOShowAllMessages;
    SetBounds(XMARGIN, cy, cm, Height);
  end;

  {------------------------------------------------------------}

  grpCompiler := TGroupBox.Create(Self);
  with grpCompiler do
  begin
    Name := 'grpCompiler';
    Parent := CompilationPage;
    SetBounds(XMARGIN, y, w, 80);
    Caption := lisCompiler;
    OnResize := @grpCompilerResize;
    Inc(y, Height + YMARGIN);
  end;

  cy := CreateGroupHead(grpCompiler,
    lblRunIfCompiler,
    chkCompilerCompile,
    chkCompilerBuild,
    chkCompilerRun
  );
  chkCompilerCompile.Checked := True;
  chkCompilerBuild.Checked := True;
  chkCompilerRun.Checked := True;

  lblCompiler := TLabel.Create(grpCompiler);
  with lblCompiler do
  begin
    Name := 'lblCompiler';
    Parent := grpCompiler;
    SetBounds(XMARGIN, cy, WCOLABEL, Height);
    Caption := lisToFPCPath;
//    Inc(cy, YMARGIN + Height);
  end;

  edtCompiler := TEdit.Create(grpCompiler);
  with edtCompiler do
  begin
    Name := 'edtCompiler';
    Parent := grpCompiler;
    SetBounds(XMARGIN + WCOLABEL, cy, w - 2 * XMARGIN - WCOLABEL - 28, Height);
    Text := '';
    inc(cy, Height+YMARGIN);
  end;

  btnCompiler := TButton.Create(Self);
  with btnCompiler do
  begin
    Name := 'btnCompiler';
    Parent := grpCompiler;
    Caption:='...';
    OnClick:=@FileBrowseBtnClick;
  end;
  with edtCompiler do // <- note here we set differnt bounds that the with
    btnCompiler.SetBounds(Left+Width+3, Top, 25, Height);

  {------------------------------------------------------------}

  ExecuteAfterGroupBox:=TGroupBox.Create(Self);
  with ExecuteAfterGroupBox do begin
    Name := 'ExecuteAfterGroupBox';
    Parent := CompilationPage;
    SetBounds(XMARGIN, y, w, 125);
    Caption := lisCOExecuteAfter;
    OnResize := @ExecuteAfterGroupBoxResize;
    Inc(y, Height + YMARGIN);
  end;

  cy := CreateGroupHead(ExecuteAfterGroupBox,
    lblRunIfExecAfter,
    chkExecAfterCompile,
    chkExecAfterBuild,
    chkExecAfterRun
  );

  ExecuteAfterCommandLabel:=TLabel.Create(Self);
  with ExecuteAfterCommandLabel do begin
    Name:='ExecuteAfterCommandLabel';
    Parent:=ExecuteAfterGroupBox;
    Caption:=lisCOCommand;
    SetBounds(XMARGIN, cy + 2, WCOLABEL, Height);
//    inc(cy, Height+YMARGIN);
  end;

  ExecuteAfterCommandEdit:=TEdit.Create(Self);
  with ExecuteAfterCommandEdit do begin
    Name:='ExecuteAfterCommandEdit';
    Parent:=ExecuteAfterGroupBox;
    Text:='';
    SetBounds(XMARGIN + WCOLABEL, cy, w - 2 * XMARGIN - WCOLABEL, Height);
    inc(cy, Height+YMARGIN);
  end;

  ExecuteAfterScanFPCCheckBox:=TCheckBox.Create(Self);
  with ExecuteAfterScanFPCCheckBox do begin
    Name:='ExecuteAfterScanFPCCheckBox';
    Parent:=ExecuteAfterGroupBox;
    Caption:=lisCOScanForFPCMessages;
    SetBounds(XMARGIN, cy, cm, Height);
  end;

  ExecuteAfterScanMakeCheckBox:=TCheckBox.Create(Self);
  with ExecuteAfterScanMakeCheckBox do begin
    Name:='ExecuteAfterScanMakeCheckBox';
    Parent:=ExecuteAfterGroupBox;
    Caption:=lisCOScanForMakeMessages;
    SetBounds(XMARGIN + cm, cy, cm, Height);
    inc(cy, Height);
  end;

  ExecuteAfterShowAllCheckBox:=TCheckBox.Create(Self);
  with ExecuteAfterShowAllCheckBox do begin
    Name:='ExecuteAfterShowAllCheckBox';
    Parent:=ExecuteAfterGroupBox;
    Caption:=lisCOShowAllMessages;
    SetBounds(XMARGIN, cy, cm, Height);
  end;

end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupSearchPathsTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupSearchPathsTab(Page: integer);
var
  y: Integer;
  LCLInterface: TLCLPlatform;
begin
  // Setup the Search Paths Tab
  PathPage:=nbMain.Page[Page];
  PathPage.Name:='PathsPage';

  y:=5;

  grpOtherUnits := TGroupBox.Create(Self);
  with grpOtherUnits do
  begin
    Parent := PathPage;
    Left := 10;
    Top := y;
    Width := Self.ClientWidth-28;
    Height := 45;
    Caption := dlgOtherUnitFiles;
    inc(y,Height+5);
  end;

  edtOtherUnits := TEdit.Create(Self);
  with edtOtherUnits do
  begin
    Parent := grpOtherUnits;
    Left := 8;
    Top := 0;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
  end;

  OtherUnitsPathEditBtn:=TPathEditorButton.Create(Self);
  with OtherUnitsPathEditBtn do begin
    Name:='OtherUnitsPathEditBtn';
    Parent:=grpOtherUnits;
    Left:=edtOtherUnits.Left+edtOtherUnits.Width+3;
    Top:=edtOtherUnits.Top;
    Width:=25;
    Height:=edtOtherUnits.Height;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;


  {------------------------------------------------------------}

  grpIncludeFiles := TGroupBox.Create(Self);
  with grpIncludeFiles do
  begin
    Parent := PathPage;
    Left := grpOtherUnits.Left;
    Top := y;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgCOIncFiles;
    inc(y,Height+5);
  end;

  edtIncludeFiles := TEdit.Create(Self);
  with edtIncludeFiles do
  begin
    Parent := grpIncludeFiles;
    Left := edtOtherUnits.Left;
    Top := edtOtherUnits.Top;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
  end;

  IncludeFilesPathEditBtn:=TPathEditorButton.Create(Self);
  with IncludeFilesPathEditBtn do begin
    Name:='IncludeFilesPathEditBtn';
    Parent:=grpIncludeFiles;
    Left:=edtIncludeFiles.Left+edtIncludeFiles.Width+3;
    Top:=edtIncludeFiles.Top;
    Width:=25;
    Height:=edtIncludeFiles.Height;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  {------------------------------------------------------------}

  grpOtherSources := TGroupBox.Create(Self);
  with grpOtherSources do
  begin
    Parent := PathPage;
    Top := y;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgCOSources;
    inc(y,Height+5);
  end;

  edtOtherSources := TEdit.Create(Self);
  with edtOtherSources do
  begin
    Parent := grpOtherSources;
    Left := edtOtherUnits.Left;
    Top := edtOtherUnits.Top;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
  end;

  OtherSourcesPathEditBtn:=TPathEditorButton.Create(Self);
  with OtherSourcesPathEditBtn do begin
    Name:='OtherSourcesPathEditBtn';
    Parent:=grpOtherSources;
    Left:=edtOtherSources.Left+edtOtherSources.Width+3;
    Top:=edtOtherSources.Top;
    Width:=25;
    Height:=edtOtherSources.Height;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  {------------------------------------------------------------}

  grpLibraries := TGroupBox.Create(Self);
  with grpLibraries do
  begin
    Parent := PathPage;
    Top := y;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgCOLibraries;
    inc(y,Height+5);
  end;

  edtLibraries := TEdit.Create(Self);
  with edtLibraries do
  begin
    Parent := grpLibraries;
    Left := edtOtherUnits.Left;
    Top := edtOtherUnits.Top;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
  end;

  LibrariesPathEditBtn:=TPathEditorButton.Create(Self);
  with LibrariesPathEditBtn do begin
    Name:='LibrariesPathEditBtn';
    Parent:=grpLibraries;
    Left:=edtLibraries.Left+edtLibraries.Width+3;
    Top:=edtLibraries.Top;
    Width:=25;
    Height:=edtLibraries.Height;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  {------------------------------------------------------------}

  grpUnitOutputDir := TGroupBox.Create(Self);
  with grpUnitOutputDir do
  begin
    Parent := PathPage;
    Top := y;
    Left := grpOtherUnits.Left;
    Width := grpOtherUnits.Width;
    Height := grpOtherUnits.Height;
    Caption := dlgUnitOutp;
    inc(y,Height+5);
  end;

  edtUnitOutputDir := TEdit.Create(Self);
  with edtUnitOutputDir do
  begin
    Parent := grpUnitOutputDir;
    Left := edtOtherUnits.Left;
    Top := edtOtherUnits.Top;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
  end;

  btnUnitOutputDir:=TButton.Create(Self);
  with btnUnitOutputDir do begin
    Name:='btnUnitOutputDir';
    Parent:=grpUnitOutputDir;
    Left:=edtUnitOutputDir.Left+edtUnitOutputDir.Width+3;
    Top:=edtUnitOutputDir.Top;
    Width:=25;
    Height:=edtUnitOutputDir.Height;
    Caption:='...';
    OnClick:=@FileBrowseBtnClick;
  end;

  {------------------------------------------------------------}

  grpDebugPath := TGroupBox.Create(Self);
  with grpDebugPath do
  begin
    Parent := PathPage;
    Left := 10;
    Top := y;
    Width := Self.ClientWidth-28;
    Height := 45;
    Caption := dlgCODebugPath;
    inc(y,Height+5);
  end;

  edtDebugPath := TEdit.Create(Self);
  with edtDebugPath do
  begin
    Parent := grpDebugPath;
    Left := 8;
    Top := 0;
    Width := Parent.ClientWidth-Left-37;
    Text := '';
  end;

  DebugPathEditBtn:=TPathEditorButton.Create(Self);
  with DebugPathEditBtn do begin
    Name:='DebugPathEditBtn';
    Parent:=grpDebugPath;
    Left:=edtDebugPath.Left+edtDebugPath.Width+3;
    Top:=edtDebugPath.Top;
    Width:=25;
    Height:=edtDebugPath.Height;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;


  {------------------------------------------------------------}

  LCLWidgetTypeGroupBox:=TGroupBox.Create(Self);
  with LCLWidgetTypeGroupBox do begin
    Name:='LCLWidgetTypeGroupBox';
    Parent := PathPage;
    Left := grpOtherUnits.Left;
    Top:= y;
    Width:=Self.ClientWidth-28;
    Height:=45;
    Caption:=lisLCLWidgetType+' (various)';
  end;

  LCLWidgetTypeComboBox:=TComboBox.Create(Self);
  with LCLWidgetTypeComboBox do begin
    Name:='LCLWidgetTypeComboBox';
    Parent := LCLWidgetTypeGroupBox;
    Left := 0;
    Top:= 0;
    Width:=150;
    with Items do begin
      BeginUpdate;
      Add(Format(lisCOdefault, [GetDefaultLCLWidgetType]));
      for LCLInterface:=Low(TLCLPlatform) to High(TLCLPlatform) do begin
        Items.Add(LCLPlatformNames[LCLInterface]);
      end;
      EndUpdate;
    end;
    ItemIndex:=1;
  end;
end;

{------------------------------------------------------------------------------}
{  TfrmCompilerOptions SetupButtonBar                                          }
{------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupButtonBar;
begin
  // Setup the Button Bar
  btnOK := TButton.Create(Self);
  with btnOK do
  begin
    Parent := Self;
    Caption := 'OK';
    OnClick := @ButtonOKClicked;
  end;

  btnCancel := TButton.Create(Self);
  with btnCancel do
  begin
    Parent := Self;
    Caption := dlgCancel;
    ModalResult := mrCancel;
  end;
  CancelControl:=btnCancel;

  btnShowOptions := TButton.Create(Self);
  with btnShowOptions do
  begin
    Parent := Self;
    Caption := dlgCOShowOptions;
    OnClick := @ButtonShowOptionsClicked;
  end;

  btnCheck := TButton.Create(Self);
  with btnCheck do
  begin
    Parent := Self;
    Caption := lisCompTest;
    OnClick := @btnTestClicked;
  end;

  btnLoadSave := TButton.Create(Self);
  with btnLoadSave do
  begin
    Parent := Self;
    Caption := dlgCOLoadSave;
    OnClick := @ButtonLoadSaveClick;
  end;
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

procedure TfrmCompilerOptions.frmCompilerOptionsResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
begin
  with nbMain do
    SetBounds(0,0,Parent.ClientWidth,Parent.ClientHeight-45);

  x:=Width - 10;
  y:=Height - btnCheck.Height - 12;

  with btnLoadSave do
    SetBounds(x-120,y,120,Height);
  dec(x,btnLoadSave.Width+10);

  with btnCheck do
    SetBounds(x-70,y,70,Height);
  dec(x,btnCheck.Width+10);

  with btnShowOptions do
    SetBounds(x-120,y,120,Height);
  dec(x,btnShowOptions.Width+10);

  with btnCancel do
    SetBounds(x-70,y,70,Height);
  dec(x,btnCancel.Width+10);

  with btnOK do
    SetBounds(x-70,y,70,Height);
  dec(x,btnOk.Width+10);
end;

procedure TfrmCompilerOptions.SetReadOnly(const AValue: boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
  btnOk.Enabled:=not FReadOnly;
  btnCheck.Enabled:=not FReadOnly;
end;

end.
