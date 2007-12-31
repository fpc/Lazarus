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
  Forms, Classes, Math, LCLProc, SysUtils, InterfaceBase,
  ComCtrls, Buttons, StdCtrls, ExtCtrls,
  Graphics, LResources, FileUtil, Dialogs, Controls, GraphType,
  ProjectIntf, IDEWindowIntf, IDEContextHelpEdit,
  PathEditorDlg, LazarusIDEStrConsts, IDEOptionDefs, LazConf, IDEProcs,
  IDEImagesIntf, CompilerOptions, ShowCompilerOpts, Project, PackageDefs;

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
    MainNoteBook: TNoteBook;
    BtnPanel: TPanel;

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
    grpAsmStyle: TRadioGroup;
    grpSyntaxOptions: TCheckGroup;

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
    grpVerbosity: TCheckGroup;
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
    InhSplitter: TSplitter;
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
    procedure FileBrowseBtnClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure InhTreeViewSelectionChanged(Sender: TObject);
    procedure chkCustomConfigFileClick(Sender: TObject);
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    procedure frmCompilerOptionsClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure grpOptimizationsResize(Sender: TObject);
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
  DisableAlign;
  try
    inherited Create(TheOwner);
    Caption := dlgCompilerOptions;

    IDEDialogLayoutList.ApplyLayout(Self,550,450);

    ImageIndexPackage := IDEImages.LoadImage(16, 'item_package');
    ImageIndexRequired := IDEImages.LoadImage(16, 'pkg_required');
    ImageIndexInherited := IDEImages.LoadImage(16, 'pkg_inherited');

    MainNotebook.PageIndex:=0;
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
  finally
    EnableAlign;
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions Destructor
------------------------------------------------------------------------------}
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

procedure TfrmCompilerOptions.FormResize(Sender: TObject);
begin
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
  DisableAlign;
  try
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
      grpAsmStyle.ItemIndex:=Options.AssemblerStyle
    else
      grpAsmStyle.ItemIndex:=0;

    with grpSyntaxOptions do
    begin
      Checked[0] := Options.Delphi2Extensions;
      Checked[1] := Options.CStyleOperators;
      Checked[2] := Options.IncludeAssertionCode;
      Checked[3] := Options.AllowLabel;
      Checked[4] := Options.CPPInline;
      Checked[5] := Options.CStyleMacros;
      Checked[6] := Options.TPCompatible;
      Checked[7] := Options.InitConstructor;
      Checked[8] := Options.StaticKeyword;
      Checked[9] := Options.DelphiCompat;
      Checked[10] := Options.UseAnsiStrings;
      Checked[11] := Options.GPCCompat;
    end;

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
      chkCompilerCompile.AnchorToNeighbour(akLeft,30,lblRunIfCompiler);
      chkCompilerCompile.Checked := crCompile in CompileReasons;
      chkCompilerBuild.Checked := crBuild in CompileReasons;
      chkCompilerRun.Checked := crRun in CompileReasons;
      chkCompilerCompile.Caption := lisCOCallOnCompile;
      chkCompilerCompile.Visible := True;
      chkCompilerBuild.Visible := True;
      chkCompilerRun.Visible := True;
      lblCompiler.AnchorToNeighbour(akTop,6,lblRunIfCompiler);
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
      lblCompiler.AnchorToNeighbour(akTop,6,chkCompilerCompile);
    end
    else begin
      lblRunIfCompiler.Visible := False;
      chkCompilerCompile.Visible := False;
      chkCompilerBuild.Visible := False;
      chkCompilerRun.Visible := False;
      lblCompiler.AnchorParallel(akTop,6,lblCompiler.Parent);
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
        //DebugLn(['CheckSearchPath ',ExpandedPath,' ',p,' ',length(ExpandedPath)]);
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
  
  // Put the compiler options from the dialog into the TCompilerOptions class
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
  Options.AssemblerStyle := grpAsmStyle.ItemIndex;

  with grpSyntaxOptions do
  begin
    Options.Delphi2Extensions := Checked[0];
    Options.CStyleOperators := Checked[1];
    Options.IncludeAssertionCode := Checked[2];
    Options.AllowLabel := Checked[3];
    Options.CPPInline := Checked[4];
    Options.CStyleMacros := Checked[5];
    Options.TPCompatible := Checked[6];
    Options.InitConstructor := Checked[7];
    Options.StaticKeyword := Checked[8];
    Options.DelphiCompat := Checked[9];
    Options.UseAnsiStrings := Checked[10];
    Options.GPCCompat := Checked[11];
  end;

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
  MainNoteBook.Page[Page].Caption:= dlgCOParsing;

  // Setup the Parsing Tab
  with grpAsmStyle do begin
    Caption := dlgCOAsmStyle+' (-R)';

    with Items do begin
      BeginUpdate;
      Add('Default');
      Add('Intel');
      Add('AT&&T');
      EndUpdate;
    end;
  end;

  with grpSyntaxOptions do begin
    AutoSize:=true;
    Caption := dlgSyntaxOptions;

    Items.Add(dlgDelphi2Ext+' (-S2)');
    Items.Add(dlgCOCOps+' (-Sc)');
    Items.Add(dlgAssertCode+' (-Sa)');
    Items.Add(dlgLabelGoto+' (-Sg)');
    Items.Add(dlgCppInline+' (-Si)');
    Items.Add(dlgCMacro+' (-Sm)');
    Items.Add(dlgBP7Cptb+' (-So)');
    Items.Add(dlgInitDoneOnly+' (-Ss)');
    Items.Add(dlgStaticKeyword+' (-St)');
    Items.Add(dlgDeplhiComp+' (-Sd)');
    Items.Add(dlgCOAnsiStr+' (-Sh)');
    Items.Add(dlgGPCComp+' (-Sp)');
  end;
end;


{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupCodeGenerationTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupCodeGenerationTab(Page: integer);
begin
  // Setup the Code Generation Tab
  MainNoteBook.Page[Page].Caption:= dlgCodeGeneration;

  grpSmartLinkUnit.Caption := dlgCOUnitStyle;
  chkSmartLinkUnit.Caption := dlgCOSmartLinkable + ' (-CX)';

  grpChecks.Caption := dlgCOChecks;
  chkChecksIO.Caption := 'I/O (-Ci)';
  chkChecksRange.Caption := dlgCORange+' (-Cr)';
  chkChecksOverflow.Caption := dlgCOOverflow+' (-Co)';
  chkChecksStack.Caption := dlgCOStack+' (-Ct)';

  grpHeapSize.Caption := dlgHeapSize +' (-Ch):';
  edtHeapSize.Text := '';

  grpGenerate.Caption := dlgCOGenerate;
  radGenNormal.Caption := dlgCONormal+' (none)';
  radGenFaster.Caption := dlgCOFast+' (-OG)';
  radGenSmaller.Caption := dlgCOSmaller+' (-Og)';

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

  lblTargetCPU.Caption :=dlgTargetCPU+' (-P)';

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

  lblTargeti386Proc.Caption := dlgTargetProc;

  with Targeti386ProcComboBox do begin
    with Items do begin
      Add('('+rsiwpDefault+')');
      Add('386/486 (-Op1)');
      Add('Pentium/Pentium MMX (-Op2)');
      Add('Pentium Pro/Pentium II/C6x86/K6 (-Op3)');
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
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupLinkingTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupLinkingTab(Page: integer);
begin
  MainNoteBook.Page[Page].Caption:= dlgCOLinking;

  // Setup the Linking Tab
  with grpDebugging do begin
    AutoSize:=true;
    Caption := dlgCODebugging;
  end;

  chkDebugGDB.Caption := dlgCOGDB+' (-g)';
  chkDebugDBX.Caption := dlgCODBX+' (-gd)';
  chkUseLineInfoUnit.Caption := dlgLNumsBct+' (-gl)';
  chkUseHeaptrc.Caption := dlgCOHeaptrc+' (-gh)';
  chkUseValgrind.Caption := dlgCOValgrind+' (-gv)';
  chkGenGProfCode.Caption := dlgGPROF+' (-pg)';
  chkSymbolsStrip.Caption := dlgCOStrip+' (-Xs)';

  grpLinkLibraries.Caption := dlgLinkLibraries;
  chkLinkSmart.Caption := dlgLinkSmart+' (-XX)';

  TargetSpecificsGrpBox.Caption := lisCOTargetOSSpecificOptions;
  chkWin32GraphicApp.Caption := 'Win32 gui application (-WG)';

  grpOptions.Caption := dlgCOOpts+' (-k)';
  chkOptionsLinkOpt.Caption := dlgPassOptsLinker;
  edtOptionsLinkOpt.Text := '';
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupMessagesTab
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupMessagesTab(Page: integer);
begin
  // Setup the Messages Tab
  MainNoteBook.Page[Page].Caption:= dlgCOMessages;

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

  grpConfigFile.Caption := dlgConfigFiles;
  chkConfigFile.Caption := dlgUseFpcCfg+' (If not checked: -n)';
  chkCustomConfigFile.Caption := dlgUseCustomConfig+' (@)';
  edtConfigPath.Text := '';

  grpCustomOptions.Caption:=lisCustomOptions2;
end;

{------------------------------------------------------------------------------
  procedure TfrmCompilerOptions.SetupInheritedTab(Page: integer);
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupInheritedTab(Page: integer);
begin
  MainNoteBook.Page[Page].Caption:= dlgCOInherited;

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

  chkCreateMakefile.Caption := 'Create Makefile';

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
begin
  // Setup the Search Paths Tab
  MainNoteBook.Page[Page].Caption:= dlgSearchPaths;

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
    Constraints.MinHeight:= 25;
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
    Constraints.MinHeight:= 25;
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
    Constraints.MinHeight:= 25;
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
    Constraints.MinHeight:= 25;
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
    Constraints.MinHeight:= 25;
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
    Constraints.MinHeight:= 25;
  end;
  edtDebugPath.AnchorToNeighbour(akRight,0,DebugPathEditBtn);

  {------------------------------------------------------------}

  LCLWidgetTypeLabel.Caption:=Format(lisCOVarious, [lisLCLWidgetType]);
  with LCLWidgetTypeComboBox do begin
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
    // MG: does not work in win32 intf: AutoSize:=True;
  end;
end;

{------------------------------------------------------------------------------
  TfrmCompilerOptions SetupButtonBar
------------------------------------------------------------------------------}
procedure TfrmCompilerOptions.SetupButtonBar;
begin
  // Setup the Button Bar
  btnOK.Caption := lisOkBtn;
  btnCancel.Caption := dlgCancel;
  CancelControl:=btnCancel;
  btnShowOptions.Caption := dlgCOShowOptions;
  btnCheck.Caption := lisCompTest;
  btnLoadSave.Hint := dlgCOLoadSave;
  btnLoadSave.Caption := '...';
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

procedure TfrmCompilerOptions.grpOptimizationsResize(Sender: TObject);
var
  x: Integer;
begin
  x:=radOptLevel1.Left+Max(radOptLevel1.Width,radOptLevel2.Width)+6;
  chkOptVarsInReg.Left:=x;
  chkOptUncertain.Left:=x;
end;

procedure TfrmCompilerOptions.SetReadOnly(const AValue: boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
  btnOk.Enabled:=not FReadOnly;
  btnCheck.Enabled:=not FReadOnly;
end;

initialization
  {$I compileroptionsdlg.lrs}

end.
