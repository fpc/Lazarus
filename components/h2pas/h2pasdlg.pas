{ Copyright (C) 2006 Mattias Gaertner

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit H2PasDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, LResources, Forms, Controls,
  Graphics, Dialogs, ComCtrls, Buttons, StdCtrls, ExtCtrls,
  LazConfigStorage,
  SynEdit, SynHighlighterCPP,
  FileProcs,
  IDEMsgIntf, MenuIntf, IDECommands, BaseIDEIntf, IDEDialogs, LazIDEIntf,
  ProjectIntf, CodeToolManager, SrcEditorIntf, IDETextConverter,
  H2PasStrConsts, H2PasConvert, IDETextConvListEdit;

type

  { TH2PasDialog }

  TH2PasDialog = class(TForm)
    MergeAllCHeadersExceptCurrentButton: TButton;
    MergeFileCheckBox: TCheckBox;
    FileInfoMemo: TMemo;
    FileStateImageList: TImageList;
    MoveFileDownButton: TButton;
    MoveFileUpButton: TButton;
    ConvertAndBuildButton: TButton;
    FileInfoGroupBox: TGroupBox;
    MainPageControl: TPageControl;
    AddIncludedCHeaderFilesButton: TButton;

    // c header files
    FilesTabSheet: TTabSheet;
    CHeaderFilesSplitter1: TSplitter;
    AddCHeadersButton: TButton;
    DisableAllCHeadersButton: TButton;
    EnableAllCHeadersButton: TButton;
    DeleteCHeadersButton: TButton;
    CHeaderFilesCheckTreeView: TTreeView;

    // pre h2pas
    PreH2PasTabSheet: TTabSheet;
    PreH2PasGroupBox: TGroupBox;
    PreH2PasEdit: TTextConvListEditor;

    // h2pas
    h2pasOptionsTabSheet: TTabSheet;
    h2pasOptionsCheckGroup: TCheckGroup;
    LibnameEdit: TEdit;
    LibNameLabel: TLabel;
    OutputExtEdit: TEdit;
    OutputExtLabel: TLabel;
    OutputDirEdit: TEdit;
    OutputDirLabel: TLabel;
    OutputDirBrowseButton: TButton;

    // post h2pas
    PostH2PasTabSheet: TTabSheet;
    PostH2PasGroupBox: TGroupBox;
    PostH2PasEdit: TTextConvListEditor;

    // settings
    SettingsTabSheet: TTabSheet;
    h2pasFilenameBrowseButton: TButton;
    H2PasFilenameEdit: TEdit;
    H2PasFilenameLabel: TLabel;
    NewSettingsButton: TButton;
    SaveSettingsAsButton: TButton;
    OpenLastProjectOnStartCheckBox: TCheckBox;

    // buttons at bottom
    OpenSettingsButton: TButton;
    SaveSettingsButton: TButton;
    ConvertButton: TButton;
    CloseButton: TButton;

    procedure AddCHeadersButtonClick(Sender: TObject);
    procedure AddIncludedCHeaderFilesButtonClick(Sender: TObject);
    procedure CHeaderFilesCheckTreeViewDblClick(Sender: TObject);
    procedure CHeaderFilesCheckTreeViewMouseDown(Sender: TOBject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CHeaderFilesCheckTreeViewSelectionChanged(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure ConstantsInsteadOfEnumsCheckBoxChange(Sender: TObject);
    procedure ConvertAndBuildButtonClick(Sender: TObject);
    procedure ConvertButtonClick(Sender: TObject);
    procedure DeleteCHeadersButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure H2PasFilenameEditEditingDone(Sender: TObject);
    procedure LibnameEditEditingDone(Sender: TObject);
    procedure MergeAllCHeadersExceptCurrentButtonClick(Sender: TObject);
    procedure MergeFileCheckBoxEditingDone(Sender: TObject);
    procedure MoveFileDownButtonClick(Sender: TObject);
    procedure MoveFileUpButtonClick(Sender: TObject);
    procedure NewSettingsButtonClick(Sender: TObject);
    procedure OpenLastProjectOnStartCheckBoxChange(Sender: TObject);
    procedure OpenSettingsButtonClick(Sender: TObject);
    procedure OutputDirBrowseButtonClick(Sender: TObject);
    procedure OutputDirEditEditingDone(Sender: TObject);
    procedure OutputExtEditEditingDone(Sender: TObject);
    procedure PostH2PasEditModified(Sender: TObject);
    procedure PreH2PasEditModified(Sender: TObject);
    procedure SaveSettingsAsButtonClick(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
    procedure EnableAllCHeadersButtonClick(Sender: TObject);
    procedure DisableAllCHeadersButtonClick(Sender: TObject);
    procedure h2pasFilenameBrowseButtonClick(Sender: TObject);
    procedure h2pasOptionsCheckGroupItemClick(Sender: TObject; Index: LongInt);
    procedure OnShowSrcEditSection(Sender: TObject);
    procedure OnAddSearchAndReplaceBeforeH2PasClick(Sender: TObject);
  private
    FConverter: TH2PasConverter;
    FSrcEditAddSearchReplaceMenuItem: TIDEMenuCommand;
    FSrcEditSection: TIDEMenuSection;
    
    function GetProject: TH2PasProject;
    
    procedure UpdateAll(ScanIncludes: boolean);
    procedure UpdateProjectChanged(ScanIncludes: boolean); // show project settings
    procedure UpdateCaption;
    procedure UpdateFileInfo;
    procedure ClearMessages;
    procedure CreateLazarusMenuItems;
    function GetNodeFilename(Node: TTreeNode): string;
    function GetCurrentCHeaderFile: TH2PasFile;
    procedure MoveCurrentFile(Offset: integer);
    function GetFileNodeStateIndex(aFile: TH2PasFile): Integer;
    procedure MarkAllCHeadersExceptCurrentToMerge;

    // project settings
    procedure UpdateFilesPage(ScanIncludes: boolean);
    procedure UpdateH2PasPage;
    procedure UpdateConvertPage;
    // global settings
    procedure UpdateSettingsPage;
    
    function ShowSelectDirDialog(const Title: string;
                                 var ADirectory: string): boolean;
    function ShowOpenFileDialog(const Title: string;
                                var AFilename: string): boolean;
    // IDE events
    function OnIDESavedAll(Sender: TObject): TModalResult;
  public
    function Convert: TModalResult;
    procedure ShowH2PasError(MsgLine: integer);

    function SaveSettings: TModalResult;
    function SaveGlobalSettings: TModalResult;
    function LoadGlobalSettings: TModalResult;
    function SaveProject(const Filename: string; Flags: TSaveFlags): TModalResult;
    function OpenProject(const Filename: string; Flags: TOpenFlags): TModalResult;
    property Converter: TH2PasConverter read FConverter;
    property Project: TH2PasProject read GetProject;
    property SrcEditSection: TIDEMenuSection read FSrcEditSection;
    property SrcEditAddSearchReplaceMenuItem: TIDEMenuCommand
                                          read FSrcEditAddSearchReplaceMenuItem;
  end;

var
  H2PasDialog: TH2PasDialog = nil;
  CmdH2PasTool: TIDECommand = nil;

procedure ExecuteH2PasTool(Sender: TObject);


procedure Register;

implementation

procedure ExecuteH2PasTool(Sender: TObject);
begin
  if H2PasDialog=nil then
    H2PasDialog:=TH2PasDialog.Create(Application);
  H2PasDialog.ShowOnTop;
end;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
begin
  // register IDE shortcut and menu item
  Key := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Cat:=IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  CmdH2PasTool := RegisterIDECommand(Cat                 ,
         h2pH2Pas, h2pCreateUnitsFromCHeaderFiles, Key, nil, @ExecuteH2PasTool);
  RegisterIDEMenuCommand(itmSecondaryTools, h2pH2PasTool, h2pH2Pas, nil, nil,
                         CmdH2PasTool);

  // register text converter tools
  TextConverterToolClasses.RegisterClass(TPreH2PasTools);
  TextConverterToolClasses.RegisterClass(TRemoveCPlusPlusExternCTool);
  TextConverterToolClasses.RegisterClass(TRemoveEmptyCMacrosTool);
  TextConverterToolClasses.RegisterClass(TReplaceEdgedBracketPairWithStar);
  TextConverterToolClasses.RegisterClass(TReplaceMacro0PointerWithNULL);
  TextConverterToolClasses.RegisterClass(TConvertFunctionTypesToPointers);
  TextConverterToolClasses.RegisterClass(TConvertEnumsToTypeDef);
  TextConverterToolClasses.RegisterClass(TCommentComplexCMacros);
  TextConverterToolClasses.RegisterClass(TCommentComplexCFunctions);
  TextConverterToolClasses.RegisterClass(TAddMissingMacroBrackets);

  TextConverterToolClasses.RegisterClass(TPostH2PasTools);
  TextConverterToolClasses.RegisterClass(TReplaceUnitFilenameWithUnitName);
  TextConverterToolClasses.RegisterClass(TRemoveDoubleSemicolons);
  TextConverterToolClasses.RegisterClass(TRemoveSystemTypes);
  TextConverterToolClasses.RegisterClass(TRemoveRedefinedPointerTypes);
  TextConverterToolClasses.RegisterClass(TRemoveEmptyTypeVarConstSections);
  TextConverterToolClasses.RegisterClass(TFixH2PasMissingIFDEFsInUnit);
  TextConverterToolClasses.RegisterClass(TReduceCompilerDirectivesInUnit);
  TextConverterToolClasses.RegisterClass(TReplaceImplicitTypes);
  TextConverterToolClasses.RegisterClass(TFixArrayOfParameterType);
  TextConverterToolClasses.RegisterClass(TAddMissingPointerTypes);
  TextConverterToolClasses.RegisterClass(TRemoveRedefinitionsInUnit);
  TextConverterToolClasses.RegisterClass(TFixAliasDefinitionsInUnit);
  TextConverterToolClasses.RegisterClass(TReplaceConstFunctionsInUnit);
  TextConverterToolClasses.RegisterClass(TReplaceTypeCastFunctionsInUnit);
  TextConverterToolClasses.RegisterClass(TFixForwardDefinitions);
  TextConverterToolClasses.RegisterClass(TAddToUsesSection);
end;

{ TH2PasDialog }

procedure TH2PasDialog.FormCreate(Sender: TObject);
begin
  Caption:=h2pCHeaderFileConverter;
  FilesTabSheet.Caption:='C header files';
    AddCHeadersButton.Caption:='Add .h files ...';
    DeleteCHeadersButton.Caption:='Delete selected .h files';
    EnableAllCHeadersButton.Caption:='Enable all .h files';
    DisableAllCHeadersButton.Caption:='Disable all .h files';
    MoveFileDownButton.Caption:='Move file down';
    MoveFileUpButton.Caption:='Move file up';
    FileInfoGroupBox.Caption:='File information';
    AddIncludedCHeaderFilesButton.Caption:='Add included .h files';
    MergeAllCHeadersExceptCurrentButton.Caption:='Merge all but this';
  MergeFileCheckBox.Caption:='Merge file';
  h2pasOptionsTabSheet.Caption:='h2pas Options';
    h2pasOptionsCheckGroup.Caption:='Options';
    with h2pasOptionsCheckGroup.Items do begin
      Add('-d  '+'Use external; for all procedures');
      Add('-D  '+'Use external libname name "func__name" for functions');
      Add('-e  '+'constants instead of enumeration type for C enums');
      Add('-c  '+'Compact outputmode, less spaces and empty lines');
      Add('-i  '+'Create an include file instead of a unit');
      Add('-p  '+'Use letter P for pointer types instead of "^"');
      Add('-pr '+'Pack all records (1 byte alignment)');
      Add('-P  '+'use proc. vars for imports');
      Add('-s  '+'Strip comments');
      Add('-S  '+'Strip comments and info');
      Add('-t  '+'Prepend  typedef  types with T');
      Add('-T  '+'Prepend  typedef  types with T, and remove __');
      Add('-v  '+'Replace pointer parameters by  var');
      Add('-w  '+'Handle special win32 macros');
      Add('-x  '+'Handle SYS__TRAP of the PalmOS header files');
      Add('-C  '+'Use types in ctypes unit');
    end;
    OutputExtLabel.Caption:='Output extension of new file';
    OutputDirLabel.Caption:='Output directory';
    LibNameLabel.Caption:='-l Library name';
  PreH2PasTabSheet.Caption:='Before h2pas';
    PreH2PasGroupBox.Caption:='Conversions before running h2pas';
  PostH2PasTabSheet.Caption:='After h2pas';
    PostH2PasGroupBox.Caption:='Conversions after running h2pas';
  SettingsTabSheet.Caption:='Settings';
    H2PasFilenameLabel.Caption:='h2pas program path';
    OpenLastProjectOnStartCheckBox.Caption:='Open last settings on start';
    SaveSettingsAsButton.Caption:='Save settings as ...';
    NewSettingsButton.Caption:='New/Clear settings';
  OpenSettingsButton.Caption:='&Open Settings';
  SaveSettingsButton.Caption:='&Save Settings';
  ConvertButton.Caption:='Run h2pas';
  ConvertAndBuildButton.Caption:='Run h2pas and compile';
  CloseButton.Caption:='&Close';
  
  PreH2PasEdit:=TTextConvListEditor.Create(Self);
  with PreH2PasEdit do
  begin
    Name:='PreH2PasEdit';
    Align:=alClient;
    OnModified:=@PreH2PasEditModified;
    Dock(PreH2PasGroupBox, Rect(0, 0, 0, 0));
    Visible:=true;// Note: it's a form, and visible default is false
  end;

  PostH2PasEdit:=TTextConvListEditor.Create(Self);
  with PostH2PasEdit do begin
    Name:='PostH2PasEdit';
    Align:=alClient;
    OnModified:=@PostH2PasEditModified;
    Dock(PostH2PasGroupBox, Rect(0, 0, 0, 0));
    Visible:=true;// Note: it's a form, and visible default is false
  end;

  LazarusIDE.AddHandlerOnSavedAll(@OnIDESavedAll);
  CreateLazarusMenuItems;

  // create converter
  FConverter:=TH2PasConverter.Create;
  LoadGlobalSettings;
  
  // create project
  if Converter.AutoOpenLastProject
  and FileExistsUTF8(Converter.CurrentProjectFilename) then
    OpenProject(Converter.CurrentProjectFilename,[]);
  if Project=nil then begin
    Converter.Project:=TH2PasProject.Create;
    PreH2PasEdit.ListOfTools:=Project.PreH2PasTools;
    PostH2PasEdit.ListOfTools:=Project.PostH2PasTools;
  end;

  UpdateAll(false);
end;

procedure TH2PasDialog.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  DlgResult: TModalResult;
begin
  //DebugLn(['TH2PasDialog.FormCloseQuery Converter.Modified=',Converter.Modified,' Project.Modified=',Project.Modified]);
  if Converter.Modified
  or ((Project<>nil) and (Project.Modified)) then begin
    DlgResult:=QuestionDlg('Save changes?',
      'Save settings?',mtConfirmation,
      [mrYes,'Save and exit',mrNo,'Discard changes and exit',
       mrCancel,'Do not exit'],0);
    case DlgResult of
    mrYes: CanClose:=SaveSettings=mrOk;
    mrNo: ;
    else CanClose:=false;
    end;
  end;
end;

procedure TH2PasDialog.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TH2PasDialog.ConstantsInsteadOfEnumsCheckBoxChange(Sender: TObject);
begin

end;

procedure TH2PasDialog.ConvertAndBuildButtonClick(Sender: TObject);
begin
  if Convert=mrOk then
    LazarusIDE.DoBuildProject(crCompile,[]);
end;

procedure TH2PasDialog.ConvertButtonClick(Sender: TObject);
begin
  Convert;
end;

procedure TH2PasDialog.DeleteCHeadersButtonClick(Sender: TObject);
var
  DeleteFiles: TStringList;
  Node: TTreeNode;
begin
  DeleteFiles:=TStringList.Create;
  Node:=CHeaderFilesCheckTreeView.GetFirstMultiSelected;
  while Node<>nil do begin
    if Node.Parent=nil then begin
      // top lvl node is a .h file
      DeleteFiles.Add(GetNodeFilename(Node));
    end;
    Node:=Node.GetNextMultiSelected;
  end;
  if DeleteFiles.Count>0 then begin
    if QuestionDlg('Confirm removal',
      'Delete these .h files from list?'#13
      +#13
      +DeleteFiles.Text,
      mtConfirmation,[mrYes,'Remove all files',mrCancel],0)=mrYes
    then begin
      Project.DeleteFiles(DeleteFiles);
    end;
    UpdateFilesPage(true);
  end;
  DeleteFiles.Free;
end;

procedure TH2PasDialog.AddCHeadersButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Title:='Add *.h files ...';
    OpenDialog.Options:=OpenDialog.Options+[ofAllowMultiSelect,ofFileMustExist];
    OpenDialog.Filter:='C header file (*.h)|*.h|All files (*.*)|'+FileMask;
    if OpenDialog.Execute then begin
      Project.AddFiles(OpenDialog.Files);
      UpdateFilesPage(true);
    end;
  finally
    StoreIDEFileDialog(OpenDialog);
    OpenDialog.Free;
  end;
end;

procedure TH2PasDialog.CHeaderFilesCheckTreeViewMouseDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  StateIconLeft: LongInt;
  AFilename: String;
  CurFile: TH2PasFile;
begin
  if (Button=mbLeft) and (Shift=[]) then ;
  Node:=CHeaderFilesCheckTreeView.GetNodeAt(X,Y);
  if (Node=nil) or (Node.Parent<>nil) then exit;
  StateIconLeft:=Node.DisplayStateIconLeft;
  if (x>=StateIconLeft) and (x<StateIconLeft+FileStateImageList.Width) then
  begin
    AFilename:=GetNodeFilename(Node);
    CurFile:=Project.CHeaderFileWithFilename(AFilename);
    if CurFile=nil then exit;
    CurFile.Enabled:=not CurFile.Enabled;
    Node.StateIndex:=GetFileNodeStateIndex(CurFile);
  end;
end;

procedure TH2PasDialog.AddIncludedCHeaderFilesButtonClick(Sender: TObject);
var
  AFile: TH2PasFile;
  sl: TStringList;
  i: Integer;
  IncFile: TH2PasFileCInclude;
  CurFilename: String;
  j: Integer;
  s: String;
begin
  AFile:=GetCurrentCHeaderFile;
  if AFile=nil then exit;
  AFile.ReadCIncludes(false);
  sl:=TStringList.Create;
  try
    for i:=0 to AFile.CIncludeCount-1 do begin
      IncFile:=AFile.CIncludes[i];
      CurFilename:=IncFile.Filename;
      if CurFilename='' then continue;
      // .h file found in include directories
      if Project.CHeaderFileWithFilename(CurFilename)<>nil then continue;
      // .h file not yet in project
      j:=sl.Count-1;
      while (j>=0) and (CompareFilenames(CurFilename,sl[j])<>0) do dec(j);
      if j>=0 then continue;
      // .h file not yet in list
      sl.Add(CurFilename);
    end;
    if sl.Count>0 then begin
      s:='';
      for i:=0 to sl.Count-1 do begin
        CurFilename:=Project.ShortenFilename(sl[i]);
        s:=s+#13+CurFilename;
      end;
      if QuestionDlg('Add .h files?',
        'Add these .h files to h2pas project:'#13
        +s+#13+'?',
        mtConfirmation,[mrYes,mrNo],0)=mrYes
      then begin
        Project.AddFiles(sl);
        Project.ReadAllCIncludes(true);
        UpdateFilesPage(false);
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TH2PasDialog.CHeaderFilesCheckTreeViewDblClick(Sender: TObject);
var
  CurFile: TH2PasFile;
begin
  CurFile:=GetCurrentCHeaderFile;
  if CurFile<>nil then
    LazarusIDE.DoOpenEditorFile(CurFile.Filename,-1, -1,[]);
end;

procedure TH2PasDialog.CHeaderFilesCheckTreeViewSelectionChanged(Sender: TObject
  );
begin
  UpdateFileInfo;
end;

procedure TH2PasDialog.FormDestroy(Sender: TObject);
begin
  PreH2PasEdit.ListOfTools:=nil;
  PostH2PasEdit.ListOfTools:=nil;
  FreeAndNil(FConverter);
end;

procedure TH2PasDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then begin
    ModalResult:=mrCancel;
    Key:=VK_UNKNOWN;
  end;
  if (Key=VK_S) and (Shift=[ssCtrl]) then begin
    SaveSettings;
    Key:=VK_UNKNOWN;
  end;
end;

procedure TH2PasDialog.H2PasFilenameEditEditingDone(Sender: TObject);
begin
  Converter.h2pasFilename:=H2PasFilenameEdit.Text;
end;

procedure TH2PasDialog.LibnameEditEditingDone(Sender: TObject);
begin
  if Project<>nil then
    Project.Libname:=LibnameEdit.Text;
end;

procedure TH2PasDialog.MergeAllCHeadersExceptCurrentButtonClick(Sender: TObject
  );
begin
  MarkAllCHeadersExceptCurrentToMerge;
end;

procedure TH2PasDialog.MergeFileCheckBoxEditingDone(Sender: TObject);
var
  CurFile: TH2PasFile;
begin
  if Project=nil then exit;
  CurFile:=GetCurrentCHeaderFile;
  if CurFile=nil then exit;
  CurFile.Merge:=MergeFileCheckBox.Checked;
  UpdateFileInfo;
end;

procedure TH2PasDialog.MoveFileDownButtonClick(Sender: TObject);
begin
  MoveCurrentFile(1);
end;

procedure TH2PasDialog.MoveFileUpButtonClick(Sender: TObject);
begin
  MoveCurrentFile(-1);
end;

procedure TH2PasDialog.NewSettingsButtonClick(Sender: TObject);
begin
  Project.Filename:='';
  Project.Clear(true);
  UpdateAll(true);
end;

procedure TH2PasDialog.OpenLastProjectOnStartCheckBoxChange(Sender: TObject);
begin
  Converter.AutoOpenLastProject:=OpenLastProjectOnStartCheckBox.Checked;
end;

procedure TH2PasDialog.OpenSettingsButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Title:='Open project (*.h2p) ...';
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
    OpenDialog.Filter:='h2pas project (*.h2p)|*.h2p|All files (*.*)|'+FileMask;
    if OpenDialog.Execute then begin
      OpenProject(OpenDialog.FileName,[]);
    end;
  finally
    StoreIDEFileDialog(OpenDialog);
    OpenDialog.Free;
  end;
end;

procedure TH2PasDialog.OutputDirBrowseButtonClick(Sender: TObject);
var
  ADirectory: String;
begin
  ADirectory:=OutputDirEdit.Text;
  if not ShowSelectDirDialog('Output directory',ADirectory) then exit;
  Project.OutputDirectory:=Project.ShortenFilename(ADirectory);
  OutputDirEdit.Text:=Project.OutputDirectory;
end;

procedure TH2PasDialog.OutputDirEditEditingDone(Sender: TObject);
begin
  Project.OutputDirectory:=OutputDirEdit.Text;
end;

procedure TH2PasDialog.OutputExtEditEditingDone(Sender: TObject);
begin
  Project.OutputExt:=OutputExtEdit.Text;
end;

procedure TH2PasDialog.PostH2PasEditModified(Sender: TObject);
begin
  Project.Modified:=true;
end;

procedure TH2PasDialog.PreH2PasEditModified(Sender: TObject);
begin
  Project.Modified:=true;
end;

procedure TH2PasDialog.SaveSettingsAsButtonClick(Sender: TObject);
begin
  SaveProject('',[sfSaveAs]);
end;

procedure TH2PasDialog.SaveSettingsButtonClick(Sender: TObject);
begin
  SaveProject('',[]);
end;

procedure TH2PasDialog.EnableAllCHeadersButtonClick(Sender: TObject);
var
  i: Integer;
begin
  CHeaderFilesCheckTreeView.BeginUpdate;
  for i:=0 to CHeaderFilesCheckTreeView.Items.TopLvlCount-1 do
    CHeaderFilesCheckTreeView.Items.TopLvlItems[i].StateIndex:=1;
  for i:=0 to Project.CHeaderFileCount-1 do
    Project.CHeaderFiles[i].Enabled:=true;
  CHeaderFilesCheckTreeView.EndUpdate;
  UpdateFileInfo;
end;

procedure TH2PasDialog.DisableAllCHeadersButtonClick(Sender: TObject);
var
  i: Integer;
begin
  CHeaderFilesCheckTreeView.BeginUpdate;
  for i:=0 to CHeaderFilesCheckTreeView.Items.TopLvlCount-1 do
    CHeaderFilesCheckTreeView.Items.TopLvlItems[i].StateIndex:=0;
  for i:=0 to Project.CHeaderFileCount-1 do
    Project.CHeaderFiles[i].Enabled:=false;
  CHeaderFilesCheckTreeView.EndUpdate;
  UpdateFileInfo;
end;

procedure TH2PasDialog.h2pasFilenameBrowseButtonClick(Sender: TObject);
var
  AFilename: String;
begin
  AFilename:=H2PasFilenameEdit.Text;
  if not ShowOpenFileDialog('Filename of h2pas program',AFilename) then exit;
  Converter.h2pasFilename:=AFilename;
  H2PasFilenameEdit.Text:=Converter.h2pasFilename;
end;

procedure TH2PasDialog.h2pasOptionsCheckGroupItemClick(Sender: TObject;
  Index: LongInt);
var
  s: string;
  p: Integer;
  OptionStr: String;
  NewValue: boolean;
begin
  if Index<0 then exit;
  NewValue:=h2pasOptionsCheckGroup.Checked[Index];
  s:=h2pasOptionsCheckGroup.Items[Index];
  p:=2;
  while (p<=length(s)) and (s[p]<>' ') do inc(p);
  OptionStr:=copy(s,2,p-2);
  
  if length(OptionStr)=1 then begin
    case OptionStr[1] of
    'd': Project.UseExternal:=NewValue;
    'D': Project.UseExternalLibname:=NewValue;
    'e': Project.ConstantsInsteadOfEnums:=NewValue;
    'c': Project.CompactOutputmode:=NewValue;
    'i': Project.CreateIncludeFile:=NewValue;
    'p': Project.PforPointers:=NewValue;
    'P': Project.UseProcVarsForImport:=NewValue;
    's': Project.StripComments:=NewValue;
    'S': Project.StripCommentsAndInfo:=NewValue;
    't': Project.TforTypedefs:=NewValue;
    'T': Project.TforTypedefsRemoveUnderscore:=NewValue;
    'v': Project.VarParams:=NewValue;
    'w': Project.Win32Header:=NewValue;
    'x': Project.PalmOSSYSTrap:=NewValue;
    'C': Project.UseCTypes:=NewValue;
    else
      raise Exception.Create('TH2PasDialog.h2pasOptionsCheckGroupItemClick: Unknown option '+OptionStr);
    end;
  end else begin
    if OptionStr='pr' then
      Project.PackAllRecords:=NewValue;
  end;
end;

procedure TH2PasDialog.OnShowSrcEditSection(Sender: TObject);
var
  SrcEdit: TSourceEditorInterface;
begin
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  SrcEditSection.Visible:=(SrcEdit<>nil)
                          and Converter.FileIsRelated(SrcEdit.FileName);
  //DebugLn(['TH2PasDialog.OnShowSrcEditSection ',SrcEditSection.Visible]);
end;

procedure TH2PasDialog.OnAddSearchAndReplaceBeforeH2PasClick(Sender: TObject);
var
  Tool: TCustomTextConverterTool;
  SrcEdit: TSourceEditorInterface;
  s: String;
begin
  //DebugLn(['TH2PasDialog.OnAddSearchAndReplaceBeforeH2PasClick']);
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;
  MainPageControl.ActivePage:=PreH2PasTabSheet;
  ShowOnTop;
  // get current source editor selection or line
  s:=SrcEdit.Selection;
  if s='' then
    s:=SrcEdit.CurrentLineText;
  // add a search and replace tool
  Tool:=PreH2PasEdit.CreateTool(TTextReplaceTool);
  TTextReplaceTool(Tool).SearchFor:=s;
  if (System.Pos(#10,s)>0) or (System.Pos(#13,s)>0) then
    TTextReplaceTool(Tool).Options:=TTextReplaceTool(Tool).Options+[trtMultiLine];
  PreH2PasEdit.PropertyGrid.RefreshPropertyValues;
  //DebugLn(['TH2PasDialog.OnAddSearchAndReplaceBeforeH2PasClick ',s]);
end;

function TH2PasDialog.GetProject: TH2PasProject;
begin
  Result:=Converter.Project;
end;

procedure TH2PasDialog.UpdateAll(ScanIncludes: boolean);
begin
  UpdateCaption;
  UpdateFilesPage(ScanIncludes);
  UpdateH2PasPage;
  UpdateConvertPage;
  UpdateSettingsPage;
end;

procedure TH2PasDialog.UpdateProjectChanged(ScanIncludes: boolean);
begin
  UpdateCaption;
  UpdateFilesPage(ScanIncludes);
  UpdateH2PasPage;
  UpdateConvertPage;
end;

procedure TH2PasDialog.UpdateCaption;
var
  s: String;
begin
  s:=h2pCHeaderFileConverter;
  if Project<>nil then
    s:=s+' - '+ExtractFileNameOnly(Project.Filename);
  Caption:=s;
end;

procedure TH2PasDialog.UpdateFileInfo;
var
  AFile: TH2PasFile;
  s: String;
  Filename: String;
  OutputFilename: String;
  i: Integer;
  IncFile: TH2PasFileCInclude;
begin
  AFile:=GetCurrentCHeaderFile;
  if AFile<>nil then begin
    Filename:=AFile.Filename;
    s:='File: '+Filename;
    if not FileExistsCached(Filename) then
      s:=s+#13+'ERROR: file not found';
    if AFile.MergedBy<>nil then begin
      OutputFilename:=AFile.MergedBy.GetOutputFilename;
      s:=s+#13+'Merged into: '+OutputFilename;
    end else begin
      OutputFilename:=AFile.GetOutputFilename;
      s:=s+#13+'Output: '+OutputFilename;
    end;

    AFile.ReadCIncludes(false);
    if AFile.CIncludeCount>0 then begin
      s:=s+#13#13+'Includes:';
      for i:=0 to AFile.CIncludeCount-1 do begin
        IncFile:=AFile.CIncludes[i];
        s:=s+#13+Project.ShortenFilename(IncFile.Filename)+':'+IntToStr(IncFile.SrcPos.Y);
      end;
      AddIncludedCHeaderFilesButton.Enabled:=true;
    end else begin
      AddIncludedCHeaderFilesButton.Enabled:=false;
    end;

    if AFile.CIncludedByCount>0 then begin
      s:=s+#13#13+'Included by:';
      for i:=0 to AFile.CIncludedByCount-1 do begin
        IncFile:=AFile.CIncludedBy[i];
        s:=s+#13+Project.ShortenFilename(IncFile.Owner.Filename)+':'+IntToStr(IncFile.SrcPos.Y);
      end;
    end;

    FileInfoMemo.Caption:=s;

    MergeFileCheckBox.Checked:=AFile.Merge;
    MergeFileCheckBox.Enabled:=true;
  end else begin
    FileInfoMemo.Caption:='No file selected.';
    MergeFileCheckBox.Enabled:=false;
    AddIncludedCHeaderFilesButton.Enabled:=false;
  end;
end;

procedure TH2PasDialog.ClearMessages;
begin
  IDEMessagesWindow.Clear;
end;

procedure TH2PasDialog.CreateLazarusMenuItems;
begin
  // add a context menu to the source editor. It will be freed by ide automatically
  fSrcEditSection:=RegisterIDESubMenu(SrcEditMenuSectionFirstStatic,
                                      'h2pas project','h2pas',nil,nil);
  fSrcEditSection.AddHandlerOnShow(@OnShowSrcEditSection);
  // add a menu item to easily create a Search and replace from the current
  // selection or line of the source editor.
  fSrcEditAddSearchReplaceMenuItem:=RegisterIDEMenuCommand(SrcEditSection,
      'Add "search and replace" tool before h2pas',
      'Add "search and replace" tool before h2pas', 
      @OnAddSearchAndReplaceBeforeH2PasClick);
end;

function TH2PasDialog.GetNodeFilename(Node: TTreeNode): string;
var
  p: LongInt;
begin
  Result:=Node.Text;
  p:=System.Pos('(',Result);
  if p>0 then
    Result:=copy(Result,1,p-2);
  Result:=Project.LongenFilename(Result);
end;

function TH2PasDialog.GetCurrentCHeaderFile: TH2PasFile;
var
  AFilename: String;
  Node: TTreeNode;
begin
  Result:=nil;
  Node:=CHeaderFilesCheckTreeView.Selected;
  if (Node=nil) or (Node.Parent<>nil) then exit;
  AFilename:=GetNodeFilename(Node);
  Result:=Project.CHeaderFileWithFilename(AFilename);
end;

procedure TH2PasDialog.MoveCurrentFile(Offset: integer);
var
  Index: LongInt;
  AFilename: String;
  NewIndex: Integer;
  Node: TTreeNode;
begin
  if Offset=0 then begin
    DebugLn(['TH2PasDialog.MoveCurrentFile Offset=0']);
    exit;
  end;
  Node:=CHeaderFilesCheckTreeView.Selected;
  if (Node=nil) or (Node.Parent<>nil) then exit;
  AFilename:=GetNodeFilename(Node);
  Index:=Project.CHeaderFileIndexWithFilename(AFilename);
  if Index<0 then begin
    DebugLn(['TH2PasDialog.MoveCurrentFile not found: Filename=',AFilename]);
    exit;
  end;
  NewIndex:=Index+Offset;
  if (NewIndex<0) or (NewIndex>=Project.CHeaderFileCount) then begin
    DebugLn(['TH2PasDialog.MoveCurrentFile out of bounds: NewIndex=',NewIndex]);
    exit;
  end;
  Project.CHeaderFileMove(Index,NewIndex);
  CHeaderFilesCheckTreeView.ConsistencyCheck;
  Node.Index:=NewIndex;
  CHeaderFilesCheckTreeView.ConsistencyCheck;
end;

function TH2PasDialog.GetFileNodeStateIndex(aFile: TH2PasFile): Integer;
begin
  if aFile=nil then
    Result:=0
  else if aFile.Enabled then
    Result:=1
  else
    Result:=0;
end;

procedure TH2PasDialog.MarkAllCHeadersExceptCurrentToMerge;
var
  CurFile: TH2PasFile;
  i: Integer;
  OtherFile: TH2PasFile;
begin
  if Project=nil then exit;
  CurFile:=GetCurrentCHeaderFile;
  if CurFile=nil then exit;
  for i:=0 to Project.CHeaderFileCount-1 do begin
    OtherFile:=Project.CHeaderFiles[i];
    OtherFile.Merge:=OtherFile<>CurFile;
  end;
  UpdateFileInfo;
end;

procedure TH2PasDialog.UpdateFilesPage(ScanIncludes: boolean);
var
  i: Integer;
  CurFile: TH2PasFile;
  OldSelection: TStringList;
  s: String;
  OldExpandedState: TTreeNodeExpandedState;
  Node: TTreeNode;
  OldSelected: String;
  j: Integer;
begin
  if ScanIncludes and (Project<>nil) then
    Project.ReadAllCIncludes(false);
  CHeaderFilesCheckTreeView.BeginUpdate;
  OldSelection:=nil;
  OldExpandedState:=TTreeNodeExpandedState.Create(CHeaderFilesCheckTreeView);
  try
    // remember old selection
    OldSelected:='';
    if CHeaderFilesCheckTreeView.Selected<>nil then
      OldSelected:=CHeaderFilesCheckTreeView.Selected.Text;
    OldSelection:=TStringList.Create;
    Node:=CHeaderFilesCheckTreeView.GetFirstMultiSelected;
    while Node<>nil do begin
      //DebugLn(['TH2PasDialog.UpdateFilesPage Node.Text="',Node.Text,'" Index=',Node.Index]);
      OldSelection.Add(Node.GetTextPath);
      Node:=Node.GetNextMultiSelected;
    end;
    
    // clear
    CHeaderFilesCheckTreeView.Items.Clear;
    
    // fill
    for i:=0 to Project.CHeaderFileCount-1 do begin
      CurFile:=Project.CHeaderFiles[i];
      s:=Project.ShortenFilename(CurFile.Filename);
      if CurFile.CIncludedByCount>0 then
        s:=s+' (included by '+IntToStr(CurFile.CIncludedByCount)+')';
      Node:=CHeaderFilesCheckTreeView.Items.Add(nil,s);
      Node.MultiSelected:=OldSelection.IndexOf(Node.GetTextPath)>=0;
      Node.Selected:=Node.Text=OldSelected;
      Node.StateIndex:=GetFileNodeStateIndex(CurFile);
      for j:=0 to CurFile.CIncludeCount-1 do begin

      end;
    end;

    // restore expanded state
    OldExpandedState.Apply(CHeaderFilesCheckTreeView);
  finally
    OldExpandedState.Free;
    OldSelection.Free;
    CHeaderFilesCheckTreeView.EndUpdate;
  end;

  UpdateFileInfo;
end;

procedure TH2PasDialog.UpdateH2PasPage;

  procedure Check(const Option: string; NewValue: boolean);
  var
    i: Integer;
    s: string;
  begin
    for i:=0 to h2pasOptionsCheckGroup.Items.Count-1 do begin
      s:=h2pasOptionsCheckGroup.Items[i];
      if copy(s,1,length(Option)+1)=Option+' ' then
        h2pasOptionsCheckGroup.Checked[i]:=NewValue;
    end;
  end;

begin
  Check('-d',Project.UseExternal);
  Check('-D',Project.UseExternalLibname);
  Check('-e',Project.ConstantsInsteadOfEnums);
  Check('-c',Project.CompactOutputmode);
  Check('-i',Project.CreateIncludeFile);
  Check('-p',Project.PforPointers);
  Check('-pr',Project.PackAllRecords);
  Check('-P',Project.UseProcVarsForImport);
  Check('-s',Project.StripComments);
  Check('-S',Project.StripCommentsAndInfo);
  Check('-t',Project.TforTypedefs);
  Check('-T',Project.TforTypedefsRemoveUnderscore);
  Check('-v',Project.VarParams);
  Check('-w',Project.Win32Header);
  Check('-x',Project.PalmOSSYSTrap);
  Check('-C',Project.UseCTypes);
  
  LibnameEdit.Text:=Project.Libname;
  OutputExtEdit.Text:=Project.OutputExt;
  OutputDirEdit.Text:=Project.OutputDirectory;
end;

procedure TH2PasDialog.UpdateConvertPage;
begin
  ClearMessages;
  PreH2PasEdit.ListOfTools:=Project.PreH2PasTools;
  PreH2PasEdit.UpdateAll;
  PostH2PasEdit.ListOfTools:=Project.PostH2PasTools;
  PostH2PasEdit.UpdateAll;
  //DebugLn(['TH2PasDialog.UpdateConvertPage PreH2PasEdit.ListOfTools=',PreH2PasEdit.ListOfTools.COmponentCount]);
end;

procedure TH2PasDialog.UpdateSettingsPage;
begin
  H2PasFilenameEdit.Text:=Converter.h2pasFilename;
  OpenLastProjectOnStartCheckBox.Checked:=Converter.AutoOpenLastProject;
end;

function TH2PasDialog.ShowSelectDirDialog(const Title: string;
  var ADirectory: string): boolean;
var
  SelectDirDialog: TSelectDirectoryDialog;
begin
  Result:=false;
  SelectDirDialog:=TSelectDirectoryDialog.Create(nil);
  try
    InitIDEFileDialog(SelectDirDialog);
    SelectDirDialog.Title:=Title;
    if (ADirectory<>'')
    and (FilenameIsAbsolute(ADirectory)) then
      SelectDirDialog.InitialDir:=ADirectory;
    if SelectDirDialog.Execute then begin
      ADirectory:=SelectDirDialog.FileName;
      Result:=true;
    end;
  finally
    StoreIDEFileDialog(SelectDirDialog);
    SelectDirDialog.Free;
  end;
end;

function TH2PasDialog.ShowOpenFileDialog(const Title: string;
  var AFilename: string): boolean;
var
  OpenDialog: TOpenDialog;
begin
  Result:=false;
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Title:=Title;
    if (AFilename<>'')
    and (FilenameIsAbsolute(AFilename)) then
      OpenDialog.InitialDir:=ExtractFilePath(AFilename);
    if AFilename<>'' then
      OpenDialog.Filename:=ExtractFileName(AFilename);
    if OpenDialog.Execute then begin
      AFilename:=OpenDialog.FileName;
      Result:=true;
    end;
  finally
    StoreIDEFileDialog(OpenDialog);
    OpenDialog.Free;
  end;
end;

function TH2PasDialog.OnIDESavedAll(Sender: TObject): TModalResult;
begin
  Result:=SaveSettings;
end;

function TH2PasDialog.Convert: TModalResult;
begin
  Result:=mrCancel;

  if not Project.HasEnabledFiles then begin
    IDEMessageDialog('Nothing to do',
      'Please enable at least one c header file that is not merged.',
      mtInformation,[mbOk],'');
    Result:=mrOK;
    exit;
  end;
  
  // save settings
  if SaveSettings<>mrOk then begin
    DebugLn(['TH2PasDialog.Convert SaveSettings failed']);
    exit;
  end;
  
  // save IDE files
  if LazarusIDE.DoSaveProject([])<>mrOK then begin
    DebugLn(['TH2PasDialog.Convert LazarusIDE.DoSaveProject failed']);
    exit;
  end;
  LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
  
  Result:=Converter.Execute;
  if Result<>mrOk then begin
    ShowH2PasError(-1);
  end;
end;

procedure TH2PasDialog.ShowH2PasError(MsgLine: integer);
begin
  if MsgLine<0 then
    MsgLine:=Converter.FindH2PasErrorMessage;
  if (MsgLine<0) or (MsgLine>=IDEMessagesWindow.LinesCount) then begin
    exit;
  end;
  
  LazarusIDE.DoJumpToCompilerMessage(MsgLine,true);
end;

function TH2PasDialog.SaveSettings: TModalResult;
begin
  Result:=SaveGlobalSettings;
  if Result<>mrOk then exit;
  if (Project<>nil) and Project.Modified then
    Result:=SaveProject('',[]);
end;

function TH2PasDialog.SaveGlobalSettings: TModalResult;
var
  Config: TConfigStorage;
begin
  Result:=mrCancel;
  try
    Config:=GetIDEConfigStorage('h2pastool.xml',false);
    try
      Converter.WindowBounds:=BoundsRect;
      Converter.Save(Config);
      Config.WriteToDisk;
    finally
      Config.Free;
    end;
    Result:=mrOk;
  except
    on E: Exception do begin
      MessageDlg('Write error',
        'Error writing global config:'#13
        +E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

function TH2PasDialog.LoadGlobalSettings: TModalResult;
var
  Config: TConfigStorage;
begin
  Result:=mrCancel;
  try
    Config:=GetIDEConfigStorage('h2pastool.xml',true);
    try
      Converter.Load(Config);
      if (Converter.WindowBounds.Left>-10)
      and (Converter.WindowBounds.Right<Screen.Width+10)
      and (Converter.WindowBounds.Top>-10)
      and (Converter.WindowBounds.Bottom<Screen.Height+10)
      and (Converter.WindowBounds.Right-Converter.WindowBounds.Left>100)
      and (Converter.WindowBounds.Bottom-Converter.WindowBounds.Top>50)
      then
        BoundsRect:=Converter.WindowBounds;
      UpdateSettingsPage;
    finally
      Config.Free;
    end;
    Result:=mrOk;
  except
    on E: Exception do begin
      MessageDlg('Read error',
        'Error reading global config:'#13
        +E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

function TH2PasDialog.SaveProject(const Filename: string; Flags: TSaveFlags
  ): TModalResult;
var
  NewFilename: String;
  SaveDialog: TSaveDialog;
  NewPath: String;
begin
  Result:=mrCancel;
  if Project=nil then exit(mrOk);
  NewFilename:=Filename;
  if NewFilename='' then
    NewFilename:=Project.Filename;
    
  // choose a filename
  if (not FilenameIsAbsolute(NewFilename))
  or (sfSaveAs in Flags) then begin
    SaveDialog:=TSaveDialog.Create(nil);
    try
      InitIDEFileDialog(SaveDialog);
      SaveDialog.Title:='Save project as ... (*.h2p)';
      //choose a nice default name
      if NewFilename='' then
        NewFilename:='project1.h2p';
      SaveDialog.FileName:=NewFilename;
      SaveDialog.Filter:='h2pas project (*.h2p)|*.h2p|All files (*.*)|'+FileMask;
      NewPath:=ExtractFilePath(NewFilename);
      if NewPath<>'' then
        SaveDialog.InitialDir:=NewPath;
      if (not SaveDialog.Execute) then exit;
      NewFilename:=SaveDialog.FileName;
      if NewFilename='' then exit;
      // append default file extension
      if ExtractFileExt(NewFilename)='' then
        NewFilename:=NewFilename+'.h2p';
      // warn if overwriting
      if FileExistsUTF8(NewFilename) then begin
        if QuestionDlg('Replace file?',
          'The file "'+NewFilename+'"'#13
          +'already exists.',
          mtConfirmation,[mrOk,'Overwrite',mrCancel,'Cancel'],0)<>mrOk
        then exit;
      end;
    finally
      StoreIDEFileDialog(SaveDialog);
      SaveDialog.Free;
    end;
  end;
  
  // save
  try
    DebugLn(['TH2PasDialog.SaveProject saving project']);
    Converter.SaveProject(NewFilename);
    Result:=mrOk;
  except
    on E: Exception do begin
      MessageDlg('Write error',
        'Error writing global config:'#13
        +E.Message,mtError,[mbCancel],0);
    end;
  end;
  UpdateCaption;
end;

function TH2PasDialog.OpenProject(const Filename: string; Flags: TOpenFlags
  ): TModalResult;
var
  NewFilename: String;
begin
  Result:=mrCancel;
  NewFilename:=ExpandFileNameUTF8(TrimFilename(Filename));
  if not FileExistsUTF8(NewFilename) then begin
    if ofOnlyIfExists in Flags then begin
      MessageDlg('File not found',
        'File not found: "'+NewFilename+'"',mtError,[mbCancel],0);
      exit;
    end;
  end;
  
  // create new project
  if Project=nil then
    Converter.Project:=TH2PasProject.Create;

  if FileExistsUTF8(NewFilename) then begin
    // load
    try
      Converter.LoadProject(NewFilename);
      Result:=mrOk;
    except
      on E: Exception do begin
        MessageDlg('Read error',
          'Error reading project from file'#13
          +NewFilename+#13
          +#13
          +E.Message,mtError,[mbCancel],0);
      end;
    end;
  end else begin
    // new project
    Project.Clear(true);
    Converter.CurrentProjectFilename:=NewFilename;
    Project.Filename:=NewFilename;
  end;
  
  UpdateProjectChanged(true);
end;

initialization
  {$I h2pasdlg.lrs}

end.


