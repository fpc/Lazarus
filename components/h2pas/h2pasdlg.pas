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
  Graphics, Dialogs, ComCtrls, Buttons, StdCtrls, ExtCtrls, CheckLst,
  LazConfigStorage,
  SynEdit, SynHighlighterCPP,
  FileProcs,
  IDEMsgIntf, MenuIntf, IDECommands, BaseIDEIntf, IDEDialogs, LazIDEIntf,
  CodeToolManager, SrcEditorIntf, IDETextConverter,
  H2PasStrConsts, H2PasConvert, IDETextConvListEdit;

type

  { TH2PasDialog }

  TH2PasDialog = class(TForm)
    FileInfoGroupBox: TGroupBox;
    FileInfoLabel: TLabel;
    MainPageControl: TPageControl;

    // c header files
    FilesTabSheet: TTabSheet;
    CHeaderFilesSplitter1: TSplitter;
    AddCHeaderFilesButton: TButton;
    UnselectAllCHeaderFilesButton: TButton;
    SelectAllCHeaderFilesButton: TButton;
    DeleteCHeaderFilesButton: TButton;
    CHeaderFilesCheckListBox: TCheckListBox;

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

    procedure AddCHeaderFilesButtonClick(Sender: TObject);
    procedure CHeaderFilesCheckListBoxClick(Sender: TObject);
    procedure CHeaderFilesCheckListBoxItemClick(Sender: TObject; Index: LongInt
      );
    procedure CloseButtonClick(Sender: TObject);
    procedure ConstantsInsteadOfEnumsCheckBoxChange(Sender: TObject);
    procedure ConvertButtonClick(Sender: TObject);
    procedure DeleteCHeaderFilesButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure H2PasFilenameEditEditingDone(Sender: TObject);
    procedure LibnameEditEditingDone(Sender: TObject);
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
    procedure SelectAllCHeaderFilesButtonClick(Sender: TObject);
    procedure UnselectAllCHeaderFilesButtonClick(Sender: TObject);
    procedure h2pasFilenameBrowseButtonClick(Sender: TObject);
    procedure h2pasOptionsCheckGroupItemClick(Sender: TObject; Index: LongInt);
    procedure OnShowSrcEditSection(Sender: TObject);
    procedure OnAddSearchAndReplaceBeforeH2PasClick(Sender: TObject);
  private
    FConverter: TH2PasConverter;
    FSrcEditAddSearchReplaceMenuItem: TIDEMenuCommand;
    FSrcEditSection: TIDEMenuSection;
    
    function GetProject: TH2PasProject;
    
    procedure UpdateAll;
    procedure UpdateProjectChanged; // show project settings
    procedure UpdateCaption;
    procedure UpdateFileInfo;
    procedure ClearMessages;
    procedure CreateLazarusMenuItems;
    function GetCurrentCHeaderFile: TH2PasFile;

    // project settings
    procedure UpdateFilesPage;
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
  TextConverterToolClasses.RegisterClass(TRemoveCPlusPlusExternCTool);
  TextConverterToolClasses.RegisterClass(TRemoveEmptyCMacrosTool);
  TextConverterToolClasses.RegisterClass(TReplaceEdgedBracketPairWithStar);
  TextConverterToolClasses.RegisterClass(TReplaceMacro0PointerWithNULL);
  TextConverterToolClasses.RegisterClass(TReplaceUnitFilenameWithUnitName);
  TextConverterToolClasses.RegisterClass(TRemoveSystemTypes);
end;

{ TH2PasDialog }

procedure TH2PasDialog.FormCreate(Sender: TObject);
begin
  Caption:=h2pCHeaderFileConverter;
  FilesTabSheet.Caption:='C header files';
    AddCHeaderFilesButton.Caption:='Add .h files ...';
    DeleteCHeaderFilesButton.Caption:='Delete selected .h files';
    SelectAllCHeaderFilesButton.Caption:='Enable all .h files';
    UnselectAllCHeaderFilesButton.Caption:='Disable all .h files';
    FileInfoGroupBox.Caption:='File information';
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
  ConvertButton.Caption:='Run converter and h2pas';
  CloseButton.Caption:='&Close';
  
  PreH2PasEdit:=TTextConvListEditor.Create(Self);
  with PreH2PasEdit do begin
    Name:='PreH2PasEdit';
    Align:=alClient;
    OnModified:=@PreH2PasEditModified;
    Parent:=PreH2PasGroupBox;
    Visible:=true;// Note: it's a form, and visible default is false
  end;

  PostH2PasEdit:=TTextConvListEditor.Create(Self);
  with PostH2PasEdit do begin
    Name:='PostH2PasEdit';
    Align:=alClient;
    OnModified:=@PostH2PasEditModified;
    Parent:=PostH2PasGroupBox;
    Visible:=true;// Note: it's a form, and visible default is false
  end;

  LazarusIDE.AddHandlerOnSavedAll(@OnIDESavedAll);
  CreateLazarusMenuItems;

  // create converter
  FConverter:=TH2PasConverter.Create;
  LoadGlobalSettings;
  
  // create project
  if Converter.AutoOpenLastProject
  and FileExists(Converter.CurrentProjectFilename) then
    OpenProject(Converter.CurrentProjectFilename,[]);
  if Project=nil then begin
    Converter.Project:=TH2PasProject.Create;
    PreH2PasEdit.ListOfTools:=Project.PreH2PasTools;
    PostH2PasEdit.ListOfTools:=Project.PostH2PasTools;
  end;

  UpdateAll;
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

procedure TH2PasDialog.ConvertButtonClick(Sender: TObject);
begin
  Convert;
end;

procedure TH2PasDialog.DeleteCHeaderFilesButtonClick(Sender: TObject);
var
  DeleteFiles: TStringList;
  Target: TStrings;
  i: Integer;
begin
  DeleteFiles:=TStringList.Create;
  Target:=CHeaderFilesCheckListBox.Items;
  for i:=0 to Target.Count-1 do begin
    if CHeaderFilesCheckListBox.Selected[i] then
      DeleteFiles.Add(Project.LongenFilename(Target[i]));
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
    UpdateFilesPage;
  end;
  DeleteFiles.Free;
end;

procedure TH2PasDialog.AddCHeaderFilesButtonClick(Sender: TObject);
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
      UpdateFilesPage;
    end;
  finally
    StoreIDEFileDialog(OpenDialog);
    OpenDialog.Free;
  end;
end;

procedure TH2PasDialog.CHeaderFilesCheckListBoxClick(Sender: TObject);
begin
  UpdateFileInfo;
end;

procedure TH2PasDialog.CHeaderFilesCheckListBoxItemClick(Sender: TObject;
  Index: LongInt);
var
  AFile: TH2PasFile;
begin
  AFile:=GetCurrentCHeaderFile;
  if AFile<>nil then
    AFile.Enabled:=CHeaderFilesCheckListBox.Checked[Index];
end;

procedure TH2PasDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fSrcEditSection);
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
  Project.Libname:=LibnameEdit.Text;
end;

procedure TH2PasDialog.NewSettingsButtonClick(Sender: TObject);
begin
  Project.Filename:='';
  Project.Clear(true);
  UpdateAll;
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

procedure TH2PasDialog.SelectAllCHeaderFilesButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to CHeaderFilesCheckListBox.Items.Count-1 do
    CHeaderFilesCheckListBox.Selected[i]:=true;
end;

procedure TH2PasDialog.UnselectAllCHeaderFilesButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to CHeaderFilesCheckListBox.Items.Count-1 do
    CHeaderFilesCheckListBox.Selected[i]:=false;
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
  SrcEdit:=SourceEditorWindow.ActiveEditor;
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
  SrcEdit:=SourceEditorWindow.ActiveEditor;
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

procedure TH2PasDialog.UpdateAll;
begin
  UpdateCaption;
  UpdateFilesPage;
  UpdateH2PasPage;
  UpdateConvertPage;
  UpdateSettingsPage;
end;

procedure TH2PasDialog.UpdateProjectChanged;
begin
  UpdateCaption;
  UpdateFilesPage;
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
begin
  AFile:=GetCurrentCHeaderFile;
  if AFile<>nil then begin
    s:='File: '+AFile.Filename;
    if not FileExistsCached(AFile.Filename) then
      s:=s+#13+'ERROR: file not found';
    FileInfoLabel.Caption:=s;
  end else begin
    FileInfoLabel.Caption:='No file selected.';
  end;
end;

procedure TH2PasDialog.ClearMessages;
begin
  IDEMessagesWindow.Clear;
end;

procedure TH2PasDialog.CreateLazarusMenuItems;
begin
  // add a context menu to the source editor
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

function TH2PasDialog.GetCurrentCHeaderFile: TH2PasFile;
var
  Index: LongInt;
  AFilename: String;
begin
  Result:=nil;
  Index:=CHeaderFilesCheckListBox.ItemIndex;
  if Index<0 then exit;
  AFilename:=Project.LongenFilename(CHeaderFilesCheckListBox.Items[Index]);
  Result:=Project.CHeaderFileWithFilename(AFilename);
end;

procedure TH2PasDialog.UpdateFilesPage;
var
  i: Integer;
  Target: TStrings;
  CurFile: TH2PasFile;
  OldSelected: TStringList;
  s: String;
begin
  Target:=CHeaderFilesCheckListBox.Items;
  Target.BeginUpdate;
  // remember old selection
  OldSelected:=TStringList.Create;
  for i:=0 to Target.Count-1 do begin
    if CHeaderFilesCheckListBox.Selected[i] then
      OldSelected.Add(Target[i]);
  end;
  // replace items in CHeaderFilesCheckListBox and restore selection
  for i:=0 to Project.CHeaderFileCount-1 do begin
    CurFile:=Project.CHeaderFiles[i];
    s:=Project.ShortenFilename(CurFile.Filename);
    if Target.Count>i then
      Target[i]:=s
    else
      Target.Add(s);
    CHeaderFilesCheckListBox.Checked[i]:=CurFile.Enabled;
    CHeaderFilesCheckListBox.Selected[i]:=
                                       OldSelected.IndexOf(CurFile.Filename)>=0;
  end;
  // remove unneeded item
  while Target.Count>Project.CHeaderFileCount do
    Target.Delete(Target.Count-1);
  Target.EndUpdate;
  OldSelected.Free;
  
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
      'No c header file is enabled, so nothing to do.',
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
  LazarusIDE.SaveSourceEditorChangesToCodeCache(-1);
  
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
      if FileExists(NewFilename) then begin
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
  NewFilename:=ExpandFileName(TrimFilename(Filename));
  if not FileExists(NewFilename) then begin
    if ofOnlyIfExists in Flags then begin
      MessageDlg('File not found',
        'File not found: "'+NewFilename+'"',mtError,[mbCancel],0);
      exit;
    end;
  end;
  
  // create new project
  if Project=nil then
    Converter.Project:=TH2PasProject.Create;

  if FileExists(NewFilename) then begin
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
  
  UpdateProjectChanged;
end;

initialization
  {$I h2pasdlg.lrs}

end.


