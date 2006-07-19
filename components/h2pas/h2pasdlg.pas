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
  Classes, SysUtils, LCLProc, LCLType, LResources, Forms, Controls, Graphics,
  Dialogs, ComCtrls, Buttons, StdCtrls, ExtCtrls, CheckLst, SynEdit,
  LazConfigStorage, FileUtil,
  MenuIntf, IDECommands, BaseIDEIntf, LazIDEIntf,
  H2PasStrConsts, H2PasConvert;

type

  { TH2PasDialog }

  TH2PasDialog = class(TForm)
    h2pasFilenameBrowseButton: TButton;
    H2PasFilenameEdit: TEdit;
    H2PasFilenameLabel: TLabel;
    OutputDirBrowseButton: TButton;
    MainPageControl: TPageControl;

    // c header files
    FilesTabSheet: TTabSheet;
    CHeaderFilesSplitter1: TSplitter;
    AddCHeaderFilesButton: TButton;
    UnselectAllCHeaderFilesButton: TButton;
    SelectAllCHeaderFilesButton: TButton;
    DeleteCHeaderFilesButton: TButton;
    CHeaderFilesCheckListBox: TCheckListBox;

    // h2pas
    h2pasOptionsTabSheet: TTabSheet;
    ConstantsInsteadOfEnumsCheckBox: TCheckBox;
    IncludeFileCheckBox: TCheckBox;
    LibnameEdit: TEdit;
    LibNameLabel: TLabel;
    OutputExtEdit: TEdit;
    OutputExtLabel: TLabel;
    PalmOSSYSTrapCheckBox: TCheckBox;
    PforPointersCheckBox: TCheckBox;
    StripCommentsCheckBox: TCheckBox;
    TforTypedefsCheckBox: TCheckBox;
    UseExternalCheckBox: TCheckBox;
    UseExternalLibnameCheckBox: TCheckBox;
    VarParamsCheckBox: TCheckBox;
    Win32HeaderCheckBox: TCheckBox;
    OutputDirEdit: TEdit;
    OutputDirLabel: TLabel;

    // settings
    SettingsTabSheet: TTabSheet;
    SaveSettingsAsButton: TButton;
    OpenLastProjectOnStartCheckBox: TCheckBox;

    // buttons at bottom
    OpenSettingsButton: TButton;
    SaveSettingsButton: TButton;
    CloseButton: TButton;

    procedure AddCHeaderFilesButtonClick(Sender: TObject);
    procedure CHeaderFilesCheckListBoxItemClick(Sender: TObject; Index: LongInt
      );
    procedure CloseButtonClick(Sender: TObject);
    procedure ConstantsInsteadOfEnumsCheckBoxChange(Sender: TObject);
    procedure DeleteCHeaderFilesButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure H2PasFilenameEditEditingDone(Sender: TObject);
    procedure IncludeFileCheckBoxChange(Sender: TObject);
    procedure LibnameEditEditingDone(Sender: TObject);
    procedure OpenLastProjectOnStartCheckBoxChange(Sender: TObject);
    procedure OpenSettingsButtonClick(Sender: TObject);
    procedure OutputDirBrowseButtonClick(Sender: TObject);
    procedure OutputDirEditEditingDone(Sender: TObject);
    procedure OutputExtEditEditingDone(Sender: TObject);
    procedure PalmOSSYSTrapCheckBoxChange(Sender: TObject);
    procedure PforPointersCheckBoxChange(Sender: TObject);
    procedure SaveSettingsAsButtonClick(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
    procedure SelectAllCHeaderFilesButtonClick(Sender: TObject);
    procedure StripCommentsCheckBoxChange(Sender: TObject);
    procedure TforTypedefsCheckBoxChange(Sender: TObject);
    procedure UnselectAllCHeaderFilesButtonClick(Sender: TObject);
    procedure UseExternalCheckBoxChange(Sender: TObject);
    procedure UseExternalLibnameCheckBoxChange(Sender: TObject);
    procedure VarParamsCheckBoxChange(Sender: TObject);
    procedure Win32HeaderCheckBoxChange(Sender: TObject);
    procedure h2pasFilenameBrowseButtonClick(Sender: TObject);
  private
    FConverter: TH2PasConverter;
    function GetProject: TH2PasProject;
    procedure UpdateCaption;
    procedure UpdateFilesPage;
    procedure UpdateH2PasPage;
    procedure UpdateSettingsPage;
    function ShowSelectDirDialog(const Title: string;
                                 var ADirectory: string): boolean;
    function ShowOpenFileDialog(const Title: string;
                                var AFilename: string): boolean;
  public
    function SaveSettings: TModalResult;
    function SaveGlobalSettings: TModalResult;
    function LoadGlobalSettings: TModalResult;
    function SaveProject(const Filename: string; Flags: TSaveFlags): TModalResult;
    function OpenProject(const Filename: string; Flags: TOpenFlags): TModalResult;
    property Converter: TH2PasConverter read FConverter;
    property Project: TH2PasProject read GetProject;
  end; 

var
  H2PasDialog: TH2PasDialog = nil;
  CmdH2PasTool: TIDECommand = nil;

procedure ExecuteH2PasTool(Sender: TObject);


procedure Register;

implementation

procedure ExecuteH2PasTool(Sender: TObject);
begin
  if H2PasDialog<>nil then exit;
  H2PasDialog:=TH2PasDialog.Create(nil);
  try
    H2PasDialog.ShowModal;
  finally
    FreeAndNil(H2PasDialog);
  end;
end;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
begin
  Key := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  Cat:=IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  CmdH2PasTool := RegisterIDECommand(Cat                 ,
         h2pH2Pas, h2pCreateUnitsFromCHeaderFiles, Key, nil, @ExecuteH2PasTool);
  RegisterIDEMenuCommand(itmSecondaryTools, h2pH2PasTool, h2pH2Pas, nil, nil,
                         CmdH2PasTool);
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
  h2pasOptionsTabSheet.Caption:='h2pas';
    ConstantsInsteadOfEnumsCheckBox.Caption:='-e Emit a series of constants instead of an enumeration type for the C enum construct';
    IncludeFileCheckBox.Caption:='-i Create an include file instead of a unit';
    LibNameLabel.Caption:='-l Library name';
    OutputExtLabel.Caption:='Output extension of new file';
    PalmOSSYSTrapCheckBox.Caption:='-x Handle SYS_TRAP of the PalmOS header files';
    PforPointersCheckBox.Caption:='-p Use the letter P in front of pointer type parameters instead of "^"';
    StripCommentsCheckBox.Caption:='-s Strip comments';
    TforTypedefsCheckBox.Caption:='-t Prepend  typedef  type names with the letter T';
    UseExternalCheckBox.Caption:='-d Use external; for all procedures';
    UseExternalLibnameCheckBox.Caption:='-D Use external libname name "func_name" for function and procedure';
    VarParamsCheckBox.Caption:='-v Replace pointer parameters by call by reference parameters';
    Win32HeaderCheckBox.Caption:='-w Handle special win32 macros';
    OutputDirLabel.Caption:='Output directory';
  SettingsTabSheet.Caption:='Settings';
    H2PasFilenameLabel.Caption:='h2pas program path';
    OpenLastProjectOnStartCheckBox.Caption:='Open last settings on start';
    SaveSettingsAsButton.Caption:='Save settings as ...';
  OpenSettingsButton.Caption:='&Open Settings';
  SaveSettingsButton.Caption:='&Save Settings';
  CloseButton.Caption:='&Close';
  

  // create converter
  FConverter:=TH2PasConverter.Create;
  LoadGlobalSettings;
  
  // create project
  if Converter.AutoOpenLastProject
  and FileExists(Converter.CurrentProjectFilename) then
    OpenProject(Converter.CurrentProjectFilename,[]);
  if Project=nil then begin
    Converter.Project:=TH2PasProject.Create;
    UpdateH2PasPage;
  end;

  UpdateCaption;
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
  Project.ConstantsInsteadOfEnums:=ConstantsInsteadOfEnumsCheckBox.Checked;
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
    OpenDialog.Filter:='C header file (*.h)|*.h|All files (*.*)|'+GetAllFilesMask;
    if OpenDialog.Execute then begin
      Project.AddFiles(OpenDialog.Files);
      UpdateFilesPage;
    end;
  finally
    StoreIDEFileDialog(OpenDialog);
    OpenDialog.Free;
  end;
end;

procedure TH2PasDialog.CHeaderFilesCheckListBoxItemClick(Sender: TObject;
  Index: LongInt);
var
  AFilename: string;
  AFile: TH2PasFile;
begin
  if (Index<0) then exit;
  AFilename:=Project.LongenFilename(CHeaderFilesCheckListBox.Items[Index]);
  AFile:=Project.CHeaderFileWithFilename(AFilename);
  if AFile<>nil then
    AFile.Enabled:=CHeaderFilesCheckListBox.Checked[Index];
end;

procedure TH2PasDialog.FormDestroy(Sender: TObject);
begin
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

procedure TH2PasDialog.IncludeFileCheckBoxChange(Sender: TObject);
begin
  Project.CreateIncludeFile:=IncludeFileCheckBox.Checked;
end;

procedure TH2PasDialog.LibnameEditEditingDone(Sender: TObject);
begin
  Project.Libname:=LibnameEdit.Text;
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
    OpenDialog.Filter:='h2pas project (*.h2p)|*.h2p|All files (*.*)|'+GetAllFilesMask;
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

procedure TH2PasDialog.PalmOSSYSTrapCheckBoxChange(Sender: TObject);
begin
  Project.PalmOSSYSTrap:=PalmOSSYSTrapCheckBox.Checked;
end;

procedure TH2PasDialog.PforPointersCheckBoxChange(Sender: TObject);
begin
  Project.PforPointers:=PforPointersCheckBox.Checked;
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

procedure TH2PasDialog.StripCommentsCheckBoxChange(Sender: TObject);
begin
  Project.StripComments:=StripCommentsCheckBox.Checked;
end;

procedure TH2PasDialog.TforTypedefsCheckBoxChange(Sender: TObject);
begin
  Project.TforTypedefs:=TforTypedefsCheckBox.Checked;
end;

procedure TH2PasDialog.UnselectAllCHeaderFilesButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to CHeaderFilesCheckListBox.Items.Count-1 do
    CHeaderFilesCheckListBox.Selected[i]:=false;
end;

procedure TH2PasDialog.UseExternalCheckBoxChange(Sender: TObject);
begin
  Project.UseExternal:=UseExternalCheckBox.Checked;
end;

procedure TH2PasDialog.UseExternalLibnameCheckBoxChange(Sender: TObject);
begin
  Project.UseExternalLibname:=UseExternalLibnameCheckBox.Checked;
end;

procedure TH2PasDialog.VarParamsCheckBoxChange(Sender: TObject);
begin
  Project.VarParams:=VarParamsCheckBox.Checked;
end;

procedure TH2PasDialog.Win32HeaderCheckBoxChange(Sender: TObject);
begin
  Project.Win32Header:=Win32HeaderCheckBox.Checked;
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

function TH2PasDialog.GetProject: TH2PasProject;
begin
  Result:=Converter.Project;
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
end;

procedure TH2PasDialog.UpdateH2PasPage;
begin
  ConstantsInsteadOfEnumsCheckBox.Checked:=Project.ConstantsInsteadOfEnums;
  IncludeFileCheckBox.Checked:=Project.CreateIncludeFile;
  LibnameEdit.Text:=Project.Libname;
  OutputExtEdit.Text:=Project.OutputExt;
  PalmOSSYSTrapCheckBox.Checked:=Project.PalmOSSYSTrap;
  PforPointersCheckBox.Checked:=Project.PforPointers;
  StripCommentsCheckBox.Checked:=Project.StripComments;
  TforTypedefsCheckBox.Checked:=Project.TforTypedefs;
  UseExternalCheckBox.Checked:=Project.UseExternal;
  UseExternalLibnameCheckBox.Checked:=Project.UseExternalLibname;
  VarParamsCheckBox.Checked:=Project.VarParams;
  Win32HeaderCheckBox.Checked:=Project.Win32Header;
  OutputDirEdit.Text:=Project.ShortenFilename(Project.OutputDirectory);
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
      Converter.WindowSize:=Point(Width,Height);
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
      if (Converter.WindowSize.X>50)
      and (Converter.WindowSize.X<Screen.Width)
      and (Converter.WindowSize.Y>50)
      and (Converter.WindowSize.Y<Screen.Height)
      then
        SetBounds(Left,Top,Converter.WindowSize.X,Converter.WindowSize.Y);
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
      SaveDialog.Filter:='h2pas project (*.h2p)|*.h2p|All files (*.*)|'+GetAllFilesMask;
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
    Project.Clear;
    Converter.CurrentProjectFilename:=NewFilename;
    Project.Filename:=NewFilename;
  end;
  
  UpdateCaption;
  UpdateFilesPage;
  UpdateH2PasPage;
end;

initialization
  {$I h2pasdlg.lrs}

end.

