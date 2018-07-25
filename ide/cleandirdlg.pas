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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    A dialog for cleaning directories.
}
unit CleanDirDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr,
  // LCL
  LCLProc, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  // LazUtils
  FileUtil, LazFileUtils, Laz2_XMLCfg, LazStringUtils,
  // IdeIntf
  IDEWindowIntf, IDEHelpIntf, IDEDialogs,
  // IDE
  IDEProcs, LazarusIDEStrConsts, LazConf, TransferMacros, InputHistory,
  ShowDeletingFilesDlg;

type

  { TCleanDirectoryDialog }

  TCleanDirectoryDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    DirBrowseButton: TButton;
    KeepTextFilesCheckbox: TCheckBox;
    SubDirsCheckbox: TCheckBox;
    SimpleSyntaxKeepCheckbox: TCheckBox;
    KeepCombobox: TComboBox;
    KeepGroupbox: TGroupBox;
    SimpleSyntaxRemoveCheckbox: TCheckBox;
    RemoveCombobox: TComboBox;
    DirCombobox: TComboBox;
    DirGroupbox: TGroupBox;
    RemoveGroupbox: TGroupBox;
    procedure CleanDirectoryDialogCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure DirBrowseButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    FMacros: TTransferMacroList;
    procedure SetMacros(const AValue: TTransferMacroList);
  public
    procedure LoadSettings;
    procedure SaveSettings;
    function GetConfigFilename: string;
    function SearchFilesToDelete(var List: TStrings): boolean;
    function DeleteFiles(List: TStrings): boolean;
    property Macros: TTransferMacroList read FMacros write SetMacros;
  end;
  
function ShowCleanDirectoryDialog(const DefaultDirectory: string;
  Macros: TTransferMacroList): TModalResult;

implementation

{$R *.lfm}

const
  CleanDirXMLFilename = 'cleandirectorydialog.xml';
  CleanDirXMLVersion = 1;

function ShowCleanDirectoryDialog(const DefaultDirectory: string;
  Macros: TTransferMacroList): TModalResult;
var
  CleanDirectoryDialog: TCleanDirectoryDialog;
begin
  CleanDirectoryDialog:=TCleanDirectoryDialog.Create(nil);
  CleanDirectoryDialog.Macros:=Macros;
  CleanDirectoryDialog.LoadSettings;
  AddToRecentList(DefaultDirectory,CleanDirectoryDialog.DirCombobox.Items,20,rltFile);
  CleanDirectoryDialog.DirComboBox.ItemIndex:=0;
  CleanDirectoryDialog.DirComboBox.Text:=DefaultDirectory;
  Result:=CleanDirectoryDialog.ShowModal;
  CleanDirectoryDialog.Free;
end;

{ TCleanDirectoryDialog }

procedure TCleanDirectoryDialog.OkButtonClick(Sender: TObject);
var
  List: TStrings;
begin
  ModalResult:=mrNone;
  SaveSettings;
  List:=nil;
  try
    if not SearchFilesToDelete(List) then exit;
    if not DeleteFiles(List) then exit;
  finally
    List.Free;
  end;
  ModalResult:=mrOk;
end;

procedure TCleanDirectoryDialog.SetMacros(const AValue: TTransferMacroList);
begin
  if FMacros=AValue then exit;
  FMacros:=AValue;
end;

procedure TCleanDirectoryDialog.CleanDirectoryDialogCreate(Sender: TObject);
begin
  Caption:=lisClDirCleanDirectory;
  DirGroupbox.Caption:=lisCodeToolsDefsInsertBehindDirectory;
  SubDirsCheckbox.Caption:=lisClDirCleanSubDirectories;
  RemoveGroupbox.Caption:=lisClDirRemoveFilesMatchingFilter;
  SimpleSyntaxRemoveCheckbox.Caption:=lisClDirSimpleSyntaxEGInsteadOf;
  KeepGroupbox.Caption:=lisClDirKeepFilesMatchingFilter;
  SimpleSyntaxKeepCheckbox.Caption:=lisClDirSimpleSyntaxEGInsteadOf;
  KeepTextFilesCheckbox.Caption:=lisClDirKeepAllTextFiles;

  ButtonPanel.OKButton.Caption:=lisClDirClean;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=lisCancel;

  ButtonPanel.OKButton.OnClick := @OKButtonClick;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;

  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TCleanDirectoryDialog.FormDestroy(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TCleanDirectoryDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TCleanDirectoryDialog.DirBrowseButtonClick(Sender: TObject);
var
  NewDirectory: String;
begin
  NewDirectory:=InputHistories.SelectDirectory(lisMenuCleanDirectory, true,
           ExtractFilePath(DirCombobox.Text),ExtractFilename(DirCombobox.Text));
  if NewDirectory<>'' then
    DirCombobox.Text:=NewDirectory;
end;

procedure TCleanDirectoryDialog.LoadSettings;
var
  XMLConfig: TXMLConfig;

  procedure LoadComboList(AComboBox: TComboBox; const Path: string;
    ListType: TRecentListType);
  var
    List: TStringList;
  begin
    List:=TStringList.Create;
    LoadRecentList(XMLConfig,List,Path,ListType);
    AComboBox.Items.Assign(List);
    if AComboBox.Items.Count > 0 then
      AComboBox.ItemIndex := 0;
    List.Free;
  end;
  
  procedure AddStandardComboItem(AComboBox: TComboBox; const Item: string);
  begin
    if AComboBox.Items.IndexOf(Item)>=0 then exit;
    AComboBox.Items.Add(Item);
    AComboBox.ItemIndex:=0;
  end;

var
  Filename: String;
  Path: String;
begin
  try
    Filename:=GetConfigFilename;
    XMLConfig:=TXMLConfig.Create(Filename);
  except
    DebugLn('ERROR: unable to open clean directory options "',Filename,'"');
    exit;
  end;
  try
    try
      Path:='CleanDirectoryOptions/';
      //FileVersion:=XMLConfig.GetValue(Path+'Version/Value',0);
      
      SubDirsCheckbox.Checked:=XMLConfig.GetValue(
                                             Path+'SubDirectories/Value',false);
      LoadComboList(DirCombobox,Path+'Directories',rltFile);
      LoadComboList(RemoveCombobox,Path+'RemoveFilters',rltFile);
      SimpleSyntaxRemoveCheckbox.Checked:=XMLConfig.GetValue(
                                         Path+'RemoveFilter/SimpleSyntax',true);
      LoadComboList(KeepCombobox,Path+'KeepFilters',rltFile);
      SimpleSyntaxKeepCheckbox.Checked:=XMLConfig.GetValue(
                                           Path+'KeepFilter/SimpleSyntax',true);
      KeepTextFilesCheckbox.Checked:=XMLConfig.GetValue(
                                               Path+'KeepTextFiles/Value',true);

      // set defaults
      AddStandardComboItem(DirCombobox,'$(ProjPath)');
      AddStandardComboItem(RemoveCombobox,'*.(bak|ppu|ppl|o|or|a|so|dll)');
      AddStandardComboItem(RemoveCombobox,'*.bak|*~');
      AddStandardComboItem(KeepCombobox,
                           '*.(pas|pp|lpr|lfm|lrs|lpi|lpk|inc|sh|xml)');

    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: unable to read clean directory options from "',
        Filename,'": ',E.Message);
    end;
  end;
end;

procedure TCleanDirectoryDialog.SaveSettings;
var
  XMLConfig: TXMLConfig;
  Filename: String;
  Path: String;
begin
  AddComboTextToRecentList(DirCombobox, 20,rltFile);
  AddComboTextToRecentList(RemoveCombobox, 20,rltFile);
  AddComboTextToRecentList(KeepCombobox, 20,rltFile);
  try
    InvalidateFileStateCache;
    Filename:=GetConfigFilename;
    XMLConfig:=TXMLConfig.CreateClean(Filename);
  except
    DebugLn('ERROR: unable to open clean directory options "',Filename,'"');
    exit;
  end;
  try
    try
      Path:='CleanDirectoryOptions/';
      XMLConfig.SetValue(Path+'Version/Value',CleanDirXMLVersion);

      XMLConfig.SetDeleteValue(Path+'SubDirectories/Value',
                               SubDirsCheckbox.Checked,false);
      SaveRecentList(XMLConfig,DirCombobox.Items,Path+'Directories');
      SaveRecentList(XMLConfig,RemoveCombobox.Items,Path+'RemoveFilters');
      XMLConfig.SetDeleteValue(Path+'RemoveFilter/SimpleSyntax',
                               SimpleSyntaxRemoveCheckbox.Checked,true);
      SaveRecentList(XMLConfig,KeepCombobox.Items,Path+'KeepFilters');
      XMLConfig.SetDeleteValue(Path+'KeepFilter/SimpleSyntax',
                               SimpleSyntaxKeepCheckbox.Checked,true);
      XMLConfig.SetDeleteValue(Path+'KeepTextFiles/Value',
                               KeepTextFilesCheckbox.Checked,true);

      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ERROR: unable to write clean directory options to "',
        Filename,'": ',E.Message);
    end;
  end;
end;

function TCleanDirectoryDialog.GetConfigFilename: string;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+CleanDirXMLFilename;
end;

function TCleanDirectoryDialog.SearchFilesToDelete(var List: TStrings): boolean;
var
  RemoveFilterRegExpr: TRegExpr;
  KeepFilterRegExpr: TRegExpr;

  function FileMatches(const Filename: string): boolean;
  var
    ShortFilename: String;
  begin
    Result:=false;
    ShortFilename:=ExtractFilename(Filename);
    if (RemoveFilterRegExpr=nil)
    or not RemoveFilterRegExpr.Exec(ExtractFilename(ShortFilename)) then exit;
    if (KeepFilterRegExpr<>nil)
    and KeepFilterRegExpr.Exec(ExtractFilename(ShortFilename)) then exit;
    if KeepTextFilesCheckbox.Checked and FileIsText(Filename) then exit;
    Result:=true;
  end;

  function SearchInDirectory(const MainDirectory: string;
    Lvl: integer): boolean;
  var
    FileInfo: TSearchRec;
    FullFilename: String;
  begin
    Result:=false;
    if (not DirPathExists(MainDirectory)) or (Lvl>20) then exit;
    if FindFirstUTF8(MainDirectory+GetAllFilesMask,
                          faAnyFile,FileInfo)=0
    then begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
        then continue;
        FullFilename:=MainDirectory+FileInfo.Name;
        if (FileInfo.Attr and faDirectory)>0 then begin
          if SubDirsCheckbox.Checked then begin
            // search recursively
            if not SearchInDirectory(AppendPathDelim(FullFilename),Lvl+1) then
              break;
          end;
        end else begin
          if FileMatches(FullFilename) then
            List.Add(FullFilename);
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
    Result:=true;
  end;
  
  function SetupFilter(var Filter: TRegExpr; SimpleSyntax: boolean;
    const FilterAsText: string): boolean;
  var
    Expr: String;
    s: String;
  begin
    Result:=false;
    if FilterAsText='' then begin
      Filter:=nil;
      Result:=true;
      exit;
    end;
    Filter:=TRegExpr.Create;
    if SimpleSyntax then
      Expr:=SimpleSyntaxToRegExpr(FilterAsText)
    else
      Expr:=FilterAsText;
    try
      Filter.Expression:=Expr;
      // do a simple test
      Filter.Exec('test.file');
      Result:=true;
    except
      on E: Exception do begin
        if SimpleSyntax then
          s:=Format(lisTheFileMaskIsInvalid, [FilterAsText])
        else
          s:=Format(lisTheFileMaskIsNotAValidRegularExpression, [FilterAsText]);
        IDEMessageDialog(lisInvalidMask, s, mtError, [mbCancel]);
      end;
    end;
  end;

var
  Directory: String;
begin
  Result:=false;
  RemoveFilterRegExpr:=nil;
  KeepFilterRegExpr:=nil;
  List:=nil;

  try
    // get directory
    Directory:=DirCombobox.Text;
    if (Macros<>nil) and (not Macros.SubstituteStr(Directory)) then exit;
    Directory:=AppendPathDelim(Directory);

    // setup filters
    if not SetupFilter(RemoveFilterRegExpr,SimpleSyntaxRemoveCheckbox.Checked,
      RemoveCombobox.Text) then exit;
    if not SetupFilter(KeepFilterRegExpr,SimpleSyntaxKeepCheckbox.Checked,
      KeepCombobox.Text) then exit;

    // search files
    List:=TStringList.Create;
    if not SearchInDirectory(Directory,0) then exit;

    Result:=true;
  finally
    RemoveFilterRegExpr.Free;
    KeepFilterRegExpr.Free;
    if not Result then begin
      List.Free;
      List:=nil;
    end;
  end;
end;

function TCleanDirectoryDialog.DeleteFiles(List: TStrings): boolean;
var
  i: Integer;
  Filename: string;
  MsgResult: TModalResult;
  ShowDeletingFilesDialog: TShowDeletingFilesDialog;
begin
  Result:=false;
  if List.Count=0 then begin
    Result:=true;
    exit;
  end;
  
  // ask user for confirmation
  ShowDeletingFilesDialog:=TShowDeletingFilesDialog.Create(Self);
  try
    ShowDeletingFilesDialog.FileList.Items.AddStrings(List);
    for i := 0 to ShowDeletingFilesDialog.FileList.Count - 1 do
      ShowDeletingFilesDialog.FileList.Checked[i] := True;

    if ShowDeletingFilesDialog.ShowModal<>mrOk then exit;

    // delete all checked files
    for i:=0 to ShowDeletingFilesDialog.FileList.Count-1 do begin
      if ShowDeletingFilesDialog.FileList.Checked[i] then
      begin
        Filename:=ShowDeletingFilesDialog.FileList.Items[i];
        DebugLn('TCleanDirectoryDialog: Deleting file ',Filename);
        if FileExistsUTF8(Filename) then begin
          repeat
            if DeleteFileUTF8(Filename) then begin
              break;
            end else begin
              MsgResult:=MessageDlg(lisErrorDeletingFile,
                Format(lisPkgMangUnableToDeleteFile, [Filename]),
                mtError,[mbAbort,mbIgnore,mbRetry],0);
              if (MsgResult=mrIgnore) then break;
              if MsgResult=mrAbort then exit;
            end;
          until false;
        end;
      end;
    end;

  finally
    ShowDeletingFilesDialog.Free;
  end;

  Result:=true;
end;

end.

