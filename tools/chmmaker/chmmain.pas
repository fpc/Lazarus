unit CHMMain;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Classes, SysUtils, Types,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, Menus, ExtCtrls, EditBtn,
  LazFileUtils, UTF8Process,
  chmsitemap, chmfilewriter;

type

  { TCHMForm }

  TCHMForm = class(TForm)
    AcNew: TAction;
    AcOpen: TAction;
    AcSave: TAction;
    AcSaveAs: TAction;
    AcClose: TAction;
    AcQuitProgram: TAction;
    AcCompile: TAction;
    AcCompileAndView: TAction;
    AcAbout: TAction;
    ActionList: TActionList;
    AddFilesBtn: TButton;
    AutoAddLinksBtn: TButton;
    AddAllBtn: TButton;
    Bevel1: TBevel;
    CompileViewBtn: TButton;
    CompileBtn: TButton;
    DefaultPageCombo: TComboBox;
    ChmFileNameEdit: TFileNameEdit;
    ChmTitleEdit: TEdit;
    CompileTimeOptionsGroupbox: TGroupBox;
    ScanHtmlCheck: TCheckBox;
    CreateSearchableCHMCheck: TCheckBox;
    FilesNoteLabel: TLabel;
    DefaultPageLabel: TLabel;
    CHMFilenameLabel: TLabel;
    OpenDialog2: TOpenDialog;
    RemoveFilesBtn: TButton;
    ChmTitleLabel: TLabel;
    Splitter: TSplitter;
    TOCEditBtn: TButton;
    IndexEditBtn: TButton;
    IndexEdit: TFileNameEdit;
    GroupBox1: TGroupBox;
    FileListBox: TListBox;
    TableOfContentsLabel: TLabel;
    IndexLabel: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    ProjSaveItem: TMenuItem;
    ProjSaveAsItem: TMenuItem;
    MenuItem12: TMenuItem;
    ProjQuitItem: TMenuItem;
    CompileItem: TMenuItem;
    CompileProjItem: TMenuItem;
    CompileOpenBttn: TMenuItem;
    ProjCloseItem: TMenuItem;
    MenuItem3: TMenuItem;
    HelpAboutItem: TMenuItem;
    ProjNewItem: TMenuItem;
    ProjOpenItem: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    MainPanel: TPanel;
    Panel2: TPanel;
    SaveDialog1: TSaveDialog;
    StatusBar: TStatusBar;
    TOCEdit: TFileNameEdit;
    procedure AcAboutExecute(Sender: TObject);
    procedure AcCloseExecute(Sender: TObject);
    procedure AcCompileAndViewExecute(Sender: TObject);
    procedure AcCompileExecute(Sender: TObject);
    procedure AcNewExecute(Sender: TObject);
    procedure AcOpenExecute(Sender: TObject);
    procedure AcQuitProgramExecute(Sender: TObject);
    procedure AcSaveAsExecute(Sender: TObject);
    procedure AcSaveExecute(Sender: TObject);
    procedure AddAllBtnClick(Sender: TObject);
    procedure AddFilesBtnClick(Sender: TObject);
    procedure AutoAddLinksBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ChmFileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure ChmFileNameEditEditingDone(Sender: TObject);
    procedure ChmTitleEditChange(Sender: TObject);
    procedure FileListBoxDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IndexEditAcceptFileName(Sender: TObject; var Value: String);
    procedure IndexEditBtnClick(Sender: TObject);
    procedure IndexEditEditingDone(Sender: TObject);
    procedure RemoveFilesBtnClick(Sender: TObject);
    procedure ScanHtmlCheckClick(Sender: TObject);
    procedure TOCEditAcceptFileName(Sender: TObject; var Value: String);
    procedure TOCEditBtnClick(Sender: TObject);
    procedure TOCEditEditingDone(Sender: TObject);
  private
    FActivated: Boolean;
    FModified: Boolean;
    procedure AddItems({%H-}AParentItem: TTreeNode; {%H-}ChmItems: TChmSiteMapItems);

    function Compile(ShowSuccessMsg: Boolean): Boolean;
    function GetModified: Boolean;
    procedure Save(aAs: Boolean);
    function CloseProject: Boolean;

    procedure AddFilesToProject(Strings: TStrings);
    procedure InitFileDialog(Dlg: TFileDialog);
    procedure ProjectDirChanged;
    function CreateRelativeProjectFile(Filename: string): string;
  public
    Project: TChmProject;
    procedure OpenProject(AFileName: String);
    // Dirty flag: has project been modified since opening?
    property Modified: Boolean read GetModified write FModified;
    function CreateAbsoluteProjectFile(Filename: string): string;
  end;

var
  CHMForm: TCHMForm;

implementation

{$R *.lfm}

uses
  CHMSiteMapEditor, CHMAbout, LHelpControl, Process;

{ TCHMForm }

procedure TCHMForm.AcAboutExecute(Sender: TObject);
begin
  with TAboutForm.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TCHMForm.AcNewExecute(Sender: TObject);
var
  bOverwrite: Boolean;
begin
  InitFileDialog(SaveDialog1);
  If SaveDialog1.Execute then
  begin
    bOverwrite := False;
    if FileExists(SaveDialog1.FileName) then
    begin
      bOverwrite := (MessageDlg('File already exists. Overwrite?', mtWarning, [mbYes, mbNo],0) = mrYes);
      if not bOverwrite then Exit;
    end;
    if (not CloseProject()) then Exit;
    if bOverwrite then DeleteFile(SaveDialog1.FileName);

    OpenProject(SaveDialog1.FileName);
    Project.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TCHMForm.AcCloseExecute(Sender: TObject);
begin
  CloseProject;
end;

procedure TCHMForm.AcCompileAndViewExecute(Sender: TObject);
var
  LHelpName: String;
  LHelpConn: TLHelpConnection;
  Proc: TProcessUTF8;
  ext: String;
begin
  // Compile
  if not Compile(false) then
    exit;

  // Open CHM in LHelp
  ext := ExtractFileExt(Application.ExeName);
  LHelpName := '../../components/chmhelp/lhelp/lhelp' + ext;
  if not FileExists(LHelpName) then
  begin
    if MessageDlg(
      'LHelp could not be located at '+ LHelpName +
      LineEnding + LineEnding +
      'Try to build using LazBuild?', mtError, [mbCancel, mbYes], 0) = mrYes then
    begin
      if not FileExists('../../lazbuild' + ext) then
      begin
        MessageDlg('LazBuild could not be found.', mtError, [mbCancel], 0);
        Exit;
      end;
      Proc := TProcessUTF8.Create(Self);
      try
        Statusbar.SimpleText := 'Building LHelp...';
        Proc.Executable := '../../../lazbuild';
        Proc.Parameters.Add('./lhelp.lpi');
        SetCurrentDir('../../components/chmhelp/lhelp/');
        Proc.Options := [poWaitOnExit];
        Proc.Execute;
        SetCurrentDir('../../../tools/chmmaker/');
        Statusbar.SimpleText := '';
        if Proc.ExitStatus <> 0 then
        begin
          MessageDlg('LHelp failed to build.', mtError, [mbCancel], 0);
          Exit;
        end;
      finally
        Proc.Free;
        Statusbar.SimpleText := '';
      end;
    end
    else
      Exit;
  end;
  LHelpConn := TLHelpConnection.Create;
  try
    LHelpConn.StartHelpServer('chmmaker', LHelpName);
    LHelpConn.OpenFile(CreateAbsoluteProjectFile(Project.OutputFileName));
  finally
    LHelpConn.Free;
  end;
end;

procedure TCHMForm.AcCompileExecute(Sender: TObject);
begin
  Compile(true);
end;

procedure TCHMForm.AcOpenExecute(Sender: TObject);
begin
  InitFileDialog(OpenDialog1);
  if OpenDialog1.Execute then
  begin
    if CloseProject() then
      OpenProject(OpenDialog1.FileName);
  end;
end;

procedure TCHMForm.AcQuitProgramExecute(Sender: TObject);
begin
  Close;
end;

procedure TCHMForm.AcSaveAsExecute(Sender: TObject);
begin
  Save(True);
end;

procedure TCHMForm.AcSaveExecute(Sender: TObject);
begin
  Save(False);
end;

procedure TCHMForm.AddItems(AParentItem: TTreeNode; ChmItems: TChmSiteMapItems);
  begin
{    for I := 0 to ChmItems.Count-1 do begin
      Item := TreeView1.Items.AddChild(AParentItem, ChmItems.Item[I].Text);
      AddItems(Item, ChmItems.Item[I].Children);
    end;
 } end;

procedure TCHMForm.Button1Click(Sender: TObject);
begin
  {SiteMap := TChmSiteMap.Create(stTOC);
  OpenDialog1.InitialDir := GetCurrentDir;
  if OpenDialog1.Execute = False then Exit;
  SiteMap.LoadFromFile(OpenDialog1.FileName);
  AddItems(nil, sitemap.Items);
  
  Stream := TMemoryStream.Create;
  
  Sitemap.SaveToStream(Stream);
  Stream.Position := 0;
  
  SynEdit1.Lines.LoadFromStream(Stream);
  Stream.Free;
   }
end;

procedure TCHMForm.AddFilesBtnClick(Sender: TObject);
begin
  InitFileDialog(OpenDialog2);
  if OpenDialog2.Execute then
  begin
    Modified := True;
    AddFilesToProject(OpenDialog2.Files);
  end;
end;

procedure TCHMForm.AddAllBtnClick(Sender: TObject);
var
  Files: TStrings;

  procedure AddDir(ADir: String);
  var
    SearchRec: TSearchRec;
    FileName: String;
  begin
    // WriteLn('Adding Dir: ', ADir);
    if FindFirst(ADir+'*', faAnyFile or faDirectory, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          if Pos('.', SearchRec.Name) = 0 then
          begin
            AddDir(IncludeTrailingPathDelimiter(ADir+SearchRec.Name));
          end;
        end
        else
        begin
          FileName := ADir+SearchRec.Name;
          FileName := ExtractRelativepath(Project.ProjectDir, FileName);
          if Files.IndexOf(FileName) = -1 then
            Files.Add(FileName);
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  end;

begin
  if MessageDlg('This will add all files in the project directory recursively.' + LineEnding +
                'Do you want to continue?',
                mtConfirmation, [mbYes, mbNo],0) <> mrYes then exit;
  Modified := True;
  Files := TStringList.Create;
  try
    Files.AddStrings(FileListBox.Items);
    AddDir(Project.ProjectDir);
    FileListBox.Items.Assign(Files);
  finally
    Files.Free;
  end;
end;

procedure TCHMForm.AutoAddLinksBtnClick(Sender: TObject);
begin
  Modified := True;
end;

procedure TCHMForm.Button2Click(Sender: TObject);
begin
    {
  if OpenDialog1.Execute = False then Exit;
  OutStream := TFileStream.Create('/home/andrew/test.chm', fmCreate or fmOpenWrite);
  Chm := TChmWriter.Create(OutStream, False);
  Chm.FilesToCompress.AddStrings(OpenDialog1.Files);
  Chm.GetFileData := @GetData;
  Chm.Title := 'test';
  Chm.DefaultPage := 'index.html';
  Chm.Execute;
  OutStream.Free;
  Chm.Free;
     }
  
  
end;

procedure TCHMForm.ChmFileNameEditAcceptFileName(Sender: TObject; var Value: String);
begin
  Modified := True;
  Value := CreateRelativeProjectFile(Value);
  if ExtractFileExt(Value) = '' then Value := Value+'.chm';
  Project.OutputFileName := Value;
end;

procedure TCHMForm.ChmFileNameEditEditingDone(Sender: TObject);
begin
  // Normalize filename and store in Project
  if (ChmFileNameEdit.FileName = '') then Exit;
  if (ExtractFileExt(ChmFileNameEdit.FileName)) = '' then ChmFileNameEdit.FileName := ChmFileNameEdit.FileName + '.chm';
  ChmFileNameEdit.FileName := CreateRelativeProjectFile(ChmFileNameEdit.FileName);
  Project.OutputFileName := ChmFileNameEdit.FileName;
  Modified := True;
end;

procedure TCHMForm.ChmTitleEditChange(Sender: TObject);
begin
  Modified := True;
end;

function TCHMForm.Compile(ShowSuccessMsg: Boolean): Boolean;
var
  OutFile: TFileStream;
begin
  Result := false;
  if (Project.OutputFileName = '') then
  begin
    MessageDlg('You must set a filename for the output CHM file.', mtError, [mbCancel], 0);
    Exit;
  end;
  Save(False);
  OutFile := TFileStream.Create(CreateAbsoluteProjectFile(Project.OutputFileName), fmCreate or fmOpenWrite);
  try
    Project.WriteChm(OutFile);
    if ShowSuccessMsg then
      ShowMessage('CHM file '+Project.OutputFileName+' was created.');
    Result := true;
  finally
    OutFile.Free;
  end;
end;

procedure TCHMForm.FileListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  FileListbox.Canvas.FillRect(ARect);
  if Pos('..', FileListBox.Items.Strings[Index]) > 0 then
  begin
    // These items won't be added to the chm because they are not within the project dir
    // so mark them with a red rectangle
    Dec(ARect.Right);
    Dec(ARect.Bottom);
    FileListBox.Canvas.Pen.Color := clRed;
    FileListBox.Canvas.Frame(ARect);
  end;
  // Draw item text
  FileListBox.Canvas.TextRect(ARect,
    2, (ARect.Top + ARect.Bottom - FileListbox.Canvas.TextHeight('Tg')) div 2,
    FileListBox.Items[Index]
  );
end;

procedure TCHMForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    Constraints.MinWidth := GroupBox1.Width + GroupBox1.BorderSpacing.Left + GroupBox1.BorderSpacing.Right +
      Splitter.Width + CompileTimeOptionsGroupbox.Width +
      Mainpanel.BorderSpacing.Left + MainPanel.BorderSpacing.Right;
    if Width < Constraints.MinWidth then
      Width := Constraints.MinWidth;
    Constraints.MinHeight :=  CHMFileNameEdit.Top + CHMFileNameEdit.Height +
      CompileBtn.Height + CompileBtn.BorderSpacing.Top +
      MainPanel.BorderSpacing.Top + MainPanel.BorderSpacing.Bottom +
      StatusBar.Height;
    if Height < Constraints.MinHeight then
      Height := Constraints.MinHeight;
  end;
end;

procedure TCHMForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  MResult: Integer;
begin
  if Modified then
  begin
    MResult := MessageDlg(
      'Project has been modified.' + LineEnding +
      'Would you like to save the changes?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0
    );
    case MResult of
      mrYes: Save(False);
      mrNo: CloseAction := caFree;
      mrCancel: CloseAction := caNone;
    end;
   end;
end;

procedure TCHMForm.FormCreate(Sender: TObject);
begin
  CloseProject;
end;

procedure TCHMForm.FormDestroy(Sender: TObject);
begin
  CloseProject;
end;

procedure TCHMForm.IndexEditAcceptFileName(Sender: TObject; var Value: String);
begin
  Modified := True;
  Value := CreateRelativeProjectFile(Value);
  if ExtractFileExt(Value) = '' then Value := Value + '.hhk';
  Project.IndexFileName := Value;
end;

procedure TCHMForm.IndexEditBtnClick(Sender: TObject);
var
  Stream: TStream;
  FileName: String;
begin
  if (Project.IndexFileName = '') then
  begin
    Project.IndexFileName := '_index.hhk';
    IndexEdit.FileName := Project.IndexFileName;
    Modified := True;
  end;

  FileName := CreateAbsoluteProjectFile(Project.IndexFileName);

  if FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  end
  else
  begin
    Stream := TFileStream.Create(FileName, fmCreate or fmOpenReadWrite);
  end;

  try
    SitemapEditForm.Execute(Stream, stIndex, FileListBox.Items);
  finally
    Stream.Free;
  end;
end;

procedure TCHMForm.IndexEditEditingDone(Sender: TObject);
begin
  // Normalize filename and store in Project
  if (IndexEdit.FileName = '') then Exit;
  if (ExtractFileExt(IndexEdit.FileName)) = '' then IndexEdit.FileName := IndexEdit.FileName + '.hhk';
  IndexEdit.FileName := CreateRelativeProjectFile(IndexEdit.FileName);
  Project.IndexFileName := IndexEdit.FileName;
  Modified := True;
end;

procedure TCHMForm.RemoveFilesBtnClick(Sender: TObject);
var
  I: Integer;
begin
  Modified := True;
  for I := FileListBox.Items.Count-1 downto 0 do
    if FileListBox.Selected[I] then FileListBox.Items.Delete(I);
  DefaultPageCombo.Items.Assign(FileListBox.Items);
end;

procedure TCHMForm.ScanHtmlCheckClick(Sender: TObject);
begin
  Modified := True;
end;

procedure TCHMForm.TOCEditAcceptFileName(Sender: TObject; var Value: String);
begin
  Modified := True;
  Value := CreateRelativeProjectFile(Value);
  if ExtractFileExt(Value) = '' then Value := Value + '.hhc';
  Project.TableOfContentsFileName := Value;
end;

procedure TCHMForm.TOCEditBtnClick(Sender: TObject);
var
  Stream: TStream;
  FileName: String;
begin
  if (Project.TableOfContentsFileName = '') then
  begin
    Project.TableOfContentsFileName := '_table_of_contents.hhc';
    TOCEdit.FileName := Project.TableOfContentsFileName;
    Modified := True;
  end;

  FileName := CreateAbsoluteProjectFile(Project.TableOfContentsFileName);

  if FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  end
  else
  begin
    Stream := TFileStream.Create(FileName, fmCreate or fmOpenReadWrite);
  end;

  try
    SitemapEditForm.Execute(Stream, stTOC, FileListBox.Items);
  finally
    Stream.Free;
  end;
end;

procedure TCHMForm.TOCEditEditingDone(Sender: TObject);
begin
  // Normalize filename and store in Project
  if (TOCEdit.FileName = '') then Exit;
  if (ExtractFileExt(TOCEdit.FileName)) = '' then TOCEdit.FileName := TOCEdit.FileName + '.hhc';
  TOCEdit.FileName := CreateRelativeProjectFile(TOCEdit.FileName);
  Project.TableOfContentsFileName := TOCEdit.FileName;
  Modified := True;
end;

function TCHMForm.GetModified: Boolean;
begin
  Result := (Project <> nil) and FModified;
end;

procedure TCHMForm.Save(aAs: Boolean);
begin
  if aAs or (Project.FileName = '') then
  begin
    InitFileDialog(SaveDialog1);
    if SaveDialog1.Execute then
    begin
      Project.FileName := ChangeFileExt(SaveDialog1.FileName,'.hfp');
      ProjectDirChanged;
    end;
  end;
  Project.Files.Assign(FileListBox.Items);
  Project.Title                   := ChmTitleEdit.Text;
  Project.TableOfContentsFileName := CreateRelativeProjectFile(TOCEdit.FileName);
  Project.IndexFileName           := CreateRelativeProjectFile(IndexEdit.FileName);
  Project.DefaultPage             := DefaultPageCombo.Text;
  Project.ScanHtmlContents        := ScanHtmlCheck.Checked;
  Project.MakeSearchable          := CreateSearchableCHMCheck.Checked;
  Project.OutputFileName          := CreateRelativeProjectFile(ChmFileNameEdit.FileName);

  Project.SaveToFile(Project.FileName);
  Modified := False;
end;

function TCHMForm.CloseProject: Boolean;
begin
  Result := True;

  if Modified then
  begin
    case (MessageDlg('Save changes?', mtConfirmation, [mbYes, mbNo, mbCancel],0)) of
      mrCancel: Exit(False);
      mrYes: Save(False);
    end;
  end;

  FileListBox.Clear;
  DefaultPageCombo.Clear;
  ChmTitleEdit.Clear();
  TOCEdit.Clear;
  IndexEdit.Clear;
  GroupBox1.Enabled := False;
  MainPanel.Enabled := False;
  AcSaveAs.Enabled := False;
  AcSave.Enabled := False;
  AcClose.Enabled := False;
  AcCompile.Enabled := False;
  AcCompileAndView.Enabled := False;
  CompileTimeOptionsGroupBox.Enabled := False;
  ScanHtmlCheck.Checked := False;
  CreateSearchableCHMCheck.Checked := False;
  FreeAndNil(Project);
  Modified := False;
end;

procedure TCHMForm.OpenProject(AFileName: String);
begin
  if not Assigned(Project) then Project := TChmProject.Create;
  Project.LoadFromFile(AFileName);
  GroupBox1.Enabled := True;
  MainPanel.Enabled := True;
  AcSaveAs.Enabled := True;
  AcSave.Enabled := True;
  AcClose.Enabled := True;
  AcCompile.Enabled := True;
  AcCompileAndView.Enabled := True;
  CompileTimeOptionsGroupBox.Enabled := True;

  FileListBox.Items.AddStrings(Project.Files);
  ChmTitleEdit.Text := Project.Title;
  TOCEdit.FileName := Project.TableOfContentsFileName;
  IndexEdit.FileName := Project.IndexFileName;
  DefaultPageCombo.Items.Assign(FileListBox.Items);
  DefaultPageCombo.Text := Project.DefaultPage;
  ScanHtmlCheck.Checked := Project.ScanHtmlContents;
  CreateSearchableCHMCheck.Checked := Project.MakeSearchable;
  ChmFileNameEdit.FileName := Project.OutputFileName;

  Modified := False;
  ProjectDirChanged;
end;

procedure TCHMForm.AddFilesToProject(Strings: TStrings);
var
  BDir: String;
  I: Integer;
  RelativePath: String;
  FileName: String;
begin
  Modified := True;
  BDir := ExtractFilePath(Project.FileName);

  for I := 0 to Strings.Count-1 do begin
    FileName := Strings.Strings[I];

    RelativePath := ExtractRelativepath(BDir, FileName);
    if Pos('..', RelativePath) > 0 then
      FileListBox.Items.AddObject(RelativePath, TObject(1))
    else
      FileListBox.Items.AddObject(RelativePath, TObject(0));
  end;
  DefaultPageCombo.Items.Assign(FileListBox.Items);
end;

procedure TCHMForm.InitFileDialog(Dlg: TFileDialog);
var
  Dir: String;
begin
  Dir:='';
  if (Project<>nil) then
    Dir:=ExtractFilePath(Project.FileName);
  if not DirPathExists(Dir) then
    Dir:=GetCurrentDirUTF8;
  Dlg.InitialDir:=Dir;
end;

procedure TCHMForm.ProjectDirChanged;
var
  Dir: String;
begin
  if Project=nil then exit;
  Dir:=ExtractFilePath(Project.FileName);

  TOCEdit.InitialDir:=Dir;
  IndexEdit.InitialDir:=Dir;
  ChmFileNameEdit.InitialDir:=Dir;
end;

function TCHMForm.CreateRelativeProjectFile(Filename: string): string;
begin
  Result:=Filename;
  if (Project=nil) or (not FilenameIsAbsolute(Project.FileName)) then exit;
  Result:=CreateRelativePath(Filename,ExtractFilePath(Project.FileName));
end;

function TCHMForm.CreateAbsoluteProjectFile(Filename: string): string;
begin
  Result:=Filename;
  if FilenameIsAbsolute(Result) then exit;
  if (Project=nil) or (not FilenameIsAbsolute(Project.FileName)) then exit;
  Result:=ExtractFilePath(Project.FileName)+Filename;
end;

end.

