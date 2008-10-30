unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, chmwriter, ComCtrls, chmsitemap, SynEdit, Menus, ExtCtrls, CheckLst,
  EditBtn, chmfilewriter;

type

  { TCHMForm }

  TCHMForm = class(TForm)
    AddFilesBtn: TButton;
    AutoAddLinksBtn: TButton;
    AddAllBtn: TButton;
    CompileViewBtn: TButton;
    CompileBtn: TButton;
    DefaultPageCombo: TComboBox;
    ChmFileNameEdit: TFileNameEdit;
    FollowLinksCheck: TCheckBox;
    CreateSearchableCHMCheck: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OpenDialog2: TOpenDialog;
    RemoveFilesBtn: TButton;
    TOCEditBtn: TButton;
    IndexEditBtn: TButton;
    IndexEdit: TFileNameEdit;
    GroupBox1: TGroupBox;
    FileListBox: TListBox;
    Label1: TLabel;
    Label2: TLabel;
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
    HelpHelpItem: TMenuItem;
    MenuItem5: TMenuItem;
    HelpAboutItem: TMenuItem;
    ProjNewItem: TMenuItem;
    ProjOpenItem: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    TOCEdit: TFileNameEdit;
    procedure AddAllBtnClick(Sender: TObject);
    procedure AddFilesBtnClick(Sender: TObject);
    procedure AutoAddLinksBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ChmFileNameEditAcceptFileName(Sender: TObject; var Value: String);
    procedure CompileBtnClick(Sender: TObject);
    procedure CompileViewBtnClick(Sender: TObject);
    procedure FileListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure IndexEditAcceptFileName(Sender: TObject; var Value: String);
    procedure IndexEditBtnClick(Sender: TObject);
    procedure ProjCloseItemClick(Sender: TObject);
    procedure ProjNewItemClick(Sender: TObject);
    procedure ProjOpenItemClick(Sender: TObject);
    procedure ProjQuitItemClick(Sender: TObject);
    procedure ProjSaveAsItemClick(Sender: TObject);
    procedure ProjSaveItemClick(Sender: TObject);
    procedure RemoveFilesBtnClick(Sender: TObject);
    procedure TOCEditAcceptFileName(Sender: TObject; var Value: String);
    procedure TOCEditBtnClick(Sender: TObject);
  private
    FModified: Boolean;
    procedure AddItems(AParentItem: TTreeNode; ChmItems: TChmSiteMapItems);

    function GetModified: Boolean;
    procedure Save(aAs: Boolean);
    procedure CloseProject;

    procedure AddFilesToProject(Strings: TStrings);
  public
    Project: TChmProject;
    procedure OpenProject(AFileName: String);
    property Modified: Boolean read GetModified write FModified;
  end; 

var
  CHMForm: TCHMForm;

implementation
uses CHMSiteMapEditor, LHelpControl, Process;
{ TCHMForm }

procedure TCHMForm.AddItems(AParentItem: TTreeNode; ChmItems: TChmSiteMapItems);
  var
    Item: TTreeNode;
    I: Integer;
  begin
{    for I := 0 to ChmItems.Count-1 do begin
      Item := TreeView1.Items.AddChild(AParentItem, ChmItems.Item[I].Text);
      AddItems(Item, ChmItems.Item[I].Children);
    end;
 } end;

procedure TCHMForm.Button1Click(Sender: TObject);
var
  SiteMap: TChmSiteMap;
  Stream: TMemoryStream;
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
  if OpenDialog2.Execute = False then exit;
  Modified := True;
  AddFilesToProject(OpenDialog2.Files);
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
    if FindFirst(ADir+'*', faAnyFile or faDirectory, SearchRec) = 0 then begin
      repeat
        if (SearchRec.Attr and faDirectory) <> 0 then begin
          if Pos('.', SearchRec.Name) = 0 then begin
            AddDir(IncludeTrailingPathDelimiter(ADir+SearchRec.Name));
          end;
        end
        else begin
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
  if MessageDlg('This will add all files in the project'#10+
                'Directory recursively. Do you want to continue?', mtConfirmation, [mbYes, mbNo],0) = mrNo then exit;
  Modified := True;
  Files := TStringList.Create;
  Files.AddStrings(FileListBox.Items);
  AddDir(Project.ProjectDir);
  FileListBox.Items.Assign(Files);
  Files.Free;
end;

procedure TCHMForm.AutoAddLinksBtnClick(Sender: TObject);
begin
  Modified := True;
end;

procedure TCHMForm.Button2Click(Sender: TObject);
var
  OutStream: TFileStream;
  CHM: TChmWriter;
  I: Integer;
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
  if ExtractFileExt(Value) = '' then Value := Value+'.chm';
end;

procedure TCHMForm.CompileBtnClick(Sender: TObject);
var
  OutFile: TFileStream;
begin
  if ChmFileNameEdit.FileName = '' then begin
    MessageDlg('You must set a filename for the output CHM file!', mtError, [mbCancel], 0);
    Exit;
  end;
  Save(False);
  OutFile := TFileStream.Create(Project.OutputFileName, fmCreate or fmOpenWrite);
  Project.WriteChm(OutFile);
  OutFile.Free;
end;


procedure TCHMForm.CompileViewBtnClick(Sender: TObject);
var
  LHelpName: String;
  LHelpConn: TLHelpConnection;
  Proc: TProcess;
begin
  if ChmFileNameEdit.FileName = '' then begin
    MessageDlg('You must set a filename for the output CHM file!', mtError, [mbCancel], 0);
    Exit;
  end;
  CompileBtnClick(Sender);
  // open
  // ...
  LHelpName := '../../components/chmhelp/lhelp/lhelp' + ExtractFileExt(Application.Name);
  if not FileExists(LHelpName) then
  begin
    if MessageDlg('LHelp could not be located at '+ LHelpName +' Try to build using lazbuild?', mtError, [mbCancel, mbYes], 0) = mrYes then
    begin
      if not FileExists('../../lazbuild') then
      begin
        MessageDlg('lazbuild coul not be found.', mtError, [mbCancel], 0);
        Exit;
      end;
      Proc := TProcess.Create(Self);
      Proc.CommandLine := '../../../lazbuild ./lhelp.lpi';
      SetCurrentDir('../../components/chmhelp/lhelp/');
      Proc.Options := [poWaitOnExit];
      Proc.Execute;
      SetCurrentDir('../../../tools/chmmaker/');
      if Proc.ExitStatus <> 0 then
      begin
        MessageDlg('lhelp failed to build', mtError, [mbCancel], 0);
        Exit;
      end;
      Proc.Free;
    end
    else
      Exit;
  end;
  LHelpConn := TLHelpConnection.Create;
  LHelpConn.StartHelpServer('chmmaker', LHelpName);
  LHelpConn.OpenFile(ChmFileNameEdit.FileName);
  LHelpConn.Free;
end;

procedure TCHMForm.FileListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
   if Pos('..', FileListBox.Items.Strings[Index]) > 0 then begin
     // These items won't be added to the chm because they are not within the project dir
     Dec(ARect.Right);
     Dec(ARect.Bottom);
     FileListBox.Canvas.Pen.Color := clRed;
     FileListBox.Canvas.Frame(ARect);
   end;
end;

procedure TCHMForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  MResult: Integer;
begin
  if Modified then begin
    MResult := MessageDlg('Project is modified would you like to save the changes?', mtConfirmation,
                                      [mbYes, mbNo, mbCancel], 0);
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

procedure TCHMForm.IndexEditAcceptFileName(Sender: TObject; var Value: String);
begin
  Modified := True;
  //Value := ExtractRelativepath(Project.ProjectDir, Value);
  //WriteLn(Value);
  Project.IndexFileName := Value;
end;

procedure TCHMForm.IndexEditBtnClick(Sender: TObject);
var
  Stream: TStream;
  FileName: String;
begin
  FileName := IndexEdit.FileName;
  if FileName = '' then begin
    FileName := Project.ProjectDir+'_index.hhk'
  end;

  if FileExists(FileName) then begin
    Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  end
  else begin
    Stream := TFileStream.Create(FileName, fmCreate or fmOpenReadWrite);
  end;

  if SitemapEditForm.Execute(Stream, stIndex, FileListBox.Items) then IndexEdit.FileName := FileName;


end;

procedure TCHMForm.ProjCloseItemClick(Sender: TObject);
begin
  CloseProject;
end;

procedure TCHMForm.ProjNewItemClick(Sender: TObject);
begin
  If SaveDialog1.Execute then begin
    if FileExists(SaveDialog1.FileName)
    and (MessageDlg('File Already Exists! Ovewrite?', mtWarning, [mbYes, mbNo],0) = mrNo) then Exit;
    OpenProject(SaveDialog1.FileName);
    Project.SaveToFile(SaveDialog1.FileName);
  end;

end;

procedure TCHMForm.ProjOpenItemClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    CloseProject;
    OpenProject(OpenDialog1.FileName);
  end;
end;

procedure TCHMForm.ProjQuitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TCHMForm.ProjSaveAsItemClick(Sender: TObject);
begin
  Save(True);
end;

procedure TCHMForm.ProjSaveItemClick(Sender: TObject);
begin
  Save(False);
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

procedure TCHMForm.TOCEditAcceptFileName(Sender: TObject; var Value: String);
begin
  Modified := True;
  Project.TableOfContentsFileName := Value;
end;

procedure TCHMForm.TOCEditBtnClick(Sender: TObject);
var
  Stream: TStream;
  FileName: String;
  BDir: String;
begin
  FileName := TOCEdit.FileName;
  if FileName = '' then begin
    FileName := Project.ProjectDir+'_table_of_contents.hhc'
  end;
  
  if FileExists(FileName) then begin
    Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  end
  else begin
    Stream := TFileStream.Create(FileName, fmCreate or fmOpenReadWrite);
  end;

  BDir := ExtractFilePath(Project.FileName);

  FileName := ExtractRelativepath(BDir, FileName);

  
  if SitemapEditForm.Execute(Stream, stTOC, FileListBox.Items) then TOCEdit.FileName := FileName;
  
end;

function TCHMForm.GetModified: Boolean;
begin
  Result := (Project <> nil) and FModified;
end;

procedure TCHMForm.Save(aAs: Boolean);
var
  SaveName: String;
begin
  if aAs or (Project.FileName = '') then
    if  SaveDialog1.Execute then
      Project.FileName := ChangeFileExt(SaveDialog1.FileName,'.hfp');
  Project.Files.Assign(FileListBox.Items);
  Project.TableOfContentsFileName := TOCEdit.FileName;
  Project.IndexFileName           := IndexEdit.FileName;
  Project.DefaultPage             := DefaultPageCombo.Text;
  Project.AutoFollowLinks         := FollowLinksCheck.Checked;
  Project.MakeSearchable          := CreateSearchableCHMCheck.Checked;
  Project.OutputFileName          := ChmFileNameEdit.FileName;

  Project.SaveToFile(Project.FileName);
  Modified := False;
end;

procedure TCHMForm.CloseProject;
begin
  FileListBox.Clear;
  DefaultPageCombo.Clear;
  TOCEdit.Clear;
  IndexEdit.Clear;
  GroupBox1.Enabled      := False;
  Panel1.Enabled         := False;
  CompileItem.Enabled    := False;
  ProjSaveAsItem.Enabled := False;
  ProjSaveItem.Enabled   := False;
  ProjCloseItem.Enabled  := False;

  FollowLinksCheck.Checked := False;
  CreateSearchableCHMCheck.Checked := False;
  FreeAndNil(Project);
end;

procedure TCHMForm.OpenProject(AFileName: String);
begin
  if not Assigned(Project) then Project := TChmProject.Create;
  Project.LoadFromFile(AFileName);
  GroupBox1.Enabled      := True;
  Panel1.Enabled         := True;
  CompileItem.Enabled    := True;
  ProjSaveAsItem.Enabled := True;
  ProjSaveItem.Enabled   := True;
  ProjCloseItem.Enabled  := True;
  
  FileListBox.Items.AddStrings(Project.Files);
  TOCEdit.FileName := Project.TableOfContentsFileName;
  IndexEdit.FileName := Project.IndexFileName;
  DefaultPageCombo.Items.Assign(FileListBox.Items);
  DefaultPageCombo.Text := Project.DefaultPage;
  FollowLinksCheck.Checked := Project.AutoFollowLinks;
  CreateSearchableCHMCheck.Checked := Project.MakeSearchable;
  ChmFileNameEdit.FileName := Project.OutputFileName;
end;

procedure TCHMForm.AddFilesToProject(Strings: TStrings);
var
  ADir, BDir: String;
  I: Integer;
  RelativePath: String;
  FileName: String;
  NewFileName: String;
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

initialization
  {$I unit1.lrs}

end.

