{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Find in files non modal window.

ToDo:
  Resourcestrings

}
unit FindInFilesWnd;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, System.UITypes, Forms, StdCtrls, Buttons, ComCtrls,
  ExtCtrls, Controls, DividerBevel, SynEdit, EnvironmentOpts, SearchPathProcs,
  MenuIntf, IDEWindowIntf, LazIDEIntf, SrcEditorIntf, IDEDialogs, LazFileUtils,
  LazLoggerBase, ProjectIntf, MacroIntf, InputHistory, EditorOptions,
  PathEditorDlg, PackageListEditorDlg;

type

  { TLazFindInFilesWindow }

  TLazFindInFilesWindow = class(TForm)
    CaseSensitiveSpeedButton: TSpeedButton;
    AutoUpdateSpeedButton: TSpeedButton;
    DirectoriesComboBox: TComboBox;
    FileMaskComboBox: TComboBox;
    TextToFindCombobox: TComboBox;
    ShowReplaceSpeedButton: TSpeedButton;
    InEditorFilesSpeedButton: TSpeedButton;
    InProjectFilesSpeedButton1: TSpeedButton;
    PkgEditSpeedButton: TSpeedButton;
    ReplaceButton: TButton;
    WherePanel: TPanel;
    PkgComboBox: TComboBox;
    ReplaceTextComboBox: TComboBox;
    ResultsTreeView: TTreeView;
    SrcDividerBevel: TDividerBevel;
    StoreAndNewSpeedButton: TSpeedButton;
    RegularExpressionsSpeedButton: TSpeedButton;
    MultilineSpeedButton: TSpeedButton;
    SynEdit1: TSynEdit;
    WholeWordsSpeedButton: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShowReplaceSpeedButtonClick(Sender: TObject);
  private
    DirectoriesPathEditBtn: TPathEditorButton;
    PkgListEditBtn: TPkgListEditorButton;
    procedure DirectoriesPathEditBtnClick(Sender: TObject);
    function DirectoriesPathEditBtnExecuted(Context: String; var NewPath: String
      ): Boolean;
    procedure PkgListEditBtnClick(Sender: TObject);
    function PkgListEditBtnExecuted(Context: String; var NewPath: String
      ): Boolean;
  protected
    function GetBaseDirectory: string; virtual;
    procedure DoFirstShow; override;
    procedure InitImageList; virtual;
    procedure LoadHistory; virtual;
    procedure SaveHistory; virtual;
  public
  end;

var
  FindInFilesWindow: TLazFindInFilesWindow;

procedure RegisterFindInFilesWnd;

implementation

{$R *.lfm}

type

  { TRegisterDummy }

  TRegisterDummy = class
    procedure mnuFindInFilesWndClicked(Sender: TObject);
  end;
var
  RegisterDummy: TRegisterDummy;

procedure RegisterFindInFilesWnd;
var
  MenuCommand: TIDEMenuCommand;
begin
  {$IFNDEF EnableFindInFilesWnd}
  exit;
  {$ENDIF}
  RegisterDummy:=TRegisterDummy.Create;
  MenuCommand:=RegisterIDEMenuCommand(itmSearchFindReplace,'FindInFilesWnd','New Find In Files');
  MenuCommand.OnClick:=@RegisterDummy.mnuFindInFilesWndClicked;
end;

{ TRegisterDummy }

procedure TRegisterDummy.mnuFindInFilesWndClicked(Sender: TObject);
begin
  if FindInFilesWindow=nil then
  begin
    IDEWindowCreators.CreateForm(FindInFilesWindow,TLazFindInFilesWindow,
       false,LazarusIDE.OwningComponent);
  end;
  IDEWindowCreators.ShowForm(FindInFilesWindow,true);
end;

{ TLazFindInFilesWindow }

procedure TLazFindInFilesWindow.FormCreate(Sender: TObject);
begin
  ShowReplaceSpeedButton.Caption:='';
  TextToFindCombobox.TextHint:='Find text';
  StoreAndNewSpeedButton.Hint:='Store results in search results window and start a new search';
  CaseSensitiveSpeedButton.Hint:='Case Sensitive';
  WholeWordsSpeedButton.Hint:='Whole words';
  RegularExpressionsSpeedButton.Hint:='Regular Expression';
  MultilineSpeedButton.Hint:='Multi line';
  AutoUpdateSpeedButton.Hint:='Start searching while you type';

  ReplaceTextComboBox.TextHint:='Replace text';
  ReplaceButton.Hint:='Replace all found matches';

  FileMaskComboBox.TextHint:='*.pas;*.inc;*.txt;*.lfm';
  InEditorFilesSpeedButton.Hint:='Search in editor files';
  InProjectFilesSpeedButton1.Hint:='Search in project files';

  // packages
  PkgComboBox.TextHint:='pkg1;pkg2';
  PkgComboBox.Text:='';
  PkgListEditBtn := TPkgListEditorButton.Create(Self);
  with PkgListEditBtn do
  begin
    Name := 'PkgListEditBtn';
    Caption := '...';
    AutoSize := true;
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, PkgComboBox);
    AnchorParallel(akBottom, 0, PkgComboBox);
    AnchorParallel(akRight, 0, WherePanel);
    ContextCaption := 'Search in packages';
    Templates:='$(ProjectPackages)';
    AssociatedComboBox:=PkgComboBox;
    OnClick := @PkgListEditBtnClick;
    OnExecuted := @PkgListEditBtnExecuted;
    Parent := WherePanel;
    TabOrder := PkgComboBox.TabOrder+1;
  end;
  PkgComboBox.AnchorToNeighbour(akRight, 0, PkgListEditBtn);

  // directories
  DirectoriesComboBox.TextHint:='folder1;folder2';
  DirectoriesComboBox.Text:='';
  DirectoriesPathEditBtn := TPathEditorButton.Create(Self);
  with DirectoriesPathEditBtn do
  begin
    Name := 'DirectoriesPathEditBtn';
    Caption := '...';
    AutoSize := true;
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, DirectoriesComboBox);
    AnchorParallel(akBottom, 0, DirectoriesComboBox);
    AnchorParallel(akRight, 0, WherePanel);
    ContextCaption := 'Search in directories';
    Templates:='$(ProjPath)'+
              ';$(LazarusDir)' +
              ';$(FPCSrcDir)';
    AssociatedComboBox:=DirectoriesComboBox;
    OnClick := @DirectoriesPathEditBtnClick;
    OnExecuted := @DirectoriesPathEditBtnExecuted;
    Parent := WherePanel;
    TabOrder := DirectoriesComboBox.TabOrder+1;
  end;
  DirectoriesComboBox.AnchorToNeighbour(akRight, 0, DirectoriesPathEditBtn);

  // preview
  SrcDividerBevel.Caption:='Preview';
  SynEdit1.Lines.Text:='No source selected';

  InitImageList;
end;

procedure TLazFindInFilesWindow.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if CloseAction=caNone then ;
  SaveHistory;
end;

procedure TLazFindInFilesWindow.FormDestroy(Sender: TObject);
begin

end;

procedure TLazFindInFilesWindow.ShowReplaceSpeedButtonClick(Sender: TObject);
begin
  DisableAlign;
  try
    if ShowReplaceSpeedButton.Down then
    begin
      ReplaceTextComboBox.Visible:=true;
      ReplaceButton.Visible:=true;
    end else begin
      ReplaceTextComboBox.Visible:=false;
      ReplaceButton.Visible:=false;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TLazFindInFilesWindow.DirectoriesPathEditBtnClick(Sender: TObject);
begin
  DirectoriesPathEditBtn.CurrentPathEditor.BaseDirectory := GetBaseDirectory;
end;

function TLazFindInFilesWindow.DirectoriesPathEditBtnExecuted(Context: String;
  var NewPath: String): Boolean;
var
  TrimmedPath, BaseDir, Dir: String;
  p, r: Integer;
begin
  if Context='' then ;
  TrimmedPath := TrimSearchPath(NewPath, '', true);
  BaseDir:=GetBaseDirectory;
  p:=1;
  repeat
    Dir:=GetNextDirectoryInSearchPath(TrimmedPath,p);
    if Dir='' then break;
    DebugLn(['TLazFindInFilesWindow.DirectoriesPathEditBtnExecuted Dir="',Dir,'"']);
    if not IDEMacros.SubstituteMacros(Dir) then
    begin
      r:=IDEMessageDialog('Invalid Macro','Path "'+Dir+'" contains unknown macros',
        mtError,[mbCancel,mbIgnore]);
      if r<>mrIgnore then
        exit(false);
    end else begin
      debugln(['TLazFindInFilesWindow.DirectoriesPathEditBtnExecuted macro resolved Dir="',Dir,'"']);
      Dir:=ExpandFileNameUTF8(Dir,BaseDir);
      debugln(['TLazFindInFilesWindow.DirectoriesPathEditBtnExecuted expanded Dir="',Dir,'"']);
      if not DirectoryExists(Dir,true) then
      begin
        r:=IDEMessageDialog('Path not found','Path "'+Dir+'" not found',
          mtError,[mbCancel,mbIgnore]);
        if r<>mrIgnore then
          exit(false);
      end;
    end;
  until false;

  NewPath:=TrimmedPath;
  Result:=true;
end;

procedure TLazFindInFilesWindow.PkgListEditBtnClick(Sender: TObject);
begin

end;

function TLazFindInFilesWindow.PkgListEditBtnExecuted(Context: String;
  var NewPath: String): Boolean;
begin
  if Context='' then ;
  if NewPath<>PkgComboBox.Text then ;
  Result:=true;
end;

function TLazFindInFilesWindow.GetBaseDirectory: string;
var
  Proj: TLazProject;
begin
  Proj:=LazarusIDE.ActiveProject;
  if Proj<>nil then
  begin
    Result:=Proj.Directory;
    if FilenameIsAbsolute(Result) then
      exit;
    Result:=LazarusIDE.GetTestBuildDirectory;
    if FilenameIsAbsolute(Result) then
      exit;
  end;
  Result:='$(LazarusDir)';
  if not IDEMacros.SubstituteMacros(Result) then
    Result:=LazarusIDE.GetPrimaryConfigPath;
end;

procedure TLazFindInFilesWindow.DoFirstShow;
begin
  LoadHistory;
  inherited DoFirstShow;
end;

procedure TLazFindInFilesWindow.InitImageList;
begin

end;

procedure TLazFindInFilesWindow.LoadHistory;

  procedure AssignToComboBox(AComboBox: TComboBox; Strings: TStrings);
  begin
    AComboBox.Items.Assign(Strings);
    if AComboBox.Items.Count>0 then
      AComboBox.ItemIndex := 0;
  end;

  procedure AddFileToComboBox(AComboBox: TComboBox; Filename: string);
  var
    i: Integer;
  begin
    if Filename='' then exit;
    Filename:=AppendPathDelim(TrimFilename(Filename));
    for i:=0 to AComboBox.Items.Count-1 do begin
      if CompareFilenames(Filename,AComboBox.Items[i])=0 then begin
        // move to front (but not top, top should be the last used directory)
        if i>2 then
          AComboBox.Items.Move(i,1);
        exit;
      end;
    end;
    // insert in front (but not top, top should be the last used directory)
    if AComboBox.Items.Count>0 then
      i:=1
    else
      i:=0;
    AComboBox.Items.Insert(i,Filename);
  end;

  procedure AddDir(Dir: string);
  var
    i: Integer;
  begin
    for i:=0 to DirectoriesComboBox.Items.Count-1 do
    begin
      if CompareFilenames(ChompPathDelim(Dir),ChompPathDelim(DirectoriesComboBox.Items[i]))=0 then
        exit;
    end;
    DirectoriesComboBox.Items.Add(Dir);
  end;

var
  SrcEdit: TSourceEditorInterface;
begin
  SrcEdit := SourceEditorManagerIntf.ActiveEditor;

  TextToFindComboBox.Items.Assign(InputHistories.FindHistory);
  ReplaceTextComboBox.Items.Assign(InputHistories.ReplaceHistory);

  if not EditorOpts.FindTextAtCursor then begin
    if TextToFindComboBox.Items.Count>0 then begin
      TextToFindComboBox.ItemIndex:=0;
      TextToFindComboBox.SelectAll;
    end;
  end;

  // show recent directories and directory of current file
  AssignToComboBox(DirectoriesComboBox, InputHistories.FindInFilesPathHistory);
  if (SrcEdit<>nil) and (FilenameIsAbsolute(SrcEdit.FileName)) then
    AddFileToComboBox(DirectoriesComboBox, ExtractFilePath(SrcEdit.FileName));
  AddDir('$(FPCSrcDir)');
  AddDir('$(LazarusDir)');
  if DirectoriesComboBox.Items.Count>0 then
    DirectoriesComboBox.Text:=DirectoriesComboBox.Items[0];

  // recent file masks
  FileMaskComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  AssignToComboBox(FileMaskComboBox, InputHistories.FindInFilesMaskHistory);

  // recent packages

end;

procedure TLazFindInFilesWindow.SaveHistory;
begin
  InputHistories.AddToFindInFilesMaskHistory(FileMaskComboBox.Text);
end;

finalization
  FreeAndNil(RegisterDummy);

end.

