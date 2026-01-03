unit frmFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, Forms, Controls, Dialogs, FileCtrl, ComCtrls, StdCtrls, ExtCtrls,
  ShellCtrls,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8, Masks, LazLoggerBase,
  // Files
  FileBrowserTypes;

type
  TOpenFileEvent = procedure(Sender: TObject; const AFileName: string) of object;

  { TFileBrowserForm }

  TFileBrowserForm = class(TForm)
    btnConfigure: TButton;
    btnReload: TButton;
    cbHidden: TCheckBox;
    cbFilePanel: TFilterComboBox;
    ShellListView: TShellListView;
    ilTreeview: TImageList;
    Panel1: TPanel;
    pnlFiles: TPanel;
    Splitter1: TSplitter;
    TV: TShellTreeView;
    procedure btnConfigureClick(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure cbHiddenChange(Sender: TObject);
    procedure ShellListViewDblClick(Sender: TObject);
    procedure cbFilePanelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TVGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TVGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure TVSelectionChanged(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbFilePanelSelect(Sender: TObject);
    procedure ShellListViewKeyPress(Sender: TObject; var Key: char);
  private
    FOnConfigure: TNotifyEvent;
    FOnOpenFile: TOpenFileEvent;
    FOnSelectDir: TNotifyEvent;
    FRootDir: string;
    FShowHidden: Boolean;
    FSelectedMask : TMaskList;
    function GetAbsolutePath(Node: TTreeNode): string;
    procedure SetRootDir(const Value: string);
  public
    procedure ShowFiles;
    { Directory the treeview starts from }
    property RootDirectory: string read FRootDir write SetRootDir;
    { Must we show hidden directories }
    property ShowHidden: Boolean read FShowHidden write FShowHidden default False;
    { Called when user double-clicks file name }
    property OnOpenFile: TOpenFileEvent read FOnOpenFile write FOnOpenFile;
    { Called when user clicks configure button }
    property OnConfigure: TNotifyEvent read FOnConfigure write FOnConfigure;
    { Called when a new directory is selected }
    property OnSelectDir: TNotifyEvent read FOnSelectDir write FOnSelectDir;
  end;

var
  FileBrowserForm: TFileBrowserForm;


implementation

{$R *.lfm}

const
  cFilter = 'All Files (' + AllFilesMask + ')|' + AllFilesMask +
            '|Source(*.pas;*.pp)|*.pas;*.pp' +
            '|Projectfiles(*.pas;*.pp;*.inc;*.lfm;*.lpr;*.lrs;*.lpi;*.lpk)|' +
            '*.pas;*.pp;*.inc;*.lfm;*.lpr;*.lrs;*.lpi;*.lpk;|';

{ TFileBrowserForm }

procedure TFileBrowserForm.FormCreate(Sender: TObject);
begin
  FShowHidden := False;
  cbFilePanel.Filter := cFilter;
 {$IFDEF MSWINDOWS}
  TV.UseBuiltinIcons := true;
 {$ELSE}
  TV.Images := ilTreeView;
  TV.OnGetImageIndex := @TVGetImageIndex;
  TV.OnGetSelectedIndex := @TVGetSelectedIndex;
 {$ENDIF}
end;

procedure TFileBrowserForm.FormShow(Sender: TObject);
begin
  //if TV.Selected <> nil then
  //  TV.Selected.Expand(False);
end;

procedure TFileBrowserForm.FormActivate(Sender: TObject);
begin
  { for some reason this does not work in FormShow }
  TV.MakeSelectionVisible;
end;

procedure TFileBrowserForm.cbFilePanelSelect(Sender: TObject);
begin
  ShellListView.Mask := cbFilePanel.Mask;
end;

procedure TFileBrowserForm.btnConfigureClick(Sender: TObject);
begin
  if Assigned(FOnConfigure) then begin
    FOnConfigure(Self);
    //RootDirectory:=;
  end;
end;

procedure TFileBrowserForm.btnReloadClick(Sender: TObject);
begin
  TV.UpdateView();
end;

procedure TFileBrowserForm.cbHiddenChange(Sender: TObject);
begin
  ShowHidden := cbHidden.Checked;
  if ShowHidden then begin
    TV.ObjectTypes := TV.ObjectTypes + [otHidden];
    ShellListView.ObjectTypes := ShellListView.ObjectTypes + [otHidden];
  end
  else begin
    TV.ObjectTypes := TV.ObjectTypes - [otHidden];
    ShellListView.ObjectTypes := ShellListView.ObjectTypes - [otHidden];
  end;
end;

procedure TFileBrowserForm.cbFilePanelChange(Sender: TObject);
begin
  ShellListView.Mask := cbFilePanel.Text;
end;

function TFileBrowserForm.GetAbsolutePath(Node: TTreeNode): string;
begin
  Result := '';
  while Node <> nil do
  begin
    if Node.Text = PathDelim then
      Result := Node.Text + Result
    else
      Result := Node.Text + PathDelim + Result;
    Node := Node.Parent;
  end;
end;

procedure TFileBrowserForm.SetRootDir(const Value: string);
begin
  if FRootDir=Value then exit;
  FRootDir:=Value;
  ShowFiles;
end;

procedure TFileBrowserForm.ShowFiles;
Var
  RootNode: TTreeNode;
begin
  TV.Root:=FRootDir;
  { Set the original root node as the selected node. }
  RootNode := TV.Items.GetFirstNode;
  TV.Selected := RootNode;
  RootNode.Expand(False);
end;

procedure TFileBrowserForm.ShellListViewDblClick(Sender: TObject);
begin
  if Assigned(FOnOpenFile) then
    FOnOpenFile(Self, TV.Path + ShellListView.Selected.Caption);
end;

procedure TFileBrowserForm.ShellListViewKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(VK_RETURN) then
   ShellListViewDblClick(Sender);
end;

procedure TFileBrowserForm.TVGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex:=0;
end;

procedure TFileBrowserForm.TVGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex:=0;
end;

procedure TFileBrowserForm.TVSelectionChanged(Sender: TObject);
begin
  if Assigned(OnSelectDir) then
    OnselectDir(Self);
end;

end.

