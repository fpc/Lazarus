unit CodeToolsDefPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, SynEdit, DefineTemplates, ExprEval, LazarusIDEStrConsts,
  FileCtrl, InputHistory;

type
  TCodeToolsDefinesDialog = class(TForm)
    CloseButton: TBUTTON;
    DirectoryBrowseButton: TBUTTON;
    DirectoryCombobox: TCOMBOBOX;
    DirectoryGroupbox: TGROUPBOX;
    ValueSynedit: TSYNEDIT;
    ValueGroupbox: TGROUPBOX;
    ValuesListview: TLISTVIEW;
    procedure CodeToolsDefinesDialogCLOSE(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure CodeToolsDefinesDialogCREATE(Sender: TObject);
    procedure DirectoryBrowseButtonCLICK(Sender: TObject);
    procedure DirectoryComboboxCHANGE(Sender: TObject);
    procedure DirectoryGroupboxRESIZE(Sender: TObject);
    procedure ValuesListviewSELECTITEM(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FDefineTree: TDefineTree;
    procedure SetDefineTree(const AValue: TDefineTree);
    procedure UpdateValues;
    procedure UpdateValue;
    procedure ClearValues;
    procedure SetComboBox(AComboBox: TComboBox; const NewText: string);
  public
    property DefineTree: TDefineTree read FDefineTree write SetDefineTree;
  end;


function ShowCodeToolsDefinesValuesDialog(ADefineTree: TDefineTree;
  const InitialDirectory: string): TModalresult;

implementation

function ShowCodeToolsDefinesValuesDialog(ADefineTree: TDefineTree;
  const InitialDirectory: string): TModalresult;
var
  CodeToolsDefinesDialog: TCodeToolsDefinesDialog;
begin
  CodeToolsDefinesDialog:=TCodeToolsDefinesDialog.Create(Application);
  if InitialDirectory<>'' then
    CodeToolsDefinesDialog.SetComboBox(CodeToolsDefinesDialog.DirectoryCombobox,
      InitialDirectory);
  CodeToolsDefinesDialog.DefineTree:=ADefineTree;
  Result:=CodeToolsDefinesDialog.ShowModal;
  CodeToolsDefinesDialog.Free;
end;

{ TCodeToolsDefinesDialog }

procedure TCodeToolsDefinesDialog.DirectoryGroupboxRESIZE(Sender: TObject);
var
  x: Integer;
begin
  with DirectoryCombobox do
    SetBounds(0,0,Parent.ClientWidth-30,Height);
  x:=DirectoryCombobox.Width;
  with DirectoryBrowseButton do
    SetBounds(x,0,Parent.ClientWidth-x,DirectoryCombobox.Height);
end;

procedure TCodeToolsDefinesDialog.ValuesListviewSELECTITEM(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateValue;
end;

procedure TCodeToolsDefinesDialog.SetDefineTree(const AValue: TDefineTree);
begin
  if FDefineTree=AValue then exit;
  FDefineTree:=AValue;
  UpdateValues;
end;

procedure TCodeToolsDefinesDialog.UpdateValues;
var
  Dir: String;
  Defines: TExpressionEvaluator;
  i: Integer;
  ListItem: TListItem;
  Value: String;
begin
  Dir:=TrimFilename(DirectoryCombobox.Text);
  if FilenameIsAbsolute(Dir) then
  if (DefineTree=nil) or (not FilenameIsAbsolute(Dir)) then begin
    ClearValues;
    exit;
  end;
  Defines:=DefineTree.GetDefinesForDirectory(Dir,false);
  ValuesListview.BeginUpdate;
  for i:=0 to Defines.Count-1 do begin
    if ValuesListview.Items.Count<=i then
      ListItem:=ValuesListview.Items.Add
    else
      ListItem:=ValuesListview.Items[i];
    ListItem.Caption:=Defines.Names(i);
    Value:=copy(Defines.Values(i),1,100);
    if ListItem.SubItems.Count<1 then
      ListItem.SubItems.Add(Value)
    else
      ListItem.SubItems[0]:=Value;
  end;
  while ValuesListview.Items.Count>Defines.Count do
    ValuesListview.Items.Delete(ValuesListview.Items.Count-1);
  ValuesListview.EndUpdate;
  UpdateValue;
end;

procedure TCodeToolsDefinesDialog.UpdateValue;
var
  VariableName: String;
  Dir: String;
  Defines: TExpressionEvaluator;
  Value: string;
begin
  Dir:=TrimFilename(DirectoryCombobox.Text);
  if (ValuesListview.Selected=nil) or (DefineTree=nil)
  or (not FilenameIsAbsolute(Dir)) then begin
    ValueGroupbox.Caption:=lisCTDefnoVariableSelected;
    ValueSynedit.Lines.Text:='';
  end else begin
    VariableName:=ValuesListview.Selected.Caption;
    ValueGroupbox.Caption:=Format(lisCTDefVariable, [VariableName]);
    Defines:=DefineTree.GetDefinesForDirectory(Dir,false);
    Value:=Defines.Variables[VariableName];
    ValueSynedit.Lines.Text:=Value;
  end;
end;

procedure TCodeToolsDefinesDialog.ClearValues;
begin
  ValuesListview.Items.Clear;
end;

procedure TCodeToolsDefinesDialog.SetComboBox(AComboBox: TComboBox;
  const NewText: string);
var
  i: Integer;
begin
  i:=AComboBox.Items.IndexOf(NewText);
  if i<0 then
    AComboBox.Items.Add(NewText)
  else
    AComboBox.ItemIndex:=i;
  AComboBox.Text:=NewText;
end;

procedure TCodeToolsDefinesDialog.CodeToolsDefinesDialogCREATE(Sender: TObject);
var
  ListColumn: TListColumn;
begin
  Caption:=lisCTDefCodeToolsDirectoryValues;
  
  ListColumn:=ValuesListview.Columns.Add;
  ListColumn.Caption:=lisCTDefVariableName;
  ListColumn.Width:=150;
  ListColumn:=ValuesListview.Columns.Add;
  ListColumn.Caption:=dlgRunOValue;
  
  DirectoryGroupbox.Caption:=lisCodeToolsDefsInsertBehindDirectory;
  CloseButton.Caption:=lisMenuClose;
  
  DirectoryCombobox.Items.Assign(
    InputHistories.HistoryLists.GetList(hlCodeToolsDirectories,true));
  if DirectoryCombobox.Items.Count>0 then
    DirectoryCombobox.ItemIndex:=0
  else
    DirectoryCombobox.Text:='';
end;

procedure TCodeToolsDefinesDialog.CodeToolsDefinesDialogCLOSE(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  InputHistories.HistoryLists.GetList(hlCodeToolsDirectories,true).Assign(
    DirectoryCombobox.Items);
end;

procedure TCodeToolsDefinesDialog.DirectoryBrowseButtonCLICK(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  Filename: string;
begin
  OpenDialog:=TSelectDirectoryDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisCTDefChooseDirectory;
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    if OpenDialog.Execute then begin
      Filename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBox(DirectoryCombobox,Filename);
      UpdateValues;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TCodeToolsDefinesDialog.DirectoryComboboxCHANGE(Sender: TObject);
begin
  UpdateValues;
end;

initialization
  {$I codetoolsdefpreview.lrs}

end.

