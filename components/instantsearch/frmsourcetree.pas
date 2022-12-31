unit frmSourceTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  EditBtn, Menus, ideinstantsearch;

type

  { TSourceTreeDefinitionForm }

  TSourceTreeDefinitionForm = class(TForm)
    bpSourceTree: TButtonPanel;
    CBEnabled: TCheckBox;
    cbRecurse: TCheckBox;
    CBAllFiles: TCheckBox;
    edtBaseDir: TDirectoryEdit;
    edtName: TEdit;
    edtExtensions: TEdit;
    lblName: TLabel;
    lblBaseDir: TLabel;
    lblExtensions: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FTree: TSourceTreeDefinition;
    procedure CheckExtensions;
    procedure SetTree(AValue: TSourceTreeDefinition);
    procedure DefToForm;
    Procedure FormToDef;
  public
    Property Tree : TSourceTreeDefinition Read FTree Write SetTree;
  end;

var
  SourceTreeDefinitionForm: TSourceTreeDefinitionForm;

implementation

uses instantsearchstrings;

{$R *.lfm}

{ TSourceTreeDefinitionForm }

procedure TSourceTreeDefinitionForm.FormCreate(Sender: TObject);
begin
  lblName.Caption:=lrsTreeName;
  edtName.TextHint:=lrsTreeNameHint;
  lblBaseDir.Caption:=lrsTreeBaseDir;
  edtBaseDir.TextHint:=lrsTreeBaseDirHint;
  cbrecurse.Caption:=lrsTreeRecurse;
  cbAllFiles.Caption:=lrsTreeAllFiles;
  cbEnabled.Caption:=lrsTreeEnabled;
  lblExtensions.Caption:=lrsTreeExtensions;
  edtExtensions.TextHint:=lrsTreeExtensionsHint;
end;

procedure TSourceTreeDefinitionForm.SetTree(AValue: TSourceTreeDefinition);
begin
  if FTree=AValue then Exit;
  FTree:=AValue;
  DefToForm;
end;

procedure TSourceTreeDefinitionForm.DefToForm;
begin
  edtName.Text:=Tree.Name;
  edtBaseDir.Directory:=Tree.BaseDir;
  edtExtensions.Text:=Tree.Extensions;
  cbRecurse.Checked:=Tree.Recurse;
  CBAllFiles.Checked:=Tree.AllFiles;
  CBEnabled.Checked:=Tree.Enabled;
  CheckExtensions;
end;

procedure TSourceTreeDefinitionForm.CheckExtensions;

begin
  edtExtensions.Enabled:=Not cbAllFiles.Checked;
end;

procedure TSourceTreeDefinitionForm.FormToDef;
begin
  Tree.Name       := edtName.Text;
  Tree.BaseDir    := edtBaseDir.Directory;
  Tree.Extensions := edtExtensions.Text;
  Tree.Recurse    := cbRecurse.Checked;
  Tree.AllFiles   := CBAllFiles.Checked;
  Tree.Enabled    := CBEnabled.Checked;
end;

procedure TSourceTreeDefinitionForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult=mrOK then
    FormToDef;
end;

procedure TSourceTreeDefinitionForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

Var
  T : TSourceTreeDefinition;

begin
  CanClose:=(edtName.Text<>'');
  if Not CanClose then
    ShowMessage(lrsErrNameCannotBeEmpty)
  else
    begin
    CanClose:=(edtBaseDir.Directory<>'') and DirectoryExists(edtBaseDir.Directory);
    if not CanClose Then
      ShowMessage(Format(lrsErrPathDoesNotExist,[edtBaseDir.Directory]))
    else if (Tree.Collection is TSourceTreeDefinitionList) then
      begin
      T:=(Tree.Collection as TSourceTreeDefinitionList).FindByName(EdtName.text);
      CanClose:=(T=Nil) or (T=Tree);
      if not CanClose then
         ShowMessage(Format(lrsErrorDuplicateName,[edtName.Text]));
      end;
    end;
end;

end.

