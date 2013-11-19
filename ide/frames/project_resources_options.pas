unit project_resources_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Graphics, Forms, Controls, ComCtrls, Dialogs,
  LCLProc, LCLType,
  IDEOptionsIntf, IDEImagesIntf, IDEDialogs, Project,
  ProjectUserResources, LCLStrConsts, ExtCtrls, StdCtrls, LazarusIDEStrConsts;

type

  { TResourcesOptionsFrame }

  TResourcesOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbResourceType: TComboBox;
    edResourceName: TEdit;
    lblResourceType: TLabel;
    lblResourceName: TLabel;
    lbResources: TListView;
    dlgOpen: TOpenDialog;
    ToolBar1: TToolBar;
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    btnClear: TToolButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure cbResourceTypeChange(Sender: TObject);
    procedure edResourceNameEditingDone(Sender: TObject);
    procedure edResourceNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbResourcesInsert(Sender: TObject; Item: TListItem);
    procedure lbResourcesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FProject: TProject;
    procedure AddResource(AFileName: String);
    procedure AddResourceItem(ResFile: String; ResType: TUserResourceType; ResName: String);
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TResourcesOptionsFrame }

procedure TResourcesOptionsFrame.btnAddClick(Sender: TObject);
var
  FileName: String;
  aFilename: String;
begin
  if dlgOpen.Execute then
  begin
    for FileName in dlgOpen.Files do
    begin
      aFilename := Filename;
      if not FProject.IsVirtual then
        aFileName := CreateRelativePath(aFileName, FProject.ProjectDirectory);
      AddResource(aFileName);
    end;
  end;
end;

procedure TResourcesOptionsFrame.btnClearClick(Sender: TObject);
begin
  if IDEMessageDialog(lisConfirmDelete, rsResourceClear, mtConfirmation, [mbYes, mbNo]) = mrYes then
    lbResources.Items.Clear;
  btnClear.Enabled := lbResources.Items.Count > 0;
end;

procedure TResourcesOptionsFrame.btnDeleteClick(Sender: TObject);
begin
  if Assigned(lbResources.Selected) then
    lbResources.Items.Delete(lbResources.Selected.Index);
  btnClear.Enabled := lbResources.Items.Count > 0;
end;

procedure TResourcesOptionsFrame.cbResourceTypeChange(Sender: TObject);
begin
  lbResources.Selected.SubItems[0] := ResourceTypeToStr[TUserResourceType(cbResourceType.ItemIndex)];
end;

procedure TResourcesOptionsFrame.edResourceNameEditingDone(Sender: TObject);
begin
  lbResources.Selected.SubItems[1] := edResourceName.Text;
end;

procedure TResourcesOptionsFrame.edResourceNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    edResourceName.EditingDone;
    Key := 0;
  end;
end;

procedure TResourcesOptionsFrame.lbResourcesInsert(Sender: TObject;
  Item: TListItem);
begin
  btnClear.Enabled := lbResources.Items.Count > 0;
end;

procedure TResourcesOptionsFrame.lbResourcesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnDelete.Enabled := Assigned(lbResources.Selected);
  edResourceName.Enabled := Assigned(lbResources.Selected);
  if edResourceName.Enabled then
    edResourceName.Text := lbResources.Selected.SubItems[1]
  else
    edResourceName.Text := '';
  cbResourceType.Enabled := Assigned(lbResources.Selected);
  if cbResourceType.Enabled then
    cbResourceType.ItemIndex := Ord(StrToResourceType(lbResources.Selected.SubItems[0]))
  else
    cbResourceType.ItemIndex := -1;
end;

procedure TResourcesOptionsFrame.AddResourceItem(ResFile: String; ResType: TUserResourceType; ResName: String);
var
  Item: TListItem;
begin
  Item := lbResources.Items.Add;
  Item.Caption := ResFile;
  Item.SubItems.Add(ResourceTypeToStr[ResType]);
  Item.SubItems.Add(ResName);
end;

procedure TResourcesOptionsFrame.AddResource(AFileName: String);
var
  ResName, Ext: String;
begin
  Ext := UTF8UpperCase(ExtractFileExt(AFileName));
  ResName := UTF8UpperCase(ExtractFileNameOnly(AFileName));
  case Ext of
    '.BMP': AddResourceItem(AFileName, rtBitmap, ResName);
    '.CUR': AddResourceItem(AFileName, rtCursor, ResName);
    '.ICO': AddResourceItem(AFileName, rtIcon, ResName);
    //'.FNT', '.FON', '.TTF': AddResourceItem(AFileName, rtFont, ResName);
  else
    AddResourceItem(AFileName, rtRCData, ResName);
  end;
end;

function TResourcesOptionsFrame.GetTitle: string;
begin
  Result := dlgPOResources;
end;

procedure TResourcesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  rt: TUserResourceType;
begin
  ToolBar1.Images := IDEImages.Images_16;
  lbResources.Column[0].Caption := rsResourceFileName;
  lbResources.Column[1].Caption := rsResourceType;
  lbResources.Column[2].Caption := rsResource;
  btnAdd.Caption := lisBtnAdd;
  btnDelete.Caption := lisBtnDelete;
  btnAdd.ImageIndex := IDEImages.LoadImage(16, 'laz_add');
  btnDelete.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');
  btnClear.Caption := lisDeleteAll;
  btnClear.ImageIndex := IDEImages.LoadImage(16, 'menu_clean');
  lblResourceName.Caption := rsResource + ':';
  lblResourceType.Caption := rsResourceType + ':';
  for rt := Low(TUserResourceType) to High(TUserResourceType) do
    cbResourceType.Items.Add(ResourceTypeToStr[rt]);
  // system resources can be:
  // 1. Graphic files
  // 2. Font files
  // 3. Any files
  dlgOpen.Filter := GraphicFilter(TGraphic)+'|'+Format(rsAllFiles,[GetAllFilesMask, GetAllFilesMask,'']);
end;

procedure TResourcesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Project: TProject absolute AOptions;
  List: TResourceList;
  I: Integer;
begin
  FProject := Project;
  lbResources.Items.Clear;
  List := Project.ProjResources.UserResources.List;
  lbResources.Items.BeginUpdate;
  try
    for I := 0 to List.Count - 1 do
      AddResourceItem(List[I]^.FileName, List[I]^.ResType, List[I]^.ResName);
  finally
    lbResources.Items.EndUpdate;
  end;
  btnClear.Enabled := lbResources.Items.Count > 0;
end;

procedure TResourcesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Project: TProject absolute AOptions;
  I: Integer;
begin
  Project.ProjResources.UserResources.List.Clear;
  for I := 0 to lbResources.Items.Count - 1 do
    Project.ProjResources.UserResources.List.AddResource(lbResources.Items[I].Caption,
      StrToResourceType(lbResources.Items[I].SubItems[0]), lbResources.Items[I].SubItems[1]);
end;

class function TResourcesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TResourcesOptionsFrame, ProjectOptionsResources);

end.

