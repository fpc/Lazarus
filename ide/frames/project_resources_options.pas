unit project_resources_options;

{$mode objfpc}{$H+}

{$I ide.inc}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, LCLStrConsts, Controls, ComCtrls, StdCtrls, Dialogs, Graphics,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8,
  // BuildIntf
  ProjectResourcesIntf,
  // IdeIntf
  IdeIntfStrConsts, IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf, IDEDialogs,
  // IDE
  Project, ProjectUserResources, LazarusIDEStrConsts;

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
    procedure edResourceNameKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure lbResourcesInsert(Sender: TObject; {%H-}Item: TListItem);
    procedure lbResourcesKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure lbResourcesSelectItem(Sender: TObject; {%H-}Item: TListItem; {%H-}Selected: Boolean);
  private
    FProject: TProject;
    FModified: Boolean;
    FSubscribed: TAbstractProjectUserResources;  // whom OnResourcesChanged is registered with
  private
    FAddResourceItemDuplicates: integer;
    FResourceNameList: TStringListUTF8Fast;     // to keep resource names unique
    FResourceFileNameList: TStringListUTF8Fast; // to keep resource file names unique
    // Used to know what was resource name before editing.
    FCurrentResName: string;
    // Reload the list view from the project model.
    procedure ReloadResources;
    // Called when the resources are changed from outside (e.g. a plugin).
    procedure OnResourcesChanged(Sender: TObject);
    // Begin adding resources.
    procedure AddResourceBegin;
    // Try to add resource. Result is false if resource is duplicate.
    function AddResource(AFileName: String): boolean;
    // Finish adding resources. If there were duplicate resources message will be shown.
    procedure AddResourceEnd;
    // Try to add resource item. Result is false if resource is duplicate.
    function AddResourceItem(ResFile: String; ResType: TUserResourceType; ResName: String): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

const
  LVSUBITEM_TYPE = 0;
  LVSUBITEM_NAME = 1;

{ TResourcesOptionsFrame }

procedure TResourcesOptionsFrame.btnAddClick(Sender: TObject);
var
  FileName: String;
  aFilename: String;
begin
  if dlgOpen.Execute() and (dlgOpen.Files.Count <> 0) then
  begin
    AddResourceBegin;
    try
      for FileName in dlgOpen.Files do
      begin
        aFilename := Filename;
        if not FProject.IsVirtual then
          aFileName := CreateRelativePath(aFileName, FProject.Directory);

        AddResource(aFileName);
      end;
    finally
      AddResourceEnd;
    end;
  end;
end;

procedure TResourcesOptionsFrame.btnClearClick(Sender: TObject);
begin
  if IDEMessageDialog(lisConfirmDelete, rsResourceClear, mtConfirmation, [mbYes, mbNo]) = mrYes then
  begin
    lbResources.Items.Clear;
    FResourceNameList.Clear;
    FResourceFileNameList.Clear;
    FModified := True;
  end;
  btnClear.Enabled := lbResources.Items.Count > 0;
end;

procedure TResourcesOptionsFrame.btnDeleteClick(Sender: TObject);
var
  resName, resFileName: String;
begin
  if Assigned(lbResources.Selected) then
  begin
    resName := lbResources.Selected.SubItems[LVSUBITEM_NAME];
    resFileName := lbResources.Selected.Caption;

    FResourceNameList.Delete(FResourceNameList.IndexOf(resName));
    FResourceFileNameList.Delete(FResourceFileNameList.IndexOf(resFileName));

    lbResources.Items.Delete(lbResources.Selected.Index);
    FModified := True;
  end;
  btnClear.Enabled := lbResources.Items.Count > 0;
end;

procedure TResourcesOptionsFrame.cbResourceTypeChange(Sender: TObject);
begin
  if Assigned(lbResources.Selected) then
  begin
    lbResources.Selected.SubItems[LVSUBITEM_TYPE] :=
                 ResourceTypeToStr[TUserResourceType(cbResourceType.ItemIndex)];
    FModified:=true;
  end;
end;

procedure TResourcesOptionsFrame.edResourceNameEditingDone(Sender: TObject);
var
  newResName: string;
begin
  if Assigned(lbResources.Selected) then
  begin
    newResName := edResourceName.Text;
    // Exit if resName wasn't changed.
    if newResName = FCurrentResName then
      exit;
    // Check if new name is unique.
    if FResourceNameList.IndexOf(newResName) <> -1 then
    begin
      // If new name is not unique show message and restore edited name.
      ShowMessage(lisResourceNameMustBeUnique);
      edResourceName.Text := FCurrentResName;
      edResourceName.SetFocus; // assume user want to continue editing
      exit;
    end;
    // Remove old name.
    FResourceNameList.Delete(FResourceNameList.IndexOf(FCurrentResName));
    // Add new name.
    FResourceNameList.Add(newResName);
    // Update in list view.
    lbResources.Selected.SubItems[LVSUBITEM_NAME] := newResName;
    // Update current name.
    FCurrentResName := newResName;
    // Resource has been changed
    FModified := True;
  end;
end;

procedure TResourcesOptionsFrame.edResourceNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    // Shouldn't call edResourceName.EditingDone because when control will lose
    // focus it will call EditingDone one more time.
    // Instead set focus to list view.
    lbResources.SetFocus;
    Key := 0;
  end;
end;

procedure TResourcesOptionsFrame.lbResourcesInsert(Sender: TObject; Item: TListItem);
begin
  btnClear.Enabled := lbResources.Items.Count > 0;
end;

procedure TResourcesOptionsFrame.lbResourcesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_INSERT:
      btnAddClick(nil);
    VK_DELETE:
      btnDeleteClick(nil);
  end;
end;

procedure TResourcesOptionsFrame.lbResourcesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnDelete.Enabled := Assigned(lbResources.Selected);

  edResourceName.Enabled := Assigned(lbResources.Selected);
  if edResourceName.Enabled then
    edResourceName.Text := lbResources.Selected.SubItems[LVSUBITEM_NAME]
  else
    edResourceName.Text := '';

  FCurrentResName := edResourceName.Text;

  cbResourceType.Enabled := Assigned(lbResources.Selected);
  if cbResourceType.Enabled then
    cbResourceType.ItemIndex := Ord(StrToResourceType(lbResources.Selected.SubItems[LVSUBITEM_TYPE]))
  else
    cbResourceType.ItemIndex := -1;
end;

procedure TResourcesOptionsFrame.AddResourceBegin;
begin
  // Initialize duplicated resource counter.
  FAddResourceItemDuplicates := 0;
end;

function TResourcesOptionsFrame.AddResource(AFileName: String): boolean;
var
  ResName, Ext: String;
begin
  Ext := UTF8UpperCase(ExtractFileExt(AFileName));
  ResName := UTF8UpperCase(ExtractFileNameOnly(AFileName));
  case Ext of
    '.BMP': Result := AddResourceItem(AFileName, rtBitmap, ResName);
    '.CUR': Result := AddResourceItem(AFileName, rtCursor, ResName);
    '.ICO': Result := AddResourceItem(AFileName, rtIcon, ResName);
    //'.FNT', '.FON', '.TTF': Result := AddResourceItem(AFileName, rtFont, ResName);
  else
    Result := AddResourceItem(AFileName, rtRCData, ResName);
  end;
end;

procedure TResourcesOptionsFrame.AddResourceEnd;
begin
  if FAddResourceItemDuplicates <> 0 then
  begin
    ShowMessageFmt(lisFailedToAddNNotUniqueResources, [FAddResourceItemDuplicates]);
    FAddResourceItemDuplicates := 0;
  end;
end;

function TResourcesOptionsFrame.AddResourceItem(ResFile: String;
  ResType: TUserResourceType; ResName: String): boolean;
var
  Item: TListItem;
begin
  if FResourceFileNameList.IndexOf(ResFile) <> -1 then
  begin
    // Such file name is already in list.
    // Ignore adding.
    exit(true);
  end;

  if FResourceNameList.IndexOf(ResName) <> -1 then
  begin
    // Such res. name already exists.
    // Don't add it twice.
    inc(FAddResourceItemDuplicates);
    exit(false);
  end;

  Item := lbResources.Items.Add;
  Item.Caption := ResFile;                       // path
  Item.SubItems.Add(ResourceTypeToStr[ResType]); // type
  Item.SubItems.Add(ResName);                    // name

  FResourceFileNameList.Add(ResFile);
  FResourceNameList.Add(ResName);
  FModified := True;
  exit(true);
end;

constructor TResourcesOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FResourceNameList := TStringListUTF8Fast.Create;
  FResourceNameList.Sorted := True;
  FResourceNameList.Duplicates := dupError;

  FResourceFileNameList := TStringListUTF8Fast.Create;
  FResourceFileNameList.Sorted := True;
  FResourceFileNameList.Duplicates := dupError;
end;

destructor TResourcesOptionsFrame.Destroy;
begin
  if Assigned(FSubscribed) then
    FSubscribed.RemoveChangeHandler(@OnResourcesChanged);
  FResourceNameList.Free;
  FResourceFileNameList.Free;
  inherited Destroy;
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
  btnAdd.ImageIndex := IDEImages.LoadImage('laz_add');
  btnDelete.ImageIndex := IDEImages.LoadImage('laz_delete');
  btnClear.Caption := lisDeleteAll;
  btnClear.ImageIndex := IDEImages.LoadImage('menu_clean');
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

procedure TResourcesOptionsFrame.ReloadResources;
var
  Res: TAbstractProjectUserResources;
  I: Integer;
  Info: TProjectUserResourceInfo;
begin
  lbResources.Items.Clear;
  FResourceNameList.Clear;
  FResourceFileNameList.Clear;
  Res := FProject.ProjResources.UserResources;
  AddResourceBegin;
  try
    lbResources.Items.BeginUpdate;
    try
      for I := 0 to Res.Count - 1 do
      begin
        Info := Res[I];
        AddResourceItem(Info.FileName, Info.ResType, Info.ResName);
      end;
    finally
      lbResources.Items.EndUpdate;
    end;
  finally
    AddResourceEnd;
  end;
  btnClear.Enabled := lbResources.Items.Count > 0;
end;

procedure TResourcesOptionsFrame.OnResourcesChanged(Sender: TObject);
begin
  // A plugin changed the resources while this dialog is open. Refresh the list
  // view. Note: this discards edits not yet committed via WriteSettings.
  if FProject = nil then
    Exit;
  ReloadResources;
  FModified := False;
end;

procedure TResourcesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  NewRes: TAbstractProjectUserResources;
begin
  FProject := (AOptions as TProjectIDEOptions).Project;
  NewRes := FProject.ProjResources.UserResources;
  if FSubscribed <> NewRes then
  begin
    if Assigned(FSubscribed) then
      FSubscribed.RemoveChangeHandler(@OnResourcesChanged);
    FSubscribed := NewRes;
    FSubscribed.AddChangeHandler(@OnResourcesChanged);
  end;
  ReloadResources;
  FModified := False;
end;

procedure TResourcesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Project: TProject;
  List: TResourceList;
  NewItems: TFPList;
  I, OldIndex: Integer;
  Item: TResourceItem;
  aFileName, aResName: String;
  aResType: TUserResourceType;
begin
  if not FModified then Exit;
  Project := (AOptions as TProjectIDEOptions).Project;
  List := Project.ProjResources.UserResources.List;

  // Compare the old list with the new one and keep the old TResourceItem for
  // unchanged files, so their cached file content survives the edit.
  NewItems := TFPList.Create;
  try
    for I := 0 to lbResources.Items.Count - 1 do
    begin
      aFileName := lbResources.Items[I].Caption;
      aResType := StrToResourceType(lbResources.Items[I].SubItems[LVSUBITEM_TYPE]);
      aResName := lbResources.Items[I].SubItems[LVSUBITEM_NAME];

      OldIndex := List.IndexOfFileName(aFileName);
      if OldIndex >= 0 then
      begin
        // Reuse the existing item (keeps its file content cache).
        Item := List[OldIndex];
        List.Extract(Item); // detach without freeing
        // Update the filename too, the casing might have changed.
        Item.FileName := aFileName;
        Item.ResType := aResType;
        Item.ResName := aResName;
      end
      else
      begin
        Item := TResourceItem.Create;
        Item.FileName := aFileName;
        Item.ResType := aResType;
        Item.ResName := aResName;
      end;
      NewItems.Add(Item);
    end;

    // Whatever is left in List are removed resources -> free them.
    List.Clear;
    // Put the reused/new items back in the order shown in the dialog.
    for I := 0 to NewItems.Count - 1 do
      List.Add(TResourceItem(NewItems[I]));
  finally
    NewItems.Free;
  end;

  Project.ProjResources.Modified := True;
end;

class function TResourcesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TResourcesOptionsFrame, ProjectOptionsResources);

end.

