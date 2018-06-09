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
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf, IDEDialogs,
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
  private
    FAddResourceItemDuplicates: integer;
    FResourceNameList: TStringList; // to keep resource names unique
    FResourceFileNameList: TStringList; // to keep resource file names unique
    // Used to know what was resource name before editing.
    FCurrentResName: string;
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

  FResourceNameList := TStringList.Create;
  FResourceNameList.Sorted := True;
  FResourceNameList.Duplicates := dupError;

  FResourceFileNameList := TStringList.Create;
  FResourceFileNameList.Sorted := True;
  FResourceFileNameList.Duplicates := dupError;
end;

destructor TResourcesOptionsFrame.Destroy;
begin
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

procedure TResourcesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  List: TResourceList;
  I: Integer;
begin
  FProject := (AOptions as TProjectIDEOptions).Project;
  lbResources.Items.Clear;
  List := FProject.ProjResources.UserResources.List;
  AddResourceBegin;
  try
    lbResources.Items.BeginUpdate;
    try
      for I := 0 to List.Count - 1 do
        AddResourceItem(List[I]^.FileName, List[I]^.ResType, List[I]^.ResName);
    finally
      lbResources.Items.EndUpdate;
    end;
  finally
    AddResourceEnd;
  end;
  btnClear.Enabled := lbResources.Items.Count > 0;
  FModified := False;
end;

procedure TResourcesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Project: TProject;
  I: Integer;
begin
  if not FModified then Exit;
  Project := (AOptions as TProjectIDEOptions).Project;
  Project.ProjResources.UserResources.List.Clear;
  for I := 0 to lbResources.Items.Count - 1 do
    Project.ProjResources.UserResources.List.AddResource(lbResources.Items[I].Caption,
      StrToResourceType(lbResources.Items[I].SubItems[LVSUBITEM_TYPE]), lbResources.Items[I].SubItems[LVSUBITEM_NAME]);
  Project.ProjResources.Modified := True;
end;

class function TResourcesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TResourcesOptionsFrame, ProjectOptionsResources);

end.

