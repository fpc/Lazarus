unit fppkg_packageoptionsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Dialogs,
  ActnList, ComboEx,
  PackageIntf,
  IDEOptEditorIntf,
  IDEOptionsIntf, SynEdit,
  fppkg_packagevariant;

type

  { TFppkgPackageOptionsFrm }

  TFppkgPackageOptionsFrm = class(TAbstractIDEOptionsEditor)
    aAddPackageVariant: TAction;
    aAddPackageVariantItem: TAction;
    aFileRemove: TAction;
    aFileAdd: TAction;
    aDeletePackageVariantItem: TAction;
    aDeletePackageVariant: TAction;
    ActionList: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    cbBuildMethod: TComboBox;
    cbPackageVariants: TComboBox;
    cbProjectFiles: TComboBox;
    gbBuildMethod: TGroupBox;
    gbPackageVariant: TGroupBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lbPackageFiles: TListBox;
    lPackageItem: TLabel;
    lbPackageVariant: TListBox;
    lPackageItem1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pPackageVariantItem: TPanel;
    seCompilerOptions: TSynEdit;
    seCustomFPMakeCode: TSynEdit;
    procedure aAddPackageVariantExecute(Sender: TObject);
    procedure aAddPackageVariantItemExecute(Sender: TObject);
    procedure aDeletePackageVariantExecute(Sender: TObject);
    procedure aDeletePackageVariantItemExecute(Sender: TObject);
    procedure aDeletePackageVariantItemUpdate(Sender: TObject);
    procedure aDeletePackageVariantUpdate(Sender: TObject);
    procedure aFileAddExecute(Sender: TObject);
    procedure aFileAddUpdate(Sender: TObject);
    procedure aFileRemoveExecute(Sender: TObject);
    procedure aFileRemoveUpdate(Sender: TObject);
    procedure cbPackageVariantsChange(Sender: TObject);
    procedure lbPackageVariantClick(Sender: TObject);
    function GetCurrentPackageVariant: TFppkgPackageVariant;
    function GetCurrentPackageVariantItem: TFppkgPackageVariantItem;
    procedure seCompilerOptionsChange(Sender: TObject);
  protected
    FModified: Boolean;
    FPackageVariantList: TFppkgPackageVariantList;
    FPackageList: TStrings;
    procedure ShowPackageVariants();
    procedure ShowCurrentPackageVariant();
    procedure ShowCurrentPackageVariantItem();
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

var
  FppkgPackageOptionID: integer = 500;


{$R *.lfm}

resourcestring
  lisFppkgPckOptsTitle = 'Fppkg';
  lisFppkgPckOptsBuildMethod = 'Supported build methods';
  lisFppkgBuildMethodFPMake = 'FPMake';
  lisFppkgBuildMethodLazarus = 'Lazbuild';
  lisFppkgBuildMethodBoth = 'Both';


{ TFppkgPackageOptionsFrm }

procedure TFppkgPackageOptionsFrm.lbPackageVariantClick(Sender: TObject);
begin
  ShowCurrentPackageVariantItem();
end;

function TFppkgPackageOptionsFrm.GetCurrentPackageVariant: TFppkgPackageVariant;
begin
  if cbPackageVariants.ItemIndex > -1 then
    Result := FPackageVariantList.Items[cbPackageVariants.ItemIndex]
  else
    Result := Nil;
end;

function TFppkgPackageOptionsFrm.GetCurrentPackageVariantItem: TFppkgPackageVariantItem;
begin
  if not Assigned(GetCurrentPackageVariant) then
    Result := Nil
  else if lbPackageVariant.ItemIndex > -1 then
    Result := GetCurrentPackageVariant.Items[lbPackageVariant.ItemIndex]
  else
    Result := Nil;
end;

procedure TFppkgPackageOptionsFrm.seCompilerOptionsChange(Sender: TObject);
begin
  GetCurrentPackageVariantItem.CompilerOptions.Assign(seCompilerOptions.Lines);
end;

procedure TFppkgPackageOptionsFrm.ShowPackageVariants();
var
  i: Integer;
begin
  for i := 0 to FPackageVariantList.Count -1 do
    begin
    if cbPackageVariants.Items.Count <= i then
      cbPackageVariants.Items.Add(FPackageVariantList[i].Name)
    else
      cbPackageVariants.Items[i] := FPackageVariantList[i].Name;
    end;
  for i := cbPackageVariants.Items.Count -1 downto FPackageVariantList.Count do
    cbPackageVariants.Items.Delete(i);
  if FPackageVariantList.Count > 0 then
    begin
    if cbPackageVariants.ItemIndex = -1 then
      cbPackageVariants.ItemIndex := 0;
    end
  else
    begin
    cbPackageVariants.ItemIndex := -1;
    end;
  ShowCurrentPackageVariant;
end;

procedure TFppkgPackageOptionsFrm.ShowCurrentPackageVariant();
var
  CurrentPackageVariant: TFppkgPackageVariant;
  i: Integer;
begin
  CurrentPackageVariant := GetCurrentPackageVariant;
  if Assigned(CurrentPackageVariant) then
    begin
    for i := 0 to CurrentPackageVariant.Items.Count -1 do
      begin
      if lbPackageVariant.Items.Count <= i then
        lbPackageVariant.Items.Add(CurrentPackageVariant.Items[i].Name)
      else
        lbPackageVariant.Items[i] := CurrentPackageVariant.Items[i].Name;
      end;
    for i := lbPackageVariant.Count -1 downto CurrentPackageVariant.Items.Count do
      lbPackageVariant.Items.Delete(i);
    Panel2.Visible := True;
    end
  else
    begin
    Panel2.Visible := False;
    end;
  ShowCurrentPackageVariantItem();
end;

procedure TFppkgPackageOptionsFrm.ShowCurrentPackageVariantItem();
var
  PackageVariantItem: TFppkgPackageVariantItem;
  i, StoredItemIndex: Integer;
  s: String;
begin
  PackageVariantItem := GetCurrentPackageVariantItem;
  if Assigned(PackageVariantItem) then
    begin
    pPackageVariantItem.Visible := True;
    lPackageItem.Caption := Format('Package variant [%s]-[%s]', [GetCurrentPackageVariant.Name, PackageVariantItem.Name]);
    seCompilerOptions.Lines.Assign(PackageVariantItem.CompilerOptions);
    StoredItemIndex := cbProjectFiles.ItemIndex;
    lbPackageFiles.Clear;
    cbProjectFiles.Clear;

    for i := 0 to FPackageList.Count -1 do
      begin
      s := FPackageList.Strings[i];
      if PackageVariantItem.PackageFiles.IndexOf(s) > -1 then
        lbPackageFiles.Items.Add(s)
      else
        cbProjectFiles.Items.Add(s);
      end;
    cbProjectFiles.ItemIndex := min(cbProjectFiles.Items.Count -1, StoredItemIndex);

    end
  else
    begin
    pPackageVariantItem.Visible := False;
    end;
end;

procedure TFppkgPackageOptionsFrm.aDeletePackageVariantUpdate(Sender: TObject);
begin
  aDeletePackageVariant.Enabled := cbPackageVariants.ItemIndex > -1;
end;

procedure TFppkgPackageOptionsFrm.aFileAddExecute(Sender: TObject);
begin
  if cbProjectFiles.ItemIndex > -1 then
    GetCurrentPackageVariantItem.PackageFiles.Add(cbProjectFiles.Text);
  ShowCurrentPackageVariantItem();
  FModified := True;
end;

procedure TFppkgPackageOptionsFrm.aFileAddUpdate(Sender: TObject);
begin
  aFileAdd.Enabled := cbProjectFiles.ItemIndex > -1;
end;

procedure TFppkgPackageOptionsFrm.aFileRemoveExecute(Sender: TObject);
var
  i: Integer;
begin
  i := lbPackageFiles.ItemIndex;
  if i > -1 then
    GetCurrentPackageVariantItem.PackageFiles.Delete(GetCurrentPackageVariantItem.PackageFiles.IndexOf(lbPackageFiles.Items[i]));
  ShowCurrentPackageVariantItem();
  FModified := True;
end;

procedure TFppkgPackageOptionsFrm.aFileRemoveUpdate(Sender: TObject);
begin
  aFileRemove.Enabled := lbPackageFiles.ItemIndex > -1;
end;

procedure TFppkgPackageOptionsFrm.cbPackageVariantsChange(Sender: TObject);
begin
  ShowCurrentPackageVariant;
end;

procedure TFppkgPackageOptionsFrm.aDeletePackageVariantExecute(Sender: TObject);
var
  PackageVariant: TFppkgPackageVariant;
begin
  PackageVariant := FPackageVariantList.FindItemByName(cbPackageVariants.Text);
  if Assigned(PackageVariant) then
    begin
    if MessageDlg('Fppkg', Format('Are you sure you want to remove the package variant [%s]?', [PackageVariant.Name]), mtConfirmation, mbYesNo, 0) = mrYes then
      begin
      FPackageVariantList.Remove(PackageVariant);
      end;
    end;
  FModified := True;
  ShowPackageVariants;
end;

procedure TFppkgPackageOptionsFrm.aDeletePackageVariantItemExecute(Sender: TObject);
var
  PackageVariantItem: TFppkgPackageVariantItem;
begin
  PackageVariantItem := GetCurrentPackageVariant.Items.FindItemByName(lbPackageVariant.Items[lbPackageVariant.ItemIndex]);
  if Assigned(PackageVariantItem) then
    begin
    if MessageDlg('Fppkg', Format('Are you sure you want to remove the item [%s]?', [PackageVariantItem.Name]), mtConfirmation, mbYesNo, 0) = mrYes then
      begin
      GetCurrentPackageVariant.Items.Remove(PackageVariantItem);
      end;
    end;
  FModified := True;
  ShowCurrentPackageVariant;
end;

procedure TFppkgPackageOptionsFrm.aDeletePackageVariantItemUpdate(Sender: TObject);
begin
  aDeletePackageVariantItem.Enabled := lbPackageVariant.ItemIndex > -1;
end;

procedure TFppkgPackageOptionsFrm.aAddPackageVariantExecute(Sender: TObject);
var
  NameVal: String;
  Variant: TFppkgPackageVariant;
begin
  if InputQuery('Fppkg', 'Enter the name of the new package-variants', false, NameVal) then
    begin
    if Assigned(FPackageVariantList.FindItemByName(NameVal)) then
      begin
      MessageDlg('Fppkg', Format('A packagevariant with the name [%s] already exists.', [NameVal]), mtError, [mbOK], 0);
      Exit;
      end;
    Variant := TFppkgPackageVariant.Create;
    Variant.Name := NameVal;
    FPackageVariantList.Add(Variant);
    end;
  FModified := True;
  ShowPackageVariants();
end;

procedure TFppkgPackageOptionsFrm.aAddPackageVariantItemExecute(Sender: TObject);
var
  CurrentVariant: TFppkgPackageVariant;
  NameVal: string;
  Item: TFppkgPackageVariantItem;
begin
  CurrentVariant := GetCurrentPackageVariant;
  Assert(Assigned(CurrentVariant));

  if InputQuery('Fppkg', 'Enter the name of the new item', false, NameVal) then
    begin
    if Assigned(CurrentVariant.Items.FindItemByName(NameVal)) then
      begin
      MessageDlg('Fppkg', Format('An item with the name [%s] already exists.', [NameVal]), mtError, [mbOK], 0);
      Exit;
      end;
    Item := TFppkgPackageVariantItem.Create;
    Item.Name := NameVal;
    CurrentVariant.Items.Add(Item);
    end;
  FModified := True;
  ShowCurrentPackageVariant();
end;

function TFppkgPackageOptionsFrm.GetTitle: String;
begin
  Result := lisFppkgPckOptsTitle;
end;

procedure TFppkgPackageOptionsFrm.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  gbBuildMethod.Caption := lisFppkgPckOptsBuildMethod;
  cbBuildMethod.Items.Clear;
  cbBuildMethod.Items.Add(lisFppkgBuildMethodLazarus);
  cbBuildMethod.Items.Add(lisFppkgBuildMethodFPMake);
  cbBuildMethod.Items.Add(lisFppkgBuildMethodBoth);
end;

procedure TFppkgPackageOptionsFrm.ReadSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TIDEPackage;
  i: Integer;
  f: TLazPackageFile;
begin
  LazPackage := (AOptions as TAbstractPackageIDEOptions).Package;
  cbBuildMethod.ItemIndex := Ord(LazPackage.BuildMethod);

  LazPackage.CustomOptions.AppendBasePath('Fppkg/');
  try
    LazPackage.CustomOptions.AppendBasePath('PackageVariants/');
    try
      FPackageVariantList.Load(LazPackage.CustomOptions);
    finally
      LazPackage.CustomOptions.UndoAppendBasePath;
    end;
    LazPackage.CustomOptions.GetValue('CustomCode', seCustomFPMakeCode.Lines);
  finally
    LazPackage.CustomOptions.UndoAppendBasePath;
  end;

  FPackageList.Clear;
  for i := 0 to LazPackage.FileCount -1 do
    if LazPackage.Files[i].GetShortFilename(false) <> '' then
      FPackageList.Add(LazPackage.Files[i].GetShortFilename(false));
  ShowPackageVariants();

  FModified := False;
end;

procedure TFppkgPackageOptionsFrm.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TIDEPackage;
begin
  LazPackage := (AOptions as TAbstractPackageIDEOptions).Package;
  LazPackage.BuildMethod := TBuildMethod(cbBuildMethod.ItemIndex);

  if FModified then
    begin
    LazPackage.Modified := True;
    FModified := False;
    end;


  LazPackage.CustomOptions.AppendBasePath('Fppkg/');
  try
    if FPackageVariantList.Count>0 then
      begin
      LazPackage.CustomOptions.AppendBasePath('PackageVariants/');
      try
        FPackageVariantList.Save(LazPackage.CustomOptions);
      finally
        LazPackage.CustomOptions.UndoAppendBasePath;
      end;
      end;
    LazPackage.CustomOptions.SetValue('CustomCode', seCustomFPMakeCode.Lines);
  finally
    LazPackage.CustomOptions.UndoAppendBasePath;
  end;
end;

class function TFppkgPackageOptionsFrm.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TAbstractPackageIDEOptions;
end;

constructor TFppkgPackageOptionsFrm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPackageVariantList := TFppkgPackageVariantList.Create(True);
  FPackageList := TStringList.Create;
  TStringList(FPackageList).Sorted := True;
  TStringList(FPackageList).Duplicates := dupIgnore;
end;

destructor TFppkgPackageOptionsFrm.Destroy;
begin
  FPackageVariantList.Free;
  FPackageList.Free;
  inherited Destroy;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TFppkgPackageOptionsFrm, FppkgPackageOptionID);
end.

