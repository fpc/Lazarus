unit opkman_createrepositorypackagefr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Dialogs,
  LazFileUtils, Graphics, Menus, Buttons, Laz2_XMLCfg, opkman_VirtualTrees,
  md5, fpjson, LCLIntf, EditBtn, opkman_serializablepackages, opkman_zipper,
  opkman_uploader;

type
  PData = ^TData;
  TData = record
    FPackageRelativePath: String;
    FPackageBaseDir: String;
    FFullPath: String;
    FDataType: Integer;
    FName: String;
    FDisplayName: String;
    FPackageType: TPackageType;
    FAuthor: String;
    FDescription: String;
    FLicense: String;
    FVersionAsString: String;
    FDependenciesAsString: String;
    FCategory: String;
    FLazCompatibility: String;
    FFPCCompatibility: String;
    FSupportedWidgetSet: String;
    FHomePageURL: String;
    FDownloadURL: String;
    FSVNURL: String;
  end;

  TPackageOperation = (poCreate, poSubmit);
  { TCreateRepositoryPackagefr }

  TCreateRepositoryPackagefr = class(TFrame)
    bCancel: TButton;
    bCreate: TButton;
    Bevel1: TBevel;
    bHelp: TButton;
    bOptions: TButton;
    bSubmit: TButton;
    cbJSONForUpdates: TCheckBox;
    edCategories: TEdit;
    edPackageDir: TDirectoryEdit;
    edDownloadURL: TEdit;
    edDisplayName: TEdit;
    edSVNURL: TEdit;
    edFPCCompatibility: TEdit;
    edHomePageURL: TEdit;
    edLazCompatibility: TEdit;
    edSupportedWidgetset: TEdit;
    imTree: TImageList;
    lbCategory: TLabel;
    lbDownloadURL: TLabel;
    lbDisplayName: TLabel;
    lbSVNURL: TLabel;
    lbFPCCompatibility: TLabel;
    lbHomePageURL: TLabel;
    lbLazCompatibility: TLabel;
    lbOF1: TLabel;
    lbOF2: TLabel;
    lbOF3: TLabel;
    lbOF4: TLabel;
    lbPackagedir: TLabel;
    lbSupportedWidgetSet: TLabel;
    pnB: TPanel;
    pnButtons: TPanel;
    pnCategories: TPanel;
    pnPackageData: TPanel;
    pnBrowse: TPanel;
    pnCategory: TPanel;
    pnMessage: TPanel;
    pnPackages: TPanel;
    pnData: TPanel;
    SDD: TSelectDirectoryDialog;
    spCategories: TSpeedButton;
    spMain: TSplitter;
    procedure bCancelClick(Sender: TObject);
    procedure bCreateClick(Sender: TObject);
    procedure bHelpClick(Sender: TObject);
    procedure bOptionsClick(Sender: TObject);
    procedure bSubmitClick(Sender: TObject);
    procedure edPackageDirAcceptDirectory(Sender: TObject; var Value: String);
    procedure edPackageDirButtonClick(Sender: TObject);
    procedure pnBrowseResize(Sender: TObject);
    procedure spCategoriesClick(Sender: TObject);
  private
    FVSTPackages: TVirtualStringTree;
    FVSTPackageData: TVirtualStringTree;
    FPackageZipper: TPackageZipper;
    FPackageDir: String;
    FPackageName: String;
    FPackageFile: String;
    FDestDir: String;
    FPackageOperation: TPackageOperation;
    procedure VSTPackagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTPackagesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTPackagesFocusChanging(Sender: TBaseVirtualTree; OldNode, {%H-}NewNode: PVirtualNode;
      {%H-}OldColumn, {%H-}NewColumn: TColumnIndex;  var Allowed: Boolean);
    procedure VSTPackagesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; {%H-}Column: TColumnIndex);
    procedure VSTPackagesChecked(Sender: TBaseVirtualTree; {%H-}Node: PVirtualNode);
    procedure VSTPackagesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTPackageDataGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTPackageDataGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTPackageDataFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DoOnZippError(Sender: TObject; AZipFile: String; const AErrMsg: String);
    procedure DoOnZipCompleted(Sender: TObject);
    function LoadPackageData(const APath: String; AData: PData): Boolean;
    procedure ShowHideControls(const AType: Integer);
    procedure EnableDisableControls(const AEnable: Boolean);
    procedure SaveExtraInfo(const ANode: PVirtualNode);
    function TranslateCategories(const AStr: String): String;
    function CanCreate: Boolean;
    function CreateJSON(var AErrMsg: String): Boolean;
    function CreateJSONForUpdates(var AErrMsg: String): Boolean;
    procedure DoOnUploadProgress(Sender: TObject; AFileName: String);
    procedure DoOnUploadError(Sender: TObject; AErrMsg: String);
    procedure DoOnUploadCompleted(Sender: TObject);
  public
    procedure InitializeFrame(const ATyp: Integer = 0);
    procedure FinalizeFrame;
  end;

implementation
uses opkman_const, opkman_common, opkman_options, opkman_categoriesfrm,
     opkman_mainfrm, opkman_updates;
{$R *.lfm}

{ TCreateRepositoryPackagefr }

procedure TCreateRepositoryPackagefr.InitializeFrame(const ATyp: Integer = 0);
begin
  lbPackagedir.Caption := rsCreateRepositoryPackageFrm_lbPackageDir_Caption;
  pnMessage.Caption := rsCreateRepositoryPackageFrm_pnMessage_Caption;
  edCategories.Text := '';
  lbLazCompatibility.Caption := rsCreateRepositoryPackageFrm_lbLazCompatibility_Caption;
  lbFPCCompatibility.Caption := rsCreateRepositoryPackageFrm_lbFPCCompatibility_Caption;
  lbSupportedWidgetSet.Caption := rsCreateRepositoryPackageFrm_lbSupportedWidgetset_Caption;
  lbCategory.Caption := rsCreateRepositoryPackageFrm_lbCategory_Caption;
  lbDisplayName.Caption := rsCreateRepositoryPackageFrm_lbDisplayName_Caption;
  lbHomePageURL.Caption := rsCreateRepositoryPackageFrm_lbHomePageURL_Caption;
  lbDownloadURL.Caption := rsCreateRepositoryPackageFrm_lbDownloadURL_Caption;
  lbSVNURL.Caption := rsCreateRepositoryPackageFrm_lbSVNURL_Caption;
  bHelp.Caption := rsCreateRepositoryPackageFrm_bHelp_Caption;
  bHelp.Hint := rsCreateRepositoryPackageFrm_bHelp_Hint;
  bOptions.Caption := rsCreateRepositoryPackageFrm_bOptions_Caption;
  bOptions.Hint := rsCreateRepositoryPackageFrm_bOptions_Hint;
  bCreate.Caption := rsCreateRepositoryPackageFrm_bCreate_Caption;
  bCreate.Hint := rsCreateRepositoryPackageFrm_bCreate_Hint;
  bSubmit.Caption := rsCreateRepositoryPackageFrm_bSubmit_Caption;
  bSubmit.Hint := rsCreateRepositoryPackageFrm_bSubmit_Hint;
  bCancel.Caption := rsCreateRepositoryPackageFrm_bCancel_Caption;
  bCancel.Hint := rsCreateRepositoryPackageFrm_bCancel_Hint;
  bSubmit.Visible := ATyp = 0;
  cbJSONForUpdates.Visible := ATyp = 0;
  pnB.Height := bSubmit.Height + 1;
  pnB.Top := (pnButtons.Height - pnB.Height) div 2;
  bCreate.Visible := ATyp = 1;

  FVSTPackages := TVirtualStringTree.Create(nil);
  with FVSTPackages do
  begin
    Parent := pnPackages;
    Align := alClient;
    Anchors := [akLeft, akTop, akRight];
    Images := imTree;
    Color := clBtnFace;
    DefaultNodeHeight := 25;
    Indent := 15;
    TabOrder := 1;
    DefaultText := '';
    Header.AutoSizeIndex := 0;
    Header.Height := 25;
    Colors.BorderColor := clBlack;
    with Header.Columns.Add do begin
      Position := 0;
      Width := 250;
      Text := rsCreateRepositoryPackageFrm_pnCaption_Caption0;
    end;
    Header.Options := [hoAutoResize, hoColumnResize, hoRestrictDrag, hoVisible, hoAutoSpring];
    Header.SortColumn := 0;
    TabOrder := 2;
    TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
    TreeOptions.PaintOptions := [toHideFocusRect, toPopupMode, toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
    TreeOptions.SelectionOptions := [toRightClickSelect];
    TreeOptions.AutoOptions := [toAutoTristateTracking];
    OnGetText := @VSTPackagesGetText;
    OnGetImageIndex := @VSTPackagesGetImageIndex;
    OnChecked := @VSTPackagesChecked;
    OnFocusChanging := @VSTPackagesFocusChanging;
    OnFocusChanged := @VSTPackagesFocusChanged;
    OnFreeNode := @VSTPackagesFreeNode;
  end;
  FVSTPackages.NodeDataSize := SizeOf(TData);

  FVSTPackageData := TVirtualStringTree.Create(nil);
  with FVSTPackageData do
  begin
    Parent := pnData;
    Align := alTop;
    Height := 200;
    Anchors := [akLeft, akTop, akRight];
    Images := imTree;
    Color := clBtnFace;
    DefaultNodeHeight := 25;
    Indent := 15;
    TabOrder := 1;
    DefaultText := '';
    Header.AutoSizeIndex := 1;
    Header.Height := 25;
    Colors.BorderColor := clBlack;
    with Header.Columns.Add do begin
      Position := 0;
      Width := 150;
      Text := rsCreateRepositoryPackageFrm_pnCaption_Caption1;
    end;
    with Header.Columns.Add do begin
      Position := 1;
      Width := 250;
      Text := rsCreateRepositoryPackageFrm_pnCaption_Caption2;
    end;
    Header.Options := [hoAutoResize, hoColumnResize, hoRestrictDrag, hoVisible, hoAutoSpring];
    Header.SortColumn := 0;
    TabOrder := 2;
    TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
    TreeOptions.PaintOptions := [toHideFocusRect, toPopupMode, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages];
    TreeOptions.SelectionOptions := [toRightClickSelect, toFullRowSelect];
    TreeOptions.AutoOptions := [toAutoTristateTracking];
    OnGetText := @VSTPackageDataGetText;
    OnGetImageIndex := @VSTPackageDataGetImageIndex;
    OnFreeNode := @VSTPackageDataFreeNode;
  end;
  FVSTPackageData.NodeDataSize := SizeOf(TData);
  ShowHideControls(0);
  EnableDisableControls(True);
end;

procedure TCreateRepositoryPackagefr.FinalizeFrame;
begin
  if Uploader <> nil then
  begin
    pnMessage.Caption := rsCreateRepositoryPackageFrm_Message10;
    pnMessage.Invalidate;
    Application.ProcessMessages;
    Uploader.StopUpload;
    Uploader.WaitFor;
    Uploader := nil;
  end;
  FVSTPackages.Clear;
  FVSTPackages.Free;
  FVSTPackageData.Clear;
  FVSTPackageData.Free;
end;

function TCreateRepositoryPackagefr.LoadPackageData(const APath: String; AData: PData): Boolean;

  function PackageTypeIdentToType(const AStr: String): TPackageType;
  begin
    for Result := Low(TPackageType) to High(TPackageType) do
      if SysUtils.CompareText(AStr, PackageTypeIdents[Result]) = 0 then
        Exit;
    Result := ptRunTime;
  end;

  function VersionBound(const AVersion: Integer): Integer;
  begin
    if AVersion > 9999 then
      Result := 9999
    else if AVersion < 0 then
      Result := 0
    else
      Result := AVersion;
  end;

  function GetVersion(const AXMLConfig: TXMLConfig; const APath: String): String;
  var
    Major, Minor, Release, Build: Integer;
  begin
    Major := VersionBound(AXMLConfig.GetValue(APath + '/Major', 0));
    Minor := VersionBound(AXMLConfig.GetValue(APath + '/Minor', 0));
    Release := VersionBound(AXMLConfig.GetValue(APath + '/Release', 0));
    Build := VersionBound(AXMLConfig.GetValue(APath + '/Build', 0));
    Result := IntToStr(Major) + '.' + IntToStr(Minor) + '.' + IntToStr(Release) + '.' + IntToStr(Build);
  end;

var
  XMLConfig: TXMLConfig;
  BasePath, Path: String;
  I, DepCount: Integer;
  PackageName: String;
  MinVer, MaxVer: String;
begin
  Result := False;
  BasePath := 'Package/';
  XMLConfig := TXMLConfig.Create(APath);
  try
    AData^.FPackageType := PackageTypeIdentToType(XMLConfig.GetValue(BasePath + 'Type/Value', PackageTypeIdents[ptRunTime]));
    AData^.FDescription := String(XMLConfig.GetValue(BasePath + 'Description/Value', ''));
    AData^.FAuthor := String(XMLConfig.GetValue(BasePath + 'Author/Value', ''));
    AData^.FLicense := String(XMLConfig.GetValue(BasePath + 'License/Value', ''));
    AData^.FVersionAsString := GetVersion(XMLConfig, BasePath + 'Version');
    DepCount := XMLConfig.GetValue(BasePath + 'RequiredPkgs/Count', 0);
    for I := 0 to DepCount - 1 do
    begin
      MinVer := '';
      MaxVer := '';
      Path := BasePath + 'RequiredPkgs/Item' + IntToStr(I + 1) + '/';
      PackageName := XMLConfig.GetValue(Path + 'PackageName/Value', '');
      if XMLConfig.GetValue(Path + 'MinVersion/Valid', False) then
      begin
        MinVer := GetVersion(XMLConfig, Path + 'MinVersion');
        PackageName := PackageName + '(' + MinVer + ')';
      end;
      if XMLConfig.GetValue(Path + 'MaxVersion/Valid', False) then
      begin
        MaxVer := GetVersion(XMLConfig, Path + 'MaxVersion');
        if MinVer = '' then
          PackageName := PackageName + '(0.0.0.0)' + '(' + MaxVer + ')'
        else
          PackageName := PackageName + '(' + MaxVer + ')';
      end;
      if AData^.FDependenciesAsString = '' then
        AData^.FDependenciesAsString := PackageName
      else
        AData^.FDependenciesAsString := AData^.FDependenciesAsString + ', ' + PackageName;
    end;
    Result := True;
  finally
    XMLConfig.Free;
  end;
end;

procedure TCreateRepositoryPackagefr.ShowHideControls(const AType: Integer);
var
  Node: PVirtualNode;
begin
  case AType of
    0: begin
         pnPackages.Visible := False;
         pnData.Visible := False;
         pnMessage.Visible := False;
       end;
    1: begin
         pnPackages.Visible := False;
         pnData.Visible := False;
         pnMessage.Visible := True;
       end;
    2: begin
         pnPackages.Visible := True;
         pnData.Visible := True;
         pnMessage.Visible := False;
         Node := FVSTPackages.GetFirstSelected;
         if Node <> nil then
         case FVSTPackages.GetNodeLevel(Node) of
           0: begin
                FVSTPackageData.Visible := False;
                pnPackageData.Visible := False;
                pnCategory.Visible := True;
              end;
           1: begin
                FVSTPackageData.Visible := True;
                pnPackageData.Visible := True;
                pnCategory.Visible := False;
              end;
         end;
       end;
  end;
end;

procedure TCreateRepositoryPackagefr.EnableDisableControls(
  const AEnable: Boolean);
begin
  pnBrowse.Enabled := AEnable;
  cbJSONForUpdates.Enabled := AEnable;
  bHelp.Enabled := AEnable;
  bOptions.Enabled := AEnable;
  bCreate.Enabled := (AEnable) and (FVSTPackages.CheckedCount > 0);
  bSubmit.Enabled := (AEnable) and (FVSTPackages.CheckedCount > 0);
  bCancel.Enabled := AEnable;
end;

procedure TCreateRepositoryPackagefr.edPackageDirButtonClick(Sender: TObject);
begin
  edPackageDir.DialogTitle := rsCreateRepositoryPackageFrm_SDDTitleSrc;
  edPackageDir.Directory := Options.LastPackagedirSrc;
end;

procedure TCreateRepositoryPackagefr.edPackageDirAcceptDirectory(
  Sender: TObject; var Value: String);
var
  PackageList: TStringList;
  I: Integer;
  Node, RootNode: PVirtualNode;
  Data, RootData: PData;
  CanGo: Boolean;
begin
  CanGo := False;
  ShowHideControls(1);
  Application.ProcessMessages;
  try
    FPackageDir := Value;
    Options.LastPackageDirSrc := FPackageDir;
    Options.Changed := True;
    PackageList := TStringList.Create;
    try
      FindPackages(FPackageDir, PackageList);
      if PackageList.Count > 0 then
      begin
        FVSTPackages.Clear;
        FVSTPackages.NodeDataSize := SizeOf(TData);
        FVSTPackageData.Clear;
        FVSTPackageData.NodeDataSize := SizeOf(TData);
        RootNode := FVSTPackages.AddChild(nil);
        RootNode^.CheckType := ctTriStateCheckBox;
        RootData := FVSTPackages.GetNodeData(RootNode);
        RootData^.FName := TPackageData(PackageList.Objects[0]).FPackageBaseDir;
        RootData^.FCategory := '';
        RootData^.FDisplayName := '';
        RootData^.FHomePageURL := '';
        RootData^.FDownloadURL := '';
        RootData^.FSVNURL := '';
        FPackageName := RootData^.FName;
        for I := 0 to PackageList.Count - 1 do
        begin
          Node := FVSTPackages.AddChild(RootNode);
          Node^.CheckType := ctTriStateCheckBox;
          Data := FVSTPackages.GetNodeData(Node);
          Data^.FName := TPackageData(PackageList.Objects[I]).FName;
          Data^.FPackageBaseDir := TPackageData(PackageList.Objects[I]).FPackageBaseDir;
          RootData^.FPackageBaseDir := Data^.FPackageBaseDir;
          Data^.FPackageRelativePath := TPackageData(PackageList.Objects[I]).FPackageRelativePath;
          Data^.FFullPath := TPackageData(PackageList.Objects[I]).FFullPath;
          if not LoadPackageData(Data^.FFullPath, Data) then
            MessageDlgEx(rsCreateRepositoryPackageFrm_Error0, mtError, [mbOk], TForm(Self.Parent));
          Data^.FLazCompatibility := '1.6, Trunk';
          Data^.FFPCCompatibility := '2.6.4, 3.0.0';
          Data^.FSupportedWidgetSet := 'win32/64, gtk2, carbon';
        end;
        FVSTPackages.FullExpand;
        RootNode := FVSTPackages.GetFirst;
        if RootNode <> nil then
        begin
          FVSTPackages.FocusedNode := RootNode;
          FVSTPackages.Selected[RootNode] := True;
          CanGo := True;
        end;
      end
      else
        MessageDlgEx(rsCreateRepositoryPackageFrm_NoPackage, mtInformation, [mbOk], TForm(Self.Parent));
    finally
      for I := PackageList.Count - 1 downto 0 do
        PackageList.Objects[I].Free;
      PackageList.Free;
    end;
  finally
    if CanGo then
      ShowHideControls(2)
    else
      ShowHideControls(0)
  end;
end;

procedure TCreateRepositoryPackagefr.pnBrowseResize(Sender: TObject);
begin
  edPackageDir.Top := (pnBrowse.Height - edPackageDir.Height) div 2;
  lbPackageDir.Left := 100;
  lbPackageDir.Top := edPackageDir.Top + (edPackageDir.Height - lbPackageDir.Height) div 2;
  edPackageDir.Left := lbPackagedir.Left + lbPackagedir.Width + 5;
  edPackageDir.Width := pnBrowse.Width - edPackageDir.Left - 120;
end;

procedure TCreateRepositoryPackagefr.spCategoriesClick(Sender: TObject);
begin
  CategoriesFrm := TCategoriesFrm.Create(Self.Parent);
  try
    CategoriesFrm.SetupControls;
    CategoriesFrm.CategoriesCSV := edCategories.Text;
    CategoriesFrm.PopulateTree;
    if CategoriesFrm.ShowModal = mrOK then
      edCategories.Text := CategoriesFrm.CategoriesCSV;
  finally
    CategoriesFrm.Free;
  end;
end;

function TCreateRepositoryPackagefr.CanCreate: Boolean;

  procedure SelectAndFocusNode(const ANode: PVirtualNode);
  begin
    FVSTPackages.Selected[ANode ] := True;
    FVSTPackages.FocusedNode := ANode;
  end;

var
  Node: PVirtualNode;
  Data: PData;
begin
  Result := False;
  Node := FVSTPackages.GetFirstSelected;
  if Node <> nil then
    SaveExtraInfo(Node);
  Node := FVSTPackages.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVSTPackages.GetNodeData(Node);
    if ((FVSTPackages.CheckState[Node] = csCheckedNormal) or (FVSTPackages.CheckState[Node] = csMixedNormal)) and (FVSTPackages.GetNodeLevel(Node) = 0) then
    begin
      ShowHideControls(2);
      if Data^.FCategory = '' then
      begin
        SelectAndFocusNode(Node);
        MessageDlgEx(rsCreateRepositoryPackageFrm_Message0 + ' "' + Data^.FName + '".', mtInformation, [mbOk], TForm(Self.Parent));
        edCategories.SetFocus;
        Exit;
      end;
    end;
    if (FVSTPackages.CheckState[Node] = csCheckedNormal) and (FVSTPackages.GetNodeLevel(Node) = 1) then
    begin
      ShowHideControls(2);
      if Trim(Data^.FLazCompatibility) = '' then
      begin
        SelectAndFocusNode(Node);
        MessageDlgEx(rsCreateRepositoryPackageFrm_Message1 + ' "' + Data^.FName + '".', mtInformation, [mbOk], TForm(Self.Parent));
        edLazCompatibility.SetFocus;
        Exit;
      end;
      if Trim(Data^.FFPCCompatibility) = '' then
      begin
        SelectAndFocusNode(Node);
        MessageDlgEx(rsCreateRepositoryPackageFrm_Message2 + ' "' + Data^.FName + '".', mtInformation, [mbOk], TForm(Self.Parent));
        edFPCCompatibility.SetFocus;
        Exit;
      end;
      if Trim(Data^.FSupportedWidgetSet) = '' then
      begin
        SelectAndFocusNode(Node);
        MessageDlgEx(rsCreateRepositoryPackageFrm_Message3 + ' "' + Data^.FName + '".', mtInformation, [mbOk], TForm(Self.Parent));
        edSupportedWidgetset.SetFocus;
        Exit;
      end;
    end;
    Node := FVSTPackages.GetNext(Node);
  end;
  Result := True;
end;

procedure TCreateRepositoryPackagefr.bCreateClick(Sender: TObject);
var
  RootNode: PVirtualNode;
  RootData: PData;
begin
  if not CanCreate then
    Exit;
  SDD.Title := rsCreateRepositoryPackageFrm_SDDTitleDst;
  SDD.InitialDir := Options.LastPackagedirDst;
  EnableDisableControls(False);
  if SDD.Execute then
  begin
    FPackageOperation := poCreate;
    Screen.Cursor := crHourGlass;
    ShowHideControls(1);
    FPackageZipper := TPackageZipper.Create;
    FPackageZipper.OnZipError := @DoOnZippError;
    FPackageZipper.OnZipCompleted := @DoOnZipCompleted;
    FDestDir := AppendPathDelim(SDD.FileName);
    Options.LastPackagedirDst := SDD.FileName;
    Options.Changed := True;
    RootNode := FVSTPackages.GetFirst;
    RootData := FVSTPackages.GetNodeData(RootNode);
    if RootData^.FDisplayName <> '' then
      FPackageName := StringReplace(RootData^.FDisplayName, ' ', '', [rfReplaceAll])
    else
      FPackageName := StringReplace(RootData^.FName, ' ', '', [rfReplaceAll]);
    FPackageFile := FDestDir + FPackageName + '.zip';
    pnMessage.Caption := rsCreateRepositoryPackageFrm_Message4;
    fPackageZipper.StartZip(FPackageDir, FPackageFile);
  end
  else
    EnableDisableControls(True);
end;

procedure TCreateRepositoryPackagefr.bSubmitClick(Sender: TObject);
var
  RootNode: PVirtualNode;
  RootData: PData;
begin
  if not CanCreate then
    Exit;
  FPackageOperation := poSubmit;
  EnableDisableControls(False);
  ShowHideControls(1);
  Screen.Cursor := crHourGlass;
  fPackageZipper := TPackageZipper.Create;
  fPackageZipper.OnZipError := @DoOnZippError;
  fPackageZipper.OnZipCompleted := @DoOnZipCompleted;
  FDestDir := Options.LocalRepositoryUpdate;
  RootNode := FVSTPackages.GetFirst;
  RootData := FVSTPackages.GetNodeData(RootNode);
  if RootData^.FDisplayName <> '' then
    FPackageName := StringReplace(RootData^.FDisplayName, ' ', '', [rfReplaceAll])
  else
    FPackageName := StringReplace(RootData^.FName, ' ', '', [rfReplaceAll]);
  FPackageFile := FDestDir + FPackageName + '.zip';
  pnMessage.Caption := rsCreateRepositoryPackageFrm_Message4;
  fPackageZipper.StartZip(FPackageDir, FPackageFile);
end;

procedure TCreateRepositoryPackagefr.bHelpClick(Sender: TObject);
begin
  OpenURL(cHelpPage_CreateRepositoryPackage);
end;

procedure TCreateRepositoryPackagefr.bOptionsClick(Sender: TObject);
begin
  MainFrm.ShowOptions(3);
end;

procedure TCreateRepositoryPackagefr.bCancelClick(Sender: TObject);
begin
  if Assigned(FPackageZipper) then
    FPackageZipper.Terminate;
  TForm(Self.Parent).ModalResult := mrCancel;
  TForm(Self.Parent).Close;
end;

procedure TCreateRepositoryPackagefr.VSTPackagesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PData;
begin
  Data := FVSTPackages.GetNodeData(Node);
  if Column = 0 then
    CellText := Data^.FName;
end;

procedure TCreateRepositoryPackagefr.VSTPackagesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if Column = 0 then
    ImageIndex := FVSTPackages.GetNodeLevel(Node);
end;

procedure TCreateRepositoryPackagefr.SaveExtraInfo(const ANode: PVirtualNode);
var
  Data: PData;
begin
  Data := FVSTPackages.GetNodeData(ANode);
  case FVSTPackages.GetNodeLevel(ANode) of
    0: begin
         Data^.FCategory := edCategories.Text;
         Data^.FDisplayName := edDisplayName.Text;
         Data^.FHomePageURL := edHomePageURL.Text;
         Data^.FDownloadURL := edDownloadURL.Text;
         Data^.FSVNURL := edSVNURL.Text;
       end;
    1: begin
         Data^.FLazCompatibility := edLazCompatibility.Text;
         Data^.FFPCCompatibility := edFPCCompatibility.Text;
         Data^.FSupportedWidgetSet := edSupportedWidgetset.Text;
       end;
  end;
end;

procedure TCreateRepositoryPackagefr.VSTPackagesFocusChanging(
  Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
  NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  if (OldNode = nil) or (NewNode = nil) or (OldNode = NewNode) then
    Exit;
  SaveExtraInfo(OldNode);
  edCategories.Text := '';
  edLazCompatibility.Text := '';
  edFPCCompatibility.Text := '';
  edSupportedWidgetset.Text := '';
  edDisplayName.Text := '';
  edHomePageURL.Text := '';
  edDownloadURL.Text := '';
  edSVNURL.Text := '';
  Allowed := True;
end;

procedure TCreateRepositoryPackagefr.VSTPackagesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PData;
  PDNode: PVirtualNode;
  PDData: PData;
  Level: Integer;
begin
  if Node = nil then
    Exit;
  Level := FVSTPackages.GetNodeLevel(Node);
  Data := FVSTPackages.GetNodeData(Node);
  if Level = 0 then
  begin
    edCategories.Text := Data^.FCategory;
    edDisplayName.Text := Data^.FDisplayName;
    edHomePageURL.Text := Data^.FHomePageURL;
    edDownloadURL.Text := Data^.FDownloadURL;
    edSVNURL.Text := Data^.FSVNURL;
  end
  else if Level = 1 then
  begin
    FVSTPackageData.Clear;

    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FVersionAsString := Data^.FVersionAsString;
    PDData^.FDataType := 2;

    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FDescription := Trim(Data^.FDescription);
    PDData^.FDataType := 3;

    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FAuthor := Data^.FAuthor;
    PDData^.FDataType := 4;

    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FPackageType := Data^.FPackageType;
    PDData^.FDataType := 5;

    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FDependenciesAsString := Data^.FDependenciesAsString;
    PDData^.FDataType := 6;

    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FLicense := Trim(Data^.FLicense);
    PDData^.FDataType := 7;

    edLazCompatibility.Text := Data^.FLazCompatibility;
    edFPCCompatibility.Text := Data^.FFPCCompatibility;
    edSupportedWidgetset.Text := Data^.FSupportedWidgetSet;
  end;
  ShowHideControls(2);
end;

procedure TCreateRepositoryPackagefr.VSTPackagesChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  EnableDisableControls(True);
end;

procedure TCreateRepositoryPackagefr.VSTPackagesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVSTPackages.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TCreateRepositoryPackagefr.VSTPackageDataGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  Data: PData;
begin
  Data := FVSTPackageData.GetNodeData(Node);
  case Column of
    0: case Data^.FDataType of
         2: CellText := rsMainFrm_VSTText_Version;
         3: CellText := rsMainFrm_VSTText_Description;
         4: CellText := rsMainFrm_VSTText_Author;
         5: CellText := rsMainFrm_VSTText_Packagetype;
         6: CellText := rsMainFrm_VSTText_Dependecies;
         7: CellText := rsMainFrm_VSTText_License;
       end;
    1: case Data^.FDataType of
         2: CellText := Data^.FVersionAsString;
         3: CellText := Data^.FDescription;
         4: CellText := Data^.FAuthor;
         5: case Data^.FPackageType of
              ptRunAndDesignTime: CellText := rsMainFrm_VSTText_PackageType0;
              ptDesignTime:       CellText := rsMainFrm_VSTText_PackageType1;
              ptRunTime:          CellText := rsMainFrm_VSTText_PackageType2;
              ptRunTimeOnly:      CellText := rsMainFrm_VSTText_PackageType3;
            end;
         6: CellText := Data^.FDependenciesAsString;
         7: CellText := Data^.FLicense;
       end;
  end;
end;

procedure TCreateRepositoryPackagefr.VSTPackageDataGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  if Column = 0 then
  begin
    Data := FVSTPackageData.GetNodeData(Node);
    ImageIndex := Data^.FDataType;
  end;
end;

procedure TCreateRepositoryPackagefr.VSTPackageDataFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVSTPackageData.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TCreateRepositoryPackagefr.DoOnZippError(Sender: TObject;
  AZipFile: String; const AErrMsg: String);
begin
   Screen.Cursor := crDefault;
   MessageDlgEx(rsCreateRepositoryPackageFrm_Error1 + ' "' + AZipFile + '". ' + rsProgressFrm_Error1 + sLineBreak +
                AErrMsg, mtError, [mbOk], TForm(Self.Parent));
   ShowHideControls(2);
   EnableDisableControls(True);
end;

function TCreateRepositoryPackagefr.TranslateCategories(const AStr: String): String;
var
  SL: TStringList;
  I, J: Integer;
  Str: String;
begin
  if Categories[0] = CategoriesEng[0] then
  begin
    Result := AStr;
    Exit;
  end;
  Result := '';
  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.StrictDelimiter := True;
    SL.DelimitedText := AStr;
    for I := 0 to SL.Count - 1 do
    begin
      Str := Trim(SL.Strings[I]);
      for J := 0 to MaxCategories - 1 do
      begin
        if Str = Categories[J] then
        begin
          if Result = '' then
            Result := CategoriesEng[J]
          else
            Result := Result + ', ' + CategoriesEng[J];
          Break;
        end;
      end;
    end;
  finally
    SL.Free;
  end;
  if Result = '' then
    Result := AStr;
end;

function TCreateRepositoryPackagefr.CreateJSONForUpdates(var AErrMsg: String): Boolean;
var
  RootNode, Node: PVirtualNode;
  RootData, Data: PData;
  JSON: TJSONStringType;
  Ms: TMemoryStream;
  UpdatePackage: TUpdatePackage;
  UpdatePackageFiles: TUpdatePackageFiles;
begin
  Result := False;
  pnMessage.Caption := rsCreateRepositoryPackageFrm_Message6;
  pnMessage.Invalidate;
  Application.ProcessMessages;
  Sleep(2000);
  UpdatePackage := TUpdatePackage.Create;
  try
    RootNode := FVSTPackages.GetFirst;
    if RootNode <> nil then
    begin
      RootData := FVSTPackages.GetNodeData(RootNode);
      UpdatePackage.UpdatePackageData.Name := RootData^.FName;
      UpdatePackage.UpdatePackageData.DownloadZipURL := RootData^.FDownloadURL;
      Node := FVSTPackages.GetFirstChild(RootNode);
      while Assigned(Node) do
      begin
        if FVSTPackages.CheckState[Node] = csCheckedNormal then
        begin
          Data := FVSTPackages.GetNodeData(Node);
          UpdatePackageFiles := TUpdatePackageFiles(UpdatePackage.UpdatePackageFiles.Add);
          UpdatePackageFiles.Name := Data^.FName;
          UpdatePackageFiles.Version := Data^.FVersionAsString;
          UpdatePackageFiles.ForceNotify := False;
          UpdatePackageFiles.InternalVersion := 1;
        end;
        Node := FVSTPackages.GetNextSibling(Node);
      end;
    end;
    JSON := '';
    if UpdatePackage.SaveToJSON(JSON) then
    begin
      JSON := StringReplace(JSON, '\/', '/', [rfReplaceAll]);
      Ms := TMemoryStream.Create;
      try
        Ms.Write(Pointer(JSON)^, Length(JSON));
        Ms.Position := 0;
        Ms.SaveToFile(FDestDir + 'update_' + FPackageName + '.json');
      finally
        MS.Free;
      end;
      Result := True;
    end
    else
      AErrMsg := rsCreateJSONForUpdatesFrm_Error1 + sLineBreak + '"' + StringReplace(UpdatePackage.LastError, '"', '', [rfReplaceAll]) + '"';
  finally
    UpdatePackage.Free;
  end;
end;

function TCreateRepositoryPackagefr.CreateJSON(var AErrMsg: String): Boolean;
var
  SerializablePackages: TSerializablePackages;
  Package: TPackage;
  PackageFile: TPackageFile;
  RootNode, Node: PVirtualNode;
  RootData, Data: PData;
  JSON: TJSONStringType;
  MS: TMemoryStream;
begin
  Result := False;
  pnMessage.Caption := rsCreateRepositoryPackageFrm_Message5;
  pnMessage.Invalidate;
  Application.ProcessMessages;
  Sleep(2000);
  SerializablePackages := TSerializablePackages.Create;
  try
    RootNode := FVSTPackages.GetFirst;
    if RootNode <> nil then
    begin
      RootData := FVSTPackages.GetNodeData(RootNode);
      Package := SerializablePackages.AddPackage(RootData^.FName);
      Package.Category := TranslateCategories(RootData^.FCategory);
      Package.RepositoryFileName := ExtractFileName(FPackageFile);
      Package.RepositoryFileSize := FileUtil.FileSize(FPackageFile);
      Package.RepositoryFileHash := MD5Print(MD5File(FPackageFile));
      Package.RepositoryDate := Trunc(now);
      Package.PackageBaseDir := RootData^.FPackageBaseDir + '\/';
      Package.DisplayName := RootData^.FDisplayName;
      Package.HomePageURL := RootData^.FHomePageURL;
      Package.DownloadURL := RootData^.FDownloadURL;
      Package.SVNURL := RootData^.FSVNURL;
      Node := FVSTPackages.GetFirstChild(RootNode);
      while Assigned(Node) do
      begin
        if FVSTPackages.CheckState[Node] = csCheckedNormal then
        begin
          Data := FVSTPackages.GetNodeData(Node);
          PackageFile := TPackageFile(Package.PackageFiles.Add);
          PackageFile.Name := Data^.FName;
          PackageFile.PackageRelativePath := Data^.FPackageRelativePath;
          if Trim(PackageFile.PackageRelativePath) <> '' then
          begin
            PackageFile.PackageRelativePath := AppendPathDelim(PackageFile.PackageRelativePath);
            PackageFile.PackageRelativePath := StringReplace(PackageFile.PackageRelativePath, PathDelim, '\/', [rfReplaceAll]);
          end;
          PackageFile.Version := TPackageVersion.Create;
          PackageFile.Version.AsString := Data^.FVersionAsString;
          PackageFile.Description := Data^.FDescription;
          PackageFile.Author := Data^.FAuthor;
          PackageFile.LazCompatibility := Data^.FLazCompatibility;
          PackageFile.FPCCompatibility := Data^.FFPCCompatibility;
          PackageFile.SupportedWidgetSet := Data^.FSupportedWidgetSet;
          PackageFile.PackageType := Data^.FPackageType;
          PackageFile.Dependencies := TPackageDependencies.Create(TPackageDependency);
          PackageFile.Dependencies.SetDependenciesAsString(Data^.FDependenciesAsString);
          PackageFile.License := Data^.FLicense;
        end;
        Node := FVSTPackages.GetNextSibling(Node);
      end;
    end;
    if SerializablePackages.Count > 0 then
    begin
      JSON := '';
      if SerializablePackages.PackagesToJSON(JSON) then
      begin
        MS := TMemoryStream.Create;
        try
          MS.Write(Pointer(JSON)^, Length(JSON));
          MS.Position := 0;
          MS.SaveToFile(FDestDir + FPackageName + '.json');
          Result := True;
        finally
          MS.Free;
        end;
      end
      else
        AErrMsg := rsCreateRepositoryPackageFrm_Error2 + sLineBreak + '"' + StringReplace(SerializablePackages.LastError, '"', '', [rfReplaceAll]) + '"'
    end;
  finally
    SerializablePackages.Free;
  end;
end;

procedure TCreateRepositoryPackagefr.DoOnZipCompleted(Sender: TObject);
var
  ErrMsg: String;
begin
  ErrMsg := '';
  if not CreateJSON(ErrMsg) then
  begin
    MessageDlgEx(ErrMsg, mtError, [mbOk], TForm(Self.Parent));
    Exit;
  end;

  if cbJSONForUpdates.Checked then
  begin
    ErrMsg := '';
    if not CreateJSONForUpdates(ErrMsg) then
    begin
      MessageDlgEx(ErrMsg, mtError, [mbOk], TForm(Self.Parent));
      Exit;
    end;
  end;

  case FPackageOperation of
    poCreate:
      begin
        Screen.Cursor := crDefault;
        ShowHideControls(2);
        EnableDisableControls(True);
        MessageDlgEx(rsCreateRepositoryPackageFrm_Message7, mtInformation, [mbOk], TForm(Self.Parent));
        TForm(Self.Parent).ModalResult := mrOk;
        TForm(Self.Parent).Close;
      end;
    poSubmit:
      begin
        Uploader := TUploader.Create;
        Uploader.OnUploadProgress := @DoOnUploadProgress;
        Uploader.OnUploadError := @DoOnUploadError;
        Uploader.OnUploadCompleted := @DoOnUploadCompleted;
        if cbJSONForUpdates.Checked then
          Uploader.StartUpload(cSubmitURL_Zip, cSubmitURL_JSON, FPackageFile, FDestDir + FPackageName + '.json', FDestDir + 'update_' + FPackageName + '.json')
        else
          Uploader.StartUpload(cSubmitURL_Zip, cSubmitURL_JSON, FPackageFile, FDestDir + FPackageName + '.json', '')
      end;
  end;
end;

procedure TCreateRepositoryPackagefr.DoOnUploadProgress(Sender: TObject;
  AFileName: String);
begin
  pnMessage.Caption := Format(rsCreateRepositoryPackageFrm_Message8, [AFileName]);
  pnMessage.Invalidate;
  Application.ProcessMessages;
end;

procedure TCreateRepositoryPackagefr.DoOnUploadError(Sender: TObject;
  AErrMsg: String);
begin
  Screen.Cursor := crDefault;
  ShowHideControls(2);
  EnableDisableControls(True);
  MessageDlgEx(AErrMsg, mtError, [mbOk], TForm(Self.Parent));
end;

procedure TCreateRepositoryPackagefr.DoOnUploadCompleted(Sender: TObject);
begin
  Screen.Cursor := crDefault;
  ShowHideControls(2);
  EnableDisableControls(True);
  Uploader := nil;
  if FileExistsUTF8(FPackageFile) then
    DeleteFileUTF8(FPackageFile);
  if FileExistsUTF8(FDestDir + FPackageName + '.json') then
    DeleteFileUTF8(FDestDir + FPackageName + '.json');
  if FileExistsUTF8(FDestDir + 'update_' + FPackageName + '.json') then
    DeleteFileUTF8(FDestDir + 'update_' + FPackageName + '.json');
  MessageDlgEx(rsCreateRepositoryPackageFrm_Message9, mtInformation, [mbOk], TForm(Self.Parent));
  TForm(Self.Parent).ModalResult := mrOk;
  TForm(Self.Parent).Close;
end;



end.

