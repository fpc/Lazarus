unit opkman_createrepositorypackagefr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Dialogs,
  LazFileUtils, Graphics, Menus, Buttons, Laz2_XMLCfg, opkman_VirtualTrees,
  md5, fpjson, LCLIntf, opkman_serializablepackages, opkman_zipper;

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
    FCategory: Integer;
    FLazCompatibility: String;
    FFPCCompatibility: String;
    FSupportedWidgetSet: String;
    FHomePageURL: String;
    FDownloadURL: String;
    FSVNURL: String;
  end;


  { TCreateRepositoryPackagefr }

  TCreateRepositoryPackagefr = class(TFrame)
    bCancel: TButton;
    bCreate: TButton;
    Bevel1: TBevel;
    cbCategory: TComboBox;
    cbOpen: TCheckBox;
    edDownloadURL: TEdit;
    edDisplayName: TEdit;
    edSVNURL: TEdit;
    edFPCCompatibility: TEdit;
    edHomePageURL: TEdit;
    edLazCompatibility: TEdit;
    edPackageDir: TEdit;
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
    pnButtons: TPanel;
    pnPackageData: TPanel;
    pnBrowse: TPanel;
    pnCategory: TPanel;
    pnMessage: TPanel;
    pnPackageDir: TPanel;
    pnPackages: TPanel;
    pnData: TPanel;
    SDD: TSelectDirectoryDialog;
    bPackageDir: TSpeedButton;
    spMain: TSplitter;
    spMain1: TSplitter;
    procedure bCancelClick(Sender: TObject);
    procedure bCreateClick(Sender: TObject);
    procedure bPackageDirClick(Sender: TObject);
    procedure pnBrowseResize(Sender: TObject);
  private
    FVSTPackages: TVirtualStringTree;
    FVSTPackageData: TVirtualStringTree;
    FCreatePressed: Boolean;
    FPackageZipper: TPackageZipper;
    FPackageDir: String;
    FPackageName: String;
    FPackageFile: String;
    FDestDir: String;
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
    procedure SaveExtraInfo(const ANode: PVirtualNode);
  public
    procedure InitializeFrame;
    procedure FinalizeFrame;
  end;

implementation
uses opkman_const, opkman_common, opkman_options;
{$R *.lfm}

{ TCreateRepositoryPackagefr }

procedure TCreateRepositoryPackagefr.InitializeFrame;
begin
  lbPackagedir.Caption := rsCreateRepositoryPackagelbPackageDir;
  pnMessage.Caption := rsCreateRepositoryPackagepnMessage;
  cbOpen.Caption := rsCreateRepositoryPackagecbOpenCaption;
  cbCategory.Text := '';
  cbCategory.Items.Add(rsMainFrmVSTTextPackageCategory0);
  cbCategory.Items.Add(rsMainFrmVSTTextPackageCategory1);
  cbCategory.Items.Add(rsMainFrmVSTTextPackageCategory2);
  cbCategory.Items.Add(rsMainFrmVSTTextPackageCategory3);
  cbCategory.Items.Add(rsMainFrmVSTTextPackageCategory4);
  cbCategory.Items.Add(rsMainFrmVSTTextPackageCategory5);
  cbCategory.Items.Add(rsMainFrmVSTTextPackageCategory6);
  cbCategory.Items.Add(rsMainFrmVSTTextPackageCategory7);
  cbCategory.Items.Add(rsMainFrmVSTTextPackageCategory8);
  lbLazCompatibility.Caption := rsCreateRepositoryPackagelbLazCompatibility;
  lbFPCCompatibility.Caption := rsCreateRepositoryPackagelbFPCCompatibility;
  lbSupportedWidgetSet.Caption := rsCreateRepositoryPackagelbSupportedWidgetset;
  lbCategory.Caption := rsCreateRepositoryPackagelbCategory;
  lbDisplayName.Caption := rsCreateRepositoryPackagelbDisplayName;
  lbHomePageURL.Caption := rsCreateRepositoryPackagelbHomePageURL;
  lbDownloadURL.Caption := rsCreateRepositoryPackagelbDownloadURL;
  lbSVNURL.Caption := rsCreateRepositoryPackagelbSVNURL;

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
      Text := rsCreateRepositoryPackagepnCaption0;
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
      Text := rsCreateRepositoryPackagepnCaption1;
    end;
    with Header.Columns.Add do begin
      Position := 1;
      Width := 250;
      Text := rsCreateRepositoryPackagepnCaption2;
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
end;

procedure TCreateRepositoryPackagefr.FinalizeFrame;
begin
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
    AData^.FDescription := XMLConfig.GetValue(BasePath + 'Description/Value', '');
    AData^.FAuthor := XMLConfig.GetValue(BasePath + 'Author/Value', '');
    AData^.FLicense := XMLConfig.GetValue(BasePath + 'License/Value', '');
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

procedure TCreateRepositoryPackagefr.bPackageDirClick(Sender: TObject);
var
  PackageList: TStringList;
  I: Integer;
  Node, RootNode: PVirtualNode;
  Data, RootData: PData;
  CanGo: Boolean;
begin
  CanGo := False;
  SDD.Title := rsCreateRepositorySDDTitleSrc;
  SDD.InitialDir := PackageOptions.LastPackagedirSrc;
  if SDD.Execute then
  begin
    ShowHideControls(1);
    Application.ProcessMessages;
    try
      FPackageDir := AppendPathDelim(SDD.FileName);
      PackageOptions.LastPackageDirSrc := FPackageDir;
      PackageOptions.Changed := True;
      edPackageDir.Text := FPackageDir;
      PackageList := TStringList.Create;
      try
        FindPackages(edPackageDir.Text, PackageList);
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
          RootData^.FCategory := -1;
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
              MessageDlgEx(rsCreateRepositoryPackageError0, mtError, [mbOk], TForm(Self.Parent));
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
          MessageDlgEx(rsCreateRepositoryPackageNoPackage, mtInformation, [mbOk], TForm(Self.Parent));
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
  end
end;


procedure TCreateRepositoryPackagefr.pnBrowseResize(Sender: TObject);
begin
  pnPackageDir.Top := (pnBrowse.Height - pnPackageDir.Height) div 2;
  lbPackageDir.Left := 100;
  lbPackageDir.Top := pnPackageDir.Top + (pnPackageDir.Height - lbPackageDir.Height) div 2;
  pnPackageDir.Left := lbPackagedir.Left + lbPackagedir.Width + 5;
  pnPackageDir.Width := pnBrowse.Width - pnPackageDir.Left - 120;
end;

procedure TCreateRepositoryPackagefr.bCreateClick(Sender: TObject);

  procedure SelectAndFocusNode(const ANode: PVirtualNode);
  begin
    FVSTPackages.Selected[ANode ] := True;
    FVSTPackages.FocusedNode := ANode;
  end;

var
  Node: PVirtualNode;
  Data: PData;
begin
  FCreatePressed := True;
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
      if Data^.FCategory = -1 then
      begin
        SelectAndFocusNode(Node);
        MessageDlgEx(rsCreateRepositoryPackageMessage0 + ' "' + Data^.FName + '".', mtInformation, [mbOk], TForm(Self.Parent));
        cbCategory.SetFocus;
        Exit;
      end;
    end;
    if (FVSTPackages.CheckState[Node] = csCheckedNormal) and (FVSTPackages.GetNodeLevel(Node) = 1) then
    begin
      ShowHideControls(2);
      if Trim(Data^.FLazCompatibility) = '' then
      begin
        SelectAndFocusNode(Node);
        MessageDlgEx(rsCreateRepositoryPackageMessage1 + ' "' + Data^.FName + '".', mtInformation, [mbOk], TForm(Self.Parent));
        edLazCompatibility.SetFocus;
        Exit;
      end;
      if Trim(Data^.FFPCCompatibility) = '' then
      begin
        SelectAndFocusNode(Node);
        MessageDlgEx(rsCreateRepositoryPackageMessage2 + ' "' + Data^.FName + '".', mtInformation, [mbOk], TForm(Self.Parent));
        edFPCCompatibility.SetFocus;
        Exit;
      end;
      if Trim(Data^.FSupportedWidgetSet) = '' then
      begin
        SelectAndFocusNode(Node);
        MessageDlgEx(rsCreateRepositoryPackageMessage3 + ' "' + Data^.FName + '".', mtInformation, [mbOk], TForm(Self.Parent));
        edSupportedWidgetset.SetFocus;
        Exit;
      end;
    end;
    Node := FVSTPackages.GetNext(Node);
  end;

  SDD.Title := rsCreateRepositorySDDTitleDst;
  SDD.InitialDir := PackageOptions.LastPackagedirDst;
  if SDD.Execute then
  begin
    fPackageZipper := TPackageZipper.Create;
    fPackageZipper.OnZipError := @DoOnZippError;
    fPackageZipper.OnZipCompleted := @DoOnZipCompleted;
    FDestDir := AppendPathDelim(SDD.FileName);
    PackageOptions.LastPackagedirDst := FDestDir;
    PackageOptions.Changed := True;
    FPackageName := StringReplace(FPackageName, ' ', '', [rfReplaceAll]);
    FPackageFile := FDestDir + FPackageName + '.zip';
    pnMessage.Caption := rsCreateRepositoryPackageMessage4;
    ShowHideControls(1);
    bCreate.Enabled := False;
    pnBrowse.Enabled := False;
    fPackageZipper.StartZip(FPackageDir, FPackageFile);
  end;
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
         Data^.FCategory := cbCategory.ItemIndex;
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
  cbCategory.ItemIndex := -1;
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
    //category
    if Data^.FCategory <> -1 then
      cbCategory.ItemIndex := Data^.FCategory;
    //displayname
    edDisplayName.Text := Data^.FDisplayName;
    //homepageURL
    edHomePageURL.Text := Data^.FHomePageURL;
    //downloadURL
    edDownloadURL.Text := Data^.FDownloadURL;
    //SVNURL
    edSVNURL.Text := Data^.FSVNURL;
  end
  else if Level = 1 then
  begin
    FVSTPackageData.Clear;
    //dependencies
    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FVersionAsString := Data^.FVersionAsString;
    PDData^.FDataType := 2;
    //description
    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FDescription := Trim(Data^.FDescription);
    PDData^.FDataType := 3;
    //author
    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FAuthor := Data^.FAuthor;
    PDData^.FDataType := 4;
    //packagetype
    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FPackageType := Data^.FPackageType;
    PDData^.FDataType := 5;
    //dependencies
    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FDependenciesAsString := Data^.FDependenciesAsString;
    PDData^.FDataType := 6;
    //license
    PDNode := FVSTPackageData.AddChild(nil);
    PDData := FVSTPackageData.GetNodeData(PDNode);
    PDData^.FLicense := Trim(Data^.FLicense);
    PDData^.FDataType := 7;
    //lazcompatibility
    edLazCompatibility.Text := Data^.FLazCompatibility;
    //fpccompatibility
    edFPCCompatibility.Text := Data^.FFPCCompatibility;
    //supportedwidgetset
    edSupportedWidgetset.Text := Data^.FSupportedWidgetSet;
  end;
  ShowHideControls(2);
end;

procedure TCreateRepositoryPackagefr.VSTPackagesChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  bCreate.Enabled := FVSTPackages.CheckedCount > 0;
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
         2: CellText := rsMainFrmVSTTextVersion;
         3: CellText := rsMainFrmVSTTextDescription;
         4: CellText := rsMainFrmVSTTextAuthor;
         5: CellText := rsMainFrmVSTTextPackagetype;
         6: CellText := rsMainFrmVSTTextDependecies;
         7: CellText := rsMainFrmVSTTextLicense;
       end;
    1: case Data^.FDataType of
         2: CellText := Data^.FVersionAsString;
         3: CellText := Data^.FDescription;
         4: CellText := Data^.FAuthor;
         5: case Data^.FPackageType of
              ptRunAndDesignTime: CellText := rsMainFrmVSTTextPackageType0;
              ptDesignTime:       CellText := rsMainFrmVSTTextPackageType1;
              ptRunTime:          CellText := rsMainFrmVSTTextPackageType2;
              ptRunTimeOnly:      CellText := rsMainFrmVSTTextPackageType3;
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
   MessageDlgEx(rsCreateRepositoryPackageError1 + ' "' + AZipFile + '". ' + rsProgressFrmError1 + sLineBreak +
                AErrMsg, mtError, [mbOk], TForm(Self.Parent));
   ShowHideControls(2);
   bCreate.Enabled := True;
   pnBrowse.Enabled := True;
end;

procedure TCreateRepositoryPackagefr.DoOnZipCompleted(Sender: TObject);
var
  SerializablePackages: TSerializablePackages;
  Package: TPackage;
  PackageFile: TPackageFile;
  RootNode, Node: PVirtualNode;
  RootData, Data: PData;
  JSON: TJSONStringType;
  MS: TMemoryStream;
begin
  pnMessage.Caption := rsCreateRepositoryPackageMessage5;
  pnMessage.Invalidate;
  Sleep(2000);

  SerializablePackages := TSerializablePackages.Create;
  try
    RootNode := FVSTPackages.GetFirst;
    if RootNode <> nil then
    begin
      RootData := FVSTPackages.GetNodeData(RootNode);
      Package := SerializablePackages.AddPackage(RootData^.FName);
      Package.Category := TPackageCategory(RootData^.FCategory);
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
          if cbOpen.Checked then
            OpenDocument(FDestDir);
          MessageDlgEx(rsCreateRepositoryPackageMessage6, mtInformation, [mbOk], TForm(Self.Parent));
        finally
          MS.Free;
        end;
      end
      else
       MessageDlgEx(rsCreateRepositoryPackageError2 + sLineBreak + SerializablePackages.LastError, mtInformation, [mbOk], TForm(Self.Parent));
    end;
    ShowHideControls(2);
  finally
    SerializablePackages.Free;
  end;
  bCreate.Enabled := True;
  pnBrowse.Enabled := True;
end;



end.

