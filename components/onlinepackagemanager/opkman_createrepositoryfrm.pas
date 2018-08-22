{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
}

unit opkman_createrepositoryfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpjson, VirtualTrees,
  // LCL
  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus,
  // IDEIntf
  PackageIntf,
  // LazUtils
  LazFileUtils, LazUTF8,
  // OpkMan
  opkman_serializablepackages;

type

  PRepository = ^TRepository;
  TRepository = record
    FName: String;
    FPath: String;
    FAddress: String;
    FDescription: String;
  end;


  { TCreateRepositoryFrm }

  TCreateRepositoryFrm = class(TForm)
    bCancel: TButton;
    bAdd: TBitBtn;
    bDelete: TBitBtn;
    bOpen: TButton;
    bCreate: TButton;
    imTree: TImageList;
    miRepDetails: TMenuItem;
    ODRep: TOpenDialog;
    pnButtons: TPanel;
    pnMessage: TPanel;
    pnPackages: TPanel;
    pnDetails: TPanel;
    pm: TPopupMenu;
    spMain: TSplitter;
    tmWait: TTimer;
    procedure bAddClick(Sender: TObject);
    procedure bCreateClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miRepDetailsClick(Sender: TObject);
    procedure pnButtonsResize(Sender: TObject);
    procedure tmWaitTimer(Sender: TObject);
  private
    FVSTPackages: TVirtualStringTree;
    FVSTDetails: TVirtualStringTree;
    FRepository: TRepository;
    FSortDirection: TSortDirection;
    FSerializablePackages: TSerializablePackages;
    procedure EnableDisableButtons(const AEnable: Boolean);
    procedure ShowHideControls(const AType: Integer);
    function LoadRepository(const AFileName: String): Boolean;
    function SaveRepository(const AFileName: String): Boolean;
    procedure PopulatePackageTree;
    procedure AddNewPackage;
    procedure AddExistingPackage(const AJSONFile, APackageFile: String);
    function GetDisplayString(const AStr: String): String;
    function LoadJSONFromFile(const AFileName: String; out AJSON: TJSONStringType): Boolean;
    function SaveJSONToFile(const AFileName: String; const AJSON: TJSONStringType): Boolean;
    function IsDuplicatePackage(const AJSON: TJSONStringType; const APackageFile: String): Boolean;
    procedure VSTPackagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTPackagesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; {%H-}Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTPackagesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure VSTPackagesCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTPackagesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Column: TColumnIndex);
    procedure VSTPackagesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

    procedure VSTDetailsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTDetailsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; {%H-}Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTDetailsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public

  end;

var
  CreateRepositoryFrm: TCreateRepositoryFrm;

implementation

uses opkman_common, opkman_const, opkman_options, opkman_repositorydetailsfrm,
     opkman_addrepositorypackagefrm, opkman_createrepositorypackagefrm;

{$R *.lfm}

{ TCreateRepositoryFrm }

type
  PData = ^TData;
  TData = record
    FRepository: TRepository;
    FPackageRelativePath: String;
    FPackageBaseDir: String;
    FFullPath: String;
    FDataType: Integer;
    FName: String;
    FDisplayName: String;
    FPackageType: TLazPackageType;
    FRepositoryFileName: String;
    FRepositoryFileSize: Int64;
    FRepositoryFileHash: String;
    FRepositoryDate: TDateTime;
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
    FCommunityDescription: String;
  end;

procedure TCreateRepositoryFrm.FormCreate(Sender: TObject);
begin
  Caption := rsCreateRepositoryFrm_Caption;
  bCreate.Caption := rsCreateRepositoryFrm_bCreate_Caption;
  bCreate.Hint := rsCreateRepositoryFrm_bCreate_Hint;
  bOpen.Caption := rsCreateRepositoryFrm_bOpen_Caption;
  bOpen.Hint := rsCreateRepositoryFrm_bOpen_Hint;
  bAdd.Caption := rsCreateRepositoryFrm_bAdd_Caption;
  bAdd.Hint := rsCreateRepositoryFrm_bAdd_Hint;
  bDelete.Caption := rsCreateRepositoryFrm_bDelete_Caption;
  bDelete.Hint := rsCreateRepositoryFrm_bDelete_Hint;
  bCancel.Caption := rsCreateRepositoryFrm_bCancel_Caption;
  bCancel.Hint := rsCreateRepositoryFrm_bCancel_Hint;
  miRepDetails.Caption := rsCreateRepositoryFrm_miRepDetails_Caption;
  FSortDirection := sdAscending;
  EnableDisableButtons(True);
  ShowHideControls(0);

  FSerializablePackages := TSerializablePackages.Create;
  FVSTPackages := TVirtualStringTree.Create(nil);
  with FVSTPackages do
  begin
    NodeDataSize := SizeOf(TData);
    Parent := pnPackages;
    Align := alClient;
    Images := imTree;
    if not Options.UseDefaultTheme then
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
      Width := 300;
      Text := rsCreateRepositoryFrm_VSTPackages_Column0;
    end;
    Header.Options := [hoAutoResize, hoColumnResize, hoRestrictDrag, hoVisible, hoAutoSpring];
    Header.SortColumn := 0;
    TabOrder := 0;
    TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
    TreeOptions.PaintOptions := [toHideFocusRect, toPopupMode, toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
    TreeOptions.SelectionOptions := [toRightClickSelect];
    TreeOptions.AutoOptions := [toAutoTristateTracking];
    IncrementalSearch := isAll;
    IncrementalSearchDirection := sdForward;
    IncrementalSearchStart := ssAlwaysStartOver;
    IncrementalSearchTimeout := 1500;
    PopupMenu := pm;
    OnGetText := @VSTPackagesGetText;
    OnGetImageIndex := @VSTPackagesGetImageIndex;
    OnHeaderClick := @VSTPackagesHeaderClick;
    OnCompareNodes := @VSTPackagesCompareNodes;
    OnFocusChanged := @VSTPackagesFocusChanged;
    OnFreeNode := @VSTPackagesFreeNode;
  end;

  FVSTDetails := TVirtualStringTree.Create(nil);
  with FVSTDetails do
  begin
    NodeDataSize := SizeOf(TData);
    Parent := pnDetails;
    Align := alClient;
    Images := imTree;
    if not Options.UseDefaultTheme then
      Color := clBtnFace;
    DefaultNodeHeight := 25;
    Indent := 15;
    TabOrder := 0;
    DefaultText := '';
    Header.AutoSizeIndex := 1;
    Header.Height := 25;
    Colors.BorderColor := clBlack;
    with Header.Columns.Add do begin
      Position := 0;
      Width := 200;
      Text := rsCreateRepositoryFrm_VSTDetails_Column0;
    end;
    with Header.Columns.Add do begin
      Position := 1;
      Width := 250;
      Text := rsCreateRepositoryFrm_VSTDetails_Column1;
    end;
    Header.Options := [hoAutoResize, hoColumnResize, hoRestrictDrag, hoVisible, hoAutoSpring];
    Header.SortColumn := 0;
    TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
    TreeOptions.PaintOptions := [toHideFocusRect, toPopupMode, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages];
    TreeOptions.SelectionOptions := [toRightClickSelect, toFullRowSelect];
    TreeOptions.AutoOptions := [toAutoTristateTracking];
    PopupMenu := pm;
    OnGetText := @VSTDetailsGetText;
    OnGetImageIndex := @VSTDetailsGetImageIndex;
    OnFreeNode := @VSTDetailsFreeNode;
  end;
end;

procedure TCreateRepositoryFrm.bCreateClick(Sender: TObject);
var
  RepositoryDetailsFrm: TRepositoryDetailsFrm;
begin
  RepositoryDetailsFrm := TRepositoryDetailsFrm.Create(Self);
  try
    RepositoryDetailsFrm.IsNew := True;
    RepositoryDetailsFrm.ShowModal;
    if RepositoryDetailsFrm.ModalResult = mrOk then
    begin
      FRepository.FName := RepositoryDetailsFrm.edName.Text;
      FRepository.FAddress := RepositoryDetailsFrm.edAddress.Text;
      FRepository.FDescription := RepositoryDetailsFrm.mDescription.Text;
      if SaveRepository(RepositoryDetailsFrm.FileName) then
        if LoadRepository(RepositoryDetailsFrm.FileName) then
           PopulatePackageTree;
    end;
  finally
    RepositoryDetailsFrm.Free;
  end;
end;

procedure TCreateRepositoryFrm.bDeleteClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PData;
  PackageFile: String;
  PackageIndex: Integer;
  CanGo: Boolean;
  JSON: TJSONStringType;
begin
  Node := FVSTPackages.GetFirstSelected;
  if Node <> nil then
  begin
    Data := FVSTPackages.GetNodeData(Node);
    if Data^.FDataType = 1 then
    begin
      if MessageDlgEx(Format(rsCreateRepositoryFrm_Conf1, [Data^.FDisplayname]), mtConfirmation, [mbYes, mbNo], Self) = mrNo then
        Exit;
      CanGo := False;
      PackageIndex := FSerializablePackages.FindPackageIndex(Data^.FName, fpbPackageName);
      if PackageIndex <> -1 then
      begin
        PackageFile := ExtractFilePath(FRepository.FPath) + Data^.FRepositoryFileName;
        FSerializablePackages.DeletePackage(PackageIndex);
        JSON := '';
        if FSerializablePackages.PackagesToJSON(JSON) then
        begin
          if SaveJSONToFile(ExtractFilePath(FRepository.FPath) + cRemoteJSONFile, JSON) then
          begin
            DeleteFile(PackageFile);
            if LoadRepository(FRepository.FPath) then
            begin
              CanGo := True;
              PopulatePackageTree;
            end;
          end;
        end;
      end;
      if not CanGo then
        MessageDlgEx(Format(rsCreateRepositoryFrm_Error5, [Data^.FDisplayname]), mtError, [mbOk], Self);
    end;
  end;
end;

function TCreateRepositoryFrm.IsDuplicatePackage(const AJSON: TJSONStringType;
  const APackageFile: String): Boolean;
var
  SP: TSerializablePackages;
  MetaPackage: TMetaPackage;
  LazarusPackage: TLazarusPackage;
  TargetPackageFile: String;
  I: Integer;
begin
  Result := False;
  SP := TSerializablePackages.Create;
  try
    if SP.JSONToPackages(AJSON) then
    begin
      MetaPackage := FSerializablePackages.FindMetaPackage(SP.Items[0].Name, fpbPackageName);
      if MetaPackage <> nil then
      begin
        Result := True;
        MessageDlgEx(Format(rsCreateRepositoryFrm_Info3, [MetaPackage.DisplayName]), mtInformation, [mbOk], Self);
      end;

      if not Result then
      begin
        for I := 0 to SP.Items[0].LazarusPackages.Count - 1 do
        begin
          LazarusPackage := FSerializablePackages.FindLazarusPackage(TLazarusPackage(SP.Items[0].LazarusPackages.Items[I]).Name);
          if LazarusPackage <> nil then
          begin
            Result := True;
            MessageDlgEx(Format(rsCreateRepositoryFrm_Info5, [TLazarusPackage(SP.Items[0].LazarusPackages.Items[I]).Name]), mtInformation, [mbOk], Self);
            Break;
          end;
        end;

        if not Result then
        begin
          TargetPackageFile := AppendPathDelim(ExtractFilePath(FRepository.FPath)) + ExtractFileName(APackageFile);
          if TargetPackageFile <> APackageFile then
          begin
            if FileExists(TargetPackageFile) then
              if MessageDlgEx(Format(rsCreateRepositoryFrm_Conf2, [TargetPackageFile]), mtInformation, [mbYes, mbNo], Self) = mrNo then
                Result := True;

            if (not Result) and (not CopyFile(APackageFile, TargetPackageFile, [cffOverwriteFile], True)) then
              Result := True;
          end;
        end;
      end;
    end;
  finally
    SP.Free;
  end;
end;

procedure TCreateRepositoryFrm.AddNewPackage;
var
  CreateRepositoryPackagesFrm: TCreateRepositoryPackagesFrm;
  JSON: TJSONStringType;
  CanGo: Boolean;
begin
  CreateRepositoryPackagesFrm := TCreateRepositoryPackagesFrm.Create(Self);
  try
    with CreateRepositoryPackagesFrm do
    begin
      SetType(1);
      DestDir := AppendPathDelim(AppendPathDelim(ExtractFilePath(FRepository.FPath)) + 'Temp');
      if not DirectoryExists(DestDir) then
        CreateDir(DestDir);
      ShowModal;
      if ModalResult = mrOk then
      begin
        CanGo := False;
        if FileExists(PackageFile) and FileExists(JSONFile) then
        begin
          if LoadJSONFromFile(JSONFile, JSON) then
          begin
            if not IsDuplicatePackage(JSON, PackageFile) then
            begin
              if FSerializablePackages.AddPackageFromJSON(JSON) then
              begin
                JSON := '';
                FSerializablePackages.Sort(stName, soAscendent);
                if FSerializablePackages.PackagesToJSON(JSON) then
                begin
                  if SaveJSONToFile(ExtractFilePath(FRepository.FPath) + cRemoteJSONFile, JSON) then
                  begin
                    if LoadRepository(FRepository.FPath) then
                    begin
                      CanGo := True;
                      PopulatePackageTree;
                    end;
                  end;
                end;
              end;
            end;
          end;
          DeleteDirectory(DestDir, False);
        end;
        if not CanGo then
          MessageDlgEx(rsCreateRepositoryFrm_Error4, mtError, [mbOk], Self)
        else
          MessageDlgEx(rsCreateRepositoryFrm_Info7, mtInformation, [mbOk], Self);
      end;
    end;
  finally
    CreateRepositoryPackagesFrm.Free;
  end;
end;

procedure TCreateRepositoryFrm.AddExistingPackage(const AJSONFile,
  APackageFile: String);
var
  JSON: TJSONStringType;
  CanGo: Boolean;
begin
  CanGo := False;
  if LoadJSONFromFile(AJSONFile, JSON) then
  begin
    if not IsDuplicatePackage(JSON, APackageFile) then
    begin
      if FSerializablePackages.AddPackageFromJSON(JSON) then
      begin
        JSON := '';
        FSerializablePackages.Sort(stName, soAscendent);
        if FSerializablePackages.PackagesToJSON(JSON) then
        begin
          if SaveJSONToFile(ExtractFilePath(FRepository.FPath) + cRemoteJSONFile, JSON) then
          begin
            if LoadRepository(FRepository.FPath) then
            begin
              CanGo := True;
              PopulatePackageTree;
            end;
          end;
        end;
      end;
    end;
  end;
  if not CanGo then
    MessageDlgEx(rsCreateRepositoryFrm_Error4, mtError, [mbOk], Self)
  else
    MessageDlgEx(rsCreateRepositoryFrm_Info7, mtInformation, [mbOk], Self);
end;

procedure TCreateRepositoryFrm.bAddClick(Sender: TObject);
begin
  AddRepositoryPackageFrm := TAddRepositoryPackageFrm.Create(Self);
  try
    AddRepositoryPackageFrm.ShowModal;
    if AddRepositoryPackageFrm.ModalResult = mrOk then
    begin
      if AddRepositoryPackageFrm.rbCreateNew.Checked then
        AddNewPackage
      else
        AddExistingPackage(AddRepositoryPackageFrm.JSONFile, AddRepositoryPackageFrm.PackageFile);
      FVSTPackages.SortTree(0, FSortDirection);
    end;
  finally
    AddRepositoryPackageFrm.Free;
  end;
end;

procedure TCreateRepositoryFrm.bOpenClick(Sender: TObject);
begin
  if ODRep.Execute then
    if LoadRepository(ODRep.FileName) then
      PopulatePackageTree;
end;

procedure TCreateRepositoryFrm.FormDestroy(Sender: TObject);
begin
  FVSTPackages.Free;
  FVSTDetails.Free;
  FSerializablePackages.Free
end;

procedure TCreateRepositoryFrm.FormShow(Sender: TObject);
begin
  tmWait.Enabled := True;
end;

procedure TCreateRepositoryFrm.miRepDetailsClick(Sender: TObject);
var
  RepositoryDetailsFrm: TRepositoryDetailsFrm;
  Node: PVirtualNode;
  Data: PData;
begin
  RepositoryDetailsFrm := TRepositoryDetailsFrm.Create(Self);
  try
    RepositoryDetailsFrm.edName.Text := FRepository.FName;
    RepositoryDetailsFrm.edAddress.Text := FRepository.FAddress;
    RepositoryDetailsFrm.mDescription.Text := FRepository.FDescription;
    RepositoryDetailsFrm.IsNew := False;
    RepositoryDetailsFrm.ShowModal;
    if RepositoryDetailsFrm.ModalResult = mrOk then
    begin
      if FRepository.FName <> RepositoryDetailsFrm.edName.Text then
      begin
        Node := FVSTPackages.GetFirst;
        if Node <> nil then
        begin
          Data := FVSTPackages.GetNodeData(Node);
          if Data^.FDataType = 0 then
          begin
            Data^.FName := RepositoryDetailsFrm.edName.Text;
            FVSTPackages.ReinitNode(Node, False);
            FVSTPackages.RepaintNode(Node);
          end;
        end;
      end;
      FRepository.FName := RepositoryDetailsFrm.edName.Text;
      FRepository.FAddress := RepositoryDetailsFrm.edAddress.Text;
      FRepository.FDescription := RepositoryDetailsFrm.mDescription.Text;
      if SaveRepository(FRepository.FPath) then
        if LoadRepository(FRepository.FPath) then
          PopulatePackageTree;
    end;
  finally
    RepositoryDetailsFrm.Free;
  end;
end;

procedure TCreateRepositoryFrm.tmWaitTimer(Sender: TObject);
begin
  tmWait.Enabled := False;
  if (Options.LastPrivateRepository <> '') and (FileExists(Options.LastPrivateRepository)) then
  begin
    if LoadRepository(Options.LastPrivateRepository) then
      PopulatePackageTree;
  end;
end;

procedure TCreateRepositoryFrm.pnButtonsResize(Sender: TObject);
begin
  bAdd.Left := (pnButtons.Width - (bAdd.Width + bDelete.Width)) div 2;
  bDelete.Left := bAdd.Left + bAdd.Width + 1;
end;

procedure TCreateRepositoryFrm.EnableDisableButtons(const AEnable: Boolean);
var
  Node: PVirtualNode;
  Data: PData;
begin
  bOpen.Enabled := AEnable;
  bCreate.Enabled := AEnable;
  bAdd.Enabled := AEnable and FileExists(Trim(FRepository.FPath));
  bCancel.Enabled := AEnable;
  if Assigned(FVSTPackages) then
  begin
    Node := FVSTPackages.GetFirstSelected;
    if Node <> nil then
    begin
      Data := FVSTPackages.GetNodeData(Node);
      bDelete.Enabled := AEnable and FileExists(Trim(FRepository.FPath)) and (Data^.FDataType = 1);
    end
    else
      bDelete.Enabled := False;
  end
  else
    bDelete.Enabled := False;
end;

procedure TCreateRepositoryFrm.ShowHideControls(const AType: Integer);
begin
  case AType of
    0: begin
         pnPackages.Visible := False;
         pnDetails.Visible := False;
         pnMessage.Visible := False;
       end;
    1: begin
         pnPackages.Visible := False;
         pnDetails.Visible := False;
         pnMessage.Visible := True;
       end;
    2: begin
         pnPackages.Visible := True;
         pnDetails.Visible := True;
         pnMessage.Visible := False;
       end;
  end;
end;

function TCreateRepositoryFrm.LoadRepository(const AFileName: String): Boolean;
var
  FS: TFileStream;
  DestDir: String;
begin
  Result := False;
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    try
      FRepository.FName := FS.ReadAnsiString;
      FRepository.FAddress := FS.ReadAnsiString;
      FRepository.FDescription := FS.ReadAnsiString;
      FRepository.FPath := AFileName;
      Caption := rsCreateRepositoryFrm_Caption + '(' + AFileName + ')';
      Options.LastPrivateRepository := AFileName;
      Options.Changed := True;
      DestDir := AppendPathDelim(AppendPathDelim(ExtractFilePath(AFileName)) + 'Temp');
      if DirectoryExists(DestDir) then
        DeleteDirectory(DestDir, False);
      Result := True;
    except
      on E: Exception do
      begin
        MessageDlgEx(Format(rsCreateRepositoryFrm_Error1, [AFileName, E.Message]), mtError, [mbOk], Self);
        Options.LastPrivateRepository := '';
        Options.Changed := True;
      end;
    end;
  finally
    FS.Free;
  end;
end;

function TCreateRepositoryFrm.SaveRepository(const AFileName: String): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  FS := TFileStream.Create(AFileName, fmCreate or fmOpenWrite or fmShareDenyWrite);
  try
    try
      FS.WriteAnsiString(FRepository.FName);
      FS.WriteAnsiString(FRepository.FAddress);
      FS.WriteAnsiString(FRepository.FDescription);
      Result := True;
    except
      on E: Exception do
        MessageDlgEx(Format(rsCreateRepositoryFrm_Error3, [AFileName, E.Message]), mtError, [mbOk], Self);
    end;
  finally
    FS.Free;
  end;
end;

procedure TCreateRepositoryFrm.PopulatePackageTree;
var
  RootNode, Node, ChildNode: PVirtualNode;
  RootData, Data, ChildData: PData;
  JSON: TJSONStringType;
  i, j: Integer;
  MetaPackage: TMetaPackage;
  LazarusPackage: TLazarusPackage;
begin

  FVSTPackages.Clear;
  FVSTPackages.NodeDataSize := SizeOf(TData);

  //add repository(DataType = 0)
  RootNode := FVSTPackages.AddChild(nil);
  RootData := FVSTPackages.GetNodeData(RootNode);
  RootData^.FName := FRepository.FName;
  RootData^.FDataType := 0;

  if LoadJSONFromFile(ExtractFilePath(FRepository.FPath) + cRemoteJSONFile, JSON) then
  begin
    FSerializablePackages.JSONToPackages(JSON);
    for I := 0 to FSerializablePackages.Count - 1 do
    begin
      MetaPackage := TMetaPackage(FSerializablePackages.Items[I]);
      Node := FVSTPackages.AddChild(RootNode);
      Data := FVSTPackages.GetNodeData(Node);
      Data^.FDisplayName := MetaPackage.DisplayName;
      Data^.FName := MetaPackage.Name;
      Data^.FCategory := MetaPackage.Category;
      Data^.FRepositoryFileName := MetaPackage.RepositoryFileName;
      Data^.FRepositoryFileSize := MetaPackage.RepositoryFileSize;
      Data^.FRepositoryFileHash := MetaPackage.RepositoryFileHash;
      Data^.FRepositoryDate := MetaPackage.RepositoryDate;
      Data^.FHomePageURL := MetaPackage.HomePageURL;
      Data^.FDownloadURL := MetaPackage.DownloadURL;
      Data^.FCommunityDescription := MetaPackage.CommunityDescription;
      Data^.FDataType := 1;
      for J := 0 to MetaPackage.LazarusPackages.Count - 1 do
      begin
        LazarusPackage := TLazarusPackage(MetaPackage.LazarusPackages.Items[J]);
        ChildNode := FVSTPackages.AddChild(Node);
        ChildData := FVSTPackages.GetNodeData(ChildNode);
        ChildData^.FName := LazarusPackage.Name;
        ChildData^.FVersionAsString := LazarusPackage.VersionAsString;
        ChildData^.FDescription := LazarusPackage.Description;
        ChildData^.FAuthor := LazarusPackage.Author;
        ChildData^.FLazCompatibility := LazarusPackage.LazCompatibility;
        ChildData^.FFPCCompatibility := LazarusPackage.FPCCompatibility;
        ChildData^.FSupportedWidgetSet := LazarusPackage.SupportedWidgetSet;
        ChildData^.FPackageType := LazarusPackage.PackageType;
        ChildData^.FLicense := LazarusPackage.License;
        ChildData^.FDependenciesAsString := LazarusPackage.DependenciesAsString;
        ChildData^.FDataType := 2;
      end;
    end;
  end;
  //properly init each node to prevent memory leaks
  FVSTPackages.FullExpand;
  FVSTPackages.FullCollapse;
  if RootNode <> nil then
  begin
    FVSTPackages.Selected[RootNode] := True;
    FVSTPackages.FocusedNode := RootNode;
    FVSTPackages.Expanded[RootNode] := True;
  end;
  ShowHideControls(2);
  EnableDisableButtons(True);
end;

function TCreateRepositoryFrm.GetDisplayString(const AStr: String): String;
var
  SL: TStringList;
  I: Integer;
begin
  Result := '';
  SL := TStringList.Create;
  try
    SL.Text := AStr;
    for I := 0 to SL.Count - 1 do
      if Result = '' then
        Result := SL.Strings[I]
      else
        Result := Result + ' ' + SL.Strings[I];
  finally
    SL.Free;
  end;
end;

function TCreateRepositoryFrm.LoadJSONFromFile(const AFileName: String;
  out AJSON: TJSONStringType): Boolean;
var
  MS: TMemoryStream;
begin
  Result := False;
  if not FileExists(AFileName) then
    Exit;
  MS := TMemoryStream.Create;
  try
    Ms.LoadFromFile(AFileName);
    if Ms.Size > 0 then
    begin
      Ms.Position := 0;
      SetLength(AJSON, MS.Size);
      MS.Read(Pointer(AJSON)^, Length(AJSON));
      Result := True;
    end;
  finally
    MS.Free;
  end;
end;

function TCreateRepositoryFrm.SaveJSONToFile(const AFileName: String;
  const AJSON: TJSONStringType): Boolean;
var
  MS: TMemoryStream;
begin
  Result := False;
  MS := TMemoryStream.Create;
  try
    Ms.Write(Pointer(AJSON)^, Length(AJSON));
    Ms.Position := 0;
    Ms.SaveToFile(AFileName);
    Result := True;
  finally
    MS.Free;
  end;
end;

procedure TCreateRepositoryFrm.VSTPackagesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PData;
begin
  Data := FVSTPackages.GetNodeData(Node);
  case Data^.FDataType of
    0: CellText := FRepository.FName;
    1: CellText := Data^.FDisplayName;
    2: CellText := Data^.FName;
  end;
end;

procedure TCreateRepositoryFrm.VSTPackagesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  Data := FVSTPackages.GetNodeData(Node);
  ImageIndex := Data^.FDataType;
end;

procedure TCreateRepositoryFrm.VSTPackagesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Button = mbLeft then
  begin
    with Sender, Treeview do
    begin
      if (SortColumn = NoColumn) or (SortColumn <> HitInfo.Column) then
      begin
        SortColumn    := HitInfo.Column;
        SortDirection := VirtualTrees.sdAscending;
      end
      else
      begin
        if SortDirection = VirtualTrees.sdAscending then
          SortDirection := VirtualTrees.sdDescending
        else
          SortDirection := VirtualTrees.sdAscending;
      end;
      SortTree(SortColumn, SortDirection, False);
      FSortDirection := SortDirection;
    end;
  end;
end;

procedure TCreateRepositoryFrm.VSTPackagesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PData;
  Data2: PData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    0: begin
         if (Data1^.FDataType < Data2^.FDataType) then
           Result := 0
         else
           Result := CompareText(Data1^.FName, Data2^.FName)
       end;
  end;
end;

procedure TCreateRepositoryFrm.VSTPackagesFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PData;
  DetailNode: PVirtualNode;
  DetailData: PData;
begin
  if Node = nil then
    Exit;

  FVSTDetails.Clear;
  FVSTDetails.NodeDataSize := SizeOf(TData);
  Data := FVSTPackages.GetNodeData(Node);
  case Data^.FDataType of
    0: begin
         //address
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FDataType := 17;
         DetailData^.FRepository.FAddress := FRepository.FAddress;
         //description
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FDataType := 3;
         DetailData^.FRepository.FDescription := FRepository.FDescription;
       end;
    1: begin
         //add category(DataType = 12)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FCategory := Data^.FCategory;
         DetailData^.FDataType := 12;
         //add Repository Filename(DataType = 13)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FRepositoryFileName := Data^.FRepositoryFileName;
         DetailData^.FDataType := 13;
         //add Repository Filesize(DataType = 14)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FRepositoryFileSize := Data^.FRepositoryFileSize;
         DetailData^.FDataType := 14;
         //add Repository Hash(DataType = 15)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FRepositoryFileHash := Data^.FRepositoryFileHash;
         DetailData^.FDataType := 15;
         //add Repository Date(DataType = 16)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FRepositoryDate := Data^.FRepositoryDate;
         DetailData^.FDataType := 16;
         FVSTDetails.Expanded[DetailNode] := True;
         //add HomePageURL(DataType = 17)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FHomePageURL := Data^.FHomePageURL;
         DetailData^.FDataType := 17;
         //add DownloadURL(DataType = 18)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FDownloadURL := Data^.FDownloadURL;
         DetailData^.FDataType := 18;
         //add CommunityDescription(DataType = 19)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FCommunityDescription := Data^.FCommunityDescription;
         DetailData^.FDataType := 19;
       end;
    2: begin
         //add description(DataType = 2)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FVersionAsString := Data^.FVersionAsString;
         DetailData^.FDataType := 2;
         //add description(DataType = 3)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FDescription := Data^.FDescription;
         DetailData^.FDataType := 3;
         //add author(DataType = 4)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FAuthor := Data^.FAuthor;
         DetailData^.FDataType := 4;
         //add lazcompatibility(DataType = 5)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FLazCompatibility := Data^.FLazCompatibility;
         DetailData^.FDataType := 5;
         //add fpccompatibility(DataType = 6)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FFPCCompatibility := Data^.FFPCCompatibility;
         DetailData^.FDataType := 6;
         //add widgetset(DataType = 7)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FSupportedWidgetSet := Data^.FSupportedWidgetSet;
         DetailData^.FDataType := 7;
         //add packagetype(DataType = 8)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FPackageType := Data^.FPackageType;
         DetailData^.FDataType := 8;
         //add license(DataType = 9)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FLicense := Data^.FLicense;
         DetailData^.FDataType := 9;
         //add dependencies(DataType = 10)
         DetailNode := FVSTDetails.AddChild(nil);
         DetailData := FVSTDetails.GetNodeData(DetailNode);
         DetailData^.FDependenciesAsString := Data^.FDependenciesAsString;
         DetailData^.FDataType := 10;
       end;
  end;
  EnableDisableButtons(True);
end;

procedure TCreateRepositoryFrm.VSTPackagesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVSTPackages.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TCreateRepositoryFrm.VSTDetailsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  PackageNode: PVirtualNode;
  PackageData: PData;
  DetailData: PData;
begin
  if TextType <> ttNormal then
    Exit;

  PackageNode := FVSTPackages.GetFirstSelected;
  if PackageNode = nil then
    Exit;

  PackageData := FVSTPackages.GetNodeData(PackageNode);
  case PackageData^.FDataType of
    0: begin
         DetailData := FVSTDetails.GetNodeData(Node);
         case DetailData^.FDataType of
           17: if Column = 0 then
                CellText := rsCreateRepositoryFrm_RepositoryAddress
              else
                CellText := DetailData^.FRepository.FAddress;
           3: if Column = 0 then
                CellText := rsCreateRepositoryFrm_RepositoryDescription
              else
                CellText := DetailData^.FRepository.FDescription;
         end;
       end;
    1: begin
         DetailData := FVSTDetails.GetNodeData(Node);
         case DetailData^.FDataType of
           12: if Column = 0 then
                 CellText := rsCreateRepositoryFrm_VSTText_Category
               else
                 CellText := DetailData^.FCategory;
           13: if Column = 0 then
                 CellText := rsCreateRepositoryFrm_VSTText_RepositoryFilename
               else
                 CellText := DetailData^.FRepositoryFileName;
           14: if Column = 0 then
                 CellText := rsCreateRepositoryFrm_VSTText_RepositoryFileSize
               else
                 CellText := FormatSize(DetailData^.FRepositoryFileSize);
           15: if Column = 0 then
                 CellText := rsCreateRepositoryFrm_VSTText_RepositoryFileHash
               else
                 CellText := DetailData^.FRepositoryFileHash;
           16: if Column = 0 then
                 CellText := rsCreateRepositoryFrm_VSTText_RepositoryFileDate
               else
                 CellText := FormatDateTime('YYYY.MM.DD', DetailData^.FRepositoryDate);
           17: if Column = 0 then
                 CellText := rsCreateRepositoryFrm_VSTText_HomePageURL
               else
                 CellText := DetailData^.FHomePageURL;
          18:  if Column = 0 then
                 CellText := rsCreateRepositoryFrm_VSTText_DownloadURL
               else
                 CellText := DetailData^.FDownloadURL;
          19:  if Column = 0 then
                  CellText := rsMainFrm_VSTText_CommunityDescription
                else
                  CellText := DetailData^.FCommunityDescription;
         end;
       end;
    2: begin
         DetailData := FVSTDetails.GetNodeData(Node);
         case DetailData^.FDataType of
           2: if Column = 0 then
                CellText := rsCreateRepositoryFrm_VSTText_Version
              else
                CellText := DetailData^.FVersionAsString;
           3: if Column = 0 then
                CellText := rsCreateRepositoryFrm_VSTText_Description
              else
                CellText := GetDisplayString(DetailData^.FDescription);
           4: if Column = 0 then
                CellText := rsCreateRepositoryFrm_VSTText_Author
              else
                CellText := DetailData^.FAuthor;
           5: if Column = 0 then
                CellText := rsCreateRepositoryFrm_VSTText_LazCompatibility
              else
                CellText := DetailData^.FLazCompatibility;
           6: if Column = 0 then
                CellText := rsCreateRepositoryFrm_VSTText_FPCCompatibility
              else
                CellText := DetailData^.FFPCCompatibility;
           7: if Column = 0 then
                CellText := rsCreateRepositoryFrm_VSTText_SupportedWidgetsets
              else
                CellText := DetailData^.FSupportedWidgetSet;
           8: if Column = 0 then
                CellText := rsCreateRepositoryFrm_VSTText_Packagetype
              else
                case DetailData^.FPackageType of
                   lptRunAndDesignTime: CellText := rsMainFrm_VSTText_PackageType0;
                   lptDesignTime:       CellText := rsMainFrm_VSTText_PackageType1;
                   lptRunTime:          CellText := rsMainFrm_VSTText_PackageType2;
                   lptRunTimeOnly:      CellText := rsMainFrm_VSTText_PackageType3;
                end;
           9: if Column = 0 then
                CellText := rsCreateRepositoryFrm_VSTText_License
              else
                CellText := GetDisplayString(DetailData^.FLicense);
          10: if Column = 0 then
                CellText := rsCreateRepositoryFrm_VSTText_Dependecies
              else
                CellText := DetailData^.FDependenciesAsString;

         end;
       end;
  end;
end;

procedure TCreateRepositoryFrm.VSTDetailsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  if Column = 0 then
  begin
    Data := FVSTDetails.GetNodeData(Node);
    ImageIndex := Data^.FDataType;
  end;
end;

procedure TCreateRepositoryFrm.VSTDetailsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVSTDetails.GetNodeData(Node);
  Finalize(Data^);
end;

end.

