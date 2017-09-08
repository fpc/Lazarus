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
  Classes, SysUtils, FileUtil,
  // LCL
  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons,
  // LazUtils
  LazFileUtils,
  // OpkMan
  opkman_VirtualTrees;

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
    bAdd: TButton;
    bCancel: TButton;
    bDelete: TButton;
    bOpen: TButton;
    bCreate: TButton;
    imTree: TImageList;
    OD: TOpenDialog;
    pnButtons: TPanel;
    pnMessage: TPanel;
    pnPackages: TPanel;
    pnDetails: TPanel;
    SD: TSaveDialog;
    spMain: TSplitter;
    procedure bCreateClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnButtonsResize(Sender: TObject);
  private
    FVSTPackages: TVirtualStringTree;
    FVSTDetails: TVirtualStringTree;
    FRepository: TRepository;
    procedure EnableDisableButtons(const AEnable: Boolean);
    procedure ShowHideControls(const AType: Integer);
    function LoadRepository(const AFileName: String; out AErrMsg: String): Boolean;
    function SaveRepository(const AFileName: String; out AErrMsg: String): Boolean;
    procedure VSTPackagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTPackagesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; {%H-}Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTPackagesHeaderClick(Sender: TVTHeader; {%H-}Column: TColumnIndex;
      Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
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

uses opkman_common, opkman_const, opkman_options, opkman_serializablepackages,
  opkman_repositorydetailsfrm;

{$R *.lfm}

{ TCreateRepositoryFrm }

type
  PMetaData = ^TMetaData;
  TMetaData = record
    FDataType: Integer;
    FName: String;
  end;

  PDetails = ^TDetails;
  TDetails = record
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
  EnableDisableButtons(True);
  ShowHideControls(0);

  //continue
  EnableDisableButtons(False);
  pnMessage.Caption := 'Not yet implemented!';
  pnMessage.Visible := True;

  FVSTPackages := TVirtualStringTree.Create(nil);
  with FVSTPackages do
  begin
    NodeDataSize := SizeOf(TMetaData);
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
      Width := 250;
      Text := rsCreateRepositoryFrm_VSTPackages_Column0;
    end;
    Header.Options := [hoAutoResize, hoColumnResize, hoRestrictDrag, hoVisible, hoAutoSpring];
    Header.SortColumn := 0;
    TabOrder := 0;
    TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
    TreeOptions.PaintOptions := [toHideFocusRect, toPopupMode, toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
    TreeOptions.SelectionOptions := [toRightClickSelect];
    TreeOptions.AutoOptions := [toAutoTristateTracking];
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
    NodeDataSize := SizeOf(TDetails);
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
      Width := 150;
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
    OnGetText := @VSTDetailsGetText;
    OnGetImageIndex := @VSTDetailsGetImageIndex;
    OnFreeNode := @VSTDetailsFreeNode;
  end;
end;

procedure TCreateRepositoryFrm.bCreateClick(Sender: TObject);
var
  ErrMsg: String;
  RepositoryDetailsFrm: TRepositoryDetailsFrm;
begin
  RepositoryDetailsFrm := TRepositoryDetailsFrm.Create(Self);
  try
    RepositoryDetailsFrm.ShowModal;
    if RepositoryDetailsFrm.ModalResult = mrOk then
    begin
      FRepository.FName := RepositoryDetailsFrm.edName.Text;
      FRepository.FAddress := RepositoryDetailsFrm.edAddress.Text;
      FRepository.FDescription := RepositoryDetailsFrm.mDescription.Text;
      if SD.Execute then
      begin
        if not SaveRepository(SD.FileName, ErrMsg) then
          MessageDlgEx(ErrMsg, mtError, [mbOk], Self);
      end;
    end;
  finally
    RepositoryDetailsFrm.Free;
  end;
end;

procedure TCreateRepositoryFrm.bOpenClick(Sender: TObject);
var
  ErrMsg: String;
begin
  if OD.Execute then
   if not LoadRepository(OD.FileName, ErrMsg) then
     MessageDlgEx(ErrMsg, mtError, [mbOk], Self);
end;

procedure TCreateRepositoryFrm.FormDestroy(Sender: TObject);
begin
  FVSTPackages.Free;
  FVSTDetails.Free;
end;

procedure TCreateRepositoryFrm.pnButtonsResize(Sender: TObject);
begin
  bAdd.Left := (pnButtons.Width - (bAdd.Width + bDelete.Width)) div 2;
  bDelete.Left := bAdd.Left + bAdd.Width + 1;
end;

procedure TCreateRepositoryFrm.EnableDisableButtons(const AEnable: Boolean);
begin
  bOpen.Enabled := AEnable;
  bCreate.Enabled := AEnable;
  bAdd.Enabled := AEnable and FileExists(Trim(FRepository.FPath));
  bDelete.Enabled := AEnable and FileExists(Trim(FRepository.FPath)) and (FVSTPackages.RootNodeCount > 0);
  bCancel.Enabled := AEnable;
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

function TCreateRepositoryFrm.LoadRepository(const AFileName: String;
  out AErrMsg: String): Boolean;
var
  FS: TFileStream;
  procedure ReadString(out AString: String);
  var
    Len: Integer;
  begin
    FS.Read(Len, SizeOf(Integer));
    SetLength(AString, Len div SizeOf(Char));
    FS.Read(Pointer(AString)^, Len);
  end;
begin
  Result := False;
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    try
      ReadString(FRepository.FName);
      ReadString(FRepository.FAddress);
      ReadString(FRepository.FDescription);
      FRepository.FPath := AFileName;
    except
      on E: Exception do
      begin
        AErrMsg := Format(rsCreateRepositoryFrm_Error1, [E.Message]);
        Exit;
      end;
    end;
    Result := FileExists(AppendPathDelim(ExtractFilePath(AFileName)) + cRemoteJSONFile);
    if not Result then
      AErrMsg := Format(rsCreateRepositoryFrm_Error1, [rsCreateRepositoryFrm_Error2])
  finally
    FS.Free;
  end;
end;

function TCreateRepositoryFrm.SaveRepository(const AFileName: String;
    out AErrMsg: String): Boolean;
var
  FS: TFileStream;
  procedure WriteString(const AString: String);
  var
    Len: Integer;
  begin
    Len := Length(AString)*SizeOf(Char);
    FS.Write(Len, SizeOf(Integer));
    FS.Write(Pointer(AString)^, Len);
  end;
begin
  Result := False;
  FS := TFileStream.Create(AFileName, fmCreate or fmOpenWrite or fmShareDenyWrite);
  try
    try
      WriteString(FRepository.FName);
      WriteString(FRepository.FAddress);
      WriteString(FRepository.FDescription);
      Result := True;
    except
      on E: Exception do
      begin
        AErrMsg := Format(rsCreateRepositoryFrm_Error3, [E.Message]);
        Exit;
      end;
    end;
  finally
    FS.Free;
  end;
end;

procedure TCreateRepositoryFrm.VSTPackagesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PMetaData;
begin
  Data := FVSTPackages.GetNodeData(Node);
  case Data^.FDataType of
    0: CellText := FRepository.FName;
    1: CellText := Data^.FName;
  end;
end;

procedure TCreateRepositoryFrm.VSTPackagesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PMetaData;
begin
  Data := FVSTPackages.GetNodeData(Node);
  ImageIndex := Data^.FDataType;
end;

procedure TCreateRepositoryFrm.VSTPackagesHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    with Sender, Treeview do
    begin
      if (SortColumn = NoColumn) or (SortColumn <> Column) then
      begin
        SortColumn    := Column;
        SortDirection := opkman_VirtualTrees.sdAscending;
      end
      else
      begin
        if SortDirection = opkman_VirtualTrees.sdAscending then
          SortDirection := opkman_VirtualTrees.sdDescending
        else
          SortDirection := opkman_VirtualTrees.sdAscending;
      end;
      SortTree(SortColumn, SortDirection, False);
    end;
  end;
end;

procedure TCreateRepositoryFrm.VSTPackagesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PMetaData;
  Data2: PMetaData;
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
begin
  //
end;

procedure TCreateRepositoryFrm.VSTPackagesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PMetaData;
begin
  Data := FVSTPackages.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TCreateRepositoryFrm.VSTDetailsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin

end;

procedure TCreateRepositoryFrm.VSTDetailsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin

end;

procedure TCreateRepositoryFrm.VSTDetailsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin

end;

end.

