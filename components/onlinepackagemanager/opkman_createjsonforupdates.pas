unit opkman_createjsonforupdates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, LCLIntf, MaskEdit, fpjson,
  opkman_VirtualTrees, opkman_serializablepackages;

type

  { TCreateJSONForUpdatesFrm }

  TCreateJSONForUpdatesFrm = class(TForm)
    bClose: TButton;
    bCreate: TButton;
    bTest: TButton;
    bHelp: TButton;
    edLinkToZip: TEdit;
    imTree: TImageList;
    lbLinkToZip: TLabel;
    pnTop: TPanel;
    pnButtons: TPanel;
    SD: TSaveDialog;
    procedure bCreateClick(Sender: TObject);
    procedure bTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FVST: TVirtualStringTree;
    FPackage: TPackage;
    FSortCol: Integer;
    FSortDir: opkman_VirtualTrees.TSortDirection;
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
  public
    procedure PopluateTree;
  end;

var
  CreateJSONForUpdatesFrm: TCreateJSONForUpdatesFrm;

implementation
uses opkman_const, opkman_common, opkman_updates;
{$R *.lfm}

{ TCreateJSONForUpdatesFrm }

type
  PData = ^TData;
  TData = record
    FName: string;
    FVersion: String;
    FForceNotify: Boolean;
    FInternalVersion: Integer;
    FImageIndex: Integer;
  end;

procedure TCreateJSONForUpdatesFrm.FormCreate(Sender: TObject);
begin
  Caption := rsCreateJSONForUpdatesFrm_Caption;
  lbLinkToZip.Caption := rsCreateJSONForUpdatesFrm_lbLinkToZip_Caption;
  bTest.Caption := rsCreateJSONForUpdatesFrm_bTest_Caption;
  bHelp.Caption := rsCreateJSONForUpdatesFrm_bHelp_Caption;
  bCreate.Caption := rsCreateJSONForUpdatesFrm_bCreate_Caption;
  bClose.Caption := rsCreateJSONForUpdatesFrm_bClose_Caption;

  FVST := TVirtualStringTree.Create(nil);
   with FVST do
   begin
     Parent := Self;
     Align := alClient;
     Anchors := [akLeft, akTop, akRight];
     Images := imTree;
     Color := clBtnFace;
     DefaultNodeHeight := 25;
     Indent := 0;
     TabOrder := 1;
     DefaultText := '';
     Header.AutoSizeIndex := 0;
     Header.Height := 25;
     Colors.BorderColor := clBlack;
     BorderSpacing.Top := 15;
     BorderSpacing.Left := 15;
     BorderSpacing.Right := 15;
     BorderSpacing.Bottom := 0;
     with Header.Columns.Add do
     begin
       Position := 0;
       Width := 250;
       Text := rsCreateJSONForUpdatesFrm_Column0_Text;
     end;
     with Header.Columns.Add do
     begin
       Position := 1;
       Width := 75;
       Text := rsCreateJSONForUpdatesFrm_Column1_Text;
       Alignment := taCenter;
     end;
     with Header.Columns.Add do
     begin
       Position := 2;
       Width := 100;
       Text := rsCreateJSONForUpdatesFrm_Column2_Text;
       Alignment := taCenter;
       Options := Options - [coVisible];
     end;
     with Header.Columns.Add do
     begin
       Position := 3;
       Width := 100;
       Text := rsCreateJSONForUpdatesFrm_Column3_Text;
       Alignment := taCenter;
       Options := Options - [coVisible];
     end;
     Header.Options := [hoAutoResize, hoRestrictDrag, hoShowSortGlyphs, hoAutoSpring, hoVisible];
     Header.SortColumn := 0;
     TabOrder := 2;
     TreeOptions.MiscOptions := [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toCheckSupport, toEditable];
     TreeOptions.PaintOptions := [toHideFocusRect, toPopupMode, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages];
     TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
     TreeOptions.AutoOptions := [toAutoTristateTracking];
     OnGetText := @VSTGetText;
     OnGetImageIndex := @VSTGetImageIndex;
     OnCompareNodes := @VSTCompareNodes;
     OnHeaderClick := @VSTHeaderClick;
     OnFreeNode := @VSTFreeNode;
   end;
   FVST.NodeDataSize := SizeOf(TData);
end;

procedure TCreateJSONForUpdatesFrm.FormDestroy(Sender: TObject);
begin
  FVST.Free;
end;

procedure TCreateJSONForUpdatesFrm.bTestClick(Sender: TObject);
begin
  if Trim(edLinkToZip.Text) = '' then
  begin
    MessageDlgEx(rsCreateJSONForUpdatesFrm_Message2, mtInformation, [mbOk], Self);
    edLinkToZip.SetFocus;
    Exit;
  end;
  OpenURL(edLinkToZip.Text);
end;

procedure TCreateJSONForUpdatesFrm.bCreateClick(Sender: TObject);
var
  UpdatePackage: TUpdatePackage;
  UpdatePackageFiles: TUpdatePackageFiles;
  JSON: TJSONStringType;
  Ms: TMemoryStream;
  Node: PVirtualNode;
  Data: PData;
begin
  if FVST.CheckedCount = 0 then
  begin
    MessageDlgEx(rsCreateJSONForUpdatesFrm_Message3, mtInformation, [mbOk], Self);
    Exit;
  end;

  if FPackage.DisplayName <> '' then
    SD.FileName := 'update_' + FPackage.DisplayName
  else
    SD.FileName := 'update_' + FPackage.Name;
  if SD.Execute then
  begin
    UpdatePackage := TUpdatePackage.Create;
    try
      UpdatePackage.UpdatePackageData.Name := FPackage.Name;
      UpdatePackage.UpdatePackageData.DownloadZipURL := edLinkToZip.Text;

      Node := FVST.GetFirst;
      while Assigned(Node) do
      begin
        if FVST.CheckState[Node] = csCheckedNormal then
        begin
          Data := FVST.GetNodeData(Node);
          UpdatePackageFiles := TUpdatePackageFiles(UpdatePackage.UpdatePackageFiles.Add);
          UpdatePackageFiles.Name := Data^.FName;
          UpdatePackageFiles.Version := Data^.FVersion;
          UpdatePackageFiles.ForceNotify := Data^.FForceNotify;
          UpdatePackageFiles.InternalVersion := Data^.FInternalVersion;
        end;
        Node := FVST.GetNext(Node);
      end;
      JSON := '';
      if UpdatePackage.SaveToJSON(JSON) then
      begin
        JSON := StringReplace(JSON, '\/', '/', [rfReplaceAll]);
        Ms := TMemoryStream.Create;
        try
          Ms.Write(Pointer(JSON)^, Length(JSON));
          Ms.Position := 0;
          Ms.SaveToFile(SD.FileName);
        finally
          MS.Free;
        end;
      end;
    finally
      UpdatePackage.Free;
    end;
  end;
end;

procedure TCreateJSONForUpdatesFrm.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  case Column of
    0: CellText := Data^.FName;
    1: CellText := Data^.FVersion;
    2: CellText := BoolToStr(Data^.FForceNotify, True);
    3: CellText := IntToStr(Data^.FInternalVersion);
  end;
end;

procedure TCreateJSONForUpdatesFrm.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
    ImageIndex := Data^.FImageIndex;
end;

procedure TCreateJSONForUpdatesFrm.VSTCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PData;
  Data2: PData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    0: Result := CompareText(Data1^.FName, Data2^.FName);
    1: Result := CompareText(Data1^.FVersion, Data2^.FVersion);
  end;
end;

procedure TCreateJSONForUpdatesFrm.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  Finalize(Data^)
end;

procedure TCreateJSONForUpdatesFrm.VSTHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Column > 1 then
    Exit;
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
        FSortDir := SortDirection;
      end;
      SortTree(SortColumn, SortDirection, False);
      FSortCol := Sender.SortColumn;
    end;
  end;

end;

procedure TCreateJSONForUpdatesFrm.PopluateTree;
var
  I, J: Integer;
  Node: PVirtualNode;
  Data: PData;
  PackageFile: TPackageFile;
begin
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    FPackage := SerializablePackages.Items[I];
    if FPackage.Checked then
    begin
      Caption := Caption + ' "' + FPackage.DisplayName +'"';
      for J := 0 to FPackage.PackageFiles.Count - 1 do
      begin
        PackageFile := TPackageFile(FPackage.PackageFiles.Items[J]);
        if PackageFile.Checked then
        begin
          Node := FVST.AddChild(nil);
          Node^.CheckType := ctTriStateCheckBox;
          FVST.CheckState[Node] := csCheckedNormal;
          Data := FVST.GetNodeData(Node);
          Data^.FName := PackageFile.Name;
          Data^.FVersion := PackageFile.VersionAsString;
          Data^.FForceNotify := False;
          Data^.FInternalVersion := 1;
          Data^.FImageIndex := 1;
        end;
      end;
      Break;
    end;
  end;
end;


end.

