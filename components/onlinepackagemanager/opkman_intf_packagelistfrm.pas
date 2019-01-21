unit opkman_intf_packagelistfrm;

{$mode objfpc}{$H+}

interface

uses
   SysUtils, Classes, laz.VirtualTrees,
  // LCL
  Forms, Controls, Buttons, Graphics, ExtCtrls, StdCtrls, LCLType, ButtonPanel,
  Menus,
  //IDEIntf
  PackageIntf,
  // OpkMan
  opkman_const, opkman_serializablepackages, opkman_options,
  opkman_Common, opkman_maindm;

type

  { TIntfPackageListFrm }

  TIntfPackageListFrm = class(TForm)
    ButtonPanel1: TButtonPanel;
    pnExpCol: TPanel;
    pnInfo: TPanel;
    spCollapse: TSpeedButton;
    spExpand: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure spCollapseClick(Sender: TObject);
    procedure spExpandClick(Sender: TObject);
  private
    FVST: TLazVirtualStringTree;
    FSortCol: Integer;
    FSortDir: laz.VirtualTrees.TSortDirection;
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    procedure PopulateTree(APkgList: TList);
    function IsLazarusPackageChecked(AName: String): Boolean;
  end;

var
  IntfPackageListFrm: TIntfPackageListFrm;

implementation

{$R *.lfm}

type
  PData = ^TData;
  TData = record
    DataType: Integer;
    LazarusPackageName: String;
    Description: String;
    Author: String;
    LazCompatibility: String;
    FPCCompatibility: String;
    SupportedWidgetSet: String;
    PackageType: TLazPackageType;
    License: String;
    Dependencies: String;
    Version: String;
    Button: TSpeedButton;
  end;

const
  IMAGE_INDEX_MAP: array[0..9] of Integer = (
    IMG_PKG_FILE, IMG_DESCRIPTION, IMG_AUTHOR,                   // 0..2
    IMG_LAZ_COMPATIBILITY, IMG_FPC_COMPATIBILITY, IMG_WIDGETSET, // 3..5
    IMG_PKG_TYPE, IMG_LICENSE, IMG_DEPENDENCIES,                 // 6..8
    IMG_FILE_VERSION);                                           // 9

{ TIntfPackageListFrm }

procedure TIntfPackageListFrm.FormCreate(Sender: TObject);
begin
  Caption := rsOPMIntfPackageListFrm_Caption;
  pnInfo.Caption := '  ' + rsOPMIntfPackageListFrm_pnInfo;
  if not Options.UseDefaultTheme then
    Self.Color := clBtnFace;
  spExpand.Caption := '';
  spExpand.Images := MainDM.Images;
  spExpand.ImageIndex := IMG_EXPAND;
  spCollapse.Caption := '';
  spCollapse.Images := MainDM.Images;
  spCollapse.ImageIndex := IMG_COLLAPSE;
  FVST := TLazVirtualStringTree.Create(nil);
  with FVST do
  begin
    Parent := Self;
    Align := alClient;
    Anchors := [akLeft, akTop, akRight];
    Images := MainDM.Images;
    DefaultNodeHeight := Scale96ToForm(25);
    Indent := Scale96ToForm(22);
    TabOrder := 0;
    DefaultText := '';
    Header.AutoSizeIndex := 1;
    Header.SortColumn := 0;
    Header.Height := Scale96ToForm(25);
    Colors.DisabledColor := clBlack;
    with Header.Columns.Add do
    begin
      Position := 0;
      Width := Scale96ToForm(200);
      Text := rsOPMIntfPackageListFrm_VSTHeaderColumn_LazarusPackage;
    end;
    with Header.Columns.Add do
    begin
      Position := 1;
      Width := Scale96ToForm(200);
      Text := rsOPMIntfPackageListFrm_VSTHeaderColumn_Data;
    end;
    Header.Options := [hoAutoResize, hoColumnResize, hoRestrictDrag, hoShowSortGlyphs, hoVisible];
    TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
    TreeOptions.PaintOptions := [toHideFocusRect, toShowRoot, toPopupMode, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages];
    TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
    TreeOptions.AutoOptions := [toAutoTristateTracking];
    HintMode := hmHint;
    ShowHint := True;
    OnGetText := @VSTGetText;
    OnGetImageIndex := @VSTGetImageIndex;
    OnCompareNodes := @VSTCompareNodes;
    OnHeaderClick := @VSTHeaderClick;
    OnGetHint := @VSTGetHint;
    OnFreeNode := @VSTFreeNode;
  end;
  FVST.NodeDataSize := SizeOf(TData);
end;

procedure TIntfPackageListFrm.FormDestroy(Sender: TObject);
begin
  FVST.Free;
end;

procedure TIntfPackageListFrm.spCollapseClick(Sender: TObject);
begin
  FVST.FullCollapse;
end;

procedure TIntfPackageListFrm.spExpandClick(Sender: TObject);
begin
  FVST.FullExpand;
end;

procedure TIntfPackageListFrm.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  case Column of
    0: case Data^.DataType of
         0: CellText := Data^.LazarusPackageName;
         1: CellText := rsMainFrm_VSTText_Description;
         2: CellText := rsMainFrm_VSTText_Author;
         3: CellText := rsMainFrm_VSTText_LazCompatibility;
         4: CellText := rsMainFrm_VSTText_FPCCompatibility;
         5: CellText := rsMainFrm_VSTText_SupportedWidgetsets;
         6: CellText := rsMainFrm_VSTText_Packagetype;
         7: CellText := rsMainFrm_VSTText_License;
         8: CellText := rsMainFrm_VSTText_Dependecies;
         9: CellText := rsMainFrm_VSTText_Version;
       end;
    1: case Data^.DataType of
         0: CellText := '';
         1: CellText := StringReplace(Data^.Description, sLineBreak, ' ', [rfReplaceAll]);
         2: CellText := Data^.Author;
         3: CellText := Data^.LazCompatibility;
         4: CellText := Data^.FPCCompatibility;
         5: CellText := Data^.SupportedWidgetSet;
         6: CellText := GetPackageTypeString(Data^.PackageType);
         7: CellText := StringReplace(Data^.License, sLineBreak, ' ', [rfReplaceAll]);
         8: CellText := Data^.Dependencies;
         9: CellText := Data^.Version;
       end;
  end;
end;

procedure TIntfPackageListFrm.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
    ImageIndex := IMAGE_INDEX_MAP[Data^.DataType];
end;

procedure TIntfPackageListFrm.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PData;
  Data2: PData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  if Column = 0 then
    Result := CompareText(Data1^.LazarusPackageName, Data2^.LazarusPackageName);
end;

procedure TIntfPackageListFrm.VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if (HitInfo.Column <> 0) and (HitInfo.Column <> 1) and (HitInfo.Column <> 3) then
    Exit;
  if HitInfo.Button = mbLeft then
  begin
    with Sender, Treeview do
    begin
      if (SortColumn = NoColumn) or (SortColumn <> HitInfo.Column) then
      begin
        SortColumn    := HitInfo.Column;
        SortDirection := laz.VirtualTrees.sdAscending;
      end
      else
      begin
        if SortDirection = laz.VirtualTrees.sdAscending then
          SortDirection := laz.VirtualTrees.sdDescending
        else
          SortDirection := laz.VirtualTrees.sdAscending;
        FSortDir := SortDirection;
      end;
      SortTree(SortColumn, SortDirection, False);
      FSortCol := Sender.SortColumn;
    end;
  end;
end;

procedure TIntfPackageListFrm.VSTGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if (Column = 0) then
    Exit;

  LineBreakStyle := hlbForceSingleLine;
  case Data^.DataType of
    1: HintText := Data^.Description;
    7: HintText := Data^.License;
   else
     HintText := '';
  end;
end;

procedure TIntfPackageListFrm.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TIntfPackageListFrm.PopulateTree(APkgList: TList);
var
  I: Integer;
  LazarusPkg: TLazarusPackage;
  RootNode, Node: PVirtualNode;
  RootData, Data: PData;
begin
  for I := 0 to APkgList.Count - 1 do
  begin
    LazarusPkg := TLazarusPackage(APkgList.Items[I]);
    //name
    RootNode := FVST.AddChild(nil);
    RootNode^.CheckType := ctTriStateCheckBox;
    RootNode^.CheckState := csCheckedNormal;
    RootData := FVST.GetNodeData(RootNode);
    RootData^.LazarusPackageName := StringReplace(LazarusPkg.Name, '.lpk', '', [rfIgnoreCase, rfReplaceAll]);
    RootData^.DataType := 0;

    //description
    Node := FVST.AddChild(RootNode);
    Data := FVST.GetNodeData(Node);
    Data^.DataType :=  1;
    Data^.Description := LazarusPkg.Description;

    //author
    Node := FVST.AddChild(RootNode);
    Data := FVST.GetNodeData(Node);
    Data^.DataType :=  2;
    Data^.Author := LazarusPkg.Author;

    //LazCompatibility
    Node := FVST.AddChild(RootNode);
    Data := FVST.GetNodeData(Node);
    Data^.DataType :=  3;
    Data^.LazCompatibility := LazarusPkg.LazCompatibility;

    //FPCCompatibility
    Node := FVST.AddChild(RootNode);
    Data := FVST.GetNodeData(Node);
    Data^.DataType :=  4;
    Data^.FPCCompatibility := LazarusPkg.FPCCompatibility;

    //SupportedWidgetSet
    Node := FVST.AddChild(RootNode);
    Data := FVST.GetNodeData(Node);
    Data^.DataType :=  5;
    Data^.SupportedWidgetSet := LazarusPkg.SupportedWidgetSet;

    //FPCCompatibility
    Node := FVST.AddChild(RootNode);
    Data := FVST.GetNodeData(Node);
    Data^.DataType :=  6;
    Data^.PackageType := LazarusPkg.PackageType;

    //License
    Node := FVST.AddChild(RootNode);
    Data := FVST.GetNodeData(Node);
    Data^.DataType :=  7;
    Data^.License := LazarusPkg.License;

    //Dependencies
    Node := FVST.AddChild(RootNode);
    Data := FVST.GetNodeData(Node);
    Data^.DataType :=  8;
    Data^.Dependencies := LazarusPkg.DependenciesAsString;

    //FPCCompatibility
    Node := FVST.AddChild(RootNode);
    Data := FVST.GetNodeData(Node);
    Data^.DataType :=  9;
    Data^.Version := LazarusPkg.VersionAsString;
  end;
  FVST.SortTree(0, sdAscending);
  if FVST.RootNodeCount = 1 then
    FVST.FullExpand
  else
    FVST.FullCollapse;
end;

function TIntfPackageListFrm.IsLazarusPackageChecked(AName: String): Boolean;
var
  Node: PVirtualNode;
  Data: PData;
begin
  Result := False;
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    if UpperCase(Data^.LazarusPackageName + '.lpk') = UpperCase(AName) then
    begin
      Result := Node^.CheckState = csCheckedNormal;
      Break;
    end;
    Node := FVST.GetNextSibling(Node);
  end;
end;

end.

