unit opkman_intf_packagelistfrm;

{$mode objfpc}{$H+}

interface

uses
   SysUtils, Classes, VirtualTrees,
  // LCL
  Forms, Controls, Buttons, Graphics, ExtCtrls, StdCtrls, LCLType, ButtonPanel,
  Menus,
  //IDEIntf
  PackageIntf,
  // OpkMan
  opkman_const, opkman_serializablepackages, opkman_options,
  opkman_Common;

type

  { TIntfPackageListFrm }

  TIntfPackageListFrm = class(TForm)
    ButtonPanel1: TButtonPanel;
    imSB: TImageList;
    imTree: TImageList;
    pnExpCol: TPanel;
    pnInfo: TPanel;
    spCollapse: TSpeedButton;
    spExpand: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure spCollapseClick(Sender: TObject);
    procedure spExpandClick(Sender: TObject);
  private
    FVST: TVirtualStringTree;
    FSortCol: Integer;
    FSortDir: VirtualTrees.TSortDirection;
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

{ TIntfPackageListFrm }

procedure TIntfPackageListFrm.FormCreate(Sender: TObject);
begin
  Caption := rsOPMIntfPackageListFrm_Caption;
  pnInfo.Caption := '  ' + rsOPMIntfPackageListFrm_pnInfo;
  if not Options.UseDefaultTheme then
    Self.Color := clBtnFace;
  FVST := TVirtualStringTree.Create(nil);
  with FVST do
  begin
    Parent := Self;
    Align := alClient;
    Anchors := [akLeft, akTop, akRight];
    Images := ImTree;
    DefaultNodeHeight := MulDiv(25, Screen.PixelsPerInch, 96);
    Indent := 22;
    TabOrder := 0;
    DefaultText := '';
    Header.AutoSizeIndex := 1;
    Header.SortColumn := 0;
    Header.Height := MulDiv(25, Screen.PixelsPerInch, 96);
    Colors.DisabledColor := clBlack;
    with Header.Columns.Add do
    begin
      Position := 0;
      Width := MulDiv(200, Screen.PixelsPerInch, 96);
      Text := rsOPMIntfPackageListFrm_VSTHeaderColumn_LazarusPackage;
    end;
    with Header.Columns.Add do
    begin
      Position := 1;
      Width := MulDiv(200, Screen.PixelsPerInch, 96);
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
    ImageIndex := Data^.DataType
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
        SortDirection := VirtualTrees.sdAscending;
      end
      else
      begin
        if SortDirection = VirtualTrees.sdAscending then
          SortDirection := VirtualTrees.sdDescending
        else
          SortDirection := VirtualTrees.sdAscending;
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

