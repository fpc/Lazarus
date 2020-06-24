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

unit opkman_categoriesfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Graphics, ExtCtrls, StdCtrls, ButtonPanel, laz.VirtualTrees,
  LCLPlatformDef,
  // OpkMan
  opkman_const, opkman_common, opkman_options, opkman_maindm;
type

  { TCategoriesFrm }

  TCategoriesFrm = class(TForm)
    BP: TButtonPanel;
    lbMessage: TLabel;
    pnMessage: TPanel;
    procedure bOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure lbMessageResize(Sender: TObject);
  private
    FVST: TLazVirtualStringTree;
    FModRes: TModalResult;
    FCategoriesCSV: String;
    FLazCompatibility: String;
    FFPCCompatibility: String;
    FSupportedWidgetSets: String;
    FLineAdded: Boolean;
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    function CheckNode(const AName: String): Boolean;
  public
    procedure SetupControls(const AType: Integer);
    procedure PopulateTree(const AType: Integer);
    property CategoriesCSV: String read FCategoriesCSV write FCategoriesCSV;
    property LazCompatibility: String read FLazCompatibility write FLazCompatibility;
    property FPCCompatibility: String read FFPCCompatibility write FFPCCompatibility;
    property SupportedWidgetSets: String read FSupportedWidgetSets write FSupportedWidgetSets;
  end;

var
  CategoriesFrm: TCategoriesFrm;

implementation

{$R *.lfm}

type
  PData = ^TData;
  TData = record
    FName: string[100];
    FImageIndex: Integer;
    FType: Integer;
  end;

{ TCategoriesFrm }

procedure TCategoriesFrm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if FModRes <> mrNone then
    ModalResult := FModRes;
end;

procedure TCategoriesFrm.bOkClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PData;
begin
  FCategoriesCSV := '';
  FLazCompatibility := '';
  FFPCCompatibility := '';
  FSupportedWidgetSets := '';
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    if FVST.CheckState[Node] = csCheckedNormal then
    begin
      case Data^.FType of
        0,1: begin
                if FCategoriesCSV = '' then
                  FCategoriesCSV := Data^.FName
                else
                  FCategoriesCSV := FCategoriesCSV + ', ' + Data^.FName;
             end;
          2: begin
               if FLazCompatibility = '' then
                 FLazCompatibility := Data^.FName
               else
                 FLazCompatibility := FLazCompatibility + ', ' + Data^.FName;
             end;
          3: begin
               if FFPCCompatibility = '' then
                 FFPCCompatibility := Data^.FName
               else
                 FFPCCompatibility := FFPCCompatibility + ', ' + Data^.FName;
             end;
          4: begin
               if FSupportedWidgetSets = '' then
                 FSupportedWidgetSets := Data^.FName
               else
                 FSupportedWidgetSets := FSupportedWidgetSets + ', ' + Data^.FName;
             end;
      end;
    end;
    Node := FVST.GetNext(Node);
  end;
end;

procedure TCategoriesFrm.FormCreate(Sender: TObject);
begin
  if not Options.UseDefaultTheme then
    Self.Color := clBtnFace;
  FVST := TLazVirtualStringTree.Create(nil);
  with FVST do
  begin
    Parent := Self;
    Align := alClient;
    Anchors := [akLeft, akTop, akRight];
    Images := MainDM.Images;
    if not Options.UseDefaultTheme then
      Color := clBtnFace;
    DefaultNodeHeight := Scale96ToForm(25);
    Indent := 0;
    TabOrder := 1;
    DefaultText := '';
    Header.AutoSizeIndex := 0;
    Header.Height := Scale96ToForm(25);
    Colors.BorderColor := clBlack;
    BorderSpacing.Top := Scale96ToForm(5);
    BorderSpacing.Left := Scale96ToForm(10);
    BorderSpacing.Right := Scale96ToForm(10);
    with Header.Columns.Add do begin
      Position := 0;
      Width := 250;
      Text := 'CategorieName';
    end;
    Header.Options := [hoAutoResize, hoColumnResize, hoRestrictDrag, hoShowSortGlyphs, hoAutoSpring];
    Header.SortColumn := 0;
    TabOrder := 1;
    TreeOptions.MiscOptions := [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toCheckSupport];
    TreeOptions.PaintOptions := [toHideFocusRect, toPopupMode, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages];
    TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
    TreeOptions.AutoOptions := [toAutoTristateTracking];
    OnGetText := @VSTGetText;
    OnGetImageIndex := @VSTGetImageIndex;
    OnCompareNodes := @VSTCompareNodes;
    OnFreeNode := @VSTFreeNode;
  end;
  FVST.NodeDataSize := SizeOf(TData);
end;

procedure TCategoriesFrm.FormDestroy(Sender: TObject);
begin
  FVST.Clear;
  FVST.Free;
end;

procedure TCategoriesFrm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    FModRes := mrYes;
    Close;
  end
  else if Key = #27 then
  begin
    FModRes := mrNo;
    Close;
  end;
end;

procedure TCategoriesFrm.lbMessageResize(Sender: TObject);
begin
  pnMessage.Height := lbMessage.Top + lbMessage.Height + 5;
end;

procedure TCategoriesFrm.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
    CellText := Data^.FName;
end;

procedure TCategoriesFrm.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
    ImageIndex := Data^.FImageIndex;
end;

procedure TCategoriesFrm.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PData;
  Data2: PData;
  Str1, Str2: String;
  Int1, Int2: Integer;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  if Column = 0 then
  begin
    if Data1^.FType = Data2^.FType then
    begin
      Str1 := Data1^.FName;
      Str1 := StringReplace(Str1, '.', '', [rfReplaceAll]);
      Int1 := StrToIntDef(Str1, -1);
      Str2 := Data2^.FName;
      Str2 := StringReplace(Str1, '.', '', [rfReplaceAll]);
      Int2 := StrToIntDef(Str2, -1);
      if (Int1 <> -1) and (Int2 <> -1) then
        Result := Int2 - Int1
      else
        Result := CompareText(Data1^.FName, Data2^.FName)
    end
    else if Data1^.FType > Data2^.FType then
      Result := 1
    else if Data1^.FType < Data2^.FType then
      Result := -1
  end;
end;

procedure TCategoriesFrm.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TCategoriesFrm.SetupControls(const AType: Integer);
begin
  FModRes := mrNone;
  BP.OKButton.Caption := rsCategoriesFrm_bYes_Caption;
  BP.CancelButton.Caption := rsCategoriesFrm_bCancel_Caption;
  case AType of
    1: Caption := rsCategoriesFrm_Caption1;
    2: Caption := rsCategoriesFrm_Caption2;
    3: Caption := rsCategoriesFrm_Caption3;
    4: Caption := rsCategoriesFrm_Caption4;
  end;
  lbMessage.Caption := rsCategoriesFrm_lbMessage_Caption;
end;

function TCategoriesFrm.CheckNode(const AName: String): Boolean;
var
  Node: PVirtualNode;
  Data: PData;
begin
  Result := False;
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    if UpperCase(Data^.FName) = UpperCase(AName) then
    begin
      FVST.CheckState[Node] := csCheckedNormal;
      Result := True;
      Break;
    end;
    Node := FVST.GetNext(Node);
  end;
end;

procedure TCategoriesFrm.PopulateTree(const AType: Integer);
var
  I: Integer;
  Node: PVirtualNode;
  Data: PData;
  SL: TStringList;
  LCLPlatform: TLCLPlatform;
begin
  FLineAdded := True;
  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.StrictDelimiter := True;
    case AType of
      1: begin
           for I := 0 to MaxCategories - 1 do
           begin
             Node := FVST.AddChild(nil);
             Node^.CheckType := ctTriStateCheckBox;
             Data := FVST.GetNodeData(Node);
             Data^.FName := Categories[I];
             Data^.FImageIndex := -1;
             if UpperCase(CategoriesEng[I]) = 'OTHER' then
               Data^.FType := 1
             else
               Data^.FType := 0;
           end;
           SL.DelimitedText := FCategoriesCSV;
         end;
      2: begin
            for I := 0 to MaxLazVersions - 1 do
            begin
              Node := FVST.AddChild(nil);
              Node^.CheckType := ctTriStateCheckBox;
              Data := FVST.GetNodeData(Node);
              Data^.FName := LazVersions[I];
              Data^.FImageIndex := -1;
              Data^.FType := 2;
            end;
           SL.DelimitedText := FLazCompatibility;
         end;
      3: begin
           for I := 0 to MaxFPCVersions - 1 do
           begin
             Node := FVST.AddChild(nil);
             Node^.CheckType := ctTriStateCheckBox;
             Data := FVST.GetNodeData(Node);
             Data^.FName := FPCVersions[I];
             Data^.FImageIndex := -1;
             Data^.FType := 3;
           end;
           SL.DelimitedText := FFPCCompatibility;
         end;
      4: begin
            for LCLPlatform := Low(TLCLPlatform) to High(TLCLPlatform) do
            begin
              Node := FVST.AddChild(nil);
              Node^.CheckType := ctTriStateCheckBox;
              Data := FVST.GetNodeData(Node);
              Data^.FName := LCLPlatformDisplayNames[LCLPlatform];
              Data^.FImageIndex := -1;
              Data^.FType := 4;
            end;
           SL.DelimitedText := FSupportedWidgetSets;
         end;
    end;
    FVST.SortTree(0, laz.VirtualTrees.sdAscending);
    for I := 0 to SL.Count - 1 do
      CheckNode(Trim(SL.Strings[I]));
  finally
    SL.Free;
  end;
end;


end.

