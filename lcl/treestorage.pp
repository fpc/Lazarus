{
 /***************************************************************************
                               treestorage.pp
                               ----------
  Provides methods to save and load a TTreeView.


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit TreeStorage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  Laz2_Dom, Laz2_XmlWrite, Laz2_XmlRead,
  LazLoggerBase;

type
  TTreeItemStorageOption = (tsoExpanded, tsoSelected, tsoFocused, tsoVisible, tsoEnabled,
                            tsoImageIndex, tsoSelectedIndex, tsoStateIndex, tsoOverlayIndex);
  TTreeItemStorageOptions = set of TTreeItemStorageOption;

  TTreeStorageCallback = procedure (ATreeNode: TTreeNode; ADomNode: TDomNode) of Object;

const
  TreeItemStorageDefaultOptions = [tsoExpanded, tsoSelected, tsoFocused, tsoVisible, tsoEnabled,
                                   tsoImageIndex, tsoSelectedIndex, tsoStateIndex, tsoOverlayIndex];

procedure TreeSaveToXML(Tree: TCustomTreeView; Fn: String; Options: TTreeItemStorageOptions = TreeItemStorageDefaultOptions; OnCallback: TTreeStorageCallBack = nil);
procedure TreeSaveToXML(Tree: TCustomTreeView; St: TStream; Options: TTreeItemStorageOptions = TreeItemStorageDefaultOptions; OnCallback: TTreeStorageCallBack = nil);
procedure TreeLoadFromXML(Tree: TCustomTreeView; const Fn: String; Options: TTreeItemStorageOptions = TreeItemStorageDefaultOptions; OnCallback: TTreeStorageCallBack = nil);
procedure TreeLoadFromXML(Tree: TCustomTreeView; St: TStream; Options: TTreeItemStorageOptions = TreeItemStorageDefaultOptions; OnCallback: TTreeStorageCallBack = nil);


implementation

const
  attrCaption = 'Caption';
  attrEnabled = 'Enabled';
  attrVisible = 'Visible';
  attrFocused = 'Focused';
  attrSelected = 'Selected';
  attrExpanded = 'Expanded';
  attrImageIndex = 'ImageIndex';
  attrSelectedIndex = 'SelectedIndex';
  attrStateIndex = 'StateIndex';
  attrOverlayIndex = 'OverlayIndex';
  sTrue = 'True';
  sFalse = 'False';
  RootNodeName = 'Treeview';
  ItemsNodeName = 'Items';

function DomStrToBoolDef(const S: DomString; Def: Boolean): Boolean;
begin
  if (CompareText(S, sTrue) = 0) then
    Result := True
  else
  begin
    if (CompareText(S, sFalse) = 0) then
      Result := False
    else
      Result := Def;
  end;
end;

procedure TreeSaveToXML(Tree: TCustomTreeView; Fn: String; Options: TTreeItemStorageOptions; OnCallback: TTreeStorageCallBack);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(Fn, fmCreate);
  try
    TreeSaveToXML(Tree, FS, Options);
  finally
    FS.Free;
  end;
end;

procedure TreeSaveToXML(Tree: TCustomTreeView; St: TStream; Options: TTreeItemStorageOptions; OnCallback: TTreeStorageCallBack);
var
  Doc: TXMLDocument;
  RootNode: TDOMElement;
  Node, ItemsNode: TDOMNode;

  Procedure WriteNode(ATreeNode: TTreeNode; ParentDomNode: TDomNode);
  var
    Child: TTreeNode;
    CurrNode: TDOMNode;
  begin
    //CurrNode := ParentDomNode.AppendChild(Doc.CreateElement(ATreeNode.Text));
    CurrNode := ParentDomNode.AppendChild(Doc.CreateElement('Node'));
    TDomElement(CurrNode).SetAttribute(attrCaption, ATreeNode.Text);
    if (tsoExpanded in Options) and ATreeNode.Expanded then
      TDomElement(CurrNode).SetAttribute(attrExpanded,sTrue);
    if (tsoVisible in Options) and not ATreeNode.Visible then
      TDomElement(CurrNode).SetAttribute(attrVisible,sFalse);
    if (tsoEnabled in Options) and not ATreeNode.Enabled then
      TDomElement(CurrNode).SetAttribute(attrEnabled,sFalse);
    if (tsoFocused in Options) and ATreeNode.Focused then
      TDomElement(CurrNode).SetAttribute(attrFocused,sTrue);
    if (tsoSelected in Options) and ATreeNode.Selected then
      TDomElement(CurrNode).SetAttribute(attrSelected,sTrue);

    if (tsoImageIndex in Options) and (ATreeNode.ImageIndex > -1) then
      TDomElement(CurrNode).SetAttribute(attrImageIndex,IntToStr(ATreeNode.ImageIndex));  //ImageIndex = TImageIndex = type Integer, so .ToString does not work
    if (tsoSelectedIndex in Options) and (ATreeNode.SelectedIndex > -1) then
      TDomElement(CurrNode).SetAttribute(attrSelectedIndex,ATreeNode.SelectedIndex.ToString);
    if (tsoStateIndex in Options) and (ATreeNode.StateIndex > -1) then
      TDomElement(CurrNode).SetAttribute(attrStateIndex,ATreeNode.StateIndex.ToString);
    if (tsoOverlayIndex in Options) and (ATreeNode.OverlayIndex > -1) then
      TDomElement(CurrNode).SetAttribute(attrOverlayIndex,ATreeNode.OverlayIndex.ToString);

    if Assigned(OnCallback) then
      OnCallback(ATreeNode, CurrNode);

    Child := ATreeNode.GetFirstChild;
    while Assigned(Child) do
    begin
      WriteNode(Child, CurrNode);
      Child := Child.GetNextSibling;
    end;
  end;

  Procedure IterateItems;
  var
    N: TTreeNode;
  begin
    N := Tree.Items[0];
    while Assigned(N) do
    begin
      WriteNode(N, ItemsNode);
      N := N.GetNextSibling;
    end;
  end;

begin
  Doc := TXMLDocument.Create;
  try
    Doc.Encoding := 'UTF-8';
    RootNode := Doc.CreateElement(RootNodeName);
    Node := Doc.AppendChild(RootNode);
    ItemsNode := Node.Appendchild(Doc.CreateElement(ItemsNodeName));
    if (Tree.Items.Count > 0) then
      IterateItems;
    WriteXMlFile(Doc, St);
  finally
    Doc.Free;
  end;
end;

procedure TreeLoadFromXML(Tree: TCustomTreeView; const Fn: String; Options: TTreeItemStorageOptions; OnCallback: TTreeStorageCallBack);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(Fn, fmOpenRead or fmShareDenyWrite);
  try
    TreeLoadFromXML(Tree, FS, Options);
  finally
    FS.Free;
  end;
end;



procedure TreeLoadFromXML(Tree: TCustomTreeView; St: TStream; Options: TTreeItemStorageOptions; OnCallback: TTreeStorageCallBack);
var
  Doc: TXMLDocument;
  DomNode: TDOMNode;
  ExpandedList: TFPList;

  function TryGetAttribute(ADomNode: TDOMNode; Attr: DOMString; out Value: DOMString): Boolean;
  begin
    Value := (TDOMElement(AdomNode).GetAttribute(Attr));
    Result := (Value <> '');
  end;

  function GetAttributeDef(ADomNode: TDOMNode; Attr: DOMString; Def: Integer): Integer;
  var
    ValueStr: DOMString;
  begin
    if TryGetAttribute(ADomNode, Attr, ValueStr) then
      Result := StrToIntDef(ValueStr, Def)
    else
      Result := Def;
  end;

  function GetAttributeDef(ADomNode: TDOMNode; Attr: DOMString; Def: Boolean): Boolean;
  var
    ValueStr: DOMString;
  begin
    if TryGetAttribute(ADomNode, Attr, ValueStr) then
      Result := DomStrToBoolDef(ValueStr, Def)
    else
      Result := Def;
  end;

  procedure ProcessNode(ADomNode: TDOMNode; TreeNode: TTreeNode);
  var
    CurrNode: TDOMNode;
    MustExpand: Boolean;
  begin
    if ADomNode = nil then Exit; // Stops if reached a leaf
    //debugln('TreeLoadFromXML.ProcessNode: Caption=',TDOMElement(ADomNode).GetAttribute(attrCaption));
    TreeNode := Tree.Items.AddChild(TreeNode, TDOMElement(ADomNode).GetAttribute(attrCaption));

    MustExpand := GetAttributeDef(ADomNode, attrExpanded, True);
    //Enabled, Visible, Focused, Selected
    if (tsoEnabled in Options) then TreeNode.Enabled := GetAttributeDef(ADomNode, attrEnabled, True);
    if (tsoVisible in Options) then TreeNode.Visible := GetAttributeDef(ADomNode, attrVisible, True);
    if (tsoFocused in Options) then TreeNode.Focused := GetAttributeDef(ADomNode, attrFocused, False);
    if (tsoSelected in Options) then TreeNode.Selected := GetAttributeDef(ADomNode, attrSelected, False);
    //ImageIndex, SelectedIndex, StateIndex, OverlayIndex
    if (tsoImageIndex in Options) then TreeNode.ImageIndex := GetAttributeDef(ADomNode, attrImageIndex, -1);
    if (tsoSelectedIndex in Options) then TreeNode.SelectedIndex := GetAttributeDef(ADomNode, attrSelectedIndex, -1);
    if (tsoStateIndex in Options) then TreeNode.StateIndex :=  GetAttributeDef(ADomNode, attrStateIndex, -1);
    if (tsoOverlayIndex in Options) then TreeNode.OverlayIndex := GetAttributeDef(ADomNode, attrOverlayIndex, -1);

    if Assigned(OnCallback) then
      OnCallback(TreeNode, ADomNode);

    if MustExpand then
      ExpandedList.Add(TreeNode);
    CurrNode := ADomNode.FirstChild;

    while CurrNode <> nil do
    begin
      ProcessNode(CurrNode, TreeNode);
      CurrNode := CurrNode.NextSibling;
    end;
  end;

  procedure ExpandNodes;
  var
    i: Integer;
  begin
    for i := 0 to ExpandedList.Count - 1 do
      TTreeNode(ExpandedList.Items[i]).Expanded := True;
  end;

begin
  Tree.Items.Clear;
  Doc := nil;
  try
    ExpandedList := TFPList.Create;
    ReadXmlFile(Doc, St);
    //debugln('Doc.DocumentElement.NodeName=',Doc.DocumentElement.NodeName);
    if (Doc.DocumentElement.NodeName = RootNodeName) then
    begin
      //don't assume that 'Items' is the first child node, it might not be the case in the future
      DomNode := Doc.DocumentElement.FindNode(ItemsNodeName);
      if Assigned(DomNode) then
        DomNode := DomNode.FirstChild;
      while (DomNode <> nil) do
      begin
        ProcessNode(DomNode, nil); // Recursive
        DomNode := DomNode.NextSibling;
      end;
      ExpandNodes;
    end;
  finally
    ExpandedList.Free;
    if Assigned(Doc) then
      Doc.Free;
  end;
end;

end.

