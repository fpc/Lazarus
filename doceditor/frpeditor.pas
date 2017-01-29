unit frpeditor;
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

  Author: Michael Van Canneyt
  Changed to Frame by Vladislav V. Sudarikov
}
{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, DOM, Forms, Controls, ExtCtrls, ComCtrls,
     StdCtrls, Dialogs, Menus, FPDEUtil, LazDEMsg, LazDEOpts, LazLogger;

Type
  { TPackageEditor }

  { TPackEditorFrame }

  TPackEditorFrame = Class(TFrame)
    GroupBox1: TGroupBox;
    lblNodeStructure: TLabel;
    lblStructure: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PEMenu: TPopupMenu;
    pnlElements: TPanel;
    pnlModules: TPanel;
    PMMenu: TPopupMenu;
    Splitter1: TSplitter;
    trvElements: TTreeView;
    trvModules: TTreeView;
    Procedure ModuleChange(Sender: TObject; Node: TTreeNode);
    Procedure ModuleChanging(Sender: TObject; Node: TTreeNode;
                             Var AllowChange : Boolean);
    Procedure ElementChange(Sender: TObject; Node: TTreeNode);
    Procedure ElementChanging(Sender: TObject; Node: TTreeNode;
                              Var AllowChange : Boolean);
  Private
    FModified : Boolean;
    FDescriptionNode : TDomNode;
    FCurrentPackage,
    FCurrentElement,
    FCurrentModule,
    FCurrentTopic : TDomElement;
    FOnSelectElement,
    FOnSelectPackage,
    FOnSelectTopic,
    FOnSelectModule : TElementEvent;
    FModuleNode : TTreeNode;
    Procedure ShowModuleElements(Module : TDomElement);
    Procedure ClearElements;
    procedure UpdateNodeImage(N: TTreeNode);
    // Element node methods.
    Procedure SelectTopic(Sender : TDomElement);
    Procedure SelectModule(Sender : TDomElement);
    Procedure SelectPackage(Sender : TDomElement);
    Procedure SelectElement(Sender : TDomElement);
  protected
    Procedure SetCurrentModule(Value : TDomElement); virtual;
    Procedure SetCurrentPackage(Value : TDomElement); virtual;
    Procedure SetCurrentElement(E : TDomElement); virtual;
    Procedure SetCurrentTopic(T : TDomElement); virtual;
    Procedure SetDescriptionNode (Value : TDomNode); virtual;
  Public
    Procedure Refresh; virtual; abstract;
    Procedure AddElement(E : TDomElement);  virtual; abstract;
    Procedure DeletePackage(P : TDomElement); virtual; abstract;
    Procedure DeleteModule(M : TDomElement); virtual; abstract;
    Procedure DeleteElement(E : TDomElement); virtual; abstract;
    Procedure DeleteTopic(T : TDomElement); virtual; abstract;
    Procedure RenamePackage(P : TDomElement); virtual; abstract;
    Procedure RenameModule(M : TDomElement); virtual; abstract;
    Procedure RenameElement(E : TDomElement); virtual; abstract;
    Procedure RenameTopic(T : TDomElement); virtual; abstract;
    procedure UpdateTree;
    procedure ExpandTree;
    Property DescriptionNode : TDomNode Read FDescriptionNode Write SetDescriptionNode;
    Property OnSelectModule  : TElementEvent Read FOnSelectModule Write FOnSelectmodule;
    Property OnSelectTopic   : TElementEvent Read FOnSelectTopic Write FOnSelectTopic;
    Property OnSelectPackage : TElementEvent Read FOnSelectPackage Write FOnSelectPackage;
    Property OnSelectElement : TElementEvent Read FOnSelectElement Write FOnSelectElement;
    Property CurrentPackage  : TDomElement Read FCurrentPackage Write SetCurrentPackage;
    Property CurrentModule   : TDomElement Read FCurrentModule Write SetCurrentModule;
    Property CurrentTopic    : TDomElement Read FCurrentTopic Write SetCurrentTopic;
    Property CurrentElement  : TDomElement Read FCurrentElement  Write SetCurrentElement;
    Property Modified        : Boolean Read FModified Write FModified;
  end;

  TPackageEditor = Class(TPackEditorFrame)
  Private
    FMRenameMenu,
    FMDeleteMenu,
    FERenameMenu,
    FECollapseAllMenu,
    FEExpandAllMenu,
    FEDeleteMenu : TMenuItem;
    // Callbacks for visual controls.
    Procedure MenuRenameClick(Sender : TObject);
    Procedure MenuDeleteClick(Sender : TObject);
    Procedure MenuCollapseAllClick(Sender: TObject);
    procedure MenuExpandAllClick(Sender: TObject);
    // Internal node methods.
    Procedure DeleteNode(Msg : String; N : TTreeNode; E : TDomElement);
    Procedure DeleteElementNode(N : TTreeNode);
    Procedure RenameNode(Msg : String; N : TTreeNode);
    Function  GetSelectedNode : TTreeNode;
    Function  NewName(ATitle : String;Var AName : String) : Boolean;
    Function  AddDomNode(E : TDomElement;Nodes: TTreeNodes;
                         AParent : TTreeNode) : TTreeNode;
    Procedure DoTopicNode(Node : TDomElement;Nodes: TTreeNodes;
                          AParent : TTreeNode);

    Procedure SetModuleNode(N : TTreeNode);
    Function  CreateElementNode(E : TDomelement) : TTreeNode;
    // Correspondence TreeNode<->TDomElement
    Function  FindPackageNode(P : TDomElement) : TTreeNode;
    Function  FindModuleNodeInNode(M : TDomElement; N : TTreeNode) : TTreeNode;
    Function  FindTopicNodeInNode(M : TDomElement; N : TTreeNode) : TTreeNode;
    Function  FindElementNode(E : TDomElement; N : TTreeNode) : TTreeNode;


    Procedure SetCurrentElementNode(N : TTreeNode);
    Procedure SetCurrentModuleNode(N : TTreeNode);
    Procedure SetCurrentPackageNode(N : TTreeNode);
    Procedure SetCurrentTopicNode(T : TTreeNode);
    // Other methods

    function GetElementName(E : TDomElement): String;
  Protected
    Procedure SetCurrentModule(Value : TDomElement); override;
    Procedure SetCurrentPackage(Value : TDomElement); override;
    Procedure SetCurrentElement(E : TDomElement); override;
    Procedure SetCurrentTopic(T : TDomElement); override;
    Procedure SetDescriptionNode (Value : TDomNode); override;
  Public
    Constructor Create(AOwner : TComponent);override;
    Procedure Refresh; override;
    Procedure AddElement(E : TDomElement); override;
    Procedure DeletePackage(P : TDomElement); override;
    Procedure DeleteModule(M : TDomElement); override;
    Procedure DeleteElement(E : TDomElement); override;
    Procedure DeleteTopic(T : TDomElement); override;
    Procedure RenamePackage(P : TDomElement); override;
    Procedure RenameModule(M : TDomElement); override;
    Procedure RenameElement(E : TDomElement); override;
    Procedure RenameTopic(T : TDomElement); override;
    procedure UpdateSelectedNodeStatus;

  end;

var
  PackEditorFrame: TPackEditorFrame;
implementation

uses frmNewNode, graphics, LCLType, strutils;

{$R *.lfm}
{ ---------------------------------------------------------------------
  Auxiliary routines
  ---------------------------------------------------------------------}

Function GetNextNode(N : TTreeNode) : TTreeNode;

begin
  Result:=N.GetNextSibling;
  If (Result=Nil) and (N.Parent<>Nil) then
    begin
    Result:=N.Parent.Items[0]; // Count is always >=0, N !!
    While (Result<>Nil) and (Result.GetNextSibling<>N) do
      Result:=Result.GetNextSibling;
    If (Result=Nil) then
      Result:=N.Parent;
    end;
end;

Function SubNodeWithElement(P : TTreeNode; E : TDomElement) : TTreeNode;

Var
  N : TTreeNode;

begin
 Result:=Nil;
 If (E<>Nil) and (P<>Nil) and (P.Count>0) then
   begin
   N:=P.Items[0];
   While (Result=Nil) and (N<>Nil) do
     If (N.Data=Pointer(E)) then
       Result:=N
     else
     begin
       //recurse
       if N.HasChildren then
         Result:=SubNodeWithElement(N, E);
       N:=N.GetNextSibling;
     end;
   end;
end;

{ ---------------------------------------------------------------------
  TPackEditorFrame
  ---------------------------------------------------------------------}

procedure TPackEditorFrame.SetCurrentModule(Value: TDomElement);
begin
  if (Value <> nil) then begin
    if (Value.ParentNode <> nil) then begin
      CurrentPackage:=Value.ParentNode as TDomElement;
      FCurrentModule:=Value;
    end;
  end;
end;

procedure TPackEditorFrame.SetCurrentPackage(Value: TDomElement);
begin
  FCurrentPackage:=Value;
end;

procedure TPackEditorFrame.SetCurrentElement(E: TDomElement);
begin
  FCurrentElement:=E;
end;

procedure TPackEditorFrame.SetCurrentTopic(T: TDomElement);

Var
  N  : TDomElement;

begin
  If (FCurrentTopic<>T) then
  begin
    if assigned(T) then begin
      N:=T.ParentNode as TDomElement;
      if IsModuleNode(N) then
        CurrentModule:=N
      else if IsPackageNode(N) then
        begin
        CurrentModule:=Nil;
        CurrentPackage:=N;
        end
      else
        Raise Exception.Create('Unknown parent node for topic node '+TDomElement(T)['name']);
    end;
    FCurrentTopic:=T;
  end;
end;

procedure TPackEditorFrame.SetDescriptionNode(Value: TDomNode);
begin
  FDescriptionNode:=Value;
end;

procedure TPackEditorFrame.UpdateTree;
var n: TTreeNode;
begin
  n := trvModules.Items[0];
  while not Assigned(TDOMElement(n.Data)) do n := n.GetNext;
  while Assigned(n) do
  begin
    UpdateNodeImage(n);
    n := n.GetNext;
  end;
  if trvElements.Items.Count > 0 then
  begin
    n := trvElements.Items[0];
    while not Assigned(TDOMElement(n.Data)) do n := n.GetNext;
    while Assigned(n) do
    begin
      UpdateNodeImage(n);
      n := n.GetNext;
    end;
  end;
end;

procedure TPackEditorFrame.ExpandTree;
begin
  trvModules.Items[0].Expand(True);
end;


{ ---------------------------------------------------------------------
  TPackageEditor
  ---------------------------------------------------------------------}

Constructor TPackageEditor.Create(AOwner : TComponent);

  Function NewMenuItem(ACaption : String; AOnClick : TNotifyEvent) : TMenuItem;

  begin
    Result:=TMenuItem.Create(Self);
    Result.Caption:=ACaption;
    Result.OnClick:=AOnClick;
  end;

begin
  Inherited;
  Width:=300;
  lblStructure.Caption := SFileStructure;
  lblNodeStructure.Caption := SModuleElements;
//  lblStructure.Hint := sfil
  FERenameMenu:=NewMenuItem(SMenuRename,@MenuRenameClick);
  FEDeleteMenu:=NewMenuItem(SMenuDelete,@MenuDeleteClick);
  FEDeleteMenu.ShortCut := ShortCut(VK_DELETE, []);
  FEExpandAllMenu:=NewMenuItem(SMenuExpandAll,@MenuExpandAllClick);
  FECollapseAllMenu:=NewMenuItem(SMenuCollapseAll,@MenuCollapseAllClick);
  PEMenu.Items.Add(FERenameMenu);
  PEMenu.Items.Add(FEDeleteMenu);
  PEMenu.Items.Add(NewMenuItem('-',nil));
  PEMEnu.Items.Add(FEExpandAllMenu);
  PEMenu.Items.Add(FECollapseAllMenu);
  trvElements.PopupMenu:=PEMenu;
  FMRenameMenu:=NewMenuItem(SMenuRename,@MenuRenameClick);
  FMDeleteMenu:=NewMenuItem(SMenuDelete,@MenuDeleteClick);
  PMMenu.Items.Add(FMRenameMenu);
  PMMenu.Items.Add(FMDeleteMenu);
  trvModules.PopupMenu:=PMMenu;
end;

Procedure TPackageEditor.SetDescriptionNode (Value : TDomNode);

begin
  Inherited;
  Refresh;
end;


procedure TPackEditorFrame.ModuleChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if Sender=nil then ;
  if Node=nil then ;
  AllowChange:=True;
end;

Procedure TPackEditorFrame.ModuleChange(Sender: TObject; Node: TTreeNode);
Var
  o : TDomElement;
begin
  if Sender=nil then ;
  If (Node<>Nil) then
    begin
    O:=TDomElement(Node.Data);
    If (O<>Nil) then
      If IsPackageNode(O) then
        SelectPackage(O)
      else if IsModuleNode(O) then
        SelectModule(O)
      else if IsTopicNode(O) then
        SelectTopic(O)
    end;
end;

procedure TPackEditorFrame.ElementChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if Sender=nil then ;
  if Node=nil then ;
  AllowChange:=True;
end;

Procedure TPackEditorFrame.SelectElement(Sender : TDomElement);

begin
  If IsElementNode(Sender) or IsModuleNode(Sender) or IsPackageNode(Sender) then
    CurrentElement:=Sender
  else // No valid node
    CurrentElement:=Nil;
  If Assigned(FOnSelectElement) then
    OnSelectElement(Sender);
end;

Procedure TPackEditorFrame.ElementChange(Sender: TObject; Node: TTreeNode);

Var
  o : TDomElement;

begin
  if Sender=nil then ;
  If (Node<>Nil) then
    begin
    O:=TDomElement(Node.Data);
    SelectElement(O)
    end;
end;


Procedure TPackEditorFrame.SelectModule(Sender : TDomElement);
begin
  CurrentTopic:=Nil;
  CurrentPackage:=Sender.ParentNode as TDomElement;
  CurrentModule:=Sender;
  CurrentElement:=Sender;
  ShowModuleElements(FCurrentModule);
  If Assigned(FOnSelectModule) then
    FOnSelectModule(Sender);
end;

Procedure TPackEditorFrame.SelectPackage(Sender : TDomElement);
begin
  CurrentElement:=Nil;
  CurrentModule:=Nil;
  CurrentTopic:=Nil;
  CurrentPackage:=Sender;
  ShowModuleElements(Nil);
  If Assigned(FOnSelectPackage) then
   FOnSelectPackage(Sender);
end;

Procedure TPackEditorFrame.SelectTopic(Sender : TDomElement);
Var
  P : TDomElement;

begin
  CurrentTopic:=Sender;
  P:=FCurrentTopic.ParentNode as TDomElement;
  if IsModuleNode(P) then
    CurrentModule:=P
  else if IsTopicNode(P) then
    CurrentPackage:=P.ParentNode as TDomElement
  else if IsPackageNode(P) then
    CurrentPackage:=p
  else
    Raise Exception.CreateFmt(SErrUnknownDomElement,[P.NodeName]);
  If Assigned(FOnSelectTopic) then
    FOnSelectTopic(Sender);
end;

Function  TPackageEditor.GetSelectedNode : TTreeNode;

begin
  Result:=trvModules.Selected;
end;

Procedure TPackageEditor.MenuRenameClick(Sender : TObject);

Var
  E : TDomElement;

begin
  if Sender=nil then ;
  E:=TDomElement(trvModules.Selected.Data);
  If Assigned(E) then
    If IsPackageNode(E)then
      RenamePackage(E)
    else if IsModuleNode(E) then
      RenameModule(E)
    Else if IsTopicNode(E) then
      RenameTopic(E)
    else if IsElementNode(E) then
      RenameElement(E)
end;


Procedure TPackageEditor.MenuDeleteClick(Sender : TObject);

Var
  E : TDomElement;

begin
  If (Sender=FEDeleteMenu) then
    begin
    E:=TDomElement(trvElements.Selected.Data);
    If IsElementNode(E) then
      DeleteElement(E);
    end
  else
    begin
    E:=TDomElement(trvModules.Selected.Data);
    If IsPackageNode(E) then
      DeleteNode(SDeletePackage,trvModules.Selected,E)
    else if IsModuleNode(E) then
      DeleteNode(SDeleteModule,trvModules.Selected,E)
    else if IsTopicNode(E) then
      DeleteNode(SDeleteTopic,trvModules.Selected,E)
    end;
end;

procedure TPackageEditor.MenuCollapseAllClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  if Sender=nil then ;
  trvElements.FullCollapse;
  Node := trvElements.Items.GetFirstNode;
  if Node<>nil then
    Node.Expand(False);
end;

procedure TPackageEditor.MenuExpandAllClick(Sender: TObject);
begin
  if Sender=nil then ;
  trvElements.FullExpand;
end;


Procedure TPackageEditor.SetModuleNode(N : TTreeNode);

begin
  If N<>Nil then
    begin
    trvModules.Selected:=N;
    ModuleChange(trvModules,N);
    end
  else
    Refresh;
end;

Procedure TPackageEditor.DeleteNode(Msg : String; N : TTreeNode; E : TDomElement);

Var
  P : TTreeNode;

begin
  If (Not ConfirmDelete) or
     (MessageDlg(Format(Msg,[E['name']]),mtConfirmation,[mbYes,mbNo],0)=mrYes) then
    begin
    P:=GetNextNode(N);
    trvModules.Items.Delete(N);
    FModified:=True;
    SetModuleNode(P);
    end;
end;

Function TPackageEditor.NewName(ATitle : String;Var AName : String) : Boolean;
begin
  Result:=false;
  With TNewNodeForm.Create(Self) do
    Try
      Caption:=ATitle;
      ENodeName.Text:=AName;
      If (ShowModal=mrOK) Then begin
        AName:=ENodeName.Text;
        Result:=AName<>'';
      end;
    Finally
      Free;
    end;
end;

Procedure TPackageEditor.RenameNode(Msg : String; N : TTreeNode);

Var
  E : TDomElement;
  S : String;

begin
  E:=TDomElement(N.Data);
  S:=E['name'];
  If NewName(Msg,S) then
    begin
    E['name']:=S;
    N.Text:=S;
    FModified:=True;
    end;
end;

Function TPackageEditor.CreateElementNode(E : TDomelement) : TTreeNode;
begin                                // FModuleNode
  Result:=trvElements.Items.AddChild(trvElements.Selected,GetElementName(E));
  Result.Data:=E;
  UpdateNodeImage(Result);
end;

Procedure TPackageEditor.DeleteElementNode(N : TTreeNode);

  procedure RemoveAllChildren(node: TTreeNode);
  var sl: Integer; de: TDOMElement;
  begin
    sl := node.Level;
    node := node.GetFirstChild;
    while Assigned(node) and (node.Level > sl) do
    begin
      de := TDomElement(node.Data);
      de.ParentNode.RemoveChild(de);
      node := node.GetNext;
    end;
  end;

Var
  Reposition : Boolean;
  P : TTreeNode;
  d: TDomElement;
begin
  Reposition:=(TDomElement(N.Data)=CurrentElement) and (CurrentElement<>Nil) ;
  d := TDomElement(N.Data);
  d.ParentNode.RemoveChild(d);
  RemoveAllChildren(N);
  P:=GetNextNode(N);
  trvElements.Items.Delete(N);
  FModified:=True;
  If Reposition then
    begin
      trvElements.Selected:=P;
      ElementChange(trvElements,P);
    end;
end;

Procedure TPackageEditor.DeleteElement(E : TDomElement);

Var
  N : TTreeNode;

begin
  N:=FindElementNode(E,Nil);
  If (N<>Nil) then
    DeleteElementNode(N);
end;

Procedure TPackageEditor.DeletePackage(P : TDomElement);

Var
  N : TTreeNode;

begin
  N:=FindPackageNode(P);
  If N<>NIl then
    DeleteNode(SDeletePackage,N,P);
end;


Procedure TPackageEditor.DeleteModule(M : TDomElement);

Var
  N : TTreeNode;

begin
  N:=FindModuleNodeInNode(M,Nil);
  If N<>NIl then
    DeleteNode(SDeleteModule,N,M);
end;


Procedure TPackageEditor.DeleteTopic(T : TDomElement);

Var
  N : TTreeNode;

begin
  N:=FindTopicNodeInNode(T,Nil);
  If N<>NIl then
    DeleteNode(SDeleteTopic,N,T);
end;


Procedure TPackageEditor.RenamePackage(P : TDomElement);

Var
  N : TTreeNode;

begin
  N:=FindPackageNode(P);
  If N<>NIl then
    RenameNode(SRenamePackage,N);
end;


Procedure TPackageEditor.RenameModule(M : TDomElement);

Var
  N : TTreeNode;

begin
  N:=FindModuleNodeInNode(M,Nil);
  If N<>NIl then
    RenameNode(SRenameModule,N);
end;


Procedure TPackageEditor.RenameTopic(T : TDomElement);

Var
  N : TTreeNode;

begin
  N:=FindTopicNodeInNode(T,Nil);
  If N<>NIl then
    RenameNode(SRenameTopic,N);
end;

procedure DebugElement(Element: TDomElement);
var
  Level: integer;
const
  NType:Array[0..12] of String[30] =
  (
  '0:UNKNOWN',
  '1:ELEMENT_NODE',
  '2:ATTRIBUTE_NODE',
  '3:TEXT_NODE',
  '4:CDATA_SECTION_NODE',
  '5:ENTITY_REFERENCE_NODE',
  '6:ENTITY_NODE',
  '7:PROCESSING_INSTRUCTION_NODE',
  '8:COMMENT_NODE',
  '9:DOCUMENT_NODE',
  '10:DOCUMENT_TYPE_NODE',
  '11:DOCUMENT_FRAGMENT_NODE',
  '12:NOTATION_NODE'
  );
  function GetLevelSpc: String;
  begin
    SetLength(Result, Level*2);
    FillChar(Result[1], Level*2, ' ');
  end;
  procedure DebugNodes(Node: TDomNode);
  begin
    Node := Node.FirstChild;
    while node<>nil do begin
      DebugLn(GetLevelSpc, 'Node=',Node.NodeName,' Type=',NType[Node.NodeType],' Value=',Node.NodeValue);
      if (node.NodeType = ELEMENT_NODE) then begin
        Inc(Level);
        DebugNodes(Node);
        Dec(Level);
      end;
      Node := Node.NextSibling;
    end;
  end;
begin
  if assigned(Element) then begin
    debugLn('Element: ', Element['name'],': ');
    level := 1;
    DebugNodes(Element);
  end else
    debugLn('Element <nil>');
end;


procedure TPackageEditor.UpdateSelectedNodeStatus;
begin
  if trvElements.Selected <> nil then
    UpdateNodeImage(trvElements.Selected);
end;

Procedure TPackageEditor.RenameElement(E : TDomElement);

Var
  N : TTreeNode;

begin
  N:=FindElementNode(E,Nil);
  If N<>NIl then
    RenameNode(SRenameElement,N);
end;

Procedure TPackEditorFrame.ClearElements;
begin
  trvElements.Items.Clear;
  FModuleNode:=Nil;
end;

Procedure TPackEditorFrame.ShowModuleElements(Module : TDomElement);
Var
  Node : TDomNode;
  SNode,PNode,TNode : TTreeNode;
  S : TStringList;
  I,L : Integer;
  N,PN : String;

begin
  ClearElements;
  If Assigned(Module) then
    begin
    FModuleNode:=trvElements.Items.Add(Nil,Module['name']);
    FModuleNode.Data:=Module;
    UpdateNodeImage(FModuleNode);
    S:=TStringList.Create;
    Try
      // get sorted list of elements
      Node:=Module.FirstChild;
      While Assigned(Node) do
        begin
        If IsElementNode(Node) then
          S.AddObject(TDomElement(Node)['name'],Node);
        Node:=Node.NextSibling;
        end;
      S.Sorted:=True;
      // root node
      TNode:=FModuleNode;
      // process list of elements, create levels
      trvElements.Items.BeginUpdate;
      try
        For I:=0 to S.Count-1 do
          begin
          PNode:=Nil;
          SNode:=TNode;
          N:=S[i];
          // look for a tentative new parents
          While (SNode<>FModuleNode) and (PNode=Nil) do
            begin
            PN:=TDomElement(SNode.Data)['name']+'.';
            L:=Length(PN);
            If CompareText(Copy(N,1,L),PN)=0 then
              PNode:=SNode;
            SNode:=SNode.Parent;
            end;
          If (PNode=Nil) then
            PNode:=FModuleNode
          else
            System.Delete(N,1,L);
          TNode:=trvElements.Items.AddChild(PNode,N);
          TNode.Data:=S.Objects[i];
          UpdateNodeImage(TNode);
          end;
      finally
        trvElements.Items.EndUpdate;
      end;
      Finally
        S.Free;
      end;
    FModuleNode.Expand(False);
    trvElements.Selected:=FModuleNode;
    ElementChange(trvElements,FModuleNode);
    end;
end;

Function TPackageEditor.AddDomNode(E : TDomElement;Nodes: TTreeNodes;AParent : TTreeNode) : TTreeNode;

begin
  Result:=Nodes.AddChild(AParent,E['name']);
  Result.Data:=E;
end;

Procedure TPackageEditor.DoTopicNode(Node : TDomElement;Nodes: TTreeNodes;AParent : TTreeNode);

Var
  N : TTreeNode;
  SubNode : TDomNode;

begin
  N:=Nodes.AddChild(AParent,Node['name']);
  N.Data:=Node;
  UpdateNodeImage(N);
  SubNode:=Node.FirstChild;
  While (SubNode<>Nil) do
    begin
    If IsTopicNode(SubNode) then
      DoTopicNode(SubNode as TDomElement,Nodes,N);
    SubNode:=SubNode.NextSibling;
    end;
end;


Procedure TPackageEditor.Refresh;

var
  Node,SubNode,SSnode : TDomNode;
  R,P,M : TTreeNode;

begin
  trvModules.Items.BeginUpdate;
  trvModules.Items.Clear;
  R:=trvModules.Items.add(Nil,SPackages);
  R.ImageIndex := 0;
  R.SelectedIndex := 0;
  If Assigned(FDescriptionNode) then
    begin
    Node:=FDescriptionNode.FirstChild;
    While Assigned(Node) do
      begin
      If IsPackageNode(Node) then
        begin
        P:=AddDomNode(Node as TDomElement,trvModules.Items,R);
        UpdateNodeImage(P);
        SubNode:=Node.FirstChild;
        While Assigned(SubNode) do
          begin
          If IsModuleNode(SubNode) then
            begin
            M:=AddDomNode(SubNode as TDomElement,trvModules.Items,P);
            UpdateNodeImage(M);
            SSNode:=SubNode.FirstChild;
            While (SSNode<>Nil) do
              begin
              if IsTopicNode(SSNode) then
                DoTopicNode(SSNode as TDomElement,trvModules.Items,M);
              SSNode:=SSNode.NextSibling;
              end;
            end
          else if IsTopicNode(SubNode) then
            DoTopicNode(SubNode as TDomElement,trvModules.Items,P);
          SubNode:=SubNode.NextSibling;
          end;
        end;
      Node:=Node.NextSibling;
      end;
    end;
  trvModules.Items.EndUpdate;
  CurrentModule:=Nil;
  FModified:=False;
end;

Function TPackageEditor.FindPackageNode(P : TDomElement) : TTreeNode;
begin
  Result:=Nil;
  Result:=SubNodeWithElement(trvModules.Items[0],P);
  If (Result=Nil) then
    Raise Exception.CreateFmt(SErrNoNodeForPackage,[P['name']]);
end;

Function TPackageEditor.FindModuleNodeInNode(M : TDomElement; N : TTreeNode) : TTreeNode;

Var
  P : TTreeNode;

begin
  Result:=Nil;
  If (N<>Nil) then
    P:=N
  else
    P:=FindPackageNode(M.ParentNode as TDomElement);
  Result:=SubNodeWithElement(P,M);
  If (Result=Nil) then
    Raise Exception.CreateFmt(SErrNoNodeForModule,[M['name']]);
end;

Function TPackageEditor.FindTopicNodeInNode(M : TDomElement; N : TTreeNode) : TTreeNode;

Var
  P : TTreeNode;
  E : TDomElement;

begin
  Result:=Nil;
  If (N<>Nil) then
    P:=N
  else
    begin
    E:=M.ParentNode as TDomElement;
    If IsModuleNode(E) then
      P:=FindModuleNodeInNode(E,FindPackageNode(E.ParentNode as TDomElement))
    else if IsTopicNode(E) then
      // Assumes that we can only nest 2 deep inside package node !!
      P:=FindTopicNodeInNode(E,FindPackageNode(E.ParentNode as TDomElement))
    else if IsPackageNode(E) then
      P:=FindPackageNode(E);
    end;
  Result:=SubNodeWithElement(P,M);
  If (Result=Nil) then
    Raise Exception.CreateFmt(SErrNoNodeForTopic,[M['name']]);
end;

Function TPackageEditor.FindElementNode(E: TDomElement; N: TTreeNode): TTreeNode;

Var
  P : TTreeNode;

begin
  If IsModuleNode(E) then
    Result:=FModuleNode
  else
    begin
    Result:=Nil;
    If (N<>Nil) then
      P:=N
    else
      P:=FModuleNode;
    Result:=SubNodeWithElement(P,E);
    end;
end;

Procedure TPackageEditor.AddElement(E : TDomElement);

Var
  N : TTreeNode;

begin
  N:=CreateElementNode(E);
  SetCurrentElementNode(N);
  FModified:=True;
end;


Procedure TPackageEditor.SetCurrentPackage(Value : TDomElement);

begin
  if (Value<>CurrentPackage) then
    begin
    Inherited;
    If (Value<>Nil) then
      SetCurrentPackageNode(FindPackageNode(Value));
    end;
end;

Procedure TPackageEditor.SetCurrentPackageNode(N : TTreeNode);

begin
  trvModules.Selected:=N;
end;

Procedure TPackageEditor.SetCurrentModule(Value : TDomElement);
begin
  If (Value<>CurrentModule) then
    begin
    Inherited;
    If Assigned(Value) then
      SetCurrentModuleNode(FindModuleNodeInNode(Value,Nil))
    else
      ClearElements;
    FCurrentModule:=Value
    end;
end;

Procedure TPackageEditor.SetCurrentModuleNode(N : TTreeNode);

Var
  P : TTreeNode;

begin
  P:=FindPackageNode(CurrentPackage);
  If Assigned(P) then
    P.Expand(False);
  trvModules.Selected:=N;
  Application.ProcessMessages;
end;

Procedure TPackageEditor.SetCurrentTopic(T : TDomElement);

Var
  N  : TDomElement;
  PN : TTreeNode;

begin
  If (CurrentTopic<>T) then
  begin
    if assigned(T) then
    begin
      N:=T.ParentNode as TDomElement;
      if IsModuleNode(N) then
        begin
        CurrentModule:=N;
        PN:=FindModuleNodeInNode(N,Nil);
        end
      else if IsPackageNode(N) then
        begin
        CurrentModule:=Nil;
        CurrentPackage:=N;
        PN:=FindPackageNode(n);
        end;
      SetCurrentTopicNode(FindTopicNodeInNode(T,PN));
    end;
  end;
  Inherited;
end;

Procedure TPackageEditor.SetCurrentTopicNode(T : TTreeNode);

begin
 T.Parent.Expand(False);
 trvModules.Selected:=T;
 If (CurrentElement<>Nil) then
   CurrentElement:=Nil;
end;

procedure TPackEditorFrame.UpdateNodeImage(N: TTreeNode);

  procedure SetNodeImage(N: TTreeNode; Index: Integer);
  begin
    n.ImageIndex := Index;
    n.SelectedIndex := Index;
  end;

  function GetImgIndex(DN: TDomNode): Integer;
  var
    N: TDomNode;
    HasShort, HasDescr, HasSealso, HasExample, HasErrors: Boolean;
  begin
     Result := ImgIndxNew;
     N := DN.FirstChild;
     HasShort := False; HasDescr := False;
     HasErrors := False;
     HasExample := False; HasSealso := False;
    while Assigned(N) do
    begin
      if (N.NodeName = 'short') and (N.TextContent <> '') then
      begin HasShort := True; Result := ImgIndxEdited; end;
      if (N.NodeName = 'descr') and (N.TextContent <> '') then
      begin HasDescr := True; Result := ImgIndxEdited; end;
      if (N.NodeName = 'seealso') and (N.HasChildNodes) then
      begin HasSealso := True; Result := ImgIndxEdited; end;
      if (N.NodeName = 'example') and (N.Attributes[0].TextContent <> '') then
      begin HasExample := True; Result := ImgIndxEdited; end;
      if (N.NodeName = 'errors') and (N.TextContent <> '') then
      begin HasErrors := True; Result := ImgIndxEdited; end;
      N := N.NextSibling;
    end;
    if HasShort or HasDescr or HasSealso or HasExample or HasErrors then
      Result := ImgIndxEdited;
  end;

var
  ImgIndex: Integer;
  Element: TDomElement;
begin
  if Assigned(N) then
  begin
    Element := TDomElement(N.Data);
    if not Assigned(Element) then
      Exit;
    ImgIndex := GetImgIndex(Element);
    SetNodeImage(N, ImgIndex);
  end;
end;

function TPackageEditor.GetElementName(E: TDomElement): String;
var fn: String;
begin
  fn := E['name'];
  Result := RightStr(fn, Length(fn) - RPos('.', fn));
end;

Procedure TPackageEditor.SetCurrentElement(E : TDomElement);
var
  SelNode:TTreeNode;
begin
  If (E<>FCurrentElement) and (E <> nil) then
    begin
    Inherited;
    If E.NodeName='module' then
      CurrentModule:=E
    else
      CurrentModule:=E.ParentNode as TDomElement;
    SelNode:=trvElements.Selected;
    //avoid selecting an already selected node (occurs in OnChange event)
    if (SelNode = nil) or (SelNode.Data <> Pointer(E)) then
      SetCurrentElementNode(FindElementNode(E,Nil));
    end;
end;

Procedure TPackageEditor.SetCurrentElementNode(N : TTreeNode);

begin
  trvElements.Selected:=N;
end;

end.



