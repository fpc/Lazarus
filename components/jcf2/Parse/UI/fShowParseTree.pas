unit fShowParseTree;

{
  AFS 2002

  A form to show a unit's parse tree
  mainly for debugiing purposes when the parse goes wrong
}


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is fShowParseTree, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, ComCtrls, ExtCtrls, StdCtrls,
  LCLIntf, LCLType,
  { local }
  ParseTreeNode;

type
  TfrmShowParseTree = class(TForm)
    StatusBar1:       TStatusBar;
    pnlTop:           TPanel;
    lblTreeCount:     TLabel;
    lblTreeDepth:     TLabel;
    pnlBottom:        TPanel;
    lblCurrent:       TLabel;
    lblDepth:         TLabel;
    lblTotalNodeCount: TLabel;
    lblImmediateChildCount: TLabel;
    cbShowWhiteSpace: TCheckBox;
    pcPages:          TPageControl;
    tsTokens:         TTabSheet;
    tsTree:           TTabSheet;
    tvParseTree:      TTreeView;
    lvTokens:         TListView;
    procedure tvParseTreeChange(Sender: TObject; Node: TTreeNode);
    procedure cbShowWhiteSpaceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvTokensClick(Sender: TObject);
    procedure lvTokensSelectItem(Sender: TObject; {%H-}Item: TListItem; {%H-}Selected: boolean);
    procedure lvTokensDblClick(Sender: TObject);
    procedure tvParseTreeDblClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    fcRootNode: TParseTreeNode;
    procedure ShowTreeNodeDetails(const pcNode: TParseTreeNode);

  public
    property RootNode: TParseTreeNode Read fcRootNode Write fcRootNode;

    procedure DisplayTree;
  end;

procedure ShowParseTree(const pcRoot: TParseTreeNode);

implementation

{$R *.lfm}

uses
  SourceToken, Tokens, JcfHelp, JcfFontSetFunctions;

procedure ShowParseTree(const pcRoot: TParseTreeNode);
var
  lfParseTree: TfrmShowParseTree;
begin
  Assert(pcRoot <> nil);

  lfParseTree := TfrmShowParseTree.Create(Application);
  try
    lfParseTree.RootNode := pcRoot;
    lfParseTree.DisplayTree;

    lfParseTree.ShowModal;

  finally
    lfParseTree.Free;
  end;
end;



procedure TfrmShowParseTree.DisplayTree;

  procedure ShowTokensInList(const pcData: TParseTreeNode);
  var
    lcNewItem: TListItem;
    lcToken: TSourceToken;
    liLoop:  integer;
    lsDesc:  string;
  begin
    { exclude this one as white space }
    if (not cbShowWhiteSpace.Checked) and (not pcData.HasChildren) and
      (pcData is TSourceToken) and (TSourceToken(pcData).TokenType in
      NotSolidTokens) then
      exit;

    { list tokens }
    if (pcData is TSourceToken) then
    begin
      lcToken := TSourceToken(pcData);

      lcNewItem := lvTokens.Items.Add;
      lcNewItem.Caption := IntToStr(lvTokens.Items.Count);

      lsDesc := TokenTypeToString(lcToken.TokenType);
      lcNewItem.SubItems.Add(lsDesc);
      lcNewItem.SubItems.Add(lcToken.SourceCode);

      lcNewItem.Data := pcData;
    end;

    // attach the children
    for liLoop := 0 to pcData.ChildNodeCount - 1 do
      ShowTokensInList(pcData.ChildNodes[liLoop]);
  end;

  procedure MakeNodeChildren(const pcGUIParent: TTreeNode; const pcData: TParseTreeNode);
  var
    lcNewItem: TTreeNode;
    liLoop: integer;
  begin
    { exclude this one as white space }
    if (not cbShowWhiteSpace.Checked) and (not pcData.HasChildren) and
      (pcData is TSourceToken) and (TSourceToken(pcData).TokenType in
      NotSolidTokens) then
      exit;

    lcNewItem := tvParseTree.Items.AddChild(pcGUIParent, pcData.Describe);
    lcNewItem.Data := pcData;

    // attach the children
    for liLoop := 0 to pcData.ChildNodeCount - 1 do
      MakeNodeChildren(lcNewItem, pcData.ChildNodes[liLoop]);
  end;

begin
  lblTreeCount.Caption := 'Tree has ' + IntToStr(fcRootNode.RecursiveChildCount) +
    ' nodes';
  lblTreeDepth.Caption := 'Tree has max depth of ' + IntToStr(fcRootNode.MaxDepth);


  lvTokens.Items.BeginUpdate;
  try
    lvTokens.Items.Clear;

    ShowTokensInList(fcRootNode);

  finally
    lvTokens.Items.EndUpdate;
  end;

  tvParseTree.Items.BeginUpdate;
  try
    tvParseTree.Items.Clear;
    MakeNodeChildren(nil, fcRootNode);

    tvParseTree.FullExpand;
  finally
    tvParseTree.Items.EndUpdate;
  end;

end;

procedure TfrmShowParseTree.tvParseTreeChange(Sender: TObject; Node: TTreeNode);
begin
  if Node = nil then
    ShowTreeNodeDetails(nil)
  else
    ShowTreeNodeDetails(Node.Data);
end;

procedure TfrmShowParseTree.ShowTreeNodeDetails(const pcNode: TParseTreeNode);
begin

  if pcNode = nil then
  begin
    lblCurrent.Caption := 'Current: none';
    lblDepth.Caption := 'Depth: -';
    lblImmediateChildCount.Caption := 'Immediate child count: -';
    lblTotalNodeCount.Caption := 'Total node count: -';
  end
  else
  begin
    lblCurrent.Caption := 'Current: ' + pcNode.Describe;
    lblDepth.Caption := 'Level: ' + IntToStr(pcNode.Level);
    lblImmediateChildCount.Caption :=
      'Immediate child count: ' + IntToStr(pcNode.ChildNodeCount);
    lblTotalNodeCount.Caption :=
      'Total node count: ' + IntToStr(pcNode.RecursiveChildCount);
  end;
end;

procedure TfrmShowParseTree.cbShowWhiteSpaceClick(Sender: TObject);
begin
  // ShowWhiteSpace setting has changed. Redisplay
  DisplayTree;
end;

procedure TfrmShowParseTree.FormShow(Sender: TObject);
begin
  pcPages.ActivePage := tsTree;
end;

procedure TfrmShowParseTree.lvTokensClick(Sender: TObject);
begin
  if lvTokens.Selected = nil then
    ShowTreeNodeDetails(nil)
  else
    ShowTreeNodeDetails(lvTokens.Selected.Data);
end;

procedure TfrmShowParseTree.lvTokensSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
begin
  if lvTokens.Selected = nil then
    ShowTreeNodeDetails(nil)
  else
    ShowTreeNodeDetails(lvTokens.Selected.Data);
end;

procedure TfrmShowParseTree.lvTokensDblClick(Sender: TObject);
var
  lpNode: pointer;
  liLoop: integer;
  lcItem: TTreeNode;
begin
  // try to select the same node in the tree
  if lvTokens.Selected = nil then
    exit;

  lpNode := lvTokens.Selected.Data;
  if lpNode = nil then
    exit;

  for liLoop := 0 to tvParseTree.Items.Count - 1 do
  begin
    lcItem := tvParseTree.Items[liLoop];
    if lcItem.Data = lpNode then
    begin
      lcItem.Selected := True;
      pcPages.ActivePage := tsTree;
      break;
    end;
  end;
end;

procedure TfrmShowParseTree.tvParseTreeDblClick(Sender: TObject);
var
  lpNode: pointer;
  liLoop: integer;
  lcItem: TListItem;
begin
  // try to select the same node in the list

  if tvParseTree.Selected = nil then
    exit;

  lpNode := tvParseTree.Selected.Data;
  if lpNode = nil then
    exit;

  for liLoop := 0 to lvTokens.Items.Count - 1 do
  begin
    lcItem := lvTokens.Items[liLoop];
    if lcItem.Data = lpNode then
    begin
      lcItem.Selected := True;
      lcItem.Focused  := True;
      lcItem.MakeVisible(False);
      pcPages.ActivePage := tsTokens;
      break;
    end;
  end;
end;

procedure TfrmShowParseTree.FormCreate(Sender: TObject);
begin
  SetObjectFontToSystemFont(Self);
end;

procedure TfrmShowParseTree.FormKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  case Key of
    VK_F1:
      try
        Application.HelpContext(HELP_MAIN);
      except
        if FileExists(Application.HelpFile) then
          OpenDocument(Application.HelpFile);
      end;
    VK_ESCAPE: Close;
  end;
end;

end.
