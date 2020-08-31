unit SortUses;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is SortUses.pas, released July 2004.
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

{ unit to sort the uses clause }

uses
  Contnrs, Classes,
  BaseVisitor, ParseTreeNode;

type
  TSortUses = class(TBaseTreeNodeVisitor)
  private

    procedure SortUsesInSections(const pcUsesIdentList: TParseTreeNode);

  protected
  public
    constructor Create; override;

    procedure PostVisitParseTreeNode(const pcNode: TObject); override;
    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  { local }
  ParseTreeNodeType, SourceToken, Tokens, JcfSettings,
  SetTransform, TokenUtils, SortUsesData;

function GetSortableToken(pcNode: TParseTreeNode): TSourceToken;
var
  lcTest: TParseTreeNode;
begin
  Result := nil;

  if pcNode is TSourceToken then
  begin
    Result := TSourceToken(pcNode);
  end
  else
  begin
    lcTest := pcNode.GetImmediateChild(nUsesItem);
    if lcTest <> nil then
      pcNode := lcTest;

    lcTest := pcNode.GetImmediateChild(nIdentifier);
    if lcTest <> nil then
      pcNode := lcTest;

    lcTest := pcNode.FirstSolidLeaf;
    if lcTest <> nil then
      Result := TSourceToken(lcTest);
  end;
end;

constructor TSortUses.Create;
begin
  inherited;

  HasPostVisit := True;
  HasSourceTokenVisit := False;
end;

function TSortUses.IsIncludedInSettings: boolean;
begin
  with FormattingSettings.Transform do
    Result := SortInterfaceUses or SortImplementationUses;
end;

procedure TSortUses.PostVisitParseTreeNode(const pcNode: TObject);
var
  lcNode: TParseTreeNode;
  lcIdentList: TParseTreeNode;
begin
  lcNode := TParseTreeNode(pcNode);

  { is this a uses clause? }
  if lcNode.NodeType <> nUses then
    exit;

  if FormattingSettings.Transform.SortUsesNoComments and lcNode.HasChildNode([ttComment]) then
    exit;

  { is it turned on in this section? }
  if lcNode.HasParentNode(nInterfaceSection) then
  begin
    { interface section }
    if not FormattingSettings.Transform.SortInterfaceUses then
      exit;
  end
  else if lcNode.HasParentNode(nImplementationSection) then
  begin
    { implentation section }
    if not FormattingSettings.Transform.SortImplementationUses then
      exit;
  end
  else
  begin
    { other - program, package or library section }
    if not FormattingSettings.Transform.SortProgramUses then
      exit;
  end;

  lcIdentList := lcNode.GetImmediateChild(nIdentList);
  if lcIdentList <> nil then
  begin
    SortUsesInSections(lcIdentList);
  end;
end;

procedure CleanEnd(const pcRootNode: TParseTreeNode);
var
  lcLastLeaf, lcRemove: TSourceToken;
begin
  lcLastLeaf := TSourceToken(pcRootNode.LastLeaf);

  while (lcLastLeaf <> nil) and (lcLastLeaf.TokenType in [ttComma, ttWhiteSpace, ttReturn]) do
  begin
    lcRemove := lcLastLeaf;
    lcLastLeaf := lcLastLeaf.PriorToken;

    { abort after a // comment }
    if (lcRemove.TokenType = ttReturn) and
      (lcLastLeaf.TokenType = ttComment) and (lcLastLeaf.CommentStyle = eDoubleSlash) then
        break;

    lcRemove.Parent.RemoveChild(lcRemove);
  end;
end;

procedure RemoveAllChildrenOnwards(const pcParent, pcChild: TParseTreeNode);
var
  lcChild, lcLast: TParseTreeNode;
begin
  if pcChild = nil then
    lcChild := pcParent.FirstLeaf
  else
    lcChild := pcChild;

  while (lcChild <> nil) and lcChild.HasParentNode(pcParent) do
  begin
    lcLast := lcChild;
    lcChild := lcChild.NextLeafNode;
    lcLast.Parent.ExtractChild(lcLast);
  end;

end;

function EndsWithComma(const pcUsesIdentList: TParseTreeNode): boolean;
var
  lcToken: TSourceToken;
begin
  lcToken := pcUsesIdentList.LastLeaf as TSourceToken;

  if (lcToken <> nil) and (not lcToken.IsSolid) then
    lcToken := lcToken.PriorSolidToken;

  Result := (lcToken <> nil) and (lcToken.TokenType = ttComma);
end;

procedure TSortUses.SortUsesInSections(const pcUsesIdentList: TParseTreeNode);
var
  lcToken: TSourceToken;
  lcSections: TObjectList;
  lcCurrentSection: TUsesSection;
  leBreakTokens: TTokenTypeSet;

  procedure CollectWhiteSpace;
  begin
    { start with a breaking section for the starting white space and comments where they are }
    while (lcToken <> nil) and (not lcToken.IsSolid) do
    begin
      lcCurrentSection.AddToken(lcToken);
      lcToken := lcToken.NextToken;
    end;
  end;

  procedure NewSection(const pbSorted: Boolean);
  begin
    lcCurrentSection := TUsesSection.Create;
    lcCurrentSection.Sorted := pbSorted;
    lcSections.Add(lcCurrentSection);
  end;

  function IsBreakToken(const pcToken: TSourceToken): Boolean;
  begin
    Result := (pcToken.TokenType in leBreakTokens) or
      ((pcToken.TokenType = ttComment) and (pcToken.CommentStyle = eCompilerDirective));
  end;

var
  lbStartFirstSection: boolean;
  lbStartUsesItem: boolean;
  liSectionLoop: integer;
  lbEndsWithComma: boolean;
begin
  leBreakTokens := [];

  if FormattingSettings.Transform.BreakUsesSortOnReturn then
    Include(leBreakTokens, ttReturn);

  if FormattingSettings.Transform.BreakUsesSortOnComment then
    Include(leBreakTokens, ttComment);

  { sections is a list of lists
    each section is a list of parse tree nodes
    Each section is sorted }
  lcSections := TObjectList.Create;
  lcSections.OwnsObjects := True;
  lbStartFirstSection := True;
  lbStartUsesItem := True;
  lbEndsWithComma := False;
  
  try
    { at least one section, but can be more }
    NewSection(True);

    { each child node is in a section }
    lcToken := TSourceToken(pcUsesIdentList.FirstLeaf);
    if (lcToken <> nil) and (not lcToken.IsSolid) then
    begin
      lcCurrentSection.Sorted := False;

      CollectWhiteSpace;
      NewSection(True);
    end;

    while (lcToken <> nil) and lcToken.HasParentNode(nIdentList) and lcToken.HasParentNode(nUses) do
    begin
      { end of section, start of the next }
      if IsBreakToken(lcToken) and (not lbStartUsesItem) then
      begin
        { the break token is in a non-sorted section }
        NewSection(False);
        lcCurrentSection.AddToken(lcToken);
        lcToken := lcToken.NextToken;

        CollectWhiteSpace;
        NewSection(True);
        continue;
      end;

      if IsIdentifier(lcToken, idAny) and (lcToken.HasParentNode(nUsesItem, 2)) then
      begin
        if not lbStartUsesItem then
        begin
          lcCurrentSection.AddUsesItem;
          lbStartUsesItem := True;
        end;
      end
      else
      begin
        { a uses item contains exactly one identifier.
         but in Delphi.Net it can be dotted, e.g.
         "System.Runtime.Remoting"
        }
        lbStartUsesItem := False;
      end;

      lcCurrentSection.AddToken(lcToken);

      { don't end the first section until it's begun }
      if lbStartFirstSection and (not (lcToken.TokenType in NotSolidTokens)) then
        lbStartFirstSection := False;

      lcToken := lcToken.NextToken;

      if lcToken.TokenType = ttDot then
      begin
        // delphi.net dotted unit name
        while (lcToken.TokenType = ttDot) or IsIdentifier(lcToken, idAny) do
        begin
          lcCurrentSection.AddToken(lcToken);
          lcToken := lcToken.NextToken;
        end;

      end;
    end;

    { remove the unsorted tokens }
    RemoveAllChildrenOnwards(pcUsesIdentList, nil);

    { sort each section, and reattach it  }
    for liSectionLoop := 0 to lcSections.Count - 1 do
    begin
       lcCurrentSection := TUsesSection(lcSections.Items[liSectionLoop]);

      if lcCurrentSection.Sorted then
      begin

        { sort this section }
        case FormattingSettings.Transform.UsesSortOrder of
          eAlpha:
            lcCurrentSection.Sort(AlphaNameSort);
          eReverseAlpha:
            lcCurrentSection.Sort(ReverseAlphaNameSort);
          eShortToLong:
            lcCurrentSection.Sort(LengthNameSort);
          eLongToShort:
            lcCurrentSection.Sort(ReverseLengthNameSort);
          else
            Assert(False);
        end;
      end;

      {  repopulate the original parent with sorted items
        if the last one ends with a comma, this one must not start with a comma }
      lcCurrentSection.AttachTo(pcUsesIdentList, not lbEndsWithComma);

      lbEndsWithComma := EndsWithComma(pcUsesIdentList);
    end;
  finally
    lcSections.Free;
  end;

  { no comma or space before the semicolon }
  CleanEnd(pcUsesIdentList);

end;

end.
