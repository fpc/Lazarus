unit UsesClauseInsert;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is UsesClauseInsert.pas, released October 2003.
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

{ AFS 2 October 2003

  - massage the uses clause. Insert units if they aren't already there

 }

uses
  { delphi }
  Classes,
  { local }
  SourceToken,
  SwitchableVisitor;

type
  TUsesClauseInsert = class(TSwitchableVisitor)
  private
    fiCount: integer;
    fbDoneInterface, fbDoneImplementation: boolean;

    procedure SetDoneSection(const pbInterface: boolean);
    procedure AddUses(const pcToken: TSourceToken; const psList: TStrings);
    procedure CreateUses(const pcToken: TSourceToken; const pbInterface: boolean);

  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
    function FinalSummary(out psMessage: string): boolean; override;

  end;

implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfSettings,
  Tokens,
  FormatFlags,
  ParseTreeNodeType,
  ParseTreeNode,
  TokenUtils;

constructor TUsesClauseInsert.Create;
begin
  inherited;

  FormatFlags := FormatFlags + [eFindReplaceUses];

  fbDoneInterface := False;
  fbDoneImplementation := False;
  fiCount := 0;
end;

function TUsesClauseInsert.IsIncludedInSettings: boolean;
begin
  Result := (FormatSettings.UsesClause.InsertInterfaceEnabled or
    FormatSettings.UsesClause.InsertImplementationEnabled);
end;

function TUsesClauseInsert.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  lbInterface, lbImplementation: boolean;
begin
  Result := False;
  if pcNode = nil then
    exit;

  lcSourceToken := TSourceToken(pcNode);

  { end of the unit }
  if (lcSourceToken.TokenType = ttDot) and
    (lcSourceToken.HasParentNode(TopOfFileSection, 1)) then
  begin
    // any uses clauses omitted that need to be here?
    if ( not fbDoneInterface) and (FormatSettings.UsesClause.InsertInterfaceEnabled) and
      (FormatSettings.UsesClause.InsertInterface.Count > 0) then
    begin
      CreateUses(lcSourceToken, True);
    end;

    if ( not fbDoneImplementation) and
      (FormatSettings.UsesClause.InsertImplementationEnabled) and
      (FormatSettings.UsesClause.InsertImplementation.Count > 0) then
    begin
      CreateUses(lcSourceToken, False);
    end;
  end;

  { only do this in a uses clause }
  if not lcSourceToken.HasParentNode(nUses) then
    exit;

  lbInterface := lcSourceToken.HasParentNode(nInterfaceSection);
  if lbInterface then
    lbImplementation := False
  else
    lbImplementation := lcSourceToken.HasParentNode(nImplementationSection);

  if not (lbImplementation or lbInterface) then
    exit;

  if (lcSourceToken.TokenType = ttSemiColon) then
  begin
    { Reached the end - insert the rest }
    if lbInterface then
      AddUses(lcSourceToken, FormatSettings.UsesClause.InsertInterface)
    else
      AddUses(lcSourceToken, FormatSettings.UsesClause.InsertImplementation);

    SetDoneSection(lbInterface);
  end;

end;

function TUsesClauseInsert.FinalSummary(out psMessage: string): boolean;
begin
  Result := (fiCount > 0);
  if Result then
  begin
    psMessage := 'Uses clause insertion: ' + IntToStr(fiCount) + ' insertions were made';
  end
  else
  begin
    psMessage := '';
  end;
end;

procedure TUsesClauseInsert.SetDoneSection(const pbInterface: boolean);
begin
  if pbInterface then
    fbDoneInterface      := True
  else
    fbDoneImplementation := True;
end;

procedure TUsesClauseInsert.AddUses(const pcToken: TSourceToken; const psList: TStrings);
var
  liLoop: integer;
  ptNew, ptLast: TSourceToken;
begin
  ptLast := pcToken.PriorToken;

  for liLoop := 0 to psList.Count - 1 do
  begin
    ptNew := TSourceToken.Create;
    ptNew.TokenType := ttComma;
    ptNew.SourceCode := ',';

    InsertTokenAfter(ptLast, ptNew);
    ptLast := ptNew;

    ptNew := TSourceToken.Create;
    ptNew.TokenType := ttWord;
    ptNew.SourceCode := psList[liLoop];

    InsertTokenAfter(ptLast, ptNew);
    ptLast := ptNew;
  end;
end;

procedure TUsesClauseInsert.CreateUses(const pcToken: TSourceToken;
  const pbInterface: boolean);
var
  lcRoot, lcSection: TParseTreeNode;
  lcKeyWord, lcNew: TSourceToken;
  lcInserts: TStrings;
  liLoop, liInsertPos: integer;
begin
  lcRoot := pcToken.Root;

  if pbInterface then
  begin
    lcSection := lcRoot.GetImmediateChild(nInterfaceSection);
    lcInserts := FormatSettings.UsesClause.InsertInterface;
  end
  else
  begin
    lcSection := lcRoot.GetImmediateChild(nImplementationSection);
    lcInserts := FormatSettings.UsesClause.InsertImplementation;
  end;

  { could be a 'program', 'library' or 'package' file with no interface/impl sections }
  if lcSection = nil then
    exit;

  { find the 'interface' and 'implmentation' section }
  lcKeyWord := TSourceToken(lcSection.FirstLeaf);

  if pbInterface then
  begin
    while lcKeyWord.TokenType <> ttInterface do
      lcKeyWord := lcKeyWord.NextToken;
  end
  else
  begin
    while lcKeyWord.TokenType <> ttImplementation do
      lcKeyWord := lcKeyWord.NextToken;
  end;

  liInsertPos := lcSection.IndexOfChild(lcKeyWord) + 1;

  lcNew := NewSpace(1);
  lcSection.InsertChild(liInsertPos, lcNew);
  Inc(liInsertPos);

  lcNew := TSourceToken.Create;
  lcNew.FileName := pcToken.FileName;
  lcNew.TokenType := ttUses;
  lcNew.SourceCode := 'uses';

  lcSection.InsertChild(liInsertPos, lcNew);
  Inc(liInsertPos);


  for liLoop := 0 to lcInserts.Count - 1 do
  begin
    { space }
    lcNew := NewSpace(1);
    lcSection.InsertChild(liInsertPos, lcNew);
    Inc(liInsertPos);

    { uses item }
    lcNew := TSourceToken.Create;
    lcNew.FileName := pcToken.FileName;
    lcNew.TokenType := ttIdentifier;
    lcNew.SourceCode := lcInserts.Strings[liLoop];

    lcSection.InsertChild(liInsertPos, lcNew);
    Inc(liInsertPos);

    { , or ;}
    lcNew := TSourceToken.Create;
    lcNew.FileName := pcToken.FileName;

    if liLoop = (lcInserts.Count - 1) then
    begin
      lcNew.TokenType  := ttSemiColon;
      lcNew.SourceCode := ';';
    end
    else
    begin
      lcNew.TokenType  := ttComma;
      lcNew.SourceCode := ',';
    end;

    lcSection.InsertChild(liInsertPos, lcNew);
    Inc(liInsertPos);
  end;
end;

end.
