unit IdentifierCaps;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is IdentifierCaps, released June 2005.
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

{ AFS 30 December 2002
    - fix capitalisation on specified words
}
{$I JcfGlobal.inc}

interface

uses
  SysUtils,
  SwitchableVisitor, SourceToken, AvgLvlTree;

type
  TIdentifierCaps = class(TSwitchableVisitor)
  private
    fiIdentifierCount: integer;
    fiNonIdentifierCount: integer;
    lsLastChange: string;
    TreeIdentifiers: TAvgLvlTree;
    TreeNotIdentifiers: TAvgLvlTree;
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): boolean; override;
    // use the first ocurrence (declaration) of the identifier in the source as normalized capitalisation.
    function TreeGetNormalizedIdentifierCapitalisation(ATree:TAvgLvlTree; Aidentifier:string):string;
    procedure UpdateToken(AToken: TSourceToken; AValue:string);
  public
    constructor Create; override;
    destructor Destroy; override;

    function IsIncludedInSettings: boolean; override;
    { return true if you want the message logged}
    function FinalSummary(out psMessage: string): boolean; override;
  end;

implementation

uses
  { local }
  Tokens, ParseTreeNodeType,
  JcfSettings, FormatFlags, TokenUtils, jcfbaseConsts;


type
  TTreeNodeData = class
    lsIdentifier: string;
    lsIdentifierLowerCase: string;
  end;

function CompareTreeNodesCaseSensitive(Data1, Data2: Pointer): integer;
begin
  Result:=CompareStr(TTreeNodeData(Data1).lsIdentifierLowerCase,TTreeNodeData(Data2).lsIdentifierLowerCase);
end;

function CompareIdentifierTreeNodeCaseSensitive(AIdentifier, AData: Pointer): integer;
begin
  Result:=CompareStr(string(AIdentifier),TTreeNodeData(AData).lsIdentifierLowerCase);
end;

function TIdentifierCaps.TreeGetNormalizedIdentifierCapitalisation(ATree:TAvgLvlTree; AIdentifier:string):string;
var
  Node:TAvgLvlTreeNode;
  NodeData: TTreeNodeData;
  lsLower: string;
begin
  result := '';
  lsLower := LowerCase(AIdentifier);
  Node:=ATree.FindKey(Pointer(lsLower),@CompareIdentifierTreeNodeCaseSensitive);
  if Node <> nil then
    Result:= TTreeNodeData(Node.Data).lsIdentifier
  else
  begin
    result := AIdentifier;
    NodeData := TTreeNodeData.Create;
    NodeData.lsIdentifier := AIdentifier;
    NodeData.lsIdentifierLowerCase := lsLower;
    ATree.Add(NodeData);
  end;
end;

function Excluded(const pt: TSourceToken): boolean;
begin
  Result := False;

  { directives in context are excluded }
  if IsDirectiveInContext(pt) then
    exit(True);

  // asm has other rules
  if pt.HasParentNode(nAsm) then
    exit(True);

  { built in types that are actually being used as types are excluded
    eg.
    // this use of 'integer' is definitly the type
    var li: integer;

    // this use is definitely not
    function Integer(const ps: string): integer;

    // this use is ambigous
    li := Integer(SomeVar);

   user defined types are things that we often *want* to set a specific caps on
   so they are not excluded }
   if (not FormattingSettings.Caps.NotIdentifiersNormalizeCapitalisation) and
     (pt.TokenType in BuiltInTypes) and (pt.HasParentNode(nType)) then
     exit(True);
end;


{ TIdentifierCaps }

constructor TIdentifierCaps.Create;
begin
  inherited;
  fiIdentifierCount  := 0;
  fiNonIdentifierCount := 0;

  lsLastChange := '';
  FormatFlags  := FormatFlags + [eCapsSpecificWord];
  TreeIdentifiers := TAvgLvlTree.Create(@CompareTreeNodesCaseSensitive);
  TreeNotIdentifiers := TAvgLvlTree.Create(@CompareTreeNodesCaseSensitive);
end;

destructor TIdentifierCaps.Destroy;
begin
  TreeIdentifiers.FreeAndClear; // free nodes and objects
  TreeIdentifiers.Free;
  TreeNotIdentifiers.FreeAndClear; // free nodes and objects
  TreeNotIdentifiers.Free;
  inherited Destroy;
end;

function TIdentifierCaps.FinalSummary(out psMessage: string): boolean;
begin
  Result := False;
  psMessage := '';


  if (fiIdentifierCount > 0) then
  begin
    Result := True;
    if fiIdentifierCount = 1 then
      psMessage := Format(lisMsgOneChangeWasMade, [lisMsgIdentifierCaps, lsLastChange])
    else
      psMessage := Format(lisMsgChangesWhereMade, [lisMsgIdentifierCaps, fiIdentifierCount]);
  end;

  if (fiNonIdentifierCount > 0) then
  begin
    Result := True;
    if psMessage <> '' then
      psMessage := psMessage + '.   ';

    if fiNonIdentifierCount = 1 then
      psMessage := psMessage + Format(lisMsgOneChangeWasMade, [lisMsgNonIdentifierCaps, lsLastChange])
    else
      psMessage := psMessage + Format(lisMsgChangesWhereMade, [lisMsgNonIdentifierCaps, fiNonIdentifierCount]);
  end;

end;

procedure TIdentifierCaps.UpdateToken(AToken: TSourceToken; AValue:string);
begin
  if AnsiCompareStr(AToken.SourceCode, AValue) <> 0 then
  begin
    lsLastChange := Format(lisMsgTo, [AToken.SourceCode, AValue]);
    AToken.SourceCode := AValue;
    Inc(fiNonIdentifierCount);
  end;
end;

function TIdentifierCaps.EnabledVisitSourceToken(const pcNode: TObject): boolean;
const
  NEITHER_NOR = [ttReturn, ttComment, ttWhiteSpace, ttConditionalCompilationRemoved, ttNumber, ttQuotedLiteralString];
var
  lcSourceToken: TSourceToken;
  lcTreeNonIdentifiers: TAvgLvlTree;
begin
  Result := False;

  lcSourceToken := TSourceToken(pcNode);

  { these tokens are common and/or long,
   but are not capitalisable text at all, don't waste time on them }
  if lcSourceToken.TokenType in NEITHER_NOR then
    exit;

  if Excluded(lcSourceToken) then
    exit;

  if FormattingSettings.Caps.NormalizeCapitalisationOneNamespace then
    lcTreeNonIdentifiers := TreeIdentifiers
  else
    lcTreeNonIdentifiers := TreeNotIdentifiers;

  if lcSourceToken.HasParentNode(nIdentifier, 2) then
  begin
    // it's an identifier
    if FormattingSettings.IdentifierCaps.Enabled then
    begin
      if FormattingSettings.IdentifierCaps.HasWord(lcSourceToken.SourceCode) then
        UpdateToken(lcSourceToken, FormattingSettings.IdentifierCaps.CapitaliseWord(lcSourceToken.SourceCode))
      else
      begin
        if FormattingSettings.Caps.Enabled and FormattingSettings.Caps.IdentifiersNormalizeCapitalisation then
          UpdateToken(lcSourceToken, TreeGetNormalizedIdentifierCapitalisation(TreeIdentifiers, lcSourceToken.SourceCode));
      end;
    end
    else
    begin
      if FormattingSettings.Caps.Enabled and FormattingSettings.Caps.IdentifiersNormalizeCapitalisation then
        UpdateToken(lcSourceToken, TreeGetNormalizedIdentifierCapitalisation(TreeIdentifiers, lcSourceToken.SourceCode));
    end;
  end
  else
  begin
    // it's not an identifier 
    if FormattingSettings.NotIdentifierCaps.Enabled then
    begin
      if FormattingSettings.NotIdentifierCaps.HasWord(lcSourceToken.SourceCode) then
        UpdateToken(lcSourceToken, FormattingSettings.NotIdentifierCaps.CapitaliseWord(lcSourceToken.SourceCode))
      else
      begin
        if FormattingSettings.Caps.Enabled and FormattingSettings.Caps.NotIdentifiersNormalizeCapitalisation then
          UpdateToken(lcSourceToken, TreeGetNormalizedIdentifierCapitalisation(lcTreeNonIdentifiers, lcSourceToken.SourceCode));
      end;
    end
    else
    begin
      if FormattingSettings.Caps.Enabled and FormattingSettings.Caps.NotIdentifiersNormalizeCapitalisation then
        UpdateToken(lcSourceToken, TreeGetNormalizedIdentifierCapitalisation(lcTreeNonIdentifiers, lcSourceToken.SourceCode));
    end;
  end;
end;

function TIdentifierCaps.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.IdentifierCaps.Enabled or FormattingSettings.NotIdentifierCaps.Enabled or
    (FormattingSettings.Caps.Enabled and (FormattingSettings.Caps.IdentifiersNormalizeCapitalisation or FormattingSettings.Caps.NotIdentifiersNormalizeCapitalisation));
end;

end.
