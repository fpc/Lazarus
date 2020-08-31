unit SortUsesData;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is SortUsesData.pas, released September 2004.
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
  Contnrs, Classes, Math, SysUtils,
  { local }
  SourceToken, ParseTreeNode;

{ some classes used in sorting a uses clause }

type
  { stores a uses unit name and some trailing tokens }
  TUsesItem = class(TObject)
  private
    fcItems: TObjectList;
    
    function GetItem(const piIndex: integer): TSourceToken;

  public
    constructor Create;
    destructor Destroy; override;

    function Count: integer;
    function SortKey: string;
    function Text: string;
    function IndentifierIndex: integer;

    procedure Add(const pcItem: TSourceToken);
    procedure AttachTo(const pcNode: TParseTreeNode; const pbAllowCommaFirst: boolean);

    property Items[const piIndex: integer]: TSourceToken read GetItem;
  end;

  { a section is one of two things
    - It is a list of uses items, to be sorted
    - breaking tokens between these lists. Not sorted  }
  TUsesSection = class(TObject)
  private
    fbSorted: boolean;
    fcItems: TObjectList;

    function GetItem(const piIndex: integer): TUsesItem;
  public
    constructor Create;
    destructor Destroy; override;

    function Count: integer;
    function Text: string;

    procedure Sort(Compare: TListSortCompare);

    procedure AddUsesItem;
    procedure AddToken(const pcItem: TSourceToken);
    procedure AttachTo(const pcNode: TParseTreeNode; pbAllowCommaFirst: boolean);

    property Sorted: boolean read fbSorted write fbSorted;

    property Items[const piIndex: integer]: TUsesItem read GetItem;
  end;

function AlphaNameSort(Item1, Item2: Pointer): integer;
function ReverseAlphaNameSort(Item1, Item2: Pointer): integer;
function LengthNameSort(Item1, Item2: Pointer): integer;
function ReverseLengthNameSort(Item1, Item2: Pointer): integer;


implementation

uses
  {local }
  ParseTreeNodeType, Tokens, TokenUtils;

function AlphaNameSort(Item1, Item2: Pointer): integer;
var
  lcItem1, lcItem2: TUsesItem;
begin
  lcItem1 := TUsesItem(Item1);
  lcItem2 := TUsesItem(Item2);

  if lcItem1 = nil then
    Result := -1
  else if lcItem2 = nil then
    Result := 1
  else
  begin
    Result := AnsiCompareText(lcItem1.SortKey, lcItem2.SortKey);
  end;
end;

function ReverseAlphaNameSort(Item1, Item2: Pointer): integer;
begin
  Result := AlphaNameSort(Item1, Item2) * -1;
end;

function LengthNameSort(Item1, Item2: Pointer): integer;
var
  lcItem1, lcItem2: TUsesItem;
begin
  lcItem1 := TUsesItem(Item1);
  lcItem2 := TUsesItem(Item2);

  if lcItem1 = nil then
    Result := -1
  else if lcItem2 = nil then
    Result := 1
  else
  begin
    Result := Sign(Length(lcItem1.SortKey) - Length(lcItem2.SortKey));

    { Length has more sort collisions that alphabetic
      same length -> Alphabetic sort }
    if Result = 0 then
      Result := AnsiCompareText(lcItem1.SortKey, lcItem2.SortKey);
  end;
end;

function ReverseLengthNameSort(Item1, Item2: Pointer): integer;
var
  lcItem1, lcItem2: TUsesItem;
begin
  lcItem1 := TUsesItem(Item1);
  lcItem2 := TUsesItem(Item2);

  if lcItem1 = nil then
    Result := -1
  else if lcItem2 = nil then
    Result := 1
  else
  begin
    Result := Sign(Length(lcItem2.SortKey) - Length(lcItem1.SortKey));

    { Soting the other way by length, but still forwards alphbetically }
    if Result = 0 then
      Result := AnsiCompareText(lcItem1.SortKey, lcItem2.SortKey);
  end;
end;


{-------------------------------------------------------------------------------
  TUsesItem }


constructor TUsesItem.Create;
begin
  inherited;
  fcItems := TObjectList.Create;
  // tokens are just referred here, owned by the parse treee
  fcItems.OwnsObjects := False;
end;

destructor TUsesItem.Destroy;
begin
  FreeAndNil(fcItems);
  inherited;
end;
procedure TUsesItem.Add(const pcItem: TSourceToken);
begin
  fcItems.Add(pcItem);
end;

function TUsesItem.Count: integer;
begin
  Result := fcItems.Count;
end;

function TUsesItem.GetItem(const piIndex: integer): TSourceToken;
begin
  Result := TSourceToken(fcItems[piIndex]);
end;

function TUsesItem.SortKey: string;
var
  liLoop: integer;
  lcTest: TSourceToken;
begin
  Result := '';

  // find the identifiers
  for liLoop := 0 to Count - 1 do
  begin
    lcTest := Items[liLoop];
    if lcTest.TokenType in [ttIdentifier, ttDot] then
    begin
      Result := Result + lcTest.SourceCode;
    end;
  end;
end;

function TUsesItem.Text: string;
var
  liLoop: integer;
  lcTest: TSourceToken;
begin
  Result := '';

  for liLoop := 0 to Count - 1 do
  begin
    lcTest := Items[liLoop];
    Result := Result + lcTest.SourceCode;
  end;
end;


function TUsesItem.IndentifierIndex: integer;
var
  liLoop: integer;
  lcTest: TSourceToken;
begin
  Result := -1;

  // find the first identifier
  for liLoop := 0 to Count - 1 do
  begin
    lcTest := Items[liLoop];
    if lcTest.TokenType = ttIdentifier then
    begin
      Result := liLoop;
      break;
    end;
  end;
end;

procedure TUsesItem.AttachTo(const pcNode: TParseTreeNode; const pbAllowCommaFirst: boolean);
var
  lbHasSolid: boolean;

  procedure AddToken(const pcToken: TSourceToken);
  begin
    // skip the comma if it's the first token and asked to do so
    if (not lbHasSolid) and (not pbAllowCommaFirst) and
    (pcToken.TokenType = ttComma) then
      exit; 

    if (not lbHasSolid) and pcToken.IsSolid then
      lbHasSolid := True;

    pcNode.AddChild(pcToken);
  end;

var
  liIndentifierIndex: integer;
  lcUsesItem: TParseTreeNode;
  lcIdent: TParseTreeNode;
  lcToken: TSourceToken;
  liloop: integer;
  lbHasComma: Boolean;
  lcComma: TSourceToken;
  lsFileName: string;

begin
  if Count < 1 then
    exit;

  Assert(pcNode <> nil);
  liIndentifierIndex := IndentifierIndex;
  lbHasSolid := False;
  lsFileName := '';

  if liIndentifierIndex < 0 then
  begin
    for liLoop := 0 to Count - 1 do
    begin
      lcToken := Items[liLoop];
      lsFileName := lcToken.FileName;
      AddToken(lcToken);
    end;
  end
  else
  begin
    for liLoop := 0 to liIndentifierIndex - 1 do
    begin
      lcToken := Items[liLoop];
      lsFileName := lcToken.FileName;
      AddToken(lcToken);
    end;

    // attach the data back onto the parent node, a uses clause
    lbHasSolid := True;
    lcUsesItem := TParseTreeNode.Create;
    lcUsesItem.NodeType := nUsesItem;

    pcNode.AddChild(lcUsesItem);

    lcIdent := TParseTreeNode.Create;
    lcIdent.NodeType := nUsesItem;

    lcUsesItem.AddChild(lcIdent);

    // add the identifier and trailing tokens
    lcIdent.AddChild(Items[liIndentifierIndex]);

    // add trailing tokens
    lbHasComma := False;
    for liLoop := liIndentifierIndex + 1 to Count - 1 do
    begin
      lcToken := Items[liLoop];
      if lcToken.TokenType = ttComma then
        lbHasComma := True;
      AddToken(lcToken);
    end;

    { needs to end with a comma }
    if not lbHasComma then
    begin
      lcComma := TSourceToken.Create;
      lcComma.FileName := lsFileName;

      lcComma.SourceCode := ',';
      lcComma.TokenType := ttComma;

      AddToken(lcComma);

      AddToken(NewSpace(1));
    end;
  end;
end;


{------------------------------------------------------------------------------
   TUsesSection }

constructor TUsesSection.Create;
begin
  inherited;
  fcItems := TObjectList.Create;
  // tokens are just referred here, owned by the parse treee
  fcItems.OwnsObjects := False;
end;

destructor TUsesSection.Destroy;
begin
  FreeAndNil(fcItems);
  inherited;
end;

procedure TUsesSection.AddUsesItem;
var
  lcNewItem: TUsesItem;
begin
  lcNewItem := TUsesItem.Create;
  fcItems.Add(lcNewItem);
end;

function TUsesSection.Count: integer;
begin
  Result := fcItems.Count;
end;

function TUsesSection.GetItem(const piIndex: integer): TUsesItem;
begin
  Result := TUsesItem(fcItems.Items[piIndex]);
end;

procedure TUsesSection.AddToken(const pcItem: TSourceToken);
begin
  if Count = 0 then
    AddUsesItem;

  // add to the last uses item
  Items[Count - 1].Add(pcItem);
end;


procedure TUsesSection.Sort(Compare: TListSortCompare);
begin
  if Sorted then
    fcItems.Sort(Compare);
end;

function TUsesSection.Text: string;
var
  liItemLoop: integer;
  lcItem: TusesItem;
begin
  Result := '';

  for liItemLoop := 0 to Count - 1 do
  begin
    lcItem := Items[liItemLoop];
    Result := Result + lcItem.Text;
  end;

end;

procedure TUsesSection.AttachTo(const pcNode: TParseTreeNode; pbAllowCommaFirst: boolean);
var
  liItemLoop: integer;
  lcItem: TusesItem;
  lcLastToken, lcNextToken: TSourceToken;
begin
  for liItemLoop := 0 to Count - 1 do
  begin
    lcItem := Items[liItemLoop];
    lcItem.AttachTo(pcNode, pbAllowCommaFirst);
    pbAllowCommaFirst := True; // set true fater first one

    // space it?
    if Sorted and (liItemLoop < (Count - 1)) and (lcItem.Count > 0) then
    begin
      lcLastToken := lcItem.Items[lcItem.Count - 1];
      if lcLastToken.TokenType = ttComma then
      begin
        lcNextToken := Items[liItemLoop + 1].Items[0];

        if lcNextToken.IsSolid then
          pcNode.AddChild(NewSpace(1));
      end;
    end;
  end
end;

end.
