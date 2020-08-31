{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is AlignStatements.pas, released April 2000.
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
unit AlignBase;


{ AFS 6 Feb 2K
  base class Generalisation of AlignAssign and co.
  This is the base class for all aligners
}

{$I JcfGlobal.inc}

interface

uses
  SysUtils, Math,
  SwitchableVisitor, SourceToken, SourceTokenList;

type

  TAlignBase = class(TSwitchableVisitor)
  private
    fcTokens: TSourceTokenList;
    fcResumeToken: TSourceToken;

    procedure AlignTheBlock(const pcToken: TSourceToken);

    function StillSuspended(const pc: TSourceToken): boolean;
    procedure IndentAll(const piIndent: integer);

    procedure AddToken(const pcToken: TSOurceToken; const pbAligned: boolean);

  protected

    { API for descendant classes }
    function TokenIsAligned(const pt: TSourceToken): boolean; virtual; abstract;
    function TokenEndsStatement(const pt: TSourceToken): boolean; virtual; abstract;
    function IsTokenInContext(const {%H-}pt: TSourceToken): boolean; virtual;
    function TokenEndsAlignment(const {%H-}pt: TSourceToken): boolean; virtual;

      { override this to let the child class see the tokens as they come
      this is used by the align vars to detect the first non-white space token after the : }

    procedure OnTokenRead(const {%H-}pt: TSourceToken); virtual;
    procedure ResetState; virtual;

    function EnabledVisitSourceToken(const pcNode: TObject): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  { jcf } JcfSettings, TokenUtils, ParseTreeNodeType;

{ TAlignBase }

constructor TAlignBase.Create;
begin
  inherited;
  fcResumeToken := nil;
  fcTokens      := TSourceTokenList.Create;
end;


destructor TAlignBase.Destroy;
begin
  FreeAndNil(fcTokens);
  inherited;
end;


function TAlignBase.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcToken: TSourceToken;
begin
  Result := False;
  lcToken := TSourceToken(pcNode);

  if StillSuspended(lcToken) then
    exit;

  if not IsTokenInContext(lcToken) then
    exit;

  if TokenIsAligned(lcToken) then
    AlignTheBlock(lcToken);
end;


procedure TAlignBase.AlignTheBlock(const pcToken: TSourceToken);
var
  liCurrent, liLastKnownAlignedStatement: integer;
  lcCurrent:    TSourceToken;
  lbDone, lbFirst, bThisStatementIsAligned, lbThisTokenIsAligned: boolean;
  liMaxIndent, liMinIndent: integer;
  liThisIndent: integer;
  liSettingsMin, liSettingsMax, liSettingsMaxVariance: integer;

  liUnalignedCount, liMaxUnaligned: integer;
  liAlignedCount: integer;
begin
  ResetState;

  Assert(TokenIsAligned(pcToken));

  lcCurrent := pcToken;
  OnTokenRead(lcCurrent);
  AddToken(lcCurrent, True);

  liLastKnownAlignedStatement := 0;
  liMaxIndent := lcCurrent.XPosition;
  liMinIndent := liMaxIndent;

  { locate end of first statement
   BufferTokens(0) is the first :=
   there must be a semicolon soon after }

  liCurrent := 0;
  repeat
    Inc(liCurrent);
    lcCurrent := lcCurrent.NextToken;

    if lcCurrent <> nil then
    begin
      OnTokenRead(pcToken);
      AddToken(lcCurrent, False);
    end;
  until (lcCurrent = nil) or TokenEndsStatement(lcCurrent);

  { end the first statement on EOF?! - abort! }
  if (lcCurrent = nil) then
    exit;

  with FormattingSettings do
  begin
    liSettingsMin := Align.MinColumn;
    liSettingsMax := Align.MaxColumn;

    if pcToken.HasParentNode(nInterfaceSection) then
      liSettingsMaxVariance := Align.MaxVarianceInterface
    else
      liSettingsMaxVariance := Align.MaxVariance;

    liMaxUnaligned := Align.MaxUnalignedStatements;
  end;

  { locate block end - include all consecutive aligned statements }
  lbDone  := False;
  lbFirst := True;
  liUnalignedCount := 0;
  liAlignedCount := 1;
  bThisStatementIsAligned := True; // first statement just read will be aligned


  { now look for consecutive similar statements to align }
  while not lbDone do
  begin
    { EOF?! - abort! }
    if (lcCurrent = nil) then
    begin
      lbDone := True;
    end
    else
    begin
      lbThisTokenIsAligned := TokenIsAligned(lcCurrent);
      if not lbFirst then
        AddToken(lcCurrent, lbThisTokenIsAligned); // first one has been added above

      lbFirst := False;

      { an aligned statement has the aligned token in it -
        e.g. an assign statement has a ':=' in it :) }
      if lbThisTokenIsAligned then
      begin
        bThisStatementIsAligned := True;

        liThisIndent := lcCurrent.XPosition;

        if liThisIndent >= liSettingsMin then
          liMinIndent := Min(liThisIndent, liMinIndent);

        { store the higest indent in liMaxIndent
          unless it is out of bounds, ie < liSettingsMin or > liSettingsMax }
        liThisIndent := Max(liThisIndent, liSettingsMin);
        if (liThisIndent > liMaxIndent) and (liThisIndent < liSettingsMax) and
          (liThisIndent <= liMinIndent + liSettingsMaxVariance) then
          liMaxIndent := liThisIndent;

        { may need to knock down the min if the first one is an outlier }
        if (liThisIndent + liSettingsMaxVariance) < liMaxIndent then
          liMaxIndent := liThisIndent;

      end;

      if TokenEndsStatement(lcCurrent) then
      begin
        { ending a statement - was it an aligned one?
          If not, maybe we should have stopped with the last statement }

        if bThisStatementIsAligned then
        begin
          liLastKnownAlignedStatement := liCurrent;
          liUnalignedCount := 0;
          bThisStatementIsAligned := False;
          Inc(liAlignedCount);
        end
        else
        begin
          { look for consecutive unaligned statements to end the aligned block
            depending on the config, this could be just 1 unalaigned statement
            or it could be more
          }
          Inc(liUnalignedCount);
          if liUnalignedCount > liMaxUnaligned then
            lbDone := True;
        end;
      end;

      if TokenEndsAlignment(lcCurrent) then
        lbDone := True;

      Inc(liCurrent);
      lcCurrent := lcCurrent.NextToken;
      OnTokenRead(lcCurrent);
    end; { not EOF }
  end; { while loop }

  { set iResume equal to the last token aligned  }
  fcResumeToken := fcTokens.SourceTokens[fcTokens.Count - 1];

  { now we know how far to go and how far to indent, do it? }
  if (liLastKnownAlignedStatement > 0) and (liAlignedCount > 1) then
    IndentAll(liMaxIndent);

  ResetState;
end;

procedure TAlignBase.IndentAll(const piIndent: integer);
var
  liLoop: integer;
  lcCurrent, lcNew: TSourceToken;
begin
  for liLoop := fcTokens.Count - 1 downto 0 do
  begin
    lcCurrent := fcTokens.SourceTokens[liLoop];

    if (lcCurrent.UserTag > 0) and (lcCurrent.XPosition < piIndent) then
    begin
      { indent to the specified level  - make a new space token }
      lcNew := InsertSpacesBefore(lcCurrent, piIndent - lcCurrent.XPosition);
      fcTokens.Insert(liLoop, lcNew);
    end;
  end;
end;

procedure TAlignBase.OnTokenRead(const pt: TSourceToken);
begin
  // here for override
end;

procedure TAlignBase.ResetState;
begin
  fcTokens.Clear;
end;

function TAlignBase.IsTokenInContext(const pt: TSourceToken): boolean;
begin
  // here for override
  Result := True;
end;

function TAlignBase.StillSuspended(const pc: TSourceToken): boolean;
begin
  if (fcResumeToken = nil) then
  begin
    // we are not suspended, so go.
    Result := False;
  end
  else
  begin
    // have we reached the end of suspension?
    Result := (fcResumeToken <> pc);
    if not Result then
      fcResumeToken := nil;
  end;

end;

procedure TAlignBase.AddToken(const pcToken: TSourceToken; const pbAligned: boolean);
begin
  Assert(pcToken <> nil);

  if pbAligned then
    pcToken.UserTag := 1
  else
    pcToken.UserTag := 0;

  fcTokens.Add(pcToken);
end;

function TAlignBase.TokenEndsAlignment(const pt: TSourceToken): boolean;
begin
  Result := False;
end;

end.
