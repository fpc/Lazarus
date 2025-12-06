{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.


Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}

unit SynPluginExternalLink;

{$mode objfpc}{$H+}

interface

uses SynEdit, SynEditMarkupCtrlMouseLink, SynEditTypes, Graphics, SysUtils, Classes;

type

  { TSynPluginExternalLink }

  TSynPluginExternalLink = class(TLazSynEditPlugin)
  private
    //FMarkupInfo : TSynHighlighterAttributesModifier; // for chaching colors, while TSynEditMarkupCtrlMouseLink = nil
    FMarkupLink: TSynEditMarkupCtrlMouseLink;
    procedure DoSearchLinkInfo(ASender: TObject; ALogXY: TLogPoint; var AnInfoState: TSynMarkupLinkInfoResult;
      var ALinkData: TSynMarkupLinkInfo);
  protected
    procedure SetEditor(const AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
  //public
  //  constructor Create(AOwner: TComponent); override;
  //  destructor Destroy; override;
  end;


implementation

function StartBy(aText: pchar; aStartBy: string): boolean;
var
  Ch: pchar;
begin
  Ch := @aStartBy[1];
  while (Ch^ <> #0) and (LowerCase(aText^) = Ch^) do begin
    Inc(Ch);
    Inc(aText);
  end;
  Result := Ch^ = #0;
end;

{ TSynPluginExternalLink }

procedure TSynPluginExternalLink.DoSearchLinkInfo(ASender: TObject; ALogXY: TLogPoint;
  var AnInfoState: TSynMarkupLinkInfoResult; var ALinkData: TSynMarkupLinkInfo);
const
  MAX_LINK_LEN = 300;
var
  aLinkStart, aLinkLength: integer;
  Len: integer;
  Ch, ChPrefix: pchar;
  Index: integer;
  Lim: integer;
  EndChars: string;
  Line: string;
  PosX: integer;
  Found: boolean;
begin
  AnInfoState := liNoData;

  Line := Editor.Lines[ALogXY.Y - 1];
  Len := Length(Line);
  PosX := ALogXY.X;
  aLinkStart := 0;
  aLinkLength := 0;
  if (Len < 4) or (PosX > Len) or (PosX < 1) then
    Exit;
  Index := PosX;
  Lim := PosX - MAX_LINK_LEN;
  if Lim < 1 then
    Lim := 1;
  Ch := @Line[PosX];

  while (aLinkStart = 0) and (Index >= Lim) do begin
    // http://  https:// file://  www.
    if (Ch^ = 'h') or (Ch^ = 'H') then
    begin
      if StartBy(Ch, 'https://') or StartBy(Ch, 'http://') then
        aLinkStart := Index;
    end
    else if (Ch^ = 'f') or (Ch^ = 'F') then begin
      if StartBy(Ch, 'file://') or StartBy(Ch, 'ftp://') then
        aLinkStart := Index;
    end
    else if (Ch^ = 'w') or (Ch^ = 'W') then begin
      if StartBy(Ch, 'www.') then begin
        aLinkStart := Index;
        // maybe have https:// or http:// before www.
        Found := False;
        if (Index - 8) >= Lim then begin
          ChPrefix:= Ch -8;
          if StartBy(ChPrefix, 'https://') then begin
            Dec(Index,8);
            aLinkStart := Index;
            Ch := ChPrefix;
            Found := true;
          end;
        end;
        if not Found and ((Index - 7) >= Lim) then begin
          ChPrefix:= Ch - 7;
          if StartBy(ChPrefix, 'http://') then begin
            Dec(Index,7);
            aLinkStart := Index;
            Ch := ChPrefix;
          end;
        end
      end;
    end;
    Dec(Index);
    Dec(Ch);
  end;
  if aLinkStart > 0 then begin
    if (Index >= 1) and ((Ch^ = '"') or (Ch^ = '''')) then
      EndChars := Ch^ + #9#0
    else if (Index >= 1) and  (Ch^ in ['a'..'z', 'A'..'Z', '0'..'9',  '_', '@','&']) then begin
      aLinkStart := 0;
      exit;
    end
    else
      EndChars := ' ' + #9#0;
    Inc(Ch);
    Inc(Index);
    while (Index <= Len) and (Pos(Ch^, EndChars) < 1) and
      (aLinkLength <= MAX_LINK_LEN) do begin
      Inc(Ch);
      Inc(Index);
      Inc(aLinkLength);
    end;
  end;
  if (PosX >= aLinkStart) and (PosX <= aLinkStart + aLinkLength) then begin
    ALinkData.StartPos := point(aLinkStart, ALogXY.Y);
    ALinkData.EndPos   := point(aLinkStart+aLinkLength, ALogXY.Y);
    ALinkData.IsLinkable := True;
    AnInfoState := liValid;
  end;
end;

procedure TSynPluginExternalLink.SetEditor(const AValue: TCustomSynEdit);
begin
  if (Editor <> nil) and (AValue <> nil) then
    raise Exception.Create('Not allowed to change editor');
  inherited SetEditor(AValue);
end;

procedure TSynPluginExternalLink.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  AValue.MarkupManager.RemoveMarkUp(FMarkupLink);
  FreeAndNil(FMarkupLink);
  inherited DoEditorRemoving(AValue);
end;

procedure TSynPluginExternalLink.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  FMarkupLink := TSynEditMarkupCtrlMouseLink.Create(AValue);
  AValue.MarkupManager.AddMarkUp(FMarkupLink);
  FMarkupLink.OnGetLinkInfo := @DoSearchLinkInfo;

  FMarkupLink.MarkupInfo.Style := [fsUnderline]; // TODO
end;

end.

