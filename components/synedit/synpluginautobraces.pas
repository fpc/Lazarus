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

unit SynPluginAutoBraces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynEditTypes, LazSynEditText, SynEditKeyCmds, SynEditTextBuffer,
  LazEditMatchingBracketUtils, LazEditMiscProcs, LazEditHighlighter, LCLType;

type

  TSynPluginAutoBraceMode = (
    abInsertClose,
    abSkipClose
  );
  TSynPluginAutoBraceModes = set of TSynPluginAutoBraceMode;

  { TSynPluginAutoBraces }

  TSynPluginAutoBraces = class(TLazSynEditPlugin)
  private
    FBracketOpenChars, FBracketCloseChars: Tcharset;
    FCurLines: TSynEditStringsLinked;
    FModes: TSynPluginAutoBraceModes;
    FFilterCloseTokens, FFilterOpenTokens: string;
    FFilterCloseSingle, FFilterCloseMulti,
    FFilterOpenSingle, FFilterOpenMulti: string;
    procedure AddToLines(ALines: TSynEditStringsLinked);
    procedure RemoveFromLines(ALines: TSynEditStringsLinked);
    procedure HookEditor;
    procedure DoBufferChanged(Sender: TObject);
    procedure DoHighlighterChanged(pSender: TSynEditStrings; pIndex, pCount: Integer);
    procedure SetModes(AValue: TSynPluginAutoBraceModes);
    procedure SetFilterCloseTokens(AValue: string);
    procedure SetFilterOpenTokens(AValue: string);

    procedure DoPreSynCommand(Sender: TObject; AfterProcessing: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);
    procedure DoPostSynCommand(Sender: TObject; AfterProcessing: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);
  protected
    procedure SetEditor(const AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Modes: TSynPluginAutoBraceModes read FModes write SetModes default [abInsertClose, abSkipClose];
    (* Filter:
       - if empty then there is no filter
       - otherwise a list of all brackets/quotes that are handled (any bracket not in the list, is not acted on)
       - each single char(byte) in the list, matches a bracket (only brackets of length=1)
       - for multi-char brackets, the token must be preceded by a space (and runs to the next space, or end of string)
       => once a space is encountered all tokens must be space separated
    *)
    property FilterOpenTokens: string read FFilterOpenTokens write SetFilterOpenTokens;
    property FilterCloseTokens: string read FFilterCloseTokens write SetFilterCloseTokens;
  end;

implementation

{ TSynPluginAutoBraces }

procedure TSynPluginAutoBraces.AddToLines(ALines: TSynEditStringsLinked);
begin
  if ALines = nil then exit;

  ALines.AddNotifyHandler(senrTextBufferChanged, @DoBufferChanged);
  ALines.AddChangeHandler(senrHighlightChanged, @DoHighlighterChanged);
  FCurLines := ALines;
end;

procedure TSynPluginAutoBraces.RemoveFromLines(ALines: TSynEditStringsLinked);
begin
  if ALines = nil then exit;

  ALines.RemoveChangeHandler(senrHighlightChanged, @DoHighlighterChanged);
  ALines.RemoveNotifyHandler(senrTextBufferChanged, @DoBufferChanged);
  FCurLines := nil;
end;

procedure TSynPluginAutoBraces.HookEditor;
begin
  if Editor <> nil then begin
    if abInsertClose in Modes then
      Editor.RegisterCommandHandler(@DoPostSynCommand, nil, [hcfPostExec])
    else
      Editor.UnregisterCommandHandler(@DoPostSynCommand);

    if abSkipClose in Modes then
      Editor.RegisterCommandHandler(@DoPreSynCommand, nil, [hcfPreExec])
    else
      Editor.UnregisterCommandHandler(@DoPreSynCommand);
  end;
end;

procedure TSynPluginAutoBraces.DoBufferChanged(Sender: TObject);
begin
  RemoveFromLines(FCurLines);
  AddToLines(ViewedTextBuffer);
end;

procedure TSynPluginAutoBraces.DoHighlighterChanged(pSender: TSynEditStrings; pIndex,
  pCount: Integer);
var
  i: Integer;
begin
  if pIndex < 0 then begin
    FBracketOpenChars := GetBracketCharSet(Editor.Highlighter, [bcfOnlyOpen]);
    FBracketCloseChars := GetBracketCharSet(Editor.Highlighter, [bcfOnlyClose]);
    for i := 1 to Length(FFilterCloseTokens) do
      Include(FBracketCloseChars, FFilterCloseTokens[i]);
  end;
end;

procedure TSynPluginAutoBraces.SetModes(AValue: TSynPluginAutoBraceModes);
begin
  if FModes = AValue then Exit;
  FModes := AValue;
  HookEditor;
end;

procedure TSynPluginAutoBraces.SetFilterCloseTokens(AValue: string);
var
  i: SizeInt;
begin
  if trim(AValue) = '' then
    AValue := '';
  if FFilterCloseTokens = AValue then Exit;
  FFilterCloseTokens := AValue;

  i := pos(' ', AValue);
  if i<1 then i := Length(AValue) + 1;
  FFilterCloseSingle := copy(AValue, 1, i-1);
  FFilterCloseMulti  := copy(AValue, i, Length(AValue))+' ';

  DoHighlighterChanged(nil, -1,-1);
end;

procedure TSynPluginAutoBraces.SetFilterOpenTokens(AValue: string);
var
  i: SizeInt;
begin
  if trim(AValue) = '' then
    AValue := '';
  if FFilterOpenTokens = AValue then Exit;
  FFilterOpenTokens := AValue;

  i := pos(' ', AValue);
  if i<1 then i := Length(AValue) + 1;
  FFilterOpenSingle := copy(AValue, 1, i-1);
  FFilterOpenMulti  := copy(AValue, i, Length(AValue))+' ';
end;

procedure TSynPluginAutoBraces.DoPreSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
var
  p, p2: TPoint;
  fnd: TLazEditBracketInfo;
  s: String;
begin
  if (Command <> ecChar) or not(AChar[1] in FBracketCloseChars) then
    exit;

  p := Editor.LogicalCaretXY;
  p2 := p;
  p2.X := p2.X + Length(AChar);
  if (Editor.TextBetweenPoints[p, p2] = AChar) then begin
    if GetBracketInfoAt(ToIdx(p.Y), p.X, Editor.Highlighter, fnd, bsdRightOrPartRight, ViewedTextBuffer)
    then begin
      s := GetBracketToken(fnd.BracketKind, False, Editor.Highlighter);
      if (FFilterCloseTokens <> '') and
         (Pos(' '+s+' ', FFilterCloseMulti) < 1) and
         ( (Length(s) > 1) or (Pos(s, FFilterCloseSingle) < 1) )
      then
        exit;

      if fnd.BracketFlags * [bfUniform, bfOpen] = [] then begin
        Handled := True;
        Editor.LogicalCaretXY := p2;
      end;
    end
    else begin // not a bracket
      if (FFilterCloseTokens <> '') and
         ( (Pos(' '+AChar+' ', FFilterCloseMulti) > 0) or
           (Pos(AChar, FFilterCloseSingle) > 0) )
      then begin
        Handled := True;
        Editor.LogicalCaretXY := p2;
      end;
    end;
  end;
end;

procedure TSynPluginAutoBraces.DoPostSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
var
  fnd: TLazEditBracketInfo;
  i: integer;
  s, l: String;
  p: TPoint;
begin
  if (Command <> ecChar) or not(AChar[1] in FBracketOpenChars) then
    exit;

  if GetBracketInfoAt(ToIdx(CaretObj.LinePos), CaretObj.BytePos,
                      Editor.Highlighter, fnd, bsdLeft, ViewedTextBuffer
     ) and
     (bfOpen in fnd.BracketFlags)
  then begin
    p := Editor.LogicalCaretXY;

    s := GetBracketToken(fnd.BracketKind, True, Editor.Highlighter);
    if (FFilterOpenTokens <> '') and
       (Pos(' '+s+' ', FFilterOpenMulti) < 1) and
       ( (Length(s) > 1) or (Pos(s, FFilterOpenSingle) < 1) )
    then
      exit;

    s := GetBracketToken(fnd.BracketKind, False, Editor.Highlighter);
    if (fnd.BracketLogLength > 1)
    then begin
      for i := Length(s)-1 downto 1 do begin
        l := Editor.Lines[ToIdx(p.Y)];
        if (strlcomp(@l[p.X], @s[1+Length(s)-i], i) = 0) and
           (GetBracketTokenIndex(copy(s, 1 + Length(s)-i, Length(s)), False, Editor.Highlighter) >= 0)
        then begin
          s := copy(s, 1, Length(s)-i);
          break;
        end;
      end;
    end;

    Editor.SetTextBetweenPoints(p, p, s, [], scamBegin);
  end;
end;

procedure TSynPluginAutoBraces.SetEditor(const AValue: TCustomSynEdit);
begin
  if (Editor <> nil) and (AValue <> nil) then
    raise Exception.Create('Not allowed to change editor');
  inherited SetEditor(AValue);
end;

procedure TSynPluginAutoBraces.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  if Editor <> nil then begin
    Editor.UnregisterCommandHandler(@DoPostSynCommand);
    Editor.UnregisterCommandHandler(@DoPreSynCommand);
  end;
  RemoveFromLines(ViewedTextBuffer);
  inherited DoEditorRemoving(AValue);
end;

procedure TSynPluginAutoBraces.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  AddToLines(ViewedTextBuffer);
  HookEditor;
  DoHighlighterChanged(nil, -1, -1);
end;

constructor TSynPluginAutoBraces.Create(AOwner: TComponent);
begin
  FModes := [abInsertClose, abSkipClose];
  inherited Create(AOwner);
end;

destructor TSynPluginAutoBraces.Destroy;
begin
  inherited Destroy;
  Editor := nil;
end;

end.

