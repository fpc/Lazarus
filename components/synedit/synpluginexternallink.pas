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

uses
  Graphics, LCLIntf, Clipbrd, SysUtils, Classes,
  SynEdit, SynEditMarkupCtrlMouseLink, SynEditTypes, SynEditHighlighter, SynEditMouseCmds,
  LazEditTextAttributes, SynEditStrConst;

const
  emcPluginExternalLinkDefaultOpen = emcPluginExternalLink + 0;
  emcPluginExternalLinkCopyToClip  = emcPluginExternalLink + 1;
  emcPluginExternalLinkSelect      = emcPluginExternalLink + 2;

  emcPluginExternalLink_Last      = emcPluginExternalLink + 2;

type

  { TSynPluginExternalLinkMouseActions }

  TSynPluginExternalLinkMouseActions = class(TSynEditMouseActions)
  public
    procedure ResetDefaults; override;
  end;

  TSynPluginExternalLink = class;

  TSynPluginExternalLinkOnLinkOpen = procedure(Sender: TSynPluginExternalLink;
    var ALinkText: string; var AHandled: Boolean);

  { TSynPluginExternalLink }

  TSynPluginExternalLink = class(TLazSynEditPlugin, IFPObserver)
  private
    FEnabled: boolean;
    FMarkupInfo: TSynHighlighterAttributesModifier; // for caching colors, while TSynEditMarkupMouseLink = nil
    FMarkupLink: TSynEditMarkupMouseLink;
    FMouseActions: TSynPluginExternalLinkMouseActions;
    FOnLinkOpen: TSynPluginExternalLinkOnLinkOpen;
    // FPOObservedChanged: MouseActions changed
    procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
    function GetCurrentLinkText: string;
    procedure SetEnabled(AValue: boolean);
    procedure DoMarkupInfoChanged(Sender: TObject);
    procedure DoGetShiftStateInfo(ASender: TObject; AShift: TShiftState; out AShouldShow,
      ACanClick: boolean);
    procedure SetMarkupInfo(AValue: TSynHighlighterAttributesModifier);
    function DoHandleMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): boolean;
    function DoMouseActionSearch(var AnInfo: TSynEditMouseActionInfo;
      HandleActionProc: TSynEditMouseActionHandler): Boolean;
    procedure ExecuteOpenLink;
    procedure DoSearchLinkInfo(ASender: TObject; ALogXY: TLogPoint; var AnInfoState: TSynMarkupLinkInfoResult;
      var ALinkData: TSynMarkupLinkInfo);
  protected
    procedure SetEditor(const AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Enabled: boolean read FEnabled write SetEnabled;
    property CurrentLinkText: string read GetCurrentLinkText;
    property MarkupInfo : TSynHighlighterAttributesModifier read FMarkupInfo write SetMarkupInfo;
    property MouseActions: TSynPluginExternalLinkMouseActions read FMouseActions;
    property OnLinkOpen: TSynPluginExternalLinkOnLinkOpen read FOnLinkOpen write FOnLinkOpen;
  end;


procedure Register;

implementation

const
  SynMouseCommandNames: array [0..2] of TIdentMapEntry = (
    (Value: emcPluginExternalLinkDefaultOpen; Name: 'emcPluginExternalLinkDefaultOpen'),
    (Value: emcPluginExternalLinkCopyToClip;  Name: 'emcPluginExternalLinkCopyToClip'),
    (Value: emcPluginExternalLinkSelect;      Name: 'emcPluginExternalLinkSelect')
  );

function SynMouseCmdToIdent(SynMouseCmd: Longint; var Ident: String): Boolean;
begin
  Ident := '';
  Result := IntToIdent(SynMouseCmd, Ident, SynMouseCommandNames);
end;

function IdentToSynMouseCmd(const Ident: string; var SynMouseCmd: Longint): Boolean;
begin
  SynMouseCmd := 0;
  Result := IdentToInt(Ident, SynMouseCmd, SynMouseCommandNames);
end;

procedure GetEditorMouseCommandValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := Low(SynMouseCommandNames) to High(SynMouseCommandNames) do
    Proc(SynMouseCommandNames[I].Name);
end;

function MouseCommandName(emc: TSynEditorMouseCommand): String;
begin
  case emc of
    emcPluginExternalLinkDefaultOpen:   Result := SYNS_emcPluginExtLinkOpenExternalLink;
    emcPluginExternalLinkCopyToClip:    Result := SYNS_emcPluginExtLinkCopyExternalLink;
    emcPluginExternalLinkSelect:        Result := SYNS_emcPluginExtLinkSelectExternalLink;
    else
      Result := '';
  end;
end;

function MouseCommandConfigName(emc: TSynEditorMouseCommand): String;
begin
  case emc of
    emcPluginExternalLinkDefaultOpen
    ..emcPluginExternalLinkSelect: Result := SYNS_emcMouseLink_opt;
    else
      Result := '';
  end;

end;

procedure Register;
begin
  RegisterMouseCmdNameAndOptProcs(@MouseCommandName, @MouseCommandConfigName);
end;

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

{ TSynPluginExternalLinkMouseActions }

procedure TSynPluginExternalLinkMouseActions.ResetDefaults;
begin
  BeginUpdate;
  try
    Clear;
    AddCommand(emcPluginExternalLinkDefaultOpen, False, mbXLeft, ccSingle, cdUp, [ssCtrl], [ssShift,ssCtrl,ssAlt]);
  finally
    EndUpdate;
  end;
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

procedure TSynPluginExternalLink.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if FMarkupLink <> nil then
    FMarkupLink.ClearShiftStateCache;
end;

function TSynPluginExternalLink.GetCurrentLinkText: string;
begin
  Result := '';
  if (FMarkupLink = nil) or (FMarkupLink.LinkStartPos.Y < 0) then
    exit;

  Result := Editor.TextBetweenPoints[FMarkupLink.LinkStartPos, FMarkupLink.LinkEndPos];
end;

procedure TSynPluginExternalLink.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;

  if FMarkupLink <> nil then
    FMarkupLink.Enabled := FEnabled;
end;

procedure TSynPluginExternalLink.DoMarkupInfoChanged(Sender: TObject);
begin
  if FMarkupLink <> nil then
    FMarkupLink.MarkupInfo.Assign(FMarkupInfo);
end;

function TSynPluginExternalLink.DoHandleMouseAction(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): boolean;
var
  s: String;
begin
  Result := False;
  if (FMarkupLink = nil) or (not FMarkupLink.IsMouseOverLink) then
    exit;

  case AnAction.Command of
    emcPluginExternalLinkDefaultOpen: begin
        ExecuteOpenLink;
        Result := True;
      end;
    emcPluginExternalLinkCopyToClip: begin
        s := CurrentLinkText;
        if s <> '' then
          Clipboard.AsText := CurrentLinkText;
        Result := True;
      end;
    emcPluginExternalLinkSelect: begin
        if (FMarkupLink = nil) or (FMarkupLink.LinkStartPos.Y >= 0) then begin
          Editor.LogicalCaretXY := FMarkupLink.LinkEndPos;
          Editor.BlockBegin := FMarkupLink.LinkStartPos;
          Editor.BlockEnd   := FMarkupLink.LinkEndPos;
          AnInfo.CaretDone     := True;
          AnInfo.IgnoreUpClick := True;
        end;
        Result := True;
      end;
  end;
end;

function TSynPluginExternalLink.DoMouseActionSearch(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := (FMarkupLink <> nil) and (FMarkupLink.Enabled) and (FMarkupLink.IsMouseOverLink);
  if Result then
    Result := HandleActionProc(FMouseActions, AnInfo);
end;

procedure TSynPluginExternalLink.ExecuteOpenLink;
var
  s: String;
  Done: Boolean;
begin
  s := CurrentLinkText;

  if FOnLinkOpen <> nil then begin
    Done := False;
    FOnLinkOpen(Self, s, Done);
    if Done then
      exit;
  end;

  if s = '' then
    exit;

  if (Length(s) > 7) and (strlicomp(PChar(s), PChar('file://'), 7) = 0) then
    OpenDocument(s)
  else
    OpenURL(s);
end;

procedure TSynPluginExternalLink.DoGetShiftStateInfo(ASender: TObject; AShift: TShiftState; out
  AShouldShow, ACanClick: boolean);
var
  i: Integer;
  act: TSynEditMouseAction;
begin
  AShouldShow := False;
  ACanClick   := False;
  for i := 0 to FMouseActions.Count - 1 do begin
    act := FMouseActions.Items[i];
    if (act.Command >= emcPluginExternalLink) and
       (act.Command <= emcPluginExternalLink_Last) and
       act.IsMatchingShiftState(AShift)
    then begin
      ACanClick := True;
      if (act.Option = emcoMouseLinkShow) then begin
        AShouldShow := True;
        exit;
      end;
    end;
  end;
end;

procedure TSynPluginExternalLink.SetMarkupInfo(AValue: TSynHighlighterAttributesModifier);
begin
  FMarkupInfo.Assign(AValue);
end;

procedure TSynPluginExternalLink.SetEditor(const AValue: TCustomSynEdit);
begin
  if (Editor <> nil) and (AValue <> nil) then
    raise Exception.Create('Not allowed to change editor');
  inherited SetEditor(AValue);
end;

procedure TSynPluginExternalLink.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  AValue.UnregisterMouseActionSearchHandler(@DoMouseActionSearch);
  AValue.UnregisterMouseActionExecHandler(@DoHandleMouseAction);
  AValue.MarkupManager.RemoveMarkUp(FMarkupLink);
  FreeAndNil(FMarkupLink);
  inherited DoEditorRemoving(AValue);
end;

procedure TSynPluginExternalLink.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  FMarkupLink := TSynEditMarkupMouseLink.Create(AValue);
  FMarkupLink.OnGetLinkInfo := @DoSearchLinkInfo;
  FMarkupLink.OnGetShiftStateInfo := @DoGetShiftStateInfo;
  FMarkupLink.MarkupInfo.Assign(FMarkupInfo);
  FMarkupLink.Enabled := FEnabled;

  AValue.MarkupManager.AddMarkUp(FMarkupLink);
  AValue.RegisterMouseActionExecHandler(@DoHandleMouseAction);
  AValue.RegisterMouseActionSearchHandler(@DoMouseActionSearch);
end;

constructor TSynPluginExternalLink.Create(AOwner: TComponent);
begin
  FEnabled := True;
  FMouseActions := TSynPluginExternalLinkMouseActions.Create(Self);
  FMouseActions.ResetDefaults;
  FMouseActions.FPOAttachObserver(Self);

  FMarkupInfo := TSynHighlighterAttributesModifier.Create;
  FMarkupInfo.Clear;
  FMarkupInfo.FrameColor := clBlue;
  FMarkupInfo.FrameEdges := sfeBottom;
  FMarkupInfo.FrameStyle := slsSolid;
  FMarkupInfo.InternalSaveDefaultValues;
  FMarkupInfo.OnChange := @DoMarkupInfoChanged;

  inherited Create(AOwner);
end;

destructor TSynPluginExternalLink.Destroy;
begin
  inherited Destroy;
  FMarkupInfo.Free;
  FMouseActions.Free;
end;

initialization
  RegisterMouseCmdIdentProcs(@IdentToSynMouseCmd, @SynMouseCmdToIdent);
  RegisterExtraGetEditorMouseCommandValues(@GetEditorMouseCommandValues);
  RegisterMouseCmdNameAndOptProcs(@MouseCommandName, @MouseCommandConfigName);

end.

