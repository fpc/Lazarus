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
  Graphics, LCLIntf, Clipbrd, SysUtils, Classes, RegExpr, LazFileUtils,
  SynEdit, SynEditMarkupCtrlMouseLink, SynEditTypes, SynEditMouseCmds,
  LazEditTextAttributes, LazEditHighlighter, SynEditStrConst;

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

  { TSynPluginExternalLinkPattern
    Text that is not a web address but stands for one, e.g. "issue #123".
    Pattern is a regular expression (case-insensitive); Url is the address to
    open, where $1..$9 are replaced by the groups of the match ($0 the whole
    match), e.g. Pattern "issue #(\d+)" with Url "https://example.org/issues/$1".
    With a Folder it only works in files in that folder (see OnGetFileName). }

  TSynPluginExternalLinkPattern = class(TCollectionItem)
  private
    FPattern: string;
    FUrl: string;
    FFolder: string;
    FRegExpr: TRegExpr;
    FInvalid: boolean;
    procedure SetPattern(AValue: string);
    procedure SetUrl(AValue: string);
    function GetRegExpr: TRegExpr;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // nil if Pattern is empty or not a valid expression
    property RegExpr: TRegExpr read GetRegExpr;
  published
    property Pattern: string read FPattern write SetPattern;
    property Url: string read FUrl write SetUrl;
    property Folder: string read FFolder write FFolder;
  end;

  { TSynPluginExternalLinkPatterns }

  TSynPluginExternalLinkPatterns = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TSynPluginExternalLinkPattern;
  public
    constructor Create(AOwner: TPersistent);
    function Add(const APattern, AUrl: string; const AFolder: string = ''): TSynPluginExternalLinkPattern; overload;
    property Items[Index: Integer]: TSynPluginExternalLinkPattern read GetItem; default;
  end;

  TSynPluginExternalLinkOnLinkOpen = procedure(Sender: TSynPluginExternalLink;
    var ALinkText: string; var AHandled: Boolean);
  TSynPluginExternalLinkGetFileName = function(Sender: TObject): string of object;

  { TSynPluginExternalLink }

  TSynPluginExternalLink = class(TLazSynEditPlugin, IFPObserver)
  private
    FEnabled: boolean;
    FMarkupInfo: TLazEditHighlighterAttributesModifier; // for caching colors, while TSynEditMarkupMouseLink = nil
    FMarkupLink: TSynEditMarkupMouseLink;
    FMouseActions: TSynPluginExternalLinkMouseActions;
    FOnLinkOpen: TSynPluginExternalLinkOnLinkOpen;
    FPatterns: TSynPluginExternalLinkPatterns;
    FOnGetFileName: TSynPluginExternalLinkGetFileName;
    // FPOObservedChanged: MouseActions changed
    procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
    function GetCurrentLinkText: string;
    function GetCurrentLinkTarget: string;
    procedure SetEnabled(AValue: boolean);
    procedure SetPatterns(AValue: TSynPluginExternalLinkPatterns);
    function FindPatternLink(const ALine: string; APosX: integer;
      out AStart, ALength: integer; out AUrl: string): boolean;
    procedure DoMarkupInfoChanged(Sender: TObject);
    procedure DoGetShiftStateInfo(ASender: TObject; AShift: TShiftState; out AShouldShow,
      ACanClick: boolean);
    procedure SetMarkupInfo(AValue: TLazEditHighlighterAttributesModifier);
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
    // The address to open: CurrentLinkText, or for a link found by Patterns its Url
    property CurrentLinkTarget: string read GetCurrentLinkTarget;
    property Patterns: TSynPluginExternalLinkPatterns read FPatterns write SetPatterns;
    // The file in the editor, for patterns with a Folder
    property OnGetFileName: TSynPluginExternalLinkGetFileName read FOnGetFileName write FOnGetFileName;
    property MarkupInfo : TLazEditHighlighterAttributesModifier read FMarkupInfo write SetMarkupInfo;
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

{ TSynPluginExternalLinkPattern }

procedure TSynPluginExternalLinkPattern.SetPattern(AValue: string);
begin
  if FPattern = AValue then Exit;
  FPattern := AValue;
  FreeAndNil(FRegExpr);
  FInvalid := False;
  Changed(False);
end;

procedure TSynPluginExternalLinkPattern.SetUrl(AValue: string);
begin
  if FUrl = AValue then Exit;
  FUrl := AValue;
  Changed(False);
end;

function TSynPluginExternalLinkPattern.GetRegExpr: TRegExpr;
begin
  if (FRegExpr = nil) and (not FInvalid) and (FPattern <> '') then begin
    FRegExpr := TRegExpr.Create;
    FRegExpr.ModifierI := True;
    try
      FRegExpr.Expression := FPattern;
      FRegExpr.Compile;
    except
      // a pattern that does not compile finds nothing
      FreeAndNil(FRegExpr);
      FInvalid := True;
    end;
  end;
  Result := FRegExpr;
end;

destructor TSynPluginExternalLinkPattern.Destroy;
begin
  FreeAndNil(FRegExpr);
  inherited Destroy;
end;

procedure TSynPluginExternalLinkPattern.Assign(Source: TPersistent);
begin
  if Source is TSynPluginExternalLinkPattern then begin
    Pattern := TSynPluginExternalLinkPattern(Source).Pattern;
    Url     := TSynPluginExternalLinkPattern(Source).Url;
    Folder  := TSynPluginExternalLinkPattern(Source).Folder;
  end
  else
    inherited Assign(Source);
end;

{ TSynPluginExternalLinkPatterns }

function TSynPluginExternalLinkPatterns.GetItem(Index: Integer): TSynPluginExternalLinkPattern;
begin
  Result := TSynPluginExternalLinkPattern(inherited Items[Index]);
end;

constructor TSynPluginExternalLinkPatterns.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TSynPluginExternalLinkPattern);
end;

function TSynPluginExternalLinkPatterns.Add(const APattern, AUrl: string;
  const AFolder: string): TSynPluginExternalLinkPattern;
begin
  Result := TSynPluginExternalLinkPattern(inherited Add);
  Result.Pattern := APattern;
  Result.Url := AUrl;
  Result.Folder := AFolder;
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
  PatternUrl: string;
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
  if (aLinkStart = 0) or (PosX > aLinkStart + aLinkLength) then
    FindPatternLink(Line, PosX, aLinkStart, aLinkLength, PatternUrl);
  if (aLinkStart > 0) and (PosX >= aLinkStart) and (PosX <= aLinkStart + aLinkLength) then begin
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

function TSynPluginExternalLink.FindPatternLink(const ALine: string; APosX: integer; out AStart,
  ALength: integer; out AUrl: string): boolean;
// The first of Patterns with a match that covers APosX (the same way a web
// address does: up to and including the position just after it).
const
  MAX_LINK_LEN = 300;
var
  i, Lim: integer;
  Expr: TRegExpr;
begin
  Result := False;
  AStart := 0;
  ALength := 0;
  AUrl := '';
  if (FPatterns = nil) or (ALine = '') then
    exit;
  // a match that starts further back can not reach APosX
  Lim := APosX - MAX_LINK_LEN;
  if Lim < 1 then
    Lim := 1;
  for i := 0 to FPatterns.Count - 1 do begin
    if (FPatterns[i].Folder <> '') and
       ((FOnGetFileName = nil) or not FileIsInPath(FOnGetFileName(Self), FPatterns[i].Folder))
    then
      continue;
    Expr := FPatterns[i].RegExpr;
    if Expr = nil then
      continue;
    try
      Expr.InputString := ALine;
      if Expr.ExecPos(Lim) then
        repeat
          if Expr.MatchPos[0] > APosX then
            break;
          if (Expr.MatchLen[0] > 0) and
             (APosX <= Expr.MatchPos[0] + Expr.MatchLen[0])
          then begin
            AStart := Expr.MatchPos[0];
            ALength := Expr.MatchLen[0];
            AUrl := Expr.Substitute(FPatterns[i].Url);
            exit(True);
          end;
        until not Expr.ExecNext;
    except
      // a bad expression at run time (e.g. too complex) is no link
    end;
  end;
end;

function TSynPluginExternalLink.GetCurrentLinkText: string;
begin
  Result := '';
  if (FMarkupLink = nil) or (FMarkupLink.LinkStartPos.Y < 0) then
    exit;

  Result := Editor.TextBetweenPoints[FMarkupLink.LinkStartPos, FMarkupLink.LinkEndPos];
end;

function TSynPluginExternalLink.GetCurrentLinkTarget: string;
var
  Line: string;
  aStart, aLength: integer;
begin
  Result := CurrentLinkText;
  if (Result = '') or (FPatterns = nil) or (FPatterns.Count = 0) then
    exit;
  // a link that is a web address opens as it is written
  if (Pos('://', Result) > 0) or StartBy(PChar(Result), 'www.') then
    exit;
  Line := Editor.Lines[FMarkupLink.LinkStartPos.Y - 1];
  if FindPatternLink(Line, FMarkupLink.LinkStartPos.X, aStart, aLength, Result) and
     (aStart = FMarkupLink.LinkStartPos.X) and
     (aStart + aLength = FMarkupLink.LinkEndPos.X)
  then
    exit;
  Result := CurrentLinkText;
end;

procedure TSynPluginExternalLink.SetPatterns(AValue: TSynPluginExternalLinkPatterns);
begin
  FPatterns.Assign(AValue);
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
        s := CurrentLinkTarget;
        if s <> '' then
          Clipboard.AsText := s;
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
  s := CurrentLinkTarget;

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

procedure TSynPluginExternalLink.SetMarkupInfo(AValue: TLazEditHighlighterAttributesModifier);
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

  FMarkupInfo := TLazEditHighlighterAttributesModifier.Create;
  FMarkupInfo.Clear;
  FMarkupInfo.FrameColor := clBlue;
  FMarkupInfo.FrameEdges := sfeBottom;
  FMarkupInfo.FrameStyle := slsSolid;
  FMarkupInfo.InternalSaveDefaultValues;
  FMarkupInfo.OnChange := @DoMarkupInfoChanged;

  FPatterns := TSynPluginExternalLinkPatterns.Create(Self);

  inherited Create(AOwner);
end;

destructor TSynPluginExternalLink.Destroy;
begin
  inherited Destroy;
  FPatterns.Free;
  FMarkupInfo.Free;
  FMouseActions.Free;
end;

initialization
  RegisterMouseCmdIdentProcs(@IdentToSynMouseCmd, @SynMouseCmdToIdent);
  RegisterExtraGetEditorMouseCommandValues(@GetEditorMouseCommandValues);
  RegisterMouseCmdNameAndOptProcs(@MouseCommandName, @MouseCommandConfigName);

end.

