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
unit SynPluginTemplateEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLType,
  SynPluginSyncronizedEditBase, SynEditKeyCmds, SynEdit, SynEditMiscProcs,
  LCLProc;

type

  { TSynEditTemplateEditKeyStrokes }

  TSynEditTemplateEditKeyStrokes = class(TSynEditKeyStrokes)
  public
    procedure ResetDefaults; override;
  end;

  { TSynEditTemplateEditKeyStrokesOffCell }

  TSynEditTemplateEditKeyStrokesOffCell = class(TSynEditKeyStrokes)
  public
    procedure ResetDefaults; override;
  end;

  { TSynPluginTemplateEdit }

  TSynPluginTemplateEdit = class(TSynPluginCustomSyncroEdit)
  private
    FCellParserEnabled: Boolean;
    FKeystrokes, FKeyStrokesOffCell: TSynEditKeyStrokes;
    FStartPoint: TPoint;
    procedure SetKeystrokes(const AValue: TSynEditKeyStrokes);
    procedure SetKeystrokesOffCell(const AValue: TSynEditKeyStrokes);
  protected
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
      var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; FinishComboOnly: Boolean;
      var ComboKeyStrokes: TSynEditKeyStrokes);
    procedure ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
              var Handled: boolean; var Command: TSynEditorCommand;
              var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);

    procedure NextCellOrFinal(SetSelect: Boolean = True; FirstsOnly: Boolean = False);
    procedure SetFinalCaret;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetTemplate(aTmpl: String; aLogCaretPos: TPoint); // Replaces current selection
    // Coords relativ to the template. base (1, 1)
    procedure AddEditCells(aCellList: TSynPluginSyncronizedEditList);

    property CellParserEnabled: Boolean read FCellParserEnabled write FCellParserEnabled;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes;
    property KeystrokesOffCell: TSynEditKeyStrokes
      read FKeystrokesOffCell write SetKeystrokesOffCell;
  end;

const
  ecSynPTmplEdNextCell           = ecPluginFirstTemplEdit +  0;
  ecSynPTmplEdNextCellSel        = ecPluginFirstTemplEdit +  1;
  ecSynPTmplEdNextCellRotate     = ecPluginFirstTemplEdit +  2;
  ecSynPTmplEdNextCellSelRotate  = ecPluginFirstTemplEdit +  3;
  ecSynPTmplEdPrevCell           = ecPluginFirstTemplEdit +  4;
  ecSynPTmplEdPrevCellSel        = ecPluginFirstTemplEdit +  5;
  ecSynPTmplEdCellHome           = ecPluginFirstTemplEdit +  6;
  ecSynPTmplEdCellEnd            = ecPluginFirstTemplEdit +  7;
  ecSynPTmplEdCellSelect         = ecPluginFirstTemplEdit +  8;
  ecSynPTmplEdFinish             = ecPluginFirstTemplEdit +  9;
  ecSynPTmplEdEscape             = ecPluginFirstTemplEdit + 10;
  ecSynPTmplEdNextFirstCell           = ecPluginFirstTemplEdit + 11;
  ecSynPTmplEdNextFirstCellSel        = ecPluginFirstTemplEdit + 12;
  ecSynPTmplEdNextFirstCellRotate     = ecPluginFirstTemplEdit + 13;
  ecSynPTmplEdNextFirstCellSelRotate  = ecPluginFirstTemplEdit + 14;
  ecSynPTmplEdPrevFirstCell           = ecPluginFirstTemplEdit + 15;
  ecSynPTmplEdPrevFirstCellSel        = ecPluginFirstTemplEdit + 16;

  // If extending the list, reserve space in SynEditKeyCmds

  ecSynPTmplEdCount               = 17;

implementation

{ TSynPluginTemplateEdit }

constructor TSynPluginTemplateEdit.Create(AOwner: TComponent);
begin
  FKeystrokes := TSynEditTemplateEditKeyStrokes.Create(Self);
  FKeystrokes.ResetDefaults;
  FKeyStrokesOffCell := TSynEditTemplateEditKeyStrokesOffCell.Create(self);
  FKeyStrokesOffCell.ResetDefaults;
  inherited Create(AOwner);
  CellParserEnabled := True;
end;

destructor TSynPluginTemplateEdit.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FKeystrokes);
  FreeAndNil(FKeyStrokesOffCell);
end;

procedure TSynPluginTemplateEdit.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  if Editor <> nil then begin
    Editor.UnRegisterKeyTranslationHandler(@TranslateKey);
    Editor.UnregisterCommandHandler(@ProcessSynCommand);
  end;
  inherited DoEditorRemoving(AValue);
end;

procedure TSynPluginTemplateEdit.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  if Editor <> nil then begin
    Editor.RegisterCommandHandler(@ProcessSynCommand, nil);
    Editor.RegisterKeyTranslationHandler(@TranslateKey);
  end;
end;

procedure TSynPluginTemplateEdit.SetKeystrokes(const AValue: TSynEditKeyStrokes);
begin
  if AValue = nil then
    FKeystrokes.Clear
  else
    FKeystrokes.Assign(AValue);
end;

procedure TSynPluginTemplateEdit.SetKeystrokesOffCell(const AValue: TSynEditKeyStrokes);
begin
  if AValue = nil then
    FKeyStrokesOffCell.Clear
  else
    FKeyStrokesOffCell.Assign(AValue);
end;

procedure TSynPluginTemplateEdit.TranslateKey(Sender: TObject; Code: word;
  SState: TShiftState; var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; FinishComboOnly: Boolean;
  var ComboKeyStrokes: TSynEditKeyStrokes);
var
  keys: TSynEditKeyStrokes;
begin
  if (not Active) or Handled then
    exit;

  if CurrentCell < 0 then
    keys := FKeyStrokesOffCell
  else
    keys := FKeyStrokes;

  if not FinishComboOnly then
    keys.ResetKeyCombo;
  Command := keys.FindKeycodeEx(Code, SState, Data, IsStartOfCombo, FinishComboOnly, ComboKeyStrokes);

  Handled := (Command <> ecNone) or IsStartOfCombo;
  if IsStartOfCombo then
    ComboKeyStrokes := keys;
end;

procedure TSynPluginTemplateEdit.ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
begin
  if Handled or AfterProcessing or not Active then exit;

  Handled := True;
  case Command of
    ecSynPTmplEdNextCell:          NextCellOrFinal(False);
    ecSynPTmplEdNextCellSel:       NextCellOrFinal(True);
    ecSynPTmplEdNextCellRotate:    NextCell(False);
    ecSynPTmplEdNextCellSelRotate: NextCell(True);
    ecSynPTmplEdPrevCell:          PreviousCell(False);
    ecSynPTmplEdPrevCellSel:       PreviousCell(True);
    ecSynPTmplEdNextFirstCell:          NextCellOrFinal(False, True);
    ecSynPTmplEdNextFirstCellSel:       NextCellOrFinal(True, True);
    ecSynPTmplEdNextFirstCellRotate:    NextCell(False, False, True);
    ecSynPTmplEdNextFirstCellSelRotate: NextCell(True, False, True);
    ecSynPTmplEdPrevFirstCell:          PreviousCell(False, False, True);
    ecSynPTmplEdPrevFirstCellSel:       PreviousCell(True, False, True);
    ecSynPTmplEdCellHome:          CellCaretHome;
    ecSynPTmplEdCellEnd:           CellCaretEnd;
    ecSynPTmplEdCellSelect:        SelectCurrentCell;
    ecSynPTmplEdFinish:            SetFinalCaret;
    ecSynPTmplEdEscape:
      begin
        Clear;
        Active := False;
      end;
    else
      Handled := False;
  end;
end;

procedure TSynPluginTemplateEdit.NextCellOrFinal(SetSelect: Boolean; FirstsOnly: Boolean);
var
  Pos: TPoint;
  i: Integer;
begin
  Pos := CaretObj.LineBytePos;
  i := Cells.IndexOf(Pos.x, Pos.y, True, CurrentCell);
  if i < 0 then begin
    i := Cells.Count - 1;
    while (i >= 0) and
      ((Cells[i].Group < 0) or (CompareCarets(Cells[i].LogEnd, Pos) <= 0))
    do
      dec(i);
  end;

  Repeat
    inc(i);
    if i >= Cells.Count then begin
      SetFinalCaret;
      exit;
    end;
  until (Cells[i].Group >= 0) and
        ((not FirstsOnly) or (Cells[i].FirstInGroup));
  CurrentCell := i;
  if CurrentCell < 0 then
    exit;
  CaretObj.LineBytePos := Cells[CurrentCell].LogStart;
  if SetSelect then
    SelectCurrentCell(True)
  else
    Editor.BlockBegin := Cells[CurrentCell].LogStart;
end;

procedure TSynPluginTemplateEdit.SetFinalCaret;
var
  c: TSynPluginSyncronizedEditCell;
begin
  if FMarkup <> nil then
    FMarkup.DoInvalidate;
  c := Cells.GroupCell[-2, 0];
  Editor.BlockBegin := c.LogStart;
  CaretObj.IncForcePastEOL;
  CaretObj.LineBytePos := c.LogStart;
  CaretObj.DecForcePastEOL;
  Cells.Clear;
  Active := False;
end;

procedure TSynPluginTemplateEdit.SetTemplate(aTmpl: String; aLogCaretPos: TPoint);
var
  Temp: TStringList;
  CellStart, StartPos: TPoint;
  i, j, k, XOffs, Grp: Integer;
  s, s2: string;
begin
  Clear;
  Active := False;
  StartPos := Editor.BlockBegin;
  FStartPoint := StartPos;
  if CellParserEnabled then begin
    Temp := TStringList.Create;
    try
      Temp.Text := aTmpl;
      if (aTmpl <> '') and (aTmpl[length(aTmpl)] in [#10,#13]) then
        Temp.Add('');

      XOffs := StartPos.X - 1;
      i := 0;
      Grp := 1;
      while i < Temp.Count do begin
        CellStart.y := StartPos.y + i;
        CellStart.x := -1;
        s := Temp[i];
        j := 1;
        k := 1;
        SetLength(s2, length(s));
        while j <= length(s) do begin
          case s[j] of
            '{':
              if (j + 1 <= Length(s)) and (s[j+1] = '{') then begin
                inc(j);
                s2[k] := s[j];
                inc(k);
              end else begin
                CellStart.x := k + XOffs;
              end;
            '}':
              if (j + 1 <= Length(s)) and (s[j+1] = '}') then begin
                inc(j);
                s2[k] := s[j];
                inc(k);
              end else
              if CellStart.x > 0 then begin
                with Cells.AddNew do begin
                  LogStart := CellStart;
                  LogEnd := Point(k +XOffs, CellStart.y);
                  Group := grp;
                  FirstInGroup := True;
                end;
                inc(grp);
                CellStart.x := -1;
              end;
            else
              begin
                s2[k] := s[j];
                inc(k);
              end;
          end;
          inc(j);
        end;

        SetLength(s2, k-1);
        Temp[i] := s2;
        inc(i);
        XOffs := 0;
      end;

      aTmpl := Temp.Text;
      // strip the trailing #13#10 that was appended by the stringlist
      i := Length(aTmpl);
      if (i >= 1) and (aTmpl[i] in [#10,#13]) then begin
        dec(i);
        if (i >= 1) and (aTmpl[i] in [#10,#13]) and (aTmpl[i] <> aTmpl[i+1]) then
          dec(i);
        SetLength(aTmpl, i);
      end;

    finally
      Temp.Free;
    end;
  end;

  Editor.SelText := aTmpl;
  with Cells.AddNew do begin
    Group := -2;
    LogStart := aLogCaretPos;
    LogEnd := aLogCaretPos;
  end;
  if (Cells.Count > 1) then begin
    Active := True;
    CurrentCell := 0;
    CaretObj.LineBytePos := Cells[0].LogStart;
    SelectCurrentCell;
    SetUndoStart;
  end
  else
    Editor.MoveCaretIgnoreEOL(Editor.LogicalToPhysicalPos(aLogCaretPos));
end;

procedure TSynPluginTemplateEdit.AddEditCells(aCellList: TSynPluginSyncronizedEditList);
var
  i, XOffs, YOffs: Integer;
  CurCell: TSynPluginSyncronizedEditCell;
  CaretPos: TSynPluginSyncronizedEditCell;
  p: TPoint;
begin
  CaretPos := nil;
  if Cells.GroupCell[-2, 0] <> nil then begin
    CaretPos := TSynPluginSyncronizedEditCell.Create;
    CaretPos.Assign(Cells.GroupCell[-2, 0]);
  end;
  Cells.Clear;

  XOffs := FStartPoint.x - 1;
  YOffs := FStartPoint.y - 1;
  for i := 0 to aCellList.Count - 1 do begin
    CurCell := aCellList[i];
    with Cells.AddNew do begin;
      Assign(CurCell);
      p := LogStart;
      if p.y = 1 then
        p.x := p.x + XOffs;
      p.y := p.y + YOffs;
      LogStart := p;
      p := LogEnd;
      if p.y = 1 then
        p.x := p.x + XOffs;
      p.y := p.y + YOffs;
      LogEnd := p;
    end;
  end;

  if CaretPos <> nil then
    Cells.AddNew.Assign(CaretPos);
  FreeAndNil(CaretPos);

  if aCellList.Count > 0 then begin
    Active := True;
    CurrentCell := 0;
    CaretObj.LineBytePos := Cells[0].LogStart;
    SelectCurrentCell;
    SetUndoStart;
  end;
end;

{ TSynEditTemplateEditKeyStrokes }

procedure TSynEditTemplateEditKeyStrokes.ResetDefaults;

  procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
     const AShift: TShiftState);
  begin
    with Add do
    begin
      Key := AKey;
      Shift := AShift;
      Command := ACmd;
    end;
  end;

begin
  Clear;
  AddKey(ecSynPTmplEdNextCellRotate,    VK_RIGHT,  [ssCtrl]);
  AddKey(ecSynPTmplEdNextCellSel,       VK_TAB,    []);
  AddKey(ecSynPTmplEdPrevCell,          VK_LEFT,   [ssCtrl]);
  AddKey(ecSynPTmplEdPrevCellSel,       VK_TAB,    [ssShift]);

  AddKey(ecSynPTmplEdCellHome,          VK_HOME,   []);
  AddKey(ecSynPTmplEdCellEnd,           VK_END,    []);
  AddKey(ecSynPTmplEdCellSelect,        VK_A,      [ssCtrl]);
  AddKey(ecSynPTmplEdFinish,            VK_RETURN, []);
  AddKey(ecSynPTmplEdEscape,            VK_ESCAPE, []);
end;

{ TSynEditTemplateEditKeyStrokesOffCell }

procedure TSynEditTemplateEditKeyStrokesOffCell.ResetDefaults;

  procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
     const AShift: TShiftState);
  begin
    with Add do
    begin
      Key := AKey;
      Shift := AShift;
      Command := ACmd;
    end;
  end;

begin
  Clear;
  AddKey(ecSynPTmplEdNextCellRotate,    VK_RIGHT,  [ssCtrl]);
  AddKey(ecSynPTmplEdNextCellSel,       VK_TAB,    []);
  AddKey(ecSynPTmplEdPrevCell,          VK_LEFT,   [ssCtrl]);
  AddKey(ecSynPTmplEdPrevCellSel,       VK_TAB,    [ssShift]);

  AddKey(ecSynPTmplEdFinish,            VK_RETURN, []);
  AddKey(ecSynPTmplEdEscape,            VK_ESCAPE, []);
end;

const
  EditorTmplEditCommandStrs: array[0..16] of TIdentMapEntry = (
    (Value: ecSynPTmplEdNextCell;                Name: 'ecSynPTmplEdNextCell'),
    (Value: ecSynPTmplEdNextCellSel;             Name: 'ecSynPTmplEdNextCellSel'),
    (Value: ecSynPTmplEdNextCellRotate;          Name: 'ecSynPTmplEdNextCellRotate'),
    (Value: ecSynPTmplEdNextCellSelRotate;       Name: 'ecSynPTmplEdNextCellSelRotate'),
    (Value: ecSynPTmplEdPrevCell;                Name: 'ecSynPTmplEdPrevCell'),
    (Value: ecSynPTmplEdPrevCellSel;             Name: 'ecSynPTmplEdPrevCellSel'),
    (Value: ecSynPTmplEdCellHome;                Name: 'ecSynPTmplEdCellHome'),
    (Value: ecSynPTmplEdCellEnd;                 Name: 'ecSynPTmplEdCellEnd'),
    (Value: ecSynPTmplEdCellSelect;              Name: 'ecSynPTmplEdCellSelect'),
    (Value: ecSynPTmplEdFinish;                  Name: 'ecSynPTmplEdFinish'),
    (Value: ecSynPTmplEdEscape;                  Name: 'ecSynPTmplEdEscape'),
    (Value: ecSynPTmplEdNextFirstCell;           Name: 'ecSynPTmplEdNextFirstCell'),
    (Value: ecSynPTmplEdNextFirstCellSel;        Name: 'ecSynPTmplEdNextFirstCellSel'),
    (Value: ecSynPTmplEdNextFirstCellRotate;     Name: 'ecSynPTmplEdNextFirstCellRotate'),
    (Value: ecSynPTmplEdNextFirstCellSelRotate;  Name: 'ecSynPTmplEdNextFirstCellSelRotate'),
    (Value: ecSynPTmplEdPrevFirstCell;           Name: 'ecSynPTmplEdPrevFirstCell'),
    (Value: ecSynPTmplEdPrevFirstCellSel;        Name: 'ecSynPTmplEdPrevFirstCellSel')
  );

function IdentToTmplEditCommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, EditorTmplEditCommandStrs);
end;

function TmplEditCommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := (Cmd >= ecPluginFirstTemplEdit) and (Cmd - ecPluginFirstTemplEdit < ecSynPTmplEdCount);
  if not Result then exit;
  Result := IntToIdent(Cmd, Ident, EditorTmplEditCommandStrs);
end;

procedure GetEditorCommandValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := Low(EditorTmplEditCommandStrs) to High(EditorTmplEditCommandStrs) do
    Proc(EditorTmplEditCommandStrs[I].Name);
end;


initialization
  RegisterKeyCmdIdentProcs(@IdentToTmplEditCommand,
                           @TmplEditCommandToIdent);
  RegisterExtraGetEditorCommandValues(@GetEditorCommandValues);

end.

