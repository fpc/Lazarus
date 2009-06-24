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
  Classes, SysUtils, math, Graphics, LCLType, SynEditMiscClasses,
  SynPluginSyncEditBase, SynEditKeyCmds, SynEdit, SynEditMiscProcs,
  SynEditTextTrimmer, SynEditTextBase, LCLProc;

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

  TSynPluginTemplateEdit = class(TSynPluginSyncEditBase)
  private
    FCellParserEnabled: Boolean;
    FKeystrokes, FKeyStrokesOffCell: TSynEditKeyStrokes;
    FStartPoint: TPoint;
    FLastCell: Integer;
    function GetMarkupInfo: TSynSelectedColor;
    function GetMarkupInfoCurrent: TSynSelectedColor;
    function GetMarkupInfoSync: TSynSelectedColor;
    procedure SetKeystrokes(const AValue: TSynEditKeyStrokes);
    procedure SetKeystrokesOffCell(const AValue: TSynEditKeyStrokes);
  protected
    procedure SetEditor(const AValue: TCustomSynEdit); override;
    procedure DoBeforeEdit(aX, aY: Integer); override;
    procedure DoOnActivate; override;
    procedure DoOnDeactivate; override;
    procedure UpdateCurrentCell;
    procedure DoCaretChanged(Sender: TObject); virtual;
    procedure TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
      var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; FinishComboOnly: Boolean;
      var ComboKeyStrokes: TSynEditKeyStrokes);
    procedure ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
              var Handled: boolean; var Command: TSynEditorCommand;
              var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);

    procedure SelectCurrentCell(Reverse: Boolean = False);
    procedure PreviousCell(SetSelect: Boolean = True);
    procedure NextCell(SetSelect: Boolean = True; CycleToFirst: Boolean = False);
    procedure CellCaretHome;
    procedure CellCaretEnd;
    procedure SetFinalCaret;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function ConvertCommandToBase(Command: TSynEditorCommand): TSynEditorCommand;
    class function ConvertBaseToCommand(Command: TSynEditorCommand): TSynEditorCommand;
    class function ConvertCommandToBaseOff(Command: TSynEditorCommand): TSynEditorCommand;
    class function ConvertBaseToCommandOff(Command: TSynEditorCommand): TSynEditorCommand;

    procedure SetTemplate(aTmpl: String; aCaretPos: TPoint); // Replaces current selection
    // Coords relativ to the template. base (1, 1)
    procedure AddEditCells(aCellList: TSynPluginSyncEditList);

    property CellParserEnabled: Boolean read FCellParserEnabled write FCellParserEnabled;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes;
    property KeystrokesOffCell: TSynEditKeyStrokes
      read FKeystrokesOffCell write SetKeystrokesOffCell;
    property MarkupInfo: TSynSelectedColor read GetMarkupInfo;
    property MarkupInfoCurrent: TSynSelectedColor read GetMarkupInfoCurrent;
    property MarkupInfoSync: TSynSelectedColor read GetMarkupInfoSync;
  end;

const
  ecSynPTmplEdNextCell           = ecPluginFirst +  0;
  ecSynPTmplEdNextCellSel        = ecPluginFirst +  1;
  ecSynPTmplEdNextCellRotate     = ecPluginFirst +  2;
  ecSynPTmplEdNextCellSelRotate  = ecPluginFirst +  3;
  ecSynPTmplEdPrevCell           = ecPluginFirst +  4;
  ecSynPTmplEdPrevCellSel        = ecPluginFirst +  5;
  ecSynPTmplEdCellHome           = ecPluginFirst +  6;
  ecSynPTmplEdCellEnd            = ecPluginFirst +  7;
  ecSynPTmplEdCellSelect         = ecPluginFirst +  8;
  ecSynPTmplEdFinish             = ecPluginFirst +  9;
  ecSynPTmplEdEscape             = ecPluginFirst + 10;

  ecSynPTmplEdLast               = ecPluginFirst + 10;

implementation

var
  KeyOffset, KeyOffsetOff: integer;

{ TSynPluginTemplateEdit }

constructor TSynPluginTemplateEdit.Create(AOwner: TComponent);
begin
  FKeystrokes := TSynEditTemplateEditKeyStrokes.Create(Self);
  FKeystrokes.ResetDefaults;
  FKeystrokes.PluginOffset := KeyOffset;
  FKeyStrokesOffCell := TSynEditTemplateEditKeyStrokesOffCell.Create(self);
  FKeyStrokesOffCell.ResetDefaults;
  FKeyStrokesOffCell.PluginOffset := KeyOffsetOff;
  inherited Create(AOwner);
  CellParserEnabled := True;
end;

destructor TSynPluginTemplateEdit.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FKeystrokes);
  FreeAndNil(FKeyStrokesOffCell);
end;

class function TSynPluginTemplateEdit.ConvertCommandToBase
  (Command: TSynEditorCommand): TSynEditorCommand;
begin
  if (Command >= ecPluginFirst + KeyOffset) and
     (Command <= ecPluginFirst + KeyOffset + ecSynPTmplEdLast)
  then
    Result := Command - KeyOffset
  else
    Result := ecNone;
end;

class function TSynPluginTemplateEdit.ConvertBaseToCommand(Command: TSynEditorCommand): TSynEditorCommand;
begin
  if (Command >= ecPluginFirst) and (Command <= ecPluginFirst + ecSynPTmplEdLast)
  then
    Result := Command + KeyOffset
  else
    Result := ecNone;
end;

class function TSynPluginTemplateEdit.ConvertCommandToBaseOff
  (Command: TSynEditorCommand): TSynEditorCommand;
begin
  if (Command >= ecPluginFirst + KeyOffsetOff) and
     (Command <= ecPluginFirst + KeyOffsetOff + ecSynPTmplEdLast)
  then
    Result := Command - KeyOffsetOff
  else
    Result := ecNone;
end;

class function TSynPluginTemplateEdit.ConvertBaseToCommandOff(Command: TSynEditorCommand): TSynEditorCommand;
begin
  if (Command >= ecPluginFirst) and (Command <= ecPluginFirst + ecSynPTmplEdLast)
  then
    Result := Command + KeyOffsetOff
  else
    Result := ecNone;
end;

procedure TSynPluginTemplateEdit.SetEditor(const AValue: TCustomSynEdit);
begin
  if Editor = AValue then exit;
  if Editor <> nil then begin
    CaretObj.RemoveChangeHandler(@DoCaretChanged);
    Editor.UnRegisterKeyTranslationHandler(@TranslateKey);
    Editor.UnregisterCommandHandler(@ProcessSynCommand);
  end;
  inherited SetEditor(AValue);
  if Editor <> nil then begin
    Editor.RegisterCommandHandler(@ProcessSynCommand, nil);
    Editor.RegisterKeyTranslationHandler(@TranslateKey);
    CaretObj.AddChangeHandler(@DoCaretChanged);
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

function TSynPluginTemplateEdit.GetMarkupInfo: TSynSelectedColor;
begin
  Result := Markup.MarkupInfo;
end;

function TSynPluginTemplateEdit.GetMarkupInfoCurrent: TSynSelectedColor;
begin
  Result := Markup.MarkupInfoCurrent;
end;

function TSynPluginTemplateEdit.GetMarkupInfoSync: TSynSelectedColor;
begin
  Result := Markup.MarkupInfoSync;
end;

procedure TSynPluginTemplateEdit.DoBeforeEdit(aX, aY: Integer);
begin
  UpdateCurrentCell;
  if CurrentCell < 0 then begin
    Clear;
    Active := False;
  end;
end;

procedure TSynPluginTemplateEdit.DoOnActivate;
var
  b: TSynEditStrings;
begin
  b := ViewedTextBuffer;
  while b <> nil do begin
    if b is TSynEditStringTrimmingList then TSynEditStringTrimmingList(b).Lock;
    if b is TSynEditStringsLinked then
      b := TSynEditStringsLinked(b).NextLines
    else
      b := nil;;
  end;
end;

procedure TSynPluginTemplateEdit.DoOnDeactivate;
var
  b: TSynEditStrings;
begin
  b := ViewedTextBuffer;
  while b <> nil do begin
    if b is TSynEditStringTrimmingList then TSynEditStringTrimmingList(b).UnLock;
    if b is TSynEditStringsLinked then
      b := TSynEditStringsLinked(b).NextLines
    else
      b := nil;;
  end;
end;

procedure TSynPluginTemplateEdit.UpdateCurrentCell;
var
  i: Integer;
begin
  i := Cells.IndexOf(CaretObj.BytePos, CaretObj.LinePos, True);
  if (i <> CurrentCell) and (CurrentCell >= 0) then
    FLastCell := CurrentCell;
  CurrentCell := i;
end;

procedure TSynPluginTemplateEdit.DoCaretChanged(Sender: TObject);
begin
  if not Active then exit;
  UpdateCurrentCell;
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

  if CurrentCell < 0 then begin
    keys := FKeyStrokesOffCell;
    FKeyStrokes.ResetKeyCombo;
  end
  else begin
    keys := FKeyStrokes;
    FKeyStrokesOffCell.ResetKeyCombo;
  end;

  if (FinishComboOnly and (ComboKeyStrokes <> keys)) then begin
    keys.ResetKeyCombo;
    exit;
  end;

  IsStartOfCombo := False;
  try
    keys.UsePluginOffset := True;
    Command := keys.FindKeycodeEx(Code, SState, Data, IsStartOfCombo,
                                        FinishComboOnly);
  finally
    keys.UsePluginOffset := False;
  end;
  Handled := (Command <> ecNone) or IsStartOfCombo;
  if Handled then begin
    ComboKeyStrokes := keys;
  end;
end;

procedure TSynPluginTemplateEdit.ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
var
  Cmd: TSynEditorCommand;
begin
  if Handled or AfterProcessing or not Active then exit;
  Cmd := ConvertCommandToBase(Command);
  if Cmd = ecNone then
    Cmd := ConvertCommandToBaseOff(Command);

  Handled := True;
  case Cmd of
    ecSynPTmplEdNextCell:          NextCell(False);
    ecSynPTmplEdNextCellSel:       NextCell(True);
    ecSynPTmplEdNextCellRotate:    NextCell(False, True);
    ecSynPTmplEdNextCellSelRotate: NextCell(True, True);
    ecSynPTmplEdPrevCell:          PreviousCell(False);
    ecSynPTmplEdPrevCellSel:       PreviousCell(True);
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

procedure TSynPluginTemplateEdit.SelectCurrentCell(Reverse: Boolean = False);
begin
  if (CurrentCell < 0) and (FLastCell >= 0) then
    CurrentCell := FLastCell;
  if (CurrentCell < 0) then
    exit;
  if Reverse then begin
    CaretObj.LineBytePos := Cells[CurrentCell].LogStart;
    Editor.BlockBegin := Cells[CurrentCell].LogEnd;
    Editor.BlockEnd := Cells[CurrentCell].LogStart;
  end else begin
    CaretObj.LineBytePos := Cells[CurrentCell].LogEnd;
    Editor.BlockBegin := Cells[CurrentCell].LogStart;
    Editor.BlockEnd := Cells[CurrentCell].LogEnd;
  end;
end;

procedure TSynPluginTemplateEdit.PreviousCell(SetSelect: Boolean = True);
var
  i, j: Integer;
  Pos: TPoint;
begin
  Pos := CaretObj.LineBytePos;
  i := Cells.IndexOf(Pos.x, Pos.y, True);
  if i < 0 then begin
    i := 0;
    while (i < Cells.Count) and
      ((Cells[i].Group < 0) or (CompareCarets(Cells[i].LogEnd, Pos) >= 0))
    do
      inc(i);
  end;

  j := 0;
  Repeat
    dec(i);
    inc(j);
    if i < 0 then
      i := Cells.Count - 1;
  until (j > Cells.Count) or (Cells[i].Group >= 0);
  CurrentCell := i;

  if CurrentCell < 0 then
    exit;
  CaretObj.LineBytePos := Cells[CurrentCell].LogEnd;
  if SetSelect then
    SelectCurrentCell
  else
    Editor.BlockBegin := Cells[CurrentCell].LogEnd;
end;

procedure TSynPluginTemplateEdit.NextCell(SetSelect: Boolean = True;
  CycleToFirst: Boolean = False);
var
  Pos: TPoint;
  i, j: Integer;
begin
  Pos := CaretObj.LineBytePos;
  i := Cells.IndexOf(Pos.x, Pos.y, True);
  if i < 0 then begin
    i := Cells.Count - 1;
    while (i >= 0) and
      ((Cells[i].Group < 0) or (CompareCarets(Cells[i].LogEnd, Pos) <= 0))
    do
      dec(i);
  end;

  j := 0;
  Repeat
    inc(i);
    inc(j);
    if i >= Cells.Count then begin
      if CycleToFirst then
        i := 0
      else begin
        SetFinalCaret;
        exit;
      end;
    end;
  until (j > Cells.Count) or (Cells[i].Group >= 0);
  CurrentCell := i;
  if CurrentCell < 0 then
    exit;
  CaretObj.LineBytePos := Cells[CurrentCell].LogStart;
  if SetSelect then
    SelectCurrentCell(True)
  else
    Editor.BlockBegin := Cells[CurrentCell].LogStart;
end;

procedure TSynPluginTemplateEdit.CellCaretHome;
begin
  if (CurrentCell < 0) and (FLastCell >= 0) then
    CurrentCell := FLastCell;
  if (CurrentCell < 0) then
    exit;
  CaretObj.LineBytePos := Cells[CurrentCell].LogStart;
  Editor.BlockBegin := Cells[CurrentCell].LogStart;
end;

procedure TSynPluginTemplateEdit.CellCaretEnd;
begin
  if (CurrentCell < 0) and (FLastCell >= 0) then
    CurrentCell := FLastCell;
  if (CurrentCell < 0) then
    exit;
  CaretObj.LineBytePos := Cells[CurrentCell].LogEnd;
  Editor.BlockBegin := Cells[CurrentCell].LogEnd;
end;

procedure TSynPluginTemplateEdit.SetFinalCaret;
var
  c: TSynPluginSyncEditCell;
begin
  c := Cells.GroupCell[-2, 0];
  Editor.BlockBegin := c.LogStart;
  CaretObj.IncForcePastEOL;
  CaretObj.LineBytePos := c.LogStart;
  CaretObj.DecForcePastEOL;
  Cells.Clear;
  Active := False;
end;

procedure TSynPluginTemplateEdit.SetTemplate(aTmpl: String; aCaretPos: TPoint);
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
    LogStart := aCaretPos;
    LogEnd := aCaretPos;
  end;
  if (Cells.Count > 1) then begin
    Active := True;
    CurrentCell := 0;
    CaretObj.LineBytePos := Cells[0].LogStart;
    SelectCurrentCell;
  end
  else
    Editor.MoveCaretIgnoreEOL(aCaretPos);
end;

procedure TSynPluginTemplateEdit.AddEditCells(aCellList: TSynPluginSyncEditList);
var
  i, XOffs, YOffs: Integer;
  CurCell: TSynPluginSyncEditCell;
  CaretPos: TSynPluginSyncEditCell;
begin
  CaretPos := nil;
  if Cells.GroupCell[-2, 0] <> nil then begin
    CaretPos := TSynPluginSyncEditCell.Create;
    CaretPos.Assign(Cells.GroupCell[-2, 0]);
  end;
  Cells.Clear;

  XOffs := FStartPoint.x - 1;
  YOffs := FStartPoint.y - 1;
  for i := 0 to aCellList.Count - 1 do begin
    CurCell := aCellList[i];
    with Cells.AddNew do begin;
      Assign(CurCell);
      if LogStart.y = 1 then
        LogStart.x := LogStart.x + XOffs;
      LogStart.y := LogStart.y + YOffs;
      if LogEnd.y = 1 then
        LogEnd.x := LogEnd.x + XOffs;
      LogEnd.y := LogEnd.y + YOffs;
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

initialization
  KeyOffset := AllocatePluginKeyRange(ecSynPTmplEdLast + 1);
  KeyOffsetOff := AllocatePluginKeyRange(ecSynPTmplEdLast + 1);

end.

