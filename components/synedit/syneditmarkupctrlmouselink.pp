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
unit SynEditMarkupCtrlMouseLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditMarkup, SynEditMiscClasses,
  SynEditMouseCmds, LazSynEditText, SynEditTypes, Controls, LCLProc;

type

  { TSynEditMarkupCtrlMouseLink }

  TSynEditMarkupCtrlMouseLink = class(TSynEditMarkup)
  private
    FCtrlMouseLine: Integer;
    FCtrlMouseX1: Integer;
    FCtrlMouseX2: Integer;
    FCtrlLinkable: Boolean;
    FCursor: TCursor;

    FLastControlIsPressed: boolean;
    FLastMouseCaret: TPoint;
    FLastMouseCaretLogical: TPoint;
    function GetIsMouseOverLink: Boolean;
    procedure SetCursor(AValue: TCursor);
    procedure SetLastMouseCaret(const AValue: TPoint);
    Procedure LinesChanged(Sender: TSynEditStrings; AIndex, ANewCount, AOldCount : Integer);
    function  IsCtrlMouseShiftState(AShift: TShiftState; OnlyShowLink: Boolean): Boolean;
    procedure InternalUpdateCtrlMouse;
    procedure UpdateSynCursor(Sender: TObject; const AMouseLocation: TSynMouseLocationInfo;
    var AnCursor: TCursor; var APriority: Integer; var AChangedBy: TObject);
  protected
    procedure SetLines(const AValue : TSynEditStringsLinked); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoEnabledChanged(Sender: TObject); override;
  public
    procedure UpdateCtrlState(aShift: TShiftState);
    procedure UpdateCtrlMouse;
    property LastMouseCaret: TPoint read FLastMouseCaret write SetLastMouseCaret;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;

    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;

    property CtrlMouseLine : Integer read FCtrlMouseLine write FCtrlMouseLine;
    property CtrlMouseX1 : Integer read FCtrlMouseX1 write FCtrlMouseX1;
    property CtrlMouseX2 : Integer read FCtrlMouseX2 write FCtrlMouseX2;
    property IsMouseOverLink: Boolean read GetIsMouseOverLink;
    property Cursor: TCursor read FCursor;
  end;

implementation
uses SynEdit;

const
  LINK_CURSOR_PRIORITY = 1;

{ TSynEditMarkupCtrlMouseLink }

procedure TSynEditMarkupCtrlMouseLink.SetLastMouseCaret(const AValue: TPoint);
begin
  if (FLastMouseCaret.X = AValue.X) and (FLastMouseCaret.Y = AValue.Y) then exit;
  FLastMouseCaret := AValue;
  if LastMouseCaret.y > 0
  then FLastMouseCaretLogical := Lines.PhysicalToLogicalPos(LastMouseCaret)
  else FLastMouseCaretLogical := LastMouseCaret;
  UpdateCtrlMouse;
end;

function TSynEditMarkupCtrlMouseLink.GetIsMouseOverLink: Boolean;
var
  NewCtrlIsPressed: Boolean;
begin
  // Normal checks only take Ctrl-State for ShowLink into account (since the cursor needs updates)
  // Here we need to check for Hiden-Links too
  NewCtrlIsPressed := IsCtrlMouseShiftState(GetKeyShiftState, False);
  if FLastControlIsPressed <> NewCtrlIsPressed then begin
    FLastControlIsPressed := NewCtrlIsPressed;
    InternalUpdateCtrlMouse;
  end;

  Result := FCtrlLinkable and (FCtrlMouseLine >= 0);
end;

procedure TSynEditMarkupCtrlMouseLink.SetCursor(AValue: TCursor);
begin
  if FCursor = AValue then Exit;
  FCursor := AValue;
  TCustomSynEdit(SynEdit).UpdateCursorOverride;
end;

procedure TSynEditMarkupCtrlMouseLink.LinesChanged(Sender: TSynEditStrings; AIndex, ANewCount,
  AOldCount: Integer);
begin
  If LastMouseCaret.Y < 0 then exit;
  LastMouseCaret := Point(-1, -1);
  UpdateCtrlMouse;
end;

procedure TSynEditMarkupCtrlMouseLink.UpdateCtrlState(aShift: TShiftState);
var
  NewCtrlIsPressed: Boolean;
begin
  NewCtrlIsPressed := IsCtrlMouseShiftState(aShift, True);
  if FLastControlIsPressed <> NewCtrlIsPressed then begin
    FLastControlIsPressed := NewCtrlIsPressed;
    InternalUpdateCtrlMouse;
  end;
end;

procedure TSynEditMarkupCtrlMouseLink.UpdateCtrlMouse;
begin
  FLastControlIsPressed := IsCtrlMouseShiftState(GetKeyShiftState, True);
  InternalUpdateCtrlMouse;
end;

procedure TSynEditMarkupCtrlMouseLink.InternalUpdateCtrlMouse;

  procedure doNotShowLink;
  begin
    if FCtrlMouseLine >= 0 then
      InvalidateSynLines(FCtrlMouseLine, FCtrlMouseLine);
    SetCursor(crDefault);
    CtrlMouseLine:=-1;
    FCtrlLinkable := False;
  end;

var
  NewY, NewX1, NewX2: Integer;
begin
  if FLastControlIsPressed and (LastMouseCaret.X>0) and (LastMouseCaret.Y>0) then begin
    // show link
    NewY := LastMouseCaret.Y;
    TCustomSynEdit(SynEdit).GetWordBoundsAtRowCol(FLastMouseCaretLogical,NewX1,NewX2);
    if (NewY = CtrlMouseLine) and
       (NewX1 = CtrlMouseX1) and
       (NewX2 = CtrlMouseX2)
    then
      exit;
    if (FCtrlMouseLine >= 0) and (FCtrlMouseLine <> NewY) then
      InvalidateSynLines(FCtrlMouseLine, FCtrlMouseLine);
    FCtrlLinkable := TCustomSynEdit(SynEdit).IsLinkable(NewY, NewX1, NewX2);
    CtrlMouseLine := fLastMouseCaret.Y;
    CtrlMouseX1 := NewX1;
    CtrlMouseX2 := NewX2;
    InvalidateSynLines(FCtrlMouseLine, FCtrlMouseLine);
    if FCtrlLinkable then
      SetCursor(crHandPoint)
    else
      doNotShowLink;
  end else
    doNotShowLink;
end;

procedure TSynEditMarkupCtrlMouseLink.UpdateSynCursor(Sender: TObject;
  const AMouseLocation: TSynMouseLocationInfo; var AnCursor: TCursor; var APriority: Integer;
  var AChangedBy: TObject);
begin
  if (Cursor = crDefault) or (APriority > LINK_CURSOR_PRIORITY) then exit;
  AnCursor := Cursor;
  APriority := LINK_CURSOR_PRIORITY;
  AChangedBy := Self;
end;

function TSynEditMarkupCtrlMouseLink.IsCtrlMouseShiftState(AShift: TShiftState;
  OnlyShowLink: Boolean): Boolean;
var
  act: TSynEditMouseAction;
  i: Integer;
begin
  Result := False;

  if not (emUseMouseActions in TCustomSynEdit(SynEdit).MouseOptions) then begin
    Result := (emShowCtrlMouseLinks in TCustomSynEdit(SynEdit).MouseOptions) and
              (AShift * ([ssShift, ssCtrl, ssAlt] + [SYNEDIT_LINK_MODIFIER]) = [SYNEDIT_LINK_MODIFIER]);
    exit;
  end;

  // todo: check FMouseSelActions if over selection?
  for i := 0 to TCustomSynEdit(SynEdit).MouseActions.Count - 1 do begin
    act := TCustomSynEdit(SynEdit).MouseActions.Items[i];
    if (act.Command = emcMouseLink) and
       ( (act.Option = emcoMouseLinkShow) or (not OnlyShowLink) ) and
       act.IsMatchingShiftState(AShift)
    then
      exit(True);
  end;

  for i := 0 to TCustomSynEdit(SynEdit).MouseTextActions.Count - 1 do begin
    act := TCustomSynEdit(SynEdit).MouseTextActions.Items[i];
    if (act.Command = emcMouseLink) and
       ( (act.Option = emcoMouseLinkShow) or (not OnlyShowLink) ) and
       act.IsMatchingShiftState(AShift)
    then
      exit(True);
  end;

  if not TCustomSynEdit(SynEdit).SelAvail then exit;

  for i := 0 to TCustomSynEdit(SynEdit).MouseSelActions.Count - 1 do begin
    act := TCustomSynEdit(SynEdit).MouseSelActions.Items[i];
    if (act.Command = emcMouseLink) and
       ( (act.Option = emcoMouseLinkShow) or (not OnlyShowLink) ) and
       act.IsMatchingShiftState(AShift)
    then
      exit(True);
  end;
end;

constructor TSynEditMarkupCtrlMouseLink.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);
  FLastControlIsPressed := false;
  FCtrlMouseLine:=-1;
  FCtrlLinkable := False;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
  MarkupInfo.Foreground := clBlue; {TODO:  invert blue to bg .... see below}
  MarkupInfo.Background := clNone;

  TCustomSynEdit(SynEdit).RegisterQueryMouseCursorHandler(@UpdateSynCursor);
end;

destructor TSynEditMarkupCtrlMouseLink.Destroy;
begin
  TCustomSynEdit(SynEdit).UnregisterQueryMouseCursorHandler(@UpdateSynCursor);
  if Lines <> nil then begin;
    Lines.RemoveModifiedHandler(senrLinesModified, @LinesChanged);
  end;
  inherited Destroy;
end;

procedure TSynEditMarkupCtrlMouseLink.SetLines(
  const AValue: TSynEditStringsLinked);
begin
  inherited SetLines(AValue);
  if Lines <> nil then begin;
    Lines.AddModifiedHandler(senrLinesModified, @LinesChanged);
  end;
end;

procedure TSynEditMarkupCtrlMouseLink.DoMarkupChanged(AMarkup: TSynSelectedColor
  );
begin
  inherited DoMarkupChanged(AMarkup);
  if FCtrlMouseLine >= 0 then
    InvalidateSynLines(FCtrlMouseLine, FCtrlMouseLine);
end;

procedure TSynEditMarkupCtrlMouseLink.DoEnabledChanged(Sender: TObject);
begin
  inherited DoEnabledChanged(Sender);
  if FCtrlMouseLine >= 0 then
    InvalidateSynLines(FCtrlMouseLine, FCtrlMouseLine);
end;

function TSynEditMarkupCtrlMouseLink.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
begin
  Result := nil;
  if (not FCtrlLinkable) or (aRow <> FCtrlMouseLine) or
     ((aStartCol.Logical < CtrlMouseX1) or (aStartCol.Logical >= CtrlMouseX2))
  then exit;
  Result := MarkupInfo;
  MarkupInfo.SetFrameBoundsLog(CtrlMouseX1, CtrlMouseX2);
end;

procedure TSynEditMarkupCtrlMouseLink.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
begin
  ANextLog := -1;
  ANextPhys := -1;
  if FCtrlMouseLine <> aRow
  then exit;

  if aStartCol.Logical < CtrlMouseX1
  then ANextLog := CtrlMouseX1;
  if (aStartCol.Logical < CtrlMouseX2) and (aStartCol.Logical >= CtrlMouseX1)
  then ANextLog := CtrlMouseX2;
end;

end.

