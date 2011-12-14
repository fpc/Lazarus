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
  SynEditMouseCmds, LazSynEditText, Controls, LCLProc;

type

  { TSynEditMarkupCtrlMouseLink }

  TSynEditMarkupCtrlMouseLink = class(TSynEditMarkup)
  private
    FCtrlMouseLine: Integer;
    FCtrlMouseX1: Integer;
    FCtrlMouseX2: Integer;
    FCtrlLinkable: Boolean;
    FCursor: TCursor;
    FCurX1, FCurX2: Integer;

    FLastControlIsPressed: boolean;
    FLastMouseCaret: TPoint;
    function GetIsMouseOverLink: Boolean;
    procedure SetLastMouseCaret(const AValue: TPoint);
    Procedure LinesChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    function  IsCtrlMouseShiftState(AShift: TShiftState; OnlyShowLink: Boolean): Boolean;
    procedure InternalUpdateCtrlMouse;
  protected
    procedure SetLines(const AValue : TSynEditStrings); override;
  public
    procedure UpdateCtrlState(aShift: TShiftState);
    procedure UpdateCtrlMouse;
    property LastMouseCaret: TPoint read FLastMouseCaret write SetLastMouseCaret;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;

    Procedure EndMarkup; override;
    Procedure PrepareMarkupForRow(aRow: Integer); override;
    Function GetMarkupAttributeAtRowCol(const aRow, aCol: Integer) : TSynSelectedColor; override;
    Function GetNextMarkupColAfterRowCol(const aRow, aCol: Integer) : Integer; override;

    property CtrlMouseLine : Integer read FCtrlMouseLine write FCtrlMouseLine;
    property CtrlMouseX1 : Integer read FCtrlMouseX1 write FCtrlMouseX1;
    property CtrlMouseX2 : Integer read FCtrlMouseX2 write FCtrlMouseX2;
    property IsMouseOverLink: Boolean read GetIsMouseOverLink;
    property Cursor: TCursor read FCursor;
  end;

implementation
uses SynEdit;

{ TSynEditMarkupCtrlMouseLink }

procedure TSynEditMarkupCtrlMouseLink.SetLastMouseCaret(const AValue: TPoint);
begin
  if (FLastMouseCaret.X = AValue.X) and (FLastMouseCaret.Y = AValue.Y) then exit;
  FLastMouseCaret := AValue;
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

procedure TSynEditMarkupCtrlMouseLink.LinesChanged(Sender: TSynEditStrings; AIndex,
  ACount: Integer);
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
    if CtrlMouseLine > 0 then
      TCustomSynEdit(SynEdit).Invalidate;
    FCursor := crDefault;
    CtrlMouseLine:=-1;
    FCtrlLinkable := False;
  end;

var
  NewY, NewX1, NewX2: Integer;
begin
  if FLastControlIsPressed and (LastMouseCaret.X>0) and (LastMouseCaret.Y>0) then begin
    // show link
    NewY := LastMouseCaret.Y;
    TCustomSynEdit(SynEdit).GetWordBoundsAtRowCol(Lines.PhysicalToLogicalPos(LastMouseCaret),NewX1,NewX2);
    if (NewY = CtrlMouseLine) and
       (NewX1 = CtrlMouseX1) and
       (NewX2 = CtrlMouseX2)
    then
      exit;
    FCtrlLinkable := TCustomSynEdit(SynEdit).IsLinkable(NewY, NewX1, NewX2);
    CtrlMouseLine := fLastMouseCaret.Y;
    CtrlMouseX1 := NewX1;
    CtrlMouseX2 := NewX2;
    TCustomSynEdit(SynEdit).Invalidate;
    if FCtrlLinkable then
      FCursor := crHandPoint
    else
      doNotShowLink;
  end else
    doNotShowLink;
end;

function TSynEditMarkupCtrlMouseLink.IsCtrlMouseShiftState(AShift: TShiftState;
  OnlyShowLink: Boolean): Boolean;
var
  act: TSynEditMouseAction;
  i: Integer;
begin
  Result := False;
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
end;

destructor TSynEditMarkupCtrlMouseLink.Destroy;
begin
  if Lines <> nil then begin;
    Lines.RemoveChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LinesChanged);
    Lines.RemoveChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LinesChanged);
  end;
  inherited Destroy;
end;

procedure TSynEditMarkupCtrlMouseLink.SetLines(const AValue: TSynEditStrings);
begin
  inherited SetLines(AValue);
  if Lines <> nil then begin;
    Lines.AddChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LinesChanged);
    Lines.AddChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LinesChanged);
  end;
end;

procedure TSynEditMarkupCtrlMouseLink.EndMarkup;
var
  LineLeft, LineTop, LineRight: integer;
  temp : TPoint;
begin
  if (FCtrlMouseLine < 1) or (not FCtrlLinkable) then exit;
  LineTop := (RowToScreenRow(FCtrlMouseLine)+1)*TCustomSynEdit(SynEdit).LineHeight-1;
  Temp := LogicalToPhysicalPos(Point(FCtrlMouseX1, FCtrlMouseLine));
  LineLeft := TCustomSynEdit(SynEdit).ScreenColumnToXValue(Temp.x);
  Temp := LogicalToPhysicalPos(Point(FCtrlMouseX2, FCtrlMouseLine));
  LineRight := TCustomSynEdit(SynEdit).ScreenColumnToXValue(Temp.x);
  with TCustomSynEdit(SynEdit).Canvas do begin
    Pen.Color := MarkupInfo.Foreground;
    MoveTo(LineLeft,LineTop);
    LineTo(LineRight,LineTop);
  end;
end;

procedure TSynEditMarkupCtrlMouseLink.PrepareMarkupForRow(aRow: Integer);
begin
  inherited PrepareMarkupForRow(aRow);
  if (aRow = FCtrlMouseLine) and FCtrlLinkable then begin
    FCurX1 := LogicalToPhysicalPos(Point(FCtrlMouseX1, FCtrlMouseLine)).x;
    FCurX2 := LogicalToPhysicalPos(Point(FCtrlMouseX2, FCtrlMouseLine)).x;
  end;
end;

function TSynEditMarkupCtrlMouseLink.GetMarkupAttributeAtRowCol(const aRow, aCol: Integer) : TSynSelectedColor;
begin
  Result := nil;
  if (not FCtrlLinkable) or (aRow <> FCtrlMouseLine) or
     ((aCol < FCurX1) or (aCol >= FCurX2))
  then exit;
  Result := MarkupInfo;
  MarkupInfo.StartX := FCurX1;
  MarkupInfo.EndX := FCurX2;
end;

function TSynEditMarkupCtrlMouseLink.GetNextMarkupColAfterRowCol(const aRow, aCol: Integer) : Integer;
begin
  Result := -1;
  if FCtrlMouseLine <> aRow
  then exit;

  if aCol < FCurX1
  then Result := FCurX1;
  if (aCol < FCurX2) and (aCol >= FCurX1)
  then Result := FCurX2;
end;

end.

