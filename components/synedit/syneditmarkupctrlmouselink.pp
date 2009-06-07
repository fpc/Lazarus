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
  SynEditMouseCmds, Controls, LCLProc;

type

  { TSynEditMarkupCtrlMouseLink }

  TSynEditMarkupCtrlMouseLink = class(TSynEditMarkup)
  private
    FCtrlMouseLine: Integer;
    FCtrlMouseX1: Integer;
    FCtrlMouseX2: Integer;
    FCurX1, FCurX2: Integer;

    FLastControlIsPressed: boolean;
    FLastMouseCaret: TPoint;
    procedure SetLastMouseCaret(const AValue: TPoint);

  public
    procedure UpdateCtrlState;
    procedure UpdateCtrlMouse;
    function  IsCtrlMouseShiftState(AShift: TShiftState): Boolean;
    property LastMouseCaret: TPoint read FLastMouseCaret write SetLastMouseCaret;
  public
    constructor Create(ASynEdit: TCustomControl);

    Procedure EndMarkup; override;
    Procedure PrepareMarkupForRow(aRow: Integer); override;
    Function GetMarkupAttributeAtRowCol(const aRow, aCol: Integer) : TSynSelectedColor; override;
    Function GetNextMarkupColAfterRowCol(const aRow, aCol: Integer) : Integer; override;

    property CtrlMouseLine : Integer read FCtrlMouseLine write FCtrlMouseLine;
    property CtrlMouseX1 : Integer read FCtrlMouseX1 write FCtrlMouseX1;
    property CtrlMouseX2 : Integer read FCtrlMouseX2 write FCtrlMouseX2;
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

procedure TSynEditMarkupCtrlMouseLink.UpdateCtrlState;
begin
  if FLastControlIsPressed <> IsCtrlMouseShiftState(GetKeyShiftState) then
    UpdateCtrlMouse;
end;

procedure TSynEditMarkupCtrlMouseLink.UpdateCtrlMouse;

  procedure doNotShowLink;
  begin
    if CtrlMouseLine > 0 then
      TSynEdit(SynEdit).Invalidate;
    TSynEdit(SynEdit).Cursor := crIBeam;
    CtrlMouseLine:=-1;
  end;

var
  NewY, NewX1, NewX2: Integer;
begin
  FLastControlIsPressed := IsCtrlMouseShiftState(GetKeyShiftState);
  if FLastControlIsPressed and (LastMouseCaret.X>0) and (LastMouseCaret.Y>0) then begin
    // show link
    NewY := LastMouseCaret.Y;
    TSynEdit(SynEdit).GetWordBoundsAtRowCol(TSynEdit(SynEdit).PhysicalToLogicalPos(LastMouseCaret),NewX1,NewX2);
    if TSynEdit(SynEdit).IsLinkable(NewY, NewX1, NewX2) then begin
      // there is a word to underline as link
      if (NewY <> CtrlMouseLine)
      or (NewX1 <> CtrlMouseX1)
      or (NewX2 <> CtrlMouseX2)
      then begin
        CtrlMouseLine := fLastMouseCaret.Y;
        CtrlMouseX1 := NewX1;
        CtrlMouseX2 := NewX2;
        TSynEdit(SynEdit).Invalidate;
        TSynEdit(SynEdit).Cursor := crHandPoint;
      end;
    end
    else
      doNotShowLink // there is no link
  end else
    doNotShowLink;
end;

function TSynEditMarkupCtrlMouseLink.IsCtrlMouseShiftState(AShift: TShiftState): Boolean;
var
  act: TSynEditMouseAction;
  i: Integer;
begin
  Result := False;
  // todo: check FMouseSelActions if over selection?
  for i := 0 to TSynEdit(SynEdit).MouseActions.Count - 1 do begin
    act := TSynEdit(SynEdit).MouseActions.Items[i];
    if (act.Command = emcMouseLink) and (act.Option = emcoMouseLinkShow) and
       act.IsMatchingShiftState(AShift)
    then
      exit(True);
  end;
end;

constructor TSynEditMarkupCtrlMouseLink.Create(ASynEdit: TCustomControl);
begin
  inherited Create(ASynEdit);
  FLastControlIsPressed := false;
  FCtrlMouseLine:=-1;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
  MarkupInfo.Foreground := clBlue; {TODO:  invert blue to bg .... see below}
  MarkupInfo.Background := clNone;
end;

procedure TSynEditMarkupCtrlMouseLink.EndMarkup;
var
  LineLeft, LineTop, LineRight: integer;
  temp : TPoint;
begin
  if FCtrlMouseLine < 1 then exit;
  LineTop := (RowToScreenRow(FCtrlMouseLine)+1)*TSynEdit(SynEdit).LineHeight-1;
  Temp := LogicalToPhysicalPos(Point(FCtrlMouseX1, FCtrlMouseLine));
  LineLeft := TSynEdit(SynEdit).ScreenColumnToXValue(Temp.x);
  Temp := LogicalToPhysicalPos(Point(FCtrlMouseX2, FCtrlMouseLine));
  LineRight := TSynEdit(SynEdit).ScreenColumnToXValue(Temp.x);
  with TSynEdit(SynEdit).Canvas do begin
    Pen.Color := MarkupInfo.Foreground;
    MoveTo(LineLeft,LineTop);
    LineTo(LineRight,LineTop);
  end;
end;

procedure TSynEditMarkupCtrlMouseLink.PrepareMarkupForRow(aRow: Integer);
begin
  inherited PrepareMarkupForRow(aRow);
  if aRow = FCtrlMouseLine then begin
    FCurX1 := LogicalToPhysicalPos(Point(FCtrlMouseX1, FCtrlMouseLine)).x;
    FCurX2 := LogicalToPhysicalPos(Point(FCtrlMouseX2, FCtrlMouseLine)).x;
  end;
end;

function TSynEditMarkupCtrlMouseLink.GetMarkupAttributeAtRowCol(const aRow, aCol: Integer) : TSynSelectedColor;
begin
  Result := nil;
  if (aRow <> FCtrlMouseLine) or ((aCol < FCurX1) or (aCol >= FCurX2))
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

