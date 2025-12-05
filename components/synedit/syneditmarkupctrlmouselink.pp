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
  Classes, SysUtils, Graphics, Controls, SynEditMarkup, SynEditMiscClasses,
  SynEditMouseCmds, LazSynEditText, SynEditTypes, LazEditTextAttributes, LazEditMiscProcs;

type

  TSynMarkupLinkInfoResult = (
    liValid,  //
    liNoData  // same as setting StartPos to negative Y / quick way to return "no link"
  );

  TSynMarkupLinkInfo=record
    StartPos: TLogPoint;
    EndPos: TLogPoint;
    IsLinkable: boolean; // should be set to true en the event to enable the link.
  end;

  TSynMarkupLinkGetBoundsEvent = procedure(ASender:TObject;
                                           var AnInfoState: TSynMarkupLinkInfoResult;
                                           var ALinkData: TSynMarkupLinkInfo
                                          ) of object;

  TSynEditMarkupCtrlMouseLink = class(TSynEditMarkup)
  private
    FCurrentLink: TSynMarkupLinkInfo;
    FCursor: TCursor;

    FLastControlIsPressed: boolean;
    FLastMouseCaret: TPoint;
    FLastMouseCaretLogical: TPoint;

    FOnGetLinkInfo: TSynMarkupLinkGetBoundsEvent;

    function GetIsMouseOverLink: Boolean;
    procedure SetCursor(AValue: TCursor);
    procedure SetLastMouseCaret(const AValue: TPoint);
    Procedure LinesChanged(Sender: TSynEditStrings; AIndex, ANewCount, AOldCount : Integer);
    function  IsCtrlMouseShiftState(AShift: TShiftState; OnlyShowLink: Boolean): Boolean;
    procedure InternalUpdateCtrlMouse;
    procedure UpdateSynCursor(Sender: TObject; const AMouseLocation: TSynMouseLocationInfo;
    var AnCursor: TCursor; var APriority: Integer; var AChangedBy: TObject);
    procedure LastCaretChanged(Sender: TObject; ACaret:TPoint);
    procedure KeyUpDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure SetLines(const AValue : TSynEditStringsLinked); override;
    procedure DoMarkupChanged(AMarkup: TLazEditTextAttribute); override;
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
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TLazEditTextAttributeModifier; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;

    property CtrlMouseLine : Integer read FCurrentLink.StartPos.Y write FCurrentLink.StartPos.Y; deprecated 'use LinkStartPos or LinkEndPos / to be removed in 5.99';
    property CtrlMouseX1 : Integer read FCurrentLink.StartPos.X write FCurrentLink.StartPos.X; deprecated 'use LinkStartPos / to be removed in 5.99';
    property CtrlMouseX2 : Integer read FCurrentLink.EndPos.X write FCurrentLink.EndPos.X; deprecated 'use LinkEndPos / to be removed in 5.99';
    property LinkStartPos: TLogPoint read FCurrentLink.StartPos write FCurrentLink.StartPos;
    property LinkEndPos: TLogPoint read FCurrentLink.EndPos write FCurrentLink.EndPos;
    property IsMouseOverLink: Boolean read GetIsMouseOverLink;
    property Cursor: TCursor read FCursor;
    property OnGetLinkInfo: TSynMarkupLinkGetBoundsEvent read FOnGetLinkInfo write FOnGetLinkInfo;
  end;

implementation

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

  Result := FCurrentLink.IsLinkable and (FCurrentLink.StartPos.Y >= 0);
end;

procedure TSynEditMarkupCtrlMouseLink.SetCursor(AValue: TCursor);
begin
  if FCursor = AValue then Exit;
  FCursor := AValue;
  SynEdit.UpdateCursorOverride;
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
    if FCurrentLink.StartPos.Y >= 0 then
      InvalidateSynLines(FCurrentLink.StartPos.Y, FCurrentLink.EndPos.Y);
    SetCursor(crDefault);

    FCurrentLink.IsLinkable := False;
  end;

var
  NewY, NewX1, NewX2: Integer;
  len:integer;
  InfoState: TSynMarkupLinkInfoResult;
  NewLinkInfo: TSynMarkupLinkInfo;
begin
  if FLastControlIsPressed and (LastMouseCaret.X>0) and (LastMouseCaret.Y>0) then begin
    // show link
    NewY := LastMouseCaret.Y;

    NewLinkInfo := Default(TSynMarkupLinkInfo);
    if Assigned(FOnGetLinkInfo) then begin
      NewLinkInfo := FCurrentLink;
      InfoState := liNoData;
      if FCurrentLink.StartPos.Y >= 0 then
        InfoState := liValid;
      OnGetLinkInfo(Self, InfoState, NewLinkInfo);
      if InfoState <> liValid then begin
        NewLinkInfo.StartPos.Y := -1;
        NewLinkInfo.IsLinkable := False;
      end;
      if (FCurrentLink.StartPos   = NewLinkInfo.StartPos) and
         (FCurrentLink.EndPos     = NewLinkInfo.EndPos) and
         (FCurrentLink.IsLinkable = NewLinkInfo.IsLinkable)
      then
        exit;
    end
    else begin
      SynEdit.GetWordBoundsAtRowCol(FLastMouseCaretLogical,NewX1,NewX2);
      if (NewY  = FCurrentLink.StartPos.Y) and
         (NewY  = FCurrentLink.EndPos.Y) and
         (NewX1 = FCurrentLink.StartPos.X) and
         (NewX2 = FCurrentLink.EndPos.X)
      then
        exit;

      NewLinkInfo.StartPos.Y := NewY;
      NewLinkInfo.EndPos.Y := NewY;
      NewLinkInfo.StartPos.X := NewX1;
      NewLinkInfo.EndPos.X := NewX2;
      NewLinkInfo.IsLinkable := SynEdit.IsLinkable(NewY, NewX1, NewX2);
    end;

    if FCurrentLink.IsLinkable and (FCurrentLink.StartPos.Y >= 0) and
       ( (FCurrentLink.StartPos.Y < NewLinkInfo.StartPos.Y) or
         (FCurrentLink.EndPos.Y   > NewLinkInfo.EndPos.Y) or
         (not NewLinkInfo.IsLinkable) or
         (NewLinkInfo.EndPos < NewLinkInfo.StartPos) or
         (NewLinkInfo.StartPos.Y < 0)
       )
    then
      InvalidateSynLines(FCurrentLink.StartPos.Y, FCurrentLink.EndPos.Y);

    FCurrentLink := NewLinkInfo;

    if FCurrentLink.IsLinkable and (FCurrentLink.StartPos.Y >= 0) then
      InvalidateSynLines(FCurrentLink.StartPos.Y, FCurrentLink.EndPos.Y);

    if FCurrentLink.IsLinkable then // todo: what should happen if start > end
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

procedure TSynEditMarkupCtrlMouseLink.LastCaretChanged(Sender: TObject; ACaret: TPoint);
begin
  LastMouseCaret := ACaret;
end;

procedure TSynEditMarkupCtrlMouseLink.KeyUpDownEvent(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  UpdateCtrlState(Shift);
end;

function TSynEditMarkupCtrlMouseLink.IsCtrlMouseShiftState(AShift: TShiftState;
  OnlyShowLink: Boolean): Boolean;
var
  act: TSynEditMouseAction;
  i: Integer;
begin
  Result := False;

  if not (emUseMouseActions in SynEdit.MouseOptions) then begin
    Result := (emShowCtrlMouseLinks in SynEdit.MouseOptions) and
              (AShift * ([ssShift, ssCtrl, ssAlt] + [SYNEDIT_LINK_MODIFIER]) = [SYNEDIT_LINK_MODIFIER]);
    exit;
  end;

  // todo: check FMouseSelActions if over selection?
  for i := 0 to SynEdit.MouseActions.Count - 1 do begin
    act := SynEdit.MouseActions.Items[i];
    if (act.Command = emcMouseLink) and
       ( (act.Option = emcoMouseLinkShow) or (not OnlyShowLink) ) and
       act.IsMatchingShiftState(AShift)
    then
      exit(True);
  end;

  for i := 0 to SynEdit.MouseTextActions.Count - 1 do begin
    act := SynEdit.MouseTextActions.Items[i];
    if (act.Command = emcMouseLink) and
       ( (act.Option = emcoMouseLinkShow) or (not OnlyShowLink) ) and
       act.IsMatchingShiftState(AShift)
    then
      exit(True);
  end;

  if not SynEdit.SelAvail then exit;

  for i := 0 to SynEdit.MouseSelActions.Count - 1 do begin
    act := SynEdit.MouseSelActions.Items[i];
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
  FCurrentLink.StartPos.Y:=-1;
  FCurrentLink.IsLinkable := False;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
  MarkupInfo.Foreground := clBlue; {TODO:  invert blue to bg .... see below}
  MarkupInfo.Background := clNone;

  SynEdit.RegisterQueryMouseCursorHandler(@UpdateSynCursor);
  SynEdit.RegisterMouseLastCaretHandler(@LastCaretChanged);
  SynEdit.RegisterBeforeKeyDownHandler(@KeyUpDownEvent);
  SynEdit.RegisterBeforeKeyUpHandler(@KeyUpDownEvent);
end;

destructor TSynEditMarkupCtrlMouseLink.Destroy;
begin
  SynEdit.UnRegisterBeforeKeyUpHandler(@KeyUpDownEvent);
  SynEdit.UnRegisterBeforeKeyDownHandler(@KeyUpDownEvent);
  SynEdit.UnRegisterMouseLastCaretHandler(@LastCaretChanged);
  SynEdit.UnregisterQueryMouseCursorHandler(@UpdateSynCursor);
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

procedure TSynEditMarkupCtrlMouseLink.DoMarkupChanged(AMarkup: TLazEditTextAttribute
  );
begin
  inherited DoMarkupChanged(AMarkup);
  if FCurrentLink.StartPos.Y >= 0 then
    InvalidateSynLines(FCurrentLink.StartPos.Y, FCurrentLink.StartPos.Y);
end;

procedure TSynEditMarkupCtrlMouseLink.DoEnabledChanged(Sender: TObject);
begin
  inherited DoEnabledChanged(Sender);
  if FCurrentLink.StartPos.Y >= 0 then
    InvalidateSynLines(FCurrentLink.StartPos.Y, FCurrentLink.StartPos.Y);
end;

function TSynEditMarkupCtrlMouseLink.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TLazEditTextAttributeModifier;
begin
  Result := nil;
  if (not FCurrentLink.IsLinkable) or (aRow <> FCurrentLink.StartPos.Y) or
     ((aStartCol.Logical < FCurrentLink.StartPos.X) or (aStartCol.Logical >= FCurrentLink.EndPos.X))
  then exit;
  Result := MarkupInfo;
  MarkupInfo.SetFrameBoundsLog(FCurrentLink.StartPos.X, FCurrentLink.EndPos.X);
end;

procedure TSynEditMarkupCtrlMouseLink.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
begin
  ANextLog := -1;
  ANextPhys := -1;
  if FCurrentLink.StartPos.Y <> aRow
  then exit;

  if aStartCol.Logical < FCurrentLink.StartPos.X
  then ANextLog := FCurrentLink.StartPos.X;
  if (aStartCol.Logical < FCurrentLink.EndPos.X) and (aStartCol.Logical >= FCurrentLink.StartPos.X)
  then ANextLog := FCurrentLink.EndPos.X;
end;

end.

