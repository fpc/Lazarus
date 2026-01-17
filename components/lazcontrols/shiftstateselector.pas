unit ShiftStateSelector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, LCLStrConsts;

type

  { TCustomShiftStateSelector }

  TCustomShiftStateSelector = class(TWinControl)
  private
    FLocked: Boolean;
    FHasMask: Boolean;
    FShiftMask: TShiftState;
    FShiftSelection: TShiftState;
    FVisibleShift: TShiftState;
    FCheckBoxes: array [TShiftStateEnum] of TCheckBox;
    FCheckBoxNames: array [TShiftStateEnum] of String;

    procedure DoCheckBoxChanged(Sender: TObject);
    function GetShiftNames(AShift: TShiftStateEnum): String;
    function GetSpacing: integer;
    function GetVertical: Boolean;
    procedure SetHasMask(AValue: Boolean);
    procedure SetShiftMask(AValue: TShiftState);
    procedure SetShiftNames(AShift: TShiftStateEnum; AValue: String);
    procedure SetShiftSelection(AValue: TShiftState);
    procedure SetSpacing(AValue: integer);
    procedure SetVertical(AValue: Boolean);
    procedure SetVisibleShift(AValue: TShiftState);
    procedure UpdateNames;
    procedure UpdateVisible;
    procedure UpdateCheckState;
  public
    constructor Create(TheOwner: TComponent); override;
    property ShiftSelection: TShiftState read FShiftSelection write SetShiftSelection default [];
    property ShiftMask: TShiftState    read FShiftMask    write SetShiftMask default [ssShift, ssCtrl, ssAlt];
    property HasMask: Boolean          read FHasMask      write SetHasMask   default False;
    property VisibleShift: TShiftState read FVisibleShift write SetVisibleShift default [ssShift, ssCtrl, ssAlt];
    property Vertical: Boolean         read GetVertical   write SetVertical  default False;
    property Spacing: integer          read GetSpacing    write SetSpacing   default 0;
    property ShiftNames[AShift: TShiftStateEnum]: String read GetShiftNames write SetShiftNames;
  end;

  TShiftStateSelector = class(TCustomShiftStateSelector)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Enabled;
    property Visible;
    property BidiMode;

    property ShiftSelection;
    property ShiftMask;
    property HasMask;
    property VisibleShift;
    property Vertical;
    property Spacing;
    //property ShiftNames; //
  end;

implementation

type

  { TCheckBoxHelper }

  TCheckBoxHelper = class helper for TCheckBox
    procedure SetDsgnVisible(AValue: Boolean);
  end;

{ TCheckBoxHelper }

procedure TCheckBoxHelper.SetDsgnVisible(AValue: Boolean);
begin
    if AValue then
      ControlStyle := ControlStyle - [csNoDesignVisible]
    else
      ControlStyle := ControlStyle + [csNoDesignVisible];
end;

{ TCustomShiftStateSelector }

procedure TCustomShiftStateSelector.SetHasMask(AValue: Boolean);
begin
  if FHasMask = AValue then Exit;
  FHasMask := AValue;
  UpdateCheckState;
end;

function TCustomShiftStateSelector.GetShiftNames(AShift: TShiftStateEnum): String;
begin
  Result := FCheckBoxNames[AShift];
end;

function TCustomShiftStateSelector.GetSpacing: integer;
begin
  Result := ChildSizing.VerticalSpacing;
end;

function TCustomShiftStateSelector.GetVertical: Boolean;
begin
  Result := ChildSizing.Layout = cclTopToBottomThenLeftToRight;
end;

procedure TCustomShiftStateSelector.DoCheckBoxChanged(Sender: TObject);
var
  st: TShiftStateEnum;
begin
  if FLocked then
    exit;

  for st in TShiftStateEnum do begin
    if Sender <> FCheckBoxes[st] then
      continue;

    if FHasMask then begin
      case FCheckBoxes[st].State of
        cbUnchecked: begin
          FShiftSelection := FShiftSelection - [st];
          FShiftMask      := FShiftMask      + [st];
        end;
        cbChecked: begin
          FShiftSelection := FShiftSelection + [st];
          FShiftMask      := FShiftMask      + [st];
        end;
        cbGrayed: begin
          FShiftSelection := FShiftSelection - [st];
          FShiftMask      := FShiftMask      - [st];
        end;
      end;
    end
    else begin
      case FCheckBoxes[st].State of
        cbUnchecked: begin
          FShiftSelection := FShiftSelection - [st];
        end;
        cbChecked: begin
          FShiftSelection := FShiftSelection + [st];
        end;
        cbGrayed: begin
          FShiftSelection := FShiftSelection - [st]; // not possible
        end;
      end;
    end;

    break;
  end;
end;

procedure TCustomShiftStateSelector.SetShiftMask(AValue: TShiftState);
begin
  if FShiftMask = AValue then Exit;
  FShiftMask := AValue;
end;

procedure TCustomShiftStateSelector.SetShiftNames(AShift: TShiftStateEnum; AValue: String);
begin
  if FCheckBoxNames[AShift] = AValue then
    exit;

  FCheckBoxNames[AShift] := AValue;
  UpdateNames;
end;

procedure TCustomShiftStateSelector.SetShiftSelection(AValue: TShiftState);
begin
  if FShiftSelection = AValue then Exit;
  FShiftSelection := AValue;
  UpdateCheckState;
end;

procedure TCustomShiftStateSelector.SetSpacing(AValue: integer);
begin
  ChildSizing.HorizontalSpacing := AValue;
  ChildSizing.VerticalSpacing := AValue;
end;

procedure TCustomShiftStateSelector.SetVertical(AValue: Boolean);
begin
  if AValue then
    ChildSizing.Layout := cclTopToBottomThenLeftToRight
  else
    ChildSizing.Layout := cclLeftToRightThenTopToBottom;
end;

procedure TCustomShiftStateSelector.SetVisibleShift(AValue: TShiftState);
begin
  if FVisibleShift = AValue then Exit;
  FVisibleShift := AValue;
end;

procedure TCustomShiftStateSelector.UpdateNames;
var
  st: TShiftStateEnum;
  s: string;
begin
  FLocked := True;
  for st in TShiftStateEnum do begin
    s := FCheckBoxNames[st];
    if s <> '' then begin
      FCheckBoxes[st].Name := s;
      continue;
    end;
    WriteStr(s, st);
    delete(s, 1, 2);
    case st of
      ssShift: FCheckBoxes[st].Name := ifsVK_SHIFT;
      ssCtrl:  FCheckBoxes[st].Name := ifsCtrl;
      ssAlt:   FCheckBoxes[st].Name := ifsAlt;
      ssMeta:  FCheckBoxes[st].Name := ifsVK_META;
      ssSuper: FCheckBoxes[st].Name := ifsVK_SUPER;
      else     FCheckBoxes[st].Name := s;
    end;
  end;
  FLocked := False;
end;

procedure TCustomShiftStateSelector.UpdateVisible;
var
  st: TShiftStateEnum;
begin
  FLocked := True;
  for st in TShiftStateEnum do begin
    FCheckBoxes[st].Visible := st in FVisibleShift;
    FCheckBoxes[st].SetDsgnVisible(st in FVisibleShift);
  end;
  FLocked := False;
end;

procedure TCustomShiftStateSelector.UpdateCheckState;
var
  st: TShiftStateEnum;
begin
  FLocked := True;
  for st in TShiftStateEnum do begin
    FCheckBoxes[st].AllowGrayed := FHasMask;
    if (FHasMask) and not(st in FShiftMask) then
      FCheckBoxes[st].State := cbGrayed
    else
    if st in FShiftSelection then
      FCheckBoxes[st].State := cbChecked
    else
      FCheckBoxes[st].State := cbUnchecked;
  end;
  FLocked := False;
end;

constructor TCustomShiftStateSelector.Create(TheOwner: TComponent);
var
  st: TShiftStateEnum;
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle
    - [csAcceptsControls]
    + [csOwnedChildrenNotSelectable];
  ShiftMask    := [ssShift, ssCtrl, ssAlt];
  VisibleShift := [ssShift, ssCtrl, ssAlt];

  for st in TShiftStateEnum do begin
    FCheckBoxes[st] := TCheckBox.Create(Self);
    FCheckBoxes[st].Parent   := Self;
    FCheckBoxes[st].OnChange := @DoCheckBoxChanged;
  end;
  UpdateVisible;
  UpdateNames;
  UpdateCheckState;

  ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  ChildSizing.ControlsPerLine := 999;
end;

end.

