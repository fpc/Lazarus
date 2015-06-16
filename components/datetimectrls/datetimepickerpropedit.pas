{
DateTimePickerPropEdit
- - - - - - - - - - - - - - - - -
Author: Zoran Vučenović, January and February 2010
        Зоран Вученовић, јануар и фебруар 2010.

   This unit is part of DateTimeCtrls package for Lazarus. It contains
component and property editors for TDateTimePicker control.

-----------------------------------------------------------
LICENCE
- - - -
   Modified LGPL -- see the file COPYING.modifiedLGPL.

-----------------------------------------------------------
NO WARRANTY
- - - - - -
   There is no warranty whatsoever.

-----------------------------------------------------------
BEST REGARDS TO LAZARUS COMMUNITY!
- - - - - - - - - - - - - - - - - -
   I do hope the DateTimeCtrls package will be useful.
}
unit DateTimePickerPropEdit;

{$mode objfpc}{$H+}

interface
// Nothing needs to be in interface section!

implementation

uses
  Classes, SysUtils, Forms, Controls, ButtonPanel, DateTimePicker,
  DBDateTimePicker, StdCtrls, Math, Menus, ComponentEditors, PropEdits;

type
    { TFormDateTimePickerEditor }

  TFormDateTimePickerEditor = class(TForm)
  private
    CallerDateTimePicker: TDateTimePicker;
    Prop: String;
    Modified: Boolean;

    ButtonPanel: TButtonPanel;
    DateTimePickerMin: TDateTimePicker;
    DateTimePicker1: TDateTimePicker;
    DateTimePickerMax: TDateTimePicker;
    Label1: TLabel;
    LabelMin: TLabel;
    LabelMax: TLabel;
    LabelNull: TLabel;
    procedure DateTimePickerMaxExit(Sender: TObject);
    procedure DateTimePickerMinExit(Sender: TObject);
    procedure DateTimePickersChange(Sender: TObject);
    procedure DateTimePicker1Enter(Sender: TObject);
    procedure DateTimePicker1Exit(Sender: TObject);
    procedure FormActivate(Sender: TObject);

    procedure Initialize(const Caller: TDateTimePicker;
                                     const PropertyName, PropertyType: String);
    procedure UpdateMinMaxBounds;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
  end;

  { TDateTimePickerComponentEditor }

  TDateTimePickerComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TDateTimePickerDateTimePropEditor }

  TDateTimePickerDateTimePropEditor = class(TDateTimePropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function AllEqual: Boolean; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

  { TSimpleDatePropEditor }

  TSimpleDatePropEditor = class(TDatePropertyEditor)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

procedure RegPropEdits;
begin
  RegisterPropertyEditor(TypeInfo(TTime), TDateTimePicker, 'Time', TDateTimePickerDateTimePropEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TDateTimePicker, 'Date', TDateTimePickerDateTimePropEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TDateTimePicker, 'MaxDate', TDateTimePickerDateTimePropEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TDateTimePicker, 'MinDate', TDateTimePickerDateTimePropEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TDBDateTimePicker, 'MaxDate', TSimpleDatePropEditor);
  RegisterPropertyEditor(TypeInfo(TDate), TDBDateTimePicker, 'MinDate', TSimpleDatePropEditor);
end;

{ TDateTimePickerComponentEditor }

procedure TDateTimePickerComponentEditor.ExecuteVerb(Index: Integer);
var
  F: TFormDateTimePickerEditor;
  DTPicker: TDateTimePicker;
begin
  if Index = 0 then begin
    if GetComponent is TDateTimePicker then begin
      F := TFormDateTimePickerEditor.CreateNew(nil, 0);
      try
        DTPicker := TDateTimePicker(GetComponent);
        if DTPicker.Kind = dtkTime then
          F.Initialize(DTPicker, '', 'TTime')
        else
          F.Initialize(DTPicker, '', '');

        if F.ShowModal = mrOK then begin
          if F.Modified then begin
            DTPicker.MinDate := TheSmallestDate;
            DTPicker.MaxDate := F.DateTimePickerMax.DateTime;
            DTPicker.MinDate := F.DateTimePickerMin.DateTime;

            DTPicker.DateTime := F.DateTimePicker1.DateTime;
            Modified;
            if Assigned(GlobalDesignHook) then
              GlobalDesignHook.RefreshPropertyValues;
          end;
        end;
      finally
        F.Free;
      end;
    end else
      raise Exception.Create('Unknown DateTimePicker object to edit.');
  end;
end;

function TDateTimePickerComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := '&Date/Time Editor...';
end;

function TDateTimePickerComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TFormDateTimePickerEditor }

procedure TFormDateTimePickerEditor.DateTimePickerMaxExit(Sender: TObject);
begin
  if DateTimePickerMin.Date > DateTimePickerMax.Date then
    DateTimePickerMin.Date := DateTimePickerMax.Date;

  UpdateMinMaxBounds;
end;

procedure TFormDateTimePickerEditor.DateTimePickerMinExit(Sender: TObject);
begin
  if DateTimePickerMax.Date < DateTimePickerMin.Date then
    DateTimePickerMax.Date := DateTimePickerMin.Date;

  UpdateMinMaxBounds;
end;

procedure TFormDateTimePickerEditor.DateTimePickersChange(Sender: TObject);
begin
  Modified := True;
end;

procedure TFormDateTimePickerEditor.DateTimePicker1Enter(Sender: TObject);
begin
  if DateTimePicker1.NullInputAllowed then
    LabelNull.Show;
end;

procedure TFormDateTimePickerEditor.DateTimePicker1Exit(Sender: TObject);
begin
  LabelNull.Hide;
end;

procedure TFormDateTimePickerEditor.FormActivate(Sender: TObject);
var
  B: Boolean;
begin
  OnActivate := nil;
  B := False;
  if Prop = 'MAXDATE' then DateTimePickerMax.SetFocus
  else if Prop = 'MINDATE' then DateTimePickerMin.SetFocus
  else begin
    DateTimePicker1.SetFocus;
    B := DateTimePicker1.NullInputAllowed;
  end;
  LabelNull.Visible := B;
  LabelNull.BringToFront;
end;

procedure TFormDateTimePickerEditor.Initialize(const Caller: TDateTimePicker;
                                    const PropertyName, PropertyType: String);
var
  I: Integer;
  DTP: array[1..3] of TDateTimePicker;

  L, T, W, H: Integer;
  R: TRect;
  M: TMonitor;
  AnchKindTrailing, AnchKindLeading: TAnchorKind;
begin
  if Assigned(Caller) then begin
    CallerDateTimePicker := Caller;
    Prop := UpperCase(PropertyName);
    BiDiMode := CallerDateTimePicker.BiDiMode;

    Modified := False;
    DateTimePicker1.Kind := dtkDateTime;
    if UpperCase(PropertyType) = 'TTIME' then
      DateTimePicker1.SelectTime
    else
      DateTimePicker1.SelectDate;

    Label1.Caption := 'Date / Time:';
    LabelMax.Caption := 'MaxDate:';
    LabelMin.Caption := 'MinDate:';
    LabelNull.Caption := '(Press N to set to NULL)';

    DateTimePickerMin.DateTime := CallerDateTimePicker.MinDate;
    DateTimePickerMax.DateTime := CallerDateTimePicker.MaxDate;
    DateTimePicker1.MinDate := CallerDateTimePicker.MinDate;
    DateTimePicker1.MaxDate := CallerDateTimePicker.MaxDate;
    DateTimePicker1.DateTime := CallerDateTimePicker.DateTime;

    DTP[1] := DateTimePickerMin;
    DTP[2] := DateTimePickerMax;
    DTP[3] := DateTimePicker1;
    for I := 1 to 3 do begin
      DTP[I].NullInputAllowed := I = 3;
      DTP[I].CenturyFrom := CallerDateTimePicker.CenturyFrom;
      DTP[I].DateDisplayOrder := CallerDateTimePicker.DateDisplayOrder;
      DTP[I].LeadingZeros := CallerDateTimePicker.LeadingZeros;
      DTP[I].DateSeparator := CallerDateTimePicker.DateSeparator;
      DTP[I].TrailingSeparator := CallerDateTimePicker.TrailingSeparator;
      DTP[I].AutoAdvance := CallerDateTimePicker.AutoAdvance;
      DTP[I].CalendarWrapperClass := CallerDateTimePicker.CalendarWrapperClass;
    end;
    DateTimePicker1.TextForNullDate := CallerDateTimePicker.TextForNullDate;
    DateTimePicker1.TimeSeparator := CallerDateTimePicker.TimeSeparator;
    DateTimePicker1.TimeDisplay := tdHMSMs;
    DateTimePicker1.TimeFormat := CallerDateTimePicker.TimeFormat;

    if IsRightToLeft then begin
      AnchKindLeading := akRight;
      AnchKindTrailing := akLeft;
    end else begin
      AnchKindLeading := akLeft;
      AnchKindTrailing := akRight;
    end;

    DateTimePickerMax.AnchorParallel(akTop, 20, Self);
    DateTimePickerMax.AnchorParallel(AnchKindTrailing, 20, Self);
    LabelMax.AnchorVerticalCenterTo(DateTimePickerMax);
    LabelMax.AnchorToNeighbour(AnchKindTrailing, 2, DateTimePickerMax);
    DateTimePickerMin.AnchorParallel(akTop, 20, Self);
    DateTimePickerMin.AnchorToNeighbour(AnchKindTrailing, 20, LabelMax);
    LabelMin.AnchorToNeighbour(AnchKindTrailing, 2, DateTimePickerMin);
    LabelMin.AnchorVerticalCenterTo(DateTimePickerMin);
    DateTimePicker1.AnchorParallel(AnchKindLeading, 0, DateTimePickerMin);
    DateTimePicker1.AnchorToNeighbour(akTop, 20, DateTimePickerMin);
    Label1.AnchorToNeighbour(AnchKindTrailing, 2, DateTimePicker1);
    Label1.AnchorVerticalCenterTo(DateTimePicker1);
    LabelNull.AnchorToNeighbour(akTop, 2, DateTimePicker1);
    LabelNull.AnchorHorizontalCenterTo(DateTimePicker1);

    ButtonPanel.Spacing := 10;
    ButtonPanel.BorderSpacing.Around := 10;

    W := Max(Label1.Width, LabelMin.Width);

    W := DateTimePickerMax.Width + DateTimePickerMin.Width
                                   + LabelMax.Width + W + 80;

    H := 2 * DateTimePickerMax.Height + LabelNull.Height + ButtonPanel.Height + 58;

    M := Screen.MonitorFromWindow(CallerDateTimePicker.Handle);

    R := M.WorkareaRect;
    // But if WorkareaRect doesn't work (not implemented for all widgetsets),
    // then take BoundsRect:
    if (R.Right <= R.Left) or (R.Bottom <= R.Top) then
      R := M.BoundsRect;

    L := (R.Left + R.Right - W) div 2;
    T := (R.Top + R.Bottom - H) div 2;

    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;

    SetBounds(L, T, W, H);
  end;

end;

procedure TFormDateTimePickerEditor.UpdateMinMaxBounds;
begin
  DateTimePicker1.MinDate := TheSmallestDate;
  DateTimePicker1.MaxDate := DateTimePickerMax.Date;
  DateTimePicker1.MinDate := DateTimePickerMin.Date;
end;

constructor TFormDateTimePickerEditor.CreateNew(AOwner: TComponent;
  Num: Integer);
var
  I: Integer;
begin
  inherited CreateNew(AOwner, Num);

  Hide;
  if Font.Size > 10 then
    Font.Size := 10;

  SetBounds(-8000, -8000, 4, 5);
  BorderStyle := bsDialog;
  BorderIcons := [biSystemMenu];
  Caption := 'DateTimePicker Editor';

  DateTimePickerMax := TDateTimePicker.Create(Self);
  DateTimePickerMin := TDateTimePicker.Create(Self);
  DateTimePicker1 := TDateTimePicker.Create(Self);
  Label1 := TLabel.Create(Self);
  LabelMin := TLabel.Create(Self);
  LabelMax := TLabel.Create(Self);
  LabelNull := TLabel.Create(Self);

  ButtonPanel := TButtonPanel.Create(Self);
  ButtonPanel.ShowButtons := [pbOK, pbCancel];
  ButtonPanel.OKButton.GlyphShowMode := gsmAlways;
  ButtonPanel.CancelButton.GlyphShowMode := gsmAlways;
  ButtonPanel.ShowBevel := False;

  DateTimePickerMax.Parent := Self;
  DateTimePickerMin.Parent := Self;
  DateTimePicker1.Parent := Self;
  Label1.Parent := Self;
  LabelMin.Parent := Self;
  LabelMax.Parent := Self;
  LabelNull.Parent := Self;
  ButtonPanel.Parent := Self;

  ButtonPanel.TabOrder := 0;
  DateTimePickerMin.TabOrder := 1;
  DateTimePickerMax.TabOrder := 2;
  DateTimePicker1.TabOrder := 3;

  for I := 0 to ControlCount - 1 do begin
    Controls[I].Anchors := [];
    Controls[I].AutoSize := True;
  end;

  DateTimePickerMax.OnExit := @DateTimePickerMaxExit;
  DateTimePickerMin.OnExit := @DateTimePickerMinExit;
  DateTimePicker1.OnExit := @DateTimePicker1Exit;
  DateTimePicker1.OnEnter := @DateTimePicker1Enter;
  DateTimePickerMin.OnChange := @DateTimePickersChange;
  DateTimePickerMax.OnChange := @DateTimePickersChange;
  DateTimePicker1.OnChange := @DateTimePickersChange;

  OnActivate := @FormActivate;
end;

destructor TFormDateTimePickerEditor.Destroy;
begin
  OnActivate := nil;
  OnShow := nil;

  DateTimePicker1.OnChange := nil;
  DateTimePickerMax.OnChange := nil;
  DateTimePickerMin.OnChange := nil;
  DateTimePicker1.OnEnter := nil;
  DateTimePicker1.OnExit := nil;
  DateTimePickerMin.OnExit := nil;
  DateTimePickerMax.OnExit := nil;

  ButtonPanel.Free;
  LabelNull.Free;
  LabelMax.Free;
  LabelMin.Free;
  Label1.Free;
  DateTimePicker1.Free;
  DateTimePickerMin.Free;
  DateTimePickerMax.Free;

  inherited Destroy;
end;

{ TDateTimePickerDateTimePropEditor }

function TDateTimePickerDateTimePropEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TDateTimePickerDateTimePropEditor.AllEqual: Boolean;
var
  DT: TDateTime;
  N: Integer;
begin
  Result := True;
  N := PropCount;
  if N > 1 then begin
    DT := TDateTime(GetFloatValue);
    repeat
      Dec(N);
      Result := EqualDateTime(DT, TDateTime(GetFloatValueAt(N)));
    until not(Result and (N > 1));
  end;
end;

function TDateTimePickerDateTimePropEditor.GetValue: string;
var
  DT: TDateTime;
  S: String;
begin
  DT := TDateTime(GetFloatValue);
  if IsNullDate(DT) then
    Result := 'NULL'
  else begin
    S := UpperCase(GetPropType^.Name);
    if S = 'TDATE' then
      Result := DateToStr(DT)
    else if S = 'TTIME' then
      Result := TimeToStr(DT)
    else
      Result := DateTimeToStr(DT);
  end;
end;

procedure TDateTimePickerDateTimePropEditor.SetValue(const Value: string);
var
  S: String;
begin
  S := Trim(Value);
  if (S > '') and (UpCase(S[1]) <> 'N') then begin
    S := UpperCase(GetPropType^.Name);
    if S = 'TDATE' then
      SetFloatValue(StrToDate(Value))
    else if S = 'TTIME' then
      SetFloatValue(StrToTime(Value))
    else
      inherited SetValue(Value);
  end else
    SetFloatValue(NullDate);
end;

procedure TDateTimePickerDateTimePropEditor.Edit;
var
  F: TFormDateTimePickerEditor;
  I: Integer;
  DT: TDateTimePicker;
begin
  for I := 0 to PropCount - 1 do
    if not (GetComponent(I) is TDateTimePicker) then
      Exit;

  F := TFormDateTimePickerEditor.CreateNew(nil, 0);
  try
    F.Initialize(TDateTimePicker(GetComponent(0)), GetName, GetPropType^.Name);
    if F.ShowModal = mrOK then begin
      if F.Modified then begin
        for I := 0 to PropCount - 1 do begin
          DT := TDateTimePicker(GetComponent(I));
          DT.MinDate := TheSmallestDate;
          DT.MaxDate := F.DateTimePickerMax.Date;
          DT.MinDate := F.DateTimePickerMin.Date;

          DT.DateTime := F.DateTimePicker1.DateTime;
        end;

        Modified;
        if Assigned(GlobalDesignHook) then
          GlobalDesignHook.RefreshPropertyValues;
      end;
    end;
  finally
    F.Free;
  end;
end;

{ TSimpleDatePropEditor }

function TSimpleDatePropEditor.GetValue: string;
begin
  Result := DateToStr(GetFloatValue);
end;

procedure TSimpleDatePropEditor.SetValue(const Value: string);
var
  S: String;
begin
  S := Trim(Value);
  if (S > '') and (UpCase(S[1]) <> 'N') then
    inherited SetValue(S);
end;

initialization
  RegPropEdits;
  RegisterComponentEditor(TDateTimePicker, TDateTimePickerComponentEditor);

end.
