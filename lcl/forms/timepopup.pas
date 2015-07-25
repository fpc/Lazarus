{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael Fuchs

  Abstract:
     Shows a time input popup for a TTimeEdit
}

unit TimePopup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, FileUtil, LCLType, Forms, Controls,
  Graphics, Dialogs, Grids, ExtCtrls, Buttons, StdCtrls, ActnList;

type
  TReturnTimeEvent = procedure (Sender: TObject; const ATime: TDateTime) of object;

  { TTimePopupForm }
  
  TTimePopupForm = class(TForm)
    Bevel1: TBevel;
    MainPanel: TPanel;
    HoursGrid: TStringGrid;
    MinutesGrid: TStringGrid;
    MoreLessBtn: TBitBtn;
    procedure GridsDblClick(Sender: TObject);
    procedure GridsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MoreLessBtnClick(Sender: TObject);
    procedure SetLayout(SimpleLayout: Boolean);
  private
    FClosed: Boolean;
    FOnReturnTime: TReturnTimeEvent;
    FSimpleLayout: Boolean;
    procedure ActivateDoubleBuffered;
    procedure CalcGridHeights;
    function GetTime: TDateTime;
    procedure Initialize(const PopupOrigin: TPoint; ATime: TDateTime);
    procedure ReturnTime;
    procedure SetTime(ATime: TDateTime);
  published
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  end;

procedure ShowTimePopup(const Position: TPoint; ATime: TDateTime; const DoubleBufferedForm: Boolean;
                        const OnReturnTime: TReturnTimeEvent; const OnShowHide: TNotifyEvent = nil);

implementation

{$R *.lfm}

procedure ShowTimePopup(const Position: TPoint; ATime: TDateTime; const DoubleBufferedForm: Boolean; const OnReturnTime: TReturnTimeEvent;
                        const OnShowHide: TNotifyEvent);
var
  NewForm: TTimePopupForm;
begin
  NewForm := TTimePopupForm.Create(nil);
  NewForm.Initialize(Position, ATime);
  NewForm.FOnReturnTime := OnReturnTime;
  NewForm.OnShow := OnShowHide;
  NewForm.OnHide := OnShowHide;
  if DoubleBufferedForm then
    NewForm.ActivateDoubleBuffered;
  NewForm.Show;
end;

procedure TTimePopupForm.SetTime(ATime: TDateTime);
var
  Hour, Minute: Integer;
begin
  Hour := HourOf(ATime);
  Minute := MinuteOf(ATime);
  HoursGrid.Col := Hour mod 12;
  HoursGrid.Row := Hour div 12;
  if FSimpleLayout then
  begin
    Minute := Minute - (Minute mod 5);
    MinutesGrid.Col := (Minute mod 30) div 5;
    MinutesGrid.Row := Minute div 30;
  end
  else
  begin
    MinutesGrid.Col := Minute  mod 5;
    MinutesGrid.Row := Minute div 5;
  end;
end;

procedure TTimePopupForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FClosed := true;
  Application.RemoveOnDeactivateHandler(@FormDeactivate);
  CloseAction := caFree;
end;

procedure TTimePopupForm.FormCreate(Sender: TObject);
begin
  FClosed := False;
  FSimpleLayout := True;
  Application.AddOnDeactivateHandler(@FormDeactivate);
  SetLayout(FSimpleLayout);
end;

procedure TTimePopupForm.FormDeactivate(Sender: TObject);
begin
  if (not FClosed) then
    Close;
end;

procedure TTimePopupForm.GridsDblClick(Sender: TObject);
begin
  ReturnTime;
end;

procedure TTimePopupForm.GridsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Handled: Boolean;
begin
  if Shift=[] then begin
    Handled := True;
    case Key of
      VK_ESCAPE          : Close;
      VK_RETURN, VK_SPACE: ReturnTime;
    else
      Handled := False;
    end;
    if Handled then
      Key := 0;
  end;
end;

procedure TTimePopupForm.MoreLessBtnClick(Sender: TObject);
var
  OldMin: Integer;
begin
  if FSimpleLayout then
  begin
    OldMin := (MinutesGrid.Row * 30) + (MinutesGrid.Col * 5);
    if (OldMin < 0) then OldMin := 0;
    if (OldMin > 59) then OldMin := 59;
    SetLayout(False);

    MinutesGrid.Col := OldMin mod 5;
    MinutesGrid.Row := OldMin div 5;
    MoreLessBtn.Caption := '<<';
  end
  else
  begin
    OldMin := (MinutesGrid.Row * 5) + (MinutesGrid.Col);
    if (OldMin < 0) then OldMin := 0;
    if (OldMin > 59) then OldMin := 59;
    OldMin := OldMin - (OldMin mod 5);
    SetLayout(True);
    MinutesGrid.Col := (OldMin mod 30) div 5;
    MinutesGrid.Row := OldMin div 30;
    MoreLessBtn.Caption := '>>';
  end;
end;

procedure TTimePopupForm.SetLayout(SimpleLayout: Boolean);
var
  r, c: Integer;
begin
  MinutesGrid.BeginUpdate;
  try
  if SimpleLayout then
  begin
    MinutesGrid.RowCount := 2;
    MinutesGrid.ColCount := 6;
    for r := 0 to MinutesGrid.RowCount - 1 do
      for c := 0 to MinutesGrid.ColCount - 1 do
        begin
          //debugln(Format('[%.2d,%.2d]: %.2d',[r,c,(r*30) + (c*5)]));
          MinutesGrid.Cells[c,r] := Format('%s%.2d',[DefaultFormatSettings.TimeSeparator,(r*30) + (c*5)]);
        end;
  end
  else
  begin
    MinutesGrid.RowCount := 12;
    MinutesGrid.ColCount := 5;
    for r := 0 to MinutesGrid.RowCount - 1 do
      for c := 0 to MinutesGrid.ColCount - 1 do
        begin
          //debugln(Format('[%.2d,%.2d]: %.2d',[r,c,(r*5) + (c)]));
          MinutesGrid.Cells[c,r] := Format('%s%.2d',[DefaultFormatSettings.TimeSeparator,(r*5) + (c)]);
        end;
  end;
  CalcGridHeights;
  FSimpleLayout := SimpleLayout;
  finally
    MinutesGrid.EndUpdate(True);
  end;
end;

procedure TTimePopupForm.ActivateDoubleBuffered;
begin
  Self.DoubleBuffered := True;
  HoursGrid.DoubleBuffered := True;
  MinutesGrid.DoubleBuffered := True;
end;

procedure TTimePopupForm.CalcGridHeights;
var
  i, RowHeightsSum: Integer;
begin
  RowHeightsSum := 0;
  for i := 0 to HoursGrid.RowCount - 1 do
    RowHeightsSum := RowHeightsSum + HoursGrid.RowHeights[i] + 1;
  HoursGrid.Constraints.MinHeight := RowHeightsSum;
  RowHeightsSum := 0;
  for i := 0 to MinutesGrid.RowCount - 1 do
    RowHeightsSum := RowHeightsSum + MinutesGrid.RowHeights[i] + 1;
  MinutesGrid.Constraints.MinHeight := RowHeightsSum;
  MinutesGrid.Height := RowHeightsSum;
end;

function TTimePopupForm.GetTime: TDateTime;
var
  Hour, Minute: Integer;
begin
  Hour := (HoursGrid.Row * 12) + (HoursGrid.Col);
  if FSimpleLayout then
    Minute := (MinutesGrid.Row * 30) + (MinutesGrid.Col * 5)
  else
    Minute := (MinutesGrid.Row * 5) + (MinutesGrid.Col);
  Result := EncodeTime(Hour, Minute, 0, 0);
end;

procedure TTimePopupForm.Initialize(const PopupOrigin: TPoint; ATime: TDateTime);
var
  ABounds: TRect;
begin
  ABounds := Screen.MonitorFromPoint(PopupOrigin).BoundsRect;
  if PopupOrigin.X + Width > ABounds.Right then
    Left := ABounds.Right - Width
  else
    Left := PopupOrigin.X;
  if PopupOrigin.Y + Height > ABounds.Bottom then
    Top := ABounds.Bottom - Height
  else
    Top := PopupOrigin.Y;
  SetTime(ATime);
end;

procedure TTimePopupForm.ReturnTime;
begin
  if Assigned(FOnReturnTime) then
    FOnReturnTime(Self, Self.GetTime);
  if not FClosed then
    Close;
end;

end.
