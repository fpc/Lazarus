{ $Id$}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Vincent Snijders

  Abstract:
     Shows a non-modal calendar popup for a TDateEdit
}

unit CalendarPopup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Calendar, LCLProc, LCLType;
  
type
  TReturnDateEvent = procedure (Sender: TObject; const Date: TDateTime) of object;

  { TCalendarPopupForm }

  TCalendarPopupForm = class(TForm)
    Calendar: TCalendar;
    procedure CalendarDblClick(Sender: TObject);
    procedure CalendarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    FCaller: TControl;
    FClosed: boolean;
    FOnReturnDate: TReturnDateEvent;
    procedure Initialize(ADate: TDateTime;
      const DisplaySettings: TDisplaySettings);
    procedure KeepInView(const PopupOrigin: TPoint);
    procedure ReturnDate;
  protected
    procedure Paint; override;
  end;

procedure ShowCalendarPopup(const APosition: TPoint; ADate: TDateTime;
    const CalendarDisplaySettings: TDisplaySettings;
    const OnReturnDate: TReturnDateEvent; const OnShowHide: TNotifyEvent = nil;
    ACaller: TControl = nil);

implementation

{$R *.lfm}

procedure ShowCalendarPopup(const APosition: TPoint; ADate: TDateTime;
  const CalendarDisplaySettings: TDisplaySettings;
  const OnReturnDate: TReturnDateEvent; const OnShowHide: TNotifyEvent;
  ACaller: TControl);
var
  PopupForm: TCalendarPopupForm;
begin
  PopupForm := TCalendarPopupForm.Create(nil);
  PopupForm.FCaller := ACaller;
  PopupForm.Initialize(ADate, CalendarDisplaySettings);
  PopupForm.FOnReturnDate := OnReturnDate;
  PopupForm.OnShow := OnShowHide;
  PopupForm.OnHide := OnShowHide;
  PopupForm.Show;
  PopupForm.KeepInView(APosition);   // must be after Show for PopupForm.AutoSize to be in effect.
end;

{ TCalendarPopupForm }

procedure TCalendarPopupForm.FormCreate(Sender: TObject);
begin
  FClosed := false;
  Application.AddOnDeactivateHandler(@FormDeactivate);
end;

procedure TCalendarPopupForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //DebugLn(['TCalendarPopupForm.FormClose ']);
  FClosed := true;
  Application.RemoveOnDeactivateHandler(@FormDeactivate);
  CloseAction := caFree;
end;

procedure TCalendarPopupForm.CalendarDblClick(Sender: TObject);
var
  P: TPoint;
  htRes: TCalendarPart;
begin
  P := Calendar.ScreenToClient(Mouse.CursorPos);
  htRes := Calendar.HitTest(P);
  if {(htRes = cpNoWhere) or }((htRes = cpDate) and (Calendar.GetCalendarView = cvMonth)) then
    ReturnDate;
end;

procedure TCalendarPopupForm.CalendarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Handled: Boolean;
begin
  if Shift=[] then
  begin
    Handled := true;
    case Key of
    VK_ESCAPE:
      Close;
    VK_RETURN, VK_SPACE:
      if (Calendar.GetCalendarView = cvMonth) then
        ReturnDate
      else
        Handled := False;
    else
      Handled := false;
    end;
    if Handled then
      Key := 0;
  end;
end;

procedure TCalendarPopupForm.FormDeactivate(Sender: TObject);
begin
  //DebugLn(['TCalendarPopupForm.FormDeactivate ',DbgSName(GetCaptureControl)]);
  //Immediately hide the form, otherwise it stays visible while e.g. user is draging
  //another form (Issue #0020647)
  Hide;
  if (not FClosed) then
    Close;
end;

procedure TCalendarPopupForm.Initialize(ADate: TDateTime;
  const DisplaySettings: TDisplaySettings);
begin
  Calendar.DateTime := ADate;
  Calendar.DisplaySettings:=DisplaySettings;
end;

procedure TCalendarPopupForm.KeepInView(const PopupOrigin: TPoint);
var
  ABounds: TRect;
  P: TPoint;
begin
  ABounds := Screen.MonitorFromPoint(PopupOrigin).WorkAreaRect; // take care of taskbar
  if PopupOrigin.X + Width > ABounds.Right then
    Left := ABounds.Right - Width
  else if PopupOrigin.X < ABounds.Left then
    Left := ABounds.Left
  else
    Left := PopupOrigin.X;
  if PopupOrigin.Y + Height > ABounds.Bottom then
  begin
    if Assigned(FCaller) then
      Top := PopupOrigin.Y - FCaller.Height - Height
    else
      Top := ABounds.Bottom - Height;
  end else if PopupOrigin.Y < ABounds.Top then
    Top := ABounds.Top
  else
    Top := PopupOrigin.Y;
  if Left < ABounds.Left then Left := 0;
  if Top < ABounds.Top then Top := 0;
end;

procedure TCalendarPopupForm.ReturnDate;
begin
  if Assigned(FOnReturnDate) then
    FOnReturnDate(Self, Calendar.DateTime);
  if not FClosed then
    Close;
end;

procedure TCalendarPopupForm.Paint;
begin
  inherited Paint;
  Canvas.Pen.Color := clWindowText;
  Canvas.Pen.Style := psSolid;
  Canvas.Rectangle(0, 0, Width, Height);
end;

end.
