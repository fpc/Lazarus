{ $Id$}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Vincent Snijders

  Abstract:
     Shows a non-modal calendar popup for a TDateEdit
}

unit CalendarPopup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  Calendar, LCLType;
  
type
  TReturnDateEvent = procedure (Sender: TObject;const Date: TDateTime) of object;

  { TCalendarPopupForm }
  TCalendarPopupForm = class(TForm)
    Calendar: TCalendar;
    procedure CalendarDblClick(Sender: TObject);
    procedure CalendarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
  private
    { private declarations }
    FClosed: boolean;
    FOnReturnDate: TReturnDateEvent;
    procedure Initialize(const PopupOrigin: TPoint; ADate: TDateTime);
    procedure ReturnDate;
  protected
    procedure Paint;override;
  public
    { public declarations }
  end;

procedure ShowCalendarPopup(const Position: TPoint; ADate: TDateTime;
                            OnReturnDate: TReturnDateEvent);

implementation

procedure ShowCalendarPopup(const Position: TPoint; ADate: TDateTime;
                            OnReturnDate: TReturnDateEvent);
var
  PopupForm: TCalendarPopupForm;
begin
  PopupForm := TCalendarPopupForm.Create(nil);
  PopupForm.Initialize(Position, ADate);
  PopupForm.FOnReturnDate := OnReturnDate;
  //TODO: Change to PopupForm.Show when gtk supports non modal forms on top of
  //modal forms.
  {$IFDEF windows}
  PopupForm.Show;
  {$ELSE}
  PopupForm.ShowModal;
  {$ENDIF}
end;

{ TCalendarPopupForm }

procedure TCalendarPopupForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FClosed := true;
  CloseAction := caFree;
end;

procedure TCalendarPopupForm.CalendarDblClick(Sender: TObject);
begin
  ReturnDate;
end;

procedure TCalendarPopupForm.CalendarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Handled: Boolean;
begin
  if Shift=[] then begin
    Handled := true;
    case Key of
    VK_ESCAPE:
      Close;
    VK_RETURN, VK_SPACE:
      ReturnDate;
    else
      Handled := false;
    end;
    if Handled then
      Key := 0;
  end;
end;

procedure TCalendarPopupForm.FormDeactivate(Sender: TObject);
begin
  if not FClosed then
    Close;
end;

procedure TCalendarPopupForm.Initialize(const PopupOrigin: TPoint;
                                              ADate: TDateTime);
var ScrSize: TPoint;
begin
  ScrSize.X := Screen.Width;
  ScrSize.Y := Screen.Height;
  if PopupOrigin.X + Width > ScrSize.X then
    Left := ScrSize.X - Width
  else
    Left := PopupOrigin.X;
  if PopupOrigin.Y + Height > ScrSize.Y then
    Top := ScrSize.Y - Height
  else
    Top := PopupOrigin.Y;
  Calendar.DateTime := ADate;
end;

procedure TCalendarPopupForm.ReturnDate;
begin
  if assigned(FOnReturnDate) then
    FOnReturnDate(Self, Calendar.DateTime);
  Close;
end;

procedure TCalendarPopupForm.Paint;
begin
  inherited Paint;
  Canvas.Pen.Color:=clWindowText;
  Canvas.Pen.Style := psSolid;
  Canvas.Rectangle(0, 0, Width-1, Height-1)
end;

initialization
  {$I calendarpopup.lrs}

end.
