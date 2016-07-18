{
LCLCalWrapper
- - - - - - - - - - - - - - - - -
Author: Zoran Vučenović
        Зоран Вученовић

   This unit is part of DateTimeCtrls package for Lazarus.

   TLCLCalendarWrapper is the default implementation of TCalendarControlWrapper
abstract class, used by DateTimePicker. Wraps LCL's TCalendar.

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
unit lclcalwrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Calendar, CalControlWrapper, LMessages;

type

  { TLCLCalendarWrapper }

  TLCLCalendarWrapper = class(TCalendarControlWrapper)
  private
    PrevCalendarWndProc: TWndMethod;
    CanClose: Boolean;
    procedure LCLCalendarWrapperWndProc(var TheMessage: TLMessage);
  public
    class function GetCalendarControlClass: TControlClass; override;
    procedure SetDate(Date: TDate); override;
    function GetDate: TDate; override;
    function AreCoordinatesOnDate(X, Y: Integer): Boolean; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TLCLCalendarWrapper }

procedure TLCLCalendarWrapper.LCLCalendarWrapperWndProc(
  var TheMessage: TLMessage);
begin
  if TheMessage.msg = LM_LBUTTONDOWN then
    CanClose := TCalendar(GetCalendarControl).GetCalendarView = cvMonth;

  if Assigned(PrevCalendarWndProc) then
    PrevCalendarWndProc(TheMessage);
end;

class function TLCLCalendarWrapper.GetCalendarControlClass: TControlClass;
begin
  Result := TCalendar;
end;

procedure TLCLCalendarWrapper.SetDate(Date: TDate);
begin
  TCalendar(GetCalendarControl).DateTime := Date;
end;

function TLCLCalendarWrapper.GetDate: TDate;
begin
  Result := TCalendar(GetCalendarControl).DateTime;
end;

function TLCLCalendarWrapper.AreCoordinatesOnDate(X, Y: Integer): Boolean;
begin
  Result :=
    CanClose and
    (TCalendar(GetCalendarControl).GetCalendarView = cvMonth) and
    (TCalendar(GetCalendarControl).HitTest(Point(X, Y)) in [cpDate, cpNoWhere]);

  CanClose := True;
end;

constructor TLCLCalendarWrapper.Create;
begin
  inherited Create;

  CanClose := True;
  PrevCalendarWndProc := GetCalendarControl.WindowProc;
  GetCalendarControl.WindowProc := @LCLCalendarWrapperWndProc;
end;

destructor TLCLCalendarWrapper.Destroy;
begin
  GetCalendarControl.WindowProc := PrevCalendarWndProc;
  inherited Destroy;
end;

end.

