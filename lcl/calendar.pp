{
 /***************************************************************************
                               Calendar.pp
                             -------------------
                             Component Library Calendar Component
                   Initial Revision  : Wed Dec 05 2001

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{
@abstract(Calendar component)
@author(Shane Miller)
@created(05 Dev 2001)
}
unit Calendar;

{$mode objfpc}{$H+}

{off $Define VerboseCalenderSetDate}

interface

uses
  Types, SysUtils, Classes, Math,
  {$IFDEF VerboseCalenderSetDate}LazLoggerBase,{$ENDIF}
  LCLType, LCLStrConsts, lMessages, Controls, LResources;

type
  TDisplaySetting = (
    dsShowHeadings,
    dsShowDayNames,
    dsNoMonthChange,
    dsShowWeekNumbers
  );
  TDisplaySettings = set of TDisplaySetting;

const
  DefaultDisplaySettings = [dsShowHeadings, dsShowDayNames];

type
  TCalendarPart = (
    cpNoWhere,      // somewhere
    cpDate,         // date part
    cpWeekNumber,   // week number
    cpTitle,        // somewhere in the title
    cpTitleBtn,     // button in the title
    cpTitleMonth,   // month value in the title
    cpTitleYear     // year value in the title
  );

  { In Windows since Vista native calendar control has four possible views.
    In other widgetsets, as well as in older windows, calendar can only have
    standard "month view" - grid with days representing a month. }
  TCalendarView = (
    cvMonth,  // grid with days in one month
    cvYear,   // grid with months in one year
    cvDecade, // grid with years from one decade
    cvCentury // grid with decades of one century
  );

  TCalDayOfWeek = (
    dowMonday, dowTuesday, dowWednesday, dowThursday,
    dowFriday, dowSaturday, dowSunday, dowDefault
  );

  EInvalidDate = class(Exception);

  { TCustomCalendar }

  TCustomCalendar = class(TWinControl)
  private
    FDateAsString : String;
    FDate: TDateTime; // last valid date
    FDisplaySettings : TDisplaySettings;
    FFirstDayOfWeek: TCalDayOfWeek;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
    FOnChange: TNotifyEvent;
    FDayChanged: TNotifyEvent;
    FMonthChanged: TNotifyEvent;
    FYearChanged: TNotifyEvent;
    FPropsChanged: Boolean;
    FLimitsChanged: Boolean;
    procedure CheckRange(ADate, AMinDate, AMaxDate: TDateTime);
    function GetDateTime: TDateTime;
    function GetMaxDateStored: Boolean;
    function GetMinDateStored: Boolean;
    procedure SetDateTime(const AValue: TDateTime);
    procedure GetProps;
    procedure SetMaxDate(AValue: TDateTime);
    procedure SetMinDate(AValue: TDateTime);
    procedure SetProps;
    function GetDisplaySettings: TDisplaySettings;
    procedure SetDisplaySettings(const AValue: TDisplaySettings);
    function GetDate: String;
    procedure SetDate(const AValue: String);
    procedure SetFirstDayOfWeek(const AValue: TCalDayOfWeek);
    function IsLimited: Boolean;
    procedure ApplyLimits;
    procedure RemoveLimits;
  protected
    class procedure WSRegisterClass; override;
    procedure LMChanged(var Message: TLMessage); message LM_CHANGED;
    procedure LMMonthChanged(var Message: TLMessage); message LM_MONTHCHANGED;
    procedure LMYearChanged(var Message: TLMessage); message LM_YEARCHANGED;
    procedure LMDayChanged(var Message: TLMessage); message LM_DAYCHANGED;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Loaded; override;
    procedure InitializeWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    function HitTest(APoint: TPoint): TCalendarPart;
    function GetCalendarView: TCalendarView;
    property Date: String read GetDate write SetDate stored False;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property DisplaySettings: TDisplaySettings read GetDisplaySettings
      write SetDisplaySettings default DefaultDisplaySettings;
    property FirstDayOfWeek: TCalDayOfWeek read FFirstDayOfWeek write SetFirstDayOfWeek default dowDefault;
    property MaxDate: TDateTime read FMaxDate write SetMaxDate stored GetMaxDateStored;
    property MinDate: TDateTime read FMinDate write SetMinDate stored GetMinDateStored;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDayChanged: TNotifyEvent read FDayChanged write FDayChanged;
    property OnMonthChanged: TNotifyEvent read FMonthChanged write FMonthChanged;
    property OnYearChanged: TNotifyEvent read FYearChanged write FYearChanged;
  end;

  { TCalendar }
  
  TCalendar = class(TCustomCalendar)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Constraints;
    property DateTime;
    property DisplaySettings;
    property DoubleBuffered;
    property FirstDayOfWeek;
    property Hint;
    property MinDate;
    property MaxDate;
    property ParentDoubleBuffered;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDayChanged;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMonthChanged;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnResize;
    property OnUTF8KeyPress;
    property OnYearChanged;
  end;

procedure Register;
  
implementation

uses
  WSCalendar;

const
  DTEpsilon = Double(1.0)/(24*3600*1000*10); //0.1 millisec (0.000000001157407407);

procedure Register;
begin
  RegisterComponents('Misc',[TCalendar]);
end;

{ TCustomCalendar }

constructor TCustomCalendar.Create(AOwner: TComponent);
var
  WSMinDate, WSMaxDate: TDateTime;
begin
  inherited Create(AOwner);
  fCompStyle := csCalendar;
  SetInitialBounds(0, 0, GetControlClassDefaultSize.CX, GetControlClassDefaultSize.CY);
  FDisplaySettings := DefaultDisplaySettings;
  FFirstDayOfWeek := dowDefault;
  ControlStyle:=ControlStyle-[csTripleClicks,csQuadClicks,csAcceptsControls,csCaptureMouse];
  FMinDate := 0.0;
  FMaxDate := 0.0;
  FLimitsChanged := False;
  DateTime := Now;
end;

function TCustomCalendar.HitTest(APoint: TPoint): TCalendarPart;
begin
  if HandleAllocated then
    Result := TWSCustomCalendarClass(WidgetSetClass).HitTest(Self, APoint)
  else
    Result := cpNoWhere;
end;

function TCustomCalendar.GetCalendarView: TCalendarView;
begin
  if HandleAllocated then
    Result := TWSCustomCalendarClass(WidgetSetClass).GetCurrentView(Self)
  else
    Result := cvMonth;
end;

procedure TCustomCalendar.Loaded;
begin
  inherited Loaded;
  if FPropsChanged then SetProps;
end;

procedure TCustomCalendar.InitializeWnd;
begin
  inherited InitializeWnd;
  //if FPropsChanged then   // removed to fix issue #0032379
  SetProps;
end;

procedure TCustomCalendar.DestroyWnd;
begin
  // fetch widgetset values in local variables
  GetProps;
  inherited DestroyWnd;
end;

function TCustomCalendar.GetDate: String;
begin
  Result := '';
  GetProps;
  Result := FDateAsString;
end;

procedure TCustomCalendar.SetDate(const AValue: String);
var
  NewDate: TDateTime;
begin
  if FDateAsString = AValue then Exit;

  NewDate:=StrToDate(AValue);  //test to see if date valid ....
  // no exception => set valid date
  FDateAsString := AValue;
  FDate := NewDate;
  SetProps;
end;

class procedure TCustomCalendar.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomCalendar;
end;

function TCustomCalendar.GetDisplaySettings: TDisplaySettings;
begin
  Result := FDisplaySettings;
end;

procedure TCustomCalendar.SetDisplaySettings(const AValue: TDisplaySettings);
begin
  if FDisplaySettings = AValue then exit;
  FDisplaySettings := AValue;
  SetProps;
end;

procedure TCustomCalendar.CheckRange(ADate, AMinDate, AMaxDate: TDateTime);
begin
  //otherwise you get a message like "Invalid Date: 31-12-9999. Must be between 1-1-0001 and 31-12-9999"
  if (ADate < SysUtils.MinDateTime) then
    raise EInvalidDate.CreateFmt(rsDateTooSmall, [DateToStr(SysUtils.MinDateTime)]);
  if (ADate > SysUtils.MaxDateTime) then
    raise EInvalidDate.CreateFmt(rsDateTooLarge, [DateToStr(SysUtils.MaxDateTime)]);

  if (ADate < AMinDate) or (ADate > AMaxDate) then
  raise EInvalidDate.CreateFmt(rsInvalidDateRangeHint, [DateToStr(ADate),
      DateToStr(AMinDate), DateToStr(AMaxDate)]);
end;

function TCustomCalendar.GetDateTime: TDateTime;
begin
  GetProps;
  Result:=FDate;
end;

function TCustomCalendar.GetMaxDateStored: Boolean;
begin
  Result := not SameValue(FMaxDate, Double(0.0), DTEpsilon);
end;

function TCustomCalendar.GetMinDateStored: Boolean;
begin
  Result := not SameValue(FMinDate, Double(0.0), DTEpsilon);
end;

procedure TCustomCalendar.SetDateTime(const AValue: TDateTime);
{$IFDEF WINDOWS}
var
  CalendarMinDate,CalendarMaxDate: integer;
{$ENDIF}
begin
  if AValue=FDate then exit;
  if IsLimited then
    CheckRange(AValue, FMinDate, FMaxDate)
  else
    CheckRange(AValue, SysUtils.MinDateTime, SysUtils.MaxDateTime);
  FDate:=AValue;
  FDateAsString:=FormatDateTime(DefaultFormatSettings.ShortDateFormat,FDate);
  {$IFDEF VerboseCalenderSetDate}
  DebugLn('TCustomCalendar.SetDateTime FDate=',DateToStr(FDate),' FDateAsString=',FDateAsString,' ShortDateFormat=',ShortDateFormat);
  {$ENDIF}
  SetProps;
end;

procedure TCustomCalendar.SetFirstDayOfWeek(const AValue: TCalDayOfWeek);
begin
  if AValue = FFirstDayOfWeek then exit;
  FFirstDayOfWeek := AValue;
  SetProps;
end;

function TCustomCalendar.IsLimited: Boolean;
begin
  Result := (Trunc(FMaxDate)-Trunc(FMinDate) > 0);
end;

procedure TCustomCalendar.ApplyLimits;
begin
  if (GetDateTime > FMaxDate) then
    SetDateTime(FMaxDate)
  else if (GetDateTime < FMinDate) then
    SetDateTime(FMinDate);
  FLimitsChanged := True;
  SetProps;
end;

procedure TCustomCalendar.RemoveLimits;
begin
  FLimitsChanged := True;
  SetProps;
end;

procedure TCustomCalendar.GetProps;
begin
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    FDate := TWSCustomCalendarClass(WidgetSetClass).GetDateTime(Self);
    FDateAsString := FormatDateTime(DefaultFormatSettings.ShortDateFormat,FDate);
    {$IFDEF VerboseCalenderSetDate}
    DebugLn('TCustomCalendar.GetProps A ',DateToStr(FDate),' ',FDateAsString);
    {$ENDIF}
  end;
end;

procedure TCustomCalendar.SetMaxDate(AValue: TDateTime);
var
  OldIsLimited: Boolean;
begin
  if (FMaxDate = AValue) then Exit;
  CheckRange(AValue, SysUtils.MinDateTime, SysUtils.MaxDateTime);
  OldIsLimited := IsLimited;
  FMaxDate := AValue;
  if IsLimited then
  begin
    ApplyLimits;
  end
  else
  begin
    if OldIsLimited then
      RemoveLimits;
  end;
end;

procedure TCustomCalendar.SetMinDate(AValue: TDateTime);
var
  OldIsLimited: Boolean;
begin
  if (FMinDate = AValue) then Exit;
  CheckRange(AValue, SysUtils.MinDateTime, SysUtils.MaxDateTime);
  OldIsLimited := IsLimited;
  FMinDate := AValue;
  if IsLimited then
    ApplyLimits
  else
  begin
    if OldIsLimited then
      RemoveLimits;
  end;
end;

procedure TCustomCalendar.SetProps;
begin
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    FPropsChanged := False;
    {$IFDEF VerboseCalenderSetDate}
    DebugLn('TCustomCalendar.SetProps A ',DateToStr(FDate),' ',FDateAsString);
    {$ENDIF}
    TWSCustomCalendarClass(WidgetSetClass).SetDateTime(Self, FDate);
    TWSCustomCalendarClass(WidgetSetClass).SetDisplaySettings(Self, FDisplaySettings);
    TWSCustomCalendarClass(WidgetSetClass).SetFirstDayOfWeek(Self, FFirstDayOfWeek);
    if FLimitsChanged then //avoid settting limits on each SetProps
    begin
      if IsLimited then
        TWSCustomCalendarClass(WidgetSetClass).SetMinMaxDate(Self,FMinDate, FMaxDate)
      else
        TWSCustomCalendarClass(WidgetSetClass).RemoveMinMaxDates(Self);
      FLimitsChanged := False;
    end;
  end
  else
    FPropsChanged := True;
end;

procedure TCustomCalendar.LMChanged(var Message: TLMessage);
var
  NewDate: TDateTime;
  OldDay, OldMonth, OldYear: word;
  NewDay, NewMonth, NewYear: word;
begin
  NewDate := TWSCustomCalendarClass(WidgetSetClass).GetDateTime(Self);
  DecodeDate(NewDate, NewYear, NewMonth, NewDay);
  DecodeDate(FDate, OldYear, OldMonth, OldDay);
  FDate:= NewDate;
  if (OldYear<>NewYear) and Assigned(OnYearChanged) then OnYearChanged(self);
  if (OldMonth<>NewMonth) and Assigned(OnMonthChanged) then OnMonthChanged(self);
  if (OldDay<>NewDay) and Assigned(OnDayChanged) then OnDayChanged(self);
  if Assigned(OnChange) then OnChange(self);
end;

procedure TCustomCalendar.LMDAYChanged(var Message: TLMessage);
begin
  if Assigned(OnDayChanged) then OnDayChanged(self);
  if Assigned(OnChange) then OnChange(self);
end;

class function TCustomCalendar.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 220;
  Result.CY := 190;
end;

procedure TCustomCalendar.LMMonthChanged(var Message: TLMessage);
begin
  if Assigned(OnMonthChanged) then OnMonthChanged(self);
  if Assigned(OnChange) then OnChange(self);
end;

procedure TCustomCalendar.LMYEARChanged(var Message: TLMessage);
begin
  if Assigned(OnYearChanged) then OnYearChanged(self);
  if Assigned(OnChange) then OnChange(self);
end;

end.
