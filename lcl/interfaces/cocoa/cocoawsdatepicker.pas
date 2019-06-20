unit CocoaWSDatePicker;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  CocoaAll,
  Classes, SysUtils, Controls, Calendar,
  WSCalendar, CocoaWSCommon, CocoaDatePicker,
  LCLtype, LclProc, LMessages, LCLMessageGlue,
  CocoaUtils, CocoaPrivate;

const
  singleDateMode                          = 0;
  rangeDateMode                           = 1;
  NSDatePickerStyle_Stepper               = 0;
  NSDatePickerStyle_ClockCal              = 1;
  NSDatePickerStyle_Edit                  = 2;
  NSHourMinuteDatePickerElementFlag       = $000c;
  NSHourMinuteSecondDatePickerElementFlag = $000e;
  NSTimeZoneDatePickerElementFlag         = $0010;
  NSYearMonthDatePickerElementFlag        = $00c0;
  NSYearMonthDayDatePickerElementFlag     = $00e0;
  NSEraDatePickerElementFlag              = $0100;

type
  TCocoaWSCustomCalendar = class(TWSCustomCalendar)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); override;
    class function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; override;
  end;

implementation

function AnsiStrToNSStr(value : AnsiString): NSString;
begin
  Result:= NSStringUtf8(String(value));
end;

function NSStrToAnsiStr(value: NSString): AnsiString;
begin
  Result:= AnsiString(NSStringToString(value));
end;

function AllocDatePicker(const ATarget: TWinControl; const AParams: TCreateParams): TCocoaDatePicker;
var
  ns : NSString;
  nsc: NSString;
  c  : NSCalendar;
  flags : NSDatePickerElementFlags;
  mode  : NSDatePickerMode;
begin
  Result:= TCocoaDatePicker.alloc.lclInitWithCreateParams(AParams);

  if Assigned(Result) then
  begin
    flags:= NSYearMonthDayDatePickerElementFlag;
    Result.setDatePickerElements(flags);

    Result.setTimeZone(NSTimeZone.localTimeZone);

    Result.setDateValue(DateTimeToNSDate(Now));

    Result.setDatePickerStyle(NSDatePickerStyle_Stepper);

    mode:= singleDateMode;
    Result.setDatePickerMode(mode);

    c := NSCalendar.alloc.initWithCalendarIdentifier(NSString.string_);
    Result.setCalendar(c);

    TCocoaDatePicker(Result).callback:= TLCLCommonCallback.Create(Result, ATarget);

    Result.setBezeled(True);

    //Result.setBordered(True);
  end;
end;

class function TCocoaWSCustomCalendar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  dp: TCocoaDatePicker;
  Params: TCreateParams;
begin
  dp:= AllocDatePicker(AWinControl, AParams);
  dp.autoResize := true;
  dp.retainAspectRatio := true;

  if Assigned(dp) then
  begin
    NSDatePickerCell(TLCLIntfHandle(dp)).setDatePickerStyle(NSDatePickerStyle_ClockCal);
  end;

  Result:= TLCLIntfHandle(dp);
end;

class function  TCocoaWSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
begin
  Result:= NSDateToDateTime(NSDatePickerCell(ACalendar.Handle).dateValue);
end;

class procedure TCocoaWSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
begin
  NSDatePickerCell(ACalendar.Handle).setDateValue(DateTimeToNSDate(ADateTime));
end;

class function TCocoaWSCustomCalendar.HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart;
begin
  // need to validate this decision...
  //Debugln('TCocoaWSCustomCalendar.HitTest Mouse Y : ' + IntToStr(APoint.y));
  if APoint.y >= 40 then
    Result:= cpDate
  else
    Result:= cpTitle;
end;

end.

