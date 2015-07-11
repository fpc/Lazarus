{*******************************************************}
{                                                       }
{         Add FastReport Date Lbrary                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{         Copyright (c) 2001 by Stalker SoftWare        }
{                                                       }
{*******************************************************}
unit frFuncDate;

interface

{$B-,V-,R-,Q-,H+}
{.$I FR.inc}

uses
  SysUtils;

 // RxLib
 function frDaysPerMonth(nYear, nMonth: Integer): Integer;
 function frFirstDayOfNextMonth(dDate:TDateTime): TDateTime;
 function frFirstDayOfPrevMonth(dDate:TDateTime): TDateTime;
 function frLastDayOfPrevMonth(dDate:TDateTime): TDateTime;
 function frIncYear(dDate: TDateTime; nDelta: Integer): TDateTime;
 function frIncDay(dDate: TDateTime; nDelta: Integer): TDateTime;

 function frIncDate(dDate: TDateTime; nDays, nMonths, nYears: Integer): TDateTime;

 function frIncDateEx(dDate: TDateTime; cDelta :String) :TDateTime;
 function frIncTimeEx(dTime: TDateTime; cDelta :String): TDateTime;
 procedure frDateDiffEx(dDate1, dDate2: TDateTime; var cDelta :String);

 // StLib
 function frIsRangeDate(dBegDate, dEndDate, dDate: TDateTime) :Boolean;
 function frStrToDateDef(cDate: String; dDefault: TDateTime): TDateTime;
 function frValidDate(cDate :String) :Boolean;
 function frIsLeapYear(AYear: Integer): Boolean;
 function frIncMonth(dDate: TDateTime; nDelta: Integer): TDateTime;

implementation

uses
  frFuncStr;

function frIsLeapYear(AYear: Integer): Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

{--------------------------------------------------------------------}
{ Function returns days per month for specified year.                }
{ nYear - year, nMonth - month                                       }
{--------------------------------------------------------------------}
function frDaysPerMonth(nYear, nMonth: Integer): Integer;
const
  DaysInMonth: array[1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

begin

 Result := DaysInMonth[nMonth];
 if (nMonth = 2) and frIsLeapYear(nYear) then Inc(Result); // leap-year Feb is special

end; { frDaysPerMonth }

{--------------------------------------------------------------------}
{ Function return first day of the next month from date dDate        }
{ in TDateTime format.                                               }
{--------------------------------------------------------------------}
function frFirstDayOfNextMonth(dDate:TDateTime): TDateTime;
var
  Year, Month, Day: Word;

begin

 DecodeDate(dDate, Year, Month, Day);
 Day := 1;
 if Month < 12 then
   Inc(Month)
 else begin
   Inc(Year);
   Month := 1;
 end; { if }
 Result := EncodeDate(Year, Month, Day);

end; { frFirstDayOfNextMonth }

{--------------------------------------------------------------------}
{ Function return first day of the previous month from date dDate    }
{ in TDateTime format.                                               }
{--------------------------------------------------------------------}
function frFirstDayOfPrevMonth(dDate:TDateTime): TDateTime;
var
  Year, Month, Day: Word;

begin

  DecodeDate(dDate, Year, Month, Day);
  Day := 1;
  if Month > 1 then
    Dec(Month)
  else begin
    Dec(Year);
    Month := 12;
  end;
  Result := EncodeDate(Year, Month, Day);

end; { frFirstDayOfPrevMonth }

{--------------------------------------------------------------------}
{ Function return last day of the previous month from date dDate     }
{ in TDateTime format.                                               }
{--------------------------------------------------------------------}
function frLastDayOfPrevMonth(dDate:TDateTime): TDateTime;
var
  D: TDateTime;
  Year, Month, Day: Word;

begin

  D := frFirstDayOfPrevMonth(dDate);
  DecodeDate(D, Year, Month, Day);
  Day := frDaysPerMonth(Year, Month);
  Result := EncodeDate(Year, Month, Day);

end; { frLastDayOfPrevMonth }

{--------------------------------------------------------------------}
{ Increase date dDate on specified count days, months, years,        }
{ returns result date.                                               }
{--------------------------------------------------------------------}
function frIncDate(dDate: TDateTime; nDays, nMonths, nYears: Integer): TDateTime;
var
  D, M, Y: Word;
  Day, Month, Year: LongInt;

begin

 DecodeDate(dDate, Y, M, D);
 Year := Y; Month := M; Day := D;
 Inc(Year, nYears);
 Inc(Year, nMonths div 12);
 Inc(Month, nMonths mod 12);

 if Month < 1 then begin
   Inc(Month, 12);
   Dec(Year);
 end
 else
 if Month > 12 then begin
   Dec(Month, 12);
   Inc(Year);
 end; { if }

 if Day > frDaysPerMonth(Year, Month) then Day := frDaysPerMonth(Year, Month);
 Result := EncodeDate(Year, Month, Day) + nDays + Frac(dDate);

end; { frIncDate }

{--------------------------------------------------------------------}
{ Increase date dDate on specified count days nDelta                 }
{--------------------------------------------------------------------}
function frIncDay(dDate: TDateTime; nDelta: Integer): TDateTime;
begin
 Result := dDate + nDelta;
end; { frIncDay }

{--------------------------------------------------------------------}
{ Increase date dDate on specified count years nDelta                }
{--------------------------------------------------------------------}
function frIncYear(dDate: TDateTime; nDelta: Integer): TDateTime;
begin
 Result := frIncDate(dDate, 0, 0, nDelta);
end;  { frIncYear }

{--------------------------------------------------------------------}
{ Increase date dDate on specified count months nDelta               }
{--------------------------------------------------------------------}
function frIncMonth(dDate: TDateTime; nDelta: Integer): TDateTime;
begin
 Result := frIncDate(dDate, 0, nDelta, 0);
end;  { frIncYear }

{--------------------------------------------------------------------}
{ Detect difference before dates Date1 and Date2 in days,            }
{ months, years.                                                     }
{--------------------------------------------------------------------}
procedure frDateDiffEx(dDate1, dDate2: TDateTime; var cDelta :String);
{ Corrected by Anatoly A. Sanko (2:450/73) }
var
  DtSwap: TDateTime;
  Day1, Day2, Month1, Month2, Year1, Year2: Word;
  Days, Months, Years: Word;

begin

 if dDate1 > dDate2 then begin
   DtSwap := dDate1;
   dDate1 := dDate2;
   dDate2 := DtSwap;
 end;
 DecodeDate(dDate1, Year1, Month1, Day1);
 DecodeDate(dDate2, Year2, Month2, Day2);
 Years := Year2 - Year1;
 Months := 0;
 Days := 0;
 if Month2 < Month1 then begin
   Inc(Months, 12);
   Dec(Years);
 end;
 Inc(Months, Month2 - Month1);
 if Day2 < Day1 then begin
   Inc(Days, frDaysPerMonth(Year1, Month1));
   if Months = 0 then begin
     Dec(Years);
     Months := 11;
   end
   else Dec(Months);
 end;
 Inc(Days, Day2 - Day1);

 // Compile string for result
 cDelta := IntToStr(Days)+';'+IntToStr(Months)+';'+IntToStr(Years);

end; { frDateDiffEx }

{----------------------------------------------------------------}
{ Return True if specified date be in given range                 }
{ vBegDate  - Begin range                                        }
{ vEndDate  - End range                                          }
{ vDate     - Checked date                                       }
{----------------------------------------------------------------}
function frIsRangeDate(dBegDate, dEndDate, dDate: TDateTime) :Boolean;
begin

 if (dDate >= dBegDate) and (dDate <= dEndDate) then
   Result := True
 else
   Result := False

end; { frIsDiapDate }

{----------------------------------------------------------------}
{ Convert string into date.                                      }
{----------------------------------------------------------------}
function frStrToDateDef(cDate: String; dDefault: TDateTime): TDateTime;
begin

 try
   Result := StrToDate(cDate)
 except
   Result := dDefault;
 end; { try }

end; { frStrToDateDef }

{--------------------------------------------------------------------}
{ Return True, if cDate really date                                  }
{--------------------------------------------------------------------}
function frValidDate(cDate :String) :Boolean;
begin

 Result := True;
 try
   StrToDate(cDate)
 except
   Result := False;
 end; { try }

end; { frValidDate }

{--------------------------------------------------------------------}
{ Increase date dDate on specified count days, months, years,        }
{ extracting them from string cDelta and return this date            }
{ as result.                                                         }
{--------------------------------------------------------------------}
function frIncDateEx(dDate: TDateTime; cDelta :String) :TDateTime;
var
  nDay, nMonth, nYear: LongInt;

begin

 // Split string on parts
 nDay := StrToInt(frExtractWord(1,cDelta,[';']));
 nMonth := StrToInt(frExtractWord(2,cDelta,[';']));
 nYear := StrToInt(frExtractWord(3,cDelta,[';']));

 Result := frIncDate(dDate, nDay, nMonth, nYear);

end; { frIncDateEx }

{--------------------------------------------------------------------}
{ Increase time ATime on specified count hours, minuts, seconds,     }
{ and milliseconds, extracting his from string cDelta                }
{--------------------------------------------------------------------}
function frIncTimeEx(dTime: TDateTime; cDelta :String): TDateTime;
var
  nHours, nMinutes, nSeconds, nMSecs: Integer;

begin

 // Split string on parts
 nHours := StrToInt(frExtractWord(1,cDelta,[';']));
 nMinutes := StrToInt(frExtractWord(2,cDelta,[';']));
 nSeconds := StrToInt(frExtractWord(3,cDelta,[';']));
 nMSecs := StrToInt(frExtractWord(4,cDelta,[';']));


 Result := dTime + (nHours div 24) + (((nHours mod 24) * 3600000 +
   nMinutes * 60000 + nSeconds * 1000 + nMSecs) / MSecsPerDay);

 if Result < 0 then Result := Result + 1;

end; { frIncTimeEx }


end.
