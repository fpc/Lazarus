{*******************************************************}
{                                                       }
{         Add FastReport SQL Support Lbrary             }
{                                                       }
{         Copyright (c) 2001 by Stalker SoftWare        }
{                                                       }
{*******************************************************}

unit frFuncSQL;

interface

uses
  SysUtils;

 function frCreateStr(cStr :String) :String;
 function frCreateNum(cNum :String ) :String;
 function frCreateDate(cDate :String; cFFormatDate: String) :String;


{.$I FR.inc}


 // StLib

implementation

uses
   frFuncStr, frFuncDate;
   
{--------------------------------------------------------------------}
{ Return quoted string cStr creating SQL request                     }
{--------------------------------------------------------------------}
function frCreateStr(cStr :String) :String;
begin
 if Trim(cStr) = '' then
   Result := 'null'
 else
   Result := CHR(39)+cStr+CHR(39);
end; { frCreateStr }

{--------------------------------------------------------------------}
{ Return prepared string with number for creating SQL request.       }
{ Possible comma in string is changed by point.                      }
{--------------------------------------------------------------------}
function frCreateNum(cNum :String) :String;
begin
 if Trim(cNum) = '' then
   Result := 'null'
 else
   Result := frReplaceStr(cNum,DecimalSeparator,'.');
end; { frCreateNum }

{--------------------------------------------------------------------}
{ Return prepared quoted string cDate                                }
{ with date for crqting SQL request.                                 }
{ cFEmptyDate   this empty date, returned by (for example)           }
{               RxDateEdit.Text. Example '  .  .    '                }
{ cFFormatDate  this format date specified by SQL server.            }
{               Example 'yyyy/mm/dd'                                 }
{--------------------------------------------------------------------}
function frCreateDate(cDate :String; cFFormatDate: String) :String;
begin

 if not frValidDate(cDate) then
   Result := 'null'
 else
   Result := CHR(39)+FormatDateTime( cFFormatDate, StrToDateTime(cDate) )+CHR(39);

end; { frCreateDate }

end.
