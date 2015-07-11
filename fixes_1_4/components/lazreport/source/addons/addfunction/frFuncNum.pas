{*******************************************************}
{                                                       }
{         Add FastReport Numeric Lbrary                 }
{                                                       }
{         Copyright (c) 2001 by Stalker SoftWare        }
{                                                       }
{*******************************************************}

unit frFuncNum;

interface

{$A+,B-,E-,R-}
{.$I FR.inc}

uses
  SysUtils;

 // StLib
 function frIsRangeNum( nBeg, nEnd, nValue: Extended ) :Boolean;
 function frValidInt(cInt :String) :Boolean;
 function frValidFloat(cFlt :String) :Boolean;
 function frStrToFloatDef(cFlt:String; nFltDef :Extended) :Extended;

implementation


{----------------------------------------------------------------}
{ Return True if specified number found in range                 }
{ nBeg   - Begin range                                           }
{ nEnd   - End range                                             }
{ nValue - Checked number                                        }
{----------------------------------------------------------------}
function frIsRangeNum( nBeg, nEnd, nValue: Extended ) :Boolean;
begin

  if (nValue >= nBeg) and (nValue <= nEnd) then
    Result := True
  else
    Result := False

end; { IsRangeNum }

{--------------------------------------------------------------------} 
{ Return True, if cInt really integer                                }  
{--------------------------------------------------------------------}
function frValidInt(cInt :String) :Boolean;
begin

 Result := True;
 try
   StrToInt(cInt);
 except
   Result := False;
 end; { try }

end; { ValidInt }

{--------------------------------------------------------------------}
{ Return True, if cFlt really float                                  }
{--------------------------------------------------------------------}
function frValidFloat(cFlt :String) :Boolean;
begin

 Result := True;
 try
   StrToFloat(cFlt);
 except
   Result := False;
 end; { try }

end; { frValidFloat }

{--------------------------------------------------------------------}
{ Convert string into Float.                                         }
{--------------------------------------------------------------------}
function frStrToFloatDef(cFlt:String; nFltDef :Extended) :Extended;
begin

 try
   Result := StrToFloat(cFlt);
 except
   Result := nFltDef;
 end; { try }

end; { frStrToFloatDef }

end.
