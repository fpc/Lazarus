unit lrAddFunctionLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LR_Class;

type
  TlrAddFunctionLibrary = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

type
  { TAddFunctionLibrary }
  TAddFunctionLibrary = class(TfrFunctionLibrary)
  private
    procedure DoScriptF(cScript: String);
    procedure DoInitStr;
    procedure DoInitNum;
    procedure DoInitDate;
    procedure DoInitSQL;
    procedure DoInitMath;
    procedure DoInitOther;
  public
    constructor Create; override;
    procedure DoFunction(FNo: Integer; p1, p2, p3: Variant; var val: Variant);override;
  end;
  
procedure Register;

var
  cFFormatDate :String;
  
implementation
uses lr_add_function_const, StrUtils,
  frFuncDate, frFuncNum, frFuncSQL, frFuncStr;

procedure Register;
begin
  RegisterComponents('LazReport',[TlrAddFunctionLibrary]);
end;

{ TAddFunctionLibrary }

{--------------------------------------------------------------------}
{ Convert from typeString into type TfrCharSet                       }
{--------------------------------------------------------------------}
function ConvCS(cStr :String) :TfrCharSet;
var
  i :Integer;
begin
 Result := [];
 for i := 1 to Length(cStr) do Include(Result, cStr[i]);
end; { ConvCS }

procedure TAddFunctionLibrary.DoScriptF(cScript: String);
var
  sl, sl1, sl2: TStringList;
begin
  if cScript <> '' then
  begin
    sl := TStringList.Create;
    sl1 := TStringList.Create;
    sl2 := TStringList.Create;
    sl.Add(cScript);
    frInterpretator.PrepareScript(sl, sl1, sl2);
    frInterpretator.DoScript(sl1);
    sl.Free;
    sl1.Free;
    sl2.Free;
  end; { if }
end; { DoScriptF }

procedure TAddFunctionLibrary.DoInitStr;
var
  rsString :String;

begin

 rsString := SStringCategory;

 // RxLib
 AddFunctionDesc('ISWORDPRESENT', rsString, SDescriptionISWORDPRESENT);
 AddFunctionDesc('WORDPOSITION', rsString, SDescriptionWORDPOSITION);
 AddFunctionDesc('EXTRACTWORD', rsString, SDescriptionEXTRACTWORD);
 AddFunctionDesc('WORDCOUNT', rsString, SDescriptionWORDCOUNT);
 AddFunctionDesc('NPOS', rsString, SDescriptionNPOS);
 AddFunctionDesc('REPLACESTR', rsString, SDescriptionREPLACESTR);

 // Delphi
 AddFunctionDesc('TRIMRIGHT', rsString, SDescriptionTRIMRIGHT);
 AddFunctionDesc('TRIMLEFT', rsString, SDescriptionTRIMLEFT);
 AddFunctionDesc('DELETE', rsString, SDescriptionDELETE);
 AddFunctionDesc('INSERT', rsString, SDescriptionINSERT);
 AddFunctionDesc('DATETOSTR', rsString, SDescriptionDATETOSTR);
 AddFunctionDesc('TIMETOSTR', rsString, SDescriptionTIMETOSTR);
 AddFunctionDesc('CHR', rsString, SDescriptionCHR);

 // StLib
 AddFunctionDesc('REPLICATE', rsString, SDescriptionREPLICATE);
 AddFunctionDesc('PADLEFT', rsString, SDescriptionPADLEFT);
 AddFunctionDesc('PADRIGHT', rsString, SDescriptionPADRIGHT);
 AddFunctionDesc('PADCENTER', rsString, SDescriptionPADCENTER);
 AddFunctionDesc('ENDPOS', rsString, SDescriptionENDPOS);
 AddFunctionDesc('LEFTCOPY', rsString, SDescriptionLEFTCOPY);
 AddFunctionDesc('RIGHTCOPY', rsString, SDescriptionRIGHTCOPY);
 AddFunctionDesc('COMPARESTR', rsString, SDescriptionCOMPARESTR);

end; { DoInitStr }

{ DoInitNum }

procedure TAddFunctionLibrary.DoInitNum;
var
  rsNum :String;

begin

 rsNum := SMathCategory;

 AddFunctionDesc('VALIDINT', rsNum, SDescriptionVALIDINT);
 AddFunctionDesc('VALIDFLOAT', rsNum, SDescriptionVALIDFLOAT);
 AddFunctionDesc('ISRANGENUM', rsNum, SDescriptionISRANGENUM);
 AddFunctionDesc('STRTOFLOATDEF', rsNum, SDescriptionSTRTOFLOATDEF);
 AddFunctionDesc('STRTOINTDEF', rsNum, SDescriptionSTRTOINTDEF);
 AddFunctionDesc('STRTOINT', rsNum, SDescriptionSTRTOINT);
 AddFunctionDesc('STRTOFLOAT', rsNum, SDescriptionSTRTOFLOAT);

end;

procedure TAddFunctionLibrary.DoInitDate;
var
  rsDate :String;
begin
  rsDate:= sDateCategory;
  // RxLib
  AddFunctionDesc('DATEDIFF', rsDate, SDescriptionDATEDIFF);
  AddFunctionDesc('INCDATE', rsDate, SDescriptionINCDATE);
  AddFunctionDesc('INCTIME', rsDate, SDescriptionINCTIME);
  AddFunctionDesc('DAYSPERMONTH', rsDate, SDescriptionDAYSPERMONTH);
  AddFunctionDesc('FIRSTDAYOFNEXTMONTH', rsDate, SDescriptionFIRSTDAYOFNEXTMONTH);
  AddFunctionDesc('FIRSTDAYOFPREVMONTH', rsDate, SDescriptionFIRSTDAYOFPREVMONTH);
  AddFunctionDesc('LASTDAYOFPREVMONTH', rsDate, SDescriptionLASTDAYOFPREVMONTH);
  AddFunctionDesc('INCDAY', rsDate, SDescriptionINCDAY);
  AddFunctionDesc('INCYEAR', rsDate, SDescriptionINCYEAR);

  // StLib
  AddFunctionDesc('ISRANGEDATE', rsDate, SDescriptionISRANGEDATE);
  AddFunctionDesc('STRTODATEDEF', rsDate, SDescriptionSTRTODATEDEF);
  AddFunctionDesc('VALIDDATE', rsDate, SDescriptionVALIDDATE);

  // Delphi
  AddFunctionDesc('INCMONTH', rsDate, SDescriptionINCMONTH);
  AddFunctionDesc('ISLEAPYEAR', rsDate, SDescriptionISLEAPYEAR);
end;

{ DoInitSQL }
procedure TAddFunctionLibrary.DoInitSQL;
var
  rsSQL :String;

begin

 rsSQL := 'SQL';

 // StLib
 AddFunctionDesc('CREATEDATE', rsSQL, SDescriptionCREATEDATE);
 AddFunctionDesc('CREATESTR', rsSQL, SDescriptionCREATESTR);
 AddFunctionDesc('CREATENUM', rsSQL, SDescriptionCREATENUM);

end;

procedure TAddFunctionLibrary.DoInitMath;
var
  rsMath :String;

begin

 rsMath := SMathCategory;

 AddFunctionDesc('ABS', rsMath, SDescriptionABS);

end;

{ DoInitOther }

procedure TAddFunctionLibrary.DoInitOther;
var
  rsOther :String;

begin

 rsOther := SOtherCategory;

 // TZ
 AddFunctionDesc('SWAP', rsOther, SDescriptionSWAP);

end;


constructor TAddFunctionLibrary.Create;
var
  rsDate:string;
begin
  inherited Create;
  Add('ABS');
  Add('CHR');
  Add('COMPARESTR');
  Add('CREATEDATE');
  Add('CREATENUM');
  Add('CREATESTR');
  Add('DATEDIFF');
  Add('DATETOSTR');
  Add('DAYSPERMONTH');
  Add('DELETE');
  Add('ENDPOS');
  Add('EXTRACTWORD');
  Add('FIRSTDAYOFNEXTMONTH');
  Add('FIRSTDAYOFPREVMONTH');
  Add('INCDATE');
  Add('INCDAY');
  Add('INCMONTH');
  Add('INCTIME');
  Add('INCYEAR');
  Add('INSERT');
  Add('ISLEAPYEAR');
  Add('ISRANGEDATE');
  Add('ISRANGENUM');
  Add('ISWORDPRESENT');
  Add('LASTDAYOFPREVMONTH');
  Add('LEFTCOPY');
  Add('NPOS');
  Add('PADCENTER');
  Add('PADLEFT');
  Add('PADRIGHT');
  Add('REPLACESTR');
  Add('REPLICATE');
  Add('RIGHTCOPY');
  Add('STRTODATEDEF');
  Add('STRTOFLOAT');
  Add('STRTOFLOATDEF');
  Add('STRTOINT');
  Add('STRTOINTDEF');
  Add('SWAP');
  Add('TIMETOSTR');
  Add('TRIMLEFT');
  Add('TRIMRIGHT');
  Add('VALIDDATE');
  Add('VALIDFLOAT');
  Add('VALIDINT');
  Add('WORDCOUNT');
  Add('WORDPOSITION');

  DoInitStr;
  DoInitNum;
  DoInitDate;
  DoInitSQL;
  DoInitMath;
  DoInitOther;
end;

procedure TAddFunctionLibrary.DoFunction(FNo: Integer; p1, p2, p3: Variant;
  var val: Variant);
var
  cStr :String;
  B:double;
begin

 val := '';

 case FNo of
   0: begin
        B:=frParser.Calc(p1);
        val := ABS(B);
      end;
   1: val := CHR(Byte(frParser.Calc(p1)));
   2: val := frCompareStr(frParser.Calc(p1),frParser.Calc(p2));
   3: val := frCreateDate(frParser.Calc(p1),cFFormatDate);
   4: val := frCreateNum(frParser.Calc(p1));
   5: val := frCreateStr(frParser.Calc(p1));
   6: begin
        cStr := frParser.Calc(p3);
        frDateDiffEx(frParser.Calc(p1),frParser.Calc(p2),cStr);
        DoScriptF(p3+':='+CHR(39)+cStr+CHR(39));
      end;
   7: val := DateToStr(frParser.Calc(p1));
   8: val := frDaysPerMonth(frParser.Calc(p1),frParser.Calc(p2));
   9: val := frDelete(frParser.Calc(p1),frParser.Calc(p2),frParser.Calc(p3));
  10: val := frEndPos(frParser.Calc(p1),frParser.Calc(p2));
  11: val := frExtractWord(frParser.Calc(p1),frParser.Calc(p2),ConvCS(frParser.Calc(p3)));
  12: val := frFirstDayOfNextMonth(frParser.Calc(p1));
  13: val := frFirstDayOfPrevMonth(frParser.Calc(p1));
  14: val := frIncDateEx(frParser.Calc(p1),frParser.Calc(p2));
  15: val := frIncDay(frParser.Calc(p1),frParser.Calc(p2));
  16: val := frIncMonth(frParser.Calc(p1),frParser.Calc(p2));
  17: val := frIncTimeEx(frParser.Calc(p1),frParser.Calc(p2));
  18: val := frIncYear(frParser.Calc(p1),frParser.Calc(p2));
  19: val := frInsert(frParser.Calc(p1),frParser.Calc(p2),frParser.Calc(p3));
  20: val := frIsLeapYear(frParser.Calc(p1));

  21: val := frIsRangeDate(frParser.Calc(p1),frParser.Calc(p2),frParser.Calc(p3));
  22: val := frIsRangeNum(frParser.Calc(p1),frParser.Calc(p2),frParser.Calc(p3));

  23: val := frIsWordPresent(frParser.Calc(p1),frParser.Calc(p2),ConvCS(frParser.Calc(p3)));
  24: val := frLastDayOfPrevMonth(frParser.Calc(p1));
  25: val := frLeftCopy(frParser.Calc(p1),frParser.Calc(p2));
  26: val := frNPos(frParser.Calc(p1),frParser.Calc(p2),frParser.Calc(p3));
  27: val := frPadCenter(frParser.Calc(p1),frParser.Calc(p2),frParser.Calc(p3));
  28: val := frPadLeft(frParser.Calc(p1),frParser.Calc(p2),frParser.Calc(p3));
  29: val := frPadRight(frParser.Calc(p1),frParser.Calc(p2),frParser.Calc(p3));
  30: val := frReplaceStr(frParser.Calc(p1),frParser.Calc(p2),frParser.Calc(p3));
  31: val := frReplicate(frParser.Calc(p1),frParser.Calc(p2));
  32: val := frRightCopy(frParser.Calc(p1),frParser.Calc(p2));
  33: val := frStrToDateDef(frParser.Calc(p1),frParser.Calc(p2));
  34: val := StrToFloat(frParser.Calc(p1));
  35: val := frStrToFloatDef(frParser.Calc(p1),frParser.Calc(p2));
  36: val := StrToInt(frParser.Calc(p1));
  37: val := StrToIntDef(frParser.Calc(p1),frParser.Calc(p2));
  38: begin
        DoScriptF('MTV2FR := ' + p1);
        DoScriptF(p1 + ' := ' + p2);
        DoScriptF(p2 + ' := MTV2FR');
      end;
  39: val := TimeToStr(frParser.Calc(p1));
  40:begin
       cStr:=frParser.Calc(p1);
       val := TrimLeft(cStr);
     end;
  41:begin
       cStr:=frParser.Calc(p1);
       val := TrimRight(cStr);
     end;
  42: val := frValidDate(frParser.Calc(p1));
  43: val := frValidFloat(frParser.Calc(p1));
  44: val := frValidInt(frParser.Calc(p1));
  45: val := frWordCount(frParser.Calc(p1),ConvCS(frParser.Calc(p2)));
  46: val := frWordPosition(frParser.Calc(p1),frParser.Calc(p2),ConvCS(frParser.Calc(p3)));
 end; { case }

end; { DoFunction }

initialization
  frRegisterFunctionLibrary(TAddFunctionLibrary);

  {$i addfunction.lrs}
end.
