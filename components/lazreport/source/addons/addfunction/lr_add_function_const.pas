unit lr_add_function_const;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

resourcestring
  SDescriptionISWORDPRESENT = 'ISWORDPRESENT(<Word>,<String>,<Delimiters>)/'+
    'Determines is word <Word> present in the string <String>.'+
    '<Delimiters> is the list of word delimiters.';

  SDescriptionWORDPOSITION = 'WORDPOSITION(<WordNo>,<String>,<Delimiters>)/'+
    'Returns position of word number <WordNo> in the string <String>.'+
    '<Delimiters> is the list of word delimiters.';

  SDescriptionEXTRACTWORD = 'EXTRACTWORD(<WordNo>,<String>,<Delimiters>)/'+
    'Returns word number <WordNo> from the string <String>.'+
    '<Delimiters> is the list of word delimiters.';

  SDescriptionWORDCOUNT = 'WORDCOUNT(<String>,<Delimiters>)/'+
    'Returns number of words in the string <String>.'+
    '<Delimiters> is the list of word delimiters.';

  SDescriptionNPOS = 'NPOS(<SubStr>,<String>,<SubStrNo>)/'+
    'Returns position of <SubStrNo>-th substring <SubStr> inclusion in '+
    'the string <String>.';

  SDescriptionREPLACESTR = 'REPLACESTR(<String>,<SubStr1>,<SubStr2>)/'+
    'Replaces all inclusions of <SubStr1> string to the <SubStr2> string '+
    'in the string <String> and returns the result.';

  SDescriptionTRIMRIGHT = 'TRIMRIGHT(<String>)/'+
    'Trims all right spaces from the string <String> and returns the result.';

  SDescriptionTRIMLEFT = 'TRIMLEFT(<String>)/'+
    'Trims all left spaces from the string <String> and returns the result.';

  SDescriptionDELETE = 'DELETE(<String>,<DelFrom>,<DelCount>)/'+
    'Deletes <DelCount> symbols starting at position <DelFrom> in the '+
    'given string <String> and returns the result.';

  SDescriptionINSERT = 'INSERT(<SubStr>,<String>,<InsertFrom>)/'+
    'Inserts <SubStr> substring into <String> string starting at position '+
    '<InsertFrom> and returns the result.';

  SDescriptionDATETOSTR = 'DATETOSTR(<Date>)/'+
    'Converts date <Date> to string and returns the result.';

  SDescriptionTIMETOSTR = 'TIMETOSTR(<Time>)/'+
    'Converts time <Time> to string and returns the result.';

  SDescriptionREPLICATE = 'REPLICATE(<Symbol>,<Length>)/'+
    'Returns the string with length <Length> that consists of symbols <Symbol>.';

  SDescriptionPADLEFT = 'PADLEFT(<String>,<Length>,<Symbol>)/'+
    'Adds symbols <Symbol> to begin of the string <String> to make it as long '+
    'as stated in the <Length> parameter and returns result string.';

  SDescriptionPADRIGHT = 'PADRIGHT(<String>,<Length>,<Symbol>)/'+
    'Adds symbols <Symbol> to end of the string <String> to make it as long '+
    'as stated in the <Length> parameter and returns result string.';

  SDescriptionPADCENTER = 'PADCENTER(<String>,<Length>,<Symbol>)/'+
    'Adds symbols <Symbol> to begin and end of the string <String> to make it as long '+
    'as stated in the <Length> parameter and returns result string.';

  SDescriptionENDPOS = 'ENDPOS(<String>,<SubStr>)/'+
    'Returns position of substring <SubStr> in the string <String> starting '+
    'at the end of the string.';

  SDescriptionLEFTCOPY = 'LEFTCOPY(<String>,<Count>)/'+
    'Copies number of symbols <Count> from the string <String> starting '+
    'at the begin of the string.';

  SDescriptionRIGHTCOPY = 'RIGHTCOPY(<String>,<Count>)/'+
    'Copies number of symbols <Count> from the string <String> starting '+
    'at the end of the string.';

  SDescriptionCOMPARESTR = 'COMPARESTR(<String1>,<String2>)/'+
    'Compares two strings. Returns the position where begins the difference '+
    'between the strings or 0 if strings are equivalent.';

  SDescriptionCHR = 'CHR(<Code>)/'+
    'Returns symbol of ASCII code <Code>.';



  SDescriptionVALIDINT = 'VALIDINT(<String>)/'+
    'Returns True if <String> is valid integer value.';

  SDescriptionVALIDFLOAT = 'VALIDFLOAT(<String>)/'+
    'Returns True if <String> is valid float value.';

  SDescriptionISRANGENUM = 'ISRANGENUM(<Number1>,<Number2>,<Number3>)/'+
    'Returns True if <Number3> is between <Number1> and <Number2>.';

  SDescriptionSTRTOFLOATDEF = 'STRTOFLOATDEF(<String>,<DefValue>)/'+
    'Converts <String> string to float value. If conversion fails, returns '+
    'default value <DefValue>.';

  SDescriptionSTRTOINTDEF = 'STRTOINTDEF(<String>,<DefValue>)/'+
    'Converts <String> string to integer value. If conversion fails, returns '+
    'default value <DefValue>.';

  SDescriptionSTRTOINT = 'STRTOINT(<String>)/'+
    'Converts <String> string to the integer value.';

  SDescriptionSTRTOFLOAT = 'STRTOFLOAT(<String>)/'+
    'Converts <String> string to the float value.';


  SDescriptionDATEDIFF = 'DATEDIFF(<Date1>,<Date2>,<var String>)/'+
    'Returns the difference between two dates <Date1> and <Date2>. '+
    'Result is in the string <String> in format "days;months;years".';

  SDescriptionINCDATE = 'INCDATE(<Date>,<String>)/'+
    'Increments the date <Date> by given number of days, months and years '+
    'passed in the <String> parameter in format "days;months;years". '+
    'Returns the result date.';

  SDescriptionINCTIME = 'INCTIME(<Time>,<String>)/'+
    'Increments the time <Time> by given number of hours, minutes, seconds '+
    'and milliseconds passed in the <String> parameter in format "h;min;sec;msec". '+
    'Returns the result time.';

  SDescriptionDAYSPERMONTH = 'DAYSPERMONTH(<Year>,<Month>)/'+
    'Returns days in the given month <Month> of the year <Year>.';

  SDescriptionFIRSTDAYOFNEXTMONTH = 'FIRSTDAYOFNEXTMONTH(<Date>)/'+
    'Returns the date of first day of the next month of date <Date>.';

  SDescriptionFIRSTDAYOFPREVMONTH = 'FIRSTDAYOFPREVMONTH(<Date>)/'+
    'Returns the date of first day of the previous month of date <Date>.';

  SDescriptionLASTDAYOFPREVMONTH = 'LASTDAYOFPREVMONTH(<Date>)/'+
    'Returns the date of last day of the previous month of date <Date>.';

  SDescriptionINCDAY = 'INCDAY(<Date>,<Number>)/'+
    'Increments the date <Date> by given number of days <Number> and returns '+
    'the result date.';

  SDescriptionINCYEAR = 'INCYEAR(<Date>,<Number>)/'+
    'Increments the date <Date> by given number of years <Number> and returns '+
    'the result date.';

  SDescriptionISRANGEDATE = 'ISRANGEDATE(<Date1>,<Date2>,<Date3>)/'+
    'Returns True if date <Date3> is between <Date1> and <Date2>.';

  SDescriptionSTRTODATEDEF = 'STRTODATEDEF(<String>,<DefDate>)/'+
    'Converts <String> string to date. If conversion fails, returns '+
    'default value <DefDate>.';

  SDescriptionVALIDDATE = 'VALIDDATE(<String>)/'+
    'Returns True if <String> string is valid date.';

  SDescriptionINCMONTH = 'INCMONTH(<Date>,<Number>)/'+
    'Increments the date <Date> by given number of months <Number> and returns '+
    'the result date.';

  SDescriptionISLEAPYEAR = 'ISLEAPYEAR(<Year>)/'+
    'Returns True if <Year> year is leap year.';


  SDescriptionCREATEDATE = 'CREATEDATE(<String>)/'+
    'Converts <String> string to string that contains date to use it in SQL '+
    'clause. To use this function put the string with desired date format '+
    'to TfrAddFunctionLibrary.FormatDate property.';

  SDescriptionCREATESTR = 'CREATESTR(<String>)/'+
    'Adds quotes to the <String> string to use it in SQL clause.';

  SDescriptionCREATENUM = 'CREATENUM(<String>)/'+
    'Converts <String> string to string that contains numeric value to use '+
    'it in SQL clause.';



  SDescriptionSWAP = 'SWAP(<var var1>,<var var2>)/'+
    'Swaps the variables var1 and var2.';



  SDescriptionABS = 'ABS(<Number>)/'+
    'Returns absolute value <Number>';


  SStringCategory = 'String';
  SMathCategory   = 'Math';
  sDateCategory   = 'Date';
  SOtherCategory  = 'Other';
implementation

end.

