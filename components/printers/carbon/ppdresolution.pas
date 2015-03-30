unit ppdresolution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MacOSAll, CarbonProc;

  function GetDefaultPPDResolution(aPrinter: PMPrinter; out HorzRes, VertRes: Integer): boolean;

implementation


function StrPasP(A,B: pchar): ansistring;
begin
  SetLength(Result, B-A);
  system.Move(A^, Result[1], B-A);
end;

procedure SkipBlanks(var A: pchar);
begin
  while A^ in [' ', #9] do
    Inc(A);    // skip white space
end;

function GetNumber(var B: pchar; var Number: Integer): boolean;
var
  A: pchar;
  Code: Integer;
begin
  Number := 0;
  result := false;
  A := B;
  while B^ in ['0'..'9'] do Inc(B);
  if A=B then
    exit;

  Val(StrPasP(A, B), Number, Code);
  result := Code=0;
end;

function ParseDefaultResolution(A:Pchar; out ResTag: ansistring; out HorzRes, VertRes: Integer): boolean;
var
  B: PChar;
begin

  result := false;
  HorzRes := 300;
  VertRes := 300;
  if A=nil then
    exit;

  inc(A, 19);                         // skip *DefaultResolution:
  SkipBlanks(A);
  B := A;
  while not (B^ in [' ', #9, #10, #13]) do inc(B);
  if A=B then
    exit;

  ResTag := StrPasP(A, B);
  A := @ResTag[1];

  // get first number
  B := A;
  result := GetNumber(B, HorzRes);
  if not result then
    exit;

  if B^='d' then begin // start of dpi, we are done
    VertRes := HorzRes;
    result := true;
    exit;
  end;
  if B^<>'x' then  // unexpected res format, expected NNNxMMMdpi
    exit;

  // get second number
  inc(B);
  A := B;
  result := GetNumber(B, VertRes);
end;

function GetDefaultResolutionFromPtr(Buf: PChar;
  var HorzRes, VertRes:Integer): boolean;
var
  A, B: PChar;
  ResTag: ansistring;
begin

  result := false;
  A := strpos(Buf, '*DefaultResolution:');
  if A=nil then
    exit;

  result := ParseDefaultResolution(A, ResTag, HorzRes, VertRes);
  if not result then
    exit;

  // now check for *OpenUI: *Resolution, maybe ResTag is just a tag
  A := strpos(Buf, '*OpenUI *Resolution');
  if A=nil then begin
    // not found, assume ResTag is a valid value
    exit;
  end;

  // restrict ourselves to this block
  B := strpos(A, '*CloseUI: *Resolution');
  if B=nil then
    exit;  // something is wrong but we have a standalone default resolution
           // we take it
  B^ := #0;

  result := false;
  repeat
    // find default resolution entry
    B := strpos(A, #10'*Resolution');
    if B<>nil then begin
      inc(B, 12);
      SkipBlanks(B);
      // is this the one we are looking for?
      if strlcomp(B, @ResTag[1], Length(ResTag))=0 then begin
        // it is, look for /HWResolution
        A := strpos(B, '/HWResolution');
        if A<>nil then begin
          // found
          inc(A, 13);
          SkipBlanks(A);
          // we are not a postscript interpreter, only look for
          // resolution values like NNN or [NNN MMM]
          if A^='[' then begin
            Inc(A);
            SkipBlanks(A);
            Result := GetNumber(A, HorzRes);
            if Result then begin
              SkipBlanks(A);
              Result := GetNumber(A, VertRes);
            end;
          end else begin
            result := GetNumber(A, HorzRes);
            VertRes := HorzRes;
          end;
        end else
          // /HWResolution not found, assume ResTag was in valid format
          result := true;

        break;
      end;
      A := B;
    end;
  until B=nil;
end;

function GetDefaultPPDResolution(aPrinter: PMPrinter; out HorzRes, VertRes: Integer
  ): boolean;
var
  PPD: ansistring;
  Name: CFStringRef;
  aURL: CFURLRef = nil;
  Range: CFRange;
  Data: CFDataRef = nil;
begin
  VertRes := 0;
  HorzRes := 0;

  CreateCFString('PMPPDDescriptionType', Name);
  Result := PMPrinterCopyDescriptionURL(aPrinter, Name, aURL)=noErr;
  FreeCFString(Name);
  if Result then begin
    PMCopyPPDData(aURL, Data);
    FreeCFString(aURL);
    if Data<>nil then begin
      Range.length := CFDataGetLength(Data);
      Range.location := 0;
      SetLength(PPD, Range.length);
      CFDataGetBytes(Data, Range, @PPD[1]);
      CFRelease(Data);
      result := GetDefaultResolutionFromPtr(@PPD[1], HorzRes, VertRes);
    end;
  end;

end;

end.

