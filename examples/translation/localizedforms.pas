{ This form can be the ancestor of other forms. It is suitable for changing
  languages at run-time. It provides the virtual method "UpdateTranslation"
  which must be overridden by each inherited form in order to update the
  translation of strings that are not automatically translated by
  DefaultTranslator (e.g. strings built up by means of Format statements).

  The unit contains also a procedure UpdateFormatSettings which adapts the
  format settings to the newly selected language, as well as UpdateBiDiMode
  which enable right-to-left mode as it is used in the Middle East (not perfect,
  though...) }

unit LocalizedForms;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, Forms;

type
  TLocalizedForm = class(TForm)
  protected
    procedure UpdateTranslation(ALang: String); virtual;
  public
    (* Activate for Lazarus version older than 1.2
    procedure FlipChildren(AllLevels: Boolean); override;
    *)
  end;

var
  LocalizedForm: TLocalizedForm;
  CurrentLang: String = '';
  CodePage: String = '';

procedure UpdateBiDiMode(ALang: String);
procedure UpdateFormatSettings(ALang: String);  // Windows only!

// Utility functions for Windows only
{$IFDEF MSWINDOWS}
function GetFullLangCodeFromLCID(LCID: Integer): String;
function GetLangCodeFromLCID(LCID: Integer): String;
function GetLCIDFromLangCode(ALang: String): Integer;
{$ENDIF}


implementation

{$R *.lfm}

{ Local procedures }

{$IFDEF MSWINDOWS}
function GetLocaleStr(LCID, LT: Longint; const Def: string): ShortString;
// borrowed from SysUtils
var
  L: Integer;
  Buf: array[0..255] of Char;
begin
  L := GetLocaleInfo(LCID, LT, Buf, SizeOf(Buf));
  if L > 0 then
    SetString(Result, @Buf[0], L - 1)
  else
    Result := Def;
end;
{$ENDIF}


{ Utility procedures - Windows-only }

{$IFDEF MSWINDOWS}
{ This is how to convert LCID to language code like 'de', 'en', etc.
  Works only for Windows.
  See also: GetFullLangCode}
function GetLangCodeFromLCID(LCID: Integer): String;
var
  language: PAnsiChar;
begin
  language := StrAlloc(255);
  try
    GetLocaleInfoA(LCID, LOCALE_SISO639LANGNAME, PAnsiChar(language), 255);
    Result := language;
  finally
    StrDispose(language);
  end;
  Result := '';
end;

{ This is how to convert LCID to language code like 'de_DE', 'en_US', etc.
  The first two characters are the country, the last two are the region.
  Works only for Windows.
  See: http://stackoverflow.com/questions/1192361/how-to-convert-microsoft-locale-id-lcid-into-language-code-or-locale-object-in/1192856#1192856
  or http://delphi.cjcsoft.net/viewthread.php?tid=45881
  or http://msdn.microsoft.com/en-us/library/dd318101%28VS.85%29.aspx }
function GetFullLangCodeFromLCID(LCID: Integer): String;
var
  language, country: PAnsiChar;
begin
  language := StrAlloc(255);
  country := StrAlloc(255);
  try
    GetLocaleInfoA(LCID, LOCALE_SISO639LANGNAME, PAnsiChar(language), 255);
    GetLocaleInfoA(LCID, LOCALE_SISO3166CTRYNAME, PAnsiChar(country), 255);
    Result := language + '_' + country;
  finally
    StrDispose(country);
    StrDispose(language);
  end;
end;

{ This function determines the LCID from the language code.
  Works only for Windows. }
function GetLCIDFromLangCode(ALang: String): Integer;
begin
 case lowercase(ALang) of
   'ar'   : Result := $0401;    // Arabic
   'bg'   : Result := $0403;    // Bulgarian
   'ca'   : Result := $0403;    // Catalan
   'cs'   : Result := $0405;    // Czech
   'de'   : Result := $0407;    // German
   'en'   : Result := $0409;    // English  (US)
   'es'   : Result := $040A;    // Spanisch
   'fi'   : Result := $040B;    // Finnish
   'fr'   : Result := $040C;    // French
   'he'   : Result := $040D;    // Hebrew
   'hu'   : Result := $040E;    // Hungarian
   'it'   : Result := $0410;    // Italian
   'jp'   : Result := $0411;    // Japanese
   'pl'   : Result := $0415;    // Polish
   'pt'   : Result := $0816;    // Portuguese (Portugal)
   'pt_br': Result := $0416;    // Portuguese (Brazil)
   'ru'   : Result := $0419;    // Russian
   'tr'   : Result := $041F;    // Turkish
   'uk'   : Result := $0422;    // Ukrainian
   'lt'   : Result := $0427;    // Lithuanian
   'zh_cn', 'zh-cn': Result := $0804;    // Chinese (China)
   'zh_tw', 'zh-tw': Result := $0404;    // Chinese (Taiwan)
   // please complete if necessary. Language code and LCIDs can be found at
   // http://www.science.co.il/Language/Locale-codes.asp
   else  raise Exception.CreateFmt('Language "%s" not supported. Please add to GetLCIDFromLangCode.',[ALang]);
 end;
end;
{$ENDIF}

{ SetDefaultSettings changes the FormatSettings according to the selected
  language. Unfortunately there is not platform-independent way to do this
  at the moment. Here is the solution for Windows. }
procedure UpdateFormatSettings(ALang: String);
{$IFDEF MSWINDOWS}
var
  LCID: Integer;
{$ENDIF}
begin
 {$IFDEF MSWINDOWS}
  // Determine the LCID for the requested language
  LCID := GetLCIDFromLangCode(ALang);

  // Now we update the format settings to the new language
 {$PUSH}
 {$WARNINGS OFF}
  GetLocaleFormatSettings(LCID, DefaultFormatSettings);
 {$POP}

  // We also store the code page that belongs to the new language. We'll need
  // that when converting FCL strings to UTF8.
  CodePage := 'cp'+GetLocaleStr(LCID, LOCALE_IDEFAULTANSICODEPAGE, '');
 {$ENDIF}
end;

procedure UpdateBiDiMode(ALang: String);
begin
  if Application.IsRTLLang(ALang) then
    Application.BidiMode := bdRightToLeft
  else
    Application.BiDiMode := bdLeftToRight;
end;


{  TLocalizedForm  }

{ This is a workaround for older version of Lazarus where FlipChildren does
  not work correctly with TRadioGroup and TCheckGroup. Activate this code
  for versions older than Lazarus 1.2 }
(*
procedure TLocalizedForm.FlipChildren(AllLevels: Boolean);

  procedure _DoFlipChildren(AControl: TWinControl);
  var
    C: TControl;
    i: Integer;
  begin
    if (not (AControl is TCustomRadioGroup)) and
       (not (AControl is TCustomCheckGroup))
    then begin
      for i:=0 to AControl.ControlCount-1 do begin
        C := AControl.Controls[i];
        if (C is TWinControl) and (TWinControl(C).ControlCount > 0) then
          _DoFlipChildren(TWinControl(C));
      end;
    end;
  end;

begin
 if AllLevels then
   _DoFlipChildren(Self);
 inherited FlipChildren(false);
end;
*)

{ Each inherited form will have to put code in procedure
  UpdateTranslation(ALang: String) to translate strings not
  automatically handled by DefaultTranslator.
  Don't forget to call "inherited". }
procedure TLocalizedForm.UpdateTranslation(ALang: String);
var
  i: Integer;
  F: TLocalizedForm;
begin
  if Application.MainForm = self then
    for i:=0 to Screen.FormCount-1 do
      if Screen.Forms[i] is TLocalizedForm then begin
        F := TLocalizedForm(Screen.Forms[i]);
        if F <> self then F.UpdateTranslation(ALang);
      end;

  if Application.IsRTLLang(ALang) <> Application.IsRTLLang(CurrentLang) then
    FlipChildren(true);
end;

end.

