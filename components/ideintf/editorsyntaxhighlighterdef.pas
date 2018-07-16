unit EditorSyntaxHighlighterDef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLazSyntaxHighlighter =
  ( lshNone, lshText, lshFreePascal, lshDelphi, lshLFM, lshXML, lshHTML,
    lshCPP, lshPerl, lshJava, lshBash, lshPython, lshPHP, lshSQL, lshCSS, lshJScript,
    lshDiff, lshBat, lshIni, lshPo, lshPike
  );

const
  LazSyntaxHighlighterNames: array[TLazSyntaxHighlighter] of String =
  ( 'None',
    'Text',
    'FreePascal',
    'Delphi',
    'LFM',
    'XML',
    'HTML',
    'C++',
    'Perl',
    'Java',
    'Bash',
    'Python',
    'PHP',
    'SQL',
    'CSS',
    'JScript',
    'Diff',
    'Bat',
    'Ini',
    'PO',
    'Pike'
  );

function GetSyntaxHighlighterCaption(h: TLazSyntaxHighlighter): string;
function StrToLazSyntaxHighlighter(const s: String): TLazSyntaxHighlighter;


implementation

function GetSyntaxHighlighterCaption(h: TLazSyntaxHighlighter): string;
begin
  if h=lshFreePascal then
    Result:='Free Pascal'
  else
    Result:=LazSyntaxHighlighterNames[h];
end;

function StrToLazSyntaxHighlighter(const s: String): TLazSyntaxHighlighter;
begin
  for Result := Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
    if (CompareText(s, LazSyntaxHighlighterNames[Result]) = 0) then
      exit;
  Result := lshFreePascal;
end;

end.

