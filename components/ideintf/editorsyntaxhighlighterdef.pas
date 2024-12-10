unit EditorSyntaxHighlighterDef;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils;

type
  TLazSyntaxHighlighter =
  ( lshNone, lshText, lshFreePascal, lshDelphi, lshLFM, lshXML, lshHTML,
    lshCPP, lshPerl, lshJava, lshBash, lshPython, lshPHP, lshSQL, lshCSS, lshJScript,
    lshDiff, lshBat, lshIni, lshPo, lshPike
  );

  TIdeSyntaxHighlighterID = type integer;

  { TIdeSyntaxHighlighterList }

  TIdeSyntaxHighlighterList = interface ['{266257FF-38B5-4071-AC90-97F6738B6F8F}']
    function GetLazSyntaxHighlighterType(AnId: TIdeSyntaxHighlighterID): TLazSyntaxHighlighter; deprecated '(to be removed in 4.99) -- Only temporary for StrToLazSyntaxHighlighter';

    function GetCount: integer;
    function GetCaptions(AnID: TIdeSyntaxHighlighterID): String;
    function GetNames(AnID: TIdeSyntaxHighlighterID): String;
    function GetSharedInstances(AnID: TIdeSyntaxHighlighterID): TObject;
    function GetSynHlClasses(AnID: TIdeSyntaxHighlighterID): TClass;

    function GetIdForLazSyntaxHighlighter(AnHighlighterType: TLazSyntaxHighlighter): TIdeSyntaxHighlighterID;
    function GetIdForFileExtension(Ext: String): TIdeSyntaxHighlighterID;
    function GetIdForFileExtension(Ext: String; ADelphiMode: boolean): TIdeSyntaxHighlighterID;
    function GetIdForName(AName: String): TIdeSyntaxHighlighterID;

    property Count: integer read GetCount;
    property Captions       [AnID: TIdeSyntaxHighlighterID]: String  read GetCaptions;
    property Names          [AnID: TIdeSyntaxHighlighterID]: String  read GetNames;
    property SynHlClasses   [AnID: TIdeSyntaxHighlighterID]: TClass  read GetSynHlClasses;     // class of TSynCustomHighlighter
    property SharedInstances[AnID: TIdeSyntaxHighlighterID]: TObject read GetSharedInstances; // TSynCustomHighlighter
  end;

const
  IdeHighlighterUnknownId      = TIdeSyntaxHighlighterID(-2); // Name not in list
  IdeHighlighterNotSpecifiedId = TIdeSyntaxHighlighterID(-1); // No Name given
  IdeHighlighterNoneID         = TIdeSyntaxHighlighterID(0);
  IdeHighlighterStartId = TIdeSyntaxHighlighterID(1); // first regulor Highlighter in IdeSyntaxHighlighters (lowest index)

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
  ) deprecated 'Use IdeSyntaxHighlighters (to be removed in 4.99)';

function GetSyntaxHighlighterCaption(h: TLazSyntaxHighlighter): string;     deprecated 'Use IdeSyntaxHighlighters (to be removed in 4.99)';
function StrToLazSyntaxHighlighter(const s: String): TLazSyntaxHighlighter; deprecated 'Use IdeSyntaxHighlighters (to be removed in 4.99)';

var
  IdeSyntaxHighlighters: TIdeSyntaxHighlighterList;

implementation

function GetSyntaxHighlighterCaption(h: TLazSyntaxHighlighter): string;
begin
  Result:=IdeSyntaxHighlighters.Captions[IdeSyntaxHighlighters.GetIdForLazSyntaxHighlighter(h)];
end;

function StrToLazSyntaxHighlighter(const s: String): TLazSyntaxHighlighter;
begin
  Result := IdeSyntaxHighlighters.GetLazSyntaxHighlighterType(IdeSyntaxHighlighters.GetIdForName(s)){%H-};
end;

end.

