unit EditorSyntaxHighlighterDef;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, LazMethodList, Graphics;

type
  TLazSyntaxHighlighter =
  ( lshNone, lshText, lshFreePascal, lshDelphi, lshLFM, lshXML, lshHTML,
    lshCPP, lshPerl, lshJava, lshBash, lshPython, lshPHP, lshSQL, lshCSS, lshJScript,
    lshDiff, lshBat, lshIni, lshPo, lshPike, lshMarkdown
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
  IdeHighlighterStartId = TIdeSyntaxHighlighterID(1); // first regular Highlighter in IdeSyntaxHighlighters (lowest index)

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
    'Pike',
    'MarkDown'
  ) deprecated 'Use IdeSyntaxHighlighters (to be removed in 4.99)';

function GetSyntaxHighlighterCaption(h: TLazSyntaxHighlighter): string;     deprecated 'Use IdeSyntaxHighlighters (to be removed in 4.99)';
function StrToLazSyntaxHighlighter(const s: String): TLazSyntaxHighlighter; deprecated 'Use IdeSyntaxHighlighters (to be removed in 4.99)';


type
  TColorSchemeAttributeFeature =
    ( hafBackColor, hafForeColor, hafFrameColor, hafAlpha, hafPrior,
      hafStyle, hafStyleMask,
      hafFrameStyle, hafFrameEdges,
      hafMarkupFoldColor, // for the MarkupFoldColor module
      hafMarkupAllOverview, // for indicator in overview-gutter
      hafCustomWords
    );
  TColorSchemeAttributeFeatures = set of TColorSchemeAttributeFeature;

  IColorSchemeAttribute = interface ['{2572547D-217A-4A83-A910-0D808ECF3317}']
    procedure ApplyTo(aDest: TObject);
    function GetMarkupAllOverviewColor: TColor;

    property MarkupAllOverviewColor: TColor read GetMarkupAllOverviewColor; // hafMarkupAllOverview
  end;

  IColorSchemeLanguage = interface ['{40A0F5E1-ADD5-4E0E-BD14-583E244C4ACC}']
    function GetName: String;
    function AttributeCount: Integer;
    function GetAttributeIntf(AnIndex: integer): IColorSchemeAttribute;  //TSynHighlighterAttributesModifier
    function GetAttributeIntf(const AStoredName: string): IColorSchemeAttribute;  //TSynHighlighterAttributesModifier
  end;

  IColorScheme = interface ['{121AB166-7458-4AD8-8122-C9AD4A259521}']
    function GetName: String;
    function Count: integer;
    function GetLanguage(AnIndex: Integer): IColorSchemeLanguage;
    function GetLanguageForHighlighter(AnHiglighter: TObject): IColorSchemeLanguage;
    function GetLanguageForHighlighter(AnHighlighterId: TIdeSyntaxHighlighterID): IColorSchemeLanguage;
  end;

  IColorSchemeList = interface ['{BA72F07B-77F5-4C36-AE9C-907980ADDEE3}']
    function Count: integer;
    function GetScheme(AnIndex: Integer): IColorScheme;
    function GetScheme(AName: String): IColorScheme;
    function GetCurrentSchemeForHighlighter(AnHiglighter: TObject): IColorScheme;
    function GetCurrentSchemeForHighlighter(AnHighlighterId: TIdeSyntaxHighlighterID): IColorScheme;

    procedure RegisterChangedHandler(AnHandler: TNotifyEvent);
    procedure UnregisterChangedHandler(AnHandler: TNotifyEvent);

    function RegisterAttributeGroup(AName: PString): integer; // pointer to resource string
    procedure AddAttribute(AnAttrGroup: integer; AnHighlighterId: TIdeSyntaxHighlighterID; AStoredName: String; AName: PString;  AFeatures: TColorSchemeAttributeFeatures; ADefaults: TObject = nil);
  end;

var
  IdeSyntaxHighlighters: TIdeSyntaxHighlighterList;
  IdeColorSchemeList: IColorSchemeList;

procedure RegisterOnIdeColorSchemeListCreated(AnHandler: TNotifyEvent);
procedure _IDE_CallOnIdeColorSchemeListCreated;


implementation

function GetSyntaxHighlighterCaption(h: TLazSyntaxHighlighter): string;
begin
  Result:=IdeSyntaxHighlighters.Captions[IdeSyntaxHighlighters.GetIdForLazSyntaxHighlighter(h)];
end;

function StrToLazSyntaxHighlighter(const s: String): TLazSyntaxHighlighter;
begin
  Result := IdeSyntaxHighlighters.GetLazSyntaxHighlighterType(IdeSyntaxHighlighters.GetIdForName(s)){%H-};
end;

var
  OnIdeColorSchemeListCreated: TMethodList;
procedure RegisterOnIdeColorSchemeListCreated(AnHandler: TNotifyEvent);
begin
  if IdeColorSchemeList <> nil then begin
    AnHandler(nil);
    exit;
  end;

  if OnIdeColorSchemeListCreated = nil then
    OnIdeColorSchemeListCreated := TMethodList.Create;
  OnIdeColorSchemeListCreated.Add(TMethod(AnHandler));
end;

procedure _IDE_CallOnIdeColorSchemeListCreated;
begin
  if OnIdeColorSchemeListCreated = nil then
    exit;
  OnIdeColorSchemeListCreated.CallNotifyEvents(nil);
  FreeAndNil(OnIdeColorSchemeListCreated);
end;

finalization
  FreeAndNil(OnIdeColorSchemeListCreated);

end.

