{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterGeneral.pas, released 2000-04-07.
The Original Code is based on the mwGeneralSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions written by Martin Waldenburg are copyright 1999 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterGeneral.pas,v 1.3 2000/11/08 22:09:59 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net
}
unit SynBeautifier;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, RegExpr, LCLProc,
  SynEditMiscClasses, SynEditMiscProcs, LazSynEditText, SynEditPointClasses,
  SynEditKeyCmds, SynHighlighterPas, SynEditHighlighterFoldBase;

type

  TSynCustomBeautifier = class;

  // Callback for indent
  TSynBeautifierSetIndentProc =
    procedure(
      (* LinePos:
           1-based, the line that should be changed *)
      LinePos: Integer;
      (* Indent:
           New indent in spaces (Logical = Physical *)
      Indent: Integer;
      (* RelativeToLinePos:
           Indent specifies +/- offset from indent on RTLine (0: for absolute indent) *)
      RelativeToLinePos: Integer = 0;
      (* IndentChars:
           String used to build indent; maybe empty, single char, or string (usually 1 tab or 1 space)
           The String will be repeated and cut as needed, then filled with spaces at the end
         * NOTE: If this is specified the TSynBeautifierIndentType is ignored
      *)
      IndentChars: String = '';
      (* IndentCharsFromLinePos:
           Use tab/space mix from this Line for indent (if specified > 0)
           "IndentChars" will only be used, if the found tab/space mix is to short
         * NOTE: If this is specified the TSynBeautifierIndentType is ignored
      *)
      IndentCharsFromLinePos: Integer = 0
    ) of object;

  // Event triggered if Lines may needs Indend
  TSynBeautifierGetIndentEvent =
    function(
      Sender: TObject;                       // the beautifier
      Editor: TObject;                       // the synedit
      LogCaret, OldLogCaret: TPoint;         // Caret after and before the edit action
      FirstLinePos, LastLinePos: Integer;    // Changed lines. this can include lines outside the range of OldLogCaret to LogCaret
      Reason: TSynEditorCommand;             // what caused the event
      SetIndentProc: TSynBeautifierSetIndentProc
     ): boolean of object;


  { TSynCustomBeautifier }

  TSynCustomBeautifier = class(TComponent)
  private
    FAutoIndent: Boolean;
    FOnGetDesiredIndent: TSynBeautifierGetIndentEvent;
    FCurrentEditor: TSynEditBase; // For callback / applyIndent
    FCurrentLines: TSynEditStrings;
  protected
    procedure DoBeforeCommand(const ACaret: TSynEditCaret;
                              var Command: TSynEditorCommand); virtual; abstract;
    procedure DoAfterCommand(const ACaret: TSynEditCaret;
                             var Command: TSynEditorCommand;
                             StartLinePos, EndLinePos: Integer); virtual; abstract;
    property CurrentEditor: TSynEditBase read FCurrentEditor;
    property CurrentLines: TSynEditStrings read FCurrentLines;
  public
    procedure Assign(Src: TPersistent); override;
    function  GetCopy: TSynCustomBeautifier;
    procedure BeforeCommand(const Editor: TSynEditBase; const Lines: TSynEditStrings;
                            const ACaret: TSynEditCaret; var Command: TSynEditorCommand;
                            InitialCmd: TSynEditorCommand);
    procedure AfterCommand(const Editor: TSynEditBase; const Lines: TSynEditStrings;
                           const ACaret: TSynEditCaret; var Command: TSynEditorCommand;
                           InitialCmd: TSynEditorCommand; StartLinePos, EndLinePos: Integer);
    // GetDesiredIndentForLine: Returns the 1-based Physical x pos
    function GetDesiredIndentForLine
             (Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret): Integer; virtual; abstract;
    function GetDesiredIndentForLine
             (Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret; out ReplaceIndent: Boolean;
              out DesiredIndent: String): Integer; virtual; abstract;
    property OnGetDesiredIndent: TSynBeautifierGetIndentEvent
      read FOnGetDesiredIndent write FOnGetDesiredIndent;
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent;
  end;

  TSynCustomBeautifierClass = class of TSynCustomBeautifier;
  TSynBeautifierIndentType = (
    sbitSpace, sbitCopySpaceTab,
    sbitPositionCaret,
    sbitConvertToTabSpace,   // convert to tabs, fill with spcaces if needed
    sbitConvertToTabOnly     // convert to tabs, even if shorter
  );

  { TSynBeautifier }

  TSynBeautifier = class(TSynCustomBeautifier)
  private
    FIndentType: TSynBeautifierIndentType;
  protected
    FLogicalIndentLen: Integer;

    function GetLine(AnIndex: Integer): String; virtual;
    procedure GetIndentInfo(const LinePos: Integer;
                            out ACharMix: String; out AnIndent: Integer;
                            AStopScanAtLine: Integer = 0);
    // requiring FCurrentEditor, FCurrentLines
    procedure DoBeforeCommand(const ACaret: TSynEditCaret;
                              var Command: TSynEditorCommand); override;
    procedure DoAfterCommand(const ACaret: TSynEditCaret;
                             var Command: TSynEditorCommand;
                             StartLinePos, EndLinePos: Integer); override;
    function GetIndent(const LinePos: Integer; out BasedOnLine: Integer;
                       AStopScanAtLine: Integer = 0): Integer;
    function AdjustCharMix(DesiredIndent: Integer; CharMix, AppendMix: String): String;
    function CreateTabSpaceMix(var DesiredIndent: Integer; OnlyTabs: Boolean): String;
    function GetCharMix(const LinePos: Integer; var Indent: Integer;
                        var IndentCharsFromLinePos: Integer;
                        ModifyIndent: Boolean = False): String;
    procedure ApplyIndent(LinePos: Integer; Indent: Integer;
                          RelativeToLinePos: Integer = 0; IndentChars: String = '';
                          IndentCharsFromLinePos: Integer = 0);
    function UnIndentLine(const ACaret: TSynEditCaret; out CaretNewX: Integer): Boolean;
  public
    procedure Assign(Src: TPersistent); override;
    // Retruns a 0-based position (even 0-based physical)
    function GetIndentForLine(Editor: TSynEditBase; const Line: string;
                        Physical: boolean): Integer;
    function GetDesiredIndentForLine
             (Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret): Integer; override;
    function GetDesiredIndentForLine
             (Editor: TSynEditBase; const Lines: TSynEditStrings;
              const ACaret: TSynEditCaret; out ReplaceIndent: Boolean;
              out DesiredIndent: String): Integer; override;
  published
    property IndentType: TSynBeautifierIndentType read FIndentType write FIndentType;
  end;

  TSynCommentContineMode = (
      sccNoPrefix,      // May still do indent, if matched
      sccPrefixAlways,  // If the pattern did not match all will be done, except the indent AFTER the prefix (can not be detected)
      sccPrefixMatch
      //sccPrefixMatchFirst
      //sccPrefixMatchPrev
      //sccPrefixMatchPrevTwo           // last 2 lines match
      //sccPrefixMatchPrevTwoExact      // last 2 lines match and same result for prefix
    );

  TSynCommentMatchLine = (
      sclMatchFirst, // Match the first line of the comment to get substitutes for Prefix ($1)
      sclMatchPrev   // Match the previous line of the comment to get substitutes for Prefix ($1)
      //sclMatchNestedFirst // For nested comments, first line of this nest.
    );

  TSynCommentMatchMode = (
    // Which parts of the first line to match sclMatchFirst or sclMatchPrev (if prev = first)
      scmMatchAfterOpening, // will not include (*,{,//. The ^ will match the first char after
      scmMatchOpening,      // will include (*,{,//. The ^ will match the ({/
      scmMatchWholeLine,    // Match the entire line
      scmMatchAtAsterisk    // AnsiComment only, will match the * of (*, but not the (
    );

  TSynCommentIndentFlag = (
    // * For Matching lines (FCommentMode)
      // By default indent is the same as for none comment lines (none overrides sciAlignOpen)
      sciNone,      // Does not Indent comment lines (Prefix may contain a fixed indent)
      sciAlignOpen, // Indent to real opening pos on first line, if comment does not start at BOL "Foo(); (*"
                    // Will force every new line back to Align.
                    // To align only once, use IndentFirstLineExtra=MaxInt

      // sciAdd...: if (only if) previous line had started with the opening token "(*" or "{".
      //            or if sciAlignOpen is set
      //            This is not done for "//", as Comment Extension will add a new "//"
      //            But Applies to sciNone
      sciAddTokenLen,        // add 1 or 2 spaces to indent (for the length of the token)
                             // in case of scmMatchAtAsterisk, 1 space is added. ("(" only)
      sciAddPastTokenIndent, // Adds any indent found past the opening token  "(*", "{" or "//".
                             // For "//" this is added after the nem "//", but before the prefix.


      sciMatchOnlyTokenLen,        // Apply the Above only if first line matches. (Only if sciAddTokenLen is specified)
      sciMatchOnlyPastTokenIndent,
      sciAlignOnlyTokenLen,        // Apply the Above only if sciAlignOpen was used (include via max)
      sciAlignOnlyPastTokenIndent,

      // flag to ignore spaces, that are matched.
      // flag to be smart, if not matched

      sciApplyIndentForNoMatch  // Apply above rules For NONE Matching lines (FCommentMode),
                                // includes FIndentFirstLineExtra
    );
  TSynCommentIndentFlags = set of TSynCommentIndentFlag;

  TSynCommentExtendMode = (
      sceNever,                // Never Extend
      sceAlways,               // Always
      sceSplitLine,            // If the line was split (caret was not at EOL, when enter was pressed
      sceMatching,             // If the line matched (even if sccPrefixAlways or sccNoPrefix
      sceMatchingSplitLine
    );

  TSynCommentType = (sctAnsi, sctBor, sctSlash);

    // end mode
  { TSynBeautifierPascal }

  TSynBeautifierPascal = class(TSynBeautifier)
  private
    FIndentMode:           Array [TSynCommentType] of TSynCommentIndentFlags;
    FIndentFirstLineExtra: Array [TSynCommentType] of String;
    FIndentFirstLineMax:   Array [TSynCommentType] of Integer;

    FCommentMode:          Array [TSynCommentType] of TSynCommentContineMode;
    FMatchMode:            Array [TSynCommentType] of TSynCommentMatchMode;
    FMatchLine:            Array [TSynCommentType] of TSynCommentMatchLine;
    FMatch:                Array [TSynCommentType] of String;
    FPrefix:               Array [TSynCommentType] of String;
    FCommentIndent:        Array [TSynCommentType] of TSynBeautifierIndentType;

    FEolMatch:             Array [TSynCommentType] of String;
    FEolMode:              Array [TSynCommentType] of TSynCommentContineMode;
    FEolPostfix:           Array [TSynCommentType] of String;
    FEolSkipLongerLine:  Array [TSynCommentType] of Boolean;

    FExtendSlashCommentMode: TSynCommentExtendMode;

  private
    FPasHighlighter: TSynPasSyn;

    FCaretAtEOL: Boolean;
    FStringBreakAppend: String;
    FStringBreakEnabled: Boolean;
    FStringBreakPrefix: String;
    FWorkLine: Integer; // 1 based
    FWorkFoldType: TSynCommentType;
    FGetLineAfterComment: Boolean;
    FRegExprEngine: TRegExpr;

    FCacheFoldEndLvlIdx,         FCacheFoldEndLvl: Integer;
    FCacheCommentLvlIdx,         FCacheCommentLvl: Integer;
    FCacheFoldTypeForIdx: Integer;
                                 FCacheFoldType: TPascalCodeFoldBlockType;
    FCacheFirstLineForIdx,       FCacheFirstLine: Integer;
    FCacheSlashColumnForIdx,     FCacheSlashColumn: Integer;
    FCacheCommentStartColForIdx, FCacheCommentStartCol: Integer;
    FCacheLastHlTokenForIdx,     FCacheLastHlTokenCol: Integer;
    FCacheLastHlTokenKind:       TtkTokenKind;

    FCacheWorkFoldEndLvl: Integer;
    FCacheWorkCommentLvl: Integer;
    FCacheWorkFoldType: TPascalCodeFoldBlockType;
    FCacheWorkFirstLine: Integer;
    FCacheWorkSlashCol: Integer;
    FCacheWorkCommentStartCol: Integer;

  protected
    function  IsPasHighlighter: Boolean;
    procedure InitCache;
    procedure InitPasHighlighter;
    // Generic Helpers
    function  GetFoldEndLevelForIdx(AIndex: Integer): Integer;
    function  GetFoldCommentLevelForIdx(AIndex: Integer): Integer;
    function  GetFoldTypeAtEndOfLineForIdx(AIndex: Integer): TPascalCodeFoldBlockType;
    function  GetFirstCommentLineForIdx(AIndex: Integer): Integer; // Result is 1 based
    function  GetSlashStartColumnForIdx(AIndex: Integer): Integer;
    function  GetLastHlTokenForIdx(AIndex: Integer; out APos: Integer): TtkTokenKind;
    function  GetCommentStartColForIdx(AIndex: Integer): Integer; // Returns Column (logic) for GetFirstCommentLineForIdx(AIndex)
    // Values based on FWorkLine
    function  GetFoldEndLevel: Integer;
    function  GetFoldCommentLevel: Integer;
    function  GetFoldTypeAtEndOfLine: TPascalCodeFoldBlockType;
    function  GetFirstCommentLine: Integer; // Result is 1 based
    function  GetSlashStartColumn: Integer; // Acts on FWorkLine-1
    function  GetCommentStartCol: Integer;  // Acts on GetFirstCommentLineForIdx(FWorkLine) / Logical Resulc

    function  GetMatchStartColForIdx(AIndex: Integer): Integer; // Res=1-based
    function  GetMatchStartPos(AIndex: Integer = -1; AForceFirst: Boolean = False): TPoint; // Res=1-based / AIndex-1 is only used for sclMatchPrev

  protected
    function GetLine(AnIndex: Integer): String; override;

    procedure DoBeforeCommand(const ACaret: TSynEditCaret;
                              var Command: TSynEditorCommand); override;
    procedure DoAfterCommand(const ACaret: TSynEditCaret;
                             var Command: TSynEditorCommand;
                             StartLinePos, EndLinePos: Integer); override;
    procedure DoNewLineInString(AStringStartY, AStringStartX: Integer;
                                const ACaret: TSynEditCaret;
                                var Command: TSynEditorCommand;
                                StartLinePos, EndLinePos: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Src: TPersistent); override;
    // Retruns a 0-based position (even 0-based physical)
  published
    // *** coments with (* *)

    (* AnsiIndentFirstLineMax:
       * For comments that do NOT start at the BOL in their opening line "  Foo; ( * comment":
         if AnsiIndentFirstLineMax is is set:
         - If the comment starts before or at the "Max-Column, then sciAlignOpen is always used.
         - If the comment starts after max column, then the specified mode is used.
           If the specified mode is sciAlignOpen it will be cut at Max
       * For comments that start at BOL in their opening line "  ( * comment":
         This is ignored


       AnsiIndentFirstLineExtra:
         For comments that do NOT start at the BOL, an extra indent is added
         on the 2nd line (except if sciAlignOpen is used (in Flags, or via A.I.FirstLineMax))
    *)
    property AnsiIndentMode: TSynCommentIndentFlags read FIndentMode[sctAnsi]
                                                   write FIndentMode[sctAnsi];
    property AnsiIndentFirstLineMax:   Integer     read FIndentFirstLineMax[sctAnsi]
                                                   write FIndentFirstLineMax[sctAnsi];
    property AnsiIndentFirstLineExtra: String      read FIndentFirstLineExtra[sctAnsi]
                                                   write FIndentFirstLineExtra[sctAnsi];

    (* match should start with ^, and leading spaces should not be in ()  "^\s?(\*\*?\*?)"
       prefix can refer to $1 and should have spaces to indent after the match "$1 "
    *)
    property AnsiCommentMode: TSynCommentContineMode read FCommentMode[sctAnsi]
                                                     write FCommentMode[sctAnsi];
    property AnsiMatch:       String                 read FMatch[sctAnsi]
                                                     write FMatch[sctAnsi];
    property AnsiPrefix :     String                 read FPrefix[sctAnsi]
                              write FPrefix[sctAnsi];
    property AnsiMatchMode:   TSynCommentMatchMode   read FMatchMode[sctAnsi]
                                                     write FMatchMode[sctAnsi];
    property AnsiMatchLine:   TSynCommentMatchLine   read FMatchLine[sctAnsi]
                                                     write FMatchLine[sctAnsi];
    property AnsiCommentIndent: TSynBeautifierIndentType read FCommentIndent[sctAnsi]
                                                         write FCommentIndent[sctAnsi];

    // Add postfix at EOL
    property AnsiEolMode: TSynCommentContineMode read FEolMode[sctAnsi]
                                                 write FEolMode[sctAnsi];
    property AnsiEolPostfix: String              read FEolPostfix[sctAnsi]
                                                 write FEolPostfix[sctAnsi];
    property AnsiEolMatch:   String              read FEolMatch[sctAnsi]
                                                 write FEolMatch[sctAnsi];
    property AnsiEolSkipLongerLine: Boolean      read FEolSkipLongerLine[sctAnsi]
                                                 write FEolSkipLongerLine[sctAnsi];

    // *** coments with { }

    property BorIndentMode: TSynCommentIndentFlags read FIndentMode[sctBor]
                                                  write FIndentMode[sctBor];
    property BorIndentFirstLineMax:   Integer     read FIndentFirstLineMax[sctBor]
                                                  write FIndentFirstLineMax[sctBor];
    property BorIndentFirstLineExtra: String      read FIndentFirstLineExtra[sctBor]
                                                  write FIndentFirstLineExtra[sctBor];

    property BorCommentMode: TSynCommentContineMode read FCommentMode[sctBor]
                                                    write FCommentMode[sctBor];
    property BorMatch:       String                 read FMatch[sctBor]
                                                    write FMatch[sctBor];
    property BorPrefix :     String                 read FPrefix[sctBor]
                             write FPrefix[sctBor];
    property BorMatchMode:   TSynCommentMatchMode   read FMatchMode[sctBor]
                                                    write FMatchMode[sctBor];
    property BorMatchLine:   TSynCommentMatchLine   read FMatchLine[sctBor]
                                                    write FMatchLine[sctBor];
    property BorCommentIndent: TSynBeautifierIndentType read FCommentIndent[sctBor]
                                                         write FCommentIndent[sctBor];

    property BorEolMode: TSynCommentContineMode read FEolMode[sctBor]
                                                write FEolMode[sctBor];
    property BorEolPostfix : String             read FEolPostfix[sctBor]
                             write FEolPostfix[sctBor];
    property BorEolMatch:    String             read FEolMatch[sctBor]
                                                write FEolMatch[sctBor];
    property BorEolSkipLongerLine: Boolean      read FEolSkipLongerLine[sctBor]
                                                write FEolSkipLongerLine[sctBor];

    // *** coments with //
    // Continue only, if Extended

    property ExtendSlashCommentMode: TSynCommentExtendMode read FExtendSlashCommentMode
                                                           write FExtendSlashCommentMode;

    property SlashIndentMode: TSynCommentIndentFlags read FIndentMode[sctSlash]
                                                    write FIndentMode[sctSlash];
    property SlashIndentFirstLineMax:   Integer     read FIndentFirstLineMax[sctSlash]
                                                    write FIndentFirstLineMax[sctSlash];
    property SlashIndentFirstLineExtra: String      read FIndentFirstLineExtra[sctSlash]
                                                    write FIndentFirstLineExtra[sctSlash];

    property SlashCommentMode: TSynCommentContineMode read FCommentMode[sctSlash]
                                                      write FCommentMode[sctSlash];
    property SlashMatch:     String                   read FMatch[sctSlash]
                                                      write FMatch[sctSlash];
    property SlashPrefix :   String                   read FPrefix[sctSlash]
                             write FPrefix[sctSlash];
    property SlashMatchMode: TSynCommentMatchMode     read FMatchMode[sctSlash]
                                                      write FMatchMode[sctSlash];
    property SlashMatchLine:   TSynCommentMatchLine   read FMatchLine[sctSlash]
                                                      write FMatchLine[sctSlash];
    property SlashCommentIndent: TSynBeautifierIndentType read FCommentIndent[sctSlash]
                                                         write FCommentIndent[sctSlash];

    property SlashEolMode: TSynCommentContineMode read FEolMode[sctSlash]
                                                  write FEolMode[sctSlash];
    property SlashEolPostfix : String             read FEolPostfix[sctSlash]
                               write FEolPostfix[sctSlash];
    property SlashEolMatch:    String             read FEolMatch[sctSlash]
                                                  write FEolMatch[sctSlash];
    property SlashEolSkipLongerLine: Boolean      read FEolSkipLongerLine[sctSlash]
                                                  write FEolSkipLongerLine[sctSlash];
    // String
    property StringBreakEnabled: Boolean read FStringBreakEnabled write FStringBreakEnabled;
    property StringBreakAppend: String read FStringBreakAppend write FStringBreakAppend;
    property StringBreakPrefix: String read FStringBreakPrefix write FStringBreakPrefix;
  end;

function dbgs(ACommentType: TSynCommentType): String; overload;
function dbgs(AContinueMode: TSynCommentContineMode): String; overload;
function dbgs(AMatchLine: TSynCommentMatchLine): String; overload;
function dbgs(AMatchMode: TSynCommentMatchMode): String; overload;
function dbgs(AIndentFlag: TSynCommentIndentFlag): String; overload;
function dbgs(AIndentFlags: TSynCommentIndentFlags): String; overload;
function dbgs(AExtendMode: TSynCommentExtendMode): String; overload;

implementation
uses SynEdit;

function dbgs(ACommentType: TSynCommentType): String;
begin
  Result := ''; WriteStr(Result, ACommentType);
end;

function dbgs(AContinueMode: TSynCommentContineMode): String;
begin
  Result := ''; WriteStr(Result, AContinueMode);
end;

function dbgs(AMatchLine: TSynCommentMatchLine): String;
begin
  Result := ''; WriteStr(Result, AMatchLine);
end;

function dbgs(AMatchMode: TSynCommentMatchMode): String;
begin
  Result := ''; WriteStr(Result, AMatchMode);
end;

function dbgs(AIndentFlag: TSynCommentIndentFlag): String;
begin
  Result := ''; WriteStr(Result, AIndentFlag);
end;

function dbgs(AIndentFlags: TSynCommentIndentFlags): String;
var
  i: TSynCommentIndentFlag;
begin
  Result := '';
  for i := low(TSynCommentIndentFlag) to high(TSynCommentIndentFlag) do
    if i in AIndentFlags then
      if Result = ''
      then Result := dbgs(i)
      else Result := Result + ',' + dbgs(i);
  if Result <> '' then
    Result := '[' + Result + ']';
end;

function dbgs(AExtendMode: TSynCommentExtendMode): String;
begin
  Result := ''; WriteStr(Result, AExtendMode);
end;


{ TSynBeautifierPascal }

function TSynBeautifierPascal.IsPasHighlighter: Boolean;
begin
  Result := (TSynEdit(FCurrentEditor).Highlighter <> nil) and
            (TSynEdit(FCurrentEditor).Highlighter is TSynPasSyn);
end;

procedure TSynBeautifierPascal.InitCache;
begin
  FCacheFoldEndLvlIdx         := -1;
  FCacheCommentLvlIdx         := -1;
  FCacheFoldTypeForIdx        := -1;
  FCacheFirstLineForIdx       := -1;
  FCacheSlashColumnForIdx     := -1;
  FCacheCommentStartColForIdx := -1;

  FCacheWorkFoldEndLvl         := -2;
  FCacheWorkCommentLvl         := -2;
  FCacheWorkFoldType           := cfbtIfThen; // dummy for not yet cached
  FCacheWorkFirstLine          := -2;
  FCacheWorkSlashCol           := -2;
  FCacheWorkCommentStartCol := -2;

end;

procedure TSynBeautifierPascal.InitPasHighlighter;
begin
  FPasHighlighter := TSynPasSyn(TSynEdit(FCurrentEditor).Highlighter);
  FPasHighlighter.CurrentLines := FCurrentLines;
  FPasHighlighter.ScanRanges;
end;

function TSynBeautifierPascal.GetFoldEndLevelForIdx(AIndex: Integer): Integer;
begin
  Result := FCacheFoldEndLvl;
  if AIndex = FCacheFoldEndLvlIdx then
    exit;
  FCacheFoldEndLvlIdx := AIndex;
  FCacheFoldEndLvl := FPasHighlighter.FoldBlockEndLevel(AIndex, FOLDGROUP_PASCAL, [sfbIncludeDisabled]);
  Result := FCacheFoldEndLvl;
end;

function TSynBeautifierPascal.GetFoldCommentLevelForIdx(AIndex: Integer): Integer;
var
  tmp: Pointer;
  Block: TPascalCodeFoldBlockType;
begin
  Result := FCacheCommentLvl;
  if AIndex = FCacheCommentLvlIdx then
    exit;
  FCacheCommentLvlIdx := AIndex;

  FCacheCommentLvl := GetFoldEndLevelForIdx(AIndex) - 1;
  while (FCacheCommentLvl > 0) do begin
    FPasHighlighter.FoldBlockNestedTypes(AIndex , FCacheCommentLvl - 1,
                 tmp, FOLDGROUP_PASCAL, [sfbIncludeDisabled]);
    Block:=TPascalCodeFoldBlockType(PtrUInt(tmp));
    if not (Block in [cfbtAnsiComment, cfbtBorCommand, cfbtSlashComment]) then
      break;
    dec(FCacheCommentLvl);
  end;
  inc(FCacheCommentLvl);
  Result := FCacheCommentLvl;
end;

function TSynBeautifierPascal.GetFoldTypeAtEndOfLineForIdx(AIndex: Integer): TPascalCodeFoldBlockType;
var
  EndLevel: Integer;
  tmp: Pointer;
begin
  // TODO cfbtNestedComment
  Result := FCacheFoldType;
  if AIndex = FCacheFoldTypeForIdx then
    exit;

  FCacheFoldTypeForIdx := AIndex;
  EndLevel := GetFoldEndLevelForIdx(AIndex);
  if (EndLevel > 0) then
  begin
    if FPasHighlighter.FoldBlockNestedTypes(AIndex, EndLevel - 1,
       tmp, FOLDGROUP_PASCAL, [sfbIncludeDisabled])
    then
      FCacheFoldType:=TPascalCodeFoldBlockType(PtrUInt(tmp))
    else
      FCacheFoldType := cfbtNone;
  end;

  while (FCacheFoldType = cfbtNestedComment) and (EndLevel > 1) do begin
    dec(EndLevel);
    if FPasHighlighter.FoldBlockNestedTypes(AIndex, EndLevel - 1,
         tmp, FOLDGROUP_PASCAL, [sfbIncludeDisabled])
    then
      FCacheFoldType:=TPascalCodeFoldBlockType(PtrUInt(tmp))
    else
      FCacheFoldType := cfbtNone;
  end;

  Result := FCacheFoldType;
end;

function TSynBeautifierPascal.GetFirstCommentLineForIdx(AIndex: Integer): Integer;
var
  FoldType: TPascalCodeFoldBlockType;
  ANewIndex, EndLvl, CommentLvl: Integer;
begin
  Result := FCacheFirstLine;
  if AIndex = FCacheFirstLineForIdx then
    exit;

  ANewIndex := AIndex;
  FoldType := GetFoldTypeAtEndOfLineForIdx(ANewIndex);
  EndLvl   := GetFoldEndLevelForIdx(ANewIndex);

  //if (EndLvl = 0) or not(FoldType in [cfbtAnsiComment, cfbtBorCommand, cfbtSlashComment]) and
  //   (AIndex > 0) and (GetSlashStartColumnForIdx(AIndex - 1) > 0)
  //then begin
  //  dec(ANewIndex);
  //  FoldType := GetFoldTypeAtEndOfLineForIdx(ANewIndex);
  //  EndLvl   := GetFoldEndLevelForIdx(ANewIndex);
  //end;

  if (EndLvl = 0) or not(FoldType in [cfbtAnsiComment, cfbtBorCommand, cfbtSlashComment])
  then begin
    Result := ToPos(AIndex) - 1; // 1 based - the line above ANewIndex
    // maybe the line above has a trailing comment
    //if (AIndex <> ANewIndex) and (ANewIndex > 0) and (GetSlashStartColumnForIdx(ANewIndex-1) > 0) then
    //  Result := ToPos(ANewIndex) - 1;
    exit;
  end;

  FCacheFirstLineForIdx := AIndex;
  FCacheFirstLine       := ToPos(ANewIndex) - 1;
  CommentLvl            := GetFoldCommentLevelForIdx(ANewIndex);

  while (FCacheFirstLine > 0) do begin
    if CommentLvl > FPasHighlighter.FoldBlockMinLevel(FCacheFirstLine-1, FOLDGROUP_PASCAL, [sfbIncludeDisabled]) then
      break;
    dec(FCacheFirstLine);
  end;

  if FoldType = cfbtSlashComment then begin
    // maybe the line above has a trailing comment
    if GetSlashStartColumnForIdx(ToIdx(FCacheFirstLine)) > 0 then
      dec(FCacheFirstLine);
  end;

  Result := FCacheFirstLine;
//debugln(['FIRST LINE ', FCacheFirstLine]);
end;

function TSynBeautifierPascal.GetSlashStartColumnForIdx(AIndex: Integer): Integer;
var
  Tk: TtkTokenKind;
begin
  Result := FCacheSlashColumn;
  if AIndex = FCacheSlashColumnForIdx then
    exit;

  FCacheSlashColumnForIdx := AIndex;
  Tk := GetLastHlTokenForIdx(AIndex, FCacheSlashColumn);
  if Tk <> SynHighlighterPas.tkComment then
    FCacheSlashColumn := -1;
  Result := FCacheSlashColumn;
end;

function TSynBeautifierPascal.GetLastHlTokenForIdx(AIndex: Integer; out
  APos: Integer): TtkTokenKind;
begin
  Result := FCacheLastHlTokenKind;
  APos   := FCacheLastHlTokenCol;
  if AIndex = FCacheLastHlTokenForIdx then
    exit;

  FCacheSlashColumnForIdx := AIndex;
  FCacheLastHlTokenKind   := SynHighlighterPas.tkUnknown;
  FCacheLastHlTokenCol    := -1;
  if (FCurrentLines[AIndex] <> '') then begin
    FPasHighlighter.StartAtLineIndex(AIndex);
    while not FPasHighlighter.GetEol do begin
      FCacheLastHlTokenKind := TtkTokenKind(FPasHighlighter.GetTokenKind);
      FCacheLastHlTokenCol := FPasHighlighter.GetTokenPos + 1;
      FPasHighlighter.Next;
    end;
  end;
  Result := FCacheLastHlTokenKind;
  APos   := FCacheLastHlTokenCol;
end;

function TSynBeautifierPascal.GetCommentStartColForIdx(AIndex: Integer): Integer;
var
  nl: TLazSynFoldNodeInfoList;
  i: Integer;
  FoldCommentLvl: Integer;
begin
  Result := FCacheCommentStartCol;
  if AIndex = FCacheCommentStartColForIdx then
    exit;
  FCacheCommentStartColForIdx := AIndex;

  if (GetFoldEndLevelForIdx(AIndex) = 0) or
     not(GetFoldTypeAtEndOfLineForIdx(AIndex) in [cfbtAnsiComment, cfbtBorCommand, cfbtSlashComment])
  then begin
    // must be SlashComment
    FCacheCommentStartCol := GetSlashStartColumnForIdx(AIndex - 1);
    //FCacheCommentStartCol := GetSlashStartColumnForIdx(AIndex);
    //if FCacheCommentStartCol > 0 then begin
    //  i := GetSlashStartColumnForIdx(AIndex - 1);
    //  if i > 0 then
    //    FCacheCommentStartCol := i;
    //end;
    Result := FCacheCommentStartCol;
//debugln(['FIRST COL prev-// ', FCacheCommentStartCol]);
    exit;
  end;

  FCacheCommentStartCol := -1;
  FoldCommentLvl := GetFoldCommentLevelForIdx(AIndex);
  nl := FPasHighlighter.FoldNodeInfo[GetFirstCommentLineForIdx(AIndex) - 1];
  nl.AddReference;
  nl.ClearFilter;
  nl.ActionFilter := [sfaOpen];
  nl.GroupFilter  := FOLDGROUP_PASCAL;
  i := nl.Count - 1;
  while i >= 0 do  begin
    if (not (sfaInvalid in nl[i].FoldAction)) and
       (nl[i].NestLvlEnd = FoldCommentLvl)
    then begin
      FCacheCommentStartCol := nl[i].LogXStart + 1;  // Highlighter pos are 0 based
      break;
    end;
    dec(i);
  end;
  nl.ReleaseReference;
//debugln(['FIRST COL ', FCacheCommentStartCol]);

  Result := FCacheCommentStartCol;
end;

function TSynBeautifierPascal.GetFoldEndLevel: Integer;
begin
  Result := FCacheWorkFoldEndLvl;
  if FCacheWorkFoldEndLvl > -2 then
    exit;
  FCacheWorkFoldEndLvl := GetFoldEndLevelForIdx(FWorkLine-1);
  Result := FCacheWorkFoldEndLvl;
end;

function TSynBeautifierPascal.GetFoldCommentLevel: Integer;
begin
  Result := FCacheWorkCommentLvl;
  if FCacheWorkCommentLvl > -2 then
    exit;
  FCacheWorkCommentLvl := GetFoldCommentLevelForIdx(FWorkLine-1);
  Result := FCacheWorkCommentLvl;
end;

function TSynBeautifierPascal.GetFoldTypeAtEndOfLine: TPascalCodeFoldBlockType;
begin
  Result := FCacheWorkFoldType;
  if FCacheWorkFoldType <> cfbtIfThen then
    exit;
  FCacheWorkFoldType := GetFoldTypeAtEndOfLineForIdx(FWorkLine-1);
  Result := FCacheWorkFoldType;
end;

function TSynBeautifierPascal.GetFirstCommentLine: Integer;
begin
  Result := FCacheWorkFirstLine;
  if FCacheWorkFirstLine > -2 then
    exit;
  FCacheWorkFirstLine := GetFirstCommentLineForIdx(FWorkLine-1);
  Result := FCacheWorkFirstLine;
end;

function TSynBeautifierPascal.GetSlashStartColumn: Integer;
begin
  Result := FCacheWorkSlashCol;
  if FCacheWorkSlashCol > -2 then
    exit;
  FCacheWorkSlashCol := GetSlashStartColumnForIdx(FWorkLine-2);
  Result := FCacheWorkSlashCol;
end;

function TSynBeautifierPascal.GetCommentStartCol: Integer;
begin
  Result := FCacheWorkCommentStartCol;
  if FCacheWorkCommentStartCol > -2 then
    exit;
  FCacheWorkCommentStartCol := GetCommentStartColForIdx(FWorkLine-1);
  Result := FCacheWorkCommentStartCol;
end;

function TSynBeautifierPascal.GetMatchStartColForIdx(AIndex: Integer): Integer;
begin
  if ToPos(AIndex) = GetFirstCommentLine then begin
    // Match on FirstLine
    case FMatchMode[FWorkFoldType] of
      scmMatchAfterOpening: begin
          if FWorkFoldType = sctBor
          then Result := GetCommentStartCol + 1
          else Result := GetCommentStartCol + 2;
        end;
      scmMatchOpening:    Result := GetCommentStartCol;
      scmMatchWholeLine:  Result := 1;
      scmMatchAtAsterisk: Result := GetCommentStartCol + 1;
    end;
  end
  else begin
    // Match on prev, not first
    case FMatchMode[FWorkFoldType] of
      scmMatchAfterOpening, scmMatchOpening, scmMatchAtAsterisk:
          Result := 1 + GetIndentForLine(nil, FCurrentLines[AIndex], False);
      scmMatchWholeLine:
          Result := 1;
    end;
  end;
end;

function TSynBeautifierPascal.GetMatchStartPos(AIndex: Integer; AForceFirst: Boolean): TPoint;
begin
  if AIndex < 0 then
    AIndex := ToIdx(FWorkLine);

  if AForceFirst then
    Result.Y := GetFirstCommentLine
  else
    case FMatchLine[FWorkFoldType] of
      sclMatchFirst: Result.Y := GetFirstCommentLine;
      sclMatchPrev:  Result.Y := ToPos(AIndex)-1; // FWorkLine - 1
    end;

  Result.X := GetMatchStartColForIdx(ToIdx(Result.Y));
end;


function TSynBeautifierPascal.GetLine(AnIndex: Integer): String;
var
  ReplacedPrefix: String;
  Indent, PreFixPos: Integer;
  r: Boolean;
  p: TPoint;
begin
  if not FGetLineAfterComment then begin
    Result := inherited GetLine(AnIndex);
    exit;
  end;

  // Need to cut of existing Prefix to find indent after prefix

  if AnIndex < GetFirstCommentLine-1 then begin
    Result := '';
//debugln(['GETLINE FROM (< First) ', AnIndex,' ''', FCurrentLines[AnIndex], ''' to empty']);
  end;

  // 1) Run the match for this line (match prev, or first)
  //    and see if we can find the replaced prefix in this line
  if AnIndex > GetFirstCommentLine-1 then begin
    p := GetMatchStartPos(AnIndex);
    FRegExprEngine.InputString:= copy(FCurrentLines[ToIdx(p.y)], p.x, MaxInt);
    FRegExprEngine.Expression := FMatch[FWorkFoldType];
    if FRegExprEngine.ExecPos(1) then begin
      ReplacedPrefix := FRegExprEngine.Substitute(FPrefix[FWorkFoldType]);
      while (ReplacedPrefix <> '') and (ReplacedPrefix[1] in [#9, ' ']) do
        delete(ReplacedPrefix, 1, 1);
      while (ReplacedPrefix <> '') and (ReplacedPrefix[length(ReplacedPrefix)] in [#9, ' ']) do
        delete(ReplacedPrefix, length(ReplacedPrefix), 1);
      if ReplacedPrefix <> '' then begin
        Result := FCurrentLines[AnIndex];
        PreFixPos := pos(ReplacedPrefix, Result);
        if (PreFixPos > 0) and (PreFixPos <= GetMatchStartColForIdx(AnIndex)) then begin
          delete(Result, 1, PreFixPos - 1 + Length(ReplacedPrefix));
//debugln(['GETLINE FROM (1) ', AnIndex,' ''', FCurrentLines[AnIndex], ''' to ''', Result, '''']);
          exit;
        end;
      end;
    end;
  end;

  // 2) See, if the current-1 line can be matched
  r := False;
  Result := FCurrentLines[AnIndex];
  Indent := GetMatchStartColForIdx(AnIndex);
  FRegExprEngine.InputString:= copy(FCurrentLines[AnIndex], Indent, MaxInt);
  FRegExprEngine.Expression := FMatch[FWorkFoldType];
  r := FRegExprEngine.ExecPos(1);
  if (not r) and (Indent > 1) and
     ((p.y <> GetFirstCommentLine) or (FMatchMode[FWorkFoldType] = scmMatchWholeLine))
  then begin
    // try whole line
// TODO: only if not first, or if setting
    FRegExprEngine.InputString := FCurrentLines[AnIndex];
    r := FRegExprEngine.ExecPos(1);
    if r then
      Indent := 1;
  end;
  if (r) then begin
    Indent := Indent + FRegExprEngine.MatchPos[0] - 1 + FRegExprEngine.MatchLen[0];
    Result := FCurrentLines[AnIndex];
    if (Indent <= Length(Result)) then
      while (Indent > 1) and (Result[Indent] in [#9, ' ']) do
        dec(Indent);
    inc(Indent);
    if Indent > 0 then begin
      Result := copy(Result, Indent, MaxInt);
//debugln(['GETLINE FROM (2) ', AnIndex,' ''', FCurrentLines[AnIndex], ''' to ''', Result, '''']);
      exit;
    end;
  end;

  // 3) maybe try currest replace, if different from 1?

  // Nothing found
  Result := '';
//debugln(['GETLINE FROM (X) ', AnIndex,' ''', FCurrentLines[AnIndex], ''' to empty']);
end;

procedure TSynBeautifierPascal.DoBeforeCommand(const ACaret: TSynEditCaret;
  var Command: TSynEditorCommand);
begin
  FCaretAtEOL := ACaret.BytePos > Length(FCurrentLines[ToIdx(ACaret.LinePos)]);
  FGetLineAfterComment := False;
  inherited DoBeforeCommand(ACaret, Command);
end;

procedure TSynBeautifierPascal.DoAfterCommand(const ACaret: TSynEditCaret;
  var Command: TSynEditorCommand; StartLinePos, EndLinePos: Integer);

var
  WorkLine, PrevLineShlasCol: Integer;
  ReplacedPrefix: String; // Each run matches only one Type
  MatchResultIntern, MatchedBOLIntern: Array [Boolean] of Boolean;  // Each run matches only one Type

  function CheckMatch(AType: TSynCommentType; AFailOnNoPattern: Boolean = False;
    AForceFirst: Boolean = False): Boolean;
  var
    p: TPoint;
  begin
    if (FMatch[AType] = '') and AFailOnNoPattern then begin
      Result := False;
      exit;
    end;

    if MatchedBOLIntern[AForceFirst] then begin
      Result := MatchResultIntern[AForceFirst];
      exit;
    end;

    p := GetMatchStartPos(-1, AForceFirst);

    FRegExprEngine.InputString:= copy(FCurrentLines[ToIdx(p.y)], p.x, MaxInt);
    FRegExprEngine.Expression := FMatch[AType];
    if not FRegExprEngine.ExecPos(1) then begin
      ReplacedPrefix := FRegExprEngine.Substitute(FPrefix[AType]);
      MatchedBOLIntern[AForceFirst] := True;
      MatchResultIntern[AForceFirst] := False;
      Result := MatchResultIntern[AForceFirst];
      exit;
    end;

    ReplacedPrefix := FRegExprEngine.Substitute(FPrefix[AType]);
    MatchedBOLIntern[AForceFirst] := True;
    MatchResultIntern[AForceFirst] := True;
    Result := MatchResultIntern[AForceFirst];

  end;

  function IsFoldTypeEnabled(AType: TSynCommentType): Boolean;
  begin
  Result := (  ( (AType <> sctSlash) or
               ( ((not FCaretAtEOL) and (FExtendSlashCommentMode <> sceNever)) or
                 ((FCaretAtEOL)     and not(FExtendSlashCommentMode in [sceNever, sceSplitLine, sceMatchingSplitLine]))
               )
             ) and
             ( (FIndentMode[AType] <> []) or
               (FCommentMode[AType] <> sccNoPrefix) or
               (FEolMode[AType] <> sccNoPrefix)
            ))
  end;

var
  Indent, dummy: Integer;
  s: String;
  FoldTyp: TSynCommentType;
  AnyEnabled: Boolean;
  ExtendSlash, BeSmart, Matching: Boolean;
  PreviousIsFirst, IsAtBOL, DidAlignOpen: Boolean;

  IndentTypeBackup: TSynBeautifierIndentType;

begin
  if (EndLinePos < 1) or
     ((Command <> ecLineBreak) and (Command <> ecInsertLine)) or
     (not IsPasHighlighter)
  then begin
    inherited DoAfterCommand(ACaret, Command, StartLinePos, EndLinePos);
    exit;
  end;

  AnyEnabled := False;
  for FoldTyp := low(TSynCommentType) to high(TSynCommentType) do
    AnyEnabled := AnyEnabled or IsFoldTypeEnabled(FoldTyp);
  if (not AnyEnabled) and (not FStringBreakEnabled) then begin
    inherited DoAfterCommand(ACaret, Command, StartLinePos, EndLinePos);
    exit;
  end;

  InitCache;
  InitPasHighlighter;
  FGetLineAfterComment := False;
  MatchedBOLIntern[True] := False;
  MatchedBOLIntern[False] := False;
  PrevLineShlasCol := -1;
  dummy := 0;

  if (Command = ecLineBreak)
  then WorkLine := ACaret.LinePos
  else WorkLine := ACaret.LinePos + 1;
  FWorkLine := WorkLine;


  // Find Foldtype
  case GetFoldTypeAtEndOfLine of
    cfbtAnsiComment:  FoldTyp := sctAnsi;
    cfbtBorCommand:   FoldTyp := sctBor;
    cfbtSlashComment: FoldTyp := sctSlash;
    else
      begin
        if (FCurrentLines[ToIdx(WorkLine)-1] <> '') and
           (FExtendSlashCommentMode <> sceNever) and
           ( (not FCaretAtEOL) or not(FExtendSlashCommentMode in [sceSplitLine, sceMatchingSplitLine]) )
        then begin
          PrevLineShlasCol := GetSlashStartColumn;
          if PrevLineShlasCol > 0 then
            FoldTyp := sctSlash;
        end;
        if PrevLineShlasCol < 0 then begin
          if (FCurrentLines[ToIdx(WorkLine)-1] <> '') and
             (GetLastHlTokenForIdx(ToIdx(WorkLine)-1, dummy) = SynHighlighterPas.tkString)
          then
            DoNewLineInString(WorkLine - 1, dummy, ACaret, Command, StartLinePos, EndLinePos)
          else
            inherited DoAfterCommand(ACaret, Command, StartLinePos, EndLinePos);
          exit;
        end;
      end;
  end;

  if not IsFoldTypeEnabled(FoldTyp) then begin
    inherited DoAfterCommand(ACaret, Command, StartLinePos, EndLinePos);
    exit;
  end;

  FWorkFoldType := FoldTyp;

  // Check if we need extend
  ExtendSlash := False;
  if (FoldTyp = sctSlash) and (ACaret.OldLineBytePos.x > GetSlashStartColumn+2) then begin
    // Check if extension is needed
    case FExtendSlashCommentMode of
      sceAlways:    ExtendSlash := True;
      sceSplitLine: ExtendSlash := not FCaretAtEOL;
      sceMatching:  ExtendSlash := CheckMatch(FoldTyp);
      sceMatchingSplitLine: ExtendSlash := CheckMatch(FoldTyp) and (not FCaretAtEOL);
    end;

    if not ExtendSlash then begin
      inherited DoAfterCommand(ACaret, Command, StartLinePos, EndLinePos);
      exit;
    end;
  end;

  // Indent
  Matching := CheckMatch(FoldTyp);
  PreviousIsFirst := (GetFirstCommentLine = WorkLine -1);
  DidAlignOpen := False;
  IsAtBOL := True;
  if PreviousIsFirst then
    IsAtBOL := GetCommentStartCol = 1 + GetIndentForLine(nil, FCurrentLines[ToIdx(GetFirstCommentLine)], False);


  // Aply indent before prefix
  if Matching or (FCommentMode[FoldTyp] = sccPrefixAlways) or (sciApplyIndentForNoMatch in FIndentMode[FoldTyp])
  then begin
    IndentTypeBackup := IndentType;
    try
      if IndentType = sbitPositionCaret then
        IndentType := sbitSpace;
      if IndentType = sbitConvertToTabOnly then
        IndentType := sbitConvertToTabSpace;

      if PreviousIsFirst and not IsAtBOL and
         (FIndentFirstLineMax[FoldTyp] > 0) and
         ( (FIndentFirstLineMax[FoldTyp] + 1 >= GetCommentStartCol) or
           (FIndentMode[FoldTyp] * [sciNone, sciAlignOpen] = [sciAlignOpen])
         )
      then begin
        // Use sciAlignOpen
        Indent := Min(
          FCurrentLines.LogicalToPhysicalCol(FCurrentLines[ToIdx(GetFirstCommentLine)], ToIdx(GetFirstCommentLine), GetCommentStartCol-1),
          FIndentFirstLineMax[FoldTyp]);
        s := GetCharMix(WorkLine, Indent, dummy);
        FLogicalIndentLen := length(s);
        FCurrentLines.EditInsert(1, WorkLine, s);
        DidAlignOpen := True;
      end
      else
      if (FIndentMode[FoldTyp] * [sciNone, sciAlignOpen] = [sciAlignOpen]) and
         (GetCommentStartCol > 0)
      then begin
        Indent := FCurrentLines.LogicalToPhysicalCol(FCurrentLines[ToIdx(GetFirstCommentLine)], ToIdx(GetFirstCommentLine), GetCommentStartCol-1);
        if FIndentFirstLineMax[FoldTyp] > 0
        then Indent := Min(Indent, FIndentFirstLineMax[FoldTyp]);
        s := GetCharMix(WorkLine, Indent, dummy);
        FLogicalIndentLen := length(s);
        FCurrentLines.EditInsert(1, WorkLine, s);
        DidAlignOpen := True;
      end
      else
      if (sciNone in FIndentMode[FoldTyp]) then begin
        // No indent
      end
      else
      begin
        inherited DoAfterCommand(ACaret, Command, StartLinePos, EndLinePos);
      end;
    finally
      IndentType := IndentTypeBackup;
    end;

    // AnsiIndentFirstLineExtra
    if PreviousIsFirst and (not IsAtBOL) and (not DidAlignOpen) then begin
      FCurrentLines.EditInsert(1 + FLogicalIndentLen, WorkLine, FIndentFirstLineExtra[FoldTyp]);
      FLogicalIndentLen := FLogicalIndentLen + length(FIndentFirstLineExtra[FoldTyp]);
    end;

  end
  else
    inherited DoAfterCommand(ACaret, Command, StartLinePos, EndLinePos);

  Indent := 0; // Extra Indent
  BeSmart := (PreviousIsFirst or (sciAlignOpen in FIndentMode[FoldTyp])) and
             (Matching or ExtendSlash or (sciApplyIndentForNoMatch in FIndentMode[FoldTyp]) or
              (FCommentMode[FoldTyp] = sccPrefixAlways)  );

  // sciAddTokenLen -- Spaces for (* or {
  if BeSmart and
     ( (sciAddTokenLen in FIndentMode[FoldTyp]) and
       ( (not(sciMatchOnlyTokenLen in FIndentMode[FoldTyp])) or CheckMatch(FoldTyp, False, True) ) and
       ( (not(sciAlignOnlyTokenLen in FIndentMode[FoldTyp])) or DidAlignOpen )
     )
  then begin
    case FoldTyp of
      sctAnsi:  if FMatchMode[FoldTyp] = scmMatchAtAsterisk
                then Indent := 1
                else Indent := 2;
      sctBor:   Indent := 1;
      sctSlash: if ExtendSlash
                then Indent := 0 // do the slashes
                else Indent := 2;
    end;
  end;

  // sciAddPastTokenIndent -- Spaces from after (* or { (to go befare prefix e.g " {  * foo")
  if BeSmart and
     ( (sciAddPastTokenIndent in FIndentMode[FoldTyp]) and
       ( (not(sciMatchOnlyPastTokenIndent in FIndentMode[FoldTyp])) or CheckMatch(FoldTyp, False, True) ) and
       ( (not(sciAlignOnlyPastTokenIndent in FIndentMode[FoldTyp])) or DidAlignOpen )
     ) and
     (GetCommentStartCol > 0) // foundStartCol
  then begin
    case FoldTyp of
      // ignores scmMatchAtAsterisk
      sctAnsi:  s := copy(FCurrentLines[ToIdx(GetFirstCommentLine)], GetCommentStartCol+2, MaxInt);
      sctBor:   s := copy(FCurrentLines[ToIdx(GetFirstCommentLine)], GetCommentStartCol+1, MaxInt);
      sctSlash: s := copy(FCurrentLines[ToIdx(GetFirstCommentLine)], GetCommentStartCol+2, MaxInt);
    end;
    Indent := Indent + GetIndentForLine(nil, s, False);
  end;
  // Extend //
  if ExtendSlash then begin
    FCurrentLines.EditInsert(1 + FLogicalIndentLen, WorkLine, '//');
    FLogicalIndentLen := FLogicalIndentLen + 2;
  end;
  if (Indent > 0) then begin
    FCurrentLines.EditInsert(1 + FLogicalIndentLen, WorkLine, StringOfChar(' ', Indent ));
    FLogicalIndentLen := FLogicalIndentLen + Indent;
  end;

  // Apply prefix
  if (FCommentMode[FoldTyp] = sccPrefixAlways) or
     ((FCommentMode[FoldTyp] = sccPrefixMatch) and Matching)
  then begin
    FCurrentLines.EditInsert(1 + FLogicalIndentLen, WorkLine, ReplacedPrefix);
    FLogicalIndentLen := FLogicalIndentLen + length(ReplacedPrefix);

    // Post  prefix indent
    FGetLineAfterComment := True;
    try
      GetIndentInfo(WorkLine, s, Indent, GetFirstCommentLine);
      if s <> '' then begin
        FCurrentLines.EditInsert(1 + FLogicalIndentLen, WorkLine, s);
        FLogicalIndentLen := FLogicalIndentLen + length(s); // logical (Indent is phisical)
      end
      else
        FLogicalIndentLen := FLogicalIndentLen + Indent; // maybe position caret
    finally
      FGetLineAfterComment := False;
    end;
  end;


  if (Command = ecLineBreak) then begin
    ACaret.IncForcePastEOL;
    ACaret.BytePos := 1 + FLogicalIndentLen;
    ACaret.DecForcePastEOL;
  end;

end;

procedure TSynBeautifierPascal.DoNewLineInString(AStringStartY, AStringStartX: Integer;
  const ACaret: TSynEditCaret; var Command: TSynEditorCommand; StartLinePos,
  EndLinePos: Integer);
var
  s: String;
  WorkLine: Integer;
  f: Boolean;
begin
  inherited DoAfterCommand(ACaret, Command, StartLinePos, EndLinePos);
  if not FStringBreakEnabled then
    exit;

  if (Command = ecLineBreak)
  then WorkLine := ACaret.LinePos - 1
  else WorkLine := ACaret.LinePos;

  s := FCurrentLines[ToIdx(WorkLine)];
  if (AStringStartX < 1) or (AStringStartX > Length(s)) or (s[AStringStartX] <> '''') then
    exit;
  f := False;
  while AStringStartX <= length(s) do begin
    if (s[AStringStartX] = '''') then f := not f;
    inc(AStringStartX);
  end;
  if not f then exit;

  ACaret.IncForcePastEOL;

  FCurrentLines.EditInsert(1 + length(s), WorkLine, '''' + FStringBreakAppend);
  //if Command = ecInsertLine then
  //  ACaret.BytePos := ACaret.BytePos + Length(FStringBreakAppend) + 1;

  FCurrentLines.EditInsert(1 + FLogicalIndentLen, WorkLine + 1, FStringBreakPrefix + '''');
  if (Command = ecLineBreak) then
    ACaret.BytePos := ACaret.BytePos + Length(FStringBreakPrefix) + 1;

  ACaret.DecForcePastEOL;
end;

constructor TSynBeautifierPascal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegExprEngine := TRegExpr.Create;
  FRegExprEngine.ModifierI := True;

  FIndentMode[sctAnsi]           := [sciAddTokenLen, sciAddPastTokenIndent];
  FIndentFirstLineExtra[sctAnsi] := '';
  FIndentFirstLineMax[sctAnsi]   := 0;

  FCommentMode[sctAnsi]          := sccPrefixMatch;
  FMatch[sctAnsi]                := '^ ?(\*)';
  FMatchMode[sctAnsi]            := scmMatchAfterOpening;
  FMatchLine[sctAnsi]            := sclMatchPrev;
  FPrefix[sctAnsi]               := '$1';

  FEolMatch[sctAnsi]             := '';
  FEolMode[sctAnsi]              := sccNoPrefix;
  FEolPostfix[sctAnsi]           := '';
  FEolSkipLongerLine[sctAnsi]    := False;


  FIndentMode[sctBor]           := [sciAddTokenLen, sciAddPastTokenIndent];
  FIndentFirstLineExtra[sctBor] := '';
  FIndentFirstLineMax[sctBor]   := 0;

  FCommentMode[sctBor]          := sccPrefixMatch;
  FMatch[sctBor]                := '^ ?(\*)';
  FMatchMode[sctBor]            := scmMatchAfterOpening;
  FMatchLine[sctBor]            := sclMatchPrev;
  FPrefix[sctBor]               := '$1';

  FEolMatch[sctBor]             := '';
  FEolMode[sctBor]              := sccNoPrefix;
  FEolPostfix[sctBor]           := '';
  FEolSkipLongerLine[sctBor]    := False;


  FExtendSlashCommentMode         := sceNever;

  FIndentMode[sctSlash]           := [];
  FIndentFirstLineExtra[sctSlash] := '';
  FIndentFirstLineMax[sctSlash]   := 0;

  FCommentMode[sctSlash]          := sccPrefixMatch;
  FMatch[sctSlash]                := '^ ?(\*)';
  FMatchMode[sctSlash]            := scmMatchAfterOpening;
  FMatchLine[sctSlash]            := sclMatchPrev;
  FPrefix[sctSlash]               := '$1';

  FEolMatch[sctSlash]             := '';
  FEolMode[sctSlash]              := sccNoPrefix;
  FEolPostfix[sctSlash]           := '';
  FEolSkipLongerLine[sctSlash]    := False;

  FStringBreakEnabled := False;
  FStringBreakAppend := ' +';
  FStringBreakPrefix := '';
end;

destructor TSynBeautifierPascal.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FRegExprEngine);
end;

procedure TSynBeautifierPascal.Assign(Src: TPersistent);
var
  i: TSynCommentType;
begin
  inherited Assign(Src);
  if not(Src is TSynBeautifierPascal) then exit;

  FExtendSlashCommentMode := TSynBeautifierPascal(Src).FExtendSlashCommentMode;

  for i := low(TSynCommentType) to high(TSynCommentType) do begin
    FIndentMode[i]           := TSynBeautifierPascal(Src).FIndentMode[i];
    FIndentFirstLineExtra[i] := TSynBeautifierPascal(Src).FIndentFirstLineExtra[i];
    FIndentFirstLineMax[i]   := TSynBeautifierPascal(Src).FIndentFirstLineMax[i];

    FCommentMode[i]          := TSynBeautifierPascal(Src).FCommentMode[i];
    FMatch[i]                := TSynBeautifierPascal(Src).FMatch[i];
    FMatchMode[i]            := TSynBeautifierPascal(Src).FMatchMode[i];
    FMatchLine[i]            := TSynBeautifierPascal(Src).FMatchLine[i];
    FPrefix[i]               := TSynBeautifierPascal(Src).FPrefix[i];

    FEolMatch[i]             := TSynBeautifierPascal(Src).FEolMatch[i];
    FEolMode[i]              := TSynBeautifierPascal(Src).FEolMode[i];
    FEolPostfix[i]           := TSynBeautifierPascal(Src).FEolPostfix[i];
    FEolSkipLongerLine[i]    := TSynBeautifierPascal(Src).FEolSkipLongerLine[i];
  end;

  FStringBreakAppend         := TSynBeautifierPascal(Src).FStringBreakAppend;
  FStringBreakEnabled        := TSynBeautifierPascal(Src).FStringBreakEnabled;
  FStringBreakPrefix         := TSynBeautifierPascal(Src).FStringBreakPrefix;

end;

{ TSynCustomBeautifier }

procedure TSynCustomBeautifier.Assign(Src: TPersistent);
begin
  if assigned(Src) and (src is TSynCustomBeautifier) then begin
    FCurrentEditor := TSynCustomBeautifier(Src).FCurrentEditor;
    FCurrentLines := TSynCustomBeautifier(Src).FCurrentLines;
    FOnGetDesiredIndent := TSynCustomBeautifier(Src).FOnGetDesiredIndent;
    FAutoIndent := TSynCustomBeautifier(Src).FAutoIndent;
  end
  else
    inherited;
end;

function TSynCustomBeautifier.GetCopy: TSynCustomBeautifier;
begin
  // Since all synedits share one beautifier, create a temp instance.
  // Todo: have individual beautifiers
  Result := TSynCustomBeautifierClass(self.ClassType).Create(nil);
  Result.assign(self);
end;

procedure TSynCustomBeautifier.BeforeCommand(const Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
  var Command: TSynEditorCommand; InitialCmd: TSynEditorCommand);
begin
  // Must be called on GetCopy
  // Todo: have individual beautifiers
  FCurrentEditor := Editor;
  FCurrentLines := Lines;
  DoBeforeCommand(ACaret, Command);
end;

procedure TSynCustomBeautifier.AfterCommand(const Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
  var Command: TSynEditorCommand; InitialCmd: TSynEditorCommand;
  StartLinePos, EndLinePos: Integer);
begin
  // Must be called on GetCopy
  // Todo: have individual beautifiers
  FCurrentEditor := Editor;
  FCurrentLines := Lines;
  DoAfterCommand(ACaret, Command, StartLinePos, EndLinePos);
end;

{ TSynBeautifier }

procedure TSynBeautifier.Assign(Src: TPersistent);
begin
  inherited Assign(Src);
  if assigned(Src) and (src is TSynBeautifier) then begin
    FIndentType := TSynBeautifier(Src).FIndentType;
    FCurrentEditor := TSynBeautifier(Src).FCurrentEditor;
    FCurrentLines := TSynBeautifier(Src).FCurrentLines;
  end;
end;

function TSynBeautifier.GetLine(AnIndex: Integer): String;
begin
  Result := FCurrentLines[AnIndex];
end;

procedure TSynBeautifier.GetIndentInfo(const LinePos: Integer; out ACharMix: String; out
  AnIndent: Integer; AStopScanAtLine: Integer);
var
  b: Integer;
begin
  ACharMix := '';
  if (GetLine(LinePos-2) = '') and (GetLine(LinePos-1) <> '') then
    AnIndent := 0
  else
    AnIndent := GetIndent(LinePos, b, AStopScanAtLine);

  if AnIndent > 0 then begin
    ACharMix := GetCharMix(LinePos, AnIndent, b, True);
    if (FIndentType = sbitPositionCaret) and (GetLine(LinePos-1) = '') then
      ACharMix := '';
  end;
end;

procedure TSynBeautifier.DoBeforeCommand(const ACaret: TSynEditCaret;
  var Command: TSynEditorCommand);
var
  x: Integer;
begin
  if (Command = ecDeleteLastChar) and
     (FAutoIndent) and
     (ACaret.CharPos > 1) and
     (not TCustomSynEdit(FCurrentEditor).ReadOnly) and
     ( (not TCustomSynEdit(FCurrentEditor).SelAvail) or
       (eoPersistentBlock in TCustomSynEdit(FCurrentEditor).Options2) ) and
     (GetIndentForLine(FCurrentEditor, ACaret.LineText, True) = ACaret.CharPos - 1)
  then begin
    FCurrentLines.UndoList.CurrentReason := ecSmartUnindent;

    UnIndentLine(ACaret, x);
    ACaret.CharPos := x;
    Command := ecNone;
  end;
end;

procedure TSynBeautifier.DoAfterCommand(const ACaret: TSynEditCaret;
  var Command: TSynEditorCommand; StartLinePos, EndLinePos: Integer);
var
  y, Indent: Integer;
  s: String;
begin
  FLogicalIndentLen := 0;
  if EndLinePos < 1 then
    exit;
  if assigned(FOnGetDesiredIndent) and
    FOnGetDesiredIndent(self, FCurrentEditor, ACaret.LineBytePos,
                        ACaret.OldLineBytePos, StartLinePos, EndLinePos, Command,
                        @ApplyIndent)
  then
    exit;

  if ((Command = ecLineBreak) or (Command = ecInsertLine)) and FAutoIndent then begin
    if (Command = ecLineBreak) then
      y := ACaret.LinePos
    else
      y := ACaret.LinePos + 1;

    GetIndentInfo(y, s, Indent);

    if s <> '' then begin;
      FCurrentLines.EditInsert(1, y, s);
      FLogicalIndentLen := length(s);
    end;

    if (Command = ecLineBreak) then begin
      ACaret.IncForcePastEOL;
      ACaret.CharPos := Indent + 1;
      ACaret.DecForcePastEOL;
    end;
  end;
end;

function TSynBeautifier.UnIndentLine(const ACaret: TSynEditCaret;
  out CaretNewX: Integer): Boolean;
var
  SpaceCount1, SpaceCount2: Integer;
  BackCounter, LogSpacePos, FillSpace: Integer;
  LogCaret: TPoint;
  Line, Temp: String;
begin
  Line := ACaret.LineText;
  SpaceCount1 := GetIndentForLine(FCurrentEditor, Line, true); // physical, desired pos
  SpaceCount2 := 0;
  if (SpaceCount1 > 0) then begin
    BackCounter := ACaret.LinePos - 2;
    while BackCounter >= 0 do begin
      Temp := FCurrentLines[BackCounter];
      SpaceCount2 := GetIndentForLine(FCurrentEditor, Temp, true);
      if (SpaceCount2 < SpaceCount1) and (temp <> '') then
        break;
      Dec(BackCounter);
    end;
  end;
  if SpaceCount2 >= SpaceCount1 then
    SpaceCount2 := 0;
  // remove visible spaces
  LogSpacePos := FCurrentLines.PhysicalToLogicalCol(Line, ACaret.LinePos-1, SpaceCount2 + 1);
  FillSpace := SpaceCount2 + 1 - FCurrentLines.LogicalToPhysicalCol(Line, ACaret.LinePos-1, LogSpacePos);
  LogCaret := ACaret.LineBytePos;
  CaretNewX :=  SpaceCount2 + 1;
  FCurrentLines.EditDelete(LogSpacePos, ACaret.LinePos, LogCaret.X - LogSpacePos);
  if FillSpace > 0 then
    FCurrentLines.EditInsert(LogSpacePos, ACaret.LinePos, StringOfChar(' ', FillSpace));
  Result := True;
end;

function TSynBeautifier.GetIndent(const LinePos: Integer; out BasedOnLine: Integer;
  AStopScanAtLine: Integer): Integer;
var
  Temp: string;
begin
  if AStopScanAtLine > 0 then
    dec(AStopScanAtLine); // Convert to index
  BasedOnLine := LinePos - 1; // Convert to index
  while (BasedOnLine > AStopScanAtLine) do begin
    dec(BasedOnLine);
    Temp := GetLine(BasedOnLine);
    if Temp <> '' then begin
      Result := GetIndentForLine(FCurrentEditor, Temp, True);
      exit;
    end;
  end;
  Result := 0;
  //BasedOnLine := LinePos;
  //Result := GetIndentForLine(FCurrentEditor, GetLine(BasedOnLine), True);
end;

function TSynBeautifier.AdjustCharMix(DesiredIndent: Integer; CharMix, AppendMix: String): String;
var
  i: Integer;
  CurLen: Integer;
begin
  CurLen := FCurrentLines.LogicalToPhysicalCol(CharMix, -1, length(CharMix)+1) - 1; // TODO: Need the real index of the line
  if AppendMix <> '' then begin
    while CurLen < DesiredIndent do begin
      CharMix := CharMix + AppendMix;
      CurLen := FCurrentLines.LogicalToPhysicalCol(CharMix, -1, length(CharMix)+1) - 1; // TODO: Need the real index of the line
    end
  end;

  i := length(CharMix);
  while CurLen > DesiredIndent do begin
    Dec(i);
    CurLen := FCurrentLines.LogicalToPhysicalCol(CharMix, -1, i+1) - 1; // TODO: Need the real index of the line
  end;

  CharMix := copy(CharMix, 1, i) + StringOfChar(' ', DesiredIndent - CurLen);
  Result := CharMix;
end;

function TSynBeautifier.CreateTabSpaceMix(var DesiredIndent: Integer;
  OnlyTabs: Boolean): String;
var
  CurLen: Integer;
begin
  CurLen := 0;
  Result := '';
  while CurLen < DesiredIndent do begin
    Result := Result + #9;
    CurLen := FCurrentLines.LogicalToPhysicalCol(Result, -1, length(Result)+1) - 1; // TODO: Need the real index of the line
  end;

  if CurLen = DesiredIndent then
    exit;

  Delete(Result, Length(Result), 1);
  if OnlyTabs then begin
    CurLen := FCurrentLines.LogicalToPhysicalCol(Result, -1, length(Result)+1) - 1; // TODO: Need the real index of the line
    DesiredIndent := CurLen;
    exit;
  end;

  repeat
    Result := Result + ' ';
    CurLen := FCurrentLines.LogicalToPhysicalCol(Result, -1, length(Result)+1) - 1; // TODO: Need the real index of the line
  until CurLen >= DesiredIndent;
end;

function TSynBeautifier.GetCharMix(const LinePos: Integer; var Indent: Integer;
  var IndentCharsFromLinePos: Integer; ModifyIndent: Boolean): String;
var
  Temp, KnownMix, BasedMix: string;
  KnownPhysLen, PhysLen: Integer;
  BackCounter: LongInt;
  OrigIndent: Integer;
begin
  OrigIndent := Indent;
  case FIndentType of
      sbitSpace, sbitPositionCaret:
      begin
        IndentCharsFromLinePos := 0;
        Result := StringOfChar(' ', Indent);
        if not ModifyIndent then Indent := OrigIndent;
        exit;
      end;
    sbitConvertToTabSpace:
      begin
        IndentCharsFromLinePos := 0;
        Result := CreateTabSpaceMix(Indent, False);
        exit;
        if not ModifyIndent then Indent := OrigIndent;
      end;
    sbitConvertToTabOnly:
      begin
        IndentCharsFromLinePos := 0;
        Result := CreateTabSpaceMix(Indent, True);
        if not ModifyIndent then Indent := OrigIndent;
        exit;
      end;
  end;

  if (IndentCharsFromLinePos > 0) and (IndentCharsFromLinePos <= FCurrentLines.Count) then
  begin
    Temp := GetLine(IndentCharsFromLinePos);
    KnownMix := copy(Temp, 1, GetIndentForLine(FCurrentEditor, Temp, False));
  end
  else
    KnownMix := '';
  BasedMix := KnownMix;
  KnownPhysLen := GetIndentForLine(FCurrentEditor, KnownMix, True);

  BackCounter := LinePos;
  while (BackCounter > 0) and (KnownPhysLen < Indent) do begin
    dec(BackCounter);
    Temp := GetLine(BackCounter);
    if Temp <> '' then begin
      Temp := copy(Temp, 1, GetIndentForLine(FCurrentEditor, Temp, False));
      PhysLen := GetIndentForLine(FCurrentEditor, Temp, True);
      if (PhysLen > KnownPhysLen) and (copy(temp, 1, length(BasedMix)) = BasedMix) then
      begin
        KnownMix := Temp;
        KnownPhysLen := PhysLen;
        IndentCharsFromLinePos := BackCounter + 1;
      end;
    end;
  end;

  Result := AdjustCharMix(Indent, KnownMix, '');
  if not ModifyIndent then Indent := OrigIndent;
end;

procedure TSynBeautifier.ApplyIndent(LinePos: Integer;
  Indent: Integer; RelativeToLinePos: Integer; IndentChars: String = '';
  IndentCharsFromLinePos: Integer = 0);
var
  CharMix: String;
  i: Integer;
begin
  if (LinePos < 1) or (LinePos > FCurrentEditor.Lines.Count) then
    exit;

  // calculate the final indent needed
  if (RelativeToLinePos > 0) and (RelativeToLinePos <= FCurrentEditor.Lines.Count) then
    Indent := Indent + GetIndentForLine(FCurrentEditor, GetLine(RelativeToLinePos-1), True);
  if Indent< 0 then
    Indent := 0;

  // Calculate the charmix
  CharMix := '';
  if Indent > 0 then begin
    if (IndentCharsFromLinePos > 0) and (IndentCharsFromLinePos <= FCurrentEditor.Lines.Count) then begin
      CharMix := GetLine(IndentCharsFromLinePos-1);
      i :=  GetIndentForLine(FCurrentEditor, CharMix, False);
      CharMix := AdjustCharMix(Indent, copy(CharMix, 1, i), IndentChars);
    end
    else if IndentChars <> '' then begin
      CharMix := AdjustCharMix(Indent, '', IndentChars);
    end
    else begin
      i := LinePos;
      CharMix := GetCharMix(LinePos, Indent, i);
    end;
  end;

  {$IFDEF VerboseIndenter}
  DebugLn(['TSynBeautifier.ApplyIndent IndentChars="',dbgstr(IndentChars),' Indent=',Indent]);
  {$ENDIF}

  i :=  GetIndentForLine(FCurrentEditor, GetLine(LinePos-1), False);
  FCurrentLines.EditDelete(1, LinePos, i);
  if (CharMix <> '') and not((FIndentType = sbitPositionCaret) and (GetLine(LinePos-1) = '')) then
    FCurrentLines.EditInsert(1, LinePos, CharMix);
  FLogicalIndentLen := length(CharMix);

  {$IFDEF VerboseIndenter}
  DebugLn(['TSynBeautifier.ApplyIndent Line="',dbgstr(FCurrentLines.ExpandedStrings[LinePos-1]),'"']);
  {$ENDIF}
end;

function TSynBeautifier.GetIndentForLine(Editor: TSynEditBase; const Line: string; Physical: boolean): Integer;
var
  p: PChar;
begin
  p := PChar(Line);
  if Assigned(p) then begin
    Result := 0;
    while p^ in [#1..#32] do begin
      Inc(p);
      Inc(Result);
    end;
    if Physical and (Result>0) then
      Result := FCurrentLines.LogicalToPhysicalCol(Line, -1, Result+1)-1; // TODO, Need the real index of the line
  end else
    Result := 0;
end;

function TSynBeautifier.GetDesiredIndentForLine(Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret;
  out ReplaceIndent: Boolean; out DesiredIndent: String): Integer;
var
  BackCounter, PhysLen: Integer;
  Temp: string;
  FoundLine: LongInt;
begin
  Result := 1;
  FCurrentLines := Lines; // for GetCurrentIndent
  BackCounter := ACaret.LinePos - 1;
  if BackCounter > 0 then
    repeat
      Dec(BackCounter);
      Temp := Lines[BackCounter];
      Result := GetIndentForLine(Editor, Temp, True) + 1; // Physical
    until (BackCounter = 0) or (Temp <> '');

  FoundLine := BackCounter + 1;
  ReplaceIndent := False;
  //if assigned(FOnGetDesiredIndent) then
  //  FOnGetDesiredIndent(Editor, ACaret.LineBytePos, ACaret.LinePos, Result,
  //                      FoundLine, ReplaceIndent);

  //if Result < 0 then exit;

  //if FoundLine > 0 then
  //  Temp := Lines[FoundLine-1]
  //else
  //  FoundLine := BackCounter + 1;
  Temp := copy(Temp, 1, GetIndentForLine(Editor, Temp, False));

  case FIndentType of
    sbitCopySpaceTab:
      begin
        DesiredIndent := copy(Temp, 1,
                   Lines.PhysicalToLogicalCol(Temp, FoundLine - 1, Result) - 1);
        PhysLen := Lines.LogicalToPhysicalCol(Temp, ACaret.LinePos - 1, Length(Temp) + 1);
        if PhysLen < Result then
          DesiredIndent := DesiredIndent + StringOfChar(' ', Result - PhysLen);
      end;
    sbitConvertToTabSpace:
      begin
        dec(Result);
        DesiredIndent := CreateTabSpaceMix(Result, False);
        inc(Result);
      end;
    sbitConvertToTabOnly:
      begin
        dec(Result);
        DesiredIndent := CreateTabSpaceMix(Result, True);
        inc(Result);
      end;
    else
      DesiredIndent := StringOfChar(' ', Result - 1);
  end;
end;

function TSynBeautifier.GetDesiredIndentForLine(Editor: TSynEditBase;
  const Lines: TSynEditStrings; const ACaret: TSynEditCaret): Integer;
var
  Dummy: String;
  Replace: Boolean;
begin
  Result := GetDesiredIndentForLine(Editor, Lines, ACaret, Replace, Dummy);
end;

end.

