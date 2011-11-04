{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Contains classes to store key-command relationships, can update
    TSynEditKeyStrokes and provides a dialog for editing a single
    commandkey.
}
unit KeyMapping;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, LCLProc, AvgLvlTree,
  Forms, Classes, SysUtils, Buttons, LResources, Controls,
  Dialogs, StringHashList, ExtCtrls,
  SynEditKeyCmds, SynPluginTemplateEdit, SynPluginSyncroEdit, Laz_XMLCfg,
  PropEdits, IDECommands, LazarusIDEStrConsts, Debugger;

type
  TKeyMapScheme = (
    kmsLazarus,
    kmsClassic,
    kmsMacOSXApple,
    kmsMacOSXLaz,
    kmsCustom
    );

const
  KeyMapSchemeNames: array[TKeyMapScheme] of string = (
    'default',
    'Classic',
    'MacOSXApple',
    'MacOSXLaz',
    'Custom'
    );

type
  //---------------------------------------------------------------------------
  // TKeyCommandCategory is used to divide the key commands in handy packets
  TKeyCommandCategory = class(TIDECommandCategory)
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    constructor Create(const AName, ADescription: string;
                       TheScope: TIDECommandScope);
  end;
  
  //---------------------------------------------------------------------------
  // class for storing the keys of a single command (key-command relationship)

  { TKeyCommandRelation }

  TKeyCommandRelation = class(TIDECommand)
  private
    procedure SetShortcutA(const AValue: TIDEShortCut); override;
    procedure SetShortcutB(const AValue: TIDEShortCut); override;
    procedure SetSingle(NewKeyA: word; NewShiftA: TShiftState;
                        NewKeyB: word; NewShiftB: TShiftState);
    procedure SetSingle(NewKeyA: word; NewShiftA: TShiftState);
    procedure SetCombo(NewKey1A: word; NewShift1A: TShiftState;
                       NewKey1B: word; NewShift1B: TShiftState;
                       NewKey2A: word; NewShift2A: TShiftState;
                       NewKey2B: word; NewShift2B: TShiftState);
    procedure SetCombo(NewKey1A: word; NewShift1A: TShiftState;
                       NewKey1B: word; NewShift1B: TShiftState);
    procedure MapShortcut(AScheme: TKeyMapScheme);
    procedure GetDefaultKeyForCommand;
    procedure GetDefaultKeyForWindowsScheme;
    procedure GetDefaultKeyForClassicScheme;
    procedure GetDefaultKeyForMacOSXScheme;
    procedure GetDefaultKeyForMacOSXLazScheme;
  protected
  public
    function GetLocalizedName: string; override;
  end;

  { TLoadedKeyCommand
    used for  }

  TLoadedKeyCommand = class
  public
    Name: string;
    ShortcutA: TIDEShortCut;
    DefaultShortcutA: TIDEShortCut;
    ShortcutB: TIDEShortCut;
    DefaultShortcutB: TIDEShortCut;
    function IsShortcutADefault: boolean;
    function IsShortcutBDefault: boolean;
    function AsString: string;
  end;

  //---------------------------------------------------------------------------
  // class for a list of key - command relations

  { TKeyCommandRelationList }

  TKeyCommandRelationList = class(TIDECommands)
  private
    fLastKey: TIDEShortCut; // for multiple key commands
    fRelations: TList; // list of TKeyCommandRelation, sorted with Command
    fCategories: TList;// list of TKeyCommandCategory
    fExtToolCount: integer;
    fLoadedKeyCommands: TAvgLvlTree;// tree of TLoadedKeyCommand sorted for name
    function GetRelation(Index: integer): TKeyCommandRelation;
    function GetRelationCount: integer;
    function AddCategory(const Name, Description: string;
                         TheScope: TIDECommandScope): integer;
    function GetNewOrExisting(Category: TIDECommandCategory;
                 Command: TIDECommand): TKeyCommandRelation;
    function GetNewOrExisting(Category: TIDECommandCategory;
                 const Name, LocalizedName: string;
                 Command: word; TheKeyA, TheKeyB: TIDEShortCut;
                 const OnExecuteMethod: TNotifyEvent = nil;
                 const OnExecuteProc: TNotifyProcedure = nil): TKeyCommandRelation;
    function AddDefault(Category: TIDECommandCategory;
                        const Name, LocalizedName: string;
                        Command: word):integer;
    procedure SetExtToolCount(NewCount: integer);
  protected
    function GetCategory(Index: integer): TIDECommandCategory; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateDefaultMapping;
    procedure Clear;
    function Count: integer;
    function CategoryCount: integer; override;
    function Find(Key: TIDEShortCut; IDEWindowClass: TCustomFormClass
                  ): TKeyCommandRelation;
    function FindIDECommand(ACommand:word): TIDECommand; override;
    function FindByCommand(ACommand:word): TKeyCommandRelation;
    function FindCategoryByName(const CategoryName: string): TIDECommandCategory; override;
    function FindCommandByName(const CommandName: string): TIDECommand; override;
    function TranslateKey(Key: word; Shift: TShiftState;
      IDEWindowClass: TCustomFormClass; UseLastKey: boolean = true): word;
    function IndexOf(ARelation: TKeyCommandRelation): integer;
    function CommandToShortCut(ACommand: word): TShortCut;
    function LoadFromXMLConfig(XMLConfig:TXMLConfig; const Path: String):boolean;
    function SaveToXMLConfig(XMLConfig:TXMLConfig; const Path: String):boolean;
    procedure AssignTo(ASynEditKeyStrokes:TSynEditKeyStrokes;
                       IDEWindowClass: TCustomFormClass);
    procedure Assign(List: TKeyCommandRelationList);
    procedure LoadScheme(const SchemeName: string);
    function CreateUniqueCategoryName(const AName: string): string;
    function CreateUniqueCommandName(const AName: string): string;
    function CreateNewCommandID: word;
    function CreateCategory(Parent: TIDECommandCategory;
                            const AName, Description: string;
                            Scope: TIDECommandScope = nil): TIDECommandCategory; override;
    function CreateCommand(Category: TIDECommandCategory;
                           const AName, Description: string;
                           const TheShortcutA, TheShortcutB: TIDEShortCut;
                           const OnExecuteMethod: TNotifyEvent = nil;
                           const OnExecuteProc: TNotifyProcedure = nil
                           ): TIDECommand; override;
  public
    property ExtToolCount: integer read fExtToolCount write SetExtToolCount;// in menu
    property Relations[Index:integer]:TKeyCommandRelation read GetRelation; default;
    property RelationCount:integer read GetRelationCount;
  end;

function IDEShortCutEmpty(const Key: TIDEShortCut): boolean;
function KeyAndShiftStateToEditorKeyString(const Key: TIDEShortCut): String;
function FindKeymapConflicts(Keymap: TKeyCommandRelationList;
                      Protocol: TStrings; out Index1, Index2: integer): integer;
function EditorCommandToDescriptionString(cmd: word): String;

function KeySchemeNameToSchemeType(const SchemeName: string): TKeyMapScheme;

function ShiftStateToCfgStr(Shift: TShiftState): string;
function KeyValuesToCfgStr(const ShortcutA, ShortcutB: TIDEShortCut): string;
function CfgStrToShiftState(const s: string): TShiftState;

function CompareLoadedKeyCommands(Data1, Data2: Pointer): integer;
function CompareNameWithLoadedKeyCommand(NameAsAnsiString, Key: Pointer): integer;


implementation

const
  KeyMappingFormatVersion = 6;

function KeySchemeNameToSchemeType(const SchemeName: string): TKeyMapScheme;
begin
  if SchemeName='' then
    exit(kmsLazarus);
  for Result:=Low(TKeyMapScheme) to High(TKeyMapScheme) do begin
    if CompareText(SchemeName,KeyMapSchemeNames[Result])=0 then
      exit;
  end;
  Result:=kmsCustom;
end;

function ShiftStateToCfgStr(Shift:TShiftState):string;
var i:integer;
begin
  i:=0;
  if ssCtrl in Shift then inc(i,1);
  if ssShift in Shift then inc(i,2);
  if ssAlt in Shift then inc(i,4);
  if ssMeta in Shift then inc(i,8);
  if ssSuper in Shift then inc(i,16);
  Result:=IntToStr(i);
end;

function KeyValuesToCfgStr(const ShortcutA, ShortcutB: TIDEShortCut): string;
begin
  Result:=IntToStr(ShortcutA.Key1) + ',' + ShiftStateToCfgStr(ShortcutA.Shift1) + ',' +
          IntToStr(ShortcutA.Key2) + ',' + ShiftStateToCfgStr(ShortcutA.Shift2) + ',' +
          IntToStr(ShortcutB.Key1) + ',' + ShiftStateToCfgStr(ShortcutB.Shift1) + ',' +
          IntToStr(ShortcutB.Key2) + ',' + ShiftStateToCfgStr(ShortcutB.Shift2);
end;

function CfgStrToShiftState(const s: string): TShiftState;
var
  i: LongInt;
begin
  Result:=[];
  i:=StrToIntDef(s,0);
  if (i and 1)<>0 then include(Result,ssCtrl);
  if (i and 2)<>0 then include(Result,ssShift);
  if (i and 4)<>0 then include(Result,ssAlt);
  if (i and 8)<>0 then include(Result,ssMeta);
  if (i and 16)<>0 then include(Result,ssSuper);
end;

function CompareLoadedKeyCommands(Data1, Data2: Pointer): integer;
var
  Key1: TLoadedKeyCommand absolute Data1;
  Key2: TLoadedKeyCommand absolute Data2;
begin
  Result:=SysUtils.CompareText(Key1.Name,Key2.Name);
end;

function CompareNameWithLoadedKeyCommand(NameAsAnsiString, Key: Pointer): integer;
var
  Name: string;
  LoadedKey: TLoadedKeyCommand absolute Key;
begin
  Pointer(Name):=NameAsAnsiString;
  Result:=SysUtils.CompareText(Name,LoadedKey.Name);
  Pointer(Name):=nil;
end;

function EditorCommandToDescriptionString(cmd: word): String;
begin
  case cmd of
    ecNone                    : Result:= dlgEnvNone;
    ecLeft                    : Result:= lisLeft;
    ecRight                   : Result:= lisRight;
    ecUp                      : Result:= dlgUpWord;
    ecDown                    : Result:= dlgDownWord;
    ecWordLeft                : Result:= srkmecWordLeft;
    ecWordRight               : Result:= srkmecWordRight;
    ecLineStart               : Result:= srkmecLineStart;
    ecLineEnd                 : Result:= srkmecLineEnd;
    ecPageUp                  : Result:= srkmecPageUp;
    ecPageDown                : Result:= srkmecPageDown;
    ecPageLeft                : Result:= srkmecPageLeft;
    ecPageRight               : Result:= srkmecPageRight;
    ecPageTop                 : Result:= srkmecPageTop;
    ecPageBottom              : Result:= srkmecPageBottom;
    ecEditorTop               : Result:= srkmecEditorTop;
    ecEditorBottom            : Result:= srkmecEditorBottom;
    ecGotoXY                  : Result:= srkmecGotoXY;
    ecLineTextStart           : Result:= srkmecLineTextStart;
    ecSelLeft                 : Result:= srkmecSelLeft;
    ecSelRight                : Result:= srkmecSelRight;
    ecSelUp                   : Result:= srkmecSelUp;
    ecSelDown                 : Result:= srkmecSelDown;
    ecSelWordLeft             : Result:= srkmecSelWordLeft;
    ecSelWordRight            : Result:= srkmecSelWordRight;
    ecSelLineStart            : Result:= srkmecSelLineStart;
    ecSelLineEnd              : Result:= srkmecSelLineEnd;
    ecSelPageUp               : Result:= srkmecSelPageUp;
    ecSelPageDown             : Result:= srkmecSelPageDown;
    ecSelPageLeft             : Result:= srkmecSelPageLeft;
    ecSelPageRight            : Result:= srkmecSelPageRight;
    ecSelPageTop              : Result:= srkmecSelPageTop;
    ecSelPageBottom           : Result:= srkmecSelPageBottom;
    ecSelEditorTop            : Result:= srkmecSelEditorTop;
    ecSelEditorBottom         : Result:= srkmecSelEditorBottom;
    ecSelLineTextStart        : Result:= srkmecSelLineTextStart;
    ecColSelUp                : Result:= srkmecColSelUp;
    ecColSelDown              : Result:= srkmecColSelDown;
    ecColSelLeft              : Result:= srkmecColSelLeft;
    ecColSelRight             : Result:= srkmecColSelRight;
    ecColSelWordLeft          : Result:= srkmecColSelWordLeft;
    ecColSelWordRight         : Result:= srkmecColSelWordRight;
    ecColSelPageDown          : Result:= srkmecColSelPageDown;
    ecColSelPageBottom        : Result:= srkmecColSelPageBottom;
    ecColSelPageUp            : Result:= srkmecColSelPageUp;
    ecColSelPageTop           : Result:= srkmecColSelPageTop;
    ecColSelLineStart         : Result:= srkmecColSelLineStart;
    ecColSelLineEnd           : Result:= srkmecColSelLineEnd;
    ecColSelEditorTop         : Result:= srkmecColSelEditorTop;
    ecColSelEditorBottom      : Result:= srkmecColSelEditorBottom;
    ecColSelLineTextStart     : Result:= srkmecColSelLineTextStart;
    ecSelGotoXY               : Result:= srkmecSelGotoXY;
    ecSelectAll               : Result:= srkmecSelectAll;
    ecDeleteLastChar          : Result:= srkmecDeleteLastChar;
    ecDeleteChar              : Result:= srkmecDeleteChar;
    ecDeleteWord              : Result:= srkmecDeleteWord;
    ecDeleteLastWord          : Result:= srkmecDeleteLastWord;
    ecDeleteBOL               : Result:= srkmecDeleteBOL;
    ecDeleteEOL               : Result:= srkmecDeleteEOL;
    ecDeleteLine              : Result:= srkmecDeleteLine;
    ecClearAll                : Result:= srkmecClearAll;
    ecLineBreak               : Result:= srkmecLineBreak;
    ecInsertLine              : Result:= srkmecInsertLine;
    ecChar                    : Result:= srkmecChar;
    ecImeStr                  : Result:= srkmecImeStr;
    ecUndo                    : Result:= lisMenuUndo;
    ecRedo                    : Result:= lisMenuRedo;
    ecCut                     : Result:= srkmecCut;
    ecCopy                    : Result:= srkmecCopy;
    ecPaste                   : Result:= srkmecPaste;
    ecScrollUp                : Result:= srkmecScrollUp;
    ecScrollDown              : Result:= srkmecScrollDown;
    ecScrollLeft              : Result:= srkmecScrollLeft;
    ecScrollRight             : Result:= srkmecScrollRight;
    ecInsertMode              : Result:= srkmecInsertMode;
    ecOverwriteMode           : Result:= srkmecOverwriteMode;
    ecToggleMode              : Result:= srkmecToggleMode;
    ecBlockIndent             : Result:= srkmecBlockIndent;
    ecBlockUnindent           : Result:= srkmecBlockUnindent;
    ecTab                     : Result:= lisTab;
    ecShiftTab                : Result:= srkmecShiftTab;
    ecMatchBracket            : Result:= srkmecMatchBracket;
    ecNormalSelect            : Result:= srkmecNormalSelect;
    ecColumnSelect            : Result:= srkmecColumnSelect;
    ecLineSelect              : Result:= srkmecLineSelect;
    ecAutoCompletion          : Result:= srkmecAutoCompletion;
    ecUserFirst               : Result:= srkmecPrevBookmark;
    ecSetFreeBookmark         : Result:= srkmecSetFreeBookmark;
    ecPrevBookmark            : Result:= srkmecPrevBookmark;
    ecNextBookmark            : Result:= srkmecNextBookmark;
    ecGotoMarker0 ..
    ecGotoMarker9             : Result:= Format(srkmecGotoMarker,[cmd-ecGotoMarker0]);
    ecSetMarker0 ..
    ecSetMarker9              : Result:= Format(srkmecSetMarker,[cmd-ecSetMarker0]);
    ecToggleMarker0 ..
    ecToggleMarker9           : Result:= Format(srkmecToggleMarker,[cmd-ecToggleMarker0]);

    ecBlockSetBegin   : Result := srkmecBlockSetBegin;
    ecBlockSetEnd     : Result := srkmecBlockSetEnd;
    ecBlockToggleHide : Result := srkmecBlockToggleHide;
    ecBlockHide       : Result := srkmecBlockHide;
    ecBlockShow       : Result := srkmecBlockShow;
    ecBlockMove       : Result := srkmecBlockMove;
    ecBlockCopy       : Result := srkmecBlockCopy;
    ecBlockDelete     : Result := srkmecBlockDelete;
    ecBlockGotoBegin  : Result := srkmecBlockGotoBegin;
    ecBlockGotoEnd    : Result := srkmecBlockGotoEnd;

    // sourcenotebook
    ecNextEditor              : Result:= srkmecNextEditor;
    ecPrevEditor              : Result:= srkmecPrevEditor;
    ecMoveEditorLeft          : Result:= srkmecMoveEditorLeft;
    ecMoveEditorRight         : Result:= srkmecMoveEditorRight;
    ecMoveEditorLeftmost      : Result:= srkmecMoveEditorLeftmost;
    ecMoveEditorRightmost     : Result:= srkmecMoveEditorRightmost;
    ecToggleBreakPoint        : Result:= srkmecToggleBreakPoint;
    ecRemoveBreakPoint        : Result:= srkmecRemoveBreakPoint;

    ecNextSharedEditor:        Result := srkmecNextSharedEditor;
    ecPrevSharedEditor:        Result := srkmecPrevSharedEditor;
    ecNextWindow:              Result := srkmecNextWindow;
    ecPrevWindow:              Result := srkmecPrevWindow;
    ecMoveEditorNextWindow:    Result := srkmecMoveEditorNextWindow;
    ecMoveEditorPrevWindow:    Result := srkmecMoveEditorPrevWindow;
    ecMoveEditorNewWindow:     Result := srkmecMoveEditorNewWindow;
    ecCopyEditorNextWindow:    Result := srkmecCopyEditorNextWindow;
    ecCopyEditorPrevWindow:    Result := srkmecCopyEditorPrevWindow;
    ecCopyEditorNewWindow:     Result := srkmecCopyEditorNewWindow;

    ecLockEditor:              Result := srkmecLockEditor;

    ecGotoEditor1..
    ecGotoEditor0             : Result:= Format(srkmecGotoEditor,[cmd-ecGotoEditor1]);
    EcFoldLevel1..
    EcFoldLevel9             : Result:= Format(srkmEcFoldLevel,[cmd-EcFoldLevel1]);
    EcFoldLevel0             : Result:= srkmecUnFoldAll;
    EcFoldCurrent            : Result:= srkmecFoldCurrent;
    EcUnFoldCurrent          : Result:= srkmecUnFoldCurrent;
    EcToggleMarkupWord       : Result := srkmecToggleMarkupWord;

    // file menu
    ecNew                     : Result:= lisMenuNewOther;
    ecNewUnit                 : Result:= lisMenuNewUnit;
    ecNewForm                 : Result:= lisMenuNewForm;
    ecOpen                    : Result:= lisMenuOpen;
    ecRevert                  : Result:= lisMenuRevert;
    ecSave                    : Result:= srkmecSave;
    ecSaveAs                  : Result:= lisMenuSaveAs;
    ecSaveAll                 : Result:= lisMenuSaveAll;
    ecClose                   : Result:= lisMenuClose;
    ecCloseAll                : Result:= lisMenuCloseAll;
    ecCleanDirectory          : Result:= lisMenuCleanDirectory;
    ecRestart                 : Result:= lisMenuRestart;
    ecQuit                    : Result:= srkmecQuit;

    // edit menu
    ecSelectionUpperCase      : Result:= lisMenuUpperCaseSelection;
    ecSelectionLowerCase      : Result:= lisMenuLowerCaseSelection;
    ecSelectionSwapCase       : Result:= lisMenuSwapCaseSelection;
    ecSelectionTabs2Spaces    : Result:= srkmecSelectionTabs2Spaces;
    ecSelectionEnclose        : Result:= lisMenuEncloseSelection;
    ecSelectionComment        : Result:= lisMenuCommentSelection;
    ecSelectionUncomment      : Result:= lisMenuUncommentSelection;
    ecToggleComment           : Result:= lisMenuToggleComment;
    ecSelectionEncloseIFDEF   : Result:= lisMenuEncloseInIFDEF;
    ecSelectionSort           : Result:= lisMenuSortSelection;
    ecSelectionBreakLines     : Result:= lisMenuBeakLinesInSelection;
    ecSelectToBrace           : Result:= lisMenuSelectToBrace;
    ecSelectCodeBlock         : Result:= lisMenuSelectCodeBlock;
    ecSelectWord              : Result:= lisMenuSelectWord;
    ecSelectLine              : Result:= lisMenuSelectLine;
    ecSelectParagraph         : Result:= lisMenuSelectParagraph;
    ecInsertCharacter         : Result:= srkmecInsertCharacter;
    ecInsertGPLNotice         : Result:= srkmecInsertGPLNotice;
    ecInsertLGPLNotice        : Result:= srkmecInsertLGPLNotice;
    ecInsertModifiedLGPLNotice: Result:= srkmecInsertModifiedLGPLNotice;
    ecInsertUserName          : Result:= srkmecInsertUserName;
    ecInsertDateTime          : Result:= srkmecInsertDateTime;
    ecInsertChangeLogEntry    : Result:= srkmecInsertChangeLogEntry;
    ecInsertCVSAuthor         : Result:= srkmecInsertCVSAuthor;
    ecInsertCVSDate           : Result:= srkmecInsertCVSDate;
    ecInsertCVSHeader         : Result:= srkmecInsertCVSHeader;
    ecInsertCVSID             : Result:= srkmecInsertCVSID;
    ecInsertCVSLog            : Result:= srkmecInsertCVSLog;
    ecInsertCVSName           : Result:= srkmecInsertCVSName;
    ecInsertCVSRevision       : Result:= srkmecInsertCVSRevision;
    ecInsertCVSSource         : Result:= srkmecInsertCVSSource;
    ecInsertGUID              : Result:= srkmecInsertGUID;
    ecInsertFilename          : Result:= srkmecInsertFilename;

    // search menu
    ecFind                    : Result:= srkmecFind;
    ecFindNext                : Result:= srkmecFindNext;
    ecFindPrevious            : Result:= srkmecFindPrevious;
    ecFindInFiles             : Result:= srkmecFindInFiles;
    ecReplace                 : Result:= srkmecReplace;
    ecIncrementalFind         : Result:= lisMenuIncrementalFind;
    ecFindProcedureDefinition : Result:= srkmecFindProcedureDefinition;
    ecFindProcedureMethod     : Result:= srkmecFindProcedureMethod;
    ecGotoLineNumber          : Result:= srkmecGotoLineNumber;
    ecFindNextWordOccurrence  : Result:= srkmecFindNextWordOccurrence;
    ecFindPrevWordOccurrence  : Result:= srkmecFindPrevWordOccurrence;
    ecJumpBack                : Result:= lisMenuJumpBack;
    ecJumpForward             : Result:= lisMenuJumpForward;
    ecAddJumpPoint            : Result:= srkmecAddJumpPoint;
    ecJumpToNextError         : Result:= lisMenuJumpToNextError;
    ecJumpToPrevError         : Result:= lisMenuJumpToPrevError;
    ecGotoIncludeDirective    : Result:= srkmecGotoIncludeDirective;
    ecOpenFileAtCursor        : Result:= srkmecOpenFileAtCursor;
    ecProcedureList           : Result:= lisPListProcedureList;

    // view menu
    ecToggleFormUnit          : Result:= srkmecToggleFormUnit;
    ecToggleObjectInsp        : Result:= srkmecToggleObjectInsp;
    ecToggleSourceEditor      : Result:= srkmecToggleSourceEditor;
    ecToggleCodeExpl          : Result:= srkmecToggleCodeExpl;
    ecToggleFPDocEditor       : Result:= srkmecToggleFPDocEditor;
    ecToggleMessages          : Result:= srkmecToggleMessages;
    ecToggleSearchResults     : Result:= srkmecToggleSearchResults;
    ecToggleWatches           : Result:= srkmecToggleWatches;
    ecToggleBreakPoints       : Result:= srkmecToggleBreakPoints;
    ecToggleDebuggerOut       : Result:= srkmecToggleDebuggerOut;
    ecToggleLocals            : Result:= srkmecToggleLocals;
    ecViewThreads             : Result:= srkmecViewThreads;
    ecViewPseudoTerminal      : Result:= srkmecViewPseudoTerminal;
    ecToggleCallStack         : Result:= srkmecToggleCallStack;
    ecToggleRegisters         : Result:= srkmecToggleRegisters;
    ecToggleAssembler         : Result:= srkmecToggleAssembler;
    ecViewHistory             : Result:= srkmecViewHistory;
    ecViewUnitDependencies    : Result:= srkmecViewUnitDependencies;
    ecViewUnitInfo            : Result:= srkmecViewUnitInfo;
    ecViewAnchorEditor        : Result:= srkmecViewAnchorEditor;
    ecViewTabOrder            : Result:= srkmecViewTabOrder;
    ecToggleCodeBrowser       : Result:= srkmecToggleCodeBrowser;
    ecToggleRestrictionBrowser: Result:= srkmecToggleRestrictionBrowser;
    ecViewComponents          : Result:= srkmecViewComponents;
    ecViewJumpHistory         : Result:= lisMenuViewJumpHistory;
    ecToggleCompPalette       : Result:= srkmecToggleCompPalette;
    ecToggleIDESpeedBtns      : Result:= srkmecToggleIDESpeedBtns;

    // codetools
    ecWordCompletion          : Result:= srkmecWordCompletion;
    ecCompleteCode            : Result:= srkmecCompleteCode;
    ecIdentCompletion         : Result:= dlgedidcomlet;
    ecShowCodeContext         : Result:= srkmecShowCodeContext;
    ecExtractProc             : Result:= srkmecExtractProc;
    ecFindIdentifierRefs      : Result:= srkmecFindIdentifierRefs;
    ecRenameIdentifier        : Result:= srkmecRenameIdentifier;
    ecInvertAssignment        : Result:= srkmecInvertAssignment;
    ecSyntaxCheck             : Result:= srkmecSyntaxCheck;
    ecGuessUnclosedBlock      : Result:= lismenuguessunclosedblock;
    ecGuessMisplacedIFDEF     : Result:= srkmecGuessMisplacedIFDEF;
    ecConvertDFM2LFM          : Result:= lismenuConvertDFMToLFM;
    ecCheckLFM                : Result:= lisMenuCheckLFM;
    ecConvertDelphiUnit       : Result:= lisMenuConvertDelphiUnit;
    ecConvertDelphiProject    : Result:= lisMenuConvertDelphiProject;
    ecConvertDelphiPackage    : Result:= lisMenuConvertDelphiPackage;
    ecConvertEncoding         : Result:= lisMenuConvertEncoding;
    ecFindDeclaration         : Result:= srkmecFindDeclaration;
    ecFindBlockOtherEnd       : Result:= srkmecFindBlockOtherEnd;
    ecFindBlockStart          : Result:= srkmecFindBlockStart;
    ecShowAbstractMethods     : Result:= srkmecShowAbstractMethods;
    ecRemoveEmptyMethods      : Result:= srkmecRemoveEmptyMethods;
    ecRemoveUnusedUnits       : Result:= srkmecRemoveUnusedUnits;
    ecUseUnit                 : Result:= lisUseUnit;
    ecFindOverloads           : Result:= srkmecFindOverloads;

    // project (menu string resource)
    ecNewProject              : Result:= lisMenuNewProject;
    ecNewProjectFromFile      : Result:= lisMenuNewProjectFromFile;
    ecOpenProject             : Result:= lisMenuOpenProject;
    ecCloseProject            : Result:= lisMenuCloseProject;
    ecSaveProject             : Result:= lisMenuSaveProject;
    ecSaveProjectAs           : Result:= lisMenuSaveProjectAs;
    ecPublishProject          : Result:= lisMenuPublishProject;
    ecProjectInspector        : Result:= lisMenuProjectInspector;
    ecAddCurUnitToProj        : Result:= lisMenuAddToProject;
    ecRemoveFromProj          : Result:= lisMenuRemoveFromProject;
    ecViewProjectUnits        : Result:= srkmecViewUnits;
    ecViewProjectForms        : Result:= srkmecViewForms;
    ecViewProjectSource       : Result:= lisMenuViewProjectSource;
    ecProjectOptions          : Result:= lisMenuProjectOptions;

    // run menu (menu string resource)
    ecCompile                 : Result:= srkmecCompile;
    ecBuild                   : Result:= srkmecBuild;
    ecQuickCompile            : Result:= srkmecQuickCompile;
    ecCleanUpCompiled         : Result:= srkmecCleanUpCompiled;
    ecAbortBuild              : Result:= srkmecAbortBuild;
    ecRun                     : Result:= srkmecRun;
    ecPause                   : Result:= srkmecPause;
    ecShowExecutionPoint      : Result:= srkmecShowExecutionPoint;
    ecStepInto                : Result:= lisMenuStepInto;
    ecStepOver                : Result:= lisMenuStepOver;
    ecStepIntoInstr           : Result:= lisMenuStepIntoInstr;
    ecStepOverInstr           : Result:= lisMenuStepOverInstr;
    ecStepIntoContext         : Result:= lisMenuStepIntoContext;
    ecStepOverContext         : Result:= lisMenuStepOverContext;
    ecStepOut                 : Result:= lisMenuStepOut;
    ecRunToCursor             : Result:= lisMenuRunToCursor;
    ecStopProgram             : Result:= srkmecStopProgram;
    ecResetDebugger           : Result:= srkmecResetDebugger;
    ecRunParameters           : Result:= srkmecRunParameters;
    ecBuildFile               : Result:= srkmecBuildFile;
    ecRunFile                 : Result:= srkmecRunFile;
    ecConfigBuildFile         : Result:= srkmecConfigBuildFile;
    ecInspect                 : Result:= srkmecInspect;
    ecEvaluate                : Result:= srkmecEvaluate;
    ecAddWatch                : Result:= srkmecAddWatch;
    ecAddBpSource             : Result:= srkmecAddBpSource;
    ecAddBpAddress            : Result:= srkmecAddBpAddress;
    ecAddBpDataWatch          : Result:= srkmecAddBpWatchPoint;

    // components menu
    ecNewPackage              : Result:= lisKMNewPackage;
    ecOpenPackage             : Result:= lisMenuOpenPackage;
    ecOpenPackageFile         : Result:= lisMenuOpenPackageFile;
    ecOpenPackageOfCurUnit    : Result:= lisMenuOpenPackageOfCurUnit;
    ecAddCurFileToPkg         : Result:= lisMenuAddCurFileToPkg;
    ecPackageGraph            : Result:= lisMenuPackageGraph;
    ecEditInstallPkgs         : Result:= lisMenuEditInstallPkgs;
    ecConfigCustomComps       : Result:= lisMenuConfigCustomComps;

    // tools menu
    ecExtToolSettings         : Result:= srkmecExtToolSettings;
    ecManageExamples          : Result:= lisMenuExampleProjects;
    ecConfigBuildLazarus      : Result:= lismenuconfigurebuildlazarus;
    ecBuildLazarus            : Result:= srkmecBuildLazarus;
    ecExtToolFirst
    ..ecExtToolLast           : Result:= Format(srkmecExtTool,[cmd-ecExtToolFirst+1]);
    ecMakeResourceString      : Result:= srkmecMakeResourceString;
    ecDiff                    : Result:= srkmecDiff;

    // environment menu
    ecEnvironmentOptions      : Result:= srkmecEnvironmentOptions;
    ecRescanFPCSrcDir         : Result:= lisMenuRescanFPCSourceDirectory;
    ecEditCodeTemplates       : Result:= lisMenuEditCodeTemplates;
    ecCodeToolsDefinesEd      : Result:= srkmecCodeToolsDefinesEd;

    // help menu
    ecAboutLazarus            : Result:= lisAboutLazarus;
    ecOnlineHelp              : Result:= lisMenuOnlineHelp;
    ecContextHelp             : Result:= lisMenuContextHelp;
    ecEditContextHelp         : Result:= lisMenuEditContextHelp;
    ecReportingBug            : Result:= srkmecReportingBug;
    ecFocusHint               : Result:= lisFocusHint;

    // desginer
    ecDesignerCopy            : Result:= lisDsgCopyComponents;
    ecDesignerCut             : Result:= lisDsgCutComponents;
    ecDesignerPaste           : Result:= lisDsgPasteComponents;
    ecDesignerSelectParent    : Result:= lisDsgSelectParentComponent;
    ecDesignerMoveToFront     : Result:= lisDsgOrderMoveToFront;
    ecDesignerMoveToBack      : Result:= lisDsgOrderMoveToBack;
    ecDesignerForwardOne      : Result:= lisDsgOrderForwardOne;
    ecDesignerBackOne         : Result:= lisDsgOrderBackOne;

    else
      begin
        Result:= srkmecunknown;
        case TSynPluginTemplateEdit.ConvertCommandToBase(cmd) of
          // Edit template
          ecSynPTmplEdNextCell:           Result := srkmecSynPTmplEdNextCell;
          ecSynPTmplEdNextCellSel:        Result := srkmecSynPTmplEdNextCellSel;
          ecSynPTmplEdNextCellRotate:     Result := srkmecSynPTmplEdNextCellRotate;
          ecSynPTmplEdNextCellSelRotate:  Result := srkmecSynPTmplEdNextCellSelRotate;
          ecSynPTmplEdPrevCell:           Result := srkmecSynPTmplEdPrevCell;
          ecSynPTmplEdPrevCellSel:        Result := srkmecSynPTmplEdPrevCellSel;
          ecSynPTmplEdCellHome:           Result := srkmecSynPTmplEdCellHome;
          ecSynPTmplEdCellEnd:            Result := srkmecSynPTmplEdCellEnd;
          ecSynPTmplEdCellSelect:         Result := srkmecSynPTmplEdCellSelect;
          ecSynPTmplEdFinish:             Result := srkmecSynPTmplEdFinish;
          ecSynPTmplEdEscape:             Result := srkmecSynPTmplEdEscape;
        end;
        case TSynPluginTemplateEdit.ConvertCommandToBaseOff(cmd) of
          // Edit template
          ecSynPTmplEdNextCell:           Result := srkmecSynPTmplEdNextCell;
          ecSynPTmplEdNextCellSel:        Result := srkmecSynPTmplEdNextCellSel;
          ecSynPTmplEdNextCellRotate:     Result := srkmecSynPTmplEdNextCellRotate;
          ecSynPTmplEdNextCellSelRotate:  Result := srkmecSynPTmplEdNextCellSelRotate;
          ecSynPTmplEdPrevCell:           Result := srkmecSynPTmplEdPrevCell;
          ecSynPTmplEdPrevCellSel:        Result := srkmecSynPTmplEdPrevCellSel;
          ecSynPTmplEdCellHome:           Result := srkmecSynPTmplEdCellHome;
          ecSynPTmplEdCellEnd:            Result := srkmecSynPTmplEdCellEnd;
          ecSynPTmplEdCellSelect:         Result := srkmecSynPTmplEdCellSelect;
          ecSynPTmplEdFinish:             Result := srkmecSynPTmplEdFinish;
          ecSynPTmplEdEscape:             Result := srkmecSynPTmplEdEscape;
        end;

        case TSynPluginSyncroEdit.ConvertCommandToBase(cmd) of
          // SyncroEdit
          ecSynPSyncroEdNextCell:            Result := srkmecSynPSyncroEdNextCell;
          ecSynPSyncroEdNextCellSel:         Result := srkmecSynPSyncroEdNextCellSel;
          ecSynPSyncroEdPrevCell:            Result := srkmecSynPSyncroEdPrevCell;
          ecSynPSyncroEdPrevCellSel:         Result := srkmecSynPSyncroEdPrevCellSel;
          ecSynPSyncroEdCellHome:            Result := srkmecSynPSyncroEdCellHome;
          ecSynPSyncroEdCellEnd:             Result := srkmecSynPSyncroEdCellEnd;
          ecSynPSyncroEdCellSelect:          Result := srkmecSynPSyncroEdCellSelect;
          ecSynPSyncroEdEscape:              Result := srkmecSynPSyncroEdEscape;
        end;
        case TSynPluginSyncroEdit.ConvertCommandToBaseOff(cmd) of
          // SyncroEdit
          ecSynPSyncroEdNextCell:            Result := srkmecSynPSyncroEdNextCell;
          ecSynPSyncroEdNextCellSel:         Result := srkmecSynPSyncroEdNextCellSel;
          ecSynPSyncroEdPrevCell:            Result := srkmecSynPSyncroEdPrevCell;
          ecSynPSyncroEdPrevCellSel:         Result := srkmecSynPSyncroEdPrevCellSel;
          ecSynPSyncroEdCellHome:            Result := srkmecSynPSyncroEdCellHome;
          ecSynPSyncroEdCellEnd:             Result := srkmecSynPSyncroEdCellEnd;
          ecSynPSyncroEdCellSelect:          Result := srkmecSynPSyncroEdCellSelect;
          ecSynPSyncroEdEscape:              Result := srkmecSynPSyncroEdEscape;
        end;
        case TSynPluginSyncroEdit.ConvertCommandToBaseSel(cmd) of
          // SyncroEdit, during selection
          ecSynPSyncroEdStart:               Result := srkmecSynPSyncroEdStart;
        end;

      end;
  end;
end;

function FindKeymapConflicts(Keymap: TKeyCommandRelationList;
   Protocol: TStrings; out Index1,Index2:integer):integer;
// 0 = ok, no errors
// >0 number of errors found
var
  a,b:integer;
  Key1: TKeyCommandRelation;
  Key2: TKeyCommandRelation;

  procedure Add(const s: string);
  begin
    debugln(s);
    Protocol.Add(s);
  end;

  procedure Check(const ShortCut1, ShortCut2: TIDEShortCut);
  // check if ShortCut1 hides ShortCut2
  begin
    if (ShortCut1.Key1=VK_UNKNOWN) then exit;
    if (ShortCut1.Key1<>ShortCut2.Key1) or (ShortCut1.Shift1<>ShortCut2.Shift1)
    then exit;
    // first key fits
    if (ShortCut1.Key2=VK_UNKNOWN) or (ShortCut2.Key2=VK_UNKNOWN)
    or ((ShortCut1.Key2=ShortCut2.Key2) and (ShortCut1.Shift2=ShortCut2.Shift2))
    then begin
      // conflict found
      if Result=0 then begin
        Index1:=a;
        Index2:=b;
      end;
      inc(Result);
      if Protocol<>nil then
      begin
        Add(srkmConflic+IntToStr(Result));
        Add(srkmCommand1+Key1.Category.Description+'/'
          +EditorCommandToDescriptionString(Key1.Command)+'"'
          +'->'+KeyAndShiftStateToEditorKeyString(ShortCut1));
        Add(srkmConflicW);
        Add(srkmCommand2+Key2.Category.Description+'/'
          +EditorCommandToDescriptionString(Key2.Command)+'"'
          +'->'+KeyAndShiftStateToEditorKeyString(ShortCut2));
        Add('');
        Key1.Category.WriteScopeDebugReport;
        Key2.Category.WriteScopeDebugReport;
      end;
    end;
  end;

begin
  Result:=0;
  Index1:=0;
  Index2:=0;
  for a:=0 to Keymap.Count-1 do begin
    Key1:=Keymap[a];
    for b:=a+1 to Keymap.Count-1 do begin
      Key2:=Keymap[b];
      {if (Key2.Command=ecConfigBuildLazarus)
      and (Key1.Command=ecFindNext) then begin
        debugln('FindKeymapConflicts ',dbgs(Key1.Category.ScopeIntersects(Key2.Category.Scope)),' ',dbgsName(Key1.Category.Scope),' ',dbgsName(Key2.Category.Scope));
      end;}
      if (not Key1.Category.ScopeIntersects(Key2.Category.Scope)) then
        continue;
      Check(Key1.ShortcutA,Key2.ShortcutA);
      Check(Key1.ShortcutA,Key2.ShortcutB);
      Check(Key1.ShortcutB,Key2.ShortcutA);
      Check(Key1.ShortcutB,Key2.ShortcutB);
    end;
  end;
end;

function IDEShortCutEmpty(const Key: TIDEShortCut): boolean;
begin
  Result:=(Key.Key1=VK_UNKNOWN) and (Key.Key2=VK_UNKNOWN);
end;

function KeyAndShiftStateToEditorKeyString(const Key: TIDEShortCut): String;
begin
  Result := KeyAndShiftStateToKeyString(Key.Key1, Key.Shift1);
  if (Key.Key2 <> VK_UNKNOWN) then
    Result := Result + ', ' + KeyAndShiftStateToKeyString(Key.Key2, Key.Shift2);
end;

{ TKeyCommandRelation }

procedure TKeyCommandRelation.SetShortcutA(const AValue: TIDEShortCut);
begin
  inherited SetShortcutA(AValue);
  {if Command=12000 then begin
    debugln('TKeyCommandRelation.SetShortcutA ',KeyAndShiftStateToEditorKeyString(ShortcutA),' ',KeyAndShiftStateToEditorKeyString(ShortcutB));
    if AValue.Key1=VK_UNKNOWN then
      RaiseGDBException('');
  end;}
end;

procedure TKeyCommandRelation.SetShortcutB(const AValue: TIDEShortCut);
begin
  inherited SetShortcutB(AValue);
  {if Command=ecBlockIndent then begin
    debugln('TKeyCommandRelation.SetShortcutB ',KeyAndShiftStateToEditorKeyString(ShortcutA),' ',KeyAndShiftStateToEditorKeyString(ShortcutB));
    if ShortcutB.Key2=VK_UNKNOWN then
      RaiseGDBException('');
  end;}
end;
procedure TKeyCommandRelation.SetSingle(NewKeyA: word; NewShiftA: TShiftState;
                                        NewKeyB: word; NewShiftB: TShiftState);
begin
  ShortcutA:=IDEShortCut(NewKeyA,NewShiftA,VK_UNKNOWN,[]);
  ShortcutB:=IDEShortCut(NewKeyB,NewShiftB,VK_UNKNOWN,[]);
end;

procedure TKeyCommandRelation.SetSingle(NewKeyA: word; NewShiftA: TShiftState);
begin
  SetSingle(NewKeyA,NewShiftA,VK_UNKNOWN,[]);
end;

procedure TKeyCommandRelation.SetCombo(NewKey1A: word; NewShift1A: TShiftState;
                                       NewKey1B: word; NewShift1B: TShiftState;
                                       NewKey2A: word; NewShift2A: TShiftState;
                                       NewKey2B: word; NewShift2B: TShiftState);
begin
  ShortcutA:=IDEShortCut(NewKey1A,NewShift1A,NewKey1B,NewShift1B);
  ShortcutB:=IDEShortCut(NewKey2A,NewShift2A,NewKey2B,NewShift2B);
end;

procedure TKeyCommandRelation.SetCombo(NewKey1A: word; NewShift1A: TShiftState;
                                       NewKey1B: word; NewShift1B: TShiftState);
begin
  SetCombo(NewKey1A,NewShift1A,NewKey1B,NewShift1B,VK_UNKNOWN,[],VK_UNKNOWN,[]);
end;

procedure TKeyCommandRelation.MapShortcut(AScheme: TKeyMapScheme);
begin
  case AScheme of
    kmsLazarus: GetDefaultKeyForCommand;
    kmsClassic: GetDefaultKeyForClassicScheme;
    kmsMacOSXApple: GetDefaultKeyForMacOSXScheme;
    kmsMacOSXLaz: GetDefaultKeyForMacOSXLazScheme;
    kmsCustom: ;
  end;
end;

function TKeyCommandRelation.GetLocalizedName: string;
begin
  Result:=inherited GetLocalizedName;
  if Result='' then begin
    Result:=EditorCommandToDescriptionString(Command);
    if Result=srkmecunknown then
      Result:=Name;
  end;
end;

procedure TKeyCommandRelation.GetDefaultKeyForCommand;
begin
  {$IFDEF LCLCarbon}
  GetDefaultKeyForMacOSXScheme;
  {$ELSE}
  GetDefaultKeyForWindowsScheme;
  {$ENDIF}
end;

procedure TKeyCommandRelation.GetDefaultKeyForWindowsScheme;
begin
  case Command of
  // moving
  ecWordLeft: SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
  ecWordRight: SetSingle(VK_RIGHT, [ssCtrl],VK_UNKNOWN,[]); // WS c
  ecLineStart: SetSingle(VK_HOME, [],VK_UNKNOWN,[]);
  ecLineEnd: SetSingle(VK_END, [],VK_UNKNOWN,[]);
  ecPageUp: SetSingle(VK_PRIOR, [],VK_UNKNOWN,[]); // ,VK_R,[SSCtrl],VK_UNKNOWN,[]);
  ecPageDown: SetSingle(VK_NEXT, [],VK_UNKNOWN,[]); // ,VK_W,[SSCtrl],VK_UNKNOWN,[]);
  ecPageLeft: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageRight: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageTop: SetSingle(VK_PRIOR, [ssCtrl],VK_UNKNOWN,[]);
  ecPageBottom: SetSingle(VK_NEXT, [ssCtrl],VK_UNKNOWN,[]);
  ecEditorTop: SetSingle(VK_HOME,[ssCtrl],VK_UNKNOWN,[]);
  ecEditorBottom: SetSingle(VK_END,[ssCtrl],VK_UNKNOWN,[]);
  ecScrollUp: SetSingle(VK_UP, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollDown: SetSingle(VK_DOWN, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollLeft: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecScrollRight: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);

  // selection
  ecCopy: SetSingle(VK_C,[ssCtrl],VK_Insert,[ssCtrl]);
  ecCut: SetSingle(VK_X,[ssCtrl],VK_Delete,[ssShift]);
  ecPaste: SetSingle(VK_V,[ssCtrl],VK_Insert,[ssShift]);
  ecNormalSelect: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecColumnSelect: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecLineSelect: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelWordLeft: SetSingle(VK_LEFT,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecSelWordRight: SetSingle(VK_RIGHT,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecSelLineStart: SetSingle(VK_HOME,[ssShift],VK_UNKNOWN,[]);
  ecSelLineEnd: SetSingle(VK_END,[ssShift],VK_UNKNOWN,[]);
  ecSelPageTop: SetSingle(VK_PRIOR, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelPageBottom: SetSingle(VK_NEXT, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelEditorTop: SetSingle(VK_HOME, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelEditorBottom: SetSingle(VK_END, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelectAll: SetSingle(VK_A,[ssCtrl],VK_UNKNOWN,[]);
  ecSelectToBrace: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectCodeBlock: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectWord: SetCombo(VK_K,[SSCtrl],VK_T,[]);
  ecSelectLine: SetCombo(VK_K,[SSCtrl],VK_L,[]);
  ecSelectParagraph: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectionUpperCase: SetCombo(VK_K,[SSCtrl],VK_N,[]);
  ecSelectionLowerCase: SetCombo(VK_K,[SSCtrl],VK_O,[]);
  ecSelectionSwapCase: SetCombo(VK_K,[SSCtrl],VK_P,[]);
  ecSelectionTabs2Spaces: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionEnclose: SetSingle(VK_N, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelectionComment: SetSingle(VK_V, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelectionUncomment: SetSingle(VK_U, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecToggleComment: SetSingle(VK_W, [ssShift,ssCtrl], VK_OEM_2, [ssCtrl]);
  ecSelectionEncloseIFDEF: SetSingle(VK_D, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelectionSort: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionBreakLines: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);

  ecBlockSetBegin   : SetCombo(VK_K,[ssCtrl],VK_B,[],  VK_K,[ssCtrl],VK_B,[ssCtrl]);
  ecBlockSetEnd     : SetCombo(VK_K,[ssCtrl],VK_K,[],  VK_K,[ssCtrl],VK_K,[ssCtrl]);
  ecBlockToggleHide : SetCombo(VK_K,[ssCtrl],VK_H,[],  VK_K,[ssCtrl],VK_H,[ssCtrl]);
  ecBlockHide       : SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockShow       : SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockMove       : SetCombo(VK_K,[ssCtrl],VK_V,[],  VK_K,[ssCtrl],VK_V,[ssCtrl]);
  ecBlockCopy       : SetCombo(VK_K,[ssCtrl],VK_C,[],  VK_K,[ssCtrl],VK_C,[ssCtrl]);
  ecBlockDelete     : SetCombo(VK_K,[ssCtrl],VK_Y,[],  VK_K,[ssCtrl],VK_Y,[ssCtrl]);
  ecBlockGotoBegin  : SetCombo(VK_Q,[ssCtrl],VK_B,[ssCtrl],  VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockGotoEnd    : SetCombo(VK_Q,[ssCtrl],VK_K,[ssCtrl],  VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // column mode selection
  ecColSelUp: SetSingle(VK_UP, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelDown: SetSingle(VK_DOWN, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelLeft: SetSingle(VK_LEFT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelRight: SetSingle(VK_RIGHT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageDown: SetSingle(VK_NEXT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageBottom: SetSingle(VK_NEXT, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelPageUp: SetSingle(VK_PRIOR, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageTop: SetSingle(VK_PRIOR, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelLineStart: SetSingle(VK_HOME, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelLineEnd: SetSingle(VK_END, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelEditorTop: SetSingle(VK_HOME, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelEditorBottom: SetSingle(VK_END, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);

  // editing
  ecBlockIndent: SetCombo(VK_I,[ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_I,[]);
  ecBlockUnindent: SetCombo(VK_U,[ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_U,[]);
  ecDeleteLastChar: SetSingle(VK_BACK, [],VK_BACK, [ssShift]);  // ctrl H used for scroll window.
  ecDeleteChar: SetSingle(VK_DELETE,[],VK_UNKNOWN,[]); // ctrl G conflicts with GO
  ecDeleteWord: SetSingle(VK_T,[ssCtrl],VK_UNKNOWN,[]);
  ecDeleteLastWord: SetSingle(VK_BACK,[ssCtrl],VK_UNKNOWN,[]);
  ecDeleteBOL: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDeleteEOL: SetCombo(VK_Y,[ssCtrl,ssShift],VK_UNKNOWN,[],VK_Q,[ssCtrl],VK_Y,[]);
  ecDeleteLine: SetSingle(VK_Y,[ssCtrl],VK_UNKNOWN,[]);
  ecClearAll: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecLineBreak: SetSingle(VK_RETURN,[],VK_UNKNOWN,[]);
  ecInsertLine: SetSingle(VK_N,[ssCtrl],VK_UNKNOWN,[]);
  ecInsertCharacter: SetSingle(VK_M,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecInsertGPLNotice: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertLGPLNotice: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertModifiedLGPLNotice: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertUserName: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertDateTime: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertChangeLogEntry: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSAuthor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSDate: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSHeader: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSID: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSLog: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSName: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSRevision: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSSource: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertGUID: SetSingle(VK_G, [ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecInsertFilename: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // command commands
  ecUndo: SetSingle(VK_Z,[ssCtrl],VK_UNKNOWN,[]);
  ecRedo: SetSingle(VK_Z,[ssCtrl,ssShift],VK_UNKNOWN,[]);

  // search & replace
  ecMatchBracket: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFind: SetCombo(VK_F,[SSCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_F,[]);
  ecFindNext: SetCombo(VK_F3,[],VK_UNKNOWN,[],VK_L,[SSCtrl],VK_UNKNOWN,[]);
  ecFindPrevious: SetSingle(VK_F3,[ssShift],VK_UNKNOWN,[]);
  ecFindInFiles: SetSingle(VK_F,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecReplace: SetCombo(VK_R,[SSCtrl],VK_UNKNOWN,[],  VK_Q,[SSCtrl],VK_A,[]);
  ecIncrementalFind: SetSingle(VK_E,[SSCtrl],VK_UNKNOWN,[]);
  ecGotoLineNumber: SetCombo(VK_G,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_G,[]);
  ecFindNextWordOccurrence: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindPrevWordOccurrence: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpBack: SetSingle(VK_H,[ssCtrl],VK_UNKNOWN,[]);
  ecJumpForward: SetSingle(VK_H,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecAddJumpPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpToPrevError: SetSingle(VK_F8,[ssCtrl, ssShift],VK_UNKNOWN,[]);
  ecJumpToNextError: SetSingle(VK_F8,[ssCtrl],VK_UNKNOWN,[]);
  ecOpenFileAtCursor: SetCombo(VK_RETURN,[ssCtrl],VK_UNKNOWN,[]);
  ecProcedureList: SetSingle(VK_G, [ssAlt],VK_UNKNOWN,[]);

  // marker
  ecSetFreeBookmark: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevBookmark: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextBookmark: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker0: SetCombo(VK_0,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_0,[]);
  ecGotoMarker1: SetCombo(VK_1,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_1,[]);
  ecGotoMarker2: SetCombo(VK_2,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_2,[]);
  ecGotoMarker3: SetCombo(VK_3,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_3,[]);
  ecGotoMarker4: SetCombo(VK_4,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_4,[]);
  ecGotoMarker5: SetCombo(VK_5,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_5,[]);
  ecGotoMarker6: SetCombo(VK_6,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_6,[]);
  ecGotoMarker7: SetCombo(VK_7,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_7,[]);
  ecGotoMarker8: SetCombo(VK_8,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_8,[]);
  ecGotoMarker9: SetCombo(VK_9,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_9,[]);
  ecToggleMarker0: SetCombo(VK_0,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_0,[]);
  ecToggleMarker1: SetCombo(VK_1,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_1,[]);
  ecToggleMarker2: SetCombo(VK_2,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_2,[]);
  ecToggleMarker3: SetCombo(VK_3,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_3,[]);
  ecToggleMarker4: SetCombo(VK_4,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_4,[]);
  ecToggleMarker5: SetCombo(VK_5,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_5,[]);
  ecToggleMarker6: SetCombo(VK_6,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_6,[]);
  ecToggleMarker7: SetCombo(VK_7,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_7,[]);
  ecToggleMarker8: SetCombo(VK_8,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_8,[]);
  ecToggleMarker9: SetCombo(VK_9,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_9,[]);
  ecSetMarker0: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker1: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker2: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker3: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker4: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker5: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker6: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker7: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker8: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker9: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // codetools
  ecAutoCompletion: SetSingle(VK_J,[ssCtrl],VK_UNKNOWN,[]);
  ecWordCompletion: SetSingle(VK_W,[ssCtrl],VK_UNKNOWN,[]);
  ecCompleteCode: SetSingle(VK_C,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecIdentCompletion: SetSingle(VK_SPACE,[ssCtrl],VK_UNKNOWN,[]);
  ecShowCodeContext: SetSingle(VK_SPACE,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecExtractProc: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindIdentifierRefs: SetSingle(VK_I,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecRenameIdentifier: SetSingle(VK_F2,[],VK_E,[ssShift,ssCtrl]);
  ecInvertAssignment: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSyntaxCheck: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessUnclosedBlock: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessMisplacedIFDEF: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDFM2LFM: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCheckLFM: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiUnit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiPackage: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertEncoding: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindProcedureDefinition: SetSingle(VK_UP,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  ecFindProcedureMethod: SetSingle(VK_DOWN,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  ecFindDeclaration: SetSingle(VK_UP,[ssAlt],VK_UNKNOWN,[]);
  ecFindBlockOtherEnd: SetCombo(VK_Q,[ssCtrl],VK_K,[]);
  ecFindBlockStart: SetCombo(VK_Q,[ssCtrl],VK_B,[]);
  ecGotoIncludeDirective: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowAbstractMethods: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveEmptyMethods: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveUnusedUnits: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecUseUnit: SetSingle(VK_F11,[ssAlt],VK_UNKNOWN,[]);
  ecFindOverloads: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // source notebook
  ecNextEditor: SetSingle(VK_TAB, [ssCtrl], VK_UNKNOWN, []);
  ecPrevEditor: SetSingle(VK_TAB, [ssShift,ssCtrl], VK_UNKNOWN, []);
  ecResetDebugger: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleBreakPoint: SetSingle(VK_F5,[],VK_UNKNOWN,[]);
  ecMoveEditorLeft: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorRight: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorLeftmost: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorRightmost: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);

  ecNextSharedEditor:        SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevSharedEditor:        SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextWindow:              SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevWindow:              SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNextWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorPrevWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNewWindow:     SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNextWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorPrevWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNewWindow:     SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  ecGotoEditor1: SetSingle(VK_1,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor2: SetSingle(VK_2,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor3: SetSingle(VK_3,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor4: SetSingle(VK_4,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor5: SetSingle(VK_5,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor6: SetSingle(VK_6,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor7: SetSingle(VK_7,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor8: SetSingle(VK_8,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor9: SetSingle(VK_9,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor0: SetSingle(VK_0,[ssAlt],VK_UNKNOWN,[]);

  ecLockEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  EcFoldLevel1: SetSingle(VK_1,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel2: SetSingle(VK_2,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel3: SetSingle(VK_3,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel4: SetSingle(VK_4,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel5: SetSingle(VK_5,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel6: SetSingle(VK_6,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel7: SetSingle(VK_7,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel8: SetSingle(VK_8,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel9: SetSingle(VK_9,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel0: SetSingle(VK_0,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldCurrent: SetSingle(VK_OEM_MINUS,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcUnFoldCurrent: SetSingle(VK_OEM_PLUS,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcToggleMarkupWord: SetSingle(VK_M,[ssAlt],VK_UNKNOWN,[]);

  // file menu
  ecNew: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewUnit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewForm: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpen: SetSingle(VK_O,[ssCtrl],VK_UNKNOWN,[]);
  ecRevert: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSave: SetSingle(VK_S,[ssCtrl],VK_UNKNOWN,[]);
  ecSaveAs: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveAll: SetSingle(VK_S,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecClose: SetSingle(VK_F4,[ssCtrl],VK_UNKNOWN,[]);
  ecCloseAll: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCleanDirectory: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRestart: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // view menu
  ecToggleObjectInsp: SetSingle(VK_F11,[],VK_UNKNOWN,[]);
  ecToggleSourceEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeExpl: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFPDocEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMessages: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewComponents: SetSingle(VK_P,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecViewJumpHistory: SetSingle(VK_J,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleSearchResults: SetSingle(VK_F,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleWatches: SetSingle(VK_W,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleBreakPoints: SetSingle(VK_B,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleLocals: SetSingle(VK_L,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecViewPseudoTerminal: if HasConsoleSupport then SetSingle(VK_O,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecViewThreads: SetSingle(VK_T,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleCallStack: SetSingle(VK_S,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleRegisters: SetSingle(VK_R,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleAssembler: SetSingle(VK_D,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleDebugEvents: SetSingle(VK_V,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleDebuggerOut: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewHistory: SetSingle(VK_H,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecViewUnitDependencies: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnitInfo: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFormUnit: SetSingle(VK_F12,[],VK_UNKNOWN,[]);
  ecViewAnchorEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeBrowser: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleRestrictionBrowser: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCompPalette: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleIDESpeedBtns: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // project menu
  ecNewProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewProjectFromFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenProject: SetSingle(VK_F11,[ssCtrl],VK_UNKNOWN,[]);
  ecCloseProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProjectAs: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPublishProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectInspector: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurUnitToProj: SetSingle(VK_F11,[ssShift],VK_UNKNOWN,[]);
  ecRemoveFromProj: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewProjectUnits: SetSingle(VK_F12,[ssCtrl],VK_UNKNOWN,[]);
  ecViewProjectForms: SetSingle(VK_F12,[ssShift],VK_UNKNOWN,[]);
  ecViewProjectSource: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectOptions: SetSingle(VK_F11,[ssShift,ssCtrl],VK_UNKNOWN,[]);

  // run menu
  ecCompile: SetSingle(VK_F9,[ssCtrl],VK_UNKNOWN,[]);
  ecBuild: SetSingle(VK_F9,[ssShift],VK_UNKNOWN,[]);
  ecQuickCompile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCleanUpCompiled: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAbortBuild: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRun: SetSingle(VK_F9,[],VK_UNKNOWN,[]);
  ecPause: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowExecutionPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepInto: SetSingle(VK_F7,[],VK_UNKNOWN,[]);
  ecStepOver: SetSingle(VK_F8,[],VK_UNKNOWN,[]);
  ecStepIntoInstr: SetSingle(VK_F7,[ssAlt],VK_UNKNOWN,[]);
  ecStepOverInstr: SetSingle(VK_F8,[ssAlt],VK_UNKNOWN,[]);
  ecStepOut: SetSingle(VK_F8,[ssShift],VK_UNKNOWN,[]);
  ecRunToCursor: SetSingle(VK_F4,[],VK_UNKNOWN,[]);
  ecStopProgram: SetSingle(VK_F2,[SSCtrl],VK_UNKNOWN,[]);
  ecRemoveBreakPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunParameters: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInspect: SetSingle(VK_F5,[ssAlt],VK_UNKNOWN,[]);
  ecEvaluate: SetSingle(VK_F7,[ssCtrl],VK_UNKNOWN,[]);
  ecAddWatch: SetSingle(VK_F5,[ssCtrl],VK_UNKNOWN,[]);
  ecAddBpSource: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddBpAddress: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddBpDataWatch: SetSingle(VK_F5,[ssShift],VK_UNKNOWN,[]);

  // components menu
  ecNewPackage: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackage: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageOfCurUnit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurFileToPkg: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPackageGraph: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditInstallPkgs: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigCustomComps: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // tools menu
  ecExtToolSettings: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecManageExamples: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildLazarus: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildLazarus: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMakeResourceString: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDiff: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // environment (in Tools menu)
  ecEnvironmentOptions: SetSingle(VK_O,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecRescanFPCSrcDir: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditCodeTemplates: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCodeToolsDefinesEd: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // help menu
  ecAboutLazarus: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOnlineHelp: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecContextHelp: SetSingle(VK_F1,[],VK_UNKNOWN,[]);
  ecEditContextHelp: SetSingle(VK_F1,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecReportingBug: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFocusHint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // designer
  ecDesignerCopy        : SetSingle(VK_C,[ssCtrl],VK_Insert,[ssCtrl]);
  ecDesignerCut         : SetSingle(VK_X,[ssCtrl],VK_Delete,[ssShift]);
  ecDesignerPaste       : SetSingle(VK_V,[ssCtrl],VK_Insert,[ssShift]);
  ecDesignerSelectParent: SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
  ecDesignerMoveToFront : SetSingle(VK_PRIOR,[ssShift],VK_UNKNOWN,[]);
  ecDesignerMoveToBack  : SetSingle(VK_NEXT,[ssShift],VK_UNKNOWN,[]);
  ecDesignerForwardOne  : SetSingle(VK_PRIOR,[ssCtrl],VK_UNKNOWN,[]);
  ecDesignerBackOne     : SetSingle(VK_NEXT,[ssCtrl],VK_UNKNOWN,[]);

  else
    begin
      SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
      case TSynPluginTemplateEdit.ConvertCommandToBase(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetSingle(VK_HOME, [],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetSingle(VK_END,  [],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetSingle(VK_A,    [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetSingle(VK_RETURN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginTemplateEdit.ConvertCommandToBaseOff(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetSingle(VK_RETURN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBase(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetSingle(VK_HOME, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetSingle(VK_END,  [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetSingle(VK_A,    [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseOff(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseSel(Command) of
        // SyncroEdit, during selection
        ecSynPSyncroEdStart:               SetSingle(VK_J,[ssCtrl],VK_UNKNOWN,[]);
      end;
    end;
  end;
end;

procedure TKeyCommandRelation.GetDefaultKeyForClassicScheme;
begin
  GetDefaultKeyForWindowsScheme;

  case Command of
  // moving
  ecLeft: SetSingle(VK_S,[ssCtrl], VK_LEFT,[]);
  ecRight: SetCombo(VK_D, [ssCtrl], VK_UNKNOWN, [], VK_RIGHT, [], VK_UNKNOWN,[]);
  ecUp: SetCombo(VK_E, [ssCtrl], VK_UNKNOWN, [], VK_UP, [], VK_UNKNOWN,[]);
  ecDown: SetCombo(VK_X, [ssCtrl], VK_UNKNOWN, [], VK_DOWN, [], VK_UNKNOWN,[]);
  ecWordLeft: SetCombo(VK_A, [ssCtrl], VK_UNKNOWN, [], VK_LEFT, [ssCtrl], VK_UNKNOWN,[]);
  ecWordRight: SetCombo(VK_F, [ssCtrl], VK_UNKNOWN, [], VK_RIGHT, [ssCtrl],VK_UNKNOWN,[]);
  ecLineStart: SetCombo(VK_Q, [ssCtrl], VK_S, [], VK_HOME, [],VK_UNKNOWN,[]);
  ecLineEnd: SetCombo(VK_Q, [ssCtrl], VK_D, [], VK_END, [],VK_UNKNOWN,[]);
  ecPageUp: SetCombo(VK_R, [ssCtrl], VK_UNKNOWN, [], VK_PRIOR, [],VK_UNKNOWN,[]);
  ecPageDown: SetCombo(VK_C, [ssCtrl], VK_UNKNOWN, [], VK_NEXT, [],VK_UNKNOWN,[]);
  // Paragraph Down: VK_B, [ssCtrl]
  ecPageLeft: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageRight: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageTop: SetCombo(VK_Q, [ssCtrl], VK_E, [], VK_HOME, [ssCtrl],VK_UNKNOWN,[]);
  ecPageBottom: SetCombo(VK_Q, [ssCtrl], VK_X, [], VK_END, [ssCtrl],VK_UNKNOWN,[]);
  ecEditorTop: SetCombo(VK_Q, [ssCtrl], VK_R, [], VK_PRIOR,[ssCtrl],VK_UNKNOWN,[]);
  ecEditorBottom: SetCombo(VK_Q, [ssCtrl], VK_C, [], VK_NEXT,[ssCtrl],VK_UNKNOWN,[]);
  ecScrollUp: SetCombo(VK_W, [ssCtrl], VK_UNKNOWN, [], VK_UP, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollDown: SetCombo(VK_Z, [ssCtrl], VK_UNKNOWN, [], VK_DOWN, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollLeft: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecScrollRight: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN,[]);

  // selection
  ecCopy: SetCombo(VK_Insert,[ssCtrl],VK_UNKNOWN, [],VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecCut: SetCombo(VK_Delete,[ssShift],VK_UNKNOWN, [],VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecPaste: SetCombo(VK_Insert,[ssShift],VK_UNKNOWN, [],VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecNormalSelect: SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecColumnSelect: SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecLineSelect: SetCombo(VK_K,[ssCtrl],VK_L,[], VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecSelWordLeft: SetCombo(VK_LEFT,[ssCtrl,ssShift],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelWordRight: SetCombo(VK_RIGHT,[ssCtrl,ssShift],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelLineStart: SetCombo(VK_HOME,[ssShift],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelLineEnd: SetCombo(VK_END,[ssShift],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelPageTop: SetCombo(VK_HOME, [ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelPageBottom: SetCombo(VK_END, [ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelEditorTop: SetSingle(VK_PRIOR, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelEditorBottom: SetCombo(VK_NEXT, [ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectAll: SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectToBrace: SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectCodeBlock: SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectWord: SetCombo(VK_K,[ssCtrl],VK_T,[], VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectLine: SetCombo(VK_O,[ssCtrl],VK_L,[], VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectParagraph: SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionUpperCase: SetCombo(VK_K,[ssCtrl],VK_N,[], VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionLowerCase: SetCombo(VK_K,[ssCtrl],VK_O,[], VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionSwapCase: SetCombo(VK_K,[SSCtrl],VK_P,[]);
  ecSelectionTabs2Spaces: SetCombo(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionEnclose: SetCombo(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionComment: SetCombo(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionUncomment: SetCombo(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecToggleComment: SetCombo(VK_OEM_2, [ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionEncloseIFDEF: SetCombo(VK_D, [ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionSort: SetCombo(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionBreakLines: SetCombo(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);

  ecBlockSetBegin   : SetCombo(VK_K,[ssCtrl],VK_B,[], VK_K,[ssCtrl],VK_B,[ssCtrl]);
  ecBlockSetEnd     : SetCombo(VK_K,[ssCtrl],VK_K,[], VK_K,[ssCtrl],VK_K,[ssCtrl]);
  ecBlockToggleHide : SetCombo(VK_K,[ssCtrl],VK_H,[], VK_K,[ssCtrl],VK_H,[ssCtrl]);
  ecBlockHide       : SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockShow       : SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockMove       : SetCombo(VK_K,[ssCtrl],VK_V,[], VK_K,[ssCtrl],VK_V,[ssCtrl]);
  ecBlockCopy       : SetCombo(VK_K,[ssCtrl],VK_C,[], VK_K,[ssCtrl],VK_C,[ssCtrl]);
  ecBlockDelete     : SetCombo(VK_K,[ssCtrl],VK_Y,[], VK_K,[ssCtrl],VK_Y,[ssCtrl]);
  ecBlockGotoBegin  : SetCombo(VK_Q,[ssCtrl],VK_B,[ssCtrl],  VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockGotoEnd    : SetCombo(VK_Q,[ssCtrl],VK_K,[ssCtrl],  VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // column mode selection
  ecColSelUp:          SetCombo(VK_UP,    [ssAlt, ssShift],       VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelDown:        SetCombo(VK_DOWN,  [ssAlt, ssShift],       VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelLeft:        SetCombo(VK_LEFT,  [ssAlt, ssShift],       VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelRight:       SetCombo(VK_RIGHT, [ssAlt, ssShift],       VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelPageDown:    SetCombo(VK_NEXT,  [ssAlt, ssShift],       VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelPageBottom:  SetCombo(VK_NEXT,  [ssAlt, ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelPageUp:      SetCombo(VK_PRIOR, [ssAlt, ssShift],       VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelPageTop:     SetCombo(VK_PRIOR, [ssAlt, ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelLineStart:   SetCombo(VK_HOME,  [ssAlt, ssShift],       VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelLineEnd:     SetCombo(VK_END,   [ssAlt, ssShift],       VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelEditorTop:   SetCombo(VK_HOME,  [ssAlt, ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelEditorBottom:SetCombo(VK_END,   [ssAlt, ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);

  // editing
  ecInsertMode:       SetSingle(VK_V,[ssCtrl],         VK_INSERT,[]);
  ecBlockIndent:      SetCombo(VK_K,[ssCtrl],VK_I,[],  VK_K,[ssCtrl],VK_I,[ssCtrl]);
  ecBlockUnindent:    SetCombo(VK_K,[ssCtrl],VK_U,[],  VK_K,[ssCtrl],VK_U,[ssCtrl]);
  ecDeleteLastChar:   SetSingle(VK_H,[ssCtrl],         VK_BACK,[]);
  ecDeleteChar:       SetSingle(VK_G,[ssCtrl],         VK_DELETE,[]);
  ecDeleteWord:       SetSingle(VK_T,[ssCtrl],         VK_UNKNOWN,[]);
  ecDeleteLastWord:   SetSingle(VK_BACK,[ssCtrl],      VK_UNKNOWN,[]);
  ecDeleteBOL:        SetCombo(VK_Q,[ssCtrl],VK_H,[],  VK_Q,[ssCtrl],VK_H,[ssCtrl]);
  ecDeleteEOL:        SetCombo(VK_Q,[ssCtrl],VK_Y,[],  VK_Q,[ssCtrl],VK_Y,[ssCtrl]);
  ecDeleteLine:       SetSingle(VK_Y,[ssCtrl],         VK_UNKNOWN,[]);
  ecClearAll:         SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecLineBreak:        SetCombo(VK_RETURN,[],VK_UNKNOWN,[], VK_M,[ssCtrl],VK_UNKNOWN,[]);
  ecInsertLine:       SetSingle(VK_N,[ssCtrl],         VK_UNKNOWN,[]);
  ecInsertCharacter:  SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertGPLNotice:  SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertLGPLNotice: SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertModifiedLGPLNotice: SetSingle(VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecInsertUserName:   SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertDateTime:   SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertChangeLogEntry: SetSingle(VK_UNKNOWN,[],     VK_UNKNOWN,[]);
  ecInsertCVSAuthor:  SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertCVSDate:    SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertCVSHeader:  SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertCVSID:      SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertCVSLog:     SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertCVSName:    SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertCVSRevision:SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);
  ecInsertCVSSource:  SetSingle(VK_UNKNOWN,[],         VK_UNKNOWN,[]);

  // command commands
  ecUndo: SetSingle(VK_BACK,[ssALT],        VK_U,[ssCtrl]);
  ecRedo: SetSingle(VK_BACK,[ssALT,ssShift],VK_UNKNOWN,[]);

  // search & replace
  ecMatchBracket:           SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFind:                   SetCombo(VK_Q,[SSCtrl],VK_F,[]);
  ecFindNext:               SetSingle(VK_L,[ssCtrl],VK_UNKNOWN,[]);
  ecFindPrevious:           SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindInFiles:            SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecReplace:                SetCombo(VK_Q,[SSCtrl],VK_A,[]);
  ecIncrementalFind:        SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoLineNumber:         SetCombo(VK_Q,[ssCtrl],VK_G,[]);
  ecFindNextWordOccurrence: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindPrevWordOccurrence: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpBack:               SetSingle(VK_B,[ssCtrl]);
  ecJumpForward:            SetSingle(VK_B,[ssShift,ssCtrl]);
  ecAddJumpPoint:           SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpToPrevError:        SetSingle(VK_F7,[ssShift,ssAlt],VK_UNKNOWN,[]);
  ecJumpToNextError:        SetSingle(VK_F8,[ssShift,ssAlt],VK_UNKNOWN,[]);
  ecOpenFileAtCursor:       SetSingle(VK_RETURN,[ssCtrl],VK_UNKNOWN,[]);

  // marker
  ecSetFreeBookmark: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevBookmark: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextBookmark: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker0: SetCombo(VK_Q,[ssCtrl],VK_0,[]);
  ecGotoMarker1: SetCombo(VK_Q,[ssCtrl],VK_1,[]);
  ecGotoMarker2: SetCombo(VK_Q,[ssCtrl],VK_2,[]);
  ecGotoMarker3: SetCombo(VK_Q,[ssCtrl],VK_3,[]);
  ecGotoMarker4: SetCombo(VK_Q,[ssCtrl],VK_4,[]);
  ecGotoMarker5: SetCombo(VK_Q,[ssCtrl],VK_5,[]);
  ecGotoMarker6: SetCombo(VK_Q,[ssCtrl],VK_6,[]);
  ecGotoMarker7: SetCombo(VK_Q,[ssCtrl],VK_7,[]);
  ecGotoMarker8: SetCombo(VK_Q,[ssCtrl],VK_8,[]);
  ecGotoMarker9: SetCombo(VK_Q,[ssCtrl],VK_9,[]);
  ecSetMarker0..ecSetMarker9: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker0: SetCombo(VK_K,[ssCtrl],VK_0,[]);
  ecToggleMarker1: SetCombo(VK_K,[ssCtrl],VK_1,[]);
  ecToggleMarker2: SetCombo(VK_K,[ssCtrl],VK_2,[]);
  ecToggleMarker3: SetCombo(VK_K,[ssCtrl],VK_3,[]);
  ecToggleMarker4: SetCombo(VK_K,[ssCtrl],VK_4,[]);
  ecToggleMarker5: SetCombo(VK_K,[ssCtrl],VK_5,[]);
  ecToggleMarker6: SetCombo(VK_K,[ssCtrl],VK_6,[]);
  ecToggleMarker7: SetCombo(VK_K,[ssCtrl],VK_7,[]);
  ecToggleMarker8: SetCombo(VK_K,[ssCtrl],VK_8,[]);
  ecToggleMarker9: SetCombo(VK_K,[ssCtrl],VK_9,[]);

  // codetools
  ecAutoCompletion: SetSingle(VK_J,[ssCtrl],VK_UNKNOWN,[]);
  ecWordCompletion: SetSingle(VK_W,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecCompleteCode: SetSingle(VK_C,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecIdentCompletion: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowCodeContext: SetSingle(VK_SPACE,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecExtractProc: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindIdentifierRefs: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRenameIdentifier: SetSingle(VK_E,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecInvertAssignment: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSyntaxCheck: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessUnclosedBlock: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessMisplacedIFDEF: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDFM2LFM: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCheckLFM: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiUnit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiPackage: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertEncoding: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindProcedureDefinition: SetSingle(VK_UP,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  ecFindProcedureMethod: SetSingle(VK_DOWN,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  ecFindDeclaration: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindBlockOtherEnd: SetCombo(VK_Q,[ssCtrl],VK_K,[]);
  ecFindBlockStart: SetCombo(VK_Q,[ssCtrl],VK_B,[]);
  ecGotoIncludeDirective: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowAbstractMethods: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveEmptyMethods: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // source notebook
  ecNextEditor: SetCombo(VK_F6,[],VK_UNKNOWN,[],VK_TAB, [ssCtrl], VK_UNKNOWN, []);
  ecPrevEditor: SetCombo(VK_F6,[ssShift],VK_UNKNOWN,[],VK_TAB, [ssShift,ssCtrl], VK_UNKNOWN, []);
  ecResetDebugger: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleBreakPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorLeft: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorRight: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorLeftmost: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorRightmost: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);

  ecNextSharedEditor:        SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevSharedEditor:        SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextWindow:              SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevWindow:              SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNextWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorPrevWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNewWindow:     SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNextWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorPrevWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNewWindow:     SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  ecGotoEditor1: SetSingle(VK_1,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor2: SetSingle(VK_2,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor3: SetSingle(VK_3,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor4: SetSingle(VK_4,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor5: SetSingle(VK_5,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor6: SetSingle(VK_6,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor7: SetSingle(VK_7,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor8: SetSingle(VK_8,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor9: SetSingle(VK_9,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor0: SetSingle(VK_0,[ssAlt],VK_UNKNOWN,[]);

  ecLockEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  EcFoldLevel1: SetSingle(VK_1,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel2: SetSingle(VK_2,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel3: SetSingle(VK_3,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel4: SetSingle(VK_4,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel5: SetSingle(VK_5,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel6: SetSingle(VK_6,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel7: SetSingle(VK_7,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel8: SetSingle(VK_8,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel9: SetSingle(VK_9,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel0: SetSingle(VK_0,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldCurrent: SetSingle(VK_OEM_PLUS,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcUnFoldCurrent: SetSingle(VK_OEM_MINUS,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcToggleMarkupWord: SetSingle(VK_M,[ssAlt],VK_UNKNOWN,[]);

  // file menu
  ecNew: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewUnit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewForm: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpen: SetSingle(VK_F3,[],VK_UNKNOWN,[]);
  ecRevert: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSave: SetSingle(VK_F2,[],VK_UNKNOWN,[]);
  ecSaveAs: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveAll: SetSingle(VK_F2,[ssShift],VK_UNKNOWN,[]);
  ecClose: SetSingle(VK_F3,[ssAlt],VK_UNKNOWN,[]);
  ecCloseAll: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCleanDirectory: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRestart: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuit: SetSingle(VK_X,[ssAlt],VK_UNKNOWN,[]);

  // view menu
  ecToggleObjectInsp: SetSingle(VK_F11,[],VK_UNKNOWN,[]);
  ecToggleSourceEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeExpl: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFPDocEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMessages: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewComponents: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewJumpHistory: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleSearchResults: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleWatches: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleBreakPoints: SetSingle(VK_F8,[ssCtrl],VK_UNKNOWN,[]);
  ecToggleLocals: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCallStack: SetSingle(VK_F3,[ssCtrl],VK_UNKNOWN,[]);
  ecToggleRegisters: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleAssembler: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleDebugEvents: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleDebuggerOut: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnitDependencies: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnitInfo: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFormUnit: SetSingle(VK_F12,[],VK_UNKNOWN,[]);
  ecViewAnchorEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeBrowser: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleRestrictionBrowser: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCompPalette: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleIDESpeedBtns: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // project menu
  ecNewProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewProjectFromFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenProject: SetSingle(VK_F11,[ssCtrl],VK_UNKNOWN,[]);
  ecCloseProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProjectAs: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPublishProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectInspector: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurUnitToProj: SetSingle(VK_F11,[ssShift],VK_UNKNOWN,[]);
  ecRemoveFromProj: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewProjectUnits: SetSingle(VK_F12,[ssCtrl],VK_UNKNOWN,[]);
  ecViewProjectForms: SetSingle(VK_F12,[ssShift],VK_UNKNOWN,[]);
  ecViewProjectSource: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectOptions: SetSingle(VK_F11,[ssShift,ssCtrl],VK_UNKNOWN,[]);

  // run menu
  ecCompile: SetSingle(VK_F9,[ssCtrl],VK_UNKNOWN,[]);
  ecBuild: SetSingle(VK_F9,[ssShift],VK_UNKNOWN,[]);
  ecQuickCompile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCleanUpCompiled: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAbortBuild: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRun: SetSingle(VK_F9,[],VK_UNKNOWN,[]);
  ecPause: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowExecutionPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepInto: SetSingle(VK_F7,[],VK_UNKNOWN,[]);
  ecStepOver: SetSingle(VK_F8,[],VK_UNKNOWN,[]);
  ecStepIntoInstr: SetSingle(VK_F7,[ssAlt],VK_UNKNOWN,[]);
  ecStepOverInstr: SetSingle(VK_F8,[ssAlt],VK_UNKNOWN,[]);
  ecStepOut: SetSingle(VK_F8,[ssShift],VK_UNKNOWN,[]);
  ecRunToCursor: SetSingle(VK_F4,[],VK_UNKNOWN,[]);
  ecStopProgram: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveBreakPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunParameters: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInspect: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEvaluate: SetSingle(VK_F4,[ssCtrl],VK_UNKNOWN,[]);
  ecAddWatch: SetSingle(VK_F7,[ssCtrl],VK_UNKNOWN,[]);
  ecAddBpSource: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddBpAddress: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddBpDataWatch: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // components menu
  ecNewPackage: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackage: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageOfCurUnit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurFileToPkg: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPackageGraph: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditInstallPkgs: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigCustomComps: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // tools menu
  ecExtToolSettings: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecManageExamples: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildLazarus: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildLazarus: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMakeResourceString: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDiff: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // environment menu
  ecEnvironmentOptions: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRescanFPCSrcDir: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditCodeTemplates: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCodeToolsDefinesEd: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // help menu
  ecAboutLazarus: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOnlineHelp: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecContextHelp: SetSingle(VK_F1,[ssCtrl],VK_UNKNOWN,[]);
  ecEditContextHelp: SetSingle(VK_F1,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecReportingBug: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFocusHint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // designer
  ecDesignerCopy        : SetCombo(VK_C,[ssCtrl],VK_UNKNOWN,[],VK_Insert,[ssCtrl],VK_UNKNOWN,[]);
  ecDesignerCut         : SetCombo(VK_X,[ssCtrl],VK_UNKNOWN,[],VK_Delete,[ssShift],VK_UNKNOWN,[]);
  ecDesignerPaste       : SetCombo(VK_V,[ssCtrl],VK_UNKNOWN,[],VK_Insert,[ssShift],VK_UNKNOWN,[]);
  ecDesignerSelectParent: SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
  ecDesignerMoveToFront : SetSingle(VK_PRIOR,[ssShift],VK_UNKNOWN,[]);
  ecDesignerMoveToBack  : SetSingle(VK_NEXT,[ssShift],VK_UNKNOWN,[]);
  ecDesignerForwardOne  : SetSingle(VK_PRIOR,[ssCtrl],VK_UNKNOWN,[]);
  ecDesignerBackOne     : SetSingle(VK_NEXT,[ssCtrl],VK_UNKNOWN,[]);

  else
    begin
      SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
      SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
      case TSynPluginTemplateEdit.ConvertCommandToBase(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetSingle(VK_HOME, [],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetSingle(VK_END,  [],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetSingle(VK_A,    [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetSingle(VK_RETURN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginTemplateEdit.ConvertCommandToBaseOff(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetSingle(VK_RETURN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBase(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetSingle(VK_HOME, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetSingle(VK_END,  [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetSingle(VK_A,    [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseOff(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseSel(Command) of
        // SyncroEdit, during selection
        ecSynPSyncroEdStart:               SetSingle(VK_J,[ssCtrl],VK_UNKNOWN,[]);
      end;
    end;
  end;
(*//F1                      Topic Search
//Ctrl+F1                Topic Search
  ecNextEditor: SetSingle(VK_F6,[]);
  ecPrevEditor: SetSingle(VK_F6,[ssShift]);
  ecWordLeft:   SetSingle(VK_A,[ssCtrl],VK_LEFT,[ssCtrl]);
  ecPageDown:   SetSingle(VK_C,[ssCtrl],VK_NEXT,[]);
//Ctrl+D                 Moves the cursor right one column, accounting for the
//autoindent setting
//Ctrl+E                 Moves the cursor up one line
//Ctrl+F                 Moves one word right
//Ctrl+G                 Deletes the character to the right of the cursor
//Ctrl+H                 Deletes the character to the left of the cursor
//Ctrl+I                  Inserts a tab
//Ctrl+L                 Search|Search Again
//Ctrl+N                 Inserts a new line
//Ctrl+P                 Causes next character to be interpreted as an ASCII
//sequence
//Ctrl+R                 Moves up one screen
//Ctrl+S                 Moves the cursor left one column, accounting for the
//autoindent setting
//Ctrl+T                 Deletes a word
//Ctrl+V                 Turns insert mode on/off
//Ctrl+W                Moves down one screen
//Ctrl+X                 Moves the cursor down one line
//Ctrl+Y                 Deletes a line
//Ctrl+Z                 Moves the cursor up one line
//Ctrl+Shift+S          Performs an incremental search

//Block commands:
//---------------
//Ctrl+K+B      Marks the beginning of a block
//Ctrl+K+C      Copies a selected block
//Ctrl+K+H      Hides/shows a selected block
//Ctrl+K+I       Indents a block by the amount specified in the Block Indent
//combo box on the General page of the Editor Options dialog box.
//Ctrl+K+K      Marks the end of a block
//Ctrl+K+L       Marks the current line as a block
//Ctrl+K+N      Changes a block to uppercase
//Ctrl+K+O      Changes a block to lowercase
//Ctrl+K+P      Prints selected block
//Ctrl+K+R      Reads a block from a file
//Ctrl+K+T       Marks a word as a block
//Ctrl+K+U      Outdents a block by the amount specified in the Block Indent
//combo box on the General page of the Editor Options dialog box.
//Ctrl+K+V      Moves a selected block
//Ctrl+K+W      Writes a selected block to a file
//Ctrl+K+Y      Deletes a selected block
//Ctrl+O+C      Turns on column blocking
//Ctrl+O+I       Marks an inclusive block
//Ctrl+O+K      Turns off column blocking
//Ctrl+O+L      Marks a line as a block
//Shift+Alt+arrow Selects column-oriented blocks
//Click+Alt+mousemv Selects column-oriented blocks
//Ctrl+Q+B      Moves to the beginning of a block
//Ctrl+Q+K      Moves to the end of a block

//Miscellaneous commands:
//-----------------------
//Ctrl+K+D      Accesses the menu bar
//Ctrl+K+E       Changes a word to lowercase
//Ctrl+K+F       Changes a word to uppercase
//Ctrl+K+S      File|Save (Default and IDE Classic only)
//Ctrl+Q+A      Search|Replace
//Ctrl+Q+F      Search|Find
//Ctrl+Q+Y      Deletes to the end of a line
//Ctrl+Q+[       Finds the matching delimiter (forward)
//Ctrl+Q+Ctrl+[ Finds the matching delimiter (forward)
//Ctrl+Q+]       Finds the matching delimiter (backward)
//Ctrl+Q+Ctrl+] Finds the matching delimiter (backward)
//Ctrl+O+A      Open file at cursor
//Ctrl+O+B      Browse symbol at cursor (Delphi only)
//Alt+right arrow  For code browsing
//Alt +left arrow For code browsing
//Ctrl+O+G      Search|Go to line number
//Ctrl+O+O      Inserts compiler options and directives
//Ctrl+O+U      Toggles case
//Bookmark commands:
//------------------
//Shortcut       Action
//Ctrl+K+0       Sets bookmark 0
//Ctrl+K+1       Sets bookmark 1
//Ctrl+K+2       Sets bookmark 2
//Ctrl+K+3       Sets bookmark 3
//Ctrl+K+4       Sets bookmark 4
//Ctrl+K+5       Sets bookmark 5
//Ctrl+K+6       Sets bookmark 6
//Ctrl+K+7       Sets bookmark 7
//Ctrl+K+8       Sets bookmark 8
//Ctrl+K+9       Sets bookmark 9
//Ctrl+K+Ctrl+0 Sets bookmark 0
//Ctrl+K+Ctrl+1 Sets bookmark 1
//Ctrl+K+Ctrl+2 Sets bookmark 2
//Ctrl+K+Ctrl+3 Sets bookmark 3
//Ctrl+K+Ctrl+4 Sets bookmark 4
//Ctrl+K+Ctrl+5 Sets bookmark 5
//Ctrl+K+Ctrl+6 Sets bookmark 6
//Ctrl+K+Ctrl+7 Sets bookmark 7
//Ctrl+K+Ctrl+8 Sets bookmark 8
//Ctrl+K+Ctrl+9 Sets bookmark 9
//Ctrl+Q+0       Goes to bookmark 0
//Ctrl+Q+1       Goes to bookmark 1
//Ctrl+Q+2       Goes to bookmark 2
//Ctrl+Q+3       Goes to bookmark 3
//Ctrl+Q+4       Goes to bookmark 4
//Ctrl+Q+5       Goes to bookmark 5
//Ctrl+Q+6       Goes to bookmark 6
//Ctrl+Q+7       Goes to bookmark 7
//Ctrl+Q+8       Goes to bookmark 8
//Ctrl+Q+9       Goes to bookmark 9
//Ctrl+Q+Ctrl+0 Goes to bookmark 0
//Ctrl+Q+Ctrl+1 Goes to bookmark 1
//Ctrl+Q+Ctrl+2 Goes to bookmark 2
//Ctrl+Q+Ctrl+3 Goes to bookmark 3
//Ctrl+Q+Ctrl+4 Goes to bookmark 4
//Ctrl+Q+Ctrl+5 Goes to bookmark 5
//Ctrl+Q+Ctrl+6 Goes to bookmark 6
//Ctrl+Q+Ctrl+7 Goes to bookmark 7
//Ctrl+Q+Ctrl+8 Goes to bookmark 8
//Ctrl+Q+Ctrl+9 Goes to bookmark 9
//Cursor movement:
//----------------
//Ctrl+Q+B      Moves to the beginning of a block
//Ctrl+Q+C      Moves to end of a file
//Ctrl+Q+D      Moves to the end of a line
//Ctrl+Q+E      Moves the cursor to the top of the window
//Ctrl+Q+K      Moves to the end of a block
//Ctrl+Q+P      Moves to previous position
//Ctrl+Q+R      Moves to the beginning of a file
//Ctrl+Q+S      Moves to the beginning of a line
//Ctrl+Q+T      Moves the viewing editor so that the current line is placed at
//the top of the window
//Ctrl+Q+U      Moves the viewing editor so that the current line is placed at
//the bottom of the window, if possible
//Ctrl+Q+X      Moves the cursor to the bottom of the window
//System keys:
//------------

//F1              Displays context-sensitive Help
//F2              File|Save
//F3              File|Open
//F4              Run to Cursor
//F5              Zooms window
//F6              Displays the next page
//F7              Run|Trace Into
//F8              Run|Step Over
//F9              Run|Run
//F11             View|Object Inspector
//F12             View|Toggle Form/Unit
//Alt+0           View|Window List
//Alt+F2          View|CPU
//Alt+F3          File|Close
//Alt+F7          Displays previous error in Message view
//Alt+F8          Displays next error in Message view
//Alt+F11        File|Use Unit (Delphi)
//Alt+F11        File|Include Unit Hdr (C++)
//Alt+F12        Displays the Code editor
//Alt+X           File|Exit
//Alt+right arrow  For code browsing forward
//Alt +left arrow For code browsing backward
//Alt +up arrow  For code browsing Ctrl-click on identifier
//Alt+Page Down Goes to the next tab
//Alt+Page Up   Goes to the previous tab
//Ctrl+F1        Topic Search
//Ctrl+F2        Run|Program Reset
//Ctrl+F3        View|Call Stack
//Ctrl+F6        Open Source/Header file (C++)
//Ctrl+F7        Add Watch at Cursor
//Ctrl+F8        Toggle Breakpoint
//Ctrl+F9        Project|Compile project (Delphi)
//Ctrl+F9        Project|Make project (C++)
//Ctrl+F11       File|Open Project
//Ctrl+F12       View|Units
//Shift+F7       Run|Trace To Next Source Line
//Shift+F11      Project|Add To Project
//Shift+F12      View|Forms
//Ctrl+D         Descends item (replaces Inspector window)
//Ctrl+N         Opens a new Inspector window
//Ctrl+S          Incremental search
//Ctrl+T          Displays the Type Cast dialog
  else
    GetDefaultKeyForCommand(Command,TheKeyA,TheKeyB);
  end;
*)
end;

procedure TKeyCommandRelation.GetDefaultKeyForMacOSXScheme;
begin
  case Command of
  // moving
  ecWordLeft: SetSingle(VK_LEFT, [ssAlt],VK_UNKNOWN,[]);
  ecWordRight: SetSingle(VK_RIGHT, [ssAlt],VK_UNKNOWN,[]);
  ecLineStart: SetSingle(VK_LEFT, [ssMeta],VK_UNKNOWN,[]);
  ecLineEnd: SetSingle(VK_RIGHT, [ssMeta],VK_UNKNOWN,[]);
  ecPageUp: SetSingle(VK_PRIOR, [],VK_UNKNOWN,[]);
  ecPageDown: SetSingle(VK_NEXT, [],VK_UNKNOWN,[]);
  ecPageLeft: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageRight: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageTop: SetSingle(VK_PRIOR, [ssAlt],VK_UNKNOWN,[]);
  ecPageBottom: SetSingle(VK_END, [ssAlt],VK_UNKNOWN,[]);
  ecEditorTop: SetSingle(VK_HOME,[],VK_UP,[ssMeta]);
  ecEditorBottom: SetSingle(VK_END,[],VK_DOWN,[ssMeta]);
  ecScrollUp: SetSingle(VK_UP, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollDown: SetSingle(VK_DOWN, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollLeft: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecScrollRight: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);

  // selection
  ecCopy: SetSingle(VK_C,[ssMeta],VK_Insert,[ssCtrl]);
  ecCut: SetSingle(VK_X,[ssMeta],VK_Delete,[ssShift]);
  ecPaste: SetSingle(VK_V,[ssMeta],VK_Insert,[ssShift]);
  ecNormalSelect: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecColumnSelect: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecLineSelect: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelWordLeft: SetSingle(VK_LEFT,[ssAlt,ssShift],VK_UNKNOWN,[]);
  ecSelWordRight: SetSingle(VK_RIGHT,[ssAlt,ssShift],VK_UNKNOWN,[]);
  ecSelLineStart: SetSingle(VK_LEFT,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecSelLineEnd: SetSingle(VK_RIGHT,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecSelPageTop: SetSingle(VK_PRIOR, [ssShift,ssAlt],VK_UNKNOWN,[]);
  ecSelPageBottom: SetSingle(VK_NEXT, [ssShift,ssAlt],VK_UNKNOWN,[]);
  ecSelEditorTop: SetSingle(VK_HOME, [ssShift],VK_UNKNOWN,[]);
  ecSelEditorBottom: SetSingle(VK_END, [ssShift],VK_UNKNOWN,[]);
  ecSelectAll: SetSingle(VK_A,[ssMeta],VK_UNKNOWN,[]);
  ecSelectToBrace: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectCodeBlock: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectWord: SetCombo(VK_K,[SSCtrl],VK_T,[]);
  ecSelectLine: SetCombo(VK_K,[SSCtrl],VK_L,[]);
  ecSelectParagraph: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectionUpperCase: SetCombo(VK_K,[SSCtrl],VK_N,[]);
  ecSelectionLowerCase: SetCombo(VK_K,[SSCtrl],VK_O,[]);
  ecSelectionSwapCase: SetCombo(VK_K,[SSCtrl],VK_P,[]);
  ecSelectionTabs2Spaces: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionEnclose: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionComment: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionUncomment: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecToggleComment: SetSingle(VK_OEM_2, [ssCtrl],VK_UNKNOWN,[]);
  ecSelectionEncloseIFDEF: SetSingle(VK_D, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelectionSort: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionBreakLines: SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);

  ecBlockSetBegin   : SetCombo(VK_K,[ssCtrl],VK_B,[],  VK_K,[ssCtrl],VK_B,[ssCtrl]);
  ecBlockSetEnd     : SetCombo(VK_K,[ssCtrl],VK_K,[],  VK_K,[ssCtrl],VK_K,[ssCtrl]);
  ecBlockToggleHide : SetCombo(VK_K,[ssCtrl],VK_H,[],  VK_K,[ssCtrl],VK_H,[ssCtrl]);
  ecBlockHide       : SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockShow       : SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockMove       : SetCombo(VK_K,[ssCtrl],VK_V,[],  VK_K,[ssCtrl],VK_V,[ssCtrl]);
  ecBlockCopy       : SetCombo(VK_K,[ssCtrl],VK_C,[],  VK_K,[ssCtrl],VK_C,[ssCtrl]);
  ecBlockDelete     : SetCombo(VK_K,[ssCtrl],VK_Y,[],  VK_K,[ssCtrl],VK_Y,[ssCtrl]);
  ecBlockGotoBegin  : SetCombo(VK_Q,[ssCtrl],VK_B,[],  VK_Q,[ssCtrl],VK_B,[ssCtrl]);
  ecBlockGotoEnd    : SetCombo(VK_Q,[ssCtrl],VK_K,[],  VK_Q,[ssCtrl],VK_K,[ssCtrl]);

// column mode selection
  ecColSelUp: SetSingle(VK_UP, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelDown: SetSingle(VK_DOWN, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelLeft: SetSingle(VK_LEFT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelRight: SetSingle(VK_RIGHT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageDown: SetSingle(VK_NEXT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageBottom: SetSingle(VK_NEXT, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelPageUp: SetSingle(VK_PRIOR, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageTop: SetSingle(VK_PRIOR, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelLineStart: SetSingle(VK_HOME, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelLineEnd: SetSingle(VK_END, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelEditorTop: SetSingle(VK_HOME, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelEditorBottom: SetSingle(VK_END, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);

  // editing
  ecBlockIndent: SetCombo(VK_I,[ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_I,[]);
  ecBlockUnindent: SetCombo(VK_U,[ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_U,[]);
  ecDeleteLastChar: SetSingle(VK_BACK, [],VK_BACK, [ssShift]);  // ctrl H used for scroll window.
  ecDeleteChar: SetSingle(VK_DELETE,[],VK_UNKNOWN,[]); // ctrl G conflicts with GO
  ecDeleteWord: SetSingle(VK_DELETE,[ssAlt],VK_UNKNOWN,[]);
  ecDeleteLastWord: SetSingle(VK_BACK,[ssCtrl],VK_UNKNOWN,[]);
  ecDeleteBOL: SetSingle(VK_BACK,[ssMeta],VK_UNKNOWN,[]);
  ecDeleteEOL: SetSingle(VK_DELETE,[ssMeta],VK_UNKNOWN,[]);
  ecDeleteLine: SetSingle(VK_Y,[ssCtrl],VK_UNKNOWN,[]);
  ecClearAll: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecLineBreak: SetSingle(VK_RETURN,[],VK_UNKNOWN,[]);
  ecInsertLine: SetSingle(VK_N,[ssShift,ssMeta],VK_UNKNOWN,[]);
  ecInsertCharacter: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertGPLNotice: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertLGPLNotice: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertModifiedLGPLNotice: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertUserName: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertDateTime: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertChangeLogEntry: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSAuthor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSDate: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSHeader: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSID: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSLog: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSName: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSRevision: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSSource: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertGUID: SetSingle(VK_G, [ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecInsertFilename: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // command commands
  ecUndo: SetSingle(VK_Z,[ssMeta],VK_UNKNOWN,[]);
  ecRedo: SetSingle(VK_Z,[ssMeta,ssShift],VK_UNKNOWN,[]);

  // search & replace
  ecMatchBracket: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFind: SetSingle(VK_F,[ssMeta],VK_UNKNOWN,[]);
  ecFindNext: SetSingle(VK_G,[ssMeta],VK_UNKNOWN,[]);
  ecFindPrevious: SetSingle(VK_G,[ssShift,ssMeta],VK_UNKNOWN,[]);
  ecFindInFiles: SetSingle(VK_F,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecReplace: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecIncrementalFind: SetSingle(VK_E,[ssMeta],VK_UNKNOWN,[]);
  ecGotoLineNumber: SetSingle(VK_L,[ssMeta],VK_UNKNOWN,[]);
  ecFindNextWordOccurrence: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindPrevWordOccurrence: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpBack: SetSingle(VK_H,[ssCtrl],VK_UNKNOWN,[]);
  ecJumpForward: SetSingle(VK_H,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecAddJumpPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpToPrevError: SetSingle(VK_ADD,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecJumpToNextError: SetSingle(VK_ADD,[ssMeta],VK_UNKNOWN,[]);
  ecOpenFileAtCursor: SetSingle(VK_RETURN,[ssCtrl],VK_UNKNOWN,[]);
  ecProcedureList: SetSingle(VK_G, [ssAlt],VK_UNKNOWN,[]);


  // marker
  ecSetFreeBookmark: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevBookmark: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextBookmark: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker0: SetSingle(VK_0,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker1: SetSingle(VK_1,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker2: SetSingle(VK_2,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker3: SetSingle(VK_3,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker4: SetSingle(VK_4,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker5: SetSingle(VK_5,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker6: SetSingle(VK_6,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker7: SetSingle(VK_7,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker8: SetSingle(VK_8,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker9: SetSingle(VK_9,[ssCtrl],VK_UNKNOWN,[]);
  ecToggleMarker0: SetCombo(VK_0,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_0,[]);
  ecToggleMarker1: SetCombo(VK_1,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_1,[]);
  ecToggleMarker2: SetCombo(VK_2,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_2,[]);
  ecToggleMarker3: SetCombo(VK_3,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_3,[]);
  ecToggleMarker4: SetCombo(VK_4,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_4,[]);
  ecToggleMarker5: SetCombo(VK_5,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_5,[]);
  ecToggleMarker6: SetCombo(VK_6,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_6,[]);
  ecToggleMarker7: SetCombo(VK_7,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_7,[]);
  ecToggleMarker8: SetCombo(VK_8,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_8,[]);
  ecToggleMarker9: SetCombo(VK_9,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_9,[]);
  ecSetMarker0..ecSetMarker9: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // codetools
  ecAutoCompletion: SetSingle(VK_J,[ssMeta],VK_UNKNOWN,[]);
  ecWordCompletion: SetSingle(VK_SPACE,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecCompleteCode: SetSingle(VK_C,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecIdentCompletion: SetSingle(VK_SPACE,[ssCtrl],VK_UNKNOWN,[]);
  ecShowCodeContext: SetSingle(VK_SPACE,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecExtractProc: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindIdentifierRefs: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRenameIdentifier: SetSingle(VK_E,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecInvertAssignment: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSyntaxCheck: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessUnclosedBlock: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessMisplacedIFDEF: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDFM2LFM: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCheckLFM: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiUnit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiPackage: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertEncoding: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindProcedureDefinition: SetSingle(VK_UP,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  ecFindProcedureMethod: SetSingle(VK_DOWN,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  ecFindDeclaration: SetSingle(VK_UP,[ssAlt],VK_UNKNOWN,[]);
  ecFindBlockOtherEnd: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindBlockStart: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoIncludeDirective: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowAbstractMethods: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveEmptyMethods: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // source notebook
  ecNextEditor: SetSingle(VK_RIGHT, [ssMeta,ssAlt], VK_UNKNOWN, []);
  ecPrevEditor: SetSingle(VK_LEFT, [ssMeta,ssAlt], VK_UNKNOWN, []);
  ecResetDebugger: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleBreakPoint: SetSingle(VK_P,[ssCtrl],VK_UNKNOWN,[]);
  ecMoveEditorLeft: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorRight: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorLeftmost: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorRightmost: SetSingle(VK_UNKNOWN, [], VK_UNKNOWN, []);

  ecNextSharedEditor:        SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevSharedEditor:        SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextWindow:              SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevWindow:              SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNextWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorPrevWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNewWindow:     SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNextWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorPrevWindow:    SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNewWindow:     SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  ecGotoEditor1: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor2: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor3: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor4: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor5: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor6: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor7: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor8: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor9: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor0: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  ecLockEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  (*
  EcFoldLevel1: SetSingle(VK_1,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel2: SetSingle(VK_2,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel3: SetSingle(VK_3,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel4: SetSingle(VK_4,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel5: SetSingle(VK_5,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel6: SetSingle(VK_6,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel7: SetSingle(VK_7,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel8: SetSingle(VK_8,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel9: SetSingle(VK_9,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel0: SetSingle(VK_0,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldCurrent: SetSingle(VK_OEM_PLUS,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcUnFoldCurrent: SetSingle(VK_OEM_MINUS,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcToggleMarkupWord: SetSingle(VK_M,[ssMeta],VK_UNKNOWN,[]);
  *)

  // file menu
  ecNew: SetSingle(VK_N,[ssMeta],VK_UNKNOWN,[]);
  ecNewUnit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewForm: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpen: SetSingle(VK_O,[ssMeta],VK_UNKNOWN,[]);
  ecRevert: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSave: SetSingle(VK_S,[ssMeta],VK_UNKNOWN,[]);
  ecSaveAs: SetSingle(VK_S,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecSaveAll: SetSingle(VK_S,[ssMeta,ssAlt],VK_UNKNOWN,[]);
  ecClose: SetSingle(VK_W,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecCloseAll: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCleanDirectory: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRestart: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // view menu
  ecToggleObjectInsp: SetSingle(VK_I,[ssAlt,ssMeta],VK_UNKNOWN,[]);
  ecToggleSourceEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeExpl: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFPDocEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMessages: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewComponents: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewJumpHistory: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleSearchResults: SetSingle(VK_F,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleWatches: SetSingle(VK_W,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleBreakPoints: SetSingle(VK_B,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleLocals: SetSingle(VK_L,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecViewPseudoTerminal: if HasConsoleSupport then SetSingle(VK_O,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecViewThreads: SetSingle(VK_T,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleCallStack: SetSingle(VK_S,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleRegisters: SetSingle(VK_R,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleAssembler: SetSingle(VK_D,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleDebugEvents: SetSingle(VK_V,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleDebuggerOut: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewHistory: SetSingle(VK_H,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecViewUnitDependencies: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnitInfo: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFormUnit: SetSingle(VK_F,[ssMeta,ssAlt],VK_UNKNOWN,[]);
  ecViewAnchorEditor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeBrowser: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleRestrictionBrowser: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCompPalette: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleIDESpeedBtns: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // project menu
  ecNewProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewProjectFromFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCloseProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProjectAs: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPublishProject: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectInspector: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurUnitToProj: SetSingle(VK_A,[ssAlt,ssMeta],VK_UNKNOWN,[]);
  ecRemoveFromProj: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewProjectUnits: SetSingle(VK_U,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecViewProjectForms: SetSingle(VK_U,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecViewProjectSource: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectOptions: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // run menu
  ecCompile: SetSingle(VK_B,[ssMeta],VK_UNKNOWN,[]);
  ecBuild: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuickCompile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCleanUpCompiled: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAbortBuild: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRun: SetSingle(VK_R,[ssMeta],VK_UNKNOWN,[]);
  ecPause: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowExecutionPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepInto: SetSingle(VK_R,[ssMeta,ssAlt],VK_UNKNOWN,[]);
  ecStepOver: SetSingle(VK_R,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecStepOut: SetSingle(VK_T,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecRunToCursor: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStopProgram: SetSingle(VK_RETURN,[ssShift,ssMeta],VK_UNKNOWN,[]);
  ecRemoveBreakPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunParameters: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInspect: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEvaluate: SetSingle(VK_E,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecAddWatch: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddBpSource: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddBpAddress: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddBpDataWatch: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // components menu
  ecNewPackage: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackage: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageOfCurUnit: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurFileToPkg: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPackageGraph: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditInstallPkgs: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigCustomComps: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // tools menu
  ecExtToolSettings: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecManageExamples: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildLazarus: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildLazarus: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMakeResourceString: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDiff: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // environment menu
  ecEnvironmentOptions: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRescanFPCSrcDir: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditCodeTemplates: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCodeToolsDefinesEd: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // help menu
  ecAboutLazarus: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOnlineHelp: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecContextHelp: SetSingle(VK_HELP,[],VK_UNKNOWN,[]);
  ecEditContextHelp: SetSingle(VK_HELP,[ssShift,ssCtrl],VK_HELP,[ssCtrl]);
  ecReportingBug: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFocusHint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // designer
  ecDesignerCopy        : SetSingle(VK_C,[ssMeta],VK_UNKNOWN,[]);
  ecDesignerCut         : SetSingle(VK_X,[ssMeta],VK_UNKNOWN,[]);
  ecDesignerPaste       : SetSingle(VK_V,[ssMeta],VK_UNKNOWN,[]);
  ecDesignerSelectParent: SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
  ecDesignerMoveToFront : SetSingle(VK_PRIOR,[ssShift],VK_UNKNOWN,[]);
  ecDesignerMoveToBack  : SetSingle(VK_NEXT,[ssShift],VK_UNKNOWN,[]);
  ecDesignerForwardOne  : SetSingle(VK_PRIOR,[ssMeta],VK_UNKNOWN,[]);
  ecDesignerBackOne     : SetSingle(VK_NEXT,[ssMeta],VK_UNKNOWN,[]);

  else
    begin
      SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
      SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
      case TSynPluginTemplateEdit.ConvertCommandToBase(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetSingle(VK_HOME, [],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetSingle(VK_END,  [],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetSingle(VK_A,    [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetSingle(VK_RETURN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginTemplateEdit.ConvertCommandToBaseOff(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetSingle(VK_RETURN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBase(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetSingle(VK_HOME, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetSingle(VK_END,  [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetSingle(VK_A,    [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseOff(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetSingle(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetSingle(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetSingle(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetSingle(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetSingle(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetSingle(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseSel(Command) of
        // SyncroEdit, during selection
        ecSynPSyncroEdStart:               SetSingle(VK_J,[ssCtrl],VK_UNKNOWN,[]);
      end;
    end;
  end;
end;

procedure TKeyCommandRelation.GetDefaultKeyForMacOSXLazScheme;
begin
  { First default to standard Mac OS X scheme }
  GetDefaultKeyForMacOSXScheme;

  { Now override some entries }
  case Command of
  // moving
  ecLineStart: SetSingle(VK_HOME, [],VK_LEFT,[ssMeta]);
  ecLineEnd: SetSingle(VK_END, [],VK_RIGHT,[ssMeta]);
  ecEditorTop: SetSingle(VK_UP,[ssMeta],VK_UNKNOWN,[]);
  ecEditorBottom: SetSingle(VK_DOWN,[ssMeta],VK_UNKNOWN,[]);

  // selection
  ecSelLineStart: SetSingle(VK_HOME, [ssShift],VK_LEFT,[ssMeta,ssShift]);
  ecSelLineEnd: SetSingle(VK_END, [ssShift],VK_RIGHT,[ssMeta,ssShift]);
  ecSelEditorTop: SetSingle(VK_HOME, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelEditorBottom: SetSingle(VK_END, [ssShift,ssCtrl],VK_UNKNOWN,[]);

  // codetools
  ecRenameIdentifier: SetSingle(VK_E, [ssShift,ssCtrl],VK_UNKNOWN,[]);

  // run menu
  ecCompile: SetSingle(VK_F9,[ssCtrl],VK_F9,[ssCtrl,ssMeta]);
  ecBuild: SetSingle(VK_F9,[ssShift],VK_UNKNOWN,[]);
  ecQuickCompile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCleanUpCompiled: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAbortBuild: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRun: SetSingle(VK_F9,[],VK_F9,[ssMeta]);
  ecPause: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowExecutionPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepInto: SetSingle(VK_F7,[],VK_F7,[ssMeta]);
  ecStepOver: SetSingle(VK_F8,[],VK_F8,[ssMeta]);
  ecStepOut: SetSingle(VK_F8,[],VK_F8,[ssShift,ssMeta]);
  ecRunToCursor: SetSingle(VK_F4,[],VK_F4,[ssMeta]);
  ecStopProgram: SetSingle(VK_F2,[ssCtrl],VK_F2,[ssCtrl,ssMeta]);
  ecRemoveBreakPoint: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunParameters: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildFile: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInspect: SetSingle(VK_F5,[ssAlt],VK_UNKNOWN,[]);
  ecEvaluate: SetSingle(VK_F7,[ssCtrl],VK_F7,[ssCtrl,ssMeta]);
  ecAddWatch: SetSingle(VK_F5,[ssCtrl],VK_F5,[ssCtrl,ssMeta]);
  ecAddBpSource: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddBpAddress: SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddBpDataWatch: SetSingle(VK_F5,[ssShift],VK_UNKNOWN,[]);
  end;
end;

{ TKeyCommandRelationList }

constructor TKeyCommandRelationList.Create;
begin
  inherited Create;
  FRelations:=TList.Create;
  fCategories:=TList.Create;
  fExtToolCount:=0;
  fLoadedKeyCommands:=TAvgLvlTree.Create(@CompareLoadedKeyCommands);
end;

destructor TKeyCommandRelationList.Destroy;
begin
  Clear;
  FRelations.Free;
  fCategories.Free;
  fLoadedKeyCommands.Free;
  inherited Destroy;
end;

procedure TKeyCommandRelationList.CreateDefaultMapping;
// create default keymapping
var
  C: TIDECommandCategory;
  o: LongInt;
begin
  Clear;
  // moving
  C:=Categories[AddCategory('CursorMoving',srkmCatCursorMoving,IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Move cursor word left', srkmecWordLeft, ecWordLeft);
  AddDefault(C, 'Move cursor word right', srkmecWordRight, ecWordRight);
  AddDefault(C, 'Move cursor to line start', srkmecLineStart, ecLineStart);
  AddDefault(C, 'Move cursor to text start in line', srkmecLineTextStart, ecLineTextStart);
  AddDefault(C, 'Move cursor to line end', srkmecLineEnd, ecLineEnd);
  AddDefault(C, 'Move cursor up one page', srkmecPageUp, ecPageUp);
  AddDefault(C, 'Move cursor down one page', srkmecPageDown, ecPageDown);
  AddDefault(C, 'Move cursor left one page', srkmecPageLeft, ecPageLeft);
  AddDefault(C, 'Move cursor right one page', srkmecPageRight, ecPageRight);
  AddDefault(C, 'Move cursor to top of page', srkmecPageTop, ecPageTop);
  AddDefault(C, 'Move cursor to bottom of page', srkmecPageBottom, ecPageBottom);
  AddDefault(C, 'Move cursor to absolute beginning', srkmecEditorTop, ecEditorTop);
  AddDefault(C, 'Move cursor to absolute end', srkmecEditorBottom, ecEditorBottom);
  AddDefault(C, 'Scroll up one line', srkmecScrollUp, ecScrollUp);
  AddDefault(C, 'Scroll down one line', srkmecScrollDown, ecScrollDown);
  AddDefault(C, 'Scroll left one char', srkmecScrollLeft, ecScrollLeft);
  AddDefault(C, 'Scroll right one char', srkmecScrollRight, ecScrollRight);

  // selection
  C:=Categories[AddCategory('Selection',srkmCatSelection, IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Copy selection to clipboard', srkmecCopy, ecCopy);
  AddDefault(C, 'Cut selection to clipboard', srkmecCut, ecCut);
  AddDefault(C, 'Paste clipboard to current position', srkmecPaste, ecPaste);
  AddDefault(C, 'Normal selection mode', srkmecNormalSelect, ecNormalSelect);
  AddDefault(C, 'Column selection mode', srkmecColumnSelect, ecColumnSelect);
  AddDefault(C, 'Line selection mode', srkmecLineSelect, ecLineSelect);
  AddDefault(C, 'Indent block', srkmecBlockIndent, ecBlockIndent);
  AddDefault(C, 'Unindent block', srkmecBlockUnindent, ecBlockUnindent);
  AddDefault(C, 'Uppercase selection', lisMenuUpperCaseSelection, ecSelectionUpperCase);
  AddDefault(C, 'Lowercase selection', lisMenuLowerCaseSelection, ecSelectionLowerCase);
  AddDefault(C, 'Swap case in selection', lisMenuSwapCaseSelection, ecSelectionSwapCase);
  AddDefault(C, 'Convert tabs to spaces in selection',
    srkmecSelectionTabs2Spaces, ecSelectionTabs2Spaces);
  AddDefault(C, 'Enclose selection', lisKMEncloseSelection, ecSelectionEnclose);
  AddDefault(C, 'Comment selection', lisMenuCommentSelection, ecSelectionComment);
  AddDefault(C, 'Uncomment selection', lisMenuUncommentSelection, ecSelectionUncomment);
  AddDefault(C, 'Toggle comment', lisMenuToggleComment, ecToggleComment);
  AddDefault(C, 'Sort selection', lisSortSelSortSelection, ecSelectionSort);
  AddDefault(C, 'Break Lines in selection', lisMenuBeakLinesInSelection, ecSelectionBreakLines);
  AddDefault(C, 'Select word left', lisKMSelectWordLeft, ecSelWordLeft);
  AddDefault(C, 'Select word right', lisKMSelectWordRight, ecSelWordRight);
  AddDefault(C, 'Select line start', lisKMSelectLineStart, ecSelLineStart);
  AddDefault(C, 'Select to text start in line', srkmecSelLineTextStart, ecSelLineTextStart);
  AddDefault(C, 'Select line end', lisKMSelectLineEnd, ecSelLineEnd);
  AddDefault(C, 'Select page top', lisKMSelectPageTop, ecSelPageTop);
  AddDefault(C, 'Select page bottom', lisKMSelectPageBottom, ecSelPageBottom);
  AddDefault(C, 'Select to absolute beginning', srkmecSelEditorTop, ecSelEditorTop);
  AddDefault(C, 'Select to absolute end', srkmecSelEditorBottom, ecSelEditorBottom);
  AddDefault(C, 'Select all', lisMenuSelectAll, ecSelectAll);
  AddDefault(C, 'Select to brace', lisMenuSelectToBrace, ecSelectToBrace);
  AddDefault(C, 'Select code block', lisMenuSelectCodeBlock, ecSelectCodeBlock);
  AddDefault(C, 'Select word', lisMenuSelectWord, ecSelectWord);
  AddDefault(C, 'Select line', lisMenuSelectLine, ecSelectLine);
  AddDefault(C, 'Select paragraph', lisMenuSelectParagraph, ecSelectParagraph);
  AddDefault(C, 'Toggle Current-Word highlight', srkmecToggleMarkupWord, EcToggleMarkupWord);

  AddDefault(C, 'Set Block begin', srkmecBlockSetBegin, ecBlockSetBegin);
  AddDefault(C, 'Set Block End', srkmecBlockSetEnd, ecBlockSetEnd);
  AddDefault(C, 'Toggle Block', srkmecBlockToggleHide, ecBlockToggleHide);
  AddDefault(C, 'Hide Block', srkmecBlockHide, ecBlockHide);
  AddDefault(C, 'Show Block', srkmecBlockShow, ecBlockShow);
  AddDefault(C, 'Move Block', srkmecBlockMove, ecBlockMove);
  AddDefault(C, 'Copy Block', srkmecBlockCopy, ecBlockCopy);
  AddDefault(C, 'Delete Block', srkmecBlockDelete, ecBlockDelete);
  AddDefault(C, 'Goto Block Begin', srkmecBlockGotoBegin, ecBlockGotoBegin);
  AddDefault(C, 'Goto Block End', srkmecBlockGotoEnd, ecBlockGotoEnd);

  // column mode selection
  C:=Categories[AddCategory('Column Selection',srkmCatColSelection,IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Column Select Up', srkmecColSelUp, ecColSelUp);
  AddDefault(C, 'Column Select Down', srkmecColSelDown, ecColSelDown);
  AddDefault(C, 'Column Select Left', srkmecColSelLeft, ecColSelLeft);
  AddDefault(C, 'Column Select Right', srkmecColSelRight, ecColSelRight);
  AddDefault(C, 'Column Select word left', srkmecColSelWordLeft, ecColSelWordLeft);
  AddDefault(C, 'Column Select word right', srkmecColSelWordRight, ecColSelWordRight);
  AddDefault(C, 'Column Select Page Down', srkmecColSelPageDown, ecColSelPageDown);
  AddDefault(C, 'Column Select Page Bottom', srkmecColSelPageBottom, ecColSelPageBottom);
  AddDefault(C, 'Column Select Page Up', srkmecColSelPageUp, ecColSelPageUp);
  AddDefault(C, 'Column Select Page Top', srkmecColSelPageTop, ecColSelPageTop);
  AddDefault(C, 'Column Select Line Start', srkmecColSelLineStart, ecColSelLineStart);
  AddDefault(C, 'Column Select to text start in line', srkmecColSelLineTextStart, ecColSelLineTextStart);
  AddDefault(C, 'Column Select Line End', srkmecColSelLineEnd, ecColSelLineEnd);
  AddDefault(C, 'Column Select to absolute beginning', srkmecColSelEditorTop, ecColSelEditorTop);
  AddDefault(C, 'Column Select to absolute end', srkmecColSelEditorBottom, ecColSelEditorBottom);

  // editing - without menu items in the IDE bar
  C:=Categories[AddCategory(CommandCategoryTextEditingName,srkmCatEditing,
                IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Delete last char', lisKMDeleteLastChar, ecDeleteLastChar);
  AddDefault(C, 'Delete char at cursor', srkmecDeletechar, ecDeleteChar);
  AddDefault(C, 'Delete to end of word', srkmecDeleteWord, ecDeleteWord);
  AddDefault(C, 'Delete to start of word', srkmecDeleteLastWord, ecDeleteLastWord);
  AddDefault(C, 'Delete to beginning of line', srkmecDeleteBOL, ecDeleteBOL);
  AddDefault(C, 'Delete to end of line', srkmecDeleteEOL, ecDeleteEOL);
  AddDefault(C, 'Delete current line', srkmecDeleteLine, ecDeleteLine);
  AddDefault(C, 'Delete whole text', srkmecClearAll, ecClearAll);
  AddDefault(C, 'Break line and move cursor', srkmecLineBreak, ecLineBreak);
  AddDefault(C, 'Break line, leave cursor', srkmecInsertLine, ecInsertLine);
  AddDefault(C, 'Enclose in $IFDEF', lisEncloseInIFDEF, ecSelectionEncloseIFDEF);
  AddDefault(C, 'Insert from Character Map', lisMenuInsertCharacter, ecInsertCharacter);
  AddDefault(C, 'Insert GPL notice', srkmecInsertGPLNotice, ecInsertGPLNotice);
  AddDefault(C, 'Insert LGPL notice', srkmecInsertLGPLNotice, ecInsertLGPLNotice);
  AddDefault(C, 'Insert modified LGPL notice', srkmecInsertModifiedLGPLNotice, ecInsertModifiedLGPLNotice);
  AddDefault(C, 'Insert username', lisKMInsertUsername, ecInsertUserName);
  AddDefault(C, 'Insert date and time', lisKMInsertDateAndTime, ecInsertDateTime);
  AddDefault(C, 'Insert ChangeLog entry', srkmecInsertChangeLogEntry, ecInsertChangeLogEntry);
  AddDefault(C, 'Insert CVS keyword Author', srkmecInsertCVSAuthor, ecInsertCVSAuthor);
  AddDefault(C, 'Insert CVS keyword Date', srkmecInsertCVSDate, ecInsertCVSDate);
  AddDefault(C, 'Insert CVS keyword Header', srkmecInsertCVSHeader, ecInsertCVSHeader);
  AddDefault(C, 'Insert CVS keyword ID', srkmecInsertCVSID, ecInsertCVSID);
  AddDefault(C, 'Insert CVS keyword Log', srkmecInsertCVSLog, ecInsertCVSLog);
  AddDefault(C, 'Insert CVS keyword Name', srkmecInsertCVSName, ecInsertCVSName);
  AddDefault(C, 'Insert CVS keyword Revision', srkmecInsertCVSRevision, ecInsertCVSRevision);
  AddDefault(C, 'Insert CVS keyword Source', srkmecInsertCVSSource, ecInsertCVSSource);
  AddDefault(C, 'Insert a GUID',srkmecInsertGUID, ecInsertGUID);
  AddDefault(C, 'Insert full Filename',srkmecInsertFilename, ecInsertFilename);

  // command commands
  C:=Categories[AddCategory('CommandCommands',srkmCatCmdCmd,nil)];
  AddDefault(C, 'Undo', lisMenuUndo, ecUndo);
  AddDefault(C, 'Redo', lisMenuRedo, ecRedo);

  // search & replace
  C:=Categories[AddCategory('SearchReplace',srkmCatSearchReplace,IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Go to matching bracket', srkmecMatchBracket, ecMatchBracket);
  AddDefault(C, 'Find text', srkmecFind, ecFind);
  AddDefault(C, 'Find next', srkmecFindNext, ecFindNext);
  AddDefault(C, 'Find previous', srkmecFindPrevious, ecFindPrevious);
  AddDefault(C, 'Find in files', srkmecFindInFiles, ecFindInFiles);
  AddDefault(C, 'Replace text', srkmecReplace, ecReplace);
  AddDefault(C, 'Find incremental', lisKMFindIncremental, ecIncrementalFind);
  AddDefault(C, 'Go to line number', srkmecGotoLineNumber, ecGotoLineNumber);
  AddDefault(C, 'Find next word occurrence', srkmecFindNextWordOccurrence, ecFindNextWordOccurrence);
  AddDefault(C, 'Find previous word occurrence', srkmecFindPrevWordOccurrence, ecFindPrevWordOccurrence);
  AddDefault(C, 'Jump back', lisMenuJumpBack, ecJumpBack);
  AddDefault(C, 'Jump forward', lisMenuJumpForward, ecJumpForward);
  AddDefault(C, 'Add jump point', srkmecAddJumpPoint, ecAddJumpPoint);
  AddDefault(C, 'View jump history', lisKMViewJumpHistory, ecViewJumpHistory);
  AddDefault(C, 'Jump to next error', lisMenuJumpToNextError, ecJumpToNextError);
  AddDefault(C, 'Jump to previous error', lisMenuJumpToPrevError, ecJumpToPrevError);
  AddDefault(C, 'Open file at cursor', srkmecOpenFileAtCursor, ecOpenFileAtCursor);
  AddDefault(C,'Procedure List ...',lisPListProcedureList,ecProcedureList);

  // folding
  C:=Categories[AddCategory('Folding',srkmCatFold,IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Fold to Level 1',  Format(srkmEcFoldLevel,[1]), EcFoldLevel1);
  AddDefault(C, 'Fold to Level 2',  Format(srkmEcFoldLevel,[2]), EcFoldLevel2);
  AddDefault(C, 'Fold to Level 3',  Format(srkmEcFoldLevel,[3]), EcFoldLevel3);
  AddDefault(C, 'Fold to Level 4',  Format(srkmEcFoldLevel,[4]), EcFoldLevel4);
  AddDefault(C, 'Fold to Level 5',  Format(srkmEcFoldLevel,[5]), EcFoldLevel5);
  AddDefault(C, 'Fold to Level 6',  Format(srkmEcFoldLevel,[6]), EcFoldLevel6);
  AddDefault(C, 'Fold to Level 7',  Format(srkmEcFoldLevel,[7]), EcFoldLevel7);
  AddDefault(C, 'Fold to Level 8',  Format(srkmEcFoldLevel,[8]), EcFoldLevel8);
  AddDefault(C, 'Fold to Level 9',  Format(srkmEcFoldLevel,[9]), EcFoldLevel9);
  AddDefault(C, 'Unfold all', srkmecUnFoldAll, EcFoldLevel0);
  AddDefault(C, 'Fold at Cursor', srkmecFoldCurrent, EcFoldCurrent);
  AddDefault(C, 'Unfold at Cursor', srkmecUnFoldCurrent, EcUnFoldCurrent);

  // marker - without menu items in the IDE bar
  C:=Categories[AddCategory('Marker',srkmCatMarker,IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Set free Bookmark', lisKMSetFreeBookmark, ecSetFreeBookmark);
  AddDefault(C, 'Previous Bookmark', srkmecPrevBookmark, ecPrevBookmark);
  AddDefault(C, 'Next Bookmark', srkmecNextBookmark, ecNextBookmark);
  AddDefault(C, 'Go to marker 0', lisKMGoToMarker0, ecGotoMarker0);
  AddDefault(C, 'Go to marker 1', lisKMGoToMarker1, ecGotoMarker1);
  AddDefault(C, 'Go to marker 2', lisKMGoToMarker2, ecGotoMarker2);
  AddDefault(C, 'Go to marker 3', lisKMGoToMarker3, ecGotoMarker3);
  AddDefault(C, 'Go to marker 4', lisKMGoToMarker4, ecGotoMarker4);
  AddDefault(C, 'Go to marker 5', lisKMGoToMarker5, ecGotoMarker5);
  AddDefault(C, 'Go to marker 6', lisKMGoToMarker6, ecGotoMarker6);
  AddDefault(C, 'Go to marker 7', lisKMGoToMarker7, ecGotoMarker7);
  AddDefault(C, 'Go to marker 8', lisKMGoToMarker8, ecGotoMarker8);
  AddDefault(C, 'Go to marker 9', lisKMGoToMarker9, ecGotoMarker9);
  AddDefault(C, 'Set marker 0', lisKMSetMarker0, ecSetMarker0);
  AddDefault(C, 'Set marker 1', lisKMSetMarker1, ecSetMarker1);
  AddDefault(C, 'Set marker 2', lisKMSetMarker2, ecSetMarker2);
  AddDefault(C, 'Set marker 3', lisKMSetMarker3, ecSetMarker3);
  AddDefault(C, 'Set marker 4', lisKMSetMarker4, ecSetMarker4);
  AddDefault(C, 'Set marker 5', lisKMSetMarker5, ecSetMarker5);
  AddDefault(C, 'Set marker 6', lisKMSetMarker6, ecSetMarker6);
  AddDefault(C, 'Set marker 7', lisKMSetMarker7, ecSetMarker7);
  AddDefault(C, 'Set marker 8', lisKMSetMarker8, ecSetMarker8);
  AddDefault(C, 'Set marker 9', lisKMSetMarker9, ecSetMarker9);
  AddDefault(C, 'Toggle marker 0', lisKMToggleMarker0, ecToggleMarker0);
  AddDefault(C, 'Toggle marker 1', lisKMToggleMarker1, ecToggleMarker1);
  AddDefault(C, 'Toggle marker 2', lisKMToggleMarker2, ecToggleMarker2);
  AddDefault(C, 'Toggle marker 3', lisKMToggleMarker3, ecToggleMarker3);
  AddDefault(C, 'Toggle marker 4', lisKMToggleMarker4, ecToggleMarker4);
  AddDefault(C, 'Toggle marker 5', lisKMToggleMarker5, ecToggleMarker5);
  AddDefault(C, 'Toggle marker 6', lisKMToggleMarker6, ecToggleMarker6);
  AddDefault(C, 'Toggle marker 7', lisKMToggleMarker7, ecToggleMarker7);
  AddDefault(C, 'Toggle marker 8', lisKMToggleMarker8, ecToggleMarker8);
  AddDefault(C, 'Toggle marker 9', lisKMToggleMarker9, ecToggleMarker9);

  // codetools
  C:=Categories[AddCategory(CommandCategoryCodeTools,srkmCatCodeTools,IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Code template completion', srkmecAutoCompletion, ecAutoCompletion);
  AddDefault(C, 'Word completion', srkmecWordCompletion, ecWordCompletion);
  AddDefault(C, 'Complete code', srkmecCompletecode, ecCompleteCode);
  AddDefault(C, 'Identifier completion', dlgEdIdComlet, ecIdentCompletion);
  AddDefault(C, 'Rename identifier', srkmecRenameIdentifier, ecRenameIdentifier);
  AddDefault(C, 'Find identifier references', srkmecFindIdentifierRefs, ecFindIdentifierRefs);
  AddDefault(C, 'Show code context', srkmecShowCodeContext, ecShowCodeContext);
  AddDefault(C, 'Extract proc', srkmecExtractProc, ecExtractProc);
  AddDefault(C, 'Invert assignment', srkmecInvertAssignment, ecInvertAssignment);
  AddDefault(C, 'Syntax check', srkmecSyntaxCheck, ecSyntaxCheck);
  AddDefault(C, 'Guess unclosed block', lisMenuGuessUnclosedBlock, ecGuessUnclosedBlock);
  AddDefault(C, 'Guess misplaced $IFDEF', srkmecGuessMisplacedIFDEF, ecGuessMisplacedIFDEF);
  AddDefault(C, 'Check LFM file in editor', lisMenuCheckLFM, ecCheckLFM);
  AddDefault(C, 'Find procedure definiton', srkmecFindProcedureDefinition, ecFindProcedureDefinition);
  AddDefault(C, 'Find procedure method', srkmecFindProcedureMethod, ecFindProcedureMethod);
  AddDefault(C, 'Find declaration', srkmecFindDeclaration, ecFindDeclaration);
  AddDefault(C, 'Find block other end', srkmecFindBlockOtherEnd, ecFindBlockOtherEnd);
  AddDefault(C, 'Find block start', srkmecFindBlockStart, ecFindBlockStart);
  AddDefault(C, 'Goto include directive', lisMenuGotoIncludeDirective, ecGotoIncludeDirective);
  AddDefault(C, 'Show abstract methods', srkmecShowAbstractMethods, ecShowAbstractMethods);
  AddDefault(C, 'Remove empty methods', srkmecRemoveEmptyMethods, ecRemoveEmptyMethods);
  AddDefault(C, 'Remove unused units', srkmecRemoveUnusedUnits, ecRemoveUnusedUnits);
  AddDefault(C, 'Add unit to uses section', lisUseUnit, ecUseUnit);
  AddDefault(C, 'Find overloads', srkmecFindOverloads, ecFindOverloads);
  AddDefault(C, 'Make resource string', srkmecMakeResourceString, ecMakeResourceString);

  // Template editing
  C:=Categories[AddCategory('Edit Template', srkmCatTemplateEdit, IDECmdScopeSrcEditOnlyTmplEdit)];
  o := TSynPluginTemplateEdit.ConvertBaseToCommand(ecPluginFirst) - ecPluginFirst;
  AddDefault(C, 'Edit Template Next Cell', srkmecSynPTmplEdNextCell, ecSynPTmplEdNextCell + o);
  AddDefault(C, 'Edit Template Next Cell (all selected)', srkmecSynPTmplEdNextCellSel, ecSynPTmplEdNextCellSel + o);
  AddDefault(C, 'Edit Template Next Cell (rotate)', srkmecSynPTmplEdNextCellRotate, ecSynPTmplEdNextCellRotate + o);
  AddDefault(C, 'Edit Template Next Cell (rotate / all selected)', srkmecSynPTmplEdNextCellSelRotate, ecSynPTmplEdNextCellSelRotate + o);
  AddDefault(C, 'Edit Template Previous Cell', srkmecSynPTmplEdPrevCell, ecSynPTmplEdPrevCell + o);
  AddDefault(C, 'Edit Template Previous Cell (all selected)', srkmecSynPTmplEdPrevCellSel, ecSynPTmplEdPrevCellSel + o);
  AddDefault(C, 'Edit Template Goto first pos in cell', srkmecSynPTmplEdCellHome, ecSynPTmplEdCellHome + o);
  AddDefault(C, 'Edit Template Goto last pos in cell', srkmecSynPTmplEdCellEnd, ecSynPTmplEdCellEnd + o);
  AddDefault(C, 'Edit Template Select cell', srkmecSynPTmplEdCellSelect, ecSynPTmplEdCellSelect + o);
  AddDefault(C, 'Edit Template Finish', srkmecSynPTmplEdFinish, ecSynPTmplEdFinish + o);
  AddDefault(C, 'Edit Template Escape', srkmecSynPTmplEdEscape, ecSynPTmplEdEscape + o);

  // Template editing not in cell
  C:=Categories[AddCategory('Edit Template Off', srkmCatTemplateEditOff, IDECmdScopeSrcEditOnlyTmplEditOff)];
  o := TSynPluginTemplateEdit.ConvertBaseToCommandOff(ecPluginFirst) - ecPluginFirst;
  AddDefault(C, 'Edit Template (off) Next Cell', srkmecSynPTmplEdNextCell, ecSynPTmplEdNextCell + o);
  AddDefault(C, 'Edit Template (off) Next Cell (all selected)', srkmecSynPTmplEdNextCellSel, ecSynPTmplEdNextCellSel + o);
  AddDefault(C, 'Edit Template (off) Next Cell (rotate)', srkmecSynPTmplEdNextCellRotate, ecSynPTmplEdNextCellRotate + o);
  AddDefault(C, 'Edit Template (off) Next Cell (rotate / all selected)', srkmecSynPTmplEdNextCellSelRotate, ecSynPTmplEdNextCellSelRotate + o);
  AddDefault(C, 'Edit Template (off) Previous Cell', srkmecSynPTmplEdPrevCell, ecSynPTmplEdPrevCell + o);
  AddDefault(C, 'Edit Template (off) Previous Cell (all selected)', srkmecSynPTmplEdPrevCellSel, ecSynPTmplEdPrevCellSel + o);
  AddDefault(C, 'Edit Template (off) Goto first pos in cell', srkmecSynPTmplEdCellHome, ecSynPTmplEdCellHome + o);
  AddDefault(C, 'Edit Template (off) Goto last pos in cell', srkmecSynPTmplEdCellEnd, ecSynPTmplEdCellEnd + o);
  AddDefault(C, 'Edit Template (off) Select cell', srkmecSynPTmplEdCellSelect, ecSynPTmplEdCellSelect + o);
  AddDefault(C, 'Edit Template (off) Finish', srkmecSynPTmplEdFinish, ecSynPTmplEdFinish + o);
  AddDefault(C, 'Edit Template (off) Escape', srkmecSynPTmplEdEscape, ecSynPTmplEdEscape + o);

  // Syncro editing
  C:=Categories[AddCategory('Syncro Edit', srkmCatSyncroEdit, IDECmdScopeSrcEditOnlySyncroEdit)];
  o := TSynPluginSyncroEdit.ConvertBaseToCommand(ecPluginFirst) - ecPluginFirst;
  AddDefault(C, 'Edit Syncro Next Cell', srkmecSynPSyncroEdNextCell, ecSynPSyncroEdNextCell + o);
  AddDefault(C, 'Edit Syncro Next Cell (all selected)', srkmecSynPSyncroEdNextCellSel, ecSynPSyncroEdNextCellSel + o);
  AddDefault(C, 'Edit Syncro Previous Cell', srkmecSynPSyncroEdPrevCell, ecSynPSyncroEdPrevCell + o);
  AddDefault(C, 'Edit Syncro Previous Cell (all selected)', srkmecSynPSyncroEdPrevCellSel, ecSynPSyncroEdPrevCellSel + o);
  AddDefault(C, 'Edit Syncro Goto first pos in cell', srkmecSynPSyncroEdCellHome, ecSynPSyncroEdCellHome + o);
  AddDefault(C, 'Edit Syncro Goto last pos in cell', srkmecSynPSyncroEdCellEnd, ecSynPSyncroEdCellEnd + o);
  AddDefault(C, 'Edit Syncro Select cell', srkmecSynPSyncroEdCellSelect, ecSynPSyncroEdCellSelect + o);
  AddDefault(C, 'Edit Syncro Escape', srkmecSynPSyncroEdEscape, ecSynPSyncroEdEscape + o);

  // Syncro editing not in cell
  C:=Categories[AddCategory('Syncro Edit Off', srkmCatSyncroEditOff, IDECmdScopeSrcEditOnlySyncroEditOff)];
  o := TSynPluginSyncroEdit.ConvertBaseToCommandOff(ecPluginFirst) - ecPluginFirst;
  AddDefault(C, 'Edit Syncro (off) Next Cell', srkmecSynPSyncroEdNextCell, ecSynPSyncroEdNextCell + o);
  AddDefault(C, 'Edit Syncro (off) Next Cell (all selected)', srkmecSynPSyncroEdNextCellSel, ecSynPSyncroEdNextCellSel + o);
  AddDefault(C, 'Edit Syncro (off) Previous Cell', srkmecSynPSyncroEdPrevCell, ecSynPSyncroEdPrevCell + o);
  AddDefault(C, 'Edit Syncro (off) Previous Cell (all selected)', srkmecSynPSyncroEdPrevCellSel, ecSynPSyncroEdPrevCellSel + o);
  AddDefault(C, 'Edit Syncro (off) Goto first pos in cell', srkmecSynPSyncroEdCellHome, ecSynPSyncroEdCellHome + o);
  AddDefault(C, 'Edit Syncro (off) Goto last pos in cell', srkmecSynPSyncroEdCellEnd, ecSynPSyncroEdCellEnd + o);
  AddDefault(C, 'Edit Syncro (off) Select cell', srkmecSynPSyncroEdCellSelect, ecSynPSyncroEdCellSelect + o);
  AddDefault(C, 'Edit Syncro (off) Escape', srkmecSynPSyncroEdEscape, ecSynPSyncroEdEscape + o);

  // Syncro editing still selecting
  C:=Categories[AddCategory('Syncro Edit Sel', srkmCatSyncroEditSel, IDECmdScopeSrcEditOnlySyncroEditSel)];
  o := TSynPluginSyncroEdit.ConvertBaseToCommandSel(ecPluginFirst) - ecPluginFirst;
  AddDefault(C, 'Edit Syncro (sel) Start', srkmecSynPSyncroEdStart, ecSynPSyncroEdStart + o);

  // source notebook - without menu items in the IDE bar
  C:=Categories[AddCategory('SourceNotebook',srkmCatSrcNoteBook,IDECmdScopeSrcEdit)];
  AddDefault(C, 'Go to next editor', srkmecNextEditor, ecNextEditor);
  AddDefault(C, 'Go to prior editor', srkmecPrevEditor, ecPrevEditor);
  AddDefault(C, 'Add break point', srkmecToggleBreakPoint, ecToggleBreakPoint);
  AddDefault(C, 'Remove break point', srkmecRemoveBreakPoint, ecRemoveBreakPoint);
  AddDefault(C, 'Move editor left', srkmecMoveEditorLeft, ecMoveEditorLeft);
  AddDefault(C, 'Move editor right', srkmecMoveEditorRight, ecMoveEditorRight);
  AddDefault(C, 'Move editor leftmost', srkmecMoveEditorLeftmost, ecMoveEditorLeftmost);
  AddDefault(C, 'Move editor rightmoust',  srkmecMoveEditorRightmost, ecMoveEditorRightmost);
  AddDefault(C, 'Go to source editor 1', lisKMGoToSourceEditor1, ecGotoEditor1);
  AddDefault(C, 'Go to source editor 2', lisKMGoToSourceEditor2, ecGotoEditor2);
  AddDefault(C, 'Go to source editor 3', lisKMGoToSourceEditor3, ecGotoEditor3);
  AddDefault(C, 'Go to source editor 4', lisKMGoToSourceEditor4, ecGotoEditor4);
  AddDefault(C, 'Go to source editor 5', lisKMGoToSourceEditor5, ecGotoEditor5);
  AddDefault(C, 'Go to source editor 6', lisKMGoToSourceEditor6, ecGotoEditor6);
  AddDefault(C, 'Go to source editor 7', lisKMGoToSourceEditor7, ecGotoEditor7);
  AddDefault(C, 'Go to source editor 8', lisKMGoToSourceEditor8, ecGotoEditor8);
  AddDefault(C, 'Go to source editor 9', lisKMGoToSourceEditor9, ecGotoEditor9);
  AddDefault(C, 'Go to source editor 10', lisKMGoToSourceEditor10, ecGotoEditor0);

  AddDefault(C, 'Go to next shared editor', srkmecNextSharedEditor, ecNextSharedEditor);
  AddDefault(C, 'Go to prior shared editor', srkmecPrevSharedEditor, ecPrevSharedEditor);
  AddDefault(C, 'Go to next window', srkmecNextWindow, ecNextWindow);
  AddDefault(C, 'Go to prior window', srkmecPrevWindow, ecPrevWindow);
  AddDefault(C, 'Move to next window', srkmecMoveEditorNextWindow, ecMoveEditorNextWindow);
  AddDefault(C, 'Move to prior window', srkmecMoveEditorPrevWindow, ecMoveEditorPrevWindow);
  AddDefault(C, 'Move to new window', srkmecMoveEditorNewWindow, ecMoveEditorNewWindow);
  AddDefault(C, 'Copy to next window', srkmecCopyEditorNextWindow, ecCopyEditorNextWindow);
  AddDefault(C, 'Copy to prior window', srkmecCopyEditorPrevWindow, ecCopyEditorPrevWindow);
  AddDefault(C, 'Copy to new window', srkmecCopyEditorNewWindow, ecCopyEditorNewWindow);

  AddDefault(C, 'Lock editor', srkmecLockEditor, ecLockEditor);

  // file menu
  C:=Categories[AddCategory('FileMenu',srkmCatFileMenu,nil)];
  AddDefault(C, 'New', lisMenuTemplateNew, ecNew);
  AddDefault(C, 'NewUnit', lisKMNewUnit, ecNewUnit);
  AddDefault(C, 'NewForm', lisMenuNewForm, ecNewForm);
  AddDefault(C, 'Open', lisHintOpen, ecOpen);
  AddDefault(C, 'Revert', lisMenuRevert, ecRevert);
  AddDefault(C, 'Save', srkmecSave, ecSave);
  AddDefault(C, 'SaveAs', lisKMSaveAs, ecSaveAs);
  AddDefault(C, 'SaveAll', lisKMSaveAll, ecSaveAll);
  AddDefault(C, 'Close', lisMenuClose, ecClose);
  AddDefault(C, 'CloseAll', lisKMCloseAll, ecCloseAll);
  AddDefault(C, 'Clean Directory', lisClDirCleanDirectory, ecCleanDirectory);
  AddDefault(C, 'Restart', lisMenuRestart, ecRestart);
  AddDefault(C, 'Quit', srkmecQuit, ecQuit);

  // view menu
  C:=Categories[AddCategory(CommandCategoryViewName,srkmCatViewMenu,nil)];
  AddDefault(C, 'Toggle view Object Inspector', lisKMToggleViewObjectInspector, ecToggleObjectInsp);
  AddDefault(C, 'Toggle view Source Editor', lisKMToggleViewSourceEditor, ecToggleSourceEditor);
  AddDefault(C, 'Toggle view Code Explorer', lisKMToggleViewCodeExplorer, ecToggleCodeExpl);
  AddDefault(C, 'Toggle view Documentation Editor', lisKMToggleViewDocumentationEditor, ecToggleFPDocEditor);
  AddDefault(C, 'Toggle view Messages', lisKMToggleViewMessages, ecToggleMessages);
  AddDefault(C, 'View Components', srkmecViewComponents, ecViewComponents);
  AddDefault(C, 'Toggle view Search Results', lisKMToggleViewSearchResults, ecToggleSearchResults);
  AddDefault(C, 'Toggle view Watches', lisKMToggleViewWatches, ecToggleWatches);
  AddDefault(C, 'Toggle view Breakpoints', lisKMToggleViewBreakpoints, ecToggleBreakPoints);
  AddDefault(C, 'Toggle view Local Variables', lisKMToggleViewLocalVariables, ecToggleLocals);
  AddDefault(C, 'Toggle view Threads', lisKMToggleViewThreads, ecViewThreads);
  if HasConsoleSupport then
  AddDefault(C, 'Toggle view Terminal Output', lisKMToggleViewPseudoTerminal, ecViewPseudoTerminal);
  AddDefault(C, 'Toggle view Call Stack', lisKMToggleViewCallStack, ecToggleCallStack);
  AddDefault(C, 'Toggle view Registers', lisKMToggleViewRegisters, ecToggleRegisters);
  AddDefault(C, 'Toggle view Assembler', lisKMToggleViewAssembler, ecToggleAssembler);
  AddDefault(C, 'Toggle view Event Log', lisKMToggleViewDebugEvents, ecToggleDebugEvents);
  AddDefault(C, 'Toggle view Debugger Output', lisKMToggleViewDebuggerOutput, ecToggleDebuggerOut);
  AddDefault(C, 'Toggle view Debug History', lisKMToggleViewHistory, ecViewHistory);
  AddDefault(C, 'View Unit Dependencies', lisMenuViewUnitDependencies, ecViewUnitDependencies);
  AddDefault(C, 'View Unit Info', lisKMViewUnitInfo, ecViewUnitInfo);
  AddDefault(C, 'Toggle between Unit and Form', lisKMToggleBetweenUnitAndForm, ecToggleFormUnit);
  AddDefault(C, 'View Anchor Editor', lisMenuViewAnchorEditor, ecViewAnchorEditor);
  AddDefault(C, 'View Tab Order', lisMenuViewTabOrder, ecViewTabOrder);
  AddDefault(C, 'Toggle view component palette', lisKMToggleViewComponentPalette, ecToggleCompPalette);
  AddDefault(C, 'Toggle view IDE speed buttons', lisKMToggleViewIDESpeedButtons, ecToggleIDESpeedBtns);

  // project menu
  C:=Categories[AddCategory('ProjectMenu',srkmCatProjectMenu,nil)];
  AddDefault(C, 'New project', lisKMNewProject, ecNewProject);
  AddDefault(C, 'New project from file', lisKMNewProjectFromFile, ecNewProjectFromFile);
  AddDefault(C, 'Open project', lisOpenProject2, ecOpenProject);
  AddDefault(C, 'Close project', lisKMCloseProject, ecCloseProject);
  AddDefault(C, 'Save project', lisKMSaveProject, ecSaveProject);
  AddDefault(C, 'Save project as', lisKMSaveProjectAs, ecSaveProjectAs);
  AddDefault(C, 'Publish project', lisKMPublishProject, ecPublishProject);
  AddDefault(C, 'Project Inspector', lisMenuProjectInspector, ecProjectInspector);
  AddDefault(C, 'Add editor file to Project', lisMenuAddToProject, ecAddCurUnitToProj);
  AddDefault(C, 'Remove active unit from project', lisKMRemoveActiveFileFromProject, ecRemoveFromProj);
  AddDefault(C, 'View Units', lisHintViewUnits, ecViewProjectUnits);
  AddDefault(C, 'View Forms', lisHintViewForms, ecViewProjectForms);
  AddDefault(C, 'View project source', lisKMViewProjectSource, ecViewProjectSource);
  AddDefault(C, 'View project options', lisKMViewProjectOptions, ecProjectOptions);

  // run menu
  C:=Categories[AddCategory('RunMenu',srkmCatRunMenu,nil)];
  AddDefault(C, 'Compile project/program', lisKMCompileProjectProgram, ecCompile);
  AddDefault(C, 'Build project/program', lisKMBuildProjectProgram, ecBuild);
  AddDefault(C, 'Quick compile, no linking', lisKMQuickCompileNoLinking, ecQuickCompile);
  AddDefault(C, 'Clean up and compile project/program', lisKMCleanUpCompiled, ecCleanUpCompiled);
  AddDefault(C, 'Abort building', lisKMAbortBuilding, ecAbortBuild);
  AddDefault(C, 'Run program', lisKMRunProgram, ecRun);
  AddDefault(C, 'Pause program', lisKMPauseProgram, ecPause);
  AddDefault(C, 'Show execution point', lisMenuShowExecutionPoint, ecShowExecutionPoint);
  AddDefault(C, 'Step into', lisMenuStepInto, ecStepInto);
  AddDefault(C, 'Step over', lisMenuStepOver, ecStepOver);
  AddDefault(C, 'Step into instr', lisMenuStepIntoInstr, ecStepIntoInstr);
  AddDefault(C, 'Step over instr', lisMenuStepOverInstr, ecStepOverInstr);
  AddDefault(C, 'Step into context', lisMenuStepIntoContext, ecStepIntoContext);
  AddDefault(C, 'Step over context', lisMenuStepOverContext, ecStepOverContext);
  AddDefault(C, 'Step out', lisMenuStepOut, ecStepOut);
  AddDefault(C, 'Run to cursor', lisMenuRunToCursor, ecRunToCursor);
  AddDefault(C, 'Stop program', lisKMStopProgram, ecStopProgram);
  AddDefault(C, 'Reset debugger', lisMenuResetDebugger, ecResetDebugger);
  AddDefault(C, 'Run parameters', dlgRunParameters, ecRunParameters);
  AddDefault(C, 'Build File', lisMenuBuildFile, ecBuildFile);
  AddDefault(C, 'Run File', lisMenuRunFile, ecRunFile);
  AddDefault(C, 'Config "Build File"', Format(lisKMConfigBuildFile, ['"', '"']), ecConfigBuildFile);
  AddDefault(C, 'Inspect', lisKMInspect, ecInspect);
  AddDefault(C, 'Evaluate/Modify', lisKMEvaluateModify, ecEvaluate);
  AddDefault(C, 'Add watch', lisKMAddWatch, ecAddWatch);
  AddDefault(C, 'Add source breakpoint', lisKMAddBpSource, ecAddBpSource);
  AddDefault(C, 'Add address breakpoint', lisKMAddBpAddress, ecAddBpAddress);
  AddDefault(C, 'Add data watchpoint', lisKMAddBpWatchPoint, ecAddBpDataWatch);

  // components menu
  C:=Categories[AddCategory('Components',srkmCatPackageMenu,nil)];
  AddDefault(C, 'New package', lisKMNewPackage, ecNewPackage);
  AddDefault(C, 'Open package', lisCompPalOpenPackage, ecOpenPackage);
  AddDefault(C, 'Open package file', lisKMOpenPackageFile, ecOpenPackageFile);
  AddDefault(C, 'Open package of current unit', lisMenuOpenPackageOfCurUnit, ecOpenPackageOfCurUnit);
  AddDefault(C, 'Add active unit to a package', lisMenuAddCurFileToPkg, ecAddCurFileToPkg);
  AddDefault(C, 'Package graph', lisMenuPackageGraph, ecPackageGraph);
  AddDefault(C, 'Configure installed packages', lisInstallUninstallPackages, ecEditInstallPkgs);
  AddDefault(C, 'Configure custom components', lisKMConfigureCustomComponents, ecConfigCustomComps);

  // tools menu
  C:=Categories[AddCategory(CommandCategoryToolMenuName,srkmCatToolMenu,nil)];
  AddDefault(C, 'External Tools settings', lisKMExternalToolsSettings, ecExtToolSettings);
  AddDefault(C, 'Example Projects', lisKMExampleProjects, ecManageExamples);
  AddDefault(C, 'Build Lazarus', lisMenuBuildLazarus, ecBuildLazarus);
  AddDefault(C, 'Configure "Build Lazarus"',
    Format(lisConfigureBuildLazarus, ['"', '"']), ecConfigBuildLazarus);
  AddDefault(C, 'Diff editor files', lisKMDiffEditorFiles, ecDiff);
  AddDefault(C, 'Convert DFM file to LFM', lisKMConvertDFMFileToLFM, ecConvertDFM2LFM);
  AddDefault(C, 'Convert Delphi unit to Lazarus unit',
    lisKMConvertDelphiUnitToLazarusUnit, ecConvertDelphiUnit);
  AddDefault(C, 'Convert Delphi project to Lazarus project',
    lisKMConvertDelphiProjectToLazarusProject, ecConvertDelphiProject);
  AddDefault(C, 'Convert Delphi package to Lazarus package',
    lisKMConvertDelphiPackageToLazarusPackage, ecConvertDelphiPackage);
  AddDefault(C, 'Convert encoding', lisConvertEncodingOfProjectsPackages, ecConvertEncoding);

  // environment (in Tools menu)
  C:=Categories[AddCategory('EnvironmentMenu',srkmCatEnvMenu,nil)];
  AddDefault(C, 'General environment options', srkmecEnvironmentOptions, ecEnvironmentOptions);
  AddDefault(C, 'Rescan FPC source directory', lisMenuRescanFPCSourceDirectory, ecRescanFPCSrcDir);
  AddDefault(C, 'Edit Code Templates', lisKMEditCodeTemplates, ecEditCodeTemplates);
  AddDefault(C, 'CodeTools defines editor', lisKMCodeToolsDefinesEditor, ecCodeToolsDefinesEd);

  // help menu
  C:=Categories[AddCategory('HelpMenu',srkmCarHelpMenu,nil)];
  AddDefault(C, 'About Lazarus', lisAboutLazarus, ecAboutLazarus);
  AddDefault(C, 'Online Help', lisMenuOnlineHelp, ecOnlineHelp);
  AddDefault(C, 'Context sensitive help', lisKMContextSensitiveHelp, ecContextHelp);
  AddDefault(C, 'Edit context sensitive help', lisKMEditContextSensitiveHelp, ecEditContextHelp);
  AddDefault(C, 'Reporting a bug', srkmecReportingBug, ecReportingBug);
  AddDefault(C, 'Focus hint', lisFocusHint, ecFocusHint);

  // designer  - without menu items in the IDE bar (at least not directly)
  C:=Categories[AddCategory('Designer',lisKeyCatDesigner,IDECmdScopeDesignerOnly)];
  AddDefault(C, 'Copy selected Components to clipboard',
    lisKMCopySelectedComponentsToClipboard, ecDesignerCopy);
  AddDefault(C, 'Cut selected Components to clipboard',
    lisKMCutSelectedComponentsToClipboard, ecDesignerCut);
  AddDefault(C, 'Paste Components from clipboard',
    lisKMPasteComponentsFromClipboard, ecDesignerPaste);
  AddDefault(C, 'Select parent component', lisDsgSelectParentComponent, ecDesignerSelectParent);
  AddDefault(C, 'Move component to front', lisDsgOrderMoveToFront, ecDesignerMoveToFront);
  AddDefault(C, 'Move component to back', lisDsgOrderMoveToBack, ecDesignerMoveToBack);
  AddDefault(C, 'Move component one forward', lisDsgOrderForwardOne, ecDesignerForwardOne);
  AddDefault(C, 'Move component one back', lisDsgOrderBackOne, ecDesignerBackOne);

  // object inspector - without menu items in the IDE bar (at least no direct)
  C:=Categories[AddCategory('Object Inspector',lisKeyCatObjInspector,IDECmdScopeObjectInspectorOnly)];

  // custom keys (for experts, task groups, dynamic menu items, etc)
  C:=Categories[AddCategory(CommandCategoryCustomName,lisKeyCatCustom,nil)];
end;

procedure TKeyCommandRelationList.Clear;
var a:integer;
begin
  fLoadedKeyCommands.FreeAndClear;
  for a:=0 to FRelations.Count-1 do
    Relations[a].Free;
  FRelations.Clear;
  for a:=0 to fCategories.Count-1 do
    Categories[a].Free;
  fCategories.Clear;
end;

function TKeyCommandRelationList.GetRelation(Index:integer):TKeyCommandRelation;
begin
  Assert((Index>=0) and (Index<Count), Format('[TKeyCommandRelationList.GetRelation] '
    + 'Index (%d) out of bounds. Count=%d', [Index, Count]));
  Result:= TKeyCommandRelation(FRelations[Index]);
end;

function TKeyCommandRelationList.GetRelationCount:integer;
begin
  Result:=FRelations.Count;
end;

function TKeyCommandRelationList.Count:integer;
begin
  Result:=FRelations.Count;
end;

function TKeyCommandRelationList.GetNewOrExisting(Category: TIDECommandCategory;
  Command: TIDECommand): TKeyCommandRelation;
begin
  Result:=GetNewOrExisting(Category,
              Command.Name,Command.LocalizedName,
              Command.Command,Command.ShortcutA,Command.ShortcutB,
              Command.OnExecute,Command.OnExecuteProc);
end;

function TKeyCommandRelationList.GetNewOrExisting(Category: TIDECommandCategory;
  const Name, LocalizedName: string; Command:word; TheKeyA, TheKeyB: TIDEShortCut;
  const OnExecuteMethod: TNotifyEvent;
  const OnExecuteProc: TNotifyProcedure): TKeyCommandRelation;
var
  LoadedKey: TLoadedKeyCommand;
  AVLNode: TAvgLvlTreeNode;
begin
  AVLNode:=fLoadedKeyCommands.FindKey(Pointer(Name),@CompareNameWithLoadedKeyCommand);
  if AVLNode=nil then begin
    // new key
    LoadedKey:=TLoadedKeyCommand.Create;
    LoadedKey.Name:=Name;
    LoadedKey.ShortcutA:=TheKeyA;
    LoadedKey.ShortcutB:=TheKeyB;
    LoadedKey.DefaultShortcutA:=TheKeyA;
    LoadedKey.DefaultShortcutB:=TheKeyB;
    fLoadedKeyCommands.Add(LoadedKey);
  end else begin
    LoadedKey:=TLoadedKeyCommand(AVLNode.Data);
    LoadedKey.DefaultShortcutA:=TheKeyA;
    LoadedKey.DefaultShortcutB:=TheKeyB;
    // old key, values were loaded (key is registered after loading keymapping)
    TheKeyA:=LoadedKey.ShortcutA;
    TheKeyB:=LoadedKey.ShortcutB;
  end;
  Result:=TKeyCommandRelation.Create(Category,Name,LocalizedName,
                          Command,TheKeyA,TheKeyB,OnExecuteMethod,OnExecuteProc);
end;

function TKeyCommandRelationList.AddDefault(Category: TIDECommandCategory;
  const Name, LocalizedName: string; Command: word): integer;
var
  CommandRelation: TKeyCommandRelation;
begin
  CommandRelation:=GetNewOrExisting(Category,Name,LocalizedName,Command,
                                    CleanIDEShortCut,CleanIDEShortCut);
  CommandRelation.GetDefaultKeyForCommand;
  Result:=FRelations.Add(CommandRelation);
end;

procedure TKeyCommandRelationList.SetExtToolCount(NewCount: integer);
var i: integer;
  ExtToolCat: TIDECommandCategory;
  ExtToolRelation: TKeyCommandRelation;
  ToolName: string;
begin
  if NewCount=fExtToolCount then exit;
  ExtToolCat:=FindCategoryByName(CommandCategoryToolMenuName);
  if NewCount>fExtToolCount then begin
    // increase available external tool commands
    while NewCount>fExtToolCount do begin
      ToolName:=Format(srkmecExtTool,[fExtToolCount]);
      FRelations.Add(GetNewOrExisting(ExtToolCat,ToolName,ToolName,
           ecExtToolFirst+fExtToolCount,CleanIDEShortCut,CleanIDEShortCut));
      inc(fExtToolCount);
    end;
  end else begin
    // decrease available external tool commands
    // they are always at the end of the Tools menu
    i:=ExtToolCat.Count-1;
    while (i>=0) and (fExtToolCount>NewCount) do begin
      if TObject(ExtToolCat[i]) is TKeyCommandRelation then begin
        ExtToolRelation:=TKeyCommandRelation(ExtToolCat[i]);
        if (ExtToolRelation.Command>=ecExtToolFirst)
        and (ExtToolRelation.Command<=ecExtToolLast) then begin
          fRelations.Remove(ExtToolRelation);
          ExtToolCat.Delete(i);
          dec(fExtToolCount);
        end;
      end;
      dec(i);
    end;
  end;
end;

function TKeyCommandRelationList.LoadFromXMLConfig(
  XMLConfig:TXMLConfig; const Path: String):boolean;
var
  a,b,p:integer;
  FileVersion: integer;
  Name: String;
  NewValue: String;

  function ReadNextInt:integer;
  begin
    Result:=0;
    while (p<=length(NewValue)) and (not (NewValue[p] in ['0'..'9']))
      do inc(p);
    while (p<=length(NewValue)) and (NewValue[p] in ['0'..'9'])
    and (Result<$10000)do begin
      Result:=Result*10+ord(NewValue[p])-ord('0');
      inc(p);
    end;
  end;

  function IntToShiftState(i:integer):TShiftState;
  begin
    Result:=[];
    if (i and 1)>0 then Include(Result,ssCtrl);
    if (i and 2)>0 then Include(Result,ssShift);
    if (i and 4)>0 then Include(Result,ssAlt);
    if (i and 8)>0 then Include(Result,ssMeta);
    if (i and 16)>0 then Include(Result,ssSuper);
  end;

  function OldKeyValuesToStr(const ShortcutA, ShortcutB: TIDEShortCut): string;
  begin
    Result:=IntToStr(ShortcutA.Key1) + ',' + ShiftStateToCfgStr(ShortcutA.Shift1) + ',' +
            IntToStr(ShortcutB.Key1) + ',' + ShiftStateToCfgStr(ShortcutB.Shift1);
  end;

  function FixShift(Shift: TShiftState): TShiftState;
  begin
    Result:=Shift;
    {$IFDEF LCLcarbon}
    if (FileVersion<5) and (Result*[ssCtrl,ssMeta]=[ssCtrl]) then
      Result:=Result-[ssCtrl]+[ssMeta];
    {$ENDIF}
  end;

  procedure Load(SubPath: string; out Key, DefaultKey: TIDEShortCut);
  begin
    DefaultKey:=CleanIDEShortCut;
    if XMLConfig.GetValue(SubPath+'Default',True) then begin
      Key:=CleanIDEShortCut;
    end else begin
      // not default
      key.Key1:=XMLConfig.GetValue(SubPath+'Key1',VK_UNKNOWN);
      key.Shift1:=CfgStrToShiftState(XMLConfig.GetValue(SubPath+'Shift1',''));
      key.Key2:=XMLConfig.GetValue(SubPath+'Key2',VK_UNKNOWN);
      key.Shift2:=CfgStrToShiftState(XMLConfig.GetValue(SubPath+'Shift2',''));
      if CompareIDEShortCuts(@Key,@CleanIDEShortCut)=0 then
        // this key is empty, mark it so that it differs from default
        key.Shift2:=[ssShift];
    end;
  end;

// LoadFromXMLConfig
var
  Key1, Key2: word;
  Shift1, Shift2: TShiftState;
  Cnt: LongInt;
  SubPath: String;
  AVLNode: TAvgLvlTreeNode;
  LoadedKey: TLoadedKeyCommand;
begin
  //debugln('TKeyCommandRelationList.LoadFromXMLConfig A ');
  FileVersion:=XMLConfig.GetValue(Path+'Version/Value',0);
  ExtToolCount:=XMLConfig.GetValue(Path+'ExternalToolCount/Value',0);

  if FileVersion>5 then begin
    Cnt:=XMLConfig.GetValue(Path+'Count',0);
    // load all keys from the config, this may be more than the current relations
    for a:=1 to Cnt do begin
      SubPath:=Path+'Item'+IntToStr(a)+'/';
      Name:=XMLConfig.GetValue(SubPath+'Name','');
      if Name='' then continue;
      AVLNode:=fLoadedKeyCommands.FindKey(Pointer(Name),
                                          @CompareNameWithLoadedKeyCommand);
      if AVLNode<>nil then begin
        LoadedKey:=TLoadedKeyCommand(AVLNode.Data);
      end else begin
        LoadedKey:=TLoadedKeyCommand.Create;
        LoadedKey.Name:=Name;
        fLoadedKeyCommands.Add(LoadedKey);
      end;
      Load(SubPath+'KeyA/',LoadedKey.ShortcutA,LoadedKey.DefaultShortcutA);
      Load(SubPath+'KeyB/',LoadedKey.ShortcutB,LoadedKey.DefaultShortcutB);
    end;
    // apply
    for a:=0 to FRelations.Count-1 do begin
      Name:=Relations[a].Name;
      if Name='' then continue;
      AVLNode:=fLoadedKeyCommands.FindKey(Pointer(Name),
                                          @CompareNameWithLoadedKeyCommand);
      if AVLNode<>nil then begin
        // there is a value in the config
        LoadedKey:=TLoadedKeyCommand(AVLNode.Data);
        if LoadedKey.IsShortcutADefault then
          Relations[a].ShortcutA:=Relations[a].DefaultShortcutA
        else
          Relations[a].ShortcutA:=LoadedKey.ShortcutA;
        if LoadedKey.IsShortcutBDefault then
          Relations[a].ShortcutB:=Relations[a].DefaultShortcutB
        else
          Relations[a].ShortcutB:=LoadedKey.ShortcutB;
      end else begin
        // no value in config => use default
        Relations[a].ShortcutA:=Relations[a].DefaultShortcutA;
        Relations[a].ShortcutB:=Relations[a].DefaultShortcutB;
      end;
    end;
  end else begin
    // FileVersion<=5
    for a:=0 to FRelations.Count-1 do begin
      Name:=lowercase(Relations[a].Name);
      for b:=1 to length(Name) do
        if not (Name[b] in ['a'..'z','A'..'Z','0'..'9']) then Name[b]:='_';

      if FileVersion<2 then
        NewValue:=XMLConfig.GetValue(Path+Name,'')
      else
        NewValue:=XMLConfig.GetValue(Path+Name+'/Value','');
      //if Relations[a].Command=ecBlockIndent then debugln('  NewValue=',NewValue);
      if NewValue='' then begin
        Relations[a].ShortcutA:=Relations[a].DefaultShortcutA;
        Relations[a].ShortcutB:=Relations[a].DefaultShortcutB;
      end else begin
        p:=1;
        Key1:=word(ReadNextInt);
        Shift1:=FixShift(IntToShiftState(ReadNextInt));
        if FileVersion>2 then begin
          Key2:=word(ReadNextInt);
          Shift2:=FixShift(IntToShiftState(ReadNextInt));
        end else begin
          Key2:=VK_UNKNOWN;
          Shift2:=[];
        end;
        Relations[a].ShortcutA:=IDEShortCut(Key1, Shift1, Key2, Shift2);

        Key1:=word(ReadNextInt);
        Shift1:=FixShift(IntToShiftState(ReadNextInt));
        if FileVersion>2 then begin
          Key2:=word(ReadNextInt);
          Shift2:=FixShift(IntToShiftState(ReadNextInt));
        end else begin
          Key2:=VK_UNKNOWN;
          Shift2:=[];
        end;
        Relations[a].ShortcutB:=IDEShortCut(Key1, Shift1, Key2, Shift2);
      end;
    end;
  end;
  Result:=true;
end;

function TKeyCommandRelationList.SaveToXMLConfig(
  XMLConfig:TXMLConfig; const Path: String):boolean;

  procedure Store(const SubPath: string; Key, DefaultKey: TIDEShortCut);
  var
    IsDefault: boolean;
    s: TShiftState;
  begin
    IsDefault:=CompareIDEShortCuts(@Key,@DefaultKey)=0;
    XMLConfig.SetDeleteValue(SubPath+'Default',IsDefault,True);
    if IsDefault then begin
      // clear values
      XMLConfig.SetDeleteValue(SubPath+'Key1',0,0);
      XMLConfig.SetDeleteValue(SubPath+'Shift1','','');
      XMLConfig.SetDeleteValue(SubPath+'Key2',0,0);
      XMLConfig.SetDeleteValue(SubPath+'Shift2','','');
    end else begin
      // store values
      XMLConfig.SetDeleteValue(SubPath+'Key1',key.Key1,VK_UNKNOWN);
      if key.Key1=VK_UNKNOWN then
        s:=[]
      else
        s:=key.Shift1;
      XMLConfig.SetDeleteValue(SubPath+'Shift1',ShiftStateToCfgStr(s),ShiftStateToCfgStr([]));
      XMLConfig.SetDeleteValue(SubPath+'Key2',key.Key2,VK_UNKNOWN);
      if key.Key2=VK_UNKNOWN then
        s:=[]
      else
        s:=key.Shift2;
      XMLConfig.SetDeleteValue(SubPath+'Shift2',ShiftStateToCfgStr(s),ShiftStateToCfgStr([]));
    end;
  end;

var a: integer;
  Name: String;
  AVLNode: TAvgLvlTreeNode;
  LoadedKey: TLoadedKeyCommand;
  Cnt: Integer;
  SubPath: String;
begin
  XMLConfig.SetValue(Path+'Version/Value',KeyMappingFormatVersion);
  XMLConfig.SetDeleteValue(Path+'ExternalToolCount/Value',ExtToolCount,0);
  // save shortcuts to fLoadedKeyCommands
  for a:=0 to FRelations.Count-1 do begin
    Name:=Relations[a].Name;
    if Name='' then continue;
    AVLNode:=fLoadedKeyCommands.FindKey(Pointer(Name),
                                        @CompareNameWithLoadedKeyCommand);
    if AVLNode<>nil then begin
      LoadedKey:=TLoadedKeyCommand(AVLNode.Data);
    end else begin
      LoadedKey:=TLoadedKeyCommand.Create;
      LoadedKey.Name:=Name;
      fLoadedKeyCommands.Add(LoadedKey);
      LoadedKey.DefaultShortcutA:=Relations[a].DefaultShortcutA;
      LoadedKey.DefaultShortcutB:=Relations[a].DefaultShortcutB;
    end;
    LoadedKey.ShortcutA:=Relations[a].ShortcutA;
    LoadedKey.ShortcutB:=Relations[a].ShortcutB;
  end;
  // save keys to config (including the one that were read from the last config
  //                      and were not used)
  Cnt:=0;
  AVLNode:=fLoadedKeyCommands.FindLowest;
  while AVLNode<>nil do begin
    LoadedKey:=TLoadedKeyCommand(AVLNode.Data);
    if (not LoadedKey.IsShortcutADefault) or (not LoadedKey.IsShortcutBDefault)
    then begin
      inc(Cnt);
      //DebugLn(['TKeyCommandRelationList.SaveToXMLConfig CUSTOM ',LoadedKey.AsString]);
      SubPath:=Path+'Item'+IntToStr(Cnt)+'/';
      XMLConfig.SetValue(SubPath+'Name',LoadedKey.Name);
      Store(SubPath+'KeyA/',LoadedKey.ShortcutA,LoadedKey.DefaultShortcutA);
      Store(SubPath+'KeyB/',LoadedKey.ShortcutB,LoadedKey.DefaultShortcutB);
    end;
    AVLNode:=fLoadedKeyCommands.FindSuccessor(AVLNode);
  end;
  XMLConfig.SetDeleteValue(Path+'Count',Cnt,0);
  Result:=true;
end;

function TKeyCommandRelationList.Find(Key: TIDEShortCut;
  IDEWindowClass: TCustomFormClass): TKeyCommandRelation;
var
  a:integer;
begin
  Result:=nil;
  //debugln(['TKeyCommandRelationList.Find START ',DbgSName(IDEWindowClass)]);
  //if IDEWindowClass=nil then RaiseGDBException('');
  if Key.Key1=VK_UNKNOWN then exit;
  for a:=0 to FRelations.Count-1 do
    with Relations[a] do begin
      //if Command=ecDesignerSelectParent then
      //  debugln('TKeyCommandRelationList.Find A ',Category.Scope.Name,' ',dbgsName(IDEWindowClass),' ',dbgs(IDECmdScopeDesignerOnly.IDEWindowClassCount),' ',dbgsName(IDECmdScopeDesignerOnly.IDEWindowClasses[0]));
      //debugln(['TKeyCommandRelationList.Find ',Name,' HasScope=',Category.Scope<>nil,' ',KeyAndShiftStateToEditorKeyString(ShortcutA),' ',KeyAndShiftStateToEditorKeyString(Key),' ',(Category.Scope<>nil)      and (not Category.Scope.HasIDEWindowClass(IDEWindowClass))]);
      //if (Category.Scope<>nil) and (Category.Scope.IDEWindowClassCount>0) then
      //  debugln(['TKeyCommandRelationList.Find ',DbgSName(Category.Scope.IDEWindowClasses[0]),' ',DbgSName(IDEWindowClass)]);

      if (Category.Scope<>nil)
      and (not Category.Scope.HasIDEWindowClass(IDEWindowClass)) then continue;
      if ((ShortcutA.Key1=Key.Key1) and (ShortcutA.Shift1=Key.Shift1) and
          (ShortcutA.Key2=Key.Key2) and (ShortcutA.Shift2=Key.Shift2))
      or ((ShortcutB.Key1=Key.Key1) and (ShortcutB.Shift1=Key.Shift1) and
          (ShortcutB.Key2=Key.Key2) and (ShortcutB.Shift2=Key.Shift2)) then
      begin
        Result:=Relations[a];
        exit;
      end;
    end;
end;

function TKeyCommandRelationList.FindIDECommand(ACommand: word): TIDECommand;
begin
  Result:=FindByCommand(ACommand);
end;

function TKeyCommandRelationList.FindByCommand(ACommand: word):TKeyCommandRelation;
var a:integer;
begin
  Result:=nil;
  for a:=0 to FRelations.Count-1 do with Relations[a] do
    if (Command=ACommand) then begin
      Result:=Relations[a];
      exit;
    end;
end;

procedure TKeyCommandRelationList.AssignTo(
  ASynEditKeyStrokes: TSynEditKeyStrokes; IDEWindowClass: TCustomFormClass);
var
  a,b,MaxKeyCnt,KeyCnt:integer;
  Key: TSynEditKeyStroke;
  CurRelation: TKeyCommandRelation;
begin
  try
    ASynEditKeyStrokes.UsePluginOffset := True;
    for a:=0 to FRelations.Count-1 do begin
      CurRelation:=Relations[a];
      if (CurRelation.ShortcutA.Key1=VK_UNKNOWN)
      or ((IDEWindowClass<>nil) and (CurRelation.Category.Scope<>nil)
          and (not CurRelation.Category.Scope.HasIDEWindowClass(IDEWindowClass)))
      then
        MaxKeyCnt:=0
      else if CurRelation.ShortcutB.Key1=VK_UNKNOWN then
        MaxKeyCnt:=1
      else
        MaxKeyCnt:=2;
      KeyCnt:=1;
      b:=ASynEditKeyStrokes.Count-1;
      // replace keys
      while b>=0 do begin
        Key:=ASynEditKeyStrokes[b];
        if Key.Command=CurRelation.Command then begin
          if KeyCnt>MaxKeyCnt then begin
            // All keys with this command are already defined
            // -> delete this one
            Key.Free;
          end else if KeyCnt=1 then begin
            // Define key1 for this command
            Key.Key:=CurRelation.ShortcutA.Key1;
            Key.Shift:=CurRelation.ShortcutA.Shift1;
            Key.Key2:=CurRelation.ShortcutA.Key2;
            Key.Shift2:=CurRelation.ShortcutA.Shift2;
          end else if KeyCnt=2 then begin
            // Define key2 for this command
            Key.Key:=CurRelation.ShortcutB.Key1;
            Key.Shift:=CurRelation.ShortcutB.Shift1;
            Key.Key2:=CurRelation.ShortcutB.Key2;
            Key.Shift2:=CurRelation.ShortcutB.Shift2;
          end;
          inc(KeyCnt);
        end
        else
        if MaxKeyCnt > 0 then begin
          // Key with a different ecCommand => Remove if it has a conflicting keystroke(s)
          if ( (CurRelation.ShortcutA.Key1 <> VK_UNKNOWN) and
               (Key.Key = CurRelation.ShortcutA.Key1)     and
               (Key.Shift = CurRelation.ShortcutA.Shift1) and
               ( (CurRelation.ShortcutA.Key2 = VK_UNKNOWN) or
                 ( (Key.Key2 = CurRelation.ShortcutA.Key2) and
                   (Key.Shift2 = CurRelation.ShortcutA.Shift2) )
               ) )
          OR ( (CurRelation.ShortcutB.Key1 <> VK_UNKNOWN) and
               (Key.Key = CurRelation.ShortcutB.Key1)     and
               (Key.Shift = CurRelation.ShortcutB.Shift1) and
               ( (CurRelation.ShortcutB.Key2 = VK_UNKNOWN) or
                 ( (Key.Key2 = CurRelation.ShortcutB.Key2) and
                   (Key.Shift2 = CurRelation.ShortcutB.Shift2) )
               ) )
          then
            Key.Free;
        end;
        dec(b);
      end;
      // Add missing keys
      while KeyCnt<=MaxKeyCnt do begin
        Key:=ASynEditKeyStrokes.Add;
        Key.Command:=CurRelation.Command;
        if KeyCnt=1 then begin
          Key.Key:=CurRelation.ShortcutA.Key1;
          Key.Shift:=CurRelation.ShortcutA.Shift1;
          Key.Key2:=CurRelation.ShortcutA.Key2;
          Key.Shift2:=CurRelation.ShortcutA.Shift2;
        end else begin
          Key.Key:=CurRelation.ShortcutB.Key1;
          Key.Shift:=CurRelation.ShortcutB.Shift1;
          Key.Key2:=CurRelation.ShortcutB.Key2;
          Key.Shift2:=CurRelation.ShortcutB.Shift2;
        end;
        inc(KeyCnt);
      end;
    end;
  finally
    ASynEditKeyStrokes.UsePluginOffset := False;
  end;
end;

procedure TKeyCommandRelationList.Assign(List: TKeyCommandRelationList);
var
  i: Integer;
  OtherCategory: TIDECommandCategory;
  OurCategory: TIDECommandCategory;
  OtherRelation: TKeyCommandRelation;
  OurRelation: TKeyCommandRelation;
begin
  // Add/assign categories
  for i:=0 to List.CategoryCount-1 do begin
    OtherCategory:=List.Categories[i];
    OurCategory:=FindCategoryByName(OtherCategory.Name);
    if OurCategory<>nil then begin
      // assign
      OurCategory.Description:=OtherCategory.Description;
      OurCategory.Scope:=OtherCategory.Scope;
    end else begin
      //DebugLn('TKeyCommandRelationList.Assign Add new category: ',OtherCategory.Name);
      AddCategory(OtherCategory.Name,OtherCategory.Description,OtherCategory.Scope);
    end;
  end;

  // Add/assign keys
  for i:=0 to List.Count-1 do begin
    OtherRelation:=List.Relations[i];
    OurRelation:=TKeyCommandRelation(FindCommandByName(OtherRelation.Name));
    if OurRelation<>nil then begin
      // assign
      OurRelation.Assign(OtherRelation);
    end else begin
      // Add
      //DebugLn('TKeyCommandRelationList.Assign Add new command: ',OtherRelation.Name);
      OurCategory:=FindCategoryByName(OtherRelation.Category.Name);
      OurRelation:=TKeyCommandRelation.Create(OtherRelation,OurCategory);
      fRelations.Add(OurRelation);
    end;
  end;

  // delete unneeded keys
  for i:=0 to CategoryCount-1 do begin
    OurCategory:=Categories[i];
    OtherCategory:=List.FindCategoryByName(OurCategory.Name);
    if OtherCategory=nil then begin
      //DebugLn('TKeyCommandRelationList.Assign remove unneeded category: ',OurCategory.Name);
      OurCategory.Free;
    end;
  end;

  // delete unneeded categories
  for i:=0 to Count-1 do begin
    OurRelation:=Relations[i];
    if List.FindCommandByName(OurRelation.Name)=nil then begin
      //DebugLn('TKeyCommandRelationList.Assign remove unneeded command: ',OurRelation.Name);
      OurRelation.Free;
    end;
  end;

  // copy ExtToolCount
  fExtToolCount:=List.ExtToolCount;
end;

procedure TKeyCommandRelationList.LoadScheme(const SchemeName: string);
var
  i: Integer;
  NewScheme: TKeyMapScheme;
begin
  NewScheme:=KeySchemeNameToSchemeType(SchemeName);
  for i:=0 to Count-1 do                  // set all keys to new scheme
    Relations[i].MapShortcut(NewScheme);
end;

function TKeyCommandRelationList.CreateUniqueCategoryName(const AName: string): string;
begin
  Result:=AName;
  if FindCategoryByName(Result)=nil then exit;
  Result:=CreateFirstIdentifier(Result);
  while FindCategoryByName(Result)<>nil do
    Result:=CreateNextIdentifier(Result);
end;

function TKeyCommandRelationList.CreateUniqueCommandName(const AName: string): string;
begin
  Result:=AName;
  if FindCommandByName(Result)=nil then exit;
  Result:=CreateFirstIdentifier(Result);
  while FindCommandByName(Result)<>nil do
    Result:=CreateNextIdentifier(Result);
end;

function TKeyCommandRelationList.CreateNewCommandID: word;
begin
  Result:=ecLazarusLast;
  while FindByCommand(Result)<>nil do inc(Result);
end;

function TKeyCommandRelationList.CreateCategory(Parent: TIDECommandCategory;
  const AName, Description: string; Scope: TIDECommandScope): TIDECommandCategory;
begin
  Result:=Categories[AddCategory(CreateUniqueCategoryName(AName),Description,Scope)];
end;

function TKeyCommandRelationList.CreateCommand(Category: TIDECommandCategory;
  const AName, Description: string; const TheShortcutA, TheShortcutB: TIDEShortCut;
  const OnExecuteMethod: TNotifyEvent;
  const OnExecuteProc: TNotifyProcedure): TIDECommand;
var
  NewName: String;
begin
  NewName:=CreateUniqueCommandName(AName);
  Result:=GetNewOrExisting(Category as TKeyCommandCategory,NewName,Description,
                        CreateNewCommandID,TheShortcutA,TheShortcutB,
                        OnExecuteMethod,OnExecuteProc);
  FRelations.Add(Result);
end;

function TKeyCommandRelationList.GetCategory(Index: integer): TIDECommandCategory;
begin
  Result:=TIDECommandCategory(fCategories[Index]);
end;

function TKeyCommandRelationList.CategoryCount: integer;
begin
  Result:=fCategories.Count;
end;

function TKeyCommandRelationList.AddCategory(const Name, Description: string;
  TheScope: TIDECommandScope): integer;
begin
  Result:=fCategories.Add(TKeyCommandCategory.Create(Name,Description,
                          TheScope));
end;

function TKeyCommandRelationList.FindCategoryByName(const CategoryName: string
  ): TIDECommandCategory;
var i: integer;
begin
  for i:=0 to CategoryCount-1 do
    if CategoryName=Categories[i].Name then begin
      Result:=Categories[i];
      exit;
    end;
  Result:=nil;
end;

function TKeyCommandRelationList.FindCommandByName(const CommandName: string
  ): TIDECommand;
var i: integer;
begin
  for i:=0 to RelationCount-1 do
    if CompareText(CommandName,Relations[i].Name)=0 then begin
      Result:=Relations[i];
      exit;
    end;
  Result:=nil;
end;

function TKeyCommandRelationList.TranslateKey(Key: word; Shift: TShiftState;
  IDEWindowClass: TCustomFormClass; UseLastKey: boolean): word;
{ If UseLastKey = true then only search for commmands with one key.
  If UseLastKey = false then search first for a command with a two keys
    combination (i.e. the last key plus this one)
    and then for a command with one key.
  If no command was found the key is stored in fLastKey.Key1.
}
var
  ARelation: TKeyCommandRelation;
begin
  //debugln(['TKeyCommandRelationList.TranslateKey ',DbgSName(IDEWindowClass)]);
  //if IDEWindowClass=nil then DumpStack;
  Result:=ecNone;
  case Key of
  VK_UNDEFINED,VK_UNKNOWN,
  VK_CONTROL,VK_LCONTROL,VK_RCONTROL,
  VK_SHIFT,VK_LSHIFT,VK_RSHIFT,
  VK_LBUTTON,VK_MBUTTON,VK_RBUTTON,
  VK_LWIN,VK_RWIN:
    begin
      //debugln(['TKeyCommandRelationList.TranslateKey ignoring ',dbgs(Key)]);
      exit;
    end;
  end;
  if UseLastKey and (fLastKey.Key1<>VK_UNKNOWN) then begin
    // the last key had no command
    // => try a two key combination command
    fLastKey.Key2 := Key;
    fLastKey.Shift2 := Shift;
    ARelation := Find(fLastKey,IDEWindowClass);
  end else begin
    ARelation := nil;
  end;
  if ARelation = nil then
  begin
    // search for a one key command
    fLastKey.Key1 := Key;
    fLastKey.Shift1 := Shift;
    fLastKey.Key2 := VK_UNKNOWN;
    fLastKey.Shift2 := [];
    ARelation := Find(fLastKey,IDEWindowClass);
  end;
  if ARelation<>nil then
  begin
    // the key has a command -> key was used => clear fLastKey
    fLastKey.Key1 := VK_UNKNOWN;
    fLastKey.Shift1 := [];
    fLastKey.Key2 := VK_UNKNOWN;
    fLastKey.Shift2 := [];
    Result:=ARelation.Command
  end;
end;

function TKeyCommandRelationList.IndexOf(ARelation: TKeyCommandRelation): integer;
begin
  Result:=fRelations.IndexOf(ARelation);
end;

function TKeyCommandRelationList.CommandToShortCut(ACommand: word): TShortCut;
var ARelation: TKeyCommandRelation;
begin
  ARelation:=FindByCommand(ACommand);
  if ARelation<>nil then
    Result:=ARelation.AsShortCut
  else
    Result:=VK_UNKNOWN;
end;

{ TKeyCommandCategory }

procedure TKeyCommandCategory.Clear;
begin
  fName:='';
  fDescription:='';
  inherited Clear;
end;

procedure TKeyCommandCategory.Delete(Index: Integer);
begin
  TObject(Items[Index]).Free;
  inherited Delete(Index);
end;

constructor TKeyCommandCategory.Create(const AName, ADescription: string;
  TheScope: TIDECommandScope);
begin
  inherited Create;
  FName:=AName;
  FDescription:=ADescription;
  FScope:=TheScope;
end;

{ TLoadedKeyCommand }

function TLoadedKeyCommand.IsShortcutADefault: boolean;
begin
  Result:=CompareIDEShortCuts(@ShortcutA,@DefaultShortcutA)=0;
end;

function TLoadedKeyCommand.IsShortcutBDefault: boolean;
begin
  Result:=CompareIDEShortCuts(@ShortcutB,@DefaultShortcutB)=0;
end;

function TLoadedKeyCommand.AsString: string;
begin
  Result:='Name="'+Name+'"'
    +' A='+KeyAndShiftStateToEditorKeyString(ShortcutA)
    +' DefA='+KeyAndShiftStateToEditorKeyString(DefaultShortcutA)
    +' B='+KeyAndShiftStateToEditorKeyString(ShortcutB)
    +' DefB='+KeyAndShiftStateToEditorKeyString(DefaultShortcutB)
    ;
end;

initialization
  RegisterKeyCmdIdentProcs({$IFDEF FPC}@{$ENDIF}IdentToIDECommand,
                           {$IFDEF FPC}@{$ENDIF}IDECommandToIdent);

end.

