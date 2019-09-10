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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils, contnrs, Laz_AVL_Tree,
  // LCL
  Forms, LCLType, LCLProc,
  // LazUtils
  Laz2_XMLCfg,
  // SynEdit
  SynEditKeyCmds, SynPluginTemplateEdit, SynPluginSyncroEdit, SynPluginMultiCaret,
  // IdeIntf
  IDECommands,
  // IDE
  LazarusIDEStrConsts, Debugger;

type
  TKeyMapScheme = (
    kmsLazarus,
    kmsClassic,
    kmsMacOSXApple,
    kmsMacOSXLaz,
    kmsDefaultToMac,
    kmsCustom
    );

const
  KeyMapSchemeNames: array[TKeyMapScheme] of string = (
    'default',
    'Classic',
    'MacOSXApple',
    'MacOSXLaz',
    'WindowsToMacOSX',
    'Custom'
    );

  (* SynEdit Plugins
     Offsets for the fixed ec... commands, defined in IDECommands
     Used in EditorOptions
  *)
  ecIdePTmplOffset      = ecSynPTmplEdNextCell - ecIdePTmplEdNextCell;
  ecIdePTmplOutOffset   = ecSynPTmplEdNextCell - ecIdePTmplEdOutNextCell;
  ecIdePSyncroOffset    = ecSynPSyncroEdNextCell - ecIdePSyncroEdNextCell;
  ecIdePSyncroOutOffset = ecSynPSyncroEdNextCell - ecIdePSyncroEdOutNextCell;
  ecIdePSyncroSelOffset = ecSynPSyncroEdStart    - ecIdePSyncroEdSelStart;

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
    FSkipSaving: Boolean;
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
    procedure GetDefaultKeyForWindowsScheme(AUseMetaKey: boolean=false);
    procedure GetDefaultKeyForClassicScheme;
    procedure GetDefaultKeyForMacOSXScheme;
    procedure GetDefaultKeyForMacOSXLazScheme;
  protected
    procedure Init; override;
  public
    function GetLocalizedName: string; override;
    property SkipSaving: Boolean read FSkipSaving write FSkipSaving;
  end;


  { TKeyStrokeList
    Specialized and optimized container for max. 3 TSynEditKeyStrokes }

  TKeyStrokeList = class
  private
    KeyStroke1: TSynEditKeyStroke;
    KeyStroke2: TSynEditKeyStroke;
    KeyStroke3: TSynEditKeyStroke;
    FCount: Integer;    // Can be max. 3.
    function GetItem(Index: Integer): TSynEditKeyStroke;
    procedure PutItem(Index: Integer; AValue: TSynEditKeyStroke);
  public
    procedure Add(aKeyStroke: TSynEditKeyStroke);
    property Items[Index: Integer]: TSynEditKeyStroke read GetItem write PutItem; default;
    property Count: integer read FCount;
  end;


  { TLoadedKeyCommand
    Used to keep shortcuts for unknown commands.
    A command can be unknown, if it is currently not registered, e.g.
    because the user started an IDE without the package that registered the command.
    When an IDE with the package is started the shortcut is restored. }

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
    fRelations: TFPList;    // list of TKeyCommandRelation
    fCategories: TFPList;   // list of TKeyCommandCategory
    fExtToolCount: integer;
    fLoadedKeyCommands: TAvlTree; // tree of TLoadedKeyCommand sorted for name
    fCmdRelCache: TAvlTree; // cache for TKeyCommandRelation sorted for command
    function AddRelation(CmdRel: TKeyCommandRelation): Integer;
    function GetRelation(Index: integer): TKeyCommandRelation;
    function GetRelationCount: integer;
    function AddCategory(const Name, Description: string;
                         TheScope: TIDECommandScope): integer;
    function SetKeyCommandToLoadedValues(Cmd: TKeyCommandRelation): TLoadedKeyCommand;
    function AddDefault(Category: TIDECommandCategory;
                        const Name, LocalizedName: string; Command: word):integer;
    procedure SetExtToolCount(NewCount: integer);
  protected
    function GetCategory(Index: integer): TIDECommandCategory; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DefineCommandCategories;
    procedure Clear;
    function Count: integer;
    function CategoryCount: integer; override;
    function Find(Key: TIDEShortCut; IDEWindowClass: TCustomFormClass): TKeyCommandRelation;
    function FindIDECommand(ACommand:word): TIDECommand; override;
    function FindByCommand(ACommand:word): TKeyCommandRelation;
    function FindCategoryByName(const CategoryName: string): TIDECommandCategory; override;
    function FindCommandByName(const CommandName: string): TIDECommand; override;
    function FindCommandsByShortCut(const ShortCutMask: TIDEShortCut;
      IDEWindowClass: TCustomFormClass = nil): TFPList; override;
    function RemoveShortCut(ShortCutMask: TIDEShortCut;
      IDEWindowClass: TCustomFormClass = nil): Integer; override;
    function TranslateKey(Key: word; Shift: TShiftState;
      IDEWindowClass: TCustomFormClass; UseLastKey: boolean = true): word;
    function IndexOf(ARelation: TKeyCommandRelation): integer;
    function CommandToShortCut(ACommand: word): TShortCut;
    function LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: String):boolean;
    function SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: String):boolean;
    procedure AssignTo(ASynEditKeyStrokes: TSynEditKeyStrokes;
                       IDEWindowClass: TCustomFormClass;
                       ACommandOffsetOffset: Integer = 0);
    procedure Assign(List: TKeyCommandRelationList);
    procedure LoadScheme(const SchemeName: string);
    function CreateUniqueCategoryName(const AName: string): string;
    function CreateUniqueCommandName(const AName: string): string;
    function CreateNewCommandID: word;
    function CreateCategory({%H-}Parent: TIDECommandCategory;
                            const AName, Description: string;
                            Scope: TIDECommandScope = nil): TIDECommandCategory; override;
    function CreateCommand(Category: TIDECommandCategory;
                           const AName, Description: string;
                           const TheShortcutA, TheShortcutB: TIDEShortCut;
                           const OnExecuteMethod: TNotifyEvent = nil;
                           const OnExecuteProc: TNotifyProcedure = nil
                           ): TIDECommand; override;
    procedure RemoveCommand(ACommand: TIDECommand);
  public
    property ExtToolCount: integer read fExtToolCount write SetExtToolCount;// in menu
    property Relations[Index:integer]: TKeyCommandRelation read GetRelation; default;
    property RelationCount:integer read GetRelationCount;
  end;

function IDEShortCutEmpty(const Key: TIDEShortCut): boolean;
function KeyAndShiftStateToEditorKeyString(const Key: TIDEShortCut): String;
function EditorCommandToDescriptionString(cmd: word): String;

function KeySchemeNameToSchemeType(const SchemeName: string): TKeyMapScheme;

function ShiftStateToCfgStr(Shift: TShiftState): string;
function KeyValuesToCfgStr(const ShortcutA, ShortcutB: TIDEShortCut): string;
function KeyValuesToCaptionStr(const ShortcutA, ShortcutB: TIDEShortCut;
  Brackets: Char = '['): String;
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

// Compare functions for fCmdRelCache
function CompareCmdRels(Data1, Data2: Pointer): integer;
var
  Key1: TKeyCommandRelation absolute Data1;
  Key2: TKeyCommandRelation absolute Data2;
begin
  Result:=Key1.Command - Key2.Command;
end;

function CompareCmdWithCmdRel(aCommand, Key: Pointer): integer;
var
  Cmd1, Cmd2: PtrInt;
  CmdRel: TKeyCommandRelation absolute Key;
begin
  {%H-}Pointer(Cmd1):=aCommand;
  Cmd2:=CmdRel.Command;
  Result:=Cmd1-Cmd2;
end;

// Compare functions for fLoadedKeyCommands
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
    ecLeft                    : Result:= srkmecKeyMapLeft;
    ecRight                   : Result:= srkmecKeyMapRight;
    ecUp                      : Result:= lisUp;
    ecDown                    : Result:= lisDown;
    ecWordLeft                : Result:= srkmecWordLeft;
    ecWordRight               : Result:= srkmecWordRight;
    ecWordEndLeft             : Result:= srkmecWordEndLeft;
    ecWordEndRight            : Result:= srkmecWordEndRight;
    ecHalfWordLeft            : Result:= srkmecHalfWordLeft;
    ecHalfWordRight           : Result:= srkmecHalfWordRight;
    ecSmartWordLeft           : Result:= srkmecSmartWordLeft;
    ecSmartWordRight          : Result:= srkmecSmartWordRight;
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
    ecStickySelection         : Result:= srkmecSelSticky;
    ecStickySelectionCol      : Result:= srkmecSelStickyCol;
    ecStickySelectionLine     : Result:= srkmecSelStickyLine;
    ecStickySelectionStop     : Result:= srkmecSelStickyStop;
    ecSelLeft                 : Result:= srkmecSelLeft;
    ecSelRight                : Result:= srkmecSelRight;
    ecSelUp                   : Result:= srkmecSelUp;
    ecSelDown                 : Result:= srkmecSelDown;
    ecSelWordLeft             : Result:= srkmecSelWordLeft;
    ecSelWordRight            : Result:= srkmecSelWordRight;
    ecSelWordEndLeft          : Result:= srkmecSelWordEndLeft;
    ecSelWordEndRight         : Result:= srkmecSelWordEndRight;
    ecSelHalfWordLeft         : Result:= srkmecSelHalfWordLeft;
    ecSelHalfWordRight        : Result:= srkmecSelHalfWordRight;
    ecSelSmartWordLeft        : Result:= srkmecSelSmartWordLeft;
    ecSelSmartWordRight       : Result:= srkmecSelSmartWordRight;
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
    ecUndo                    : Result:= lisUndo;
    ecRedo                    : Result:= lisRedo;
    ecCut                     : Result:= srkmecCut;
    ecCopy                    : Result:= srkmecCopy;
    ecPaste                   : Result:= srkmecPaste;
    ecMultiPaste              : Result:= srkmecMultiPaste;
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
    ecSetFreeBookmark         : Result:= srkmecSetFreeBookmark;
    ecClearBookmarkForFile    : Result:= srkmecClearBookmarkForFile;
    ecClearAllBookmark        : Result:= srkmecClearAllBookmark;
    ecPrevBookmark            : Result:= srkmecPrevBookmark;
    ecNextBookmark            : Result:= srkmecNextBookmark;
    ecGotoMarker0 ..
    ecGotoMarker9             : Result:= Format(srkmecGotoMarker,[cmd-ecGotoMarker0]);
    ecSetMarker0 ..
    ecSetMarker9              : Result:= Format(srkmecSetMarker,[cmd-ecSetMarker0]);
    ecToggleMarker0 ..
    ecToggleMarker9           : Result:= Format(srkmecToggleMarker,[cmd-ecToggleMarker0]);
    ecGotoBookmarks           : Result:= uemGotoBookmarks;
    ecToggleBookmarks         : Result:= uemToggleBookmarks;
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

    ecZoomOut         : Result := srkmecZoomOut;
    ecZoomIn          : Result := srkmecZoomIn;
    ecZoomNorm        : Result := dlfMouseSimpleButtonZoomReset;

    // multi caret
    ecPluginMultiCaretSetCaret          : Result := srkmecPluginMultiCaretSetCaret;
    ecPluginMultiCaretUnsetCaret        : Result := srkmecPluginMultiCaretUnsetCaret;
    ecPluginMultiCaretToggleCaret       : Result := srkmecPluginMultiCaretToggleCaret;
    ecPluginMultiCaretClearAll          : Result := srkmecPluginMultiCaretClearAll;

    ecPluginMultiCaretModeCancelOnMove  : Result := srkmecPluginMultiCaretModeCancelOnMove;
    ecPluginMultiCaretModeMoveAll       : Result := srkmecPluginMultiCaretModeMoveAll;


    // sourcenotebook
    ecNextEditor              : Result:= srkmecNextEditor;
    ecPrevEditor              : Result:= srkmecPrevEditor;
    ecPrevEditorInHistory     : Result:= srkmecPrevEditorInHistory;
    ecNextEditorInHistory     : Result:= srkmecNextEditorInHistory;
    ecMoveEditorLeft          : Result:= srkmecMoveEditorLeft;
    ecMoveEditorRight         : Result:= srkmecMoveEditorRight;
    ecMoveEditorLeftmost      : Result:= srkmecMoveEditorLeftmost;
    ecMoveEditorRightmost     : Result:= srkmecMoveEditorRightmost;
    ecToggleBreakPoint        : Result:= srkmecToggleBreakPoint;
    ecToggleBreakPointEnabled : Result:= srkmecToggleBreakPointEnabled;
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
    ecOpenUnit                : Result:= lisMenuOpenUnit;
    ecRevert                  : Result:= lisMenuRevert;
    ecSave                    : Result:= lisSave;
    ecSaveAs                  : Result:= lisMenuSaveAs;
    ecSaveAll                 : Result:= lisSaveAll;
    ecClose                   : Result:= lisClose;
    ecCloseAll                : Result:= lisMenuCloseAll;
    ecCloseOtherTabs          : Result:= uemCloseOtherPages;
    ecCloseRightTabs          : Result:= uemCloseOtherPagesRight;
    ecCleanDirectory          : Result:= lisMenuCleanDirectory;
    ecRestart                 : Result:= lisRestart;
    ecQuit                    : Result:= lisQuit;

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
    ecInsertGPLNoticeTranslated: Result:= srkmecInsertGPLNoticeTranslated;
    ecInsertLGPLNotice        : Result:= srkmecInsertLGPLNotice;
    ecInsertLGPLNoticeTranslated: Result:= srkmecInsertLGPLNoticeTranlated;
    ecInsertModifiedLGPLNotice: Result:= srkmecInsertModifiedLGPLNotice;
    ecInsertModifiedLGPLNoticeTranslated: Result:= srkmecInsertModifiedLGPLNoticeTranslated;
    ecInsertMITNotice         : Result:= srkmecInsertMITNotice;
    ecInsertMITNoticeTranslated: Result:= srkmecInsertMITNoticeTranslated;
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
    ecJumpToInterface         : Result:= lisMenuJumpToInterface;
    ecJumpToInterfaceUses     : Result:= lisMenuJumpToInterfaceUses;
    ecJumpToImplementation    : Result:= lisMenuJumpToImplementation;
    ecJumpToImplementationUses: Result:= lisMenuJumpToImplementationUses;
    ecJumpToInitialization    : Result:= lisMenuJumpToInitialization;
    ecJumpToProcedureHeader   : Result:= lisMenuJumpToProcedureHeader;
    ecJumpToProcedureBegin    : Result:= lisMenuJumpToProcedureBegin;
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
    ecViewMacroList           : Result:= srkmecViewEditorMacros;
    ecViewJumpHistory         : Result:= lisMenuViewJumpHistory;
    ecToggleCompPalette       : Result:= srkmecToggleCompPalette;
    ecToggleIDESpeedBtns      : Result:= srkmecToggleIDESpeedBtns;

    // codetools
    ecWordCompletion          : Result:= srkmecWordCompletion;
    ecCompleteCode            : Result:= lisMenuCompleteCode;
    ecCompleteCodeInteractive : Result:= lisMenuCompleteCodeInteractive;
    ecIdentCompletion         : Result:= dlgedidcomlet;
    ecShowCodeContext         : Result:= srkmecShowCodeContext;
    ecExtractProc             : Result:= srkmecExtractProc;
    ecFindIdentifierRefs      : Result:= srkmecFindIdentifierRefs;
    ecFindUsedUnitRefs        : Result:= lisMenuFindReferencesOfUsedUnit;
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
    ecProjectResaveFormsWithI18n: Result:= lisMenuResaveFormsWithI18n;
    ecPublishProject          : Result:= lisMenuPublishProject;
    ecProjectInspector        : Result:= lisMenuProjectInspector;
    ecAddCurUnitToProj        : Result:= lisMenuAddToProject;
    ecRemoveFromProj          : Result:= lisMenuRemoveFromProject;
    ecViewProjectUnits        : Result:= srkmecViewUnits;
    ecViewProjectForms        : Result:= srkmecViewForms;
    ecViewProjectSource       : Result:= lisMenuViewProjectSource;
    ecProjectOptions          : Result:= lisMenuProjectOptions;
    ecProjectChangeBuildMode  : Result:= lisChangeBuildMode;

    // run menu (menu string resource)
    ecCompile                 : Result:= srkmecCompile;
    ecBuild                   : Result:= srkmecBuild;
    ecQuickCompile            : Result:= srkmecQuickCompile;
    ecCleanUpAndBuild         : Result:= srkmecCleanUpAndBuild;
    ecBuildManyModes          : Result:= srkmecBuildManyModes;
    ecAbortBuild              : Result:= srkmecAbortBuild;
    ecRunWithoutDebugging     : Result:= srkmecRunWithoutDebugging;
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
    ecAttach                  : Result:= srkmecAttach;
    ecDetach                  : Result:= srkmecDetach;
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
    ecNewPkgComponent         : Result:= lisMenuPkgNewPackageComponent;
    ecPackageGraph            : Result:= lisMenuPackageGraph;
    ecPackageLinks            : Result:= lisMenuPackageLinks;
    ecEditInstallPkgs         : Result:= lisMenuEditInstallPkgs;
    ecConfigCustomComps       : Result:= lisMenuConfigCustomComps;

    // tools menu
    ecEnvironmentOptions      : Result:= srkmecEnvironmentOptions;
    ecRescanFPCSrcDir         : Result:= lisMenuRescanFPCSourceDirectory;
    ecEditCodeTemplates       : Result:= lisMenuEditCodeTemplates;
    ecCodeToolsDefinesEd      : Result:= lisKMCodeToolsDefinesEditor;
    ecManageDesktops          : Result:= lisDesktops;

    ecExtToolSettings         : Result:= srkmecExtToolSettings;
    ecManageExamples          : Result:= lisMenuExampleProjects;
    ecConfigBuildLazarus      : Result:= lismenuconfigurebuildlazarus;
    ecBuildLazarus            : Result:= srkmecBuildLazarus;
    ecExtToolFirst
    ..ecExtToolLast           : Result:= Format(srkmecExtTool,[cmd-ecExtToolFirst+1]);
    ecMakeResourceString      : Result:= srkmecMakeResourceString;
    ecDiff                    : Result:= srkmecDiff;

    // window menu
    ecManageSourceEditors     : Result:= lisSourceEditorWindowManager;

    // help menu
    ecAboutLazarus            : Result:= lisAboutLazarus;
    ecOnlineHelp              : Result:= lisMenuOnlineHelp;
    ecContextHelp             : Result:= lisMenuContextHelp;
    ecEditContextHelp         : Result:= lisMenuEditContextHelp;
    ecReportingBug            : Result:= srkmecReportingBug;
    ecFocusHint               : Result:= lisFocusHint;
    ecSmartHint               : Result:= lisMenuShowSmartHint;

    // desginer
    ecDesignerCopy            : Result:= lisDsgCopyComponents;
    ecDesignerCut             : Result:= lisDsgCutComponents;
    ecDesignerPaste           : Result:= lisDsgPasteComponents;
    ecDesignerSelectParent    : Result:= lisDsgSelectParentComponent;
    ecDesignerMoveToFront     : Result:= lisDsgOrderMoveToFront;
    ecDesignerMoveToBack      : Result:= lisDsgOrderMoveToBack;
    ecDesignerForwardOne      : Result:= lisDsgOrderForwardOne;
    ecDesignerBackOne         : Result:= lisDsgOrderBackOne;

    // macro
    ecSynMacroRecord          : Result:= srkmecSynMacroRecord;
    ecSynMacroPlay            : Result:= srkmecSynMacroPlay;

    // Edit template
    ecIdePTmplEdNextCell:                Result := srkmecSynPTmplEdNextCell;
    ecIdePTmplEdNextCellSel:             Result := srkmecSynPTmplEdNextCellSel;
    ecIdePTmplEdNextCellRotate:          Result := srkmecSynPTmplEdNextCellRotate;
    ecIdePTmplEdNextCellSelRotate:       Result := srkmecSynPTmplEdNextCellSelRotate;
    ecIdePTmplEdPrevCell:                Result := srkmecSynPTmplEdPrevCell;
    ecIdePTmplEdPrevCellSel:             Result := srkmecSynPTmplEdPrevCellSel;
    ecIdePTmplEdNextFirstCell:           Result := srkmecSynPTmplEdNextFirstCell;
    ecIdePTmplEdNextFirstCellSel:        Result := srkmecSynPTmplEdNextFirstCellSel;
    ecIdePTmplEdNextFirstCellRotate:     Result := srkmecSynPTmplEdNextFirstCellRotate;
    ecIdePTmplEdNextFirstCellSelRotate:  Result := srkmecSynPTmplEdNextFirstCellSelRotate;
    ecIdePTmplEdPrevFirstCell:           Result := srkmecSynPTmplEdPrevFirstCell;
    ecIdePTmplEdPrevFirstCellSel:        Result := srkmecSynPTmplEdPrevFirstCellSel;
    ecIdePTmplEdCellHome:                Result := srkmecSynPTmplEdCellHome;
    ecIdePTmplEdCellEnd:                 Result := srkmecSynPTmplEdCellEnd;
    ecIdePTmplEdCellSelect:              Result := srkmecSynPTmplEdCellSelect;
    ecIdePTmplEdFinish:                  Result := srkmecSynPTmplEdFinish;
    ecIdePTmplEdEscape:                  Result := srkmecSynPTmplEdEscape;
    // Edit template
    ecIdePTmplEdOutNextCell:                Result := srkmecSynPTmplEdNextCell;
    ecIdePTmplEdOutNextCellSel:             Result := srkmecSynPTmplEdNextCellSel;
    ecIdePTmplEdOutNextCellRotate:          Result := srkmecSynPTmplEdNextCellRotate;
    ecIdePTmplEdOutNextCellSelRotate:       Result := srkmecSynPTmplEdNextCellSelRotate;
    ecIdePTmplEdOutPrevCell:                Result := srkmecSynPTmplEdPrevCell;
    ecIdePTmplEdOutPrevCellSel:             Result := srkmecSynPTmplEdPrevCellSel;
    ecIdePTmplEdOutNextFirstCell:           Result := srkmecSynPTmplEdNextFirstCell;
    ecIdePTmplEdOutNextFirstCellSel:        Result := srkmecSynPTmplEdNextFirstCellSel;
    ecIdePTmplEdOutNextFirstCellRotate:     Result := srkmecSynPTmplEdNextFirstCellRotate;
    ecIdePTmplEdOutNextFirstCellSelRotate:  Result := srkmecSynPTmplEdNextFirstCellSelRotate;
    ecIdePTmplEdOutPrevFirstCell:           Result := srkmecSynPTmplEdPrevFirstCell;
    ecIdePTmplEdOutPrevFirstCellSel:        Result := srkmecSynPTmplEdPrevFirstCellSel;
    ecIdePTmplEdOutCellHome:                Result := srkmecSynPTmplEdCellHome;
    ecIdePTmplEdOutCellEnd:                 Result := srkmecSynPTmplEdCellEnd;
    ecIdePTmplEdOutCellSelect:              Result := srkmecSynPTmplEdCellSelect;
    ecIdePTmplEdOutFinish:                  Result := srkmecSynPTmplEdFinish;
    ecIdePTmplEdOutEscape:                  Result := srkmecSynPTmplEdEscape;
    // SyncroEdit
    ecIdePSyncroEdNextCell:              Result := srkmecSynPSyncroEdNextCell;
    ecIdePSyncroEdNextCellSel:           Result := srkmecSynPSyncroEdNextCellSel;
    ecIdePSyncroEdPrevCell:              Result := srkmecSynPSyncroEdPrevCell;
    ecIdePSyncroEdPrevCellSel:           Result := srkmecSynPSyncroEdPrevCellSel;
    ecIdePSyncroEdNextFirstCell:         Result := srkmecSynPSyncroEdNextFirstCell;
    ecIdePSyncroEdNextFirstCellSel:      Result := srkmecSynPSyncroEdNextFirstCellSel;
    ecIdePSyncroEdPrevFirstCell:         Result := srkmecSynPSyncroEdPrevFirstCell;
    ecIdePSyncroEdPrevFirstCellSel:      Result := srkmecSynPSyncroEdPrevFirstCellSel;
    ecIdePSyncroEdCellHome:              Result := srkmecSynPSyncroEdCellHome;
    ecIdePSyncroEdCellEnd:               Result := srkmecSynPSyncroEdCellEnd;
    ecIdePSyncroEdCellSelect:            Result := srkmecSynPSyncroEdCellSelect;
    ecIdePSyncroEdEscape:                Result := srkmecSynPSyncroEdEscape;
    // SyncroEdit
    ecIdePSyncroEdOutNextCell:              Result := srkmecSynPSyncroEdNextCell;
    ecIdePSyncroEdOutNextCellSel:           Result := srkmecSynPSyncroEdNextCellSel;
    ecIdePSyncroEdOutPrevCell:              Result := srkmecSynPSyncroEdPrevCell;
    ecIdePSyncroEdOutPrevCellSel:           Result := srkmecSynPSyncroEdPrevCellSel;
    ecIdePSyncroEdOutNextFirstCell:         Result := srkmecSynPSyncroEdNextFirstCell;
    ecIdePSyncroEdOutNextFirstCellSel:      Result := srkmecSynPSyncroEdNextFirstCellSel;
    ecIdePSyncroEdOutPrevFirstCell:         Result := srkmecSynPSyncroEdPrevFirstCell;
    ecIdePSyncroEdOutPrevFirstCellSel:      Result := srkmecSynPSyncroEdPrevFirstCellSel;
    ecIdePSyncroEdOutCellHome:              Result := srkmecSynPSyncroEdCellHome;
    ecIdePSyncroEdOutCellEnd:               Result := srkmecSynPSyncroEdCellEnd;
    ecIdePSyncroEdOutCellSelect:            Result := srkmecSynPSyncroEdCellSelect;
    ecIdePSyncroEdOutEscape:                Result := srkmecSynPSyncroEdEscape;
    // SyncroEdit, during selection
    ecIdePSyncroEdSelStart:            Result := srkmecSynPSyncroEdStart;

    else
      begin
        Result:= srkmecunknown;

      end;
  end;
end;

function KeyValuesToCaptionStr(const ShortcutA, ShortcutB: TIDEShortCut;
  Brackets: Char): String;
  function AddBrakets(S: String): String;
  begin
    if Brackets = '[' then
      Result := '[' + S + ']'
    else if Brackets = '(' then
      Result := '(' + S + ')'
    else if Brackets > #0 then
      Result := Brackets + S + Brackets
    else
      Result := S;
  end;
begin
  Result := '';
  if (ShortcutA.Key1 = VK_UNKNOWN) and (ShortcutB.Key1 = VK_UNKNOWN) then
    Result := Result{ + lisNone2 }
  else
  if (ShortcutA.Key1 = VK_UNKNOWN) then
    Result := Result + AddBrakets(KeyAndShiftStateToEditorKeyString(ShortcutB))
  else
  if (ShortcutB.Key1 = VK_UNKNOWN) then
    Result := Result + AddBrakets(KeyAndShiftStateToEditorKeyString(ShortcutA))
  else
    Result := Result + AddBrakets(KeyAndShiftStateToEditorKeyString(ShortcutA))
                     + '  '+lisOr+'  ' +
                       AddBrakets(KeyAndShiftStateToEditorKeyString(ShortcutB));
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

{ TKeyStrokeList }

procedure TKeyStrokeList.Add(aKeyStroke: TSynEditKeyStroke);
begin
  case FCount of
    0: begin KeyStroke1 := aKeyStroke; Inc(FCount); end;
    1: begin KeyStroke2 := aKeyStroke; Inc(FCount); end;
    2: begin KeyStroke3 := aKeyStroke; Inc(FCount); end;
    3: raise Exception.Create('TKeyStrokePair supports only 3 items');
  end;
end;

function TKeyStrokeList.GetItem(Index: Integer): TSynEditKeyStroke;
begin
  if Index >= FCount then
    raise Exception.Create('TKeyStrokePair: Index out of bounds!');
  case Index of
    0: Result := KeyStroke1;
    1: Result := KeyStroke2;
    2: Result := KeyStroke3;
    else Result := Nil;
  end;
end;

procedure TKeyStrokeList.PutItem(Index: Integer; AValue: TSynEditKeyStroke);
begin
  if Index >= FCount then
    raise Exception.Create('TKeyStrokePair: Index out of bounds!');
  case Index of
    0: KeyStroke1 := AValue;
    1: KeyStroke2 := AValue;
    2: KeyStroke3 := AValue;
  end;
end;

{ TKeyCommandRelation }

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
    kmsDefaultToMac: GetDefaultKeyForWindowsScheme(true);
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
  {$IFDEF Darwin}
  GetDefaultKeyForMacOSXScheme;
  {$ELSE}
  GetDefaultKeyForWindowsScheme;
  {$ENDIF}
end;

procedure TKeyCommandRelation.GetDefaultKeyForWindowsScheme(AUseMetaKey: boolean=false);
var
  XCtrl: TShiftStateEnum;
begin
  if AUseMetaKey then
    XCtrl:=ssMeta
  else
    XCtrl:=ssCtrl;

  case Command of
  // moving
  ecLeft:                SetSingle(VK_LEFT,[]);
  ecRight:               SetSingle(VK_RIGHT,[]);
  ecUp:                  SetSingle(VK_UP,[]);
  ecDown:                SetSingle(VK_DOWN,[]);
  ecWordLeft:            SetSingle(VK_LEFT,[XCtrl]);
  ecWordRight:           SetSingle(VK_RIGHT,[XCtrl]); // WS c
  ecLineStart:           SetSingle(VK_HOME,[]);
  ecLineEnd:             SetSingle(VK_END,[]);
  ecPageUp:              SetSingle(VK_PRIOR,[]); // ,VK_R,[XCtrl],VK_UNKNOWN,[]);
  ecPageDown:            SetSingle(VK_NEXT,[]); // ,VK_W,[XCtrl],VK_UNKNOWN,[]);
  ecPageLeft:            SetSingle(VK_UNKNOWN,[]);
  ecPageRight:           SetSingle(VK_UNKNOWN,[]);
  ecPageTop:             SetSingle(VK_PRIOR,[XCtrl]);
  ecPageBottom:          SetSingle(VK_NEXT,[XCtrl]);
  ecEditorTop:           SetSingle(VK_HOME,[XCtrl]);
  ecEditorBottom:        SetSingle(VK_END,[XCtrl]);
  ecScrollUp:            SetSingle(VK_UP,[XCtrl]);
  ecScrollDown:          SetSingle(VK_DOWN,[XCtrl]);
  ecScrollLeft:          SetSingle(VK_UNKNOWN,[]);
  ecScrollRight:         SetSingle(VK_UNKNOWN,[]);

  // selection
  ecSelLeft:             SetSingle(VK_LEFT,[ssShift]);
  ecSelRight:            SetSingle(VK_RIGHT,[ssShift]);
  ecSelUp:               SetSingle(VK_UP,[ssShift]);
  ecSelDown:             SetSingle(VK_DOWN,[ssShift]);
  ecCopy:                SetSingle(VK_C,[XCtrl],         VK_Insert,[XCtrl]);
  ecCut:                 SetSingle(VK_X,[XCtrl],         VK_Delete,[ssShift]);
  ecPaste:               SetSingle(VK_V,[XCtrl],         VK_Insert,[ssShift]);
  ecMultiPaste:          SetSingle(VK_UNKNOWN,[]);
  ecNormalSelect:        SetSingle(VK_UNKNOWN,[]);
  ecColumnSelect:        SetSingle(VK_UNKNOWN,[]);
  ecLineSelect:          SetSingle(VK_UNKNOWN,[]);
  ecSelWordLeft:         SetSingle(VK_LEFT,[XCtrl,ssShift]);
  ecSelWordRight:        SetSingle(VK_RIGHT,[XCtrl,ssShift]);
  ecSelLineStart:        SetSingle(VK_HOME,[ssShift]);
  ecSelLineEnd:          SetSingle(VK_END,[ssShift]);
  ecSelPageTop:          SetSingle(VK_PRIOR,[ssShift,XCtrl]);
  ecSelPageBottom:       SetSingle(VK_NEXT,[ssShift,XCtrl]);
  ecSelEditorTop:        SetSingle(VK_HOME,[ssShift,XCtrl]);
  ecSelEditorBottom:     SetSingle(VK_END,[ssShift,XCtrl]);
  ecSelectAll:           SetSingle(VK_A,[XCtrl]);
  ecSelectToBrace:       SetSingle(VK_UNKNOWN,[]);
  ecSelectCodeBlock:     SetSingle(VK_UNKNOWN,[]);
  ecSelectWord:          SetCombo(VK_K,[XCtrl],VK_T,[]);
  ecSelectLine:          SetCombo(VK_K,[XCtrl],VK_L,[]);
  ecSelectParagraph:     SetSingle(VK_UNKNOWN,[]);
  ecSelectionUpperCase:  SetCombo(VK_K,[XCtrl],VK_N,[]);
  ecSelectionLowerCase:  SetCombo(VK_K,[XCtrl],VK_O,[]);
  ecSelectionSwapCase:   SetCombo(VK_K,[XCtrl],VK_P,[]);
  ecSelectionTabs2Spaces:SetSingle(VK_UNKNOWN,[]);
  ecSelectionEnclose:    SetSingle(VK_N,[ssShift,XCtrl]);
  ecSelectionComment:    SetSingle(VK_V,[ssShift,XCtrl]);
  ecSelectionUncomment:  SetSingle(VK_U,[ssShift,XCtrl]);
  ecToggleComment:       SetSingle(VK_OEM_2,[XCtrl]);
  ecSelectionEncloseIFDEF:SetSingle(VK_D,[ssShift,XCtrl]);
  ecSelectionSort:       SetSingle(VK_UNKNOWN,[]);
  ecSelectionBreakLines: SetSingle(VK_UNKNOWN,[]);

  ecStickySelection:     SetCombo(VK_K,[XCtrl],VK_S,[]);
  ecStickySelectionCol:  SetCombo(VK_K,[XCtrl],VK_S,[ssAlt]);
  ecStickySelectionStop: SetCombo(VK_K,[XCtrl],VK_E,[]);

  ecBlockSetBegin:       SetCombo(VK_K,[XCtrl],VK_B,[]);
  ecBlockSetEnd:         SetCombo(VK_K,[XCtrl],VK_K,[]);
  ecBlockToggleHide:     SetCombo(VK_K,[XCtrl],VK_H,[]);
  ecBlockHide:           SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockShow:           SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockMove:           SetCombo(VK_K,[XCtrl],VK_V,[]);
  ecBlockCopy:           SetCombo(VK_K,[XCtrl],VK_C,[]);
  ecBlockDelete:         SetCombo(VK_K,[XCtrl],VK_Y,[]);
  ecBlockGotoBegin:      SetCombo(VK_Q,[XCtrl],VK_B,[]);
  ecBlockGotoEnd:        SetCombo(VK_Q,[XCtrl],VK_K,[]);

  // column mode selection
  ecColSelUp:            SetSingle(VK_UP,[ssAlt,ssShift]);
  ecColSelDown:          SetSingle(VK_DOWN,[ssAlt,ssShift]);
  ecColSelLeft:          SetSingle(VK_LEFT,[ssAlt,ssShift]);
  ecColSelRight:         SetSingle(VK_RIGHT,[ssAlt,ssShift]);
  ecColSelPageDown:      SetSingle(VK_NEXT,[ssAlt,ssShift]);
  ecColSelPageBottom:    SetSingle(VK_NEXT,[ssAlt,ssShift,XCtrl]);
  ecColSelPageUp:        SetSingle(VK_PRIOR,[ssAlt,ssShift]);
  ecColSelPageTop:       SetSingle(VK_PRIOR,[ssAlt,ssShift,XCtrl]);
  ecColSelLineStart:     SetSingle(VK_HOME,[ssAlt,ssShift]);
  ecColSelLineEnd:       SetSingle(VK_END,[ssAlt,ssShift]);
  ecColSelEditorTop:     SetSingle(VK_HOME,[ssAlt,ssShift,XCtrl]);
  ecColSelEditorBottom:  SetSingle(VK_END,[ssAlt,ssShift,XCtrl]);

  // multi caret
  ecPluginMultiCaretSetCaret:    SetSingle(VK_INSERT,[ssShift, XCtrl]);
  ecPluginMultiCaretUnsetCaret:  SetSingle(VK_DELETE,[ssShift, XCtrl]);
  //ecPluginMultiCaretToggleCaret: SetSingle(VK_INSERT,[ssShift, XCtrl]);
  ecPluginMultiCaretClearAll:    SetSingle(VK_ESCAPE,[ssShift, ssCtrl], VK_ESCAPE,[]);

  ecPluginMultiCaretModeCancelOnMove:  SetCombo(VK_Q,[ssShift, XCtrl], VK_X,[ssShift, XCtrl]);
  ecPluginMultiCaretModeMoveAll:       SetCombo(VK_Q,[ssShift, XCtrl], VK_M,[ssShift, XCtrl]);

  // editing
  ecBlockIndent:         SetCombo(VK_I,[XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_I,[]);
  ecBlockUnindent:       SetCombo(VK_U,[XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_U,[]);
  ecDeleteLastChar:      SetSingle(VK_BACK,[], VK_BACK,[ssShift]); // ctrl H used for scroll window.
  ecDeleteChar:          SetSingle(VK_DELETE,[]); // ctrl G conflicts with GO
  ecDeleteWord:          SetSingle(VK_T,[XCtrl], VK_DELETE,[XCtrl]);
  ecDeleteLastWord:      SetSingle(VK_BACK,[XCtrl]);
  ecDeleteBOL:           SetSingle(VK_UNKNOWN,[]);
  ecDeleteEOL:           SetCombo(VK_Y,[XCtrl,ssShift],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_Y,[]);
  ecDeleteLine:          SetSingle(VK_Y,[XCtrl]);
  ecClearAll:            SetSingle(VK_UNKNOWN,[]);
  ecLineBreak:           SetSingle(VK_RETURN,[]);
  ecInsertLine:          SetSingle(VK_N,[XCtrl]);
  ecInsertCharacter:     SetSingle(VK_M,[ssShift,XCtrl]);
  ecInsertGPLNotice:     SetSingle(VK_UNKNOWN,[]);
  ecInsertLGPLNotice:    SetSingle(VK_UNKNOWN,[]);
  ecInsertModifiedLGPLNotice:SetSingle(VK_UNKNOWN,[]);
  ecInsertMITNotice:     SetSingle(VK_UNKNOWN,[]);
  ecInsertUserName:      SetSingle(VK_UNKNOWN,[]);
  ecInsertDateTime:      SetSingle(VK_UNKNOWN,[]);
  ecInsertChangeLogEntry:SetSingle(VK_UNKNOWN,[]);
  ecInsertCVSAuthor:     SetSingle(VK_UNKNOWN,[]);
  ecInsertCVSDate:       SetSingle(VK_UNKNOWN,[]);
  ecInsertCVSHeader:     SetSingle(VK_UNKNOWN,[]);
  ecInsertCVSID:         SetSingle(VK_UNKNOWN,[]);
  ecInsertCVSLog:        SetSingle(VK_UNKNOWN,[]);
  ecInsertCVSName:       SetSingle(VK_UNKNOWN,[]);
  ecInsertCVSRevision:   SetSingle(VK_UNKNOWN,[]);
  ecInsertCVSSource:     SetSingle(VK_UNKNOWN,[]);
  ecInsertGUID:          SetSingle(VK_G,[XCtrl,ssShift]);
  ecInsertFilename:      SetSingle(VK_UNKNOWN,[]);

  // command commands
  ecUndo:                SetSingle(VK_Z,[XCtrl]);
  ecRedo:                SetSingle(VK_Z,[XCtrl,ssShift]);

  // search & replace
  ecMatchBracket:        SetSingle(VK_UNKNOWN,[]);
  ecFind:                SetCombo(VK_F,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_F,[]);
  ecFindNext:            SetSingle(VK_F3,[],                   VK_L,[XCtrl]);
  ecFindPrevious:        SetSingle(VK_F3,[ssShift]);
  ecFindInFiles:         SetSingle(VK_F,[XCtrl,ssShift]);
  ecReplace:             SetCombo(VK_R,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_A,[]);
  ecIncrementalFind:     SetSingle(VK_E,[XCtrl]);
  ecGotoLineNumber:      SetCombo(VK_G,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_G,[]);
  ecFindNextWordOccurrence:SetSingle(VK_UNKNOWN,[]);
  ecFindPrevWordOccurrence:SetSingle(VK_UNKNOWN,[]);
  ecJumpBack:            SetSingle(VK_H,[XCtrl],VK_LEFT,[ssAlt]);
  ecJumpForward:         SetSingle(VK_H,[XCtrl,ssShift],VK_RIGHT,[ssAlt]);
  ecAddJumpPoint:        SetSingle(VK_UNKNOWN,[]);
  ecJumpToPrevError:     SetSingle(VK_F8,[XCtrl, ssShift]);
  ecJumpToNextError:     SetSingle(VK_F8,[XCtrl]);
  ecOpenFileAtCursor:    SetSingle(VK_RETURN,[XCtrl]);
  ecProcedureList:       SetSingle(VK_G,[ssAlt]);

  // marker
  ecSetFreeBookmark:     SetSingle(VK_UNKNOWN,[]);
  ecClearBookmarkForFile:SetSingle(VK_UNKNOWN,[]);
  ecClearAllBookmark:    SetSingle(VK_UNKNOWN,[]);
  ecPrevBookmark:        SetSingle(VK_UNKNOWN,[]);
  ecNextBookmark:        SetSingle(VK_UNKNOWN,[]);
  ecGotoMarker0:         SetCombo(VK_0,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_0,[]);
  ecGotoMarker1:         SetCombo(VK_1,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_1,[]);
  ecGotoMarker2:         SetCombo(VK_2,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_2,[]);
  ecGotoMarker3:         SetCombo(VK_3,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_3,[]);
  ecGotoMarker4:         SetCombo(VK_4,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_4,[]);
  ecGotoMarker5:         SetCombo(VK_5,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_5,[]);
  ecGotoMarker6:         SetCombo(VK_6,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_6,[]);
  ecGotoMarker7:         SetCombo(VK_7,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_7,[]);
  ecGotoMarker8:         SetCombo(VK_8,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_8,[]);
  ecGotoMarker9:         SetCombo(VK_9,[XCtrl],VK_UNKNOWN,[], VK_Q,[XCtrl],VK_9,[]);
  ecToggleMarker0:       SetCombo(VK_0,[ssShift,XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_0,[]);
  ecToggleMarker1:       SetCombo(VK_1,[ssShift,XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_1,[]);
  ecToggleMarker2:       SetCombo(VK_2,[ssShift,XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_2,[]);
  ecToggleMarker3:       SetCombo(VK_3,[ssShift,XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_3,[]);
  ecToggleMarker4:       SetCombo(VK_4,[ssShift,XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_4,[]);
  ecToggleMarker5:       SetCombo(VK_5,[ssShift,XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_5,[]);
  ecToggleMarker6:       SetCombo(VK_6,[ssShift,XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_6,[]);
  ecToggleMarker7:       SetCombo(VK_7,[ssShift,XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_7,[]);
  ecToggleMarker8:       SetCombo(VK_8,[ssShift,XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_8,[]);
  ecToggleMarker9:       SetCombo(VK_9,[ssShift,XCtrl],VK_UNKNOWN,[], VK_K,[XCtrl],VK_9,[]);
  ecSetMarker0:          SetSingle(VK_UNKNOWN,[]);
  ecSetMarker1:          SetSingle(VK_UNKNOWN,[]);
  ecSetMarker2:          SetSingle(VK_UNKNOWN,[]);
  ecSetMarker3:          SetSingle(VK_UNKNOWN,[]);
  ecSetMarker4:          SetSingle(VK_UNKNOWN,[]);
  ecSetMarker5:          SetSingle(VK_UNKNOWN,[]);
  ecSetMarker6:          SetSingle(VK_UNKNOWN,[]);
  ecSetMarker7:          SetSingle(VK_UNKNOWN,[]);
  ecSetMarker8:          SetSingle(VK_UNKNOWN,[]);
  ecSetMarker9:          SetSingle(VK_UNKNOWN,[]);
  ecGotoBookmarks:       SetSingle(VK_B,[ssCtrl]);
  ecToggleBookmarks:     SetSingle(VK_B,[ssCtrl,ssShift]);

  // codetools
  ecAutoCompletion:      SetSingle(VK_J,[XCtrl]);
  ecWordCompletion:      SetSingle(VK_W,[XCtrl]);
  ecCompleteCode:        SetSingle(VK_C,[XCtrl,ssShift]);
  ecCompleteCodeInteractive: SetSingle(VK_X,[XCtrl,ssShift]);
  ecIdentCompletion:     SetSingle(VK_SPACE,[XCtrl]);
  ecShowCodeContext:     SetSingle(VK_SPACE,[XCtrl,ssShift]);
  ecExtractProc:         SetSingle(VK_UNKNOWN,[]);
  ecFindIdentifierRefs:  SetSingle(VK_I,[XCtrl,ssShift]);
  ecFindUsedUnitRefs:    SetSingle(VK_UNKNOWN,[]);
  ecRenameIdentifier:    SetSingle(VK_F2,[],        VK_E,[ssShift,XCtrl]);
  ecInvertAssignment:    SetSingle(VK_UNKNOWN,[]);
  ecSyntaxCheck:         SetSingle(VK_UNKNOWN,[]);
  ecGuessUnclosedBlock:  SetSingle(VK_UNKNOWN,[]);
  ecGuessMisplacedIFDEF: SetSingle(VK_UNKNOWN,[]);
  ecConvertDFM2LFM:      SetSingle(VK_UNKNOWN,[]);
  ecCheckLFM:            SetSingle(VK_UNKNOWN,[]);
  ecConvertDelphiUnit:   SetSingle(VK_UNKNOWN,[]);
  ecConvertDelphiProject:SetSingle(VK_UNKNOWN,[]);
  ecConvertDelphiPackage:SetSingle(VK_UNKNOWN,[]);
  ecConvertEncoding:     SetSingle(VK_UNKNOWN,[]);
  ecFindProcedureDefinition:SetSingle(VK_UP,[ssShift,XCtrl]);
  ecFindProcedureMethod: SetSingle(VK_DOWN,[ssShift,XCtrl]);
  ecFindDeclaration:     SetSingle(VK_UP,[ssAlt]);
  ecFindBlockOtherEnd:   SetCombo(VK_Q,[XCtrl],VK_O,[]);
  ecFindBlockStart:      SetCombo(VK_Q,[XCtrl],VK_M,[]);
  ecGotoIncludeDirective:SetSingle(VK_UNKNOWN,[]);
  ecShowAbstractMethods: SetSingle(VK_UNKNOWN,[]);
  ecRemoveEmptyMethods:  SetSingle(VK_UNKNOWN,[]);
  ecRemoveUnusedUnits:   SetSingle(VK_UNKNOWN,[]);
  ecUseUnit:             SetSingle(VK_F11,[ssAlt]);
  ecFindOverloads:       SetSingle(VK_UNKNOWN,[]);

  // source notebook
  ecNextEditor:          SetSingle(VK_TAB,[XCtrl]);
  ecPrevEditor:          SetSingle(VK_TAB,[ssShift,XCtrl]);
  ecPrevEditorInHistory: SetSingle(VK_OEM_3,[XCtrl]);//~
  ecNextEditorInHistory: SetSingle(VK_OEM_3,[ssShift,XCtrl]);//~
  ecResetDebugger:       SetSingle(VK_UNKNOWN,[]);
  ecToggleBreakPoint:    SetSingle(VK_F5,[]);
  ecToggleBreakPointEnabled:    SetSingle(VK_F5,[ssShift, XCtrl]);
  ecMoveEditorLeft:      SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorRight:     SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorLeftmost:  SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorRightmost: SetSingle(VK_UNKNOWN,[]);

  ecNextSharedEditor:    SetSingle(VK_UNKNOWN,[]);
  ecPrevSharedEditor:    SetSingle(VK_UNKNOWN,[]);
  ecNextWindow:          SetSingle(VK_UNKNOWN,[]);
  ecPrevWindow:          SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorNextWindow:SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorPrevWindow:SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorNewWindow: SetSingle(VK_UNKNOWN,[]);
  ecCopyEditorNextWindow:SetSingle(VK_UNKNOWN,[]);
  ecCopyEditorPrevWindow:SetSingle(VK_UNKNOWN,[]);
  ecCopyEditorNewWindow: SetSingle(VK_UNKNOWN,[]);

  ecGotoEditor1:         SetSingle(VK_1,[ssAlt]);
  ecGotoEditor2:         SetSingle(VK_2,[ssAlt]);
  ecGotoEditor3:         SetSingle(VK_3,[ssAlt]);
  ecGotoEditor4:         SetSingle(VK_4,[ssAlt]);
  ecGotoEditor5:         SetSingle(VK_5,[ssAlt]);
  ecGotoEditor6:         SetSingle(VK_6,[ssAlt]);
  ecGotoEditor7:         SetSingle(VK_7,[ssAlt]);
  ecGotoEditor8:         SetSingle(VK_8,[ssAlt]);
  ecGotoEditor9:         SetSingle(VK_9,[ssAlt]);
  ecGotoEditor0:         SetSingle(VK_0,[ssAlt]);

  ecLockEditor:          SetSingle(VK_UNKNOWN,[]);

  EcFoldLevel1:          SetSingle(VK_1,[ssAlt,ssShift]);
  EcFoldLevel2:          SetSingle(VK_2,[ssAlt,ssShift]);
  EcFoldLevel3:          SetSingle(VK_3,[ssAlt,ssShift]);
  EcFoldLevel4:          SetSingle(VK_4,[ssAlt,ssShift]);
  EcFoldLevel5:          SetSingle(VK_5,[ssAlt,ssShift]);
  EcFoldLevel6:          SetSingle(VK_6,[ssAlt,ssShift]);
  EcFoldLevel7:          SetSingle(VK_7,[ssAlt,ssShift]);
  EcFoldLevel8:          SetSingle(VK_8,[ssAlt,ssShift]);
  EcFoldLevel9:          SetSingle(VK_9,[ssAlt,ssShift]);
  EcFoldLevel0:          SetSingle(VK_0,[ssAlt,ssShift]);
  EcFoldCurrent:         SetSingle(VK_OEM_MINUS,[ssAlt,ssShift]);
  EcUnFoldCurrent:       SetSingle(VK_OEM_PLUS,[ssAlt,ssShift]);
  EcToggleMarkupWord:    SetSingle(VK_M,[ssAlt]);

  // file menu
  ecNew:                 SetSingle(VK_UNKNOWN,[]);
  ecNewUnit:             SetSingle(VK_UNKNOWN,[]);
  ecNewForm:             SetSingle(VK_UNKNOWN,[]);
  ecOpen:                SetSingle(VK_O,[XCtrl]);
  ecOpenUnit:            SetSingle(VK_F12,[ssAlt]);
  ecRevert:              SetSingle(VK_UNKNOWN,[]);
  ecSave:                SetSingle(VK_S,[XCtrl]);
  ecSaveAs:              SetSingle(VK_UNKNOWN,[]);
  ecSaveAll:             SetSingle(VK_S,[XCtrl,ssShift]);
  ecClose:               SetSingle(VK_F4,[XCtrl]);
  ecCloseAll:            SetSingle(VK_UNKNOWN,[]);
  ecCloseOtherTabs:      SetSingle(VK_UNKNOWN,[]);
  ecCloseRightTabs:      SetSingle(VK_UNKNOWN,[]);
  ecCleanDirectory:      SetSingle(VK_UNKNOWN,[]);
  ecRestart:             SetSingle(VK_UNKNOWN,[]);
  ecQuit:                SetSingle(VK_UNKNOWN,[]);

  // view menu
  ecToggleObjectInsp:    SetSingle(VK_F11,[]);
  ecToggleSourceEditor:  SetSingle(VK_UNKNOWN,[]);
  ecToggleCodeExpl:      SetSingle(VK_UNKNOWN,[]);
  ecToggleFPDocEditor:   SetSingle(VK_UNKNOWN,[]);
  ecToggleMessages:      SetSingle(VK_UNKNOWN,[]);
  ecViewComponents:      SetSingle(VK_P,[XCtrl,ssAlt]);
  ecViewJumpHistory:     SetSingle(VK_J,[XCtrl,ssAlt]);
  ecToggleSearchResults: SetSingle(VK_F,[XCtrl,ssAlt]);
  ecToggleWatches:       SetSingle(VK_W,[XCtrl,ssAlt]);
  ecToggleBreakPoints:   SetSingle(VK_B,[XCtrl,ssAlt]);
  ecToggleLocals:        SetSingle(VK_L,[XCtrl,ssAlt],     VK_L,[XCtrl,ssShift]);
  ecViewPseudoTerminal: if HasConsoleSupport then SetSingle(VK_O,[XCtrl,ssAlt]);
  ecViewThreads:         SetSingle(VK_T,[XCtrl,ssAlt]);
  ecToggleCallStack:     SetSingle(VK_S,[XCtrl,ssAlt]);
  ecToggleRegisters:     SetSingle(VK_R,[XCtrl,ssAlt]);
  ecToggleAssembler:     SetSingle(VK_D,[XCtrl,ssAlt]);
  ecToggleDebugEvents:   SetSingle(VK_V,[XCtrl,ssAlt]);
  ecToggleDebuggerOut:   SetSingle(VK_UNKNOWN,[]);
  ecViewHistory:         SetSingle(VK_H,[XCtrl,ssAlt]);
  ecViewUnitDependencies:SetSingle(VK_UNKNOWN,[]);
  ecViewUnitInfo:        SetSingle(VK_UNKNOWN,[]);
  ecToggleFormUnit:      SetSingle(VK_F12,[]);
  ecViewAnchorEditor:    SetSingle(VK_UNKNOWN,[]);
  ecToggleCodeBrowser:   SetSingle(VK_UNKNOWN,[]);
  ecToggleRestrictionBrowser:SetSingle(VK_UNKNOWN,[]);
  ecToggleCompPalette:   SetSingle(VK_UNKNOWN,[]);
  ecToggleIDESpeedBtns:  SetSingle(VK_UNKNOWN,[]);

  // project menu
  ecNewProject:          SetSingle(VK_UNKNOWN,[]);
  ecNewProjectFromFile:  SetSingle(VK_UNKNOWN,[]);
  ecOpenProject:         SetSingle(VK_F11,[XCtrl]);
  ecCloseProject:        SetSingle(VK_UNKNOWN,[]);
  ecSaveProject:         SetSingle(VK_UNKNOWN,[]);
  ecSaveProjectAs:       SetSingle(VK_UNKNOWN,[]);
  ecProjectResaveFormsWithI18n: SetSingle(VK_UNKNOWN,[]);
  ecPublishProject:      SetSingle(VK_UNKNOWN,[]);
  ecProjectInspector:    SetSingle(VK_UNKNOWN,[]);
  ecAddCurUnitToProj:    SetSingle(VK_F11,[ssShift]);
  ecRemoveFromProj:      SetSingle(VK_UNKNOWN,[]);
  ecViewProjectUnits:    SetSingle(VK_F12,[XCtrl]);
  ecViewProjectForms:    SetSingle(VK_F12,[ssShift]);
  ecViewProjectSource:   SetSingle(VK_UNKNOWN,[]);
  ecProjectOptions:      SetSingle(VK_F11,[ssShift,XCtrl]);
  ecProjectChangeBuildMode:SetSingle(VK_UNKNOWN,[]);

  // run menu
  ecCompile:             SetSingle(VK_F9,[XCtrl]);
  ecBuild:               SetSingle(VK_F9,[ssShift]);
  ecQuickCompile:        SetSingle(VK_UNKNOWN,[]);
  ecCleanUpAndBuild:     SetSingle(VK_UNKNOWN,[]);
  ecBuildManyModes:      SetSingle(VK_UNKNOWN,[]);
  ecAbortBuild:          SetSingle(VK_UNKNOWN,[]);
  ecRunWithoutDebugging: SetSingle(VK_F9, [XCtrl, ssShift]);
  ecRun:                 SetSingle(VK_F9,[]);
  ecPause:               SetSingle(VK_UNKNOWN,[]);
  ecShowExecutionPoint:  SetSingle(VK_UNKNOWN,[]);
  ecStepInto:            SetSingle(VK_F7,[]);
  ecStepOver:            SetSingle(VK_F8,[]);
  ecStepIntoInstr:       SetSingle(VK_F7,[ssAlt]);
  ecStepOverInstr:       SetSingle(VK_F8,[ssAlt]);
  ecStepOut:             SetSingle(VK_F8,[ssShift]);
  ecRunToCursor:         SetSingle(VK_F4,[]);
  ecStopProgram:         SetSingle(VK_F2,[XCtrl]);
  ecRemoveBreakPoint:    SetSingle(VK_UNKNOWN,[]);
  ecRunParameters:       SetSingle(VK_UNKNOWN,[]);
  ecBuildFile:           SetSingle(VK_UNKNOWN,[]);
  ecRunFile:             SetSingle(VK_UNKNOWN,[]);
  ecConfigBuildFile:     SetSingle(VK_UNKNOWN,[]);
  ecInspect:             SetSingle(VK_F5,[ssAlt]);
  ecEvaluate:            SetSingle(VK_F7,[XCtrl]);
  ecAddWatch:            SetSingle(VK_F5,[XCtrl]);
  ecAddBpSource:         SetSingle(VK_UNKNOWN,[]);
  ecAddBpAddress:        SetSingle(VK_UNKNOWN,[]);
  ecAddBpDataWatch:      SetSingle(VK_F5,[ssShift]);

  // components menu
  ecNewPackage:          SetSingle(VK_UNKNOWN,[]);
  ecOpenPackage:         SetSingle(VK_UNKNOWN,[]);
  ecOpenPackageFile:     SetSingle(VK_UNKNOWN,[]);
  ecOpenPackageOfCurUnit:SetSingle(VK_UNKNOWN,[]);
  ecAddCurFileToPkg:     SetSingle(VK_UNKNOWN,[]);
  ecNewPkgComponent:     SetSingle(VK_UNKNOWN,[]);
  ecPackageGraph:        SetSingle(VK_UNKNOWN,[]);
  ecPackageLinks:        SetSingle(VK_UNKNOWN,[]);
  ecEditInstallPkgs:     SetSingle(VK_UNKNOWN,[]);
  ecConfigCustomComps:   SetSingle(VK_UNKNOWN,[]);

  // tools menu
  ecEnvironmentOptions:  SetSingle(VK_O,[ssShift,XCtrl]);
  ecRescanFPCSrcDir:     SetSingle(VK_UNKNOWN,[]);
  ecEditCodeTemplates:   SetSingle(VK_UNKNOWN,[]);
  ecCodeToolsDefinesEd:  SetSingle(VK_UNKNOWN,[]);

  ecExtToolSettings:     SetSingle(VK_UNKNOWN,[]);
  ecManageExamples:      SetSingle(VK_UNKNOWN,[]);
  ecBuildLazarus:        SetSingle(VK_UNKNOWN,[]);
  ecConfigBuildLazarus:  SetSingle(VK_UNKNOWN,[]);
  ecMakeResourceString:  SetSingle(VK_UNKNOWN,[]);
  ecDiff:                SetSingle(VK_UNKNOWN,[]);

  // window menu
  ecManageSourceEditors:       SetSingle(VK_W,[ssShift,XCtrl]);

  // help menu
  ecAboutLazarus:        SetSingle(VK_UNKNOWN,[]);
  ecOnlineHelp:          SetSingle(VK_UNKNOWN,[]);
  ecContextHelp:         SetSingle(VK_F1,[]);
  ecEditContextHelp:     SetSingle(VK_F1,[ssShift,XCtrl]);
  ecReportingBug:        SetSingle(VK_UNKNOWN,[]);
  ecFocusHint:           SetSingle(VK_UNKNOWN,[]);

  // designer
  ecDesignerCopy:        SetSingle(VK_C,[XCtrl],   VK_Insert,[XCtrl]);
  ecDesignerCut:         SetSingle(VK_X,[XCtrl],   VK_Delete,[ssShift]);
  ecDesignerPaste:       SetSingle(VK_V,[XCtrl],   VK_Insert,[ssShift]);
  ecDesignerSelectParent:SetSingle(VK_ESCAPE,[]);
  ecDesignerMoveToFront: SetSingle(VK_PRIOR,[ssShift]);
  ecDesignerMoveToBack:  SetSingle(VK_NEXT,[ssShift]);
  ecDesignerForwardOne:  SetSingle(VK_PRIOR,[XCtrl]);
  ecDesignerBackOne:     SetSingle(VK_NEXT,[XCtrl]);
  ecDesignerToggleNonVisComps: SetSingle(VK_UNKNOWN,[]);

  // macro
  ecSynMacroRecord:      SetSingle(VK_R,[ssShift, XCtrl]);
  ecSynMacroPlay:        SetSingle(VK_P,[ssShift, XCtrl]);

  ecIdePTmplEdNextCell:         SetSingle(VK_RIGHT,[XCtrl]);
  ecIdePTmplEdNextCellSel:      SetSingle(VK_TAB,[]);
  ecIdePTmplEdNextCellRotate:   SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdNextCellSelRotate:SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdPrevCell:         SetSingle(VK_LEFT,[XCtrl]);
  ecIdePTmplEdPrevCellSel:      SetSingle(VK_TAB,[ssShift]);
  ecIdePTmplEdCellHome:         SetSingle(VK_HOME,[]);
  ecIdePTmplEdCellEnd:          SetSingle(VK_END,[]);
  ecIdePTmplEdCellSelect:       SetSingle(VK_A,[XCtrl]);
  ecIdePTmplEdFinish:           SetSingle(VK_RETURN,[]);
  ecIdePTmplEdEscape:           SetSingle(VK_ESCAPE,[]);
  // Edit template
  ecIdePTmplEdOutNextCell:         SetSingle(VK_RIGHT,[XCtrl]);
  ecIdePTmplEdOutNextCellSel:      SetSingle(VK_TAB,[]);
  ecIdePTmplEdOutNextCellRotate:   SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutNextCellSelRotate:SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutPrevCell:         SetSingle(VK_LEFT,[XCtrl]);
  ecIdePTmplEdOutPrevCellSel:      SetSingle(VK_TAB,[ssShift]);
  ecIdePTmplEdOutCellHome:         SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutCellEnd:          SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutCellSelect:       SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutFinish:           SetSingle(VK_RETURN,[]);
  ecIdePTmplEdOutEscape:           SetSingle(VK_ESCAPE,[]);
  // SyncroEdit
  ecIdePSyncroEdNextCell:       SetSingle(VK_RIGHT,[XCtrl]);
  ecIdePSyncroEdNextCellSel:    SetSingle(VK_TAB,[]);
  ecIdePSyncroEdPrevCell:       SetSingle(VK_LEFT,[XCtrl]);
  ecIdePSyncroEdPrevCellSel:    SetSingle(VK_TAB,[ssShift]);
  ecIdePSyncroEdCellHome:       SetSingle(VK_HOME,[]);
  ecIdePSyncroEdCellEnd:        SetSingle(VK_END,[]);
  ecIdePSyncroEdCellSelect:     SetSingle(VK_A,[XCtrl]);
  ecIdePSyncroEdEscape:         SetSingle(VK_ESCAPE,[]);
  // SyncroEdit
  ecIdePSyncroEdOutNextCell:       SetSingle(VK_RIGHT,[XCtrl]);
  ecIdePSyncroEdOutNextCellSel:    SetSingle(VK_TAB,[]);
  ecIdePSyncroEdOutPrevCell:       SetSingle(VK_LEFT,[XCtrl]);
  ecIdePSyncroEdOutPrevCellSel:    SetSingle(VK_TAB,[ssShift]);
  ecIdePSyncroEdOutCellHome:       SetSingle(VK_UNKNOWN,[]);
  ecIdePSyncroEdOutCellEnd:        SetSingle(VK_UNKNOWN,[]);
  ecIdePSyncroEdOutCellSelect:     SetSingle(VK_UNKNOWN,[]);
  ecIdePSyncroEdOutEscape:         SetSingle(VK_ESCAPE,[]);
  // SyncroEdit, during selection
  ecIdePSyncroEdSelStart:          SetSingle(VK_J,[XCtrl]);

  else
    begin
      SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        // Edit template
    end;
  end;
end;

procedure TKeyCommandRelation.GetDefaultKeyForClassicScheme;
begin
  GetDefaultKeyForWindowsScheme;

  case Command of
  // moving
  ecLeft:                SetSingle(VK_S,[ssCtrl], VK_LEFT,[]);
  ecRight:               SetSingle(VK_D,[ssCtrl], VK_RIGHT,[]);
  ecUp:                  SetSingle(VK_E,[ssCtrl], VK_UP,[]);
  ecDown:                SetSingle(VK_X,[ssCtrl], VK_DOWN,[]);
  ecWordLeft:            SetSingle(VK_A,[ssCtrl], VK_LEFT,[ssCtrl]);
  ecWordRight:           SetSingle(VK_F,[ssCtrl], VK_RIGHT,[ssCtrl]);
  ecLineStart:           SetCombo(VK_Q,[ssCtrl],VK_S,[],   VK_HOME,[],VK_UNKNOWN,[]);
  ecLineEnd:             SetCombo(VK_Q,[ssCtrl],VK_D,[],   VK_END,[],VK_UNKNOWN,[]);
  ecPageUp:              SetSingle(VK_R,[ssCtrl], VK_PRIOR,[]);
  ecPageDown:            SetSingle(VK_C,[ssCtrl], VK_NEXT,[]);
  // Paragraph Down: VK_B, [ssCtrl]
  ecPageLeft:            SetSingle(VK_UNKNOWN,[]);
  ecPageRight:           SetSingle(VK_UNKNOWN,[]);
  ecPageTop:             SetCombo(VK_Q,[ssCtrl],VK_E,[],   VK_HOME,[ssCtrl],VK_UNKNOWN,[]);
  ecPageBottom:          SetCombo(VK_Q,[ssCtrl],VK_X,[],   VK_END,[ssCtrl],VK_UNKNOWN,[]);
  ecEditorTop:           SetCombo(VK_Q,[ssCtrl],VK_R,[],   VK_PRIOR,[ssCtrl],VK_UNKNOWN,[]);
  ecEditorBottom:        SetCombo(VK_Q,[ssCtrl],VK_C,[],   VK_NEXT,[ssCtrl],VK_UNKNOWN,[]);
  ecScrollUp:            SetSingle(VK_W,[ssCtrl], VK_UP,[ssCtrl]);
  ecScrollDown:          SetSingle(VK_Z,[ssCtrl], VK_DOWN,[ssCtrl]);
  ecScrollLeft:          SetSingle(VK_UNKNOWN,[]);
  ecScrollRight:         SetSingle(VK_UNKNOWN,[]);

  // selection
  ecSelLeft:             SetSingle(VK_LEFT,[ssShift]);
  ecSelRight:            SetSingle(VK_RIGHT,[ssShift]);
  ecSelUp:               SetSingle(VK_UP,[ssShift]);
  ecSelDown:             SetSingle(VK_DOWN,[ssShift]);
  ecCopy:                SetSingle(VK_Insert,[ssCtrl]);
  ecCut:                 SetSingle(VK_Delete,[ssShift]);
  ecPaste:               SetSingle(VK_Insert,[ssShift]);
  ecMultiPaste:          SetSingle(VK_UNKNOWN,[]);
  ecNormalSelect:        SetCombo(VK_O,[ssCtrl],VK_K,[]);
  ecColumnSelect:        SetCombo(VK_O,[ssCtrl],VK_C,[]);
  ecLineSelect:          SetCombo(VK_K,[ssCtrl],VK_L,[]);
  ecSelWordLeft:         SetSingle(VK_LEFT,[ssCtrl,ssShift]);
  ecSelWordRight:        SetSingle(VK_RIGHT,[ssCtrl,ssShift]);
  ecSelLineStart:        SetSingle(VK_HOME,[ssShift]);
  ecSelLineEnd:          SetSingle(VK_END,[ssShift]);
  ecSelPageTop:          SetSingle(VK_HOME,[ssShift,ssCtrl]);
  ecSelPageBottom:       SetSingle(VK_END,[ssShift,ssCtrl]);
  ecSelEditorTop:        SetSingle(VK_PRIOR,[ssShift,ssCtrl]);
  ecSelEditorBottom:     SetSingle(VK_NEXT,[ssShift,ssCtrl]);
  ecSelectAll:           SetSingle(VK_UNKNOWN,[]);
  ecSelectToBrace:       SetSingle(VK_UNKNOWN,[]);
  ecSelectCodeBlock:     SetSingle(VK_UNKNOWN,[]);
  ecSelectWord:          SetCombo(VK_K,[ssCtrl],VK_T,[]);
  ecSelectLine:          SetCombo(VK_O,[ssCtrl],VK_L,[]);
  ecSelectParagraph:     SetSingle(VK_UNKNOWN,[]);
  ecSelectionUpperCase:  SetCombo(VK_K,[ssCtrl],VK_N,[]);
  ecSelectionLowerCase:  SetCombo(VK_K,[ssCtrl],VK_O,[]);
  ecSelectionSwapCase:   SetCombo(VK_K,[SSCtrl],VK_P,[]);
  ecSelectionTabs2Spaces:SetSingle(VK_UNKNOWN,[]);
  ecSelectionEnclose:    SetSingle(VK_UNKNOWN,[]);
  ecSelectionComment:    SetSingle(VK_UNKNOWN,[]);
  ecSelectionUncomment:  SetSingle(VK_UNKNOWN,[]);
  ecToggleComment:       SetSingle(VK_OEM_2,[ssCtrl]);
  ecSelectionEncloseIFDEF:SetSingle(VK_D,[ssShift,ssCtrl]);
  ecSelectionSort:       SetSingle(VK_UNKNOWN,[]);
  ecSelectionBreakLines: SetSingle(VK_UNKNOWN,[]);

  ecBlockSetBegin:       SetCombo(VK_K,[ssCtrl],VK_B,[]);
  ecBlockSetEnd:         SetCombo(VK_K,[ssCtrl],VK_K,[]);
  ecBlockToggleHide:     SetCombo(VK_K,[ssCtrl],VK_H,[]);
  ecBlockHide:           SetSingle(VK_UNKNOWN,[]);
  ecBlockShow:           SetSingle(VK_UNKNOWN,[]);
  ecBlockMove:           SetCombo(VK_K,[ssCtrl],VK_V,[]);
  ecBlockCopy:           SetCombo(VK_K,[ssCtrl],VK_C,[]);
  ecBlockDelete:         SetCombo(VK_K,[ssCtrl],VK_Y,[]);
  ecBlockGotoBegin:      SetCombo(VK_Q,[ssCtrl],VK_B,[]);
  ecBlockGotoEnd:        SetCombo(VK_Q,[ssCtrl],VK_K,[]);

  // column mode selection
  ecColSelUp:            SetSingle(VK_UP,   [ssAlt,ssShift]);
  ecColSelDown:          SetSingle(VK_DOWN, [ssAlt,ssShift]);
  ecColSelLeft:          SetSingle(VK_LEFT, [ssAlt,ssShift]);
  ecColSelRight:         SetSingle(VK_RIGHT,[ssAlt,ssShift]);
  ecColSelPageDown:      SetSingle(VK_NEXT, [ssAlt,ssShift]);
  ecColSelPageBottom:    SetSingle(VK_NEXT, [ssAlt,ssShift,ssCtrl]);
  ecColSelPageUp:        SetSingle(VK_PRIOR,[ssAlt,ssShift]);
  ecColSelPageTop:       SetSingle(VK_PRIOR,[ssAlt,ssShift,ssCtrl]);
  ecColSelLineStart:     SetSingle(VK_HOME, [ssAlt,ssShift]);
  ecColSelLineEnd:       SetSingle(VK_END,  [ssAlt,ssShift]);
  ecColSelEditorTop:     SetSingle(VK_HOME, [ssAlt,ssShift,ssCtrl]);
  ecColSelEditorBottom:  SetSingle(VK_END,  [ssAlt,ssShift,ssCtrl]);

  // multi caret
  ecPluginMultiCaretSetCaret:    SetSingle(VK_INSERT,[ssShift, ssCtrl]);
  ecPluginMultiCaretUnsetCaret:  SetSingle(VK_DELETE,[ssShift, ssCtrl]);
  //ecPluginMultiCaretToggleCaret: SetSingle(VK_INSERT,[ssShift, ssCtrl]);
  ecPluginMultiCaretClearAll:    SetSingle(VK_ESCAPE,[ssShift, ssCtrl], VK_ESCAPE,[]);

  ecPluginMultiCaretModeCancelOnMove:  SetCombo(VK_Q,[ssShift, ssCtrl], VK_X,[ssShift, ssCtrl]);
  ecPluginMultiCaretModeMoveAll:       SetCombo(VK_Q,[ssShift, ssCtrl], VK_M,[ssShift, ssCtrl]);

  // editing
  ecInsertMode:          SetSingle(VK_V,[ssCtrl],    VK_INSERT,[]);
  ecBlockIndent:         SetCombo(VK_K,[ssCtrl],VK_I,[]);
  ecBlockUnindent:       SetCombo(VK_K,[ssCtrl],VK_U,[]);
  ecDeleteLastChar:      SetSingle(VK_H,[ssCtrl],    VK_BACK,[]);
  ecDeleteChar:          SetSingle(VK_G,[ssCtrl],    VK_DELETE,[]);
  ecDeleteWord:          SetSingle(VK_T,[ssCtrl]);
  ecDeleteLastWord:      SetSingle(VK_BACK,[ssCtrl]);
  ecDeleteBOL:           SetCombo(VK_Q,[ssCtrl],VK_H,[]);
  ecDeleteEOL:           SetCombo(VK_Q,[ssCtrl],VK_Y,[]);
  ecDeleteLine:          SetSingle(VK_Y,[ssCtrl]);
  ecClearAll:            SetSingle(VK_UNKNOWN,[]);
  ecLineBreak:           SetSingle(VK_RETURN,[],     VK_M,[ssCtrl]);
  ecInsertLine:          SetSingle(VK_N,[ssCtrl]);
  ecInsertCharacter:     SetSingle(VK_UNKNOWN,[]);
  // all insert text snippet keys have no default key

  // command commands
  ecUndo:                SetSingle(VK_BACK,[ssALT],  VK_U,[ssCtrl]);
  ecRedo:                SetSingle(VK_BACK,[ssALT,ssShift]);

  // search & replace
  ecMatchBracket:        SetSingle(VK_UNKNOWN,[]);
  ecFind:                SetCombo(VK_Q,[SSCtrl],VK_F,[]);
  ecFindNext:            SetSingle(VK_L,[ssCtrl]);
  ecFindPrevious:        SetSingle(VK_UNKNOWN,[]);
  ecFindInFiles:         SetSingle(VK_UNKNOWN,[]);
  ecReplace:             SetCombo(VK_Q,[SSCtrl],VK_A,[]);
  ecIncrementalFind:     SetSingle(VK_UNKNOWN,[]);
  ecGotoLineNumber:      SetCombo(VK_Q,[ssCtrl],VK_G,[]);
  ecFindNextWordOccurrence:SetSingle(VK_UNKNOWN,[]);
  ecFindPrevWordOccurrence:SetSingle(VK_UNKNOWN,[]);
  ecJumpBack:            SetSingle(VK_B,[ssCtrl]);
  ecJumpForward:         SetSingle(VK_B,[ssShift,ssCtrl]);
  ecAddJumpPoint:        SetSingle(VK_UNKNOWN,[]);
  ecJumpToPrevError:     SetSingle(VK_F7,[ssShift,ssAlt]);
  ecJumpToNextError:     SetSingle(VK_F8,[ssShift,ssAlt]);
  ecOpenFileAtCursor:    SetSingle(VK_RETURN,[ssCtrl]);

  // marker
  ecSetFreeBookmark:     SetSingle(VK_UNKNOWN,[]);
  ecClearBookmarkForFile:SetSingle(VK_UNKNOWN,[]);
  ecClearAllBookmark:    SetSingle(VK_UNKNOWN,[]);
  ecPrevBookmark:        SetSingle(VK_UNKNOWN,[]);
  ecNextBookmark:        SetSingle(VK_UNKNOWN,[]);
  ecGotoMarker0:         SetCombo(VK_Q,[ssCtrl],VK_0,[]);
  ecGotoMarker1:         SetCombo(VK_Q,[ssCtrl],VK_1,[]);
  ecGotoMarker2:         SetCombo(VK_Q,[ssCtrl],VK_2,[]);
  ecGotoMarker3:         SetCombo(VK_Q,[ssCtrl],VK_3,[]);
  ecGotoMarker4:         SetCombo(VK_Q,[ssCtrl],VK_4,[]);
  ecGotoMarker5:         SetCombo(VK_Q,[ssCtrl],VK_5,[]);
  ecGotoMarker6:         SetCombo(VK_Q,[ssCtrl],VK_6,[]);
  ecGotoMarker7:         SetCombo(VK_Q,[ssCtrl],VK_7,[]);
  ecGotoMarker8:         SetCombo(VK_Q,[ssCtrl],VK_8,[]);
  ecGotoMarker9:         SetCombo(VK_Q,[ssCtrl],VK_9,[]);
  ecSetMarker0..ecSetMarker9: SetSingle(VK_UNKNOWN,[]);
  ecToggleMarker0:       SetCombo(VK_K,[ssCtrl],VK_0,[]);
  ecToggleMarker1:       SetCombo(VK_K,[ssCtrl],VK_1,[]);
  ecToggleMarker2:       SetCombo(VK_K,[ssCtrl],VK_2,[]);
  ecToggleMarker3:       SetCombo(VK_K,[ssCtrl],VK_3,[]);
  ecToggleMarker4:       SetCombo(VK_K,[ssCtrl],VK_4,[]);
  ecToggleMarker5:       SetCombo(VK_K,[ssCtrl],VK_5,[]);
  ecToggleMarker6:       SetCombo(VK_K,[ssCtrl],VK_6,[]);
  ecToggleMarker7:       SetCombo(VK_K,[ssCtrl],VK_7,[]);
  ecToggleMarker8:       SetCombo(VK_K,[ssCtrl],VK_8,[]);
  ecToggleMarker9:       SetCombo(VK_K,[ssCtrl],VK_9,[]);

  // codetools
  ecAutoCompletion:      SetSingle(VK_J,[ssCtrl]);
  ecWordCompletion:      SetSingle(VK_W,[ssShift,ssCtrl]);
  ecCompleteCode:        SetSingle(VK_C,[ssShift,ssCtrl]);
  ecCompleteCodeInteractive: SetSingle(VK_X,[ssCtrl,ssShift]);
  ecIdentCompletion:     SetSingle(VK_UNKNOWN,[]);
  ecShowCodeContext:     SetSingle(VK_SPACE,[ssShift,ssCtrl]);
  ecExtractProc:         SetSingle(VK_UNKNOWN,[]);
  ecFindIdentifierRefs:  SetSingle(VK_UNKNOWN,[]);
  ecFindUsedUnitRefs:    SetSingle(VK_UNKNOWN,[]);
  ecRenameIdentifier:    SetSingle(VK_E,[ssShift,ssCtrl]);
  ecInvertAssignment:    SetSingle(VK_UNKNOWN,[]);
  ecSyntaxCheck:         SetSingle(VK_UNKNOWN,[]);
  ecGuessUnclosedBlock:  SetSingle(VK_UNKNOWN,[]);
  ecGuessMisplacedIFDEF: SetSingle(VK_UNKNOWN,[]);
  ecConvertDFM2LFM:      SetSingle(VK_UNKNOWN,[]);
  ecCheckLFM:            SetSingle(VK_UNKNOWN,[]);
  ecConvertDelphiUnit:   SetSingle(VK_UNKNOWN,[]);
  ecConvertDelphiProject:SetSingle(VK_UNKNOWN,[]);
  ecConvertDelphiPackage:SetSingle(VK_UNKNOWN,[]);
  ecConvertEncoding:     SetSingle(VK_UNKNOWN,[]);
  ecFindProcedureDefinition:SetSingle(VK_UP,[ssShift,SSCtrl]);
  ecFindProcedureMethod: SetSingle(VK_DOWN,[ssShift,SSCtrl]);
  ecFindDeclaration:     SetSingle(VK_UNKNOWN,[]);
  ecFindBlockOtherEnd:   SetCombo(VK_Q,[ssCtrl],VK_O,[]);
  ecFindBlockStart:      SetCombo(VK_Q,[ssCtrl],VK_M,[]);
  ecGotoIncludeDirective:SetSingle(VK_UNKNOWN,[]);
  ecShowAbstractMethods: SetSingle(VK_UNKNOWN,[]);
  ecRemoveEmptyMethods:  SetSingle(VK_UNKNOWN,[]);

  // source notebook
  ecNextEditor:          SetSingle(VK_F6,[],         VK_TAB,[ssCtrl]);
  ecPrevEditor:          SetSingle(VK_F6,[ssShift],  VK_TAB,[ssShift,ssCtrl]);
  ecPrevEditorInHistory: SetSingle(VK_OEM_3,[ssCtrl]);//~
  ecNextEditorInHistory: SetSingle(VK_OEM_3,[ssShift,ssCtrl]);//~
  ecResetDebugger:       SetSingle(VK_UNKNOWN,[]);
  ecToggleBreakPoint:    SetSingle(VK_UNKNOWN,[]);
  ecToggleBreakPointEnabled:    SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorLeft:      SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorRight:     SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorLeftmost:  SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorRightmost: SetSingle(VK_UNKNOWN,[]);

  ecNextSharedEditor:    SetSingle(VK_UNKNOWN,[]);
  ecPrevSharedEditor:    SetSingle(VK_UNKNOWN,[]);
  ecNextWindow:          SetSingle(VK_UNKNOWN,[]);
  ecPrevWindow:          SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorNextWindow:SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorPrevWindow:SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorNewWindow: SetSingle(VK_UNKNOWN,[]);
  ecCopyEditorNextWindow:SetSingle(VK_UNKNOWN,[]);
  ecCopyEditorPrevWindow:SetSingle(VK_UNKNOWN,[]);
  ecCopyEditorNewWindow: SetSingle(VK_UNKNOWN,[]);

  ecGotoEditor1:         SetSingle(VK_1,[ssAlt]);
  ecGotoEditor2:         SetSingle(VK_2,[ssAlt]);
  ecGotoEditor3:         SetSingle(VK_3,[ssAlt]);
  ecGotoEditor4:         SetSingle(VK_4,[ssAlt]);
  ecGotoEditor5:         SetSingle(VK_5,[ssAlt]);
  ecGotoEditor6:         SetSingle(VK_6,[ssAlt]);
  ecGotoEditor7:         SetSingle(VK_7,[ssAlt]);
  ecGotoEditor8:         SetSingle(VK_8,[ssAlt]);
  ecGotoEditor9:         SetSingle(VK_9,[ssAlt]);
  ecGotoEditor0:         SetSingle(VK_0,[ssAlt]);

  ecLockEditor:          SetSingle(VK_UNKNOWN,[]);

  EcFoldLevel1:          SetSingle(VK_1,[ssAlt,ssShift]);
  EcFoldLevel2:          SetSingle(VK_2,[ssAlt,ssShift]);
  EcFoldLevel3:          SetSingle(VK_3,[ssAlt,ssShift]);
  EcFoldLevel4:          SetSingle(VK_4,[ssAlt,ssShift]);
  EcFoldLevel5:          SetSingle(VK_5,[ssAlt,ssShift]);
  EcFoldLevel6:          SetSingle(VK_6,[ssAlt,ssShift]);
  EcFoldLevel7:          SetSingle(VK_7,[ssAlt,ssShift]);
  EcFoldLevel8:          SetSingle(VK_8,[ssAlt,ssShift]);
  EcFoldLevel9:          SetSingle(VK_9,[ssAlt,ssShift]);
  EcFoldLevel0:          SetSingle(VK_0,[ssAlt,ssShift]);
  EcFoldCurrent:         SetSingle(VK_OEM_PLUS,[ssAlt,ssShift]);
  EcUnFoldCurrent:       SetSingle(VK_OEM_MINUS,[ssAlt,ssShift]);
  EcToggleMarkupWord:    SetSingle(VK_M,[ssAlt]);
  ecGotoBookmarks:       SetSingle(VK_B,[ssCtrl]);
  ecToggleBookmarks:     SetSingle(VK_B,[ssCtrl,ssShift]);

  // file menu
  ecNew:                 SetSingle(VK_UNKNOWN,[]);
  ecNewUnit:             SetSingle(VK_UNKNOWN,[]);
  ecNewForm:             SetSingle(VK_UNKNOWN,[]);
  ecOpen:                SetSingle(VK_F3,[]);
  ecOpenUnit:            SetSingle(VK_F12,[ssAlt]);
  ecRevert:              SetSingle(VK_UNKNOWN,[]);
  ecSave:                SetSingle(VK_F2,[]);
  ecSaveAs:              SetSingle(VK_UNKNOWN,[]);
  ecSaveAll:             SetSingle(VK_F2,[ssShift]);
  ecClose:               SetSingle(VK_F3,[ssAlt]);
  ecCloseAll:            SetSingle(VK_UNKNOWN,[]);
  ecCloseOtherTabs:      SetSingle(VK_UNKNOWN,[]);
  ecCloseRightTabs:      SetSingle(VK_UNKNOWN,[]);
  ecCleanDirectory:      SetSingle(VK_UNKNOWN,[]);
  ecRestart:             SetSingle(VK_UNKNOWN,[]);
  ecQuit:                SetSingle(VK_X,[ssAlt]);

  // view menu
  ecToggleObjectInsp:    SetSingle(VK_F11,[]);
  ecToggleSourceEditor:  SetSingle(VK_UNKNOWN,[]);
  ecToggleCodeExpl:      SetSingle(VK_UNKNOWN,[]);
  ecToggleFPDocEditor:   SetSingle(VK_UNKNOWN,[]);
  ecToggleMessages:      SetSingle(VK_UNKNOWN,[]);
  ecViewComponents:      SetSingle(VK_UNKNOWN,[]);
  ecViewJumpHistory:     SetSingle(VK_UNKNOWN,[]);
  ecToggleSearchResults: SetSingle(VK_UNKNOWN,[]);
  ecToggleWatches:       SetSingle(VK_UNKNOWN,[]);
  ecToggleBreakPoints:   SetSingle(VK_F8,[ssCtrl]);
  ecToggleLocals:        SetSingle(VK_UNKNOWN,[]);
  ecToggleCallStack:     SetSingle(VK_F3,[ssCtrl]);
  ecToggleRegisters:     SetSingle(VK_UNKNOWN,[]);
  ecToggleAssembler:     SetSingle(VK_UNKNOWN,[]);
  ecToggleDebugEvents:   SetSingle(VK_UNKNOWN,[]);
  ecToggleDebuggerOut:   SetSingle(VK_UNKNOWN,[]);
  ecViewUnitDependencies:SetSingle(VK_UNKNOWN,[]);
  ecViewUnitInfo:        SetSingle(VK_UNKNOWN,[]);
  ecToggleFormUnit:      SetSingle(VK_F12,[]);
  ecViewAnchorEditor:    SetSingle(VK_UNKNOWN,[]);
  ecToggleCodeBrowser:   SetSingle(VK_UNKNOWN,[]);
  ecToggleRestrictionBrowser:SetSingle(VK_UNKNOWN,[]);
  ecToggleCompPalette:   SetSingle(VK_UNKNOWN,[]);
  ecToggleIDESpeedBtns:  SetSingle(VK_UNKNOWN,[]);

  // project menu
  ecNewProject:          SetSingle(VK_UNKNOWN,[]);
  ecNewProjectFromFile:  SetSingle(VK_UNKNOWN,[]);
  ecOpenProject:         SetSingle(VK_F11,[ssCtrl]);
  ecCloseProject:        SetSingle(VK_UNKNOWN,[]);
  ecSaveProject:         SetSingle(VK_UNKNOWN,[]);
  ecSaveProjectAs:       SetSingle(VK_UNKNOWN,[]);
  ecProjectResaveFormsWithI18n: SetSingle(VK_UNKNOWN,[]);
  ecPublishProject:      SetSingle(VK_UNKNOWN,[]);
  ecProjectInspector:    SetSingle(VK_UNKNOWN,[]);
  ecAddCurUnitToProj:    SetSingle(VK_F11,[ssShift]);
  ecRemoveFromProj:      SetSingle(VK_UNKNOWN,[]);
  ecViewProjectUnits:    SetSingle(VK_F12,[ssCtrl]);
  ecViewProjectForms:    SetSingle(VK_F12,[ssShift]);
  ecViewProjectSource:   SetSingle(VK_UNKNOWN,[]);
  ecProjectOptions:      SetSingle(VK_F11,[ssShift,ssCtrl]);
  ecProjectChangeBuildMode:SetSingle(VK_UNKNOWN,[]);

  // run menu
  ecCompile:             SetSingle(VK_F9,[ssCtrl]);
  ecBuild:               SetSingle(VK_F9,[ssShift]);
  ecQuickCompile:        SetSingle(VK_UNKNOWN,[]);
  ecCleanUpAndBuild:     SetSingle(VK_UNKNOWN,[]);
  ecBuildManyModes:      SetSingle(VK_UNKNOWN,[]);
  ecAbortBuild:          SetSingle(VK_UNKNOWN,[]);
  ecRunWithoutDebugging: SetSingle(VK_F9,[ssCtrl, ssShift]);
  ecRun:                 SetSingle(VK_F9,[]);
  ecPause:               SetSingle(VK_UNKNOWN,[]);
  ecShowExecutionPoint:  SetSingle(VK_UNKNOWN,[]);
  ecStepInto:            SetSingle(VK_F7,[]);
  ecStepOver:            SetSingle(VK_F8,[]);
  ecStepIntoInstr:       SetSingle(VK_F7,[ssAlt]);
  ecStepOverInstr:       SetSingle(VK_F8,[ssAlt]);
  ecStepOut:             SetSingle(VK_F8,[ssShift]);
  ecRunToCursor:         SetSingle(VK_F4,[]);
  ecStopProgram:         SetSingle(VK_F2,[ssCtrl]);
  ecRemoveBreakPoint:    SetSingle(VK_UNKNOWN,[]);
  ecRunParameters:       SetSingle(VK_UNKNOWN,[]);
  ecBuildFile:           SetSingle(VK_UNKNOWN,[]);
  ecRunFile:             SetSingle(VK_UNKNOWN,[]);
  ecConfigBuildFile:     SetSingle(VK_UNKNOWN,[]);
  ecInspect:             SetSingle(VK_UNKNOWN,[]);
  ecEvaluate:            SetSingle(VK_F4,[ssCtrl]);
  ecAddWatch:            SetSingle(VK_F7,[ssCtrl]);
  ecAddBpSource:         SetSingle(VK_UNKNOWN,[]);
  ecAddBpAddress:        SetSingle(VK_UNKNOWN,[]);
  ecAddBpDataWatch:      SetSingle(VK_UNKNOWN,[]);

  // components menu
  ecNewPackage:          SetSingle(VK_UNKNOWN,[]);
  ecOpenPackage:         SetSingle(VK_UNKNOWN,[]);
  ecOpenPackageFile:     SetSingle(VK_UNKNOWN,[]);
  ecOpenPackageOfCurUnit:SetSingle(VK_UNKNOWN,[]);
  ecAddCurFileToPkg:     SetSingle(VK_UNKNOWN,[]);
  ecNewPkgComponent:     SetSingle(VK_UNKNOWN,[]);
  ecPackageGraph:        SetSingle(VK_UNKNOWN,[]);
  ecPackageLinks:        SetSingle(VK_UNKNOWN,[]);
  ecEditInstallPkgs:     SetSingle(VK_UNKNOWN,[]);
  ecConfigCustomComps:   SetSingle(VK_UNKNOWN,[]);

  // tools menu
  ecEnvironmentOptions:  SetSingle(VK_O,[ssShift,ssCtrl]);
  ecRescanFPCSrcDir:     SetSingle(VK_UNKNOWN,[]);
  ecEditCodeTemplates:   SetSingle(VK_UNKNOWN,[]);
  ecCodeToolsDefinesEd:  SetSingle(VK_UNKNOWN,[]);

  ecExtToolSettings:     SetSingle(VK_UNKNOWN,[]);
  ecManageExamples:      SetSingle(VK_UNKNOWN,[]);
  ecBuildLazarus:        SetSingle(VK_UNKNOWN,[]);
  ecConfigBuildLazarus:  SetSingle(VK_UNKNOWN,[]);
  ecMakeResourceString:  SetSingle(VK_UNKNOWN,[]);
  ecDiff:                SetSingle(VK_UNKNOWN,[]);

  // window menu
  ecManageSourceEditors:       SetSingle(VK_UNKNOWN,[]);

  // help menu
  ecAboutLazarus:        SetSingle(VK_UNKNOWN,[]);
  ecOnlineHelp:          SetSingle(VK_UNKNOWN,[]);
  ecContextHelp:         SetSingle(VK_F1,[ssCtrl]);
  ecEditContextHelp:     SetSingle(VK_F1,[ssCtrl,ssShift]);
  ecReportingBug:        SetSingle(VK_UNKNOWN,[]);
  ecFocusHint:           SetSingle(VK_UNKNOWN,[]);

  // designer
  ecDesignerCopy:        SetSingle(VK_C,[ssCtrl],    VK_Insert,[ssCtrl]);
  ecDesignerCut:         SetSingle(VK_X,[ssCtrl],    VK_Delete,[ssShift]);
  ecDesignerPaste:       SetSingle(VK_V,[ssCtrl],    VK_Insert,[ssShift]);
  ecDesignerSelectParent:SetSingle(VK_ESCAPE,[]);
  ecDesignerMoveToFront: SetSingle(VK_PRIOR,[ssShift]);
  ecDesignerMoveToBack:  SetSingle(VK_NEXT,[ssShift]);
  ecDesignerForwardOne:  SetSingle(VK_PRIOR,[ssCtrl]);
  ecDesignerBackOne:     SetSingle(VK_NEXT,[ssCtrl]);
  ecDesignerToggleNonVisComps: SetSingle(VK_UNKNOWN,[]);

  // macro
  ecSynMacroRecord:      SetSingle(VK_R,[ssShift, ssCtrl]);
  ecSynMacroPlay:        SetSingle(VK_P,[ssShift, ssCtrl]);

  // Edit template
  ecIdePTmplEdNextCell:         SetSingle(VK_RIGHT,[ssCtrl]);
  ecIdePTmplEdNextCellSel:      SetSingle(VK_TAB,[]);
  ecIdePTmplEdNextCellRotate:   SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdNextCellSelRotate:SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdPrevCell:         SetSingle(VK_LEFT,[ssCtrl]);
  ecIdePTmplEdPrevCellSel:      SetSingle(VK_TAB,[ssShift]);
  ecIdePTmplEdCellHome:         SetSingle(VK_HOME,[]);
  ecIdePTmplEdCellEnd:          SetSingle(VK_END,[]);
  ecIdePTmplEdCellSelect:       SetSingle(VK_A,[ssCtrl]);
  ecIdePTmplEdFinish:           SetSingle(VK_RETURN,[]);
  ecIdePTmplEdEscape:           SetSingle(VK_ESCAPE,[]);
  // Edit template
  ecIdePTmplEdOutNextCell:         SetSingle(VK_RIGHT,[ssCtrl]);
  ecIdePTmplEdOutNextCellSel:      SetSingle(VK_TAB,[]);
  ecIdePTmplEdOutNextCellRotate:   SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutNextCellSelRotate:SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutPrevCell:         SetSingle(VK_LEFT,[ssCtrl]);
  ecIdePTmplEdOutPrevCellSel:      SetSingle(VK_TAB,[ssShift]);
  ecIdePTmplEdOutCellHome:         SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutCellEnd:          SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutCellSelect:       SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutFinish:           SetSingle(VK_RETURN,[]);
  ecIdePTmplEdOutEscape:           SetSingle(VK_ESCAPE,[]);
  // SyncroEdit
  ecIdePSyncroEdNextCell:       SetSingle(VK_RIGHT,[ssCtrl]);
  ecIdePSyncroEdNextCellSel:    SetSingle(VK_TAB,[]);
  ecIdePSyncroEdPrevCell:       SetSingle(VK_LEFT,[ssCtrl]);
  ecIdePSyncroEdPrevCellSel:    SetSingle(VK_TAB,[ssShift]);
  ecIdePSyncroEdCellHome:       SetSingle(VK_HOME,[]);
  ecIdePSyncroEdCellEnd:        SetSingle(VK_END,[]);
  ecIdePSyncroEdCellSelect:     SetSingle(VK_A,[ssCtrl]);
  ecIdePSyncroEdEscape:         SetSingle(VK_ESCAPE,[]);
  // SyncroEdit
  ecIdePSyncroEdOutNextCell:       SetSingle(VK_RIGHT,[ssCtrl]);
  ecIdePSyncroEdOutNextCellSel:    SetSingle(VK_TAB,[]);
  ecIdePSyncroEdOutPrevCell:       SetSingle(VK_LEFT,[ssCtrl]);
  ecIdePSyncroEdOutPrevCellSel:    SetSingle(VK_TAB,[ssShift]);
  ecIdePSyncroEdOutCellHome:       SetSingle(VK_UNKNOWN,[]);
  ecIdePSyncroEdOutCellEnd:        SetSingle(VK_UNKNOWN,[]);
  ecIdePSyncroEdOutCellSelect:     SetSingle(VK_UNKNOWN,[]);
  ecIdePSyncroEdOutEscape:         SetSingle(VK_ESCAPE,[]);
  // SyncroEdit, during selection
  ecIdePSyncroEdSelStart:          SetSingle(VK_J,[ssCtrl]);

  else
    begin
      SetSingle(VK_UNKNOWN,[],VK_UNKNOWN,[]);
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
  ecLeft:                SetSingle(VK_LEFT,[]);
  ecRight:               SetSingle(VK_RIGHT,[]);
  ecUp:                  SetSingle(VK_UP,[]);
  ecDown:                SetSingle(VK_DOWN,[]);
  ecWordLeft:            SetSingle(VK_LEFT,[ssAlt]);
  ecWordRight:           SetSingle(VK_RIGHT,[ssAlt]);
  ecLineStart:           SetSingle(VK_LEFT,[ssMeta]);
  ecLineEnd:             SetSingle(VK_RIGHT,[ssMeta]);
  ecPageUp:              SetSingle(VK_PRIOR,[]);
  ecPageDown:            SetSingle(VK_NEXT,[]);
  ecPageLeft:            SetSingle(VK_UNKNOWN,[]);
  ecPageRight:           SetSingle(VK_UNKNOWN,[]);
  ecPageTop:             SetSingle(VK_PRIOR,[ssAlt]);
  ecPageBottom:          SetSingle(VK_END,[ssAlt]);
  ecEditorTop:           SetSingle(VK_HOME,[],       VK_UP,[ssMeta]);
  ecEditorBottom:        SetSingle(VK_END,[],        VK_DOWN,[ssMeta]);
  ecScrollUp:            SetSingle(VK_UP,[ssCtrl]);
  ecScrollDown:          SetSingle(VK_DOWN,[ssCtrl]);
  ecScrollLeft:          SetSingle(VK_UNKNOWN,[]);
  ecScrollRight:         SetSingle(VK_UNKNOWN,[]);

  // selection
  ecSelLeft:             SetSingle(VK_LEFT,[ssShift]);
  ecSelRight:            SetSingle(VK_RIGHT,[ssShift]);
  ecSelUp:               SetSingle(VK_UP,[ssShift]);
  ecSelDown:             SetSingle(VK_DOWN,[ssShift]);
  ecCopy:                SetSingle(VK_C,[ssMeta],    VK_Insert,[ssCtrl]);
  ecCut:                 SetSingle(VK_X,[ssMeta],    VK_Delete,[ssShift]);
  ecPaste:               SetSingle(VK_V,[ssMeta],    VK_Insert,[ssShift]);
  ecMultiPaste:          SetSingle(VK_UNKNOWN,[]);
  ecNormalSelect:        SetSingle(VK_UNKNOWN,[]);
  ecColumnSelect:        SetSingle(VK_UNKNOWN,[]);
  ecLineSelect:          SetSingle(VK_UNKNOWN,[]);
  ecSelWordLeft:         SetSingle(VK_LEFT,[ssAlt,ssShift]);
  ecSelWordRight:        SetSingle(VK_RIGHT,[ssAlt,ssShift]);
  ecSelLineStart:        SetSingle(VK_LEFT,[ssMeta,ssShift]);
  ecSelLineEnd:          SetSingle(VK_RIGHT,[ssMeta,ssShift]);
  ecSelPageTop:          SetSingle(VK_PRIOR,[ssAlt,ssShift]);
  ecSelPageBottom:       SetSingle(VK_NEXT,[ssAlt,ssShift]);
  ecSelEditorTop:        SetSingle(VK_HOME,[ssShift]);
  ecSelEditorBottom:     SetSingle(VK_END,[ssShift]);
  ecSelectAll:           SetSingle(VK_A,[ssMeta]);
  ecSelectToBrace:       SetSingle(VK_UNKNOWN,[]);
  ecSelectCodeBlock:     SetSingle(VK_UNKNOWN,[]);
  ecSelectWord:          SetCombo(VK_K,[SSCtrl],VK_T,[]);
  ecSelectLine:          SetCombo(VK_K,[SSCtrl],VK_L,[]);
  ecSelectParagraph:     SetSingle(VK_UNKNOWN,[]);
  ecSelectionUpperCase:  SetCombo(VK_K,[SSCtrl],VK_N,[]);
  ecSelectionLowerCase:  SetCombo(VK_K,[SSCtrl],VK_O,[]);
  ecSelectionSwapCase:   SetCombo(VK_K,[SSCtrl],VK_P,[]);
  ecSelectionTabs2Spaces:SetSingle(VK_UNKNOWN,[]);
  ecSelectionEnclose:    SetSingle(VK_UNKNOWN,[]);
  ecSelectionComment:    SetSingle(VK_UNKNOWN,[]);
  ecSelectionUncomment:  SetSingle(VK_UNKNOWN,[]);
  ecToggleComment:       SetSingle(VK_OEM_2,[ssCtrl]);
  ecSelectionEncloseIFDEF:SetSingle(VK_D,[ssShift,ssCtrl]);
  ecSelectionSort:       SetSingle(VK_UNKNOWN,[]);
  ecSelectionBreakLines: SetSingle(VK_UNKNOWN,[]);

  ecStickySelection:     SetCombo(VK_K,[ssCtrl],VK_S,[]);
  ecStickySelectionCol:  SetCombo(VK_K,[ssCtrl],VK_S,[ssAlt]);
  ecStickySelectionStop: SetCombo(VK_K,[ssCtrl],VK_E,[]);

  ecBlockSetBegin:       SetCombo(VK_K,[ssCtrl],VK_B,[]);
  ecBlockSetEnd:         SetCombo(VK_K,[ssCtrl],VK_K,[]);
  ecBlockToggleHide:     SetCombo(VK_K,[ssCtrl],VK_H,[]);
  ecBlockHide:           SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockShow:           SetCombo(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockMove:           SetCombo(VK_K,[ssCtrl],VK_V,[]);
  ecBlockCopy:           SetCombo(VK_K,[ssCtrl],VK_C,[]);
  ecBlockDelete:         SetCombo(VK_K,[ssCtrl],VK_Y,[]);
  ecBlockGotoBegin:      SetCombo(VK_Q,[ssCtrl],VK_B,[]);
  ecBlockGotoEnd:        SetCombo(VK_Q,[ssCtrl],VK_K,[]);

// column mode selection
  ecColSelUp:            SetSingle(VK_UP,[ssAlt,ssShift]);
  ecColSelDown:          SetSingle(VK_DOWN,[ssAlt,ssShift]);
  ecColSelLeft:          SetSingle(VK_UNKNOWN,[]); // VK_LEFT,[ssAlt,ssShift] conflicts.
  ecColSelRight:         SetSingle(VK_UNKNOWN,[]); // VK_RIGHT,[ssAlt,ssShift] conflicts.
  ecColSelPageDown:      SetSingle(VK_UNKNOWN,[]); // VK_NEXT,[ssAlt,ssShift] conflicts.
  ecColSelPageBottom:    SetSingle(VK_NEXT,[ssAlt,ssShift,ssCtrl]);
  ecColSelPageUp:        SetSingle(VK_UNKNOWN,[]); // VK_PRIOR,[ssAlt,ssShift] conflicts.
  ecColSelPageTop:       SetSingle(VK_PRIOR,[ssAlt,ssShift,ssCtrl]);
  ecColSelLineStart:     SetSingle(VK_HOME,[ssAlt,ssShift]);
  ecColSelLineEnd:       SetSingle(VK_END,[ssAlt,ssShift]);
  ecColSelEditorTop:     SetSingle(VK_HOME,[ssAlt,ssShift,ssCtrl]);
  ecColSelEditorBottom:  SetSingle(VK_END,[ssAlt,ssShift,ssCtrl]);

  // multi caret
  ecPluginMultiCaretSetCaret:    SetSingle(VK_INSERT,[ssShift, ssCtrl]);
  ecPluginMultiCaretUnsetCaret:  SetSingle(VK_DELETE,[ssShift, ssCtrl]);
  //ecPluginMultiCaretToggleCaret: SetSingle(VK_INSERT,[ssShift, ssCtrl]);
  ecPluginMultiCaretClearAll:    SetSingle(VK_ESCAPE,[ssShift, ssCtrl], VK_ESCAPE,[]);

  ecPluginMultiCaretModeCancelOnMove:  SetCombo(VK_Q,[ssShift, ssCtrl], VK_X,[ssShift, ssCtrl]);
  ecPluginMultiCaretModeMoveAll:       SetCombo(VK_Q,[ssShift, ssCtrl], VK_M,[ssShift, ssCtrl]);

  // editing
  ecBlockIndent:         SetCombo(VK_I,[ssCtrl],VK_UNKNOWN,[],  VK_K,[SSCtrl],VK_I,[]);
  ecBlockUnindent:       SetCombo(VK_U,[ssCtrl],VK_UNKNOWN,[],  VK_K,[SSCtrl],VK_U,[]);
  ecDeleteLastChar:      SetSingle(VK_BACK,[],       VK_BACK,[ssShift]); // ctrl H used for scroll window.
  ecDeleteChar:          SetSingle(VK_DELETE,[]); // ctrl G conflicts with GO
  ecDeleteWord:          SetSingle(VK_DELETE,[ssAlt]);
  ecDeleteLastWord:      SetSingle(VK_BACK,[ssCtrl]);
  ecDeleteBOL:           SetSingle(VK_BACK,[ssMeta]);
  ecDeleteEOL:           SetSingle(VK_DELETE,[ssMeta]);
  ecDeleteLine:          SetSingle(VK_Y,[ssCtrl]);
  ecClearAll:            SetSingle(VK_UNKNOWN,[]);
  ecLineBreak:           SetSingle(VK_RETURN,[]);
  ecInsertLine:          SetSingle(VK_N,[ssShift,ssMeta]);
  ecInsertCharacter:     SetSingle(VK_UNKNOWN,[]);
  ecInsertGUID:          SetSingle(VK_G,[ssCtrl,ssShift]);
  // Note: all insert text snippet keys have no default key

  // command commands
  ecUndo:                SetSingle(VK_Z,[ssMeta]);
  ecRedo:                SetSingle(VK_Z,[ssMeta,ssShift]);

  // search & replace
  ecMatchBracket:        SetSingle(VK_UNKNOWN,[]);
  ecFind:                SetSingle(VK_F,[ssMeta]);
  ecFindNext:            SetSingle(VK_G,[ssMeta]);
  ecFindPrevious:        SetSingle(VK_G,[ssShift,ssMeta]);
  ecFindInFiles:         SetSingle(VK_F,[ssMeta,ssShift]);
  ecReplace:             SetSingle(VK_UNKNOWN,[]);
  ecIncrementalFind:     SetSingle(VK_E,[ssMeta]);
  ecGotoLineNumber:      SetSingle(VK_L,[ssMeta]);
  ecFindNextWordOccurrence:SetSingle(VK_UNKNOWN,[]);
  ecFindPrevWordOccurrence:SetSingle(VK_UNKNOWN,[]);
  ecJumpBack:            SetSingle(VK_H,[ssCtrl]);
  ecJumpForward:         SetSingle(VK_H,[ssCtrl,ssShift]);
  ecAddJumpPoint:        SetSingle(VK_UNKNOWN,[]);
  ecJumpToPrevError:     SetSingle(VK_ADD,[ssMeta,ssShift]);
  ecJumpToNextError:     SetSingle(VK_ADD,[ssMeta]);
  ecOpenFileAtCursor:    SetSingle(VK_RETURN,[ssCtrl]);
  ecProcedureList:       SetSingle(VK_G,[ssAlt]);

  // marker
  ecSetFreeBookmark:     SetSingle(VK_UNKNOWN,[]);
  ecClearBookmarkForFile:SetSingle(VK_UNKNOWN,[]);
  ecClearAllBookmark:    SetSingle(VK_UNKNOWN,[]);
  ecPrevBookmark:        SetSingle(VK_UNKNOWN,[]);
  ecNextBookmark:        SetSingle(VK_UNKNOWN,[]);
  ecGotoMarker0:         SetSingle(VK_0,[ssCtrl]);
  ecGotoMarker1:         SetSingle(VK_1,[ssCtrl]);
  ecGotoMarker2:         SetSingle(VK_2,[ssCtrl]);
  ecGotoMarker3:         SetSingle(VK_3,[ssCtrl]);
  ecGotoMarker4:         SetSingle(VK_4,[ssCtrl]);
  ecGotoMarker5:         SetSingle(VK_5,[ssCtrl]);
  ecGotoMarker6:         SetSingle(VK_6,[ssCtrl]);
  ecGotoMarker7:         SetSingle(VK_7,[ssCtrl]);
  ecGotoMarker8:         SetSingle(VK_8,[ssCtrl]);
  ecGotoMarker9:         SetSingle(VK_9,[ssCtrl]);
  ecToggleMarker0:       SetCombo(VK_0,[ssShift,ssCtrl],VK_UNKNOWN,[], VK_K,[SSCtrl],VK_0,[]);
  ecToggleMarker1:       SetCombo(VK_1,[ssShift,ssCtrl],VK_UNKNOWN,[], VK_K,[SSCtrl],VK_1,[]);
  ecToggleMarker2:       SetCombo(VK_2,[ssShift,ssCtrl],VK_UNKNOWN,[], VK_K,[SSCtrl],VK_2,[]);
  ecToggleMarker3:       SetCombo(VK_3,[ssShift,ssCtrl],VK_UNKNOWN,[], VK_K,[SSCtrl],VK_3,[]);
  ecToggleMarker4:       SetCombo(VK_4,[ssShift,ssCtrl],VK_UNKNOWN,[], VK_K,[SSCtrl],VK_4,[]);
  ecToggleMarker5:       SetCombo(VK_5,[ssShift,ssCtrl],VK_UNKNOWN,[], VK_K,[SSCtrl],VK_5,[]);
  ecToggleMarker6:       SetCombo(VK_6,[ssShift,ssCtrl],VK_UNKNOWN,[], VK_K,[SSCtrl],VK_6,[]);
  ecToggleMarker7:       SetCombo(VK_7,[ssShift,ssCtrl],VK_UNKNOWN,[], VK_K,[SSCtrl],VK_7,[]);
  ecToggleMarker8:       SetCombo(VK_8,[ssShift,ssCtrl],VK_UNKNOWN,[], VK_K,[SSCtrl],VK_8,[]);
  ecToggleMarker9:       SetCombo(VK_9,[ssShift,ssCtrl],VK_UNKNOWN,[], VK_K,[SSCtrl],VK_9,[]);
  ecSetMarker0..ecSetMarker9: SetSingle(VK_UNKNOWN,[]);

  // codetools
  ecAutoCompletion:      SetSingle(VK_J,[ssMeta]);
  ecWordCompletion:      SetSingle(VK_SPACE,[ssCtrl,ssAlt]);
  ecCompleteCode:        SetSingle(VK_C,[ssCtrl,ssShift]);
  ecCompleteCodeInteractive: SetSingle(VK_X,[ssCtrl,ssShift]);
  ecIdentCompletion:     SetSingle(VK_SPACE,[ssCtrl]);
  ecShowCodeContext:     SetSingle(VK_SPACE,[ssCtrl,ssShift]);
  ecExtractProc:         SetSingle(VK_UNKNOWN,[]);
  ecFindIdentifierRefs:  SetSingle(VK_UNKNOWN,[]);
  ecFindUsedUnitRefs:    SetSingle(VK_UNKNOWN,[]);
  ecRenameIdentifier:    SetSingle(VK_E,[ssMeta,ssShift]);
  ecInvertAssignment:    SetSingle(VK_UNKNOWN,[]);
  ecSyntaxCheck:         SetSingle(VK_UNKNOWN,[]);
  ecGuessUnclosedBlock:  SetSingle(VK_UNKNOWN,[]);
  ecGuessMisplacedIFDEF: SetSingle(VK_UNKNOWN,[]);
  ecConvertDFM2LFM:      SetSingle(VK_UNKNOWN,[]);
  ecCheckLFM:            SetSingle(VK_UNKNOWN,[]);
  ecConvertDelphiUnit:   SetSingle(VK_UNKNOWN,[]);
  ecConvertDelphiProject:SetSingle(VK_UNKNOWN,[]);
  ecConvertDelphiPackage:SetSingle(VK_UNKNOWN,[]);
  ecConvertEncoding:     SetSingle(VK_UNKNOWN,[]);
  ecFindProcedureDefinition:SetSingle(VK_UP,[ssShift,SSCtrl]);
  ecFindProcedureMethod: SetSingle(VK_DOWN,[ssShift,SSCtrl]);
  ecFindDeclaration:     SetSingle(VK_UP,[ssAlt]);
  ecFindBlockOtherEnd:   SetSingle(VK_UNKNOWN,[]);
  ecFindBlockStart:      SetSingle(VK_UNKNOWN,[]);
  ecGotoIncludeDirective:SetSingle(VK_UNKNOWN,[]);
  ecShowAbstractMethods: SetSingle(VK_UNKNOWN,[]);
  ecRemoveEmptyMethods:  SetSingle(VK_UNKNOWN,[]);

  // source notebook
  ecNextEditor:          SetSingle(VK_RIGHT,[ssMeta,ssAlt]);
  ecPrevEditor:          SetSingle(VK_LEFT,[ssMeta,ssAlt]);
  ecResetDebugger:       SetSingle(VK_UNKNOWN,[]);
  ecToggleBreakPoint:    SetSingle(VK_P,[ssCtrl]);
  ecToggleBreakPointEnabled:    SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorLeft:      SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorRight:     SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorLeftmost:  SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorRightmost: SetSingle(VK_UNKNOWN,[]);

  ecNextSharedEditor:    SetSingle(VK_UNKNOWN,[]);
  ecPrevSharedEditor:    SetSingle(VK_UNKNOWN,[]);
  ecNextWindow:          SetSingle(VK_UNKNOWN,[]);
  ecPrevWindow:          SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorNextWindow:SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorPrevWindow:SetSingle(VK_UNKNOWN,[]);
  ecMoveEditorNewWindow: SetSingle(VK_UNKNOWN,[]);
  ecCopyEditorNextWindow:SetSingle(VK_UNKNOWN,[]);
  ecCopyEditorPrevWindow:SetSingle(VK_UNKNOWN,[]);
  ecCopyEditorNewWindow: SetSingle(VK_UNKNOWN,[]);

  ecGotoEditor1:         SetSingle(VK_UNKNOWN,[]);
  ecGotoEditor2:         SetSingle(VK_UNKNOWN,[]);
  ecGotoEditor3:         SetSingle(VK_UNKNOWN,[]);
  ecGotoEditor4:         SetSingle(VK_UNKNOWN,[]);
  ecGotoEditor5:         SetSingle(VK_UNKNOWN,[]);
  ecGotoEditor6:         SetSingle(VK_UNKNOWN,[]);
  ecGotoEditor7:         SetSingle(VK_UNKNOWN,[]);
  ecGotoEditor8:         SetSingle(VK_UNKNOWN,[]);
  ecGotoEditor9:         SetSingle(VK_UNKNOWN,[]);
  ecGotoEditor0:         SetSingle(VK_UNKNOWN,[]);

  ecLockEditor:          SetSingle(VK_UNKNOWN,[]);

  (*
  EcFoldLevel1:          SetSingle(VK_1,[ssMeta,ssShift]);
  EcFoldLevel2:          SetSingle(VK_2,[ssMeta,ssShift]);
  EcFoldLevel3:          SetSingle(VK_3,[ssMeta,ssShift]);
  EcFoldLevel4:          SetSingle(VK_4,[ssMeta,ssShift]);
  EcFoldLevel5:          SetSingle(VK_5,[ssMeta,ssShift]);
  EcFoldLevel6:          SetSingle(VK_6,[ssMeta,ssShift]);
  EcFoldLevel7:          SetSingle(VK_7,[ssMeta,ssShift]);
  EcFoldLevel8:          SetSingle(VK_8,[ssMeta,ssShift]);
  EcFoldLevel9:          SetSingle(VK_9,[ssMeta,ssShift]);
  EcFoldLevel0:          SetSingle(VK_0,[ssMeta,ssShift]);
  EcFoldCurrent:         SetSingle(VK_OEM_PLUS,[ssMeta,ssShift]);
  EcUnFoldCurrent:       SetSingle(VK_OEM_MINUS,[ssMeta,ssShift]);
  EcToggleMarkupWord:    SetSingle(VK_M,[ssMeta]);
  *)

  ecGotoBookmarks:       SetSingle(VK_UNKNOWN,[]);
  ecToggleBookmarks:     SetSingle(VK_UNKNOWN,[]);

  // file menu
  ecNew:                 SetSingle(VK_N,[ssMeta]);
  ecNewUnit:             SetSingle(VK_UNKNOWN,[]);
  ecNewForm:             SetSingle(VK_UNKNOWN,[]);
  ecOpen:                SetSingle(VK_O,[ssMeta]);
  ecOpenUnit:            SetSingle(VK_F12,[ssAlt]);
  ecRevert:              SetSingle(VK_UNKNOWN,[]);
  ecSave:                SetSingle(VK_S,[ssMeta]);
  ecSaveAs:              SetSingle(VK_S,[ssMeta,ssShift]);
  ecSaveAll:             SetSingle(VK_S,[ssMeta,ssAlt]);
  ecClose:               SetSingle(VK_W,[ssMeta,ssShift]);
  ecCloseAll:            SetSingle(VK_UNKNOWN,[]);
  ecCloseOtherTabs:      SetSingle(VK_UNKNOWN,[]);
  ecCloseRightTabs:      SetSingle(VK_UNKNOWN,[]);
  ecCleanDirectory:      SetSingle(VK_UNKNOWN,[]);
  ecRestart:             SetSingle(VK_UNKNOWN,[]);
  ecQuit:                SetSingle(VK_UNKNOWN,[]);

  // view menu
  ecToggleObjectInsp:    SetSingle(VK_I,[ssAlt,ssMeta]);
  ecToggleSourceEditor:  SetSingle(VK_UNKNOWN,[]);
  ecToggleCodeExpl:      SetSingle(VK_UNKNOWN,[]);
  ecToggleFPDocEditor:   SetSingle(VK_UNKNOWN,[]);
  ecToggleMessages:      SetSingle(VK_UNKNOWN,[]);
  ecViewComponents:      SetSingle(VK_UNKNOWN,[]);
  ecViewJumpHistory:     SetSingle(VK_UNKNOWN,[]);
  ecToggleSearchResults: SetSingle(VK_F,[ssCtrl,ssAlt]);
  ecToggleWatches:       SetSingle(VK_W,[ssCtrl,ssAlt]);
  ecToggleBreakPoints:   SetSingle(VK_B,[ssCtrl,ssAlt]);
  ecToggleLocals:        SetSingle(VK_L,[ssCtrl,ssAlt],     VK_L,[ssCtrl,ssShift]);
  ecViewPseudoTerminal: if HasConsoleSupport then SetSingle(VK_O,[ssCtrl,ssAlt]);
  ecViewThreads:         SetSingle(VK_T,[ssCtrl,ssAlt]);
  ecToggleCallStack:     SetSingle(VK_S,[ssCtrl,ssAlt]);
  ecToggleRegisters:     SetSingle(VK_R,[ssCtrl,ssAlt]);
  ecToggleAssembler:     SetSingle(VK_D,[ssCtrl,ssAlt]);
  ecToggleDebugEvents:   SetSingle(VK_V,[ssCtrl,ssAlt]);
  ecToggleDebuggerOut:   SetSingle(VK_UNKNOWN,[]);
  ecViewHistory:         SetSingle(VK_H,[ssCtrl,ssAlt]);
  ecViewUnitDependencies:SetSingle(VK_UNKNOWN,[]);
  ecViewUnitInfo:        SetSingle(VK_UNKNOWN,[]);
  ecToggleFormUnit:      SetSingle(VK_F,[ssMeta,ssAlt]);
  ecViewAnchorEditor:    SetSingle(VK_UNKNOWN,[]);
  ecToggleCodeBrowser:   SetSingle(VK_UNKNOWN,[]);
  ecToggleRestrictionBrowser:SetSingle(VK_UNKNOWN,[]);
  ecToggleCompPalette:   SetSingle(VK_UNKNOWN,[]);
  ecToggleIDESpeedBtns:  SetSingle(VK_UNKNOWN,[]);

  // project menu
  ecNewProject:          SetSingle(VK_UNKNOWN,[]);
  ecNewProjectFromFile:  SetSingle(VK_UNKNOWN,[]);
  ecOpenProject:         SetSingle(VK_UNKNOWN,[]);
  ecCloseProject:        SetSingle(VK_UNKNOWN,[]);
  ecSaveProject:         SetSingle(VK_UNKNOWN,[]);
  ecSaveProjectAs:       SetSingle(VK_UNKNOWN,[]);
  ecProjectResaveFormsWithI18n: SetSingle(VK_UNKNOWN,[]);
  ecPublishProject:      SetSingle(VK_UNKNOWN,[]);
  ecProjectInspector:    SetSingle(VK_UNKNOWN,[]);
  ecAddCurUnitToProj:    SetSingle(VK_A,[ssAlt,ssMeta]);
  ecRemoveFromProj:      SetSingle(VK_UNKNOWN,[]);
  ecViewProjectUnits:    SetSingle(VK_U,[ssCtrl,ssAlt]);
  ecViewProjectForms:    SetSingle(VK_U,[ssShift,ssCtrl]);
  ecViewProjectSource:   SetSingle(VK_UNKNOWN,[]);
  ecProjectOptions:      SetSingle(VK_UNKNOWN,[]);
  ecProjectChangeBuildMode:SetSingle(VK_UNKNOWN,[]);

  // run menu
  ecCompile:             SetSingle(VK_B,[ssMeta]);
  ecBuild:               SetSingle(VK_UNKNOWN,[]);
  ecQuickCompile:        SetSingle(VK_UNKNOWN,[]);
  ecCleanUpAndBuild:     SetSingle(VK_UNKNOWN,[]);
  ecBuildManyModes:      SetSingle(VK_UNKNOWN,[]);
  ecAbortBuild:          SetSingle(VK_UNKNOWN,[]);
  ecRunWithoutDebugging: SetSingle(VK_R,[ssMeta, ssCtrl]);
  ecRun:                 SetSingle(VK_R,[ssMeta]);
  ecPause:               SetSingle(VK_UNKNOWN,[]);
  ecShowExecutionPoint:  SetSingle(VK_UNKNOWN,[]);
  ecStepInto:            SetSingle(VK_R,[ssMeta,ssAlt]);
  ecStepOver:            SetSingle(VK_R,[ssMeta,ssShift]);
  ecStepOut:             SetSingle(VK_T,[ssMeta,ssShift]);
  ecRunToCursor:         SetSingle(VK_UNKNOWN,[]);
  ecStopProgram:         SetSingle(VK_RETURN,[ssShift,ssMeta]);
  ecRemoveBreakPoint:    SetSingle(VK_UNKNOWN,[]);
  ecRunParameters:       SetSingle(VK_UNKNOWN,[]);
  ecBuildFile:           SetSingle(VK_UNKNOWN,[]);
  ecRunFile:             SetSingle(VK_UNKNOWN,[]);
  ecConfigBuildFile:     SetSingle(VK_UNKNOWN,[]);
  ecInspect:             SetSingle(VK_UNKNOWN,[]);
  ecEvaluate:            SetSingle(VK_E,[ssCtrl,ssShift]);
  ecAddWatch:            SetSingle(VK_UNKNOWN,[]);
  ecAddBpSource:         SetSingle(VK_UNKNOWN,[]);
  ecAddBpAddress:        SetSingle(VK_UNKNOWN,[]);
  ecAddBpDataWatch:      SetSingle(VK_UNKNOWN,[]);

  // components menu
  ecNewPackage:          SetSingle(VK_UNKNOWN,[]);
  ecOpenPackage:         SetSingle(VK_UNKNOWN,[]);
  ecOpenPackageFile:     SetSingle(VK_UNKNOWN,[]);
  ecOpenPackageOfCurUnit:SetSingle(VK_UNKNOWN,[]);
  ecAddCurFileToPkg:     SetSingle(VK_UNKNOWN,[]);
  ecNewPkgComponent:     SetSingle(VK_UNKNOWN,[]);
  ecPackageGraph:        SetSingle(VK_UNKNOWN,[]);
  ecPackageLinks:        SetSingle(VK_UNKNOWN,[]);
  ecEditInstallPkgs:     SetSingle(VK_UNKNOWN,[]);
  ecConfigCustomComps:   SetSingle(VK_UNKNOWN,[]);

  // tools menu
  ecEnvironmentOptions:  SetSingle(VK_LCL_COMMA,[ssMeta]); // Cmd-semicolon
  ecRescanFPCSrcDir:     SetSingle(VK_UNKNOWN,[]);
  ecEditCodeTemplates:   SetSingle(VK_UNKNOWN,[]);
  ecCodeToolsDefinesEd:  SetSingle(VK_UNKNOWN,[]);

  ecExtToolSettings:     SetSingle(VK_UNKNOWN,[]);
  ecManageExamples:      SetSingle(VK_UNKNOWN,[]);
  ecBuildLazarus:        SetSingle(VK_UNKNOWN,[]);
  ecConfigBuildLazarus:  SetSingle(VK_UNKNOWN,[]);
  ecMakeResourceString:  SetSingle(VK_UNKNOWN,[]);
  ecDiff:                SetSingle(VK_UNKNOWN,[]);

  // window menu
  ecManageSourceEditors:       SetSingle(VK_W,[ssShift,ssCtrl]);

  // help menu
  ecAboutLazarus:        SetSingle(VK_UNKNOWN,[]);
  ecOnlineHelp:          SetSingle(VK_UNKNOWN,[]);
  ecContextHelp:         SetSingle(VK_F1,[],VK_HELP,[]);
  ecEditContextHelp:     SetSingle(VK_F1,[ssShift,ssCtrl], VK_HELP,[ssCtrl]);
  ecReportingBug:        SetSingle(VK_UNKNOWN,[]);
  ecFocusHint:           SetSingle(VK_UNKNOWN,[]);
  ecSmartHint:           SetSingle(VK_UNKNOWN,[]);

  // designer
  ecDesignerCopy:        SetSingle(VK_C,[ssMeta]);
  ecDesignerCut:         SetSingle(VK_X,[ssMeta]);
  ecDesignerPaste:       SetSingle(VK_V,[ssMeta]);
  ecDesignerSelectParent:SetSingle(VK_ESCAPE,[]);
  ecDesignerMoveToFront: SetSingle(VK_PRIOR,[ssShift]);
  ecDesignerMoveToBack:  SetSingle(VK_NEXT,[ssShift]);
  ecDesignerForwardOne:  SetSingle(VK_PRIOR,[ssMeta]);
  ecDesignerBackOne:     SetSingle(VK_NEXT,[ssMeta]);
  ecDesignerToggleNonVisComps: SetSingle(VK_UNKNOWN,[]);

  // macro
  ecSynMacroRecord:      SetSingle(VK_R,[ssShift, ssCtrl]);
  ecSynMacroPlay:        SetSingle(VK_P,[ssShift, ssCtrl]);

  // Edit template
  ecIdePTmplEdNextCell:         SetSingle(VK_RIGHT,[ssCtrl]);
  ecIdePTmplEdNextCellSel:      SetSingle(VK_TAB,[]);
  ecIdePTmplEdNextCellRotate:   SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdNextCellSelRotate:SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdPrevCell:         SetSingle(VK_LEFT,[ssCtrl]);
  ecIdePTmplEdPrevCellSel:      SetSingle(VK_TAB,[ssShift]);
  ecIdePTmplEdCellHome:         SetSingle(VK_HOME,[]);
  ecIdePTmplEdCellEnd:          SetSingle(VK_END,[]);
  ecIdePTmplEdCellSelect:       SetSingle(VK_A,[ssCtrl]);
  ecIdePTmplEdFinish:           SetSingle(VK_RETURN,[]);
  ecIdePTmplEdEscape:           SetSingle(VK_ESCAPE,[]);
  // Edit template
  ecIdePTmplEdOutNextCell:         SetSingle(VK_RIGHT,[ssCtrl]);
  ecIdePTmplEdOutNextCellSel:      SetSingle(VK_TAB,[]);
  ecIdePTmplEdOutNextCellRotate:   SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutNextCellSelRotate:SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutPrevCell:         SetSingle(VK_LEFT,[ssCtrl]);
  ecIdePTmplEdOutPrevCellSel:      SetSingle(VK_TAB,[ssShift]);
  ecIdePTmplEdOutCellHome:         SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutCellEnd:          SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutCellSelect:       SetSingle(VK_UNKNOWN,[]);
  ecIdePTmplEdOutFinish:           SetSingle(VK_RETURN,[]);
  ecIdePTmplEdOutEscape:           SetSingle(VK_ESCAPE,[]);
  // SyncroEdit
  ecIdePSyncroEdNextCell:       SetSingle(VK_RIGHT,[ssCtrl]);
  ecIdePSyncroEdNextCellSel:    SetSingle(VK_TAB,[]);
  ecIdePSyncroEdPrevCell:       SetSingle(VK_LEFT,[ssCtrl]);
  ecIdePSyncroEdPrevCellSel:    SetSingle(VK_TAB,[ssShift]);
  ecIdePSyncroEdCellHome:       SetSingle(VK_HOME,[]);
  ecIdePSyncroEdCellEnd:        SetSingle(VK_END,[]);
  ecIdePSyncroEdCellSelect:     SetSingle(VK_A,[ssCtrl]);
  ecIdePSyncroEdEscape:         SetSingle(VK_ESCAPE,[]);
  // SyncroEdit
  ecIdePSyncroEdOutNextCell:       SetSingle(VK_RIGHT,[ssCtrl]);
  ecIdePSyncroEdOutNextCellSel:    SetSingle(VK_TAB,[]);
  ecIdePSyncroEdOutPrevCell:       SetSingle(VK_LEFT,[ssCtrl]);
  ecIdePSyncroEdOutPrevCellSel:    SetSingle(VK_TAB,[ssShift]);
  ecIdePSyncroEdOutCellHome:       SetSingle(VK_UNKNOWN,[]);
  ecIdePSyncroEdOutCellEnd:        SetSingle(VK_UNKNOWN,[]);
  ecIdePSyncroEdOutCellSelect:     SetSingle(VK_UNKNOWN,[]);
  ecIdePSyncroEdOutEscape:         SetSingle(VK_ESCAPE,[]);
  // SyncroEdit, during selection
  ecIdePSyncroEdSelStart:          SetSingle(VK_J,[ssCtrl]);

  else
    begin
      SetSingle(VK_UNKNOWN,[]);
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
  ecLeft:                SetSingle(VK_LEFT,[]);
  ecRight:               SetSingle(VK_RIGHT,[]);
  ecUp:                  SetSingle(VK_UP,[]);
  ecDown:                SetSingle(VK_DOWN,[]);
  ecLineStart:           SetSingle(VK_HOME,[],        VK_LEFT,[ssMeta]);
  ecLineEnd:             SetSingle(VK_END,[],         VK_RIGHT,[ssMeta]);
  ecEditorTop:           SetSingle(VK_UP,[ssMeta]);
  ecEditorBottom:        SetSingle(VK_DOWN,[ssMeta]);

  // selection
  ecSelLeft:             SetSingle(VK_LEFT,[ssShift]);
  ecSelRight:            SetSingle(VK_RIGHT,[ssShift]);
  ecSelUp:               SetSingle(VK_UP,[ssShift]);
  ecSelDown:             SetSingle(VK_DOWN,[ssShift]);
  ecSelLineStart:        SetSingle(VK_HOME,[ssShift], VK_LEFT,[ssMeta,ssShift]);
  ecSelLineEnd:          SetSingle(VK_END,[ssShift],  VK_RIGHT,[ssMeta,ssShift]);
  ecSelEditorTop:        SetSingle(VK_HOME,[ssShift,ssCtrl]);
  ecSelEditorBottom:     SetSingle(VK_END,[ssShift,ssCtrl]);

  // codetools
  ecRenameIdentifier:    SetSingle(VK_E,[ssShift,ssCtrl]);

  // run menu
  ecCompile:             SetSingle(VK_F9,[ssCtrl],    VK_F9,[ssCtrl,ssMeta]);
  ecBuild:               SetSingle(VK_F9,[ssShift]);
  ecQuickCompile:        SetSingle(VK_UNKNOWN,[]);
  ecCleanUpAndBuild:     SetSingle(VK_UNKNOWN,[]);
  ecBuildManyModes:      SetSingle(VK_UNKNOWN,[]);
  ecAbortBuild:          SetSingle(VK_UNKNOWN,[]);
  ecRun:                 SetSingle(VK_F9,[],          VK_F9,[ssMeta]);
  ecPause:               SetSingle(VK_UNKNOWN,[]);
  ecShowExecutionPoint:  SetSingle(VK_UNKNOWN,[]);
  ecStepInto:            SetSingle(VK_F7,[],          VK_F7,[ssMeta]);
  ecStepOver:            SetSingle(VK_F8,[],          VK_F8,[ssMeta]);
  ecStepOut:             SetSingle(VK_F8,[ssShift],   VK_F8,[ssShift,ssMeta]);
  ecRunToCursor:         SetSingle(VK_F4,[],          VK_F4,[ssMeta]);
  ecStopProgram:         SetSingle(VK_F2,[ssCtrl],    VK_F2,[ssCtrl,ssMeta]);
  ecRemoveBreakPoint:    SetSingle(VK_UNKNOWN,[]);
  ecRunParameters:       SetSingle(VK_UNKNOWN,[]);
  ecBuildFile:           SetSingle(VK_UNKNOWN,[]);
  ecRunFile:             SetSingle(VK_UNKNOWN,[]);
  ecConfigBuildFile:     SetSingle(VK_UNKNOWN,[]);
  ecInspect:             SetSingle(VK_F5,[ssAlt]);
  ecEvaluate:            SetSingle(VK_F7,[ssCtrl],  VK_F7,[ssCtrl,ssMeta]);
  ecAddWatch:            SetSingle(VK_F5,[ssCtrl],  VK_F5,[ssCtrl,ssMeta]);
  ecAddBpSource:         SetSingle(VK_UNKNOWN,[]);
  ecAddBpAddress:        SetSingle(VK_UNKNOWN,[]);
  ecAddBpDataWatch:      SetSingle(VK_F5,[ssShift]);
  end;
end;

procedure TKeyCommandRelation.Init;
begin
  inherited;
  FSkipSaving := False;
end;

{ TKeyCommandRelationList }

constructor TKeyCommandRelationList.Create;
begin
  inherited Create;
  FRelations:=TFPList.Create;
  fCategories:=TFPList.Create;
  fExtToolCount:=0;
  fLoadedKeyCommands:=TAvlTree.Create(@CompareLoadedKeyCommands);
  fCmdRelCache:=TAvlTree.Create(@CompareCmdRels);
end;

destructor TKeyCommandRelationList.Destroy;
begin
  Clear;
  FRelations.Free;
  fCategories.Free;
  fCmdRelCache.Free;
  fLoadedKeyCommands.Free;
  inherited Destroy;
end;

procedure TKeyCommandRelationList.DefineCommandCategories;
// Define a category for each command

  function n(const s: string): string;
  begin
    Result:=StringReplace(s,'&','',[]);
  end;

var
  C: TIDECommandCategory;
begin
  Clear;
  // moving
  C:=Categories[AddCategory('CursorMoving',srkmCatCursorMoving,IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Move cursor left', srkmecLeft, ecLeft);
  AddDefault(C, 'Move cursor right', srkmecRight, ecRight);
  AddDefault(C, 'Move cursor up', srkmecUp, ecUp);
  AddDefault(C, 'Move cursor down', srkmecDown, ecDown);
  AddDefault(C, 'Move cursor word left', srkmecWordLeft, ecWordLeft);
  AddDefault(C, 'Move cursor word right', srkmecWordRight, ecWordRight);
  AddDefault(C, 'Move cursor word end left', srkmecWordEndLeft, ecWordEndLeft);
  AddDefault(C, 'Move cursor word end right', srkmecWordEndRight, ecWordEndRight);
  AddDefault(C, 'Move cursor half word left', srkmecHalfWordLeft, ecHalfWordLeft);
  AddDefault(C, 'Move cursor half word right', srkmecHalfWordRight, ecHalfWordRight);
  AddDefault(C, 'Smart move cursor word left', srkmecSmartWordLeft, ecSmartWordLeft);
  AddDefault(C, 'Smart move cursor word right', srkmecSmartWordRight, ecSmartWordRight);
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
  AddDefault(C, 'Select cursor left', srkmecSelLeft, ecSelLeft);
  AddDefault(C, 'Select cursor right', srkmecSelRight, ecSelRight);
  AddDefault(C, 'Select cursor up', srkmecSelUp, ecSelUp);
  AddDefault(C, 'Select cursor down', srkmecSelDown, ecSelDown);

  AddDefault(C, 'Copy selection to clipboard', srkmecCopy, ecCopy);
  AddDefault(C, 'Cut selection to clipboard', srkmecCut, ecCut);
  AddDefault(C, 'Paste clipboard to current position', srkmecPaste, ecPaste);
  AddDefault(C, 'Multi paste clipboard to current position', srkmecMultiPaste, ecMultiPaste);
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
  AddDefault(C, 'Select word end left', srkmecSelWordEndLeft, ecSelWordEndLeft);
  AddDefault(C, 'Select word end right', srkmecSelWordEndRight, ecSelWordEndRight);
  AddDefault(C, 'Select half word left', srkmecSelHalfWordLeft, ecSelHalfWordLeft);
  AddDefault(C, 'Select half word right', srkmecSelHalfWordRight, ecSelHalfWordRight);
  AddDefault(C, 'Smart select word left', srkmecSelSmartWordLeft, ecSelSmartWordLeft);
  AddDefault(C, 'Smart select word right', srkmecSelSmartWordRight, ecSelSmartWordRight);
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
  AddDefault(C, 'Start sticky selecting', srkmecSelSticky, ecStickySelection);
  AddDefault(C, 'Start sticky selecting (Columns)', srkmecSelStickyCol, ecStickySelectionCol);
  AddDefault(C, 'Start sticky selecting (Line)', srkmecSelStickyLine, ecStickySelectionLine);
  AddDefault(C, 'Stop sticky selecting', srkmecSelStickyStop, ecStickySelectionStop);

  // Persistent Block
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

  // multi caret
  C:=Categories[AddCategory('MultiCaret', srkmCatMultiCaret, IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Add extra caret', srkmecPluginMultiCaretSetCaret, ecPluginMultiCaretSetCaret);
  AddDefault(C, 'Remove extra caret', srkmecPluginMultiCaretUnsetCaret, ecPluginMultiCaretUnsetCaret);
  AddDefault(C, 'Toggle extra caret', srkmecPluginMultiCaretToggleCaret, ecPluginMultiCaretToggleCaret);
  AddDefault(C, 'Cursor keys clear all extra carets', srkmecPluginMultiCaretModeCancelOnMove, ecPluginMultiCaretModeCancelOnMove);
  AddDefault(C, 'Cursor keys move all extra carets', srkmecPluginMultiCaretModeMoveAll, ecPluginMultiCaretModeMoveAll);
  C:=Categories[AddCategory('MultiCaret', srkmCatMultiCaret, IDECmdScopeSrcEditOnlyMultiCaret)];
  AddDefault(C, 'Clear all extra carets', srkmecPluginMultiCaretClearAll, ecPluginMultiCaretClearAll);

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
  AddDefault(C, 'Insert GPL notice translated', srkmecInsertGPLNoticeTranslated, ecInsertGPLNoticeTranslated);
  AddDefault(C, 'Insert LGPL notice', srkmecInsertLGPLNotice, ecInsertLGPLNotice);
  AddDefault(C, 'Insert LGPL notice translated', srkmecInsertLGPLNoticeTranlated, ecInsertLGPLNoticeTranslated);
  AddDefault(C, 'Insert modified LGPL notice', srkmecInsertModifiedLGPLNotice, ecInsertModifiedLGPLNotice);
  AddDefault(C, 'Insert modified LGPL notice translated', srkmecInsertModifiedLGPLNoticeTranslated, ecInsertModifiedLGPLNoticeTranslated);
  AddDefault(C, 'Insert MIT notice', srkmecInsertMITNotice, ecInsertMITNotice);
  AddDefault(C, 'Insert MIT notice translated', srkmecInsertMITNoticeTranslated, ecInsertMITNoticeTranslated);
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
  AddDefault(C, 'Undo', lisUndo, ecUndo);
  AddDefault(C, 'Redo', lisRedo, ecRedo);

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
  AddDefault(C, 'Clear Bookmarks for current file', srkmecClearBookmarkForFile, ecClearBookmarkForFile);
  AddDefault(C, 'Clear all Bookmarks', srkmecClearAllBookmark, ecClearAllBookmark);
  AddDefault(C, 'Previous Bookmark', srkmecPrevBookmark, ecPrevBookmark);
  AddDefault(C, 'Next Bookmark', srkmecNextBookmark, ecNextBookmark);
  AddDefault(C, 'Go to Bookmark...', uemGotoBookmarks, ecGotoBookmarks);
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
  AddDefault(C, 'Toggle Bookmark...', uemToggleBookmarks, ecToggleBookmarks);
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
  AddDefault(C, 'Complete code', lisMenuCompleteCode, ecCompleteCode);
  AddDefault(C, 'Complete code (with dialog)', lisMenuCompleteCodeInteractive, ecCompleteCodeInteractive);
  AddDefault(C, 'Identifier completion', dlgEdIdComlet, ecIdentCompletion);
  AddDefault(C, 'Rename identifier', srkmecRenameIdentifier, ecRenameIdentifier);
  AddDefault(C, 'Find identifier references', srkmecFindIdentifierRefs, ecFindIdentifierRefs);
  AddDefault(C, 'Find references of used unit', lisMenuFindReferencesOfUsedUnit, ecFindUsedUnitRefs);
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
  AddDefault(C, 'Jump to Interface', lisMenuJumpToInterface, ecJumpToInterface);
  AddDefault(C, 'Jump to Interface uses', lisMenuJumpToInterfaceUses, ecJumpToInterfaceUses);
  AddDefault(C, 'Jump to Implementation', lisMenuJumpToImplementation, ecJumpToImplementation);
  AddDefault(C, 'Jump to Implementation uses', lisMenuJumpToImplementationUses, ecJumpToImplementationUses);
  AddDefault(C, 'Jump to Initialization', lisMenuJumpToInitialization, ecJumpToInitialization);
  AddDefault(C, 'Jump to Procedure header', lisMenuJumpToProcedureHeader, ecJumpToProcedureHeader);
  AddDefault(C, 'Jump to Procedure begin', lisMenuJumpToProcedureBegin, ecJumpToProcedureBegin);
  AddDefault(C, 'Show abstract methods', srkmecShowAbstractMethods, ecShowAbstractMethods);
  AddDefault(C, 'Remove empty methods', srkmecRemoveEmptyMethods, ecRemoveEmptyMethods);
  AddDefault(C, 'Remove unused units', srkmecRemoveUnusedUnits, ecRemoveUnusedUnits);
  AddDefault(C, 'Add unit to uses section', lisUseUnit, ecUseUnit);
  AddDefault(C, 'Find overloads', srkmecFindOverloads, ecFindOverloads);
  AddDefault(C, 'Make resource string', srkmecMakeResourceString, ecMakeResourceString);

  // Macro editing
  C:=Categories[AddCategory('MacroRecording', srkmCatMacroRecording, IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Record Macro', srkmecSynMacroRecord, ecSynMacroRecord);
  AddDefault(C, 'Play Macro', srkmecSynMacroPlay, ecSynMacroPlay);
  AddDefault(C, 'View Editor Macros', srkmecViewEditorMacros, ecViewMacroList);

  // Template editing
  C:=Categories[AddCategory('Edit Template', srkmCatTemplateEdit, IDECmdScopeSrcEditOnlyTmplEdit)];
  AddDefault(C, 'Edit Template Next Cell', srkmecSynPTmplEdNextCell, ecIdePTmplEdNextCell);
  AddDefault(C, 'Edit Template Next Cell (all selected)', srkmecSynPTmplEdNextCellSel, ecIdePTmplEdNextCellSel);
  AddDefault(C, 'Edit Template Next Cell (rotate)', srkmecSynPTmplEdNextCellRotate, ecIdePTmplEdNextCellRotate);
  AddDefault(C, 'Edit Template Next Cell (rotate / all selected)', srkmecSynPTmplEdNextCellSelRotate, ecIdePTmplEdNextCellSelRotate);
  AddDefault(C, 'Edit Template Previous Cell', srkmecSynPTmplEdPrevCell, ecIdePTmplEdPrevCell);
  AddDefault(C, 'Edit Template Previous Cell (all selected)', srkmecSynPTmplEdPrevCellSel, ecIdePTmplEdPrevCellSel);
  AddDefault(C, 'Edit Template Next First Cell', srkmecSynPTmplEdNextFirstCell, ecIdePTmplEdNextFirstCell);
  AddDefault(C, 'Edit Template Next First Cell (all selected)', srkmecSynPTmplEdNextFirstCellSel, ecIdePTmplEdNextFirstCellSel);
  AddDefault(C, 'Edit Template Next First Cell (rotate)', srkmecSynPTmplEdNextFirstCellRotate, ecIdePTmplEdNextFirstCellRotate);
  AddDefault(C, 'Edit Template Next First Cell (rotate / all selected)', srkmecSynPTmplEdNextFirstCellSelRotate, ecIdePTmplEdNextFirstCellSelRotate);
  AddDefault(C, 'Edit Template Previous First Cell', srkmecSynPTmplEdPrevFirstCell, ecIdePTmplEdPrevFirstCell);
  AddDefault(C, 'Edit Template Previous First Cell (all selected)', srkmecSynPTmplEdPrevFirstCellSel, ecIdePTmplEdPrevFirstCellSel);
  AddDefault(C, 'Edit Template Goto first pos in cell', srkmecSynPTmplEdCellHome, ecIdePTmplEdCellHome);
  AddDefault(C, 'Edit Template Goto last pos in cell', srkmecSynPTmplEdCellEnd, ecIdePTmplEdCellEnd);
  AddDefault(C, 'Edit Template Select cell', srkmecSynPTmplEdCellSelect, ecIdePTmplEdCellSelect);
  AddDefault(C, 'Edit Template Finish', srkmecSynPTmplEdFinish, ecIdePTmplEdFinish);
  AddDefault(C, 'Edit Template Escape', srkmecSynPTmplEdEscape, ecIdePTmplEdEscape);

  // Template editing not in cell
  C:=Categories[AddCategory('Edit Template Off', srkmCatTemplateEditOff, IDECmdScopeSrcEditOnlyTmplEditOff)];
  AddDefault(C, 'Edit Template (off) Next Cell', srkmecSynPTmplEdNextCell, ecIdePTmplEdOutNextCell);
  AddDefault(C, 'Edit Template (off) Next Cell (all selected)', srkmecSynPTmplEdNextCellSel, ecIdePTmplEdOutNextCellSel);
  AddDefault(C, 'Edit Template (off) Next Cell (rotate)', srkmecSynPTmplEdNextCellRotate, ecIdePTmplEdOutNextCellRotate);
  AddDefault(C, 'Edit Template (off) Next Cell (rotate / all selected)', srkmecSynPTmplEdNextCellSelRotate, ecIdePTmplEdOutNextCellSelRotate);
  AddDefault(C, 'Edit Template (off) Previous Cell', srkmecSynPTmplEdPrevCell, ecIdePTmplEdOutPrevCell);
  AddDefault(C, 'Edit Template (off) Previous Cell (all selected)', srkmecSynPTmplEdPrevCellSel, ecIdePTmplEdOutPrevCellSel);
  AddDefault(C, 'Edit Template (off) Next First Cell', srkmecSynPTmplEdNextFirstCell, ecIdePTmplEdOutNextFirstCell);
  AddDefault(C, 'Edit Template (off) Next First Cell (all selected)', srkmecSynPTmplEdNextFirstCellSel, ecIdePTmplEdOutNextFirstCellSel);
  AddDefault(C, 'Edit Template (off) Next First Cell (rotate)', srkmecSynPTmplEdNextFirstCellRotate, ecIdePTmplEdOutNextFirstCellRotate);
  AddDefault(C, 'Edit Template (off) Next First Cell (rotate / all selected)', srkmecSynPTmplEdNextFirstCellSelRotate, ecIdePTmplEdOutNextFirstCellSelRotate);
  AddDefault(C, 'Edit Template (off) Previous First Cell', srkmecSynPTmplEdPrevFirstCell, ecIdePTmplEdOutPrevFirstCell);
  AddDefault(C, 'Edit Template (off) Previous First Cell (all selected)', srkmecSynPTmplEdPrevFirstCellSel, ecIdePTmplEdOutPrevFirstCellSel);
  AddDefault(C, 'Edit Template (off) Goto first pos in cell', srkmecSynPTmplEdCellHome, ecIdePTmplEdOutCellHome);
  AddDefault(C, 'Edit Template (off) Goto last pos in cell', srkmecSynPTmplEdCellEnd, ecIdePTmplEdOutCellEnd);
  AddDefault(C, 'Edit Template (off) Select cell', srkmecSynPTmplEdCellSelect, ecIdePTmplEdOutCellSelect);
  AddDefault(C, 'Edit Template (off) Finish', srkmecSynPTmplEdFinish, ecIdePTmplEdOutFinish);
  AddDefault(C, 'Edit Template (off) Escape', srkmecSynPTmplEdEscape, ecIdePTmplEdOutEscape);

  // Syncro editing
  C:=Categories[AddCategory('Syncro Edit', srkmCatSyncroEdit, IDECmdScopeSrcEditOnlySyncroEdit)];
  AddDefault(C, 'Edit Syncro Next Cell', srkmecSynPSyncroEdNextCell, ecIdePSyncroEdNextCell);
  AddDefault(C, 'Edit Syncro Next Cell (all selected)', srkmecSynPSyncroEdNextCellSel, ecIdePSyncroEdNextCellSel);
  AddDefault(C, 'Edit Syncro Previous Cell', srkmecSynPSyncroEdPrevCell, ecIdePSyncroEdPrevCell);
  AddDefault(C, 'Edit Syncro Previous Cell (all selected)', srkmecSynPSyncroEdPrevCellSel, ecIdePSyncroEdPrevCellSel);
  AddDefault(C, 'Edit Syncro Next First Cell', srkmecSynPSyncroEdNextFirstCell, ecIdePSyncroEdNextFirstCell);
  AddDefault(C, 'Edit Syncro Next First Cell (all selected)', srkmecSynPSyncroEdNextFirstCellSel, ecIdePSyncroEdNextFirstCellSel);
  AddDefault(C, 'Edit Syncro First Previous Cell', srkmecSynPSyncroEdPrevFirstCell, ecIdePSyncroEdPrevFirstCell);
  AddDefault(C, 'Edit Syncro First Previous Cell (all selected)', srkmecSynPSyncroEdPrevFirstCellSel, ecIdePSyncroEdPrevFirstCellSel);
  AddDefault(C, 'Edit Syncro Goto first pos in cell', srkmecSynPSyncroEdCellHome, ecIdePSyncroEdCellHome);
  AddDefault(C, 'Edit Syncro Goto last pos in cell', srkmecSynPSyncroEdCellEnd, ecIdePSyncroEdCellEnd);
  AddDefault(C, 'Edit Syncro Select cell', srkmecSynPSyncroEdCellSelect, ecIdePSyncroEdCellSelect);
  AddDefault(C, 'Edit Syncro Escape', srkmecSynPSyncroEdEscape, ecIdePSyncroEdEscape);

  // Syncro editing not in cell
  C:=Categories[AddCategory('Syncro Edit Off', srkmCatSyncroEditOff, IDECmdScopeSrcEditOnlySyncroEditOff)];
  AddDefault(C, 'Edit Syncro (off) Next Cell', srkmecSynPSyncroEdNextCell, ecIdePSyncroEdOutNextCell);
  AddDefault(C, 'Edit Syncro (off) Next Cell (all selected)', srkmecSynPSyncroEdNextCellSel, ecIdePSyncroEdOutNextCellSel);
  AddDefault(C, 'Edit Syncro (off) Previous Cell', srkmecSynPSyncroEdPrevCell, ecIdePSyncroEdOutPrevCell);
  AddDefault(C, 'Edit Syncro (off) Previous Cell (all selected)', srkmecSynPSyncroEdPrevCellSel, ecIdePSyncroEdOutPrevCellSel);
  AddDefault(C, 'Edit Syncro (off) Next First Cell', srkmecSynPSyncroEdNextFirstCell, ecIdePSyncroEdOutNextFirstCell);
  AddDefault(C, 'Edit Syncro (off) Next First Cell (all selected)', srkmecSynPSyncroEdNextFirstCellSel, ecIdePSyncroEdOutNextFirstCellSel);
  AddDefault(C, 'Edit Syncro (off) Previous First Cell', srkmecSynPSyncroEdPrevFirstCell, ecIdePSyncroEdOutPrevFirstCell);
  AddDefault(C, 'Edit Syncro (off) Previous First Cell (all selected)', srkmecSynPSyncroEdPrevFirstCellSel, ecIdePSyncroEdOutPrevFirstCellSel);
  AddDefault(C, 'Edit Syncro (off) Goto first pos in cell', srkmecSynPSyncroEdCellHome, ecIdePSyncroEdOutCellHome);
  AddDefault(C, 'Edit Syncro (off) Goto last pos in cell', srkmecSynPSyncroEdCellEnd, ecIdePSyncroEdOutCellEnd);
  AddDefault(C, 'Edit Syncro (off) Select cell', srkmecSynPSyncroEdCellSelect, ecIdePSyncroEdOutCellSelect);
  AddDefault(C, 'Edit Syncro (off) Escape', srkmecSynPSyncroEdEscape, ecIdePSyncroEdOutEscape);

  // Syncro editing still selecting
  C:=Categories[AddCategory('Syncro Edit Sel', srkmCatSyncroEditSel, IDECmdScopeSrcEditOnlySyncroEditSel)];
  AddDefault(C, 'Edit Syncro (sel) Start', srkmecSynPSyncroEdStart, ecIdePSyncroEdSelStart);

  // source notebook - without menu items in the IDE bar
  C:=Categories[AddCategory('SourceNotebook',srkmCatSrcNoteBook,IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Go to next editor', srkmecNextEditor, ecNextEditor);
  AddDefault(C, 'Go to prior editor', srkmecPrevEditor, ecPrevEditor);
  AddDefault(C, 'Go to previous editor in history', srkmecPrevEditorInHistory, ecPrevEditorInHistory);
  AddDefault(C, 'Go to next editor in history', srkmecNextEditorInHistory, ecNextEditorInHistory);
  AddDefault(C, 'Add break point', srkmecToggleBreakPoint, ecToggleBreakPoint);
  AddDefault(C, 'Enable/Disable break point', srkmecToggleBreakPointEnabled, ecToggleBreakPointEnabled);
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

  AddDefault(C, 'Zoom Reset', dlfMouseSimpleButtonZoomReset, ecZoomNorm);
  AddDefault(C, 'Zoom In', srkmecZoomIn, ecZoomIn);
  AddDefault(C, 'Zoom Out', srkmecZoomOut, ecZoomOut);

  // file menu
  C:=Categories[AddCategory('FileMenu',srkmCatFileMenu,nil)];
  AddDefault(C, 'New', lisNew, ecNew);
  AddDefault(C, 'NewUnit', lisKMNewUnit, ecNewUnit);
  AddDefault(C, 'NewForm', lisMenuNewForm, ecNewForm);
  AddDefault(C, 'Open', lisOpen, ecOpen);
  AddDefault(C, 'OpenUnit', lisOpenUnit, ecOpenUnit);
  AddDefault(C, 'Revert', lisMenuRevert, ecRevert);
  AddDefault(C, 'Save', lisSave, ecSave);
  AddDefault(C, 'SaveAs', lisKMSaveAs, ecSaveAs);
  AddDefault(C, 'SaveAll', lisKMSaveAll, ecSaveAll);
  AddDefault(C, 'Close', lisClose, ecClose);
  AddDefault(C, 'CloseAll', lisCloseAll, ecCloseAll);
  AddDefault(C, 'CloseAllOther', uemCloseOtherPagesPlain, ecCloseOtherTabs);
  AddDefault(C, 'CloseAllRight', uemCloseOtherPagesRightPlain, ecCloseRightTabs);
  AddDefault(C, 'Clean Directory', lisClDirCleanDirectory, ecCleanDirectory);
  AddDefault(C, 'Restart', lisRestart, ecRestart);
  AddDefault(C, 'Quit', lisQuit, ecQuit);

  // view menu
  C:=Categories[AddCategory(CommandCategoryViewName,srkmCatViewMenu,nil)];
  AddDefault(C, 'Toggle view Object Inspector', lisKMToggleViewObjectInspector, ecToggleObjectInsp);
  AddDefault(C, 'Toggle view Source Editor', lisKMToggleViewSourceEditor, ecToggleSourceEditor);
  AddDefault(C, 'Toggle view Code Explorer', lisKMToggleViewCodeExplorer, ecToggleCodeExpl);
  AddDefault(C, 'Toggle view Code Browser', lisKMToggleViewCodeBrowser, ecToggleCodeBrowser);
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
  AddDefault(C, 'Resave forms with i18n', lisMenuResaveFormsWithI18n,
    ecProjectResaveFormsWithI18n);
  AddDefault(C, 'Publish project', lisKMPublishProject, ecPublishProject);
  AddDefault(C, 'Project Inspector', lisMenuProjectInspector, ecProjectInspector);
  AddDefault(C, 'Add editor file to Project', lisMenuAddToProject, ecAddCurUnitToProj);
  AddDefault(C, 'Remove active unit from project', lisKMRemoveActiveFileFromProject, ecRemoveFromProj);
  AddDefault(C, 'View Units', lisHintViewUnits, ecViewProjectUnits);
  AddDefault(C, 'View Forms', lisHintViewForms, ecViewProjectForms);
  AddDefault(C, 'View project source', lisKMViewProjectSource, ecViewProjectSource);
  AddDefault(C, 'View project options', lisKMViewProjectOptions, ecProjectOptions);
  AddDefault(C, 'Change build mode', lisChangeBuildMode, ecProjectChangeBuildMode);

  // run menu
  C:=Categories[AddCategory('RunMenu',srkmCatRunMenu,nil)];
  AddDefault(C, 'Compile project/program', lisKMCompileProjectProgram, ecCompile);
  AddDefault(C, 'Build project/program', lisKMBuildProjectProgram, ecBuild);
  AddDefault(C, 'Quick compile, no linking', lisKMQuickCompileNoLinking, ecQuickCompile);
  AddDefault(C, 'Clean up and build', lisKMCleanUpAndBuild, ecCleanUpAndBuild);
  AddDefault(C, 'Build many modes', lisKMBuildManyModes, ecBuildManyModes);
  AddDefault(C, 'Abort building', lisKMAbortBuilding, ecAbortBuild);
  AddDefault(C, 'Run without debugging', lisMenuRunWithoutDebugging, ecRunWithoutDebugging);
  AddDefault(C, 'Run program', lisKMRunProgram, ecRun);
  AddDefault(C, 'Pause program', lisKMPauseProgram, ecPause);
  AddDefault(C, 'Show execution point', n(lisMenuShowExecutionPoint), ecShowExecutionPoint);
  AddDefault(C, 'Step into', n(lisMenuStepInto), ecStepInto);
  AddDefault(C, 'Step over', n(lisMenuStepOver), ecStepOver);
  AddDefault(C, 'Step into instr', lisMenuStepIntoInstr, ecStepIntoInstr);
  AddDefault(C, 'Step over instr', lisMenuStepOverInstr, ecStepOverInstr);
  AddDefault(C, 'Step into context', lisMenuStepIntoContext, ecStepIntoContext);
  AddDefault(C, 'Step over context', lisMenuStepOverContext, ecStepOverContext);
  AddDefault(C, 'Step out', n(lisMenuStepOut), ecStepOut);
  AddDefault(C, 'Run to cursor', n(lisMenuRunToCursor), ecRunToCursor);
  AddDefault(C, 'Stop program', lisKMStopProgram, ecStopProgram);
  AddDefault(C, 'Reset debugger', lisMenuResetDebugger, ecResetDebugger);
  AddDefault(C, 'Run parameters', dlgRunParameters, ecRunParameters);
  AddDefault(C, 'Attach to program', srkmecAttach, ecAttach);
  AddDefault(C, 'Detach from program', srkmecDetach, ecDetach);
  AddDefault(C, 'Build File', lisMenuBuildFile, ecBuildFile);
  AddDefault(C, 'Run File', lisMenuRunFile, ecRunFile);
  AddDefault(C, 'Config "Build File"', lisKMConfigBuildFile, ecConfigBuildFile);
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
  AddDefault(C, 'Add new component to a package', lisMenuPkgNewPackageComponent, ecNewPkgComponent);
  AddDefault(C, 'Package graph', lisMenuPackageGraph, ecPackageGraph);
  AddDefault(C, 'Package links', lisMenuPackageLinks, ecPackageLinks);
  AddDefault(C, 'Configure installed packages', lisInstallUninstallPackages, ecEditInstallPkgs);
  AddDefault(C, 'Configure custom components', lisKMConfigureCustomComponents, ecConfigCustomComps);

  // tools menu
  C:=Categories[AddCategory(CommandCategoryToolMenuName,srkmCatToolMenu,nil)];
//  C:=Categories[AddCategory('EnvironmentMenu',srkmCatEnvMenu,nil)];
  AddDefault(C, 'General environment options', srkmecEnvironmentOptions, ecEnvironmentOptions);
  AddDefault(C, 'Rescan FPC source directory', lisMenuRescanFPCSourceDirectory, ecRescanFPCSrcDir);
  AddDefault(C, 'Edit Code Templates', lisKMEditCodeTemplates, ecEditCodeTemplates);
  AddDefault(C, 'CodeTools defines editor', lisKMCodeToolsDefinesEditor, ecCodeToolsDefinesEd);
  AddDefault(C, 'Manage desktops', dlgManageDesktops, ecManageDesktops);

  AddDefault(C, 'External Tools settings', lisKMExternalToolsSettings, ecExtToolSettings);
  AddDefault(C, 'Example Projects', lisKMExampleProjects, ecManageExamples);
  AddDefault(C, 'Build Lazarus', lisMenuBuildLazarus, ecBuildLazarus);
  AddDefault(C, 'Configure "Build Lazarus"', lisConfigureBuildLazarus, ecConfigBuildLazarus);
  AddDefault(C, 'Diff editor files', lisKMDiffEditorFiles, ecDiff);
  AddDefault(C, 'Convert DFM file to LFM', lisKMConvertDFMFileToLFM, ecConvertDFM2LFM);
  AddDefault(C, 'Convert Delphi unit to Lazarus unit',
    lisKMConvertDelphiUnitToLazarusUnit, ecConvertDelphiUnit);
  AddDefault(C, 'Convert Delphi project to Lazarus project',
    lisKMConvertDelphiProjectToLazarusProject, ecConvertDelphiProject);
  AddDefault(C, 'Convert Delphi package to Lazarus package',
    lisKMConvertDelphiPackageToLazarusPackage, ecConvertDelphiPackage);
  AddDefault(C, 'Convert encoding', lisConvertEncodingOfProjectsPackages, ecConvertEncoding);
  // window menu
//  C:=Categories[AddCategory('WindowMenu',srkmCarWindowMenu,nil)];
  AddDefault(C, 'Editor Window Manager', lisSourceEditorWindowManager, ecManageSourceEditors);

  // help menu
  C:=Categories[AddCategory('HelpMenu',srkmCarHelpMenu,nil)];
  AddDefault(C, 'About Lazarus', lisAboutLazarus, ecAboutLazarus);
  AddDefault(C, 'Online Help', lisMenuOnlineHelp, ecOnlineHelp);
  AddDefault(C, 'Context sensitive help', lisKMContextSensitiveHelp, ecContextHelp);
  AddDefault(C, 'Edit context sensitive help', lisKMEditContextSensitiveHelp, ecEditContextHelp);
  AddDefault(C, 'Reporting a bug', srkmecReportingBug, ecReportingBug);
  AddDefault(C, 'Focus hint', lisFocusHint, ecFocusHint);
  AddDefault(C, 'Context sensitive smart hint', lisMenuShowSmartHint, ecSmartHint);

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
  AddDefault(C, 'Toggle showing non visual components',
    lisDsgToggleShowingNonVisualComponents, ecDesignerToggleNonVisComps);

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
  fCmdRelCache.Clear;
  for a:=0 to fCategories.Count-1 do
    Categories[a].Free;
  fCategories.Clear;
end;

function TKeyCommandRelationList.AddRelation(CmdRel: TKeyCommandRelation): Integer;
begin
  Result := FRelations.Add(CmdRel);
  fCmdRelCache.Add(CmdRel);
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

function TKeyCommandRelationList.SetKeyCommandToLoadedValues(Cmd: TKeyCommandRelation
  ): TLoadedKeyCommand;
var
  AVLNode: TAvlTreeNode;
begin
  AVLNode:=fLoadedKeyCommands.FindKey(Pointer(Cmd.Name),@CompareNameWithLoadedKeyCommand);
  if AVLNode=nil then begin
    // new key
    Result:=TLoadedKeyCommand.Create;
    Result.Name:=Cmd.Name;
    Result.DefaultShortcutA:=Cmd.ShortcutA;
    Result.DefaultShortcutB:=Cmd.ShortcutB;
    Result.ShortcutA:=Result.DefaultShortcutA;
    Result.ShortcutB:=Result.DefaultShortcutB;
    fLoadedKeyCommands.Add(Result);
  end else begin
    Result:=TLoadedKeyCommand(AVLNode.Data);
    Result.DefaultShortcutA:=Cmd.ShortcutA;
    Result.DefaultShortcutB:=Cmd.ShortcutB;
    // old key, values were loaded (key is registered after loading keymapping)
    Cmd.ShortcutA:=Result.ShortcutA;
    Cmd.ShortcutB:=Result.ShortcutB;
  end;
end;

function TKeyCommandRelationList.AddDefault(Category: TIDECommandCategory;
  const Name, LocalizedName: string; Command: word): integer;
var
  CmdRel: TKeyCommandRelation;
begin
  CmdRel:=TKeyCommandRelation.Create(Category, Name, LocalizedName, Command);
  CmdRel.GetDefaultKeyForCommand;
  CmdRel.DefaultShortcutA:=CmdRel.ShortcutA;
  CmdRel.DefaultShortcutB:=CmdRel.ShortcutB;
  SetKeyCommandToLoadedValues(CmdRel);
  Result:=AddRelation(CmdRel);
end;

procedure TKeyCommandRelationList.SetExtToolCount(NewCount: integer);
var
  i: integer;
  ExtToolCat: TIDECommandCategory;
  ExtToolRelation: TKeyCommandRelation;
  ToolLocalizedName: string;
  cmd: word;
  CmdRel: TKeyCommandRelation;
begin
  if NewCount=fExtToolCount then exit;
  //debugln(['TKeyCommandRelationList.SetExtToolCount NewCount=',NewCount,' fExtToolCount=',fExtToolCount]);
  ExtToolCat:=FindCategoryByName(CommandCategoryToolMenuName);
  //for i:=0 to ExtToolCat.Count-1 do
  //  debugln(['  ',i,'/',ExtToolCat.Count,' ',TKeyCommandRelation(ExtToolCat[i]).Name]);
  if NewCount>fExtToolCount then begin
    // increase available external tool commands
    while NewCount>fExtToolCount do begin
      ToolLocalizedName:=Format(srkmecExtTool,[fExtToolCount]);
      cmd:=ecExtToolFirst+fExtToolCount;
      CmdRel:=TKeyCommandRelation.Create(ExtToolCat,
        Format('External tool %d',[fExtToolCount]), // keep name untranslated
        ToolLocalizedName, cmd);
      AddRelation(CmdRel);
      inc(fExtToolCount);
    end;
  end else begin
    // decrease available external tool commands
    // Note: the commands are somewhere in the list, not neccesarily at the end
    i:=ExtToolCat.Count-1;
    while (i>=0) do begin
      if TObject(ExtToolCat[i]) is TKeyCommandRelation then begin
        ExtToolRelation:=TKeyCommandRelation(ExtToolCat[i]);
        cmd:=ExtToolRelation.Command;
        if (cmd>=ecExtToolFirst) and (cmd<=ecExtToolLast)
        and (cmd>=ecExtToolFirst+fExtToolCount) then begin
          fRelations.Remove(ExtToolRelation);
          fCmdRelCache.Remove(ExtToolRelation);
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
    and (Result<$10000) do begin
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
  AVLNode: TAvlTreeNode;
  LoadedKey: TLoadedKeyCommand;
begin
  //debugln('TKeyCommandRelationList.LoadFromXMLConfig A ');
  FileVersion:=XMLConfig.GetValue(Path+'Version/Value',0);
  ExtToolCount:=XMLConfig.GetValue(Path+'ExternalToolCount/Value',0);

  if FileVersion>5 then begin
    Cnt:=XMLConfig.GetValue(Path+'Count',0);
    // load all keys from the config, this may be more than the current relations
    // for example because the command is not yet registered.
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
      //if Name='ShowUnitDictionary' then
      //  debugln(['TKeyCommandRelationList.LoadFromXMLConfig ',LoadedKey.AsString]);
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
  XMLConfig:TXMLConfig; const Path: String): boolean;

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
  AVLNode: TAvlTreeNode;
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
    if Relations[a].SkipSaving then continue;
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
  i:integer;
begin
  Result:=nil;
  //debugln(['TKeyCommandRelationList.Find START, IDEWindowClass=',DbgSName(IDEWindowClass),
  //         ', Key1=', Key.Key1, ', Key2=', Key.Key2]);
  //if IDEWindowClass=nil then RaiseGDBException('');
  if Key.Key1=VK_UNKNOWN then exit;
  for i:=0 to FRelations.Count-1 do
    with Relations[i] do begin
      //if Command=ecDesignerSelectParent then
      //  debugln('TKeyCommandRelationList.Find A ',Category.Scope.Name,' ',dbgsName(IDEWindowClass),
      //          ' ',dbgs(IDECmdScopeDesignerOnly.IDEWindowClassCount),
      //          ' ',dbgsName(IDECmdScopeDesignerOnly.IDEWindowClasses[0]));
      //debugln(['TKeyCommandRelationList.Find ',Name,' HasScope=',Category.Scope<>nil,
      //         ' ',KeyAndShiftStateToEditorKeyString(ShortcutA),
      //         ' ',KeyAndShiftStateToEditorKeyString(Key),
      //         ' ',(Category.Scope<>nil) and (not Category.Scope.HasIDEWindowClass(IDEWindowClass))]);
      //if (Category.Scope<>nil) and (Category.Scope.IDEWindowClassCount>0) then
      //  debugln(['TKeyCommandRelationList.Find ',DbgSName(Category.Scope.IDEWindowClasses[0]),
      //           ' ',DbgSName(IDEWindowClass)]);
      if (Category.Scope<>nil)
      and (not Category.Scope.HasIDEWindowClass(IDEWindowClass)) then continue;
      if ((ShortcutA.Key1=Key.Key1) and (ShortcutA.Shift1=Key.Shift1) and
          (ShortcutA.Key2=Key.Key2) and (ShortcutA.Shift2=Key.Shift2))
      or ((ShortcutB.Key1=Key.Key1) and (ShortcutB.Shift1=Key.Shift1) and
          (ShortcutB.Key2=Key.Key2) and (ShortcutB.Shift2=Key.Shift2)) then
      begin
        Result:=Relations[i];
        exit;
      end;
    end;
end;

function TKeyCommandRelationList.FindIDECommand(ACommand: word): TIDECommand;
begin
  Result:=FindByCommand(ACommand);
end;

function TKeyCommandRelationList.FindByCommand(ACommand: word): TKeyCommandRelation;
var
  AVLNode: TAvlTreeNode;
begin
  AVLNode:=fCmdRelCache.FindKey({%H-}Pointer(PtrUInt(ACommand)), @CompareCmdWithCmdRel);
  if Assigned(AVLNode) then
    Result:=TKeyCommandRelation(AVLNode.Data)
  else
    Result:=nil;
end;

// Command compare functions for AvgLvlTree for fast lookup.
function CompareCmd(Data1, Data2: Pointer): integer;
var
  List1: TKeyStrokeList absolute Data1;
  List2: TKeyStrokeList absolute Data2;
  Cmd1, Cmd2: TSynEditorCommand;
begin
  Cmd1 := List1.KeyStroke1.Command;
  Cmd2 := List2.KeyStroke1.Command;
  if      Cmd1 > Cmd2 then Result:=-1
  else if Cmd1 < Cmd2 then Result:=1
  else Result:=0;
end;

function CompareKeyCmd(Data1, Data2: Pointer): integer;
var
  Cmd: PtrUInt absolute Data1;
  List2: TKeyStrokeList absolute Data2;
  Cmd1, Cmd2: TSynEditorCommand;
begin
  Cmd1 := Cmd;
  Cmd2 := List2.KeyStroke1.Command;
  if      Cmd1 > Cmd2 then Result:=-1
  else if Cmd1 < Cmd2 then Result:=1
  else Result:=0;
end;

procedure TKeyCommandRelationList.AssignTo(ASynEditKeyStrokes: TSynEditKeyStrokes;
  IDEWindowClass: TCustomFormClass; ACommandOffsetOffset: Integer = 0);
var
  Node: TAvlTreeNode;
  ccid: Word;
  CategoryMatches: Boolean;
  ToBeFreedKeys: TObjectList;
  SequentialWithCtrl: TFPList;
  SequentialWithoutCtrl: TFPList;

  function ShiftConflict(aKey: TSynEditKeyStroke): Boolean;
  // This is called when first part of combo has Ctrl and 2nd part has Ctrl or nothing.
  //  Check if ignoring Ctrl in second part would create a conflict.
  var
    ConflictList: TFPList;
    psc: PIDEShortCut;
    i: integer;
  begin
    if aKey.Shift2 = [ssCtrl] then
      ConflictList := SequentialWithoutCtrl
    else
      ConflictList := SequentialWithCtrl;
    for i:=0 to ConflictList.Count-1 do begin
      psc:=ConflictList[i];
      if (psc^.Key1=aKey.Key) and (psc^.Key2=aKey.Key2) then
        Exit(True);        // Found
    end;
    Result := False;
  end;

  procedure SetKeyCombo(aKey: TSynEditKeyStroke; aShortcut: PIDEShortCut);
  // Define a key for a command
  begin
    aKey.Key   :=aShortcut^.Key1;
    aKey.Shift :=aShortcut^.Shift1;
    aKey.Key2  :=aShortcut^.Key2;
    aKey.Shift2:=aShortcut^.Shift2;
    // Ignore the second Ctrl key in sequential combos unless both variations are defined.
    // For example "Ctrl-X, Y" and "Ctrl-X, Ctrl-Y" are then treated the same.
    if (aKey.Key2<>VK_UNKNOWN) and (aKey.Shift=[ssCtrl]) and (aKey.Shift2-[ssCtrl]=[])
    and not ShiftConflict(aKey) then begin
      aKey.ShiftMask2:=[ssCtrl];
      aKey.Shift2:=[];
    end
    else
      aKey.ShiftMask2:=[];
  end;

  procedure UpdateOrAddKeyStroke(aOffset: integer; aShortcut: PIDEShortCut);
  // Update an existing KeyStroke or add a new one
  var
    Key: TSynEditKeyStroke;
    KeyList: TKeyStrokeList;
  begin
    if Assigned(Node) then
      KeyList:=TKeyStrokeList(Node.Data);
    if Assigned(Node) and (KeyList.FCount>aOffset) then begin
      Key:=KeyList[aOffset];       // Already defined -> update
      if CategoryMatches and (aShortcut^.Key1<>VK_UNKNOWN) then
        SetKeyCombo(Key, aShortcut)
      else
        ToBeFreedKeys.Add(Key);    // No shortcut -> delete from the collection
    end
    else if CategoryMatches and (aShortcut^.Key1<>VK_UNKNOWN) then begin
      Key:=ASynEditKeyStrokes.Add;                // Add a new key
      Key.Command:=ccid;
      SetKeyCombo(Key, aShortcut);
    end;
  end;

  procedure SaveSequentialCtrl(aShortcut: PIDEShortCut);
  // Save the shortcut when it is a sequential combo and first modifier is Ctrl
  //  and second modifier is either Ctrl or nothing.
  begin
    if (aShortcut^.Shift1=[ssCtrl]) then begin
      if (aShortcut^.Shift2=[ssCtrl]) then   // Second modifier is Ctrl
        SequentialWithCtrl.Add(aShortcut)
      else if (aShortcut^.Shift2=[]) then    // No second modifier
        SequentialWithoutCtrl.Add(aShortcut);
    end;
  end;

var
  i, j: integer;
  Key: TSynEditKeyStroke;
  KeyStrokesByCmds: TAvlTree;
  KeyList: TKeyStrokeList;
  CurRelation: TKeyCommandRelation;
  POUsed: Boolean;
  SameCmdKey: TSynEditKeyStroke;
begin
  (* ACommandOffsetOffset
     The IDE defines its own fixed value command-id for plugins.
     Map them to the plugin ID
     - ecIdePTmplEdOutNextCell and ecIdePTmplEdNextCell both map to ecSynPTmplEdNextCell
     - which maps to "ecPluginFirst + n", as many others.
     But the IDE requires unique values.
     The unique values in the plugin (+ KeyOffset) can not be used, as they are not at fixed numbers
  *)
  KeyStrokesByCmds:=TAvlTree.Create(@CompareCmd);
  ToBeFreedKeys:=TObjectList.Create;
  POUsed:=ASynEditKeyStrokes.UsePluginOffset;
  SequentialWithCtrl:=TFPList.Create;
  SequentialWithoutCtrl:=TFPList.Create;
  try
    ASynEditKeyStrokes.UsePluginOffset := False;
    // Save all SynEditKeyStrokes into a tree map for fast lookup, sorted by command.
    for i:=ASynEditKeyStrokes.Count-1 downto 0 do begin
      Key:=ASynEditKeyStrokes[i];
      Node:=KeyStrokesByCmds.FindKey({%H-}Pointer(Key.Command), @CompareKeyCmd);
      if Assigned(Node) then begin // Another key is already defined for this command
        KeyList:=TKeyStrokeList(Node.Data);
        if KeyList.FCount < 3 then
          KeyList.Add(Key)
        else begin
          DebugLn(['TKeyCommandRelationList.AssignTo: WARNING: fourth key for command ',EditorCommandToDescriptionString(Key.Command),':']);
          for j:=0 to KeyList.FCount-1 do begin
            SameCmdKey:=KeyList[j];
            debugln(['  ',j,'/',KeyList.FCount,' ',KeyAndShiftStateToKeyString(SameCmdKey.Key,SameCmdKey.Shift)]);
          end;
          debugln(['  ',4,'/',KeyList.FCount,' ',KeyAndShiftStateToKeyString(Key.Key,Key.Shift)]);
          Key.Free; // This deletes the key from TSynEditKeyStrokes container as well.
        end;
      end
      else begin
        KeyList:=TKeyStrokeList.Create;
        KeyList.Add(Key);
        KeyStrokesByCmds.Add(KeyList);
      end;
    end;
    // Cache sequential combos with and without Ctrl key.
    for i:=0 to FRelations.Count-1 do begin
      CurRelation:=Relations[i];
      SaveSequentialCtrl(@CurRelation.ShortcutA);
      SaveSequentialCtrl(@CurRelation.ShortcutB);
    end;
    // Iterate all KeyCommandRelations and copy / update them to SynEditKeyStrokes.
    for i:=0 to FRelations.Count-1 do begin
      CurRelation:=Relations[i];
      CategoryMatches:=(IDEWindowClass=nil)
                   or (CurRelation.Category.Scope=nil)
                    or CurRelation.Category.Scope.HasIDEWindowClass(IDEWindowClass);
      ccid:=CurRelation.Command;
      if (ccid >= ecFirstPlugin) and (ccid < ecLastPlugin) then
        ccid:=ccid+ACommandOffsetOffset;
      // Get SynEditKeyStrokes from the lookup tree
      Node:=KeyStrokesByCmds.FindKey({%H-}Pointer(ccid), @CompareKeyCmd);
      // First and second shortcuts for this command
      UpdateOrAddKeyStroke(0, @CurRelation.ShortcutA);
      UpdateOrAddKeyStroke(1, @CurRelation.ShortcutB);
    end;
  finally
    SequentialWithoutCtrl.Free;
    SequentialWithCtrl.Free;
    ToBeFreedKeys.Free;              // Free also Key objects.
    KeyStrokesByCmds.FreeAndClear;   // Free also KeyLists.
    KeyStrokesByCmds.Free;
    ASynEditKeyStrokes.UsePluginOffset:=POUsed;
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
  // add/assign categories
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

  // add/assign keys
  for i:=0 to List.Count-1 do begin
    OtherRelation:=List.Relations[i];
    OurRelation:=TKeyCommandRelation(FindCommandByName(OtherRelation.Name));
    if OurRelation<>nil then begin
      // assign
      OurRelation.Assign(OtherRelation);
    end else begin
      // add
      //DebugLn('TKeyCommandRelationList.Assign Add new command: ',OtherRelation.Name);
      OurCategory:=FindCategoryByName(OtherRelation.Category.Name);
      OurRelation:=TKeyCommandRelation.Create(OtherRelation,OurCategory);
      AddRelation(OurRelation);
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
  while FindByCommand(Result)<>nil do
    inc(Result);
end;

function TKeyCommandRelationList.CreateCategory(Parent: TIDECommandCategory;
  const AName, Description: string; Scope: TIDECommandScope): TIDECommandCategory;
begin
  Result:=Categories[AddCategory(CreateUniqueCategoryName(AName),Description,Scope)];
end;

function TKeyCommandRelationList.CreateCommand(Category: TIDECommandCategory; const AName,
  Description: string; const TheShortcutA, TheShortcutB: TIDEShortCut;
  const OnExecuteMethod: TNotifyEvent; const OnExecuteProc: TNotifyProcedure): TIDECommand;
var
  NewName: String;
  cmd: word;
  CmdRel: TKeyCommandRelation;
begin
  NewName:=CreateUniqueCommandName(AName);
  cmd:=CreateNewCommandID;
  CmdRel:=TKeyCommandRelation.Create(Category as TKeyCommandCategory,
                      NewName, Description, cmd,
                      TheShortcutA, TheShortcutB, OnExecuteMethod, OnExecuteProc);
  SetKeyCommandToLoadedValues(CmdRel);
  AddRelation(CmdRel);
  Result:=CmdRel;
end;

procedure TKeyCommandRelationList.RemoveCommand(ACommand: TIDECommand);
begin
  fRelations.Remove(ACommand);
  fCmdRelCache.Remove(ACommand);
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
  Result:=fCategories.Add(TKeyCommandCategory.Create(Name,Description,TheScope));
end;

function TKeyCommandRelationList.FindCategoryByName(const CategoryName: string): TIDECommandCategory;
var i: integer;
begin
  for i:=0 to CategoryCount-1 do
    if CategoryName=Categories[i].Name then
      Exit(Categories[i]);
  Result:=nil;
end;

function TKeyCommandRelationList.FindCommandByName(const CommandName: string): TIDECommand;
var i: integer;
begin
  for i:=0 to RelationCount-1 do
    if CompareText(CommandName,Relations[i].Name)=0 then
      Exit(Relations[i]);
  Result:=nil;
end;

function TKeyCommandRelationList.FindCommandsByShortCut(
  const ShortCutMask: TIDEShortCut; IDEWindowClass: TCustomFormClass): TFPList;

  function KeyFits(const aShortCut: TIDEShortCut): boolean;
  begin
    if (ShortCutMask.Key1=VK_UNKNOWN) then
      exit(true); // fits all
    Result:=((aShortCut.Key1=ShortCutMask.Key1) and (aShortCut.Shift1=ShortCutMask.Shift1))
      and ((aShortCut.Key2=VK_UNKNOWN)
        or (ShortCutMask.Key2=VK_UNKNOWN)
        or ((aShortCut.Key2=ShortCutMask.Key2) and (aShortCut.Shift2=ShortCutMask.Shift2)));
  end;

var
  i: Integer;
begin
  Result:=TFPList.Create;
  if (ShortCutMask.Key1<>VK_UNKNOWN)
  and (not IsValidIDECommandKey(ShortCutMask.Key1)) then
    exit;
  for i:=0 to FRelations.Count-1 do
    with Relations[i] do begin
      if (IDEWindowClass<>nil)
      and (Category.Scope<>nil)
      and (not Category.Scope.HasIDEWindowClass(IDEWindowClass)) then continue;
      if KeyFits(ShortcutA) or KeyFits(ShortcutB) then
        Result.Add(Relations[i]);
    end;
end;

function TKeyCommandRelationList.RemoveShortCut(ShortCutMask: TIDEShortCut;
  IDEWindowClass: TCustomFormClass): Integer;
// Removes the given shortcut from every command. Returns the number deleted.
// An IDE extension package may want to use a reserved shortcut and remove it.

  procedure CheckAndRemove(pShortCut: PIDEShortCut);
  begin
    if ((pShortCut^.Key1=ShortCutMask.Key1) and (pShortCut^.Shift1=ShortCutMask.Shift1))
      and ((pShortCut^.Key2=VK_UNKNOWN)
        or (ShortCutMask.Key2=VK_UNKNOWN)
        or ((pShortCut^.Key2=ShortCutMask.Key2) and (pShortCut^.Shift2=ShortCutMask.Shift2))) then
    begin
      pShortCut^.Key1:=VK_UNKNOWN;
      pShortCut^.Shift1:=[];
      pShortCut^.Key2:=VK_UNKNOWN;
      pShortCut^.Shift2:=[];
      Inc(Result);
    end;
  end;

var
  i: Integer;
begin
  Result:=0;
  if ShortCutMask.Key1=VK_UNKNOWN then
    Exit;
  for i:=0 to FRelations.Count-1 do
    with Relations[i] do
      if (IDEWindowClass=nil) or (Category.Scope=nil)
      or Category.Scope.HasIDEWindowClass(IDEWindowClass) then
      begin
        CheckAndRemove(@ShortcutA);
        CheckAndRemove(@ShortcutB);
      end;
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
  if not IsValidIDECommandKey(Key) then
  begin
    //debugln(['TKeyCommandRelationList.TranslateKey ignoring ',dbgs(Key)]);
    exit;
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
var
  ARelation: TKeyCommandRelation;
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
  RegisterKeyCmdIdentProcs(@IdentToIDECommand,
                           @IDECommandToIdent);

end.

