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
  PropEdits, IDECommands, LazarusIDEStrConsts;

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
  protected
    procedure SetShortcutA(const AValue: TIDEShortCut); override;
    procedure SetShortcutB(const AValue: TIDEShortCut); override;
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
    function Add(Category: TIDECommandCategory; Command: TIDECommand):integer;
    function Add(Category: TIDECommandCategory;
                 const Name, LocalizedName: string;
                 Command: word; TheKeyA, TheKeyB: TIDEShortCut;
                 const OnExecuteMethod: TNotifyEvent = nil;
                 const OnExecuteProc: TNotifyProcedure = nil):integer;
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
function EditorCommandLocalizedName(cmd: word;
                                    const DefaultName: string): string;

procedure GetDefaultKeyForCommand(Command: word;
                                  out TheKeyA, TheKeyB: TIDEShortCut);
procedure GetDefaultKeyForWindowsScheme(Command: word;
                                  out TheKeyA, TheKeyB: TIDEShortCut);
procedure GetDefaultKeyForClassicScheme(Command: word;
                                        var TheKeyA, TheKeyB: TIDEShortCut);
procedure GetDefaultKeyForMacOSXScheme(Command: word;
                                       var TheKeyA, TheKeyB: TIDEShortCut);
procedure GetDefaultKeyForMacOSXLazScheme(Command: word;
                                       var TheKeyA, TheKeyB: TIDEShortCut);
function KeySchemeNameToSchemeType(const SchemeName: string): TKeyMapScheme;

function ShiftStateToCfgStr(Shift: TShiftState): string;
function KeyValuesToCfgStr(const ShortcutA, ShortcutB: TIDEShortCut): string;
function CfgStrToShiftState(const s: string): TShiftState;

function CompareLoadedKeyCommands(Data1, Data2: Pointer): integer;
function CompareNameWithLoadedKeyCommand(NameAsAnsiString, Key: Pointer): integer;


implementation


const
  KeyMappingFormatVersion = 6;

function EditorCommandLocalizedName(cmd: word;
  const DefaultName: string): string;
begin
  Result:=EditorCommandToDescriptionString(cmd);
  if Result=srkmecunknown then
    Result:=DefaultName;
end;

procedure GetDefaultKeyForCommand(Command: word; out TheKeyA,
  TheKeyB: TIDEShortCut);
begin
  {$IFDEF LCLCarbon}
  GetDefaultKeyForMacOSXScheme(Command,TheKeyA,TheKeyB);
  {$ELSE}
  GetDefaultKeyForWindowsScheme(Command,TheKeyA,TheKeyB);
  {$ENDIF}
end;

procedure GetDefaultKeyForWindowsScheme(Command: word;
  out TheKeyA, TheKeyB: TIDEShortCut);

  procedure SetResult(NewKeyA: word; NewShiftA: TShiftState;
    NewKeyB: word; NewShiftB: TShiftState);
  begin
    TheKeyA:=IDEShortCut(NewKeyA,NewShiftA,VK_UNKNOWN,[]);
    TheKeyB:=IDEShortCut(NewKeyB,NewShiftB,VK_UNKNOWN,[]);
  end;

  procedure SetResult2(
    NewKey1A: word; NewShift1A: TShiftState;
    NewKey1B: word; NewShift1B: TShiftState;
    NewKey2A: word; NewShift2A: TShiftState;
    NewKey2B: word; NewShift2B: TShiftState);
  begin
    TheKeyA:=IDEShortCut(NewKey1A,NewShift1A,NewKey1B,NewShift1B);
    TheKeyB:=IDEShortCut(NewKey2A,NewShift2A,NewKey2B,NewShift2B);
  end;


//  procedure SetResult(NewKeyA: word; NewShiftA: TShiftState);
//  begin
//    SetResult(NewKeyA,NewShiftA,VK_UNKNOWN,[]);
//  end;

begin
  case Command of
  // moving
  ecWordLeft: SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
  ecWordRight: SetResult(VK_RIGHT, [ssCtrl],VK_UNKNOWN,[]); // WS c
  ecLineStart: SetResult(VK_HOME, [],VK_UNKNOWN,[]);
  ecLineEnd: SetResult(VK_END, [],VK_UNKNOWN,[]);
  ecPageUp: SetResult(VK_PRIOR, [],VK_UNKNOWN,[]); // ,VK_R,[SSCtrl],VK_UNKNOWN,[]);
  ecPageDown: SetResult(VK_NEXT, [],VK_UNKNOWN,[]); // ,VK_W,[SSCtrl],VK_UNKNOWN,[]);
  ecPageLeft: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageRight: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageTop: SetResult(VK_PRIOR, [ssCtrl],VK_UNKNOWN,[]);
  ecPageBottom: SetResult(VK_NEXT, [ssCtrl],VK_UNKNOWN,[]);
  ecEditorTop: SetResult(VK_HOME,[ssCtrl],VK_UNKNOWN,[]);
  ecEditorBottom: SetResult(VK_END,[ssCtrl],VK_UNKNOWN,[]);
  ecScrollUp: SetResult(VK_UP, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollDown: SetResult(VK_DOWN, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollLeft: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecScrollRight: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);

  // selection
  ecCopy: SetResult(VK_C,[ssCtrl],VK_Insert,[ssCtrl]);
  ecCut: SetResult(VK_X,[ssCtrl],VK_Delete,[ssShift]);
  ecPaste: SetResult(VK_V,[ssCtrl],VK_Insert,[ssShift]);
  ecNormalSelect: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecColumnSelect: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecLineSelect: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelWordLeft: SetResult(VK_LEFT,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecSelWordRight: SetResult(VK_RIGHT,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecSelLineStart: SetResult(VK_HOME,[ssShift],VK_UNKNOWN,[]);
  ecSelLineEnd: SetResult(VK_END,[ssShift],VK_UNKNOWN,[]);
  ecSelPageTop: SetResult(VK_PRIOR, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelPageBottom: SetResult(VK_NEXT, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelEditorTop: SetResult(VK_HOME, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelEditorBottom: SetResult(VK_END, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelectAll: SetResult(VK_A,[ssCtrl],VK_UNKNOWN,[]);
  ecSelectToBrace: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectCodeBlock: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectWord: SetResult2(VK_K,[SSCtrl],VK_T,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectLine: SetResult2(VK_K,[SSCtrl],VK_L,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectParagraph: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectionUpperCase: SetResult2(VK_K,[SSCtrl],VK_N,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectionLowerCase: SetResult2(VK_K,[SSCtrl],VK_O,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectionTabs2Spaces: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionEnclose: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionComment: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionUncomment: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecToggleComment: SetResult(VK_OEM_2, [ssCtrl], VK_UNKNOWN, []);
  ecSelectionConditional: SetResult(VK_D, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelectionSort: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionBreakLines: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);

  ecBlockSetBegin   : SetResult2(VK_K,[ssCtrl],VK_B,[],  VK_K,[ssCtrl],VK_B,[ssCtrl]);
  ecBlockSetEnd     : SetResult2(VK_K,[ssCtrl],VK_K,[],  VK_K,[ssCtrl],VK_K,[ssCtrl]);
  ecBlockToggleHide : SetResult2(VK_K,[ssCtrl],VK_H,[],  VK_K,[ssCtrl],VK_H,[ssCtrl]);
  ecBlockHide       : SetResult2(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockShow       : SetResult2(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockMove       : SetResult2(VK_K,[ssCtrl],VK_V,[],  VK_K,[ssCtrl],VK_V,[ssCtrl]);
  ecBlockCopy       : SetResult2(VK_K,[ssCtrl],VK_C,[],  VK_K,[ssCtrl],VK_C,[ssCtrl]);
  ecBlockDelete     : SetResult2(VK_K,[ssCtrl],VK_Y,[],  VK_K,[ssCtrl],VK_Y,[ssCtrl]);
  ecBlockGotoBegin  : SetResult2(VK_Q,[ssCtrl],VK_B,[ssCtrl],  VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockGotoEnd    : SetResult2(VK_Q,[ssCtrl],VK_K,[ssCtrl],  VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // column mode selection
  ecColSelUp: SetResult(VK_UP, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelDown: SetResult(VK_DOWN, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelLeft: SetResult(VK_LEFT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelRight: SetResult(VK_RIGHT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageDown: SetResult(VK_NEXT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageBottom: SetResult(VK_NEXT, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelPageUp: SetResult(VK_PRIOR, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageTop: SetResult(VK_PRIOR, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelLineStart: SetResult(VK_HOME, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelLineEnd: SetResult(VK_END, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelEditorTop: SetResult(VK_HOME, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelEditorBottom: SetResult(VK_END, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);

  // editing
  ecBlockIndent: SetResult2(VK_I,[ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_I,[]);
  ecBlockUnindent: SetResult2(VK_U,[ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_U,[]);
  ecDeleteLastChar: SetResult(VK_BACK, [],VK_BACK, [ssShift]);  // ctrl H used for scroll window.
  ecDeleteChar: SetResult(VK_DELETE,[],VK_UNKNOWN,[]); // ctrl G conflicts with GO
  ecDeleteWord: SetResult(VK_T,[ssCtrl],VK_UNKNOWN,[]);
  ecDeleteLastWord: SetResult(VK_BACK,[ssCtrl],VK_UNKNOWN,[]);
  ecDeleteBOL: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDeleteEOL: SetResult2(VK_Y,[ssCtrl,ssShift],VK_UNKNOWN,[],VK_Q,[ssCtrl],VK_Y,[]);
  ecDeleteLine: SetResult(VK_Y,[ssCtrl],VK_UNKNOWN,[]);
  ecClearAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecLineBreak: SetResult(VK_RETURN,[],VK_UNKNOWN,[]);
  ecInsertLine: SetResult(VK_N,[ssCtrl],VK_UNKNOWN,[]);
  ecInsertCharacter: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertGPLNotice: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertLGPLNotice: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertModifiedLGPLNotice: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertUserName: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertDateTime: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertChangeLogEntry: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSAuthor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSDate: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSHeader: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSID: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSLog: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSName: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSRevision: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSSource: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertGUID: SetResult(VK_G, [ssCtrl,ssShift],VK_UNKNOWN,[]);

  // command commands
  ecUndo: SetResult(VK_Z,[ssCtrl],VK_UNKNOWN,[]);
  ecRedo: SetResult(VK_Z,[ssCtrl,ssShift],VK_UNKNOWN,[]);

  // search & replace
  ecMatchBracket: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFind: SetResult2(VK_Q,[SSCtrl],VK_F,[],VK_F,[SSCtrl],VK_UNKNOWN,[]);
  ecFindNext: SetResult2(VK_F3,[],VK_UNKNOWN,[],VK_L,[SSCtrl],VK_UNKNOWN,[]);
  ecFindPrevious: SetResult(VK_F3,[ssShift],VK_UNKNOWN,[]);
  ecFindInFiles: SetResult(VK_F,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecReplace: SetResult2(VK_R,[SSCtrl],VK_UNKNOWN,[],  VK_Q,[SSCtrl],VK_A,[]);
  ecIncrementalFind: SetResult(VK_E,[SSCtrl],VK_UNKNOWN,[]);
  ecGotoLineNumber: SetResult2(VK_G,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_G,[]);
  ecFindNextWordOccurrence: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindPrevWordOccurrence: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpBack: SetResult(VK_H,[ssCtrl],VK_UNKNOWN,[]);
  ecJumpForward: SetResult(VK_H,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecAddJumpPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewJumpHistory: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpToPrevError: SetResult(VK_F8,[ssCtrl, ssShift],VK_UNKNOWN,[]);
  ecJumpToNextError: SetResult(VK_F8,[ssCtrl],VK_UNKNOWN,[]);
  ecOpenFileAtCursor: SetResult2(VK_RETURN,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProcedureList: SetResult(VK_G, [ssAlt],VK_UNKNOWN,[]);


  // marker
  ecSetFreeBookmark: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevBookmark: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextBookmark: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker0: SetResult2(VK_0,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_0,[]);
  ecGotoMarker1: SetResult2(VK_1,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_1,[]);
  ecGotoMarker2: SetResult2(VK_2,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_2,[]);
  ecGotoMarker3: SetResult2(VK_3,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_3,[]);
  ecGotoMarker4: SetResult2(VK_4,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_4,[]);
  ecGotoMarker5: SetResult2(VK_5,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_5,[]);
  ecGotoMarker6: SetResult2(VK_6,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_6,[]);
  ecGotoMarker7: SetResult2(VK_7,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_7,[]);
  ecGotoMarker8: SetResult2(VK_8,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_8,[]);
  ecGotoMarker9: SetResult2(VK_9,[ssCtrl],VK_UNKNOWN,[],VK_Q,[SSCtrl],VK_9,[]);
  ecToggleMarker0: SetResult2(VK_0,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_0,[]);
  ecToggleMarker1: SetResult2(VK_1,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_1,[]);
  ecToggleMarker2: SetResult2(VK_2,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_2,[]);
  ecToggleMarker3: SetResult2(VK_3,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_3,[]);
  ecToggleMarker4: SetResult2(VK_4,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_4,[]);
  ecToggleMarker5: SetResult2(VK_5,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_5,[]);
  ecToggleMarker6: SetResult2(VK_6,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_6,[]);
  ecToggleMarker7: SetResult2(VK_7,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_7,[]);
  ecToggleMarker8: SetResult2(VK_8,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_8,[]);
  ecToggleMarker9: SetResult2(VK_9,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_9,[]);
  ecSetMarker0: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker1: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker2: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker3: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker4: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker5: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker6: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker7: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker8: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker9: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // codetools
  ecAutoCompletion: SetResult(VK_J,[ssCtrl],VK_UNKNOWN,[]);
  ecWordCompletion: SetResult(VK_W,[ssCtrl],VK_UNKNOWN,[]);
  ecCompleteCode: SetResult(VK_C,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecIdentCompletion: SetResult(VK_SPACE,[ssCtrl],VK_UNKNOWN,[]);
  ecShowCodeContext: SetResult(VK_SPACE,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecExtractProc: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindIdentifierRefs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRenameIdentifier: SetResult(VK_E,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecInvertAssignment: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSyntaxCheck: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessUnclosedBlock: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessMisplacedIFDEF: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDFM2LFM: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCheckLFM: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiUnit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiPackage: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertEncoding: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindProcedureDefinition: SetResult(VK_UP,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  ecFindProcedureMethod: SetResult(VK_DOWN,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  ecFindDeclaration: SetResult(VK_UP,[ssAlt],VK_UNKNOWN,[]);
  ecFindBlockOtherEnd: SetResult2(VK_Q,[ssCtrl],VK_K,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindBlockStart: SetResult2(VK_Q,[ssCtrl],VK_B,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoIncludeDirective: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowAbstractMethods: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveEmptyMethods: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveUnusedUnits: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindOverloads: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // source notebook
  ecNextEditor: SetResult(VK_TAB, [ssCtrl], VK_UNKNOWN, []);
  ecPrevEditor: SetResult(VK_TAB, [ssShift,ssCtrl], VK_UNKNOWN, []);
  ecResetDebugger: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleBreakPoint: SetResult(VK_F5,[],VK_UNKNOWN,[]);
  ecMoveEditorLeft: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorRight: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorLeftmost: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorRightmost: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, []);

  ecNextSharedEditor:        SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevSharedEditor:        SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextWindow:              SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevWindow:              SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNextWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorPrevWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNewWindow:     SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNextWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorPrevWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNewWindow:     SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  ecGotoEditor1: SetResult(VK_1,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor2: SetResult(VK_2,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor3: SetResult(VK_3,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor4: SetResult(VK_4,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor5: SetResult(VK_5,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor6: SetResult(VK_6,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor7: SetResult(VK_7,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor8: SetResult(VK_8,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor9: SetResult(VK_9,[ssAlt],VK_UNKNOWN,[]);
  ecGotoEditor0: SetResult(VK_0,[ssAlt],VK_UNKNOWN,[]);

  ecLockEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  EcFoldLevel1: SetResult(VK_1,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel2: SetResult(VK_2,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel3: SetResult(VK_3,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel4: SetResult(VK_4,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel5: SetResult(VK_5,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel6: SetResult(VK_6,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel7: SetResult(VK_7,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel8: SetResult(VK_8,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel9: SetResult(VK_9,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel0: SetResult(VK_0,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcFoldCurrent: SetResult(VK_OEM_MINUS,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcUnFoldCurrent: SetResult(VK_OEM_PLUS,[ssAlt,ssShift],VK_UNKNOWN,[]);
  EcToggleMarkupWord: SetResult(VK_M,[ssAlt],VK_UNKNOWN,[]);

  // file menu
  ecNew: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewUnit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewForm: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpen: SetResult(VK_O,[ssCtrl],VK_UNKNOWN,[]);
  ecRevert: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSave: SetResult(VK_S,[ssCtrl],VK_UNKNOWN,[]);
  ecSaveAs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveAll: SetResult(VK_S,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecClose: SetResult(VK_F4,[ssCtrl],VK_UNKNOWN,[]);
  ecCloseAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCleanDirectory: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRestart: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // view menu
  ecToggleObjectInsp: SetResult(VK_F11,[],VK_UNKNOWN,[]);
  ecToggleSourceEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeExpl: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFPDocEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMessages: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleSearchResults: SetResult(VK_F,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleWatches: SetResult(VK_W,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleBreakPoints: SetResult(VK_B,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleLocals: SetResult(VK_L,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleCallStack: SetResult(VK_S,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleRegisters: SetResult(VK_R,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleAssembler: SetResult(VK_D,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleDebugEvents: SetResult(VK_V,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleDebuggerOut: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnits: SetResult(VK_F12,[ssCtrl],VK_UNKNOWN,[]);
  ecViewForms: SetResult(VK_F12,[ssShift],VK_UNKNOWN,[]);
  ecViewUnitDependencies: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnitInfo: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFormUnit: SetResult(VK_F12,[],VK_UNKNOWN,[]);
  ecViewAnchorEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeBrowser: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleRestrictionBrowser: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCompPalette: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleIDESpeedBtns: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // project menu
  ecNewProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewProjectFromFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenProject: SetResult(VK_F11,[ssCtrl],VK_UNKNOWN,[]);
  ecCloseProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProjectAs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPublishProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectInspector: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurUnitToProj: SetResult(VK_F11,[ssShift],VK_UNKNOWN,[]);
  ecRemoveFromProj: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewProjectSource: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectOptions: SetResult(VK_F11,[ssShift,ssCtrl],VK_UNKNOWN,[]);

  // run menu
  ecBuild: SetResult(VK_F9,[ssCtrl],VK_UNKNOWN,[]);
  ecBuildAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuickCompile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAbortBuild: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRun: SetResult(VK_F9,[],VK_UNKNOWN,[]);
  ecPause: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowExecutionPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepInto: SetResult(VK_F7,[],VK_UNKNOWN,[]);
  ecStepOver: SetResult(VK_F8,[],VK_UNKNOWN,[]);
  ecStepIntoInstr: SetResult(VK_F7,[ssAlt],VK_UNKNOWN,[]);
  ecStepOverInstr: SetResult(VK_F8,[ssAlt],VK_UNKNOWN,[]);
  ecStepOut: SetResult(VK_F8,[ssShift],VK_UNKNOWN,[]);
  ecRunToCursor: SetResult(VK_F4,[],VK_UNKNOWN,[]);
  ecStopProgram: SetResult(VK_F2,[SSCtrl],VK_UNKNOWN,[]);
  ecRemoveBreakPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunParameters: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInspect: SetResult(VK_F5,[ssAlt],VK_UNKNOWN,[]);
  ecEvaluate: SetResult(VK_F7,[ssCtrl],VK_UNKNOWN,[]);
  ecAddWatch: SetResult(VK_F5,[ssCtrl],VK_UNKNOWN,[]);

  // components menu
  ecNewPackage: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackage: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageOfCurUnit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurUnitToPkg: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPackageGraph: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditInstallPkgs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigCustomComps: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // tools menu
  ecExtToolSettings: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildLazarus: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildLazarus: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMakeResourceString: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDiff: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // environment menu
  ecEnvironmentOptions: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditCodeTemplates: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCodeToolsDefinesEd: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRescanFPCSrcDir: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // help menu
  ecAboutLazarus: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOnlineHelp: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecContextHelp: SetResult(VK_F1,[],VK_UNKNOWN,[]);
  ecEditContextHelp: SetResult(VK_F1,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecReportingBug: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // designer
  ecDesignerCopy        : SetResult(VK_C,[ssCtrl],VK_Insert,[ssCtrl]);
  ecDesignerCut         : SetResult(VK_X,[ssCtrl],VK_Delete,[ssShift]);
  ecDesignerPaste       : SetResult(VK_V,[ssCtrl],VK_Insert,[ssShift]);
  ecDesignerSelectParent: SetResult(VK_ESCAPE,[],VK_UNKNOWN,[]);
  ecDesignerMoveToFront : SetResult(VK_PRIOR,[ssShift],VK_UNKNOWN,[]);
  ecDesignerMoveToBack  : SetResult(VK_NEXT,[ssShift],VK_UNKNOWN,[]);
  ecDesignerForwardOne  : SetResult(VK_PRIOR,[ssCtrl],VK_UNKNOWN,[]);
  ecDesignerBackOne     : SetResult(VK_NEXT,[ssCtrl],VK_UNKNOWN,[]);

  else
    begin
      SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
      case TSynPluginTemplateEdit.ConvertCommandToBase(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetResult(VK_HOME, [],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetResult(VK_END,  [],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetResult(VK_A,    [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetResult(VK_RETURN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginTemplateEdit.ConvertCommandToBaseOff(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetResult(VK_RETURN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBase(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetResult(VK_HOME, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetResult(VK_END,  [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetResult(VK_A,    [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseOff(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseSel(Command) of
        // SyncroEdit, during selection
        ecSynPSyncroEdStart:               SetResult(VK_J,[ssCtrl],VK_UNKNOWN,[]);
      end;
    end;
  end;
end;

procedure GetDefaultKeyForClassicScheme(Command: word;
  var TheKeyA, TheKeyB: TIDEShortCut);

  procedure SetResult(
    NewKey1A: word; NewShift1A: TShiftState;
    NewKey1B: word; NewShift1B: TShiftState;
    NewKey2A: word; NewShift2A: TShiftState;
    NewKey2B: word; NewShift2B: TShiftState);
  begin
    TheKeyA:=IDEShortCut(NewKey1A,NewShift1A,NewKey1B,NewShift1B);
    TheKeyB:=IDEShortCut(NewKey2A,NewShift2A,NewKey2B,NewShift2B);
  end;

  procedure SetResult(NewKeyA: word; NewShiftA: TShiftState;
    NewKeyB: word; NewShiftB: TShiftState);
  begin
    TheKeyA:=IDEShortCut(NewKeyA,NewShiftA,VK_UNKNOWN,[]);
    TheKeyB:=IDEShortCut(NewKeyB,NewShiftB,VK_UNKNOWN,[]);
  end;

  procedure SetResult(NewKeyA: word; NewShiftA: TShiftState);
  begin
    SetResult(NewKeyA,NewShiftA,VK_UNKNOWN,[]);
  end;

begin
  GetDefaultKeyForWindowsScheme(Command,TheKeyA,TheKeyB);

  case Command of
  // moving
  ecWordLeft:SetResult(VK_A, [ssCtrl], VK_UNKNOWN, [], VK_LEFT, [ssCtrl], VK_UNKNOWN,[]);
  ecWordRight: SetResult(VK_D, [ssCtrl], VK_UNKNOWN, [], VK_RIGHT, [ssCtrl],VK_UNKNOWN,[]);
  ecLineStart: SetResult(VK_Q, [ssCtrl], VK_S, [], VK_HOME, [],VK_UNKNOWN,[]);
  ecLineEnd: SetResult(VK_Q, [ssCtrl], VK_D, [], VK_END, [],VK_UNKNOWN,[]);
  ecPageUp: SetResult(VK_R, [ssCtrl], VK_UNKNOWN, [], VK_PRIOR, [],VK_UNKNOWN,[]);
  ecPageDown: SetResult(VK_C, [ssCtrl], VK_UNKNOWN, [], VK_NEXT, [],VK_UNKNOWN,[]);
  ecPageLeft: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageRight: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageTop: SetResult(VK_Q, [ssCtrl], VK_E, [], VK_HOME, [ssCtrl],VK_UNKNOWN,[]);
  ecPageBottom: SetResult(VK_Q, [ssCtrl], VK_X, [], VK_END, [ssCtrl],VK_UNKNOWN,[]);
  ecEditorTop: SetResult(VK_Q, [ssCtrl], VK_R, [], VK_PRIOR,[ssCtrl],VK_UNKNOWN,[]);
  ecEditorBottom: SetResult(VK_Q, [ssCtrl], VK_C, [], VK_NEXT,[ssCtrl],VK_UNKNOWN,[]);
  ecScrollUp: SetResult(VK_W, [ssCtrl], VK_UNKNOWN, [], VK_UP, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollDown: SetResult(VK_Z, [ssCtrl], VK_UNKNOWN, [], VK_DOWN, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollLeft: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecScrollRight: SetResult(VK_UNKNOWN, [], VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // selection
  ecCopy: SetResult(VK_Insert,[ssCtrl],VK_UNKNOWN, [],VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecCut: SetResult(VK_Delete,[ssShift],VK_UNKNOWN, [],VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecPaste: SetResult(VK_Insert,[ssShift],VK_UNKNOWN, [],VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecNormalSelect: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecColumnSelect: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecLineSelect: SetResult(VK_K,[ssCtrl],VK_L,[], VK_UNKNOWN, [],VK_UNKNOWN, []);
  ecSelWordLeft: SetResult(VK_LEFT,[ssCtrl,ssShift],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelWordRight: SetResult(VK_RIGHT,[ssCtrl,ssShift],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelLineStart: SetResult(VK_HOME,[ssShift],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelLineEnd: SetResult(VK_END,[ssShift],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelPageTop: SetResult(VK_HOME, [ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelPageBottom: SetResult(VK_END, [ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelEditorTop: SetResult(VK_PRIOR, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelEditorBottom: SetResult(VK_NEXT, [ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectToBrace: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectCodeBlock: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectWord: SetResult(VK_K,[ssCtrl],VK_T,[], VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectLine: SetResult(VK_O,[ssCtrl],VK_L,[], VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectParagraph: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionUpperCase: SetResult(VK_K,[ssCtrl],VK_N,[], VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionLowerCase: SetResult(VK_K,[ssCtrl],VK_O,[], VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionTabs2Spaces: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionEnclose: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionComment: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionUncomment: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecToggleComment: SetResult(VK_OEM_2, [ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionConditional: SetResult(VK_D, [ssShift,ssCtrl],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionSort: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecSelectionBreakLines: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[], VK_UNKNOWN, [], VK_UNKNOWN, []);

  ecBlockSetBegin   : SetResult(VK_K,[ssCtrl],VK_B,[],  VK_K,[ssCtrl],VK_B,[ssCtrl]);
  ecBlockSetEnd     : SetResult(VK_K,[ssCtrl],VK_K,[],  VK_K,[ssCtrl],VK_K,[ssCtrl]);
  ecBlockToggleHide : SetResult(VK_K,[ssCtrl],VK_H,[],  VK_K,[ssCtrl],VK_H,[ssCtrl]);
  ecBlockHide       : SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockShow       : SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockMove       : SetResult(VK_K,[ssCtrl],VK_V,[],  VK_K,[ssCtrl],VK_V,[ssCtrl]);
  ecBlockCopy       : SetResult(VK_K,[ssCtrl],VK_C,[],  VK_K,[ssCtrl],VK_C,[ssCtrl]);
  ecBlockDelete     : SetResult(VK_K,[ssCtrl],VK_Y,[ssCtrl],  VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockGotoBegin  : SetResult(VK_Q,[ssCtrl],VK_B,[ssCtrl],  VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockGotoEnd    : SetResult(VK_Q,[ssCtrl],VK_K,[ssCtrl],  VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // column mode selection
  ecColSelUp: SetResult(VK_UP, [ssAlt, ssShift], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelDown: SetResult(VK_DOWN, [ssAlt, ssShift], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelLeft: SetResult(VK_LEFT, [ssAlt, ssShift], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelRight: SetResult(VK_RIGHT, [ssAlt, ssShift], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelPageDown: SetResult(VK_NEXT, [ssAlt, ssShift], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelPageBottom: SetResult(VK_NEXT, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelPageUp: SetResult(VK_PRIOR, [ssAlt, ssShift], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelPageTop: SetResult(VK_PRIOR, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelLineStart: SetResult(VK_HOME, [ssAlt, ssShift], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelLineEnd: SetResult(VK_END, [ssAlt, ssShift], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelEditorTop: SetResult(VK_HOME, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);
  ecColSelEditorBottom: SetResult(VK_END, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[], VK_UNKNOWN,[], VK_UNKNOWN,[]);

  // editing
  ecBlockIndent: SetResult(VK_K,[ssCtrl],VK_I,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockUnindent: SetResult(VK_K,[ssCtrl],VK_U,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDeleteLastChar: SetResult(VK_H,[ssCtrl],VK_UNKNOWN,[],VK_BACK, [],VK_UNKNOWN,[]);
  ecDeleteChar: SetResult(VK_DELETE,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDeleteWord: SetResult(VK_T,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDeleteLastWord: SetResult(VK_BACK,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDeleteBOL: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDeleteEOL: SetResult(VK_K,[ssCtrl],VK_Y,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDeleteLine: SetResult(VK_Y,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecClearAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecLineBreak: SetResult(VK_RETURN,[],VK_UNKNOWN,[],VK_M,[ssCtrl],VK_UNKNOWN,[]);
  ecInsertLine: SetResult(VK_N,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCharacter: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertGPLNotice: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertLGPLNotice: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertModifiedLGPLNotice: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertUserName: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertDateTime: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertChangeLogEntry: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSAuthor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSDate: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSHeader: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSID: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSLog: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSName: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSRevision: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSSource: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // command commands
  ecUndo: SetResult(VK_BACK,[ssALT],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRedo: SetResult(VK_BACK,[ssALT,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // search & replace
  ecMatchBracket: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFind: SetResult(VK_Q,[SSCtrl],VK_F,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindNext: SetResult(VK_L,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindPrevious: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindInFiles: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecReplace: SetResult(VK_Q,[SSCtrl],VK_A,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecIncrementalFind: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoLineNumber: SetResult(VK_G,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindNextWordOccurrence: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindPrevWordOccurrence: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpBack: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpForward: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddJumpPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewJumpHistory: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpToPrevError: SetResult(VK_F7,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpToNextError: SetResult(VK_F8,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenFileAtCursor: SetResult(VK_RETURN,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // marker
  ecSetFreeBookmark: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevBookmark: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextBookmark: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker0: SetResult(VK_Q,[ssCtrl],VK_0,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker1: SetResult(VK_Q,[ssCtrl],VK_1,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker2: SetResult(VK_Q,[ssCtrl],VK_2,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker3: SetResult(VK_Q,[ssCtrl],VK_3,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker4: SetResult(VK_Q,[ssCtrl],VK_4,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker5: SetResult(VK_Q,[ssCtrl],VK_5,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker6: SetResult(VK_Q,[ssCtrl],VK_6,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker7: SetResult(VK_Q,[ssCtrl],VK_7,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker8: SetResult(VK_Q,[ssCtrl],VK_8,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker9: SetResult(VK_Q,[ssCtrl],VK_9,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSetMarker0..ecSetMarker9: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker0: SetResult(VK_K,[ssCtrl],VK_0,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker1: SetResult(VK_K,[ssCtrl],VK_1,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker2: SetResult(VK_K,[ssCtrl],VK_2,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker3: SetResult(VK_K,[ssCtrl],VK_3,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker4: SetResult(VK_K,[ssCtrl],VK_4,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker5: SetResult(VK_K,[ssCtrl],VK_5,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker6: SetResult(VK_K,[ssCtrl],VK_6,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker7: SetResult(VK_K,[ssCtrl],VK_7,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker8: SetResult(VK_K,[ssCtrl],VK_8,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMarker9: SetResult(VK_K,[ssCtrl],VK_9,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // codetools
  ecAutoCompletion: SetResult(VK_J,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecWordCompletion: SetResult(VK_W,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCompleteCode: SetResult(VK_C,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecIdentCompletion: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowCodeContext: SetResult(VK_SPACE,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecExtractProc: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindIdentifierRefs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRenameIdentifier: SetResult(VK_E,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInvertAssignment: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSyntaxCheck: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessUnclosedBlock: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessMisplacedIFDEF: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDFM2LFM: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCheckLFM: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiUnit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiPackage: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertEncoding: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindProcedureDefinition: SetResult(VK_UP,[ssShift,SSCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindProcedureMethod: SetResult(VK_DOWN,[ssShift,SSCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindDeclaration: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindBlockOtherEnd: SetResult(VK_Q,[ssCtrl],VK_K,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindBlockStart: SetResult(VK_Q,[ssCtrl],VK_B,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoIncludeDirective: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowAbstractMethods: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveEmptyMethods: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // source notebook
  ecNextEditor: SetResult(VK_F6,[],VK_UNKNOWN,[],VK_TAB, [ssCtrl], VK_UNKNOWN, []);
  ecPrevEditor: SetResult(VK_F6,[ssShift],VK_UNKNOWN,[],VK_TAB, [ssShift,ssCtrl], VK_UNKNOWN, []);
  ecResetDebugger: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleBreakPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorLeft: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorRight: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorLeftmost: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorRightmost: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  ecNextSharedEditor:        SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevSharedEditor:        SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextWindow:              SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevWindow:              SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNextWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorPrevWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNewWindow:     SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNextWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorPrevWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNewWindow:     SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  ecGotoEditor1: SetResult(VK_1,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor2: SetResult(VK_2,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor3: SetResult(VK_3,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor4: SetResult(VK_4,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor5: SetResult(VK_5,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor6: SetResult(VK_6,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor7: SetResult(VK_7,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor8: SetResult(VK_8,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor9: SetResult(VK_9,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor0: SetResult(VK_0,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  ecLockEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  EcFoldLevel1: SetResult(VK_1,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcFoldLevel2: SetResult(VK_2,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcFoldLevel3: SetResult(VK_3,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcFoldLevel4: SetResult(VK_4,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcFoldLevel5: SetResult(VK_5,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcFoldLevel6: SetResult(VK_6,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcFoldLevel7: SetResult(VK_7,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcFoldLevel8: SetResult(VK_8,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcFoldLevel9: SetResult(VK_9,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcFoldLevel0: SetResult(VK_0,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcFoldCurrent: SetResult(VK_OEM_PLUS,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcUnFoldCurrent: SetResult(VK_OEM_MINUS,[ssAlt,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  EcToggleMarkupWord: SetResult(VK_M,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // file menu
  ecNew: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewUnit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewForm: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpen: SetResult(VK_F3,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRevert: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSave: SetResult(VK_F2,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveAs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveAll: SetResult(VK_F2,[ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecClose: SetResult(VK_F3,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCloseAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCleanDirectory: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRestart: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuit: SetResult(VK_X,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // view menu
  ecToggleObjectInsp: SetResult(VK_F11,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleSourceEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeExpl: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFPDocEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMessages: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleSearchResults: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleWatches: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleBreakPoints: SetResult(VK_F8,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleLocals: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCallStack: SetResult(VK_F3,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleRegisters: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleAssembler: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleDebugEvents: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleDebuggerOut: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnits: SetResult(VK_F12,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewForms: SetResult(VK_F12,[ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnitDependencies: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnitInfo: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFormUnit: SetResult(VK_F12,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewAnchorEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeBrowser: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleRestrictionBrowser: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCompPalette: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleIDESpeedBtns: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // project menu
  ecNewProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewProjectFromFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenProject: SetResult(VK_F11,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCloseProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProjectAs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPublishProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectInspector: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurUnitToProj: SetResult(VK_F11,[ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveFromProj: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewProjectSource: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectOptions: SetResult(VK_F11,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // run menu
  ecBuild: SetResult(VK_F9,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuickCompile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAbortBuild: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRun: SetResult(VK_F9,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPause: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowExecutionPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepInto: SetResult(VK_F7,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepOver: SetResult(VK_F8,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepIntoInstr: SetResult(VK_F7,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepOverInstr: SetResult(VK_F8,[ssAlt],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepOut: SetResult(VK_F8,[ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunToCursor: SetResult(VK_F4,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStopProgram: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveBreakPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunParameters: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInspect: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEvaluate: SetResult(VK_F4,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddWatch: SetResult(VK_F7,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // components menu
  ecNewPackage: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackage: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageOfCurUnit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurUnitToPkg: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPackageGraph: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditInstallPkgs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigCustomComps: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // tools menu
  ecExtToolSettings: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildLazarus: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildLazarus: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMakeResourceString: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDiff: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // environment menu
  ecEnvironmentOptions: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditCodeTemplates: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCodeToolsDefinesEd: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRescanFPCSrcDir: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // help menu
  ecAboutLazarus: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOnlineHelp: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecContextHelp: SetResult(VK_F1,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditContextHelp: SetResult(VK_F1,[ssCtrl,ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecReportingBug: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // designer
  ecDesignerCopy        : SetResult(VK_C,[ssCtrl],VK_UNKNOWN,[],VK_Insert,[ssCtrl],VK_UNKNOWN,[]);
  ecDesignerCut         : SetResult(VK_X,[ssCtrl],VK_UNKNOWN,[],VK_Delete,[ssShift],VK_UNKNOWN,[]);
  ecDesignerPaste       : SetResult(VK_V,[ssCtrl],VK_UNKNOWN,[],VK_Insert,[ssShift],VK_UNKNOWN,[]);
  ecDesignerSelectParent: SetResult(VK_ESCAPE,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDesignerMoveToFront : SetResult(VK_PRIOR,[ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDesignerMoveToBack  : SetResult(VK_NEXT,[ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDesignerForwardOne  : SetResult(VK_PRIOR,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDesignerBackOne     : SetResult(VK_NEXT,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);

  else
    begin
      SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
      SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
      case TSynPluginTemplateEdit.ConvertCommandToBase(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetResult(VK_HOME, [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetResult(VK_END,  [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetResult(VK_A,    [ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetResult(VK_RETURN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginTemplateEdit.ConvertCommandToBaseOff(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetResult(VK_RETURN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBase(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetResult(VK_HOME, [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetResult(VK_END,  [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetResult(VK_A,    [ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseOff(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseSel(Command) of
        // SyncroEdit, during selection
        ecSynPSyncroEdStart:               SetResult(VK_J,[ssCtrl],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
      end;
    end;
  end;
(*//F1                      Topic Search
//Ctrl+F1                Topic Search
  ecNextEditor: SetResult(VK_F6,[]);
  ecPrevEditor: SetResult(VK_F6,[ssShift]);
  ecWordLeft:   SetResult(VK_A,[ssCtrl],VK_LEFT,[ssCtrl]);
  ecPageDown:   SetResult(VK_C,[ssCtrl],VK_NEXT,[]);
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

procedure GetDefaultKeyForMacOSXScheme(Command: word; var TheKeyA, TheKeyB: TIDEShortCut);

  procedure SetResult(NewKeyA: word; NewShiftA: TShiftState;
    NewKeyB: word; NewShiftB: TShiftState);
  begin
    TheKeyA:=IDEShortCut(NewKeyA, NewShiftA, VK_UNKNOWN, []);
    TheKeyB:=IDEShortCut(NewKeyB, NewShiftB, VK_UNKNOWN, []);
  end;

  procedure SetResult2(
    NewKey1A: word; NewShift1A: TShiftState;
    NewKey1B: word; NewShift1B: TShiftState;
    NewKey2A: word; NewShift2A: TShiftState;
    NewKey2B: word; NewShift2B: TShiftState);
  begin
    TheKeyA:=IDEShortCut(NewKey1A,NewShift1A,NewKey1B,NewShift1B);
    TheKeyB:=IDEShortCut(NewKey2A,NewShift2A,NewKey2B,NewShift2B);
  end;

begin
  case Command of
  // moving
  ecWordLeft: SetResult(VK_LEFT, [ssAlt],VK_UNKNOWN,[]);
  ecWordRight: SetResult(VK_RIGHT, [ssAlt],VK_UNKNOWN,[]);
  ecLineStart: SetResult(VK_LEFT, [ssMeta],VK_UNKNOWN,[]);
  ecLineEnd: SetResult(VK_RIGHT, [ssMeta],VK_UNKNOWN,[]);
  ecPageUp: SetResult(VK_PRIOR, [],VK_UNKNOWN,[]);
  ecPageDown: SetResult(VK_NEXT, [],VK_UNKNOWN,[]);
  ecPageLeft: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageRight: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPageTop: SetResult(VK_PRIOR, [ssAlt],VK_UNKNOWN,[]);
  ecPageBottom: SetResult(VK_END, [ssAlt],VK_UNKNOWN,[]);
  ecEditorTop: SetResult(VK_HOME,[],VK_UP,[ssMeta]);
  ecEditorBottom: SetResult(VK_END,[],VK_DOWN,[ssMeta]);
  ecScrollUp: SetResult(VK_UP, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollDown: SetResult(VK_DOWN, [ssCtrl],VK_UNKNOWN,[]);
  ecScrollLeft: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecScrollRight: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);

  // selection
  ecCopy: SetResult(VK_C,[ssMeta],VK_Insert,[ssCtrl]);
  ecCut: SetResult(VK_X,[ssMeta],VK_Delete,[ssShift]);
  ecPaste: SetResult(VK_V,[ssMeta],VK_Insert,[ssShift]);
  ecNormalSelect: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecColumnSelect: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecLineSelect: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelWordLeft: SetResult(VK_LEFT,[ssAlt,ssShift],VK_UNKNOWN,[]);
  ecSelWordRight: SetResult(VK_RIGHT,[ssAlt,ssShift],VK_UNKNOWN,[]);
  ecSelLineStart: SetResult(VK_LEFT,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecSelLineEnd: SetResult(VK_RIGHT,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecSelPageTop: SetResult(VK_PRIOR, [ssShift,ssAlt],VK_UNKNOWN,[]);
  ecSelPageBottom: SetResult(VK_NEXT, [ssShift,ssAlt],VK_UNKNOWN,[]);
  ecSelEditorTop: SetResult(VK_HOME, [ssShift],VK_UNKNOWN,[]);
  ecSelEditorBottom: SetResult(VK_END, [ssShift],VK_UNKNOWN,[]);
  ecSelectAll: SetResult(VK_A,[ssMeta],VK_UNKNOWN,[]);
  ecSelectToBrace: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectCodeBlock: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectWord: SetResult2(VK_K,[SSCtrl],VK_T,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectLine: SetResult2(VK_K,[SSCtrl],VK_L,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectParagraph: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectionUpperCase: SetResult2(VK_K,[SSCtrl],VK_N,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectionLowerCase: SetResult2(VK_K,[SSCtrl],VK_O,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSelectionTabs2Spaces: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionEnclose: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionComment: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionUncomment: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecToggleComment: SetResult(VK_OEM_2, [ssCtrl],VK_UNKNOWN,[]);
  ecSelectionConditional: SetResult(VK_D, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelectionSort: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
  ecSelectionBreakLines: SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);

  ecBlockSetBegin   : SetResult2(VK_K,[ssCtrl],VK_B,[],  VK_K,[ssCtrl],VK_B,[ssCtrl]);
  ecBlockSetEnd     : SetResult2(VK_K,[ssCtrl],VK_K,[],  VK_K,[ssCtrl],VK_K,[ssCtrl]);
  ecBlockToggleHide : SetResult2(VK_K,[ssCtrl],VK_H,[],  VK_K,[ssCtrl],VK_H,[ssCtrl]);
  ecBlockHide       : SetResult2(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockShow       : SetResult2(VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBlockMove       : SetResult2(VK_K,[ssCtrl],VK_V,[],  VK_K,[ssCtrl],VK_V,[ssCtrl]);
  ecBlockCopy       : SetResult2(VK_K,[ssCtrl],VK_C,[],  VK_K,[ssCtrl],VK_C,[ssCtrl]);
  ecBlockDelete     : SetResult2(VK_K,[ssCtrl],VK_Y,[],  VK_K,[ssCtrl],VK_Y,[ssCtrl]);
  ecBlockGotoBegin  : SetResult2(VK_Q,[ssCtrl],VK_B,[],  VK_Q,[ssCtrl],VK_B,[ssCtrl]);
  ecBlockGotoEnd    : SetResult2(VK_Q,[ssCtrl],VK_K,[],  VK_Q,[ssCtrl],VK_K,[ssCtrl]);

// column mode selection
  ecColSelUp: SetResult(VK_UP, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelDown: SetResult(VK_DOWN, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelLeft: SetResult(VK_LEFT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelRight: SetResult(VK_RIGHT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageDown: SetResult(VK_NEXT, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageBottom: SetResult(VK_NEXT, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelPageUp: SetResult(VK_PRIOR, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelPageTop: SetResult(VK_PRIOR, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelLineStart: SetResult(VK_HOME, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelLineEnd: SetResult(VK_END, [ssAlt, ssShift], VK_UNKNOWN,[]);
  ecColSelEditorTop: SetResult(VK_HOME, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);
  ecColSelEditorBottom: SetResult(VK_END, [ssAlt, ssShift,ssCtrl], VK_UNKNOWN,[]);

  // editing
  ecBlockIndent: SetResult2(VK_I,[ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_I,[]);
  ecBlockUnindent: SetResult2(VK_U,[ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_U,[]);
  ecDeleteLastChar: SetResult(VK_BACK, [],VK_BACK, [ssShift]);  // ctrl H used for scroll window.
  ecDeleteChar: SetResult(VK_DELETE,[],VK_UNKNOWN,[]); // ctrl G conflicts with GO
  ecDeleteWord: SetResult(VK_DELETE,[ssAlt],VK_UNKNOWN,[]);
  ecDeleteLastWord: SetResult(VK_BACK,[ssCtrl],VK_UNKNOWN,[]);
  ecDeleteBOL: SetResult(VK_BACK,[ssMeta],VK_UNKNOWN,[]);
  ecDeleteEOL: SetResult(VK_DELETE,[ssMeta],VK_UNKNOWN,[]);
  ecDeleteLine: SetResult(VK_Y,[ssCtrl],VK_UNKNOWN,[]);
  ecClearAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecLineBreak: SetResult(VK_RETURN,[],VK_UNKNOWN,[]);
  ecInsertLine: SetResult(VK_N,[ssShift,ssMeta],VK_UNKNOWN,[]);
  ecInsertCharacter: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertGPLNotice: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertLGPLNotice: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertModifiedLGPLNotice: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertUserName: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertDateTime: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertChangeLogEntry: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSAuthor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSDate: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSHeader: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSID: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSLog: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSName: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSRevision: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertCVSSource: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInsertGUID: SetResult(VK_G, [ssCtrl,ssShift],VK_UNKNOWN,[]);

  // command commands
  ecUndo: SetResult(VK_Z,[ssMeta],VK_UNKNOWN,[]);
  ecRedo: SetResult(VK_Z,[ssMeta,ssShift],VK_UNKNOWN,[]);

  // search & replace
  ecMatchBracket: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFind: SetResult(VK_F,[ssMeta],VK_UNKNOWN,[]);
  ecFindNext: SetResult(VK_G,[ssMeta],VK_UNKNOWN,[]);
  ecFindPrevious: SetResult(VK_G,[ssShift,ssMeta],VK_UNKNOWN,[]);
  ecFindInFiles: SetResult(VK_F,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecReplace: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecIncrementalFind: SetResult(VK_E,[ssMeta],VK_UNKNOWN,[]);
  ecGotoLineNumber: SetResult(VK_L,[ssMeta],VK_UNKNOWN,[]);
  ecFindNextWordOccurrence: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindPrevWordOccurrence: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpBack: SetResult(VK_H,[ssCtrl],VK_UNKNOWN,[]);
  ecJumpForward: SetResult(VK_H,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecAddJumpPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewJumpHistory: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecJumpToPrevError: SetResult(VK_ADD,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecJumpToNextError: SetResult(VK_ADD,[ssMeta],VK_UNKNOWN,[]);
  ecOpenFileAtCursor: SetResult(VK_RETURN,[ssCtrl],VK_UNKNOWN,[]);
  ecProcedureList: SetResult(VK_G, [ssAlt],VK_UNKNOWN,[]);


  // marker
  ecSetFreeBookmark: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevBookmark: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextBookmark: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoMarker0: SetResult(VK_0,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker1: SetResult(VK_1,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker2: SetResult(VK_2,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker3: SetResult(VK_3,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker4: SetResult(VK_4,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker5: SetResult(VK_5,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker6: SetResult(VK_6,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker7: SetResult(VK_7,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker8: SetResult(VK_8,[ssCtrl],VK_UNKNOWN,[]);
  ecGotoMarker9: SetResult(VK_9,[ssCtrl],VK_UNKNOWN,[]);
  ecToggleMarker0: SetResult2(VK_0,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_0,[]);
  ecToggleMarker1: SetResult2(VK_1,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_1,[]);
  ecToggleMarker2: SetResult2(VK_2,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_2,[]);
  ecToggleMarker3: SetResult2(VK_3,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_3,[]);
  ecToggleMarker4: SetResult2(VK_4,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_4,[]);
  ecToggleMarker5: SetResult2(VK_5,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_5,[]);
  ecToggleMarker6: SetResult2(VK_6,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_6,[]);
  ecToggleMarker7: SetResult2(VK_7,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_7,[]);
  ecToggleMarker8: SetResult2(VK_8,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_8,[]);
  ecToggleMarker9: SetResult2(VK_9,[ssShift,ssCtrl],VK_UNKNOWN,[],VK_K,[SSCtrl],VK_9,[]);
  ecSetMarker0..ecSetMarker9: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // codetools
  ecAutoCompletion: SetResult(VK_J,[ssMeta],VK_UNKNOWN,[]);
  ecWordCompletion: SetResult(VK_SPACE,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecCompleteCode: SetResult(VK_C,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecIdentCompletion: SetResult(VK_SPACE,[ssCtrl],VK_UNKNOWN,[]);
  ecShowCodeContext: SetResult(VK_SPACE,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecExtractProc: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindIdentifierRefs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRenameIdentifier: SetResult(VK_E,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecInvertAssignment: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSyntaxCheck: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessUnclosedBlock: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGuessMisplacedIFDEF: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDFM2LFM: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCheckLFM: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiUnit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertDelphiPackage: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConvertEncoding: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindProcedureDefinition: SetResult(VK_UP,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  ecFindProcedureMethod: SetResult(VK_DOWN,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  ecFindDeclaration: SetResult(VK_UP,[ssAlt],VK_UNKNOWN,[]);
  ecFindBlockOtherEnd: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecFindBlockStart: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoIncludeDirective: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowAbstractMethods: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRemoveEmptyMethods: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // source notebook
  ecNextEditor: SetResult(VK_RIGHT, [ssMeta,ssAlt], VK_UNKNOWN, []);
  ecPrevEditor: SetResult(VK_LEFT, [ssMeta,ssAlt], VK_UNKNOWN, []);
  ecResetDebugger: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleBreakPoint: SetResult(VK_P,[ssCtrl],VK_UNKNOWN,[]);
  ecMoveEditorLeft: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorRight: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorLeftmost: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, []);
  ecMoveEditorRightmost: SetResult(VK_UNKNOWN, [], VK_UNKNOWN, []);

  ecNextSharedEditor:        SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevSharedEditor:        SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNextWindow:              SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPrevWindow:              SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNextWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorPrevWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMoveEditorNewWindow:     SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNextWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorPrevWindow:    SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCopyEditorNewWindow:     SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  ecGotoEditor1: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor2: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor3: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor4: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor5: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor6: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor7: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor8: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor9: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecGotoEditor0: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  ecLockEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  (*
  EcFoldLevel1: SetResult(VK_1,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel2: SetResult(VK_2,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel3: SetResult(VK_3,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel4: SetResult(VK_4,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel5: SetResult(VK_5,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel6: SetResult(VK_6,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel7: SetResult(VK_7,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel8: SetResult(VK_8,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel9: SetResult(VK_9,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldLevel0: SetResult(VK_0,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcFoldCurrent: SetResult(VK_OEM_PLUS,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcUnFoldCurrent: SetResult(VK_OEM_MINUS,[ssMeta,ssShift],VK_UNKNOWN,[]);
  EcToggleMarkupWord: SetResult(VK_M,[ssMeta],VK_UNKNOWN,[]);
  *)

  // file menu
  ecNew: SetResult(VK_N,[ssMeta],VK_UNKNOWN,[]);
  ecNewUnit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewForm: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpen: SetResult(VK_O,[ssMeta],VK_UNKNOWN,[]);
  ecRevert: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSave: SetResult(VK_S,[ssMeta],VK_UNKNOWN,[]);
  ecSaveAs: SetResult(VK_S,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecSaveAll: SetResult(VK_S,[ssMeta,ssAlt],VK_UNKNOWN,[]);
  ecClose: SetResult(VK_W,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecCloseAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCleanDirectory: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRestart: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // view menu
  ecToggleObjectInsp: SetResult(VK_I,[ssAlt,ssMeta],VK_UNKNOWN,[]);
  ecToggleSourceEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeExpl: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFPDocEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleMessages: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleSearchResults: SetResult(VK_F,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleWatches: SetResult(VK_W,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleBreakPoints: SetResult(VK_B,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleLocals: SetResult(VK_L,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleCallStack: SetResult(VK_S,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleRegisters: SetResult(VK_R,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleAssembler: SetResult(VK_D,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleDebugEvents: SetResult(VK_V,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecToggleDebuggerOut: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnits: SetResult(VK_U,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  ecViewForms: SetResult(VK_U,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecViewUnitDependencies: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewUnitInfo: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleFormUnit: SetResult(VK_F,[ssMeta,ssAlt],VK_UNKNOWN,[]);
  ecViewAnchorEditor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCodeBrowser: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleRestrictionBrowser: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleCompPalette: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecToggleIDESpeedBtns: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // project menu
  ecNewProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecNewProjectFromFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCloseProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecSaveProjectAs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPublishProject: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectInspector: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurUnitToProj: SetResult(VK_A,[ssAlt,ssMeta],VK_UNKNOWN,[]);
  ecRemoveFromProj: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecViewProjectSource: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecProjectOptions: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // run menu
  ecBuild: SetResult(VK_B,[ssMeta],VK_UNKNOWN,[]);
  ecBuildAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuickCompile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAbortBuild: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRun: SetResult(VK_R,[ssMeta],VK_UNKNOWN,[]);
  ecPause: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowExecutionPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepInto: SetResult(VK_R,[ssMeta,ssAlt],VK_UNKNOWN,[]);
  ecStepOver: SetResult(VK_R,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecStepOut: SetResult(VK_T,[ssMeta,ssShift],VK_UNKNOWN,[]);
  ecRunToCursor: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStopProgram: SetResult(VK_RETURN,[ssShift,ssMeta],VK_UNKNOWN,[]);
  ecRemoveBreakPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunParameters: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInspect: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEvaluate: SetResult(VK_E,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  ecAddWatch: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // components menu
  ecNewPackage: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackage: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOpenPackageOfCurUnit: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAddCurUnitToPkg: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecPackageGraph: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditInstallPkgs: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigCustomComps: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // tools menu
  ecExtToolSettings: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildLazarus: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildLazarus: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecMakeResourceString: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecDiff: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // environment menu
  ecEnvironmentOptions: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecEditCodeTemplates: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecCodeToolsDefinesEd: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRescanFPCSrcDir: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // help menu
  ecAboutLazarus: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecOnlineHelp: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecContextHelp: SetResult(VK_HELP,[],VK_UNKNOWN,[]);
  ecEditContextHelp: SetResult(VK_HELP,[ssShift,ssCtrl],VK_HELP,[ssCtrl]);
  ecReportingBug: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // designer
  ecDesignerCopy        : SetResult(VK_C,[ssMeta],VK_UNKNOWN,[]);
  ecDesignerCut         : SetResult(VK_X,[ssMeta],VK_UNKNOWN,[]);
  ecDesignerPaste       : SetResult(VK_V,[ssMeta],VK_UNKNOWN,[]);
  ecDesignerSelectParent: SetResult(VK_ESCAPE,[],VK_UNKNOWN,[]);
  ecDesignerMoveToFront : SetResult(VK_PRIOR,[ssShift],VK_UNKNOWN,[]);
  ecDesignerMoveToBack  : SetResult(VK_NEXT,[ssShift],VK_UNKNOWN,[]);
  ecDesignerForwardOne  : SetResult(VK_PRIOR,[ssMeta],VK_UNKNOWN,[]);
  ecDesignerBackOne     : SetResult(VK_NEXT,[ssMeta],VK_UNKNOWN,[]);

  else
    begin
      SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
      SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
      case TSynPluginTemplateEdit.ConvertCommandToBase(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetResult(VK_HOME, [],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetResult(VK_END,  [],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetResult(VK_A,    [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetResult(VK_RETURN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginTemplateEdit.ConvertCommandToBaseOff(Command) of
        // Edit template
        ecSynPTmplEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellRotate:      SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdNextCellSelRotate:   SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPTmplEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPTmplEdCellHome:            SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellEnd:             SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdCellSelect:          SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdFinish:              SetResult(VK_RETURN,[],VK_UNKNOWN,[]);
        ecSynPTmplEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBase(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetResult(VK_HOME, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetResult(VK_END,  [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetResult(VK_A,    [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseOff(Command) of
        // SyncroEdit
        ecSynPSyncroEdNextCell:            SetResult(VK_RIGHT,[ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdNextCellSel:         SetResult(VK_TAB,  [],      VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCell:            SetResult(VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
        ecSynPSyncroEdPrevCellSel:         SetResult(VK_TAB,  [ssShift],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellHome:            SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellEnd:             SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdCellSelect:          SetResult(VK_UNKNOWN, [],VK_UNKNOWN,[]);
        ecSynPSyncroEdEscape:              SetResult(VK_ESCAPE,[],VK_UNKNOWN,[]);
      end;
      case TSynPluginSyncroEdit.ConvertCommandToBaseSel(Command) of
        // SyncroEdit, during selection
        ecSynPSyncroEdStart:               SetResult(VK_J,[ssCtrl],VK_UNKNOWN,[]);
      end;
    end;
  end;
end;

procedure GetDefaultKeyForMacOSXLazScheme(Command: word; var TheKeyA, TheKeyB: TIDEShortCut);

  procedure SetResult(NewKeyA: word; NewShiftA: TShiftState;
    NewKeyB: word; NewShiftB: TShiftState);
  begin
    TheKeyA:=IDEShortCut(NewKeyA, NewShiftA, VK_UNKNOWN, []);
    TheKeyB:=IDEShortCut(NewKeyB, NewShiftB, VK_UNKNOWN, []);
  end;

  procedure SetResult2(
    NewKey1A: word; NewShift1A: TShiftState;
    NewKey1B: word; NewShift1B: TShiftState;
    NewKey2A: word; NewShift2A: TShiftState;
    NewKey2B: word; NewShift2B: TShiftState);
  begin
    TheKeyA:=IDEShortCut(NewKey1A,NewShift1A,NewKey1B,NewShift1B);
    TheKeyB:=IDEShortCut(NewKey2A,NewShift2A,NewKey2B,NewShift2B);
  end;

begin
  { First default to standard Mac OS X scheme }
  GetDefaultKeyForMacOSXScheme(Command, TheKeyA, TheKeyB);

  { Now override some entries }
  case Command of
  // moving
  ecLineStart: SetResult(VK_HOME, [],VK_LEFT,[ssMeta]);
  ecLineEnd: SetResult(VK_END, [],VK_RIGHT,[ssMeta]);
  ecEditorTop: SetResult(VK_UP,[ssMeta],VK_UNKNOWN,[]);
  ecEditorBottom: SetResult(VK_DOWN,[ssMeta],VK_UNKNOWN,[]);

  // selection
  ecSelLineStart: SetResult(VK_HOME, [ssShift],VK_LEFT,[ssMeta,ssShift]);
  ecSelLineEnd: SetResult(VK_END, [ssShift],VK_RIGHT,[ssMeta,ssShift]);
  ecSelEditorTop: SetResult(VK_HOME, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  ecSelEditorBottom: SetResult(VK_END, [ssShift,ssCtrl],VK_UNKNOWN,[]);

  // codetools
  ecRenameIdentifier: SetResult(VK_E, [ssShift,ssCtrl],VK_UNKNOWN,[]);

  // run menu
  ecBuild: SetResult(VK_F9,[ssCtrl],VK_F9,[ssCtrl,ssMeta]);
  ecBuildAll: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecQuickCompile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecAbortBuild: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRun: SetResult(VK_F9,[],VK_F9,[ssMeta]);
  ecPause: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecShowExecutionPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecStepInto: SetResult(VK_F7,[],VK_F7,[ssMeta]);
  ecStepOver: SetResult(VK_F8,[],VK_F8,[ssMeta]);
  ecStepOut: SetResult(VK_F8,[],VK_F8,[ssShift,ssMeta]);
  ecRunToCursor: SetResult(VK_F4,[],VK_F4,[ssMeta]);
  ecStopProgram: SetResult(VK_F2,[ssCtrl],VK_F2,[ssCtrl,ssMeta]);
  ecRemoveBreakPoint: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunParameters: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecBuildFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecRunFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecConfigBuildFile: SetResult(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  ecInspect: SetResult(VK_F5,[ssAlt],VK_UNKNOWN,[]);
  ecEvaluate: SetResult(VK_F7,[ssCtrl],VK_F7,[ssCtrl,ssMeta]);
  ecAddWatch: SetResult(VK_F5,[ssCtrl],VK_F5,[ssCtrl,ssMeta]);
  end;
end;

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

function CompareNameWithLoadedKeyCommand(NameAsAnsiString, Key: Pointer
  ): integer;
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
    ecSelectionUpperCase      : Result:= lismenuuppercaseselection;
    ecSelectionLowerCase      : Result:= lismenulowercaseselection;
    ecSelectionTabs2Spaces    : Result:= srkmecSelectionTabs2Spaces;
    ecSelectionEnclose        : Result:= lisMenuEncloseSelection;
    ecSelectionComment        : Result:= lismenucommentselection;
    ecSelectionUncomment      : Result:= lismenuuncommentselection;
    ecToggleComment           : Result:= lismenutogglecomment;
    ecSelectionConditional    : Result:= lisMenuConditionalSelection;
    ecSelectionSort           : Result:= lismenusortselection;
    ecSelectionBreakLines     : Result:= lisMenuBeakLinesInSelection;
    ecSelectToBrace           : Result:= lismenuselecttobrace;
    ecSelectCodeBlock         : Result:= lismenuselectcodeblock;
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

    // search menu
    ecFind                    : Result:= srkmecFind;
    ecFindNext                : Result:= srkmecFindNext;
    ecFindPrevious            : Result:= srkmecFindPrevious;
    ecFindInFiles             : Result:= srkmecFindInFiles;
    ecReplace                 : Result:= srkmecReplace;
    ecIncrementalFind         : Result:= lismenuincrementalfind;
    ecFindProcedureDefinition : Result:= srkmecFindProcedureDefinition;
    ecFindProcedureMethod     : Result:= srkmecFindProcedureMethod;
    ecGotoLineNumber          : Result:= srkmecGotoLineNumber;
    ecFindNextWordOccurrence  : Result:= srkmecFindNextWordOccurrence;
    ecFindPrevWordOccurrence  : Result:= srkmecFindPrevWordOccurrence;
    ecJumpBack                : Result:= lismenujumpback;
    ecJumpForward             : Result:= lismenujumpforward;
    ecAddJumpPoint            : Result:= srkmecAddJumpPoint;
    ecViewJumpHistory         : Result:= lismenuviewjumphistory;
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
    ecToggleCallStack         : Result:= srkmecToggleCallStack;
    ecToggleRegisters         : Result:= srkmecToggleRegisters;
    ecToggleAssembler         : Result:= srkmecToggleAssembler;
    ecViewUnits               : Result:= srkmecViewUnits;
    ecViewForms               : Result:= srkmecViewForms;
    ecViewUnitDependencies    : Result:= srkmecViewUnitDependencies;
    ecViewUnitInfo            : Result:= srkmecViewUnitInfo;
    ecViewAnchorEditor        : Result:= srkmecViewAnchorEditor;
    ecToggleCodeBrowser       : Result:= srkmecToggleCodeBrowser;
    ecToggleRestrictionBrowser: Result:= srkmecToggleRestrictionBrowser;
    ecViewComponents          : Result:= srkmecViewComponents;
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
    ecViewProjectSource       : Result:= lisMenuViewSource;
    ecProjectOptions          : Result:= lisMenuProjectOptions;

    // run menu (menu string resource)
    ecBuild                   : Result:= srkmecBuild;
    ecBuildAll                : Result:= srkmecBuildAll;
    ecQuickCompile            : Result:= srkmecQuickCompile;
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

    // components menu
    ecNewPackage              : Result:= lisKMNewPackage;
    ecOpenPackage             : Result:= lisMenuOpenPackage;
    ecOpenPackageFile         : Result:= lisMenuOpenPackageFile;
    ecOpenPackageOfCurUnit    : Result:= lisMenuOpenPackageOfCurUnit;
    ecAddCurUnitToPkg         : Result:= lisMenuAddCurUnitToPkg;
    ecPackageGraph            : Result:= lisMenuPackageGraph;
    ecEditInstallPkgs         : Result:= lisMenuEditInstallPkgs;
    ecConfigCustomComps       : Result:= lisMenuConfigCustomComps;

    // tools menu
    ecExtToolSettings         : Result:= srkmecExtToolSettings;
    ecConfigBuildLazarus      : Result:= lismenuconfigurebuildlazarus;
    ecBuildLazarus            : Result:= srkmecBuildLazarus;
    ecExtToolFirst
    ..ecExtToolLast           : Result:= Format(srkmecExtTool,[cmd-ecExtToolFirst+1]);
    ecMakeResourceString      : Result:= srkmecMakeResourceString;
    ecDiff                    : Result:= srkmecDiff;

    // environment menu
    ecEnvironmentOptions      : Result:= srkmecEnvironmentOptions;
    ecEditCodeTemplates       : Result:= lisMenuEditCodeTemplates;
    ecCodeToolsDefinesEd      : Result:= srkmecCodeToolsDefinesEd;
    ecRescanFPCSrcDir         : Result:= lisMenuRescanFPCSourceDirectory;

    // help menu
    ecAboutLazarus            : Result:= lisAboutLazarus;
    ecOnlineHelp              : Result:= lisMenuOnlineHelp;
    ecContextHelp             : Result:= lisMenuContextHelp;
    ecEditContextHelp         : Result:= lisMenuEditContextHelp;
    ecReportingBug            : Result:= srkmecReportingBug;

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
  begin
    if (ShortCut1.Key1=VK_UNKNOWN) then exit;
    if (ShortCut1.Key1<>ShortCut2.Key1) or (ShortCut1.Shift1<>ShortCut2.Shift1)
    then exit;
    if ((ShortCut1.Key2=ShortCut2.Key2) and (ShortCut1.Shift2=ShortCut2.Shift2))
        or (ShortCut1.Key2=VK_UNKNOWN) or (ShortCut2.Key2=VK_UNKNOWN)
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
        Add(srkmCommand1+Key1.Category.Description+' '
          +EditorCommandToDescriptionString(Key1.Command)+'"'
          +'->'+KeyAndShiftStateToEditorKeyString(ShortCut1));
        Add(srkmConflicW);
        Add(srkmCommand2+Key2.Category.Description+' '
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

function TKeyCommandRelation.GetLocalizedName: string;
begin
  Result:=inherited GetLocalizedName;
  if Result='' then
    Result:=EditorCommandLocalizedName(Command,Name);
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
var
  C: TIDECommandCategory;
  o: LongInt;
begin
  Clear;

  // create default keymapping

  // moving
  C:=Categories[AddCategory('CursorMoving',srkmCatCursorMoving,
                IDECmdScopeSrcEditOnly)];
  //p:=Relations[Add(C,'Name1',12000,IDEShortCut(vk_P,[ssShift,ssAlt],VK_UNKNOWN,[]),CLeanIDEShortCut)];
  //debugln('TKeyCommandRelationList.Add A ',p.Name,' ',KeyAndShiftStateToEditorKeyString(p.ShortcutA),' ',dbgs(p));
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
  AddDefault(C, 'Move cursor to bottom of page', srkmecPageBottom, ecPageBottom
    );
  AddDefault(C, 'Move cursor to absolute beginning', srkmecEditorTop,
    ecEditorTop);
  AddDefault(C, 'Move cursor to absolute end', srkmecEditorBottom,
    ecEditorBottom);
  AddDefault(C, 'Scroll up one line', srkmecScrollUp, ecScrollUp);
  AddDefault(C, 'Scroll down one line', srkmecScrollDown, ecScrollDown);
  AddDefault(C, 'Scroll left one char', srkmecScrollLeft, ecScrollLeft);
  AddDefault(C, 'Scroll right one char', srkmecScrollRight, ecScrollRight);

  // selection
  C:=Categories[AddCategory('Selection',srkmCatSelection,
                IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Copy selection to clipboard', srkmecCopy, ecCopy);
  AddDefault(C, 'Cut selection to clipboard', srkmecCut, ecCut);
  AddDefault(C, 'Paste clipboard to current position', srkmecPaste, ecPaste);
  AddDefault(C, 'Normal selection mode', srkmecNormalSelect, ecNormalSelect);
  AddDefault(C, 'Column selection mode', srkmecColumnSelect, ecColumnSelect);
  AddDefault(C, 'Line selection mode', srkmecLineSelect, ecLineSelect);
  AddDefault(C, 'Indent block', srkmecBlockIndent, ecBlockIndent);
  AddDefault(C, 'Unindent block', srkmecBlockUnindent, ecBlockUnindent);
  AddDefault(C, 'Uppercase selection', lisMenuUpperCaseSelection,
    ecSelectionUpperCase);
  AddDefault(C, 'Lowercase selection', lisMenuLowerCaseSelection,
    ecSelectionLowerCase);
  AddDefault(C, 'Convert tabs to spaces in selection',
    srkmecSelectionTabs2Spaces, ecSelectionTabs2Spaces);
  AddDefault(C, 'Enclose selection', lisKMEncloseSelection, ecSelectionEnclose);
  AddDefault(C, 'Comment selection', lisMenuCommentSelection, ecSelectionComment
    );
  AddDefault(C, 'Uncomment selection', lisMenuUncommentSelection,
    ecSelectionUncomment);
  AddDefault(C, 'Toggle comment', lisMenuToggleComment, ecToggleComment
    );
  AddDefault(C, 'Sort selection', lisSortSelSortSelection, ecSelectionSort);
  AddDefault(C, 'Break Lines in selection', lisMenuBeakLinesInSelection,
    ecSelectionBreakLines);
  AddDefault(C, 'Select word left', lisKMSelectWordLeft, ecSelWordLeft);
  AddDefault(C, 'Select word right', lisKMSelectWordRight, ecSelWordRight);
  AddDefault(C, 'Select line start', lisKMSelectLineStart, ecSelLineStart);
  AddDefault(C, 'Select to text start in line', srkmecSelLineTextStart, ecSelLineTextStart);
  AddDefault(C, 'Select line end', lisKMSelectLineEnd, ecSelLineEnd);
  AddDefault(C, 'Select page top', lisKMSelectPageTop, ecSelPageTop);
  AddDefault(C, 'Select page bottom', lisKMSelectPageBottom, ecSelPageBottom);
  AddDefault(C, 'Select to absolute beginning', srkmecSelEditorTop,
    ecSelEditorTop);
  AddDefault(C, 'Select to absolute end', srkmecSelEditorBottom,
    ecSelEditorBottom);
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
  C:=Categories[AddCategory('Column Selection',srkmCatColSelection,
                IDECmdScopeSrcEditOnly)];
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
  AddDefault(C, 'Delete to start of word', srkmecDeleteLastWord,
    ecDeleteLastWord);
  AddDefault(C, 'Delete to beginning of line', srkmecDeleteBOL, ecDeleteBOL);
  AddDefault(C, 'Delete to end of line', srkmecDeleteEOL, ecDeleteEOL);
  AddDefault(C, 'Delete current line', srkmecDeleteLine, ecDeleteLine);
  AddDefault(C, 'Delete whole text', srkmecClearAll, ecClearAll);
  AddDefault(C, 'Break line and move cursor', srkmecLineBreak, ecLineBreak);
  AddDefault(C, 'Break line, leave cursor', srkmecInsertLine, ecInsertLine);
  AddDefault(C, 'Insert $IFDEF', lisKMInsertIFDEF, ecSelectionConditional);
  AddDefault(C, 'Insert from Character Map', lisMenuInsertCharacter,
    ecInsertCharacter);
  AddDefault(C, 'Insert GPL notice', srkmecInsertGPLNotice, ecInsertGPLNotice);
  AddDefault(C, 'Insert LGPL notice', srkmecInsertLGPLNotice, ecInsertLGPLNotice
    );
  AddDefault(C, 'Insert modified LGPL notice', srkmecInsertModifiedLGPLNotice,
    ecInsertModifiedLGPLNotice);
  AddDefault(C, 'Insert username', lisKMInsertUsername, ecInsertUserName);
  AddDefault(C, 'Insert date and time', lisKMInsertDateAndTime, ecInsertDateTime
    );
  AddDefault(C, 'Insert ChangeLog entry', srkmecInsertChangeLogEntry,
    ecInsertChangeLogEntry);
  AddDefault(C, 'Insert CVS keyword Author', srkmecInsertCVSAuthor,
    ecInsertCVSAuthor);
  AddDefault(C, 'Insert CVS keyword Date', srkmecInsertCVSDate, ecInsertCVSDate
    );
  AddDefault(C, 'Insert CVS keyword Header', srkmecInsertCVSHeader,
    ecInsertCVSHeader);
  AddDefault(C, 'Insert CVS keyword ID', srkmecInsertCVSID, ecInsertCVSID);
  AddDefault(C, 'Insert CVS keyword Log', srkmecInsertCVSLog, ecInsertCVSLog);
  AddDefault(C, 'Insert CVS keyword Name', srkmecInsertCVSName, ecInsertCVSName
    );
  AddDefault(C, 'Insert CVS keyword Revision', srkmecInsertCVSRevision,
    ecInsertCVSRevision); ;
  AddDefault(C, 'Insert CVS keyword Source', srkmecInsertCVSSource,
    ecInsertCVSSource);
  AddDefault(C, 'Insert a GUID',srkmecInsertGUID, ecInsertGUID);

  // command commands
  C:=Categories[AddCategory('CommandCommands',srkmCatCmdCmd,nil)];
  AddDefault(C, 'Undo', lisMenuUndo, ecUndo);
  AddDefault(C, 'Redo', lisMenuRedo, ecRedo);

  // search & replace
  C:=Categories[AddCategory('SearchReplace',srkmCatSearchReplace,
                IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Go to matching bracket', srkmecMatchBracket, ecMatchBracket);
  AddDefault(C, 'Find text', srkmecFind, ecFind);
  AddDefault(C, 'Find next', srkmecFindNext, ecFindNext);
  AddDefault(C, 'Find previous', srkmecFindPrevious, ecFindPrevious);
  AddDefault(C, 'Find in files', srkmecFindInFiles, ecFindInFiles);
  AddDefault(C, 'Replace text', srkmecReplace, ecReplace);
  AddDefault(C, 'Find incremental', lisKMFindIncremental, ecIncrementalFind);
  AddDefault(C, 'Go to line number', srkmecGotoLineNumber, ecGotoLineNumber);
  AddDefault(C, 'Find next word occurrence', srkmecFindNextWordOccurrence,
    ecFindNextWordOccurrence);
  AddDefault(C, 'Find previous word occurrence', srkmecFindPrevWordOccurrence,
    ecFindPrevWordOccurrence);
  AddDefault(C, 'Jump back', lisMenuJumpBack, ecJumpBack);
  AddDefault(C, 'Jump forward', lisMenuJumpForward, ecJumpForward);
  AddDefault(C, 'Add jump point', srkmecAddJumpPoint, ecAddJumpPoint);
  AddDefault(C, 'View jump history', lisKMViewJumpHistory, ecViewJumpHistory);
  AddDefault(C, 'Jump to next error', lisMenuJumpToNextError, ecJumpToNextError
    );
  AddDefault(C, 'Jump to previous error', lisMenuJumpToPrevError,
    ecJumpToPrevError);
  AddDefault(C, 'Open file at cursor', srkmecOpenFileAtCursor,
    ecOpenFileAtCursor);
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
  C:=Categories[AddCategory('CodeTools',srkmCatCodeTools,IDECmdScopeSrcEditOnly)];
  AddDefault(C, 'Code template completion', srkmecAutoCompletion,
    ecAutoCompletion);
  AddDefault(C, 'Word completion', srkmecWordCompletion, ecWordCompletion);
  AddDefault(C, 'Complete code', srkmecCompletecode, ecCompleteCode);
  AddDefault(C, 'Identifier completion', dlgEdIdComlet, ecIdentCompletion);
  AddDefault(C, 'Rename identifier', srkmecRenameIdentifier, ecRenameIdentifier);
  AddDefault(C, 'Find identifier references', srkmecFindIdentifierRefs,
    ecFindIdentifierRefs);
  AddDefault(C, 'Show code context', srkmecShowCodeContext, ecShowCodeContext);
  AddDefault(C, 'Extract proc', srkmecExtractProc, ecExtractProc);
  AddDefault(C, 'Invert assignment', srkmecInvertAssignment, ecInvertAssignment);
  AddDefault(C, 'Syntax check', srkmecSyntaxCheck, ecSyntaxCheck);
  AddDefault(C, 'Guess unclosed block', lisMenuGuessUnclosedBlock,
    ecGuessUnclosedBlock);
  AddDefault(C, 'Guess misplaced $IFDEF', srkmecGuessMisplacedIFDEF,
    ecGuessMisplacedIFDEF);
  AddDefault(C, 'Check LFM file in editor', lisMenuCheckLFM, ecCheckLFM);
  AddDefault(C, 'Find procedure definiton', srkmecFindProcedureDefinition,
    ecFindProcedureDefinition);
  AddDefault(C, 'Find procedure method', srkmecFindProcedureMethod,
    ecFindProcedureMethod);
  AddDefault(C, 'Find declaration', srkmecFindDeclaration, ecFindDeclaration);
  AddDefault(C, 'Find block other end', srkmecFindBlockOtherEnd,
    ecFindBlockOtherEnd);
  AddDefault(C, 'Find block start', srkmecFindBlockStart, ecFindBlockStart);
  AddDefault(C, 'Goto include directive', lisMenuGotoIncludeDirective,
    ecGotoIncludeDirective);
  AddDefault(C, 'Show abstract methods', srkmecShowAbstractMethods,
    ecShowAbstractMethods);
  AddDefault(C, 'Remove empty methods', srkmecRemoveEmptyMethods,
    ecRemoveEmptyMethods);
  AddDefault(C, 'Remove unused units', srkmecRemoveUnusedUnits,
    ecRemoveUnusedUnits);
  AddDefault(C, 'Find overloads', srkmecFindOverloads,
    ecFindOverloads);

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
  C:=Categories[AddCategory('SourceNotebook',srkmCatSrcNoteBook,
                IDECmdScopeSrcEdit)];
  AddDefault(C, 'Go to next editor', srkmecNextEditor, ecNextEditor);
  AddDefault(C, 'Go to prior editor', srkmecPrevEditor, ecPrevEditor);
  AddDefault(C, 'Add break point', srkmecToggleBreakPoint, ecToggleBreakPoint);
  AddDefault(C, 'Remove break point', srkmecRemoveBreakPoint, ecRemoveBreakPoint
    );
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
  AddDefault(C, 'Toggle view Object Inspector', lisKMToggleViewObjectInspector,
    ecToggleObjectInsp);
  AddDefault(C, 'Toggle view Source Editor', lisKMToggleViewSourceEditor,
    ecToggleSourceEditor);
  AddDefault(C, 'Toggle view Code Explorer', lisKMToggleViewCodeExplorer,
    ecToggleCodeExpl);
  AddDefault(C, 'Toggle view Documentation Editor',
    lisKMToggleViewDocumentationEditor, ecToggleFPDocEditor);
  AddDefault(C, 'Toggle view Messages', lisKMToggleViewMessages,
    ecToggleMessages);
  AddDefault(C, 'Toggle view Search Results', lisKMToggleViewSearchResults,
    ecToggleSearchResults);
  AddDefault(C, 'Toggle view Watches', lisKMToggleViewWatches, ecToggleWatches);
  AddDefault(C, 'Toggle view Breakpoints', lisKMToggleViewBreakpoints,
    ecToggleBreakPoints);
  AddDefault(C, 'Toggle view Local Variables', lisKMToggleViewLocalVariables,
    ecToggleLocals);
  AddDefault(C, 'Toggle view Call Stack', lisKMToggleViewCallStack,
    ecToggleCallStack);
  AddDefault(C, 'Toggle view Registers', lisKMToggleViewRegisters,
    ecToggleRegisters);
  AddDefault(C, 'Toggle view Assembler', lisKMToggleViewAssembler,
    ecToggleAssembler);
  AddDefault(C, 'Toggle view Event Log', lisKMToggleViewDebugEvents, ecToggleDebugEvents);
  AddDefault(C, 'Toggle view Debugger Output', lisKMToggleViewDebuggerOutput,
    ecToggleDebuggerOut);
  AddDefault(C, 'View Units', lisHintViewUnits, ecViewUnits);
  AddDefault(C, 'View Forms', lisHintViewForms, ecViewForms);
  AddDefault(C, 'View Unit Dependencies', lisMenuViewUnitDependencies,
    ecViewUnitDependencies);
  AddDefault(C, 'View Unit Info', lisKMViewUnitInfo, ecViewUnitInfo);
  AddDefault(C, 'Toggle between Unit and Form', lisKMToggleBetweenUnitAndForm,
    ecToggleFormUnit);
  AddDefault(C, 'View Anchor Editor', lisMenuViewAnchorEditor,
    ecViewAnchorEditor);
  AddDefault(C, 'Toggle view component palette',
    lisKMToggleViewComponentPalette, ecToggleCompPalette);
  AddDefault(C, 'Toggle view IDE speed buttons',
    lisKMToggleViewIDESpeedButtons, ecToggleIDESpeedBtns);

  // project menu
  C:=Categories[AddCategory('ProjectMenu',srkmCatProjectMenu,nil)];
  AddDefault(C, 'New project', lisKMNewProject, ecNewProject);
  AddDefault(C, 'New project from file', lisKMNewProjectFromFile,
    ecNewProjectFromFile);
  AddDefault(C, 'Open project', lisOpenProject2, ecOpenProject);
  AddDefault(C, 'Close project', lisKMCloseProject, ecCloseProject);
  AddDefault(C, 'Save project', lisKMSaveProject, ecSaveProject);
  AddDefault(C, 'Save project as', lisKMSaveProjectAs, ecSaveProjectAs);
  AddDefault(C, 'Publish project', lisKMPublishProject, ecPublishProject);
  AddDefault(C, 'Project Inspector', lisMenuProjectInspector, ecProjectInspector
    );
  AddDefault(C, 'Add active unit to project', lisKMAddActiveUnitToProject,
    ecAddCurUnitToProj);
  AddDefault(C, 'Remove active unit from project',
    lisKMRemoveActiveUnitFromProject, ecRemoveFromProj);
  AddDefault(C, 'View project source', lisKMViewProjectSource,
    ecViewProjectSource);
  AddDefault(C, 'View project options', lisKMViewProjectOptions,
    ecProjectOptions);

  // run menu
  C:=Categories[AddCategory('RunMenu',srkmCatRunMenu,nil)];
  AddDefault(C, 'Build project/program', lisKMBuildProjectProgram, ecBuild);
  AddDefault(C, 'Build all files of project/program',
    lisKMBuildAllFilesOfProjectProgram, ecBuildAll);
  AddDefault(C, 'Quick compile, no linking', lisKMQuickCompileNoLinking,
    ecQuickCompile);
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
  AddDefault(C, 'Config "Build File"', Format(lisKMConfigBuildFile, ['"', '"']
    ), ecConfigBuildFile);
  AddDefault(C, 'Inspect', lisKMInspect, ecInspect);
  AddDefault(C, 'Evaluate/Modify', lisKMEvaluateModify, ecEvaluate);
  AddDefault(C, 'Add watch', lisKMAddWatch, ecAddWatch);

  // components menu
  C:=Categories[AddCategory('Components',srkmCatPackageMenu,nil)];
  AddDefault(C, 'New package', lisKMNewPackage, ecNewPackage);
  AddDefault(C, 'Open package', lisCompPalOpenPackage, ecOpenPackage);
  AddDefault(C, 'Open package file', lisKMOpenPackageFile, ecOpenPackageFile);
  AddDefault(C, 'Open package of current unit', lisMenuOpenPackageOfCurUnit,
    ecOpenPackageOfCurUnit);
  AddDefault(C, 'Add active unit to a package', lisMenuAddCurUnitToPkg,
    ecAddCurUnitToPkg);
  AddDefault(C, 'Package graph', lisKMPackageGraph, ecPackageGraph);
  AddDefault(C, 'Configure installed packages',
    lisKMConfigureInstalledPackages, ecEditInstallPkgs);
  AddDefault(C, 'Configure custom components', lisKMConfigureCustomComponents,
    ecConfigCustomComps);

  // tools menu
  C:=Categories[AddCategory(CommandCategoryToolMenuName,srkmCatToolMenu,nil)];
  AddDefault(C, 'External Tools settings', lisKMExternalToolsSettings,
    ecExtToolSettings);
  AddDefault(C, 'Build Lazarus', lisMenuBuildLazarus, ecBuildLazarus);
  AddDefault(C, 'Configure "Build Lazarus"', Format(lisConfigureBuildLazarus, [
    '"', '"']), ecConfigBuildLazarus);
  AddDefault(C, 'Make resource string', srkmecMakeResourceString,
    ecMakeResourceString);
  AddDefault(C, 'Diff editor files', lisKMDiffEditorFiles, ecDiff);
  AddDefault(C, 'Convert DFM file to LFM', lisKMConvertDFMFileToLFM,
    ecConvertDFM2LFM);
  AddDefault(C, 'Convert Delphi unit to Lazarus unit',
    lisKMConvertDelphiUnitToLazarusUnit, ecConvertDelphiUnit);
  AddDefault(C, 'Convert Delphi project to Lazarus project',
    lisKMConvertDelphiProjectToLazarusProject, ecConvertDelphiProject);
  AddDefault(C, 'Convert Delphi package to Lazarus package',
    lisKMConvertDelphiPackageToLazarusPackage, ecConvertDelphiPackage);
  AddDefault(C, 'Convert encoding',
    lisConvertEncodingOfProjectsPackages, ecConvertEncoding);

  // environment menu
  C:=Categories[AddCategory('EnvironmentMenu',srkmCatEnvMenu,nil)];
  AddDefault(C, 'General environment options', srkmecEnvironmentOptions,
    ecEnvironmentOptions);
  AddDefault(C, 'Edit Code Templates', lisKMEditCodeTemplates,
    ecEditCodeTemplates);
  AddDefault(C, 'CodeTools defines editor', lisKMCodeToolsDefinesEditor,
    ecCodeToolsDefinesEd);
  AddDefault(C, 'Rescan FPC source directory', lisMenuRescanFPCSourceDirectory,
    ecRescanFPCSrcDir);

  // help menu
  C:=Categories[AddCategory('HelpMenu',srkmCarHelpMenu,nil)];
  AddDefault(C, 'About Lazarus', lisAboutLazarus, ecAboutLazarus);
  AddDefault(C, 'Online Help', lisMenuOnlineHelp, ecOnlineHelp);
  AddDefault(C, 'Context sensitive help', lisKMContextSensitiveHelp,
    ecContextHelp);
  AddDefault(C, 'Edit context sensitive help', lisKMEditContextSensitiveHelp,
    ecEditContextHelp);
  AddDefault(C, 'Reporting a bug', srkmecReportingBug, ecReportingBug);

  // designer  - without menu items in the IDE bar (at least not directly)
  C:=Categories[AddCategory('Designer',lisKeyCatDesigner,IDECmdScopeDesignerOnly)];
  AddDefault(C, 'Copy selected Components to clipboard',
    lisKMCopySelectedComponentsToClipboard, ecDesignerCopy);
  AddDefault(C, 'Cut selected Components to clipboard',
    lisKMCutSelectedComponentsToClipboard, ecDesignerCut);
  AddDefault(C, 'Paste Components from clipboard',
    lisKMPasteComponentsFromClipboard, ecDesignerPaste);
  AddDefault(C, 'Select parent component', lisDsgSelectParentComponent,
    ecDesignerSelectParent);
  AddDefault(C, 'Move component to front', lisDsgOrderMoveToFront,
    ecDesignerMoveToFront);
  AddDefault(C, 'Move component to back', lisDsgOrderMoveToBack,
    ecDesignerMoveToBack);
  AddDefault(C, 'Move component one forward', lisDsgOrderForwardOne,
    ecDesignerForwardOne);
  AddDefault(C, 'Move component one back', lisDsgOrderBackOne, ecDesignerBackOne
    );

  // object inspector - without menu items in the IDE bar (at least no direct)
  C:=Categories[AddCategory('Object Inspector',lisKeyCatObjInspector,
                            IDECmdScopeObjectInspectorOnly)];

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

function TKeyCommandRelationList.GetRelation(
  Index:integer):TKeyCommandRelation;
begin
  if (Index<0) or (Index>=Count) then
  begin
    DebugLn('[TKeyCommandRelationList.GetRelation] Index out of bounds '
      ,IntToStr(Index),' Count=',IntToStr(Count));
    // creates an exception, that gdb catches:
    if (Index div ((Index and 1) div 10000))=0 then ;
  end;
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

function TKeyCommandRelationList.Add(Category: TIDECommandCategory;
  const Name, LocalizedName: string;
  Command:word; TheKeyA, TheKeyB: TIDEShortCut;
  const OnExecuteMethod: TNotifyEvent;
  const OnExecuteProc: TNotifyProcedure):integer;
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
    // old key, values were loaded
    // (key is registered after loading keymapping)
    TheKeyA:=LoadedKey.ShortcutA;
    TheKeyB:=LoadedKey.ShortcutB;
  end;
  Result:=FRelations.Add(TKeyCommandRelation.Create(Category,Name,LocalizedName,
                        Command,TheKeyA,TheKeyB,OnExecuteMethod,OnExecuteProc));
end;

function TKeyCommandRelationList.AddDefault(Category: TIDECommandCategory;
  const Name, LocalizedName: string; Command: word): integer;
var
  TheKeyA, TheKeyB: TIDEShortCut;
begin
  GetDefaultKeyForCommand(Command,TheKeyA,TheKeyB);
  {if Command=ecBlockIndent then begin
    debugln('TKeyCommandRelationList.AddDefault A ',KeyAndShiftStateToEditorKeyString(TheKeyA),' ',KeyAndShiftStateToEditorKeyString(TheKeyB));
  end;}
  Result:=Add(Category,Name,LocalizedName,Command,TheKeyA,TheKeyB);
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
      Add(ExtToolCat,ToolName,ToolName,
           ecExtToolFirst+fExtToolCount,CleanIDEShortCut,CleanIDEShortCut);
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

function TKeyCommandRelationList.FindIDECommand(ACommand: word
  ): TIDECommand;
begin
  Result:=FindByCommand(ACommand);
end;

function TKeyCommandRelationList.FindByCommand(
  ACommand: word):TKeyCommandRelation;
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
      // add missing keys
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
      // add
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
  CurRelation: TKeyCommandRelation;
  NewScheme: TKeyMapScheme;
  TheKeyA, TheKeyB: TIDEShortCut;
begin
  NewScheme:=KeySchemeNameToSchemeType(SchemeName);
  // set all keys to new scheme
  for i:=0 to Count-1 do begin
    CurRelation:=Relations[i];
    case NewScheme of
    kmsLazarus: GetDefaultKeyForCommand(CurRelation.Command,TheKeyA,TheKeyB);
    kmsClassic: GetDefaultKeyForClassicScheme(CurRelation.Command,
                                              TheKeyA,TheKeyB);
    kmsMacOSXApple: GetDefaultKeyForMacOSXScheme(CurRelation.Command,TheKeyA,TheKeyB);
    kmsMacOSXLaz: GetDefaultKeyForMacOSXLazScheme(CurRelation.Command,TheKeyA,TheKeyB);
    kmsCustom: ;
    end;
    CurRelation.ShortcutA:=TheKeyA;
    CurRelation.ShortcutB:=TheKeyB;
  end;
end;

function TKeyCommandRelationList.CreateUniqueCategoryName(const AName: string
  ): string;
begin
  Result:=AName;
  if FindCategoryByName(Result)=nil then exit;
  Result:=CreateFirstIdentifier(Result);
  while FindCategoryByName(Result)<>nil do
    Result:=CreateNextIdentifier(Result);
end;

function TKeyCommandRelationList.CreateUniqueCommandName(const AName: string
  ): string;
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
  Result:=Categories[
                AddCategory(CreateUniqueCategoryName(AName),Description,Scope)];
end;

function TKeyCommandRelationList.CreateCommand(Category: TIDECommandCategory;
  const AName, Description: string; const TheShortcutA,
  TheShortcutB: TIDEShortCut;
  const OnExecuteMethod: TNotifyEvent;
  const OnExecuteProc: TNotifyProcedure): TIDECommand;
var
  NewName: String;
begin
  NewName:=CreateUniqueCommandName(AName);
  Result:=Relations[Add(Category as TKeyCommandCategory,
                        NewName,Description,
                        CreateNewCommandID,TheShortcutA,TheShortcutB,
                        OnExecuteMethod,OnExecuteProc)];
end;

function TKeyCommandRelationList.GetCategory(Index: integer
  ): TIDECommandCategory;
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

function TKeyCommandRelationList.Add(Category: TIDECommandCategory;
  Command: TIDECommand): integer;
begin
  Result:=Add(Category,
              Command.Name,Command.LocalizedName,
              Command.Command,Command.ShortcutA,Command.ShortcutB,
              Command.OnExecute,Command.OnExecuteProc);
  //if Command.Command=12000 then
  //  debugln('TKeyCommandRelationList.Add A ',Command.Name,' ',KeyAndShiftStateToEditorKeyString(Command.ShortcutA),' ',KeyAndShiftStateToEditorKeyString(Relations[Result].ShortcutA),' ',dbgs(Command));
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

function TKeyCommandRelationList.IndexOf(ARelation: TKeyCommandRelation
  ): integer;
begin
  Result:=fRelations.IndexOf(ARelation);
end;

function TKeyCommandRelationList.CommandToShortCut(ACommand: word
  ): TShortCut;
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

