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
  LCLLinux, LCLType,
  Forms, Classes, SysUtils, Buttons, LResources, StdCtrls, Controls,
  SynEdit, SynEditKeyCmds, Laz_XMLCfg, Dialogs, StringHashList,
  LazarusIDEStrConsts;

const
  { editor commands constants. see syneditkeycmds.pp for more
  
   These values can change from version to version, so DO NOT save them to file!
  
  }
  ecNone                 = SynEditKeyCmds.ecNone;
  
  ecFind                 = ecUserFirst + 1;
  ecFindAgain            = ecUserFirst + 2;
  ecFindNext             = ecFindAgain;
  ecReplace              = ecUserFirst + 3;
  ecIncrementalFind      = ecUserFirst + 4;
  ecFindProcedureDefinition = ecUserFirst + 5;
  ecFindProcedureMethod  = ecUserFirst + 6;
  ecGotoLineNumber       = ecUserFirst + 7;

  ecNextEditor           = ecUserFirst + 8;
  ecPrevEditor           = ecUserFirst + 9;
  ecMoveEditorLeft       = ecUserFirst + 10;
  ecMoveEditorRight      = ecUserFirst + 11;

  ecPeriod               = ecUserFirst + 12;

  ecFindPrevious         = ecUserFirst + 13;
  ecFindInFiles          = ecUserFirst + 14;
  ecJumpBack             = ecUserFirst + 15;
  ecJumpForward          = ecUserFirst + 16;
  ecAddJumpPoint         = ecUserFirst + 17;
  ecViewJumpHistory      = ecUserFirst + 18;

  ecFindDeclaration      = ecUserFirst + 20;
  ecFindBlockOtherEnd    = ecUserFirst + 21;
  ecFindBlockStart       = ecUserFirst + 22;
  ecOpenFileAtCursor     = ecUserFirst + 23;
  ecGotoIncludeDirective = ecUserFirst + 24;

  ecSelectionUpperCase   = ecUserFirst + 50;
  ecSelectionLowerCase   = ecUserFirst + 51;
  ecSelectionTabs2Spaces = ecUserFirst + 52;
  ecSelectionComment     = ecUserFirst + 53;
  ecSelectionUncomment   = ecUserFirst + 54;
  ecSelectionSort        = ecUserFirst + 55;
  ecSelectToBrace        = ecUserFirst + 56;
  ecSelectCodeBlock      = ecUserFirst + 57;
  ecSelectLine           = ecUserFirst + 58;
  ecSelectParagraph      = ecUserFirst + 59;
  
  ecInsertGPLNotice      = ecUserFirst + 80;
  ecInsertLGPLNotice     = ecUserFirst + 81;
  ecInsertUserName       = ecUserFirst + 82;
  ecInsertDateTime       = ecUserFirst + 83;
  ecInsertChangeLogEntry = ecUserFirst + 84;
  ecInsertCVSAuthor      = ecUserFirst + 85;
  ecInsertCVSDate        = ecUserFirst + 86;
  ecInsertCVSHeader      = ecUserFirst + 87;
  ecInsertCVSID          = ecUserFirst + 88;
  ecInsertCVSLog         = ecUserFirst + 89;
  ecInsertCVSName        = ecUserFirst + 90;
  ecInsertCVSRevision    = ecUserFirst + 91;
  ecInsertCVSSource      = ecUserFirst + 92;

  ecWordCompletion       = ecUserFirst + 100;
  ecCompleteCode         = ecUserFirst + 101;
  ecIdentCompletion      = ecUserFirst + 102;
  ecSyntaxCheck          = ecUserFirst + 103;
  ecGuessUnclosedBlock   = ecUserFirst + 104;
  ecGuessMisplacedIFDEF  = ecUserFirst + 105;
  ecConvertDFM2LFM       = ecUserFirst + 106;
  ecMakeResourceString   = ecUserFirst + 107;
  ecDiff                 = ecUserFirst + 108;

  ecNew                  = ecUserFirst + 201;
  ecNewUnit              = ecUserFirst + 202;
  ecNewForm              = ecUserFirst + 203;
  ecOpen                 = ecUserFirst + 205;
  ecRevert               = ecUserFirst + 206;
  ecSave                 = ecUserFirst + 207;
  ecSaveAs               = ecUserFirst + 208;
  ecSaveAll              = ecUserFirst + 209;
  ecClose                = ecUserFirst + 210;
  ecCloseAll             = ecUserFirst + 211;
  ecQuit                 = ecUserFirst + 212;

  ecJumpToEditor         = ecUserFirst + 300;
  ecToggleFormUnit       = ecUserFirst + 301;
  ecToggleObjectInsp     = ecUserFirst + 302;
  ecToggleProjectExpl    = ecUserFirst + 303;
  ecToggleCodeExpl       = ecUserFirst + 304;
  ecToggleMessages       = ecUserFirst + 305;
  ecToggleWatches        = ecUserFirst + 306;
  ecToggleBreakPoints    = ecUserFirst + 307;
  ecToggleDebuggerOut    = ecUserFirst + 308;
  ecViewUnits            = ecUserFirst + 309;
  ecViewForms            = ecUserFirst + 310;
  ecViewUnitDependencies = ecUserFirst + 311;
  ecToggleLocals         = ecUserFirst + 312;
  ecToggleCallStack      = ecUserFirst + 313;

  ecBuild                = ecUserFirst + 400;
  ecRun                  = ecUserFirst + 401;
  ecPause                = ecUserFirst + 402;
  ecStepInto             = ecUserFirst + 403;
  ecStepOver             = ecUserFirst + 404;
  ecRunToCursor          = ecUserFirst + 405;
  ecStopProgram          = ecUserFirst + 406;
  ecBuildAll             = ecUserFirst + 407;
  ecBuildLazarus         = ecUserFirst + 408;

  ecExtToolFirst         = ecUserFirst + 500;
  ecExtToolLast          = ecUserFirst + 599;

  ecNewProject           = ecUserFirst + 700;
  ecNewProjectFromFile   = ecUserFirst + 701;
  ecOpenProject          = ecUserFirst + 702;
  ecSaveProject          = ecUserFirst + 703;
  ecSaveProjectAs        = ecUserFirst + 704;
  ecPublishProject       = ecUserFirst + 705;
  ecAddCurUnitToProj     = ecUserFirst + 706;
  ecRemoveFromProj       = ecUserFirst + 707;
  ecViewProjectSource    = ecUserFirst + 708;
  ecViewProjectTodos     = ecUserFirst + 709;
  ecProjectOptions       = ecUserFirst + 710;

  ecRunParameters        = ecUserFirst + 800;
  ecCompilerOptions      = ecUserFirst + 801;
  ecExtToolSettings      = ecUserFirst + 802;
  ecConfigBuildLazarus   = ecUserFirst + 803;
  ecEnvironmentOptions   = ecUserFirst + 804;
  ecEditorOptions        = ecUserFirst + 805;
  ecCodeToolsOptions     = ecUserFirst + 806;
  ecCodeToolsDefinesEd   = ecUserFirst + 807;

  ecAboutLazarus         = ecUserFirst + 900;
  
  ecGotoEditor1          = ecUserFirst + 2000;
  ecGotoEditor2          = ecGotoEditor1 + 1;
  ecGotoEditor3          = ecGotoEditor2 + 1;
  ecGotoEditor4          = ecGotoEditor3 + 1;
  ecGotoEditor5          = ecGotoEditor4 + 1;
  ecGotoEditor6          = ecGotoEditor5 + 1;
  ecGotoEditor7          = ecGotoEditor6 + 1;
  ecGotoEditor8          = ecGotoEditor7 + 1;
  ecGotoEditor9          = ecGotoEditor8 + 1;
  ecGotoEditor0          = ecGotoEditor9 + 1;


type
  TCommandArea = (caSourceEditor, caDesigner);
  TCommandAreas = set of TCommandArea;
  
const
  caAll = [caSourceEditor, caDesigner];
  caSrcEditOnly = [caSourceEditor];
  caDesignOnly = [caDesigner];
  
type
  //---------------------------------------------------------------------------
  // TKeyCommandCategory is used to divide the key commands in handy packets
  TKeyCommandCategory = class(TList)
  public
    Name: string;
    Description: string;
    Parent: TKeyCommandCategory;
    Areas: TCommandAreas;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    constructor Create(const AName, ADescription: string;
      TheAreas: TCommandAreas);
  end;
  
  //---------------------------------------------------------------------------
  // class for storing the keys of a single command (key-command relationship)
  TKeyCommandRelation = class
  private
    fParent: TKeyCommandCategory;
    procedure SetParent(const AValue: TKeyCommandCategory);
  public
    Name: ShortString;
    Command: word;  // see the ecXXX constants above
    Key1: word;
    Shift1: TShiftState;
    Key2: word;
    Shift2: TShiftState;
    property Parent: TKeyCommandCategory read fParent write SetParent;
    constructor Create(AParent: TKeyCommandCategory; AName:ShortString;
      ACommand: word;
      AKey1:Word; AShift1:TShiftState; AKey2:Word; AShift2:TShiftState);
    function AsShortCut: TShortCut;
    function LocalizedName: string;
  end;

  //---------------------------------------------------------------------------
  // class for a list of key - command relations
  TKeyCommandRelationList = class
  private
    fRelations: TList; // list of TKeyCommandRelation, sorted with Command
    fCategories: TList;// list of TKeyCommandCategory
    fExtToolCount: integer;
    function GetCategory(Index: integer): TKeyCommandCategory;
    function GetRelation(Index:integer):TKeyCommandRelation;
    function AddCategory(const Name, Description: string;
       TheAreas: TCommandAreas): integer;
    function Add(Category: TKeyCommandCategory; const Name:shortstring;
       Command:word;
       Key1:Word; Shift1:TShiftState; 
       Key2:Word; Shift2:TShiftState):integer;
    function ShiftStateToStr(Shift:TShiftState):AnsiString;
    procedure SetExtToolCount(NewCount: integer);
  public
    function Count: integer;
    function CategoryCount: integer;
    function Find(AKey:Word; AShiftState:TShiftState;
      Areas: TCommandAreas): TKeyCommandRelation;
    function FindByCommand(ACommand:word): TKeyCommandRelation;
    function FindCategoryByName(const CategoryName: string): TKeyCommandCategory;
    function TranslateKey(AKey:Word; AShiftState:TShiftState;
      Areas: TCommandAreas): word;
    function IndexOf(ARelation: TKeyCommandRelation): integer;
    function CommandToShortCut(ACommand: word): TShortCut;
    function LoadFromXMLConfig(XMLConfig:TXMLConfig; Prefix:AnsiString):boolean;
    function SaveToXMLConfig(XMLConfig:TXMLConfig; Prefix:AnsiString):boolean;
    procedure AssignTo(ASynEditKeyStrokes:TSynEditKeyStrokes;
      Areas: TCommandAreas);
    constructor Create;
    destructor Destroy; override;
    property ExtToolCount: integer read fExtToolCount write SetExtToolCount;
    property Relations[Index:integer]:TKeyCommandRelation read GetRelation;
    property Categories[Index: integer]: TKeyCommandCategory read GetCategory;
  end;

  //---------------------------------------------------------------------------
  // form for editing one command - key relationship
  TKeyMappingEditForm = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    CommandLabel: TLabel;
    Key1GroupBox: TGroupBox;
    Key1CtrlCheckBox: TCheckBox;
    Key1AltCheckBox: TCheckBox;
    Key1ShiftCheckBox: TCheckBox;
    Key1KeyComboBox: TComboBox;
    Key1GrabButton: TButton;
    Key2GroupBox: TGroupBox;
    Key2CtrlCheckBox: TCheckBox;
    Key2AltCheckBox: TCheckBox;
    Key2ShiftCheckBox: TCheckBox;
    Key2KeyComboBox: TComboBox;
    Key2GrabButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure Key1GrabButtonClick(Sender: TObject);
    procedure Key2GrabButtonClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift:TShiftState);
  private
    GrabbingKey: integer; // 0=none, 1=Default key, 2=Alternative key
    procedure ActivateGrabbing(AGrabbingKey: integer);
    procedure DeactivateGrabbing;
    procedure SetComboBox(AComboBox: TComboBox; AValue: string);
  public
    constructor Create(TheOwner:TComponent); override;
    KeyCommandRelationList:TKeyCommandRelationList;
    KeyIndex:integer;
  end;

function KeyAndShiftStateToStr(Key:Word; ShiftState:TShiftState):AnsiString;
function ShowKeyMappingEditForm(Index:integer;
   AKeyCommandRelationList:TKeyCommandRelationList):TModalResult;
function KeyStrokesConsistencyErrors(ASynEditKeyStrokes:TSynEditKeyStrokes;
   Protocol: TStrings; var Index1,Index2:integer):integer;
function EditorCommandToDescriptionString(cmd: word):AnsiString;
function EditorCommandLocalizedName(cmd: word;
  const DefaultName: string): string;
function StrToVKCode(const s: string): integer;

var KeyMappingEditForm: TKeyMappingEditForm;

const
  KeyCategoryToolMenuName = 'ToolMenu';

implementation


const
  KeyMappingFormatVersion = 2;
  UnknownVKPrefix = 'Word(''';
  UnknownVKPostfix = ''')';

  VirtualKeyStrings: TStringHashList = nil;
  
function EditorCommandLocalizedName(cmd: word;
  const DefaultName: string): string;
begin
  Result:=EditorCommandToDescriptionString(cmd);
  if Result=srkmecunknown then
    Result:=DefaultName;
end;

function StrToVKCode(const s: string): integer;
var
  i: integer;
  Data: Pointer;
begin
  Result:=VK_UNKNOWN;
  if (length(UnknownVKPrefix)<length(s))
  and (AnsiStrLComp(PChar(s),PChar(UnknownVKPrefix),length(UnknownVKPrefix))=0)
  then
    Result:=StrToIntDef(copy(s,7,length(s)-8),VK_UNKNOWN)
  else if s<>'none' then begin
    if VirtualKeyStrings=nil then begin
      VirtualKeyStrings:=TStringHashList.Create(true);
      for i:=1 to 300 do
        VirtualKeyStrings.Add(KeyAndShiftStateToStr(i,[]),Pointer(i));
      for i:=VK_IRREGULAR+33 to VK_IRREGULAR+255 do
        VirtualKeyStrings.Add(KeyAndShiftStateToStr(i,[]),Pointer(i));
    end;
  end else
    exit;
  Data:=VirtualKeyStrings.Data[s];
  if Data<>nil then
    Result:=integer(Data);
end;

function ShowKeyMappingEditForm(Index:integer;
  AKeyCommandRelationList:TKeyCommandRelationList):TModalResult;
   
  procedure InitComboBox(AComboBox: TComboBox; AKey: integer);
  var s: string;
    i: integer;
  begin
    s:=KeyAndShiftStateToStr(AKey,[]);
    i:=AComboBox.Items.IndexOf(s);
    if i>=0 then
      AComboBox.ItemIndex:=i
    else if lowercase(copy(s,1,5))='word(' then begin
      AComboBox.Items.Add(s);
      AComboBox.ItemIndex:=AComboBox.Items.IndexOf(s);
    end else
      AComboBox.ItemIndex:=0;
  end;
   
begin
  Result:=mrCancel;
  if KeyMappingEditForm<>nil then exit;
  KeyMappingEditForm:=TKeyMappingEditForm.Create(Application);
  with KeyMappingEditForm do
    try
      KeyCommandRelationList:=AKeyCommandRelationList;
      KeyIndex:=Index;
      Caption:=srkmEditKeys;

      with KeyCommandRelationList.Relations[Index] do
      begin
        CommandLabel.Caption:=srkmCommand+LocalizedName;
        if Key1<>VK_UNKNOWN then
        begin
          Key1CtrlCheckBox.Checked:=ssCtrl in Shift1;
          Key1AltCheckBox.Checked:=ssAlt in Shift1;
          Key1ShiftCheckBox.Checked:=ssShift in Shift1;
          InitComboBox(Key1KeyComboBox,Key1);
        end;
        if Key2<>VK_UNKNOWN then
        begin
          Key2CtrlCheckBox.Checked:=ssCtrl in Shift2;
          Key2AltCheckBox.Checked:=ssAlt in Shift2;
          Key2ShiftCheckBox.Checked:=ssShift in Shift2;
          InitComboBox(Key2KeyComboBox,Key2);
        end;
      end;
      Result:=ShowModal;
    finally
      Free;
      KeyMappingEditForm:=nil;
    end;
end;

function EditorCommandToDescriptionString(cmd: word):AnsiString;
begin
  case cmd of
    ecNone                  : Result:= dlgEnvNone;
    ecLeft                  : Result:= srvk_left;
    ecRight                 : Result:= srvk_right;
    ecUp                    : Result:= dlgUpWord;
    ecDown                  : Result:= dlgDownWord;
    ecWordLeft              : Result:= srkmecWordLeft;
    ecWordRight             : Result:= srkmecWordRight;
    ecLineStart             : Result:= srkmecLineStart;
    ecLineEnd               : Result:= srkmecLineEnd;
    ecPageUp                : Result:= srkmecPageUp;
    ecPageDown              : Result:= srkmecPageDown;
    ecPageLeft              : Result:= srkmecPageLeft;
    ecPageRight             : Result:= srkmecPageRight;
    ecPageTop               : Result:= srkmecPageTop;
    ecPageBottom            : Result:= srkmecPageBottom;
    ecEditorTop             : Result:= srkmecEditorTop;
    ecEditorBottom          : Result:= srkmecEditorBottom;
    ecGotoXY                : Result:= srkmecGotoXY;
    ecSelLeft               : Result:= srkmecSelLeft;
    ecSelRight              : Result:= srkmecSelRight;
    ecSelUp                 : Result:= srkmecSelUp;
    ecSelDown               : Result:= srkmecSelDown;
    ecSelWordLeft           : Result:= srkmecSelWordLeft;
    ecSelWordRight          : Result:= srkmecSelWordRight;
    ecSelLineStart          : Result:= srkmecSelLineStart;
    ecSelLineEnd            : Result:= srkmecSelLineEnd;
    ecSelPageUp             : Result:= srkmecSelPageUp;
    ecSelPageDown           : Result:= srkmecSelPageDown;
    ecSelPageLeft           : Result:= srkmecSelPageLeft;
    ecSelPageRight          : Result:= srkmecSelPageRight;
    ecSelPageTop            : Result:= srkmecSelPageTop;
    ecSelPageBottom         : Result:= srkmecSelPageBottom;
    ecSelEditorTop          : Result:= srkmecSelEditorTop;
    ecSelEditorBottom       : Result:= srkmecSelEditorBottom;
    ecSelGotoXY             : Result:= srkmecSelGotoXY;
    ecSelectAll             : Result:= srkmecSelectAll;
    ecDeleteLastChar        : Result:= srkmecDeleteLastChar;
    ecDeleteChar            : Result:= srkmecDeleteChar;
    ecDeleteWord            : Result:= srkmecDeleteWord;
    ecDeleteLastWord        : Result:= srkmecDeleteLastWord;
    ecDeleteBOL             : Result:= srkmecDeleteBOL;
    ecDeleteEOL             : Result:= srkmecDeleteEOL;
    ecDeleteLine            : Result:= srkmecDeleteLine;
    ecClearAll              : Result:= srkmecClearAll;
    ecLineBreak             : Result:= srkmecLineBreak;
    ecInsertLine            : Result:= srkmecInsertLine;
    ecChar                  : Result:= srkmecChar;
    ecImeStr                : Result:= srkmecImeStr;
    ecUndo                  : Result:= lisMenuUndo;
    ecRedo                  : Result:= lisMenuRedo;
    ecCut                   : Result:= srkmecCut;
    ecCopy                  : Result:= srkmecCopy;
    ecPaste                 : Result:= srkmecPaste;
    ecScrollUp              : Result:= srkmecScrollUp;
    ecScrollDown            : Result:= srkmecScrollDown;
    ecScrollLeft            : Result:= srkmecScrollLeft;
    ecScrollRight           : Result:= srkmecScrollRight;
    ecInsertMode            : Result:= srkmecInsertMode;
    ecOverwriteMode         : Result:= srkmecOverwriteMode;
    ecToggleMode            : Result:= srkmecToggleMode;
    ecBlockIndent           : Result:= srkmecBlockIndent;
    ecBlockUnindent         : Result:= srkmecBlockUnindent;
    ecTab                   : Result:= srVK_TAB;
    ecShiftTab              : Result:= srkmecShiftTab;
    ecMatchBracket          : Result:= srkmecMatchBracket;
    ecNormalSelect          : Result:= srkmecNormalSelect;
    ecColumnSelect          : Result:= srkmecColumnSelect;
    ecLineSelect            : Result:= srkmecLineSelect;
    ecAutoCompletion        : Result:= srkmecAutoCompletion;
    ecUserFirst             : Result:= srkmecUserFirst;
    ecGotoMarker0 ..
    ecGotoMarker9           : Result:= Format(srkmecGotoMarker,[cmd-ecGotoMarker0]);
    ecSetMarker0 ..
    ecSetMarker9            : Result:= Format(srkmecSetMarker,[cmd-ecSetMarker0]);
    ecPeriod                : Result:= srkmecPeriod;

    // sourcenotebook
    ecJumpToEditor          : Result:= srkmecJumpToEditor;
    ecNextEditor            : Result:= srkmecNextEditor;
    ecPrevEditor            : Result:= srkmecPrevEditor;
    ecMoveEditorLeft        : Result:= srkmecMoveEditorLeft;
    ecMoveEditorRight       : Result:= srkmecMoveEditorRight;
    ecGotoEditor1..
    ecGotoEditor0           : Format(srkmecGotoEditor,[cmd-ecGotoEditor1]);

    // file menu
    ecNew                   : Result:= srkmecNew;
    ecNewUnit               : Result:= srkmecNewUnit;
    ecNewForm               : Result:= srkmecNewForm;
    ecOpen                  : Result:= lisMenuOpen;
    ecRevert                : Result:= lisMenuRevert;
    ecSave                  : Result:= lisMenuSave;
    ecSaveAs                : Result:= srkmecSaveAs;
    ecSaveAll               : Result:= srkmecSaveAll;
    ecClose                 : Result:= lismenuclose;
    ecCloseAll              : Result:= srkmecCloseAll;
    ecQuit                  : Result:= lismenuquit;
    
    // edit menu
    ecSelectionUpperCase    : Result:= lismenuuppercaseselection;
    ecSelectionLowerCase    : Result:= lismenulowercaseselection;
    ecSelectionTabs2Spaces  : Result:= srkmecSelectionTabs2Spaces;
    ecSelectionComment      : Result:= lismenucommentselection;
    ecSelectionUncomment    : Result:= lismenuuncommentselection;
    ecSelectionSort         : Result:= lismenusortselection;
    ecSelectToBrace         : Result:= lismenuselecttobrace;
    ecSelectCodeBlock       : Result:= lismenuselectcodeblock;
    ecSelectLine            : Result:= lismenuselectline;
    ecSelectParagraph       : Result:= lismenuselectparagraph;
    ecInsertGPLNotice       : Result:= srkmecInsertGPLNotice;
    ecInsertLGPLNotice      : Result:= srkmecInsertLGPLNotice;
    ecInsertUserName        : Result:= srkmecInsertUserName;
    ecInsertDateTime        : Result:= srkmecInsertDateTime;
    ecInsertChangeLogEntry  : Result:= srkmecInsertChangeLogEntry;
    ecInsertCVSAuthor       : Result:= srkmecInsertCVSAuthor;
    ecInsertCVSDate         : Result:= srkmecInsertCVSDate;
    ecInsertCVSHeader       : Result:= srkmecInsertCVSHeader;
    ecInsertCVSID           : Result:= srkmecInsertCVSID;
    ecInsertCVSLog          : Result:= srkmecInsertCVSLog;
    ecInsertCVSName         : Result:= srkmecInsertCVSName;
    ecInsertCVSRevision     : Result:= srkmecInsertCVSRevision;
    ecInsertCVSSource       : Result:= srkmecInsertCVSSource;

    // search menu
    ecFind                  : Result:= srkmecFind;
    ecFindNext              : Result:= srkmecFindNext;
    ecFindPrevious          : Result:= srkmecFindPrevious;
    ecFindInFiles           : Result:= srkmecFindInFiles;
    ecReplace               : Result:= srkmecReplace;
    ecIncrementalFind       : Result:= lismenuincrementalfind;
    ecFindProcedureDefinition:Result:= srkmecFindProcedureDefinition;
    ecFindProcedureMethod   : Result:= srkmecFindProcedureMethod;
    ecGotoLineNumber        : Result:= srkmecGotoLineNumber;
    ecJumpBack              : Result:= lismenujumpback;
    ecJumpForward           : Result:= lismenujumpforward;
    ecAddJumpPoint          : Result:= srkmecAddJumpPoint;
    ecViewJumpHistory       : Result:= lismenuviewjumphistory;
    ecOpenFileAtCursor      : Result:= srkmecOpenFileAtCursor;
    ecGotoIncludeDirective  : Result:= srkmecGotoIncludeDirective;

    // view menu
    ecToggleFormUnit        : Result:= srkmecToggleFormUnit;
    ecToggleObjectInsp      : Result:= srkmecToggleObjectInsp;
    ecToggleProjectExpl     : Result:= srkmecToggleProjectExpl;
    ecToggleCodeExpl        : Result:= srkmecToggleCodeExpl;
    ecToggleMessages        : Result:= srkmecToggleMessages;
    ecToggleWatches         : Result:= srkmecToggleWatches;
    ecToggleBreakPoints     : Result:= srkmecToggleBreakPoints;
    ecToggleDebuggerOut     : Result:= srkmecToggleDebuggerOut;
    ecToggleLocals          : Result:= srkmecToggleLocals;
    ecToggleCallStack       : Result:= srkmecToggleCallStack;
    ecViewUnits             : Result:= srkmecViewUnits;
    ecViewForms             : Result:= srkmecViewForms;
    ecViewUnitDependencies  : Result:= srkmecViewUnitDependencies;

    // codetools
    ecWordCompletion        : Result:= srkmecWordCompletion;
    ecCompleteCode          : Result:= srkmecCompleteCode;
    ecIdentCompletion       : Result:= dlgedidcomlet;
    ecSyntaxCheck           : Result:= srkmecSyntaxCheck;
    ecGuessUnclosedBlock    : Result:= lismenuguessunclosedblock;
    ecGuessMisplacedIFDEF   : Result:= srkmecGuessMisplacedIFDEF;
    ecConvertDFM2LFM        : Result:= lismenuconvertdfmtolfm;
    ecFindDeclaration       : Result:= srkmecFindDeclaration;
    ecFindBlockOtherEnd     : Result:= srkmecFindBlockOtherEnd;
    ecFindBlockStart        : Result:= srkmecFindBlockStart;

    // project (menu string resource)
    ecNewProject            : Result:= lisMenuNewProject;
    ecNewProjectFromFile    : Result:= lisMenuNewProjectFromFile;
    ecOpenProject           : Result:= lisMenuOpenProject;
    ecSaveProject           : Result:= lisMenuSaveProject;
    ecSaveProjectAs         : Result:= lisMenuSaveProjectAs;
    ecPublishProject        : Result:= lisMenuPublishProject;
    ecAddCurUnitToProj      : Result:= lisMenuAddUnitToProject;
    ecRemoveFromProj        : Result:= lisMenuRemoveUnitFromProject;
    ecViewProjectSource     : Result:= lisMenuViewSource;
    ecViewProjectTodos      : Result:= lisMenuViewProjectTodos;
    ecProjectOptions        : Result:= lisMenuProjectOptions;

    // run menu (menu string resource)
    ecBuild                 : Result:= srkmecBuild;
    ecBuildAll              : Result:= srkmecBuildAll;
    ecRun                   : Result:= srkmecRun;
    ecPause                 : Result:= srkmecPause;
    ecStepInto              : Result:= lisMenuStepInto;
    ecStepOver              : Result:= lisMenuStepOver;
    ecRunToCursor           : Result:= lisMenuRunToCursor;
    ecStopProgram           : Result:= srkmecStopProgram;
    ecRunParameters         : Result:= srkmecRunParameters;
    ecCompilerOptions       : Result:= srkmecCompilerOptions;
    
    // tools menu
    ecExtToolSettings       : Result:= srkmecExtToolSettings;
    ecConfigBuildLazarus    : Result:= lismenuconfigurebuildlazarus;
    ecBuildLazarus          : Result:= srkmecBuildLazarus;
    ecExtToolFirst..
    ecExtToolLast           : Result:= Format(srkmecExtTool,[cmd-ecExtToolFirst+1]);
    ecMakeResourceString    : Result:= srkmecMakeResourceString;
    ecDiff                  : Result:= srkmecDiff;

    // environment menu
    ecEnvironmentOptions    : Result:= srkmecEnvironmentOptions;
    ecEditorOptions         : Result:= lismenueditoroptions;
    ecCodeToolsOptions      : Result:= srkmecCodeToolsOptions;
    ecCodeToolsDefinesEd    : Result:= srkmecCodeToolsDefinesEd;

    // help menu
    ecAboutLazarus          : Result:= lisMenuAboutLazarus;

    else
      Result:= srkmecunknown;
  end;
end;

function KeyStrokesConsistencyErrors(ASynEditKeyStrokes:TSynEditKeyStrokes;
   Protocol: TStrings; var Index1,Index2:integer):integer;
// 0 = ok, no errors
// >0 number of errors found
var a,b:integer;
  Key1,Key2:TSynEditKeyStroke;
begin
  Result:=0;
  for a:=0 to ASynEditKeyStrokes.Count-1 do begin
    Key1:=ASynEditKeyStrokes[a];
    for b:=a+1 to ASynEditKeyStrokes.Count-1 do begin
      Key2:=ASynEditKeyStrokes[b];
      if (Key1.Key=VK_UNKNOWN)
      or (Key1.Command=Key2.Command)
      then
        continue;
      if ((Key1.Key=Key2.Key) and (Key1.Shift=Key2.Shift))
      or ((Key1.Key2<>VK_UNKNOWN)
        and (Key1.Key2=Key2.Key) and (Key1.Shift2=Key2.Shift2)) then
      begin
        // consistency error
        if Result=0 then begin
          Index1:=a;
          Index2:=b;
        end;
        inc(Result);
        if Protocol<>nil then
        begin
          Protocol.Add(srkmConflic+IntToStr(Result));
          Protocol.Add(srkmCommand1
            +EditorCommandToDescriptionString(Key1.Command)+'"'
            +'->'+KeyAndShiftStateToStr(Key1.Key,Key1.Shift));
          Protocol.Add(srkmConflicW);
          Protocol.Add(srkmCommand2
            +EditorCommandToDescriptionString(Key2.Command)+'"'
            +'->'+KeyAndShiftStateToStr(Key2.Key,Key2.Shift)
           );
          Protocol.Add('');
        end;
      end;
    end;
  end;
end;

function KeyAndShiftStateToStr(Key:Word; ShiftState:TShiftState):AnsiString;
var
  p, ResultLen: integer;

  procedure AddStr(const s: string);
  var
    OldP: integer;
  begin
    if s<>'' then begin
      OldP:=p;
      inc(p,length(s));
      if p<=ResultLen then
        Move(s[1],Result[OldP+1],length(s));
    end;
  end;

  procedure AddAttribute(const s: string);
  begin
    if p>0 then
      AddStr('+');
    AddStr(s);
  end;
  
  procedure AddAttributes;
  begin
    if ssCtrl in ShiftState then AddAttribute('Ctrl');
    if ssAlt in ShiftState then AddAttribute('Alt');
    if ssShift in ShiftState then AddAttribute('Shift');
  end;
  
  procedure AddKey;
  begin
    if p>0 then  AddStr(' ');
    
    case Key of
    VK_UNKNOWN    :AddStr(srVK_UNKNOWN);
    VK_LBUTTON    :AddStr(srVK_LBUTTON);
    VK_RBUTTON    :AddStr(srVK_RBUTTON);
    VK_CANCEL     :AddStr(dlgCancel);
    VK_MBUTTON    :AddStr(srVK_MBUTTON);
    VK_BACK       :AddStr(srVK_BACK);
    VK_TAB        :AddStr(srVK_TAB);
    VK_CLEAR      :AddStr(srVK_CLEAR);
    VK_RETURN     :AddStr(srVK_RETURN);
    VK_SHIFT      :AddStr(srVK_SHIFT);
    VK_CONTROL    :AddStr(srVK_CONTROL);
    VK_MENU       :AddStr(srVK_MENU);
    VK_PAUSE      :AddStr(srVK_PAUSE);
    VK_CAPITAL    :AddStr(srVK_CAPITAL);
    VK_KANA       :AddStr(srVK_KANA);
  //  VK_HANGUL     :AddStr('Hangul');
    VK_JUNJA      :AddStr(srVK_JUNJA);
    VK_FINAL      :AddStr(srVK_FINAL);
    VK_HANJA      :AddStr(srVK_HANJA );
  //  VK_KANJI      :AddStr('Kanji');
    VK_ESCAPE     :AddStr(srVK_ESCAPE);
    VK_CONVERT    :AddStr(srVK_CONVERT);
    VK_NONCONVERT :AddStr(srVK_NONCONVERT);
    VK_ACCEPT     :AddStr(srVK_ACCEPT);
    VK_MODECHANGE :AddStr(srVK_MODECHANGE);
    VK_SPACE      :AddStr(srVK_SPACE);
    VK_PRIOR      :AddStr(srVK_PRIOR);
    VK_NEXT       :AddStr(srVK_NEXT);
    VK_END        :AddStr(srVK_END);
    VK_HOME       :AddStr(srVK_HOME);
    VK_LEFT       :AddStr(srVK_LEFT);
    VK_UP         :AddStr(srVK_UP);
    VK_RIGHT      :AddStr(srVK_RIGHT);
    VK_DOWN       :AddStr(dlgdownword);
    VK_SELECT     :AddStr(lismenuselect);
    VK_PRINT      :AddStr(srVK_PRINT);
    VK_EXECUTE    :AddStr(srVK_EXECUTE);
    VK_SNAPSHOT   :AddStr(srVK_SNAPSHOT);
    VK_INSERT     :AddStr(srVK_INSERT);
    VK_DELETE     :AddStr(dlgeddelete);
    VK_HELP       :AddStr(srVK_HELP);
    VK_0..VK_9    :AddStr(IntToStr(Key-VK_0));
    VK_A..VK_Z    :AddStr(chr(ord('A')+Key-VK_A));
    VK_LWIN       :AddStr(srVK_LWIN);
    VK_RWIN       :AddStr(srVK_RWIN);
    VK_APPS       :AddStr(srVK_APPS);
    VK_NUMPAD0..VK_NUMPAD9:  AddStr(Format(srVK_NUMPAD,[Key-VK_NUMPAD0]));
    VK_MULTIPLY   :AddStr('*');
    VK_ADD        :AddStr('+');
    VK_SEPARATOR  :AddStr('|');
    VK_SUBTRACT   :AddStr('-');
    VK_DECIMAL    :AddStr('.');
    VK_DIVIDE     :AddStr('/');
    VK_F1..VK_F24 : AddStr('F'+IntToStr(Key-VK_F1+1));
    VK_NUMLOCK    :AddStr(srVK_NUMLOCK);
    VK_SCROLL     :AddStr(srVK_SCROLL);
    VK_EQUAL      :AddStr('=');
    VK_COMMA      :AddStr(',');
    VK_POINT      :AddStr('.');
    VK_SLASH      :AddStr('/');
    VK_AT         :AddStr('@');
    else
      if (Key>=VK_IRREGULAR+33) and (Key<=VK_IRREGULAR+255) then
      begin
        AddStr(srVK_IRREGULAR);
        AddStr(chr(Key-VK_IRREGULAR));
      end
      else
      begin
        AddStr(UnknownVKPrefix);
        AddStr(IntToStr(Key));
        AddStr(UnknownVKPostfix);
      end;
    end;
  end;
  
  procedure AddAttributesAndKey;
  begin
    AddAttributes;
    AddKey;
  end;

begin
  ResultLen:=0;
  p:=0;
  AddAttributesAndKey;
  ResultLen:=p;
  SetLength(Result,ResultLen);
  p:=0;
  AddAttributesAndKey;
end;

{ TKeyMappingEditForm }

constructor TKeyMappingEditForm.Create(TheOwner:TComponent);
var a:integer;
  s:AnsiString;
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then
  begin
    SetBounds((Screen.Width-200) div 2,(Screen.Height-270) div 2,216,310);
    Caption:=srkmEditForCmd;
    OnKeyUp:=@FormKeyUp;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Caption:='Ok';
      Left:=15;
      Top:=Self.ClientHeight-Height-15;
      Width:=80;
      OnClick:=@OkButtonClick;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Caption:=dlgCancel;
      Left:=125;
      Top:=OkButton.Top;
      Width:=OkButton.Width;
      OnClick:=@CancelButtonClick;
    end;

    CommandLabel:=TLabel.Create(Self);
    with CommandLabel do begin
      Name:='CommandLabel';
      Parent:=Self;
      Caption:=srkmCommand;
      Left:=5;
      Top:=5;
      Width:=Self.ClientWidth-Left-Left;
      Height:=20;
    end;

    Key1GroupBox:=TGroupBox.Create(Self);
    with Key1GroupBox do begin
      Name:='Key1GroupBox';
      Parent:=Self;
      Caption:=srkmKey;
      Left:=5;
      Top:=CommandLabel.Top+CommandLabel.Height+8;
      Width:=Self.ClientWidth-Left-Left;
      Height:=110;
    end;

    Key1CtrlCheckBox:=TCheckBox.Create(Self);
    with Key1CtrlCheckBox do begin
      Name:='Key1CtrlCheckBox';
      Parent:=Key1GroupBox;
      Caption:='Ctrl';
      Left:=5;
      Top:=2;
      Width:=55;
      Height:=20;
    end;

    Key1AltCheckBox:=TCheckBox.Create(Self);
    with Key1AltCheckBox do begin
      Name:='Key1AltCheckBox';
      Parent:=Key1GroupBox;
      Caption:='Alt';
      Left:=Key1CtrlCheckBox.Left+Key1CtrlCheckBox.Width+10;
      Top:=Key1CtrlCheckBox.Top;
      Height:=20;
      Width:=Key1CtrlCheckBox.Width;
    end;

    Key1ShiftCheckBox:=TCheckBox.Create(Self);
    with Key1ShiftCheckBox do begin
      Name:='Key1ShiftCheckBox';
      Parent:=Key1GroupBox;
      Caption:='Shift';
      Left:=Key1AltCheckBox.Left+Key1AltCheckBox.Width+10;
      Top:=Key1CtrlCheckBox.Top;
      Height:=20;
      Width:=Key1CtrlCheckBox.Width;
    end;

    Key1KeyComboBox:=TComboBox.Create(Self);
    with Key1KeyComboBox do begin
      Name:='Key1KeyComboBox';
      Parent:=Key1GroupBox;
      Left:=5;
      Top:=Key1CtrlCheckBox.Top+Key1CtrlCheckBox.Height+5;
      Width:=190;
      Items.BeginUpdate;
      Items.Add('none');
      for a:=1 to 145 do begin
        s:=KeyAndShiftStateToStr(a,[]);
        if lowercase(copy(s,1,5))<>'word(' then
          Items.Add(s);
      end;
      Items.EndUpdate;
      ItemIndex:=0;
    end;
    
    Key1GrabButton:=TButton.Create(Self);
    with Key1GrabButton do begin
      Parent:=Key1GroupBox;
      Left:=5;
      Top:=Key1KeyComboBox.Top+Key1KeyComboBox.Height+5;
      Width:=Key1KeyComboBox.Width;
      Height:=25;
      Caption:=srkmGrabKey;
      Name:='Key1GrabButton';
      OnClick:=@Key1GrabButtonClick;
    end;

    Key2GroupBox:=TGroupBox.Create(Self);
    with Key2GroupBox do begin
      Name:='Key2GroupBox';
      Parent:=Self;
      Caption:=srkmAlternKey;
      Left:=5;
      Top:=Key1GroupBox.Top+Key1GroupBox.Height+8;
      Width:=Key1GroupBox.Width;
      Height:=110;
    end;

    Key2CtrlCheckBox:=TCheckBox.Create(Self);
    with Key2CtrlCheckBox do begin
      Name:='Key2CtrlCheckBox';
      Parent:=Key2GroupBox;
      Caption:='Ctrl';
      Left:=5;
      Top:=2;
      Width:=55;
      Height:=20;
    end;

    Key2AltCheckBox:=TCheckBox.Create(Self);
    with Key2AltCheckBox do begin
      Name:='Key2AltCheckBox';
      Parent:=Key2GroupBox;
      Caption:='Alt';
      Left:=Key2CtrlCheckBox.Left+Key2CtrlCheckBox.Width+10;
      Top:=Key2CtrlCheckBox.Top;
      Height:=20;
      Width:=Key2CtrlCheckBox.Width;
    end;

    Key2ShiftCheckBox:=TCheckBox.Create(Self);
    with Key2ShiftCheckBox do begin
      Name:='Key2ShiftCheckBox';
      Parent:=Key2GroupBox;
      Caption:='Shift';
      Left:=Key2AltCheckBox.Left+Key2AltCheckBox.Width+10;
      Top:=Key2CtrlCheckBox.Top;
      Height:=20;
      Width:=Key2CtrlCheckBox.Width;
    end;

    Key2KeyComboBox:=TComboBox.Create(Self);
    with Key2KeyComboBox do begin
      Name:='Key2KeyComboBox';
      Parent:=Key2GroupBox;
      Left:=5;
      Top:=Key2CtrlCheckBox.Top+Key2CtrlCheckBox.Height+5;
      Width:=190;
      Items.BeginUpdate;
      Items.Add('none');
      for a:=1 to 145 do begin
        s:=KeyAndShiftStateToStr(a,[]);
        if lowercase(copy(s,1,5))<>'word(' then
          Items.Add(s);
      end;
      Items.EndUpdate;
      ItemIndex:=0;
    end;
    
    Key2GrabButton:=TButton.Create(Self);
    with Key2GrabButton do begin
      Parent:=Key2GroupBox;
      Left:=5;
      Top:=Key2KeyComboBox.Top+Key2KeyComboBox.Height+5;
      Width:=Key2KeyComboBox.Width;
      Height:=25;
      Caption:=srkmGrabKey;
      Name:='Key2GrabButton';
      OnClick:=@Key2GrabButtonClick;
    end;

  end;
  GrabbingKey:=0;
end;

procedure TKeyMappingEditForm.OkButtonClick(Sender:TObject);
var NewKey1,NewKey2:integer;
  NewShiftState1,NewShiftState2:TShiftState;
  AText:AnsiString;
  DummyRelation, CurRelation:TKeyCommandRelation;
begin
  NewKey1:=VK_UNKNOWN;
  NewShiftState1:=[];
  NewKey2:=VK_UNKNOWN;
  NewShiftState2:=[];
  NewKey1:=StrToVKCode(Key1KeyComboBox.Text);
  if NewKey1<>VK_UNKNOWN then
  begin
    if Key1CtrlCheckBox.Checked then include(NewShiftState1,ssCtrl);
    if Key1AltCheckBox.Checked then include(NewShiftState1,ssAlt);
    if Key1ShiftCheckBox.Checked then include(NewShiftState1,ssShift);
  end;

  CurRelation:=KeyCommandRelationList.Relations[KeyIndex];
  DummyRelation:=KeyCommandRelationList.Find(NewKey1,NewShiftState1,
                                                      CurRelation.Parent.Areas);
  if (DummyRelation<>nil) 
  and (DummyRelation<>KeyCommandRelationList.Relations[KeyIndex]) then
  begin
    AText:=Format(srkmAlreadyConnected,[KeyAndShiftStateToStr(NewKey1,NewShiftState1),DummyRelation.Name]);
    MessageDlg(AText,mtError,[mbok],0);
    exit;
  end;

  NewKey2:=StrToVKCode(Key2KeyComboBox.Text);
  if (NewKey1=NewKey2) and (NewShiftState1=NewShiftState2) then
    NewKey2:=VK_UNKNOWN;
  if NewKey2<>VK_UNKNOWN then
  begin
    if Key2CtrlCheckBox.Checked then include(NewShiftState2,ssCtrl);
    if Key2AltCheckBox.Checked then include(NewShiftState2,ssAlt);
    if Key2ShiftCheckBox.Checked then include(NewShiftState2,ssShift);
  end;
  DummyRelation:=KeyCommandRelationList.Find(NewKey2,NewShiftState2,CurRelation.Parent.Areas);
  
  if (DummyRelation<>nil) and (DummyRelation<>KeyCommandRelationList.Relations[KeyIndex]) then
  begin
    AText:=Format(srkmAlreadyConnected,[KeyAndShiftStateToStr(NewKey2,NewShiftState2),DummyRelation.Name]);
    MessageDlg(AText,mterror,[mbok],0);
    exit;
  end;

  if NewKey1=VK_UNKNOWN then
  begin
    NewKey1:=NewKey2;
    NewShiftState1:=NewShiftState2;
    NewKey2:=VK_UNKNOWN;
  end;

  with CurRelation do
  begin
    Key1:=NewKey1;
    Shift1:=NewShiftState1;
    Key2:=NewKey2;
    Shift2:=NewShiftState2;
  end;
  ModalResult:=mrOk;
end;

procedure TKeyMappingEditForm.CancelButtonClick(Sender:TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TKeyMappingEditForm.Key1GrabButtonClick(Sender: TObject);
begin
  ActivateGrabbing(1);
end;

procedure TKeyMappingEditForm.Key2GrabButtonClick(Sender: TObject);
begin
  ActivateGrabbing(2);
end;

procedure TKeyMappingEditForm.DeactivateGrabbing;
var i: integer;
begin
  if GrabbingKey=0 then exit;
  // enable all components
  for i:=0 to ComponentCount-1 do
  begin
    if (Components[i] is TWinControl) then
      TWinControl(Components[i]).Enabled:=true;
  end;
  
  if GrabbingKey=1 then
    Key1GrabButton.Caption:=srkmGrabKey
  else if GrabbingKey=2 then
           Key2GrabButton.Caption:=srkmGrabKey;
  GrabbingKey:=0;
end;

procedure TKeyMappingEditForm.SetComboBox(AComboBox: TComboBox; AValue: string);
var i: integer;
begin
  i:=AComboBox.Items.IndexOf(AValue);
  if i>=0 then
    AComboBox.ItemIndex:=i
  else
  begin
    AComboBox.Items.Add(AValue);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AValue);
  end;
end;

procedure TKeyMappingEditForm.ActivateGrabbing(AGrabbingKey: integer);
var i: integer;
begin
  if GrabbingKey>0 then exit;
  GrabbingKey:=AGrabbingKey;
  if GrabbingKey=0 then exit;
  // disable all components
  for i:=0 to ComponentCount-1 do
  begin
    if (Components[i] is TWinControl) then
    begin
      if ((GrabbingKey=1) and (Components[i]<>Key1GrabButton)
         and (Components[i]<>Key1GroupBox))
         or ((GrabbingKey=2) and (Components[i]<>Key2GrabButton)
         and (Components[i]<>Key2GroupBox)) then
                   TWinControl(Components[i]).Enabled:=false;
    end;
  end;
  if GrabbingKey=1 then
    Key1GrabButton.Caption:=srkmPressKey
  else if GrabbingKey=2 then
           Key2GrabButton.Caption:=srkmPressKey;
end;

procedure TKeyMappingEditForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift:TShiftState);
begin
  {writeln('TKeyMappingEditForm.FormKeyUp Sender=',Classname
     ,' Key=',Key,' Ctrl=',ssCtrl in Shift,' Shift=',ssShift in Shift
     ,' Alt=',ssAlt in Shift,' AsString=',KeyAndShiftStateToStr(Key,Shift)
     );}
  if Key in [VK_CONTROL, VK_SHIFT, VK_LCONTROL, VK_RCONTROl,
             VK_LSHIFT, VK_RSHIFT] then exit;
  if (GrabbingKey in [1,2]) then
  begin
    if GrabbingKey=1 then
    begin
      Key1CtrlCheckBox.Checked:=(ssCtrl in Shift);
      Key1ShiftCheckBox.Checked:=(ssShift in Shift);
      Key1AltCheckBox.Checked:=(ssAlt in Shift);
      SetComboBox(Key1KeyComboBox,KeyAndShiftStateToStr(Key,[]));
    end
    else if GrabbingKey=2 then
         begin
           Key2CtrlCheckBox.Checked:=(ssCtrl in Shift);
           Key2ShiftCheckBox.Checked:=(ssShift in Shift);
           Key2AltCheckBox.Checked:=(ssAlt in Shift);
           SetComboBox(Key2KeyComboBox,KeyAndShiftStateToStr(Key,[]));
         end;
    Key:=0;
    DeactivateGrabbing;
  end;
end;


{ TKeyCommandRelation }

constructor TKeyCommandRelation.Create(AParent: TKeyCommandCategory;
  AName:ShortString; ACommand:word;
  AKey1:Word;AShift1:TShiftState;AKey2:Word;AShift2:TShiftState);
begin
  Name:=AName;
  Command:=ACommand;
  Key1:=AKey1;
  Shift1:=AShift1;
  Key2:=AKey2;
  Shift2:=AShift2;
  Parent:=AParent;
end;

procedure TKeyCommandRelation.SetParent(const AValue: TKeyCommandCategory);
begin
  if Parent<>AValue then
  begin
    // unbind
    if Parent<>nil then
    begin
      Parent.Remove(Self);
    end;
    // bind
    fParent:=AValue;
    if Parent<>nil then
    begin
      Parent.Add(Self);
    end;
  end;
end;

function TKeyCommandRelation.AsShortCut: TShortCut;
begin
  Result:=Key1;
  if ssCtrl in Shift1 then
    Result:=Result+scCtrl;
  if ssShift in Shift1 then
    Result:=Result+scShift;
  if ssAlt in Shift1 then
    Result:=Result+scAlt;
end;

function TKeyCommandRelation.LocalizedName: string;
begin
  Result:=EditorCommandLocalizedName(Command,Name);
end;

{ TKeyCommandRelationList }

constructor TKeyCommandRelationList.Create;
var C: TKeyCommandCategory;
begin
  inherited Create;
  FRelations:=TList.Create;
  fCategories:=TList.Create;
  fExtToolCount:=0;

  // create default keymapping

  // moving
  C:=Categories[AddCategory('CursorMoving',srkmCatCursorMoving,caSrcEditOnly)];
  Add(C,'Move cursor word left',ecWordLeft, VK_LEFT, [ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Move cursor word right',ecWordRight, VK_RIGHT, [ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Move cursor to line start',ecLineStart, VK_HOME, [],VK_UNKNOWN,[]);
  Add(C,'Move cursor to line end',ecLineEnd, VK_END, [],VK_UNKNOWN,[]);
  Add(C,'Move cursor up one page',ecPageUp, VK_PRIOR, [],VK_UNKNOWN,[]);
  Add(C,'Move cursor down one page',ecPageDown, VK_NEXT, [],VK_UNKNOWN,[]);
  Add(C,'Move cursor left one page',ecPageLeft,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Move cursor right one page',ecPageRight,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Move cursor to top of page',ecPageTop, VK_PRIOR, [ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Move cursor to bottom of page',ecPageBottom, VK_NEXT, [ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Move cursor to absolute beginning',ecEditorTop,VK_HOME,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Move cursor to absolute end',ecEditorBottom,VK_END,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Scroll up one line',ecScrollUp, VK_UP, [ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Scroll down one line',ecScrollDown, VK_DOWN, [ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Scroll left one char',ecScrollLeft, VK_UNKNOWN, [],VK_UNKNOWN,[]);
  Add(C,'Scroll right one char',ecScrollRight, VK_UNKNOWN, [],VK_UNKNOWN,[]);

  // selection
  C:=Categories[AddCategory('Selection',srkmCatSelection,caSrcEditOnly)];
  Add(C,'Copy selection to clipboard',ecCopy,VK_C,[ssCtrl],VK_Insert,[ssCtrl]);
  Add(C,'Cut selection to clipboard',ecCut,VK_X,[ssCtrl],VK_Delete,[ssShift]);
  Add(C,'Paste clipboard to current position',ecPaste,VK_V,[ssCtrl],VK_Insert,[ssShift]);
  Add(C,'Normal selection mode',ecNormalSelect,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Column selection mode',ecColumnSelect,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Line selection mode',ecLineSelect,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Select word left',ecSelWordLeft,VK_LEFT,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  Add(C,'Select word right',ecSelWordRight,VK_RIGHT,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  Add(C,'Select line start',ecSelLineStart,VK_HOME,[ssShift],VK_UNKNOWN,[]);
  Add(C,'Select line end',ecSelLineEnd,VK_END,[ssShift],VK_UNKNOWN,[]);
  Add(C,'Select page top',ecSelPageTop,VK_PRIOR, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Select page bottom',ecSelPageBottom,VK_NEXT, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Select to absolute beginning',ecSelEditorTop,VK_HOME, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Select to absolute end',ecSelEditorBottom,VK_END, [ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Select all',ecSelectAll,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Select to brace',ecSelectToBrace,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Select code block',ecSelectCodeBlock,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Select line',ecSelectLine,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Select paragraph',ecSelectParagraph,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Uppercase selection',ecSelectionUpperCase,VK_UNKNOWN, [],VK_UNKNOWN,[]);
  Add(C,'Lowercase selection',ecSelectionLowerCase,VK_UNKNOWN, [],VK_UNKNOWN,[]);
  Add(C,'Convert tabs to spaces in selection',ecSelectionTabs2Spaces,VK_UNKNOWN, [],VK_UNKNOWN,[]);
  Add(C,'Comment selection',ecSelectionComment,VK_UNKNOWN, [],VK_UNKNOWN,[]);
  Add(C,'Uncomment selection',ecSelectionUncomment,VK_UNKNOWN, [],VK_UNKNOWN,[]);
  Add(C,'Sort selection',ecSelectionSort,VK_UNKNOWN, [],VK_UNKNOWN,[]);

  // editing
  C:=Categories[AddCategory('editing commands',srkmCatEditing,caSrcEditOnly)];
  Add(C,'Indent block',ecBlockIndent,VK_I,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Unindent block',ecBlockUnindent,VK_U,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Delete last char',ecDeleteLastChar,VK_BACK, [],VK_BACK, [ssShift]);
  Add(C,'Delete char at cursor',ecDeleteChar,VK_DELETE,[],VK_UNKNOWN,[]);
  Add(C,'Delete to end of word',ecDeleteWord,VK_T,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Delete to start of word',ecDeleteLastWord,VK_BACK,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Delete to beginning of line',ecDeleteBOL,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Delete to end of line',ecDeleteEOL,VK_Y,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  Add(C,'Delete current line',ecDeleteLine,VK_Y,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Delete whole text',ecClearAll,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Break line and move cursor',ecLineBreak,VK_RETURN,[],VK_UNKNOWN,[]);
  Add(C,'Break line, leave cursor',ecInsertLine,VK_N,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Insert GPL notice',ecInsertGPLNotice,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert LGPL notice',ecInsertLGPLNotice,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert username',ecInsertUserName,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert date and time',ecInsertDateTime,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert ChangeLog entry',ecInsertChangeLogEntry,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert CVS keyword Author',ecInsertCVSAuthor,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert CVS keyword Date',ecInsertCVSDate,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert CVS keyword Header',ecInsertCVSHeader,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert CVS keyword ID',ecInsertCVSID,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert CVS keyword Log',ecInsertCVSLog,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert CVS keyword Name',ecInsertCVSName,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Insert CVS keyword Revision',ecInsertCVSRevision,VK_UNKNOWN,[],VK_UNKNOWN,[]);;
  Add(C,'Insert CVS keyword Source',ecInsertCVSSource,VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // command commands
  C:=Categories[AddCategory('CommandCommands',srkmCatCmdCmd,caAll)];
  Add(C,'Undo',ecUndo,VK_Z,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Redo',ecRedo,VK_Z,[ssCtrl,ssShift],VK_UNKNOWN,[]);

  // search & replace
  C:=Categories[AddCategory('SearchReplace',srkmCatSearchReplace,caSrcEditOnly)];
  Add(C,'Go to matching bracket',ecMatchBracket,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Find text',ecFind,VK_F,[SSCtrl],VK_UNKNOWN,[]);
  Add(C,'Find next',ecFindNext,VK_F3,[],VK_UNKNOWN,[]);
  Add(C,'Find previous',ecFindPrevious,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Find in files',ecFindInFiles,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Replace text',ecReplace,VK_R,[SSCtrl],VK_UNKNOWN,[]);
  Add(C,'Find incremental',ecIncrementalFind,VK_E,[SSCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to line number',ecGotoLineNumber,VK_G,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Jump back',ecJumpBack,VK_H,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Jump forward',ecJumpForward,VK_H,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  Add(C,'Add jump point',ecAddJumpPoint,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'View jump history',ecViewJumpHistory,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Open file at cursor',ecOpenFileAtCursor,VK_RETURN,[ssCtrl],VK_UNKNOWN,[]);

  // marker
  C:=Categories[AddCategory('Marker',srkmCatMarker,caSrcEditOnly)];
  Add(C,'Go to marker 0',ecGotoMarker0,VK_0,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to marker 1',ecGotoMarker1,VK_1,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to marker 2',ecGotoMarker2,VK_2,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to marker 3',ecGotoMarker3,VK_3,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to marker 4',ecGotoMarker4,VK_4,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to marker 5',ecGotoMarker5,VK_5,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to marker 6',ecGotoMarker6,VK_6,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to marker 7',ecGotoMarker7,VK_7,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to marker 8',ecGotoMarker8,VK_8,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to marker 9',ecGotoMarker9,VK_9,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Set marker 0',ecSetMarker0,VK_0,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Set marker 1',ecSetMarker1,VK_1,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Set marker 2',ecSetMarker2,VK_2,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Set marker 3',ecSetMarker3,VK_3,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Set marker 4',ecSetMarker4,VK_4,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Set marker 5',ecSetMarker5,VK_5,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Set marker 6',ecSetMarker6,VK_6,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Set marker 7',ecSetMarker7,VK_7,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Set marker 8',ecSetMarker8,VK_8,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Set marker 9',ecSetMarker9,VK_9,[ssShift,ssCtrl],VK_UNKNOWN,[]);

  // codetools
  C:=Categories[AddCategory('CodeTools',srkmCatCodeTools,caSrcEditOnly)];
  Add(C,'Code template completion',ecAutoCompletion,VK_J,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Word completion',ecWordCompletion,VK_W,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Complete code',ecCompleteCode,VK_C,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  Add(C,'Identifier completion',ecIdentCompletion,VK_SPACE,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Syntax check',ecSyntaxCheck,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Guess unclosed block',ecGuessUnclosedBlock,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Guess misplaced $IFDEF',ecGuessMisplacedIFDEF,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Convert DFM file to LFM',ecConvertDFM2LFM,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Find procedure definiton',ecFindProcedureDefinition,
                                 VK_UP,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  Add(C,'Find procedure method',ecFindProcedureMethod,
                                 VK_DOWN,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  Add(C,'Find declaration',ecFindDeclaration,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Find block other end',ecFindBlockOtherEnd,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Find block start',ecFindBlockStart,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Goto include directive',ecGotoIncludeDirective,VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // source notebook
  C:=Categories[AddCategory('SourceNotebook',srkmCatSrcNoteBook,caAll)];
  Add(C,'Go to next editor',ecNextEditor, VK_TAB, [ssCtrl], VK_UNKNOWN, []);
  Add(C,'Go to prior editor',ecPrevEditor, VK_TAB, [ssShift,ssCtrl], VK_UNKNOWN, []);
  Add(C,'Go to source editor 1',ecGotoEditor0,VK_1,[ssAlt],VK_UNKNOWN,[]);
  Add(C,'Go to source editor 2',ecGotoEditor0,VK_2,[ssAlt],VK_UNKNOWN,[]);
  Add(C,'Go to source editor 3',ecGotoEditor0,VK_3,[ssAlt],VK_UNKNOWN,[]);
  Add(C,'Go to source editor 4',ecGotoEditor0,VK_4,[ssAlt],VK_UNKNOWN,[]);
  Add(C,'Go to source editor 5',ecGotoEditor0,VK_5,[ssAlt],VK_UNKNOWN,[]);
  Add(C,'Go to source editor 6',ecGotoEditor0,VK_6,[ssAlt],VK_UNKNOWN,[]);
  Add(C,'Go to source editor 7',ecGotoEditor0,VK_7,[ssAlt],VK_UNKNOWN,[]);
  Add(C,'Go to source editor 8',ecGotoEditor0,VK_8,[ssAlt],VK_UNKNOWN,[]);
  Add(C,'Go to source editor 9',ecGotoEditor0,VK_9,[ssAlt],VK_UNKNOWN,[]);
  Add(C,'Go to source editor 10',ecGotoEditor0,VK_0,[ssAlt],VK_UNKNOWN,[]);
  Add(C,'Move editor left',ecMoveEditorLeft, VK_UNKNOWN, [], VK_UNKNOWN, []);
  Add(C,'Move editor right',ecMoveEditorLeft, VK_UNKNOWN, [], VK_UNKNOWN, []);

  // file menu
  C:=Categories[AddCategory('FileMenu',srkmCatFileMenu,caAll)];
  Add(C,'New',ecNew,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'NewUnit',ecNewUnit,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'NewForm',ecNewForm,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Open',ecOpen,VK_O,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Revert',ecRevert,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Save',ecSave,VK_S,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'SaveAs',ecSaveAs,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'SaveAll',ecSaveAll,VK_S,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  Add(C,'Close',ecClose,VK_F4,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'CloseAll',ecCloseAll,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Quit',ecQuit,VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // view menu
  C:=Categories[AddCategory('ViewMenu',srkmCatViewMenu,caAll)];
  Add(C,'Toggle view Object Inspector',ecToggleObjectInsp,VK_F11,[],VK_UNKNOWN,[]);
  Add(C,'Toggle view Project Explorer',ecToggleProjectExpl,VK_F11,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  Add(C,'Toggle view Code Explorer',ecToggleCodeExpl,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Toggle view Messages',ecToggleMessages,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Toggle view Watches',ecToggleWatches,VK_W,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  Add(C,'Toggle view Breakpoints',ecToggleBreakPoints,VK_B,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  Add(C,'Toggle view Local Variables',ecToggleLocals,VK_L,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  Add(C,'Toggle view Call Stack',ecToggleCallStack,VK_S,[ssCtrl,ssAlt],VK_UNKNOWN,[]);
  Add(C,'Toggle view Debugger Output',ecToggleDebuggerOut,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'View Units',ecViewUnits,VK_F12,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'View Forms',ecViewForms,VK_F12,[ssShift],VK_UNKNOWN,[]);
  Add(C,'View Unit Dependencies',ecViewUnitDependencies,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Focus to source editor',ecJumpToEditor,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Toggle between Unit and Form',ecToggleFormUnit,VK_F12,[],VK_UNKNOWN,[]);

  // project menu
  C:=Categories[AddCategory('ProjectMenu',srkmCatProjectMenu,caAll)];
  Add(C,'New project',ecNewProject,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'New project from file',ecNewProjectFromFile,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Open project',ecOpenProject,VK_F11,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Save project',ecSaveProject,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Save project as',ecSaveProjectAs,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Publish project',ecPublishProject,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Add active unit to project',ecAddCurUnitToProj,VK_F11,[ssShift],VK_UNKNOWN,[]);
  Add(C,'Remove active unit from project',ecRemoveFromProj,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'View project source',ecViewProjectSource,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'View project ToDo list',ecViewProjectTodos,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'View project options',ecProjectOptions,VK_F11,[ssShift,ssCtrl],VK_UNKNOWN,[]);

  // run menu
  C:=Categories[AddCategory('RunMenu',srkmCatRunMenu,caAll)];
  Add(C,'Build project/program',ecBuild,VK_F9,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Build all files of project/program',ecBuildAll,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Run program',ecRun,VK_F9,[],VK_UNKNOWN,[]);
  Add(C,'Pause program',ecPause,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Step into',ecStepInto,VK_F7,[],VK_UNKNOWN,[]);
  Add(C,'Step over',ecStepOver,VK_F8,[],VK_UNKNOWN,[]);
  Add(C,'Run to cursor',ecRunToCursor,VK_F4,[],VK_UNKNOWN,[]);
  Add(C,'Stop program',ecStopProgram,VK_F2,[SSCtrl],VK_UNKNOWN,[]);
  Add(C,'Compiler options',ecCompilerOptions,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Run parameters',ecRunParameters,VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // tools menu
  C:=Categories[AddCategory(KeyCategoryToolMenuName,srkmCatToolMenu,caAll)];
  Add(C,'External Tools settings',ecExtToolSettings,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Build Lazarus',ecBuildLazarus,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Configure "Build Lazarus"',ecConfigBuildLazarus,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Make resource string',ecMakeResourceString,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Diff editor files',ecDiff,VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // environment menu
  C:=Categories[AddCategory('EnvironmentMenu',srkmCatEnvMenu,caAll)];
  Add(C,'General environment options',ecEnvironmentOptions,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Editor options',ecEditorOptions,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'CodeTools options',ecCodeToolsOptions,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'CodeTools defines editor',ecCodeToolsDefinesEd,VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // help menu
  C:=Categories[AddCategory('HelpMenu',srkmCarHelpMenu,caAll)];
  Add(C,'About Lazarus',ecAboutLazarus,VK_UNKNOWN,[],VK_UNKNOWN,[]);
end;

destructor TKeyCommandRelationList.Destroy;
var a:integer;
begin
  for a:=0 to FRelations.Count-1 do
    Relations[a].Free;
  FRelations.Free;
  for a:=0 to fCategories.Count-1 do
    Categories[a].Free;
  fCategories.Free;
  inherited Destroy;
end;

function TKeyCommandRelationList.GetRelation(
  Index:integer):TKeyCommandRelation;
begin
  if (Index<0) or (Index>=Count) then
  begin
    writeln('[TKeyCommandRelationList.GetRelation] Index out of bounds '
      ,Index,' Count=',Count);
    // creates an exception, that gdb catches:
    if (Index div ((Index and 1) div 10000))=0 then ;
  end;
  Result:= TKeyCommandRelation(FRelations[Index]);
end;

function TKeyCommandRelationList.Count:integer;
begin
  Result:=FRelations.Count;
end;

function TKeyCommandRelationList.Add(Category: TKeyCommandCategory;
  const Name:shortstring;
  Command:word;
  Key1:Word; Shift1:TShiftState; Key2:Word; Shift2:TShiftState):integer;
begin
  Result:=FRelations.Add(TKeyCommandRelation.Create(Category,Name,Command
      ,Key1,Shift1,Key2,Shift2));
end;

procedure TKeyCommandRelationList.SetExtToolCount(NewCount: integer);
var i: integer;
  ExtToolCat: TKeyCommandCategory;
  ExtToolRelation: TKeyCommandRelation;
begin
  if NewCount=fExtToolCount then exit;
  ExtToolCat:=FindCategoryByName(KeyCategoryToolMenuName);
  if NewCount>fExtToolCount then begin
    // increase available external tool commands
    while NewCount>fExtToolCount do begin
      Add(ExtToolCat,Format(srkmecExtTool,[fExtToolCount]),
           ecExtToolFirst+fExtToolCount,VK_UNKNOWN,[],VK_UNKNOWN,[]);
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
  XMLConfig:TXMLConfig; Prefix:AnsiString):boolean;
var a,b,p:integer;
  Name:ShortString;
  DefaultStr,NewValue:AnsiString;

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
  end;

// LoadFromXMLConfig
var FileVersion: integer;
begin
  FileVersion:=XMLConfig.GetValue(Prefix+'Version/Value',0);
  ExtToolCount:=XMLConfig.GetValue(Prefix+'ExternalToolCount/Value',0);
  for a:=0 to FRelations.Count-1 do begin
    Name:=lowercase(Relations[a].Name);
    for b:=1 to length(Name) do
      if not (Name[b] in ['a'..'z','A'..'Z','0'..'9']) then Name[b]:='_';
    with Relations[a] do
      DefaultStr:=IntToStr(Key1)+','+ShiftStateToStr(Shift1)
              +','+IntToStr(Key2)+','+ShiftStateToStr(Shift2);
    if FileVersion<2 then
      NewValue:=XMLConfig.GetValue(Prefix+Name,DefaultStr)
    else
      NewValue:=XMLConfig.GetValue(Prefix+Name+'/Value',DefaultStr);
    p:=1;
    with Relations[a] do begin
      Key1:=ReadNextInt;
      Shift1:=IntToShiftState(ReadNextInt);
      Key2:=ReadNextInt;
      Shift2:=IntToShiftState(ReadNextInt);
    end;
  end;
  Result:=true;
end;

function TKeyCommandRelationList.SaveToXMLConfig(
  XMLConfig:TXMLConfig; Prefix:AnsiString):boolean;
var a,b:integer;
  Name:ShortString;
  s:AnsiString;
begin
  XMLConfig.SetValue(Prefix+'Version/Value',KeyMappingFormatVersion);
  XMLConfig.SetValue(Prefix+'ExternalToolCount/Value',ExtToolCount);
  for a:=0 to FRelations.Count-1 do begin
    Name:=lowercase(Relations[a].Name);
    for b:=1 to length(Name) do
      if not (Name[b] in ['a'..'z','A'..'Z','0'..'9']) then Name[b]:='_';
    with Relations[a] do
      s:=IntToStr(Key1)+','+ShiftStateToStr(Shift1)
        +','+IntToStr(Key2)+','+ShiftStateToStr(Shift2);
    XMLConfig.SetValue(Prefix+Name+'/Value',s);
  end;
  Result:=true;
end;

function TKeyCommandRelationList.ShiftStateToStr(Shift:TShiftState):AnsiString;
var i:integer;
begin
  i:=0;
  if ssCtrl in Shift then inc(i,1);
  if ssShift in Shift then inc(i,2);
  if ssAlt in Shift then inc(i,4);
  Result:=IntToStr(i);
end;

function TKeyCommandRelationList.Find(AKey:Word; AShiftState:TShiftState;
  Areas: TCommandAreas):TKeyCommandRelation;
var a:integer;
begin
  Result:=nil;
  if AKey=VK_UNKNOWN then exit;
  for a:=0 to FRelations.Count-1 do with Relations[a] do begin
    if Parent.Areas*Areas=[] then continue;
    if ((Key1=AKey) and (Shift1=AShiftState)) 
    or ((Key2=AKey) and (Shift2=AShiftState)) then begin
      Result:=Relations[a];
      exit;
    end;
  end;
end;

function TKeyCommandRelationList.FindByCommand(
  ACommand:word):TKeyCommandRelation;
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
  ASynEditKeyStrokes:TSynEditKeyStrokes; Areas: TCommandAreas);
var
  a,b,MaxKeyCnt,KeyCnt:integer;
  Key: TSynEditKeyStroke;
  CurRelation: TKeyCommandRelation;
begin
  for a:=0 to FRelations.Count-1 do begin
    CurRelation:=Relations[a];
    if (CurRelation.Key1=VK_UNKNOWN)
    or ((CurRelation.Parent.Areas*Areas)=[]) then
      MaxKeyCnt:=0
    else if CurRelation.Key2=VK_UNKNOWN then
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
          Key.Key:=CurRelation.Key1;
          Key.Shift:=CurRelation.Shift1;
          Key.Key2:=VK_UNKNOWN;
          Key.Shift2:=[];
        end else if KeyCnt=2 then begin
          // Define key2 for this command
          Key.Key:=CurRelation.Key2;
          Key.Shift:=CurRelation.Shift2;
          Key.Key2:=VK_UNKNOWN;
          Key.Shift2:=[];
        end;
        inc(KeyCnt);
      end;
      dec(b);
    end;
    // add missing keys
    while KeyCnt<=MaxKeyCnt do begin
      Key:=ASynEditKeyStrokes.Add;
      Key.Command:=CurRelation.Command;
      if KeyCnt=1 then begin
        Key.Key:=CurRelation.Key1;
        Key.Shift:=CurRelation.Shift1;
      end else begin
        Key.Key:=CurRelation.Key2;
        Key.Shift:=CurRelation.Shift2;
      end;
      Key.Key2:=VK_UNKNOWN;
      Key.Shift2:=[];
      inc(KeyCnt);
    end;
  end;
end;

function TKeyCommandRelationList.GetCategory(Index: integer): TKeyCommandCategory;
begin
  Result:=TKeyCommandCategory(fCategories[Index]);
end;

function TKeyCommandRelationList.CategoryCount: integer;
begin
  Result:=fCategories.Count;
end;

function TKeyCommandRelationList.AddCategory(const Name, Description: string;
  TheAreas: TCommandAreas): integer;
begin
  Result:=fCategories.Add(TKeyCommandCategory.Create(Name,Description,TheAreas));
end;

function TKeyCommandRelationList.FindCategoryByName(const CategoryName: string
  ): TKeyCommandCategory;
var i: integer;
begin
  for i:=0 to CategoryCount-1 do
    if CategoryName=Categories[i].Name then begin
      Result:=Categories[i];
      exit;
    end;
  Result:=nil;
end;

function TKeyCommandRelationList.TranslateKey(AKey: Word;
  AShiftState: TShiftState; Areas: TCommandAreas): word;
var
  ARelation: TKeyCommandRelation;
begin
  ARelation:=Find(AKey,AShiftState,Areas);
  if ARelation<>nil then
    Result:=ARelation.Command
  else
    Result:=ecNone;
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
  Name:='';
  Description:='';
  inherited Clear;
end;

procedure TKeyCommandCategory.Delete(Index: Integer);
begin
  TObject(Items[Index]).Free;
  inherited Delete(Index);
end;

constructor TKeyCommandCategory.Create(const AName, ADescription: string;
  TheAreas: TCommandAreas);
begin
  inherited Create;
  Name:=AName;
  Description:=ADescription;
  Areas:=TheAreas;
end;


//------------------------------------------------------------------------------
initialization
  KeyMappingEditForm:=nil;
  
finalization
  VirtualKeyStrings.Free;
  VirtualKeyStrings:=nil;

end.

