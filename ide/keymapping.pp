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

  ToDo:
}
unit KeyMapping;

{$mode objfpc}{$H+}

interface

uses
  LCLLinux, LCLType,
  Forms, Classes, SysUtils, Buttons, LResources, StdCtrls, Controls,
  SynEdit, SynEditKeyCmds, Laz_XMLCfg, Dialogs, StringHashList;

const
  { editor commands constants. see syneditkeycmds.pp for more
  
   These values can change from version to version, so DO NOT save them to file!
  
  }
  ecNone                 = SynEditKeyCmds.ecNone;
  
  ecFind                 = ecUserFirst + 1;
  ecFindAgain            = ecUserFirst + 2;
  ecFindNext             = ecFindAgain;
  ecReplace              = ecUserFirst + 3;
  ecFindProcedureDefinition = ecUserFirst + 4;
  ecFindProcedureMethod  = ecUserFirst + 5;
  ecGotoLineNumber       = ecUserFirst + 6;

  ecNextEditor           = ecUserFirst + 7;
  ecPrevEditor           = ecUserFirst + 8;
  ecMoveEditorLeft       = ecUserFirst + 9;
  ecMoveEditorRight      = ecUserFirst + 10;

  ecPeriod               = ecUserFirst + 11;

  ecFindPrevious         = ecUserFirst + 12;
  ecFindInFiles          = ecUserFirst + 13;
  ecJumpBack             = ecUserFirst + 14;
  ecJumpForward          = ecUserFirst + 15;
  ecAddJumpPoint         = ecUserFirst + 16;
  ecViewJumpHistory      = ecUserFirst + 17;

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
  ecSelectToBrace        = ecUserFirst + 55;
  ecSelectCodeBlock      = ecUserFirst + 56;
  ecSelectLine           = ecUserFirst + 57;
  ecSelectParagraph      = ecUserFirst + 58;
  
  ecInsertGPLNotice      = ecUserFirst + 80;
  ecInsertUserName       = ecUserFirst + 81;
  ecInsertDateTime       = ecUserFirst + 82;
  ecInsertChangeLogEntry = ecUserFirst + 83;
  ecInsertCVSAuthor      = ecUserFirst + 84;
  ecInsertCVSDate        = ecUserFirst + 85;
  ecInsertCVSHeader      = ecUserFirst + 86;
  ecInsertCVSID          = ecUserFirst + 87;
  ecInsertCVSLog         = ecUserFirst + 88;
  ecInsertCVSName        = ecUserFirst + 89;
  ecInsertCVSRevision    = ecUserFirst + 90;
  ecInsertCVSSource      = ecUserFirst + 91;

  ecWordCompletion       = ecUserFirst + 100;
  ecCompleteCode         = ecUserFirst + 101;
  ecIdentCompletion      = ecUserFirst + 102;
  ecSyntaxCheck          = ecUserFirst + 103;
  ecGuessUnclosedBlock   = ecUserFirst + 104;
  ecGuessMisplacedIFDEF  = ecUserFirst + 105;
  ecConvertDFM2LFM       = ecUserFirst + 106;

  ecNew                  = ecUserFirst + 201;
  ecNewUnit              = ecUserFirst + 202;
  ecNewForm              = ecUserFirst + 203;
  ecOpen                 = ecUserFirst + 204;
  ecRevert               = ecUserFirst + 205;
  ecSave                 = ecUserFirst + 206;
  ecSaveAs               = ecUserFirst + 207;
  ecSaveAll              = ecUserFirst + 208;
  ecClose                = ecUserFirst + 209;
  ecCloseAll             = ecUserFirst + 210;
  ecQuit                 = ecUserFirst + 211;

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
  ecAddCurUnitToProj     = ecUserFirst + 705;
  ecRemoveFromProj       = ecUserFirst + 706;
  ecViewProjectSource    = ecUserFirst + 707;
  ecProjectOptions       = ecUserFirst + 708;

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
      Caption:='Edit Keys';
      with KeyCommandRelationList.Relations[Index] do begin
        CommandLabel.Caption:='Command: '+Name;
        if Key1<>VK_UNKNOWN then begin
          Key1CtrlCheckBox.Checked:=ssCtrl in Shift1;
          Key1AltCheckBox.Checked:=ssAlt in Shift1;
          Key1ShiftCheckBox.Checked:=ssShift in Shift1;
          InitComboBox(Key1KeyComboBox,Key1);
        end;
        if Key2<>VK_UNKNOWN then begin
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
    ecNone: Result:= 'None';
    ecLeft: Result:= 'Left';
    ecRight: Result:= 'Right';
    ecUp: Result:= 'Up';
    ecDown: Result:= 'Down';
    ecWordLeft: Result:= 'Word Left';
    ecWordRight: Result:= 'Word Right';
    ecLineStart: Result:= 'Line Start';
    ecLineEnd: Result:= 'Line End';
    ecPageUp: Result:= 'Page Up';
    ecPageDown: Result:= 'Page Down';
    ecPageLeft: Result:= 'Page Left';
    ecPageRight: Result:= 'Page Right';
    ecPageTop: Result:= 'Page Top';
    ecPageBottom: Result:= 'Page Bottom';
    ecEditorTop: Result:= 'Editor Top';
    ecEditorBottom: Result:= 'Editor Bottom';
    ecGotoXY: Result:= 'Goto XY';
    ecSelLeft: Result:= 'SelLeft';
    ecSelRight: Result:= 'SelRight';
    ecSelUp: Result:= 'Sel Up';
    ecSelDown: Result:= 'Sel Down';
    ecSelWordLeft: Result:= 'Sel Word Left';
    ecSelWordRight: Result:= 'Sel Word Right';
    ecSelLineStart: Result:= 'Sel Line Start';
    ecSelLineEnd: Result:= 'Sel Line End';
    ecSelPageUp: Result:= 'Sel Page Up';
    ecSelPageDown: Result:= 'Sel Page Down';
    ecSelPageLeft: Result:= 'Sel Page Left';
    ecSelPageRight: Result:= 'Sel Page Right';
    ecSelPageTop: Result:= 'Sel Page Top';
    ecSelPageBottom: Result:= 'Sel Page Bottom';
    ecSelEditorTop: Result:= 'Sel Editor Top';
    ecSelEditorBottom: Result:= 'Sel Editor Bottom';
    ecSelGotoXY: Result:= 'Sel Goto XY';
    ecSelectAll: Result:= 'Select All';
    ecDeleteLastChar: Result:= 'Delete Last Char';
    ecDeleteChar: Result:= 'Delete Char';
    ecDeleteWord: Result:= 'Delete Word';
    ecDeleteLastWord: Result:= 'Delete Last Word';
    ecDeleteBOL: Result:= 'Delete BOL';
    ecDeleteEOL: Result:= 'Delete EOL';
    ecDeleteLine: Result:= 'Delete Line';
    ecClearAll: Result:= 'Clear All';
    ecLineBreak: Result:= 'Line Break';
    ecInsertLine: Result:= 'Insert Line';
    ecChar: Result:= 'Char';
    ecImeStr: Result:= 'Ime Str';
    ecUndo: Result:= 'Undo';
    ecRedo: Result:= 'Redo';
    ecCut: Result:= 'Cut';
    ecCopy: Result:= 'Copy';
    ecPaste: Result:= 'Paste';
    ecScrollUp: Result:= 'Scroll Up';
    ecScrollDown: Result:= 'Scroll Down';
    ecScrollLeft: Result:= 'Scroll Left';
    ecScrollRight: Result:= 'Scroll Right';
    ecInsertMode: Result:= 'Insert Mode';
    ecOverwriteMode: Result:= 'Overwrite Mode';
    ecToggleMode: Result:= 'Toggle Mode';
    ecBlockIndent: Result:= 'Block Indent';
    ecBlockUnindent: Result:= 'Block Unindent';
    ecTab: Result:= 'Tab';
    ecShiftTab: Result:= 'Shift Tab';
    ecMatchBracket: Result:= 'Match Bracket';
    ecNormalSelect: Result:= 'Normal Select';
    ecColumnSelect: Result:= 'Column Select';
    ecLineSelect: Result:= 'Line Select';
    ecAutoCompletion: Result:= 'Auto Completion';
    ecUserFirst: Result:= 'User First';
    ecGotoMarker0: Result:= 'Goto Marker 0';
    ecGotoMarker1: Result:= 'Goto Marker 1';
    ecGotoMarker2: Result:= 'Goto Marker 2';
    ecGotoMarker3: Result:= 'Goto Marker 3';
    ecGotoMarker4: Result:= 'Goto Marker 4';
    ecGotoMarker5: Result:= 'Goto Marker 5';
    ecGotoMarker6: Result:= 'Goto Marker 6';
    ecGotoMarker7: Result:= 'Goto Marker 7';
    ecGotoMarker8: Result:= 'Goto Marker 8';
    ecGotoMarker9: Result:= 'Goto Marker 9';
    ecSetMarker0: Result:= 'Set Marker 0';
    ecSetMarker1: Result:= 'Set Marker 1';
    ecSetMarker2: Result:= 'Set Marker 2';
    ecSetMarker3: Result:= 'Set Marker 3';
    ecSetMarker4: Result:= 'Set Marker 4';
    ecSetMarker5: Result:= 'Set Marker 5';
    ecSetMarker6: Result:= 'Set Marker 6';
    ecSetMarker7: Result:= 'Set Marker 7';
    ecSetMarker8: Result:= 'Set Marker 8';
    ecSetMarker9: Result:= 'Set Marker 9';
    ecPeriod: Result:= 'period';

    // sourcenotebook
    ecJumpToEditor: Result:='jump to editor';
    ecNextEditor: Result:= 'next editor';
    ecPrevEditor: Result:= 'previous editor';
    ecMoveEditorLeft: Result:= 'move editor left';
    ecMoveEditorRight: Result:= 'move editor right';
    ecGotoEditor1: Result:='goto editor 1';
    ecGotoEditor2: Result:='goto editor 2';
    ecGotoEditor3: Result:='goto editor 3';
    ecGotoEditor4: Result:='goto editor 4';
    ecGotoEditor5: Result:='goto editor 5';
    ecGotoEditor6: Result:='goto editor 6';
    ecGotoEditor7: Result:='goto editor 7';
    ecGotoEditor8: Result:='goto editor 8';
    ecGotoEditor9: Result:='goto editor 9';
    ecGotoEditor0: Result:='goto editor 10';

    // file menu
    ecNew: Result:='new';
    ecNewUnit: Result:='new unit';
    ecNewForm: Result:='new form';
    ecOpen: Result:= 'open';
    ecRevert: Result:= 'revert';
    ecSave: Result:= 'save';
    ecSaveAs: Result:= 'save as';
    ecSaveAll: Result:= 'save all';
    ecClose: Result:= 'close';
    ecCloseAll: Result:= 'close all';
    ecQuit: Result:= 'quit';
    
    // edit menu
    ecSelectionUpperCase: Result:='Selection uppercase';
    ecSelectionLowerCase: Result:='Selection lowercase';
    ecSelectionTabs2Spaces: Result:='Selection tabs to spaces';
    ecSelectionComment: Result:='Comment selection';
    ecSelectionUncomment: Result:='Uncomment selection';
    ecSelectToBrace: Result:= 'Select to brace';
    ecSelectCodeBlock: Result:= 'Select code block';
    ecSelectLine: Result:= 'Select line';
    ecSelectParagraph: Result:= 'Select paragraph';
    ecInsertGPLNotice: Result:='Insert GPL notice';
    ecInsertUserName: Result:='Insert current username';
    ecInsertDateTime: Result:='Insert current date and time';
    ecInsertChangeLogEntry: Result:='Insert ChangeLog entry';
    ecInsertCVSAuthor: Result:='Insert CVS keyword Author';
    ecInsertCVSDate: Result:='Insert CVS keyword Date';
    ecInsertCVSHeader: Result:='Insert CVS keyword Header';
    ecInsertCVSID: Result:='Insert CVS keyword ID';
    ecInsertCVSLog: Result:='Insert CVS keyword Log';
    ecInsertCVSName: Result:='Insert CVS keyword Name';
    ecInsertCVSRevision: Result:='Insert CVS keyword Revision';
    ecInsertCVSSource: Result:='Insert CVS keyword Source';

    // search menu
    ecFind: Result:= 'Find text';
    ecFindNext: Result:= 'Find next';
    ecFindPrevious: Result:= 'Find Previous';
    ecFindInFiles: Result:= 'Find in files';
    ecReplace: Result:= 'Replace text';
    ecFindProcedureDefinition: Result:= 'find procedure definition';
    ecFindProcedureMethod: Result:= 'find procedure method';
    ecGotoLineNumber: Result:= 'goto line number';
    ecJumpBack: Result:='jump back';
    ecJumpForward: Result:='jump forward';
    ecAddJumpPoint: Result:='add jump point';
    ecViewJumpHistory: Result:='view jump history';
    ecOpenFileAtCursor: Result:='open file at cursor';
    ecGotoIncludeDirective: Result:='goto to include directive of current include file';

    // view menu
    ecToggleFormUnit: Result:= 'switch between form and source';
    ecToggleObjectInsp: Result:= 'view object inspector';
    ecToggleProjectExpl: Result:= 'view project explorer';
    ecToggleCodeExpl: Result:= 'view code explorer';
    ecToggleMessages: Result:= 'view messages';
    ecToggleWatches: Result:= 'view watches';
    ecToggleBreakPoints: Result:= 'view breakpoints';
    ecToggleDebuggerOut: Result:= 'view debugger output';
    ecToggleLocals: Result := 'view local variables';
    ecToggleCallStack: Result := 'view call stack';
    ecViewUnits: Result:= 'view units';
    ecViewForms: Result:= 'view forms';
    ecViewUnitDependencies: Result:= 'view unit dependencies';

    // codetools
    ecWordCompletion: Result:= 'word completion';
    ecCompleteCode: Result:= 'complete code';
    ecIdentCompletion: Result:= 'identifier completion';
    ecSyntaxCheck: Result:='syntax check';
    ecGuessUnclosedBlock: Result:='guess unclosed block';
    ecGuessMisplacedIFDEF: Result:='guess misplaced $IFDEF';
    ecConvertDFM2LFM: Result:='convert DFM file to LFM';
    ecFindDeclaration: Result:='find declaration';
    ecFindBlockOtherEnd: Result:='find block other end';
    ecFindBlockStart: Result:='find block start';

    // project
    ecNewProject: Result:='New project';
    ecNewProjectFromFile: Result:='New project from file';
    ecOpenProject: Result:='Open project';
    ecSaveProject: Result:='Save project';
    ecSaveProjectAs: Result:='Save project as';
    ecAddCurUnitToProj: Result:='Add active unit to project';
    ecRemoveFromProj: Result:='Remove active unit from project';
    ecViewProjectSource: Result:='View project source';
    ecProjectOptions: Result:='View project options';

    // run menu
    ecBuild: Result:= 'build program/project';
    ecBuildAll: Result:= 'build all files of program/project';
    ecRun: Result:= 'run program';
    ecPause: Result:= 'pause program';
    ecStepInto: Result:= 'step into';
    ecStepOver: Result:= 'step over';
    ecRunToCursor: Result:= 'run to cursor';
    ecStopProgram: Result:= 'stop program';
    ecRunParameters: Result:= 'run parameters';
    ecCompilerOptions: Result:= 'compiler options';
    
    // tools menu
    ecExtToolSettings: Result:= 'external tools settings';
    ecConfigBuildLazarus: Result:= 'configure build-lazarus';
    ecBuildLazarus: Result:= 'build lazarus';
    ecExtToolFirst..ecExtToolLast:
              Result:='external tool '+IntToStr(cmd-ecExtToolFirst+1);

    // environment menu
    ecEnvironmentOptions: Result:= 'environment options';
    ecEditorOptions: Result:= 'editor options';
    ecCodeToolsOptions: Result:= 'codetools options';
    ecCodeToolsDefinesEd: Result:= 'codetools defines editor';

    // help menu
    ecAboutLazarus: Result:= 'about lazarus';

    else
      Result:='unknown editor command';
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
        if Protocol<>nil then begin
          Protocol.Add('Conflict '+IntToStr(Result));
          Protocol.Add('    command1 "'
            +EditorCommandToDescriptionString(Key1.Command)+'"'
            +'->'+KeyAndShiftStateToStr(Key1.Key,Key1.Shift));
          Protocol.Add(' conflicts with ');
          Protocol.Add('    command2 "'
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
    AddStr(' ');
    case Key of
    VK_UNKNOWN    :AddStr('Unknown');
    VK_LBUTTON    :AddStr('Mouse Button Left');
    VK_RBUTTON    :AddStr('Mouse Button Right');
    VK_CANCEL     :AddStr('Cancel');
    VK_MBUTTON    :AddStr('Mouse Button Middle');
    VK_BACK       :AddStr('Backspace');
    VK_TAB        :AddStr('Tab');
    VK_CLEAR      :AddStr('Clear');
    VK_RETURN     :AddStr('Return');
    VK_SHIFT      :AddStr('Shift');
    VK_CONTROL    :AddStr('Control');
    VK_MENU       :AddStr('Menu');
    VK_PAUSE      :AddStr('Pause');
    VK_CAPITAL    :AddStr('Capital');
    VK_KANA       :AddStr('Kana');
  //  VK_HANGUL     :AddStr('Hangul');
    VK_JUNJA      :AddStr('Junja');
    VK_FINAL      :AddStr('Final');
    VK_HANJA      :AddStr('Hanja');
  //  VK_KANJI      :AddStr('Kanji');
    VK_ESCAPE     :AddStr('Escape');
    VK_CONVERT    :AddStr('Convert');
    VK_NONCONVERT :AddStr('Nonconvert');
    VK_ACCEPT     :AddStr('Accept');
    VK_MODECHANGE :AddStr('Mode Change');
    VK_SPACE      :AddStr('Space');
    VK_PRIOR      :AddStr('Prior');
    VK_NEXT       :AddStr('Next');
    VK_END        :AddStr('End');
    VK_HOME       :AddStr('Home');
    VK_LEFT       :AddStr('Left');
    VK_UP         :AddStr('Up');
    VK_RIGHT      :AddStr('Right');
    VK_DOWN       :AddStr('Down');
    VK_SELECT     :AddStr('Select');
    VK_PRINT      :AddStr('Print');
    VK_EXECUTE    :AddStr('Execute');
    VK_SNAPSHOT   :AddStr('Snapshot');
    VK_INSERT     :AddStr('Insert');
    VK_DELETE     :AddStr('Delete');
    VK_HELP       :AddStr('Help');
    VK_0..VK_9    :AddStr(IntToStr(Key-VK_0));
    VK_A..VK_Z    :AddStr(chr(ord('A')+Key-VK_A));
    VK_LWIN       :AddStr('left windows key');
    VK_RWIN       :AddStr('right windows key');
    VK_APPS       :AddStr('application key');
    VK_NUMPAD0..VK_NUMPAD9: begin AddStr('Numpad ');AddStr(IntToStr(Key-VK_NUMPAD0)); end;
    VK_MULTIPLY   :AddStr('*');
    VK_ADD        :AddStr('+');
    VK_SEPARATOR  :AddStr('|');
    VK_SUBTRACT   :AddStr('-');
    VK_DECIMAL    :AddStr('.');
    VK_DIVIDE     :AddStr('/');
    VK_F1..VK_F24 :begin AddStr('F'); AddStr(IntToStr(Key-VK_F1+1)); end;
    VK_NUMLOCK    :AddStr('Numlock');
    VK_SCROLL     :AddStr('Scroll');
    VK_EQUAL      :AddStr('=');
    VK_COMMA      :AddStr(',');
    VK_POINT      :AddStr('.');
    VK_SLASH      :AddStr('/');
    VK_AT         :AddStr('@');
    else
      if (Key>=VK_IRREGULAR+33) and (Key<=VK_IRREGULAR+255) then
        AddStr(chr(Key-VK_IRREGULAR))
      else begin
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
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-200) div 2,(Screen.Height-270) div 2,216,310);
    Caption:='Edit keys for command';
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
      Show;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Caption:='Cancel';
      Left:=125;
      Top:=OkButton.Top;
      Width:=OkButton.Width;
      OnClick:=@CancelButtonClick;
      Show;
    end;

    CommandLabel:=TLabel.Create(Self);
    with CommandLabel do begin
      Name:='CommandLabel';
      Parent:=Self;
      Caption:='Command';
      Left:=5;
      Top:=5;
      Width:=Self.ClientWidth-Left-Left;
      Height:=20;
      Show;
    end;

    Key1GroupBox:=TGroupBox.Create(Self);
    with Key1GroupBox do begin
      Name:='Key1GroupBox';
      Parent:=Self;
      Caption:='Key';
      Left:=5;
      Top:=CommandLabel.Top+CommandLabel.Height+8;
      Width:=Self.ClientWidth-Left-Left;
      Height:=110;
      Show;
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
      Show;
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
      Show;
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
      Show;
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
      Show;
    end;
    
    Key1GrabButton:=TButton.Create(Self);
    with Key1GrabButton do begin
      Parent:=Key1GroupBox;
      Left:=5;
      Top:=Key1KeyComboBox.Top+Key1KeyComboBox.Height+5;
      Width:=Key1KeyComboBox.Width;
      Height:=25;
      Caption:='Grab Key';
      Name:='Key1GrabButton';
      OnClick:=@Key1GrabButtonClick;
      Show;
    end;

    Key2GroupBox:=TGroupBox.Create(Self);
    with Key2GroupBox do begin
      Name:='Key2GroupBox';
      Parent:=Self;
      Caption:='Alternative Key';
      Left:=5;
      Top:=Key1GroupBox.Top+Key1GroupBox.Height+8;
      Width:=Key1GroupBox.Width;
      Height:=110;
      Show;
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
      Show;
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
      Show;
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
      Show;
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
      Show;
    end;
    
    Key2GrabButton:=TButton.Create(Self);
    with Key2GrabButton do begin
      Parent:=Key2GroupBox;
      Left:=5;
      Top:=Key2KeyComboBox.Top+Key2KeyComboBox.Height+5;
      Width:=Key2KeyComboBox.Width;
      Height:=25;
      Caption:='Grab Key';
      Name:='Key2GrabButton';
      OnClick:=@Key2GrabButtonClick;
      Show;
    end;

  end;
  GrabbingKey:=0;
end;

procedure TKeyMappingEditForm.OkButtonClick(Sender:TObject);
var NewKey1,NewKey2:integer;
  NewShiftState1,NewShiftState2:TShiftState;
  ACaption,AText:AnsiString;
  DummyRelation, CurRelation:TKeyCommandRelation;
begin
  NewKey1:=VK_UNKNOWN;
  NewShiftState1:=[];
  NewKey2:=VK_UNKNOWN;
  NewShiftState2:=[];
  NewKey1:=StrToVKCode(Key1KeyComboBox.Text);
  if NewKey1<>VK_UNKNOWN then begin
    if Key1CtrlCheckBox.Checked then include(NewShiftState1,ssCtrl);
    if Key1AltCheckBox.Checked then include(NewShiftState1,ssAlt);
    if Key1ShiftCheckBox.Checked then include(NewShiftState1,ssShift);
  end;
  CurRelation:=KeyCommandRelationList.Relations[KeyIndex];
  DummyRelation:=KeyCommandRelationList.Find(NewKey1,NewShiftState1,
                                                      CurRelation.Parent.Areas);
  if (DummyRelation<>nil) 
  and (DummyRelation<>KeyCommandRelationList.Relations[KeyIndex]) then begin
    ACaption:='No No No';
    AText:=' The key "'+KeyAndShiftStateToStr(NewKey1,NewShiftState1)+'"'
            +' is already connected to "'+DummyRelation.Name+'".';
    MessageDlg(ACaption,AText,mterror,[mbok],0);
    exit;
  end;
  NewKey2:=StrToVKCode(Key2KeyComboBox.Text);
  if (NewKey1=NewKey2) and (NewShiftState1=NewShiftState2) then
    NewKey2:=VK_UNKNOWN;
  if NewKey2<>VK_UNKNOWN then begin
    if Key2CtrlCheckBox.Checked then include(NewShiftState2,ssCtrl);
    if Key2AltCheckBox.Checked then include(NewShiftState2,ssAlt);
    if Key2ShiftCheckBox.Checked then include(NewShiftState2,ssShift);
  end;
  DummyRelation:=KeyCommandRelationList.Find(NewKey2,NewShiftState2,
                                                      CurRelation.Parent.Areas);
  if (DummyRelation<>nil) 
  and (DummyRelation<>KeyCommandRelationList.Relations[KeyIndex]) then begin
    ACaption:='No No No';
    AText:=' The key "'+KeyAndShiftStateToStr(NewKey2,NewShiftState2)+'"'
            +' is already connected to "'+DummyRelation.Name+'".';
    MessageDlg(ACaption,AText,mterror,[mbok],0);
    exit;
  end;
  if NewKey1=VK_UNKNOWN then begin
    NewKey1:=NewKey2;
    NewShiftState1:=NewShiftState2;
    NewKey2:=VK_UNKNOWN;
  end;
  with CurRelation do begin
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
  for i:=0 to ComponentCount-1 do begin
    if (Components[i] is TWinControl) then
      TWinControl(Components[i]).Enabled:=true;
  end;
  if GrabbingKey=1 then
    Key1GrabButton.Caption:='Grab Key'
  else if GrabbingKey=2 then
    Key2GrabButton.Caption:='Grab Key';
  GrabbingKey:=0;
end;

procedure TKeyMappingEditForm.SetComboBox(AComboBox: TComboBox; AValue: string);
var i: integer;
begin
  i:=AComboBox.Items.IndexOf(AValue);
  if i>=0 then
    AComboBox.ItemIndex:=i
  else begin
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
  for i:=0 to ComponentCount-1 do begin
    if (Components[i] is TWinControl) then begin
      if ((GrabbingKey=1) and (Components[i]<>Key1GrabButton)
      and (Components[i]<>Key1GroupBox))
      or ((GrabbingKey=2) and (Components[i]<>Key2GrabButton)
      and (Components[i]<>Key2GroupBox)) then
        TWinControl(Components[i]).Enabled:=false;
    end;
  end;
  if GrabbingKey=1 then
    Key1GrabButton.Caption:='Please press a key ...'
  else if GrabbingKey=2 then
    Key2GrabButton.Caption:='Please press a key ...';
end;

procedure TKeyMappingEditForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift:TShiftState);
begin
  //writeln('TKeyMappingEditForm.FormKeyUp Sender=',Classname
  //   ,' Key=',Key,' Ctrl=',ssCtrl in Shift,' Shift=',ssShift in Shift
  //   ,' Alt=',ssAlt in Shift,' AsString=',KeyAndShiftStateToStr(Key,Shift)
  //   );
  if Key in [VK_CONTROL, VK_SHIFT, VK_LCONTROL, VK_RCONTROl,
             VK_LSHIFT, VK_RSHIFT] then exit;
  if (GrabbingKey in [1,2]) then begin
    if GrabbingKey=1 then begin
      Key1CtrlCheckBox.Checked:=(ssCtrl in Shift);
      Key1ShiftCheckBox.Checked:=(ssShift in Shift);
      Key1AltCheckBox.Checked:=(ssAlt in Shift);
      SetComboBox(Key1KeyComboBox,KeyAndShiftStateToStr(Key,[]));
    end else if GrabbingKey=2 then begin
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
  if Parent<>AValue then begin
    // unbind
    if Parent<>nil then begin
      Parent.Remove(Self);
    end;
    // bind
    fParent:=AValue;
    if Parent<>nil then begin
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
  C:=Categories[AddCategory('CursorMoving','Cursor moving commands',caSrcEditOnly)];
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
  C:=Categories[AddCategory('Selection','Text selection commands',caSrcEditOnly)];
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

  // editing
  C:=Categories[AddCategory('editing commands','Text editing commands',caSrcEditOnly)];
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
  C:=Categories[AddCategory('CommandCommands','Command commands',caAll)];
  Add(C,'Undo',ecUndo,VK_Z,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Redo',ecRedo,VK_Z,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  
  // search & replace
  C:=Categories[AddCategory('SearchReplace','Text search and replace commands',caSrcEditOnly)];
  Add(C,'Go to matching bracket',ecMatchBracket,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Find text',ecFind,VK_F,[SSCtrl],VK_UNKNOWN,[]);
  Add(C,'Find next',ecFindNext,VK_F3,[],VK_UNKNOWN,[]);
  Add(C,'Find previous',ecFindPrevious,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Find in files',ecFindInFiles,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Replace text',ecReplace,VK_R,[SSCtrl],VK_UNKNOWN,[]);
  Add(C,'Go to line number',ecGotoLineNumber,VK_G,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Jump back',ecJumpBack,VK_H,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Jump forward',ecJumpForward,VK_H,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  Add(C,'Add jump point',ecAddJumpPoint,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'View jump history',ecViewJumpHistory,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Open file at cursor',ecOpenFileAtCursor,VK_RETURN,[ssCtrl],VK_UNKNOWN,[]);

  // marker
  C:=Categories[AddCategory('Marker','Text marker commands',caSrcEditOnly)];
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
  C:=Categories[AddCategory('CodeTools','CodeTools commands',caSrcEditOnly)];
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
  C:=Categories[AddCategory('SourceNotebook','Source Notebook commands',caAll)];
  Add(C,'Go to next editor',ecNextEditor, VK_S, [ssShift,ssCtrl], VK_UNKNOWN, []);
  Add(C,'Go to prior editor',ecPrevEditor, VK_A, [ssShift,ssCtrl], VK_UNKNOWN, []);
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
  C:=Categories[AddCategory('FileMenu','File menu commands',caAll)];
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
  C:=Categories[AddCategory('ViewMenu','View menu commands',caAll)];
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
  C:=Categories[AddCategory('ProjectMenu','Project menu commands',caAll)];
  Add(C,'New project',ecNewProject,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'New project from file',ecNewProjectFromFile,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Open project',ecOpenProject,VK_F11,[ssCtrl],VK_UNKNOWN,[]);
  Add(C,'Save project',ecSaveProject,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Save project as',ecSaveProjectAs,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Add active unit to project',ecAddCurUnitToProj,VK_F11,[ssShift],VK_UNKNOWN,[]);
  Add(C,'Remove active unit from project',ecRemoveFromProj,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'View project source',ecViewProjectSource,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'View project options',ecProjectOptions,VK_F11,[ssShift,ssCtrl],VK_UNKNOWN,[]);

  // run menu
  C:=Categories[AddCategory('RunMenu','Run menu commands',caAll)];
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
  C:=Categories[AddCategory(KeyCategoryToolMenuName,'Tools menu commands',caAll)];
  Add(C,'External Tools settings',ecExtToolSettings,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Build Lazarus',ecBuildLazarus,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Configure "Build Lazarus"',ecConfigBuildLazarus,VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // environment menu
  C:=Categories[AddCategory('EnvironmentMenu','Environment menu commands',caAll)];
  Add(C,'General environment options',ecEnvironmentOptions,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'Editor options',ecEditorOptions,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'CodeTools options',ecCodeToolsOptions,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add(C,'CodeTools defines editor',ecCodeToolsDefinesEd,VK_UNKNOWN,[],VK_UNKNOWN,[]);

  // help menu
  C:=Categories[AddCategory('HelpMenu','Help menu commands',caAll)];
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
  if (Index<0) or (Index>=Count) then begin
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
      Add(ExtToolCat,'External tool '+IntToStr(fExtToolCount),
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
  Key:TSynEditKeyStroke;
begin
  for a:=0 to FRelations.Count-1 do begin
    if (Relations[a].Key1=VK_UNKNOWN)
    or ((Relations[a].Parent.Areas*Areas)=[]) then
      MaxKeyCnt:=0
    else if Relations[a].Key2=VK_UNKNOWN then
      MaxKeyCnt:=1
    else
      MaxKeyCnt:=2;
    KeyCnt:=1;
    b:=0;
    while b<ASynEditKeyStrokes.Count do begin
      if ASynEditKeyStrokes[b].Command=Relations[a].Command then begin
        if KeyCnt>MaxKeyCnt then begin
          ASynEditKeyStrokes[b].Free;
        end else if KeyCnt=1 then begin
          ASynEditKeyStrokes[b].Key:=Relations[a].Key1;
          ASynEditKeyStrokes[b].Shift:=Relations[a].Shift1;
          ASynEditKeyStrokes[b].Key2:=VK_UNKNOWN;
          ASynEditKeyStrokes[b].Shift2:=[];
          inc(b);
        end else if KeyCnt=2 then begin
          ASynEditKeyStrokes[b].Key:=Relations[a].Key2;
          ASynEditKeyStrokes[b].Shift:=Relations[a].Shift2;
          ASynEditKeyStrokes[b].Key2:=VK_UNKNOWN;
          ASynEditKeyStrokes[b].Shift2:=[];
          inc(b);
        end;
        inc(KeyCnt);
      end else inc(b);
    end;
    while KeyCnt<=MaxKeyCnt do begin
      Key:=ASynEditKeyStrokes.Add;
      Key.Command:=Relations[a].Command;
      if KeyCnt=1 then begin
        Key.Key:=Relations[a].Key1;
        Key.Shift:=Relations[a].Shift1;
      end else begin
        Key.Key:=Relations[a].Key2;
        Key.Shift:=Relations[a].Shift2;
      end;
      Key.Key2:=VK_UNKNOWN;
      Key.Shift2:=[];
      inc(KeyCnt);
    end;
  end;
end;

function TKeyCommandRelationList.GetCategory(Index: integer
  ): TKeyCommandCategory;
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

