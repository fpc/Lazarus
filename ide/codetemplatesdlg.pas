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
    A dialog for adding and editing code templates

}
unit CodeTemplatesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  ClipBrd, StdCtrls, Buttons, ExtCtrls, Menus, FileUtil, ButtonPanel,
  // synedit
  SynEdit, SynHighlighterPas, SynEditAutoComplete, SynRegExpr,
  // codetools
  CodeToolManager, CodeAtom, CodeCache, KeywordFuncLists, BasicCodeTools,
  PascalParserTool,
  // IDEIntf
  IDECommands, TextTools, SrcEditorIntf, MenuIntf, IDEWindowIntf, LazIDEIntf,
  IDEHelpIntf,
  // IDE
  IDEProcs, InputHistory, LazarusIDEStrConsts, EditorOptions, CodeMacroSelect,
  CodeMacroPrompt;

type
  TAutoCompleteOption = (
    acoLineBreak,
    acoSpace,
    acoWordEnd,
    acoRemoveChar
    );

const
  AutoCompleteOptionNames: array[TAutoCompleteOption] of shortstring = (
    'AutoOnLineBreak',
    'AutoOnSpace',
    'AutoOnWordEnd',
    'RemoveChar' // do not add the typed character
  );

type

  { TCodeTemplateDialog }

  TCodeTemplateDialog = class(TForm)
    AddButton: TButton;
    ASynPasSyn: TSynFreePascalSyn;
    AutoOnOptionsCheckGroup: TCheckGroup;
    ButtonPanel: TButtonPanel;
    EditTemplateGroupBox: TGroupBox;
    InsertMacroButton: TButton;
    KeepSubIndentCheckBox: TCheckBox;
    UseMacrosCheckBox: TCheckBox;
    RenameButton: TButton;
    DeleteButton: TButton;
    TemplateListBox: TListBox;
    TemplateSynEdit: TSynEdit;
    TemplatesGroupBox: TGroupBox;
    FilenameButton: TButton;
    FilenameEdit: TEdit;
    FilenameGroupBox: TGroupBox;
    MainPopupMenu: TPopupMenu;
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure RenameButtonClick(Sender: TObject);
    procedure FilenameButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure InsertMacroButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure OnCopyMenuItem(Sender: TObject);
    procedure OnCutMenuItem(Sender: TObject);
    procedure OnInsertMacroMenuItem(Sender: TObject);
    procedure OnPasteMenuItem(Sender: TObject);
    procedure TemplateListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    SynAutoComplete: TSynEditAutoComplete;
    LastTemplate: integer;
    procedure BuildPopupMenu;
    procedure DoInsertMacro;
  public
    procedure FillCodeTemplateListBox;
    procedure ShowCurCodeTemplate;
    procedure SaveCurCodeTemplate;
  end;

  { TCodeTemplateEditForm }

  TCodeTemplateEditForm = class(TForm)
    TokenLabel: TLabel;
    TokenEdit: TEdit;
    CommentLabel: TLabel;
    CommentEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    procedure CodeTemplateEditFormResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  public
    SynAutoComplete: TSynEditAutoComplete;
    TemplateIndex: integer;
    constructor Create(TheOwner: TComponent); override;
  end;

  { TLazCodeMacros }

  TLazCodeMacros = class(TIDECodeMacros)
  private
    FItems: TFPList; // list of TIDECodeMacro
  protected
    function GetItems(Index: integer): TIDECodeMacro; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Items[Index: integer]: TIDECodeMacro read GetItems; default;
    function Count: integer; override;
    function Add(Macro: TIDECodeMacro): integer; override;
    function FindByName(const AName: string): TIDECodeMacro; override;
    function CreateUniqueName(const AName: string): string; override;
  end;

function ShowCodeTemplateDialog: TModalResult;

function AddCodeTemplate(ASynAutoComplete: TSynEditAutoComplete;
  var Token, Comment: string): TModalResult;
function EditCodeTemplate(ASynAutoComplete: TSynEditAutoComplete;
  Index: integer): TModalResult;
  
procedure CreateStandardCodeMacros;

// standard code macros
function CodeMacroUpper(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroLower(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroPaste(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroProcedureHead(const Parameter: string;
                        InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroProcedureName(const Parameter: string;
                        InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroDate(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroTime(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroDateTime(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroAddMissingEnd(const Parameter: string;
                        InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroOfAll(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroPrevWord(const Parameter: string;
                        InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroWordAtCursor(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;

const
  CodeTemplatesMenuRootName = 'CodeTemplates';

var
  CodeTemplateCopyIDEMenuCommand: TIDEMenuCommand;
  CodeTemplateCutIDEMenuCommand: TIDEMenuCommand;
  CodeTemplatePasteIDEMenuCommand: TIDEMenuCommand;
  CodeTemplateInsertMacroIDEMenuCommand: TIDEMenuCommand;

procedure RegisterStandardCodeTemplatesMenuItems;

implementation

{$R *.lfm}

function ShowCodeTemplateDialog: TModalResult;
var
  CodeTemplateDialog: TCodeTemplateDialog;
begin
  CodeTemplateDialog:=TCodeTemplateDialog.Create(nil);
  Result:=CodeTemplateDialog.ShowModal;
  CodeTemplateDialog.Free;
end;

function AddCodeTemplate(ASynAutoComplete:TSynEditAutoComplete;
  var Token,Comment: string):TModalResult;
var
  CodeTemplateEditForm:TCodeTemplateEditForm;
begin
  Result:=mrCancel;
  CodeTemplateEditForm:=TCodeTemplateEditForm.Create(nil);
  try
    CodeTemplateEditForm.SynAutoComplete:=ASynAutoComplete;
    CodeTemplateEditForm.TemplateIndex:=ASynAutoComplete.Completions.Count;
    CodeTemplateEditForm.Caption:=lisCodeTemplAddCodeTemplate;
    CodeTemplateEditForm.OkButton.Caption:=lisCodeTemplAdd;
    CodeTemplateEditForm.TokenEdit.Text:=Token;
    CodeTemplateEditForm.CommentEdit.Text:=Comment;
    Result:=CodeTemplateEditForm.ShowModal;
    if Result=mrOk then begin
      Token:=CodeTemplateEditForm.TokenEdit.Text;
      Comment:=CodeTemplateEditForm.CommentEdit.Text;
    end;
  finally
    CodeTemplateEditForm.Free;
  end;
end;

function EditCodeTemplate(ASynAutoComplete:TSynEditAutoComplete;
  Index:integer):TModalResult;
var
  CodeTemplateEditForm:TCodeTemplateEditForm;
begin
  Result:=mrCancel;
  if (Index<0) or (Index>=ASynAutoComplete.Completions.Count) then exit;
  CodeTemplateEditForm:=TCodeTemplateEditForm.Create(nil);
  try
    CodeTemplateEditForm.SynAutoComplete:=ASynAutoComplete;
    CodeTemplateEditForm.TemplateIndex:=Index;
    CodeTemplateEditForm.Caption:=lisCodeTemplEditCodeTemplate;
    CodeTemplateEditForm.OkButton.Caption:=lisCodeTemplChange;
    CodeTemplateEditForm.TokenEdit.Text:=ASynAutoComplete.Completions[Index];
    CodeTemplateEditForm.CommentEdit.Text:=
      ASynAutoComplete.CompletionComments[Index];
    Result:=CodeTemplateEditForm.ShowModal;
    if Result=mrOk then begin
      ASynAutoComplete.Completions[Index]:=
        CodeTemplateEditForm.TokenEdit.Text;
      ASynAutoComplete.CompletionComments[Index]:=
        CodeTemplateEditForm.CommentEdit.Text;
    end;
  finally
    CodeTemplateEditForm.Free;
  end;
end;

function CodeMacroUpper(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
begin
  Value:=UpperCase(Parameter);
  Result:=true;
end;
                        
function CodeMacroLower(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
begin
  Value:=LowerCase(Parameter);
  Result:=true;
end;

function CodeMacroPaste(const Parameter: string; InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
begin
  Value:=Clipboard.AsText;
  Result:=true;
end;

function CodeMacroProcedureHead(const Parameter: string;
  InteractiveValue: TPersistent; SrcEdit: TSourceEditorInterface; var Value,
  ErrorMsg: string): boolean;
var
  Params: TStrings;
  Param: string;
  i: Integer;
  Attributes: TProcHeadAttributes;
  CodeBuf: TCodeBuffer;
  XY: TPoint;
  p: integer;
  StartPos: Integer;
begin
  //debugln('CodeMacroProcedureHead A ',Parameter);

  // parse attributes
  Params:=SplitString(Parameter,',');
  if Params<>nil then begin
    try
      Attributes:=[];
      for i:=0 to Params.Count-1 do begin
        Param:=Params[i];
        if SysUtils.CompareText(Param,'WithStart')=0 then
          Include(Attributes,phpWithStart)
        else if SysUtils.CompareText(Param,'WithStart')=0 then
          Include(Attributes,phpWithStart)
        else if SysUtils.CompareText(Param,'WithoutClassKeyword')=0 then
          Include(Attributes,phpWithoutClassKeyword)
        else if SysUtils.CompareText(Param,'AddClassName')=0 then
          Include(Attributes,phpAddClassName)
        else if SysUtils.CompareText(Param,'WithoutClassName')=0 then
          Include(Attributes,phpWithoutClassName)
        else if SysUtils.CompareText(Param,'WithoutName')=0 then
          Include(Attributes,phpWithoutName)
        else if SysUtils.CompareText(Param,'WithoutParamList')=0 then
          Include(Attributes,phpWithoutParamList)
        else if SysUtils.CompareText(Param,'WithVarModifiers')=0 then
          Include(Attributes,phpWithVarModifiers)
        else if SysUtils.CompareText(Param,'WithParameterNames')=0 then
          Include(Attributes,phpWithParameterNames)
        else if SysUtils.CompareText(Param,'WithoutParamTypes')=0 then
          Include(Attributes,phpWithoutParamTypes)
        else if SysUtils.CompareText(Param,'WithDefaultValues')=0 then
          Include(Attributes,phpWithDefaultValues)
        else if SysUtils.CompareText(Param,'WithResultType')=0 then
          Include(Attributes,phpWithResultType)
        else if SysUtils.CompareText(Param,'WithOfObject')=0 then
          Include(Attributes,phpWithOfObject)
        else if SysUtils.CompareText(Param,'WithCallingSpecs')=0 then
          Include(Attributes,phpWithCallingSpecs)
        else if SysUtils.CompareText(Param,'WithProcModifiers')=0 then
          Include(Attributes,phpWithProcModifiers)
        else if SysUtils.CompareText(Param,'WithComments')=0 then
          Include(Attributes,phpWithComments)
        else if SysUtils.CompareText(Param,'InUpperCase')=0 then
          Include(Attributes,phpInUpperCase)
        else if SysUtils.CompareText(Param,'CommentsToSpace')=0 then
          Include(Attributes,phpCommentsToSpace)
        else if SysUtils.CompareText(Param,'WithoutBrackets')=0 then
          Include(Attributes,phpWithoutBrackets)
        else if SysUtils.CompareText(Param,'WithoutSemicolon')=0 then
          Include(Attributes,phpWithoutSemicolon)
        else begin
          Result:=false;
          ErrorMsg:='Unknown Option: "'+Param+'"';
          exit;
        end;
      end;

    finally
      Params.Free;
    end;
  end;

  //debugln('CodeMacroProcedureHead B ',dbgs(Attributes));
  CodeBuf:=SrcEdit.CodeToolsBuffer as TCodeBuffer;
  XY:=SrcEdit.CursorTextXY;
  CodeBuf.LineColToPosition(XY.Y,XY.X,p);
  if p>0 then begin
    StartPos:=GetIdentStartPosition(CodeBuf.Source,p);
    XY.X += StartPos-p;
  end;
  if not CodeToolBoss.ExtractProcedureHeader(CodeBuf,XY.X,XY.Y,Attributes,Value)
  then begin
    Result:=false;
    ErrorMsg:=CodeToolBoss.ErrorMessage;
    LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;
  //debugln('CodeMacroProcedureHead C Value="',Value,'"');

  Result:=true;
end;

function CodeMacroProcedureName(const Parameter: string;
  InteractiveValue: TPersistent; SrcEdit: TSourceEditorInterface; var Value,
  ErrorMsg: string): boolean;
begin
  Result:=CodeMacroProcedureHead(
                          'WithoutParamList,WithoutBrackets,WithoutSemicolon',
                          InteractiveValue,SrcEdit,Value,ErrorMsg);
end;

function CodeMacroDate(const Parameter: string; InteractiveValue: TPersistent;
  SrcEdit: TSourceEditorInterface; var Value, ErrorMsg: string): boolean;
begin
  if Parameter<>'' then
    Value:=FormatDateTime(Parameter,Now)
  else
    Value:=DateToStr(Now);
  Result:=true;
end;

function CodeMacroTime(const Parameter: string; InteractiveValue: TPersistent;
  SrcEdit: TSourceEditorInterface; var Value, ErrorMsg: string): boolean;
begin
  if Parameter<>'' then
    Value:=FormatDateTime(Parameter,Now)
  else
    Value:=TimeToStr(Now);
  Result:=true;
end;

function CodeMacroDateTime(const Parameter: string;
  InteractiveValue: TPersistent; SrcEdit: TSourceEditorInterface; var Value,
  ErrorMsg: string): boolean;
begin
  if Parameter<>'' then
    Value:=FormatDateTime(Parameter,Now)
  else
    Value:=DateTimeToStr(Now);
  Result:=true;
end;

function CodeMacroAddMissingEnd(const Parameter: string;
  InteractiveValue: TPersistent; SrcEdit: TSourceEditorInterface; var Value,
  ErrorMsg: string): boolean;
{ checks if at current position a block end should be inserted
  Examples:
  
  No block end required:
    begin|
    end
    
    repeat|
    until
    
    begin|
      repeat
    
  Block end required:
    begin
      begin|
    end;
}
var
  Line: String;
  p: TPoint;
  CodeXYPos: TCodeXYPosition;
begin
  Result:=true;
  Value:='';
  Line:=SrcEdit.CurrentLineText;
  p:=SrcEdit.CursorTextXY;
  if p.y<1 then exit;
  CodeXYPos.X:=p.x;
  CodeXYPos.Y:=p.y;
  CodeXYPos.Code:=SrcEdit.CodeToolsBuffer as TCodeBuffer;
  if CodeXYPos.Code=nil then exit;

  // ToDo
  
  while (p.y<=SrcEdit.LineCount) do begin
    Line:=SrcEdit.Lines[p.y-1];
    while (p.x<=length(Line)) do begin
      if IsSpaceChar[Line[p.x]] then
        inc(p.x)
      else begin
        if CompareIdentifiers(@Line[p.x],'end')=0 then begin
          // has already an end
          exit;
        end else begin
          // missing end
          Value:=LineEnding+'end;'+LineEnding;
        end;
      end;
    end;
    inc(p.y);
    p.x:=1;
  end;
end;

function CodeMacroOfAll(const Parameter: string; InteractiveValue: TPersistent;
  SrcEdit: TSourceEditorInterface; var Value, ErrorMsg: string): boolean;
// completes
//  case SomeEnum of
//  <list of enums>
//  end;
var
  List: TStrings;
  Code: TCodeBuffer;
  CaretXY: TPoint;
  p: integer;
  i: Integer;
begin
  List:=TStringList.Create;
  try
    CaretXY:=SrcEdit.CursorTextXY;
    Code:=SrcEdit.CodeToolsBuffer as TCodeBuffer;
    Code.LineColToPosition(CaretXY.Y,CaretXY.X,p);
    if p<1 then begin
      ErrorMsg:='outside of code';
      exit(false);
    end;
    while (p>1) and (IsIdentChar[Code.Source[p-1]]) do
    begin
      dec(p);
      dec(CaretXY.X);
    end;
    if not CodeToolBoss.GetValuesOfCaseVariable(
      SrcEdit.CodeToolsBuffer as TCodeBuffer,
      CaretXY.X,CaretXY.Y,List) then
    begin
      Result:=false;
      ErrorMsg:=CodeToolBoss.ErrorMessage;
      if ErrorMsg='' then
        ErrorMsg:='missing case variable';
      LazarusIDE.DoJumpToCodeToolBossError;
      exit;
    end;

    Value:='';
    for i:=0 to List.Count-1 do
      Value:=Value+List[i]+': ;'+LineEnding;
  finally
    List.Free;
  end;

  Result:=true;
end;

function CodeMacroPrevWord(const Parameter: string;
  InteractiveValue: TPersistent; SrcEdit: TSourceEditorInterface; var Value,
  ErrorMsg: string): boolean;
{ gets word previous to the cursor position in current line
  Examples:

  line
  i 0 count-1 forb|

  with code template
  for $PrevWord(1) := $PrevWord(2) to $PrevWord(3) do // template:$PrevWord(0)
  begin
    |
  end;$PrevWord(-1)

  is expanded to
  for i := 0 to count-1 do // template:forb
  begin
    |
  end;

  if $PrevWord(2) is empty, then template
  is expanded to
  for i := | to  do // template:forb
  begin

  end;

  $PrevWord(0) expands to template itself, i.e. 'forb'
  $PrevWord(-1) expands to empty string and is used in the end
  if macro to delete words in the beginning of the line
}
var
  Line,s: String;
  p: TPoint;
  CodeXYPos: TCodeXYPosition;
  re : TRegExpr;
  iParam,lastword,firstword,Position : Integer;
  st: TStringList;
begin
  iParam:=StrToIntDef(Parameter,-1);
  Result:=true;
  Value:='';
  Line:=SrcEdit.CurrentLineText;
  p:=SrcEdit.CursorTextXY;
  if p.y<1 then exit;
  CodeXYPos.X:=p.x;
  CodeXYPos.Y:=p.y;
  CodeXYPos.Code:=SrcEdit.CodeToolsBuffer as TCodeBuffer;
  if CodeXYPos.Code=nil then exit;

  st:=TStringList.Create;
  re:=TRegExpr.Create;
  re.Expression:='[\w\-+*\(\)\[\].^@]+';
  if(re.Exec(Line))then
  begin
    firstword:=re.MatchPos[0];
    repeat
      st.Add(re.Match[0]);
      lastword:=re.MatchPos[0];
    until (not re.ExecNext);
  end;
  s:=st[st.count-1];
  st.Delete(st.count-1);
  st.Insert(0,s);
  if(iParam<0)then
  begin
    p.X:=SrcEdit.CursorTextXY.x;
    CodeXYPos.Code.LineColToPosition(CodeXYPos.Y,firstword,Position);
    CodeXYPos.Code.Delete(Position,lastword-firstword);
    p.X:=p.X-(lastword-firstword);
    SrcEdit.CursorTextXY:=p;
    Value:='';
  end
  else
  begin
    if(iParam<st.count)then
      Value:=st[iParam]
    else
      Value:='|';
  end;
  st.Free;
  re.Free;
end;

function CodeMacroWordAtCursor(const Parameter: string;
  InteractiveValue: TPersistent; SrcEdit: TSourceEditorInterface; var Value,
  ErrorMsg: string): boolean;
var
  SynEditor: TSynEdit;
begin
  SynEditor:=SrcEdit.EditorControl as TSynEdit;
  Value:=SynEditor.GetWordAtRowCol(SynEditor.LogicalCaretXY);
  Result:=true;
end;

function CodeMacroEditParam(const Parameter: string;
  InteractiveValue: TPersistent; SrcEdit: TSourceEditorInterface; var Value,
  ErrorMsg: string; TemplateParser: TIDETemplateParser): boolean;
var
  p: TLazTemplateParser;
  temp: TStringList;
  i, g: Integer;
  s: String;
begin
  p := TLazTemplateParser(TemplateParser);
  Value := Parameter;
  g := -1;
  temp := TStringList.Create;
  try
    s := Parameter;
    while length(s) > 0 do begin
      if s[1] = '"' then begin
        System.Delete(s, 1, 1);
        i := pos('"', s);
      end
      else
        i := pos(',', s);
      if i < 1 then
        i := length(s) + 1;
      temp.add(copy(s, 1, i - 1));
      System.Delete(s, 1, i);
    end;
    //temp.CommaText := Parameter;
    if temp.Count > 0 then begin
      Value := temp[0];
      temp.Delete(0);

      i := temp.IndexOfName('Sync');
      if i < 0 then
        i := temp.IndexOfName('S');
      if i >= 0 then
        i := StrToIntDef(temp.ValueFromIndex[i], -1)
      else
      if (temp.IndexOf('Sync') >= 0) or (temp.IndexOf('S') >= 0) then begin
        i := p.EditCellList.Count - 1;
        while i >= 0 do begin
          if TLazSynPluginSyncronizedEditCell(p.EditCellList[i]).CellValue = Value then
            break;
          dec(i);
        end;
      end;

      dec(i);
      if (i >= 0) and (i < p.EditCellList.Count)  then begin
        Value := TLazSynPluginSyncronizedEditCell(p.EditCellList[i]).CellValue;
        g := TLazSynPluginSyncronizedEditCell(p.EditCellList[i]).Group;
      end;
    end;
  finally
    temp.Free;
  end;
  with TLazSynPluginSyncronizedEditCell(p.EditCellList.AddNew) do begin
    LogStart := Point(p.DestPosX, p.DestPosY);
    LogEnd := Point(p.DestPosX + length(Value), p.DestPosY);
    if g < 0 then
      Group := p.EditCellList.Count
    else
      Group := g;
    CellValue := Value;
  end;
  Result := True;
end;

procedure RegisterStandardCodeTemplatesMenuItems;
var
  Path: string;
begin
  CodeTemplatesMenuRoot := RegisterIDEMenuRoot(CodeTemplatesMenuRootName);
  Path := CodeTemplatesMenuRoot.Name;
  CodeTemplateCutIDEMenuCommand := RegisterIDEMenuCommand(Path, 'Cut',
    lisMenuCut);
  CodeTemplateCopyIDEMenuCommand := RegisterIDEMenuCommand(Path, 'Copy',
    lisMenuCopy);
  CodeTemplatePasteIDEMenuCommand := RegisterIDEMenuCommand(Path, 'Paste',
    lisMenuPaste);
  CodeTemplateInsertMacroIDEMenuCommand := RegisterIDEMenuCommand(Path,
                                                'InsertMacro', lisInsertMacro);
end;

procedure CreateStandardCodeMacros;
begin
  IDECodeMacros:=TLazCodeMacros.Create;
  RegisterCodeMacro('Upper', lisUppercaseString,
                    lisUppercaseStringGivenAsParameter,
                    @CodeMacroUpper,nil);
  RegisterCodeMacro('Lower', lisLowercaseString,
                    lisLowercaseStringGivenAsParameter,
                    @CodeMacroLower,nil);
  RegisterCodeMacro('Paste', lisPasteClipboard,
                    lisPasteTextFromClipboard,
                    @CodeMacroPaste,nil);
  RegisterCodeMacro('ProcedureHead', lisInsertProcedureHead,
     'Insert header of current procedure'#13
    +#13
    +'Optional Parameters (comma separated):'#13
    +'WithStart,          // proc keyword e.g. ''function'', ''class procedure'''#13
    +'WithoutClassKeyword,// without ''class'' proc keyword'#13
    +'AddClassName,       // extract/add ClassName.'#13
    +'WithoutClassName,   // skip classname'#13
    +'WithoutName,        // skip function name'#13
    +'WithoutParamList,   // skip param list'#13
    +'WithVarModifiers,   // extract ''var'', ''out'', ''const'''#13
    +'WithParameterNames, // extract parameter names'#13
    +'WithoutParamTypes,  // skip colon, param types and default values'#13
    +'WithDefaultValues,  // extract default values'#13
    +'WithResultType,     // extract colon + result type'#13
    +'WithOfObject,       // extract ''of object'''#13
    +'WithCallingSpecs,   // extract cdecl; inline;'#13
    +'WithProcModifiers,  // extract forward; alias; external;'#13
    +'WithComments,       // extract comments and spaces'#13
    +'InUpperCase,        // turn to uppercase'#13
    +'CommentsToSpace,    // replace comments with a single space'#13
    +'                      //  (default is to skip unnecessary space,'#13
    +'                      //    e.g ''Do   ;'' normally becomes ''Do;'''#13
    +'                      //    with this option you get ''Do ;'')'#13
    +'WithoutBrackets,    // skip start- and end-bracket of parameter list'#13
    +'WithoutSemicolon,   // skip semicolon at end'#13,
    @CodeMacroProcedureHead,nil);
  RegisterCodeMacro('ProcedureName', lisInsertProcedureName,
                    lisInsertNameOfCurrentProcedure,
                    @CodeMacroProcedureName,nil);
  RegisterCodeMacro('Date', lisInsertDate,
                    lisInsertDateOptionalFormatString,
                    @CodeMacroDate,nil);
  RegisterCodeMacro('Time', lisInsertTime,
                    lisInsertTimeOptionalFormatString,
                    @CodeMacroTime,nil);
  RegisterCodeMacro('DateTime', lisInsertDateAndTime,
                    lisInsertDateAndTimeOptionalFormatString,
                    @CodeMacroDateTime,nil);
  RegisterCodeMacro('AddMissingEnd', lisInsertEndIfNeeded,
                     lisCheckIfTheNextTokenInSourceIsAnEndAndIfNotReturnsL,
                    @CodeMacroAddMissingEnd,nil);
  RegisterCodeMacro('OfAll', lisListOfAllCaseValues,
                    lisReturnsListOfAllValuesOfCaseVariableInFrontOfVaria,
                    @CodeMacroOfAll,nil);
  RegisterCodeMacro('WordAtCursor', lisGetWordAtCurrentCursorPosition,
                    lisGetWordAtCurrentCursorPosition,
                    @CodeMacroWordAtCursor,nil);
  RegisterCodeMacro('PrevWord', lisPrecedingWord,
                    'Returns parameter-indexed word from the current line preceding cursor position.'+LineEnding+LineEnding+
                    'Words in a line are numbered 1,2,3,... from left to right, but the last word'+LineEnding+
                    'which is always a macro command to be expanded has number 0, thus $PrevWord(0)'+LineEnding+
                    'is always the current macro'+LineEnding+LineEnding+
                    'Example line:'+LineEnding+
                    'i 0 count-1 forb|'+LineEnding+
                    'Here $PrevWord(0)=forb, $PrevWord(1)=1, $PrevWord(2)=0, $PrevWord(2)=count-1'+LineEnding+LineEnding+
                    'In the end of your template use $PrevWord(-1) which expands to an empty string, but performs an '+
                    'importaint operation of wiping off all of the $PrevWords found. In addition here is a regexp that is used'+
                    'to detect words for this macro: [\w\-+*\(\)\[\].^@]+',
                    @CodeMacroPrevWord,nil);
  RegisterCodeMacroEx('Param', lisTemplateEditParamCell,
                    lisTemplateEditParamCellHelp,
                    @CodeMacroEditParam,nil);
end;

{ TCodeTemplateEditForm }

constructor TCodeTemplateEditForm.Create(TheOwner:TComponent);
begin
  inherited CreateNew(TheOwner, 1);
  if LazarusResources.Find(ClassName)=nil then begin
    Width:=300;
    Height:=150;
    Position:=poScreenCenter;
    OnResize:=@CodeTemplateEditFormResize;

    TokenLabel:=TLabel.Create(Self);
    with TokenLabel do begin
      Name:='TokenLabel';
      Parent:=Self;
      Caption:=lisCodeTemplToken;
      Left:=12;
      Top:=6;
      Width:=Self.ClientWidth-Left-Left;
      Show;
    end;

    TokenEdit:=TEdit.Create(Self);
    with TokenEdit do begin
      Name:='TokenEdit';
      Parent:=Self;
      Left:=10;
      Top:=TokenLabel.Top+TokenLabel.Height+2;
      Width:=Self.ClientWidth-Left-Left-4;
      Text:='';
      Show;
    end;

    CommentLabel:=TLabel.Create(Self);
    with CommentLabel do begin
      Name:='CommentLabel';
      Parent:=Self;
      Caption:=lisCodeTemplComment;
      Left:=12;
      Top:=TokenEdit.Top+TokenEdit.Height+10;
      Width:=Self.ClientWidth-Left-Left;
      Show;
    end;

    CommentEdit:=TEdit.Create(Self);
    with CommentEdit do begin
      Name:='CommentEdit';
      Parent:=Self;
      Left:=10;
      Top:=CommentLabel.Top+CommentLabel.Height+2;
      Width:=Self.ClientWidth-Left-Left-4;
      Text:='';
      Show;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Caption:=lisLazBuildOk;
      OnClick:=@OkButtonClick;
      Left:=50;
      Top:=Self.ClientHeight-Height-12;
      Width:=80;
      Show;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Caption:=dlgCancel;
      ModalResult:=mrCancel;
      Width:=80;
      Left:=Self.ClientWidth-50-Width;
      Top:=Self.ClientHeight-Height-12;
      Show;
    end;
  end;
  CodeTemplateEditFormResize(nil);
end;

procedure TCodeTemplateEditForm.CodeTemplateEditFormResize(Sender: TObject);
begin
  with TokenLabel do begin
    Left:=12;
    Top:=6;
    Width:=Self.ClientWidth-Left-Left;
  end;

  with TokenEdit do begin
    Left:=10;
    Top:=TokenLabel.Top+TokenLabel.Height+2;
    Width:=Self.ClientWidth-Left-Left-4;
  end;

  with CommentLabel do begin
    Left:=12;
    Top:=TokenEdit.Top+TokenEdit.Height+10;
    Width:=Self.ClientWidth-Left-Left;
  end;

  with CommentEdit do begin
    Left:=10;
    Top:=CommentLabel.Top+CommentLabel.Height+2;
    Width:=Self.ClientWidth-Left-Left-4;
  end;

  with OkButton do begin
    Left:=50;
    Top:=Self.ClientHeight-Height-12;
    Width:=80;
  end;

  with CancelButton do begin
    Width:=80;
    Left:=Self.ClientWidth-50-Width;
    Top:=Self.ClientHeight-Height-12;
  end;
end;

procedure TCodeTemplateEditForm.OkButtonClick(Sender:TObject);
var a:integer;
  AText,ACaption:AnsiString;
begin
  a:=SynAutoComplete.Completions.IndexOf(TokenEdit.Text);
  if (a<0) or (a=TemplateIndex) then
    ModalResult:=mrOk
  else begin
    AText:=Format(lisCodeTemplATokenAlreadyExists, ['"', TokenEdit.Text, '"']);
    ACaption:=lisCodeTemplError;
    MessageDlg(ACaption,AText,mterror,[mbok],0);
  end;
end;

{ TCodeTemplateDialog }

procedure TCodeTemplateDialog.FormCreate(Sender: TObject);
var
  s: String;
  ColorScheme: String;
begin
  IDEDialogLayoutList.ApplyLayout(Self,600,450);

  SynAutoComplete:=TSynEditAutoComplete.Create(Self);
  LastTemplate:=-1;

  // init captions
  Caption:=dlgEdCodeTempl;
  AddButton.Caption:=lisCodeTemplAdd;
  RenameButton.Caption:=lisLazBuildRename;
  DeleteButton.Caption:=dlgEdDelete;
  TemplatesGroupBox.Caption:=lisCTDTemplates;

  ButtonPanel.OKButton.Caption:=lisOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=dlgCancel;

  FilenameGroupBox.Caption:=lisDebugOptionsFrmModule;
  UseMacrosCheckBox.Caption:=lisEnableMacros;
  InsertMacroButton.Caption:=lisInsertMacro;
  KeepSubIndentCheckBox.Caption:=lisKeepSubIndentation;
  KeepSubIndentCheckBox.Hint:=lisKeepRelativeIndentationOfMultiLineTemplate;
  AutoOnOptionsCheckGroup.Caption:=lisCodeTemplAutoCompleteOn;
  AutoOnOptionsCheckGroup.Items.Add(lisAutomaticallyOnLineBreak);
  AutoOnOptionsCheckGroup.Items.Add(lisAutomaticallyOnSpace);
  AutoOnOptionsCheckGroup.Items.Add(lisAutomaticallyOnWordEnd);
  AutoOnOptionsCheckGroup.Items.Add(lisAutomaticallyRemoveCharacter);

  FilenameEdit.Text:=EditorOpts.CodeTemplateFileName;

  // init synedit
  ColorScheme:=EditorOpts.ReadColorScheme(ASynPasSyn.GetLanguageName);
  EditorOpts.ReadHighlighterSettings(ASynPasSyn,ColorScheme);
  if EditorOpts.UseSyntaxHighlight then
    TemplateSynEdit.Highlighter:=ASynPasSyn
  else
    TemplateSynEdit.Highlighter:=nil;
  EditorOpts.SetMarkupColors(TemplateSynEdit);
  EditorOpts.GetSynEditSettings(TemplateSynEdit);
  EditorOpts.KeyMap.AssignTo(TemplateSynEdit.KeyStrokes,
                             TSourceEditorWindowInterface);
  TemplateSynEdit.Gutter.Visible:=false;

  // init SynAutoComplete
  with SynAutoComplete do begin
    s:=EditorOpts.CodeTemplateFileName;
    if FileExistsUTF8(s) then
      try
        AutoCompleteList.LoadFromFile(UTF8ToSys(s));
      except
        DebugLn('NOTE: unable to read code template file ''',s,'''');
      end;
  end;
  
  // init listbox
  FillCodeTemplateListBox;
  with TemplateListBox do
    if Items.Count>0 then begin
      ItemIndex:=0;
      ShowCurCodeTemplate;
    end;
    
  BuildPopupMenu;
end;

procedure TCodeTemplateDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TCodeTemplateDialog.InsertMacroButtonClick(Sender: TObject);
begin
  DoInsertMacro;
end;

procedure TCodeTemplateDialog.OkButtonClick(Sender: TObject);
var
  Res: TModalResult;
begin
  SaveCurCodeTemplate;

  EditorOpts.CodeTemplateFileName:=FilenameEdit.Text;
  //EditorOpts.CodeTemplateIndentToTokenStart:=
  //  (CodeTemplateIndentTypeRadioGroup.ItemIndex=0);

  EditorOpts.Save;

  if BuildBorlandDCIFile(SynAutoComplete) then begin
    Res:=mrOk;
    repeat
      try
        SynAutoComplete.AutoCompleteList.SaveToFile(
          UTF8ToSys(EditorOpts.CodeTemplateFileName));
      except
        res:=MessageDlg(' Unable to write code templates to file '''
          +EditorOpts.CodeTemplateFileName+'''! ',mtError
          ,[mbAbort, mbIgnore, mbRetry],0);
        if res=mrAbort then exit;
      end;
    until Res<>mrRetry;
  end;

  ModalResult:=mrOk;
end;

procedure TCodeTemplateDialog.OnCopyMenuItem(Sender: TObject);
begin
  TemplateSynEdit.CopyToClipboard;
end;

procedure TCodeTemplateDialog.OnCutMenuItem(Sender: TObject);
begin
  TemplateSynEdit.CutToClipboard;
end;

procedure TCodeTemplateDialog.OnInsertMacroMenuItem(Sender: TObject);
begin
  DoInsertMacro;
end;

procedure TCodeTemplateDialog.OnPasteMenuItem(Sender: TObject);
begin
  TemplateSynEdit.PasteFromClipboard;
end;

procedure TCodeTemplateDialog.AddButtonClick(Sender: TObject);
var
  Token: String;
  Comment: String;
  Index: PtrInt;
begin
  SaveCurCodeTemplate;
  Token:='new';
  Comment:='(custom)';
  if AddCodeTemplate(SynAutoComplete,Token,Comment)=mrOk then begin
    SynAutoComplete.AddCompletion(Token, '', Comment);
    FillCodeTemplateListBox;
    Index := SynAutoComplete.Completions.IndexOf(Token);
    if Index >= 0
    then Index := TemplateListBox.Items.IndexOfObject(TObject(Pointer(Index)));
    if Index >= 0
    then TemplateListBox.ItemIndex:=Index;
    
    ShowCurCodeTemplate;
  end;
end;

procedure TCodeTemplateDialog.DeleteButtonClick(Sender: TObject);
var
  a, idx: LongInt;
begin
  idx := TemplateListBox.ItemIndex;
  if idx < 0 then exit;
  a := PtrInt(TemplateListBox.Items.Objects[idx]);
  if a < 0 then exit;

  if MessageDlg(dlgDelTemplate
      +'"'+SynAutoComplete.Completions[a]+' - '
      +SynAutoComplete.CompletionComments[a]+'"'
      +'?',mtConfirmation,[mbOk,mbCancel],0)=mrOK
  then begin
    SynAutoComplete.DeleteCompletion(a);
    LastTemplate := -1; // to prevent the saving of the deleted template
    FillCodeTemplateListBox;
    if idx < TemplateListBox.Items.Count then begin
      TemplateListBox.ItemIndex := idx;
    end;
    ShowCurCodeTemplate;
  end;
end;

procedure TCodeTemplateDialog.RenameButtonClick(Sender: TObject);
var
  a, idx: LongInt;
begin
  idx := TemplateListBox.ItemIndex;
  if idx < 0 then exit;
  a := PtrInt(TemplateListBox.Items.Objects[idx]);
  if a < 0 then exit;

  if EditCodeTemplate(SynAutoComplete, a)=mrOk then begin
    TemplateListBox.Items[idx]:=
       SynAutoComplete.Completions[a]
       +' - "'+SynAutoComplete.CompletionComments[a]+'"';
    ShowCurCodeTemplate;
  end;
end;

procedure TCodeTemplateDialog.FilenameButtonClick(Sender: TObject);
var OpenDialog:TOpenDialog;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    with OpenDialog do begin
      Title:=dlgChsCodeTempl;
      Filter:='DCI file (*.dci)|*.dci|' + dlgAllFiles  + '|' + GetAllFilesMask;
      if Execute then
        FilenameEdit.Text:=FileName;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TCodeTemplateDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TCodeTemplateDialog.TemplateListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  SaveCurCodeTemplate;
  ShowCurCodeTemplate;
end;

procedure TCodeTemplateDialog.BuildPopupMenu;
begin
  CodeTemplateCopyIDEMenuCommand.OnClick:=@OnCopyMenuItem;
  CodeTemplateCutIDEMenuCommand.OnClick:=@OnCutMenuItem;
  CodeTemplatePasteIDEMenuCommand.OnClick:=@OnPasteMenuItem;
  CodeTemplateInsertMacroIDEMenuCommand.OnClick:=@OnInsertMacroMenuItem;

  // assign the root TMenuItem to the registered menu root.
  MainPopupMenu:=TPopupMenu.Create(Self);
  // This will automatically create all registered items
  CodeTemplatesMenuRoot.MenuItem := MainPopupMenu.Items;
  //MainPopupMenu.Items.WriteDebugReport('TMessagesView.Create ');
  
  PopupMenu:=MainPopupMenu;
end;

procedure TCodeTemplateDialog.DoInsertMacro;
var
  Macro: TIDECodeMacro;
  Parameter: string;
begin
  Macro:=ShowCodeMacroSelectDialog(Parameter);
  if Macro<>nil then begin
    TemplateSynEdit.SelText:='$'+Macro.Name+'('+Parameter+')';
  end;
end;

procedure TCodeTemplateDialog.FillCodeTemplateListBox;
var
  a: PtrInt;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    for a:=0 to SynAutoComplete.Completions.Count-1 do begin
      // Add the index in SynAutoComplete as Object, since both indexes won't
      // be in sync after sorting
      sl.AddObject(SynAutoComplete.Completions[a]
          +' - "'+SynAutoComplete.CompletionComments[a]+'"', TObject(Pointer(a)));
    end;
    sl.Sort;
    TemplateListBox.Items.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TCodeTemplateDialog.ShowCurCodeTemplate;
var
  EnableMacros, KeepSubIndent: boolean;
  LineCount: integer;

  procedure AddLine(const s: string);
  begin
    TemplateSynEdit.Lines.Add(s);
    inc(LineCount);
  end;

var
  idx, a, sp, ep: integer;
  s: string;
  AutoOnCat: array[TAutoCompleteOption] of Boolean;
  Attributes: TStrings;
  c: TAutoCompleteOption;
begin
  EnableMacros:=false;
  KeepSubIndent:=false;
  for c:=Low(TAutoCompleteOption) to High(TAutoCompleteOption) do
    AutoOnCat[c]:=false;
  
  LineCount := 0;
  idx := TemplateListBox.ItemIndex;
  // search template
  if idx >= 0
  then a := PtrInt(TemplateListBox.Items.Objects[idx])
  else a := -1;

  TemplateSynEdit.Lines.BeginUpdate;
  TemplateSynEdit.Lines.Clear;

  //debugln('TCodeTemplateDialog.ShowCurCodeTemplate A a=',dbgs(a));
  if a >= 0
  then begin
    EditTemplateGroupBox.Caption:=dbgstr(SynAutoComplete.Completions[a])
                           +' - '+dbgstr(SynAutoComplete.CompletionComments[a]);
    Attributes:=SynAutoComplete.CompletionAttributes[a];
    EnableMacros:=Attributes.IndexOfName(CodeTemplateEnableMacros)>=0;
    KeepSubIndent:=Attributes.IndexOfName(CodeTemplateKeepSubIndent)>=0;
    for c:=Low(TAutoCompleteOption) to High(TAutoCompleteOption) do
      AutoOnCat[c]:=Attributes.IndexOfName(AutoCompleteOptionNames[c])>=0;
    LastTemplate := -1;
    s:=SynAutoComplete.CompletionValues[a];
    //debugln('TCodeTemplateDialog.ShowCurCodeTemplate s="',s,'"');
    sp:=1;
    ep:=1;
    while ep<=length(s) do begin
      if s[ep] in [#10,#13] then begin
        AddLine(copy(s,sp,ep-sp));
        inc(ep);
        if (ep<=length(s)) and (s[ep] in [#10,#13]) and (s[ep-1]<>s[ep]) then
          inc(ep);
        sp:=ep;
      end else inc(ep);
    end;
    if (ep>sp) or ((s<>'') and (s[length(s)] in [#10,#13])) then
      AddLine(copy(s,sp,ep-sp));
  end else begin
    EditTemplateGroupBox.Caption:='no template selected';
  end;
  LastTemplate := a;
  TemplateSynEdit.Lines.EndUpdate;
  TemplateSynEdit.Invalidate;
  UseMacrosCheckBox.Checked:=EnableMacros;
  KeepSubIndentCheckBox.Checked:=KeepSubIndent;
  for c:=Low(TAutoCompleteOption) to High(TAutoCompleteOption) do
    AutoOnOptionsCheckGroup.Checked[ord(c)]:=AutoOnCat[c];
end;

procedure TCodeTemplateDialog.SaveCurCodeTemplate;
var
  a: LongInt;

  procedure SetBooleanAttribute(const AttrName: string; NewValue: boolean);
  var
    Attributes: TStrings;
    l: LongInt;
  begin
    Attributes:=SynAutoComplete.CompletionAttributes[a];
    if NewValue then
      Attributes.Values[AttrName]:='true'
    else begin
      l:=Attributes.IndexOfName(AttrName);
      if l>=0 then
        Attributes.Delete(l);
    end;
  end;

var
  NewValue: string;
  l: integer;
  c: TAutoCompleteOption;
begin
  if LastTemplate<0 then exit;
  a := LastTemplate;
  //DebugLn('TCodeTemplateDialog.SaveCurCodeTemplate A a=',dbgs(a));
  NewValue:=TemplateSynEdit.Lines.Text;
  // remove last EOL
  if NewValue<>'' then begin
    l:=length(NewValue);
    if NewValue[l] in [#10,#13] then begin
      dec(l);
      if (l>0) and (NewValue[l] in [#10,#13])
      and (NewValue[l]<>NewValue[l+1]) then
        dec(l);
      SetLength(NewValue,l);
    end;
  end;
  SynAutoComplete.CompletionValues[a]:=NewValue;
  
  SetBooleanAttribute(CodeTemplateEnableMacros,UseMacrosCheckBox.Checked);
  SetBooleanAttribute(CodeTemplateKeepSubIndent,KeepSubIndentCheckBox.Checked);
  for c:=low(TAutoCompleteOption) to High(TAutoCompleteOption) do
     SetBooleanAttribute(AutoCompleteOptionNames[c],AutoOnOptionsCheckGroup.Checked[ord(c)]);

  //DebugLn('TCodeTemplateDialog.SaveCurCodeTemplate NewValue="',NewValue,'" SynAutoComplete.CompletionValues[a]="',SynAutoComplete.CompletionValues[a],'"');
end;

{ TLazCodeMacros }

function TLazCodeMacros.GetItems(Index: integer): TIDECodeMacro;
begin
  Result:=TIDECodeMacro(FItems[Index]);
end;

constructor TLazCodeMacros.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TLazCodeMacros.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TLazCodeMacros.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do TObject(FItems[i]).Free;
  FItems.Clear;
end;

function TLazCodeMacros.Count: integer;
begin
  Result:=FItems.Count;
end;

function TLazCodeMacros.Add(Macro: TIDECodeMacro): integer;
begin
  if FindByName(Macro.Name)<>nil then
    RaiseGDBException('TLazCodeMacros.Add Name already exists');
  Result:=FItems.Add(Macro);
end;

function TLazCodeMacros.FindByName(const AName: string): TIDECodeMacro;
var
  i: LongInt;
begin
  i:=Count-1;
  while (i>=0) do begin
    Result:=Items[i];
    if (SysUtils.CompareText(Result.Name,AName)=0) then exit;
    dec(i);
  end;
  Result:=nil;
end;

function TLazCodeMacros.CreateUniqueName(const AName: string): string;
begin
  Result:=AName;
  if FindByName(Result)=nil then exit;
  Result:=CreateFirstIdentifier(Result);
  while FindByName(Result)<>nil do
    Result:=CreateNextIdentifier(Result);
end;

end.
