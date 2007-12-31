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
  ClipBrd, StdCtrls, Buttons, ExtCtrls, Menus, FileUtil,
  SynEdit, SynHighlighterPas, SynEditAutoComplete, CodeToolManager, CodeCache,
  KeywordFuncLists, BasicCodeTools, PascalParserTool,
  IDECommands, TextTools, SrcEditorIntf, MenuIntf, IDEWindowIntf, LazIDEIntf,
  IDEProcs, InputHistory, LazarusIDEStrConsts, EditorOptions, CodeMacroSelect;

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
    EditTemplateGroupBox: TGroupBox;
    OkButton: TBitBtn;
    CancelButton: TBitBtn;
    InsertMacroButton: TButton;
    BtnPanel: TPanel;
    UseMacrosCheckBox: TCheckBox;
    EditButton: TButton;
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
    procedure EditButtonClick(Sender: TObject);
    procedure FilenameButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
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
    constructor Create(TheOwner: TComponent); override;
    SynAutoComplete: TSynEditAutoComplete;
    TemplateIndex: integer;
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
function CodeMacroDateTime(const Parameter: string;
                        InteractiveValue: TPersistent;
                        SrcEdit: TSourceEditorInterface;
                        var Value, ErrorMsg: string): boolean;
function CodeMacroAddMissingEnd(const Parameter: string;
                        InteractiveValue: TPersistent;
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

function ShowCodeTemplateDialog: TModalResult;
var
  CodeTemplateDialog: TCodeTemplateDialog;
begin
  CodeTemplateDialog:=TCodeTemplateDialog.Create(nil);
  Result:=CodeTemplateDialog.ShowModal;
  CodeTemplateDialog.Free;
end;

function AddCodeTemplate(ASynAutoComplete:TSynEditAutoComplete;
  var Token,Comment:ansistring):TModalResult;
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

  //debugln('CodeMacroProcedureHead B ');
  if not CodeToolBoss.ExtractProcedureHeader(
    SrcEdit.CodeToolsBuffer as TCodeBuffer,
    SrcEdit.CursorTextXY.X,SrcEdit.CursorTextXY.Y,Attributes,Value) then
  begin
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
var
  Line: String;
  p: TPoint;
  CodeBuf: TCodeBuffer;
begin
  Result:=true;
  Value:='';
  Line:=SrcEdit.CurrentLineText;
  p:=SrcEdit.CursorTextXY;
  if p.y<1 then exit;
  CodeBuf:=SrcEdit.CodeToolsBuffer as TCodeBuffer;
  if CodeBuf=nil then exit;
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

procedure RegisterStandardCodeTemplatesMenuItems;
var
  Path: string;
begin
  CodeTemplatesMenuRoot := RegisterIDEMenuRoot(CodeTemplatesMenuRootName);
  Path := CodeTemplatesMenuRoot.Name;
  CodeTemplateCutIDEMenuCommand := RegisterIDEMenuCommand(Path,'Cut','Cut');
  CodeTemplateCopyIDEMenuCommand := RegisterIDEMenuCommand(Path,'Copy','Copy');
  CodeTemplatePasteIDEMenuCommand := RegisterIDEMenuCommand(Path,'Paste','Paste');
  CodeTemplateInsertMacroIDEMenuCommand := RegisterIDEMenuCommand(Path,
                                                  'InsertMacro','Insert Macro');
end;

procedure CreateStandardCodeMacros;
begin
  IDECodeMacros:=TLazCodeMacros.Create;
  RegisterCodeMacro('Upper','uppercase string',
                    'Uppercase string given as parameter',
                    @CodeMacroUpper,nil);
  RegisterCodeMacro('Lower','lowercase string',
                    'Lowercase string given as parameter',
                    @CodeMacroLower,nil);
  RegisterCodeMacro('Paste','paste clipboard',
                    'Paste text from clipboard',
                    @CodeMacroPaste,nil);
  RegisterCodeMacro('ProcedureHead','insert procedure head',
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
  RegisterCodeMacro('ProcedureName','insert procedure name',
                    'Insert name of current procedure',
                    @CodeMacroProcedureName,nil);
  RegisterCodeMacro('Date','insert date',
                    'Insert date. Optional: format string',
                    @CodeMacroDate,nil);
  RegisterCodeMacro('Time','insert time',
                    'Insert time. Optional: format string',
                    @CodeMacroTime,nil);
  RegisterCodeMacro('DateTime','insert date and time',
                    'Insert date and time. Optional: format string',
                    @CodeMacroDateTime,nil);
  RegisterCodeMacro('AddMissingEnd','insert end if needed',
                     'check if the next token in source'
                    +' is an end and if not returns lineend + end; + lineend',
                    @CodeMacroAddMissingEnd,nil);
end;

{ TCodeTemplateEditForm }

constructor TCodeTemplateEditForm.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
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

//    Application.MessageBox(PChar(AText),PChar(ACaption),0);
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
  EditButton.Caption:=lisCodeToolsDefsEdit;
  DeleteButton.Caption:=dlgEdDelete;
  CancelButton.Caption:=dlgCancel;
  TemplatesGroupBox.Caption:=lisCTDTemplates;
  OkButton.Caption:=lisLazBuildOk;
  FilenameGroupBox.Caption:=lisToDoLFile;
  UseMacrosCheckBox.Caption:=lisEnableMacros;
  InsertMacroButton.Caption:=lisCTInsertMacro;
  AutoOnOptionsCheckGroup.Caption:=lisCodeTemplAutoCompleteOn;
  AutoOnOptionsCheckGroup.Items.Add(lisAutomaticallyOnLineBreak);
  AutoOnOptionsCheckGroup.Items.Add(lisAutomaticallyOnSpace);
  AutoOnOptionsCheckGroup.Items.Add(lisAutomaticallyOnWordEnd);
  AutoOnOptionsCheckGroup.Items.Add(lisAutomaticallyRemoveCharacter);

  FilenameEdit.Text:=EditorOpts.CodeTemplateFileName;

  // init synedit
  ColorScheme:=EditorOpts.ReadColorScheme(ASynPasSyn.GetLanguageName);
  EditorOpts.AddSpecialHilightAttribsToHighlighter(ASynPasSyn);
  EditorOpts.ReadHighlighterSettings(ASynPasSyn,ColorScheme);
  if EditorOpts.UseSyntaxHighlight then
    TemplateSynEdit.Highlighter:=ASynPasSyn
  else
    TemplateSynEdit.Highlighter:=nil;
  EditorOpts.GetSynEditSettings(TemplateSynEdit);
  EditorOpts.KeyMap.AssignTo(TemplateSynEdit.KeyStrokes,
                             TSourceEditorWindowInterface);
  TemplateSynEdit.Gutter.Visible:=false;

  // init SynAutoComplete
  with SynAutoComplete do begin
    s:=EditorOpts.CodeTemplateFileName;
    if FileExists(s) then
      try
        AutoCompleteList.LoadFromFile(s);
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
          EditorOpts.CodeTemplateFileName);
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

procedure TCodeTemplateDialog.EditButtonClick(Sender: TObject);
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
  EnableMacros: boolean;
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

initialization
  {$I codetemplatesdlg.lrs}

end.
