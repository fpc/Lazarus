{
/***************************************************************************
                               UnitEditor.pp
                             -------------------




 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
{This unit builds the TSourceNotbook that the editors are held on.  It also has
 a class that controls the editors (TSourceEditor)
}

unit UnitEditor;

{$mode objfpc}
{$H+}
//{$DEFINE NEW_EDITOR}
{$DEFINE NEW_EDITOR_SYNEDIT}

interface

uses
  Classes, Controls, Forms, Buttons, ComCtrls, SysUtils, Dialogs, FormEditor,
  FindReplaceDialog, EditorOptions, CustomFormEditor, KeyMapping, StdCtrls,
  Compiler, MsgView, WordCompletion, CodeTools,
{$ifdef NEW_EDITOR_SYNEDIT}
  SynEdit, SynEditHighlighter, SynHighlighterPas, SynEditAutoComplete,
  SynEditKeyCmds,SynCompletion, 
{$else}
  wcustomedit,mwPasSyn,
{$endif}
  Graphics, Extctrls, Menus;

type
{$ifdef NEW_EDITOR_SYNEDIT}
  TmwCustomEdit = TSynEdit;
  TmwPasSyn = TSynPasSyn;
{$endif}
  // --------------------------------------------------------------------------

  TSrcEditMarkerType = (semActiveBreakPoint, semInactiveBreakPoint);
  TSrcEditMarkerTypes = set of TSrcEditMarkerType;

  // --------------------------------------------------------------------------

  TNotifyFileEvent = procedure(Sender: TObject; Filename : AnsiString) of object;

  TOnCreateDeleteBreakPoint = procedure(Sender: TObject; line:integer) of object;

  TOnProcessUserCommand = procedure(Sender: TObject;
            Command: integer; var Handled: boolean) of object;
  TOnUserCommandProcessed = procedure(Sender: TObject;
            Command: integer; var Handled: boolean) of object;

{---- TSource Editor ---
  TSourceEditor is the class that controls access for the Editor and the source
  code. It creates the PopupMenu that appears when you right-click on the
  editor. It calls the editor functions for bookmarks.
 ---- TSource Editor ---}
  TSourceEditor = class
  private
    //FAOwner is normally a TSourceNotebook.  This is set in the Create constructor.
    FAOwner : TComponent;
{$ifdef NEW_EDITOR_SYNEDIT}
    FEditor     : TSynEdit;
    FCodeTemplates: TSynEditAutoComplete;
{$else}
    FHighlighter: TmwPasSyn;
    FEditor     : TmwCustomEdit;
{$endif}
    //if this is a Form or Datamodule, this is used
    FControl: TComponent;

    FFileName : AnsiString;
    FUnitName : String;

    FPopUpMenu : TPopupMenu;
    FSyntaxHighlighterType: TLazSyntaxHighlighter;
    FErrorLine: integer;
    FExecutionLine: integer;

    FOnAfterClose : TNotifyEvent;
    FOnAfterOpen : TNotifyEvent;
    FOnAfterSave : TNotifyEvent;
    FOnBeforeClose : TNotifyEvent;
    FOnBeforeOpen : TNotifyEvent;
    FOnBeforeSave : TNotifyEvent;
    FOnEditorChange: TNotifyEvent;
    FOnCreateBreakPoint: TOnCreateDeleteBreakPoint;
    FOnDeleteBreakPoint: TOnCreateDeleteBreakPoint;
    FVisible : Boolean;

    Function FindFile(Value : String) : String;

    Function GetSource : TStrings;
    Procedure SetSource(value : TStrings);
    Function GetCurrentCursorXLine : Integer;
    Procedure SetCurrentCursorXLine(num : Integer);
    Function GetCurrentCursorYLine : Integer;
    Procedure SetCurrentCursorYLine(num : Integer);
    Function GetModified : Boolean;
    procedure SetModified(NewValue:boolean);
    Function GetInsertMode : Boolean;
    Function GetReadonly : Boolean;
    Function TextUnderCursor : String;
    Function GotoMethod(Value : String) : Integer;
    Function GotoMethodDeclaration(Value : String) : Integer;
    procedure SetCodeTemplates(
         NewCodeTemplates: TSynEditAutoComplete);
    procedure SetPopupMenu(NewPopupMenu: TPopupMenu);

    Function GotoLine(Value : Integer) : Integer;

    Procedure CreateEditor(AOwner : TComponent; AParent: TWinControl);
  protected
    FindText : String;
    ErrorMsgs : TStrings;
    Procedure DisplayControl;
    Procedure ReParent(AParent : TWinControl);

    Procedure OpenAtCursorClicked(Sender : TObject);

    Procedure ProcessUserCommand(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: char; Data: pointer);
    Procedure UserCommandProcessed(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: char; Data: pointer);
    Procedure ccOnTimer(sender : TObject);
    Procedure ccAddMessage(Texts : String);
    Function  ccParse(Texts : String) : TStrings;

    Procedure FocusEditor;  // called by TSourceNotebook when the Notebook page
                            // changes so the editor is focused
    Procedure EditorStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure OnGutterClick(Sender: TObject; X, Y, Line: integer;
         mark: TSynEditMark);
    procedure OnEditorSpecialLineColor(Sender: TObject; Line: integer;
         var Special: boolean; var FG, BG: TColor);
    Function RefreshEditorSettings : Boolean;
    procedure SetSyntaxHighlighterType(ASyntaxHighlighterType: TLazSyntaxHighlighter);
    procedure SetErrorLine(NewLine: integer);
    procedure SetExecutionLine(NewLine: integer);

    property Visible : Boolean read FVisible write FVisible default False;
  public
    constructor Create(AOwner : TComponent; AParent : TWinControl);
    destructor Destroy; override;
    Procedure SelectText(LineNum,CharStart,LineNum2,CharEnd : Integer);
    Function Close : Boolean;

    procedure StartFindAndReplace(Replace:boolean);
    procedure OnReplace(Sender: TObject; const ASearch, AReplace:
       string; Line, Column: integer; var Action: TSynReplaceAction);
    procedure DoFindAndReplace;
    procedure FindAgain;

    procedure GetDialogPosition(Width, Height:integer; var Left,Top:integer);
    property Control : TComponent read FControl write FControl;
    property CurrentCursorXLine : Integer
       read GetCurrentCursorXLine write SetCurrentCursorXLine;
    property CurrentCursorYLine : Integer
       read GetCurrentCursorYLine write SetCurrentCursorYLine;
    property Owner : TComponent read FAOwner;
    property Source : TStrings read GetSource write SetSource;
    property UnitName : String read FUnitName write fUnitname;
    property FileName : AnsiString read FFileName write FFilename;
    property Modified : Boolean read GetModified write SetModified;
    property ReadOnly : Boolean read GetReadOnly;
    property InsertMode : Boolean read GetInsertmode;
    property CodeTemplates: TSynEditAutoComplete
       read FCodeTemplates write SetCodeTemplates;
    property PopupMenu:TPopupMenu read FPopUpMenu write SetPopUpMenu;
    property EditorComponent:TSynEdit read FEditor;
    property SyntaxHighlighterType: TLazSyntaxHighlighter 
       read fSyntaxHighlighterType write SetSyntaxHighlighterType;
    property ErrorLine: integer read FErrorLine write SetErrorLine;
    property ExecutionLine: integer read FExecutionLine write SetExecutionLine;

    property OnAfterClose : TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnBeforeClose : TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnAfterOpen : TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnBeforeOpen : TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterSave : TNotifyEvent read FOnAfterSave write FOnAfterSave;
    property OnBeforeSave : TNotifyEvent read FOnBeforeSave write FOnBeforeSave;
    property OnEditorChange: TNotifyEvent read FOnEditorChange write FOnEditorChange;
    property OnCreateBreakPoint: TOnCreateDeleteBreakPoint
       read FOnCreateBreakPoint write FOnCreateBreakPoint;
    property OnDeleteBreakPoint: TOnCreateDeleteBreakPoint
       read FOnDeleteBreakPoint write FOnDeleteBreakPoint;
  end;


  TSourceNotebook = class(TForm)
  private
    FMainIDE : TComponent;
    FFormEditor : TFormEditor;
    FCodeTemplateModul : TSynEditAutoComplete;

    FSourceEditorList : TList; // list of TSourceEditor
    FSaveDialog : TSaveDialog;
    FOpenDialog : TOpenDialog;

    FOnNewClicked : TNotifyEvent;
    FOnOpenClicked : TNotifyEvent;
    FOnOpenFileAtCursorClicked : TNotifyEvent;
    FOnCloseClicked : TNotifyEvent;
    FOnSaveClicked : TNotifyEvent;
    FOnSaveAsClicked : TNotifyEvent;
    FOnSaveAllClicked : TNotifyEvent;
    FOnToggleFormUnitClicked : TNotifyEvent;
    FOnProcessUserCommand: TOnProcessUserCommand;
    FOnUserCommandProcessed: TOnProcessUserCommand;

    // PopupMenu
    Procedure BuildPopupMenu;

    Procedure BookMarkClicked(Sender : TObject);
    Procedure BookMarkGotoClicked(Sender : TObject);
    Procedure ReadOnlyClicked(Sender : TObject);
    Procedure ToggleBreakpointClicked(Sender : TObject);
    Procedure ToggleLineNumbersClicked(Sender : TObject);
    Procedure OpenAtCursorClicked(Sender : TObject);
    Procedure BookmarkGoTo(Value: Integer);
    Procedure BookMarkToggle(Value : Integer);
  protected
    ccSelection : String;
     
    Function CreateNotebook : Boolean;
    Function DisplayPage(SE : TSourceEditor) : Boolean;
    Function NewSE(Pagenum : Integer) : TSourceEditor;
    Procedure EditorChanged(sender : TObject);
    Procedure ccExecute(Sender : TObject);
    Procedure ccCancel(Sender : TObject);
    procedure ccComplete(var Value: ansistring; Shift: TShiftState);
    function OnSynCompletionPaintItem(AKey: string; ACanvas: TCanvas;
       X, Y: integer): boolean;
    procedure OnSynCompletionSearchPosition(var APosition:integer);

    Procedure NextEditor;
    Procedure PrevEditor;
    procedure UpdateStatusBar;
    MarksImgList : TImageList;
    Procedure ProcessParentCommand(Sender: TObject; 
       var Command: TSynEditorCommand; var AChar: char; Data: pointer);
    Procedure ParentCommandProcessed(Sender: TObject; 
       var Command: TSynEditorCommand; var AChar: char; Data: pointer);
    function FindBookmark(BookmarkID: integer): TSourceEditor;
    function FindPageWithEditor(ASourceEditor: TSourceEditor):integer;
    function GetEditors(Index:integer):TSourceEditor;
  public
    SearchPaths: string;

    property Editors[Index:integer]:TSourceEditor read GetEditors;
    function EditorCount:integer;
    function FindSourceEditorWithPageIndex(PageIndex:integer):TSourceEditor;
    Function GetActiveSE : TSourceEditor;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Function ActiveUnitName : String;
    Function ActiveFileName : AnsiString;
    Function GetSourceForUnit(UnitName : String) : TStrings;
    Function SetSourceForUnit(UnitName : String; NewSource : TStrings) : Boolean;
    Function FindUniquePageName(FileName:string; IgnorePageIndex:integer):string;
    function SomethingModified: boolean;

    Procedure DisplayFormforActivePage;
    Procedure DisplayCodeforControl(Control : TObject);
    Procedure DisplayCodefromUnitName(UnitName : String);

    procedure CloseClicked(Sender : TObject);
    Procedure NewClicked(Sender: TObject);
    procedure OpenClicked(Sender : TObject);
    procedure SaveClicked(Sender : TObject);
    procedure SaveAllClicked(Sender : TObject);
    procedure SaveAsClicked(Sender : TObject);
    procedure ToggleFormUnitClicked(Sender: TObject);

    procedure FindClicked(Sender : TObject);
    procedure ReplaceClicked(Sender : TObject);
    procedure FindAgainClicked(Sender : TObject);

    Procedure NewFile(UnitName: String; Source : TStrings);
    Procedure CloseFile(PageIndex:integer);

    Procedure ToggleBookmark(Value : Integer);
    Procedure GotoBookmark(Value: Integer);

    Procedure ReloadEditorOptions;
    Property CodeTemplateModul: TSynEditAutoComplete
       read FCodeTemplateModul write FCodeTemplateModul;
    procedure OnCodeTemplateTokenNotFound(Sender: TObject; AToken: string; 
                                AnEditor: TCustomSynEdit; var Index:integer);
    procedure OnWordCompletionGetSource(var Source:TStrings; SourceIndex:integer);

    function Empty: boolean;
    property FormEditor : TFormEditor read FFormEditor write FFormEditor;
    property MainIDE : TComponent read FMainIDE;
  published
    Notebook : TNotebook;
    SrcPopUpMenu : TPopupMenu;
    StatusBar : TStatusBar;
    ToggleMenuItem : TMenuItem;
    Procedure NotebookPageChanged(Sender : TObject);
    property OnNewClicked : TNotifyEvent read FOnNewClicked write FOnNewClicked;
    property OnOpenClicked : TNotifyEvent read FOnOPenClicked write FOnOpenClicked;
    property OnOpenFileAtCursorClicked : TNotifyEvent 
       read FOnOpenFileAtCursorClicked write FOnOpenFileAtCursorClicked;
    property OnCloseClicked : TNotifyEvent read FOnCloseClicked write FOnCloseClicked;
    property OnSaveClicked : TNotifyEvent read FOnSaveClicked write FOnSaveClicked;
    property OnSaveAsClicked : TNotifyEvent 
       read FOnSaveAsClicked write FOnSaveAsClicked;
    property OnSaveAllClicked : TNotifyEvent 
       read FOnSaveAllClicked write FOnSaveAllClicked;
    property OnToggleFormUnitClicked : TNotifyEvent 
       read FOnToggleFormUnitClicked write FOnToggleFormUnitClicked;
    property OnProcessUserCommand: TOnProcessUserCommand
       read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnUserCommandProcessed: TOnUserCommandProcessed
       read FOnUserCommandProcessed write FOnUserCommandProcessed;
  end;

{Goto dialog}

   TfrmGoto = class(TForm)
     Label1 : TLabel;
     Edit1 : TEdit;
     btnOK : TBitbtn;
     btnCancel : TBitBtn;
     procedure Edit1KeyDown(Sender: TObject; var Key:Word; Shift:TShiftState);
   public
     constructor Create(AOwner : TComponent); override;
   end;

implementation

uses
  LCLLinux, TypInfo, LResources, LazConf, EnvironmentOpts;

type
  TCompletionType = (ctNone, ctWordCompletion, ctTemplateCompletion,
                     ctCodeCompletion);

const
  TSrcEditMarkerImgIndex: array[TSrcEditMarkerType] of integer = (
       10,  // active breakpoint
       11   // inactive breakpoint
    );

var
  aHighlighter: TSynPasSyn;
  aCompletion : TSynCompletion;
  scompl : TSynBaseCompletion;  //used in ccexecute and cccomplete
  CurrentCompletionType: TCompletionType;
  GotoDialog : TfrmGoto;
  CodeCompletionTimer : TTimer;
  AWordCompletion : TWordCompletion;



{ TSourceEditor }

{The constructor for @link(TSourceEditor).  AOwner is the @link(TSOurceNotebook)
 and the AParent is usually a page of a @link(TNotebook)
}

constructor TSourceEditor.Create(AOwner : TComponent; AParent : TWinControl);
Begin
writeln('TSourceEditor.Create A ',AOwner.Classname,' ',AParent.Classname);
  inherited Create;
  FAOwner := AOwner;

  FSyntaxHighlighterType:=lshNone;
  FErrorLine:=-1;
  FExecutionLine:=-1;

  FControl := nil;
writeln('TSourceEditor.Create B ');
  CreateEditor(AOwner,AParent);
writeln('TSourceEditor.Create END ');
end;

destructor TSourceEditor.Destroy;
begin
writeln('TSourceEditor.Destroy A ',FEditor.Name);
  FEditor.Free;
writeln('TSourceEditor.Destroy B ');
  inherited Destroy;
writeln('TSourceEditor.Destroy END ');
end;

{------------------------------G O T O   L I N E  -----------------------------}
Function TSourceEditor.GotoLine(Value : Integer) : Integer;
Var
  P : TPoint;
  TopLine: integer;
Begin
  P.X := 0;
  P.Y := Value;
  TopLine := P.Y - (FEditor.LinesInWindow div 2);
  if TopLine < 1 then TopLine:=1;
  FEditor.CaretXY := P;
  FEditor.TopLine := TopLine;
  Result:=FEditor.CaretY;
end;

{-----------------------------G O T O   M E T H O D ---------------------------}

Function TSourceEditor.GotoMethod(Value : String) : Integer;
Var
  I : Integer;
  Texts2 : String;
Begin
  Result := -1;
  if Length(Value) <= 1 then Exit;

  //move down looking for the classname.texts
  //we need to parse for the class name eventually
  //for now just search for .procedurename
  Value := '.'+lowercase(value);
  for I := CurrentCursorYLine to Source.Count -1 do
    begin
      Texts2 := Lowercase(Source.Strings[i]);
      if (pos('procedure',Texts2) <> 0) or (pos('function',texts2) <> 0)
      then begin
        if pos(Value,texts2) <> 0 then
           begin
             FEditor.TopLine := I-1;
             CurrentCursorYLine := I+1;
             Result := I;
             Break;
           end;
        end;
     end;
End;


{-----------------------G O T O   M E T H O D    D E C L A R A T I O N---------}
Function TSourceEditor.GotoMethodDeclaration(Value : String) : Integer;
Var
I : Integer;
Texts2 : String;
Begin
     Result := -1;
     if Length(Value) <= 1 then Exit;

     //move down looking for the classname.texts
     //we need to parse for the class name eventually
    //for now just search for .procedurename
     Value := lowercase(value);
     for I := 0 to Source.Count -1 do
         begin
            Texts2 := Lowercase(Source.Strings[i]);
            if (pos('procedure',Texts2) <> 0) or (pos('function',texts2) <> 0) then
                begin
                if pos(Value,texts2) <> 0 then
                   begin
                     FEditor.TopLine := I;
                     CurrentCursorYLine := I+1;
                     Result := I;
                     Break;
                   end;
                end;
           end;

End;


{--------------------------TEXT UNDER CURSOR-----------------------------------}
Function TSourceEditor.TextUnderCursor : String;
var
  Texts : String;
  EditorLine : String;
  X : Integer;
Begin
  //get the text by the cursor.
  EditorLine := FEditor.Lines.Strings[GetCurrentCursorYLine-1];
  X := GetCurrentCursorXLine-1;

  //walk backwards to a space or non-standard character.
  while (ord(upcase(EditorLine[x])) >= 65)
  and (ord(upcase(EditorLine[x])) <= 90) and (X>1) do
    dec(x);
  if (X > 1) then Inc(x);

  Texts := Copy(EditorLine,X,length(EditorLine));  //chop off the beginning

  X := 1;
  while (ord(upcase(Texts[x])) >= 65) and (ord(upcase(Texts[x])) <= 90)
  and (X< length(Texts)) do
   inc(x);

  if (X < Length(Texts) ) and (X >1)  then dec(x);

  if not(((ord(upcase(Texts[x])) >= 65) and (ord(upcase(Texts[x])) <= 90))) then
    dec(x);
  Texts := Copy(Texts,1,X);

  Result := Texts;
end;

Procedure TSourceEditor.OpenAtCursorClicked(Sender : TObject);
{var
  Texts : String;
  Found : Boolean;
  SearchDir : String;
  AppDIr : String;
  TempDir : String;
  Num : Integer;
  DirDelimiter : Char;}
Begin
{  Texts := TextunderCursor;
  if Length(Texts) <= 1 then Exit;

  Found := False;
  //check the current directory
  Found := True;
  if FileExists(Lowercase(Texts)) then 
    TSOurceNotebook(FAOwner).OpenFile(Lowercase(Texts), True)
  else
    if FileExists(Lowercase(Texts)+'.pp') then 
      TSOurceNotebook(FAOwner).OpenFile(Lowercase(Texts)+'.pp', True)
    else
      if FileExists(Lowercase(Texts)+'.pas') then 
        TSOurceNotebook(FAOwner).OpenFile(Lowercase(Texts)+'.pas', True)
      else
        Found := False;

  if not Found then begin
    // check the default LCL directory if not Found
    Found := True;
    AppDir := ExtractFilePath(Application.Exename);
    if FileExists(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)) then
      TSourceNotebook(FAOwner).OpenFile(
        AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts), True)
    else
      if FileExists(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pp') 
      then
        TSourceNotebook(FAOwner).OpenFile(
          AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pp', True)
      else
        if FileExists(
          AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pas')
        then
          TSourceNotebook(FAOwner).OpenFile(
            AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pas', True)
        else
          Found := False;
  end;

  if not Found then begin
    //Get the search directories
    DirDelimiter := '/';
    SearchDir := TSourceNotebook(Owner).SearchPaths;
    Writeln('Searchdir is '+Searchdir);
    Num := pos(';',SearchDir);
    While (not Found) and (SearchDir <> '') do
    Begin
      if Num = 0 then Num := Length(SearchDir)+1;
      TempDir := Copy(SearchDir,1,num-1);
      Delete(SearchDir,1,Num);
      if tempDir[Length(TempDir)] <> DirDelimiter then
        TempDir := TempDir + DirDelimiter;
      Found := True;
      if FileExists(TempDir+Lowercase(Texts)) then
        TSourceNotebook(FAOwner).OpenFile(TempDir+Lowercase(Texts), True)
      else
        if FileExists(TempDir+Lowercase(Texts)+'.pp') then
          TSourceNotebook(FAOwner).OpenFile(TempDir+Lowercase(Texts)+'.pp', True)
        else
          if FileExists(TempDir+Lowercase(Texts)+'.pas') then 
            TSourceNotebook(FAOwner).OpenFile(TempDir+Lowercase(Texts)+'.pas', True)
          else
            Found := False;
      Num := pos(';',SearchDir);
    end; //while
  end;

  If not Found then
    Application.MessageBox('File not found','Error',MB_OK);
}
end;

procedure TSourceEditor.GetDialogPosition(Width, Height:integer; 
  var Left,Top:integer);
var P:TPoint;
begin
  with EditorComponent do
    P := ClientToScreen(Point(CaretXPix, CaretYPix));
  Left:=EditorComponent.ClientOrigin.X+(EditorComponent.Width - Width) div 2;
  Top:=P.Y-Height-2*EditorComponent.LineHeight;
  if Top<10 then Top:=P.y+2*EditorComponent.LineHeight;
end;

{------------------------------S T A R T  F I N D-----------------------------}
procedure TSourceEditor.StartFindAndReplace(Replace:boolean);
var ALeft,ATop:integer;
Begin
  if Replace then
    FindReplaceDlg.Options :=
      FindReplaceDlg.Options + [ssoReplace, ssoReplaceAll, ssoPrompt]
  else
    FindReplaceDlg.Options :=
      FindReplaceDlg.Options - [ssoReplace, ssoReplaceAll, ssoPrompt];

  with EditorComponent do begin
    if SelAvail and (BlockBegin.Y = BlockEnd.Y) then
      FindReplaceDlg.FindText := SelText
    else
      FindReplaceDlg.FindText := GetWordAtRowCol(CaretXY);
  end;

  GetDialogPosition(FindReplaceDlg.Width,FindReplaceDlg.Height,ALeft,ATop);
  FindReplaceDlg.Left:=ALeft;
  FindReplaceDlg.Top:=ATop;
  if (FindReplaceDlg.ShowModal <> mrCancel) then
    DoFindAndReplace;
End;

{------------------------------F I N D  A G A I N ----------------------------}
procedure TSourceEditor.FindAgain;
var OldOptions: TSynSearchOptions;
Begin
  OldOptions:=FindReplaceDlg.Options;
  FindReplaceDlg.Options:=FindReplaceDlg.Options-[ssoEntireScope];
  DoFindAndReplace;
  FindReplaceDlg.Options:=OldOptions;
End;

procedure TSourceEditor.DoFindAndReplace;
var OldCaretXY:TPoint;
  AText,ACaption:AnsiString;
  TopLine: integer;
begin
  OldCaretXY:=EditorComponent.CaretXY;
  EditorComponent.SearchReplace(
    FindReplaceDlg.FindText,FindReplaceDlg.ReplaceText,FindReplaceDlg.Options);
  if (OldCaretXY.X=EditorComponent.CaretX)
  and (OldCaretXY.Y=EditorComponent.CaretY)
  and not (ssoReplaceAll in FindReplaceDlg.Options) then begin
    ACaption:='Message';
    AText:='Search string '''+FindReplaceDlg.FindText+''' not found!';
    Application.MessageBox(PChar(AText),PChar(ACaption),MB_OK);
  end else begin
    TopLine := EditorComponent.CaretY - (EditorComponent.LinesInWindow div 2);
    if TopLine < 1 then TopLine:=1;
    EditorComponent.TopLine := TopLine;
  end;
end;

procedure TSourceEditor.OnReplace(Sender: TObject; const ASearch, AReplace:
  string; Line, Column: integer; var Action: TSynReplaceAction);
var a:integer;
  ACaption,AText:AnsiString;
begin
  ACaption:='Prompt for replace';
  AText:='Replace this occurrence of '''+ASearch+''' with '''+AReplace+'''?';
  a:=Application.MessageBox(PChar(AText),PChar(ACaption),MB_YESNOCANCEL);
  case a of
    mrYes:Action:=raReplace;
    mrNo :Action:=raSkip;
    mrAll:Action:=raReplaceAll;
  else
    Action:=raCancel;
  end;
end;

Procedure TSourceEditor.FocusEditor;
Begin
  FEditor.SetFocus;
end;


Function TSourceEditor.GetReadOnly : Boolean;
Begin
  Result :=  FEditor.ReadOnly;
End;

Procedure TSourceEditor.ProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: char; Data: pointer);
var
  Y,I : Integer;
  P: TPoint;
  Texts, Texts2, TheName : String;
  Handled: boolean;
Begin
  Handled:=true;
  case Command of
  ecCodeCompletion :
    if TCustomSynEdit(Sender).ReadOnly=false then begin
      CurrentCompletionType:=ctCodeCompletion;
      TextS := FEditor.LineText;
      i := FEditor.CaretX;
      if i > length(TextS) then
        TextS2 := ''
      else begin
        dec(i);
        while (i > 0) and (TextS[i] in ['a'..'z','A'..'Z','0'..'9','_']) do
          dec(i);
        TextS2 := copy(TextS, i + 1, FEditor.CaretX - i - 1);
      end;
      with TCustomSynEdit(Sender) do
        P := ClientToScreen(Point(CaretXPix - length('constructor  ')*CharWidth
                , CaretYPix + LineHeight));
      aCompletion.Editor:=TCustomSynEdit(Sender);
      aCompletion.Execute(TextS2,P.X,P.Y);
    end;

  ecWordCompletion :
    if TCustomSynEdit(Sender).ReadOnly=false then begin
      CurrentCompletionType:=ctWordCompletion;
      TextS := FEditor.LineText;
      i := FEditor.CaretX - 1;
      if i > length(TextS) then
        TextS2 := ''
      else begin
        dec(i);
        while (i > 0) and (TextS[i] in ['a'..'z','A'..'Z','0'..'9','_']) do
          dec(i);
        TextS2 := Trim(copy(TextS, i + 1, FEditor.CaretX - i - 1));
      end;
      with TCustomSynEdit(Sender) do
        P := ClientToScreen(Point(CaretXPix - length(TextS2)*CharWidth
                , CaretYPix + LineHeight));
      aCompletion.Editor:=TCustomSynEdit(Sender);
      aCompletion.Execute(TextS2,P.X,P.Y);
    end;

  ecFind :
    StartFindAndReplace(false);

  ecFindAgain :
    FindAgain;

  ecReplace:
    StartFindAndReplace(true);

  ecFindProcedureMethod :
    Begin
      //jump down to the procedure definition
      Texts := TextUnderCursor;  //this should be a procedure name.
      GotoMethod(Texts);
    end;

  ecFindProcedureDefinition :
    Begin
      Y := CurrentCursorYLine;
      Texts2 := Lowercase(Source.Strings[Y-1]);
      Writeln('The source line = '+Texts2);
      I := pos('function',Texts2);
      if I = 0 then
      I := pos('procedure',Texts2);
      While (I = 0) and (Y > 0) do begin
        dec(Y);
        Texts2 := Lowercase(Source.Strings[Y-1]);
        Writeln('The source line = '+Texts2);
        I := pos('function ',Texts2);
        if I = 0 then
          I := pos('procedure ',Texts2);
      end;
      if I <> 0 then
      Begin
        TheName := '';
        I := pos('.',Texts2);
        inc(i);
        while (not(Texts2[i] in [';',' ','('])) do
          Begin
            TheName := TheName + Texts2[i];
            inc(i);
          end;
        Writeln('Thename = '+TheName);
        GotoMethodDeclaration(TheName);
      end;
     end;

  ecGotoLineNumber :
    begin 
      GotoDialog.Edit1.Text:='';
      if (GotoDialog.ShowModal = mrOK) then begin
        try
          GotoLine(StrToInt(GotoDialog.Edit1.Text));
        except
          GotoLine(0);
        end;
      end;
    end;

  ecPeriod :
    Begin
      Y := CurrentCursorYLine;
      Texts := Lowercase(Source.Strings[Y-1]);
      if InsertMode then
        Texts := Copy(Texts,1,CurrentCursorXLine)+'.'
              +Copy(Texts,CurrentCursorXLine+1,Length(Texts))
      else
        Texts[CurrentCursorXLine] := '.';
      Source.Strings[Y-1] := Texts;
      CodeCompletionTimer.OnTimer := @CCOnTimer;
      CodeCompletionTimer.Enabled := True;
    end;
    
  else
    begin
      Handled:=false;
      TSourceNotebook(FaOwner).ProcessParentCommand(self,Command,aChar,Data);
    end;
  end;  //case
  if Handled then Command:=ecNone;
end;

Procedure TSourceEditor.UserCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: char; Data: pointer);
  //Handled: boolean;
begin
  {Handled:=true;
  case Command of
  
  else begin
    Handled:=false;}
    TSourceNotebook(FaOwner).ParentCommandProcessed(self,Command,aChar,Data);
  {end;
  if Handled then Command:=ecNone;}
end;

Procedure TSourceEditor.EditorStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
Begin
  If Assigned(OnEditorChange) then
     OnEditorChange(sender);
end;

procedure TSourceEditor.OnGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
var i:integer;
  AllMarks: TSynEditMarks;
  BreakPtMark: TSynEditMark;
begin
  // create or delete breakpoint
  // find breakpoint mark at line
  fEditor.Marks.GetMarksForLine(Line, AllMarks);
  BreakPtMark:=nil;
  for i:=1 to maxMarks do begin
    if (AllMarks[i]<>nil)
    and (AllMarks[i].ImageIndex=TSrcEditMarkerImgIndex[semActiveBreakPoint]) then
    begin
      BreakPtMark:=AllMarks[i];
      break;
    end;
  end;
  if BreakPtMark<>nil then begin
    // delete breakpoint
    if Assigned(FOnDeleteBreakPoint) then FOnDeleteBreakPoint(Self,Line);
    fEditor.Marks.Remove(BreakPtMark);
    BreakPtMark.Free;
    fEditor.Modified:=true;
  end else begin
    // create breakpoint
    BreakPtMark:=TSynEditMark.Create(fEditor);
    with BreakPtMark do begin
      ImageIndex:=TSrcEditMarkerImgIndex[semActiveBreakPoint];
      Visible:=true;
    end;
    BreakPtMark.Line:=Line;
    fEditor.Marks.Place(BreakPtMark);
    if Assigned(FOnCreateBreakPoint) then FOnCreateBreakPoint(Self,Line);
    fEditor.Modified:=true;
  end;
end;

procedure TSourceEditor.OnEditorSpecialLineColor(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
var i:integer;
  AllMarks: TSynEditMarks;
begin
  if ErrorLine=Line then begin
    FG:=EditorOpts.ErrorLineElement.Foreground;
    BG:=EditorOpts.ErrorLineElement.Background;
    Special:=true;
  end else if ExecutionLine=Line then begin
    FG:=EditorOpts.ExecutionPointElement.Foreground;
    BG:=EditorOpts.ExecutionPointElement.Background;
    Special:=true;
  end else begin
    fEditor.Marks.GetMarksForLine(Line, AllMarks);
    for i:=1 to maxMarks do begin
      if (AllMarks[i]<>nil) then begin
        if (AllMarks[i].ImageIndex=TSrcEditMarkerImgIndex[semActiveBreakPoint])
        then begin
          FG:=EditorOpts.EnabledBreakPointElement.Foreground;
          BG:=EditorOpts.EnabledBreakPointElement.Background;
          Special:=true;
          exit;
        end else if 
          (AllMarks[i].ImageIndex=TSrcEditMarkerImgIndex[semInactiveBreakPoint])
        then begin
          FG:=EditorOpts.DisabledBreakPointElement.Foreground;
          BG:=EditorOpts.DisabledBreakPointElement.Background;
          Special:=true;
          exit;
        end;
      end;
    end;
  end;
end;

procedure TSourceEditor.SetSyntaxHighlighterType(
  ASyntaxHighlighterType: TLazSyntaxHighlighter);
begin
  if EditorOPts.UseSyntaxHighlight then begin
    case ASyntaxHighlighterType of
      lshFreePascal,lshDelphi:
        FEditor.Highlighter:=aHighlighter;
    else  
      FEditor.Highlighter:=nil;
    end;
  end else FEditor.Highlighter:=nil;
  if ASyntaxHighlighterType<>fSyntaxHighlighterType then begin

    fSyntaxHighlighterType:=ASyntaxHighlighterType;
  end;
end;

procedure TSourceEditor.SetErrorLine(NewLine: integer);
begin
  if fErrorLine=NewLine then exit;
  fErrorLine:=NewLine;
  EditorComponent.Invalidate;
end;

procedure TSourceEditor.SetExecutionLine(NewLine: integer);
begin
  if fExecutionLine=NewLine then exit;
  fExecutionLine:=NewLine;
  EditorComponent.Invalidate;
end;

Function TSourceEditor.RefreshEditorSettings : Boolean;
Begin
  Result := False;

  EditorOpts.GetHighlighterSettings(aHighlighter);
  SetSyntaxHighlighterType(fSyntaxHighlighterType);

  EditorOpts.GetSynEditSettings(FEditor);
end;

Function TSourceEditor.FindFile(Value : String) : String;
var
  Found : Boolean;
  DirDelimiter : String;
  SearchDir : String;
  Num : Integer;
  tempDir : String;
Begin
  Result := '';
  Found := False;
  DirDelimiter := '/';
  SearchDir := TSourceNotebook(Owner).SearchPaths;
  Writeln('Searcvhdir is '+Searchdir);
  Num := pos(';',SearchDir);
  While (not Found) and (SearchDir <> '') do
  Begin
    if Num = 0 then Num := Length(SearchDir)+1;
    TempDir := Copy(SearchDir,1,num-1);
    Delete(SearchDir,1,Num);
    if tempDir[Length(TempDir)] <> DirDelimiter then
       TempDir := TempDir + DirDelimiter;
    Found := True;

    if FileExists(TempDir+Value) then
       Result := TempDir+Value
       else
       Found := False;
  end; //while

End;



Function TSourceEditor.ccParse(Texts : String) : TStrings;
const
  symtable = '---Symtable ';
  Level1 = '  ***';
  Level2 = '    ***';

  kdClass = 1;
  kdProcedure = 2;

var
  s : TStrings;
  I : Integer;
  Browser : TStringList;
  UnitStart : Integer;
  Found : Boolean;
  tempFile : TStringList;
  tempFileName : String;
  TempLine  : String;
  kind : Integer;
  num1,num2 : Integer;
begin
  TempFile := TStringList.Create;
  S := TStringList.Create;
  Result := nil;
  Browser := TstringList.Create;
  Browser.LoadFromFile(ExtractFilePath(Application.Exename)+'browser.log');



  For I := 0 to Browser.Count-1 do
    if Browser.strings[I] = symtable+uppercase(Unitname) then
       break;

  if I >=Browser.Count-1 then Exit;

  UnitStart := I;

  //remove the period from TEXTS if it's the last character
  if Texts[length(texts)] = '.' then
    Begin
      Texts := Copy(Texts,1,Length(texts)-1);
      kind := kdClass;
    end
    else
  if Texts[length(texts)] = '(' then
    Begin
      Texts := Copy(Texts,1,Length(texts)-1);
      kind := kdProcedure;
    end
    else
    Exit;

  Texts := uppercase(texts);
  //find ***TEXTS***
  Found := False;
  I := UnitStart+1;
  While (Browser.strings[I] <> symtable+uppercase(Unitname)) and (not found) do
     if Browser.Strings[I] = Level1+Texts+'***' then
        Found := true
        else
        inc(i);

  if Found then
     begin  //determine what it is.
     //grab the line it's defined on.
     Writeln('The next line is '+Browser.Strings[i+1]);
     TempFileName := Copy(trim(Browser.Strings[i+1]),1
         ,pos('(',trim(Browser.Strings[i+1]))-1);
     Writeln('TemmpFileName = '+TempFilename);
     if FileExists('./temp/'+TempFileName) then
        TempFileName := './temp/'+TempFileName
        else
        TempFileName := FindFile(TempFileName);

     if TempFileName = '' then Exit;
     tempFile.LoadFromFile(TempFileName);
     //ok, the file is loaded.  Parse it to see what TEXTS is defined as.
     Num1 := pos('(',Browser.Strings[i+1])+1;
     Num2 := ((pos(',',Browser.Strings[i+1])-1) - pos('(',Browser.Strings[i+1])+1)-1;
     tempLine := TempFile.Strings[StrtoInt(Copy(Browser.Strings[i+1],Num1,Num2))-1];
     writeln('TEMPLINE = '+TempLine);
     //templine now contains Form1: TForm1;  or something like that

     case Kind of
       kdClass :  //search for a colon then the name
               Begin

               end;
      end;
   end;

  S.Add('testing');
  Result := S;
end;

Procedure TSourceEditor.ccAddMessage(Texts : String);
Begin
  ErrorMsgs.Add(Texts);
End;

Procedure TSourceEditor.ccOnTimer(sender : TObject);
Begin
  CodeCompletionTimer.Enabled := False;
//  FEditor.KeyDown(FEditor,word(' '),[ssCtrl]);
End;


Procedure TSourceEditor.CreateEditor(AOwner : TComponent; AParent: TWinControl);
var OldSource: TStringList;
  NewName: string;
  i: integer;
Begin
  OldSource:=TStringList.Create;
  if assigned(FEditor) then Begin
writeln('TSourceEditor.CreateEditor  freeing old FEditor');
    OldSource.Assign(FEditor.Lines);
    NewName:=FEditor.Name;
    FEditor.Free;
    FEditor:=nil;
  end else begin
    i:=0;
    repeat
      inc(i);
      NewName:='SynEdit'+IntToStr(i);
    until (FAOwner.FindComponent(NewName)=nil);
  end;

  FEditor:=TSynEdit.Create(FAOwner);
writeln('TSourceEditor.CreateEditor  FEditorName="',NewName,'"');  
  with FEditor do begin
    Name:=NewName;
    Parent := AParent;
    Align := alClient;
    BookMarkOptions.EnableKeys := false;
    OnStatusChange := @EditorStatusChanged;
    OnProcessUserCommand := @ProcessUserCommand;
    OnCommandProcessed := @UserCommandProcessed;
    OnReplaceText := @OnReplace;
    OnGutterClick := @Self.OnGutterClick;
    OnSpecialLineColors:=@OnEditorSpecialLineColor;
    Show;
  end;
  if FCodeTemplates<>nil then
    FCodeTemplates.AddEditor(FEditor);
  RefreshEditorSettings;
  aCompletion.AddEditor(FEditor);
  FEditor.Lines.Assign(OldSource);
  OldSource.Free;
  FEditor.SetFocus;
end;

Procedure TSourceEditor.DisplayControl;
Begin
if FControl = nil then Exit;

if (FControl is TCustomForm) then TCustomForm(FControl).Show
    else
    if (FCOntrol is TControl) then TControl(FCOntrol).Visible := True;

//Bringtofront does not work yet.
//TControl(FControl).BringToFront;
//so I hide it and unhide it.

TControl(FCOntrol).Visible := False;
TControl(FCOntrol).Visible := True;
end;

Function TSourceEditor.GetSource : TStrings;
Begin
  //return synedit's source.
  Result := FEditor.Lines;
end;

Procedure TSourceEditor.SetSource(value : TStrings);
Begin
  FEditor.Lines.Assign(Value);
end;

Function TSourceEditor.GetCurrentCursorXLine : Integer;
Begin
  Result := FEditor.CaretX
end;

Procedure TSourceEditor.SetCurrentCursorXLine(num : Integer);
Begin
  FEditor.CaretX := Num;
end;

Function TSourceEditor.GetCurrentCursorYLine : Integer;
Begin
  Result := FEditor.CaretY;
end;

Procedure TSourceEditor.SetCurrentCursorYLine(num : Integer);
Begin
  FEditor.CaretY := Num;
end;

Procedure TSourceEditor.SelectText(LineNum,CharStart,LineNum2,CharEnd : Integer);
var
   P : TPoint;
Begin
   P.X := CharStart;
   P.Y := LineNum;
   FEditor.BlockBegin := P;
   P.X := CharEnd;
   P.Y := LineNum2;
   FEditor.BlockEnd := P;
end;

Function TSourceEditor.GetModified : Boolean;
Begin
  Result := FEditor.Modified;
end;

procedure TSourceEditor.SetModified(NewValue:boolean);
begin
  FEditor.Modified:=NewValue;
end;

Function TSourceEditor.GetInsertMode : Boolean;
Begin
  Result := FEditor.Insertmode;
end;

Function TSourceEditor.Close : Boolean;
Begin
  Result := True;
  If Assigned(FOnBeforeClose) then
    FOnBeforeClose(Self);

  Visible := False;
  If Assigned(FOnAfterClose) then FOnAfterClose(Self);
end;

Procedure TSourceEditor.ReParent(AParent : TWInControl);
Begin
  CreateEditor(FAOwner,AParent);
End;

procedure TSourceEditor.SetCodeTemplates(
  NewCodeTemplates: TSynEditAutoComplete);
begin
  if NewCodeTemplates<>FCodeTemplates then begin
    if FCodeTemplates<>nil then
      FCodeTemplates.RemoveEditor(FEditor);
    if NewCodeTemplates<>nil then
      NewCodeTemplates.AddEditor(FEditor);
  end;
end;

procedure TSourceEditor.SetPopupMenu(NewPopupMenu: TPopupMenu);
begin
  if NewPopupMenu<>FPopupMenu then begin
    FPopupMenu:=NewPopupMenu;
    if FEditor<>nil then FEditor.PopupMenu:=NewPopupMenu;
  end;
end;

{------------------------------------------------------------------------}
                      { TSourceNotebook }

constructor TSourceNotebook.Create(AOwner: TComponent);


  function LoadPixmapRes(ResourceName:string; PixMap:TPixMap):boolean;
  var
    ms:TMemoryStream;
    res:TLResource;
  begin
    Result:=false;
    res:=LazarusResources.Find(ResourceName);
    if (res = nil) or (res.Value<>'') then begin
      if res.ValueType='XPM' then begin
        ms:=TMemoryStream.Create;
        try
          ms.Write(res.Value[1],length(res.Value));
          ms.Position:=0;
          PixMap.LoadFromStream(ms);
          Result:=true;
        finally
          ms.Free;
        end;
      end;
    end;
  end;


// TSourceNotebook.Create
var
  Pixmap1 : TPixmap;
  I : Integer;
begin
  inherited Create(AOwner);
  Caption := 'Lazarus Source Editor';

  if (EnvironmentOptions.SaveWindowPositions) 
  and (EnvironmentOptions.WindowPositionsValid) then begin
    BoundsRect:=EnvironmentOptions.SourceEditorBounds;
  end else begin
    Left := 260;
    Top := 150;
    Width := 400;
    Height := 400;
  end;

  FMainIDE := AOwner;

  FSourceEditorList := TList.Create;
  FCodeTemplateModul:=TSynEditAutoComplete.Create(Self);
  with FCodeTemplateModul do begin
    if FileExists(EditorOpts.CodeTemplateFilename) then
      AutoCompleteList.LoadFromFile(EditorOpts.CodeTemplateFilename)
    else
      if FileExists('lazarus.dci') then
        AutoCompleteList.LoadFromFile('lazarus.dci');
    OnTokenNotFound:=@OnCodeTemplateTokenNotFound;
    EndOfTokenChr:='[]{},.;:"+-*^@$\<>=''';
  end;
  if aWordCompletion=nil then begin
    aWordCompletion:=TWordCompletion.Create;
    with AWordCompletion do begin
      WordBufferCapacity:=100;
      OnGetSource:=@OnWordCompletionGetSource;
    end;
  end;
  FSaveDialog := TSaveDialog.Create(Self);
  FOpenDialog := TOpenDialog.Create(Self);
  BuildPopupMenu;


  MarksImgList := TImageList.Create(AOwner);

  //load 10 bookmark images
  for I := 0 to 9 do Begin
    Pixmap1:=TPixMap.Create;
    Pixmap1.TransparentColor:=clBtnFace;
    if not LoadPixmapRes('bookmark'+inttostr(i),Pixmap1) then
           LoadPixmapRes('default',Pixmap1);
    MarksImgList.Add(Pixmap1,nil);
  end;
  // load active breakpoint image
  Pixmap1:=TPixMap.Create;
  Pixmap1.TransparentColor:=clBtnFace;
  if not LoadPixmapRes('ActiveBreakPoint',Pixmap1) then
         LoadPixmapRes('default',Pixmap1);
  MarksImgList.Add(Pixmap1,nil);
  // load inactive breakpoint image
  Pixmap1:=TPixMap.Create;
  //Pixmap1.TransparentColor:=clBtnFace;
  if not LoadPixmapRes('InactiveBreakPoint',Pixmap1) then
         LoadPixmapRes('default',Pixmap1);
  MarksImgList.Add(Pixmap1,nil);
  

  aHighlighter:=TSynPasSyn.Create(AOwner);
    with aHighlighter do begin
    end;


  aCompletion := TSynCompletion.Create(AOwner);
    with aCompletion do
      Begin
        EndOfTokenChr:='()[]';
writeln('AAAA 1');
        Width:=400;
writeln('AAAA 2');
        OnExecute := @ccExecute;
        OnCancel := @ccCancel;
        OnCodeCompletion := @ccComplete;
        OnPaintItem:=@OnSynCompletionPaintItem;
        OnSearchPosition:=@OnSynCompletionSearchPosition;
        ShortCut:=Menus.ShortCut(VK_UNKNOWN,[]);
      end;


  StatusBar := TStatusBar.Create(self);
    with Statusbar do
      begin
       Parent := Self;
       Name := 'StatusBar';
       Visible := True;
       SimpleText := 'This is a test';
       Panels.Add;       //x,y coord
       Panels.Add;       //Readonly/Modified
       Panels.Add;       //OVR/INS
       Panels.Add;       //Unitname
       Panels[0].Text := '';
       Panels[0].Width := 100;
       Panels[0].Bevel := pbLowered;
       Panels[1].Text := '';
       Panels[1].Bevel := pbLowered;
       Panels[1].Width := 150;
       Panels[2].Text := '';
       Panels[2].Bevel := pbLowered;
       Panels[2].Width := 50;
       Panels[3].Text := 'INS';
       Panels[3].Bevel := pbLowered;
       Panels[3].Width := 50;
       SimplePanel := False;
      end;

  GotoDialog := TfrmGoto.Create(self);

  CodeCompletionTimer := TTimer.Create(self);
  CodeCompletionTimer.Enabled := False;
  CodeCompletionTimer.Interval := 500;


 Writeln('TSourceNotebook create exiting');
end;

destructor TSourceNotebook.Destroy;
var i: integer;
begin
writeln('[TSourceNotebook.Destroy]');
  for i:=FSourceEditorList.Count-1 downto 0 do
    Editors[i].Free;
  if Notebook<>nil then begin
    Notebook.Free;
    NoteBook:=nil;
  end;
  FCodeTemplateModul.Free;
  FSourceEditorList.Free;
  Gotodialog.free;
  inherited Destroy;
end;

function TSourceNotebook.OnSynCompletionPaintItem(AKey: string; 
  ACanvas: TCanvas;  X, Y: integer): boolean;
var i: integer;
begin
  with ACanvas do begin
    Font.Name:=EditorOpts.EditorFont;
    Font.Size:=EditorOpts.EditorFontHeight;
    Font.Style:=[fsBold];
    Font.Style:=[];
    i := 1;
    while i <= Length(AKey) do
      case AKey[i] of
        #1: begin
            Font.Color := (Ord(AKey[i + 3]) shl 8 + Ord(AKey[i + 2])) shl 8 + Ord(AKey[i + 1]);
            inc(i, 4);
          end;
        #2: begin
            Font.Color := (Ord(AKey[i + 3]) shl 8 + Ord(AKey[i + 2])) shl 8 + Ord(AKey[i + 1]);
            inc(i, 4);
          end;
        #3: begin
            case AKey[i + 1] of
              'B': Font.Style := Font.Style + [fsBold];
              'b': Font.Style := Font.Style - [fsBold];
              'U': Font.Style := Font.Style + [fsUnderline];
              'u': Font.Style := Font.Style - [fsUnderline];
              'I': Font.Style := Font.Style + [fsItalic];
              'i': Font.Style := Font.Style - [fsItalic];
            end;
            inc(i, 2);
          end;
      else
        TextOut(x, y, AKey[i]);
        x := x + TextWidth(AKey[i]);
        inc(i);
      end;
  end;
  Result:=true;
end;

procedure TSourceNotebook.OnWordCompletionGetSource(var Source:TStrings;
  SourceIndex:integer);
var TempEditor: TSourceEditor;
  i:integer;
begin
  TempEditor:=GetActiveSE;
  if SourceIndex=0 then begin
    Source:=TempEditor.EditorComponent.Lines;
  end else begin
    i:=0;
    while (i<FSourceEditorList.Count) do begin
      if Editors[i]<>TempEditor then dec(SourceIndex);
      if SourceIndex=0 then begin
        Source:=Editors[i].EditorComponent.Lines;
        exit;
      end;
      inc(i);
    end;
    Source:=nil;
  end;
end;

procedure TSourceNotebook.OnCodeTemplateTokenNotFound(Sender: TObject;
  AToken: string; AnEditor: TCustomSynEdit; var Index:integer);
var P:TPoint;
begin
  if (AnEditor.ReadOnly=false) and (CurrentCompletionType=ctNone) then begin
    CurrentCompletionType:=ctTemplateCompletion;
    with AnEditor do
      P := ClientToScreen(Point(CaretXPix,CaretYPix+LineHeight));
    aCompletion.Editor:=AnEditor;
    aCompletion.Execute(AToken,P.X,P.Y);
  end;
end;

procedure TSourceNotebook.OnSynCompletionSearchPosition(var APosition:integer);
var i,x:integer;
  CurStr,s:Ansistring;
  SL:TStringList;
begin
  if sCompl=nil then exit;
  case CurrentCompletionType of

    ctWordCompletion:
      begin
        // rebuild completion list
        APosition:=0;
        CurStr:=sCompl.CurrentString;
        SL:=TStringList.Create;
        try
          aWordCompletion.GetWordList(SL, CurStr, false, 30);
          sCompl.ItemList:=SL;
        finally
          SL.Free;
        end;
      end;

    ctCodeCompletion,ctTemplateCompletion:
      begin
        // search CurrentString in bold words (words after #3'B')
        CurStr:=sCompl.CurrentString;
        i:=0;
        while i<sCompl.ItemList.Count do begin
          s:=sCompl.ItemList[i];
          x:=1;
          while (x<=length(s)) and (s[x]<>#3) do inc(x);
          if x<length(s) then begin
            inc(x,2);
            if AnsiCompareText(CurStr,copy(s,x,length(CurStr)))=0 then begin
              APosition:=i;
              break;
            end;
          end;
          inc(i);
        end;
      end;

  end;
end;

procedure TSourceNotebook.ccComplete(var Value: ansistring; Shift: TShiftState);
var
  p1,p2:integer;
Begin
  if sCompl=nil then exit;
  case CurrentCompletionType of
    ctTemplateCompletion, ctCodeCompletion:
      begin
        // the completion is the bold text between #3'B' and #3'b'
        p1:=Pos(#3,Value);
        if p1>=0 then begin
          p2:=p1+2;
          while (p2<=length(Value)) and (Value[p2]<>#3) do inc(p2);
          Value:=copy(Value,p1+2,p2-p1-2);
          // keep parent identifier (in front of '.')
          p1:=length(ccSelection);
          while (p1>=1) and (ccSelection[p1]<>'.') do dec(p1);
          if p1>=1 then
            Value:=copy(ccSelection,1,p1)+Value;
        end;
      end;
    ctWordCompletion: ;
  end;

  ccSelection := '';
  case CurrentCompletionType of
    ctTemplateCompletion:
      begin
        if Value<>'' then
          FCodeTemplateModul.ExecuteCompletion(Value,GetActiveSE.EditorComponent);
        Value:='';
      end;
    ctWordCompletion:
      begin
        if Value<>'' then AWordCompletion.AddWord(Value);
      end;
  end;
  sCompl.Deactivate;
  sCompl:=nil;
  CurrentCompletionType:=ctNone;
End;

Procedure TSourceNotebook.ccCancel(Sender : TObject);
var ActSE: TSourceEditor;
begin
  if sCompl=nil then exit;
  sCompl.Deactivate;
  sCompl:=nil;
  CurrentCompletionType:=ctNone;
  ActSE:=GetActiveSE;
  if ActSE<>nil then LCLLinux.ShowCaret(ActSE.EditorComponent.Handle);
end;

Procedure TSourceNotebook.ccExecute(Sender : TObject);
type
   TMethodRec = record
    Flags : TParamFlags;
    ParamName : ShortString;
    TypeName : ShortString;
   end;
var
  S : TStrings;
  CompInt : TComponentInterface;
  CompName, Prefix, CurLine : String;
  I, X1, X2 : Integer;
  propKind : TTypeKind;
  TypeInfo : PTypeInfo;
  TypeData : PTypeData;
  NewStr,ParamStr,PropName : String;
  Count,Offset,Len : Integer;
  MethodRec : TMethodRec;

Begin
  CompInt := nil;
  sCompl := TSynBaseCompletion(Sender);
  S := TStringList.Create;
  Prefix := sCompl.CurrentString;
  case CurrentCompletionType of
   ctCodeCompletion:
    begin
      ccSelection := Prefix;
      with GetActiveSE.EditorComponent do begin
        CurLine:=LineText;
        X1:=CaretX-1;
      end;
      if X1>length(CurLine) then X1:=length(CurLine);
      while (X1>0) and (CurLine[X1]<>'.') do dec(X1);
      X2:=X1-1;
      while (X2>0) and (CurLine[X2] in ['A'..'Z','a'..'z','0'..'9','_']) do dec(X2);
      CompName:=copy(CurLine,X2+1,X1-X2-1);
      CompInt := TComponentInterface(FormEditor1.FindComponentByName(CompName));
      if CompInt = nil then Exit;
      //get all methods
      NewStr := '';
      for I := 0 to CompInt.GetPropCount-1 do
      Begin
        PropName:=#3'B'+CompInt.GetPropName(I)+#3'b';
        PropKind := CompInt.GetPropType(i);
        case PropKind of
          tkMethod :
            Begin
              TypeInfo := CompInt.GetPropTypeInfo(I);
              TypeData :=  GetTypeData(TypeInfo);

              //check for parameters
              if TypeData^.ParamCount > 0 then
              Begin
                {Writeln('----');
                for Count := 0 to 60 do
                  if TypeData^.ParamList[Count] in ['a'..'z','A'..'Z','0'..'9'] then
                    Write(TypeData^.ParamList[Count])
                  else
                    Begin
                      Write('$',HexStr(ord(TypeData^.ParamList[Count]),3),' ');
                    end;
                }
                ParamStr := '';
                Offset:=0;
                for Count := 0 to TypeData^.ParamCount-1 do
                begin
                  Len:=1;  // strange: SizeOf(TParamFlags) is 4, but the data is only 1 byte
                  Move(TypeData^.ParamList[Offset],MethodRec.Flags,Len);
                  inc(Offset,Len);

                  Len:=ord(TypeData^.ParamList[Offset]);
                  inc(Offset);
                  SetLength(MethodRec.ParamName,Len);
                  Move(TypeData^.ParamList[Offset],MethodRec.ParamName[1],Len);
                  inc(Offset,Len);

                  Len:=ord(TypeData^.ParamList[Offset]);
                  inc(Offset);
                  SetLength(MethodRec.TypeName,Len);
                  Move(TypeData^.ParamList[Offset],MethodRec.TypeName[1],Len);
                  inc(Offset,Len);

                  if ParamStr<>'' then ParamStr:=';'+ParamStr;
                  if MethodRec.ParamName='' then
                    ParamStr:=MethodRec.TypeName+ParamStr
                  else
                    ParamStr:=MethodRec.ParamName+':'+MethodRec.TypeName+ParamStr;
                  if (pfVar in MethodRec.Flags) then ParamStr := 'var '+ParamStr;
                  if (pfConst in MethodRec.Flags) then ParamStr := 'const '+ParamStr;
                  if (pfOut in MethodRec.Flags) then ParamStr := 'out '+ParamStr;
                end;
                NewStr:='('+ParamStr+')';
              end else NewStr:='';
              case TypeData^.MethodKind of
                mkProcedure : 
                  NewStr := 'procedure   '+PropName+' :'+CompInt.GetPropTypeName(I);
                mkFunction  : 
                  NewStr := 'function    '+PropName+' :'+CompInt.GetPropTypeName(I);
                mkClassFunction : 
                  NewStr := 'function    '+PropName+' :'+'Function '+NewStr;
                mkClassProcedure : 
                  NewStr := 'procedure   '+PropName+' :'+'Procedure '+NewStr;
                mkConstructor : 
                  NewStr := 'constructor '+PropName+' '+'procedure ';
                mkDestructor : 
                  NewStr := 'destructor  '+PropName+' '+'procedure ';
              end;
            end;


          tkObject : 
             NewStr := 'object      '+PropName+' :'+CompInt.GetPropTypeName(I);
          tkInteger,tkChar,tkEnumeration,tkWChar : 
             NewStr := 'var         '+PropName+' :'+CompInt.GetPropTypeName(I);
          tkBool :  
             NewStr := 'var         '+PropName+' :'+CompInt.GetPropTypeName(I);
          tkClass : 
             NewStr := 'class       '+PropName+' :'+CompInt.GetPropTypeName(I);
          tkFloat :  
             NewStr := 'var         '+PropName+' :'+CompInt.GetPropTypeName(I);
          tkSString :  
             NewStr := 'var         '+PropName+' :'+CompInt.GetPropTypeName(I);
          tkUnKnown,tkLString,tkWString,tkAString,tkVariant :  
             NewStr := 'var         '+PropName+' :'+CompInt.GetPropTypeName(I);
        end;

        if NewStr <> '' then S.Add(NewStr);
        NewStr := '';
      end;  // end for
    end;

   ctWordCompletion:
     begin
       ccSelection:='';
     end;

   ctTemplateCompletion:
     begin
       ccSelection:='';
       for I:=0 to FCodeTemplateModul.Completions.Count-1 do begin
         NewStr:=FCodeTemplateModul.Completions[I];
         if NewStr<>'' then begin
           NewStr:=#3'B'+NewStr+#3'b';
           while length(NewStr)<10+4 do NewStr:=NewStr+' ';
           NewStr:=NewStr+' '+FCodeTemplateModul.CompletionComments[I];
           S.Add(NewStr);
         end;
       end;
     end;

  end;

  sCompl.ItemList := S;
  sCompl.CurrentString:=Prefix;
End;

Function TSourceNotebook.CreateNotebook : Boolean;
Begin
  Result := False;
  if not assigned(Notebook) then
    Begin
      Result := True;
      Notebook := TNotebook.Create(self);
      with Notebook do
        Begin
          Parent := Self;
          Align := alClient;
          Left := 0;
          Top :=2;
          Width := ClientWidth;
          Height := ClientHeight-Notebook.top;
          Pages.Strings[0] := 'unit1';
          PageIndex := 0;   // Set it to the first page
          OnPageChanged := @NotebookPageChanged;
          Show;
        end; //with
      Show;  //used to display the code form

    end;
End;

Procedure TSourceNotebook.BuildPopupMenu;

  Function Seperator : TMenuItem;
  Begin
    Result := TMenuItem.Create(Self);
    Result.Caption := '-';
  end;

var
  MenuItem : TMenuItem;
  SubMenuItem : TMenuItem;
  I : Integer;
Begin
  SrcPopupMenu := TPopupMenu.Create(Self);
  SrcPopupMenu.AutoPopup := True;

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name:='ClosePageMenuItem';
  MenuItem.Caption := '&Close Page';
  MenuItem.OnClick := @CloseClicked;
  SrcPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name:='OpenFileAtCursorMenuItem';
  MenuItem.Caption := '&Open file at cursor';
  MenuItem.OnClick := @OpenAtCursorClicked;
  SrcPopupMenu.Items.Add(MenuItem);

  SrcPopupMenu.Items.Add(Seperator);

  ToggleMenuItem := TMenuItem.Create(Self);
  ToggleMenuItem.Name:='ToggleMenuItem';
  ToggleMenuItem.Caption := '&Toggle Bookmark';
  SrcPopupMenu.Items.Add(ToggleMenuItem);

  for I := 0 to 9 do
    Begin
      SubMenuItem := TMenuItem.Create(Self);
      SubMenuItem.Name:='SubToggleMenuItem'+IntToStr(I);
      SubMenuItem.Caption := 'Bookmark '+inttostr(i);
      SubMenuItem.OnClick := @BookmarkClicked;
      SubMenuItem.Tag := I;
      ToggleMenuItem.Add(SubMenuItem);
    end;

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name:='GotoBookmarkMenuItem';
  MenuItem.Caption := '&Goto Bookmark';
  SrcPopupMenu.Items.Add(MenuItem);

  for I := 0 to 9 do
    Begin
      SubMenuItem := TMenuItem.Create(Self);
      SubmenuItem.Name:='GotoBookmark'+IntToStr(I)+'MenuItem';
      SubMenuItem.Caption := 'Bookmark '+inttostr(i);
      SubMenuItem.OnClick := @BookmarkGotoClicked;
      SubMenuItem.Tag := I;
      MenuItem.Add(SubMenuItem);
    end;

  SrcPopupMenu.Items.Add(Seperator);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name:='ReadOnlyMenuItem';
  MenuItem.Caption := 'Read Only';
  MenuItem.OnClick := @ReadOnlyClicked;
  SrcPopupMenu.Items.Add(MenuItem);

  SrcPopupMenu.Items.Add(Seperator);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name:='DebugMenuItem';
  MenuItem.Caption := 'Debug';
  SrcPopupMenu.Items.Add(MenuItem);

      SubMenuItem := TMenuItem.Create(Self);
      SubMenuItem.Name := 'ToggleBreakpointMenuItem';
      SubMenuItem.Caption := '&Toggle Breakpoint';
      SubMenuItem.OnClick := @ToggleBreakpointClicked;
      MenuItem.Add(SubMenuItem);

      SubMenuItem := TMenuItem.Create(Self);
      SubMenuItem.Name := 'RunToCursorMenuItem';
      SubMenuItem.Caption := '&Run to Cursor';
      //SubMenuItem.OnClick := @ToggleBreakpoint;
      MenuItem.Add(SubMenuItem);

  SrcPopupMenu.Items.Add(Seperator);
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name := 'ShowLineNumbersMenuItem';
  MenuItem.Caption := 'Show Line Numbers';
  menuItem.OnClick := @ToggleLineNumbersClicked;
  SrcPopupMenu.Items.Add(MenuItem);

end;

Procedure TSourceNotebook.EditorChanged(sender : TObject);
Begin
  UpdateStatusBar;
End;

Function TSourceNotebook.NewSe(PageNum : Integer) : TSourceEditor;
Begin
writeln('TSourceNotebook.NewSe A');
  if CreateNotebook then Pagenum := 0;
  if Pagenum < 0 then begin
    // add a new page right to the current
    Pagenum := Notebook.PageIndex+1;
    Notebook.Pages.Insert(PageNum,FindUniquePageName('',-1));
  end;
writeln('TSourceNotebook.NewSe B  ',Notebook.PageIndex,',',NoteBook.Pages.Count);
  Result := TSourceEditor.Create(Self,Notebook.Page[PageNum]);
writeln('TSourceNotebook.NewSe C');
  FSourceEditorList.Add(Result);
  Result.FUnitName:=Notebook.Pages[PageNum];
  Result.CodeTemplates:=CodeTemplateModul;
  Notebook.PageIndex := Pagenum;
  Result.EditorComponent.BookMarkOptions.BookmarkImages := MarksImgList;
  Result.PopupMenu:=SrcPopupMenu;
  Result.OnEditorChange := @EditorChanged;
writeln('TSourceNotebook.NewSe end');
end;

Procedure TSourceNotebook.DisplayCodeforControl(Control : TObject);
Var
   I,X : Integer;
Begin
   X := FSourceEditorList.Count;
   if X = 0 then Exit;
   I := 0;
   while  (I < X)
   and (TSourceEditor(FSourceEditorList.Items[I]).Control <> TComponent(Control)) do
     Begin
       inc(i);
       Writeln(' I = '+inttostr(i));
     end;

   if I < X then
     DisplayPage(TSourceEditor(FSOurceEditorList.Items[I]));
End;

Procedure TSourceNotebook.DisplayCodefromUnitName(UnitName : String);
Var
   I,X : Integer;
Begin
   X := FSourceEditorList.Count;
   if X = 0 then Exit;
   I := 0;
   while  (I < X)
   and (Uppercase(TSourceEditor(FSourceEditorList.Items[I]).Unitname)
     <> Uppercase(Unitname)) do
   Begin
     inc(i);
   end;
   if I < X then
     DisplayPage(TSourceEditor(FSOurceEditorList.Items[I]));
end;

Procedure TSourceNotebook.DisplayFormforActivePage;
Begin
Writeln('DisplayFormForActivePage');
  GetActiveSE.DisplayControl;
Writeln('Exiting DisplayFormForActivePage');
End;

Function TSourceNotebook.DisplayPage(SE : TSourceEditor) : Boolean;
Var
   I,X : Integer;
   TempEditor : TControl;
Begin
   Result := False;

    for X := 0 to Notebook.Pages.Count-1 do
        Begin
          With Notebook.Page[X] do
          for I := 0 to ControlCount-1 do
               if Controls[I] is TmwCustomEdit then
                  Begin
                     TempEditor := Controls[I];
                     Break;
                  end;
          if SE.EditorComponent = TempEditor then Begin
              Writeln('The editor was found on page '+inttostr(x));
              Break;
              end;
        End;


  if SE.EditorComponent = TempEditor then
  Begin
    Notebook.PageIndex := X;
    //Bringtofront does not work yet.
    //Notebook.BringToFront;
    //so I hide it and unhide it.
    Visible := False;
    Visible := True;

  end
  else
  Begin  //the SE isn't on a page so we need to create a page for it.
    Notebook.PageIndex := Notebook.Pages.Add(SE.UnitName);
    SE.ReParent(Notebook.Page[Notebook.Pageindex]);
  end;
end;

function TSourceNotebook.FindSourceEditorWithPageIndex(
  PageIndex:integer):TSourceEditor;
var I:integer;
  TempEditor : TControl;
begin
  Result := nil;
  if (FSourceEditorList=nil)
    or (Notebook=nil) 
    or (PageIndex<0) or (PageIndex>=Notebook.Pages.Count) then exit;
  TempEditor:=nil;
  with Notebook.Page[PageIndex] do
    for I := 0 to ControlCount-1 do
      if Controls[I] is TmwCustomEdit then
        Begin
          TempEditor := Controls[I];
          Break;
        end;
  if TempEditor=nil then exit;
  I := FSourceEditorList.Count-1;
  while (I>=0) 
  and (TSourceEditor(FSourceEditorList[I]).EditorComponent <> TempEditor) do
    dec(i);
  if i<0 then exit;
  Result := TSourceEditor(FSourceEditorList[i]);
end;

Function TSourceNotebook.GetActiveSE : TSourceEditor;
Begin
  Result := nil;
  if (FSourceEditorList=nil) or (FSourceEditorList.Count=0)
    or (Notebook=nil) or (Notebook.PageIndex<0) then exit;
  Result:= FindSourceEditorWithPageIndex(Notebook.PageIndex);
end;


Function TSourceNotebook.Empty : Boolean;
Begin
  Result := (not assigned(Notebook)) or (Notebook.Pages.Count = 0);
end;

function TSourceNotebook.SomethingModified: boolean;
var i: integer;
begin
  Result:=false;
  for i:=0 to EditorCount-1 do Result:=Result or Editors[i].Modified;
end;

Procedure TSourceNotebook.NextEditor;
Begin
  if Notebook.PageIndex < Notebook.Pages.Count-1 then
    Notebook.PageIndex := Notebook.PageIndex+1
  else
    NoteBook.PageIndex := 0;
End;


Procedure TSourceNotebook.PrevEditor;
Begin
  if Notebook.PageIndex > 0 then
    Notebook.PageIndex := Notebook.PageIndex-1
  else
    NoteBook.PageIndex := NoteBook.Pages.Count-1;
End;


Procedure TSourceNotebook.OpenClicked(Sender: TObject);
Begin
  if Assigned(FOnOpenClicked) then FOnOpenClicked(Sender);
end;

Procedure TSourceNotebook.FindClicked(Sender : TObject);
var TempEditor:TSourceEditor;
Begin
  TempEditor:=GetActiveSe;
  if TempEditor <> nil then TempEditor.StartFindAndReplace(false);
End;

Procedure TSourceNotebook.ReplaceClicked(Sender : TObject);
var TempEditor:TSourceEditor;
Begin
  TempEditor:=GetActiveSe;
  if TempEditor <> nil then TempEditor.StartFindAndReplace(true);
End;

Procedure TSourceNotebook.FindAgainClicked(Sender : TObject);
var TempEditor:TSourceEditor;
Begin
  TempEditor:=GetActiveSe;
  if TempEditor <> nil then TempEditor.FindAgain;
End;

Procedure TSourceNotebook.BookMarkClicked(Sender : TObject);
// popup menu toggle bookmark clicked
var
  MenuItem : TMenuItem;
Begin
  MenuItem := TMenuItem(sender);
  BookMarkToggle(MenuItem.Tag);
end;

Procedure TSourceNotebook.BookMarkGotoClicked(Sender : TObject);
// popup menu goto bookmark clicked
var
  MenuItem : TMenuItem;
Begin
  MenuItem := TMenuItem(sender);
  GotoBookMark(MenuItem.Tag);
end;

Procedure TSourceNotebook.ReadOnlyClicked(Sender : TObject);
var ActEdit:TSourceEditor;
begin
  ActEdit:=GetActiveSE;
  ActEdit.EditorComponent.ReadOnly := not(ActEdit.EditorComponent.ReadOnly);
  UpdateStatusBar;
end;

Procedure TSourceNotebook.ToggleBreakpointClicked(Sender : TObject);
begin
  // ToDo
end;

Procedure TSourceNotebook.ToggleLineNumbersClicked(Sender : TObject);
var
  MenuITem : TMenuItem;
  ActEdit:TSourceEditor;
begin
  MenuItem := TMenuITem(Sender);
  ActEdit:=GetActiveSE;
  MenuItem.Checked := not(ActEdit.EditorComponent.Gutter.ShowLineNumbers);
  ActEdit.EditorComponent.Gutter.ShowLineNumbers := MenuItem.Checked;
end;

Procedure TSourceNotebook.OpenAtCursorClicked(Sender : TObject);
begin
  if Assigned(FOnOpenFileAtCursorClicked) then
    FOnOpenFileAtCursorClicked(Sender);
end;

Procedure TSourceNotebook.BookMarkToggle(Value : Integer);
var
  MenuItem : TMenuItem;
  ActEdit,AnEdit:TSourceEditor;
Begin
  MenuItem := TMenuItem(ToggleMenuItem.Items[Value]);
  MenuItem.Checked := not(MenuItem.Checked);
  ActEdit:=GetActiveSE;

  AnEdit:=FindBookmark(Value);
  if AnEdit<>nil then AnEdit.EditorComponent.ClearBookMark(Value);
  if MenuItem.Checked then
    Begin
      ActEdit.EditorComponent.SetBookMark(Value,
         ActEdit.EditorComponent.CaretX,ActEdit.EditorComponent.CaretY);
      MenuItem.Caption := MenuItem.Caption + '*';
    end
  else
    begin
      MenuItem.Caption := copy(MenuItem.Caption,1,Length(MenuItem.Caption)-1);
    end;
end;

{This is called from outside to toggle a bookmark}
Procedure TSourceNotebook.ToggleBookmark(Value : Integer);
Begin
  BookMarkToggle(Value);
End;

Procedure TSourceNotebook.BookMarkGoto(Value : Integer);
var AnEditor:TSourceEditor;
begin
  if Notebook=nil then exit;
  AnEditor:=FindBookmark(Value);
  if AnEditor<>nil then begin
    AnEditor.EditorComponent.GotoBookMark(Value);
    Notebook.PageIndex:=FindPageWithEditor(AnEditor);
  end;
end;

{This is called from outside to Goto a bookmark}
Procedure TSourceNotebook.GoToBookmark(Value: Integer);
begin
  BookMarkGoTo(Value);
End;

Procedure TSourceNotebook.NewFile(UnitName: String; Source : TStrings);
Var
  TempEditor : TSourceEditor;
Begin
  //create a new page
  TempEditor := NewSE(-1);
  TempEditor.Unitname := Unitname;
  TempEditor.Source := Source;
  Notebook.Pages[Notebook.PageIndex] :=
    FindUniquePageName(UnitName,Notebook.PageIndex);
writeln('[TSourceNotebook.NewFile] end');
end;

Procedure TSourceNotebook.CloseFile(PageIndex:integer);
var TempEditor: TSourceEditor;
Begin
writeln('TSourceNotebook.CloseFile A  PageIndex=',PageIndex);
  TempEditor:= FindSourceEditorWithPageIndex(PageIndex);
  if TempEditor=nil then exit;
  TempEditor.Close;
  FSourceEditorList.Remove(TempEditor);
  TempEditor.Free;
  if Notebook.Pages.Count>1 then begin
writeln('TSourceNotebook.CloseFile B  PageIndex=',PageIndex);
    Notebook.Pages.Delete(PageIndex);
writeln('TSourceNotebook.CloseFile C  PageIndex=',PageIndex);
    UpdateStatusBar;
  end else begin
writeln('TSourceNotebook.CloseFile D  PageIndex=',PageIndex);
    Notebook.Free;
writeln('TSourceNotebook.CloseFile E  PageIndex=',PageIndex);
    Notebook:=nil;
    Hide;
  end;
writeln('TSourceNotebook.CloseFile END');
end;

Procedure TSourceNotebook.NewClicked(Sender: TObject);
Begin
  if Assigned(FOnNewClicked) then FOnNewClicked(Sender);
End;

Procedure TSourceNotebook.SaveClicked(Sender: TObject);
Begin
  if Assigned(FOnSaveClicked) then FOnSaveClicked(Sender);
end;

Function TSourceNotebook.ActiveUnitName : String;
Begin
  Result := GetActiveSE.UnitName;
end;

Function TSourceNotebook.ActiveFileName : AnsiString;
Begin
  Result := GetActiveSE.FileName;
end;

function TSourceNotebook.GetEditors(Index:integer):TSourceEditor;
begin
  Result:=TSourceEditor(FSourceEditorList[Index]);
end;

function TSourceNotebook.EditorCount:integer;
begin
  Result:=FSourceEditorList.Count;
end;

Procedure TSourceNotebook.CloseClicked(Sender : TObject);
Begin
  if Assigned(FOnCloseClicked) then FOnCloseClicked(Sender);
end;

Function TSourceNotebook.FindUniquePageName(FileName:string; 
  IgnorePageIndex:integer):string;
var I:integer;
  ShortName,Ext:string;

  function PageNameExists(AName:string):boolean;
  var a:integer;
  begin
    Result:=false;
    if Notebook=nil then exit;
    for a:=0 to Notebook.Pages.Count-1 do begin
      if (a<>IgnorePageIndex) 
      and (lowercase(Notebook.Pages[a])=lowercase(AName)) then begin
        Result:=true;
        exit;
      end;
    end;
  end;

begin
  if FileName='' then begin
    FileName:='unit1';
    if not PageNameExists(FileName) then begin
      Result:=Filename;
      exit;
    end;
  end;
  ShortName:=ExtractFileName(FileName);
  Ext:=ExtractFileExt(ShortName);
  if (Ext='.pp') or (Ext='.pas') then
    ShortName:=copy(ShortName,1,length(ShortName)-length(Ext));
  Result:=ShortName;
  if PageNameExists(Result) then begin
    i:=1;
    repeat
      inc(i);
      Result:=ShortName+'('+IntToStr(i)+')';
    until PageNameExists(Result)=false;
  end;
end;

Procedure TSourceNotebook.SaveAsClicked(Sender : TObject);
Begin
  if Assigned(FOnSaveAsClicked) then FOnSaveAsClicked(Sender);
end;

Procedure TSourceNotebook.SaveAllClicked(Sender : TObject);
Begin
  if Assigned(FOnSaveAllClicked) then FOnSaveAllClicked(Sender);
end;

procedure TSourceNotebook.ToggleFormUnitClicked(Sender: TObject);
begin
  if Assigned(FOnToggleFormUnitClicked) then FOnToggleFormUnitClicked(Sender);
end;

Function TSourceNotebook.GetSourceForUnit(UnitName : String) : TStrings;
Var
  I : Integer;
  TempEditor : TSourceEditor;
begin
   For I := 0 to  FSourceEditorList.Count-1 do
       Begin
        TempEditor := TSourceEditor(FSourceEditorList.Items[i]);
        if Uppercase(TempEditor.UnitName) = Uppercase(Unitname) then
           Break;
       End;

        if Uppercase(TempEditor.UnitName) = Uppercase(Unitname) then
        Result := TempEditor.Source
          else
        Result := nil;

End;

Function TSourceNotebook.SetSourceForUnit(UnitName : String;
  NewSource : TStrings) : Boolean;
Var
  I : Integer;
  TempEditor : TSourceEditor;
begin
   Result := False;
   For I := 0 to  FSourceEditorList.Count-1 do
       Begin
        TempEditor := TSourceEditor(FSourceEditorList.Items[i]);
        if Uppercase(TempEditor.UnitName) = Uppercase(Unitname) then
           Break;
       End;

        if Uppercase(TempEditor.UnitName) = Uppercase(Unitname) then
        Begin
          TempEditor.Source := NewSource;
          Result := True;
        end;
End;

Procedure TSourceNotebook.UpdateStatusBar;
var
  tempEditor : TSourceEditor;
begin
   if not Visible then exit;
   TempEditor := GetActiveSE;
   if TempEditor = nil then Exit;
   TempEditor.ErrorLine:=-1;
   Statusbar.Panels[3].Text := ExtractFileName(TempEditor.Filename);

   If TempEditor.Modified then
     StatusBar.Panels[1].Text := 'Modified'
   else
     StatusBar.Panels[1].Text := '';

   If TempEditor.ReadOnly then
     if StatusBar.Panels[1].Text <> '' then
       StatusBar.Panels[1].Text := StatusBar.Panels[1].Text + '/ReadOnly'
     else
       StatusBar.Panels[1].Text := 'Readonly';


   Statusbar.Panels[0].Text :=
     Format(' %6d:%4d',[TempEditor.CurrentCursorYLine,TempEditor.CurrentCursorXLine]);

   if GetActiveSE.InsertMode then
     Statusbar.Panels[2].Text := 'INS' else
     Statusbar.Panels[2].Text := 'OVR';
End;

function TSourceNotebook.FindBookmark(BookmarkID: integer): TSourceEditor;
var i,x,y:integer;
begin
  for i:=0 to EditorCount-1 do begin
    if Editors[i].EditorComponent.GetBookmark(BookMarkID,x,y) then begin
      Result:=Editors[i];
      exit;
    end;
  end;
  Result:=nil;
end;

function TSourceNotebook.FindPageWithEditor(
  ASourceEditor: TSourceEditor):integer;
var i:integer;
begin
  if Notebook=nil then begin
    Result:=-1;
  end else begin
    Result:=Notebook.Pages.Count-1;
    while (Result>=0) do begin
      with Notebook.Page[Result] do
        for I := 0 to ControlCount-1 do
          if Controls[I]=ASourceEditor.EditorComponent then exit;
      dec(Result);
    end;
  end;
end;

Procedure TSourceNotebook.NotebookPageChanged(Sender : TObject);
var TempEditor:TSourceEditor;
Begin
  TempEditor:=GetActiveSE;
  if TempEditor <> nil then
  begin
    TempEditor.FocusEditor;
    UpdateStatusBar;
  end;
end;

Procedure TSourceNotebook.ProcessParentCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: char; Data: pointer);
var Handled: boolean;
begin
  if Assigned(FOnProcessUserCommand) then begin
    Handled:=false;
    FOnProcessUserCommand(Self,Command,Handled);
    if Handled then exit;
  end;

  Handled:=true;
  case Command of
  ecNextEditor:
    NextEditor;

  ecPrevEditor :
    PrevEditor;

  ecSave :
    SaveClicked(self);

  ecOpen :
    OpenClicked(self);

  ecJumpToEditor :
    Begin
      // This is NOT implemented yet

    end;

  ecGotoEditor1..ecGotoEditor9,ecGotoEditor0:
    if Notebook.Pages.Count>Command-ecGotoEditor1 then
      Notebook.PageIndex:=Command-ecGotoEditor1;

  ecToggleFormUnit:
    ToggleFormUnitClicked(self);
    
  ecGotoMarker0..ecGotoMarker9:
    BookMarkGoto(Command - ecGotoMarker0);

  ecSetMarker0..ecSetMarker9:
    BookMarkToggle(Command - ecSetMarker0);
  else
    Handled:=false;
  end;  //case
  if Handled then Command:=ecNone;
end;

Procedure TSourceNotebook.ParentCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: char; Data: pointer);
var Handled: boolean;
begin
  if Assigned(FOnUserCommandProcessed) then begin
    Handled:=false;
    FOnUserCommandProcessed(Self,Command,Handled);
    if Handled then exit;
  end;

  Handled:=true;
  case Command of
  ecClose :
    CloseClicked(self);
  else
    Handled:=false;
  end;  //case
  if Handled then Command:=ecNone;
end;

Procedure TSourceNotebook.ReloadEditorOptions;
var
  I : integer;
Begin
  //this reloads the colors for the highlighter and other editor settings.
  for I := 0 to FSourceEditorList.Count-1 do
    TSourceEditor(FSourceEditorList.Items[i]).RefreshEditorSettings;

  // reload code templates
  with CodeTemplateModul do begin
    if FileExists(EditorOpts.CodeTemplateFilename) then
      AutoCompleteList.LoadFromFile(EditorOpts.CodeTemplateFilename)
    else
      if FileExists('lazarus.dci') then
        AutoCompleteList.LoadFromFile('lazarus.dci');
  end;
end;


{  GOTO DIALOG}

Constructor TfrmGoto.Create(AOWner : TComponent);
begin
  inherited Create(AOwner);

  if LazarusResources.Find(ClassName)=nil then begin
    position := poScreenCenter;
    Width := 250;
    Height := 100;
    Caption := 'Goto';

    Label1 := TLabel.Create(self);
    with Label1 do
    Begin
      Parent := self;
      Top := 10;
      Left := 5;
      Caption := 'Goto line :';
      Visible := True;
    end;

    Edit1 := TEdit.Create(self);
    with Edit1 do
    Begin
      Parent := self;
      Top := 30;
      Width := self.width-40;
      Left := 5;
      Caption := '';
      OnKeyDown:=@Edit1KeyDown;
      Visible := True;
    end;

    btnOK := TBitbtn.Create(self);
    with btnOK do
    Begin
      Parent := self;
      Top := 70;
      Left := 40;
      kind := bkOK;
      Visible := True;
    end;

    btnCancel := TBitbtn.Create(self);
    with btnCancel do
    Begin
      Parent := self;
      Top := 70;
      Left := 120;
      kind := bkCancel;
      Visible := True;
    end;

    Edit1.SetFocus;
  end;
end;

procedure TfrmGoto.Edit1KeyDown(Sender: TObject; var Key:Word;
   Shift:TShiftState);
begin
  if (Key=VK_RETURN) then ModalResult:=mrOk;
  if (Key=VK_ESCAPE) then ModalResult:=mrCancel;
end;


initialization
  aHighlighter:=nil;
  aCompletion:=nil;
  scompl:=nil;
  GotoDialog:=nil;
  CodeCompletionTimer:=nil;
  AWordCompletion:=nil;

{$I designer/bookmark.lrs}

finalization
  if aWordCompletion<>nil then begin
    aWordCompletion.Free;
    aWordCompletion:=nil;
  end;


end.



