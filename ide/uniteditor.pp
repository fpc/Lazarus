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
{$H+}
{This unit builds the TSourceNotbook that the editors are held on.  It also has
 a class that controls the editors (TSourceEditor)
}

//{$DEFINE NEW_EDITOR}
{$DEFINE NEW_EDITOR_SYNEDIT}
unit UnitEditor;

{$mode objfpc}

interface

uses
  classes, Controls, forms,buttons,comctrls,sysutils,Dialogs,FormEditor,Find_Dlg,
{$ifdef NEW_EDITOR_SYNEDIT}
  SynEdit, SynEditHighlighter, SynHighlighterPas, SynEditAutoComplete,
  SynEditKeyCmds,
{$else}
	mwcustomedit,mwPasSyn,
{$endif}
       Graphics,Extctrls,Menus;

type

{$ifdef NEW_EDITOR_SYNEDIT}
  TmwCustomEdit = TSynEdit;
  TmwPasSyn = TSynPasSyn;
{$endif}

  TNotifyFileEvent = procedure(Sender: Tobject; Filename : String) of Object;


{---- TSource Editor ---
  TSourceEditor is the class that controls access the the Editor and the source code.
  It creates the PopupMenu that appears when you right-click on the editor.  It calls
  the editor functions for bookmarks, saves/opens files, adds control code to the source,
  creates the initial code for a form, and holds the unitname and filename properties.
 ---- TSource Editor ---}
  TSourceEditor = class
  private
    //FAOwner is normally a TSourceNotebook.  This is set in the Create constructor.
    FAOwner : TComponent;
{$ifdef NEW_EDITOR_SYNEDIT}
    FEditor     : TSynEdit;
    FSynAutoComplete: TSynAutoComplete;
{$else}
    FHighlighter: TmwPasSyn;
    FEditor     : TmwCustomEdit;
{$endif}
    //if this is a Form or Datamodule, this is used
    FControl: TComponent;

    //pulled out of the editor by the Function FEditor.CaretX
    FCurrentCursorXLine : Integer;

    //pulled out of the editor by the Function FEditor.CaretY
    FCurrentCursorYLine : Integer;

    //Set during OPEN and Save
    FFileName : String;

    // Used GetModified like this -> Result := FEditor.Modified
    FModified : Boolean;

    // Used GetReadolny like this -> Result := FEditor.Readonly
    FReadOnly : Boolean;

    //created during the constructor.  This is the popup you see when right-clicking on the editor
    FPopUpMenu : TPopupMenu;

    //pulled out of the editor by getting it's TStrings
    FSource : TStringList;

    //set on OPEN/SAVE
    FUnitName : String;

    FOnAfterClose : TNotifyEvent;
    FOnAfterOpen : TNotifyEvent;
    FOnAfterSave : TNotifyEvent;
    FOnBeforeClose : TNotifyEvent;
    FOnBeforeOpen : TNotifyEvent;
    FOnBeforeSave : TNotifyEvent;
    FOnEditorChange: TNotifyEvent;
    FVisible : Boolean;

    Procedure BuildPopupMenu;
    Function GetSource : TStrings;
    Procedure SetSource(value : TStrings);
    Function GetCurrentCursorXLine : Integer;
    Procedure SetCurrentCursorXLine(num : Integer);
    Function GetCurrentCursorYLine : Integer;
    Procedure SetCurrentCursorYLine(num : Integer);
    Function GetAncestor : String;
    Function GetModified : Boolean;
    Function GetInsertMode : Boolean;
    Function GetReadonly : Boolean;
    Function TextUnderCursor : String;
    Function GotoMethod(Value : String) : Integer;
    Function GotoMethodDeclaration(Value : String) : Integer;


    Procedure CreateEditor(AOwner : TComponent; AParent: TWinControl);
    Procedure CreateFormFromUnit;
  protected
    ToggleMenuItem : TMenuItem;
    Procedure DisplayControl;
    Procedure ReParent(AParent : TWinControl);

    Procedure BookMarkClicked(Sender : TObject);
    Procedure BookMarkGotoClicked(Sender : TObject);
    Procedure ReadOnlyClicked(Sender : TObject);
    Procedure ToggleBreakpointClicked(Sender : TObject);
    Procedure ToggleLineNumbersClicked(Sender : TObject);
    Procedure OpenAtCursorClicked(Sender : TObject);

    Procedure BookMarkToggle(Value : Integer);
    Procedure BookMarkGoto(Value : Integer);


    Procedure ProcessUserCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: char; Data: pointer);

    Procedure FocusEditor;  //called by TSourceNotebook whne the Notebook page changes so the editor is focused

    Procedure EditorStatusChanged(Sender: TObject; Changes: TSynStatusChanges);

    Function StartFind : Boolean;
    Function FindAgain(StartX,StartLine : Integer) : Boolean;

    property Editor : TmwCustomEdit read FEditor;
    property Visible : Boolean read FVisible write FVisible default False;
    FindText : String;
  public
    constructor Create(AOwner : TComponent; AParent : TWinControl);
    destructor Destroy; override;
    Procedure AddControlCode(_Control : TComponent);
    Procedure RemoveControlCode(_Control : TComponent);
    Procedure SelectText(LineNum,CharStart,LineNum2,CharEnd : Integer);
    Procedure CreateFormUnit(AForm : TCustomForm);
    Procedure CreateNewUnit;
    Function IsControlUnit : Boolean;
    Function Close : Boolean;
    Function Save : Boolean;
    Function Open : Boolean;

    property Control : TComponent read FControl write FControl;
    property CurrentCursorXLine : Integer read GetCurrentCursorXLine write SetCurrentCursorXLine;
    property CurrentCursorYLine : Integer read GetCurrentCursorYLine write SetCurrentCursorYLine;
    property Owner : TComponent read FAOwner;
    property Source : TStrings read GetSource write SetSource;
    property UnitName : String read FUnitName write FUnitname;
    property FileName : String read FFileName write FFilename;
    property Modified : Boolean read GetModified;
    property ReadOnly : Boolean read GetReadOnly;
    property InsertMode : Boolean read GetInsertmode;

    property OnAfterClose : TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnBeforeClose : TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnAfterOpen : TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnBeforeOpen : TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterSave : TNotifyEvent read FOnAfterSave write FOnAfterSave;
    property OnBeforeSave : TNotifyEvent read FOnBeforeSave write FOnBeforeSave;
    property OnEditorChange: TNotifyEvent read FOnEditorChange write FOnEditorChange;


  end;


  TSourceNotebook = class(TFORM)
  private
    Notebook1 : TNotebook;
    StatusBar : TStatusBar;
    FEmpty : Boolean;
    FFormEditor : TFormEditor;
    FSourceEditorList : TList;
    FSaveDialog : TSaveDialog;
    FOpenDialog : TOpenDialog;
    FOnOpenFile : TNotifyFileEvent;
    FOnCloseFile : TNotifyFileEvent;
    FOnSaveFile : TNotifyFileEvent;
    FMainIDE : TComponent;
    Function GetEmpty : Boolean;  //look at the # of pages
    Procedure NoteBookPageChanged(Sender : TObject);
  protected
    Function CreateNotebook : Boolean;
    Function GetActiveSE : TSourceEditor;
    Function DisplayPage(SE : TSourceEditor) : Boolean;
    Function NewSE(Pagenum : Integer) : TSourceEditor;
    Procedure EditorChanged(sender : TObject);
    procedure UpdateStatusBar;
    Bookmarks : TImageList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Function ActiveUnitName : String;
    Function ActiveFileName : String;
    Function CreateUnitFromForm(AForm : TForm) : TSourceEditor;
    Function GetSourceForUnit(UnitName : String) : TStrings;
    Function SetSourceForUnit(UnitName : String; NewSource : TStrings) : Boolean;

    Procedure DisplayFormforActivePage;
    Procedure DisplayCodeforControl(Control : TObject);
    Procedure DisplayCodefromUnitName(UnitName : String);

    procedure CloseClicked(Sender : TObject);
    Procedure NewClicked(Sender: TObject);
    procedure OpenClicked(Sender : TObject);
    procedure SaveClicked(Sender : TObject);
    procedure SaveAllClicked(Sender : TObject);
    procedure SaveAsClicked(Sender : TObject);

    procedure FindClicked(Sender : TObject);
    procedure FindAgainClicked(Sender : TObject);

    Procedure NewFile(UnitName: String; Source : TStrings; aVisible : Boolean);
    Procedure OpenFile(FileName: String; aVisible : Boolean);

    Procedure ToggleBookmark(Value : Integer);
    Procedure GotoBookmark(Value: Integer);


    property OnCloseFile : TNotifyFileEvent read FOnCloseFile write FOnCloseFile;
    property OnOpenFile : TNotifyFileEvent read FOnOPenFile write FOnOPenFile;
    property OnSaveFile : TNotifyFileEvent read FOnSaveFile write FOnSaveFile;
    property Empty : Boolean read GetEmpty;
    property FormEditor : TFormEditor read FFormEditor write FFormEditor;
    property MainIDE : TComponent read FMainIDE;
  end;


implementation
uses
  LCLLinux,TypInfo,LResources,Main,LazConf;

const
  ecFind = ecUserFirst+1;
  ecFindAgain = ecUserFirst+2;
  ecFindProcedureDefinition = ecUserFirst+3;
  ecFindProcedureMethod = ecUserFirst+4;

var
Editor_Num : Integer;
aHighlighter: TSynPasSyn;

{ TSourceEditor }

{The constructor for @link(TSourceEditor).  AOwner is the @link(TSOurceNotebook) and
 the AParent is usually a page of a @link(TNotebook)
}
constructor TSourceEditor.create(AOwner : TComponent; AParent : TWinControl);
Begin
  inherited Create;
  FAOwner := AOwner;

  FSource := TStringList.create;

  BuildPopupMenu;
  FControl := nil;
  CreateEditor(AOwner,AParent);
  FEditor.PopupMenu := FPopupMenu;

end;

destructor TSourceEditor.destroy;
begin
  FEditor.Free;
  FSource.free;
  inherited;
end;

Procedure TSourceEditor.BuildPopupMenu;

     Function Seperator : TMenuItem;
     Begin
     Result := TMenuItem.Create(FAOwner);
     Result.Caption := '-';
     end;

var
  MenuItem : TMenuItem;
  SubMenuItem : TMenuItem;
  I : Integer;


Begin
  FPopupMenu := TPopupMenu.Create(FAOwner);
  FPopupMenu.AutoPopup := True;

  MenuItem := TMenuItem.Create(FAOwner);
  MenuItem.Caption := '&Close Page';
  MenuItem.OnClick := @TSourceNotebook(FAOwner).CloseClicked;
  FPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FAOwner);
  MenuItem.Caption := '&Open file at cursor';
  MenuItem.OnClick := @OpenAtCursorClicked;
  FPopupMenu.Items.Add(MenuItem);

  FPopupMenu.Items.Add(Seperator);

  ToggleMenuItem := TMenuItem.Create(FAOwner);
  ToggleMenuItem.Caption := '&Toggle Bookmark';
  FPopupMenu.Items.Add(ToggleMenuItem);

  for I := 0 to 9 do
     Begin
      SubMenuItem := TMenuItem.Create(FAOwner);
      SubMenuItem.Caption := 'Bookmark '+inttostr(i);
      SubMenuItem.OnClick := @BookmarkClicked;
      SubMenuItem.Tag := I;
      ToggleMenuItem.Add(SubMenuItem);
     end;

  MenuItem := TMenuItem.Create(FAOwner);
  MenuItem.Caption := '&Goto Bookmark';
  FPopupMenu.Items.Add(MenuItem);

  for I := 0 to 9 do
     Begin
      SubMenuItem := TMenuItem.Create(FAOwner);
      SubMenuItem.Caption := 'Bookmark '+inttostr(i);
      SubMenuItem.OnClick := @BookmarkGotoClicked;
      SubMenuItem.Tag := I;
      MenuItem.Add(SubMenuItem);
     end;

  FPopupMenu.Items.Add(Seperator);

  MenuItem := TMenuItem.Create(FAOwner);
  MenuItem.Caption := 'Read Only';
  MenuItem.OnClick := @ReadOnlyClicked;
  FPopupMenu.Items.Add(MenuItem);

  FPopupMenu.Items.Add(Seperator);
  MenuItem := TMenuItem.Create(FAOwner);
  MenuItem.Caption := 'Debug';
  FPopupMenu.Items.Add(MenuItem);

      SubMenuItem := TMenuItem.Create(FAOwner);
      SubMenuItem.Caption := '&Toggle Breakpoint';
      SubMenuItem.OnClick := @ToggleBreakpointClicked;
      MenuItem.Add(SubMenuItem);

      SubMenuItem := TMenuItem.Create(FAOwner);
      SubMenuItem.Caption := '&Run to Cursor';
      //SubMenuItem.OnClick := @ToggleBreakpoint;
      MenuItem.Add(SubMenuItem);

  FPopupMenu.Items.Add(Seperator);
  MenuItem := TMenuItem.Create(FAOwner);
  MenuItem.Caption := 'Line Numbers';
  menuItem.OnClick := @ToggleLineNumbersClicked;
  FPopupMenu.Items.Add(MenuItem);

end;

{------------------------------G O T O   M E T H O D ---------------------------------}

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
            if (pos('procedure',Texts2) <> 0) or (pos('function',texts2) <> 0) then
                begin
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


{------------------------------G O T O   M E T H O D    D E C L A R A T I O N---------}
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


{------------------------------TEXT UNDER CURSOR----------------------------------------}
Function TSourceEditor.TextUnderCursor : String;
var
  Texts : String;
  EditorLine : String;
  X : Integer;
  Found : Boolean;
  SearchDir : String;
  AppDIr : String;
  TempDir : String;
  Num : Integer;

Begin
  //get the text by the cursor.
  EditorLine := FEditor.Lines.Strings[GetCurrentCursorYLine-1];
  X := GetCurrentCursorXLine-1;

  //walk backwards to a space or non-standard character.
  while (ord(upcase(EditorLine[x])) >= 65) and (ord(upcase(EditorLine[x])) <= 90) and (X>1) do
    dec(x);
  if (X > 1) then Inc(x);

  Texts := Copy(EditorLine,X,length(EditorLine));  //chop off the beginning

  X := 1;
  while (ord(upcase(Texts[x])) >= 65) and (ord(upcase(Texts[x])) <= 90) and (X< length(Texts)) do
   inc(x);

  if (X < Length(Texts) ) and (X >1)  then dec(x);

  if not(((ord(upcase(Texts[x])) >= 65) and (ord(upcase(Texts[x])) <= 90))) then dec(x);
  Texts := Copy(Texts,1,X);

  Result := Texts;
end;

{------------------------------Book Mark Clicked ---------------------------------}
Procedure TSourceEditor.BookMarkClicked(Sender : TObject);
var
  MenuItem : TMenuItem;
Begin
  MenuItem := TMenuItem(sender);
  BookMarkToggle(MenuItem.Tag);
end;

{------------------------------BOOK MARK GOTO CLICKED ---------------------------------}
Procedure TSourceEditor.BookMarkGotoClicked(Sender : TObject);
var
  MenuItem : TMenuItem;
Begin
  MenuItem := TMenuItem(sender);
  BookMarkGoto(MenuItem.Tag);
end;


{------------------------------BOOKMARK TOGGLE ---------------------------------}
Procedure TSourceEditor.BookMarkToggle(Value : Integer);
var
   MenuItem : TmenuItem;
Begin
  MenuItem := TmenuItem(ToggleMenuItem.Items[Value]);
  MenuItem.Checked := not(MenuItem.Checked);

  if MenuItem.Checked then
     Begin
        FEditor.SetBookMark(Value,GetCurrentCursorXLine,GetCurrentCursorYLine);
        MenuItem.Caption := MenuItem.Caption + '*';
     end
     else
     begin
     FEditor.ClearBookMark(Value);
     MenuItem.Caption := copy(MenuItem.Caption,1,Length(MenuItem.Caption)-1);
     end;


End;



{------------------------------BOOKMARK GOTO ---------------------------------}
Procedure TSourceEditor.BookMarkGoto(Value : Integer);
Begin
  FEditor.GotoBookMark(Value);
End;

Procedure TSourceEditor.OpenAtCursorClicked(Sender : TObject);
var
  Texts : String;
  Found : Boolean;
  SearchDir : String;
  AppDIr : String;
  TempDir : String;
  Num : Integer;
  DirDelimiter : Char;
Begin
  Texts := TextunderCursor;
  if Length(Texts) <= 1 then Exit;

  Found := False;
  //check the current directory
  Found := True;
  if FileExists(Lowercase(Texts)) then TSOurceNotebook(FAOwner).OpenFile(Lowercase(Texts), True)
     else
  if FileExists(Lowercase(Texts)+'.pp') then TSOurceNotebook(FAOwner).OpenFile(Lowercase(Texts)+'.pp', True)
     else
  if FileExists(Lowercase(Texts)+'.pas') then TSOurceNotebook(FAOwner).OpenFile(Lowercase(Texts)+'.pas', True)
     else
     Found := False;

// check the default LCL directory if not Found
     Found := True;
  AppDir := ExtractFilePath(Application.Exename);
  if FileExists(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)) then TSOurceNotebook(FAOwner).OpenFile(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts), True)
     else
  if FileExists(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pp') then TSOurceNotebook(FAOwner).OpenFile(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pp', True)
     else
  if FileExists(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pas') then TSOurceNotebook(FAOwner).OpenFile(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pas', True)
     else
     Found := False;


// if Not Found then
//Get the search directories
  DirDelimiter := '/';
  SearchDir := TMainIDE(TSourceNotebook(FAOwner).MainIDE).SearchPaths;
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
  if FileExists(TempDir+Lowercase(Texts)) then TSOurceNotebook(FAOwner).OpenFile(TempDir+Lowercase(Texts), True)
     else
  if FileExists(TempDir+Lowercase(Texts)+'.pp') then TSOurceNotebook(FAOwner).OpenFile(TempDir+Lowercase(Texts)+'.pp', True)
     else
  if FileExists(TempDir+Lowercase(Texts)+'.pas') then TSOurceNotebook(FAOwner).OpenFile(TempDir+Lowercase(Texts)+'.pas', True)
     else
     Found := False;
  Num := pos(';',SearchDir);
  end; //while

  If not Found then
     Application.MessageBox('File not found','Error',MB_OK);


end;

{--------------------------S T A R T  F I N D-----------------------}
Function TSourceEditor.StartFind : Boolean;
Begin
  Result := False;
  if not Assigned(FindDialog1) then
         FindDialog1 := TFindDialog.Create(nil);

  if (FindDialog1.ShowModal = mrOK) then
     Begin
        if not FindDialog1.cbCaseSensitive.Checked then
        FindText := uppercase(FindDialog1.edtTextToFind.Text);
        Result := FindAgain(1,0);

     end;

End;

{--------------------------F I N D  A G A I N -----------------------}
Function TSourceEditor.FindAgain(StartX,StartLine : Integer) : Boolean;
var
  I,X     : Integer;
  Texts   : String;
  TempLine : String;
  P        : TPoint;
Begin
Result := False;

if FindText = '' then Exit;

if (StartX < 0) or (StartLine < 0) then
    Begin
        StartX := CurrentCursorXLine-1;
        StartLine := CurrentCursorYLine-1;
    end;

  for I := StartLine to Source.Count-1 do
      begin
        TempLine := Copy(Source.Strings[i],StartX,Length(Source.Strings[i]));
        if not FindDialog1.cbCaseSensitive.Checked then
               TempLine := uppercase(TempLine);


        X := pos(FindText,Templine);
        if (X > 0) and (FindDialog1.cbWholeWords.Checked) then
           begin
              //check the character prior to X
              if X > 1 then
                 if  (TempLine[X-1] in ['a'..'z', 'A'..'Z', '0'..'9']) then
                     X := 0;
              if (X < Length(TempLine)) and (X > 0) then
                 if  (TempLine[X+1] in ['a'..'z', 'A'..'Z', '0'..'9']) then
                     X := 0;
            end;
         if (X > 0) then
             Begin
                 //re-position cursor and break;
                 P.X := X+Startx-1;
                 P.Y := I+1;
                 FEditor.BlockBegin:= P;
                 P.X := P.X+Length(FindText);
                 FEditor.BlockEND :=P;
                 FEditor.CaretXY := p;
                 Result := True;
                 Exit;
             end;
          StartX := 1;

           end;
End;



Procedure TSourceEditor.ToggleLineNumbersClicked(Sender : TObject);
var
  MenuITem : TMenuItem;
begin
  MenuItem := TMenuITem(Sender);
  MenuItem.Checked := not(MenuItem.Checked);
  FEditor.Gutter.ShowLineNumbers := MenuItem.Checked;
End;


Procedure TSourceEditor.ReadOnlyClicked(Sender : TObject);
var
  MenuItem : TMenuItem;
Begin
  MenuItem := TMenuItem(sender);
  FEditor.ReadOnly := not(FEditor.ReadOnly);
//set the statusbar text;
end;

Procedure TSourceEditor.FocusEditor;
Begin
  FEditor.SetFocus;
end;


Function TSourceEditor.GetReadOnly : Boolean;
Begin
  Result :=  FEditor.ReadOnly;
End;

Procedure TSourceEditor.ToggleBreakpointClicked(Sender : TObject);
Begin

end;

Procedure TSourceEditor.ProcessUserCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: char; Data: pointer);
var
  Y,I : Integer;
  Texts,Texts2,TheName : String;
Begin
Writeln('[ProcessUserCommand]  --------------');
  case Command of
    ecFind : Begin
               FindText := '';
               StartFind;
             end;

    ecFindAgain : Begin
                   if FindText = '' then
                      StartFind
                   else
                      if not(FindAgain(CurrentCursorXLine-1,CurrentCursorYLine-1)) then
                         if Application.MessageBox('Search String not found.  Start from the beginning?','Not Found',mb_YesNo) = mrYEs then
                            FindAgain(1,0);
                  end;

    ecFindProcedureMethod :  Begin
                                    //jump down to the procedure definition
                                    Texts := TextUnderCursor;  //this should be a procedure name.
                                    GotoMethod(Texts);
                                 end;

    ecFindProcedureDefinition :   Begin
    Y := CurrentCursorYLine;
    Texts2 := Lowercase(Source.Strings[Y-1]);
    Writeln('The source line = '+Texts2);
    I := pos('function',Texts2);
    if I = 0 then
    I := pos('procedure',Texts2);
    While (I = 0) and (Y > 0) do
    begin
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

   end;  //case

end;


Procedure TSourceEditor.EditorStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
Begin
  If Assigned(OnEditorChange) then
     OnEditorChange(sender);

end;

Procedure TSourceEditor.CreateEditor(AOwner : TComponent; AParent: TWinControl);
Begin
if assigned(FEditor) then
   Begin
      FSource.Assign(FEditor.Lines);
      FEditor.Free;
   end;

{SynEdit}

    FSynAutoComplete:=TSynAutoComplete.Create(FAOwner);
    with FSynAutoComplete do begin
      if FileExists(SetDirSeparators(GetPrimaryConfigPath+'/lazarus.dci')) then
      AutoCompleteList.LoadFromFile(SetDirSeparators(GetPrimaryConfigPath+'/lazarus.dci'))
	else
      AutoCompleteList.LoadFromFile('lazarus.dci');
    end;

    FEditor:=TSynEdit.Create(FAOwner);
    with FEditor do
    begin
    Name:='SynEdit'+Inttostr(Editor_num);
    inc(Editor_num);
    Parent := AParent;
    SetBounds(0,25,TWinControl(FAOwner).ClientWidth - 10,TWinControl(FAOwner).ClientHeight -10);
    Align := alClient;
    Highlighter:=aHighlighter;
    Gutter.Color:=clBlue;
    AddKey(ecAutoCompletion, word('J'), [ssCtrl], 0, []);
    AddKey(ecFind, word('F'), [ssCtrl], 0, []);
    AddKey(ecFindAgain, VK_F3, [], 0, []);
    AddKey(ecFindProcedureDefinition, VK_UP, [ssShift,ssCtrl], 0, []);
    AddKey(ecFindProcedureMethod, VK_Down, [ssShift,ssCtrl], 0, []);

    OnStatusChange := @EditorStatusChanged;
    OnProcessUserCommand := @ProcessUserCommand;
    Show;
    end;
    FSynAutoComplete.AddEditor(FEditor);
{SynEdit}

FEditor.Lines.Assign(FSource);

end;

Procedure TSourceEditor.AddControlCode(_Control : TComponent);
var
  PT : PTypeData;
  PI : PTypeInfo;
  nmControlType : String;
  I : Integer;
  NewSource : String;
  TempSource : TStringList;
  Ancestor : String;
begin
  TempSource := TStringList.Create;
  TempSource.Assign(Source);

  //get the control name
  PI := _Control.ClassInfo;
  nmControlType := PI^.Name;
  Ancestor := GetAncestor;
  Ancestor := 'TFORM';

//find the place in the code to add this now.
//Anyone have good method sfor parsing the source to find spots like this?
//here I look for the Name of the customform, the word "Class", and it's ancestor on the same line
//not very good because it could be a comment or just a description of the class.
//but for now I'll use it.
For I := 0 to TempSource.Count-1 do
    begin
    Writeln('Ancestor is '+Ancestor);
    Writeln('TWinControl(_Control.Owner).Name is '+TWinControl(_Control.Owner).Name);
    Writeln('Line is '+TempSource.Strings[i]);
    if (pos(Ancestor,TempSource.Strings[i]) <> 0) and (pos(lowercase(TWinControl(_Control.Owner).Name),lowercase(TempSource.Strings[i])) <> 0) and (pos('CLASS',Uppercase(TempSource.Strings[i])) <> 0) then
        Break;
    end;


  //if I => FSource.Count then I didn't find the line...
  If I < TempSource.Count-1 then
     Begin
       //alphabetical
       inc(i);
       NewSource := _Control.Name+' : '+nmControlType+';';

       //  Here I decide if I need to try and insert the control's text code in any certain order.
       //if there's no controls then I just insert it, otherwise...
       if TWincontrol(_Control.Owner).ControlCount > 0 then
       while NewSource > (trim(TempSource.Strings[i])) do
         inc(i);

          TempSource.Insert(i,'       '+NewSource);
     end;


Source := TempSource;
end;


{Called when a control is deleted from the form}
Procedure TSourceEditor.RemoveControlCode(_Control : TComponent);
var
  PT : PTypeData;
  PI : PTypeInfo;
  nmControlType : String;
  I : Integer;
  NewSource : String;
  TempSource : TStringList;
  Ancestor : String;
begin
  TempSource := TStringList.Create;
  TempSource.Assign(Source);

  //get the control name
  PI := _Control.ClassInfo;
  nmControlType := _Control.name;
  Ancestor := GetAncestor;

//find the place in the code to start looking for it

For I := 0 to TempSource.Count-1 do
    if (pos(Ancestor,TempSource.Strings[i]) <> 0) and (pos(TWinControl(_Control.Owner).Name,TempSource.Strings[i]) <> 0) and (pos('CLASS',Uppercase(TempSource.Strings[i])) <> 0) then
        Break;

  //if I => FSource.Count then I didn't find the line...
  If I < TempSource.Count then
     Begin
       //alphabetical
       inc(i);
       NewSource := _Control.Name+' : '+nmControlType+';';

       while NewSource < (trim(TempSource.Strings[i])) do
         inc(i);

         If NewSource = (trim(TempSource.Strings[i])) then
             TempSource.Delete(I);
     end;
Source := TempSource;
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
  //return mwedit's source.
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

Function TSourceEditor.GetInsertMode : Boolean;
Begin
  Result := FEditor.Insertmode;
end;


//Get's the ancestor of the FControl.
//For example the ancestor of a TForm1 = class(xxxx) is the xxxx
Function TSourceEditor.GetAncestor : String;
var
  PI : PTypeInfo;
begin
  PI := FControl.ClassInfo;
  Result := PI^.Name;
  Delete(Result,1,1);
end;


Procedure TSourceEditor.CreateFormUnit(AForm  : TCustomForm);
Var
  I : Integer;
  nmForm : String;
  nmAncestor : String;
  TempSource : TStringList;
Begin
  FControl := AForm;
  TempSource := TStringList.Create;

  nmAncestor := GetAncestor;

  nmForm := FControl.Name;

  with TempSource do
   try
     Add(Format('unit %s;', [FUnitName]));
     Add('');
     Add('interface');
     Add('');
     Add('uses Classes, Graphics, Controls, Forms, Dialogs;');
     Add('');
     Add('type');
     Add(Format('     T%s = class(T%s)', [nmForm,'FORM']));
     Add('     private');
     Add('     { private declarations }');
     Add('     public');
     Add('     { public declarations }');
     Add('     end;');
     Add('');
     Add('var');
     Add(Format('     %s: T%0:s;', [nmForm]));
     Add('');
     Add('implementation');
     Add('');
     Add('end.');
   except
   //raise an exception
   end;
  Source := TempSource;
tempSource.Free;
end;

{____________________________________________}
{                 CREATEFORMFROMUNIT         }
{This method checks to see if the loaded unit is a form unit.
 If so, it creates the form                                  }
Procedure TSourceEditor.CreateFormFromUnit;
Begin
end;


Procedure TSourceEditor.CreateNewUnit;
Var
  I : Integer;
  TempSource : TStringList;
Begin
  TempSource := TStringList.Create;


//figure out what the unit name should be...
//  FUnitName:='Unit1';  //just assigning it to this for now

  with TempSource do
   try
     Add(Format('unit %s;', [FUnitName]));
     Add('');
     Add('interface');
     Add('');
     Add('implementation');
     Add('');
     Add('end.');
   except
   //raise an exception
   end;
  Source := TempSource;
tempSource.Free;
End;



Function TSourceEditor.Close : Boolean;
Begin
  Result := True;
  If Assigned(FOnBeforeClose) then
     Begin
      FOnBeforeClose(Self);
     end;

//  FSource.Clear;
  Visible := False;
  If Assigned(FOnAfterClose) then FOnAfterClose(Self);
end;

Function TSourceEditor.Open : Boolean;
Begin
  Writeln('[TSourceEditor] Open');
  Result := True;
  If Assigned(FOnBeforeOpen) then FOnBeforeOpen(Self);

  try
    FEditor.Lines.LoadFromFile(FileName);
    FModified := False;
    FUnitName := ExtractFileName(Filename);

    //see if this is a form file
    CreateFormfromUnit;
  except
    Result := False;
  end;

  if Result then
     If Assigned(FOnAfterOpen) then FOnAfterOpen(Self);
  Writeln('[TSourceEditor] Open Done');
end;


Function TSourceEditor.Save : Boolean;
Begin
  Result := True;
  If Assigned(FOnBeforeSave) then FOnBeforeSave(Self);

  try
    FEditor.Lines.SaveToFile(FileName);
    FModified := False;
  except
    Result := False;
  end;

  If Assigned(FOnAfterSave) then FOnAfterSave(Self);
end;

Procedure TSourceEditor.ReParent(AParent : TWInControl);
Begin
CreateEditor(FAOwner,AParent);
End;


Function TSourceEditor.IsControlUnit : Boolean;
Begin
Result := (FControl <> nil);
end;



{------------------------------------------------------------------------}
                      { TSourceNotebook }

constructor TSourceNotebook.Create(AOwner: TComponent);


function LoadResource(ResourceName:string; PixMap:TPixMap):boolean;
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


var
Pixmap1 : TPixmap;
I : Integer;
begin
  inherited Create(AOwner);
  Caption := 'Lazarus Editor';
  Left := 0;
  Top := 0;
  Width := 600;
  height := 600;
  FMainIDE := AOwner;

  FSourceEditorList := TList.Create;
  FSaveDialog := TSaveDialog.Create(Self);
  FOpenDialog := TOpenDialog.Create(Self);

  Bookmarks := TImageList.Create(AOwner);

  //load 10 images
  for I := 0 to 9 do
  Begin
  Pixmap1:=TPixMap.Create;
  Pixmap1.TransparentColor:=clBtnFace;

    if not LoadResource('bookmark'+inttostr(i),Pixmap1) then
           LoadResource('default',Pixmap1);
    Bookmarks.Add(Pixmap1,nil);
  end;

  aHighlighter:=TSynPasSyn.Create(AOwner);
    with aHighlighter do begin
      CommentAttri.ForeGround:=clBlue;
      CommentAttri.Style:=[fsBold,fsItalic];
      AsmAttri.ForeGround:=clGreen;
      IdentifierAttri.Style:=[fsBold];
      NumberAttri.ForeGround:=clGreen;
      //SpaceAttri.Background:=clWhite;
      StringAttri.ForeGround:=clRed;
      SymbolAttri.ForeGround:=clBlack;
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
       Panels.Add;       //Unitname
       Panels.Add;       //OVR/INS
       Panels[0].Text := '';
       Panels[0].Width := 100;
       Panels[0].Bevel := pbLowered;
       Panels[1].Text := '';
       Panels[1].Bevel := pbLowered;
       Panels[1].Width := 150;
       Panels[2].Text := '';
       Panels[2].Bevel := pbLowered;
       Panels[2].Width := 100;
       Panels[3].Text := 'INS';
       Panels[3].Bevel := pbLowered;
       Panels[3].Width := 50;
       SimplePanel := False;
      end;


end;

destructor TSourceNotebook.Destroy;
begin

  FSourceEditorList.Free;
  aHighlighter.Free;
  inherited Destroy;
end;

Function TSourceNotebook.CreateNotebook : Boolean;
Begin
  Writeln('TSourceNotebook] CreateNotebook');
  Result := False;
  if not assigned(Notebook1) then
     Begin
     Result := True;
     Notebook1 := TNotebook.Create(self);
     with Notebook1 do
          Begin
            Parent := Self;
            Align := alClient;
	    Left := 0;
            Top :=2;
            Width := ClientWidth;
            Height := ClientHeight-Notebook1.top;
            Pages.Strings[0] := 'Unit1';
            PageIndex := 0;   // Set it to the first page
            OnPageChanged := @NoteBookPageChanged;
            Show;
          end; //with
      Show;  //used to display the code form

      end;

  Writeln('TSourceNotebook] CreateNotebook done');
End;

Function TSourceNotebook.CreateUnitFromForm(AForm : TForm): TSourceEditor;
Var
  TempSourceEditor : TSourceEditor;
  Notebook_Just_Created : Boolean;
  PageIndex : Integer;
begin

  Notebook_Just_Created := (not assigned(Notebook1)) or
                           (Notebook1.Pages.Count = 0);

  if Notebook_Just_Created then
  TempSourceEditor := NewSe(0)
  else
  tempSourceEditor := NewSe(-1);

  TempSourceEditor.CreateFormUnit(AForm);

  Notebook1.Pages.Strings[Notebook1.PageIndex] := TempSourceEditor.Unitname;

  Result := TempSourceEditor;
  Show;
end;

Procedure TSourceNotebook.EditorChanged(sender : TObject);
Begin
Writeln('EditorChanged');
  UpdateStatusBar;
Writeln('EditorChanged done');
End;

Function TSourceNotebook.NewSe(PageNum : Integer) : TSourceEditor;
var
  UnitIndex,I:integer;

Begin
UnitIndex := 0;
 if CreateNotebook then Pagenum := 0;

  if Pagenum = -1 then begin //add a new page
    repeat
      inc(UnitIndex);
      I:=FSourceEditorList.Count-1;
      while (I>=0)
      and (lowercase(TSourceEditor(FSourceEditorList[I]).UnitName)
          <>'unit'+IntToStr(UnitIndex)) do dec(I);
    until I<0;
    Pagenum := Notebook1.Pages.Add('Unit'+IntToStr(UnitIndex));

  end;
  Result := TSourceEditor.Create(Self,Notebook1.Page[PageNum]);
  Result.FUnitName:=Notebook1.Pages[PageNum];
  Notebook1.Pageindex := Pagenum;
  FSourceEditorList.Add(Result);
  Result.Editor.BookMarkOptions.BookmarkImages := Bookmarks;
  Result.OnEditorChange := @EditorChanged;
end;


Procedure TSourceNotebook.DisplayCodeforControl(Control : TObject);
Var
   I,X : Integer;
Begin
   X := FSourceEditorList.Count;
   if X = 0 then Exit;
   I := 0;
   while  (I < X) and (TSourceEditor(FSourceEditorList.Items[I]).Control <> TComponent(Control)) do
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
   while  (I < X) and (Uppercase(TSourceEditor(FSourceEditorList.Items[I]).Unitname) <> Uppercase(Unitname)) do
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


    for X := 0 to Notebook1.Pages.Count-1 do
        Begin
          With Notebook1.Page[X] do
          for I := 0 to ControlCount-1 do
               if Controls[I] is TmwCustomEdit then
                  Begin
                     TempEditor := Controls[I];
                     Break;
                  end;
          if SE.Editor = TempEditor then Begin
              Writeln('The editor was found on page '+inttostr(x));
              Break;
              end;
        End;


    if SE.Editor = TempEditor then
       Begin
          Notebook1.PageIndex := X;
         //Bringtofront does not work yet.
         //Notebook1.BringToFront;
         //so I hide it and unhide it.
         Visible := False;
         Visible := True;

       end
       else
       Begin  //the SE isn't on a page so we need to create a page for it.
       Notebook1.PageIndex := Notebook1.Pages.Add(SE.UnitName);
       SE.ReParent(Notebook1.Page[Notebook1.Pageindex]);
       end;




end;


Function TSourceNotebook.GetActiveSE : TSourceEditor;
Var
   I,X : Integer;
   TempEditor : TControl;
Begin
   Result := nil;
   X := FSourceEditorList.Count;
   if X = 0 then Exit;

   with Notebook1.Page[Notebook1.Pageindex] do
        Begin
          if ControlCount = 0 then Exit;
          for I := 0 to ControlCount-1 do
               if Controls[I] is TmwCustomEdit then
                  Begin
                     TempEditor := Controls[I];
                     Break;
                  end;
        End;

//TempEditor now is the editor on the active page
//Compare it to the editor help by the SourceEditors
   I := 0;
   while TSourceEditor(FSourceEditorList[I]).Editor <> TempEditor do
         inc(i);

   Result := TSourceEditor(FSourceEditorList[i]);
end;


Function TSourceNotebook.GetEmpty : Boolean;
Begin
Result := (not assigned(Notebook1)) or (Notebook1.Pages.Count = 0);
end;

Procedure TSourceNotebook.OpenClicked(Sender: TObject);
Var
    TempEditor : TSourceEditor;
Begin
   if (sender is TMenuItem) and (TMenuItem(sender).name <> 'FileOpen') then  //the down arrow next to open was selected
       OpenFile(TMenuItem(sender).Caption,True)
   else
   Begin
     FOpenDialog.Title := 'Open';
     if FOpenDialog.Execute then  Begin
        //create a new page
        Writeln('create a new editor');
        TempEditor := NewSE(-1);
        Writeln('Done create a new editor');
        TempEditor.Filename := FOpenDialog.Filename;
        if (TempEditor.Open) then
        Begin
           Writeln('1');
           if assigned(FOnOpenFile) then FOnOpenFile(TObject(TempEditor),FOpenDialog.Filename);
           Writeln('2');
           Notebook1.Pages.Strings[Notebook1.Pageindex] := TempEditor.UnitName;
           Writeln('3');
        end;
      end;
   end;
  TempEditor.Visible := True;
UpdateStatusBar;
end;

Procedure TSourceNotebook.FindClicked(Sender : TObject);
Begin
  if GetActiveSe <> nil then
     GetActiveSE.StartFind;
End;

Procedure TSourceNotebook.FindAgainClicked(Sender : TObject);
Begin
  if GetActiveSe <> nil then
     GetActiveSE.FindAgain(-1,-1);  //-1 uses the currect x and y coords of the cursor in the window
End;




{This is called from outside to toggle a bookmark of the active TSourceEditor}
Procedure TSourceNotebook.ToggleBookmark(Value : Integer);
Begin
   GetActiveSE.BookMarkToggle(Value);
End;


{This is called from outside to Goto a bookmark on the active TSourceEditor}
Procedure TSourceNotebook.GoToBookmark(Value: Integer);
Begin
   GetActiveSE.BookMarkGoto(Value);
End;

Procedure TSourceNotebook.NewFile(UnitName: String; Source : TStrings; aVisible : Boolean);
Var
    TempEditor : TSourceEditor;
Begin
      //create a new page
      TempEditor := NewSE(-1);
      TempEditor.Unitname := Unitname;
      TempEditor.Source := Source;
      if Visible then Notebook1.Pages.Strings[Notebook1.Pageindex] := TempEditor.UnitName;
      TempEditor.Visible := aVisible;
end;

Procedure TSourceNotebook.OpenFile(FileName: String; aVisible : Boolean);
Var
    TempEditor : TSourceEditor;
Begin
      if FileExists(Filename) then
      begin
      //create a new page
      TempEditor := NewSE(-1);
      TempEditor.Filename := Filename;

      if (TempEditor.OPen) then
        Begin
           if assigned(FOnOPenFile) then FOnOpenFile(TObject(TempEditor),FOpenDialog.Filename);
           if Visible then Notebook1.Pages.Strings[Notebook1.Pageindex] := ExtractFileName(TempEditor.UnitName);
           TempEditor.Visible := aVisible;
        end;

      end;

end;

Procedure TSourceNotebook.NewClicked(Sender: TObject);
Var
    TempEditor : TSourceEditor;
Begin
      //create a new page
      TempEditor := NewSE(-1);
      TempEditor.CreateNewUnit;
      Notebook1.Pages.Strings[Notebook1.Pageindex] := TempEditor.UnitName;
      TempEditor.Visible := True;
      UpdateStatusBar;

End;

Procedure TSourceNotebook.SaveClicked(Sender: TObject);
Begin
if ActiveFileName <> '' then
   begin
   if (GetActiveSE.Save) then
   if assigned(FOnSaveFile) then FOnSaveFile(TObject(GetActiveSE),ActiveFilename)
   end
else
SaveAsClicked(Sender);

UpdateStatusBar;

end;

Function TSourceNotebook.ActiveUnitName : String;
Begin
Result := GetActiveSE.UnitName;
end;

Function TSourceNotebook.ActiveFileName : String;
Begin
Result := GetActiveSE.FileName;
end;


Procedure TSourceNotebook.CloseClicked(Sender : TObject);
Begin
if (GetActiveSE.Modified) then
    If Application.MessageBox('Source has changed.  Save now?','Warning',mb_YesNo) = mrYes then
       SaveClicked(Sender);

    if (GetActiveSE.Close) then
    if assigned(FOnCloseFile) then FOnCloseFile(self,ActiveFileName);

    Notebook1.Pages.Delete(Notebook1.Pageindex);


if Notebook1.Pages.Count = 0 then
        Hide;

UpdateStatusBar;

end;

Procedure TSourceNotebook.SaveAsClicked(Sender : TObject);
Begin
  FSaveDialog.Title := 'Save '+ActiveUnitName+' as :';
  if ActiveFileName <> '' then
     FSaveDialog.Filename := ActiveFileName
     else
     FSaveDialog.Filename := ActiveUnitName+'.pp';


  if FSaveDialog.Execute then
  begin
    GetActiveSe.FileName := FSaveDialog.Filename;
    if (GetActiveSE.Save) then
       if assigned(FOnSaveFile) then FOnSaveFile(TObject(GetActiveSE),ActiveFilename);
  end
  else
    Exit;

end;

Procedure TSourceNotebook.SaveAllClicked(Sender : TObject);
Var
   I,X : Integer;
   TempEditor : TSourceEditor;
Begin
   For I := 0 to  FSourceEditorList.Count-1 do
       Begin
        TempEditor := TSourceEditor(FSourceEditorList.Items[i]);
        if TempEditor.Visible then
        Begin
        FSaveDialog.Title := 'Save '+TempEditor.UnitName+' as :';
        if TempEditor.FileName <> '' then
           FSaveDialog.Filename := TempEditor.FileName
           else
           FSaveDialog.Filename := TempEditor.UnitName+'.pp';

        if FSaveDialog.Execute then
           begin
           TempEditor.FileName := FSaveDialog.Filename;
           if (TempEditor.Save) then
                if assigned(FOnSaveFile) then FOnSaveFile(TObject(TempEditor),TempEditor.FileName);
           end
           else
           Break;
        end;
        end;
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

Function TSourceNotebook.SetSourceForUnit(UnitName : String; NewSource : TStrings) : Boolean;
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
   TempEditor := GetActiveSE;
   if TempEditor = nil then Exit;
   Writeln('Updating status bar...');

   Statusbar.Panels[2].Text := GetActiveSE.Unitname;

   If GetActiveSE.Modified then StatusBar.Panels[1].Text := 'Modified'
   else
   StatusBar.Panels[1].Text := '';

   If GetActiveSE.ReadOnly then
      if StatusBar.Panels[1].Text <> '' then  StatusBar.Panels[1].Text := StatusBar.Panels[1].Text + '/ReadOnly'
         else
         StatusBar.Panels[1].Text := 'Readonly';


   Statusbar.Panels[0].Text := Inttostr(GetActiveSE.CurrentCursorXLine) + ','+ Inttostr(GetActiveSE.CurrentCursorYLine);

   if GetActiveSE.InsertMode then
     Statusbar.Panels[3].Text := 'INS' else
     Statusbar.Panels[3].Text := 'OVR';
End;

Procedure TSourceNotebook.NoteBookPageChanged(Sender : TObject);
Begin
  if GetActiveSE <> nil then
  begin
    GetActiveSE.FocusEditor;
    UpdateStatusBar;
  end;
end;

initialization
Editor_Num := 0;


{$I designer/bookmark.lrs}

end.
