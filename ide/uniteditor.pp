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
//{$DEFINE NEW_EDITOR}
//{$DEFINE NEW_EDITOR_SYNEDIT}
unit UnitEditor;

{$mode objfpc}

interface

uses
  classes, Controls, forms,buttons,sysutils,Dialogs,
{$ifdef NEW_EDITOR_SYNEDIT}
  synedit,SysHighlighterpas,
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


  TSourceEditor = class
  private
    FAOwner : TComponent; //Owned by a TSourceNotebook
{$ifdef NEW_EDITOR_SYNEDIT}
    FHighlighter: TSynPasSyn;
    FEditor     : TSynEditor
{$else}
    FHighlighter: TmwPasSyn;
    FEditor     : TmwCustomEdit;
{$endif}
    FControl: TComponent;  //if this is a Form or Datamodule, this is used

    //pulled out of the editor by the Function Getxxx
    FCurrentCursorXLine : Integer;
    //pulled out of the editor by the Function Getxxx
    FCurrentCursorYLine : Integer;
    FFileName : String;
    FModified : Boolean;

    FPopUpMenu : TPopupMenu;

    //pulled out of the editor by getting it's TStrings
    FSource : TStringList;
    FUnitName : String;

    FOnAfterClose : TNotifyEvent;
    FOnAfterOpen : TNotifyEvent;
    FOnAfterSave : TNotifyEvent;
    FOnBeforeClose : TNotifyEvent;
    FOnBeforeOpen : TNotifyEvent;
    FOnBeforeSave : TNotifyEvent;

    Procedure BuildPopupMenu;
    Function GetSource : TStrings;
    Procedure SetSource(value : TStrings);
    Function GetCurrentCursorXLine : Integer;
    Procedure SetCurrentCursorXLine(num : Integer);
    Function GetCurrentCursorYLine : Integer;
    Procedure SetCurrentCursorYLine(num : Integer);
    Function GetAncestor : String;
    Function GetModified : Boolean;
    Function TextUnderCursor : String;
    Function GotoMethod(Value : String) : Integer;
    Function GotoMethodDeclaration(Value : String) : Integer;

    Procedure CreateEditor(AOwner : TComponent; AParent: TWinControl);

  protected
    ToggleMenuItem : TMenuItem;
    Procedure DisplayControl;
    Procedure ReParent(AParent : TWinControl);

    Procedure BookMarkClicked(Sender : TObject);
    Procedure BookMarkGotoClicked(Sender : TObject);
    Procedure ReadOnlyClicked(Sender : TObject);
    Procedure ToggleBreakpointClicked(Sender : TObject);
    Procedure OpenAtCursorClicked(Sender : TObject);

    Procedure BookMarkToggle(Value : Integer);
    Procedure BookMarkGoto(Value : Integer);
    Procedure EditorKeyDown(Sender : TObject; var Key: Word; Shift : TShiftState);
    Procedure EditorKeyUp(Sender : TObject; var Key: Word; Shift : TShiftState);

    property Editor : TmwCustomEdit read FEditor;

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

    property Control : TComponent read FControl;
    property CurrentCursorXLine : Integer read GetCurrentCursorXLine write SetCurrentCursorXLine;
    property CurrentCursorYLine : Integer read GetCurrentCursorYLine write SetCurrentCursorYLine;
    property Owner : TComponent read FAOwner;
    property Source : TStrings read GetSource write SetSource;
    property UnitName : String read FUnitName;
    property FileName : String read FFileName write FFilename;
    property Modified : Boolean read GetModified;

    property OnAfterClose : TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnBeforeClose : TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnAfterOpen : TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnBeforeOpen : TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterSave : TNotifyEvent read FOnAfterSave write FOnAfterSave;
    property OnBeforeSave : TNotifyEvent read FOnBeforeSave write FOnBeforeSave;
  end;


  TSourceNotebook = class(TFORM)
  private
    Notebook1 : TNotebook;
    FEmpty : Boolean;
    FSourceEditorList : TList;
    FSaveDialog : TSaveDialog;
    FOpenDialog : TOpenDialog;
    FOnOpenFile : TNotifyFileEvent;
    FOnCloseFile : TNotifyFileEvent;
    FOnSaveFile : TNotifyFileEvent;
    FMainIDE : TComponent;
    Function GetEmpty : Boolean;  //look at the # of pages
  protected
    Function CreateNotebook : Boolean;
    Function GetActiveSE : TSourceEditor;
    Function DisplayPage(SE : TSourceEditor) : Boolean;
    Function NewSE(Pagenum : Integer) : TSourceEditor;
    Bookmarks : TImageList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Function ActiveUnitName : String;
    Function ActiveFileName : String;
    Procedure DisplayFormforActivePage;
    Procedure DisplayCodeforControl(Control : TObject);
    Function CreateUnitFromForm(AForm : TForm) : TSourceEditor;

    procedure CloseClicked(Sender : TObject);
    Procedure NewClicked(Sender: TObject);
    procedure OpenClicked(Sender : TObject);
    procedure SaveClicked(Sender : TObject);
    procedure SaveAllClicked(Sender : TObject);
    procedure SaveAsClicked(Sender : TObject);

    Procedure OpenFile(FileName: String);

    Procedure ToggleBookmark(Value : Integer);
    Procedure GoToBookmark(Value: Integer);

    property OnCloseFile : TNotifyFileEvent read FOnCloseFile write FOnCloseFile;
    property OnOpenFile : TNotifyFileEvent read FOnOPenFile write FOnOPenFile;
    property OnSaveFile : TNotifyFileEvent read FOnSaveFile write FOnSaveFile;
    property Empty : Boolean read GetEmpty;
    property MainIDE : TComponent read FMainIDE;
  end;


implementation
uses
  LCLLinux,TypInfo,LResources,Main;


{ TSourceEditor }


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
  FHighlighter.free;
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
  if FileExists(Lowercase(Texts)) then TSOurceNotebook(FAOwner).OpenFile(Lowercase(Texts))
     else
  if FileExists(Lowercase(Texts)+'.pp') then TSOurceNotebook(FAOwner).OpenFile(Lowercase(Texts)+'.pp')
     else
  if FileExists(Lowercase(Texts)+'.pas') then TSOurceNotebook(FAOwner).OpenFile(Lowercase(Texts)+'.pas')
     else
     Found := False;

// check the default LCL directory if not Found
     Found := True;
  AppDir := ExtractFilePath(Application.Exename);
  if FileExists(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)) then TSOurceNotebook(FAOwner).OpenFile(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts))
     else
  if FileExists(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pp') then TSOurceNotebook(FAOwner).OpenFile(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pp')
     else
  if FileExists(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pas') then TSOurceNotebook(FAOwner).OpenFile(AppDir+'lcl'+AppDir[Length(AppDir)]+Lowercase(Texts)+'.pas')
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
  if FileExists(TempDir+Lowercase(Texts)) then TSOurceNotebook(FAOwner).OpenFile(TempDir+Lowercase(Texts))
     else
  if FileExists(TempDir+Lowercase(Texts)+'.pp') then TSOurceNotebook(FAOwner).OpenFile(TempDir+Lowercase(Texts)+'.pp')
     else
  if FileExists(TempDir+Lowercase(Texts)+'.pas') then TSOurceNotebook(FAOwner).OpenFile(TempDir+Lowercase(Texts)+'.pas')
     else
     Found := False;
  Num := pos(';',SearchDir);
  end; //while

  If not Found then
     Application.MessageBox('File not found','Error',MB_OK);


end;


Procedure TSourceEditor.ReadOnlyClicked(Sender : TObject);
var
  MenuItem : TMenuItem;
Begin
  MenuItem := TMenuItem(sender);
  FEditor.ReadOnly := not(FEditor.ReadOnly);
//set the statusbar text;
end;


Procedure TSourceEditor.ToggleBreakpointClicked(Sender : TObject);
Begin

end;


Procedure TSourceEditor.EditorKeyDown(Sender : TObject; var Key: Word; Shift : TShiftState);
var
  Texts,nmForm : String;
  Texts2 : String;
  I,Y : Integer;
  TheName : String;
Begin
  if (Key = 40) and ( ssCTRL in Shift) then
     Begin
     //jump down to the procedure definition
     Texts := TextUnderCursor;  //this should be a procedure name.
     GotoMethod(Texts);

     end
  else
  if (Key = 38) and (ssCTRL in Shift) and (ssShift in Shift)then
     Begin
     //jump up to the procedure definition
    //move up until you find the work PROCEDURE or FUNCTION
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


end;

Procedure TSourceEditor.EditorKeyUp(Sender : TObject; var Key: Word; Shift : TShiftState);
Begin

end;


Procedure TSourceEditor.CreateEditor(AOwner : TComponent; AParent: TWinControl);
Begin
if assigned(FEditor) then
   Begin
      FSource.Assign(FEditor.Lines);
      FEditor.Free;
   end;

FEditor := TmwCustomEdit.Create(FAOwner);
  with FEditor do
  begin
    Parent := AParent;
    Top := 25;
    Left := 0;
    Width := TWinControl(FAOwner).ClientWidth - 10;//clientwidth;//500;
    Height :=TWinControl(FAOwner).ClientHeight -10;//clientheight;//250;
    Align := alClient;
    {$IFDEF NEW_EDITOR}
    Gutter.Color := clBtnface;
    Gutter.ShowLineNumbers := True;
   {$ELSE}
    GutterColor := clBtnface;
    {$ENDIF}
    Color := clWindow;
    Visible := True;
    Font.Name := 'courier';
    Font.Size := 12;
    if FHighlighter = nil
    then begin
            FHighlighter := TmwPasSyn.Create(FAOwner);
              with TmwPasSyn(FHighLighter) do
                   begin
                     CommentAttri.Foreground := clNavy;
                     NumberAttri.Foreground := clRed;
                     KeyAttri.Foreground := clGreen;
                   end;
         end;
      OnKeyDown := @EditorKeyDown;
      OnKeyUp := @EditorKeyUp;
  end;
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

//find the place in the code to add this now.
//Anyone have good method sfor parsing the source to find spots like this?
//here I look for the Name of the customform, the word "Class", and it's ancestor on the same line
//not very good because it could be a comment or just a description of the class.
//but for now I'll use it.
For I := 0 to TempSource.Count-1 do
    if (pos(Ancestor,TempSource.Strings[i]) <> 0) and (pos(TWinControl(_Control.Owner).Name,TempSource.Strings[i]) <> 0) and (pos('CLASS',Uppercase(TempSource.Strings[i])) <> 0) then
        Break;



  //if I => FSource.Count then I didn't find the line...
  If I < TempSource.Count then
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

//figure out what the unit name should be...
  FUnitName:='Unit1';  //just assigning it to this for now
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
     Add(Format('     T%s = class(T%s)', [nmForm,nmAncestor]));
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

Procedure TSourceEditor.CreateNewUnit;
Var
  I : Integer;
  TempSource : TStringList;
Begin
  TempSource := TStringList.Create;


//figure out what the unit name should be...
  FUnitName:='Unit1';  //just assigning it to this for now

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

  FSource.Clear;

  If Assigned(FOnAfterClose) then FOnAfterClose(Self);
end;

Function TSourceEditor.Open : Boolean;
Begin
  Result := True;
  If Assigned(FOnBeforeOpen) then FOnBeforeOpen(Self);

  try
    FEditor.Lines.LoadFromFile(FileName);
    FUnitName := Filename;
    FModified := False;
  except
    Result := False;
  end;

  if Result then
     If Assigned(FOnAfterOpen) then FOnAfterOpen(Self);
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
    res:LResource;
  begin
    Result:=false;
    res:=LazarusResources.Find(ResourceName);
    if (res.Value<>'') then begin
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
end;

destructor TSourceNotebook.Destroy;
begin

  FSourceEditorList.Free;

  inherited Destroy;
end;

Function TSourceNotebook.CreateNotebook : Boolean;
Begin
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
            Pages.Strings[0] := 'NewUnit.pp';
            PageIndex := 0;   // Set it to the first page
            Show;
          end; //with
      Show;  //used to display the code form
      end;

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

Function TSourceNotebook.NewSe(PageNum : Integer) : TSourceEditor;

Begin
 if CreateNotebook then Pagenum := 0;

if Pagenum = -1 then //add a new page
  Pagenum := Notebook1.Pages.Add('title');
  Result := TSourceEditor.Create(Self,Notebook1.Page[PageNum]);
  Notebook1.Pageindex := Pagenum;
  FSourceEditorList.Add(Result);
  Writeln('Assigning bookmark images');
  Result.Editor.BookmarkImages := Bookmarks;

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
       OpenFile(TMenuItem(sender).Caption)
   else
   Begin
     FOpenDialog.Title := 'Open';
     if FOpenDialog.Execute then  Begin
        //create a new page
        TempEditor := NewSE(-1);
        TempEditor.Filename := FOpenDialog.Filename;
        if (TempEditor.Open) then
        Begin
           if assigned(FOnOPenFile) then FOnOpenFile(self,FOpenDialog.Filename);
           Notebook1.Pages.Strings[Notebook1.Pageindex] := TempEditor.UnitName;
        end;
      end;
   end;
end;



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


Procedure TSourceNotebook.OpenFile(FileName: String);
Var
    TempEditor : TSourceEditor;
Begin
      if FileExists(Filename) then
      begin
      //create a new page
      TempEditor := NewSE(-1);
      TempEditor.Filename := Filename;
      TempEditor.OPen;
      Notebook1.Pages.Strings[Notebook1.Pageindex] := ExtractFileName(TempEditor.UnitName);
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

initialization


{$I designer/bookmark.lrs}

end.
