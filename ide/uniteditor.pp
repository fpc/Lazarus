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
       Graphics,Extctrls;

type

{$ifdef NEW_EDITOR_SYNEDIT}
  TmwCustomEdit = TSynEdit;
  TmwPasSyn = TSynPasSyn;
{$endif}

  TSourceEditor = class
  private
    FAOwner : TComponent;  //this will likeley be the notebook page it's on
{$ifdef NEW_EDITOR_SYNEDIT}
    FHighlighter: TSynPasSyn;
    FEditor     : TSynEditor
{$else}
    FHighlighter: TmwPasSyn;
    FEditor     : TmwCustomEdit;
{$endif}
    FControl: TComponent;  //if this is a Form or Datamodule, this is used
    FCurrentCursorXLine : Integer;  //pulled out of the editor by the Function Getxxx
    FCurrentCursorYLine : Integer;  //pulled out of the editor by the Function Getxxx
    FFileName : String;
    FModified : Boolean;
    FSource : TStringList; //pulled out of the editor by the Function Getxxx
    FUnitName : String;

    FOnAfterClose : TNotifyEvent;
    FOnAfterOpen : TNotifyEvent;
    FOnAfterSave : TNotifyEvent;
    FOnBeforeClose : TNotifyEvent;
    FOnBeforeOpen : TNotifyEvent;
    FOnBeforeSave : TNotifyEvent;

    Function GetSource : TStrings;
    Procedure SetSource(value : TStrings);
    Function GetCurrentCursorXLine : Integer;
    Procedure SetCurrentCursorXLine(num : Integer);
    Function GetCurrentCursorYLine : Integer;
    Procedure SetCurrentCursorYLine(num : Integer);
    Function GetAncestor : String;
    Function GetModified : Boolean;
  protected
    Procedure DisplayControl;
    Procedure ReParent(AParent : TWinControl);
    property Control : TComponent read FControl;
    property Editor : TmwCustomEdit read FEditor;
  public
    constructor Create(AOwner : TComponent; AParent : TWinControl);
    destructor Destroy; override;
    Procedure AddControlCode(_Control : TComponent);
    Procedure SelectText(LineNum,CharStart,LineNum2,CharEnd : Integer);
    Procedure KeyPressed(Sender : TObject; var key: char);
    Procedure CreateFormUnit(AForm : TCustomForm);
    Function Close : Boolean;
    Function Save : Boolean;
    Function Open : Boolean;

    property Source : TStrings read GetSource write SetSource;
    property CurrentCursorXLine : Integer read GetCurrentCursorXLine write SetCurrentCursorXLine;
    property CurrentCursorYLine : Integer read GetCurrentCursorYLine write SetCurrentCursorYLine;
    property Owner : TComponent read FAOwner;
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
    Function GetEmpty : Boolean;  //look at the # of pages

  protected
    Function CreateNotebook : Boolean;
    Function GetActiveSE : TSourceEditor;
    Function ActiveUnitName : String;
    Function ActiveFileName : String;
    Function DisplayPage(SE : TSourceEditor) : Boolean;
    Function NewSE(Pagenum : Integer) : TSourceEditor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Procedure DisplayFormforActivePage;
    Procedure DisplayCodeforControl(Control : TObject);
    Function CreateUnitFromForm(AForm : TForm) : TSourceEditor;
    procedure CloseClicked(Sender : TObject);
    procedure OpenClicked(Sender : TObject);
    procedure SaveClicked(Sender : TObject);
    procedure SaveAllClicked(Sender : TObject);
    procedure SaveAsClicked(Sender : TObject);

    property Empty : Boolean read GetEmpty;
  end;


implementation
uses
  LCLLinux,TypInfo;


{ TSourceEditor }

constructor TSourceEditor.create(AOwner : TComponent; AParent : TWinControl);
Begin
inherited Create;
FAOwner := AOwner;

FSource := TStringList.create;

FEditor := TmwCustomEdit.Create(FAOwner);
  with FEditor do
  begin
    Parent := AParent;
    Top := 25;
    Left := 0;
    Align := alClient;
    Width := TWinControl(FAOwner).ClientWidth - 10;//clientwidth;//500;
    Height :=TWinControl(FAOwner).ClientHeight -10;//clientheight;//250;
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
      OnKeyPress := @KeyPRessed;
  end;


end;

destructor TSourceEditor.destroy;
begin
  FHighlighter.free;
  FEditor.Free;
  FSource.free;
  inherited;
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

Procedure TSourceEditor.DisplayControl;
Begin
Writeln('DisplayCOntrol');
if (FControl is TCustomForm) then TCustomForm(FControl).Show
    else
    if (FCOntrol is TControl) then TControl(FCOntrol).Visible := True;

//Bringtofront does not work yet.
//TControl(FControl).BringToFront;
//so I hide it and unhide it.
TCOntrol(FCOntrol).Visible :=False;
TCOntrol(FCOntrol).Visible := True;
Writeln('Exit DisplayCOntrol');
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

Procedure TSourceEditor.KeyPressed(Sender : TObject; var key: char);
Begin

end;

Function TSourceEditor.GetModified : Boolean;
Begin
Result := FSource <> FEditor.Lines;
end;

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
     Add('     { private declarations}');
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
  except
    Result := False;
  end;

  If Assigned(FOnAfterOpen) then FOnAfterOpen(Self);
end;


Function TSourceEditor.Save : Boolean;
Begin
  Result := True;
  If Assigned(FOnBeforeSave) then FOnBeforeSave(Self);

  try
    FEditor.Lines.SaveToFile(FileName);
  except
    Result := False;
  end;

  If Assigned(FOnAfterSave) then FOnAfterSave(Self);
end;

Procedure TSourceEditor.ReParent(AParent : TWInControl);
Begin
with FEditor do
   Begin
    Parent := AParent;
    Top := 25;
    Left := 0;
    Align := alClient;
    Width := TWinControl(FAOwner).ClientWidth - 10;//clientwidth;//500;
    Height :=TWinControl(FAOwner).ClientHeight -10;//clientheight;//250;
   end;
End;


{------------------------------------------------------------------------}
                      { TSourceNotebook }

constructor TSourceNotebook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'Lazarus Editor';
  Left := 0;
  Top := 0;
  Width := 600;
  height := 600;

  FSourceEditorList := TList.Create;
  FSaveDialog := TSaveDialog.Create(Self);
  FOpenDialog := TOpenDialog.Create(Self);
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
  Notebook_Just_Created := (not assigned(Notebook1)) or (Notebook1.Pages.Count = 0);

  if Notebook_Just_Created then
  TempSourceEditor := NewSe(0)
  else
  tempSourceEditor := NewSe(-1);

  TempSourceEditor.CreateFormUnit(AForm);

  Notebook1.Pages.Strings[Notebook1.PageIndex] := TempSourceEditor.Unitname;

  Result := TempSourceEditor;
  Show;
end;

Function TSOurceNotebook.NewSe(PageNum : Integer) : TSourceEditor;

Begin
 if CreateNotebook then Pagenum := 0;

if Pagenum = -1 then
  Pagenum := Notebook1.Pages.Add('title');
  Result := TSourceEditor.Create(Self,Notebook1.Page[PageNum]);
  Notebook1.Pageindex := Pagenum;
  FSourceEditorList.Add(Result);

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
          if SE.Editor = TempEditor then Break;
        End;

    if X < Notebook1.Pages.Count then
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
Result := Notebook1.Pages.Count = 0;
end;

Procedure TSourceNotebook.OpenClicked(Sender: TObject);
Var
    TempEditor : TSourceEditor;
Begin
  Writeln('***********************OPENCLICKED');
   FOpenDialog.Title := 'Open';
Writeln('1');
   if FOpenDialog.Execute then  Begin
Writeln('2');
      //create a new page
      TempEditor := NewSE(-1);
Writeln('3');
      TempEditor.Filename := FOpenDialog.Filename;
      TempEditor.OPen;
      Notebook1.Pages.Strings[Notebook1.Pageindex] := TempEditor.UnitName;
Writeln('4');
      end;

end;

Procedure TSourceNotebook.SaveClicked(Sender: TObject);
Begin
if ActiveFileName <> '' then
GetActiveSE.Save
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

    GetActiveSE.Close;

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
    GetActiveSE.Save;
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
           TempEditor.Save;
           end
           else
           Break;
        end;
end;


end.
