{
 /***************************************************************************
                               IDEEditor.pp
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
unit IDEEditor;

{$mode objfpc}

interface

uses
  classes, Controls, forms,buttons,sysutils,
	mwcustomedit,mwPasSyn, Graphics,Extctrls;//,TabNotBk;

type

  TIDEEditor = class(TFORM)
    Notebook1 : TNotebook;
  private
    FEmpty : Boolean;
    FHighlighter: TmwPasSyn;
    function CreateNewEditor(const AParent: TWinControl): TmwCustomEdit;
  protected
    Procedure IDEEditorPaint(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Function AddPage(title: String; Lines : TStringList) : TmwCustomEdit;
    Procedure DeletePage(Value : Integer);
    Function GetEditorfromPage(Value : Integer) : TmwCustomEdit;
    property Empty : Boolean read FEmpty write FEmpty;
  end;


var
  IDEEditor1 : TIDEEditor;

implementation
uses
  LCLLinux;

constructor TIDEEDitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'Lazarus Editor';
  Left := 0;
  Top := 0;
  Width := 600;
  height := 600;

  Notebook1 := TNotebook.Create(Self);
  Notebook1.Parent := Self;
  Notebook1.Align := alClient;
  Notebook1.Left := 0;
  Notebook1.Top :=2;
  Notebook1.Width := ClientWidth;
  Notebook1.Height := ClientHeight-Notebook1.top;
  Notebook1.Pages.Strings[0] := 'NewUnit.pp';
  Notebook1.PageIndex := 0;   // Set it to the first page
  Notebook1.Show;
  Empty := True;

  FHighlighter := nil;
{
  FHighlighter := TmwPasSyn.Create(Self);
  FHighlighter.CommentAttri.Foreground := clNavy;
  FHighlighter.NumberAttri.Foreground := clRed;
  FHighlighter.KeyAttri.Foreground := clGreen;
}

//OnPaint := @IDEEditorPaint;
end;

destructor TIDEEditor.Destroy;
begin
  FHighlighter.Free;
  inherited Destroy;
end;

function TIDEEditor.CreateNewEditor(const AParent: TWinControl): TmwCustomEdit;
begin
  if FHighlighter = nil
  then begin
    FHighlighter := TmwPasSyn.Create(Self);
    FHighlighter.CommentAttri.Foreground := clNavy;
    FHighlighter.NumberAttri.Foreground := clRed;
    FHighlighter.KeyAttri.Foreground := clGreen;
  end;


  Result := TmwCustomEdit.Create(Self);
  with Result do
  begin
    Parent := AParent;
    Top := 25;
    Left := 0;
    Align := alClient;
    Width := Notebook1.ClientWidth - 10;//clientwidth;//500;
    Height := Notebook1.ClientHeight -10;//clientheight;//250;
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
      // MWE: Don't ask me wy but a highlighter created outside
      //      an editor doesn't work
      Highlighter := TmwPasSyn.Create(Self);
      with TmwPasSyn(HighLighter) do
      begin
        CommentAttri.Foreground := clNavy;
        NumberAttri.Foreground := clRed;
        KeyAttri.Foreground := clGreen;
      end;
      FHighLighter := HighLighter;
    end 
    else HighLighter := FHighLighter;
//    HighLighter := TmwPasSyn.Create(Self);
//    TmwPasSyn(HighLighter).CommentAttri.Foreground := clNavy;
//    TmwPasSyn(HighLighter).NumberAttri.Foreground := clRed;
//    TmwPasSyn(HighLighter).KeyAttri.Foreground := clGreen;
    
  end;

end;

Procedure TIDEEditor.IDEEditorPaint(Sender : TObject);
Begin
Assert(False, 'Trace:IDEEDitor paint...');
end;


Function TIDEEditor.AddPage(title: String; Lines : TStringList) : TmwCustomEdit;
var
  PageIndex: Integer;
Begin
  if Empty
  then begin
    // temp fix for empty notebook (it has one page)
    Empty := False;
    Notebook1.Pages.Strings[0] := Title;
    PageIndex := 0;
  end
  else PageIndex := Notebook1.Pages.Add(Title);

  Result := CreateNewEditor(Notebook1.Page[PageIndex]);
  Result.Lines.Assign(Lines);


  with Notebook1.Pages do
    Assert(False, Format('Trace:New Page title is %s  --- New Page Index is %d',[Strings[Count - 1], Count - 1]));

end;

Procedure TIDEEditor.DeletePage(Value : Integer);
var
tempedit : TmwCustomEdit;
Begin
//Check to see if the page is changed
with Notebook1.Page[Value] do
Begin
  TempEdit := GetEditorFromPage(Value);
  if tempEdit <> nil then
     Begin
      if TempEdit.Modified then
         begin
          if Application.Messagebox('Save this unit first?', 'Question', MB_YESNO) = IDYES then
		Assert(False, 'Trace:**************************MRYES')
		else
		Assert(False, 'Trace:**************************MRNO');
         end;
     end;
  end;


Notebook1.Pages.Delete(Value);
if Notebook1.Pages.Count = 0 then
   Begin
   Empty := True;
   Close;
   end;
end;

Function TIdeEditor.GetEditorfromPage(Value : Integer) : TmwCustomEdit;
var
I : Integer;
Begin
Result := nil;
with Notebook1.Page[Value] do
  Begin
  for I := 0 to ControlCount-1 do
      Begin
      if Controls[I] is TmwCustomEdit then
         Begin
         Assert(False, 'Trace:*******************************FOUND TmwCUSTOMEDIT**************');
         Result := TmwCustomEdit(Controls[I]);
         break;
         end;
      end;
   end;

end;

end.
