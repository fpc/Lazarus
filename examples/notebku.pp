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
}
unit notebku;

{$mode objfpc}
{$H+}

interface

uses Classes, SysUtils, Controls, Forms, ExtCtrls, Buttons{, TabNotBk};

type
   TForm1 = class(TForm)
   private
   public
     fNotebk: TNotebook;
     fNotebook: TNotebook;
     fButton1: TButton;
     fButton2: TButton;
     fButton3: TButton;
     fButton4: TButton;
     fButton5: TButton;
     fButton6: TButton;
     constructor Create(AOwner: TComponent); override;
     procedure Button1Click(Sender: TObject);
     procedure Button2Click(Sender: TObject);
     procedure Button3Click(Sender: TObject);
     procedure Button4Click(Sender: TObject);
     procedure Button5Click(Sender: TObject);
     procedure Button6Click(Sender: TObject);
   end;

var
  Form1: TForm1;

implementation

constructor TForm1.Create(AOwner: TComponent);	
begin
   inherited Create(AOwner);
   Caption := 'Notebook Test';
   Width := 300;
   Height := 400;
   Left := 200;
   Top := 200;

   // Create the Notebook
   fNotebk := TNotebook.Create(Self);
   with fNotebk do
   begin
      Parent :=  Self;
      Align := alClient;
      Left := 0;
      Top :=0;
      Width := Self.Width;
      Height := Self.Height;
      if Pages.PageCount>0 then
        Pages.Strings[0] := 'Page 1'
      else
        Pages.Add('Page 1');
      Pages.Add('Page 2');
      Pages.Add('Page 3');
      Pages.Add('Page 4');
      Pages.Add('Page 5');
      Show; 
   end;

   // Create the Tabbed Notebook
   fNotebook := TNotebook.Create(Self);
   with fNotebook do
   begin
      Parent := fNotebk.Page[4];
      Left := 20;
      Top := 50;
      Width := Parent.Width - 50;
      Height := Parent.Height - 80;
      if Pages.PageCount>0 then
        Pages.Strings[0] := 'Page 1'
      else
        Pages.Add('Page 1');
      Pages.Add('Page 2');
      Pages.Add('Page 3');
      Pages.Add('Page 4');
      Pages.Add('Page 5');
      Show;
   end;

   { Create Goto First Page Button on last page of Notebook
     Delphi Way
     This way uses the Pages.Objects property which returns a generic object that
     must be case to a TPage. This is the way Delphi works. }
   fButton1 := TButton.Create(Self);
   with fButton1 do
   begin
      Parent := fNotebk.Page[4];
      OnClick := @Button1Click;
      Width := 150;
      Height := 23;
      left := (fNotebk.Width - Width) div 2;
      top := 20;
      Caption := 'Goto First Page';
      Show; 
   end;

   { Create Goto Last Page Button on first page of Notebook
     Custom Way
     This way uses the Page property which returns a TPage object. The Pages.Objects 
     property returns a generic object that must be case to a TPage. This property is
     specific to Lazarus }
   fButton2 := TButton.Create(Self);
   with fButton2 do
   begin
      Parent := fNotebk.Page[0];
      OnClick := @Button2Click;
      Width := 150;
      Height := 23;
      left := (fNotebk.Width - Width) div 2;
      top := 20;
      Caption := 'Goto Last Page';
      Show; 
   end;

   // Create Close Button on the form
   fButton3 := TButton.Create(Self);
   with fButton3 do
   begin
      Parent := Self;
      OnClick := @Button3Click;
      Width := 70;
      Height := 23;
      left := (Parent.Width - Width - 20);
      top := (Parent.Height - 40);
      Caption := 'Close';
      Show; 
   end;

   // Create Show/Hide Tabs Button on first page of TabbedNotebook
   fButton4 := TButton.Create(fNotebook.Page[0]);
   with fButton4 do
   begin
      Parent := fNotebook.Page[0];
      OnClick := @Button4Click;
      Width := 90;
      Height := 23;
      left := (250 {Parent.Widht} - Width) div 2;
      top := 150;
      Caption := 'Hide Tabs';
      Show; 
   end;
 
   // Create Delete Page Button on third page of TabbedNotebook
   fButton5 := TButton.Create(fNotebook.Page[2]);
   with fButton5 do
   begin
      Parent := fNotebook.Page[2];
      OnClick := @Button5Click;
      Width := 120;
      Height := 23;
      left := (250 {Parent.Widht} - Width) div 2;
      top := 100;
      Caption := 'Delete Page';
      Show; 
   end;
 
   // Create Set Tabs Position Button on first page of TabbedNotebook
   fButton6 := TButton.Create(fNotebook.Page[0]);
   with fButton6 do
   begin
      Parent := fNotebook.Page[0];
      OnClick := @Button6Click;
      Width := 150;
      Height := 23;
      left := (250 {Parent.Widht} - Width) div 2;
      top := 190;
      Caption := 'Set Tab Position';
      Show; 
   end;

   // Goto the first page of the Notebook 
   fNotebk.PageIndex := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  //writeln('Button 1 Clicked');
  fNotebk.PageIndex := 0;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  //writeln('Button 2 Clicked');
  fNotebk.PageIndex := fNotebk.Pages.Count - 1;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  //writeln('Close Button Clicked');
  Free;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  //writeln('Show/Hide Tabs Button Clicked');

(*
  fNotebook.ShowTabs := not fNotebook.ShowTabs;
  if (fNotebook.ShowTabs) then
    fButton4.Caption := 'Hide Tabs'
  else
    fButton4.Caption := 'Show Tabs';
*)
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  //writeln('Delete Page Button Clicked');

  if (fNotebook.Pages.Count > 1) then
  begin
    // Make sure we don't delete the page with the delete button on it
    if (fNotebook.PageIndex = fNotebook.Pages.Count - 1) then
      fNotebook.Pages.Delete(fNotebook.Pages.Count - 2)
    else
      fNotebook.Pages.Delete(fNotebook.Pages.Count - 1);
  end;

end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  //writeln('Set Tab Position Button Clicked');

(*
  if fNotebook.TabPosition = tpTop then
     fNotebook.TabPosition := tpRight
  else if fNotebook.TabPosition = tpRight then
     fNotebook.TabPosition := tpBottom
  else if fNotebook.TabPosition = tpBottom then
     fNotebook.TabPosition := tpLeft
  else if fNotebook.TabPosition = tpLeft then
     fNotebook.TabPosition := tpTop;
*)
end;

end.

{
  $Log$
  Revision 1.3  2002/09/02 19:10:27  lazarus
  MG: TNoteBook now starts with no Page and TPage has no auto names

  Revision 1.2  2002/05/10 06:57:50  lazarus
  MG: updated licenses

  Revision 1.1  2000/07/13 10:28:21  michael
  + Initial import

  Revision 1.9  2000/04/17 23:39:23  lazarus
  MWE:
    * Updated examples makefile
    * Reviewd the notebook example
    + Added Listbox example (crashes on startup)

  Revision 1.8  1999/10/05 02:41:34  lazarus
  *** empty log message ***

  Revision 1.7  1999/09/22 20:29:53  lazarus
  *** empty log message ***

  Revision 1.6  1999/09/13 03:36:26  lazarus
  Fixed some bugs and added a couple more features to the example.     caw

  Revision 1.5  1999/08/17 13:31:35  lazarus
  Modified the examples to say "OnClick" insterad of "OnClicked"

  Revision 1.4  1999/08/04 05:27:44  lazarus
  Changed to test both TNotebook and TTabbedNotebook.   CAW

}

