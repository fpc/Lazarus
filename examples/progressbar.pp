{
 /***************************************************************************
                          progressbar - example
                          ---------------------

                   Just a simple example to show & verify functionality
                   of the lazarus TTimer / TProgressBar classes.

                   Initial Revision  : Sun Aug 15 1999

                   by Stefan Hille <stoppok@osibisa.ms.sub.org>

 ***************************************************************************/

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
program Progressbar;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, StdCtrls, Forms, Buttons, Menus, ComCtrls,
  SysUtils, Extctrls;


type
  TForm1 = class(TFORM)
  public
    Progre1: TProgressBar;
    Timer0 : TTimer;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    mnuFile: TMainMenu;
    itmFileQuit: TMenuItem;
    constructor Create(AOwner: TComponent); override;
    procedure LoadMainMenu;
    procedure mnuQuitClicked(Sender : TObject);
  protected
    procedure Button1CLick(Sender : TObject);
    procedure Button2CLick(Sender : TObject);
    procedure Button3CLick(Sender : TObject);
    procedure Button4CLick(Sender : TObject);
    procedure Button5CLick(Sender : TObject);
    procedure Button6CLick(Sender : TObject);
    procedure Button7CLick(Sender : TObject);
    procedure TimedOut(Sender : TObject);
  end;

var
  Form1 : TForm1;


constructor TForm1.Create(AOwner: TComponent);	
begin
   inherited Create(AOwner);
   Caption := 'ProgressBar Demo v0.1';
   LoadMainMenu;
end;

procedure TForm1.Button1Click(Sender : TObject);
Begin
   if assigned (progre1) then begin
        Progre1.Position := 0;
   	progre1.Min := progre1.Min - 1
   end;
End;

procedure TForm1.Button2Click(Sender : TObject);
Begin
   if assigned (progre1) then begin
        Progre1.Position := 0;
   	progre1.Min := progre1.Min + 1;
   end;
End;

procedure TForm1.Button3Click(Sender : TObject);
Begin
   if assigned (progre1) then begin
	Progre1.Position := 0;
	progre1.Max := progre1.Max +1;
   end;
End;

procedure TForm1.Button4Click(Sender : TObject);
Begin
   if assigned (progre1) then begin
        Progre1.Position := 0;
	progre1.Max := progre1.Max -1;
   end;
End;

procedure TForm1.Button5Click(Sender : TObject);
Begin
   if assigned (progre1) then begin
        Progre1.Smooth := not Progre1.Smooth;
	if assigned (Button6) 
	  then Button6.Visible := Progre1.Smooth;
   end;
End;

procedure TForm1.Button6Click(Sender : TObject);
Begin
   if assigned (progre1) then begin
        Progre1.BarShowtext := not Progre1.BarShowtext;
   end;
End;

procedure TForm1.Button7Click(Sender : TObject);
Begin
   if assigned (progre1) then 
   begin
     case Progre1.Orientation of
        pbVertical      : Progre1.Orientation := pbRightToLeft;
        pbRightToLeft   : Progre1.Orientation := pbTopDown;
	pbTopDown       : Progre1.Orientation := pbHorizontal;
        pbHorizontal    : Progre1.Orientation := pbVertical;
     end;
   end;
End;

procedure TForm1.TimedOut(Sender : TObject);
Begin
   if assigned (progre1) then
   begin
      Progre1.StepIt;
      if Progre1.Position = Progre1.Max
         then Progre1.Position := Progre1.min;
   end;
End;

{------------------------------------------------------------------------------}
procedure TForm1.LoadMainMenu;

begin
	{ set the height and width }
	Height := 350;
	Width := 700;

	{ Create the timer }
	Timer0 := TTimer.Create (Self);
	Timer0.OnTimer := @TimedOut;

	{ Create a progressbar }
	Progre1 := TProgressBar.Create (Self);
	Progre1.Parent := Self;
	Progre1.Left := 300;
	Progre1.Top  := 30;
	Progre1.Width:= 250;
	Progre1.Height:= 40;
	Progre1.BarShowText := true;
	Progre1.Smooth := True;
	Progre1.Show;


	{ Create a few buttons }
	Button2 := TButton.Create(Self);
	Button2.Parent := Self; 
	Button2.Left := 200;
	Button2.Top := 70;
	Button2.Width := 80;
	Button2.Height := 30;
	Button2.Show;
	Button2.Caption := 'PMin ++';
//	Button2.ToolTip := 'Tool Tip';
//	Button2.ShowToolTip := True;
	Button2.OnClick := @Button2Click;


	Button1 := TButton.Create(Self);
	Button1.Parent := Self; 
	Button1.Left := 50;
	Button1.Top := 70;
	Button1.Width := 80;
	Button1.Height := 30;
	Button1.Show;
	Button1.Caption := 'PMin--';
	Button1.OnClick := @Button1Click;

	{ Create 2 more buttons outside the groupbox }
	Button3 := TButton.Create(Self);
	Button3.Parent := Self; 
	Button3.Left := 50;
	Button3.Top := 30;
	Button3.Width := 80;
	Button3.Height := 30;
	Button3.Show;
	Button3.Caption := 'PMax++';
//	Button3.ToolTip := 'Tool Tip';
//	Button3.ShowToolTip := True;
	Button3.OnClick := @Button3Click;

	Button4 := TButton.Create(Self);
	Button4.Parent := Self; 
	Button4.Left := 200;
	Button4.Top := 30;
	Button4.Width := 80;
	Button4.Height := 30;
	Button4.Show;
	Button4.Caption := 'PMax--';
	Button4.OnClick := @Button4Click;

	Button5 := TButton.Create(Self);
	Button5.Parent := Self; 
	Button5.Left := 100;
	Button5.Top := 110;
	Button5.Width := 130;
	Button5.Height := 30;
	Button5.Show;
	Button5.Caption := 'Toggle Smooth';
	Button5.OnClick := @Button5Click;

	Button6 := TButton.Create(Self);
	Button6.Parent := Self; 
	Button6.Left := 100;
	Button6.Top := 150;
	Button6.Width := 130;
	Button6.Height := 30;
	Button6.Show;
	Button6.Caption := 'Toggle Text';
	Button6.OnClick := @Button6Click;
	Button6.Visible := Progre1.Smooth;

	Button7 := TButton.Create(Self);
	Button7.Parent := Self; 
	Button7.Left := 100;
	Button7.Top := 190;
	Button7.Width := 130;
	Button7.Height := 30;
	Button7.Show;
	Button7.Caption := 'Orientation';
	Button7.OnClick := @Button7Click;

	{ create a menubar }
	mnuFile := TMainMenu.Create(Self);
        mnuFile.Name:='mnuFile';
        Menu := mnuFile;

	itmFileQuit := TMenuItem.Create(Self);
	itmFileQuit.Caption := 'Quit';
	itmFileQuit.OnClick := @mnuQuitClicked;
	mnuFile.Items.Add(itmFileQuit);

end;

{------------------------------------------------------------------------------}
procedure TForm1.mnuQuitClicked(Sender : TObject);
begin
  Close;
end;
{------------------------------------------------------------------------------}

begin
   Application.Initialize; { calls InitProcedure which starts up GTK }
   Application.CreateForm(TForm1, Form1);
   Application.Run;
end.

