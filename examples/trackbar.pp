 {
 /***************************************************************************
                          trackbar - example
                          ------------------

                   Just a simple example to show & verify functionality
                   of the lazarus TTrackbar class.

                   Initial Revision  : Sun Aug 05 1999

                   by Stefan Hille <stoppok@osibisa.ms.sub.org>

  TODO: - button "orientation" will crash the program if pressed
        - add a label which displays current trackbar value and is updated
          by a procedure connected to "OnChange"
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
program Trackbar;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, Forms, Buttons, StdCtrls, Menus, ComCtrls,
  SysUtils, Controls;

type
        TForm1 = class(TFORM)
        public
          Button1: TButton;
          Button2: TButton;
          Button3: TButton;
          Button4: TButton;
          Button5: TButton;
          Button6: TButton;
          Button7: TButton;
          Button8: TButton;
          Track1 : TTRackBar;
          Track2 : TTRackBar;
          mnuMain: TMainMenu;
          itmFile: TMenuItem;
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
          procedure Button8CLick(Sender : TObject);
          procedure Track1Change(Sender : TObject);
        end;

var
Form1 : TForm1;

constructor TForm1.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Caption := 'TrackBar Demo v0.1';
   LoadMainMenu;
end;

procedure TForm1.Button1Click(Sender : TObject);
Begin
   if assigned (Track1) then begin
        Track1.Max := Track1.Max + 1;
   end;
End;

procedure TForm1.Button2Click(Sender : TObject);
Begin
   if assigned (track1) then begin
        if Track1.orientation = trHorizontal
          then Track1.orientation := trVertical
          else Track1.orientation := trHorizontal;
   end;
End;

procedure TForm1.Button3Click(Sender : TObject);
Begin
   if assigned (Track1) then begin
        writeln ('Setting new position');
        Track1.Position := Track1.Position + 1;
   end;
End;

procedure TForm1.Button4Click(Sender : TObject);
Begin
   if assigned (Track1) then begin
     writeln ('Setting new position for scale. Old:', ord (Track1.ScalePos));
     case Track1.ScalePos of
       trLeft  : Track1.ScalePos := trRight;
       trRight : Track1.ScalePos := trTop;
       trTop   : Track1.ScalePos := trBottom;
       trBottom: Track1.ScalePos := trLeft;
     end;
   end;
End;

procedure TForm1.Button5Click(Sender : TObject);
Begin
   if assigned (Track1) then begin
        writeln ('Toggling showing tickmarks');
     if Track1.TickStyle = tsNone
       then Track1.TickStyle := tsAuto
       else Track1.TickStyle := tsNone;
   end;
End;

procedure TForm1.Button6Click(Sender : TObject);
Begin
   if assigned (Track1) then begin
   end;
End;

procedure TForm1.Button7Click(Sender : TObject);
Begin
   if assigned (Track1) then begin
        writeln ('Incrementing LineSize');
        Track1.LineSize := Track1.LineSize + 1;
   end;
End;

procedure TForm1.Button8Click(Sender : TObject);
Begin
   if assigned (Track1) then begin
        writeln ('Incrementing PageSize');
        Track1.PageSize := Track1.PageSize + 1;
   end;
End;

procedure TForm1.Track1Change(Sender : TObject);
begin
   if assigned (Track1) then begin
        writeln ('*** CALLBACK ONCHANGE!!!!! ***');
        Track1.PageSize := Track1.PageSize + 1;
   end;
end;

{------------------------------------------------------------------------------}
procedure TForm1.LoadMainMenu;

begin
        { set the height and width }
        Height := 350;
        Width := 700;

        { Setting up horizontal trackbar }
        Track1 := TTrackBar.Create (Self);
        Track1.Parent := Self;
        Track1.Left := 50;
        Track1.Top := 150;
        Track1.Width := 140;
        Track1.Height := 140;
        Track1.Min := 0;
        Track1.Max := 100;
        Track1.OnChange := @Track1Change;
        Track1.Show;

        { Setting up vertical trackbar }
        Track2 := TTrackBar.Create (Self);
        Track2.Parent := Self;
        Track2.Orientation := trVertical;
        Track2.Left := 500;
        Track2.Top := 150;
        Track2.Width := 20;
        Track2.Height := 100;
        Track2.Min := 0;
        Track2.Max := 100;
        Track2.Show;

        { create some buttons to change trackbar properties }
        Button2 := TButton.Create(Self);
        button2.Parent := Self;
        Button2.Left := 200;
        Button2.Top := 30;
        Button2.Width := 100;
        Button2.Height := 30;
        Button2.Caption := 'Orientation';
        Button2.OnClick := @Button2Click;
        Button2.Show;

        Button1 := TButton.Create(Self);
        button1.Parent := Self;
        Button1.Left := 200;
        Button1.Top := 70;
        Button1.Width := 100;
        Button1.Height := 30;
        Button1.Caption := 'Max++';
        Button1.OnClick := @Button1Click;
        Button1.Show;

        Button3 := TButton.Create(Self);
        button3.Parent := Self;
        Button3.Left := 200;
        Button3.Top := 110;
        Button3.Width := 100;
        Button3.Height := 30;
        Button3.Show;
        Button3.Caption := 'Position++';
        Button3.OnClick := @Button3Click;

        Button4 := TButton.Create(Self);
        button4.Parent := Self;
        Button4.Left := 350;
        Button4.Top := 30;
        Button4.Width := 130;
        Button4.Height := 30;
        Button4.Show;
        Button4.Caption := 'GTK: TextPos';
        Button4.OnClick := @Button4Click;

        Button5 := TButton.Create(Self);
        button5.Parent := Self;
        Button5.Left := 350;
        Button5.Top := 70;
        Button5.Width := 130;
        Button5.Height := 30;
        Button5.Show;
        Button5.Caption := 'GTK: Show Text';
        Button5.OnClick := @Button5Click;

        Button6 := TButton.Create(Self);
        button6.Parent := Self;
        Button6.Left := 350;
        Button6.Top := 110;
        Button6.Width := 130;
        Button6.Height := 30;
        Button6.Show;
        Button6.Caption := '---';
        Button6.Visible := false;
        Button6.OnClick := @Button6Click;

        Button7 := TButton.Create(Self);
        button7.Parent := Self;
        Button7.Left := 200;
        Button7.Top := 150;
        Button7.Width := 100;
        Button7.Height := 30;
        Button7.Show;
        Button7.Caption := 'LineSize++';
        Button7.OnClick := @Button7Click;

        Button8 := TButton.Create(Self);
        button8.Parent := Self;
        Button8.Left := 350;
        Button8.Top := 150;
        Button8.Width := 130;
        Button8.Height := 30;
        Button8.Caption := 'PageSize++';
        Button8.OnClick := @Button8Click;
        Button8.Show;

        { create a menubar }
   mnuMain := TMainMenu.Create(Self);
   Menu := mnuMain;

   itmFile := TMenuItem.Create(Self);
   itmFile.Caption := 'File';
   Menu.Items.Add(itmFile);

   itmFileQuit := TMenuItem.Create(Self);
   itmFileQuit.Caption := 'Quit';
   itmFileQuit.OnClick := @mnuQuitClicked;
   itmFile.Add(itmFileQuit);
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
