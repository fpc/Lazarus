 { ***************************************************************************
                          MessageDialogs - example
                          ------------------------

   Just a simple example to show & verify functionality of the lazarus 
   functions to deal with message dialogs.

   by Stefan Hille <stoppok@osibisa.ms.sub.org>

  ****************************************************************************

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
Program MessagDialogs;

{$mode objfpc}

uses classes, forms, dialogs, buttons;
     
type
   TMainForm = class(TForm)
   private
   protected
   public
      button1 : TButton;
      constructor Create(AOwner: TComponent); override;
      procedure button1Click(Sender : TObject);
   end;

var
   MainForm : TMainForm;

constructor TMainForm.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Caption := 'Message Show';
   Width   := 200;
   Height  := 75;
   Left    := 200;
   Top     := 200;

   button1 := TButton.Create(Self);
   button1.OnClick := @button1click;
   button1.Parent  := Self;
   button1.left    := (width - 85) div 2 ;
   button1.top     := (height - 32) div 2;
   button1.width   := 85;
   button1.height  := 32;
   button1.caption := 'Start Show';
   button1.Show;
end;

procedure TMainForm.Button1Click(Sender : TObject);
begin
   ShowMessage ('First simple test!');
   MessageDlg  ('Two buttons now...', mtError, [mbOK,mbCancel], 0);
   MessageDlg  ('Warning, not fully implemented', mtWarning, [mbYes, mbNo, mbOK,mbCancel], 0);
   ShowMessageFmt ('The show will end now'+#13+'%s', [MainForm.Caption]);
   close;
end;


begin
   Application.Initialize;
   Application.CreateForm(TMainForm, MainForm);
   Application.Run;
end.
{
   $Log$
   Revision 1.1  2001/03/03 00:51:54  lazarus
   + new sample to show Message dialogs
   stoppok

}   
