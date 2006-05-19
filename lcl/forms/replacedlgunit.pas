{
 /***************************************************************************
                               replacedlgunit.pas
                               ------------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Abstract:
    Only the .lrs file is used by the LCL (dialogs.pp).
    This unit is itself is not compiled into the LCL.
    This form is used to design the .lfm and lrs file.

    The code is copied to lcl/include/replacedialog.inc.
}
unit ReplaceDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, finddlgunit, ExtCtrls;

type

  TReplaceDialog = class;

  { TReplaceDialogForm }

  TReplaceDialogForm = class(TForm)
    Button1: TButton;
    btnReplace: TButton;
    btnReplaceAll: TButton;
    Button4: TButton;
    btnHelp: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    EditFind: TEdit;
    EditReplace: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    procedure btnReplaceAllClick(Sender: TObject);
  private
    ReplOwner: TReplaceDialog;
  public
    { public declarations }
  end; 

implementation

{ TReplaceDialogForm }

procedure TReplaceDialogForm.btnReplaceAllClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1:ReplOwner.FOptions:=ReplOwner.FOptions + [frFindNext];
    2:ReplOwner.FOptions:=ReplOwner.FOptions + [frReplace];
    3:ReplOwner.FOptions:=ReplOwner.FOptions + [frReplaceAll];
  end;
  if RadioGroup1.ItemIndex = 0 then
    ReplOwner.FOptions:=ReplOwner.FOptions + [frDown]
  else
    ReplOwner.FOptions:=ReplOwner.FOptions - [frDown];

  if CheckBox1.Checked then
    ReplOwner.FOptions:=ReplOwner.FOptions + [frWholeWord]
  else
    ReplOwner.FOptions:=ReplOwner.FOptions - [frWholeWord];

  if CheckBox2.Checked then
    ReplOwner.FOptions:=ReplOwner.FOptions + [frMatchCase]
  else
    ReplOwner.FOptions:=ReplOwner.FOptions - [frMatchCase];
end;



initialization
  {$I replasedlgunit.lrs}

end.

