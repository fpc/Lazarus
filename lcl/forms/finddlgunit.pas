{
 /***************************************************************************
                               finddlgunit.pas
                               ---------------

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
    
    The code is copied to lcl/include/finddialog.inc.
}
unit FindDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls;

type

  { TFindDialogForm }

  TFindDialogForm = class(TForm)
    btnFind: TButton;
    Button2: TButton;
    btnHelp: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    EditFind: TEdit;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    procedure EditFindChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

{ TFindDialogForm }

procedure TFindDialogForm.EditFindChange(Sender: TObject);
begin
  btnFind.Enabled:=EditFind.Text<>'';
end;

procedure TFindDialogForm.FormCreate(Sender: TObject);
begin
  EditFindChange(nil);
end;


initialization
  {$I finddlgunit.lrs}
end.

