{
 /***************************************************************************
                               finddlgunit.pas
                               ---------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
    This form is used to design the .lfm and .lrs file.
    
    The source code is copied to ../lcl/include/finddialog.inc.
    The lrs included in the initialization section of lcl/dialogs.pp
}
unit FindDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls;

type

  { TFindDialogForm }

  TFindDialogForm = class(TForm)
    EntireScopeCheckBox: TCheckBox;
    FindButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    WholeWordsOnlyCheckBox: TCheckBox;
    CaseSensitiveCheckBox: TCheckBox;
    EditFind: TEdit;
    FindLabel: TLabel;
    DirectionRadioGroup: TRadioGroup;
    procedure EditFindChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

{$R *.lfm}

{ TFindDialogForm }

procedure TFindDialogForm.EditFindChange(Sender: TObject);
begin
  FindButton.Enabled:=EditFind.Text<>'';
end;

procedure TFindDialogForm.FormCreate(Sender: TObject);
begin
  EditFindChange(nil);
end;

end.

