{
 /***************************************************************************
                               finddlgunit.pas
                               ---------------

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Note:
   Only the .lfm file is used by the LCL (dialogs.pp).
   This unit is itself is not compiled into the LCL.
   This form is used to design the .lfm file.

   The source code is a copy of ../lcl/include/finddialog.inc.
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
    BtnPanel: TPanel;
    EntireScopeCheckBox: TCheckBox;
    FindButton: TButton;
    CancelButton: TButton;
    FlagsPanel: TPanel;
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

