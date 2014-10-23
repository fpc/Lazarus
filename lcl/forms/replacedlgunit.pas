{
 /***************************************************************************
                               replacedlgunit.pas
                               ------------------

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
unit ReplaceDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, FindDlgUnit, ExtCtrls;

type

  TReplaceDialog = class;

  { TReplaceDialogForm }

  TReplaceDialogForm = class(TForm)
    PromptOnReplaceCheckBox: TCheckBox;
    EntireScopeCheckBox: TCheckBox;
    FindMoreButton: TButton;
    ReplaceButton: TButton;
    ReplaceAllButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    WholeWordsOnlyCheckBox: TCheckBox;
    CaseSensitiveCheckBox: TCheckBox;
    EditFind: TEdit;
    EditReplace: TEdit;
    TextLabel: TLabel;
    ReplaceLabel: TLabel;
    DirectionRadioGroup: TRadioGroup;
  private
  public
    { public declarations }
  end; 

implementation

{$R *.lfm}

{ TReplaceDialogForm }

end.

