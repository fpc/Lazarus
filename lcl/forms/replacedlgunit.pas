{
 /***************************************************************************
                               replacedlgunit.pas
                               ------------------

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
    This form is used to design the .lfm and lrs file.

    The code is copied to ../lcl/include/replacedialog.inc.
    The lrs included in the initialization section of lcl/dialogs.pp
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

