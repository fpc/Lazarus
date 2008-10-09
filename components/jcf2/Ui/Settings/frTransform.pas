unit frTransform;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is frTransform.pas.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL")
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  Classes, Graphics, Controls, Forms, 
  StdCtrls, ExtCtrls,
  { local }
  frmBaseSettingsFrame;

type
  TfTransform = class(TfrSettingsFrame)
    cbBlockEndSemicolons: TCheckBox;
    rbBeginEnd: TRadioGroup;
    bgSortUses: TGroupBox;
    cbSortInterfaceUses: TCheckBox;
    cbSortImplementationUses: TCheckBox;
    cbBreakUsesSortOnComment: TCheckBox;
    cbBreakUsesSortOnReturn: TCheckBox;
    rgUsesSortOrder: TRadioGroup;
    cbNoComments: TCheckBox;
    cbSortProgramUses: TCheckBox;
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;
  end;

implementation

uses SettingsTypes, JcfSettings, JcfHelp, SetTransform;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

constructor TfTransform.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_TRANSFORM;
end;

procedure TfTransform.Read;
begin
  with FormatSettings.Transform do
  begin
    rbBeginEnd.ItemIndex := Ord(BeginEndStyle);
    cbBlockEndSemicolons.Checked := AddBlockEndSemicolon;

    cbSortInterfaceUses.Checked := SortInterfaceUses;
    cbSortImplementationUses.Checked := SortImplementationUses;
    cbSortProgramUses.Checked := SortProgramUses;
    cbBreakUsesSortOnReturn.Checked := BreakUsesSortOnReturn;
    cbBreakUsesSortOnComment.Checked := BreakUsesSortOnComment;

    rgUsesSortOrder.ItemIndex := Ord(UsesSortOrder);

    cbNoComments.Checked := SortUsesNoComments;
  end;

end;

procedure TfTransform.Write;
begin
  with FormatSettings.Transform do
  begin
    BeginEndStyle := TTriOptionStyle(rbBeginEnd.ItemIndex);
    AddBlockEndSemicolon := cbBlockEndSemicolons.Checked;

    SortInterfaceUses := cbSortInterfaceUses.Checked;
    SortImplementationUses := cbSortImplementationUses.Checked;
    SortProgramUses := cbSortProgramUses.Checked;

    BreakUsesSortOnReturn := cbBreakUsesSortOnReturn.Checked;
    BreakUsesSortOnComment := cbBreakUsesSortOnComment.Checked;

    UsesSortOrder := TUsesSortOrder(rgUsesSortOrder.ItemIndex);

    SortUsesNoComments := cbNoComments.Checked;
  end;

end;

end.
