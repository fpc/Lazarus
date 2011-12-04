{
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
unit codetools_wordpolicy_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, ExtCtrls,
  SourceChanger, CodeToolsOptions, LazarusIDEStrConsts, IDEOptionsIntf;

type

  { TCodetoolsWordPolicyOptionsFrame }

  TCodetoolsWordPolicyOptionsFrame = class(TAbstractIDEOptionsEditor)
    IdentifierPolicyRadioGroup: TRadioGroup;
    KeyWordPolicyRadioGroup: TRadioGroup;
  private
    { private declarations }
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCodetoolsWordPolicyOptionsFrame }

function TCodetoolsWordPolicyOptionsFrame.GetTitle: String;
begin
  Result := dlgWordsPolicies;
end;

procedure TCodetoolsWordPolicyOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  with KeyWordPolicyRadioGroup do begin
    Caption:=dlgKeywordPolicy ;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvNone);
      Add(dlgCDTLower);
      Add(dlgCDTUPPERCASE);
      Add(dlg1UP2low);
      EndUpdate;
    end;
  end;

  with IdentifierPolicyRadioGroup do begin
    Caption:=dlgIdentifierPolicy;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvNone);
      Add(dlgCDTLower);
      Add(dlgCDTUPPERCASE);
      Add(dlg1UP2low);
      EndUpdate;
    end;
  end;
end;

procedure TCodetoolsWordPolicyOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodetoolsOptions do
  begin
    case KeyWordPolicy of
      wpLowerCase:
        KeyWordPolicyRadioGroup.ItemIndex:=1;
      wpUpperCase:
        KeyWordPolicyRadioGroup.ItemIndex:=2;
      wpLowerCaseFirstLetterUp:
        KeyWordPolicyRadioGroup.ItemIndex:=3;
    else
      // wpNone
      KeyWordPolicyRadioGroup.ItemIndex:=0;
    end;
    case IdentifierPolicy of
      wpLowerCase:
        IdentifierPolicyRadioGroup.ItemIndex:=1;
      wpUpperCase:
        IdentifierPolicyRadioGroup.ItemIndex:=2;
      wpLowerCaseFirstLetterUp:
        IdentifierPolicyRadioGroup.ItemIndex:=3;
    else
      // wpNone
      IdentifierPolicyRadioGroup.ItemIndex:=0;
    end;
  end;
end;

procedure TCodetoolsWordPolicyOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodetoolsOptions do
  begin
    case KeyWordPolicyRadioGroup.ItemIndex of
      0: KeyWordPolicy:=wpNone;
      1: KeyWordPolicy:=wpLowerCase;
      2: KeyWordPolicy:=wpUpperCase;
      3: KeyWordPolicy:=wpLowerCaseFirstLetterUp;
    end;
    case IdentifierPolicyRadioGroup.ItemIndex of
      0: IdentifierPolicy:=wpNone;
      1: IdentifierPolicy:=wpLowerCase;
      2: IdentifierPolicy:=wpUpperCase;
      3: IdentifierPolicy:=wpLowerCaseFirstLetterUp;
    end;
  end;
end;

class function TCodetoolsWordPolicyOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodetoolsOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsWordPolicyOptionsFrame, CdtOptionsWords);
end.

