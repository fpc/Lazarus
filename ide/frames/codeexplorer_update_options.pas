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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit codeexplorer_update_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  Forms, ExtCtrls, StdCtrls,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  LazarusIDEStrConsts, CodeExplOpts;

type

  { TCodeExplorerUpdateOptionsFrame }

  TCodeExplorerUpdateOptionsFrame = class(TAbstractIDEOptionsEditor)
    FollowCursorCheckBox: TCheckBox;
    ModeRadioGroup: TRadioGroup;
    RefreshRadioGroup: TRadioGroup;
  private
    { private declarations }
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCodeExplorerUpdateOptionsFrame }

function TCodeExplorerUpdateOptionsFrame.GetTitle: String;
begin
  Result := lisCEOUpdate;
end;

procedure TCodeExplorerUpdateOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  ModeRadioGroup.Caption := lisCEOMode;
  with ModeRadioGroup do
  begin
    Items[0] := lisCEOModeCategory;
    Items[1] := lisCEOModeSource;
  end;
  RefreshRadioGroup.Caption := lisCEORefreshAutomatically;
  with RefreshRadioGroup do
  begin
    Items[0] := lisCEONeverOnlyManually;
    Items[1] := lisCEOWhenSwitchingFile;
    Items[2] := lisCEOOnIdle;
  end;
  FollowCursorCheckBox.Caption := lisCEFollowCursor;
  FollowCursorCheckBox.Hint :=
    lisWhenTheSourceEditorCursorMovesShowTheCurrentNodeIn;
end;

procedure TCodeExplorerUpdateOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodeExplorerOptions do
  begin
    case Refresh of
      cerManual: RefreshRadioGroup.ItemIndex := 0;
      cerSwitchEditorPage: RefreshRadioGroup.ItemIndex := 1;
      cerOnIdle: RefreshRadioGroup.ItemIndex := 2;
    else
      RefreshRadioGroup.ItemIndex := 1;
    end;

    case Mode of
      cemCategory: ModeRadioGroup.ItemIndex := 0;
      cemSource: ModeRadioGroup.ItemIndex := 1;
    else
      ModeRadioGroup.ItemIndex := 0;
    end;

    FollowCursorCheckBox.Checked := FollowCursor;
  end;
end;

procedure TCodeExplorerUpdateOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodeExplorerOptions do
  begin
    case RefreshRadioGroup.ItemIndex of
      0: Refresh := cerManual;
      1: Refresh := cerSwitchEditorPage;
      2: Refresh := cerOnIdle;
    end;

    case ModeRadioGroup.ItemIndex of
      0: Mode := cemCategory;
      1: Mode := cemSource;
    end;

    FollowCursor := FollowCursorCheckBox.Checked;
  end;
end;

class function TCodeExplorerUpdateOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeExplorerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodeExplorer, TCodeExplorerUpdateOptionsFrame, cdeOptionsUpdate);

end.

