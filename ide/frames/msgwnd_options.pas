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

  Author: Mattias Gaertner

  Abstract:
    IDE option frame for Messages window.
}
unit MsgWnd_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazLoggerBase, IDEOptionsIntf, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ColorBox, ExtCtrls, LazarusIDEStrConsts,
  EnvironmentOpts;

type

  { TMsgWndOptionsFrame }

  TMsgWndOptionsFrame = class(TAbstractIDEOptionsEditor)
    MWDblClickJumpsCheckBox: TCheckBox;
    MWHideIconsCheckBox: TCheckBox;
    MWOptsLeftBevel: TBevel;
    MWColorBox: TColorBox;
    MWColorListBox: TColorListBox;
    MWColorsGroupBox: TGroupBox;
    MWOptionsLabel: TLabel;
    MWOptsRightBevel: TBevel;
    MWSetAllColorsToLabel: TLabel;
    MWSetDefaultColorsButton: TButton;
    procedure MWColorBoxChange(Sender: TObject);
    procedure MWColorListBoxGetColors(Sender: TCustomColorListBox;
      Items: TStrings);
    procedure MWColorListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure MWSetDefaultColorsButtonClick(Sender: TObject);
  private
    fReady: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

var
  MsgWndOptionsFrame: TMsgWndOptionsFrame;

implementation

{$R *.lfm}

{ TMsgWndOptionsFrame }

procedure TMsgWndOptionsFrame.MWColorListBoxGetColors(
  Sender: TCustomColorListBox; Items: TStrings);
begin
  Items.Add(dlgBackColor);
  Items.Add('Tool Header: Running');
  Items.Add('Tool Header: Success');
  Items.Add('Tool Header: Failed');
  Items.Add('Tool Header: Scrolled up');
end;

procedure TMsgWndOptionsFrame.MWColorBoxChange(Sender: TObject);
var
  i: Integer;
begin
  i:=MWColorListBox.ItemIndex;
  if not fReady or (i < 0) then
    exit;
  MWColorListBox.Colors[i]:=MWColorBox.Selected;
end;

procedure TMsgWndOptionsFrame.MWColorListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  if not (fReady and User) then
    Exit;
  MWColorBox.Selected := MWColorListBox.Selected;
end;

procedure TMsgWndOptionsFrame.MWSetDefaultColorsButtonClick(Sender: TObject);
var
  c: TMsgWndColor;
begin
  for c in TMsgWndColor do
    MWColorListBox.Colors[ord(c)]:=MsgWndDefaultColors[c];
  MWColorBox.Selected := MWColorListBox.Selected;
end;

constructor TMsgWndOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MWOptionsLabel.Caption:=dlgOIOptions;
  MWColorsGroupBox.Caption:=dlgColors;
  MWSetAllColorsToLabel.Caption:=lisSetAllColors;
  MWSetDefaultColorsButton.Caption:=lisLazarusDefault;
  MWHideIconsCheckBox.Caption := dlgHideMessagesIcons;
  MWDblClickJumpsCheckBox.Caption:=lisEnvJumpFromMessageToSrcOnDblClickOtherwiseSingleClick;
end;

function TMsgWndOptionsFrame.GetTitle: String;
begin
  Result:=lisMenuViewMessages;
end;

procedure TMsgWndOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  fReady:=false;
  {$IFDEF EnableNewExtTools}
  {$ELSE}
  MWColorsGroupBox.Visible:=false;
  {$ENDIF}
end;

procedure TMsgWndOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  o: TEnvironmentOptions;
  c: TMsgWndColor;
begin
  o:=(AOptions as TEnvironmentOptions);
  for c in TMsgWndColor do
    MWColorListBox.Colors[ord(c)] := o.MsgViewColors[c];
  MWHideIconsCheckBox.Checked := o.HideMessagesIcons;
  MWDblClickJumpsCheckBox.Checked:=o.MsgViewDblClickJumps;

  fReady:=true;
end;

procedure TMsgWndOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  o: TEnvironmentOptions;
  c: TMsgWndColor;
begin
  o:=(AOptions as TEnvironmentOptions);
  for c in TMsgWndColor do
    o.MsgViewColors[c] := MWColorListBox.Colors[ord(c)];
  o.HideMessagesIcons := MWHideIconsCheckBox.Checked;
  o.MsgViewDblClickJumps := MWDblClickJumpsCheckBox.Checked;
end;

class function TMsgWndOptionsFrame.
  SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TMsgWndOptionsFrame, EnvOptionsMessages);
end.

