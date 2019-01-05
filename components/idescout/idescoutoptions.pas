{ IDE options frame for IDE Scout options

  Copyright (C) 2018  Michael van Canneyt  michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit IDEScoutOptions;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, Dialogs, Spin, ExtCtrls, ColorBox,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, frmScout, IDEScoutStrConsts;


Type
  { TIDEScoutOptionsFrame }

  TIDEScoutOptionsFrame = class(TAbstractIDEOptionsEditor)
    CGSearch: TCheckGroup;
    CBShowShortCut: TCheckBox;
    CBMatchColor: TColorBox;
    CBShortCutColor: TColorBox;
    CBShowCategory: TCheckBox;
    CBSelectComponent: TCheckBox;
    GBColors: TGroupBox;
    GBComponents: TGroupBox;
    LSEComponentDefaultWidth: TLabel;
    LSEComponentDefaultHeight: TLabel;
    LCBShortCut: TLabel;
    LCBMatchColor: TLabel;
    GBOptions: TGroupBox;
    SEComponentDefaultWidth: TSpinEdit;
    SEComponentDefaultHeight: TSpinEdit;
    procedure CGSearchItemClick(Sender: TObject; Index: integer);
  private
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TIDEScoutOptionsFrame }

procedure TIDEScoutOptionsFrame.CGSearchItemClick(Sender: TObject;
  Index: integer);
begin
  if Index=Ord(stComponents) then
    GBComponents.Enabled:=CGsearch.Checked[Index];
end;

function TIDEScoutOptionsFrame.GetTitle: String;
begin
  Result:=isrsIDEScout;
end;

procedure TIDEScoutOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  CGSearch.Caption:=isrsSearchScope;
  CGSearch.Items[0]:=isrsCommands;
  CGSearch.Items[1]:=isrsRecentProjects;
  CGSearch.Items[2]:=isrsRecentFiles;
  CGSearch.Items[3]:=isrsRecentPackages;
  CGSearch.Items[4]:=isrsComponents;

  GBOptions.Caption:=isrsOptions;
  CBShowShortCut.Caption:=isrsShowShOrtcutWhenAvailable;
  CBShowCategory.Caption:=isrsShowCategoryWhenAvailable;

  GBColors.Caption:=isrsColors;
  LCBMatchColor.Caption:=isrsMatches;
  LCBShortCut.Caption:=isrsShortcut;

  GBComponents.Caption:=isrsComponents;
  CBSelectComponent.Caption:=isrsOnlySelectOnComponentPalette;
  LSEComponentDefaultWidth.Caption:=isrsDefaultWidth;
  LSEComponentDefaultHeight.Caption:=isrsDefaultHeight;
end;

procedure TIDEScoutOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

Var
  st : TScoutTerrain;

begin
  for St in TScoutTerrain do
    CGSearch.Checked[Ord(St)]:=St in ScoutTerrains;
  CBSelectComponent.Checked:=Not TComponentItem.Drop;
  SEComponentDefaultHeight.Value:=TComponentItem.DefaultHeight;
  SEComponentDefaultWidth.Value:=TComponentItem.DefaultWidth;
  CBShowCategory.Checked  := ShowCmdCategory;
  CBShowShortCut.Checked  := ShowShortCutKey;
  CBMatchColor.Selected   := MatchColor;
  CBShortCutColor.Selected := KeyStrokeColor;
end;

procedure TIDEScoutOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

Var
  st : TScoutTerrain;
  STS : TScoutTerrains;

begin
  STS:=[];
  for ST in TScoutTerrain do
    if CGSearch.Checked[Ord(ST)] then
      Include(STS,ST);
  ScoutTerrains:=STS;
  TComponentItem.Drop:=not CBSelectComponent.Checked;
  TComponentItem.DefaultHeight:=SEComponentDefaultHeight.Value;
  TComponentItem.DefaultWidth:=SEComponentDefaultWidth.Value;
  ShowCmdCategory:=CBShowCategory.Checked;
  ShowShortCutKey:=CBShowShortCut.Checked;
  MatchColor:=CBMatchColor.Selected;
  KeyStrokeColor:=CBShortCutColor.Selected;
  SaveScoutOptions;
  ApplyScoutOptions;
end;

class function TIDEScoutOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

