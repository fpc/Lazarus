{ IDE options frame for IDE Spotter options

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
unit IDESPotterOptions;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, Dialogs, Spin, ExtCtrls, ColorBox,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEUtils, frmSpotter;


Type
  { TIDESpotterOptionsFrame }

  TIDESpotterOptionsFrame = class(TAbstractIDEOptionsEditor)
    CGSearch: TCheckGroup;
    CBShowShortCut: TCheckBox;
    CBMatchColor: TColorBox;
    CBShortCutColor: TColorBox;
    CBShowCategory: TCheckBox;
    CBSelectComponent: TCheckBox;
    Colors: TGroupBox;
    GBComponents: TGroupBox;
    Label1: TLabel;
    LComponentheight: TLabel;
    LCBShortCut: TLabel;
    LCBMatchColor: TLabel;
    Options: TGroupBox;
    SEComponentDefaultWidth: TSpinEdit;
    SEComponentDefaultHeight: TSpinEdit;
    procedure CGSearchClick(Sender: TObject);
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

{ TIDESpotterOptionsFrame }

procedure TIDESpotterOptionsFrame.CGSearchClick(Sender: TObject);
begin

end;

procedure TIDESpotterOptionsFrame.CGSearchItemClick(Sender: TObject;
  Index: integer);
begin
  if Index=Ord(shComponents) then
    GBComponents.Enabled:=CGsearch.Checked[Index];
end;

function TIDESpotterOptionsFrame.GetTitle: String;
begin
  Result:='IDE Spotter';
end;

procedure TIDESpotterOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);


begin
  // Do nothing, maybe localize ?
end;

procedure TIDESpotterOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

Var
  sh : TSpotHighlight;

begin
  for SH in TSpotHighlight do
    CGSearch.Checked[Ord(SH)]:=SH in SpotHighlights;
  CBSelectComponent.Checked:=Not TComponentItem.Drop;
  SEComponentDefaultHeight.Value:=TComponentItem.DefaultHeight;
  SEComponentDefaultWidth.Value:=TComponentItem.DefaultWidth;
  CBShowCategory.Checked  := ShowCmdCategory;
  CBShowShortCut.Checked  := ShowShortCutKey;
  CBMatchColor.Selected   := MatchColor;
  CBShortCutColor.Selected := KeyStrokeColor;
end;

procedure TIDESpotterOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

Var
  sh : TSpotHighlight;
  SHS : TSpotHighlights;

begin
  SHS:=[];
  for SH in TSpotHighlight do
    if CGSearch.Checked[Ord(SH)] then
      Include(SHS,SH);
  SpotHighlights:=SHS;
  TComponentItem.Drop:=not CBSelectComponent.Checked;
  TComponentItem.DefaultHeight:=SEComponentDefaultHeight.Value;
  TComponentItem.DefaultWidth:=SEComponentDefaultWidth.Value;
  ShowCmdCategory:=CBShowCategory.Checked;
  ShowShortCutKey:=CBShowShortCut.Checked;
  MatchColor:=CBMatchColor.Selected;
  KeyStrokeColor:=CBShortCutColor.Selected;
  SaveSpotterOptions;
  ApplySpotterOptions;
end;

class function TIDESpotterOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

