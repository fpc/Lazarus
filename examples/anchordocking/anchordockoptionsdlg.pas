{ Unit implementing anchor docking.

  Copyright (C) 2010 Mattias Gaertner mattias@freepascal.org

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
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit AnchorDockOptionsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  Spin, StdCtrls, ComCtrls, AnchorDocking, AnchorDockStr;

type

  { TAnchorDockOptionsDialog }

  TAnchorDockOptionsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    DragThresholdLabel: TLabel;
    DragThresholdTrackBar: TTrackBar;
    HeaderAlignLeftLabel: TLabel;
    HeaderAlignLeftTrackBar: TTrackBar;
    HeaderAlignTopLabel: TLabel;
    HeaderAlignTopTrackBar: TTrackBar;
    ScaleOnResizeCheckBox: TCheckBox;
    SplitterWidthLabel: TLabel;
    SplitterWidthTrackBar: TTrackBar;
    procedure FormCreate(Sender: TObject);
  private
    FMaster: TAnchorDockMaster;
    procedure SetMaster(const AValue: TAnchorDockMaster);
  public
    property Master: TAnchorDockMaster read FMaster write SetMaster;
  end;

function ShowAnchorDockOptions(ADockMaster: TAnchorDockMaster): TModalResult;

implementation

function ShowAnchorDockOptions(ADockMaster: TAnchorDockMaster): TModalResult;
var
  Dlg: TAnchorDockOptionsDialog;
begin
  Dlg:=TAnchorDockOptionsDialog.Create(nil);
  try
    Dlg.Master:=ADockMaster;
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TAnchorDockOptionsDialog }

procedure TAnchorDockOptionsDialog.FormCreate(Sender: TObject);
begin
  DragThresholdLabel.Caption:=adrsDragThreshold;
  DragThresholdTrackBar.Hint:=adrsAmountOfPixelOfMouseMovementBeforeDragStarts;
  HeaderAlignTopLabel.Caption:=adrsHeaderAlignTop;
  HeaderAlignTopTrackBar.Hint:=
    adrsMoveHeaderToTopWhenWidthHeight100HeaderAlignTop;
  HeaderAlignLeftLabel.Caption:=adrsHeaderAlignLeft;
  HeaderAlignLeftTrackBar.Hint:=
    adrsMoveHeaderToLeftWhenWidthHeight100HeaderAlignLeft;
  SplitterWidthLabel.Caption:=adrsSplitterWidth;
  SplitterWidthTrackBar.Hint:=adrsSplitterThickness;
  ScaleOnResizeCheckBox.Caption:=adrsScaleOnResize;
  ScaleOnResizeCheckBox.Hint:=adrsScaleSubSitesWhenASiteIsResized;
end;

procedure TAnchorDockOptionsDialog.SetMaster(const AValue: TAnchorDockMaster);
begin
  if FMaster=AValue then exit;
  FMaster:=AValue;
  if Master<>nil then begin
    DragThresholdTrackBar.Position:=Master.DragTreshold;
    HeaderAlignTopTrackBar.Position:=Master.HeaderAlignTop;
    HeaderAlignLeftTrackBar.Position:=Master.HeaderAlignLeft;
    SplitterWidthTrackBar.Position:=Master.SplitterWidth;
    ScaleOnResizeCheckBox.Checked:=Master.ScaleOnResize;
  end;
end;

end.

