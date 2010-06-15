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
  StdCtrls, ComCtrls, AnchorDocking, AnchorDockStr;

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
    procedure ButtonPanel1Click(Sender: TObject);
    procedure DragThresholdTrackBarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HeaderAlignLeftTrackBarChange(Sender: TObject);
    procedure HeaderAlignTopTrackBarChange(Sender: TObject);
    procedure SplitterWidthTrackBarChange(Sender: TObject);
  private
    FMaster: TAnchorDockMaster;
    procedure SetMaster(const AValue: TAnchorDockMaster);
    procedure UpdateDragThresholdLabel;
    procedure UpdateHeaderAlignTopLabel;
    procedure UpdateHeaderAlignLeftLabel;
    procedure UpdateSplitterWidthLabel;
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
  Caption:='General docking options';
  UpdateDragThresholdLabel;
  DragThresholdTrackBar.Hint:=
    adrsAmountOfPixelTheMouseHasToDragBeforeDragStarts;
  UpdateHeaderAlignTopLabel;
  HeaderAlignTopTrackBar.Hint:=
    adrsMoveHeaderToTopWhenWidthHeight100HeaderAlignTop;
  UpdateHeaderAlignLeftLabel;
  HeaderAlignLeftTrackBar.Hint:=
    adrsMoveHeaderToLeftWhenWidthHeight100HeaderAlignLeft;
  UpdateSplitterWidthLabel;
  SplitterWidthTrackBar.Hint:=adrsSplitterThickness;
  ScaleOnResizeCheckBox.Caption:=adrsScaleOnResize;
  ScaleOnResizeCheckBox.Hint:=adrsScaleSubSitesWhenASiteIsResized;

  ButtonPanel1.OKButton.ModalResult:=mrNone;
  ButtonPanel1.OKButton.OnClick:=@ButtonPanel1Click;
end;

procedure TAnchorDockOptionsDialog.HeaderAlignLeftTrackBarChange(Sender: TObject
  );
begin
  UpdateHeaderAlignLeftLabel;
end;

procedure TAnchorDockOptionsDialog.HeaderAlignTopTrackBarChange(Sender: TObject
  );
begin
  UpdateHeaderAlignTopLabel;
end;

procedure TAnchorDockOptionsDialog.SplitterWidthTrackBarChange(Sender: TObject);
begin
  UpdateSplitterWidthLabel;
end;

procedure TAnchorDockOptionsDialog.ButtonPanel1Click(Sender: TObject);
begin
  Master.DragTreshold    := DragThresholdTrackBar.Position;
  Master.HeaderAlignTop  := HeaderAlignTopTrackBar.Position;
  Master.HeaderAlignLeft := HeaderAlignLeftTrackBar.Position;
  Master.SplitterWidth   := SplitterWidthTrackBar.Position;
  Master.ScaleOnResize   := ScaleOnResizeCheckBox.Checked;
  ModalResult:=mrOk;
end;

procedure TAnchorDockOptionsDialog.DragThresholdTrackBarChange(Sender: TObject);
begin
  UpdateDragThresholdLabel;
end;

procedure TAnchorDockOptionsDialog.SetMaster(const AValue: TAnchorDockMaster);
begin
  if FMaster=AValue then exit;
  FMaster:=AValue;
  if Master<>nil then begin
    DragThresholdTrackBar.Position:=Master.DragTreshold;
    UpdateDragThresholdLabel;
    HeaderAlignTopTrackBar.Position:=Master.HeaderAlignTop;
    UpdateHeaderAlignTopLabel;
    HeaderAlignLeftTrackBar.Position:=Master.HeaderAlignLeft;
    UpdateHeaderAlignLeftLabel;
    SplitterWidthTrackBar.Position:=Master.SplitterWidth;
    UpdateSplitterWidthLabel;
    ScaleOnResizeCheckBox.Checked:=Master.ScaleOnResize;
  end;
end;

procedure TAnchorDockOptionsDialog.UpdateDragThresholdLabel;
begin
  DragThresholdLabel.Caption:=adrsDragThreshold
                             +' ('+IntToStr(DragThresholdTrackBar.Position)+')';
end;

procedure TAnchorDockOptionsDialog.UpdateHeaderAlignTopLabel;
begin
  HeaderAlignTopLabel.Caption:=adrsHeaderAlignTop
                            +' ('+IntToStr(HeaderAlignTopTrackBar.Position)+')';
end;

procedure TAnchorDockOptionsDialog.UpdateHeaderAlignLeftLabel;
begin
  HeaderAlignLeftLabel.Caption:=adrsHeaderAlignLeft
                           +' ('+IntToStr(HeaderAlignLeftTrackBar.Position)+')';
end;

procedure TAnchorDockOptionsDialog.UpdateSplitterWidthLabel;
begin
  SplitterWidthLabel.Caption:=adrsSplitterWidth
                             +' ('+IntToStr(SplitterWidthTrackBar.Position)+')';
end;

end.

