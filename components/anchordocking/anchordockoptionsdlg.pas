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
  StdCtrls, ComCtrls, LCLProc, AnchorDocking, AnchorDockStr;

type
  TAnchorDockOptionsFlag = (
    adofShow_ShowHeader
    );
  TAnchorDockOptionsFlags = set of TAnchorDockOptionsFlag;

  { TAnchorDockOptionsFrame }

  TAnchorDockOptionsFrame = class(TFrame)
    DragThresholdLabel: TLabel;
    DragThresholdTrackBar: TTrackBar;
    HeaderAlignLeftLabel: TLabel;
    HeaderAlignLeftTrackBar: TTrackBar;
    HeaderAlignTopLabel: TLabel;
    HeaderAlignTopTrackBar: TTrackBar;
    HideHeaderCaptionForFloatingCheckBox: TCheckBox;
    ScaleOnResizeCheckBox: TCheckBox;
    ShowHeaderCaptionCheckBox: TCheckBox;
    ShowHeaderCheckBox: TCheckBox;
    SplitterWidthLabel: TLabel;
    SplitterWidthTrackBar: TTrackBar;
    procedure OkClick(Sender: TObject);
    procedure DragThresholdTrackBarChange(Sender: TObject);
    procedure HeaderAlignLeftTrackBarChange(Sender: TObject);
    procedure HeaderAlignTopTrackBarChange(Sender: TObject);
    procedure SplitterWidthTrackBarChange(Sender: TObject);
  private
    FFlags: TAnchorDockOptionsFlags;
    FMaster: TAnchorDockMaster;
    FSettings: TAnchorDockSettings;
    procedure SetFlags(AValue: TAnchorDockOptionsFlags);
    procedure SetMaster(const AValue: TAnchorDockMaster);
    procedure SetSettings(AValue: TAnchorDockSettings);
    procedure UpdateDragThresholdLabel;
    procedure UpdateHeaderAlignTopLabel;
    procedure UpdateHeaderAlignLeftLabel;
    procedure UpdateSplitterWidthLabel;
    procedure ApplyFlags;
  public
    procedure SaveToMaster;
    procedure LoadFromMaster;
    procedure SaveToSettings(TheSettings: TAnchorDockSettings);
    procedure LoadFromSettings(TheSettings: TAnchorDockSettings);
    property Master: TAnchorDockMaster read FMaster write SetMaster;
    property Settings: TAnchorDockSettings read FSettings write SetSettings;
    property Flags: TAnchorDockOptionsFlags read FFlags write SetFlags;
  end;

function ShowAnchorDockOptions(ADockMaster: TAnchorDockMaster): TModalResult;

implementation

function ShowAnchorDockOptions(ADockMaster: TAnchorDockMaster): TModalResult;
var
  Dlg: TForm;
  OptsFrame: TAnchorDockOptionsFrame;
  BtnPanel: TButtonPanel;
begin
  Dlg:=TForm.Create(nil);
  try
    Dlg.DisableAutoSizing;
    Dlg.Position:=poScreenCenter;
    Dlg.AutoSize:=true;
    Dlg.Caption:=adrsGeneralDockingOptions;

    OptsFrame:=TAnchorDockOptionsFrame.Create(Dlg);
    OptsFrame.Align:=alClient;
    OptsFrame.Parent:=Dlg;
    OptsFrame.Master:=ADockMaster;

    BtnPanel:=TButtonPanel.Create(Dlg);
    BtnPanel.ShowButtons:=[pbOK, pbCancel];
    BtnPanel.OKButton.OnClick:=@OptsFrame.OkClick;
    BtnPanel.Parent:=Dlg;
    Dlg.EnableAutoSizing;
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TAnchorDockOptionsFrame }

procedure TAnchorDockOptionsFrame.HeaderAlignLeftTrackBarChange(Sender: TObject);
begin
  UpdateHeaderAlignLeftLabel;
end;

procedure TAnchorDockOptionsFrame.HeaderAlignTopTrackBarChange(Sender: TObject);
begin
  UpdateHeaderAlignTopLabel;
end;

procedure TAnchorDockOptionsFrame.SplitterWidthTrackBarChange(Sender: TObject);
begin
  UpdateSplitterWidthLabel;
end;

procedure TAnchorDockOptionsFrame.OkClick(Sender: TObject);
begin
  if Settings<>nil then
    SaveToSettings(Settings);
  if Master<>nil then
    SaveToMaster;
end;

procedure TAnchorDockOptionsFrame.DragThresholdTrackBarChange(Sender: TObject);
begin
  UpdateDragThresholdLabel;
end;

procedure TAnchorDockOptionsFrame.SetMaster(const AValue: TAnchorDockMaster);
begin
  if FMaster=AValue then exit;
  FMaster:=AValue;
  if Master<>nil then
    LoadFromMaster;
end;

procedure TAnchorDockOptionsFrame.SetFlags(AValue: TAnchorDockOptionsFlags);
begin
  if FFlags=AValue then Exit;
  FFlags:=AValue;
  ApplyFlags;
end;

procedure TAnchorDockOptionsFrame.SetSettings(AValue: TAnchorDockSettings);
begin
  if FSettings=AValue then Exit;
  FSettings:=AValue;
  if Settings<>nil then
    LoadFromSettings(Settings);
end;

procedure TAnchorDockOptionsFrame.UpdateDragThresholdLabel;
begin
  DragThresholdLabel.Caption:=adrsDragThreshold
                             +' ('+IntToStr(DragThresholdTrackBar.Position)+')';
end;

procedure TAnchorDockOptionsFrame.UpdateHeaderAlignTopLabel;
begin
  HeaderAlignTopLabel.Caption:=adrsHeaderAlignTop
                            +' ('+IntToStr(HeaderAlignTopTrackBar.Position)+')';
end;

procedure TAnchorDockOptionsFrame.UpdateHeaderAlignLeftLabel;
begin
  HeaderAlignLeftLabel.Caption:=adrsHeaderAlignLeft
                           +' ('+IntToStr(HeaderAlignLeftTrackBar.Position)+')';
end;

procedure TAnchorDockOptionsFrame.UpdateSplitterWidthLabel;
begin
  SplitterWidthLabel.Caption:=adrsSplitterWidth
                             +' ('+IntToStr(SplitterWidthTrackBar.Position)+')';
end;

procedure TAnchorDockOptionsFrame.ApplyFlags;
begin
  ShowHeaderCheckBox.Visible:=adofShow_ShowHeader in Flags;
end;

procedure TAnchorDockOptionsFrame.SaveToMaster;
var
  CurSettings: TAnchorDockSettings;
begin
  CurSettings:=TAnchorDockSettings.Create;
  try
    SaveToSettings(CurSettings);
    Master.LoadSettings(CurSettings);
  finally
    CurSettings.Free;
  end;
end;

procedure TAnchorDockOptionsFrame.LoadFromMaster;
var
  CurSettings: TAnchorDockSettings;
begin
  CurSettings:=TAnchorDockSettings.Create;
  try
    Master.SaveSettings(CurSettings);
    LoadFromSettings(CurSettings);
  finally
    CurSettings.Free;
  end;
end;

procedure TAnchorDockOptionsFrame.SaveToSettings(
  TheSettings: TAnchorDockSettings);
begin
  TheSettings.DragTreshold:=DragThresholdTrackBar.Position;
  TheSettings.HeaderAlignTop:=HeaderAlignTopTrackBar.Position;
  TheSettings.HeaderAlignLeft:=HeaderAlignLeftTrackBar.Position;
  TheSettings.SplitterWidth:=SplitterWidthTrackBar.Position;
  TheSettings.ScaleOnResize:=ScaleOnResizeCheckBox.Checked;
  TheSettings.ShowHeader:=ShowHeaderCheckBox.Checked;
  TheSettings.ShowHeaderCaption:=ShowHeaderCaptionCheckBox.Checked;
  TheSettings.HideHeaderCaptionFloatingControl:=HideHeaderCaptionForFloatingCheckBox.Checked;
end;

procedure TAnchorDockOptionsFrame.LoadFromSettings(
  TheSettings: TAnchorDockSettings);
begin
  DragThresholdTrackBar.Hint:=
    adrsAmountOfPixelTheMouseHasToDragBeforeDragStarts;
  DragThresholdTrackBar.Position:=TheSettings.DragTreshold;
  UpdateDragThresholdLabel;

  HeaderAlignTopTrackBar.Hint:=
    adrsMoveHeaderToTopWhenWidthHeight100HeaderAlignTop;
  HeaderAlignTopTrackBar.Position:=TheSettings.HeaderAlignTop;
  UpdateHeaderAlignTopLabel;

  HeaderAlignLeftTrackBar.Hint:=
    adrsMoveHeaderToLeftWhenWidthHeight100HeaderAlignLeft;
  HeaderAlignLeftTrackBar.Position:=TheSettings.HeaderAlignLeft;
  UpdateHeaderAlignLeftLabel;

  SplitterWidthTrackBar.Hint:=adrsSplitterThickness;
  SplitterWidthTrackBar.Position:=TheSettings.SplitterWidth;
  UpdateSplitterWidthLabel;

  ScaleOnResizeCheckBox.Caption:=adrsScaleOnResize;
  ScaleOnResizeCheckBox.Hint:=adrsScaleSubSitesWhenASiteIsResized;
  ScaleOnResizeCheckBox.Checked:=TheSettings.ScaleOnResize;

  ShowHeaderCheckBox.Caption:=adrsShowHeaders;
  ShowHeaderCheckBox.Hint:=
    adrsEachDockedWindowHasAHeaderThatAllowsDraggingHasACo;
  ShowHeaderCheckBox.Checked:=TheSettings.ShowHeader;

  ShowHeaderCaptionCheckBox.Caption:=adrsShowHeaderCaptions;
  ShowHeaderCaptionCheckBox.Hint:=adrsShowCaptionsOfDockedControlsInTheHeader;
  ShowHeaderCaptionCheckBox.Checked:=TheSettings.ShowHeaderCaption;

  HideHeaderCaptionForFloatingCheckBox.Caption:=adrsNoCaptionsForFloatingSites;
  HideHeaderCaptionForFloatingCheckBox.Hint:=
    adrsHideHeaderCaptionsForSitesWithOnlyOneDockedControl;
  HideHeaderCaptionForFloatingCheckBox.Checked:=
    TheSettings.HideHeaderCaptionFloatingControl;
end;

end.

