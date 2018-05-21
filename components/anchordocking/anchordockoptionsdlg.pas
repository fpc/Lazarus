{ For license see anchordocking.pas
}
unit AnchorDockOptionsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types,
  Forms, Controls, ButtonPanel, StdCtrls, ComCtrls, ExtCtrls, Spin,
  AnchorDocking, AnchorDockStr;

type
  TAnchorDockOptionsFlag = (
    adofShow_ShowHeader
    );
  TAnchorDockOptionsFlags = set of TAnchorDockOptionsFlag;

  { TAnchorDockOptionsFrame }

  TAnchorDockOptionsFrame = class(TFrame)
    DragThresholdSpinEdit: TSpinEdit;
    HeaderAlignLeftSpinEdit: TSpinEdit;
    HeaderAlignTopSpinEdit: TSpinEdit;
    HighlightFocused: TCheckBox;
    FlattenHeaders: TCheckBox;
    FilledHeaders: TCheckBox;
    DragThresholdLabel: TLabel;
    DragThresholdTrackBar: TTrackBar;
    HeaderAlignLeftLabel: TLabel;
    HeaderAlignLeftTrackBar: TTrackBar;
    HeaderAlignTopLabel: TLabel;
    HeaderAlignTopTrackBar: TTrackBar;
    HeaderStyleComboBox: TComboBox;
    HeaderStyleLabel: TLabel;
    HideHeaderCaptionForFloatingCheckBox: TCheckBox;
    ScaleOnResizeCheckBox: TCheckBox;
    ShowHeaderCaptionCheckBox: TCheckBox;
    ShowHeaderCheckBox: TCheckBox;
    SplitterWidthLabel: TLabel;
    SplitterWidthSpinEdit: TSpinEdit;
    SplitterWidthTrackBar: TTrackBar;
    procedure FrameClick(Sender: TObject);
    procedure HeaderStyleComboBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure OkClick(Sender: TObject);
    procedure DragThresholdTrackBarChange(Sender: TObject);
    procedure HeaderAlignLeftTrackBarChange(Sender: TObject);
    procedure HeaderAlignTopTrackBarChange(Sender: TObject);
    procedure ShowHeaderCheckBoxChange(Sender: TObject);
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
    procedure UpdateHeaderOptions;
    procedure ApplyFlags;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SaveToMaster;
    procedure LoadFromMaster;
    procedure SaveToSettings(TheSettings: TAnchorDockSettings);
    procedure LoadFromSettings(TheSettings: TAnchorDockSettings);
    property Master: TAnchorDockMaster read FMaster write SetMaster;
    property Settings: TAnchorDockSettings read FSettings write SetSettings;
    property Flags: TAnchorDockOptionsFlags read FFlags write SetFlags;
  end;

var
  DefaultAnchorDockOptionFlags: TAnchorDockOptionsFlags = [];

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
    Dlg.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('ShowAnchorDockOptions'){$ENDIF};
    try
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
    finally
      Dlg.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('ShowAnchorDockOptions'){$ENDIF};
    end;
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

procedure TAnchorDockOptionsFrame.ShowHeaderCheckBoxChange(Sender: TObject);
begin
  UpdateHeaderOptions;
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

procedure TAnchorDockOptionsFrame.HeaderStyleComboBoxDrawItem(
  Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  DrawADHeader(TComboBox(Control).Canvas,TADHeaderStyle(Index),ARect,true,False);
end;

procedure TAnchorDockOptionsFrame.FrameClick(Sender: TObject);
begin

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
  UpdateHeaderOptions;
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

procedure TAnchorDockOptionsFrame.UpdateHeaderOptions;
var
  HasHeaders: Boolean;
begin
  HasHeaders:=ShowHeaderCheckBox.Checked;
  ShowHeaderCaptionCheckBox.Enabled:=HasHeaders;
  HideHeaderCaptionForFloatingCheckBox.Enabled:=HasHeaders;
  FlattenHeaders.Enabled:=HasHeaders;
  FilledHeaders.Enabled:=HasHeaders;
end;

procedure TAnchorDockOptionsFrame.ApplyFlags;
begin
  ShowHeaderCheckBox.Visible:=adofShow_ShowHeader in Flags;
  if ShowHeaderCheckBox.Visible then
    ShowHeaderCaptionCheckBox.BorderSpacing.Left:=15
  else
    ShowHeaderCaptionCheckBox.BorderSpacing.Left:=0;
end;

constructor TAnchorDockOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FFlags:=DefaultAnchorDockOptionFlags;
end;

procedure TAnchorDockOptionsFrame.SaveToMaster;
var
  CurSettings: TAnchorDockSettings;
begin
  CurSettings:=TAnchorDockSettings.Create;
  try
    Master.SaveSettings(CurSettings);
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
  TheSettings.HeaderStyle:=TADHeaderStyle(HeaderStyleComboBox.ItemIndex);
  TheSettings.DragTreshold:=DragThresholdTrackBar.Position;
  TheSettings.HeaderAlignTop:=HeaderAlignTopTrackBar.Position;
  TheSettings.HeaderAlignLeft:=HeaderAlignLeftTrackBar.Position;
  TheSettings.SplitterWidth:=SplitterWidthTrackBar.Position;
  TheSettings.ScaleOnResize:=ScaleOnResizeCheckBox.Checked;
  TheSettings.ShowHeader:=ShowHeaderCheckBox.Checked;
  TheSettings.ShowHeaderCaption:=ShowHeaderCaptionCheckBox.Checked;
  TheSettings.HideHeaderCaptionFloatingControl:=HideHeaderCaptionForFloatingCheckBox.Checked;
  TheSettings.HeaderFlatten:=FlattenHeaders.Checked;
  TheSettings.HeaderFilled:=FilledHeaders.Checked;
  TheSettings.HeaderHighlightFocused:=HighlightFocused.Checked;
end;

procedure TAnchorDockOptionsFrame.LoadFromSettings(
  TheSettings: TAnchorDockSettings);
var
  hs: TADHeaderStyle;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    for hs:=Low(TADHeaderStyle) to High(TADHeaderStyle) do
      sl.Add(ADHeaderStyleNames[hs]);
    HeaderStyleComboBox.Items.Assign(sl);
  finally
    sl.Free;
  end;
  HeaderStyleLabel.Caption:=adrsHeaderStyle;
  HeaderStyleComboBox.ItemIndex:=ord(TheSettings.HeaderStyle);

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
  UpdateHeaderOptions;

  ShowHeaderCaptionCheckBox.Caption:=adrsShowHeaderCaptions;
  ShowHeaderCaptionCheckBox.Hint:=adrsShowCaptionsOfDockedControlsInTheHeader;
  ShowHeaderCaptionCheckBox.Checked:=TheSettings.ShowHeaderCaption;

  HideHeaderCaptionForFloatingCheckBox.Caption:=adrsNoCaptionsForFloatingSites;
  HideHeaderCaptionForFloatingCheckBox.Hint:=
    adrsHideHeaderCaptionsForSitesWithOnlyOneDockedControl;
  HideHeaderCaptionForFloatingCheckBox.Checked:=
    TheSettings.HideHeaderCaptionFloatingControl;

  FlattenHeaders.Checked:=TheSettings.HeaderFlatten;
  FlattenHeaders.Caption:=adrsFlattenHeaders;
  FlattenHeaders.Hint:=adrsFlattenHeadersHint;

  FilledHeaders.Checked:=TheSettings.HeaderFilled;
  FilledHeaders.Caption:=adrsFilledHeaders;
  FilledHeaders.Hint:=adrsFilledHeadersHint;

  HighlightFocused.Checked:=TheSettings.HeaderHighlightFocused;
  HighlightFocused.Caption:=adrsHighlightFocused;
  HighlightFocused.Hint:=adrsHighlightFocusedHint;
end;

end.

