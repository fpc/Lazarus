{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael W. Vogel
}

unit DockedOptionsIDE;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils,
  // LazUtils
  LazConfigStorage, LazFileUtils, LazFileCache,
  // LCL
  LCLProc, ComCtrls, Graphics, DockedStrConsts,
  // IdeIntf
  BaseIDEIntf;

type
  TAnchorColor = (
    acControl,
    acControlBorder,
    acAnchorTop,
    acAnchorLeft,
    acAnchorRight,
    acAnchorBottom,
    acAnchorTarget);

const
  AnchorColorStr: array[TAnchorColor] of String = (
    SAnchorControlColorCaption,
    SAnchorControlBorderCaption,
    SAnchorTopColorCaption,
    SAnchorLeftColorCaption,
    SAnchorRightColorCaption,
    SAnchorBottomColorCaption,
    SAnchorTargetColorCaption);

type

  { TDockedOptions }

  TDockedOptions = class
  private const
    DefaultAnchorBorderColor  = clBtnShadow;
    DefaultAnchorControlColor = clBtnFace;
    DefaultAnchorTargetColor  = $00AAFF;
    DefaultAnchorTopColor     = $FF0000;
    DefaultAnchorLeftColor    = $FF00FF;
    DefaultAnchorRightColor   = $0000FF;
    DefaultAnchorBottomColor  = $FFAA00;
    DefaultResizerColor       = $AAFFAA;
  private
    FAnchorBorderColor: TColor;
    FAnchorBottomColor: TColor;
    FAnchorControlColor: TColor;
    FAnchorLeftColor: TColor;
    FAnchorRightColor: TColor;
    FAnchorTabVisible: Boolean;
    FAnchorTargetColor: TColor;
    FAnchorTopColor: TColor;
    FCaptureDistance: Integer;
    FChangeStamp: Integer;
    FForceRefreshing: Boolean;
    FLastSavedChangeStamp: Integer;
    FMouseBorderFactor: Integer;
    FResizerColor: TColor;
    FTabPosition: TTabPosition;
    FTreatAlign: Boolean;
    FTreatBorder: Boolean;
    function  GetModified: Boolean;
    procedure SetAnchorBorderColor(AValue: TColor);
    procedure SetAnchorBottomColor(AValue: TColor);
    procedure SetAnchorControlColor(AValue: TColor);
    procedure SetAnchorLeftColor(AValue: TColor);
    procedure SetAnchorRightColor(AValue: TColor);
    procedure SetAnchorTabVisible(AValue: Boolean);
    procedure SetAnchorTargetColor(AValue: TColor);
    procedure SetAnchorTopColor(AValue: TColor);
    procedure SetCaptureDistance(AValue: Integer);
    procedure SetForceRefreshing(AValue: Boolean);
    procedure SetModified(AValue: Boolean);
    procedure SetMouseBorderFactor(AValue: Integer);
    procedure SetResizerColor(AValue: TColor);
    procedure SetTabPosition(AValue: TTabPosition);
    procedure SetTreatAlign(AValue: Boolean);
    procedure SetTreatBorder(AValue: Boolean);
  public
    constructor Create;
    procedure SaveSafe;
    procedure LoadSafe;
    procedure SaveToFile(AFilename: String);
    procedure LoadFromFile(AFilename: String);
    procedure IncreaseChangeStamp;
  public
    property ChangeStamp: Integer read FChangeStamp;
    property Modified: Boolean read GetModified write SetModified;

    property AnchorBorderColor: TColor read FAnchorBorderColor write SetAnchorBorderColor;
    property AnchorControlColor: TColor read FAnchorControlColor write SetAnchorControlColor;
    property AnchorTabVisible: Boolean read FAnchorTabVisible write SetAnchorTabVisible;
    property AnchorTargetColor: TColor read FAnchorTargetColor write SetAnchorTargetColor;
    property AnchorTopColor: TColor read FAnchorTopColor write SetAnchorTopColor;
    property AnchorLeftColor: TColor read FAnchorLeftColor write SetAnchorLeftColor;
    property AnchorRightColor: TColor read FAnchorRightColor write SetAnchorRightColor;
    property AnchorBottomColor: TColor read FAnchorBottomColor write SetAnchorBottomColor;
    property CaptureDistance: Integer read FCaptureDistance write SetCaptureDistance;
    property ForceRefreshing: Boolean read FForceRefreshing write SetForceRefreshing;
    property MouseBorderFactor: Integer read FMouseBorderFactor write SetMouseBorderFactor;
    property ResizerColor: TColor read FResizerColor write SetResizerColor;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition;
    property TreatAlign: Boolean read FTreatAlign write SetTreatAlign;
    property TreatBorder: Boolean read FTreatBorder write SetTreatBorder;
  end;

const
  DockedOptionsFileName = 'dockedformeditoroptions.xml';
  DockedFileVersion = 1;

var
  DockedOptions: TDockedOptions = nil;

implementation

{ TDockedOptions }

function TDockedOptions.GetModified: Boolean;
begin
  Result := FLastSavedChangeStamp <> FChangeStamp;
end;

procedure TDockedOptions.SetAnchorBorderColor(AValue: TColor);
begin
  if FAnchorBorderColor = AValue then Exit;
  FAnchorBorderColor := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetAnchorBottomColor(AValue: TColor);
begin
  if FAnchorBottomColor = AValue then Exit;
  FAnchorBottomColor := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetAnchorControlColor(AValue: TColor);
begin
  if FAnchorControlColor = AValue then Exit;
  FAnchorControlColor := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetAnchorLeftColor(AValue: TColor);
begin
  if FAnchorLeftColor = AValue then Exit;
  FAnchorLeftColor := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetAnchorRightColor(AValue: TColor);
begin
  if FAnchorRightColor = AValue then Exit;
  FAnchorRightColor := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetAnchorTabVisible(AValue: Boolean);
begin
  if FAnchorTabVisible = AValue then Exit;
  FAnchorTabVisible := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetAnchorTargetColor(AValue: TColor);
begin
  if FAnchorTargetColor = AValue then Exit;
  FAnchorTargetColor := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetAnchorTopColor(AValue: TColor);
begin
  if FAnchorTopColor = AValue then Exit;
  FAnchorTopColor := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetCaptureDistance(AValue: Integer);
begin
  if FCaptureDistance = AValue then Exit;
  FCaptureDistance := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetForceRefreshing(AValue: Boolean);
begin
  if FForceRefreshing = AValue then Exit;
  FForceRefreshing := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetModified(AValue: Boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    FLastSavedChangeStamp := FChangeStamp;
end;

procedure TDockedOptions.SetMouseBorderFactor(AValue: Integer);
begin
  if FMouseBorderFactor = AValue then Exit;
  FMouseBorderFactor := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetResizerColor(AValue: TColor);
begin
  if FResizerColor = AValue then Exit;
  FResizerColor := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetTabPosition(AValue: TTabPosition);
begin
  if FTabPosition = AValue then Exit;
  FTabPosition := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetTreatAlign(AValue: Boolean);
begin
  if FTreatAlign = AValue then Exit;
  FTreatAlign := AValue;
  IncreaseChangeStamp;
end;

procedure TDockedOptions.SetTreatBorder(AValue: Boolean);
begin
  if FTreatBorder = AValue then Exit;
  FTreatBorder := AValue;
  IncreaseChangeStamp;
end;

constructor TDockedOptions.Create;
begin
  FAnchorBorderColor  := DefaultAnchorBorderColor;
  FAnchorControlColor := DefaultAnchorControlColor;
  FAnchorTabVisible   := True;
  FAnchorTargetColor  := DefaultAnchorTargetColor;
  FAnchorTopColor     := DefaultAnchorTopColor;
  FAnchorLeftColor    := DefaultAnchorLeftColor;
  FAnchorRightColor   := DefaultAnchorRightColor;
  FAnchorBottomColor  := DefaultAnchorBottomColor;
  FCaptureDistance    := 10;
  FForceRefreshing    := True;
  FMouseBorderFactor  := 1;
  FResizerColor       := DefaultResizerColor;
  FTabPosition        := tpTop;
  FTreatAlign         := True;
  FTreatBorder        := True;
end;

procedure TDockedOptions.SaveSafe;
begin
  try
    SaveToFile(DockedOptionsFileName);
    Modified := False;
  except
    on E: Exception do
      DebugLn(['Error: (lazarus) [TDockedOptions.SaveSafe] ', E.Message]);
  end;
end;

procedure TDockedOptions.LoadSafe;
begin
  try
    LoadFromFile(DockedOptionsFileName);
  except
    on E: Exception do
      DebugLn(['Error: (lazarus) [TDockedOptions.LoadSafe] ', E.Message]);
  end;
  Modified := False;
end;

procedure TDockedOptions.SaveToFile(AFilename: String);
var
  Cfg: TConfigStorage;
begin
  Cfg := GetIDEConfigStorage(AFilename, False);
  try
    Cfg.SetDeleteValue('AnchorBorderColor/Value',  AnchorBorderColor,  DefaultAnchorBorderColor);
    Cfg.SetDeleteValue('AnchorControlColor/Value', AnchorControlColor, DefaultAnchorControlColor);
    Cfg.SetDeleteValue('AnchorTopColor/Value',     AnchorTopColor,     DefaultAnchorTopColor);
    Cfg.SetDeleteValue('AnchorLeftColor/Value',    AnchorLeftColor,    DefaultAnchorLeftColor);
    Cfg.SetDeleteValue('AnchorRightColor/Value',   AnchorRightColor,   DefaultAnchorRightColor);
    Cfg.SetDeleteValue('AnchorBottomColor/Value',  AnchorBottomColor,  DefaultAnchorBottomColor);
    Cfg.SetDeleteValue('AnchorTargetColor/Value',  AnchorTargetColor,  DefaultAnchorTargetColor);
    Cfg.SetDeleteValue('AnchorTabVisible/Value',   AnchorTabVisible,   True);
    Cfg.SetDeleteValue('CaptureDistance/Value',    CaptureDistance,    10);
    Cfg.SetDeleteValue('ForceRefreshing/Value',    ForceRefreshing,    True);
    Cfg.SetDeleteValue('MouseBorderFactor/Value',  MouseBorderFactor,  1);
    Cfg.SetDeleteValue('ResizerColor/Value',       ResizerColor,       DefaultResizerColor);
    Cfg.SetDeleteValue('TabPosition/Value',        Integer(TabPosition), Integer(tpTop));
    Cfg.SetDeleteValue('TreatAlign/Value',         TreatAlign,         True);
    Cfg.SetDeleteValue('TreatBorder/Value',        TreatBorder,        True);
  finally
    Cfg.Free;
  end;
end;

procedure TDockedOptions.LoadFromFile(AFilename: String);
var
  Cfg: TConfigStorage;
begin
  Cfg := GetIDEConfigStorage(AFilename, True);
  try
    AnchorBorderColor  := Cfg.GetValue('AnchorBorderColor/Value',  DefaultAnchorBorderColor);
    AnchorControlColor := Cfg.GetValue('AnchorControlColor/Value', DefaultAnchorControlColor);
    AnchorTopColor     := Cfg.GetValue('AnchorTopColor/Value',     DefaultAnchorTopColor);
    AnchorLeftColor    := Cfg.GetValue('AnchorLeftColor/Value',    DefaultAnchorLeftColor);
    AnchorRightColor   := Cfg.GetValue('AnchorRightColor/Value',   DefaultAnchorRightColor);
    AnchorBottomColor  := Cfg.GetValue('AnchorBottomColor/Value',  DefaultAnchorBottomColor);
    AnchorTargetColor  := Cfg.GetValue('AnchorTargetColor/Value',  DefaultAnchorTargetColor);
    AnchorTabVisible   := Cfg.GetValue('AnchorTabVisible/Value',   True);
    CaptureDistance    := Cfg.GetValue('CaptureDistance/Value',    10);
    ForceRefreshing    := Cfg.GetValue('ForceRefreshing/Value',    True);
    MouseBorderFactor  := Cfg.GetValue('MouseBorderFactor/Value',  1);
    ResizerColor       := Cfg.GetValue('ResizerColor/Value',       DefaultResizerColor);
    TabPosition        := TTabPosition(Cfg.GetValue('TabPosition/Value', Integer(tpTop)));
    TreatAlign         := Cfg.GetValue('TreatAlign/Value',         True);
    TreatBorder        := Cfg.GetValue('TreatBorder/Value',        True);
  finally
    Cfg.Free;
  end;
end;

procedure TDockedOptions.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp(FChangeStamp);
end;

end.

