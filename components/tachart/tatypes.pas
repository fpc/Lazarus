{
 /***************************************************************************
                               TATypes.pas
                               -----------
              Component Library Standard Graph Element Types


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Authors: Luнs Rodrigues, Philippe Martinole, Alexander Klenin

}
unit TATypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  TAChartUtils;

type
  TCustomChart = class(TCustomControl);

  TChartPen = class(TPen)
  private
    FVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TLegendAlignment = (laLeft, laRight, laTop, laBottom);

  { TChartElement }

  TChartElement = class(TPersistent)
  private
    FVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  protected
    FOwner: TCustomChart;
    procedure StyleChanged(Sender: TObject);
  public
    constructor Create(AOwner: TCustomChart);
    procedure Assign(Source: TPersistent); override;

    procedure SetOwner(AOwner: TCustomChart);

    property Visible: Boolean read FVisible write SetVisible;
  end;

  TChartLegend = class(TChartElement)
  private
    FAlignment: TLegendAlignment;
    FFont: TFont;
    FFrame: TChartPen;

    procedure SetAlignment(AValue: TLegendAlignment);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TLegendAlignment read FAlignment write SetAlignment;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Visible;
  end;

  TChartTitle = class(TChartElement)
  private
    FAlignment: TAlignment;
    FBrush: TBrush;
    FFont: TFont;
    FFrame: TChartPen;
    FText: TStrings;

    procedure SetAlignment(AValue: TAlignment);
    procedure SetBrush(AValue: TBrush);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetText(AValue: TStrings);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Text: TStrings read FText write SetText;
    property Visible;
  end;

  TChartAxisTitle = class(TChartElement)
  private
    FAngle: Integer;
    FCaption: String;
    FFont: TFont;

    procedure SetAngle(AValue: Integer);
    procedure SetCaption(AValue: String);
    procedure SetFont(AValue: TFont);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Angle: Integer read FAngle write SetAngle;
    property Caption: String read FCaption write SetCaption;
    property Font: TFont read FFont write SetFont;
  end;

  TChartAxis = class(TChartElement)
  private
    FGrid: TChartPen;
    FInverted: Boolean;
    FTitle: TChartAxisTitle;

    procedure SetGrid(AValue: TChartPen);
    procedure SetInverted(AValue: Boolean);
    procedure SetTitle(AValue: TChartAxisTitle);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Grid: TChartPen read FGrid write SetGrid;
    // Inverts the axis scale from increasing to decreasing.
    property Inverted: boolean read FInverted write SetInverted;
    property Title: TChartAxisTitle read FTitle write SetTitle;
    property Visible;
  end;

  { TChartMarks }

  TChartMarks = class(TChartElement)
  private
    FFormat: String;
    FStyle: TSeriesMarksStyle;
    procedure SetFormat(const AValue: String);
    procedure SetStyle(const AValue: TSeriesMarksStyle);
  public
    constructor Create(AOwner: TCustomChart);

    procedure Assign(Source: TPersistent); override;
    function IsMarkLabelsVisible: Boolean;
  published
    property Format: String read FFormat write SetFormat;
    property Style: TSeriesMarksStyle
      read FStyle write SetStyle default smsNone;
    property Visible default true;
  end;

implementation

{ TChartPen }

procedure TChartPen.Assign(Source: TPersistent);
begin
  if Source is TChartPen then
    with TChartPen(Source) do
      FVisible := Visible;
  inherited Assign( Source );
end;

procedure TChartPen.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
  if Assigned(OnChange) then OnChange(Self);
end;

{ TChartElement }

procedure TChartElement.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TChartElement then
    Self.FVisible := TChartElement(Source).FVisible;
end;

constructor TChartElement.Create(AOwner: TCustomChart);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TChartElement.SetOwner(AOwner: TCustomChart);
begin
  FOwner := AOwner;
end;

procedure TChartElement.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
  StyleChanged(Self);
end;

procedure TChartElement.StyleChanged(Sender: TObject);
begin
  if FOwner <> nil then
    FOwner.Invalidate;
end;

{ TChartLegend }

procedure TChartLegend.Assign(Source: TPersistent);
begin
  if Source is TChartLegend then
    with TChartLegend(Source) do begin
      Self.FAlignment := FAlignment;
      Self.FVisible := FVisible;
    end;

  inherited Assign(Source);
end;

constructor TChartLegend.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FAlignment := laRight;
  FVisible := false;

  FFont := TFont.Create;
  FFont.OnChange := @StyleChanged;
  FFrame := TChartPen.Create;
  FFrame.OnChange := @StyleChanged;
end;

destructor TChartLegend.Destroy;
begin
  FFont.Free;
  FFrame.Free;

  inherited;
end;

procedure TChartLegend.SetAlignment(AValue: TLegendAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetFrame(AValue: TChartPen);
begin
  FFrame.Assign(AValue);
  StyleChanged(Self);
end;

{ TChartTitle }

procedure TChartTitle.Assign(Source: TPersistent);
begin
  if Source is TChartTitle then
    with TChartTitle(Source) do begin
      Self.FAlignment := Alignment;
      Self.FBrush.Assign(Brush);
      Self.FFont.Assign(Font);
      Self.FFrame.Assign(Frame);
      Self.FText.Assign(Text);
   end;

  inherited Assign(Source);
end;

constructor TChartTitle.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);

  FBrush := TBrush.Create;
  FBrush.Color := FOwner.Color;
  FBrush.OnChange := @StyleChanged;
  FFont := TFont.Create;
  FFont.Color := clBlue;
  FFont.OnChange := @StyleChanged;
  FFrame := TChartPen.Create;
  FFrame.OnChange := @StyleChanged;
  FText := TStringList.Create;
end;

destructor TChartTitle.Destroy;
begin
  FBrush.Free;
  FFont.Free;
  FFrame.Free;
  FText.Free;

  inherited;
end;

procedure TChartTitle.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  StyleChanged(Self);
end;

procedure TChartTitle.SetBrush(AValue: TBrush);
begin
  FBrush.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartTitle.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartTitle.SetFrame(AValue: TChartPen);
begin
  FFrame.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartTitle.SetText(AValue: TStrings);
begin
  FText.Assign(AValue);
  StyleChanged(Self);
end;

{ TChartAxisTitle }

procedure TChartAxisTitle.Assign(Source: TPersistent);
begin
  if Source is TChartAxisTitle then
    with TChartAxisTitle(Source) do begin
      FCaption := Caption;
      FAngle := Angle;
      FFont.Assign(Font);
    end;
  inherited Assign(Source);
end;

constructor TChartAxisTitle.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.OnChange := @StyleChanged;
end;

destructor TChartAxisTitle.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TChartAxisTitle.SetAngle(AValue: Integer);
begin
  FAngle := AValue;
  StyleChanged(Self);
end;

procedure TChartAxisTitle.SetCaption(AValue: String);
begin
  FCaption := AValue;
  StyleChanged(Self);
end;

procedure TChartAxisTitle.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  StyleChanged(Self);
end;

{ TChartAxis }

procedure TChartAxis.Assign(Source: TPersistent);
begin
  if Source is TChartAxis then
    with TChartAxis(Source) do begin
      FGrid.Assign(Grid);
      FInverted := Inverted;
      FTitle.Assign(Title);
    end;
  inherited Assign(Source);
end;

constructor TChartAxis.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FTitle := TChartAxisTitle.Create(AOwner);
  FGrid := TChartPen.Create;
  FGrid.OnChange := @StyleChanged;
end;

destructor TChartAxis.Destroy;
begin
  FTitle.Free;
  FGrid.Free;
  inherited;
end;

procedure TChartAxis.SetGrid(AValue: TChartPen);
begin
  FGrid.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartAxis.SetInverted(AValue: Boolean);
begin
  FInverted := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetTitle(AValue: TChartAxisTitle);
begin
  FTitle.Assign(AValue);
  StyleChanged(Self);
end;

{ TChartMarks }

procedure TChartMarks.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TChartMarks then
    with TChartMarks(Source) do begin
      Self.FFormat := FFormat;
      Self.FStyle := FStyle;
    end;
end;

constructor TChartMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FStyle := smsNone;
  FVisible := true;
end;

function TChartMarks.IsMarkLabelsVisible: Boolean;
begin
  Result := Visible and (Style <> smsNone) and (Format <> '');
end;

procedure TChartMarks.SetFormat(const AValue: String);
begin
  if FFormat = AValue then exit;
  FFormat := AValue;
  FStyle := High(FStyle);
  while (FStyle > smsCustom) and (SERIES_MARK_FORMATS[FStyle] <> AValue) do
    Dec(FStyle);
  StyleChanged(Self);
end;

procedure TChartMarks.SetStyle(const AValue: TSeriesMarksStyle);
begin
  if FStyle = AValue then exit;
  FStyle := AValue;
  if FStyle <> smsCustom then
    FFormat := SERIES_MARK_FORMATS[FStyle];
  StyleChanged(Self);
end;

end.

