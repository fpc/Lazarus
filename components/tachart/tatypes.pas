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
  Classes, SysUtils, Graphics, Controls;

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

  TChartLegend = class(TPersistent)
  private
    FAlignment: TLegendAlignment;
    FFont: TFont;
    FFrame: TChartPen;
    FOwner: TCustomChart;
    FVisible: Boolean;

    procedure SetAlignment(AValue: TLegendAlignment);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetVisible(AValue: Boolean);

    procedure StyleChanged(Sender: TObject);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TLegendAlignment read FAlignment write SetAlignment;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TChartTitle = class(TPersistent)
  private
    FAlignment: TAlignment;
    FBrush: TBrush;
    FFont: TFont;
    FFrame: TChartPen;
    FOwner: TCustomChart;
    FText: TStrings;
    FVisible: Boolean;

    procedure SetAlignment(AValue: TAlignment);
    procedure SetBrush(AValue: TBrush);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetText(AValue: TStrings);
    procedure SetVisible(AValue: Boolean);

    procedure StyleChanged(Sender: TObject);
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
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TChartAxisTitle = class(TPersistent)
  private
    FAngle: Integer;
    FCaption: String;
    FFont: TFont;
    FOwner: TCustomChart;

    procedure SetAngle(AValue: Integer);
    procedure SetCaption(AValue: String);
    procedure SetFont(AValue: TFont);

    procedure StyleChanged(Sender: TObject);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Angle: Integer read FAngle write SetAngle;
    property Caption: String read FCaption write SetCaption;
    property Font: TFont read FFont write SetFont;
  end;

  TChartAxis = class(TPersistent)
  private
    FGrid: TChartPen;
    FInverted: Boolean;
    FOwner: TCustomChart;
    FTitle: TChartAxisTitle;
    FVisible: Boolean;

    procedure SetGrid(AValue: TChartPen);
    procedure SetInverted(AValue: Boolean);
    procedure SetTitle(AValue: TChartAxisTitle);
    procedure SetVisible(AValue: Boolean);

    procedure StyleChanged(Sender: TObject);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Grid: TChartPen read FGrid write SetGrid;
    //Inverts the axis scale from increasing to decreasing
    property Inverted: boolean read FInverted write SetInverted;
    property Title: TChartAxisTitle read FTitle write SetTitle;
    property Visible: Boolean read FVisible write SetVisible;
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
  inherited create;
  FAlignment := laRight;
  FOwner := AOwner;
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

procedure TChartLegend.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.StyleChanged(Sender: TObject);
begin
  FOwner.Invalidate;
end;

{ TChartTitle }

procedure TChartTitle.Assign(Source: TPersistent);
begin
  if Source is TChartTitle then
    with TChartLegend(Source) do begin
      Self.FBrush.Assign(Brush);
      Self.FFont.Assign(Font);
      Self.FFrame.Assign(Frame);
      Self.FText.Assign(Text);
      Self.FVisible := FVisible;
   end;

  inherited Assign(Source);
end;

constructor TChartTitle.Create(AOwner: TCustomChart);
begin
  inherited Create;
  FOwner := AOwner;

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

procedure TChartTitle.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
  StyleChanged(Self);
end;

procedure TChartTitle.StyleChanged(Sender: TObject);
begin
  FOwner.Invalidate;
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
  inherited Create;
  FOwner := AOwner;
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

procedure TChartAxisTitle.StyleChanged(Sender: TObject);
begin
  FOwner.Invalidate;
end;

{ TChartAxis }

procedure TChartAxis.Assign(Source: TPersistent);
begin
  if Source is TChartAxis then
    with TChartAxis(Source) do begin
      FTitle.Assign(Title);
      FVisible := Visible;
    end;
  inherited Assign(Source);
end;

constructor TChartAxis.Create(AOwner: TCustomChart);
begin
  inherited Create;
  FOwner := AOwner;
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

procedure TChartAxis.SetVisible(AValue: boolean);
begin
  FVisible := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.StyleChanged(Sender: TObject);
begin
  FOwner.Invalidate;
end;

end.

