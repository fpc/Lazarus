{

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

 Authors: Alexander Klenin

}
unit TAStyles;

{$H+}

interface

uses
  Classes, Graphics, SysUtils, TAChartUtils;

type
  { TChartStyle }

  TChartStyle = class(TCollectionItem)
  private
    FBrush: TBrush;
    FPen: TPen;
    FRepeatCount: Cardinal;
    procedure SetBrush(const AValue: TBrush);
    procedure SetPen(const AValue: TPen);
    procedure SetRepeatCount(AValue: Cardinal);
    procedure StyleChanged(ASender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Apply(ACanvas: TCanvas);
  published
    property Brush: TBrush read FBrush write SetBrush;
    property Pen: TPen read FPen write SetPen;
    property RepeatCount: Cardinal
      read FRepeatCount write SetRepeatCount default 1;
  end;

  TChartStyles = class;

  { TChartStyleList }

  TChartStyleList = class(TCollection)
  private
    FOwner: TChartStyles;
    function GetStyle(AIndex: Integer): TChartStyle;
  protected
    function GetOwner: TPersistent; override;
    procedure Changed;
  public
    constructor Create(AOwner: TChartStyles);
    property Style[AIndex: Integer]: TChartStyle read GetStyle; default;
  end;

  { TChartStyles }

  TChartStyles = class(TComponent)
  private
    FBroadcaster: TBroadcaster;
    FStyles: TChartStyleList;
    procedure SetStyles(const AValue: TChartStyleList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Apply(ACanvas: TCanvas; AIndex: Cardinal);
    property Broadcaster: TBroadcaster read FBroadcaster;
  published
    property Styles: TChartStyleList read FStyles write SetStyles;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartStyles]);
end;

{ TChartStyle }

procedure TChartStyle.Apply(ACanvas: TCanvas);
begin
  ACanvas.Brush := Brush;
  ACanvas.Pen := Pen;
end;

procedure TChartStyle.Assign(Source: TPersistent);
begin
  if Source is TChartStyle then
    with Source as TChartStyle do begin
      Self.Brush := Brush;
      Self.Pen := Pen;
    end;
  inherited Assign(Source);
end;

constructor TChartStyle.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FBrush := TBrush.Create;
  FBrush.OnChange := @StyleChanged;
  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
  FRepeatCount := 1;
end;

destructor TChartStyle.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FPen);
  inherited Destroy;
end;

function TChartStyle.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
end;

procedure TChartStyle.SetBrush(const AValue: TBrush);
begin
  if FBrush = AValue then exit;
  FBrush := AValue;
end;

procedure TChartStyle.SetPen(const AValue: TPen);
begin
  if FPen = AValue then exit;
  FPen := AValue;
end;

procedure TChartStyle.SetRepeatCount(AValue: Cardinal);
begin
  if FRepeatCount = AValue then exit;
  FRepeatCount := AValue;
  StyleChanged(Self);
end;

procedure TChartStyle.StyleChanged(ASender: TObject);
begin
  Unused(ASender);
  TChartStyleList(Collection).Changed;
end;

{ TChartStyleList }

procedure TChartStyleList.Changed;
begin
  TChartStyles(Owner).Broadcaster.Broadcast(Self);
end;

constructor TChartStyleList.Create(AOwner: TChartStyles);
begin
  inherited Create(TChartStyle);
  FOwner := AOwner;
end;

function TChartStyleList.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TChartStyleList.GetStyle(AIndex: Integer): TChartStyle;
begin
  Result := Items[AIndex] as TChartStyle;
end;

{ TChartStyles }

procedure TChartStyles.Apply(ACanvas: TCanvas; AIndex: Cardinal);
var
  totalRepeatCount: Cardinal = 0;
  i: Integer;
begin
  for i := 0 to Styles.Count - 1 do
    totalRepeatCount += Styles[i].RepeatCount;
  if totalRepeatCount = 0 then exit;
  AIndex := AIndex mod totalRepeatCount;
  totalRepeatCount := 0;
  for i := 0 to Styles.Count - 1 do begin
    totalRepeatCount += Styles[i].RepeatCount;
    if AIndex < totalRepeatCount then begin
      Styles[i].Apply(ACanvas);
      break;
    end;
  end;
end;

constructor TChartStyles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBroadcaster := TBroadcaster.Create;
  FStyles := TChartStyleList.Create(Self);
end;

destructor TChartStyles.Destroy;
begin
  FreeAndNil(FBroadcaster);
  FreeAndNil(FStyles);
  inherited Destroy;
end;

procedure TChartStyles.SetStyles(const AValue: TChartStyleList);
begin
  if FStyles = AValue then exit;
  FStyles := AValue;
  Broadcaster.Broadcast(Self);
end;

end.

