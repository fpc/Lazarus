{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Alexander Klenin

}
unit TAStyles;

{$H+}

interface

uses
  Classes, Graphics, SysUtils, TAChartUtils, TADrawUtils;

type
  { TChartStyle }

  TChartStyle = class(TCollectionItem)
  private
    FBrush: TBrush;
    FPen: TPen;
    FFont: TFont;
    FRepeatCount: Cardinal;
    FText: String;
    FUseBrush: Boolean;
    FUsePen: Boolean;
    FUseFont: Boolean;
    procedure SetBrush(AValue: TBrush);
    procedure SetFont(AValue: TFont);
    procedure SetPen(AValue: TPen);
    procedure SetRepeatCount(AValue: Cardinal);
    procedure SetText(AValue: String);
    procedure SetUseBrush(AValue: Boolean);
    procedure SetUseFont(AValue: Boolean);
    procedure SetUsePen(AValue: Boolean);
    procedure StyleChanged(ASender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public
    procedure Apply(ADrawer: IChartDrawer);
    procedure Assign(Source: TPersistent); override;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
    property RepeatCount: Cardinal
      read FRepeatCount write SetRepeatCount default 1;
    property Text: String read FText write SetText;
    property UseBrush: Boolean read FUseBrush write SetUseBrush default true;
    property UseFont: Boolean read FUseFont write SetUseFont default true;
    property UsePen: Boolean read FUsePen write SetUsePen default true;
  end;

  TChartStyles = class;

  TChartStyleEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TChartStyle;
    property Current: TChartStyle read GetCurrent;
  end;

  { TChartStyleList }

  TChartStyleList = class(TCollection)
  private
    FOwner: TChartStyles;
    function GetStyle(AIndex: Integer): TChartStyle;
  protected
    procedure Changed;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TChartStyles);
    function GetEnumerator: TChartStyleEnumerator; inline;
    property Style[AIndex: Integer]: TChartStyle read GetStyle; default;
  end;

  { TChartStyles }

  TChartStyles = class(TComponent)
  private
    FBroadcaster: TBroadcaster;
    FStyles: TChartStyleList;
    procedure SetStyles(AValue: TChartStyleList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Add: TChartStyle;
    procedure Apply(ADrawer: IChartDrawer; AIndex: Cardinal); overload;
    function StyleByIndex(AIndex: Cardinal): TChartStyle;
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

{ TChartStyleEnumerator }

function TChartStyleEnumerator.GetCurrent: TChartStyle;
begin
  Result := TChartStyle(inherited GetCurrent);
end;

{ TChartStyle }

procedure TChartStyle.Apply(ADrawer: IChartDrawer);
begin
  if UseBrush then
    ADrawer.Brush := Brush;
  if UseFont then
    ADrawer.Font := Font;
  if UsePen then
    ADrawer.Pen := Pen;
end;

procedure TChartStyle.Assign(Source: TPersistent);
begin
  if Source is TChartStyle then
    with Source as TChartStyle do begin
      Self.Brush := Brush;
      Self.Font := Font;
      Self.Pen := Pen;
    end;
  inherited Assign(Source);
end;

constructor TChartStyle.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FBrush := TBrush.Create;
  FBrush.OnChange := @StyleChanged;
  FFont := TFont.Create;
  FFont.OnChange := @StyleChanged;
  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
  FRepeatCount := 1;
  FUseBrush := true;
  FUseFont := true;
  FUsePen := true;
end;

destructor TChartStyle.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FFont);
  FreeAndNil(FPen);
  inherited Destroy;
end;

function TChartStyle.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
end;

procedure TChartStyle.SetBrush(AValue: TBrush);
begin
  if FBrush = AValue then exit;
  FBrush := AValue;
end;

procedure TChartStyle.SetFont(AValue: TFont);
begin
  if FFont = AValue then exit;
  FFont := AValue;
end;

procedure TChartStyle.SetPen(AValue: TPen);
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

procedure TChartStyle.SetText(AValue: String);
begin
  if FText = AValue then exit;
  FText := AValue;
  StyleChanged(Self);
end;

procedure TChartStyle.SetUseBrush(AValue: Boolean);
begin
  if FUseBrush = AValue then exit;
  FUseBrush := AValue;
  StyleChanged(Self);
end;

procedure TChartStyle.SetUseFont(AValue: Boolean);
begin
  if FUseFont = AValue then exit;
  FUseFont := AValue;
  StyleChanged(Self);
end;

procedure TChartStyle.SetUsePen(AValue: Boolean);
begin
  if FUsePen = AValue then exit;
  FUsePen := AValue;
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

function TChartStyleList.GetEnumerator: TChartStyleEnumerator;
begin
  Result := TChartStyleEnumerator.Create(Self);
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

function TChartStyles.Add: TChartStyle;
begin
  Result := TChartStyle(FStyles.Add);
end;

procedure TChartStyles.Apply(ADrawer: IChartDrawer; AIndex: Cardinal);
var
  style: TChartStyle;
begin
  style := StyleByIndex(AIndex);
  if style <> nil then
    style.Apply(ADrawer);
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

procedure TChartStyles.SetStyles(AValue: TChartStyleList);
begin
  if FStyles = AValue then exit;
  FStyles := AValue;
  Broadcaster.Broadcast(Self);
end;

function TChartStyles.StyleByIndex(AIndex: Cardinal): TChartStyle;
var
  totalRepeatCount: Cardinal = 0;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Styles.Count - 1 do
    totalRepeatCount += Styles[i].RepeatCount;
  if totalRepeatCount = 0 then
    exit;
  AIndex := AIndex mod totalRepeatCount;
  totalRepeatCount := 0;
  for i := 0 to Styles.Count - 1 do begin
    Result := Styles[i];
    totalRepeatCount += Result.RepeatCount;
    if AIndex < totalRepeatCount then exit;
  end;
end;

end.

