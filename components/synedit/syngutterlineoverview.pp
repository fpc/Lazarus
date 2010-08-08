unit SynGutterLineOverview;

{$I synedit.inc}

interface

uses
  Classes, Graphics, Controls, LCLProc, LCLType, LCLIntf, sysutils, math,
  SynGutterBase, SynEditTypes, SynEditTextBase, SynEditTextBuffer, SynEditMiscClasses;

type
  TSynGutterLineOverview = class;
  TSynGutterLineOverviewProviderList = class;

  { TSynGutterLineOverviewProvider }

  TSynGutterLineOverviewProvider = class(TSynObjectListItem)
  private
    FColor: TColor;
    FHeight: Integer;
    FProviderList: TSynGutterLineOverviewProviderList;
    FGutterPart: TSynGutterLineOverview;
    FPriority: Integer;
    FRGBColor: TColor;
    function  GetList: TSynGutterLineOverviewProviderList;
    procedure SetColor(const AValue: TColor);
    procedure SetHeight(const AValue: Integer);
    procedure SetPriority(const AValue: Integer);
  protected
    function  Compare(Other: TSynObjectListItem): Integer; override;
    procedure DoChange(Sender: TObject);

    procedure InvalidateTextLines(AFromLine, AToLine: Integer);
    procedure InvalidatePixelLines(AFromLine, AToLine: Integer);
    function  TextLineToPixel(ALine: Integer): Integer;
    procedure ReCalc; virtual;                                                  // Does not invalidate
    function PixLineHeight: Integer;

    function  SynEdit: TSynEditBase;
    property  Owner: TSynGutterLineOverviewProviderList read GetList; //the list
    property  GutterPart: TSynGutterLineOverview read FGutterPart;
    property  RGBColor: TColor read FRGBColor;

    procedure Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property Height: Integer read FHeight write SetHeight;
  published
    property Priority: Integer read FPriority write SetPriority;
    property Color: TColor read FColor write SetColor;
  end;

  { TSynGutterLineOverviewProviderList }

  TSynGutterLineOverviewProviderList = class(TSynObjectList)
  private
    function GetGutterPart: TSynGutterLineOverview;
    function GetProviders(AIndex: Integer): TSynGutterLineOverviewProvider;
  public
    constructor Create(AOwner: TComponent); override;
    property Owner: TSynGutterLineOverview read GetGutterPart;
    property Providers[AIndex: Integer]: TSynGutterLineOverviewProvider
             read GetProviders; default;
  end;

  { TSynGutterLOvProviderCurrentPage }

  TSynGutterLOvProviderCurrentPage = class(TSynGutterLineOverviewProvider)
  private
    FCurTopLine, FCurLinesInWindow: Integer;
    FPixelTopLine, FPixelBottomLine: Integer;
  protected
    procedure SynStatusChanged(Sender: TObject; Changes: TSynStatusChanges);

    procedure Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer); override;
    procedure ReCalc; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

  TSynGutterLOvProviderModifiedLines = class(TSynGutterLineOverviewProvider)
  end;

  TSynGutterLOvProviderBookmarks = class(TSynGutterLineOverviewProvider)
  end;

  TSynGutterLOvProviderCustom = class(TSynGutterLineOverviewProvider)
  end;

  { TSynChildWinControl
    Allow individual invalidates, for less painting
  }

  TSynChildWinControl = class(TCustomControl)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSynGutterLineOverview }

  TSynGutterLineOverview = class(TSynGutterPartBase)
  private
    FProviders: TSynGutterLineOverviewProviderList;
    FWinControl: TSynChildWinControl;
  protected
    function  PreferedWidth: Integer; override;
    procedure Init; override;
    procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
    procedure BufferChanged(Sender: TObject);
    procedure SetVisible(const AValue : boolean); override;
    procedure DoChange(Sender: TObject); override;
  protected
    procedure InvalidateTextLines(AFromLine, AToLine: Integer);
    procedure InvalidatePixelLines(AFromLine, AToLine: Integer);
    function  TextLineToPixel(ALine: Integer): Integer;
    procedure DoResize(Sender: TObject); override;
    Procedure PaintWinControl(Sender: TObject);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    property Providers: TSynGutterLineOverviewProviderList read FProviders;
  published
    property MarkupInfo;
  end;

implementation
uses
  SynEdit;

{ TSynGutterLineOverviewProvider }

constructor TSynGutterLineOverviewProvider.Create(AOwner: TComponent);
begin
  inherited;
  FGutterPart := Owner.Owner;
  FColor := clGray;
  FriendEdit := SynEdit;
end;

destructor TSynGutterLineOverviewProvider.Destroy;
begin
  inherited Destroy;
end;

function TSynGutterLineOverviewProvider.GetList: TSynGutterLineOverviewProviderList;
begin
  Result := TSynGutterLineOverviewProviderList(inherited Owner);
end;

procedure TSynGutterLineOverviewProvider.SetColor(const AValue: TColor);
begin
  if FColor = AValue then exit;
  FColor := AValue;
  FRGBColor := ColorToRGB(AValue);
  DoChange(Self);
end;

procedure TSynGutterLineOverviewProvider.SetHeight(const AValue: Integer);
begin
  if FHeight = AValue then exit;
  FHeight := AValue;
  ReCalc;
end;

procedure TSynGutterLineOverviewProvider.SetPriority(const AValue: Integer);
begin
  if FPriority = AValue then exit;
  FPriority := AValue;
  Owner.Sort;
end;

function TSynGutterLineOverviewProvider.SynEdit: TSynEditBase;
begin
  Result := FGutterPart.SynEdit;
end;

function TSynGutterLineOverviewProvider.Compare(Other: TSynObjectListItem): Integer;
begin
  Result := Priority - TSynGutterLineOverviewProvider(Other).Priority;
  if Result = 0 then
    Result := inherited Compare(Other);
end;

procedure TSynGutterLineOverviewProvider.DoChange(Sender: TObject);
begin
  FGutterPart.DoChange(Sender);
end;

procedure TSynGutterLineOverviewProvider.InvalidateTextLines(AFromLine, AToLine: Integer);
begin
  FGutterPart.InvalidateTextLines(AFromLine, AToLine);
end;

procedure TSynGutterLineOverviewProvider.InvalidatePixelLines(AFromLine, AToLine: Integer);
begin
  FGutterPart.InvalidatePixelLines(AFromLine, AToLine);
end;

function TSynGutterLineOverviewProvider.TextLineToPixel(ALine: Integer): Integer;
var
  c: Integer;
begin
  if ALine < 0 then exit(-1);
  c := TextBuffer.Count;
  if c = 0 then
    Result := -1
  else
    Result := (ALine - 1) * Height div c;
end;

procedure TSynGutterLineOverviewProvider.ReCalc;
begin
  // nothing
end;

function TSynGutterLineOverviewProvider.PixLineHeight: Integer;
begin
  Result := 1;
end;

procedure TSynGutterLineOverviewProvider.Paint(Canvas: TCanvas; AClip: TRect;
  TopOffset: integer);
begin
  // nothing
end;

{ TSynGutterLineOverviewProviderList }

function TSynGutterLineOverviewProviderList.GetGutterPart: TSynGutterLineOverview;
begin
  Result := TSynGutterLineOverview(inherited Owner);
end;

function TSynGutterLineOverviewProviderList.GetProviders(AIndex: Integer): TSynGutterLineOverviewProvider;
begin
  Result := TSynGutterLineOverviewProvider(BaseItems[AIndex]);
end;

constructor TSynGutterLineOverviewProviderList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sorted := True;
end;

{ TSynGutterLOvProviderCurrentPage }

procedure TSynGutterLOvProviderCurrentPage.SynStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  InvalidatePixelLines(FPixelTopLine, FPixelBottomLine);
  FCurTopLine := TSynEdit(SynEdit).TopLine;
  FCurLinesInWindow := TSynEdit(SynEdit).LinesInWindow;
  ReCalc;
  InvalidatePixelLines(FPixelTopLine, FPixelBottomLine);
end;

procedure TSynGutterLOvProviderCurrentPage.ReCalc;
begin
  FPixelTopLine    := TextLineToPixel(FCurTopLine);
  FPixelBottomLine := TextLineToPixel(FCurTopLine + FCurLinesInWindow - 1) - 1 + PixLineHeight;
end;

procedure TSynGutterLOvProviderCurrentPage.Paint(Canvas: TCanvas; AClip: TRect;
  TopOffset: integer);
begin
  if (FPixelBottomLine < AClip.Top - TopOffset) or
     (FPixelTopLine > AClip.Bottom - TopOffset)
  then
    exit;

  AClip.Top    := Max(AClip.Top, FPixelTopLine+TopOffset);
  AClip.Bottom := Min(AClip.Bottom, FPixelBottomLine+TopOffset);
  Canvas.Brush.Color := FRGBColor;
  Canvas.FillRect(AClip);
end;

constructor TSynGutterLOvProviderCurrentPage.Create(AOwner: TComponent);
begin
  inherited;
  FColor := 0;
  Color := clGray;
  TSynEdit(SynEdit).RegisterStatusChangedHandler({$IFDEF FPC}@{$ENDIF}SynStatusChanged,
                                                 [scTopLine, scLinesInWindow]);
end;

destructor TSynGutterLOvProviderCurrentPage.Destroy;
begin
  TSynEdit(SynEdit).UnRegisterStatusChangedHandler({$IFDEF FPC}@{$ENDIF}SynStatusChanged);
  inherited;
end;

{ TSynChildWinControl }

constructor TSynChildWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  if AOwner is TWinControl then
    DoubleBuffered := TWinControl(AOwner).DoubleBuffered;
end;

{ TSynGutterLineOverview }

procedure TSynGutterLineOverview.Init;
begin
  inherited Init;
  TSynEditStringList(TextBuffer).AddGenericHandler(senrLineCount, TMethod({$IFDEF FPC}@{$ENDIF}LineCountChanged));
  TSynEditStringList(TextBuffer).AddGenericHandler(senrTextBufferChanged, TMethod({$IFDEF FPC}@{$ENDIF}BufferChanged));
  FWinControl := TSynChildWinControl.Create(Self);
  FWinControl.Parent := SynEdit;
  FWinControl.DoubleBuffered := SynEdit.DoubleBuffered;
  FWinControl.OnPaint := {$IFDEF FPC}@{$ENDIF}PaintWinControl;
  DoResize(Self);

  FProviders := TSynGutterLineOverviewProviderList.Create(Self);
  MarkupInfo.Background := clLtGray;
  LineCountchanged(nil, 0, 0);
end;

destructor TSynGutterLineOverview.Destroy;
begin
  TSynEditStringList(TextBuffer).RemoveHanlders(self);
  FreeAndNil(FProviders);
  FreeAndNil(FWinControl);
  inherited Destroy;
end;

procedure TSynGutterLineOverview.LineCountChanged(Sender: TSynEditStrings; AIndex,
  ACount: Integer);
var
  r: TRect;
begin
  if not SynEdit.HandleAllocated then exit;
  FWinControl.Invalidate;
end;

procedure TSynGutterLineOverview.BufferChanged(Sender: TObject);
begin
  TSynEditStringList(Sender).RemoveHanlders(self);
  TSynEditStringList(TextBuffer).AddGenericHandler(senrLineCount, TMethod({$IFDEF FPC}@{$ENDIF}LineCountChanged));
  TSynEditStringList(TextBuffer).AddGenericHandler(senrTextBufferChanged, TMethod({$IFDEF FPC}@{$ENDIF}BufferChanged));
  LineCountChanged(nil, 0, 0);
end;

procedure TSynGutterLineOverview.SetVisible(const AValue: boolean);
begin
  inherited SetVisible(AValue);
  FWinControl.Visible := Visible;
end;

procedure TSynGutterLineOverview.DoChange(Sender: TObject);
begin
  inherited;
  FWinControl.Invalidate;
end;

procedure TSynGutterLineOverview.InvalidateTextLines(AFromLine, AToLine: Integer);
begin
  InvalidatePixelLines(TextLineToPixel(AFromLine), TextLineToPixel(AToLine));
end;

procedure TSynGutterLineOverview.InvalidatePixelLines(AFromLine, AToLine: Integer);
var
  r: TRect;
begin
  if not SynEdit.HandleAllocated then exit;
  r := Rect(0, Top, Width, Top + Height);
  r.Top := AFromLine;
  r.Bottom := AToLine;
  InvalidateRect(FWinControl.Handle, @r, False);
end;

function TSynGutterLineOverview.TextLineToPixel(ALine: Integer): Integer;
var
  c: Integer;
begin
  if ALine < 0 then exit(-1);
  c := TextBuffer.Count;
  if c = 0 then
    Result := -1
  else
    Result := (ALine - 1) * Height div c;
end;

procedure TSynGutterLineOverview.DoResize(Sender: TObject);
var
  i: Integer;
begin
  inherited DoResize(Sender);
  if not SynEdit.HandleAllocated then exit;
  FWinControl.Top := Top;
  FWinControl.Left := Left;
  FWinControl.Width := Width;
  FWinControl.Height := Height;
  FWinControl.Invalidate;
  for i := 0 to FProviders.Count - 1 do
    FProviders[i].Height := Height;
end;

procedure TSynGutterLineOverview.PaintWinControl(Sender: TObject);
var
  i: Integer;
  AClip: TRect;
begin
  if not Visible then exit;
  AClip := FWinControl.Canvas.ClipRect;
  AClip.Left := 0;
  AClip.Right := Width;
  FWinControl.Canvas.Brush.Color := MarkupInfo.Background;
  FWinControl.Canvas.FillRect(AClip);

  for i := 0 to Providers.Count - 1 do
    Providers[i].Paint(FWinControl.Canvas, AClip, 0);
end;

function TSynGutterLineOverview.PreferedWidth: Integer;
begin
  Result := 10;
end;

procedure TSynGutterLineOverview.Assign(Source : TPersistent);
begin
  if Assigned(Source) and (Source is TSynGutterLineOverview) then
  begin
    inherited;
    // Todo: assign providerlist?
  end;
  inherited;
end;

procedure TSynGutterLineOverview.Paint(Canvas : TCanvas; AClip : TRect; FirstLine, LastLine : integer);
begin
  // do nothing
end;

end.

