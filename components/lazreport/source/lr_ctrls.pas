
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Tool controls              }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Ctrls;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,LMessages,Messages,
  Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls,Menus,comctrls,
  
  LR_Fpc,

  GraphType,LCLType,LCLIntf;

type
  TfrButtonState = (fbsUp, fbsDisabled, fbsDown, fbsExclusive, fbsInactive);

  TfrSpeedButton = class(TGraphicControl)
  private
    FGroupIndex: Integer;
    FGlyph: Pointer;
    FDown: Boolean;
    FFlat: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FInactiveGrayed: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure UpdateExclusive;
    procedure SetDown(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetInactiveGrayed(Value: Boolean);
    procedure UpdateTracking;
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure WMLButtonDblClk(var Message: TLMLButtonDown); message LM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    FMouseInControl: Boolean;
    FState: TfrButtonState;
    function GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure DrawGlyph(aCanvas:TCanvas; X,Y:Integer; aEnabled:Boolean);
  published
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Caption;
    property Enabled;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GrayedInactive: Boolean read FInactiveGrayed write SetInactiveGrayed
      default True;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

  TfrTBSeparator = class(TGraphicControl)
  protected
    FDrawBevel: Boolean;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetDrawBevel(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Align;
    property DrawBevel: Boolean read FDrawBevel write SetDrawBevel default True;
    property Height;
    property Width;
  end;

  TfrTBPanel = class(TPanel)
  protected
    procedure SetParent(AParent:TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  end;

  TfrTBButton = class(TfrSpeedButton)
  protected
    procedure SetParent(AParent:TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Flat default True;
  end;

procedure Register;

implementation

type
  THackBitmap=Class(TBitmap);
  
  TGlyphList = class(TImageList)
  private
    Used: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
{$IFDEF Delphi2}
    function Add(Image, Mask: TBitmap): Integer;
    procedure ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
{$ENDIF}
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

  TGlyphCache = class
  private
    GlyphLists: TFpList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;

  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexs: array[TfrButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function MapColor(Color: TColor): TColor;
    function CreateButtonGlyph(State: TfrButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
      State: TfrButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TfrButtonState);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      var GlyphPos: TPoint; var TextBounds: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TfrButtonState; Transparent: Boolean): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TGlyphList }

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;

destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

{$IFDEF Delphi2}
function TGlyphList.Add(Image, Mask: TBitmap): Integer;
begin
  Result := AllocateIndex;
  Replace(Result, Image, Mask);
  Inc(FCount);
end;

procedure TGlyphList.ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
var
  TempIndex: Integer;
  Image, Mask: TBitmap;
begin
  if HandleAllocated then begin
    TempIndex := inherited AddMasked(NewImage, MaskColor);
    if TempIndex <> -1 then
    try
      Image := TBitmap.Create;
      Mask := TBitmap.Create;
      try
        with Image do begin
          Height := Self.Height;
          Width := Self.Width;
        end;
        with Mask do begin
          Monochrome := True; { fix }
          Height := Self.Height;
          Width := Self.Width;
        end;
        ImageList_Draw(Handle, TempIndex, Image.Canvas.Handle, 0, 0, ILD_NORMAL);
        ImageList_Draw(Handle, TempIndex, Mask.Canvas.Handle, 0, 0, ILD_MASK);
        ImageList_Replace(Handle, Index, Image.Handle, Mask.Handle);
      finally
        Image.Free;
        Mask.Free;
      end;
    finally
      inherited Delete(TempIndex);
    end
  end;
  Change;
end;
{$ENDIF}

procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;

{ TGlyphCache }

constructor TGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TFpList.Create;
end;

destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := TGlyphList(GlyphLists[I]);
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;

procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;


var
  GlyphCache: TGlyphCache = nil;
  Pattern: TBitmap = nil;
  ButtonCount: Integer = 0;
  CacheBitmap: TBitmap = nil;


procedure CreateBrushPattern;
var
  X, Y: Integer;
begin
  Pattern := TBitmap.Create;
  Pattern.Width := 8;
  Pattern.Height := 8;
  with Pattern.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect(Rect(0, 0, Pattern.Width, Pattern.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then  { toggles between even/odd pixles }
          Pixels[X, Y] := clWhite;     { on even/odd rows }
  end;
  CacheBitmap := TBitmap.Create;
  CacheBitmap.Width := 100; CacheBitmap.Height := 100;
end;

{ TButtonGlyph }

constructor TButtonGlyph.Create;
var
  I: TfrButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := @GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
end;

destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

procedure TButtonGlyph.Invalidate;
var
  I: TfrButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
  end;
end;

function TButtonGlyph.MapColor(Color: TColor): TColor;
var
  Index: Byte;
begin
  if (Color = FTransparentColor) or (ColorToRGB(Color) =
    ColorToRGB(clBtnFace)) then Result := Color
  else begin
    Color := ColorToRGB(Color);
    Index := Byte(Longint(Word(GetRValue(Color)) * 77 +
      Word(GetGValue(Color)) * 150 + Word(GetBValue(Color)) * 29) shr 8);
    Result := RGB(Index, Index, Index);
  end;
end;

function TButtonGlyph.CreateButtonGlyph(State: TfrButtonState): Integer;
const
  ROP_DSPDxax = $00E20746;
var
  TmpImage, DDB, MonoBmp: TBitmap;
  IWidth, IHeight, X, Y: Integer;
  IRect, ORect: TRect;
  I: TfrButtonState;
  DestDC: HDC;
begin
  if (State = fbsDown) and (NumGlyphs < 3) then State := fbsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then I := fbsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      fbsUp, fbsDown, fbsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor);
        end;
      fbsInactive:
        begin
          TmpImage.Canvas.BrushCopy(IRect, FOriginal, ORect, FTransparentColor);
          with TmpImage do
            for X := 0 to Width - 1 do
              for Y := 0 to Height - 1 do
                Canvas.Pixels[X, Y] := MapColor(Canvas.Pixels[X, Y]);
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, ColorToRGB(clBtnFace));
        end;
      fbsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
{$IFNDEF Delphi2}
            DDB.HandleType := bmDDB;
{$ENDIF}
            if NumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, DDB.Canvas, ORect);
              MonoBmp.Monochrome := True;
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;

              { Convert white to clBtnHighlight }
              DDB.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
              DDB.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to clBtnFace }
              DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnFace;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
{$IFNDEF Delphi2}
                HandleType := bmDDB;
{$ENDIF}
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
{$IFNDEF Delphi2}
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
{$ELSE}
          FIndexs[State] := FGlyphList.Add(TmpImage, nil);
{$ENDIF}
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  THackBitmap(fOriginal).Changing(nil);
end;

procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
  State: TfrButtonState; Transparent: Boolean);
var
  Index: Integer;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State);
  //** if Transparent then
  FGlyphList.Draw(Canvas,X,Y,Index,True);

  //**
  {  ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
      clNone, clNone, ILD_Transparent)
  else
    ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
      ColorToRGB(clBtnFace), clNone, ILD_Normal);
  }
end;

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TfrButtonState);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = fbsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clWhite;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, 0);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clDkGray;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, 0);
    end else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  var GlyphPos: TPoint; var TextBounds: TRect);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height) else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left);
  Inc(GlyphPos.Y, Client.Top);
  OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top);
end;

function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  State: TfrButtonState; Transparent: Boolean): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Caption, Layout, Margin, Spacing,
    GlyphPos, Result);
  DrawButtonGlyph(Canvas, GlyphPos.X, GlyphPos.Y, State, Transparent);
  DrawButtonText(Canvas, Caption, Result, State);
end;


{ TfrSpeedButton }


constructor TfrSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := @GlyphChanged;
  SetBounds(0, 0, 22, 22);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  ParentFont := True;
  FLayout := blGlyphLeft;
  FMargin := -1;
  FSpacing := 4;
  FInactiveGrayed := True;
  Inc(ButtonCount);
end;

destructor TfrSpeedButton.Destroy;
begin
  TButtonGlyph(FGlyph).Free;
  Dec(ButtonCount);
  if ButtonCount = 0 then
  begin
    CacheBitmap.Free;
    Pattern.Free;
    Pattern := nil;
  end;
  inherited Destroy;
end;

procedure TfrSpeedButton.Loaded;
var
  State: TfrButtonState;
begin
  inherited Loaded;
  if Enabled then
    State := fbsInactive else
    State := fbsDisabled;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

procedure TfrSpeedButton.Paint;
var
  PaintRect, R: TRect;
  CacheCanvas: TCanvas;
  AState: TfrButtonState;
  Transparent: Boolean;
begin
  if Pattern = nil then CreateBrushPattern;
  AState := FState;
  if FInactiveGrayed and Enabled then
    if FFlat and not FMouseInControl and not (csDesigning in ComponentState) then
      FState := fbsInactive;
  if not Enabled and not (csDesigning in ComponentState) then
  begin
    FState := fbsDisabled;
    FDragging := False;
  end
  else if FState = fbsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := fbsExclusive else
      FState := fbsUp;

  if (Width > 100) or (Height > 100) then
    CacheCanvas := Canvas else
    CacheCanvas := CacheBitmap.Canvas;
  CacheCanvas.Font := Font;

  if FFlat then
  begin
    PaintRect := Rect(0, 0, Width, Height);
    CacheCanvas.Brush.Color := clBtnFace;
    CacheCanvas.FillRect(PaintRect);
    if FState = fbsDown then
      OffsetRect(PaintRect, 1, 1);
  end
  else
    PaintRect := Rect(0, 0, Width, Height);
    //**
    {PaintRect := DrawButtonFace(CacheCanvas, Rect(0, 0, Width, Height), 1, bsNew,
        False, FState in [fbsDown, fbsExclusive], False);
    }
    
  if FFlat then
    Transparent := Enabled and (((FState = fbsExclusive) or
       ((AState = fbsExclusive) and (FState = fbsInactive))) and not FMouseInControl)
  else
    Transparent := FState = fbsExclusive;
  if Transparent then
  begin
    CacheCanvas.Brush.Bitmap := Pattern;
    CacheCanvas.FillRect(PaintRect);
  end;

  TButtonGlyph(FGlyph).Draw(CacheCanvas, PaintRect, Caption, FLayout, FMargin,
    FSpacing, FState, Transparent);

  if FFlat and Enabled then
  begin
    PaintRect := Rect(0, 0, Width, Height);
    if FMouseInControl or (AState = fbsExclusive) then
      if AState in [fbsDown, fbsExclusive] then
        LR_Fpc.Frame3D(CacheCanvas, PaintRect, clBtnShadow, clBtnHighlight, 1)
      else
        LR_Fpc.Frame3D(CacheCanvas, PaintRect, clBtnHighlight, clBtnShadow, 1);
  end;

  R := Rect(0, 0, Width, Height);
  if Canvas.Handle <> CacheCanvas.Handle then
    Canvas.CopyRect(R, CacheCanvas, R);

  if FFlat and (FState = fbsUp) and (csDesigning in ComponentState) then
    LR_Fpc.Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
  FState := AState;
end;

procedure TfrSpeedButton.UpdateTracking;
var
  P: TPoint;
begin
  if FFlat then
  begin
    GetCursorPos(P);
    FMouseInControl := Enabled and (FindDragTarget(P, True) = Self);
  end;
end;

procedure TfrSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := fbsDown;
      Repaint;
    end;
    FDragging := True;
  end;
end;

procedure TfrSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TfrButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then NewState := fbsUp
    else NewState := fbsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := fbsExclusive else NewState := fbsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Repaint;
    end;
  end;
end;

procedure TfrSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      { Redraw face in-case mouse is captured }
      FState := fbsUp;
      FMouseInControl := False;
      if not (FState in [fbsExclusive, fbsDown]) then Repaint;
    end
    else
      if DoClick then SetDown(not FDown)
      else
      begin
        if FDown then FState := fbsExclusive;
        Repaint;
      end;
    UpdateTracking;
    Invalidate;
    if DoClick then Click;
  end;
end;

procedure TfrSpeedButton.Click;
begin
  inherited Click;
end;

procedure TfrSpeedButton.DrawGlyph(aCanvas:TCanvas; X,Y:Integer; aEnabled:Boolean);
const
  NewState: array[Boolean] of TfrButtonState = (fbsDisabled, fbsUp);
begin
  TButtonGlyph(FGlyph).DrawButtonGlyph(aCanvas, X, Y, NewState[aEnabled], False);
end;

function TfrSpeedButton.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

procedure TfrSpeedButton.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;

function TfrSpeedButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TfrSpeedButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 4 then Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TfrSpeedButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TfrSpeedButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TfrSpeedButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then FState := fbsExclusive
    else FState := fbsUp;
    Invalidate;
    if Value then UpdateExclusive;
  end;
end;

procedure TfrSpeedButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TfrSpeedButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TfrSpeedButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TfrSpeedButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TfrSpeedButton.WMLButtonDblClk(var Message: TLMLButtonDown);
begin
  inherited;
  if FDown then DblClick;
end;

procedure TfrSpeedButton.CMEnabledChanged(var Message: TMessage);
const
  NewState: array[Boolean] of TfrButtonState = (fbsDisabled, fbsUp);
begin
  TButtonGlyph(FGlyph).CreateButtonGlyph(NewState[Enabled]);
  UpdateTracking;
  Invalidate;
end;

procedure TfrSpeedButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TfrSpeedButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TfrSpeedButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := fbsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TfrSpeedButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TfrSpeedButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TfrSpeedButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TfrSpeedButton.CMSysColorChange(var Message: TMessage);
begin
  Invalidate;
end;

procedure TfrSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FFlat and (not FMouseInControl) and Enabled then
  begin
    if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
    FMouseInControl := True;
    Invalidate;
  end;
end;

procedure TfrSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FFlat and FMouseInControl and Enabled then
  begin
    if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
    FMouseInControl := False;
    Invalidate;
  end;
end;

function TfrSpeedButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

procedure TfrSpeedButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TfrSpeedButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TfrSpeedButton.SetInactiveGrayed(Value: Boolean);
begin
  if Value <> FInactiveGrayed then begin
    FInactiveGrayed := Value;
    Invalidate;
  end;
end;


{ TTBSeparator }

function GetAlign(al:TAlign): TAlign;
begin
  if al in [alLeft, alRight] then
    Result := alTop else
    Result := alLeft;
end;

constructor TfrTBSeparator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alLeft;
  Width := 8;
  Height := 8;
  FDrawBevel := True;
end;

procedure TfrTBSeparator.SetParent(AParent:TWinControl);
begin
  inherited;
  if not (csDestroying in ComponentState) and (AParent <> nil) then
    Align := GetAlign(AParent.Parent.Align);
end;

procedure TfrTBSeparator.SetDrawBevel(Value: Boolean);
begin
  FDrawBevel := Value;
  Invalidate;
end;

procedure TfrTBSeparator.Paint;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    Pen.Style := psClear;
    Rectangle(0, 0, Width, Height);
    Pen.Style := psSolid;
    if FDrawBevel then
    case Align of
      alLeft, alRight:
      begin
        Pen.Color := clBtnShadow;
        MoveTo(Width div 2 - 1, 2);
        LineTo(Width div 2 - 1, Height - 2);
        Pen.Color := clBtnHighlight;
        MoveTo(Width div 2, 2);
        LineTo(Width div 2, Height - 2);
      end;
      alTop, alBottom:
      begin
        Pen.Color := clBtnShadow;
        MoveTo(2, Height div 2 - 1);
        LineTo(Width - 2, Height div 2 - 1);
        Pen.Color := clBtnHighlight;
        MoveTo(2, Height div 2);
        LineTo(Width - 2, Height div 2);
      end;
    end;
    if csDesigning in ComponentState then
    begin
      Brush.Style := bsClear;
      Pen.Style := psDot;
      Pen.Color := clBtnShadow;
      Rectangle(0, 0, Width - 1, Height - 1);
    end;
  end;
end;

constructor TfrTBPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alLeft;
  Width := 8;
  Height := 8;
end;

procedure TfrTBPanel.SetParent(AParent:TWinControl);
begin
  inherited;
  if not (csDestroying in ComponentState) and (AParent <> nil) then
    Align := GetAlign(AParent.Parent.Align);
end;

procedure TfrTBPanel.Paint;
begin
  with Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(Rect(0, 0, Width, Height));
    if csDesigning in ComponentState then
    begin
      Brush.Style := bsClear;
      Pen.Style := psDot;
      Pen.Color := clBtnShadow;
      Rectangle(0, 0, Width - 1, Height - 1);
    end;
  end;
end;

{ TTBButton }

constructor TfrTBButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alLeft;
  Flat := True;
end;

procedure TfrTBButton.SetParent(AParent:TWinControl);
begin
  inherited;
  if not (csDestroying in ComponentState) and (AParent <> nil) then
    Align := GetAlign(AParent.Parent.Align);
end;


procedure Register;
begin
  RegisterComponents('LR Tools', [TfrSpeedButton, TfrTBButton, TfrTBSeparator, TfrTBPanel]);
end;

end.

