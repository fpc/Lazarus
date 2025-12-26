{
 /***************************************************************************
                                markdown.control
                                ----------------
                             Markdown drawing control

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit markdown.control;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, markdown.elements, markdown.canvasrender, markdown.parser;

Type
  TMarkdownImageEvent = markdown.canvasrender.TMarkdownImageEvent;
  TOpenURLEvent = procedure(Sender : TObject; aURL : String) of object;
  { TMarkDownControl }

  { TCustomMarkDownControl }

  TCustomMarkDownControl = class(TCustomControl)
  private
    FMarkDown: TStrings;
    FOnOpenURL: TOpenURLEvent;
    FRenderer : TMarkDownCanvasRenderer;
    FCalculatedWidth : LongInt;
    fCalculatedHeight : LongInt;
    FIsSelecting: Boolean;
    FSelectionStart: TSelectionPoint;
    FlastCalcWidth : Longint;
    function GetColor(AIndex: Integer): TColor;
    function GetDocument: TMarkDownDocument;
    function GetInteger(AIndex: Integer): Integer;
    function GetOnGetImage: TMarkdownImageEvent;
    function GetString(AIndex: Integer): string;
    function GetSelectionColor: TColor;
    procedure SetRenderColor(AIndex: Integer; AValue: TColor);
    procedure SetInteger(AIndex: Integer; AValue: Integer);
    procedure SetMarkDown(AValue: TStrings);
    procedure SetOnGetImage(AValue: TMarkdownImageEvent);
    procedure SetSelectionColor(AValue: TColor);
    procedure SetString(AIndex: Integer; AValue: string);
  protected
    procedure DoMarkdownChanged(Sender : TObject); virtual;
    procedure ParseMarkDown; virtual;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    property Document : TMarkDownDocument Read GetDocument;
  Public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure Paint; override;
    procedure CalcLayout; virtual;
    procedure ClearSelection;
    function GetSelectionText: String;
    procedure CopySelectionToClipBoard;
    property MarkDown : TStrings Read FMarkDown Write SetMarkDown;
    property BaseFontSize: Integer index 0 read GetInteger Write SetInteger;
    property FontName: string index 0 read GetString write SetString;
    property MonoFontName: string index 1 read GetString write SetString;
    property FontColor: TColor index 0 read GetColor write SetRenderColor;
    property FontCodeColor: TColor index 1 read GetColor write SetRenderColor;
    property FontQuoteColor: TColor index 2 read GetColor write SetRenderColor;
    property HyperLinkColor: TColor index 3 read GetColor write SetRenderColor;
    property BGCodeColor: TColor index 4 read GetColor write SetRenderColor;
    property BulletChar1 : string index 2 read GetString write SetString;
    property BulletChar2 : string index 3 read GetString write SetString;
    property BulletChar3 : string index 4 read GetString write SetString;
    Property BlockQuoteIndent : Integer index 1 read GetInteger Write SetInteger;
    property ParagraphSpacing : Integer index 2 read GetInteger Write SetInteger;
    property ExtraIndent : Integer index 3 read GetInteger Write SetInteger;
    Property ImageMargin : integer index 4 read GetInteger Write SetInteger;
    property OnGetImage : TMarkdownImageEvent read GetOnGetImage Write SetOnGetImage;
    property OnOpenURL : TOpenURLEvent Read FOnOpenURL Write FOnOpenURL;
    property SelectionColor: TColor read GetSelectionColor write SetSelectionColor;
    property SelectedText: String read GetSelectionText;
  end;

  TMarkDownControl = class(TCustomMarkDownControl)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property MarkDown;
    property ParentBackground;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses markdown.processors, LCLType,Clipbrd;

{ TMarkDownControl }

procedure TCustomMarkDownControl.SetMarkDown(AValue: TStrings);
begin
  if FMarkDown=AValue then Exit;
  FMarkDown.Assign(AValue);
end;

function TCustomMarkDownControl.GetOnGetImage: TMarkdownImageEvent;
begin
  Result:=FRenderer.OnGetImage;
end;

function TCustomMarkDownControl.GetColor(AIndex: Integer): TColor;
begin
  Case aIndex of
    0 : Result:=FRenderer.FontColor;
    1 : Result:=FRenderer.FontCodeColor;
    2 : Result:=FRenderer.FontQuoteColor;
    3 : Result:=FRenderer.FontHyperLinkColor;
    4 : Result:=FRenderer.BGCodeColor;
  end;
end;

function TCustomMarkDownControl.GetDocument: TMarkDownDocument;
begin
  Result:=FRenderer.Document;
end;

function TCustomMarkDownControl.GetInteger(AIndex: Integer): Integer;
begin
  Case aIndex of
    0 : Result:=FRenderer.BaseFontSize;
    1 : Result:=FRenderer.BlockQuoteIndent;
    2 : Result:=FRenderer.ParagraphSpacing;
    3 : Result:=FRenderer.ExtraIndent;
    4 : Result:=FRenderer.ImageMargin;
  end;
end;

function TCustomMarkDownControl.GetString(AIndex: Integer): string;
begin
  Case aIndex of
    0 : Result:=FRenderer.FontName;
    1 : Result:=FRenderer.MonoFontName;
  end;
end;

procedure TCustomMarkDownControl.SetRenderColor(AIndex: Integer; AValue: TColor);
begin
  Case aIndex of
    0 : FRenderer.FontColor:=aValue;
    1 : FRenderer.FontCodeColor:=aValue;
    2 : FRenderer.FontQuoteColor:=aValue;
    3 : FRenderer.FontHyperLinkColor:=aValue;
    4 : FRenderer.BGCodeColor:=aValue;
  end;
end;

procedure TCustomMarkDownControl.SetInteger(AIndex: Integer; AValue: Integer);
begin
  Case aIndex of
    0 : FRenderer.BaseFontSize:=aValue;
    1 : FRenderer.BlockQuoteIndent:=aValue;
    2 : FRenderer.ParagraphSpacing:=aValue;
    3 : FRenderer.ExtraIndent:=aValue;
    4 : FRenderer.ImageMargin:=aValue;
  end;
end;

procedure TCustomMarkDownControl.SetOnGetImage(AValue: TMarkdownImageEvent);
begin
  FRenderer.OnGetImage:=aValue;
end;

procedure TCustomMarkDownControl.SetString(AIndex: Integer; AValue: string);
begin
  Case aIndex of
    0 : FRenderer.FontName:=aValue;
    1 : FRenderer.MonoFontName:=aValue;
  end;
end;

procedure TCustomMarkDownControl.DoMarkdownChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    ParseMarkDown;
end;

constructor TCustomMarkDownControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMarkDown:=TStringList.Create;
  TStringList(FMarkDown).OnChange:=@DoMarkdownChanged;
  FRenderer:=TMarkDownCanvasRenderer.Create(Nil);
  FIsSelecting := False;
  FSelectionStart.LayoutItemIndex := -1;
  FSelectionStart.CharOffset := 0;
end;

procedure TCustomMarkDownControl.ParseMarkDown;

begin
  FRenderer.ParseMarkdown(FMarkDown);
  CalcLayout;
end;

procedure TCustomMarkDownControl.CalcLayout;

begin
  FlastCalcWidth:=Width;
  FRenderer.BGColor:=Self.Color;
  FRenderer.CalculateLayout(Canvas,Width,FCalculatedWidth,fCalculatedHeight);
  Height:=FCalculatedHeight;
end;

procedure TCustomMarkDownControl.Click;
var
  P : TPoint;
  URL : UTF8String;
begin
  inherited Click;
  if assigned(FOnOpenURL) then
    begin
    P:=ScreenToControl(Mouse.CursorPos);
    if FRenderer.HitTestLink(P.X,P.Y,URL) then
      FOnOpenURL(Self,URL);
    end;
end;

procedure TCustomMarkDownControl.CopySelectionToClipBoard;

begin
  Clipboard.AsText:=GetSelectionText;
end;

procedure TCustomMarkDownControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift=[ssCtrl]) and ((Key=Ord('C')) or (Key=VK_INSERT)) then
    begin
    Key:=0;
    CopySelectionToClipBoard;
    end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TCustomMarkDownControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    // Start text selection
    FSelectionStart := FRenderer.HitTestText(X, Y);
    if FSelectionStart.LayoutItemIndex <> -1 then
    begin
      FIsSelecting := True;
      FRenderer.ClearSelection;
      Invalidate;
    end;
  end;
end;

procedure TCustomMarkDownControl.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure NormalizeSelection(aStart, aCurrent : TSelectionPoint; out P1, P2 : TSelectionPoint);
  begin
    if aStart.LayoutItemIndex>aCurrent.LayoutItemIndex then
      begin
      p1:=aCurrent;
      p2:=aStart;
      end
    else
      begin
      p1:=aStart;
      p2:=aCurrent;
      end
  end;

var
  CurrentPoint: TSelectionPoint;
  P1,P2 : TSelectionPoint;

begin
  inherited MouseMove(Shift, X, Y);

  if FIsSelecting and (ssLeft in Shift) then
  begin
    CurrentPoint := FRenderer.HitTestText(X, Y);
    if CurrentPoint.LayoutItemIndex <> -1 then
    begin
      NormalizeSelection(FSelectionStart, CurrentPoint, P1, P2);
      FRenderer.SetSelection(P1,P2);
      Invalidate;
    end;
  end;
end;

procedure TCustomMarkDownControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    FIsSelecting := False;
  end;
end;

procedure TCustomMarkDownControl.Resize;
begin
  inherited Resize;
  if Width<>FlastCalcWidth then
    CalcLayout;
end;

procedure TCustomMarkDownControl.ClearSelection;
begin
  FRenderer.ClearSelection;
  FIsSelecting := False;
  Invalidate;
end;

function TCustomMarkDownControl.GetSelectionText: String;
begin
  Result := FRenderer.GetSelectedText;
end;

function TCustomMarkDownControl.GetSelectionColor: TColor;
begin
  Result := FRenderer.SelectionColor;
end;

procedure TCustomMarkDownControl.SetSelectionColor(AValue: TColor);
begin
  FRenderer.SelectionColor := AValue;
end;

destructor TCustomMarkDownControl.destroy;
begin
  FreeAndNil(FRenderer);
  FreeAndNil(FMarkDown);
  inherited destroy;
end;

procedure TCustomMarkDownControl.Paint;
begin
  Canvas.Brush.Color:=Self.Color;
  Canvas.Brush.Style:=bsSolid;
  Canvas.FillRect(0,0,Width,Height);
  if (FCalculatedWidth>0) and (fCalculatedHeight>0) then
    begin
    FRenderer.BGColor:=Color;
    FRenderer.DrawLayout(Canvas,0,0);
    end;
end;

end.

