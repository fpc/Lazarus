{ ------------------------------------------------------------------------------
                                AttrGrid.pas
--------------------------------------------------------------------------------  
  Modified Grids units

  Implements a derived StringGrid which stores cell attributes (background and
  text color, font name, font size, text alignment, wordwrap) in the internal
  data structure (TVirtualGrid with pointers to TCellProps records containing
  Attr pointers interpreted here as pointers to TCellAttr records).
  
  To simplify usage the grid's class keeps its old name, TStringGrid. Therefore,
  unit AttrGrids must be listed AFTER the normal grids unit in the uses clause
  of each form needing the new grids features.
  
  License: 
  LGPL2 with linking exception - like the license of the Lazarus LCL.
-------------------------------------------------------------------------------}
unit AttrGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, GraphMath, Dialogs, Grids, Types;

type
  TCellAttr = record
    Alignment: TAlignment;
    BkColor: TColor;
    FontColor: TColor;
    FontName: String;
    FontSize: Integer;
    FontStyle: TFontStyles;
    Layout: TTextlayout;
    Wordwrap: Boolean;
  end;
  PCellAttr = ^TCellAttr;

  TCellAttrVirtualGrid = class(TVirtualGrid)
  protected
    procedure DisposeCell(var P: PCellProps); override;
  end;

  { TStringGrid }

  TStringGrid = class(Grids.TStringGrid)
  private
    FDefAlignment: TAlignment;
    FDefLayout: TTextLayout;
    function GetCellAlignment(ACol, ARow: Integer): TAlignment;
    function GetCellAttr(ACol, ARow: Integer): PCellAttr;
    function GetCellBkColor(ACol, ARow: Integer): TColor;
    function GetCellFontColor(ACol, ARow: Integer): TColor;
    function GetCellFontName(ACol, ARow: Integer): String;
    function GetCellFontSize(ACol, ARow: Integer): Integer;
    function GetCellFontStyle(ACol, ARow: Integer): TFontStyles;
    function GetCellLayout(ACol, ARow: Integer): TTextLayout;
    function GetCellWordWrap(ACol, ARow: Integer): Boolean;
    procedure SetCellAlignment(ACol, ARow: Integer; AValue: TAlignment);
    procedure SetCellAttr(ACol, ARow: Integer; AValue: PCellAttr);
    procedure SetCellBkColor(ACol, ARow: Integer; AValue: TColor);
    procedure SetCellFontColor(ACol, ARow: Integer; AValue: TColor);
    procedure SetCellFontName(ACol, ARow: Integer; AValue: String);
    procedure SetCellFontSize(ACol, ARow: Integer; AValue: Integer);
    procedure SetCellFontStyle(ACol, ARow: Integer; AValue: TFontStyles);
    procedure SetCellLayout(ACol, ARow: Integer; AValue: TTextLayout);
    procedure SetCellWordWrap(ACol, ARow: Integer; AValue: Boolean);
  protected
    function CreateVirtualGrid: TVirtualGrid; override;
    function DefaultCellAttr: TCellAttr;
    function GetCellAttrOrDefault(ACol, ARow: Integer): TCellAttr;
    function NewCellAttr(var ACellProps: PCellProps): PCellAttr;
    procedure PrepareCanvas(ACol, ARow: Integer; AState:TGridDrawState); override;
    function SameCellAttr(Attr1, Attr2: PCellAttr): Boolean;
    procedure UpdateCell(ACol, ARow: Integer);

  public
    constructor Create(AOwner: TComponent); override;
    property CellAlignment[ACol, ARow: Integer]: TAlignment read GetCellAlignment write SetCellAlignment;
    property CellAttr[ACol, ARow: Integer]: PCellAttr read GetCellAttr write SetCellAttr;
    property CellBkColor[ACol, ARow: Integer]: TColor read GetCellBkColor write SetCellBkColor;
    property CellFontColor[ACol, ARow: Integer]: TColor read GetCellFontColor write SetCellFontColor;
    property CellFontName[ACol, ARow: Integer]: String read GetCellFontName write SetCellFontName;
    property CellFontSize[ACol, ARow: Integer]: Integer read GetCellFontSize write SetCellFontSize;
    property CellFontStyle[ACol, ARow: Integer]: TFontStyles read GetCellFontStyle write SetCellFontStyle;
    property CellLayout[ACol, ARow: Integer]: TTextLayout read GetCellLayout write SetCellLayout;
    property CellWordWrap[ACol, ARow: Integer]: Boolean read GetCellWordWrap write SetCellWordWrap;
    property DefAlignment: TAlignment read FDefAlignment write FDefAlignment;
    property DefLayout: TTextLayout read FDefLayout write FDefLayout;

  end;


implementation

{ TCellAttrVirtualGrid }

procedure TCellAttrVirtualGrid.DisposeCell(var P: PCellProps);
begin
  if Assigned(P) and Assigned(P^.Attr) then
  begin
    PCellAttr(P^.Attr)^.FontName := '';
    Dispose(PCellAttr(P^.Attr));
  end;
  inherited;
end;


{ extended TStringGrid }

constructor TStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FDefAlignment := taLeftJustify;
  FDefLayout := tlCenter;
end;

function TStringGrid.CreateVirtualGrid: TVirtualGrid;
begin
  Result := TCellAttrVirtualGrid.Create;
end;

function TStringGrid.DefaultCellAttr: TCellAttr;
begin
  Result.Alignment := FDefAlignment;
  Result.BkColor := Color;
  Result.FontColor := Font.Color;
  Result.FontName := 'default';
  Result.FontSize := 0;
  Result.FontStyle := [];
  Result.Layout := FDefLayout;
  Result.WordWrap := false;
end;

function TStringGrid.GetCellAlignment(ACol, ARow: Integer): TAlignment;
var
  attr: PCellAttr;
begin
  Result := taLeftJustify;
  attr := GetCellAttr(ACol, ARow);
  if Assigned(attr) then
    Result := attr^.Alignment;
end;

function TStringGrid.GetCellAttr(ACol, ARow: Integer): PCellAttr;
var
  C: PCellProps;
begin
  C := FGrid.Celda[ACol, ARow];
  if (C = nil) then
    Result := nil
  else
    Result := PCellAttr(C^.Attr);
end;

function TStringGrid.GetCellAttrOrDefault(ACol, ARow: Integer): TCellAttr;
var
  attr: PCellAttr;
begin
  attr := GetCellAttr(ACol, ARow);
  if Assigned(attr) then
    Result := attr^
  else
    Result := DefaultcellAttr;
end;

function TStringGrid.GetCellBkColor(ACol, ARow: Integer): TColor;
var
  attr: PCellAttr;
begin
  Result := Color;
  attr := GetCellAttr(ACol, ARow);
  if Assigned(attr) then
    Result := attr^.BkColor;
end;

function TStringGrid.GetCellFontColor(ACol, ARow: Integer): TColor;
var
  attr: PCellAttr;
begin
  Result := Color;
  attr := GetCellAttr(ACol, ARow);
  if Assigned(attr) then
    Result := attr^.FontColor;
end;

function TStringGrid.GetCellFontName(ACol, ARow: Integer): String;
var
  attr: PCellAttr;
begin
  Result := 'default';
  attr := GetCellAttr(ACol, ARow);
  if Assigned(attr) then
    Result := attr^.FontName;
end;

function TStringGrid.GetCellFontSize(ACol, ARow: Integer): Integer;
var
  attr: PCellAttr;
begin
  Result := 0;
  attr := GetCellAttr(ACol, ARow);
  if Assigned(attr) then
    Result := attr^.FontSize;
end;

function TStringGrid.GetCellFontStyle(ACol, ARow: Integer): TFontStyles;
var
  attr: PCellAttr;
begin
  Result := [];
  attr := GetCellAttr(ACol, ARow);
  if Assigned(attr) then
    Result := attr^.FontStyle;
end;

function TStringGrid.GetCellLayout(ACol, ARow: Integer): TTextLayout;
var
  attr: PCellAttr;
begin
  Result := tlCenter;
  attr := GetCellAttr(ACol, ARow);
  if Assigned(attr) then
    Result := attr^.Layout;
end;

function TStringGrid.GetCellWordWrap(ACol, ARow: Integer): Boolean;
var
  attr: PCellAttr;
begin
  Result := false;
  attr := GetCellAttr(ACol, ARow);
  if Assigned(attr) then
    Result := attr^.WordWrap;
end;

function TStringGrid.NewCellAttr(var ACellProps: PCellProps): PCellAttr;
begin
  if ACellProps = nil then
  begin
    ACellProps := FGrid.GetDefaultCell;
    ACellProps^.Data := nil;
  end;

  if ACellProps^.Attr = nil then
  begin
    New(Result);
    Result^ := DefaultCellAttr;
    ACellProps^.Attr := Result;
  end else
    Result := PCellAttr(ACellProps^.Attr);
end;

procedure TStringGrid.PrepareCanvas(ACol, ARow: Integer; AState:TGridDrawState);
var
  C: PCellProps;
  attr: TCellAttr;
  savedHandler: TOnPrepareCanvasEvent;
  isSelected: Boolean;
  txtStyle: TTextStyle;
begin
  savedHandler := OnPrepareCanvas;
  try
    OnPrepareCanvas := nil;   // Avoid firing OnPrepareCanvas event twice
    inherited;
    txtstyle := Canvas.TextStyle;
    GetSelectedState(AState, isSelected);
    C := FGrid.Celda[ACol, ARow];
    if (C <> nil) and (C^.Attr <> nil) then
    begin
      attr := PCellAttr(C^.Attr)^;
      txtStyle.Alignment := attr.Alignment;
      txtStyle.Layout := attr.Layout;
      txtStyle.WordBreak := attr.WordWrap;
      txtStyle.SingleLine := false; //not attr.WordWrap;
      Canvas.Font.Name := attr.FontName;
      Canvas.Font.Style := attr.FontStyle;
      Canvas.Font.Size := attr.FontSize;
      if not isSelected then
      begin
        Canvas.Brush.Color := attr.BkColor;
        Canvas.Font.Color := attr.FontColor;
      end;
    end else
    begin
      txtStyle.Alignment := FDefAlignment;
      txtStyle.Layout := FDefLayout;
    end;
    Canvas.TextStyle := txtStyle;
  finally
    OnPrepareCanvas := savedHandler;
    DoPrepareCanvas(ACol, ARow, AState);
  end;
end;

function TStringGrid.SameCellAttr(Attr1, Attr2: PCellAttr): Boolean;
begin
  if (Attr1 <> nil) and (Attr2 <> nil) then
    Result := (Attr1^.Alignment = Attr2^.Alignment) and
              (Attr1^.BkColor   = Attr2^.BkColor)   and
              (Attr1^.FontColor = Attr2^.FontColor) and
              (Attr1^.FontName  = Attr2^.FontName)  and
              (Attr1^.FontSize  = Attr2^.FontSize)  and
              (Attr1^.FontStyle = Attr2^.FontStyle) and
              (Attr1^.Layout    = Attr2^.Layout)    and
              (Attr1^.WordWrap  = Attr2^.WordWrap)
  else
  if (Attr1 = nil) and (Attr2 = nil) then
    Result := true
  else
    Result := false;
end;

procedure TStringGrid.SetCellAlignment(ACol, ARow: Integer; AValue: TAlignment);
var
  attr: TCellAttr;
begin
  attr := GetCellAttrOrDefault(ACol, ARow);
  if attr.Alignment <> AValue then
  begin
    attr.Alignment := AValue;
    SetCellAttr(ACol, ARow, @attr);
  end;
end;

procedure TStringGrid.SetCellAttr(ACol, ARow: Integer; AValue: PCellAttr);
var
  C: PCellProps;
  attr: PCellAttr;
begin
  C := FGrid.Celda[ACol, ARow];
  if (C = nil) then
  begin
    attr := NewCellAttr(C);
    FGrid.Celda[ACol, ARow] := C;
  end;
  if C^.Attr = nil then
    C^.Attr := NewCellAttr(C);
  if Assigned(AValue) then
    PCellAttr(C^.Attr)^ := AValue^
  else
    C^.Attr := nil;
  Modified := not SameCellAttr(AValue, C^.Attr);
  if Modified then InvalidateCell(ACol, ARow);
end;

procedure TStringGrid.SetCellBkColor(ACol, ARow: Integer; AValue: TColor);
var
  attr: TCellAttr;
begin
  attr := GetCellAttrOrDefault(ACol, ARow);
  if attr.BkColor <> AValue then
  begin
    attr.BkColor := AValue;
    SetCellAttr(ACol, ARow, @attr);
  end;
end;

procedure TStringGrid.SetCellFontColor(ACol, ARow: Integer; AValue: TColor);
var
  attr: TCellAttr;
begin
  attr := GetCellAttrOrDefault(ACol, ARow);
  if attr.FontColor <> AValue then
  begin
    attr.FontColor := AValue;
    SetCellAttr(ACol, ARow, @attr);
  end;
end;

procedure TStringGrid.SetCellFontName(ACol, ARow: Integer; AValue: String);
var
  attr: TCellAttr;
begin
  attr := GetCellAttrOrDefault(ACol, ARow);
  if attr.FontName <> AValue then
  begin
    attr.FontName := AValue;
    SetCellAttr(ACol, ARow, @attr);
  end;
end;

procedure TStringGrid.SetCellFontSize(ACol, ARow: Integer; AValue: Integer);
var
  attr: TCellAttr;
begin
  attr := GetCellAttrOrDefault(ACol, ARow);
  if attr.FontSize <> AValue then
  begin
    attr.FontSize := AValue;
    SetCellAttr(ACol, ARow, @attr);
  end;
end;

procedure TStringGrid.SetCellFontStyle(ACol, ARow: Integer; AValue: TFontStyles);
var
  attr: TCellAttr;
begin
  attr := GetCellAttrOrDefault(ACol, ARow);
  if attr.FontStyle <> AValue then
  begin
    attr.FontStyle := AValue;
    SetCellAttr(ACol, ARow, @attr);
  end;
end;

procedure TStringGrid.SetCellLayout(ACol, ARow: Integer; AValue: TTextLayout);
var
  attr: TCellAttr;
begin
  attr := GetCellAttrOrDefault(ACol, ARow);
  if attr.Layout <> AValue then
  begin
    attr.Layout := AValue;
    SetCellAttr(ACol, ARow, @attr);
  end;
end;

procedure TStringGrid.SetCellWordWrap(ACol, ARow: Integer; AValue: Boolean);
var
  attr: TCellAttr;
begin
  attr := GetCellAttrOrDefault(ACol, ARow);
  if attr.WordWrap <> AValue then
  begin
    attr.WordWrap := AValue;
    SetCellAttr(ACol, ARow, @attr);
  end;
end;

procedure TStringGrid.UpdateCell(ACol, ARow: Integer);
begin
  if EditorMode and (ACol = Col) and (ARow = Row) and
    not (gfEditorUpdateLock in GridFlags) then
  begin
    EditorDoSetValue;
  end;
  InvalidateCell(aCol, aRow);
end;

end.

