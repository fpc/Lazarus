{
  Implements non-native regions with support for managing their Z-order

  Author: Felipe Monteiro de Carvalho
}
unit lazregions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcanvas;

type

  { TLazRegionPart }

  TLazRegionPart = class
  public
    function IsPointInPart(AX, AY: Integer): Boolean; virtual;
  end;

  { TLazRegionRect }

  TLazRegionRect = class(TLazRegionPart)
  public
    Rect: TRect;
    function IsPointInPart(AX, AY: Integer): Boolean; override;
  end;

  {$if defined(ver2_4) or defined(ver2_5) or defined(ver2_6)}
  TFPCustomRegion = class
    function GetBoundingRect: TRect; virtual; abstract;
    function IsPointInRegion(AX, AY: Integer): Boolean; virtual; abstract;
  end;
  {$endif}

  TLazRegion = class(TFPCustomRegion)
  public
    // The parts of a region should all be inside valid areas of the region
    // so if a combination operation removes some areas of the region, then
    // these areas should be removed from all parts of the region
    // There is no z-order for the parts, they are all validly inside the region area
    Parts: TFPList; // of TLazRegionPart
    IsSimpleRectRegion: Boolean; // Indicates whether this region has only 1 rectangular part
    Rect: TRect; // Used for performance increase when IsSimpleRectRegion is on
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddRectangle(ARect: TRect);
    procedure SetAsSimpleRectRegion(ARect: TRect);
    function GetBoundingRect: TRect; override;
    function IsPointInRegion(AX, AY: Integer): Boolean; override;
  end;

  { This is a region which can hold other region holders inside it }

  { TLazRegionWithChilds }

  TLazRegionWithChilds = class(TLazRegion)
  public
    Parent: TLazRegionWithChilds;
    // The order in this list is also the Z-Order of the sub regions inside it
    // The element with index zero is the bottom-most one
    Childs: TFPList; // of TLazRegionWithChilds
    UserData: TObject; // available link to another object
    constructor Create; override;
    destructor Destroy; override;
    function IsPointInRegion(AX, AY: Integer): TLazRegionWithChilds; virtual;
  end;

function IsPointInPolygon(AX, AY: Integer; const APolygon: array of TPoint): Boolean;

implementation

//  The function will return True if the point x,y is inside the polygon, or
//  False if it is not.
//
//  Original C code: http://www.visibone.com/inpoly/inpoly.c.txt
//
//  Translation from C by Felipe Monteiro de Carvalho
//
//  License: Public Domain
function IsPointInPolygon(AX, AY: Integer; const APolygon: array of TPoint): Boolean;
var
  xnew, ynew: Cardinal;
  xold,yold: Cardinal;
  x1,y1: Cardinal;
  x2,y2: Cardinal;
  i, npoints: Integer;
  inside: Integer = 0;
begin
  Result := False;
  npoints := Length(APolygon);
  if (npoints < 3) then Exit;
  xold := APolygon[npoints-1].X;
  yold := APolygon[npoints-1].Y;
  for i := 0 to npoints - 1 do
  begin
    xnew := APolygon[i].X;
    ynew := APolygon[i].Y;
    if (xnew > xold) then
    begin
      x1:=xold;
      x2:=xnew;
      y1:=yold;
      y2:=ynew;
    end
    else
    begin
      x1:=xnew;
      x2:=xold;
      y1:=ynew;
      y2:=yold;
    end;
    if (((xnew < AX) = (AX <= xold))         // edge "open" at left end
      and ((AY-y1)*(x2-x1) < (y2-y1)*(AX-x1))) then
    begin
      inside := not inside;
    end;
    xold:=xnew;
    yold:=ynew;
  end;
  Result := inside <> 0;
end;

{ TLazRegionPart }

function TLazRegionPart.IsPointInPart(AX, AY: Integer): Boolean;
begin
  Result := False;
end;

{ TLazRegionRect }

function TLazRegionRect.IsPointInPart(AX, AY: Integer): Boolean;
begin
  Result := (AX >= Rect.Left) and (AX <= Rect.Right) and
    (AY >= Rect.Top) and (AY <= Rect.Bottom);
end;

{ TLazRegion }

constructor TLazRegion.Create;
begin
  inherited Create;
  Parts := TFPList.Create;
  IsSimpleRectRegion := True;
end;

destructor TLazRegion.Destroy;
begin
  Parts.Free;
  inherited Destroy;
end;

procedure TLazRegion.AddRectangle(ARect: TRect);
var
  lNewRect: TLazRegionRect;
begin
  lNewRect := TLazRegionRect.Create;
  lNewRect.Rect := ARect;
  Parts.Add(lNewRect);
end;

procedure TLazRegion.SetAsSimpleRectRegion(ARect: TRect);
begin
  IsSimpleRectRegion := True;
  Rect := ARect;
end;

function TLazRegion.GetBoundingRect: TRect;
begin
  Result := Rect;
end;

{
  Checks if a point is inside this region
}
function TLazRegion.IsPointInRegion(AX, AY: Integer): Boolean;
var
  i: Integer;
begin
  if IsSimpleRectRegion then
  begin
    Result := (AX >= Rect.Left) and (AX <= Rect.Right) and
      (AY >= Rect.Top) and (AY <= Rect.Bottom);
  end
  else
  begin
    Result := False;
    for i := 0 to Parts.Count-1 do
    begin
      // being inside 1 subpart is enough
      if TLazRegionPart(Parts.Items[i]).IsPointInPart(AX, AY) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

{ TLazRegionWithChilds }

constructor TLazRegionWithChilds.Create;
begin
  inherited Create;
  Childs := TFPList.Create;
end;

destructor TLazRegionWithChilds.Destroy;
begin
  Childs.Free;
  inherited Destroy;
end;

{
  Returns itself or a child, depending on where the point was found
  or nil if the point is neither in the region nor in any children

  Part of the behavior is implemented in TLazRegionWithChilds
}
function TLazRegionWithChilds.IsPointInRegion(AX, AY: Integer): TLazRegionWithChilds;
var
  i: Integer;
  lIsInside: Boolean;
begin
  Result := nil;
  // First check if it is inside itself
  lIsInside := inherited IsPointInRegion(AX, AY);

  // If it is, then check if it is in any of the children
  if lIsInside then
  begin
    Result := nil;

    for i := 0 to Childs.Count-1 do
    begin
      Result := TLazRegionWithChilds(Childs.Items[i]).IsPointInRegion(AX, AY);
    end;

    // if it wasn't in any sub region, it is really in this region
    if Result = nil then Result := Self;
  end;
end;

end.

