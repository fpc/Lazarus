{%MainUnit ../graphics.pp}
{******************************************************************************
                          TPatternBitmapCache
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{
  Delphi does not expose any of the internal structures that deal
  with the management or byproducts of calls to AllocPatternBitmap,
  especially the caching mechanisme and the cache are not exposed.
  So all class definitions for this are in the implementation section.
}


type
  { TPatternBitmap }

  TPatternBitmap = class
  private
    FBitmap: TBitmap;
  public
    ColorBG: TColor;
    ColorFG: TColor;
    constructor Create(AColorBG, AColorFG: TColor);
    destructor Destroy; override;
    function GetBitmap: TBitmap;
  end;

  { TPatternBitmapCache }

  TPatternBitmapCache = class
  private
    FLock: TCriticalSection;
    FList: TAvlTree;
    function InternalCompare(Tree: TAvlTree; Data1, Data2: Pointer): integer;
  protected
    procedure Lock;
    procedure UnLock;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(ABitmap: TPatternBitmap): TPatternBitmap;
    function FindBitmap(AColorBG, AColorFG: TColor): TPatternBitmap;
    function Count: Integer;
  end;

var
  PatternBitmapCache: TPatternBitmapCache;

type
  TPatternRec = record
    ColorBG, ColorFG: TColor;
  end;
  PPatternRec = ^TPatternRec;

function CompareColors(C1, C2: TColor): Integer;
begin
  if (C1 = C2) then
    Result := 0
  else
    if (C1 < C2) then
      Result := -1
    else Result := +1;
end;

function AllocPatternBitmap(colorBG, colorFG: TColor): TBitmap;
var
  x,y: Integer;
  PatternBmp: TPatternBitmap;
begin
  Result := nil;
  if not Assigned(PatternBitmapCache) then
    Exit;
  PatternBmp := PatternBitmapCache.FindBitmap(colorBG, colorFG);
  if not Assigned(PatternBmp) then
  begin
    {$ifdef debugpatternbitmap}
    debugln('AllocPatternBitmap: FindBitmap = nil');
    {$endif}
    PatternBmp := TPatternBitmap.Create(colorBG, colorFG);
    PatternBitmapCache.Add(PatternBmp);
  end;
  {$ifdef debugpatternbitmap}
  debugln(['AllocPatternBitmap: FindBitmap = ',Pointer(PatternBmp)]);
  {$endif}
  Result := PatternBmp.GetBitmap;
end;

constructor TPatternBitmap.Create(AColorBG, AColorFG: TColor);
var
  x, y: Integer;
begin
  ColorBG := AColorBG and $00FFFFFF; //don't use systemcolors
  ColorFG := AColorFG and $00FFFFFF;
  FBitmap := TBitmap.Create;
  FBitmap.Width := 8;
  FBitmap.Height := 8;
  FBitmap.Canvas.Brush.Style := bsSolid;
  FBitmap.Canvas.Brush.Color := colorBG;
  FBitmap.Canvas.Rectangle(0,0,7,7);
  for x := 0 to 7 do for y := 0 to 7 do
  begin
    if ((not Odd(x)) and (not Odd(y))) xor ((Odd(x)) and (Odd(y))) then
      FBitmap.Canvas.Pixels[x,y] := colorFG;
  end;
end;

destructor TPatternBitmap.Destroy;
begin
  {$ifdef debugpatternbitmap}
  debugln(['TPatternBitmap.Destroy: Freeing FBitmap: ',Pointer(FBitmap)]);
  {$endif}
  FBitmap.Clear;
  FBitmap.Free;
  inherited Destroy;
end;

function TPatternBitmap.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

{ TPatternBitmapCache }


function TPatternBitmapCache.InternalCompare(Tree: TAvlTree; Data1, Data2: Pointer): integer;
var
  Bmp1: TPatternBitmap absolute Data1;
  Bmp2: TPatternBitmap absolute Data2;
begin
  Result := CompareColors(Bmp1.ColorBG, Bmp2.ColorBG);
  if (Result = 0) then
    Result := CompareColors(Bmp1.ColorFG, Bmp2.ColorFG);
  {$ifdef debugpatternbitmap}
  debugln(['TPatternBitmapCache.InternalCompare: Result = ',Result]);
  {$endif}
end;

procedure TPatternBitmapCache.Lock;
begin
  FLock.Enter;
end;

procedure TPatternBitmapCache.UnLock;
begin
  FLock.Leave;
end;

constructor TPatternBitmapCache.Create;
begin
  {$ifdef debugpatternbitmap}
  debugln(['TPatternBitmapCache.Create']);
  {$endif}
  FLock := TCriticalSection.Create;
  FList := TAvlTree.CreateObjectCompare(@InternalCompare);
end;

destructor TPatternBitmapCache.Destroy;
begin
  {$ifdef debugpatternbitmap}
    debugln(['TPatternBitmapCache.Destroy: Assigned(WidgetSet) = ',Assigned(WidgetSet),
           ', FList.Count = ',FList.Count]);
  {$endif}
  FList.FreeAndClear;
  FList.Free;
  FLock.Free;
  inherited Destroy;
end;

function TPatternBitmapCache.Add(ABitmap: TPatternBitmap): TPatternBitmap;
begin
  Lock;
  try
    if (FindBitmap(ABitmap.ColorBG, ABitmap.ColorFG) <> nil) then
      RaiseGDBException(ClassName+'.Add: ABitmap added twice');
    {$ifdef debugpatternbitmap}
    debugln('TPatternBitmapCache.Add: FindBitmap = nil');
    {$endif}
    Result := TPatternBitmap((FList.Add(ABitmap)).Data);
    {$ifdef debugpatternbitmap}
    debugln(['  Result = ',Pointer(Result)]);
    {$endif}
    if (FList.Count mod 1000 = 0) then debugln([Self.ClassName,'.Add: Added ',FList.Count,' items.']);
    if (FindBitmap(ABitmap.ColorBG, ABitmap.ColorFG) = nil) then
    begin
      {$IFNDEF DisableChecks}
      DebugLn(['TPatternBitmapCache fatal error: cannot retrieve added bitmap: ', Pointer(Result)]);
      {$ENDIF}
      RaiseGDBException(ClassName+' fatal error: cannot retrieve added bitmap');
    end;
  finally
    UnLock;
  end;
end;

function InternalCompareKeyWithData(Key, Data: Pointer): Integer;
var
  PatternRec: PPatternRec absolute Key;
  Bmp: TPatternBitmap absolute Data;
begin
  Result := CompareColors(PatternRec^.ColorBG, Bmp.ColorBG);
  if (Result = 0) then
  begin
    Result := CompareColors(PatternRec^.ColorFG, Bmp.ColorFG);
  end;
end;

function TPatternBitmapCache.FindBitmap(AColorBG, AColorFG: TColor): TPatternBitmap;
var
  PatternRec: TPatternRec;
  Res: TAvlTreeNode;
begin
  Lock;
  Result := nil;
  AColorBG := AColorBG and $00FFFFFF; //do not use systemcolors
  AColorFG := AColorFG and $00FFFFFF; //do not use systemcolors
  PatternRec.ColorBG := AColorBG;
  PatternRec.ColorFG := AColorFG;
  Res := FList.FindKey(@PatternRec, @InternalCompareKeyWithData);
  if (Res <> nil) then
    Result := TPatternBitmap(Res.Data);
  UnLock;
end;

function TPatternBitmapCache.Count: Integer;
begin
  Result := FList.Count;
end;

