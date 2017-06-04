unit TTProfile;

{$R-} // TODO: Fix out-of-bounds accesses.
{$mode objfpc}{$H+}

interface

uses
{$IFDEF VIRTUALPASCAL}
     Use32,
{$ENDIF}
     TTTypes,
     SysUtils;

const
  PoolMaxCapacity = 512*1024 div 4; { = 512 Ko }
  PoolIdleCapacity = 64*1024 div 4; { = 64 Ko }

type
  TCurveDirection  = ( GoingUnknown, GoingUp, GoingDown, GoingHoriz );
  TPoint = record x, y : long; end;
  TBezierStack = packed array[0..64*2] of TPoint;
  PBezierStack = ^TBezierStack;

  TProfile = class;
  TProfileCollection = class;

  { TRenderPool }

  TRenderPool = class
  protected
    Precision, PrecisionHalf, BezierPrecision: integer;
    BoundsMinY,BoundsMaxY: integer;
    scaleShift: integer;

    function Bezier_Down(miny, maxy: Long): boolean;
    function Bezier_State(y1, y2, y3: TT_F26Dot6): TCurveDirection;
    function Bezier_To(x, y, Cx, Cy: LongInt): boolean;
    function Bezier_Up(miny, maxy: Long): boolean;
    function CEILING(x: Long): Long;
    function DecomposeCurve(first, last: Int; flipped: Boolean;
      points: TT_Points; flags: PByte): boolean;
    function FLOOR(x: Long): Long;
    function FRAC(x: Long): Int;
    function GetCapacity: integer;
    function Line_Down(x1, y1, x2, y2, miny, maxy: LongInt): boolean;
    function Line_To(x, y: LongInt): boolean;
    function Line_Up(x1, y1, x2, y2, miny, maxy: LongInt): boolean;
    procedure Move_To(x, y: Longint);
    procedure PushBezier(x1, y1, x2, y2, x3, y3: LongInt);
    function SCALED(x: Long): Long;
    procedure Split_Bezier(base: PBezierStack);
    function TRUNC(x: Long): Long;

    function RequireCapacity(count: integer): boolean;
    procedure PushValue(AValue: Long); inline;
    property Capacity: integer read GetCapacity;
  public
    Joint    : Boolean;    (* Indicates that the last arc stopped sharp *)
                           (* on a scan-line. Important to get rid of   *)
                           (* doublets                                  *)
    Fresh    : Boolean;    (* Indicates a new Profile which 'Start' field *)
                           (* must be set                                 *)
    cProfile  : TProfile;  (* current Profile                *)
    ProfileColl: TProfileCollection;
    LastX,                 (* Last and extrema coordinates during *)
    LastY : longint;       (* rendering                           *)
    CurveDir     : TCurveDirection;     (* State of current trace *)

    Arcs     : TBezierStack;
    CurArc   : Int;             (* stack's top                 *)

    data: packed array of Long;
    position: integer;

    constructor Create(APrecision, ABezierPrecision: integer);
    destructor Destroy; override;
    procedure SetPrecision(APrecision, ABezierPrecision: integer);
    procedure SetBounds(MinY,MaxY: integer);
    procedure SetScaleShift(value: integer);
    procedure Clear;
    procedure ReduceCapacity;
    function Convert_Glyph(flipped: Boolean; points: TT_Points; flags: PByte;
      Outs: TT_PConStarts; nContours : Int): boolean;
  end;

  TProfile = class
    Pool    : TRenderPool;
    Flow    : Int;       (* ascending or descending Profile *)
    Height  : Int;       (* Profile's height in scanlines   *)
    Start   : Int;       (* Profile's starting scanline     *)
    Offset  : integer;   (* offset of first coordinate in   *)
                         (* render pool                     *)

    X       : Longint;   (* current coordinate during sweep *)

    nextInContour : TProfile;  (* next Profile of the same contour*)
    nextInColl: TProfile;      (* next Profile in collection      *)

    nextInList, prevInList : TProfile;  (* in linked list *)

    constructor Create(APool: TRenderPool; ADirection: TCurveDirection);
  end;

  { TProfileCollection }

  TProfileCollection = class
  protected
    procedure Remove_Profile;
  public
    Pool    : TRenderPool;
    prevProfile : TProfile;
    fProfile  : TProfile;  (* head of Profiles linked list   *)
    gProfile  : TProfile;  (* last Profile in case of impact *)

    nProfs   : Int;        (* current number of Profiles *)

    constructor Create(APool: TRenderPool);
    procedure Clear;
    procedure New_Profile(ADirection: TCurveDirection);
    procedure End_Profile;
    destructor Destroy; override;
  end;

procedure ProfileList_Init( out L : TProfile );
procedure ProfileList_InsertFirstElement( var L: TProfile; Element: TProfile);
procedure ProfileList_AppendToList( var ListOrElement: TProfile; ElementOrListToAppend: TProfile );
procedure ProfileList_Remove( var List: TProfile; ElementToRemove: TProfile);
procedure ProfileList_SortByX( var L: TProfile);
procedure ProfileList_SortByStart( var L: TProfile);
function ProfileList_Count( L: TProfile): integer;
procedure ProfileList_Split( List : TProfile; out Part1, Part2: TProfile);

implementation

uses
     TTCalc,       { used for MulDiv }
     TTError ;


(************************************************)
(*                                              *)
(*  ProfileList_Init                            *)
(*                                              *)
(*    Init an empty profile linked list.        *)
(*                                              *)
(************************************************)

procedure ProfileList_Init( out L : TProfile );
begin
  L := nil;
end;

(************************************************)
(*                                              *)
(*  ProfileList_Append :                        *)
(*                                              *)
(*    Inserts a new Profile in a linked list.   *)
(*                                              *)
(************************************************)

procedure ProfileList_InsertFirstElement(var L: TProfile; Element: TProfile);
begin
  Element.nextInList := L;
  if L <> nil then L.prevInList := Element;
  L := Element;
end;

procedure ProfileList_AppendToList( var ListOrElement: TProfile; ElementOrListToAppend: TProfile );
var cur,next: TProfile;
begin
  if ElementOrListToAppend = nil then exit;
  if ListOrElement = nil then
    ListOrElement := ElementOrListToAppend else
  begin
    cur := ListOrElement;
    next := cur.nextInList;
    while next <> nil do
    begin
      cur := next;
      next := cur.nextInList;
    end;
    ElementOrListToAppend.prevInList := cur;
    cur.nextInList := ElementOrListToAppend;
  end;
end;

(************************************************)
(*                                              *)
(*  ProfileList_Remove :                        *)
(*                                              *)
(*    Removes an old Profile from a linked list *)
(*                                              *)
(************************************************)

procedure ProfileList_Remove( var List: TProfile; ElementToRemove: TProfile);
var prev,next: TProfile;
begin
  if ElementToRemove = nil then exit;
  prev := ElementToRemove.prevInList;
  next := ElementToRemove.nextInList;
  if prev <> nil then
  begin
    prev.nextInList := next;
    ElementToRemove.prevInList := nil;
  end;
  if next <> nil then
  begin
    next.prevInList := prev;
    ElementToRemove.nextInList := nil;
  end;
  if ElementToRemove = List then List := next;
end;

procedure ProfileList_SortByX(var L: TProfile);

  function Merge(P1,P2: TProfile): TProfile;
  var
    cur: TProfile;
  begin
    result := nil;
    if P1 = nil then result := P2
    else if P2 = nil then result := P1
    else
    begin
      cur := nil;
      while (P1 <> nil) and (P2 <> nil) do
      begin
        if P1.X <= P2.X then
        begin
          if cur = nil then
          begin
            result := P1;
            result.prevInList := nil;
            cur := result;
          end else
          begin
            cur.nextInList := P1;
            P1.prevInList := cur;
            cur := P1;
          end;
          P1 := P1.nextInList;
        end else
        begin
          if cur = nil then
          begin
            result := P2;
            result.prevInList := nil;
            cur := result;
          end else
          begin
            cur.nextInList := P2;
            P2.prevInList := cur;
            cur := P2;
          end;
          P2 := P2.nextInList;
        end;
      end;
      //append end of list
      if P1 <> nil then
      begin
        cur.nextInList := P1;
        P1.prevInList := cur;
      end else
      if P2 <> nil then
      begin
        cur.nextInList := P2;
        P2.prevInList := cur;
      end;
    end;
  end;

var L1,L2: TProfile;
begin
  if (L = nil) or (L.nextInList = nil) then exit;
  ProfileList_Split(L, L1,L2);
  ProfileList_SortByX(L1);
  ProfileList_SortByX(L2);
  L := Merge(L1, L2);
end;

procedure ProfileList_SortByStart(var L: TProfile);

  function Merge(P1,P2: TProfile): TProfile;
  var
    cur: TProfile;
  begin
    result := nil;
    if P1 = nil then result := P2
    else if P2 = nil then result := P1
    else
    begin
      cur := nil;
      while (P1 <> nil) and (P2 <> nil) do
      begin
        if P1.Start <= P2.Start then
        begin
          if cur = nil then
          begin
            result := P1;
            result.prevInList := nil;
            cur := result;
          end else
          begin
            cur.nextInList := P1;
            P1.prevInList := cur;
            cur := P1;
          end;
          P1 := P1.nextInList;
        end else
        begin
          if cur = nil then
          begin
            result := P2;
            result.prevInList := nil;
            cur := result;
          end else
          begin
            cur.nextInList := P2;
            P2.prevInList := cur;
            cur := P2;
          end;
          P2 := P2.nextInList;
        end;
      end;
      //append end of list
      if P1 <> nil then
      begin
        cur.nextInList := P1;
        P1.prevInList := cur;
      end else
      if P2 <> nil then
      begin
        cur.nextInList := P2;
        P2.prevInList := cur;
      end;
    end;
  end;

var L1,L2: TProfile;
begin
  if (L = nil) or (L.nextInList = nil) then exit;
  ProfileList_Split(L, L1,L2);
  ProfileList_SortByStart(L1);
  ProfileList_SortByStart(L2);
  L := Merge(L1, L2);
end;

function ProfileList_Count(L: TProfile): integer;
begin
  result := 0;
  While L <> nil do
  begin
    inc(result);
    L := L.nextInList;
  end;
end;

procedure ProfileList_Split(List: TProfile; out Part1, Part2: TProfile);
var n,m: integer;
    p: TProfile;
begin
  if List = nil then
  begin
    Part1 := nil;
    Part2 := nil;
    exit;
  end;
  n := 0;
  p := List;
  while p <> nil do
  begin
    inc(n);
    p := p.nextInList;
  end;
  m := (n+1) shr 1;
  p := List;
  while m > 0 do
  begin
    p := p.nextInList;
    dec(m);
  end;

  Part1 := List;
  Part2 := p;

  if p <> nil then
  begin
    if p.prevInList = nil then
      raise Exception.Create('Incoherent list');
    p.prevInList.nextInList := nil;
    p.prevInList := nil;
  end;
end;

{* integer computation *}

function TRenderPool.TRUNC( x : Long ) : Long; inline;
begin
  Trunc := (x and -Precision) div Precision;
end;

function TRenderPool.FRAC( x : Long ) : Int; inline;
begin
  Frac := x and (Precision-1);
end;

function TRenderPool.FLOOR( x : Long ) : Long; inline;
begin
  Floor := x and -Precision;
end;

function TRenderPool.CEILING( x : Long ) : Long; inline;
begin
  Ceiling := (x + Precision-1) and -Precision;
end;

function TRenderPool.SCALED( x : Long ) : Long; inline;
begin
  SCALED := (x shl scaleShift) - precisionHalf;
end;


{ TProfileCollection }

constructor TProfileCollection.Create(APool: TRenderPool);
begin
  Pool := APool;
  nProfs:= 0;
  prevProfile := nil;
  Pool.cProfile := nil;
  fProfile := nil;
  gProfile := nil;
  Pool.ProfileColl := self;
end;

procedure TProfileCollection.Clear;
var p,p2: TProfile;
begin
  p := fProfile;
  while p <> nil do
  begin
   p2 := p.nextInColl;
   p.Free;
   p := p2;
  end;

  nProfs:= 0;
  prevProfile := nil;
  Pool.cProfile := nil;
  fProfile := nil;
  gProfile := nil;
end;

procedure TProfileCollection.New_Profile(ADirection: TCurveDirection);
var nProfile: TProfile;
begin
  nProfile := TProfile.Create(Pool, ADirection);
  if fProfile = nil then fProfile := nProfile;
  if gProfile = nil then gProfile := nProfile;
  if Pool.cProfile <> nil then
  begin
    Pool.cProfile.nextInColl := nProfile;
    Pool.cProfile.nextInContour := nProfile; //by default same contour
  end;
  prevProfile := Pool.cProfile;
  Pool.cProfile := nProfile;

  Pool.CurveDir  := ADirection;
  Pool.Fresh := True;
  Pool.Joint := False;
  inc(nProfs);
end;

procedure TProfileCollection.Remove_Profile;
begin
  if prevProfile <> nil then
  begin
    prevProfile.nextInColl := nil;
    if prevProfile.nextInContour = Pool.cProfile then
      prevProfile.nextInContour := nil;
    if gProfile = Pool.cProfile then gProfile := nil;
    Pool.cProfile.Free;
    Pool.cProfile := prevProfile;
    prevProfile := nil;
    dec(nProfs);
  end;
end;

procedure TProfileCollection.End_Profile;
var H: integer;
begin
  Pool.CurveDir  := GoingUnknown;
  Pool.Fresh := False;
  Pool.Joint := False;

  if (Pool.cProfile = nil) or (Pool.cProfile.Height <> 0) then exit;
  H := Pool.position - Pool.cProfile.Offset;
  if H = 0 then
    Remove_Profile
  else
    Pool.cProfile.Height := H;
end;

destructor TProfileCollection.Destroy;
begin
  Pool.ProfileColl := nil;
  inherited Destroy;
end;

{ TRenderPool }

function TRenderPool.GetCapacity: integer;
begin
  result := length(data);
end;

constructor TRenderPool.Create(APrecision, ABezierPrecision: integer);
begin
  position := 0;
  SetPrecision(APrecision, ABezierPrecision);
  setlength(data,64);
  CurveDir := GoingUnknown;
  Joint := false;
  Fresh := False;
  cProfile := nil;
  BoundsMinY := 0;
  BoundsMaxY := 65535;
  ProfileColl := TProfileCollection.Create(self);
end;

destructor TRenderPool.Destroy;
begin
  ProfileColl.Free;
  inherited Destroy;
end;

procedure TRenderPool.SetPrecision(APrecision, ABezierPrecision: integer);
begin
  precision := APrecision;
  precisionHalf := APrecision shr 1;
  BezierPrecision := ABezierPrecision;
end;

procedure TRenderPool.SetBounds(MinY, MaxY: integer);
begin
  BoundsMinY := MinY;
  BoundsMaxY := MaxY;
end;

procedure TRenderPool.SetScaleShift(value: integer);
begin
  scaleShift := value;
end;

procedure TRenderPool.Clear;
begin
  position := 0;
  CurveDir := GoingUnknown;
  Joint := false;
  Fresh := False;
  cProfile := nil;
  ProfileColl.Clear;
end;

procedure TRenderPool.ReduceCapacity;
begin
  if Capacity > PoolIdleCapacity then
     setLength(data, PoolIdleCapacity);
end;

function TRenderPool.RequireCapacity(count: integer): boolean;
begin
  if count > PoolMaxCapacity then
    result := false
  else
  begin
    if length(data) < count then
    begin
      count := count*2;
      if count > PoolMaxCapacity then
        count := PoolMaxCapacity;
      setlength(data, count);
    end;
    result := true;
  end;
end;

procedure TRenderPool.PushValue(AValue: Long);
begin
  if cProfile = nil then
    raise Exception.Create('Out of profile');
  data[position] := AValue;
  inc( position );
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    TProfile.Create                                             *)
(*                                                                          *)
(* Description: Creates a new Profile in the render pool                    *)
(*                                                                          *)
(* Input:       ADirection    state/orientation of the new Profile          *)
(*                                                                          *)
(****************************************************************************)

constructor TProfile.Create(APool: TRenderPool; ADirection : TCurveDirection );
begin
  Case ADirection of

    GoingUp  : Flow := TT_Flow_Up;
    GoingDown : Flow := TT_Flow_Down;
  else
    raise exception.Create('ERROR : Inconsistent Profile' );
  end;

  Pool    := APool;
  Start   := 0;
  Height  := 0;
  if APool <> nil then
    Offset  := APool.position;
  nextInContour   := nil;
  nextInColl := nil;

  nextInList := nil;
  prevInList := nil;
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    Split_Bezier                                                *)
(*                                                                          *)
(* Description: Subdivises one Bezier arc into two joint                    *)
(*              sub-arcs in the Bezier stack.                               *)
(*                                                                          *)
(* Input:       None ( subdivised bezier is taken from the top of the       *)
(*              stack )                                                     *)
(*                                                                          *)
(* Returns:     Nada                                                        *)
(*                                                                          *)
(****************************************************************************)

procedure TRenderPool.Split_Bezier( base : PBezierStack );
var
  arc  : PBezierStack;
  a, b : Long;
begin
{$IF defined(CPUI386) and defined(CPU32) and not defined(NO_ASM)} {$asmmode intel}
  asm
    push esi
    push ebx
    push ecx

    mov  esi, base

    mov  eax, [esi+2*8]       (* arc^[4].x := arc^[2].x *)
    mov  ebx, [esi+1*8]       (* b := arc^[1].x *)
    mov  ecx, [esi+0*8]       (* b := (arc^[0].x+b) div 2 *)

    mov  [esi+4*8], eax

    add  eax, ebx             (* a := (arc^[2].x+b) div 2 *)
    add  ebx, ecx
    mov  edx, eax
    mov  ecx, ebx
    sar  edx, 31
    sar  ecx, 31
    sub  eax, edx
    sub  ebx, ecx
    sar  eax, 1
    sar  ebx, 1

    mov  [esi+3*8], eax       (* arc^[3].x := a *)
    mov  [esi+1*8], ebx

    add  eax, ebx             (* arc[2].x := (a+b) div 2 *)
    mov  edx, eax
    sar  edx, 31
    sub  eax, edx
    sar  eax, 1
    mov  [esi+2*8], eax

    add  esi, 4

    mov  eax, [esi+2*8]       (* arc^[4].x := arc^[2].x *)
    mov  ebx, [esi+1*8]       (* b := arc^[1].x *)
    mov  ecx, [esi+0*8]       (* b := (arc^[0].x+b) div 2 *)

    mov  [esi+4*8], eax

    add  eax, ebx             (* a := (arc^[2].x+b) div 2 *)
    add  ebx, ecx
    mov  edx, eax
    mov  ecx, ebx
    sar  edx, 31
    sar  ecx, 31
    sub  eax, edx
    sub  ebx, ecx
    sar  eax, 1
    sar  ebx, 1

    mov  [esi+3*8], eax       (* arc^[3].x := a *)
    mov  [esi+1*8], ebx

    add  eax, ebx             (* arc[2].x := (a+b) div 2 *)
    mov  edx, eax
    sar  edx, 31
    sub  eax, edx
    sar  eax, 1
    mov  [esi+2*8], eax

    pop ecx
    pop ebx
    pop esi
  end;
{$ELSE}
  arc := base;

  arc^[4].x := arc^[2].x;
  b := arc^[1].x;
  a := (arc^[2].x + b) div 2; arc^[3].x := a;
  b := (arc^[0].x + b) div 2; arc^[1].x := b;
  arc^[2].x := (a+b) div 2;

  arc^[4].y := arc^[2].y;
  b := arc^[1].y;
  a := (arc^[2].y + b) div 2; arc^[3].y := a;
  b := (arc^[0].y + b) div 2; arc^[1].y := b;
  arc^[2].y := (a+b) div 2;
{$ENDIF}
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    Push_Bezier                                                 *)
(*                                                                          *)
(* Description: Clears the Bezier stack and pushes a new Arc on top of it.  *)
(*                                                                          *)
(* Input:       x1,y1 x2,y2 x3,y3  new Bezier arc                           *)
(*                                                                          *)
(* Returns:     nada                                                        *)
(*                                                                          *)
(****************************************************************************)

procedure TRenderPool.PushBezier( x1, y1, x2, y2, x3, y3 : LongInt );
begin
  curArc:=0;

  with Arcs[CurArc+2] do begin x:=x1; y:=y1; end;
  with Arcs[CurArc+1] do begin x:=x2; y:=y2; end;
  with Arcs[ CurArc ] do begin x:=x3; y:=y3; end;
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    Line_Up                                                     *)
(*                                                                          *)
(* Description: Compute the x-coordinates of an ascending line segment      *)
(*              and stores them in the render pool.                         *)
(*                                                                          *)
(* Input:       x1,y1 x2,y2  Segment start (x1,y1) and end (x2,y2) points   *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if Render Pool overflow.                              *)
(*                                                                          *)
(****************************************************************************)

function TRenderPool.Line_Up( x1, y1, x2, y2, miny, maxy : LongInt ) : boolean;
var
  Dx, Dy               : LongInt;
  e1, e2, f1, f2, size : Int;
  Ix, Rx, Ax           : LongInt;
begin
  Line_Up := True;

  Dx := x2-x1; Dy := y2-y1;

  if (Dy <= 0) or (y2 < MinY) or (y1 > MaxY) then exit;

  if y1 < MinY then
   begin
    x1 := x1 + MulDiv( Dx, MinY-y1, Dy );
    e1 := Trunc(MinY);
    f1 := 0;
   end
  else
   begin
    e1 := Trunc(y1);
    f1 := Frac(y1);
   end;

  if y2 > MaxY then
   begin
    x2 := x2 + MulDiv( Dx, MaxY-y2, Dy );
    e2 := Trunc(MaxY);
    f2 := 0;
   end
  else
   begin
    e2 := Trunc(y2);
    f2 := Frac(y2);
   end;

  if f1 > 0 then
    if e1 = e2 then exit
    else
      begin
        inc( x1, MulDiv( Dx, precision-f1, Dy ) );
        inc( e1 );
      end
  else
    if Joint then
      dec( self.Position );

  Joint := (f2 = 0);

  (* Indicates that the segment stopped sharp on a ScanLine *)

  if Fresh then
   begin
    cProfile.Start := e1;
    Fresh           := False;
   end;

  size := ( e2-e1 )+1;
  if not RequireCapacity( self.Position+size ) then
   begin
     Line_Up := False;
     Error   := Err_Ras_Overflow;
     exit;
   end;

  if Dx > 0 then
    begin
      Ix := (Precision*Dx) div Dy;
      Rx := (Precision*Dx) mod Dy;
      Dx := 1;
    end
  else
    begin
      Ix := -((Precision*-Dx) div Dy);
      Rx :=   (Precision*-Dx) mod Dy;
      Dx := -1;
    end;

  Ax  := -Dy;

  while size > 0 do
  begin
    PushValue(x1);

    inc( x1, Ix );
    inc( ax, rx );

    if ax >= 0 then
    begin
      dec( ax, dy );
      inc( x1, dx );
    end;

    dec( size );
  end;

end;

(****************************************************************************)
(*                                                                          *)
(* Function:    Line_Down                                                   *)
(*                                                                          *)
(* Description: Compute the x-coordinates of a descending line segment      *)
(*              and stores them in the render pool.                         *)
(*                                                                          *)
(* Input:       x1,y1 x2,y2  Segment start (x1,y1) and end (x2,y2) points   *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if Render Pool overflow.                              *)
(*                                                                          *)
(****************************************************************************)

function TRenderPool.Line_Down( x1, y1, x2, y2, miny, maxy : LongInt ): boolean;
var
  _fresh : Boolean;
begin
  _fresh  := fresh;

  Line_Down := Line_Up( x1, -y1, x2, -y2, -maxy, -miny );

  if _fresh and not fresh then
    cProfile.start := -cProfile.start;
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    Bezier_Up                                                   *)
(*                                                                          *)
(* Description: Compute the x-coordinates of an ascending bezier arc        *)
(*              and stores them in the render pool.                         *)
(*                                                                          *)
(* Input:       None.The arc is taken from the top of the Bezier stack.     *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if Render Pool overflow.                              *)
(*                                                                          *)
(****************************************************************************)

function TRenderPool.Bezier_Up( miny, maxy : Long ) : boolean;
var
  y1, y2, e, e2, e0         : LongInt;
  carc, debArc, f1          : Int;
  base                      : PBezierStack;
  maxArc                    : Int;
label
  Fin;
begin
  Bezier_Up := True;

  carc := curArc;
  maxArc := length(Arcs)-cArc;
  base := @Arcs[cArc];
  y1   := base^[2].y;
  y2   := base^[0].y;

  if ( y2 < MinY ) or ( y1 > MaxY ) then
    goto Fin;

  e2 := FLOOR(y2);

  if e2 > MaxY then e2 := MaxY;

  e0 := MinY;

  if y1 < MinY then
    e := MinY
  else
   begin
    e  := CEILING(y1);
    f1 := FRAC(y1);
    e0 := e;

    if f1 = 0 then
     begin

      //avoid duplicates
      if Joint then begin dec(self.position); Joint:=False; end;

      PushValue(base^[2].x);
      inc( e, Precision );
     end
   end;

  if Fresh then
   begin
    cProfile.Start := TRUNC(e0);
    Fresh := False;
   end;

  if e2 < e then
    goto Fin;

  (* overflow ? *)
  if not RequireCapacity( self.position + TRUNC(e2-e)+ 1) then
    begin
      Bezier_Up := False;
      Error     := Err_Ras_Overflow;
      exit;
    end;

  debArc := cArc;

  while ( cArc >= debArc ) and ( e <= e2 ) do
   begin
    Joint := False;
    y2    := base^[0].y;

    if y2 > e then
      begin
        y1 := base^[2].y;
        if ( y2-y1 >= BezierPrecision ) and (cArc + 2 < maxArc) then
          begin
            Split_Bezier( base );
            inc( cArc, 2 );
            base := @base^[2];
          end
        else
          begin
            PushValue( base^[2].x +
                       MulDiv( base^[0].x - base^[2].x,
                                      e - y1,
                                      y2 - y1 ) );
            dec( cArc, 2 );
            base := @Arcs[cArc];
            inc( e, Precision );
          end;
      end
    else
      begin
        if y2 = e then
        begin
          joint := True;
          PushValue( Arcs[cArc].x);
          inc( e, Precision );
        end;
        dec( cArc, 2 );
        base := @Arcs[cArc];
      end
  end;

Fin:
  dec( curArc, 2);
  exit;
end;


(****************************************************************************)
(*                                                                          *)
(* Function:    Bezier_Down                                                 *)
(*                                                                          *)
(* Description: Compute the x-coordinates of a descending bezier arc        *)
(*              and stores them in the render pool.                         *)
(*                                                                          *)
(* Input:       None. Arc is taken from the top of the Bezier stack.        *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if Render Pool overflow.                              *)
(*                                                                          *)
(****************************************************************************)

function TRenderPool.Bezier_Down( miny, maxy : Long ) : boolean;
var
  base   : PBezierStack;
  _fresh : Boolean;
begin
  _fresh := fresh;
  base   := @Arcs[curArc];

  base^[0].y := -base^[0].y;
  base^[1].y := -base^[1].y;
  base^[2].y := -base^[2].y;

  Bezier_Down := Bezier_Up( -maxy, -miny );

  if _fresh and not fresh then
    cProfile.start := -cProfile.start;

  base^[0].y := -base^[0].y;
end;

procedure TRenderPool.Move_To( x,y: Longint);
begin
  LastX := x;
  LastY := y;
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    Line_To                                                     *)
(*                                                                          *)
(* Description: Injects a new line segment and adjust Profiles list.        *)
(*                                                                          *)
(* Input:       x, y : segment endpoint ( start point with Move_To )        *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if Render Pool overflow or Incorrect Profile          *)
(*                                                                          *)
(****************************************************************************)

function TRenderPool.Line_To( x, y : LongInt ) : boolean;
var LineDir: TCurveDirection;
begin
  Line_To := False;

  if y > LastY then LineDir := GoingUp else
  if y < lastY then LineDir := GoingDown else
    LineDir := GoingHoriz;

  if (CurveDir <> LineDir) and (LineDir <> GoingHoriz) then
  begin
    if CurveDir <> GoingUnknown then ProfileColl.End_Profile;
    if LineDir <> GoingHoriz then
      ProfileColl.New_Profile( LineDir);
  end;

  Case CurveDir of
    GoingUp  : if not Line_Up  ( LastX, LastY, X, Y, BoundsMiny, BoundsMaxy ) then exit;
    GoingDown : if not Line_Down( LastX, LastY, X, Y, BoundsMiny, BoundsMaxy ) then exit;
   end;

  LastX := x;
  LastY := y;

  Line_To := True;
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    Bezier_State                                                *)
(*                                                                          *)
(* Description: Determines the state (ascending/descending/flat/undet)      *)
(*              of a Bezier arc, along one given axis.                      *)
(*                                                                          *)
(* Input:       y1, y2, y3 : coordinates of the Bezier arc.                 *)
(*                           along the concerned axis.                      *)
(*                                                                          *)
(* Returns:     State, i.e. Ascending, Descending, Flat or Undetermined     *)
(*                                                                          *)
(****************************************************************************)


function TRenderPool.Bezier_State( y1, y2, y3 : TT_F26Dot6 ) : TCurveDirection;
begin
  (* determine orientation of a Bezier arc *)
  if y1 = y2 then

    if y2 = y3 then Bezier_State := GoingHoriz
    else
    if y2 > y3 then Bezier_State := GoingDown
    else
                    Bezier_State := GoingUp
  else
  if y1 > y2 then

    if y2 >= y3 then Bezier_State := GoingDown
    else
                     Bezier_State := GoingUnknown
  else

    if y2 <= y3 then Bezier_State := GoingUp
    else
                     Bezier_State := GoingUnknown;
end;


(****************************************************************************)
(*                                                                          *)
(* Function:    Bezier_To                                                   *)
(*                                                                          *)
(* Description: Injects a new bezier arc and adjust Profiles list.          *)
(*                                                                          *)
(* Input:       x,   y : arc endpoint ( start point with Move_To )          *)
(*              Cx, Cy : control point                                      *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if Render Pool overflow or Incorrect Profile          *)
(*                                                                          *)
(****************************************************************************)


function TRenderPool.Bezier_To( x, y, Cx, Cy : LongInt ) : boolean;
var
  y3, x3   : LongInt;
  BezierDir : TCurveDirection;
begin
  Bezier_To := False;

  PushBezier( LastX, LastY, Cx, Cy, X, Y );

  while ( curArc >= 0 ) do
   begin
    y3 := Arcs[curArc].y;
    x3 := Arcs[curArc].x;

    BezierDir := Bezier_State( Arcs[curArc+2].y, Arcs[curArc+1].y, y3 );

    case BezierDir of

      GoingHoriz   : dec( curArc, 2 );

      GoingUnknown : if curArc + 2 < length(arcs) then
                    begin
                      Split_Bezier( @Arcs[curArc] );
                      inc( curArc, 2 );
                    end else
                      raise exception.Create('Bezier overflow');
    else
      if CurveDir <> BezierDir then
      begin
        if CurveDir <> GoingUnknown then
            ProfileColl.End_Profile;

        ProfileColl.New_Profile( BezierDir );
      end;

      case CurveDir of
        GoingUp   : if not Bezier_Up( BoundsMiny, BoundsMaxy ) then exit;
        GoingDown : if not Bezier_Down( BoundsMiny, BoundsMaxy ) then exit;
      end;

    end;
  end;

  LastX := x3;
  LastY := y3;

  Bezier_To := True;
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    DecomposeCurve                                              *)
(*                                                                          *)
(* Description: This functions scans the outline arrays in order to         *)
(*              emit individual segments and beziers by calling the         *)
(*              functions Line_To and Bezier_To. It handles all weird       *)
(*              cases, like when the first point is off the curve, or       *)
(*              when there are simply no "on" points in the contour !       *)
(*                                                                          *)
(* Input:       xCoord, yCoord : array coordinates to use.                  *)
(*              first,  last   : indexes of first and last point in         *)
(*                               contour.                                   *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if case of error.                                     *)
(*                                                                          *)
(* Notes:       The function assumes that 'first' < 'last'                  *)
(*                                                                          *)
(****************************************************************************)

procedure swap( var x, y : Long );  inline;
var
  s : Long;
begin
  s := x; x := y; y := s;
end;

function TRenderPool.DecomposeCurve( first,  last   : Int;
                         flipped        : Boolean;
                         points: TT_Points; flags: PByte) : boolean;
var
  index : Int;

  x, y   : Long;    (* current point                *)
  cx, cy : Long;    (* current Bezier control point *)
  mx, my : Long;    (* middle point                 *)

  x_first, y_first : Long;    (* first point coordinates *)
  x_last,  y_last  : Long;    (* last point coordinates  *)

  on_curve : Boolean;
begin

  DecomposeCurve := False;

  with points^[first] do
  begin
    x_first := SCALED( x );
    y_first := SCALED( y );
  end;

  if flipped then  swap( x_first, y_first );

  with points^[last] do
  begin
    x_last  := SCALED( x );
    y_last  := SCALED( y );
  end;

  if flipped then  swap( x_last, y_last );

  Move_To(x_first, y_first);
  cx := x_first;
  cy := y_first;

  index    := first;
  on_curve := Flags^[first] and 1 <> 0;

  (* check first point, and set origin *)
  if not on_curve then
  begin
    (* first point is off the curve - yes, this happens !! *)

    if Flags^[last] and 1 <> 0 then
      Move_To(x_last,y_last)  (* start at last point if it is *)
    else                      (* on the curve                 *)
      begin
        (* if both first and last point    *)
        (* are off the curve, start midway *)
        Move_To( (LastX + x_last) div 2, (LastY + y_last) div 2);

        (* record midpoint in x_last,y_last *)
        x_last := LastX;
        y_last := LastY;
      end;
  end;


  (* now process each contour point *)
  while ( index < last ) do
  begin
    inc( index );

    x := SCALED( points^[index].x );
    y := SCALED( points^[index].y );

    if flipped then swap( x, y );

    if on_curve then
      begin
        (* the previous point was on the curve *)

        on_curve := Flags^[index] and 1 <> 0;
        if on_curve then
          begin
            (* two successive on points -> emit segment *)
            if not Line_To( x, y ) then exit;
          end
        else
          begin
            (* else, keep current point as control for next bezier *)
            cx := x;
            cy := y;
          end;
      end
    else
      begin
        (* the previous point was off the curve *)

        on_curve := Flags^[index] and 1 <> 0;
        if on_curve then
          begin
            (* reaching on point -> emit Bezier *)
            if not Bezier_To( x, y, cx, cy ) then exit;
          end
        else
          begin
            (* two successive off points -> create middle point *)
            (* then emit Bezier                                 *)
            mx := (cx + x) div 2;
            my := (cy + y) div 2;

            if not Bezier_To( mx, my, cx, cy ) then exit;

            cx := x;
            cy := y;
          end;
      end;
  end;

  (* end of contour, close curve cleanly *)
  if ( Flags^[first] and 1 <> 0 ) then

    if on_curve then
      if not Line_To( x_first, y_first ) then exit else
    else
      if not Bezier_To( x_first, y_first, cx, cy ) then exit else

  else
    if not on_curve then
      if not Bezier_To( x_last, y_last, cx, cy ) then exit;

  DecomposeCurve := True;
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    Convert_Glyph                                               *)
(*                                                                          *)
(* Description: Converts a glyph into a series of segments and arcs         *)
(*              and make a Profiles list with them.                         *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if any error was encountered during render.           *)
(*                                                                          *)
(****************************************************************************)

Function TRenderPool.Convert_Glyph( flipped : Boolean; points: TT_Points; flags: PByte; Outs : TT_PConStarts; nContours : Int ) : boolean;
var
  i, j : Int;

begin
  result := False;

  try

    j        := 0;
    for i := 0 to nContours-1 do
    begin
      // assign to nil to know first profile in contour
      ProfileColl.gProfile := nil;

      (* decompose a single contour into individual segments and *)
      (* beziers                                                 *)

      if not DecomposeCurve( j, outs^[i], flipped, points, flags) then exit;
      j := outs^[i] + 1;

      ProfileColl.End_Profile;

      (* We _must_ take care of the case when the first and last arcs join  *)
      (* while having the same orientation                                  *)

      if ( Frac(lastY) = 0 ) and
         ( lastY >= BoundsMinY ) and
         ( lastY <= BoundsMaxY ) then

        if ( ProfileColl.gProfile <> nil ) and                 (* gProfile can be nil   *)
           ( cProfile <> nil) and
           ( cProfile.Height > 0) and
           ( ProfileColl.gProfile.Start = cProfile.Start + (cProfile.Height-1)*cProfile.Flow) and
           ( ProfileColl.gProfile.Flow = cProfile.Flow ) then  (* if the contour was    *)
                                                               (* too small to be drawn *)
        begin
          dec( self.position );
          dec( cProfile.Height);
        end;

      //if (cProfile <> nil) and (cProfile.Height > 1) then dec (cProfile.Height);

      // close contour
      if ProfileColl.gProfile <> nil then cProfile.nextInContour := ProfileColl.gProfile;

    end;
    result := true;

  except
    on ex: Exception do
    begin

    end;
  end;
end;

end.

