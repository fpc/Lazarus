(*******************************************************************
 *
 *  TTRaster.Pas                                              v 1.2
 *
 *  The FreeType glyph rasterizer.
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 *  NOTES : This version supports the following :
 *
 *    - direct grayscaling
 *    - sub-banding
 *    - drop-out modes 4 and 5
 *    - second pass for complete drop-out control ( bitmap only )
 *    - variable precision
 *
 *   Re-entrancy is _not_ planned.
 *
 *   Changes between 1.1 and 1.2 :
 *
 *     - no more trace tables, now uses linked list to sort
 *       coordinates.
 *
 *     - reduced code size using function dispatch within a generic
 *       draw_sweep function.
 *
 *     - added variable precision for finer rendering at small ppems
 *
 *
 *   Note that its interface may change in the future.
 *
 ******************************************************************)

Unit TTRASTER;

interface

{$R-} // TODO: Fix out-of-bounds accesses.
{$I TTCONFIG.INC}

uses
{$IFDEF VIRTUALPASCAL}
     Use32,
{$ENDIF}
     TTTypes,
     TTProfile;

{$IFDEF CONST_PREC}

const
  Precision_Bits   = 6;
  Precision        = 1 shl Precision_Bits;
  Precision_Half   = Precision div 2;
  Precision_Step   = Precision_Half;
  Precision_Shift  = 0;
  Precision_Mask   = -Precision;
  Precision_Jitter = 2;

{$ENDIF}

type
  Function_Sweep_Init = procedure( var min, max : Int ) of object;

  Function_Sweep_Span = procedure( y     : Int;
                                   x1    : TT_F26dot6;
                                   x2    : TT_F26dot6;
                                   Left  : TProfile;
                                   Right : TProfile ) of object;

  Function_Sweep_Step = procedure of object;

  { TFreeTypeRasterizer }

  TFreeTypeRasterizer = class(TFreeTypeCustomRasterizer)
  private
    Precision_Bits   : Int;       (* Fractional bits of Raster coordinates *)
    Precision        : Int;
    Precision_Half   : Int;
    Precision_Step   : Int;       (* Bezier subdivision minimal step       *)
    Precision_Shift  : Int;       (* Shift used to convert coordinates     *)
    Precision_Mask   : Longint;   (* integer truncatoin mask               *)
    Precision_Jitter : Int;

    Pool     : TRenderPool;(* Profiles buffer a.k.a. Render Pool *)

    Cible      : TT_Raster_Map; (* Description of target map *)

    BWidth     : integer;
    BCible     : PByte;   (* target bitmap buffer *)
    GCible     : PByte;   (* target pixmap buffer *)

    TraceBOfs   : Int;     (* current offset in target bitmap         *)
    TraceBIncr  : Int;     (* increment to next line in target bitmap *)
    TraceGOfs   : Int;     (* current offset in targer pixmap         *)
    TraceGIncr  : Int;     (* increment to next line in target pixmap *)

    gray_min_x : Int;     (* current min x during gray rendering *)
    gray_max_x : Int;     (* current max x during gray rendering *)

    (* Dispatch variables : *)

    Proc_Sweep_Init : Function_Sweep_Init;  (* Sweep initialisation *)
    Proc_Sweep_Span : Function_Sweep_Span;  (* Span drawing         *)
    Proc_Sweep_Drop : Function_Sweep_Span;  (* Drop out control     *)
    Proc_Sweep_Step : Function_Sweep_Step;  (* Sweep line step      *)
    Proc_Sweep_Direct: TDirectRenderingFunction; (* Direct rendering *)

    Direct_X, Direct_Y, Direct_TX: integer;

    Points   : TT_Points;
    Flags    : PByte;           (* current flags array     *)
    Outs     : TT_PConStarts;   (* current endpoints array *)

    //nPoints,            (* current number of points   *)
    nContours : Int;    (* current number of contours *)

    DropOutControl : Byte;  (* current drop-out control mode *)

    Grays : TT_Gray_Palette;
    (* gray palette used during gray-levels rendering *)
    (* 0 : background .. 4 : foreground               *)

    BGray_Data  : PByte;   { temporary bitmap for grayscale      }
    BGray_Incr  : integer; { increment for temp bitmap           }
    BGray_End   : integer; { ending offset of temporary bitmap   }
    BGray_Capacity: integer; { current capacity of temp bitmap   }

    Second_Pass : boolean;
    (* indicates wether an horizontal pass should be performed  *)
    (* to control drop-out accurately when calling Render_Glyph *)
    (* Note that there is no horizontal pass during gray render *)

    (* better set it off at ppem >= 18                          *)

    procedure BGray_NeedCapacity(c: integer);
    function Draw_Sweep(MinY, MaxY: integer; PixelGrain: integer): boolean;
    procedure Horizontal_Gray_Sweep_Drop(y: Int; x1, x2: TT_F26dot6; Left,
      Right: TProfile);
    procedure Horizontal_Gray_Sweep_Span(y: Int; x1, x2: TT_F26dot6; {%H-}Left,
      {%H-}Right: TProfile);
    procedure Horizontal_Sweep_Drop(y: Int; x1, x2: TT_F26dot6; Left,
      Right: TProfile);
    procedure Horizontal_Sweep_Init(var {%H-}min, {%H-}max: Int);
    procedure Horizontal_Sweep_Span(y: Int; x1, x2: TT_F26dot6; {%H-}Left,
      {%H-}Right: TProfile);
    procedure Horizontal_Sweep_Step;
    function ProcessCoordinate(var List: TProfile): integer;
    procedure Raster_Object_Init;
    procedure Raster_Object_Done;
    function Render_Single_Pass(vertical: Boolean; OutputMinY, OutputMaxY,
      PixelGrain: integer): boolean;
    {$IFNDEF CONST_PREC}procedure Set_High_Precision(High: boolean);
    procedure Set_Second_Pass(Pass: boolean);
    procedure Vertical_Gray_Sweep_Init(var min, {%H-}max: Int);
    procedure Vertical_Gray_Sweep_Init_Direct(var min, max: Int);
    procedure Vertical_Gray_Sweep_Init_Direct_HQ(var min, max: Int);
    procedure Vertical_Gray_Sweep_Init_HQ(var min, {%H-}max: Int);
    procedure Vertical_Gray_Sweep_Step;
    procedure Vertical_Gray_Sweep_Step_Direct;
    procedure Vertical_Gray_Sweep_Step_Direct_HQ;
    procedure Vertical_Gray_Sweep_Step_HQ;
    procedure Vertical_Sweep_Drop(y: Int; x1, x2: TT_F26dot6; Left,
      Right: TProfile);
    procedure Vertical_Sweep_Init(var min, {%H-}max: Int);
    procedure Vertical_Sweep_Span({%H-}y: Int; x1, x2: TT_F26dot6; {%H-}Left,
      {%H-}Right: TProfile);
    procedure Vertical_Sweep_Step;
{$ENDIF}
  public
    function Render_Glyph( var glyph  : TT_Outline;
                           var target : TT_Raster_Map ) : TError; override;

    (* Render one glyph in the target bitmap (1-bit per pixel)       *)

    function Render_Gray_Glyph( var glyph   : TT_Outline;
                                var target  : TT_Raster_Map;
                                palette : PTT_Gray_Palette ) : TError; override;

    (* Render one gray-level glyph in the target pixmap              *)
    (* palette points to an array of 5 colors used for the rendering *)
    (* use nil to reuse the last palette. Default is VGA graylevels  *)

    function Render_Gray_Glyph_HQ( var glyph   : TT_Outline;
                                var target  : TT_Raster_Map ) : TError; override;

    function Render_Directly_Gray_Glyph( var glyph   : TT_Outline;
                                x,y,tx,ty: integer;
                                OnRender: TDirectRenderingFunction;
                                palette : PTT_Gray_Palette) : TError; override;

    function Render_Directly_Gray_Glyph_HQ( var glyph   : TT_Outline;
                                x,y,tx,ty: integer;
                                OnRender: TDirectRenderingFunction) : TError; override;

    procedure Set_Raster_Palette(const palette: TT_Gray_Palette); override;

    constructor Create;
    destructor Destroy; override;
  end;

  { These functions round up minimum and maximum value of an interval over
    data which is organized by grains of constant size. For example, if
    the size of the grain is 4, then minimum values can be 0, 4, 8, etc.
    and maximum values can be 3, 7, 11, etc. }
  function IncludeFullGrainMin(minValue: integer; Grain: integer): integer;
  function IncludeFullGrainMax(maxValue: integer; Grain: integer): integer;

  function TTRaster_Init: TError;
  procedure TTRaster_Done;

  function TTGetDefaultRasterizer: TFreeTypeRasterizer;

implementation

uses
     TTError,
     SysUtils;

const
  Pixel_Bits = 6;        (* fractional bits of input coordinates  *)

const
  LMask : array[0..7] of Byte
        = ($FF,$7F,$3F,$1F,$0F,$07,$03,$01);

  RMask : array[0..7] of Byte
        = ($80,$C0,$E0,$F0,$F8,$FC,$FE,$FF);
  (* left and right fill bitmasks *)

var
    Count_Table : array[0..255] of Word;
    (* Look-up table used to quickly count set bits in a gray 2x2 cell *)

    BitCountTable: packed array[0..255] of byte; //number of bits 'on' in a byte

function IncludeFullGrainMin(minValue: integer; Grain: integer): integer;
begin
  if minValue mod Grain <> 0 then
  begin
    if minValue > 0 then result := minValue - (minValue mod Grain)
      else result := minValue - (Grain - (-minValue) mod Grain);
  end else
    result := minValue;
end;

function IncludeFullGrainMax(maxValue: integer; Grain: integer): integer;
begin
  if maxValue mod Grain <> Grain-1 then
  begin
    if maxValue > 0 then result := maxValue + (Grain-1 - (maxValue mod Grain))
    else result := maxValue + (((-maxValue) mod Grain) - 1);
  end
  else
    result := maxValue;
end;

{$IFNDEF CONST_PREC}

(****************************************************************************)
(*                                                                          *)
(* Function:    Set_High_Precision                                          *)
(*                                                                          *)
(* Description: Sets precision variables according to param flag            *)
(*                                                                          *)
(* Input:       High     set to True for high precision ( typically for     *)
(*                       ppem < 18 ), false otherwise.                      *)
(*                                                                          *)
(****************************************************************************)

procedure TFreeTypeRasterizer.Set_High_Precision( High : boolean );
begin
  if High then
    begin
      Precision_Bits   := 10;
      Precision_Step   := 128;
      Precision_Jitter := 24;
    end
  else
    begin
      Precision_Bits   := 6;
      Precision_Step   := 32;
      Precision_Jitter := 2;
    end;

  Precision       := 1 shl Precision_Bits;
  Precision_Half  := Precision shr 1;
  Precision_Shift := Precision_Bits - Pixel_Bits;
  Precision_Mask  := -Precision;
  if Pool <> nil then Pool.SetPrecision(Precision,Precision_Step);
end;

{$ENDIF}

procedure TFreeTypeRasterizer.Set_Second_Pass( Pass : boolean );
begin
  second_pass := pass;
end;


  (************************************************)
  (*                                              *)
  (*  Process next coordinate                     *)
  (*  Returns: count                              *)
  (*                                              *)
  (************************************************)

  function TFreeTypeRasterizer.ProcessCoordinate( var List : TProfile ): integer;
  var
    current : TProfile;
  begin
    result := 0;
    if List = nil then exit;

    current := list;
    repeat
      with current do
      begin
        X := Pool.Data[offset];
        inc( offset, flow );
        dec( height );
        current := nextInList;
        inc(result);
      end;
    until current = nil;
  end;

(********************************************************************)
(*                                                                  *)
(*  Generic Sweep Drawing routine                                   *)
(*                                                                  *)
(*                                                                  *)
(*                                                                  *)
(********************************************************************)

function TFreeTypeRasterizer.Draw_Sweep(MinY,MaxY: integer; PixelGrain: integer) : boolean;

label
  Skip_To_Next;

var
  y      : Int;
  P, Q   : TProfile;

  Top,
  Bottom,
  min_Y,
  max_Y: Int;

  x1, x2, xs, e1, e2 : LongInt;

  Wait  : TProfile;

  Draw_Left  : TProfile;
  Draw_Right : TProfile;

  Drop_Left  : TProfile;
  Drop_Right : TProfile;

  P_Left,  Q_Left  : TProfile;
  P_Right, Q_Right : TProfile;

  dropouts  : Int;
  countLeft, countRight: integer;

begin
  if Pool.ProfileColl.fProfile = nil then
  begin
    result := true;
    exit;
  end;

  Draw_Sweep := False;

  (* Init the empty linked lists *)

  ProfileList_Init( Wait );

  ProfileList_Init( Draw_Left  );
  ProfileList_Init( Draw_Right );

  ProfileList_Init( Drop_Left  );
  ProfileList_Init( Drop_Right );

  (* First, compute min Y and max Y *)

  max_Y := MinY;
  min_Y := MaxY;

  P     := Pool.ProfileColl.fProfile;
  while P <> nil do
   with P do
    begin
     Q := P.nextInColl;

     if p.Flow = TT_Flow_Down then
     begin //flip coordinates
       Dec(p.Start, p.Height-1);
       Inc(p.Offset, p.Height-1);
     end;

     Bottom := P.Start;
     Top    := Bottom + P.Height-1;

     if min_Y > Bottom then min_Y := Bottom;
     if max_Y < Top    then max_Y := Top;

     X := 0;
     ProfileList_InsertFirstElement( Wait, P );

     P := Q;
   end;

  min_y := IncludeFullGrainMin(min_y,PixelGrain);
  max_y := IncludeFullGrainMax(max_y,PixelGrain);

  if min_Y < MinY then min_Y := MinY;
  if max_Y > MaxY then max_Y := MaxY;

  ProfileList_SortByStart( Wait );

  (* Now inits the sweeps *)

  Proc_Sweep_Init( min_Y, max_Y );

  (* Let's go *)

  for y := min_Y to max_Y do
  begin

    (* Look in the wait list for new activations *)

    while (Wait <> nil) and (Wait.Start <= y) do
    begin
      P := Wait;
      ProfileList_Remove(Wait, Wait);

      if P.Height > 0 then
        case P.Flow of
          TT_Flow_Up   : ProfileList_InsertFirstElement( Draw_Left, P );
          TT_Flow_Down : ProfileList_InsertFirstElement( Draw_Right, P );
        else
          raise Exception.Create('Unexpected flow');
        end;
    end;

    (* Get next coordinate *)
    countLeft := ProcessCoordinate( Draw_Left );
    countRight := ProcessCoordinate( Draw_Right );
    dropouts  := 0;

    if countLeft = countRight then
    begin
      (* sort the drawing lists *)

      ProfileList_SortByX( Draw_Left );
      ProfileList_SortByX( Draw_Right );

      (* Let's trace *)

      P_Left  := Draw_Left;
      P_Right := Draw_Right;

      while ( P_Left <> nil ) and (P_Right <> nil) do
      begin

        {$IFDEF ASSERT}
        if P_Right = nil then
          Halt(13);
        {$ENDIF}

        Q_Left  := P_Left.nextInList;
        Q_Right := P_Right.nextInList;

        {$IFDEF ASSERT}
        if Q_Right = nil then
          Halt(11);
        {$ENDIF}

        x1 := P_Left.X;
        x2 := P_Right.X;

        if x1 > x2 then
          begin
            xs := x1;
            x1 := x2;
            x2 := xs;
          end;

        if ( x2-x1 <= Precision ) then
          begin
            e1 := ( x1+Precision-1 ) and Precision_Mask;
            e2 := x2 and Precision_Mask;

            if (dropOutControl <> 0) and
               ((e1 > e2) or (e2 = e1 + Precision)) then
            begin
              P_Left.x  := x1;
              P_Right.x := x2;

              ProfileList_Remove( Draw_Left,  P_Left );
              ProfileList_Remove( Draw_Right, P_Right );

              ProfileList_AppendToList( Drop_Left,  P_Left );
              ProfileList_AppendToList( Drop_Right, P_Right );
              inc( dropouts );

              goto Skip_To_Next;
            end
          end;

        Proc_Sweep_Span( y, x1, x2, P_Left, P_Right );

        (* We finalize the Profile if needed *)

        if P_Left.height = 0 then
            ProfileList_Remove( Draw_Left,  P_Left  );

        if P_Right.height = 0 then
            ProfileList_Remove( Draw_Right, P_Right );

    Skip_To_Next:

        P_Left  := Q_Left;
        P_Right := Q_Right;
      end;
    end else
    begin
      P_Left  := Draw_Left;
      while ( P_Left <> nil ) do
      begin
        Q_Left  := P_Left.nextInList;
        {x1 := P_Left.X;
        Proc_Sweep_Span( y, x1-Precision_Half, x1+Precision_Half, P_Left, P_Left );}
        if P_Left.height = 0 then
            ProfileList_Remove( Draw_Left,  P_Left  );
        P_Left  := Q_Left;
      end;

      P_Right := Draw_Right;
      while ( P_Right <> nil ) do
      begin
        Q_Right  := P_Right.nextInList;
        {x2 := P_Right.X;
        Proc_Sweep_Span( y, x2-Precision_Half, x2+Precision_Half, P_Right, P_Right );}
        if P_Right.height = 0 then
            ProfileList_Remove( Draw_Right, P_Right );
        P_Right  := Q_Right;
      end;
    end;

    {$IFDEF ASSERT}
    if P_Right <> nil then
      Halt(10);
    {$ENDIF}

    (* Now perform the dropouts only _after_ the span drawing *)

    P_Left  := Drop_Left;
    P_Right := Drop_Right;

    while ( dropouts > 0 ) do
    begin

      Q_Left  := P_Left.nextInList;
      Q_Right := P_Right.nextInList;

      ProfileList_Remove( Drop_Left, P_Left );
      ProfileList_Remove( Drop_Right, P_Right );

      Proc_Sweep_Drop( y, P_Left.x, P_Right.x, P_Left, P_Right );

      if P_Left.height > 0 then
        ProfileList_InsertFirstElement( Draw_Left, P_Left );

      if P_Right.height > 0 then
        ProfileList_InsertFirstElement( Draw_Right, P_Right );

      P_Left  := Q_Left;
      P_Right := Q_Right;

      dec( dropouts );
    end;

    (* Step to next line *)

    Proc_Sweep_Step;

  end;

  Draw_Sweep := True;

end;

{$I ttraster_sweep.inc}

(****************************************************************************)
(*                                                                          *)
(* Function:    Render_Single_Pass                                          *)
(*                                                                          *)
(* Description: Performs one sweep with sub-banding.                        *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if any error was encountered during render.           *)
(*                                                                          *)
(****************************************************************************)

function TFreeTypeRasterizer.Render_Single_Pass( vertical : Boolean; OutputMinY, OutputMaxY, PixelGrain: integer ) : boolean;
var
  OutputY, OutputBandY, BandHeight: Integer;
begin
  Render_Single_Pass := False;
  OutputY := OutputMinY;
  BandHeight := PixelGrain;
  while OutputY+BandHeight < OutputMaxY do BandHeight := BandHeight shl 1;

  while OutputY <= OutputMaxY do
    begin

      Error   := Err_Ras_None;
      OutputBandY := OutputY+BandHeight-1;
      if OutputBandY > OutputMaxY then OutputBandY := OutputMaxY;
      Pool.SetBounds(OutputY*Precision,OutputBandY*Precision);
      Pool.Clear;

      if Pool.Convert_Glyph( vertical, points, flags, outs, nContours ) then
      begin
        if Draw_Sweep(OutputY, OutputBandY, PixelGrain) then
          OutputY := OutputBandY + 1
        else
        begin
          Pool.Clear;
          Pool.ReduceCapacity;
          exit;
        end;

      end else
      begin
        if Error <> Err_Ras_Overflow then exit;
        Error := Err_Ras_None;

        BandHeight := BandHeight shr 1;
        if BandHeight < PixelGrain then
        begin
          Error := Err_Ras_Invalid;
          Pool.Clear;
          Pool.ReduceCapacity;
          exit;
        end;
      end;
    end;

  Pool.Clear;
  Pool.ReduceCapacity;
  Render_Single_Pass := true;
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    Render_Glyph                                                *)
(*                                                                          *)
(* Description: Renders a glyph in a bitmap.      Sub-banding if needed     *)
(*                                                                          *)
(* Input:       AGlyph   Glyph record                                       *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if any error was encountered during render.           *)
(*                                                                          *)
(****************************************************************************)

function TFreeTypeRasterizer.Render_Glyph( var glyph  : TT_Outline;
                       var target : TT_Raster_Map ) : TError;
begin

 Render_Glyph := Failure;

 if Pool = nil then
   begin
     Error := Err_Ras_NotIni;
     exit;
   end;

 if glyph.conEnds^[glyph.n_contours-1] > glyph.n_points then
   begin
     Error := Err_Ras_Invalid_Contours;
     exit;
   end;

 Cible := target;

 Outs      := glyph.conEnds;
 Flags     := PByte(glyph.flags);
 //nPoints   := Glyph.n_points;
 nContours := Glyph.n_contours;

 points := Glyph.points;

 Set_High_Precision( glyph.high_precision );
 pool.SetScaleShift(precision_shift);
 DropOutControl := glyph.dropout_mode;
 second_pass    := glyph.second_pass;

 Error := Err_Ras_None;

 (* Vertical Sweep *)

{$IFDEF FPC}
 Proc_Sweep_Init := @Vertical_Sweep_Init;
 Proc_Sweep_Span := @Vertical_Sweep_Span;
 Proc_Sweep_Drop := @Vertical_Sweep_Drop;
 Proc_Sweep_Step := @Vertical_Sweep_Step;
{$ELSE}
 Proc_Sweep_Init := Vertical_Sweep_Init;
 Proc_Sweep_Span := Vertical_Sweep_Span;
 Proc_Sweep_Drop := Vertical_Sweep_Drop;
 Proc_Sweep_Step := Vertical_Sweep_Step;
{$ENDIF}

 BWidth := Cible.width;
 BCible := PByte( Cible.Buffer );

 if not Render_Single_Pass( False, 0,Cible.Rows-1, 1 ) then exit;

 (* Horizontal Sweep *)

 if Second_Pass then
 begin

{$IFDEF FPC}
   Proc_Sweep_Init := @Horizontal_Sweep_Init;
   Proc_Sweep_Span := @Horizontal_Sweep_Span;
   Proc_Sweep_Drop := @Horizontal_Sweep_Drop;
   Proc_Sweep_Step := @Horizontal_Sweep_Step;
{$ELSE}
   Proc_Sweep_Init := Horizontal_Sweep_Init;
   Proc_Sweep_Span := Horizontal_Sweep_Span;
   Proc_Sweep_Drop := Horizontal_Sweep_Drop;
   Proc_Sweep_Step := Horizontal_Sweep_Step;
{$ENDIF}

   BWidth := Cible.rows;
   BCible := PByte( Cible.Buffer );

   if not Render_Single_Pass( True, 0, Cible.Width-1, 1 ) then exit;

 end;

 Render_Glyph := Success;
end;

procedure TFreeTypeRasterizer.BGray_NeedCapacity(c: integer);
begin
 if c > BGray_Capacity then
 begin
   if BGray_Data <> nil then freemem(BGray_Data);
   BGray_Capacity := c*2;
   getmem(BGray_Data, BGray_Capacity);
 end;
 fillchar(BGray_Data^, c, 0);
end;

(****************************************************************************)
(*                                                                          *)
(* Function:    Render_Gray_Glyph                                           *)
(*                                                                          *)
(* Description: Renders a glyph with grayscaling. Sub-banding if needed     *)
(*                                                                          *)
(* Input:       AGlyph   Glyph record                                       *)
(*                                                                          *)
(* Returns:     True on success                                             *)
(*              False if any error was encountered during render.           *)
(*                                                                          *)
(****************************************************************************)

  function TFreeTypeRasterizer.Render_Gray_Glyph( var glyph   : TT_Outline;
                              var target  : TT_Raster_Map;
                              palette : PTT_Gray_Palette ) : TError;
  const Zoom = 2;
begin

 Render_Gray_Glyph := Failure;

 cible := target;

 if palette <> nil then
   move( palette^, Grays, sizeof(TT_Gray_Palette) );

 Outs      := Glyph.conEnds;
 Flags     := PByte(glyph.flags);
 //nPoints   := Glyph.n_points;
 nContours := Glyph.n_contours;

 points := Glyph.points;

 Set_High_Precision( glyph.high_precision );
 pool.SetScaleShift(precision_shift+1);
 DropOutControl := glyph.dropout_mode;
 second_pass    := glyph.high_precision;

 Error := Err_Ras_None;

 BGray_Incr := (Cible.Width*Zoom+7) shr 3;
 BGray_End  := BGray_Incr*Zoom;
 BGray_NeedCapacity(BGray_End);
 BWidth := BGray_Incr shl 3;

 BCible := PByte( BGray_Data   );
 GCible := PByte( Cible.Buffer );

{$IFDEF FPC}
 Proc_Sweep_Init := @Vertical_Gray_Sweep_Init;
 Proc_Sweep_Span := @Vertical_Sweep_Span;
 Proc_Sweep_Drop := @Vertical_Sweep_Drop;
 Proc_Sweep_Step := @Vertical_Gray_Sweep_Step;
{$ELSE}
 Proc_Sweep_Init := Vertical_Gray_Sweep_Init;
 Proc_Sweep_Span := Vertical_Sweep_Span;
 Proc_Sweep_Drop := Vertical_Sweep_Drop;
 Proc_Sweep_Step := Vertical_Gray_Sweep_Step;
{$ENDIF}

 if not Render_Single_Pass( False, 0, Zoom*Cible.Rows - 1, Zoom ) then exit;

 (* Horizontal Sweep *)

 if Second_Pass then
 begin

{$IFDEF FPC}
   Proc_Sweep_Init := @Horizontal_Sweep_Init;
   Proc_Sweep_Span := @Horizontal_Gray_Sweep_Span;
   Proc_Sweep_Drop := @Horizontal_Gray_Sweep_Drop;
   Proc_Sweep_Step := @Horizontal_Sweep_Step;
{$ELSE}
   Proc_Sweep_Init := Horizontal_Sweep_Init;
   Proc_Sweep_Span := Horizontal_Gray_Sweep_Span;
   Proc_Sweep_Drop := Horizontal_Gray_Sweep_Drop;
   Proc_Sweep_Step := Horizontal_Sweep_Step;
{$ENDIF}

   BWidth := Cible.rows;
   GCible := PByte( Cible.Buffer );

   if not Render_Single_Pass( True, 0,Cible.Width*Zoom-1, Zoom ) then exit;

 end;

 Render_Gray_Glyph := Success;
 exit;

end;

function TFreeTypeRasterizer.Render_Gray_Glyph_HQ( var glyph   : TT_Outline;
                            var target  : TT_Raster_Map ) : TError;
const Zoom = 8;
begin

  Render_Gray_Glyph_HQ := Failure;

  cible := target;

  Outs      := Glyph.conEnds;
  Flags     := PByte(glyph.flags);
  //nPoints   := Glyph.n_points;
  nContours := Glyph.n_contours;

  points := Glyph.points;

  Set_High_Precision( false );
  pool.SetScaleShift(precision_shift+3);
  DropOutControl := glyph.dropout_mode;
  second_pass    := false;

  Error := Err_Ras_None;

  BGray_Incr := (Cible.Width*Zoom+7) shr 3;
  BGray_End  := BGray_Incr*Zoom;
  BGray_NeedCapacity(BGray_End);
  BWidth := BGray_Incr shl 3;

  BCible := PByte( BGray_Data   );
  GCible := PByte( Cible.Buffer );

  {$IFDEF FPC}
  Proc_Sweep_Init := @Vertical_Gray_Sweep_Init_HQ;
  Proc_Sweep_Span := @Vertical_Sweep_Span;
  Proc_Sweep_Drop := @Vertical_Sweep_Drop;
  Proc_Sweep_Step := @Vertical_Gray_Sweep_Step_HQ;
  {$ELSE}
  Proc_Sweep_Init := Vertical_Gray_Sweep_Init_HQ;
  Proc_Sweep_Span := Vertical_Sweep_Span;
  Proc_Sweep_Drop := Vertical_Sweep_Drop;
  Proc_Sweep_Step := Vertical_Gray_Sweep_Step_HQ;
  {$ENDIF}

  if not Render_Single_Pass( False, 0, Zoom*Cible.Rows - 1, Zoom ) then exit;

  Render_Gray_Glyph_HQ := Success;
  exit;

end;

{************************ direct rendering ********************}

procedure TFreeTypeRasterizer.Vertical_Gray_Sweep_Init_Direct_HQ( var min, max : Int );
begin
  Vertical_Gray_Sweep_Init_HQ ( min, max);
  dec(Direct_Y, min div 8);
  traceGOfs := 0;
  TraceGIncr:= 0;
end;

procedure TFreeTypeRasterizer.Vertical_Gray_Sweep_Step_Direct_HQ;
begin
  Vertical_Gray_Sweep_Step_HQ;
  If TraceBOfs = 0 then
  begin
    Proc_Sweep_Direct(Direct_X,Direct_Y,Direct_TX,pointer(GCible));
    dec(Direct_Y);
    fillchar(cible.Buffer^, cible.Cols, 0);
  end;
end;

procedure TFreeTypeRasterizer.Vertical_Gray_Sweep_Init_Direct( var min, max : Int );
begin
  Vertical_Gray_Sweep_Init ( min, max);
  dec(Direct_Y, min div 2);
  traceGOfs := 0;
  TraceGIncr:= 0;
end;

procedure TFreeTypeRasterizer.Vertical_Gray_Sweep_Step_Direct;
begin
  Vertical_Gray_Sweep_Step;
  If TraceBOfs = 0 then
  begin
    Proc_Sweep_Direct(Direct_X,Direct_Y,Direct_TX,pointer(GCible));
    dec(Direct_Y);
    fillchar(cible.Buffer^, cible.Cols, 0);
  end;
end;

function TFreeTypeRasterizer.Render_Directly_Gray_Glyph(var glyph: TT_Outline; x, y, tx,
  ty: integer; OnRender: TDirectRenderingFunction; palette : PTT_Gray_Palette): TError;
const Zoom = 2;
begin

  Render_Directly_Gray_Glyph := Failure;

  if palette <> nil then
    move( palette^, Grays, sizeof(TT_Gray_Palette) );

  Direct_X := x;
  Direct_Y := y+ty-1;
  Direct_TX := tx;

  cible.Width := tx;
  cible.Rows := ty;
  cible.Cols := (cible.Width+3) and not 3;
  cible.Flow := TT_Flow_Down;
  getmem(cible.Buffer, cible.Cols);
  fillchar(cible.Buffer^, cible.Cols, 0);

  Outs      := Glyph.conEnds;
  Flags     := PByte(glyph.flags);
  //nPoints   := Glyph.n_points;
  nContours := Glyph.n_contours;

  points := Glyph.points;

  Set_High_Precision( glyph.high_precision );
  pool.SetScaleShift(precision_shift+1);
  DropOutControl := glyph.dropout_mode;
  second_pass    := false;

  Error := Err_Ras_None;

  BGray_Incr := (Cible.Width*Zoom+7) shr 3;
  BGray_End  := BGray_Incr*Zoom;
  BGray_NeedCapacity(BGray_End);
  BWidth := BGray_Incr shl 3;

  BCible := PByte( BGray_Data   );
  GCible := PByte( Cible.Buffer );

  {$IFDEF FPC}
  Proc_Sweep_Init := @Vertical_Gray_Sweep_Init_Direct;
  Proc_Sweep_Span := @Vertical_Sweep_Span;
  Proc_Sweep_Drop := @Vertical_Sweep_Drop;
  Proc_Sweep_Step := @Vertical_Gray_Sweep_Step_Direct;
  Proc_Sweep_Direct:= OnRender;
  {$ELSE}
  Proc_Sweep_Init := Vertical_Gray_Sweep_Init_Direct;
  Proc_Sweep_Span := Vertical_Sweep_Span;
  Proc_Sweep_Drop := Vertical_Sweep_Drop;
  Proc_Sweep_Step := Vertical_Gray_Sweep_Step_Direct;
  Proc_Sweep_Direct:= OnRender;
  {$ENDIF}

  if Render_Single_Pass( False, 0, Zoom*Cible.Rows - 1, Zoom ) then
    Render_Directly_Gray_Glyph := Success;

  freemem(cible.Buffer, cible.Cols);

end;

function TFreeTypeRasterizer.Render_Directly_Gray_Glyph_HQ( var glyph   : TT_Outline;
                            x,y,tx,ty: integer;
                            OnRender: TDirectRenderingFunction) : TError;
const Zoom = 8;
begin

  Render_Directly_Gray_Glyph_HQ := Failure;

  Direct_X := x;
  Direct_Y := y+ty-1;
  Direct_TX := tx;

  cible.Width := tx;
  cible.Rows := ty;
  cible.Cols := (cible.Width+3) and not 3;
  cible.Flow := TT_Flow_Down;
  getmem(cible.Buffer, cible.Cols);
  fillchar(cible.Buffer^, cible.Cols, 0);

  Outs      := Glyph.conEnds;
  Flags     := PByte(glyph.flags);
  //nPoints   := Glyph.n_points;
  nContours := Glyph.n_contours;

  points := Glyph.points;

  Set_High_Precision( false );
  pool.SetScaleShift(precision_shift+3);
  DropOutControl := glyph.dropout_mode;
  second_pass    := false;

  Error := Err_Ras_None;

  BGray_Incr := (Cible.Width*Zoom+7) shr 3;
  BGray_End  := BGray_Incr*Zoom;
  BGray_NeedCapacity(BGray_End);
  BWidth := BGray_Incr shl 3;

  BCible := PByte( BGray_Data   );
  GCible := PByte( Cible.Buffer );

  {$IFDEF FPC}
  Proc_Sweep_Init := @Vertical_Gray_Sweep_Init_Direct_HQ;
  Proc_Sweep_Span := @Vertical_Sweep_Span;
  Proc_Sweep_Drop := @Vertical_Sweep_Drop;
  Proc_Sweep_Step := @Vertical_Gray_Sweep_Step_Direct_HQ;
  Proc_Sweep_Direct:= OnRender;
  {$ELSE}
  Proc_Sweep_Init := Vertical_Gray_Sweep_Init_Direct_HQ;
  Proc_Sweep_Span := Vertical_Sweep_Span;
  Proc_Sweep_Drop := Vertical_Sweep_Drop;
  Proc_Sweep_Step := Vertical_Gray_Sweep_Step_Direct_HQ;
  Proc_Sweep_Direct:= OnRender;
  {$ENDIF}

  if Render_Single_Pass( False, 0, Zoom*Cible.Rows - 1, Zoom ) then
    Render_Directly_Gray_Glyph_HQ := Success;

  freemem(cible.Buffer, cible.Cols);

end;

procedure TFreeTypeRasterizer.Set_Raster_Palette(const palette: TT_Gray_Palette);
begin
  move( palette, Grays, sizeof(TT_Gray_Palette) );
end;

constructor TFreeTypeRasterizer.Create;
begin
  Raster_Object_Init;
end;

destructor TFreeTypeRasterizer.Destroy;
begin
  Raster_Object_Done;
end;



(****************************************************************************)
(*                                                                          *)
(* Function:    Init_Rasterizer                                             *)
(*                                                                          *)
(* Description: Initializes the rasterizer.                                 *)
(*                                                                          *)
(* Input:       rasterBlock   target bitmap/pixmap description              *)
(*              profBuffer    pointer to the render pool                    *)
(*              profSize      size in bytes of the render pool              *)
(*                                                                          *)
(* Returns:     1 ( always, but we should check parameters )                *)
(*                                                                          *)
(****************************************************************************)

procedure TFreeTypeRasterizer.Raster_Object_Init;
const
  Default_Grays : array[0..4] of Byte
                = ( 0, 23, 27, 29, 31 );
var i: integer;
begin
  Pool := nil;
  BGray_Data := nil;
  BGray_Capacity := 0;

  (* default Grays takes the gray levels of the standard VGA *)
  (* 256 colors mode                                                *)

  for i := 0 to high(Grays) do
    Grays[i] := Default_Grays[i];

  Set_High_Precision(False);
  Set_Second_Pass(False);
  Pool := TRenderPool.Create(Precision,Precision_Step);

  DropOutControl := 2;
  Error          := Err_Ras_None;
end;

procedure TFreeTypeRasterizer.Raster_Object_Done;
begin
  Pool.Free;
  if BGray_Data <> nil then
    FreeMem( BGray_Data, BGray_Capacity );
end;

var
  DefaultRasterizer: TFreeTypeRasterizer;

function TTRaster_Init: TError;
var l,c,i,j: integer;
begin
  { Initialisation of Count_Table }

  for i := 0 to 255 do
  begin
    l := 0;
    j := i;
    for c := 0 to 3 do
    begin
      l := l shl 4;
      if ( j and $80 <> 0 ) then inc(l);
      if ( j and $40 <> 0 ) then inc(l);
      j := (j shl 2) and $FF;
    end;
    Count_table[i] := l;
  end;

  for i := 0 to 255 do
  begin
    BitCountTable[i] := (i and 1) + (i shr 1 and 1) + (i shr 2 and 1) + (i shr 3 and 1) +
       (i shr 4 and 1) + (i shr 5 and 1) + (i shr 6 and 1) + (i shr 7 and 1);
  end;

  DefaultRasterizer := nil;

  result := Success;
end;

procedure TTRaster_Done;
begin
  if DefaultRasterizer <> nil then
    DefaultRasterizer.Free;
end;

function TTGetDefaultRasterizer: TFreeTypeRasterizer;
begin
  if DefaultRasterizer = nil then
    DefaultRasterizer := TFreeTypeRasterizer.Create;
  result := DefaultRasterizer;
end;

end.
