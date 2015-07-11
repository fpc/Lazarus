{    This flag is currently ignored by the Virtual Compiler }

(***********************************************************************)
(*                                                                     *)
(*  Vertical Sweep Procedure Set :                                     *)
(*                                                                     *)
(*  These three routines are used during the vertical black/white      *)
(*  sweep phase by the generic Draw_Sweep function.                    *)
(*                                                                     *)
(***********************************************************************)

procedure TFreeTypeRasterizer.Vertical_Sweep_Init( var min, {%H-}max : Int );
begin
  case Cible.flow of

    TT_Flow_Up : begin
                   traceBOfs  := min * Cible.cols;
                   traceBIncr := Cible.cols;
                 end;
  else
    traceBOfs  := (Cible.rows - 1 - min)*Cible.cols;
    traceBIncr := -Cible.cols;
  end;

  gray_min_x := 0;
  gray_max_x := 0;
end;



procedure TFreeTypeRasterizer.Vertical_Sweep_Span( {%H-}y     : Int;
                               x1,
                               x2    : TT_F26dot6;
                               {%H-}Left,
                               {%H-}Right : TProfile );
var
  e1, e2  : Longint;
  c1, c2  : Int;
  f1, f2  : Int;
  base    : PByte;
begin
  e1 := (( x1+Precision-1 ) and Precision_Mask) div Precision;

  if ( x2-x1-Precision <= Precision_Jitter ) then
    e2 := e1
  else
    e2 := ( x2 and Precision_Mask ) div Precision;

  if (e2 >= 0) and (e1 < BWidth) then

    begin
      if e1 <  0      then e1 := 0;
      if e2 >= BWidth then e2 := BWidth-1;

      c1 := e1 shr 3;
      c2 := e2 shr 3;

      f1 := e1 and 7;
      f2 := e2 and 7;

      if gray_min_X > c1 then gray_min_X := c1;
      if gray_max_X < c2 then gray_max_X := c2;

      base := @BCible^[TraceBOfs + c1];

      if c1 = c2 then
        base^[0] := base^[0] or ( LMask[f1] and Rmask[f2] )
      else
       begin
         base^[0] := base^[0] or LMask[f1];

         if c2>c1+1 then
           FillChar( base^[1], c2-c1-1, $FF );

         base     := @base^[c2-c1];
         base^[0] := base^[0] or RMask[f2];
       end
    end;
end;


procedure TFreeTypeRasterizer.Vertical_Sweep_Drop( y     : Int;
                               x1,
                               x2    : TT_F26dot6;
                               Left,
                               Right : TProfile );
var
  e1, e2  : Longint;
  c1  : Int;
  f1  : Int;

  j : Int;
begin

  (* Drop-out control *)

  e1 := ( x1+Precision-1 ) and Precision_Mask;
  e2 := x2 and Precision_Mask;

  (* We are guaranteed that x2-x1 <= Precision here *)

  if e1 > e2 then
   if e1 = e2 + Precision then

    case DropOutControl of

      (* Drop-out Control Rule #3 *)
      1 : e1 := e2;

      4 : begin
            e1 := ((x1+x2+1) div 2 + Precision-1) and Precision_Mask;
            e2 := e1;
          end;

      (* Drop-out Control Rule #4 *)

      (* The spec is not very clear regarding rule #4. It       *)
      (* presents a method that is way too costly to implement  *)
      (* while the general idea seems to get rid of 'stubs'.    *)
      (*                                                        *)
      (* Here, we only get rid of stubs recognized when :       *)
      (*                                                        *)
      (*  upper stub :                                          *)
      (*                                                        *)
      (*   - P_Left and P_Right are in the same contour         *)
      (*   - P_Right is the successor of P_Left in that contour *)
      (*   - y is the top of P_Left and P_Right                 *)
      (*                                                        *)
      (*  lower stub :                                          *)
      (*                                                        *)
      (*   - P_Left and P_Right are in the same contour         *)
      (*   - P_Left is the successor of P_Right in that contour *)
      (*   - y is the bottom of P_Left                          *)
      (*                                                        *)

      2,5 : begin

            if ( x2-x1 < Precision_Half ) then
            begin
              (* upper stub test *)

              if ( Left.nextInContour = Right ) and
                 ( Left.Height <= 0 )  then exit;

              (* lower stub test *)

              if ( Right.nextInContour = Left ) and
                 ( Left.Start = y   ) then exit;
            end;

            (* Check that the rightmost pixel is not already set *)
            e1 := e1 div Precision;

            c1 := e1 shr 3;
            f1 := e1 and 7;

            if ( e1 >= 0 ) and ( e1 < BWidth )                and
               ( BCible^[TraceBOfs+c1] and ($80 shr f1) <> 0 ) then
              exit;

            case DropOutControl of
              2 : e1 := e2;
              5 : e1 := ((x1+x2+1) div 2 + Precision-1) and Precision_Mask;
            end;

            e2 := e1;

          end;
    else
      exit;  (* unsupported mode *)
    end

   else
  else
    e2 := e1;   (* when x1 = e1, x2 = e2, e2 = e1 + 64 *)

  e1 := e1 div Precision;

  if (e1 >= 0) and (e1 < BWidth ) then
    begin
      c1 := e1 shr 3;
      f1 := e1 and 7;

      if gray_min_X > c1 then gray_min_X := c1;
      if gray_max_X < c1 then gray_max_X := c1;

      j := TraceBOfs + c1;

      BCible^[j] := BCible^[j] or ($80 shr f1);
    end;
end;



procedure TFreeTypeRasterizer.Vertical_Sweep_Step;
begin
  inc( TraceBOfs, traceBIncr );
end;


(***********************************************************************)
(*                                                                     *)
(*  Horizontal Sweep Procedure Set :                                   *)
(*                                                                     *)
(*  These three routines are used during the horizontal black/white    *)
(*  sweep phase by the generic Draw_Sweep function.                    *)
(*                                                                     *)
(***********************************************************************)

procedure TFreeTypeRasterizer.Horizontal_Sweep_Init( var {%H-}min, {%H-}max : Int );
begin
  (* Nothing, really *)
end;


procedure TFreeTypeRasterizer.Horizontal_Sweep_Span( y     : Int;
                                 x1,
                                 x2    : TT_F26dot6;
                                 {%H-}Left,
                                 {%H-}Right : TProfile );
var
  e1, e2  : Longint;
  c1  : Int;
  f1  : Int;

  j : Int;
begin

  if ( x2-x1 < Precision ) then
  begin
    e1 := ( x1+(Precision-1) ) and Precision_Mask;
    e2 := x2 and Precision_Mask;

    if e1 = e2 then
    begin
      c1 := y shr 3;
      f1 := y and 7;

      if (e1 >= 0) then
      begin
        e1 := e1 shr Precision_Bits;
        if Cible.flow = TT_Flow_Up then
          j := c1 + e1*Cible.cols
        else
          j := c1 + (Cible.rows-1-e1)*Cible.cols;
        if e1 < Cible.Rows then
          BCible^[j] := BCible^[j] or ($80 shr f1);
      end;
    end;
  end;

{$IFDEF RIEN}
  e1 := ( x1+(Precision-1) ) and Precision_Mask;
  e2 := x2 and Precision_Mask;

  (* We are here guaranteed that x2-x1 > Precision *)

   c1 := y shr 3;
   f1 := y and 7;

   if (e1 >= 0) then
   begin
     e1 := e1 shr Precision_Bits;
     if Cible.flow = TT_Flow_Up then
       j := c1 + e1*Cible.cols
     else
       j := c1 + (Cible.rows-1-e1)*Cible.cols;
     if e1 < Cible.Rows then
       BCible^[j] := BCible^[j] or ($80 shr f1);
   end;

   if (e2 >= 0) then
   begin
     e2 := e2 shr Precision_Bits;
     if Cible.flow = TT_Flow_Up then
       j := c1 + e1*Cible.cols
     else
       j := c1 + (Cible.rows-1-e2)*Cible.cols;
     if (e2 <> e1) and (e2 < Cible.Rows) then
       BCible^[j] := BCible^[j] or ($80 shr f1);
   end;
{$ENDIF}

end;



procedure TFreeTypeRasterizer.Horizontal_Sweep_Drop( y     : Int;
                                 x1,
                                 x2    : TT_F26dot6;
                                 Left,
                                 Right : TProfile );
var
  e1, e2  : Longint;
  c1  : Int;
  f1  : Int;

  j : Int;
begin

  e1 := ( x1+(Precision-1) ) and Precision_Mask;
  e2 := x2 and Precision_Mask;

  (* During the horizontal sweep, we only take care of drop-outs *)

  if e1 > e2 then
   if e1 = e2 + Precision then

    case DropOutControl of

      0 : exit;

      (* Drop-out Control Rule #3 *)
      1 : e1 := e2;

      4 : begin
            e1 := ( (x1+x2) div 2 +Precision div 2 ) and Precision_Mask;
            e2 := e1;
          end;

      (* Drop-out Control Rule #4 *)

      (* The spec is not very clear regarding rule #4. It       *)
      (* presents a method that is way too costly to implement  *)
      (* while the general idea seems to get rid of 'stubs'.    *)
      (*                                                        *)

      2,5 : begin

              (* rightmost stub test *)

              if ( Left.nextInContour = Right ) and
                 ( Left.Height <= 0 )  then exit;

              (* leftmost stub test *)

              if ( Right.nextInContour = Left ) and
                 ( Left.Start = y   ) then exit;

              (* Check that the upmost pixel is not already set *)

              e1 := e1 div Precision;

              c1 := y shr 3;
              f1 := y and 7;

              if Cible.flow = TT_Flow_Up then
                j := c1 + e1*Cible.cols
              else
                j := c1 + (Cible.rows-1-e1)*Cible.cols;

              if ( e1 >= 0 ) and ( e1 < Cible.Rows ) and
                 ( BCible^[j] and ($80 shr f1) <> 0 ) then exit;

              case DropOutControl of
                2 : e1 := e2;
                5 : e1 := ((x1+x2) div 2 + Precision_Half) and Precision_Mask;
              end;

              e2 := e1;
            end;
    else
      exit;  (* Unsupported mode *)
    end;

   c1 := y shr 3;
   f1 := y and 7;

   if (e1 >= 0) then
   begin
     e1 := e1 shr Precision_Bits;
     if Cible.flow = TT_Flow_Up then
       j := c1 + e1*Cible.cols
     else
       j := c1 + (Cible.rows-1-e1)*Cible.cols;
     if e1 < Cible.Rows then BCible^[j] := BCible^[j] or ($80 shr f1);
   end;

end;



procedure TFreeTypeRasterizer.Horizontal_Sweep_Step;
begin
  (* Nothing, really *)
end;

(***********************************************************************)
(*                                                                     *)
(*  Vertical Gray Sweep Procedure Set :                                *)
(*                                                                     *)
(*  These two   routines are used during the vertical gray-levels      *)
(*  sweep phase by the generic Draw_Sweep function.                    *)
(*                                                                     *)
(*                                                                     *)
(*  NOTES :                                                            *)
(*                                                                     *)
(*  - The target pixmap's width *must* be a multiple of 4              *)
(*                                                                     *)
(*  - you have to use the function Vertical_Sweep_Span for             *)
(*    the gray span call.                                              *)
(*                                                                     *)
(***********************************************************************)

procedure TFreeTypeRasterizer.Vertical_Gray_Sweep_Init( var min, {%H-}max : Int );
begin
  case Cible.flow of

    TT_Flow_Up : begin
                   traceGOfs  := (min div 2)*Cible.cols;
                   traceGIncr := Cible.cols;
                 end;
  else
    traceGOfs  := (Cible.rows-1- (min div 2))*Cible.cols;
    traceGIncr := -Cible.cols;
  end;

  TraceBOfs   :=  0;
  TraceBIncr  :=  BGray_Incr;
  gray_min_x :=  Cible.Cols;
  gray_max_x := -Cible.Cols;
end;

procedure TFreeTypeRasterizer.Vertical_Gray_Sweep_Init_HQ( var min, {%H-}max : Int );
begin
  case Cible.flow of

    TT_Flow_Up : begin
                   traceGOfs  := (min div 8)*Cible.cols;
                   traceGIncr := Cible.cols;
                 end;
    TT_Flow_Down: begin
                  traceGOfs  := (Cible.rows-1- (min div 8))*Cible.cols;
                  traceGIncr := -Cible.cols;
                end;
    else
    begin
      traceGOfs := 0;
      traceGIncr := 0;
    end;
  end;

  TraceBOfs   :=  0;
  TraceBIncr  :=  BGray_Incr;
  gray_min_x :=  Cible.Cols;
  gray_max_x := -Cible.Cols;
end;


procedure TFreeTypeRasterizer.Vertical_Gray_Sweep_Step;
var
  j, c1, c2 : Int;
begin
  inc( TraceBOfs, TraceBIncr );

  if TraceBOfs = BGray_End then
  begin

    if gray_max_X >= 0 then
    begin

      if gray_max_x > cible.cols-1 then gray_max_x := cible.cols-1;
      if gray_min_x < 0            then gray_min_x := 0;

      j := TraceGOfs + gray_min_x*4;

      for c1 := gray_min_x to gray_max_x do
      begin

        c2 := Count_Table[ BCible^[c1           ] ] +
              Count_Table[ BCible^[c1+BGray_Incr] ];

        if c2 <> 0 then
        begin
          BCible^[c1           ] := 0;
          BCible^[c1+BGray_Incr] := 0;

          GCible^[j] := GCible^[j] or Grays[ (c2 and $F000) shr 12 ]; inc(j);
          GCible^[j] := GCible^[j] or Grays[ (c2 and $0F00) shr  8 ]; inc(j);
          GCible^[j] := GCible^[j] or Grays[ (c2 and $00F0) shr  4 ]; inc(j);
          GCible^[j] := GCible^[j] or Grays[ (c2 and $000F)        ]; inc(j);
        end
        else
          inc( j, 4 );

      end;
    end;

    TraceBOfs   := 0;
    inc( TraceGOfs, traceGIncr );

    gray_min_x :=  Cible.Cols;
    gray_max_x := -Cible.Cols;
  end;
end;

procedure TFreeTypeRasterizer.Vertical_Gray_Sweep_Step_HQ;
var
  j, c1 : Int;
  c2, c3: byte;
begin
  inc( TraceBOfs, TraceBIncr );

  if TraceBOfs = BGray_End then
  begin

    if gray_max_X >= 0 then
    begin

      if gray_max_x > cible.cols-1 then gray_max_x := cible.cols-1;
      if gray_min_x < 0            then gray_min_x := 0;

      j := TraceGOfs + gray_min_x;

      for c1 := gray_min_x to gray_max_x do
      begin

        c2 := BitCountTable[ BCible^[c1                ] ] +
              BitCountTable[ BCible^[c1 + BGray_Incr   ] ] +
              BitCountTable[ BCible^[c1 + BGray_Incr*2 ] ] +
              BitCountTable[ BCible^[c1 + BGray_Incr*3 ] ] +
              BitCountTable[ BCible^[c1 + BGray_Incr*4 ] ] +
              BitCountTable[ BCible^[c1 + BGray_Incr*5 ] ] +
              BitCountTable[ BCible^[c1 + BGray_Incr*6 ] ] +
              BitCountTable[ BCible^[c1 + BGray_Incr*7 ] ];

        if c2 <> 0 then
        begin
          BCible^[c1             ] := 0;
          BCible^[c1+BGray_Incr  ] := 0;
          BCible^[c1+BGray_Incr*2] := 0;
          BCible^[c1+BGray_Incr*3] := 0;
          BCible^[c1+BGray_Incr*4] := 0;
          BCible^[c1+BGray_Incr*5] := 0;
          BCible^[c1+BGray_Incr*6] := 0;
          BCible^[c1+BGray_Incr*7] := 0;

          if c2 >= 63 then GCible^[j] := $ff else
          begin
            c2 := c2 shl 2;
            c3 := GCible^[j];
            if c3 = 0 then GCible^[j] := c2 else
             GCible^[j] := c2 + (c3*(not c2) shr 8);
          end;
        end;
        inc( j );

      end;
    end;

    TraceBOfs   := 0;
    inc( TraceGOfs, traceGIncr );

    gray_min_x :=  Cible.Cols;
    gray_max_x := -Cible.Cols;
  end;
end;

(***********************************************************************)
(*                                                                     *)
(*  Horizontal Gray Sweep Procedure Set :                              *)
(*                                                                     *)
(*  These three routines are used during the horizontal gray-levels    *)
(*  sweep phase by the generic Draw_Sweep function.                    *)
(*                                                                     *)
(***********************************************************************)

procedure TFreeTypeRasterizer.Horizontal_Gray_Sweep_Span( y     : Int;
                                      x1,
                                      x2    : TT_F26dot6;
                                      {%H-}Left,
                                      {%H-}Right : TProfile );
var
  e1, e2    : TT_F26Dot6;
  j : Int;
begin
  exit;
  y  := y div 2;

  e1 := ( x1+(Precision-1) ) and Precision_Mask;
  e2 := x2 and Precision_Mask;

  if (e1 >= 0) then
  begin
    e1 := e1 shr (Precision_Bits+1);
(*    if Cible.flow = TT_Flow_Up then *)
      j := y + e1*Cible.cols;
(*    else
//      j := y + (Cible.rows-1-e1)*Cible.cols;  *)
    if e1 < Cible.Rows then
      if GCible^[j] = Grays[0] then
        GCible^[j] := Grays[1];
  end;

  if (e2 >= 0) then
  begin
    e2 := e2 shr (Precision_Bits+1);
(*    if Cible.flow = TT_Flow_Up then *)
      j := y + e2*Cible.cols;
(*    else
//      j := y + (Cible.rows-1-e2)*Cible.cols; *)
    if (e2 <> e1) and (e2 < Cible.Rows) then
      if GCible^[j] = Grays[0] then
        GCible^[j] := Grays[1];
  end;

end;


procedure TFreeTypeRasterizer.Horizontal_Gray_Sweep_Drop( y     : Int;
                                      x1,
                                      x2    : TT_F26dot6;
                                      Left,
                                      Right : TProfile );
var
  e1, e2  : Longint;
  color   : Byte;
  j : Int;
begin

  e1 := ( x1+(Precision-1) ) and Precision_Mask;
  e2 := x2 and Precision_Mask;

  (* During the horizontal sweep, we only take care of drop-outs *)

  if e1 > e2 then
   if e1 = e2 + Precision then

    case DropOutControl of

      0 : exit;

      (* Drop-out Control Rule #3 *)
      1 : e1 := e2;

      4 : begin
            e1 := ( (x1+x2) div 2 +Precision div 2 ) and Precision_Mask;
            e2 := e1;
          end;

      (* Drop-out Control Rule #4 *)

      (* The spec is not very clear regarding rule #4. It       *)
      (* presents a method that is way too costly to implement  *)
      (* while the general idea seems to get rid of 'stubs'.    *)
      (*                                                        *)

      2,5 : begin

              (* lowest stub test *)

              if ( Left.nextInContour = Right ) and
                 ( Left.Height <= 0 )  then exit;

              (* upper stub test *)

              if ( Right.nextInContour = Left ) and
                 ( Left.Start = y    ) then exit;

              case DropOutControl of
                2 : e1 := e2;
                5 : e1 := ((x1+x2) div 2 + Precision_Half) and Precision_Mask;
              end;

              e2 := e1;
            end;
    else
      exit;  (* Unsupported mode *)
    end;

   if (e1 >= 0) then
   begin
     (* A small trick to make 'average' thin line appear in *)
     (* medium gray..                                       *)

     if ( x2-x1 >= Precision_Half ) then
       color := Grays[2]
     else color := Grays[1];

     e1 := e1 shr (Precision_Bits+1);
     if Cible.flow = TT_Flow_Up then
       j := (y div 2) + e1*Cible.cols
     else
       j := (y div 2) + (Cible.rows-1-e1)*Cible.cols;
     if e1 < Cible.Rows then
       if GCible^[j] = Grays[0] then
         GCible^[j] := color;
   end;
end;

