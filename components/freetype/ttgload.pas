(*******************************************************************
 *
 *  ttgload.pas                                                  1.0
 *
 *    TrueType glyph loader
 *
 *  Copyright 1996, 1997 by
 *  David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 ******************************************************************)

Unit TTGLoad;

interface

{$R-} // TODO: Fix out-of-bounds accesses.
{$I TTCONFIG.INC}

uses
  LazFreeType, TTError, TTTypes, TTObjs;

  function  Load_TrueType_Glyph( instance    : PInstance;
                                 glyph       : PGlyph;
                                 glyph_index : Word;
                                 load_flags  : Int     ) : TError;
  (* loads a font glyph into a given glyph info. The instance and  *)
  (* glyph objects faces _must_ match. The load_flags indicates    *)
  (* what kind of values should be written to the glyph object     *)
  (* ( metrics, outline in EM coordinates, grid-fitted outline.. ) *)
  (*                                                               *)
  (* by default ( i.e. with load_flags = 0 ), this function only   *)
  (* returns the unscaled glyph metrics and points in EM units.    *)
  (*                                                               *)
  (* Use the following flags to query scaling and hinting ops.     *)

 (********************************************************)
 (* return horizontal or vertical metrics in font units  *)
 (* for a given glyph. The metrics are the left side     *)
 (* bearing [resp. top side bearing] and advance width   *)
 (* [resp. advance height].                              *)
 (*                                                      *)
 (* This function may move later to another component..  *)
 (*                                                      *)
 procedure TT_Get_Metrics( var header  : TT_Horizontal_Header;
                           index       : Int;
                           var bearing : TT_Pos;
                           var advance : TT_Pos );

 function  Get_Advance_Widths( face : PFace;
                               ppem : Int    ) : PByte;

implementation

uses
  TTTables,
  TTCalc,
  TTFile,
  TTLoad,
  TTVar;

const
  ARGS_ARE_WORDS      = $01;
  ARGS_ARE_XY_VALUES  = $02;
  ROUND_XY_TO_GRID    = $04;
  WE_HAVE_A_SCALE     = $08;
  (* reserved           $10 *)
  MORE_COMPONENTS     = $20;
  WE_HAVE_AN_XY_SCALE = $40;
  WE_HAVE_A_2X2       = $80;
  WE_HAVE_INSTR       = $100;
  USE_MY_METRICS      = $200;


 (********************************************************)
 (* return horizontal or vertical metrics in font units  *)
 (* for a given glyph. The metrics are the left side     *)
 (* bearing [resp. top side bearing] and advance width   *)
 (* [resp. advance height].                              *)
 (*                                                      *)
 (* This function may move later to another component..  *)
 (*                                                      *)
 procedure TT_Get_Metrics( var header  : TT_Horizontal_Header;
                           index       : Int;
                           var bearing : TT_Pos;
                           var advance : TT_Pos );
 var
   k     : Int;
   longs : PTableLongMetrics;
 begin
   k := header.number_Of_HMetrics;

   if index < k then
     begin
       longs   := PTableLongMetrics(header.long_metrics);
       bearing := longs^[index].bearing;
       advance := longs^[index].advance;
     end
   else
     begin
       bearing := PTableShortMetrics(header.short_metrics)^[index-k];
       advance := PTableLongMetrics(header.long_metrics)^[k-1].advance;
     end;
 end;



 (********************************************************)
 (* return horizontal metrics in font units for a        *)
 (* given glyph. if "check" is true, take care of        *)
 (* mono-spaced fonts by returning the aw max.           *)
 (*                                                      *)
 procedure Get_HMetrics( face     : PFace;
                         index    : Int;
                         check    : Boolean;
                         var  lsb : Int;
                         var  aw  : Int );
 var
   left_bearing, advance : TT_Pos;
 begin
   TT_Get_Metrics( face^.horizontalHeader, index, left_bearing{%H-}, advance{%H-});

   lsb := Int(left_bearing);
   aw  := Int(advance);

   if check and (face^.postscript.isFixedPitch <> 0) then
     aw := face^.horizontalHeader.advance_Width_Max;
 end;



 (********************************************************)
 (* return advance width table for a given pixel size    *)
 (* if it is found in the font's "hdmx" table (if any)   *)
 (*                                                      *)
 function Get_Advance_Widths( face  : PFace;
                              ppem  : Int    ) : PByte;
 var
   n : Integer;
 begin
   with face^.hdmx do
     for n := 0 to num_records-1 do
       if records^[n].ppem = ppem then
       begin
         Get_Advance_Widths := records^[n].widths;
         exit;
       end;

   Get_Advance_Widths := nil;
 end;

 (********************************************************)
 (* copy current glyph into original one                 *)
 (*                                                      *)
 procedure  cur_to_org( n   : int;
                        pts : PGlyph_Zone );
 var
   k : int;
 begin
   for k := 0 to n-1 do with pts^ do
     org^[k] := cur^[k];
 end;


 (********************************************************)
 (* copy original glyph into current one                 *)
 (*                                                      *)
 procedure  org_to_cur( n   : int;
                        pts : PGlyph_Zone );
 var
   k : int;
 begin
   for k := 0 to n-1 do with pts^ do
     cur^[k] := org^[k];
 end;

 (********************************************************)
 (* translate an array of coordinates                    *)
 (*                                                      *)
 procedure  translate_array( n      : int;
                             coords : TT_Points;
                             dx, dy : TT_Pos );
 var
   k : Int;
 begin
   if dx <> 0 then
     for k := 0 to n-1 do inc( coords^[k].x, dx );
   if dy <> 0 then
     for k := 0 to n-1 do inc( coords^[k].y, dy );
 end;

 function Count_Composite_Components(AStream: TFreeTypeStream;
                                     AStartOffset: Long;
                                     out ACount: Int): TError;
 var
   save_pos : Long;
   flags    : Int;
   skip     : Int;
 label
   Restore;
 begin
   Count_Composite_Components := Failure;
   ACount := 0;
   save_pos := AStream.Position;

   if AStream.SeekFile(AStartOffset) then
     exit;

   repeat
     if AStream.AccessFrame(4) then
       goto Restore;

     flags := AStream.Get_UShort;
     AStream.Get_UShort;
     AStream.ForgetFrame;

     if flags and ARGS_ARE_WORDS <> 0 then
       skip := 4
     else
       skip := 2;

     if flags and WE_HAVE_A_SCALE <> 0 then
       inc(skip, 2);

     if flags and WE_HAVE_AN_XY_SCALE <> 0 then
       inc(skip, 4);

     if flags and WE_HAVE_A_2X2 <> 0 then
       inc(skip, 8);

     if AStream.SkipFile(skip) then
       goto Restore;

     inc(ACount);
   until flags and MORE_COMPONENTS = 0;

   Count_Composite_Components := Success;

 Restore:
   if AStream.SeekFile(save_pos) then
     Count_Composite_Components := Failure;
 end;

 procedure Get_Composite_Variation_Delta(exec           : PExec_Context;
                                         subg           : PSubGlyph_Record;
                                         point_index    : Int;
                                         point_count    : Int;
                                         already_scaled : Boolean;
                                         var dx, dy     : TT_Pos);
 var
   points : TT_Points;
   total  : Int;
   k      : Int;
 begin
   dx := 0;
   dy := 0;

   if (exec^.face^.variations = nil) or
      (exec^.instance^.variations = nil) or
      (point_count <= 0) then
     exit;

   total := point_count + TTVar_PhantomPointCount;
   if (point_index < 0) or (point_index >= total) then
     exit;

   GetMem(points, total * SizeOf(TT_Vector));
   try
     for k := 0 to total - 1 do
     begin
       points^[k].x := 0;
       points^[k].y := 0;
     end;

     exec^.face^.variations.ApplyGlyphDeltas(exec^.instance^.variations,
       subg^.index, points, nil, total, 0, already_scaled,
       exec^.metrics.x_scale1, exec^.metrics.x_scale2,
       exec^.metrics.y_scale1, exec^.metrics.y_scale2);

     dx := points^[point_index].x;
     dy := points^[point_index].y;
   finally
     FreeMem(points, total * SizeOf(TT_Vector));
   end;
 end;

 (********************************************************)
 (* mount one zone on top of another one                 *)
 (*                                                      *)
 procedure  mount_zone( var source : TGlyph_Zone;
                        var target : TGlyph_Zone );
 var
   np, nc : Int;
 begin
   np := source.n_points;
   nc := source.n_contours;

   target.org   := pointer(@source.org^[np]);
   target.cur   := pointer(@source.cur^[np]);
   target.flags := pointer(@source.flags^[np]);

   target.conEnds := pointer(@source.conEnds^[nc]);

   target.n_points   := 0;
   target.n_contours := 0;
 end;

(*******************************************************************
 *
 *  Function    :  Load_Simple_Glyph
 *
 *
 ******************************************************************)


 function Load_Simple_Glyph( AStream       : TFreeTypeStream;
                             exec          : PExec_Context;
                             n_contours    : Int;
                             left_contours : Int;
                             left_points   : Int;
                             load_flags    : Int;
                             subg          : PSubGlyph_Record ) : TError;
 var
  n_points,
  n_ins, k : Int;
  h_origin, h_advance: Int;

   c, cnt : Byte;
   face   : PFace;
   coords : TT_Points;
   flag   : TT_PTouchTable;
   x, y   : TT_F26dot6;
   pts    : PGlyph_Zone;

 label
   Fail, Fail_File, Fail_Exec;
 begin
   Load_Simple_Glyph := Failure;

   face := exec^.face;

   (* simple check *)

   if ( n_contours > left_contours ) then
     begin
       {$IFDEF FREETYPE_DEBUG}
       Writeln( 'ERROR: Glyph index ',i,' has ',Gl.numberOfContours );
       Writeln( ' contours > left ', left_contours );
       {$ENDIF}
       error := TT_Err_Too_Many_Contours;
       goto Fail;
     end;

   (* preparing the execution context *)
   mount_zone( subg^.zone, exec^.pts );

   (* Reading the contours endpoints *)

   if AStream.AccessFrame( (n_contours+1)*2 ) then
     goto Fail_File;

   n_points := 0;

   for k := 0 to n_contours-1 do
     begin
       {$IFDEF FREETYPE_DEBUG} Write( n_points,' '); {$ENDIF}
       n_points              := AStream.GET_Short;
       exec^.pts.conEnds^[k] := n_points;
       inc( n_points );
     end;

   if n_points > left_points then
     begin
       {$IFDEF FREETYPE_DEBUG} Writeln( 'ERROR: Too many points' ); {$ENDIF}
       error := TT_Err_Too_Many_Points;
       goto Fail;
     end;

   (* Loading instructions *)

   n_ins := AStream.GET_Short;

   AStream.ForgetFrame;

{
   if not subg^.is_hinted then

     if TT_Skip_File( n_ins ) then
       goto Fail_File
     else
     (* skip the instructions *)

   else }
     begin

       {$IFDEF FREETYPE_DEBUG} Writeln('Instructions size : ', n_ins); {$ENDIF}

       if n_ins > face^.maxProfile.maxSizeOfInstructions then
       begin
         {$IFDEF FREETYPE_DEBUG} Writeln('Too many instructions'); {$ENDIF}
         error := TT_Err_Too_Many_Ins;
         goto Fail;
       end;

       with exec^ do
       begin
         if AStream.ReadFile( glyphIns^, n_ins ) then
           goto Fail_File;

         glyphSize := n_ins;

         if Set_CodeRange( exec,
                           TT_CodeRange_Glyph,
                           glyphIns,
                           glyphSize ) then
           goto Fail_Exec;
       end
     end;

   (* read the flags *)

   if AStream.CheckAndAccessFrame( n_points*5 )
     then goto Fail;

   k    := 0;
   flag := exec^.pts.flags;

   while ( k < n_points ) do
    begin
     c        := AStream.GET_Byte;
     flag^[k] := c;
     inc(k);

     if c and 8 <> 0 then
       begin
         cnt := AStream.GET_Byte;

         while ( cnt > 0 ) do
           begin
             flag^[k] := c;
             inc( k );
             dec( cnt );
           end
       end
    end;

   (* Read the X *)

   x      := 0;
   coords := exec^.pts.org;

   for k := 0 to n_points-1 do
   begin
     if flag^[k] and 2 <> 0 then

        if flag^[k] and 16 <> 0 then inc( x,  AStream.GET_Byte )
                                else inc( x, -AStream.GET_Byte )
     else
        if flag^[k] and 16 = 0  then inc( x, AStream.GET_Short );

     coords^[k].x := x;
   end;

   (* Read the Y *)

   y := 0;

   for k := 0 to n_points-1 do
   begin
     if flag^[k] and 4 <> 0 then

       if flag^[k] and 32 <> 0 then inc( y,  AStream.GET_Byte )
                               else inc( y, -AStream.GET_Byte )
     else
       if flag^[k] and 32 = 0  then inc( y, AStream.GET_Short );

     coords^[k].y := y;
   end;

   AStream.ForgetFrame;

  (* Now add the phantom points. Variation tables can address all four
     TrueType phantom points, so they must exist before gvar is applied. *)
  h_origin := n_points + TTVar_PhantomHOrigin;
  h_advance := n_points + TTVar_PhantomHAdvance;

  (* pp1 = xMin - lsb == glyph origin *)
  coords^[h_origin].x := subg^.bbox.XMin-subg^.leftBearing;
  coords^[h_origin].y := 0;

  (* pp2 = pp1 + aw == glyph next position *)
  coords^[h_advance].x := subg^.bbox.xMin-
                          subg^.leftBearing + subg^.advanceWidth;
  coords^[h_advance].y := 0;

  coords^[n_points + TTVar_PhantomVOrigin].x := 0;
  coords^[n_points + TTVar_PhantomVOrigin].y := 0;
  coords^[n_points + TTVar_PhantomVAdvance].x := 0;
  coords^[n_points + TTVar_PhantomVAdvance].y := 0;

  for k := 0 to n_points-1 do
    exec^.pts.flags^[k] := exec^.pts.flags^[k] and TT_Flag_On_Curve;

  for k := 0 to TTVar_PhantomPointCount - 1 do
    exec^.pts.flags^[n_points + k] := 0;

  inc( n_points, TTVar_PhantomPointCount );

  if (face^.variations <> nil) and (exec^.instance^.variations <> nil) then
    face^.variations.ApplyGlyphDeltas(exec^.instance^.variations, subg^.index,
      exec^.pts.org, exec^.pts.conEnds, n_points, n_contours, false,
      exec^.metrics.x_scale1, exec^.metrics.x_scale2,
      exec^.metrics.y_scale1, exec^.metrics.y_scale2);

   (* now eventually scale and hint the glyph *)

   pts := @exec^.pts;
   pts^.n_points := n_points;

   exec^.pts.n_contours := n_contours;

   if load_flags and TT_Load_Scale_Glyph = 0 then
     begin
       (* no scaling, just copy the org arrays into the cur ones *)
       org_to_cur( n_points, pts );
     end
   else
     begin

       (* first scale the glyph points *)
       for k := 0 to n_points-1 do with pts^ do
         org^[k].x := Scale_X( exec^.metrics, org^[k].x );

       for k := 0 to n_points-1 do with pts^ do
         org^[k].y := Scale_Y( exec^.metrics, org^[k].y );

      (* if hinting, round pp1, and shift the glyph accordingly *)
      if subg^.is_hinted then
      begin
        h_origin := n_points - TTVar_PhantomPointCount + TTVar_PhantomHOrigin;
        h_advance := n_points - TTVar_PhantomPointCount + TTVar_PhantomHAdvance;
        x := pts^.org^[h_origin].x;
        x := ((x+32) and -64) - x;
        translate_array( n_points, pts^.org, x, 0 );

         org_to_cur( n_points, pts );

         (* set the advance width *)
         (*
         x := (Scale_X( exec^.metrics, subg^.advanceWidth )+32) and -64;
         with pts^ do
           cur_x^[n_points-1] := cur_x^[n_points-2]+x;
         *)
         with pts^ do
          cur^[h_advance].x := (cur^[h_advance].x+32) and -64;

         (* now consider hinting *)
         if (exec^.glyphSize > 0) then
         begin
             exec^.is_composite := False;

             if Context_Run( exec, load_flags and TT_Load_Debug <> 0 ) then
               goto Fail_Exec;
         end;
       end
       else
         org_to_cur( n_points, pts );
     end;

   (* save glyph origin and advance points *)
  if not subg^.preserve_pps then
  begin
    h_origin := n_points - TTVar_PhantomPointCount + TTVar_PhantomHOrigin;
    h_advance := n_points - TTVar_PhantomPointCount + TTVar_PhantomHAdvance;
    subg^.pp1 := pts^.cur^[h_origin];
    subg^.pp2 := pts^.cur^[h_advance];
  end;

   Load_Simple_Glyph := Success;

 Fail:
   exit;

 Fail_File:
   error := TT_Err_File_Error;
   exit;

 Fail_Exec:
   error := exec^.error;
   exit;
 end;

(*******************************************************************
 *
 *  Function    :  Load_Composite_End
 *
 *
 ******************************************************************)

 function  Load_Composite_End( AStream: TFreeTypeStream;
                               n_points   : Int;
                               {%H-}n_contours : Int;
                               exec       : PExec_Context;
                               subg       : PSubglyph_Record;
                               load_flags : Int;
                               debug      : Boolean ) : TError;
 var
   pts     : PGlyph_Zone;
   n_ins   : Int;
   k       : Int;
   x, y    : TT_Pos;
   h_origin_delta, h_advance_delta : TT_Pos;

 label
   Fail, Fail_File, Fail_Exec;
 begin
   Load_Composite_End := Failure;

   if subg^.is_hinted and
      (subg^.element_flag and WE_HAVE_INSTR <> 0) then
   begin
     if AStream.AccessFrame(2) then goto Fail_File;
     n_ins := AStream.Get_UShort;
     AStream.ForgetFrame;

     (* load the instructions *)
     {$IFDEF FREETYPE_DEBUG} Writeln('Instructions size : ', n_ins); {$ENDIF}

     if n_ins > exec^.face^.maxProfile.maxSizeOfInstructions then
     begin
       {$IFDEF FREETYPE_DEBUG} Writeln('Too many instructions'); {$ENDIF}
       error := TT_Err_Too_Many_Ins;
       goto Fail;
     end;
   end
   else
     n_ins := 0;

   if n_ins > 0 then with exec^ do
   begin
     if AStream.ReadFile( glyphIns^, n_ins ) then
       goto Fail_File;

     glyphSize := n_ins;

     if Set_CodeRange( exec,
                       TT_CodeRange_Glyph,
                       glyphIns,
                       glyphSize ) then goto Fail_File;
   end;

  if subg^.componentCount > 0 then
  begin
    Get_Composite_Variation_Delta(exec, subg,
      subg^.componentCount + TTVar_PhantomHOrigin, subg^.componentCount,
      load_flags and TT_Load_Scale_Glyph <> 0, h_origin_delta, y);
    inc(subg^.pp1.x, h_origin_delta);
    inc(subg^.pp1.y, y);

    Get_Composite_Variation_Delta(exec, subg,
      subg^.componentCount + TTVar_PhantomHAdvance, subg^.componentCount,
      load_flags and TT_Load_Scale_Glyph <> 0, h_advance_delta, y);
    inc(subg^.pp2.x, h_advance_delta);
    inc(subg^.pp2.y, y);
  end;

  (* prepare the execution context *)
  inc( n_points, TTVar_PhantomPointCount );

  exec^.pts     := subg^.zone;
  pts           := @exec^.pts;
  pts^.n_points := n_points;

  (* add phantom points *)
  with pts^ do
  begin
    cur^[n_points - TTVar_PhantomPointCount + TTVar_PhantomHOrigin] := subg^.pp1;
    cur^[n_points - TTVar_PhantomPointCount + TTVar_PhantomHAdvance] := subg^.pp2;
    cur^[n_points - TTVar_PhantomPointCount + TTVar_PhantomVOrigin].x := 0;
    cur^[n_points - TTVar_PhantomPointCount + TTVar_PhantomVOrigin].y := 0;
    cur^[n_points - TTVar_PhantomPointCount + TTVar_PhantomVAdvance].x := 0;
    cur^[n_points - TTVar_PhantomPointCount + TTVar_PhantomVAdvance].y := 0;
    for k := 0 to TTVar_PhantomPointCount - 1 do
      flags^[n_points - TTVar_PhantomPointCount + k] := 0;
  end;

  (* if hinting, round the phantom points *)
  if subg^.is_hinted then
  begin
    x := ((subg^.pp1.x+32) and -64);
    pts^.cur^[n_points - TTVar_PhantomPointCount + TTVar_PhantomHOrigin].x := x;

    x := ((subg^.pp2.x+32) and -64);
    pts^.cur^[n_points - TTVar_PhantomPointCount + TTVar_PhantomHAdvance].x := x;
  end;

   for k := 0 to n_points-1 do
     pts^.flags^[k] := pts^.flags^[k] and TT_Flag_On_Curve;

   cur_to_org( n_points, pts );

   (* now consider hinting *)
   if subg^.is_hinted and (n_ins > 0) then
   begin
     exec^.is_composite := true;

     if Context_Run( exec, debug ) then
       goto Fail_Exec;
   end;

  (* save glyph origin and advance points *)
  subg^.pp1 := pts^.cur^[n_points - TTVar_PhantomPointCount + TTVar_PhantomHOrigin];
  subg^.pp2 := pts^.cur^[n_points - TTVar_PhantomPointCount + TTVar_PhantomHAdvance];

   Load_Composite_End := Success;
   error := TT_Err_Ok;

 Fail:
   exit;

 Fail_File:
   error := TT_Err_File_Error;
   goto Fail;

 Fail_Exec:
   error := exec^.error;
   goto Fail;

 end;


(*******************************************************************
 *
 *  Function    :  Init_Glyph_Component
 *
 *
 ******************************************************************)


 procedure Init_Glyph_Component( element   : PSubGlyph_Record;
                                 original  : PSubGlyph_Record;
                                 exec      : PExec_Context );
 begin
   with element^ do
   begin
     index     := -1;
     is_scaled := false;
     is_hinted := false;

     if original <> nil then
       mount_zone( original^.zone, zone )
     else
       zone := exec^.pts;

     zone.n_contours := 0;
     zone.n_points   := 0;

     arg1 := 0;
     arg2 := 0;

     element_flag := 0;
     preserve_pps := false;

     transform.xx := 1 shl 16;
     transform.xy := 0;
     transform.yx := 0;
     transform.yy := 1 shl 16;

     transform.ox := 0;
     transform.oy := 0;
     componentCount := 0;
     componentIndex := 0;
     varDeltaX := 0;
     varDeltaY := 0;

     leftBearing  := 0;
     advanceWidth := 0;
   end;
 end;


 function  Load_TrueType_Glyph( instance    : PInstance;
                                glyph       : PGlyph;
                                glyph_index : Word;
                                load_flags  : Int     ) : TError;
 type
   TPhases = ( Load_Exit,
               Load_Glyph,
               Load_Simple,
               Load_Composite,
               Load_End );
   (* the composite loader is a simple automata wich states *)
   (* are defined by the TPhases enumeration                *)

 var
   face : PFace;

   num_points    : Int;
   num_contours  : Int;
   left_points   : Int;
   left_contours : Int;

   table,
   index,
   load_top : Int;

   new_flags, k, l : Int;

   glyph_offset, offset : Long;

   vec, nvec : TT_Vector;

   xx, xy, yx, yy : TT_Fixed;

   exec   : PExec_Context;
   ftstream : TFreeTypeStream;

   subglyph, subglyph2 : PSubGlyph_Record;

   base_pts : TGlyph_Zone;

   phase : TPhases;

   debug : Boolean;

   top_bearing     : TT_Pos;
   advance_height  : TT_Pos;

   //error      : TT_Error;
   delta      : Long;
   widths     : PByte;
   horizontal : TT_Horizontal_Header;
 label
   Fin,
   Fail,
   Fail_File,
   Fail_Handle,
   Fail_Index;

 begin

   Load_TrueType_Glyph := Failure;

   (* check handle *)

   if (instance = nil) or (instance^.owner = nil) then
     begin
       error := TT_Err_Invalid_Face_Handle;
       exit;
     end;

   face := instance^.owner;

   table := LookUp_TrueType_Table( face, 'glyf');
   if table < 0 then
     begin
       {$IFDEF FREETYPE_DEBUG}
       Trace1( 'TTApi.load_glyph : couldn''t find glyf table' );
       {$ENDIF}
       error := TT_Err_Table_Missing;
       exit;
     end;

   glyph_offset := face^.dirTables^[table].Offset;

   (* query new execution context *)

   if instance^.debug then
     exec := instance^.context (* if debugging, use the pre-alloced context *)
   else
     exec := New_Context(instance);

   if exec = nil then
     begin
       error := TT_Err_Out_Of_Memory;
       exit;
     end;

   Context_Load( exec, instance );

   glyph^.outline.high_precision := ( instance^.metrics.y_ppem < 24 );

   glyph^.is_composite := false;

   (* save its critical pointers that will be modified *)
   (* during load                                      *)

   base_pts := exec^.pts;

   (* init variables *)

   left_points   := face^.maxPoints;
   left_contours := face^.maxContours;

   num_points   := 0;
   num_contours := 0;

   load_top := 0;
   subglyph := @exec^.loadStack^[0];

   Init_Glyph_Component( subglyph, nil, exec );

   subglyph^.index     := glyph_index;
   subglyph^.is_hinted := load_flags and TT_Load_Hint_Glyph <> 0;

   if instance^.GS.instruct_control and 1 <> 0 then
     subglyph^.is_hinted := False;

   (* now access stream *)

   if TT_Use_Stream( face^.stream, ftstream {%H-}) then
     goto Fin;

   (* Main Loading Loop *)

   phase := Load_Glyph;

   while phase <> Load_Exit do
   begin

     subglyph := @exec^.loadStack^[load_top];

     case phase of

       (************************************************************)
       (*                                                          *)
       (* Load_Glyph state                                         *)
       (*                                                          *)
       (*   reading a glyph's generic header to determine          *)
       (*   wether it's simple or composite                        *)
       (*                                                          *)
       (* exit states : Load_Simple and Load_Composite             *)
       (*                                                          *)

       Load_Glyph:
         begin
           (* check glyph index and table *)

           index := subglyph^.index;
           if (index < 0) or (index >= face^.numGlyphs) then
           begin
             error := TT_Err_Invalid_Glyph_Index;
             goto Fail;
           end;

           (* load glyph metrics *)
           Get_HMetrics( face, index, true,
                         subglyph^.leftBearing,
                         subglyph^.advanceWidth );

           (* load glyph *)
           if (index+1 < face^.numLocations) and
              (face^.glyphLocations^[index] = face^.glyphLocations^[index+1]) then
             begin

               (* as noticed by Frederic Loyer, these are spaces, not *)
               (* the 'unknown' glyph                                 *)
               num_points   := 0;
               num_contours := 0;

               subglyph^.bbox.xMin := 0;
               subglyph^.bbox.xMax := 0;
               subglyph^.bbox.yMin := 0;
               subglyph^.bbox.yMax := 0;

               subglyph^.pp1.x := 0;
               subglyph^.pp2.x := subglyph^.advanceWidth;

               exec^.pts.n_points := TTVar_PhantomPointCount;
               exec^.pts.n_contours := 0;
               exec^.pts.org^[TTVar_PhantomHOrigin] := subglyph^.pp1;
               exec^.pts.org^[TTVar_PhantomHAdvance] := subglyph^.pp2;
               exec^.pts.org^[TTVar_PhantomVOrigin].x := 0;
               exec^.pts.org^[TTVar_PhantomVOrigin].y := 0;
               exec^.pts.org^[TTVar_PhantomVAdvance].x := 0;
               exec^.pts.org^[TTVar_PhantomVAdvance].y := 0;
               for k := 0 to TTVar_PhantomPointCount - 1 do
                 exec^.pts.flags^[k] := 0;

               if (face^.variations <> nil) and
                  (exec^.instance^.variations <> nil) then
                 face^.variations.ApplyGlyphDeltas(exec^.instance^.variations,
                   subglyph^.index, exec^.pts.org, exec^.pts.conEnds,
                   TTVar_PhantomPointCount, 0, false,
                   exec^.metrics.x_scale1, exec^.metrics.x_scale2,
                   exec^.metrics.y_scale1, exec^.metrics.y_scale2);

               if load_flags and TT_LOAD_Scale_Glyph <> 0 then
               begin
                 for k := 0 to TTVar_PhantomPointCount - 1 do
                 begin
                   exec^.pts.org^[k].x := Scale_X( exec^.metrics,
                     exec^.pts.org^[k].x );
                   exec^.pts.org^[k].y := Scale_Y( exec^.metrics,
                     exec^.pts.org^[k].y );
                 end;
               end;
               org_to_cur( TTVar_PhantomPointCount, @exec^.pts );
               subglyph^.pp1 := exec^.pts.cur^[TTVar_PhantomHOrigin];
               subglyph^.pp2 := exec^.pts.cur^[TTVar_PhantomHAdvance];

               exec^.glyphSize := 0;
               phase := Load_End;
             end
           else
             begin
               offset := glyph_offset + face^.glyphLocations^[index];

               (* read first glyph header *)

               if ftstream.SeekFile( offset ) or
                  ftstream.AccessFrame( 5*sizeof(Short) ) then
                 goto Fail_File;

               num_contours        := ftstream.GET_Short;
               subglyph^.bbox.xMin := ftstream.GET_Short;
               subglyph^.bbox.yMin := ftstream.GET_Short;
               subglyph^.bbox.xMax := ftstream.GET_Short;
               subglyph^.bbox.yMax := ftstream.GET_Short;

               ftstream.ForgetFrame;

               {$IFDEF FREETYPE_DEBUG}
               Writeln('Glyph ', i );

               Writeln(' # of Contours : ',num_contours );
               Writeln(' xMin : ',subglyph^.xMin:4,'  xMax : ',subglyph^.xMax);
               Writeln(' yMin : ',subglyph^.yMin:4,'  yMax : ',subglyph^.yMax);
               Writeln('-');
               {$ENDIF}

               if num_contours > left_contours then
               begin
                 {$IFDEF FREETYPE_DEBUG}
                 Writeln( 'ERROR: Glyph index ', i, ' has ', num_contours );
                 Writeln(' contours > left ', left_contours );
                 {$ENDIF}
                 error := TT_Err_Too_Many_Contours;
                 goto Fail;
               end;

               with subglyph^ do
               begin
                 pp1.x := bbox.xMin - leftBearing;
                 pp1.y := 0;
                 pp2.x := pp1.x + advanceWidth;
                 pp2.y := 0;

                 if load_flags and TT_Load_Scale_Glyph <> 0 then
                 begin
                   pp1.x := Scale_X( exec^.metrics, pp1.x );
                   pp2.x := Scale_X( exec^.metrics, pp2.x );
                 end;
               end;

               (* is it a simple glyph ? *)
               if num_contours >= 0 then
                 phase := Load_Simple
               else
                 begin
                   if Count_Composite_Components(ftstream, ftstream.Position,
                     subglyph^.componentCount) then
                     goto Fail_File;
                   subglyph^.componentIndex := 0;
                   phase := Load_Composite;
                 end;

             end
         end;

       (************************************************************)
       (*                                                          *)
       (* Load_Simple state                                        *)
       (*                                                          *)
       (*   reading a simple glyph (num_contours must be set to    *)
       (*   the glyph's number of contours..)                      *)
       (*                                                          *)
       (* exit states : Load_End                                   *)
       (*                                                          *)

       Load_Simple :
         begin
           new_flags := load_flags;

           if not subglyph^.is_hinted then
             new_flags := new_flags and not TT_Load_Hint_Glyph;
           (* disable hinting when scaling *)

           if new_flags and TT_Load_Debug <> 0 then
             if load_top > 0 then
               new_flags := new_flags and not TT_Load_Debug;

           if instance^.GS.instruct_control and 2 <> 0 then
             exec^.GS := Default_GraphicsState
           else
             exec^.GS := instance^.GS;

           if Load_Simple_Glyph(
                       ftstream,
                       exec,
                       num_contours,
                       left_contours,
                       left_points,
                       new_flags,
                       subglyph ) then
             goto Fail;

          num_points := exec^.pts.n_points-TTVar_PhantomPointCount;

           phase := Load_End;
         end;

       (************************************************************)
       (*                                                          *)
       (* Load_Composite state                                     *)
       (*                                                          *)
       (*   reading a composite glyph header a pushing a new       *)
       (*   load element on the stack..                            *)
       (*                                                          *)
       (* exit states : Load_Glyph                                 *)
       (*                                                          *)

       Load_Composite :
         begin

           glyph^.is_composite := true;

           (* create a new element *)

           inc( load_top );

           if load_top > face^.maxComponents then
           begin
             error := TT_Err_Invalid_Composite;
             goto Fail;
           end;

           subglyph2 := @exec^.loadStack^[load_top];

           Init_Glyph_Component( subglyph2, subglyph, nil );

           subglyph2^.index     := -1;
           subglyph2^.is_hinted := subglyph^.is_hinted;

           (* now read composite header *)

           if ftstream.AccessFrame( 4 ) then
             goto Fail_File;

           new_flags := ftstream.Get_UShort;

           subglyph^.element_flag := new_flags;
           subglyph2^.index       := ftstream.Get_UShort;

           ftstream.ForgetFrame;

           k := 2;

           if new_flags and ARGS_ARE_WORDS <> 0 then
             inc( k, 2 );

           if new_flags and WE_HAVE_A_SCALE <> 0 then
             inc( k, 2 );

           if new_flags and WE_HAVE_AN_XY_SCALE <> 0 then
             inc( k, 4 );

           if new_flags and WE_HAVE_A_2X2 <> 0 then
             inc( k, 8 );

           if ftstream.AccessFrame( k ) then
             goto Fail_File;

           if new_flags and ARGS_ARE_WORDS <> 0 then
             begin
               k := SmallInt(ftstream.Get_Short);
               l := SmallInt(ftstream.Get_Short);
             end
           else
             begin
               k := ShortInt(ftstream.Get_Byte);
               l := ShortInt(ftstream.Get_Byte);
             end;

           subglyph^.arg1 := k;
           subglyph^.arg2 := l;

           if new_flags and ARGS_ARE_XY_VALUES <> 0 then
             begin
               subglyph^.transform.ox := k;
               subglyph^.transform.oy := l;
             end;

           xx := 1 shl 16;
           xy := 0;
           yx := 0;
           yy := 1 shl 16;

           if new_flags and WE_HAVE_A_SCALE <> 0 then
             begin
               xx := Long(ftstream.Get_Short) shl 2;
               yy := xx;

               subglyph2^.is_scaled := true;
             end
           else if new_flags and WE_HAVE_AN_XY_SCALE <> 0 then
             begin
               xx := Long(ftstream.Get_Short) shl 2;
               yy := Long(ftstream.Get_Short) shl 2;

               subglyph2^.is_scaled := true;
             end
           else if new_flags and WE_HAVE_A_2X2 <> 0 then
             begin
               xx := Long(ftstream.Get_Short) shl 2;
               xy := Long(ftstream.Get_Short) shl 2;
               yx := Long(ftstream.Get_Short) shl 2;
               yy := Long(ftstream.Get_Short) shl 2;

               subglyph2^.is_scaled := true;
             end;

           subglyph^.transform.xx := xx;
           subglyph^.transform.xy := xy;
           subglyph^.transform.yx := yx;
           subglyph^.transform.yy := yy;
           subglyph^.varDeltaX := 0;
           subglyph^.varDeltaY := 0;

           if subglyph^.componentCount > 0 then
           begin
             Get_Composite_Variation_Delta(exec, subglyph,
               subglyph^.componentIndex, subglyph^.componentCount,
               false, subglyph^.varDeltaX, subglyph^.varDeltaY);
             inc(subglyph^.componentIndex);
           end;

           delta := MulDiv_Round( xx, yy, 1 shl 16 ) -
                    MulDiv_Round( xy, yx, 1 shl 16 );

           if abs(delta) <> 1 shl 16 then
             subglyph2^.is_hinted := false;

           ftstream.ForgetFrame;

           subglyph^.file_offset := ftstream.Position;

           phase := Load_Glyph;
         end;

       (************************************************************)
       (*                                                          *)
       (* Load_End state                                           *)
       (*                                                          *)
       (*   after loading a glyph, apply transform and offset      *)
       (*   where necessary, pops element and continue or          *)
       (*   stop process..                                         *)
       (*                                                          *)
       (* exit states : Load_Composite and Load_Exit               *)
       (*                                                          *)

       Load_End :
         if load_top > 0 then
           begin

             subglyph2 := subglyph;

             dec( load_top );
             subglyph := @exec^.loadStack^[load_top];

             (* check advance width and left side bearing *)

             if not subglyph^.preserve_pps and
                (subglyph^.element_flag and USE_MY_METRICS <> 0) then
             begin

               subglyph^.leftBearing  := subglyph2^.leftBearing;
               subglyph^.advanceWidth := subglyph2^.advanceWidth;

               subglyph^.pp1 := subglyph2^.pp1;
               subglyph^.pp2 := subglyph2^.pp2;

               subglyph^.preserve_pps := true;
             end;

             (* apply scale/symmetry/rotation/wathever *)

             for k := 0 to num_points-1 do with subglyph^ do
             begin
               vec := subglyph2^.zone.cur^[k];

               nvec.x := MulDiv_Round( vec.x, transform.xx, 1 shl 16 ) +
                         MulDiv_Round( vec.y, transform.yx, 1 shl 16 );

               nvec.y := MulDiv_Round( vec.x, transform.xy, 1 shl 16 ) +
                         MulDiv_Round( vec.y, transform.yy, 1 shl 16 );

               subglyph2^.zone.cur^[k] := nvec;

               vec := subglyph2^.zone.org^[k];

               nvec.x := MulDiv_Round( vec.x, transform.xx, 1 shl 16 ) +
                         MulDiv_Round( vec.y, transform.yx, 1 shl 16 );

               nvec.y := MulDiv_Round( vec.x, transform.xy, 1 shl 16 ) +
                         MulDiv_Round( vec.y, transform.yy, 1 shl 16 );

               subglyph2^.zone.org^[k] := nvec;
             end;

             (* adjust counts *)
             for k := 0 to num_contours-1 do
               inc( subglyph2^.zone.conEnds^[k], subglyph^.zone.n_points );

             inc( subglyph^.zone.n_points,   num_points );
             inc( subglyph^.zone.n_contours, num_contours );

             dec( left_points, num_points );
             dec( left_contours, num_contours );

             (* apply offset *)

             if subglyph^.element_flag and ARGS_ARE_XY_VALUES = 0 then
               begin
                 k := subglyph^.arg1;
                 l := subglyph^.arg2;

                 if (k < 0) or (k >= subglyph^.zone.n_points ) or
                    (l < 0) or (l >= num_points) then
                   begin
                     error := TT_Err_Invalid_Composite;
                     goto Fail;
                   end;

                 inc( l, subglyph^.zone.n_points );

                 vec.x := subglyph^.zone.cur^[k].x -
                          subglyph^.zone.cur^[l].x;

                 vec.y := subglyph^.zone.cur^[k].y -
                          subglyph^.zone.cur^[l].y;

                 if load_flags and TT_Load_Scale_Glyph <> 0 then
                 begin
                   inc(vec.x, Scale_X( exec^.metrics, subglyph^.varDeltaX ));
                   inc(vec.y, Scale_Y( exec^.metrics, subglyph^.varDeltaY ));
                 end
                 else
                 begin
                   inc(vec.x, subglyph^.varDeltaX);
                   inc(vec.y, subglyph^.varDeltaY);
                 end;
               end
             else
               begin
                 vec.x := subglyph^.transform.ox + subglyph^.varDeltaX;
                 vec.y := subglyph^.transform.oy + subglyph^.varDeltaY;

                 if load_flags and TT_Load_Scale_Glyph <> 0 then
                 begin
                   vec.x := Scale_X( exec^.metrics, vec.x );
                   vec.y := Scale_Y( exec^.metrics, vec.y );

                   if subglyph^.element_flag and ROUND_XY_TO_GRID <> 0 then
                   begin
                     vec.x := (vec.x+32) and -64;
                     vec.y := (vec.y+32) and -64;
                   end;
                 end
               end;

             translate_array( num_points, subglyph2^.zone.cur, vec.x, vec.y );

             cur_to_org( num_points, @subglyph2^.zone );

             num_points   := subglyph^.zone.n_points;
             num_contours := subglyph^.zone.n_contours;

             (* check for last component *)

             if ftstream.SeekFile( subglyph^.file_offset ) then
               goto Fail_File;

             if subglyph^.element_flag and MORE_COMPONENTS <> 0 then
               phase := Load_Composite
             else
               begin
                 debug := ( load_top = 0 ) and
                          ( load_flags and TT_Load_Debug <> 0 );

                 if Load_Composite_End( ftstream,
                                        num_points,
                                        num_contours,
                                        exec,
                                        subglyph,
                                        load_flags,
                                        debug ) then goto Fail;
                 phase := Load_End;
               end;

           end
         else
           phase := Load_Exit;

     end;
   end;

   (* finally, copy the points arrays to the glyph object *)

   exec^.pts := base_pts;

   (* copy also the phantom points, the debugger needs them *)
   inc( num_points, TTVar_PhantomPointCount );

   for k := 0 to num_points-1 do with glyph^.outline do
   begin
     points^[k] := exec^.pts.cur^[k];
     flags ^[k] := exec^.pts.flags^[k];
   end;

   for k := 0 to num_contours-1 do with glyph^.outline do
     conEnds^[k] := exec^.pts.conEnds^[k];

   glyph^.outline.n_points    := num_points;
   glyph^.outline.n_contours  := num_contours;
   glyph^.outline.second_pass := true;

   TT_Get_Outline_BBox( glyph^.outline, glyph^.metrics.bbox, TTVar_PhantomPointCount );

   glyph^.metrics.horiBearingX := glyph^.metrics.bbox.xMin - subglyph^.pp1.x;
   glyph^.metrics.horiBearingY := glyph^.metrics.bbox.yMax;
   glyph^.metrics.horiAdvance  := subglyph^.pp2.x - subglyph^.pp1.x;

   glyph^.computed_width := glyph^.metrics.horiAdvance;
   glyph^.precalc_width  := -1;

   (* Now take care of vertical metrics. In the case where there is    *)
   (* no vertical information within the font (which is relatively     *)
   (* common), make up some metrics "by hand"..                        *)
   (*                                                                  *)

   begin
     (* get the unscaled "tsb" and "ah"                            *)
     (* don't assume that both the vertical header and metrics are *)
     (* present in a font file...                                  *)
     if face^.verticalInfo and
        ( face^.verticalHeader.number_Of_VMetrics > 0 ) then
       begin
       (* apparently, the following line isn't accepted by the FreePascal *)
       (* compiler. It complains because the typecast occurs on a 'var'   *)
       (* parameter. Don't know if this is compiler bug or not, but I     *)
       (* changed the code with some stupid copy trick..                  *)
       (*                                                                 *)
       (* TT_Get_Metrics( TT_Horizontal_Header(face^.verticalHeader),     *)
       (*                 glyph_index,                                    *)
       (*                 top_bearing,                                    *)
       (*                 advance_height );                               *)
       (*                                                                 *)
          horizontal := TT_Horizontal_Header(face^.verticalHeader);
          top_bearing:=0;
          advance_height:=0;
          TT_Get_Metrics( horizontal,
                          glyph_index,
                          top_bearing,
                          advance_height );
       end
     else
       begin
         (* Make up the distances from the horizontal header..       *)
         (*                                                          *)
         (* The typographic values are the only portable ones, which *)
         (* is why we use them..                                     *)
         (*                                                          *)
         (* The sTypoDescender field is always negative, unlike the  *)
         (* Windows Descender..                                      *)
         (*                                                          *)
         with face^.os2 do
         begin
           top_bearing    := sTypoLineGap div 2;
           advance_height := sTypoAscender - sTypoDescender + sTypoLineGap;
         end;

       end;

       (* now scale the metrics *)
       if load_flags and TT_Load_Scale_Glyph <> 0 then
       begin
         top_bearing    := Scale_Y( exec^.metrics, top_bearing );
         advance_height := Scale_Y( exec^.metrics, advance_height );
       end;

       with glyph^.metrics do
       begin
         vertBearingX := ( bbox.xMin - bbox.xMax ) div 2;
         vertBearingY := top_bearing;
         vertAdvance  := advance_height;

         if load_flags and TT_Load_Hint_Glyph <> 0 then
         begin
           vertBearingX := vertBearingX and -64;
           vertBearingY := (vertBearingY + 63) and -64;
           vertAdvance  := (vertAdvance+32) and -64;
         end;
       end;

   end;

   (* use hdmx table to adjust advance width as necessary *)
   if load_flags and TT_Load_Default = TT_Load_Default then
   begin
     widths := Get_Advance_Widths( exec^.face,
                                   exec^.instance^.metrics.x_ppem );
     if widths <> nil then
     begin
       glyph^.metrics.horiAdvance := widths^[glyph_index]*64;
       glyph^.precalc_width       := glyph^.metrics.horiAdvance;
     end;
   end;

   (* in case of hinting, shift the glyph so that (0,0) corresponds *)
   (* to the glyph origin.                                          *)
   if subglyph^.is_hinted then
   begin
     glyph^.metrics.horiBearingX := (glyph^.metrics.bbox.xMin and -64) -
                                     subglyph^.pp1.x;

     glyph^.metrics.horiAdvance  := (glyph^.metrics.horiAdvance+32) and -64;
     glyph^.computed_width       := (glyph^.computed_width+32) and -64;

     translate_array( num_points,
                      glyph^.outline.points,
                      -subglyph^.pp1.x,
                      0 );
   end;

   glyph^.outline.dropout_mode := exec^.GS.scan_type;

   Load_TrueType_Glyph := Success;

 Fail:
   TT_Done_Stream( face^.stream );

 Fin:

   (* reset the execution context *)
   exec^.pts := base_pts;

   if instance^.debug then
     begin
       exec^.pts.n_points   := num_points;
       exec^.pts.n_contours := num_contours;
     end
   else
     Done_Context( exec);

   exit;

 Fail_File:
   error := TT_Err_File_Error;
   goto Fail;

 Fail_Handle:
   error := TT_Err_Invalid_Instance_Handle;
   exit;

 Fail_Index:
   error := TT_Err_Invalid_Glyph_Index;
   exit;

 end;


end.
