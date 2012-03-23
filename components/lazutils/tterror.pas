(*******************************************************************
 *
 *  tterror.pas                                                  1.0
 *
 *    Simple Error management unit
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

unit TTError;

interface

uses TTTypes;

  procedure Check_Error( error : Integer );

  procedure Panic1( message : String );
  procedure Trace1( message : String );

  const

    Err_Ras_None             =  0;
    Err_Ras_NotIni           = -2;  (* Rasterizer not Initialized    *)
    Err_Ras_Overflow         = -3;  (* Profile Table Overflow        *)
    Err_Ras_Neg_H            = -4;  (* Negative Height encountered ! *)
    Err_Ras_Invalid          = -5;  (* Invalid value encountered !   *)
    Err_Ras_Invalid_Contours = -6;


(* The Pascal version of the library doesn't support multiple       *)
(* threads. We use a global error variable, called simply "error"   *)
(* to report all defects. The various functions return an error     *)
(* condition, which can be either Success (false) or Failure (true) *)

(* Note that the use of macros in the C version to automate error   *)
(* reporting makes the two source trees very similar, even if they  *)
(* differ from some design points like this one                     *)

var
  error : integer;

implementation


  procedure Panic1( message : String );
  begin
    writeln( message );
    halt(1);
  end;


  procedure Trace1( message : String );
  begin
    writeln( message );
  end;


  procedure Check_Error( error : Integer );
  var
    num : String[4];
  begin
    if error <> TT_Err_Ok then
    begin
      str( -error:3, num );
      Panic1( 'Error code = ' + num );
    end;
  end;

end.

