{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the parser, based on m68k - by DoDi.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

(* In other compiler files the M68K $define is used to pick M68K specific values.
  That's why we have to retain some strings, until the Dummy compiler is
  handled in fpcdefs.inc and other places.
*)

Unit CPUInfo;

Interface

  uses
    globtype;

Type
   bestreal = double;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = type extended;
   ts64comp = extended;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tcputype = (
      {$IFDEF dummy}
//hard coded in Globals for M68K
      cpu_none,
      cpu_MC68020
      {$ELSE}
      cpu_none,
       cpu_MC68000,
       cpu_MC68020,
       cpu_Coldfire
      {$ENDIF}
      );

   tfputype = (
     {$IFDEF dummy}
      //hard coded in Globals for M68K
      fpu_none,
      fpu_soft,
      //hard coded in Nld
      fpu_libgcc
     {$ELSE}
      fpu_none,
      fpu_soft,
      fpu_libgcc,
      fpu_68881
     {$ENDIF}
     );

Const
   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_stdcall,
     { the difference to stdcall is only the name mangling }
     pocall_cdecl,
     { the difference to stdcall is only the name mangling }
     pocall_cppdecl,
     { this used by the PalmOS port only }
     pocall_syscall
   ];

   cputypestr : array[tcputype] of string[8] = (
   {$IFDEF dummy}
    '',
    'anyCPU'
   {$ELSE}
     '',
     '68000',
     '68020',
     'COLDFIRE'
   {$ENDIF}
   );

   fputypestr : array[tfputype] of string[6] = (
   {$IFDEF dummy}
      '',
      'anyFPU',
      'LIBGCC'
   {$ELSE}
      '',
     'SOFT',
     'LIBGCC',
     '68881'
   {$ENDIF}
   );

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches
                                 +genericlevel2optimizerswitches
                                 +genericlevel3optimizerswitches
                                 { no need to write info about those }
                                 -[cs_opt_level1,cs_opt_level2,cs_opt_level3]
                                 +[cs_opt_regvar,cs_opt_loopunroll];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches + [cs_opt_regvar,cs_opt_stackframe];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [{,cs_opt_loopunroll}];

Implementation

end.
