{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Contains the base types for the parser, based on M68K - by DoDi.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

(* This is a port from M68K, to make something work.
  Parts have been excluded, based on "dummy", should be removed later.
  Eventually more can be removed...
*)

unit cpubase;

{$i fpcdefs.inc}

interface

uses
  globtype,
  strings,cutils,cclasses,aasmbase,cpuinfo,cgbase;

{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

type
   tasmop = (a_none
   {$IFDEF dummy}
   {$ELSE}
      ,
     a_abcd,a_add,a_adda,a_addi,a_addq,a_addx,a_and,a_andi,
     a_asl,a_asr,a_bcc,a_bcs,a_beq,a_bge,a_bgt,a_bhi,
     a_ble,a_bls,a_blt,a_bmi,a_bne,a_bpl,a_bvc,a_bvs,
     a_bchg,a_bclr,a_bra,a_bset,a_bsr,a_btst,a_chk,
     a_clr,a_cmp,a_cmpa,a_cmpi,a_cmpm,a_dbcc,a_dbcs,a_dbeq,a_dbge,
     a_dbgt,a_dbhi,a_dble,a_dbls,a_dblt,a_dbmi,a_dbne,a_dbra,
     a_dbpl,a_dbt,a_dbvc,a_dbvs,a_dbf,a_divs,a_divu,
     a_eor,a_eori,a_exg,a_illegal,a_ext,a_jmp,a_jsr,
     a_lea,a_link,a_lsl,a_lsr,a_move,a_movea,a_movei,a_moveq,
     a_movem,a_movep,a_muls,a_mulu,a_nbcd,a_neg,a_negx,
     a_nop,a_not,a_or,a_ori,a_pea,a_rol,a_ror,a_roxl,
     a_roxr,a_rtr,a_rts,a_sbcd,a_scc,a_scs,a_seq,a_sge,
     a_sgt,a_shi,a_sle,a_sls,a_slt,a_smi,a_sne,
     a_spl,a_st,a_svc,a_svs,a_sf,a_sub,a_suba,a_subi,a_subq,
     a_subx,a_swap,a_tas,a_trap,a_trapv,a_tst,a_unlk,
     a_rte,a_reset,a_stop,
     { mc68010 instructions }
     a_bkpt,a_movec,a_moves,a_rtd,
     { mc68020 instructions }
     a_bfchg,a_bfclr,a_bfexts,a_bfextu,a_bfffo,
     a_bfins,a_bfset,a_bftst,a_callm,a_cas,a_cas2,
     a_chk2,a_cmp2,a_divsl,a_divul,a_extb,a_pack,a_rtm,
     a_trapcc,a_tracs,a_trapeq,a_trapf,a_trapge,a_trapgt,
     a_traphi,a_traple,a_trapls,a_traplt,a_trapmi,a_trapne,
     a_trappl,a_trapt,a_trapvc,a_trapvs,a_unpk,
     { fpu processor instructions - directly supported only. }
     { ieee aware and misc. condition codes not supported   }
     a_fabs,a_fadd,
     a_fbeq,a_fbne,a_fbngt,a_fbgt,a_fbge,a_fbnge,
     a_fblt,a_fbnlt,a_fble,a_fbgl,a_fbngl,a_fbgle,a_fbngle,
     a_fdbeq,a_fdbne,a_fdbgt,a_fdbngt,a_fdbge,a_fdbnge,
     a_fdblt,a_fdbnlt,a_fdble,a_fdbgl,a_fdbngl,a_fdbgle,a_fdbngle,
     a_fseq,a_fsne,a_fsgt,a_fsngt,a_fsge,a_fsnge,
     a_fslt,a_fsnlt,a_fsle,a_fsgl,a_fsngl,a_fsgle,a_fsngle,
     a_fcmp,a_fdiv,a_fmove,a_fmovem,
     a_fmul,a_fneg,a_fnop,a_fsqrt,a_fsub,a_fsgldiv,
     a_fsflmul,a_ftst,
     a_ftrapeq,a_ftrapne,a_ftrapgt,a_ftrapngt,a_ftrapge,a_ftrapnge,
     a_ftraplt,a_ftrapnlt,a_ftraple,a_ftrapgl,a_ftrapngl,a_ftrapgle,a_ftrapngle,
     { protected instructions }
     a_cprestore,a_cpsave,
     { fpu unit protected instructions                    }
     { and 68030/68851 common mmu instructions            }
     { (this may include 68040 mmu instructions)          }
     a_frestore,a_fsave,a_pflush,a_pflusha,a_pload,a_pmove,a_ptest,
     { useful for assembly language output }
     a_label,a_dbxx,a_sxx,a_bxx,a_fbxx
   {$ENDIF}
   );

  {# This should define the array of instructions as string }
  op2strtable=array[tasmop] of string[11];

Const
  {# First value of opcode enumeration }
  firstop = low(tasmop);
  {# Last value of opcode enumeration  }
  lastop  = high(tasmop);

{*****************************************************************************
                                  Registers
*****************************************************************************}
{ TODO : inline the included files, strip excess entries }
    type
      { Number of registers used for indexing in tables }
      tregisterindex=0..{ $i r68knor.inc}34-1;

    const
      { Available Superregisters }
      {$i r68ksup.inc}

      { ? whatever... }
      R_SUBWHOLE = R_SUBNONE;

      { Available Registers }
      {$i r68kcon.inc}

      { Integer Super registers first and last }
      first_int_imreg = RS_D7+1;

      { Float Super register first and last }
      first_fpu_imreg     = RS_FP7+1;

      { Integer Super registers first and last }
      first_addr_imreg = RS_SP+1;

      { MM Super register first and last }
      first_mm_supreg    = 0;
      first_mm_imreg     = 0;

      maxfpuregs = 8;

{ TODO: FIX BSSTART}
      regnumber_count_bsstart = 16;

{$IFDEF dummy}
  //nothing required?
{$ELSE}

      regnumber_table : array[tregisterindex] of tregister = (
        {$i r68knum.inc}
      );

      regstabs_table : array[tregisterindex] of shortint = (
        {$i r68ksta.inc}
      );

      regdwarf_table : array[tregisterindex] of shortint = (
{ TODO: reused stabs values!}
        {$i r68ksta.inc}
      );

      { registers which may be destroyed by calls }
      VOLATILE_INTREGISTERS = [RS_D0,RS_D1];
      VOLATILE_FPUREGISTERS = [];
      VOLATILE_ADDRESSREGISTER = [RS_A0,RS_A1];

    type
      totherregisterset = set of tregisterindex;
{$ENDIF}


{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond=(C_None,
         C_CC,C_LS,C_CS,C_LT,C_EQ,C_MI,C_F,C_NE,
         C_GE,C_PL,C_GT,C_T,C_HI,C_VC,C_LE,C_VS
      );


    const
      cond2str:array[TAsmCond] of string[3]=('',
        'cc','ls','cs','lt','eq','mi','f','ne',
        'ge','pl','gt','t','hi','vc','le','vs'
      );

{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlags = (
          F_E,F_NE,
          F_G,F_L,F_GE,F_LE,F_C,F_NC,F_A,F_AE,F_B,F_BE);

{*****************************************************************************
                                Reference
*****************************************************************************}

    type
      { direction of address register :      }
      {              (An)     (An)+   -(An)  }
      tdirection = (dir_none,dir_inc,dir_dec);


{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

       { S_NO = No Size of operand   }
       { S_B  = 8-bit size operand   }
       { S_W  = 16-bit size operand  }
       { S_L  = 32-bit size operand  }
       { Floating point types        }
       { S_FS  = single type (32 bit) }
       { S_FD  = double/64bit integer }
       { S_FX  = Extended type      }
       topsize = (S_NO,S_B,S_W,S_L,S_FS,S_FD,S_FX,S_IQ);

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      {# maximum number of operands in assembler instruction }
      max_operands = 4;

{*****************************************************************************
                          Default generic sizes
*****************************************************************************}

      {# Defines the default address size for a processor, }
      OS_ADDR = OS_32;
      {# the natural int size for a processor,             }
      OS_INT = OS_32;
      OS_SINT = OS_S32;
      {# the maximum float size for a processor,           }
      OS_FLOAT = OS_F64;
      {# the size of a vector register for a processor     }
      OS_VECTOR = OS_M128;

{$IFDEF dummy}
{$ELSE}
{*****************************************************************************
                               GDB Information
*****************************************************************************}

      {# Register indexes for stabs information, when some
         parameters or variables are stored in registers.

         Taken from m68kelf.h (DBX_REGISTER_NUMBER)
         from GCC 3.x source code.

         This is not compatible with the m68k-sun
         implementation.
      }
      stab_regindex : array[tregisterindex] of shortint =
      (
        {$i r68ksta.inc}
      );
{$ENDIF}

{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      {# Stack pointer register }
      NR_STACK_POINTER_REG = NR_SP;
      RS_STACK_POINTER_REG = RS_SP;
      {# Frame pointer register }
{ TODO: FIX ME!!! frame pointer is A5 on Amiga, but A6 on unixes?}
      NR_FRAME_POINTER_REG = NR_A5;
      RS_FRAME_POINTER_REG = RS_A5;

      {# Register for addressing absolute data in a position independant way,
         such as in PIC code. The exact meaning is ABI specific. For
         further information look at GCC source : PIC_OFFSET_TABLE_REGNUM
      }
{ TODO: FIX ME!!! pic offset reg conflicts with frame pointer?}
      NR_PIC_OFFSET_REG = NR_A5;
      { Return address for DWARF }
{ TODO: just a guess!}
      NR_RETURN_ADDRESS_REG = NR_A0;
      { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_D0;
      RS_FUNCTION_RETURN_REG = RS_D0;
      { Low part of 64bit return value }
      NR_FUNCTION_RETURN64_LOW_REG = NR_D0;
      RS_FUNCTION_RETURN64_LOW_REG = RS_D0;
      { High part of 64bit return value }
      NR_FUNCTION_RETURN64_HIGH_REG = NR_D1;
      RS_FUNCTION_RETURN64_HIGH_REG = RS_D1;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_FUNCTION_RETURN_REG;
      RS_FUNCTION_RESULT_REG = RS_FUNCTION_RETURN_REG;
      { The lowh part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_LOW_REG = NR_FUNCTION_RETURN64_LOW_REG;
      RS_FUNCTION_RESULT64_LOW_REG = RS_FUNCTION_RETURN64_LOW_REG;
      { The high part of 64bit value returned from a function }
      NR_FUNCTION_RESULT64_HIGH_REG = NR_FUNCTION_RETURN64_HIGH_REG;
      RS_FUNCTION_RESULT64_HIGH_REG = RS_FUNCTION_RETURN64_HIGH_REG;

      {# Floating point results will be placed into this register }
      NR_FPU_RESULT_REG = NR_FP0;

{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

      {# Registers which must be saved when calling a routine declared as
         cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
         saved should be the ones as defined in the target ABI and / or GCC.

         This value can be deduced from CALLED_USED_REGISTERS array in the
         GCC source.
      }
      saved_standard_registers : array[0..5] of tsuperregister = (RS_D2,RS_D3,RS_D4,RS_D5,RS_D6,RS_D7);
      saved_standard_address_registers : array[0..3] of tsuperregister = (RS_A2,RS_A3,RS_A4,RS_A5);
      
      { this is only for the generic code which is not used for this architecture }
      saved_mm_registers : array[0..0] of tsuperregister = (RS_NO);

      {# Required parameter alignment when calling a routine declared as
         stdcall and cdecl. The alignment value should be the one defined
         by GCC or the target ABI.

         The value of this constant is equal to the constant
         PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 4;  { for 32-bit version only }


{*****************************************************************************
                            CPU Dependent Constants
*****************************************************************************}

{$IFDEF dummy}
function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
function reg_cgsize(const reg: tregister): tcgsize;

function std_regnum_search(const s:string):Tregister;
function std_regname(r:Tregister):string;

function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}

{$ELSE}
{*****************************************************************************
                                  Helpers
*****************************************************************************}

function  is_calljmp(o:tasmop):boolean;

procedure inverse_flags(var r : TResFlags);
function  flags_to_cond(const f: TResFlags) : TAsmCond;
function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
function reg_cgsize(const reg: tregister): tcgsize;

function findreg_by_number(r:Tregister):tregisterindex;
function std_regnum_search(const s:string):Tregister;
function std_regname(r:Tregister):string;

function isaddressregister(reg : tregister) : boolean;

function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}
function dwarf_reg(r:tregister):shortint;
{$ENDIF}

implementation

{$IFDEF dummy}
function reg_cgsize(const reg: tregister): tcgsize;
begin
  result:=OS_32;
end;

function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
begin
  Result := R_SUBWHOLE;
end;

function std_regnum_search(const s:string):Tregister;
begin
  Result := TRegisterLowEnum;
end;

function std_regname(r:Tregister):string;
begin
  Result := '';
end;

{$ELSE}
uses
  verbose,
  rgbase;


    const
      std_regname_table : array[tregisterindex] of string[7] = (
        {$i r68kstd.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i r68krni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i r68ksri.inc}
      );


{$IFDEF dummy}
{$ELSE}
{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function is_calljmp(o:tasmop):boolean;
      begin
        is_calljmp := false;
        if o in [A_BXX,A_FBXX,A_DBXX,A_BCC..A_BVS,A_DBCC..A_DBVS,A_FBEQ..A_FSNGLE,
          A_JSR,A_BSR,A_JMP] then
           is_calljmp := true;
      end;


    procedure inverse_flags(var r: TResFlags);
      const flagsinvers : array[F_E..F_BE] of tresflags =
            (F_NE,F_E,
             F_LE,F_GE,
             F_L,F_G,
             F_NC,F_C,
             F_BE,F_B,
             F_AE,F_A);
      begin
         r:=flagsinvers[r];
      end;



    function flags_to_cond(const f: TResFlags) : TAsmCond;
      const flags2cond: array[tresflags] of tasmcond = (
          C_EQ,{F_E     equal}
          C_NE,{F_NE    not equal}
          C_GT,{F_G     gt signed}
          C_LT,{F_L     lt signed}
          C_GE,{F_GE    ge signed}
          C_LE,{F_LE    le signed}
          C_CS,{F_C     carry set}
          C_CC,{F_NC    carry clear}
          C_HI,{F_A     gt unsigned}
          C_CC,{F_AE    ge unsigned}
          C_CS,{F_B     lt unsigned}
          C_LS);{F_BE    le unsigned}
      begin
        flags_to_cond := flags2cond[f];
      end;

    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
      var p: pointer;
      begin
        case s of
          OS_NO: begin
{ TODO: FIX ME!!! results in bad code generation}
            cgsize2subreg:=R_SUBWHOLE;
            end;

          OS_8,OS_S8:
            cgsize2subreg:=R_SUBWHOLE;
          OS_16,OS_S16:
            cgsize2subreg:=R_SUBWHOLE;
          OS_32,OS_S32:
            cgsize2subreg:=R_SUBWHOLE;
          OS_64,OS_S64:
            begin
//             writeln('64bit regsize?');
             cgsize2subreg:=R_SUBWHOLE;
            end;
          OS_F32 :
            cgsize2subreg:=R_SUBFS;
          OS_F64 :
            cgsize2subreg:=R_SUBFD;
{
            begin
              // is this correct? (KB)
              cgsize2subreg:=R_SUBNONE;
            end;
}
          else begin
            writeln('M68K: invalid register size');
    // this supposed to be debug
    //        p:=nil; dword(p^):=0;
    //        internalerror(200301231);
            cgsize2subreg:=R_SUBWHOLE;
          end;
        end;
      end;


    function reg_cgsize(const reg: tregister): tcgsize;
      begin
        case getregtype(reg) of
          R_ADDRESSREGISTER,
          R_INTREGISTER :
            result:=OS_32;
          R_FPUREGISTER :
            result:=OS_F64;
          else
            internalerror(200303181);
        end;
      end;


    function findreg_by_number(r:Tregister):tregisterindex;
      begin
        result:=findreg_by_number_table(r,regnumber_index);
      end;


    function std_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_name_table(s,std_regname_table,std_regname_index)];
      end;


    function std_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number_table(r,regnumber_index);
        if p<>0 then
          result:=std_regname_table[p]
        else
          result:=generic_regname(r);
      end;


    function isaddressregister(reg : tregister) : boolean;
      begin
        result:=getregtype(reg)=R_ADDRESSREGISTER;
      end;



    function dwarf_reg(r:tregister):shortint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
        if result=-1 then
          internalerror(200603251);
      end;
{$ENDIF}
{$ENDIF}

function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
begin
  result := c;
end;

function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}
begin
  result := c1 = c2;
end;

end.
