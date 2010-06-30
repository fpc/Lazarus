{
    Copyright (c) 1998-2002 by the FPC team

    This unit implements the code generator for the parser - by DoDi.

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

(* This unit *must* register a CPU object, in the initialization section.
  The class is derived from TCG and (here) does nothing.
*)

unit cgcpu;

{.$DEFINE DEBUG_CHARLIE}

{$IFNDEF DEBUG_CHARLIE}
  {$WARNINGS OFF}
{$ENDIF}

{$i fpcdefs.inc}

interface

uses
 cgbase,cgobj,globtype,
 aasmbase,aasmtai,aasmdata,aasmcpu,
 cpubase,cpuinfo,
 parabase,cpupara,
 node,symconst,symtype,symdef,
 cgutils,cg64f32;

type
  tcg68k = class(tcg)
    procedure init_register_allocators;override;
    procedure done_register_allocators;override;

    procedure a_param_reg(list : TAsmList;size : tcgsize;r : tregister;const cgpara : tcgpara);override;
    procedure a_param_const(list : TAsmList;size : tcgsize;a : aint;const cgpara : tcgpara);override;
    procedure a_param_ref(list : TAsmList;size : tcgsize;const r : treference;const cgpara : tcgpara);override;
    procedure a_paramaddr_ref(list : TAsmList;const r : treference;const cgpara : tcgpara);override;

    procedure a_call_name(list : TAsmList;const s : string; weak: boolean);override;
    procedure a_call_reg(list : TAsmList;reg : tregister);override;

    procedure a_load_const_reg(list : TAsmList;size : tcgsize;a : aint;register : tregister);override;
    procedure a_load_const_ref(list : TAsmList; tosize: tcgsize; a : aint;const ref : treference);override;

    procedure a_load_reg_ref(list : TAsmList;fromsize,tosize : tcgsize;register : tregister;const ref : treference);override;
    procedure a_load_reg_reg(list : TAsmList;fromsize,tosize : tcgsize;reg1,reg2 : tregister);override;
    procedure a_load_ref_reg(list : TAsmList;fromsize,tosize : tcgsize;const ref : treference;register : tregister);override;
    procedure a_load_ref_ref(list : TAsmList;fromsize,tosize : tcgsize;const sref : treference;const dref : treference);override;

    procedure a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);override;
    procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister); override;
    procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister); override;
    procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference); override;

    procedure a_loadmm_reg_reg(list: TAsmList;fromsize,tosize : tcgsize; reg1, reg2: tregister;shuffle : pmmshuffle); override;
    procedure a_loadmm_ref_reg(list: TAsmList;fromsize,tosize : tcgsize; const ref: treference; reg: tregister;shuffle : pmmshuffle); override;
    procedure a_loadmm_reg_ref(list: TAsmList;fromsize,tosize : tcgsize; reg: tregister; const ref: treference;shuffle : pmmshuffle); override;
    procedure a_parammm_reg(list: TAsmList; size: tcgsize; reg: tregister;const locpara : TCGPara;shuffle : pmmshuffle); override;

    procedure a_op_const_reg(list : TAsmList; Op: TOpCG; size: tcgsize; a: aint; reg: TRegister); override;
    //        procedure a_op_const_ref(list : TAsmList; Op: TOpCG; size: TCGSize; a: aint; const ref: TReference); override;
    procedure a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; reg1, reg2: TRegister); override;

    procedure a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : aint;reg : tregister;
      l : tasmlabel);override;
    procedure a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel); override;
    procedure a_jmp_always(list : TAsmList;l: tasmlabel); override;
    procedure a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel); override;
    procedure g_flags2reg(list: TAsmList; size: TCgSize; const f: tresflags; reg: TRegister); override;

    procedure g_concatcopy(list : TAsmList;const source,dest : treference;len : aint);override;
    { generates overflow checking code for a node }
    procedure g_overflowcheck(list: TAsmList; const l:tlocation; def:tdef); override;
    procedure g_copyvaluepara_openarray(list : TAsmList;const ref:treference;const lenloc:tlocation;elesize:aint;destreg:tregister);override;

    procedure g_proc_entry(list : TAsmList;localsize : longint;nostackframe:boolean);override;
    procedure g_proc_exit(list : TAsmList;parasize:longint;nostackframe:boolean);override;

    //        procedure g_restore_frame_pointer(list : TAsmList);override;
    //        procedure g_return_from_proc(list : TAsmList;parasize : aint);override;
    procedure g_restore_registers(list:TAsmList);override;
    procedure g_save_registers(list:TAsmList);override;

    //        procedure g_save_all_registers(list : TAsmList);override;
    //        procedure g_restore_all_registers(list : TAsmList;const funcretparaloc:TCGPara);override;

    procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);override;

    protected
    function fixref(list: TAsmList; var ref: treference): boolean;
    private
    { # Sign or zero extend the register to a full 32-bit value.
        The new value is left in the same register.
    }
    procedure sign_extend(list: TAsmList;_oldsize : tcgsize; reg: tregister);
    procedure a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);
  end;

{$IFDEF dummy}
{$ELSE}
tcg64f68k = class(tcg64f32)
  procedure a_op64_reg_reg(list : TAsmList;op:TOpCG; size: tcgsize; regsrc,regdst : tregister64);override;
  procedure a_op64_const_reg(list : TAsmList;op:TOpCG; size: tcgsize; value : int64;regdst : tregister64);override;
end;

{ This function returns true if the reference+offset is valid.
 Otherwise extra code must be generated to solve the reference.

 On the m68k, this verifies that the reference is valid
 (e.g : if index register is used, then the max displacement
  is 256 bytes, if only base is used, then max displacement
  is 32K
}
function isvalidrefoffset(const ref: treference): boolean;

const
TCGSize2OpSize: Array[tcgsize] of topsize =
  (S_NO,S_B,S_W,S_L,S_L,S_NO,S_B,S_W,S_L,S_L,S_NO,
   S_FS,S_FD,S_FX,S_NO,S_NO,
   S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO);
{$ENDIF}


implementation

{$IFDEF dummy}
{$ELSE}
uses
  globals,verbose,systems,cutils,
  symsym,defutil,paramgr,procinfo,
  rgobj,tgobj,rgcpu,fmodule;


const
{ opcode table lookup }
topcg2tasmop: Array[topcg] of tasmop =
(
 A_NONE,
 A_MOVE,
 A_ADD,
 A_AND,
 A_DIVU,
 A_DIVS,
 A_MULS,
 A_MULU,
 A_NEG,
 A_NOT,
 A_OR,
 A_ASR,
 A_LSL,
 A_LSR,
 A_SUB,
 A_EOR,
 A_NONE,
 A_NONE
);


TOpCmp2AsmCond: Array[topcmp] of TAsmCond =
(
 C_NONE,
 C_EQ,
 C_GT,
 C_LT,
 C_GE,
 C_LE,
 C_NE,
 C_LS,
 C_CS,
 C_CC,
 C_HI
);


 function isvalidrefoffset(const ref: treference): boolean;
  begin
     isvalidrefoffset := true;
     if ref.index <> NR_NO then
       begin
         if ref.base <> NR_NO then
            internalerror(20020814);
         if (ref.offset < low(shortint)) or (ref.offset > high(shortint)) then
            isvalidrefoffset := false
       end
     else
       begin
         if (ref.offset < low(smallint)) or (ref.offset > high(smallint)) then
            isvalidrefoffset := false;
       end;
  end;


{****************************************************************************}
{                               TCG68K                                       }
{****************************************************************************}


function use_push(const cgpara:tcgpara):boolean;
  begin
    result:=(not use_fixed_stack) and
            assigned(cgpara.location) and
            (cgpara.location^.loc=LOC_REFERENCE) and
            (cgpara.location^.reference.index=NR_STACK_POINTER_REG);
  end;


procedure tcg68k.init_register_allocators;
  begin
    inherited init_register_allocators;
    rg[R_INTREGISTER]:=trgcpu.create(R_INTREGISTER,R_SUBWHOLE,
      [RS_D0,RS_D1,RS_D2,RS_D3,RS_D4,RS_D5,RS_D6,RS_D7],
      first_int_imreg,[]);
    rg[R_ADDRESSREGISTER]:=trgcpu.create(R_ADDRESSREGISTER,R_SUBWHOLE,
      [RS_A0,RS_A1,RS_A2,RS_A3,RS_A4,RS_A5,RS_A6],
      first_addr_imreg,[]);
    rg[R_FPUREGISTER]:=trgcpu.create(R_FPUREGISTER,R_SUBNONE,
      [RS_FP0,RS_FP1,RS_FP2,RS_FP3,RS_FP4,RS_FP5,RS_FP6,RS_FP7],
      first_fpu_imreg,[]);
  end;


procedure tcg68k.done_register_allocators;
  begin
    rg[R_INTREGISTER].free;
    rg[R_FPUREGISTER].free;
    rg[R_ADDRESSREGISTER].free;
    inherited done_register_allocators;
  end;


procedure tcg68k.a_param_reg(list : TAsmList;size : tcgsize;r : tregister;const cgpara : tcgpara);
  var
    pushsize : tcgsize;
    ref : treference;
  begin
{$ifdef DEBUG_CHARLIE}
//        writeln('a_param_reg');
{$endif DEBUG_CHARLIE}
    { it's probably necessary to port this from x86 later, or provide an m68k solution (KB) }
{ TODO: FIX ME! check_register_size()}
    // check_register_size(size,r);
    if use_push(cgpara) then
      begin
        cgpara.check_simple_location;
        if tcgsize2size[cgpara.location^.size]>cgpara.alignment then
          pushsize:=cgpara.location^.size
        else
          pushsize:=int_cgsize(cgpara.alignment);

        reference_reset_base(ref, NR_STACK_POINTER_REG, 0, cgpara.alignment);
        ref.direction := dir_dec;
        list.concat(taicpu.op_reg_ref(A_MOVE,tcgsize2opsize[pushsize],makeregsize(list,r,pushsize),ref));
      end
    else
      inherited a_param_reg(list,size,r,cgpara);
  end;


procedure tcg68k.a_param_const(list : TAsmList;size : tcgsize;a : aint;const cgpara : tcgpara);
  var
    pushsize : tcgsize;
    ref : treference;
  begin
{$ifdef DEBUG_CHARLIE}
//        writeln('a_param_const');
{$endif DEBUG_CHARLIE}
    if use_push(cgpara) then
      begin
        cgpara.check_simple_location;
        if tcgsize2size[cgpara.location^.size]>cgpara.alignment then
          pushsize:=cgpara.location^.size
        else
          pushsize:=int_cgsize(cgpara.alignment);

        reference_reset_base(ref, NR_STACK_POINTER_REG, 0, cgpara.alignment);
        ref.direction := dir_dec;
        list.concat(taicpu.op_const_ref(A_MOVE,tcgsize2opsize[pushsize],a,ref));
      end
    else
      inherited a_param_const(list,size,a,cgpara);
  end;


procedure tcg68k.a_param_ref(list : TAsmList;size : tcgsize;const r : treference;const cgpara : tcgpara);

    procedure pushdata(paraloc:pcgparalocation;ofs:aint);
    var
      pushsize : tcgsize;
      tmpreg   : tregister;
      href     : treference;
      ref      : treference;
    begin
      if not assigned(paraloc) then
        exit;
{ TODO: FIX ME!!! this also triggers location bug }
      {if (paraloc^.loc<>LOC_REFERENCE) or
         (paraloc^.reference.index<>NR_STACK_POINTER_REG) or
         (tcgsize2size[paraloc^.size]>sizeof(aint)) then
        internalerror(200501162);}

      { Pushes are needed in reverse order, add the size of the
        current location to the offset where to load from. This
        prevents wrong calculations for the last location when
        the size is not a power of 2 }
      if assigned(paraloc^.next) then
        pushdata(paraloc^.next,ofs+tcgsize2size[paraloc^.size]);
      { Push the data starting at ofs }
      href:=r;
      inc(href.offset,ofs);
      if tcgsize2size[paraloc^.size]>cgpara.alignment then
        pushsize:=paraloc^.size
      else
        pushsize:=int_cgsize(cgpara.alignment);

      reference_reset_base(ref, NR_STACK_POINTER_REG, 0, tcgsize2size[paraloc^.size]);
      ref.direction := dir_dec;

      if tcgsize2size[paraloc^.size]<cgpara.alignment then
        begin
          tmpreg:=getintregister(list,pushsize);
          a_load_ref_reg(list,paraloc^.size,pushsize,href,tmpreg);
          list.concat(taicpu.op_reg_ref(A_MOVE,tcgsize2opsize[pushsize],tmpreg,ref));
        end
      else
          list.concat(taicpu.op_ref_ref(A_MOVE,tcgsize2opsize[pushsize],href,ref));
    end;

  var
    len : aint;
    href : treference;
  begin
{$ifdef DEBUG_CHARLIE}
//        writeln('a_param_ref');
{$endif DEBUG_CHARLIE}

    { cgpara.size=OS_NO requires a copy on the stack }
    if use_push(cgpara) then
      begin
        { Record copy? }
        if (cgpara.size in [OS_NO,OS_F64]) or (size=OS_NO) then
          begin
            cgpara.check_simple_location;
            len:=align(cgpara.intsize,cgpara.alignment);
            g_stackpointer_alloc(list,len);
            reference_reset_base(href,NR_STACK_POINTER_REG,0,cgpara.alignment);
            g_concatcopy(list,r,href,len);
          end
        else
          begin
            if tcgsize2size[cgpara.size]<>tcgsize2size[size] then
              internalerror(200501161);
            { We need to push the data in reverse order,
              therefor we use a recursive algorithm }
            pushdata(cgpara.location,0);
          end
      end
    else
      inherited a_param_ref(list,size,r,cgpara);
  end;


procedure tcg68k.a_paramaddr_ref(list : TAsmList;const r : treference;const cgpara : tcgpara);
  var
    tmpreg : tregister;
    opsize : topsize;
  begin
{$ifdef DEBUG_CHARLIE}
//        writeln('a_paramaddr_ref');
{$endif DEBUG_CHARLIE}
    with r do
      begin
        { i suppose this is not required for m68k (KB) }
//            if (segment<>NR_NO) then
//              cgmessage(cg_e_cant_use_far_pointer_there);
        if not use_push(cgpara) then
          begin
            cgpara.check_simple_location;
            opsize:=tcgsize2opsize[OS_ADDR];
            if (segment=NR_NO) and (base=NR_NO) and (index=NR_NO) then
              begin
                if assigned(symbol) then
//                      list.concat(Taicpu.Op_sym_ofs(A_PUSH,opsize,symbol,offset))
                else;
//                      list.concat(Taicpu.Op_const(A_PUSH,opsize,offset));
              end
            else if (segment=NR_NO) and (base=NR_NO) and (index<>NR_NO) and
                    (offset=0) and (scalefactor=0) and (symbol=nil) then
//                  list.concat(Taicpu.Op_reg(A_PUSH,opsize,index))
            else if (segment=NR_NO) and (base<>NR_NO) and (index=NR_NO) and
                    (offset=0) and (symbol=nil) then
//                  list.concat(Taicpu.Op_reg(A_PUSH,opsize,base))
            else
              begin
                tmpreg:=getaddressregister(list);
                a_loadaddr_ref_reg(list,r,tmpreg);
//                    list.concat(taicpu.op_reg(A_PUSH,opsize,tmpreg));
              end;
          end
        else
          inherited a_paramaddr_ref(list,r,cgpara);
      end;
  end;



function tcg68k.fixref(list: TAsmList; var ref: treference): boolean;

   begin
     result:=false;
     { The Coldfire and MC68020+ have extended
       addressing capabilities with a 32-bit
       displacement.
     }
     if (current_settings.cputype<>cpu_MC68000) then
       exit;
     if (ref.base<>NR_NO) then
       begin
         if (ref.index <> NR_NO) and assigned(ref.symbol) then
            internalerror(20020814);
         { base + reg }
         if ref.index <> NR_NO then
            begin
               { base + reg + offset }
               if (ref.offset < low(shortint)) or (ref.offset > high(shortint)) then
                 begin
                    list.concat(taicpu.op_const_reg(A_ADD,S_L,ref.offset,ref.base));
                    fixref := true;
                    ref.offset := 0;
                    exit;
                 end;
            end
         else
         { base + offset }
         if (ref.offset < low(smallint)) or (ref.offset > high(smallint)) then
           begin
             list.concat(taicpu.op_const_reg(A_ADD,S_L,ref.offset,ref.base));
             fixref := true;
             ref.offset := 0;
             exit;
           end;
       end;
   end;



procedure tcg68k.a_call_name(list : TAsmList;const s : string; weak: boolean);
  var
    sym: tasmsymbol;
  begin
    if not(weak) then
      sym:=current_asmdata.RefAsmSymbol(s)
    else
      sym:=current_asmdata.WeakRefAsmSymbol(s);

    list.concat(taicpu.op_sym(A_JSR,S_NO,current_asmdata.RefAsmSymbol(s)));
  end;


procedure tcg68k.a_call_reg(list : TAsmList;reg: tregister);
  var
    tmpref : treference;
	tmpreg : tregister;
  begin
{$ifdef DEBUG_CHARLIE}
	list.concat(tai_comment.create(strpnew('a_call_reg')));
{$endif}
	if isaddressregister(reg) then
	  begin
	    { if we have an address register, we can jump to the address directly }
        reference_reset_base(tmpref,reg,0,4);
	  end
	else
	  begin
	    { if we have a data register, we need to move it to an address register first }
	    tmpreg:=getaddressregister(list);
        reference_reset_base(tmpref,tmpreg,0,4);
	    list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg,tmpreg));
	  end;
	list.concat(taicpu.op_ref(A_JSR,S_NO,tmpref));
 end;



procedure tcg68k.a_load_const_reg(list : TAsmList;size : tcgsize;a : aint;register : tregister);
  begin
{$ifdef DEBUG_CHARLIE}
//        writeln('a_load_const_reg');
{$endif DEBUG_CHARLIE}

    if isaddressregister(register) then
     begin
       list.concat(taicpu.op_const_reg(A_MOVE,S_L,longint(a),register))
     end
    else
    if a = 0 then
       list.concat(taicpu.op_reg(A_CLR,S_L,register))
    else
     begin
       if (longint(a) >= low(shortint)) and (longint(a) <= high(shortint)) then
          list.concat(taicpu.op_const_reg(A_MOVEQ,S_L,longint(a),register))
       else
          list.concat(taicpu.op_const_reg(A_MOVE,S_L,longint(a),register))
     end;
  end;

procedure tcg68k.a_load_const_ref(list : TAsmList; tosize: tcgsize; a : aint;const ref : treference);
  begin
{$ifdef DEBUG_CHARLIE}
    list.concat(tai_comment.create(strpnew('a_load_const_ref')));
{$endif DEBUG_CHARLIE}

    list.concat(taicpu.op_const_ref(A_MOVE,S_L,longint(a),ref));
  end;


procedure tcg68k.a_load_reg_ref(list : TAsmList;fromsize,tosize : tcgsize;register : tregister;const ref : treference);
  var
   href : treference;
  begin
    href := ref;
    fixref(list,href);
{$ifdef DEBUG_CHARLIE}
    list.concat(tai_comment.create(strpnew('a_load_reg_ref')));
{$endif DEBUG_CHARLIE}
    { move to destination reference }
    list.concat(taicpu.op_reg_ref(A_MOVE,TCGSize2OpSize[fromsize],register,href));
  end;


procedure tcg68k.a_load_ref_ref(list : TAsmList;fromsize,tosize : tcgsize;const sref : treference;const dref : treference);
  var
    aref: treference;
    bref: treference;
  begin
    aref := sref;
    bref := dref;
    fixref(list,aref);
    fixref(list,bref);
{$ifdef DEBUG_CHARLIE}
//        writeln('a_load_ref_ref');
{$endif DEBUG_CHARLIE}
    list.concat(taicpu.op_ref_ref(A_MOVE,TCGSize2OpSize[fromsize],aref,bref));
  end;


procedure tcg68k.a_load_reg_reg(list : TAsmList;fromsize,tosize : tcgsize;reg1,reg2 : tregister);
  begin
     { move to destination register }
     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1,reg2));
     { zero/sign extend register to 32-bit }
     sign_extend(list, fromsize, reg2);
  end;


procedure tcg68k.a_load_ref_reg(list : TAsmList;fromsize,tosize : tcgsize;const ref : treference;register : tregister);
  var
   href : treference;
  begin
     href := ref;
     fixref(list,href);
     list.concat(taicpu.op_ref_reg(A_MOVE,TCGSize2OpSize[fromsize],href,register));
     { extend the value in the register }
     sign_extend(list, tosize, register);
  end;


procedure tcg68k.a_loadaddr_ref_reg(list : TAsmList;const ref : treference;r : tregister);
 var
   href : treference;
//       p: pointer;
  begin
     { TODO: FIX ME!!! take a look on this mess again...}
//        if getregtype(r)=R_ADDRESSREGISTER then
//          begin
//            writeln('address reg?!?');
//            p:=nil; dword(p^):=0; {DEBUG CODE... :D )
//            internalerror(2002072901);
//          end;
    href:=ref;
    fixref(list, href);
    list.concat(taicpu.op_ref_reg(A_LEA,S_L,href,r));
  end;


procedure tcg68k.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister);
  begin
    { in emulation mode, only 32-bit single is supported }
    if cs_fp_emulation in current_settings.moduleswitches then
      list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1,reg2))
    else
      list.concat(taicpu.op_reg_reg(A_FMOVE,tcgsize2opsize[tosize],reg1,reg2));
  end;


procedure tcg68k.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister);
 var
  opsize : topsize;
  href : treference;
  tmpreg : tregister;
  begin
    opsize := tcgsize2opsize[fromsize];
    { extended is not supported, since it is not available on Coldfire }
    if opsize = S_FX then
      internalerror(20020729);
    href := ref;
    fixref(list,href);
    { in emulation mode, only 32-bit single is supported }
    if cs_fp_emulation in current_settings.moduleswitches then
       list.concat(taicpu.op_ref_reg(A_MOVE,S_L,href,reg))
    else
       begin
         list.concat(taicpu.op_ref_reg(A_FMOVE,opsize,href,reg));
         if (tosize < fromsize) then
           a_loadfpu_reg_reg(list,fromsize,tosize,reg,reg);
       end;
  end;

procedure tcg68k.a_loadfpu_reg_ref(list: TAsmList; fromsize,tosize: tcgsize; reg: tregister; const ref: treference);
  var
   opsize : topsize;
  begin
    opsize := tcgsize2opsize[tosize];
    { extended is not supported, since it is not available on Coldfire }
    if opsize = S_FX then
      internalerror(20020729);
    { in emulation mode, only 32-bit single is supported }
    if cs_fp_emulation in current_settings.moduleswitches then
      list.concat(taicpu.op_reg_ref(A_MOVE,S_L,reg, ref))
    else
      list.concat(taicpu.op_reg_ref(A_FMOVE,opsize,reg, ref));
  end;


procedure tcg68k.a_loadmm_reg_reg(list: TAsmList;fromsize,tosize : tcgsize; reg1, reg2: tregister;shuffle : pmmshuffle);
  begin
    internalerror(20020729);
  end;


procedure tcg68k.a_loadmm_ref_reg(list: TAsmList;fromsize,tosize : tcgsize; const ref: treference; reg: tregister;shuffle : pmmshuffle);
  begin
    internalerror(20020729);
  end;


procedure tcg68k.a_loadmm_reg_ref(list: TAsmList;fromsize,tosize : tcgsize; reg: tregister; const ref: treference;shuffle : pmmshuffle);
  begin
    internalerror(20020729);
  end;


procedure tcg68k.a_parammm_reg(list: TAsmList; size: tcgsize; reg: tregister;const locpara : TCGPara;shuffle : pmmshuffle);
  begin
    internalerror(20020729);
  end;


procedure tcg68k.a_op_const_reg(list : TAsmList; Op: TOpCG; size: tcgsize; a: aint; reg: TRegister);
  var
   scratch_reg : tregister;
   scratch_reg2: tregister;
   opcode : tasmop;
   r,r2 : Tregister;
  begin
    optimize_op_const(op, a);
    opcode := topcg2tasmop[op];
    case op of
      OP_NONE :
        begin
          { Opcode is optimized away }
        end;
      OP_MOVE :
        begin
          { Optimized, replaced with a simple load }
          a_load_const_reg(list,size,a,reg);
        end;
      OP_ADD :
          begin
            if (a >= 1) and (a <= 8) then
                list.concat(taicpu.op_const_reg(A_ADDQ,S_L,a, reg))
            else
              begin
                { all others, including coldfire }
                list.concat(taicpu.op_const_reg(A_ADD,S_L,a, reg));
              end;
          end;
      OP_AND,
      OP_OR:
          begin
             list.concat(taicpu.op_const_reg(topcg2tasmop[op],S_L,longint(a), reg));
          end;
      OP_DIV :
          begin
             internalerror(20020816);
          end;
      OP_IDIV :
          begin
             internalerror(20020816);
          end;
      OP_IMUL :
          begin
            if current_settings.cputype = cpu_MC68000 then
               begin
                 r:=NR_D0;
                 r2:=NR_D1;
                 cg.getcpuregister(list,NR_D0);
                 cg.getcpuregister(list,NR_D1);
                 list.concat(taicpu.op_const_reg(A_MOVE,S_L,a, r));
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, r2));
                 cg.a_call_name(list,'FPC_MUL_LONGINT',false);
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,r, reg));
                 cg.ungetcpuregister(list,r);
                 cg.ungetcpuregister(list,r2);

               end
              else
                begin
                  if (isaddressregister(reg)) then
                   begin
                     scratch_reg := getintregister(list,OS_INT);
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, scratch_reg));
                     list.concat(taicpu.op_const_reg(A_MULS,S_L,a,scratch_reg));
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,scratch_reg,reg));
                   end
                  else
                     list.concat(taicpu.op_const_reg(A_MULS,S_L,a,reg));
                end;
          end;
      OP_MUL :
          begin
             if current_settings.cputype = cpu_MC68000 then
               begin
                 r:=NR_D0;
                 r2:=NR_D1;
                 cg.getcpuregister(list,NR_D0);
                 cg.getcpuregister(list,NR_D1);
                 list.concat(taicpu.op_const_reg(A_MOVE,S_L,a, r));
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, r2));
                 cg.a_call_name(list,'FPC_MUL_LONGWORD',false);
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,r, reg));
                 cg.ungetcpuregister(list,r);
                 cg.ungetcpuregister(list,r2);
               end
              else
                begin
                  if (isaddressregister(reg)) then
                   begin
                     scratch_reg := getintregister(list,OS_INT);
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, scratch_reg));
                     list.concat(taicpu.op_const_reg(A_MULU,S_L,a,scratch_reg));
                     list.concat(taicpu.op_reg_reg(A_MOVE,S_L,scratch_reg,reg));
                   end
                  else
                     list.concat(taicpu.op_const_reg(A_MULU,S_L,a,reg));
                end;
          end;
      OP_SAR,
      OP_SHL,
      OP_SHR :
          begin
            if (a >= 1) and (a <= 8) then
             begin
               { now allowed to shift an address register }
               if (isaddressregister(reg)) then
                 begin
                   scratch_reg := getintregister(list,OS_INT);
                   list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, scratch_reg));
                   list.concat(taicpu.op_const_reg(opcode,S_L,a, scratch_reg));
                   list.concat(taicpu.op_reg_reg(A_MOVE,S_L,scratch_reg,reg));
                 end
               else
                 list.concat(taicpu.op_const_reg(opcode,S_L,a, reg));
             end
            else
             begin
               { we must load the data into a register ... :() }
               scratch_reg := cg.getintregister(list,OS_INT);
               list.concat(taicpu.op_const_reg(A_MOVE,S_L,a, scratch_reg));
               { again... since shifting with address register is not allowed }
               if (isaddressregister(reg)) then
                 begin
                   scratch_reg2 := cg.getintregister(list,OS_INT);
                   list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg, scratch_reg2));
                   list.concat(taicpu.op_reg_reg(opcode,S_L,scratch_reg, scratch_reg2));
                   list.concat(taicpu.op_reg_reg(A_MOVE,S_L,scratch_reg2,reg));
                 end
               else
                 list.concat(taicpu.op_reg_reg(opcode,S_L,scratch_reg, reg));
             end;
          end;
      OP_SUB :
          begin
            if (a >= 1) and (a <= 8) then
                list.concat(taicpu.op_const_reg(A_SUBQ,S_L,a,reg))
            else
              begin
                { all others, including coldfire }
                list.concat(taicpu.op_const_reg(A_SUB,S_L,a, reg));
              end;
          end;
      OP_XOR :
          begin
            list.concat(taicpu.op_const_reg(A_EORI,S_L,a, reg));
          end;
    else
        internalerror(20020729);
     end;
  end;

{
procedure tcg68k.a_op_const_ref(list : TAsmList; Op: TOpCG; size: TCGSize; a: aint; const ref: TReference);
  var
    opcode: tasmop;
  begin
    writeln('a_op_const_ref');

    optimize_op_const(op, a);
    opcode := topcg2tasmop[op];
    case op of
      OP_NONE :
        begin
          { opcode was optimized away }
        end;
      OP_MOVE :
        begin
          { Optimized, replaced with a simple load }
          a_load_const_ref(list,size,a,ref);
        end;
      else
        begin
          internalerror(2007010101);
        end;
    end;
  end;
}

procedure tcg68k.a_op_reg_reg(list : TAsmList; Op: TOpCG; size: TCGSize; reg1, reg2: TRegister);
  var
   hreg1,hreg2,r,r2: tregister;
  begin
    case op of
      OP_ADD :
          begin
             if current_settings.cputype = cpu_ColdFire then
              begin
                { operation only allowed only a longword }
                sign_extend(list, size, reg1);
                sign_extend(list, size, reg2);
                list.concat(taicpu.op_reg_reg(A_ADD,S_L,reg1, reg2));
              end
             else
              begin
                list.concat(taicpu.op_reg_reg(A_ADD,TCGSize2OpSize[size],reg1, reg2));
              end;
          end;
      OP_AND,OP_OR,
      OP_SAR,OP_SHL,
      OP_SHR,OP_SUB,OP_XOR :
          begin
             { load to data registers }
             if (isaddressregister(reg1)) then
               begin
                 hreg1 := getintregister(list,OS_INT);
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1,hreg1));
               end
             else
               hreg1 := reg1;

             if (isaddressregister(reg2))  then
               begin
                  hreg2:= getintregister(list,OS_INT);
                  list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2,hreg2));
               end
             else
               hreg2 := reg2;

             if current_settings.cputype = cpu_ColdFire then
              begin
                { operation only allowed only a longword }
                {!***************************************
                  in the case of shifts, the value to
                  shift by, should already be valid, so
                  no need to sign extend the value
                 !
                }
                if op in [OP_AND,OP_OR,OP_SUB,OP_XOR] then
                   sign_extend(list, size, hreg1);
                sign_extend(list, size, hreg2);
                list.concat(taicpu.op_reg_reg(topcg2tasmop[op],S_L,hreg1, hreg2));
              end
             else
              begin
                list.concat(taicpu.op_reg_reg(topcg2tasmop[op],TCGSize2OpSize[size],hreg1, hreg2));
              end;

             { move back result into destination register }
             if reg2 <> hreg2 then
               begin
                  list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hreg2,reg2));
               end;
          end;
      OP_DIV :
          begin
             internalerror(20020816);
          end;
      OP_IDIV :
          begin
             internalerror(20020816);
          end;
      OP_IMUL :
          begin
             sign_extend(list, size,reg1);
             sign_extend(list, size,reg2);
             if current_settings.cputype = cpu_MC68000 then
               begin
                 r:=NR_D0;
                 r2:=NR_D1;
                 cg.getcpuregister(list,NR_D0);
                 cg.getcpuregister(list,NR_D1);
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1, r));
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2, r2));
                 cg.a_call_name(list,'FPC_MUL_LONGINT',false);
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,r, reg2));
                 cg.ungetcpuregister(list,r);
                 cg.ungetcpuregister(list,r2);
               end
              else
                begin
//                     writeln('doing 68020');

                 if (isaddressregister(reg1)) then
                   hreg1 := getintregister(list,OS_INT)
                 else
                   hreg1 := reg1;
                 if (isaddressregister(reg2))  then
                   hreg2:= getintregister(list,OS_INT)
                 else
                   hreg2 := reg2;

                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1,hreg1));
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2,hreg2));

                 list.concat(taicpu.op_reg_reg(A_MULS,S_L,reg1,reg2));

                 { move back result into destination register }

                 if reg2 <> hreg2 then
                   begin
                      list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hreg2,reg2));
                   end;
                end;
          end;
      OP_MUL :
          begin
             sign_extend(list, size,reg1);
             sign_extend(list, size,reg2);
             if current_settings.cputype = cpu_MC68000 then
               begin
                 r:=NR_D0;
                 r2:=NR_D1;
                 cg.getcpuregister(list,NR_D0);
                 cg.getcpuregister(list,NR_D1);
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1, r));
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2, r2));
                 cg.a_call_name(list,'FPC_MUL_LONGWORD',false);
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,r, reg2));
                 cg.ungetcpuregister(list,r);
                 cg.ungetcpuregister(list,r2);
               end
              else
                begin
                 if (isaddressregister(reg1)) then
                  begin
                   hreg1 := cg.getintregister(list,OS_INT);
                   list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg1,hreg1));
                  end
                 else
                   hreg1 := reg1;

                 if (isaddressregister(reg2))  then
                  begin
                   hreg2:= cg.getintregister(list,OS_INT);
                   list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2,hreg2));
                  end
                 else
                   hreg2 := reg2;

                 list.concat(taicpu.op_reg_reg(A_MULU,S_L,reg1,reg2));

                 { move back result into destination register }
                 if reg2<>hreg2 then
                   begin
                      list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hreg2,reg2));
                   end;
                end;
          end;
      OP_NEG,
      OP_NOT :
          Begin
            { if there are two operands, move the register,
              since the operation will only be done on the result
              register.
            }
            if reg1 <> NR_NO then
              cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,reg1,reg2);

            if (isaddressregister(reg2)) then
              begin
                 hreg2 := getintregister(list,OS_INT);
                 list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg2,hreg2));
               end
              else
                hreg2 := reg2;

            { coldfire only supports long version }
            if current_settings.cputype = cpu_ColdFire then
              begin
                sign_extend(list, size,hreg2);
                list.concat(taicpu.op_reg(topcg2tasmop[op],S_L,hreg2));
              end
            else
              begin
                list.concat(taicpu.op_reg(topcg2tasmop[op],TCGSize2OpSize[size],hreg2));
              end;

            if reg2 <> hreg2 then
              begin
                list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hreg2,reg2));
              end;

          end;
    else
        internalerror(20020729);
     end;
  end;



procedure tcg68k.a_cmp_const_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;a : aint;reg : tregister;
        l : tasmlabel);
  var
   hregister : tregister;
  begin
   if a = 0 then
     begin
       list.concat(taicpu.op_reg(A_TST,TCGSize2OpSize[size],reg));
     end
   else
     begin
       if (current_settings.cputype = cpu_ColdFire) then
         begin
           {
             only longword comparison is supported,
             and only on data registers.
           }
           hregister := getintregister(list,OS_INT);
           { always move to a data register }
           list.concat(taicpu.op_reg_reg(A_MOVE,S_L,reg,hregister));
           { sign/zero extend the register }
           sign_extend(list, size,hregister);
           list.concat(taicpu.op_const_reg(A_CMPI,S_L,a,hregister));
         end
       else
         begin
           list.concat(taicpu.op_const_reg(A_CMPI,TCGSize2OpSize[size],a,reg));
         end;
     end;
     { emit the actual jump to the label }
     a_jmp_cond(list,cmp_op,l);
  end;

procedure tcg68k.a_cmp_reg_reg_label(list : TAsmList;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : tasmlabel);
  begin
     list.concat(taicpu.op_reg_reg(A_CMP,tcgsize2opsize[size],reg1,reg2));
     { emit the actual jump to the label }
     a_jmp_cond(list,cmp_op,l);
  end;

procedure tcg68k.a_jmp_always(list : TAsmList;l: tasmlabel);
  var
   ai: taicpu;
  begin
     ai := Taicpu.op_sym(A_JMP,S_NO,l);
     ai.is_jmp := true;
     list.concat(ai);
  end;

procedure tcg68k.a_jmp_flags(list : TAsmList;const f : TResFlags;l: tasmlabel);
   var
     ai : taicpu;
   begin
     ai := Taicpu.op_sym(A_BXX,S_NO,l);
     ai.SetCondition(flags_to_cond(f));
     ai.is_jmp := true;
     list.concat(ai);
   end;

procedure tcg68k.g_flags2reg(list: TAsmList; size: TCgSize; const f: tresflags; reg: TRegister);
   var
     ai : taicpu;
     hreg : tregister;
   begin
      { move to a Dx register? }
      if (isaddressregister(reg)) then
        begin
          hreg := getintregister(list,OS_INT);
          a_load_const_reg(list,size,0,hreg);
          ai:=Taicpu.Op_reg(A_Sxx,S_B,hreg);
          ai.SetCondition(flags_to_cond(f));
          list.concat(ai);

          if (current_settings.cputype = cpu_ColdFire) then
            begin
             { neg.b does not exist on the Coldfire
               so we need to sign extend the value
               before doing a neg.l
             }
             list.concat(taicpu.op_reg(A_EXTB,S_L,hreg));
             list.concat(taicpu.op_reg(A_NEG,S_L,hreg));
            end
          else
            begin
              list.concat(taicpu.op_reg(A_NEG,S_B,hreg));
            end;
         list.concat(taicpu.op_reg_reg(A_MOVE,S_L,hreg,reg));
        end
      else
      begin
        a_load_const_reg(list,size,0,reg);
        ai:=Taicpu.Op_reg(A_Sxx,S_B,reg);
        ai.SetCondition(flags_to_cond(f));
        list.concat(ai);

        if (current_settings.cputype = cpu_ColdFire) then
          begin
             { neg.b does not exist on the Coldfire
               so we need to sign extend the value
               before doing a neg.l
             }
             list.concat(taicpu.op_reg(A_EXTB,S_L,reg));
             list.concat(taicpu.op_reg(A_NEG,S_L,reg));
          end
        else
          begin
           list.concat(taicpu.op_reg(A_NEG,S_B,reg));
          end;
      end;
   end;



procedure tcg68k.g_concatcopy(list : TAsmList;const source,dest : treference;len : aint);

 var
     helpsize : longint;
     i : byte;
     reg8,reg32 : tregister;
     swap : boolean;
     hregister : tregister;
     iregister : tregister;
     jregister : tregister;
     hp1 : treference;
     hp2 : treference;
     hl : tasmlabel;
     hl2: tasmlabel;
     popaddress : boolean;
     srcref,dstref : treference;

  begin
     popaddress := false;

//	 writeln('concatcopy:',len);

     { this should never occur }
     if len > 65535 then
       internalerror(0);

     hregister := getintregister(list,OS_INT);
//         if delsource then
//            reference_release(list,source);

     { from 12 bytes movs is being used }
     if {(not loadref) and} ((len<=8) or (not(cs_opt_size in current_settings.optimizerswitches) and (len<=12))) then
       begin
          srcref := source;
          dstref := dest;
          helpsize:=len div 4;
          { move a dword x times }
          for i:=1 to helpsize do
            begin
               a_load_ref_reg(list,OS_INT,OS_INT,srcref,hregister);
               a_load_reg_ref(list,OS_INT,OS_INT,hregister,dstref);
               inc(srcref.offset,4);
               inc(dstref.offset,4);
               dec(len,4);
            end;
          { move a word }
          if len>1 then
            begin
               a_load_ref_reg(list,OS_16,OS_16,srcref,hregister);
               a_load_reg_ref(list,OS_16,OS_16,hregister,dstref);
               inc(srcref.offset,2);
               inc(dstref.offset,2);
               dec(len,2);
            end;
          { move a single byte }
          if len>0 then
            begin
               a_load_ref_reg(list,OS_8,OS_8,srcref,hregister);
               a_load_reg_ref(list,OS_8,OS_8,hregister,dstref);
            end
       end
     else
       begin
          iregister:=getaddressregister(list);
          jregister:=getaddressregister(list);
          { reference for move (An)+,(An)+ }
          reference_reset(hp1,source.alignment);
          hp1.base := iregister;   { source register }
          hp1.direction := dir_inc;
          reference_reset(hp2,dest.alignment);
          hp2.base := jregister;
          hp2.direction := dir_inc;
          { iregister = source }
          { jregister = destination }

{              if loadref then
             cg.a_load_ref_reg(list,OS_INT,OS_INT,source,iregister)
          else}
             a_loadaddr_ref_reg(list,source,iregister);

          a_loadaddr_ref_reg(list,dest,jregister);

          { double word move only on 68020+ machines }
          { because of possible alignment problems   }
          { use fast loop mode }
          if (current_settings.cputype=cpu_MC68020) then
            begin
               helpsize := len - len mod 4;
               len := len mod 4;
               list.concat(taicpu.op_const_reg(A_MOVE,S_L,helpsize div 4,hregister));
               current_asmdata.getjumplabel(hl2);
               a_jmp_always(list,hl2);
               current_asmdata.getjumplabel(hl);
               a_label(list,hl);
               list.concat(taicpu.op_ref_ref(A_MOVE,S_L,hp1,hp2));
               a_label(list,hl2);
               list.concat(taicpu.op_reg_sym(A_DBRA,S_L,hregister,hl));
               if len > 1 then
                 begin
                    dec(len,2);
                    list.concat(taicpu.op_ref_ref(A_MOVE,S_W,hp1,hp2));
                 end;
               if len = 1 then
                 list.concat(taicpu.op_ref_ref(A_MOVE,S_B,hp1,hp2));
            end
          else
            begin
               { Fast 68010 loop mode with no possible alignment problems }
               helpsize := len;
               list.concat(taicpu.op_const_reg(A_MOVE,S_L,helpsize,hregister));
               current_asmdata.getjumplabel(hl2);
               a_jmp_always(list,hl2);
               current_asmdata.getjumplabel(hl);
               a_label(list,hl);
               list.concat(taicpu.op_ref_ref(A_MOVE,S_B,hp1,hp2));
               a_label(list,hl2);
               list.concat(taicpu.op_reg_sym(A_DBRA,S_L,hregister,hl));
            end;

          { restore the registers that we have just used olny if they are used! }
          if jregister = NR_A1 then
            hp2.base := NR_NO;
          if iregister = NR_A0 then
            hp1.base := NR_NO;
//              reference_release(list,hp1);
//              reference_release(list,hp2);
       end;

//           if delsource then
//               tg.ungetiftemp(list,source);
end;

procedure tcg68k.g_overflowcheck(list: TAsmList; const l:tlocation; def:tdef);
  begin
  end;

procedure tcg68k.g_copyvaluepara_openarray(list : TAsmList;const ref:treference;const lenloc:tlocation;elesize:aint;destreg:tregister);
  begin
  end;


procedure tcg68k.g_proc_entry(list: TAsmList; localsize: longint; nostackframe:boolean);
  var
    r,rsp: TRegister;
    ref  : TReference;
  begin
{$ifdef DEBUG_CHARLIE}
//        writeln('proc entry, localsize:',localsize);
{$endif DEBUG_CHARLIE}

    if not nostackframe then
      begin
	    if localsize<>0 then
	      begin
	        { size can't be negative }
		if (localsize < 0) then
		  internalerror(2006122601);

            { Not to complicate the code generator too much, and since some }
            { of the systems only support this format, the localsize cannot }
            { exceed 32K in size.                                           }
            if (localsize > high(smallint)) then
              CGMessage(cg_e_localsize_too_big);

            list.concat(taicpu.op_reg_const(A_LINK,S_W,NR_FRAME_POINTER_REG,-localsize));
	      end
	    else
	      begin
	        list.concat(taicpu.op_reg_const(A_LINK,S_W,NR_FRAME_POINTER_REG,0));
(*
		{ FIXME! - Carl's original code uses this method. However,
		  according to the 68060 users manual, a LINK is faster than
		  two moves. So, use a link in #0 case too, for now. I'm not
		  really sure tho', that LINK supports #0 disposition, but i
		  see no reason why it shouldn't support it. (KB) }

	        { when localsize = 0, use two moves, instead of link }
		r:=NR_FRAME_POINTER_REG;
		rsp:=NR_STACK_POINTER_REG;

	        reference_reset_base(ref,NR_STACK_POINTER_REG,0);
		ref.direction:=dir_dec;
            list.concat(taicpu.op_reg_ref(A_MOVE,S_L,r,ref));
            list.concat(taicpu.op_reg_reg(A_MOVE,S_L,rsp,r));
		*)
	      end;
      end;
  end;

{    procedure tcg68k.g_restore_frame_pointer(list : TAsmList);
  var
    r:Tregister;
  begin
    r:=NR_FRAME_POINTER_REG;
    list.concat(taicpu.op_reg(A_UNLK,S_NO,r));
  end;
}

procedure tcg68k.g_proc_exit(list : TAsmList; parasize: longint; nostackframe: boolean);
  var
//        r,hregister : TRegister;
    localsize: aint;
    spr : TRegister;
    fpr : TRegister;
    ref : TReference;
  begin
    if not nostackframe then
      begin
        localsize := current_procinfo.calc_stackframe_size;
{$ifdef DEBUG_CHARLIE}
//            writeln('proc exit with stackframe, size:',localsize,' parasize:',parasize);
{$endif DEBUG_CHARLIE}
        list.concat(taicpu.op_reg(A_UNLK,S_NO,NR_FRAME_POINTER_REG));
	    parasize := parasize - target_info.first_parm_offset; { i'm still not 100% confident that this is
	                                                            correct here, but at least it looks less
								    hacky, and makes some sense (KB) }
        if (parasize<>0) then
          begin
            { only 68020+ supports RTD, so this needs another code path
              for 68000 and Coldfire (KB) }
{ TODO: 68020+ only code generation, without fallback}
            list.concat(taicpu.op_const(A_RTD,S_NO,parasize));
          end
        else
          list.concat(taicpu.op_none(A_RTS,S_NO));
      end
    else
      begin
{$ifdef DEBUG_CHARLIE}
//            writeln('proc exit, no stackframe');
{$endif DEBUG_CHARLIE}
        list.concat(taicpu.op_none(A_RTS,S_NO));
      end;

//         writeln('g_proc_exit');
     { Routines with the poclearstack flag set use only a ret.
       also  routines with parasize=0     }
       (*
     if current_procinfo.procdef.proccalloption in clearstack_pocalls then
       begin
         { complex return values are removed from stack in C code PM }
         if paramanager.ret_in_param(current_procinfo.procdef.returndef,current_procinfo.procdef.proccalloption) then
           list.concat(taicpu.op_const(A_RTD,S_NO,4))
         else
           list.concat(taicpu.op_none(A_RTS,S_NO));
       end
     else if (parasize=0) then
       begin
         list.concat(taicpu.op_none(A_RTS,S_NO));
       end
     else
       begin
        { return with immediate size possible here
          signed!
          RTD is not supported on the coldfire     }
        if (current_settings.cputype=cpu_MC68020) and (parasize<$7FFF) then
            list.concat(taicpu.op_const(A_RTD,S_NO,parasize))
        { manually restore the stack }
        else
          begin
            { We must pull the PC Counter from the stack, before  }
            { restoring the stack pointer, otherwise the PC would }
            { point to nowhere!                                   }

            { save the PC counter (pop it from the stack)         }
            hregister:=NR_A3;
            cg.a_reg_alloc(list,hregister);
            reference_reset_base(ref,NR_STACK_POINTER_REG,0);
            ref.direction:=dir_inc;
            list.concat(taicpu.op_ref_reg(A_MOVE,S_L,ref,hregister));
            { can we do a quick addition ... }
            r:=NR_SP;
            if (parasize > 0) and (parasize < 9) then
               list.concat(taicpu.op_const_reg(A_ADDQ,S_L,parasize,r))
            else { nope ... }
               list.concat(taicpu.op_const_reg(A_ADD,S_L,parasize,r));

            { restore the PC counter (push it on the stack)       }
            reference_reset_base(ref,NR_STACK_POINTER_REG,0);
            ref.direction:=dir_dec;
            cg.a_reg_alloc(list,hregister);
            list.concat(taicpu.op_reg_ref(A_MOVE,S_L,hregister,ref));
            list.concat(taicpu.op_none(A_RTS,S_NO));
           end;
       end;
       *)
  end;


procedure Tcg68k.g_save_registers(list:TAsmList);
  var
    tosave : tcpuregisterset;
    ref : treference;
  begin
  {!!!!!
    tosave:=std_saved_registers;
    { only save the registers which are not used and must be saved }
    tosave:=tosave*(rg[R_INTREGISTER].used_in_proc+rg[R_ADDRESSREGISTER].used_in_proc);
    reference_reset_base(ref,NR_STACK_POINTER_REG,0);
    ref.direction:=dir_dec;
    if tosave<>[] then
      list.concat(taicpu.op_regset_ref(A_MOVEM,S_L,tosave,ref));
  }
  end;


procedure Tcg68k.g_restore_registers(list:TAsmList);
  var
    torestore : tcpuregisterset;
    r:Tregister;
    ref : treference;
  begin
  {!!!!!!!!
    torestore:=std_saved_registers;
    { should be intersected with used regs, no ? }
    torestore:=torestore*(rg[R_INTREGISTER].used_in_proc+rg[R_ADDRESSREGISTER].used_in_proc);
    reference_reset_base(ref,NR_STACK_POINTER_REG,0);
    ref.direction:=dir_inc;
    if torestore<>[] then
      list.concat(taicpu.op_ref_regset(A_MOVEM,S_L,ref,torestore));
  }
  end;

{
procedure tcg68k.g_save_all_registers(list : TAsmList);
  begin
  end;

procedure tcg68k.g_restore_all_registers(list : TAsmList;const funcretparaloc:TCGPara);
  begin
  end;
}
procedure tcg68k.sign_extend(list: TAsmList;_oldsize : tcgsize; reg: tregister);
  begin
    case _oldsize of
     { sign extend }
     OS_S8:
          begin
            if (isaddressregister(reg)) then
               internalerror(20020729);
            if (current_settings.cputype = cpu_MC68000) then
              begin
                list.concat(taicpu.op_reg(A_EXT,S_W,reg));
                list.concat(taicpu.op_reg(A_EXT,S_L,reg));
              end
            else
              begin
//    		    list.concat(tai_comment.create(strpnew('sign extend byte')));
                list.concat(taicpu.op_reg(A_EXTB,S_L,reg));
              end;
          end;
     OS_S16:
          begin
            if (isaddressregister(reg)) then
               internalerror(20020729);
//    		list.concat(tai_comment.create(strpnew('sign extend word')));
            list.concat(taicpu.op_reg(A_EXT,S_L,reg));
          end;
     { zero extend }
     OS_8:
          begin
//    		list.concat(tai_comment.create(strpnew('zero extend byte')));
            list.concat(taicpu.op_const_reg(A_AND,S_L,$FF,reg));
          end;
     OS_16:
          begin
//    		list.concat(tai_comment.create(strpnew('zero extend word')));
            list.concat(taicpu.op_const_reg(A_AND,S_L,$FFFF,reg));
          end;
    end; { otherwise the size is already correct }
  end;

 procedure tcg68k.a_jmp_cond(list : TAsmList;cond : TOpCmp;l: tasmlabel);

   var
     ai : taicpu;

   begin
     if cond=OC_None then
       ai := Taicpu.Op_sym(A_JMP,S_NO,l)
     else
       begin
         ai:=Taicpu.Op_sym(A_Bxx,S_NO,l);
         ai.SetCondition(TOpCmp2AsmCond[cond]);
       end;
     ai.is_jmp:=true;
     list.concat(ai);
   end;


procedure tcg68k.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
{
    procedure loadvmttor11;
    var
      href : treference;
    begin
      reference_reset_base(href,NR_R3,0);
      cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,NR_R11);
    end;

    procedure op_onr11methodaddr;
    var
      href : treference;
    begin
      if (procdef.extnumber=$ffff) then
        Internalerror(200006139);
      { call/jmp  vmtoffs(%eax) ; method offs }
      reference_reset_base(href,NR_R11,procdef._class.vmtmethodoffset(procdef.extnumber));
      if not((longint(href.offset) >= low(smallint)) and
             (longint(href.offset) <= high(smallint))) then
        begin
          list.concat(taicpu.op_reg_reg_const(A_ADDIS,NR_R11,NR_R11,
            smallint((href.offset shr 16)+ord(smallint(href.offset and $ffff) < 0))));
          href.offset := smallint(href.offset and $ffff);
        end;
      list.concat(taicpu.op_reg_ref(A_LWZ,NR_R11,href));
      list.concat(taicpu.op_reg(A_MTCTR,NR_R11));
      list.concat(taicpu.op_none(A_BCTR));
    end;
}
  var
    make_global : boolean;
  begin
    if not(procdef.proctypeoption in [potype_function,potype_procedure]) then
      Internalerror(200006137);
    if not assigned(procdef._class) or
       (procdef.procoptions*[po_classmethod, po_staticmethod,
         po_methodpointer, po_interrupt, po_iocheck]<>[]) then
      Internalerror(200006138);
    if procdef.owner.symtabletype<>ObjectSymtable then
      Internalerror(200109191);

    make_global:=false;
    if (not current_module.is_unit) or
       create_smartlink or
       (procdef.owner.defowner.owner.symtabletype=globalsymtable) then
      make_global:=true;

    if make_global then
      List.concat(Tai_symbol.Createname_global(labelname,AT_FUNCTION,0))
    else
      List.concat(Tai_symbol.Createname(labelname,AT_FUNCTION,0));

    { set param1 interface to self  }
//        g_adjust_self_value(list,procdef,ioffset);

    { case 4 }
    if po_virtualmethod in procdef.procoptions then
      begin
//            loadvmttor11;
//            op_onr11methodaddr;
      end
    { case 0 }
    else
//          list.concat(taicpu.op_sym(A_B,current_asmdata.RefAsmSymbol(procdef.mangledname)));

    List.concat(Tai_symbol_end.Createname(labelname));
  end;


{****************************************************************************}
{                               TCG64F68K                                    }
{****************************************************************************}
procedure tcg64f68k.a_op64_reg_reg(list : TAsmList;op:TOpCG;size: tcgsize; regsrc,regdst : tregister64);
var
 hreg1, hreg2 : tregister;
 opcode : tasmop;
begin
//    writeln('a_op64_reg_reg');
  opcode := topcg2tasmop[op];
  case op of
    OP_ADD :
       begin
          { if one of these three registers is an address
            register, we'll really get into problems!
          }
          if isaddressregister(regdst.reglo) or
             isaddressregister(regdst.reghi) or
             isaddressregister(regsrc.reghi) then
               internalerror(20020817);
          list.concat(taicpu.op_reg_reg(A_ADD,S_L,regsrc.reglo,regdst.reglo));
          list.concat(taicpu.op_reg_reg(A_ADDX,S_L,regsrc.reghi,regdst.reghi));
       end;
    OP_AND,OP_OR :
        begin
          { at least one of the registers must be a data register }
          if (isaddressregister(regdst.reglo) and
              isaddressregister(regsrc.reglo)) or
             (isaddressregister(regsrc.reghi) and
              isaddressregister(regdst.reghi))
             then
               internalerror(20020817);
          cg.a_op_reg_reg(list,op,OS_32,regsrc.reglo,regdst.reglo);
          cg.a_op_reg_reg(list,op,OS_32,regsrc.reghi,regdst.reghi);
        end;
    { this is handled in 1st pass for 32-bit cpu's (helper call) }
    OP_IDIV,OP_DIV,
    OP_IMUL,OP_MUL: internalerror(2002081701);
    { this is also handled in 1st pass for 32-bit cpu's (helper call) }
    OP_SAR,OP_SHL,OP_SHR: internalerror(2002081702);
    OP_SUB:
       begin
          { if one of these three registers is an address
            register, we'll really get into problems!
          }
          if isaddressregister(regdst.reglo) or
             isaddressregister(regdst.reghi) or
             isaddressregister(regsrc.reghi) then
               internalerror(20020817);
          list.concat(taicpu.op_reg_reg(A_SUB,S_L,regsrc.reglo,regdst.reglo));
          list.concat(taicpu.op_reg_reg(A_SUBX,S_L,regsrc.reghi,regdst.reghi));
       end;
    OP_XOR:
      begin
          if isaddressregister(regdst.reglo) or
             isaddressregister(regsrc.reglo) or
             isaddressregister(regsrc.reghi) or
             isaddressregister(regdst.reghi) then
               internalerror(20020817);
          list.concat(taicpu.op_reg_reg(A_EOR,S_L,regsrc.reglo,regdst.reglo));
          list.concat(taicpu.op_reg_reg(A_EOR,S_L,regsrc.reghi,regdst.reghi));
      end;
  end; { end case }
end;


procedure tcg64f68k.a_op64_const_reg(list : TAsmList;op:TOpCG;size: tcgsize; value : int64;regdst : tregister64);
var
 lowvalue : cardinal;
 highvalue : cardinal;
 hreg : tregister;
begin
//    writeln('a_op64_const_reg');
  { is it optimized out ? }
//    if cg.optimize64_op_const_reg(list,op,value,reg) then
//       exit;

  lowvalue := cardinal(value);
  highvalue:= value shr 32;

 { the destination registers must be data registers }
 if  isaddressregister(regdst.reglo) or
     isaddressregister(regdst.reghi) then
       internalerror(20020817);
 case op of
    OP_ADD :
       begin
         hreg:=cg.getintregister(list,OS_INT);
         list.concat(taicpu.op_const_reg(A_MOVE,S_L,highvalue,hreg));
         list.concat(taicpu.op_const_reg(A_ADD,S_L,lowvalue,regdst.reglo));
         list.concat(taicpu.op_reg_reg(A_ADDX,S_L,hreg,regdst.reglo));
       end;
    OP_AND :
        begin
          list.concat(taicpu.op_const_reg(A_AND,S_L,lowvalue,regdst.reglo));
          list.concat(taicpu.op_const_reg(A_AND,S_L,highvalue,regdst.reglo));
        end;
    OP_OR :
        begin
          list.concat(taicpu.op_const_reg(A_OR,S_L,lowvalue,regdst.reglo));
          list.concat(taicpu.op_const_reg(A_OR,S_L,highvalue,regdst.reglo));
        end;
    { this is handled in 1st pass for 32-bit cpus (helper call) }
    OP_IDIV,OP_DIV,
    OP_IMUL,OP_MUL: internalerror(2002081701);
    { this is also handled in 1st pass for 32-bit cpus (helper call) }
    OP_SAR,OP_SHL,OP_SHR: internalerror(2002081702);
    OP_SUB:
       begin
         hreg:=cg.getintregister(list,OS_INT);
         list.concat(taicpu.op_const_reg(A_MOVE,S_L,highvalue,hreg));
         list.concat(taicpu.op_const_reg(A_SUB,S_L,lowvalue,regdst.reglo));
         list.concat(taicpu.op_reg_reg(A_SUBX,S_L,hreg,regdst.reglo));
       end;
    OP_XOR:
      begin
          list.concat(taicpu.op_const_reg(A_EOR,S_L,lowvalue,regdst.reglo));
          list.concat(taicpu.op_const_reg(A_EOR,S_L,highvalue,regdst.reglo));
      end;
  end; { end case }
end;
{$ENDIF}

{ tcg68k }

{ TODO : add appropriate code, nothing by default (we don't generate code) }

procedure tcg68k.init_register_allocators;
begin
  //inherited init_register_allocators;
end;

procedure tcg68k.done_register_allocators;
begin
  //inherited done_register_allocators;
end;

procedure tcg68k.a_param_reg(list: TAsmList; size: tcgsize; r: tregister;
  const cgpara: tcgpara);
begin
  //inherited a_param_reg(list, size, r, cgpara);
end;

procedure tcg68k.a_param_const(list: TAsmList; size: tcgsize; a: aint;
  const cgpara: tcgpara);
begin
  //inherited a_param_const(list, size, a, cgpara);
end;

procedure tcg68k.a_param_ref(list: TAsmList; size: tcgsize;
  const r: treference; const cgpara: tcgpara);
begin
  //inherited a_param_ref(list, size, r, cgpara);
end;

procedure tcg68k.a_paramaddr_ref(list: TAsmList; const r: treference;
  const cgpara: tcgpara);
begin
  //inherited a_paramaddr_ref(list, r, cgpara);
end;

procedure tcg68k.a_call_name(list: TAsmList; const s: string; weak: boolean);
begin
  //inherited a_call_name(list, s, weak);
end;

procedure tcg68k.a_call_reg(list: TAsmList; reg: tregister);
begin
  //inherited a_call_reg(list, reg);
end;

procedure tcg68k.a_load_const_reg(list: TAsmList; size: tcgsize; a: aint;
  register: tregister);
begin
  //inherited a_load_const_reg(list, size, a, register);
end;

procedure tcg68k.a_load_const_ref(list: TAsmList; tosize: tcgsize; a: aint;
  const ref: treference);
begin
  //inherited a_load_const_ref(list, tosize, a, ref);
end;

procedure tcg68k.a_load_reg_ref(list: TAsmList; fromsize, tosize: tcgsize;
  register: tregister; const ref: treference);
begin
  //inherited a_load_reg_ref(list, fromsize, tosize, register, ref);
end;

procedure tcg68k.a_load_reg_reg(list: TAsmList; fromsize, tosize: tcgsize;
  reg1, reg2: tregister);
begin
  //inherited a_load_reg_reg(list, fromsize, tosize, reg1, reg2);
end;

procedure tcg68k.a_load_ref_reg(list: TAsmList; fromsize, tosize: tcgsize;
  const ref: treference; register: tregister);
begin
  //inherited a_load_ref_reg(list, fromsize, tosize, ref, register);
end;

procedure tcg68k.a_load_ref_ref(list: TAsmList; fromsize, tosize: tcgsize;
  const sref: treference; const dref: treference);
begin
  //inherited a_load_ref_ref(list, fromsize, tosize, sref, dref);
end;

procedure tcg68k.a_loadaddr_ref_reg(list: TAsmList; const ref: treference;
  r: tregister);
begin
  //inherited a_loadaddr_ref_reg(list, ref, r);
end;

procedure tcg68k.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize;
  reg1, reg2: tregister);
begin
  //inherited a_loadfpu_reg_reg(list, fromsize, tosize, reg1, reg2);
end;

procedure tcg68k.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize;
  const ref: treference; reg: tregister);
begin
  //inherited a_loadfpu_ref_reg(list, fromsize, tosize, ref, reg);
end;

procedure tcg68k.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize;
  reg: tregister; const ref: treference);
begin
  //inherited a_loadfpu_reg_ref(list, fromsize, tosize, reg, ref);
end;

procedure tcg68k.a_loadmm_reg_reg(list: TAsmList; fromsize, tosize: tcgsize;
  reg1, reg2: tregister; shuffle: pmmshuffle);
begin
  //inherited a_loadmm_reg_reg(list, fromsize, tosize, reg1, reg2, shuffle);
end;

procedure tcg68k.a_loadmm_ref_reg(list: TAsmList; fromsize, tosize: tcgsize;
  const ref: treference; reg: tregister; shuffle: pmmshuffle);
begin
  //inherited a_loadmm_ref_reg(list, fromsize, tosize, ref, reg, shuffle);
end;

procedure tcg68k.a_loadmm_reg_ref(list: TAsmList; fromsize, tosize: tcgsize;
  reg: tregister; const ref: treference; shuffle: pmmshuffle);
begin
  //inherited a_loadmm_reg_ref(list, fromsize, tosize, reg, ref, shuffle);
end;

procedure tcg68k.a_parammm_reg(list: TAsmList; size: tcgsize; reg: tregister;
  const locpara: TCGPara; shuffle: pmmshuffle);
begin
  //inherited a_parammm_reg(list, size, reg, locpara, shuffle);
end;

procedure tcg68k.a_op_const_reg(list: TAsmList; Op: TOpCG; size: tcgsize;
  a: aint; reg: TRegister);
begin
  //inherited a_op_const_reg(list, Op, size, a, reg);
end;

procedure tcg68k.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: TCGSize; reg1,
  reg2: TRegister);
begin
  //inherited a_op_reg_reg(list, Op, size, reg1, reg2);
end;

procedure tcg68k.a_cmp_const_reg_label(list: TAsmList; size: tcgsize;
  cmp_op: topcmp; a: aint; reg: tregister; l: tasmlabel);
begin
  //inherited a_cmp_const_reg_label(list, size, cmp_op, a, reg, l);
end;

procedure tcg68k.a_cmp_reg_reg_label(list: TAsmList; size: tcgsize;
  cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel);
begin
  //inherited a_cmp_reg_reg_label(list, size, cmp_op, reg1, reg2, l);
end;

procedure tcg68k.a_jmp_always(list: TAsmList; l: tasmlabel);
begin
  //inherited a_jmp_always(list, l);
end;

procedure tcg68k.a_jmp_flags(list: TAsmList; const f: TResFlags; l: tasmlabel);
begin
  //inherited a_jmp_flags(list, f, l);
end;

procedure tcg68k.g_flags2reg(list: TAsmList; size: TCgSize; const f: tresflags;
  reg: TRegister);
begin
  //inherited g_flags2reg(list, size, f, reg);
end;

procedure tcg68k.g_concatcopy(list: TAsmList; const source, dest: treference;
  len: aint);
begin
  //inherited g_concatcopy(list, source, dest, len);
end;

procedure tcg68k.g_overflowcheck(list: TAsmList; const l: tlocation; def: tdef
  );
begin
  //inherited g_overflowcheck(list, l, def);
end;

procedure tcg68k.g_copyvaluepara_openarray(list: TAsmList;
  const ref: treference; const lenloc: tlocation; elesize: aint;
  destreg: tregister);
begin
  //inherited g_copyvaluepara_openarray(list, ref, lenloc, elesize, destreg);
end;

procedure tcg68k.g_proc_entry(list: TAsmList; localsize: longint;
  nostackframe: boolean);
begin
  //inherited g_proc_entry(list, localsize, nostackframe);
end;

procedure tcg68k.g_proc_exit(list: TAsmList; parasize: longint;
  nostackframe: boolean);
begin
  //inherited g_proc_exit(list, parasize, nostackframe);
end;

procedure tcg68k.g_restore_registers(list: TAsmList);
begin
  //inherited g_restore_registers(list);
end;

procedure tcg68k.g_save_registers(list: TAsmList);
begin
  //inherited g_save_registers(list);
end;

procedure tcg68k.g_intf_wrapper(list: TAsmList; procdef: tprocdef;
  const labelname: string; ioffset: longint);
begin
  //inherited g_intf_wrapper(list, procdef, labelname, ioffset);
end;

function tcg68k.fixref(list: TAsmList; var ref: treference): boolean;
begin

end;

procedure tcg68k.sign_extend(list: TAsmList; _oldsize: tcgsize; reg: tregister
  );
begin

end;

procedure tcg68k.a_jmp_cond(list: TAsmList; cond: TOpCmp; l: tasmlabel);
begin

end;

begin
  cg := tcg68k.create;
  //cg64 :=tcg64f68k.create;
end.
