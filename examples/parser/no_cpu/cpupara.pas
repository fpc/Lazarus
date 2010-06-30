{
    Copyright (c) 2002 by Florian Klaempfl

    Generates the argument location information for the parser - from 680x0 by DoDi.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
{ Generates the argument location information for 680x0.
}

unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cpubase,
      aasmdata,
      symconst,symtype,symdef,symsym,
      parabase,paramgr,cgbase;

    type
       { Returns the location for the nr-st 32 Bit int parameter
         if every parameter before is an 32 Bit int parameter as well
         and if the calling conventions for the helper routines of the
         rtl are used.
       }
       tm68kparamanager = class(tparamanager)
       private
          procedure init_values(var curintreg, curfloatreg: tsuperregister; var cur_stack_offset: aword);
          function create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
                                               var curintreg, curfloatreg: tsuperregister; var cur_stack_offset: aword):longint;
          function parseparaloc(p : tparavarsym;const s : string) : boolean;override;
          function parsefuncretloc(p : tabstractprocdef; const s : string) : boolean;override;
       public
          procedure create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
          procedure createtempparaloc(list: TAsmList;calloption : tproccalloption;parasym : tparavarsym;var cgpara:TCGPara);override;
       public //must override these abstract methods
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara : TCGPara);override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          { TODO : strip and implement create_varargs_paraloc_info }
          //create_varargs_paraloc_info - should be overridden!!!
       end;

  implementation

    uses
       verbose,
       globals,
       systems,
       cpuinfo,cgutils,
       defutil;

    procedure tm68kparamanager.getintparaloc(calloption : tproccalloption; nr : longint;var cgpara : TCGPara);
      var
        paraloc : pcgparalocation;
      begin
         if nr<1 then
           internalerror(2002070801);
         cgpara.reset;
         cgpara.size:=OS_INT;
         cgpara.alignment:=std_param_align;
         paraloc:=cgpara.add_location;
         with paraloc^ do
           begin
              { warning : THIS ONLY WORKS WITH INTERNAL ROUTINES,
                WHICH MUST ALWAYS PASS 4-BYTE PARAMETERS!!
              }
              loc:=LOC_REFERENCE;
              reference.index:=NR_STACK_POINTER_REG;
              reference.offset:=target_info.first_parm_offset+nr*4;
           end;
      end;

    function getparaloc(p : tdef) : tcgloc;

      begin
         result:=LOC_REFERENCE;
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         case p.typ of
            orddef:
              result:=LOC_REGISTER;
            floatdef:
              result:=LOC_FPUREGISTER;
            enumdef:
              result:=LOC_REGISTER;
            pointerdef:
              result:=LOC_REGISTER;
            formaldef:
              result:=LOC_REGISTER;
            classrefdef:
              result:=LOC_REGISTER;
            recorddef:
              if (target_info.abi<>abi_powerpc_aix) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            objectdef:
              if is_object(p) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            stringdef:
              if is_shortstring(p) or is_longstring(p) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            procvardef:
              if (po_methodpointer in tprocvardef(p).procoptions) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            filedef:
              result:=LOC_REGISTER;
            arraydef:
              result:=LOC_REFERENCE;
            setdef:
              if is_smallset(p) then
                result:=LOC_REGISTER
              else
                result:=LOC_REFERENCE;
            variantdef:
              result:=LOC_REFERENCE;
            { avoid problems with errornous definitions }
            errordef:
              result:=LOC_REGISTER;
            else
              internalerror(2002071001);
         end;
         }
      end;


{ TODO: copied from ppc cg, needs work}
    function tm68kparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        { var,out always require address }
        if varspez in [vs_var,vs_out] then
          begin
            result:=true;
            exit;
          end;
        case def.typ of
          variantdef,
          formaldef :
            result:=true;
          recorddef:
            result:=true;
          arraydef:
            result:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                             is_open_array(def) or
                             is_array_of_const(def) or
                             is_array_constructor(def);
          objectdef :
            result:=is_object(def);
          setdef :
            result:=not is_smallset(def);
          stringdef :
            result:=tstringdef(def).stringtype in [st_shortstring,st_longstring];
          procvardef :
            result:=po_methodpointer in tprocvardef(def).procoptions;
        end;
      end;

    procedure tm68kparamanager.init_values(var curintreg, curfloatreg: tsuperregister; var cur_stack_offset: aword);
      begin
        cur_stack_offset:=8;
        curintreg:=RS_D0;
        curfloatreg:=RS_FP0;
      end;

    procedure tm68kparamanager.create_funcretloc_info(p: tabstractprocdef; side: tcallercallee);
      var
        retcgsize: tcgsize;
      begin
        { Constructors return self instead of a boolean }
        if (p.proctypeoption=potype_constructor) then
          retcgsize:=OS_ADDR
        else
          retcgsize:=def_cgsize(p.returndef);

        location_reset(p.funcretloc[side],LOC_INVALID,OS_NO);

        { explicit paraloc specified? }
        if po_explicitparaloc in p.procoptions then
         begin
           p.funcretloc[side].loc:=LOC_REGISTER;
           p.funcretloc[side].register:=p.exp_funcretloc;
           p.funcretloc[side].size:=retcgsize;
           exit;
         end;

        { void has no location }
        if is_void(p.returndef) then
          begin
            location_reset(p.funcretloc[side],LOC_VOID,OS_NO);
            exit;
          end;
        { Return is passed as var parameter }
        if ret_in_param(p.returndef,p.proccalloption) then
          begin
            p.funcretloc[side].loc:=LOC_REFERENCE;
            p.funcretloc[side].size:=retcgsize;
            exit;
          end;
        { Return in FPU register? }
        if not(cs_fp_emulation in current_settings.moduleswitches) and (p.returndef.typ=floatdef) then
          begin
            p.funcretloc[side].loc:=LOC_FPUREGISTER;
            p.funcretloc[side].register:=NR_FPU_RESULT_REG;
            p.funcretloc[side].size:=retcgsize;
          end
        else
         { Return in register }
          begin
            if retcgsize in [OS_64,OS_S64] then
             begin
               { low 32bits }
               p.funcretloc[side].loc:=LOC_REGISTER;
               p.funcretloc[side].size:=OS_64;
               if side=callerside then
                 p.funcretloc[side].register64.reglo:=NR_FUNCTION_RESULT64_LOW_REG
               else
                 p.funcretloc[side].register64.reglo:=NR_FUNCTION_RETURN64_LOW_REG;
               { high 32bits }
               if side=calleeside then
                 p.funcretloc[side].register64.reghi:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 p.funcretloc[side].register64.reghi:=NR_FUNCTION_RETURN64_HIGH_REG;
             end
            else
             begin
               p.funcretloc[side].loc:=LOC_REGISTER;
               p.funcretloc[side].size:=retcgsize;
               if side=callerside then
                 p.funcretloc[side].register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(R_INTREGISTER,retcgsize))
               else
                 p.funcretloc[side].register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(R_INTREGISTER,retcgsize));
             end;
          end;
      end;

    function tm68kparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg: tsuperregister;
      begin
        init_values(curintreg,curfloatreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,cur_stack_offset);

        create_funcretloc_info(p,side);
      end;

function tm68kparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
                         var curintreg, curfloatreg: tsuperregister; var cur_stack_offset: aword):longint;
var
  paraloc      : pcgparalocation;
  hp           : tparavarsym;
  paracgsize   : tcgsize;
  paralen      : aint;
  parasize     : longint;
	paradef      : tdef;
        i            : longint;
	loc          : tcgloc;
	nextintreg,
	nextfloatreg : tsuperregister;
	stack_offset : longint;

begin
  result:=0;
	nextintreg:=curintreg;
	nextfloatreg:=curfloatreg;
	stack_offset:=cur_stack_offset;

  parasize:=0;

  for i:=0 to p.paras.count-1 do
    begin
      hp:=tparavarsym(paras[i]);
	    paradef:=hp.vardef;

	    { syscall for AmigaOS can have already a paraloc set }
      if (vo_has_explicit_paraloc in hp.varoptions) then
        begin
          if not(vo_is_syscall_lib in hp.varoptions) then
            internalerror(200506051);
          continue;
        end;
      hp.paraloc[side].reset;

      { currently only support C-style array of const }
      if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
         is_array_of_const(paradef) then
        begin
{$ifdef DEBUG_CHARLIE}
          writeln('loc register');
{$endif DEBUG_CHARLIE}
          paraloc:=hp.paraloc[side].add_location;
          { hack: the paraloc must be valid, but is not actually used }
          paraloc^.loc:=LOC_REGISTER;
		      paraloc^.register:=NR_D0;
          paraloc^.size:=OS_ADDR;
          break;
        end;

      if (hp.varspez in [vs_var,vs_out]) or
         push_addr_param(hp.varspez,paradef,p.proccalloption) or
         is_open_array(paradef) or
         is_array_of_const(paradef) then
        begin
{$ifdef DEBUG_CHARLIE}
          writeln('loc register');
{$endif DEBUG_CHARLIE}
          paradef:=voidpointertype;
          loc:=LOC_REGISTER;
          paracgsize := OS_ADDR;
          paralen := tcgsize2size[OS_ADDR];
        end
      else
        begin
          if not is_special_array(paradef) then
            paralen:=paradef.size
          else
            paralen:=tcgsize2size[def_cgsize(paradef)];

          loc:=getparaloc(paradef);
          paracgsize:=def_cgsize(paradef);
          { for things like formaldef }
          if (paracgsize=OS_NO) then
            begin
              paracgsize:=OS_ADDR;
              paralen := tcgsize2size[OS_ADDR];
            end;
        end;

      hp.paraloc[side].alignment:=std_param_align;
      hp.paraloc[side].size:=paracgsize;
      hp.paraloc[side].intsize:=paralen;

      if (paralen = 0) then
        if (paradef.typ = recorddef) then
          begin
            paraloc:=hp.paraloc[side].add_location;
            paraloc^.loc := LOC_VOID;
          end
        else
          internalerror(200506052);
      { can become < 0 for e.g. 3-byte records }
      while (paralen > 0) do
        begin
          paraloc:=hp.paraloc[side].add_location;
          {
            by default, the m68k doesn't know any register parameters  (FK)
          if (loc = LOC_REGISTER) and
             (nextintreg <= RS_D2) then
            begin
		    //writeln('loc register');
              paraloc^.loc := loc;
              { make sure we don't lose whether or not the type is signed }
              if (paradef.typ <> orddef) then
                paracgsize := int_cgsize(paralen);
              if (paracgsize in [OS_NO,OS_64,OS_S64]) then
                paraloc^.size := OS_INT
              else
                paraloc^.size := paracgsize;
              paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
              inc(nextintreg);
              dec(paralen,tcgsize2size[paraloc^.size]);
            end
          else if (loc = LOC_FPUREGISTER) and
                  (nextfloatreg <= RS_FP2) then
            begin
//		    writeln('loc fpuregister');
              paraloc^.loc:=loc;
              paraloc^.size := paracgsize;
              paraloc^.register:=newreg(R_FPUREGISTER,nextfloatreg,R_SUBWHOLE);
              inc(nextfloatreg);
              dec(paralen,tcgsize2size[paraloc^.size]);
            end
          else { LOC_REFERENCE }
}
            begin
{$ifdef DEBUG_CHARLIE}
		          writeln('loc reference');
{$endif DEBUG_CHARLIE}
              paraloc^.loc:=LOC_REFERENCE;
              paraloc^.size:=int_cgsize(paralen);
              if (side = callerside) then
                paraloc^.reference.index:=NR_STACK_POINTER_REG
              else
                paraloc^.reference.index:=NR_FRAME_POINTER_REG;
              paraloc^.reference.offset:=stack_offset;
              inc(stack_offset,align(paralen,4));
              paralen := 0;
            end;
        end;
    end;
   result:=stack_offset;
//	 writeln('stack offset:',stack_offset);
end;


{

            if push_addr_param(hp.varspez,paradef,p.proccalloption) then
              paracgsize:=OS_ADDR
            else
              begin
                paracgsize:=def_cgsize(paradef);
                if paracgsize=OS_NO then
                  paracgsize:=OS_ADDR;
              end;
            hp.paraloc[side].size:=paracgsize;
            hp.paraloc[side].Alignment:=std_param_align;
            paraloc:=hp.paraloc[side].add_location;
            paraloc^.size:=paracgsize;
            paraloc^.loc:=LOC_REFERENCE;
            if side=callerside then
              paraloc^.reference.index:=NR_STACK_POINTER_REG
            else
              paraloc^.reference.index:=NR_FRAME_POINTER_REG;
            paraloc^.reference.offset:=target_info.first_parm_offset+parasize;
          end;
	create_funcretloc_info(p,side);
        result:=parasize;
      end;
}

    function tm68kparamanager.parsefuncretloc(p : tabstractprocdef; const s : string) : boolean;
      begin
        result:=false;
        case target_info.system of
          system_m68k_amiga:
            begin
              if s='D0' then
                p.exp_funcretloc:=NR_D0
              else if s='D1' then
                p.exp_funcretloc:=NR_D1
              else if s='D2' then
                p.exp_funcretloc:=NR_D2
              else if s='D3' then
                p.exp_funcretloc:=NR_D3
              else if s='D4' then
                p.exp_funcretloc:=NR_D4
              else if s='D5' then
                p.exp_funcretloc:=NR_D5
              else if s='D6' then
                p.exp_funcretloc:=NR_D6
              else if s='D7' then
                p.exp_funcretloc:=NR_D7
              else if s='A0' then
                p.exp_funcretloc:=NR_A0
              else if s='A1' then
                p.exp_funcretloc:=NR_A1
              else if s='A2' then
                p.exp_funcretloc:=NR_A2
              else if s='A3' then
                p.exp_funcretloc:=NR_A3
              else if s='A4' then
                p.exp_funcretloc:=NR_A4
              else if s='A5' then
                p.exp_funcretloc:=NR_A5
              { 'A6' is problematic, since it's the frame pointer in fpc,
                so it should be saved before a call! }
              else if s='A6' then
                p.exp_funcretloc:=NR_A6
              { 'A7' is the stack pointer on 68k, can't be overwritten by API calls }
              else
                p.exp_funcretloc:=NR_NO;

              if p.exp_funcretloc<>NR_NO then result:=true;
            end;
          else
            internalerror(2005121801);
        end;
      end;


    function tm68kparamanager.parseparaloc(p : tparavarsym;const s : string) : boolean;
      var
        paraloc : pcgparalocation;
      begin
        result:=false;
        case target_info.system of
          system_m68k_amiga:
            begin
              p.paraloc[callerside].alignment:=4;
              paraloc:=p.paraloc[callerside].add_location;
              paraloc^.loc:=LOC_REGISTER;
              paraloc^.size:=def_cgsize(p.vardef);
              { pattern is always uppercase'd }
              if s='D0' then
                paraloc^.register:=NR_D0
              else if s='D1' then
                paraloc^.register:=NR_D1
              else if s='D2' then
                paraloc^.register:=NR_D2
              else if s='D3' then
                paraloc^.register:=NR_D3
              else if s='D4' then
                paraloc^.register:=NR_D4
              else if s='D5' then
                paraloc^.register:=NR_D5
              else if s='D6' then
                paraloc^.register:=NR_D6
              else if s='D7' then
                paraloc^.register:=NR_D7
              else if s='A0' then
                paraloc^.register:=NR_A0
              else if s='A1' then
                paraloc^.register:=NR_A1
              else if s='A2' then
                paraloc^.register:=NR_A2
              else if s='A3' then
                paraloc^.register:=NR_A3
              else if s='A4' then
                paraloc^.register:=NR_A4
              else if s='A5' then
                paraloc^.register:=NR_A5
              { 'A6' is problematic, since it's the frame pointer in fpc,
                so it should be saved before a call! }
              else if s='A6' then
                paraloc^.register:=NR_A6
              { 'A7' is the stack pointer on 68k, can't be overwritten by API calls }
              else
                exit;

              { copy to callee side }
              p.paraloc[calleeside].add_location^:=paraloc^;
            end;
          else
            internalerror(200405092);
        end;
        result:=true;
      end;


    procedure tm68kparamanager.createtempparaloc(list: TAsmList;calloption : tproccalloption;parasym : tparavarsym;var cgpara:TCGPara);
      var
        paraloc : pcgparalocation;
      begin
        paraloc:=parasym.paraloc[callerside].location;
        { No need for temps when value is pushed }
        if not(use_fixed_stack) and
           assigned(paraloc) and
           (paraloc^.loc=LOC_REFERENCE) and
           (paraloc^.reference.index=NR_STACK_POINTER_REG) then
          duplicateparaloc(list,calloption,parasym,cgpara)
        else
          inherited createtempparaloc(list,calloption,parasym,cgpara);
      end;


begin
  paramanager:=tm68kparamanager.create;
end.
