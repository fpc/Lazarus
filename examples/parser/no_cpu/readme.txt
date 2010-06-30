This directory contains the "no_cpu" project files.
"no_cpu" is intended for an compiler/parser without code generation,
but it can be used as a template for other real CPUs.

The fpcsrc/compilers/ and systems/ have to be added to the project pathes.


Project/compiler options
------------------------
The no_cpu/ directory has to be part of the search pathes. This becomes essential
when the project is moved into the fpcsrc/compiler/ directory.

Since compiler/fpcdefs.inc has a built-in list of known compilers,
we pretend to be a M68K (32 bit) cpu. This $define has to be set in the
compiler options, along with the Dummy define for further switching.

aasmcpu.pas (used by Compiler...)
-----------
This file is a dedicated part of the project (really necessary?).
It includes several other cpu-specific files, some of which register themselves
in dedicated (global) variables.

In the initialization section the following global variables are set:
  cai_align:=tai_align; //dummy class, defined here
  cai_cpu:=taicpu;      //dummy class, defined here

ag68kgas (used by CpuTarg)
--------
Registers the TAssembler class. Can be inlined for the parser.

aoptcpu (used by AOpt)
-------
Defines and registers:
  casmoptimizer:=TCpuAsmOptimizer;

aoptcpub (used by AOptObj)
--------
Assembler OPTimizer CPU specific Base.

aoptcpud (used by AOpt)
--------
Defines TAOptDFACpu = class(TAOptDFA), not used in the parser.

cgcpu (used by Compiler)
-----
Defines and registers:
  type tcg68k = class(tcg)
  cg := tcg68k.create;
The methods of the base class are overridden to do nothing, for the parser.

cpubase.pas (used by Node...)
-----------
This unit is used during code generation.
When CG is disabled, only these declarations are required:
  type tasmop //opcodes
  const firstop, lastop
Otherwise a bunch of types, constants and several functions must be defined.

cpuinfo.pas (used by Globals...)
-----------
This unit must define the types:
  tcputype, tfputype

cpunode (referenced by Compiler, IFnDEF NOPASS2)
-------
Includes some units, that initialize global links:
  n68kadd,
  n68kcal,
  n68kmat,
  n68kcnv

cpupara (used by Compiler...)
-------
Declares and registers:
  paramanager:=tm68kparamanager.create;
Must override some abstract methods:
push_addr_param
getintparaloc
create_paraloc_info
create_varargs_paraloc_info


cpupi (used by Compiler...)
-----
Declares and registers ProcInfo:
  type tm68kprocinfo = class(tcgprocinfo)
  cprocinfo:=tm68kprocinfo;


cputarg (used by Compiler)
-------
Uses some units that initialize global links:
  (ra68kmot - removde)
  ag68kgas


Special nodes, can be omitted (referenced by CpuNode)
=============================
  n68kadd
  n68kcal
  n68kcnv
  n68kmat

ra68k (used by ra68kmot - no more)
-----
Can be removed?
Defines and registers:


ra68kmot (used by CpuTarg - no more)
--------
Motorola assembler (unused, can be removed)
Defines and registers:
  RegisterAsmMode(asmmode_m68k_mot_info);
  RegisterAsmMode(asmmode_m68k_standard_info);


Some files currently are included, can be inlined:
==================================================
m68kreg.dat	//registers
r68kcon.inc
r68knum.inc	//dwarf(?) register numbers
r68ksup.inc	//supervisor registers


Adaptations
===========
Several classes have to be derived from base classes, 
and must be registered in predefined global variables (as type or created object).

assemble
--------
Defines assembler enum, type(s), variable(s) and a list of classes.
CAssembler[tasm] must contain a valid assembler class, derived from TAssembler.
TAsm is the enum of all predefined assemblers.

We can add our dummy assembler as either AS_NONE or as_m68k_mit,
using RegisterAssembler().

cgobj
-----
