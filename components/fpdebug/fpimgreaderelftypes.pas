{
 This unit contains the types needed for reading Elf images.

 This file was ported from DUBY. See svn log for details

 ---------------------------------------------------------------------------

  ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit FpImgReaderElfTypes;

{$mode objfpc}{$H+}

interface

type
  Elf32_Addr  = LongWord; // Unsigned program address
  Elf32_Half  = Word;     // Unsigned medium integer  
  Elf32_Off   = LongWord; // Usigned file offset
  Elf32_Sword = Integer;  // Signed large integer
  Elf32_Word  = LongWord; // Usigned large integer
  
  Elf64_Addr   = Qword;    // Unsigned program address 
  Elf64_Off    = QWord;    // Unsigned ﬁle offset 
  Elf64_Half   = Word;     // Unsigned medium integer 
  Elf64_Word   = LongWord; // Unsigned integer 
  Elf64_Sword  = Integer;  // Signed integer 
  Elf64_Xword  = QWord;    // Unsigned long integer 
  Elf64_Sxword = Int64;    // Signed long integer 

const
  EINDENT = 16;  

type 
  elf_ident = packed record
  case byte of
   0: (e_ident: array [0..EINDENT-1] of byte);
   1: (id_mag        : array[0..3] of Char;          
       id_class      : Byte;               
       id_data       : Byte;               
       id_version    : Byte;              
       id_OSABI      : Byte;              
       id_AbiVersion : Byte;              
      );
  end;

const  
  EI_MAG0       = 0; // $7F
  EI_MAG1       = 1; // E
  EI_MAG2       = 2; // L
  EI_MAG3       = 3; // F
  EI_CLASS      = 4;
  EI_DATA       = 5;
  EI_VERSION    = 6;
  EI_OSABI      = 7;
  EI_ABIVERSION = 8;

  ELFMAGIC     = chr($7f)+'ELF';

  //elf class
  ELFCLASSNONE = 0;
  ELFCLASS32   = 1;
  ELFCLASS64   = 2;

  //byte order
  ELFDATANONE  = 0;
  ELFDATA2LSB  = 1;
  ELFDATA2MSB  = 2;
  
  // Operating System and ABI Identifiers, e_ident[EI_OSABI] 
  ELFOSABI_SYSV = 0;  // System V ABI 
  ELFOSABI_HPUX = 1;  // HP-UX operating system
  ELFOSABI_NETBSD = 2;
  ELFOSABI_LINUX = 3;
  ELFOSABI_STANDALONE = 255; // Standalone (embedded) application 
  
  
type  
  //note: it doesn't include Ident block
  Elf32_EHdr = packed record
    e_ident       : elf_ident;    { ELF identification }
    e_type        : Elf32_Half;   { Object file type }
    e_machine     : Elf32_Half;   { Machine type }
    e_version     : Elf32_Word;   { Object file version }
    e_entry       : Elf32_addr;   { Entry point address }
    e_phoff       : Elf32_Off;    { Program header offset }
    e_shoff       : Elf32_Off;    { Section header offset }
    e_flags       : Elf32_Word;   { Processor-specific flags }
    e_ehsize      : Elf32_Half;   { ELF header size }
    e_phentsize   : Elf32_Half;   { Size of program header entry }
    e_phnum       : Elf32_Half;   { Number of program header entries }
    e_shetsize    : Elf32_Half;   { Size of section header entry }
    e_shnum       : Elf32_Half;   { Number of section header entries }
    e_shstrndx    : Elf32_Half;   { Section name string table index }
  end;
  PElf32_EHdr = ^Elf32_EHdr;
  
  Elf64_EHdr = packed record
    e_ident       : elf_ident;    { ELF identification }
    e_type        : Elf64_Half;   { Object file type } 
    e_machine     : Elf64_Half;   { Machine type } 
    e_version     : Elf64_Word;   { Object file version } 
    e_entry       : Elf64_Addr;   { Entry point address } 
    e_phoff       : Elf64_Off;    { Program header offset } 
    e_shoff       : Elf64_Off;    { Section header offset }
    e_flags       : Elf64_Word;   { Processor-specific flags } 
    e_ehsize      : Elf64_Half;   { ELF header size }  
    e_phentsize   : Elf64_Half;   { Size of program header entry } 
    e_phun        : Elf64_Half;   { Number of program header entries } 
    e_shentsize   : Elf64_Half;   { Size of section header entry } 
    e_shnum       : Elf64_Half;   { Number of section header entries } 
    e_shstrndx    : Elf64_Half;   { Section name string table index }
  end;
  PElf64_EHdr = ^Elf64_EHdr;
  
const
  // object file type {Elf32_Hdr.e_type}
  ET_NONE      = 0;     // No ﬁle type
  ET_REL       = 1;     // Relocatable object ﬁle .o
  ET_EXEC      = 2;     // Executable ﬁle 
  ET_DYN       = 3;     // Shared object ﬁle .so
  ET_CORE      = 4;     // Core ﬁle
  ET_LOOS      = $fe00; // os-specific
  ET_HIOS      = $feff;
  ET_LOPROC    = $ff00; // processor-specific
  ET_HIPROC    = $ffff;
  
  // machine type     {Elf32_Hdr.e_machine}
  EM_NONE        = 0;
  EM_SPARC       = 2;
  EM_386         = 3;
  EM_68K         = 4;
  EM_PPC         = 20;
  EM_PPC64       = 21;
  EM_ARM         = 40;
  EM_OLD_ALPHA   = 41;
  EM_IA_64       = 50;
  EM_X86_64      = 62;
  EM_AVR         = 83;
  EM_ALPHA       = $9026; //unofficial, but used by gnu toolchain

  //elf version       {Elf32_Hdr.e_version}
  EV_NONE      = 0;
  EV_CURRENT   = 1;

  SHN_UNDEF     = $0;    // Used to mark an undeﬁned or meaningless section reference
  SHN_LORESERVE = $ff00; {This value speciﬁes the lower bound of the range of reserved indexes.}
  SHN_LOPROC    = $ff00;
  SHN_HIPROC    = $ff1f;
  SHN_ABS       = $fff1; // Indicates that the corresponding reference is an absolute value 
  SHN_COMMON    = $fff2; // Indicates a symbol that has been declared as a common block (Fortran COMMON or C tentative declaration)
  SHN_HIRESERVE = $ffff;

type  
  Elf32_shdr = packed record
    sh_name      : Elf32_Word;
    sh_type      : Elf32_Word;
    sh_flags     : Elf32_Word;
    sh_addr      : Elf32_Addr;
    sh_offset    : Elf32_Off;
    sh_size      : Elf32_Word;
    sh_link      : Elf32_Word;
    sh_info      : Elf32_Word;
    sh_addralign : Elf32_Word;
    sh_entsize   : Elf32_Word;
  end;
  PElf32_shdr = ^Elf32_shdr;
  
  Elf64_Shdr = packed record
    sh_name      : Elf64_Word;   // Section name
    sh_type      : Elf64_Word;   // Section type
    sh_flags     : Elf64_Xword;  // Section attributes
    sh_address   : Elf64_Addr;   // Virtual address in memory
    sh_offset    : Elf64_Off;    // Offset in file
    sh_size      : Elf64_Xword;  // Size of section
    sh_link      : Elf64_Word;   // Link to other section
    sh_info      : Elf64_Word;   // Miscellaneous information
    sh_addralign : Elf64_Xword;  // Address alignment boundary
    sh_entsize   : Elf64_Xword;  // Size of entries, if section has table
  end;
  PElf64_Shdr = ^Elf64_Shdr;
  

const
  //section type
  SHT_NULL     = 0;         // Marks an unused section header 
  SHT_PROGBITS = 1;         // Contains information deﬁned by the program
  SHT_SYMTAB   = 2;         // Contains a linker symbol table
  SHT_STRTAB   = 3;         // Contains a string table
  SHT_RELA     = 4;         // Contains “Rela” type relocation entries
  SHT_HASH     = 5;         // Contains a symbol hash table
  SHT_DYNAMIC  = 6;         // Contains dynamic linking tables
  SHT_NOTE     = 7;         // Contains note information
  SHT_NOBITS   = 8;         // Contains uninitialized space; does not occupy any space in the ﬁle
  SHT_REL      = 9;         // Contains “Rel” type relocation entries 
  SHT_SHLIB    = 10;        // Reserved
  SHT_DYNSYM   = 11;        // Contains a dynamic loader symbol table
  
  SHT_LOOS     = $60000000; // Environment-speciﬁc use
  SHT_HIOS     = $6fffffff;
  SHT_LOPROC   = $70000000; // Processor-speciﬁc use
  SHT_HIPROC   = $7fffffff; 

  //section attribute flags
  SHF_WRITE     = 1;          // Section contains writable data
  SHF_ALLOC     = 2;          // Section is allocated in memory image of program
  SHF_EXECINSTR = 4;          // Section contains executable instructions
  SHF_MASKOS    = $0f000000;  // Environment-speciﬁc use
  SHF_MASKPROC  = $f0000000;  // Processor-speciﬁc use
  

type
  Elf32_Sym = packed record
    st_name    : Elf32_Word;
    st_value   : Elf32_Addr;
    st_size    : Elf32_Word;
    st_info    : byte;
    st_other   : byte;
    st_shndx   : Elf32_Half;
  end;
  PElf32_Sym = ^Elf32_Sym;
  
  Elf64_Sym = packed record
    st_name    : Elf64_Word;   // Symbol name 
    st_info    : Byte;         // Type and Binding attributes
    st_other   : Byte;         // Reserved
    st_shndx   : Elf64_Half;   // Section table index
    st_value   : Elf64_Addr;   // Symbol value
    st_Size    : Elf64_Xword   // Size of object (e.g., common)
  end;
  PElf64_Sym = ^Elf64_Sym;
  
  
const
  //symbol bindings
  STB_LOCAL  = 0;    // Not visible outside the object ﬁle
  STB_GLOBAL = 1;    // Global symbol, visible to all object files
  STB_WEAK   = 2;    // Global scope, but with lower precedence than global symbols
  STB_LOOS   = 10;   // Environment-speciﬁc use
  STB_HIOS   = 12;
  STB_LOPROC = 13;   // Processor-speciﬁc use
  STB_HIPROC = 15;
  
  //symbol types
  STT_NOTYPE         = 0;  // No type speciﬁed (e.g., an absolute symbol)
  STT_OBJECT         = 1;  // Data object 
  STT_FUNC           = 2;  // Function entry point
  STT_SECTION        = 3;  // Symbol is associated with a section
  STT_FILE           = 4;  // Source ﬁle associated with the object ﬁle
  STT_COMMON         = 5;
  STT_TLS            = 6;
  STT_LOOS           = 10; // Environment-speciﬁc use
  STT_HIOS           = 12;
  STT_LOPROC         = 13; // Processor-speciﬁc use
  STT_SPARC_REGISTER = 13;  
  STT_HIPROC         = 15;


  
{
  Relocation

  Relocation is the process of connecting symbolic references with symbolic 
  deﬁnitions. For example, when a program calls a function, the associated call 
  instruction must transfer control to the proper destination address at execution. 
  In other words, relocatable ﬁles must have information that describes how 
  to modify their section contents, thus allowing executable and shared object 
  ﬁles to hold the right information for a process’s program image. 
  Relocation entriesare these data.
}

type
  Elf32_Rel = packed record
    r_offset : Elf32_Addr;
    r_info   : Elf32_Word;
  end;
  
  Elf32_rela = packed record
    r_offset : Elf32_Addr;
    r_info   : Elf32_Word;
    r_addend : Elf32_Sword;
  end;

{ r_offset This member gives the location at which to apply the relocation action. For a relocatable 
           file, the value is the byte offset from the beginning of the section to the storage unit affected 
           by the relocation. For an executable ﬁle or a shared object, the value is the virtual address of 
           the storage unit affected by the relocation. 
  r_info   This member gives both the symbol table index with respect to which the relocation must be 
           made, and the type of relocation to apply. For example, a call instruction’s relocation entry 
           would hold the symbol table index of the function being called. If the index isSTN_UNDEF, 
           the undeﬁned symbol index, the relocation uses 0 as the ‘‘symbol value.’’ Relocation types 
           are processor-speciﬁc. When the text refers to a relocation entry’s relocation type or symbol 
           table index, it means the result of applyingELF32_R_TYPEorELF32_R_SYM, respectively, 
           to the entry’sr_infomember   }      


  Elf64_Rel = packed record
    r_offset : Elf64_Addr;    // Address of reference 
    r_info   : Elf64_Xword;   // Symbol index and type of relocation
  end;
  PElf64_Rel = ^Elf64_Rel;

  Elf64_Rela = packed record
    r_offset : Elf64_Addr;    // Address of reference
    r_info   : Elf64_Xword;   // Symbol index and type of relocation
    r_addend : Elf64_Sxword;  // Constant part of expression
  end;
  PElf64_Rela = ^Elf64_Rela;
           
           
const
  R_386_NONE     = 0;
  R_386_32       = 1;
  R_386_PC32     = 2;
  R_386_GOT32    = 3;
  R_386_PLT32    = 4;
  R_386_COPY     = 5;
  R_386_GLOB_DAT = 6;
  R_386_JMP_SLOT = 7;
  R_386_RELATIVE = 8;
  R_386_GOTOFF   = 9;
  R_386_GOTPC    = 10;

 
function Elf32_R_Sym(i: LongWord): LongWord; inline;
function Elf32_R_Type(i: LongWord): LongWord; inline;
function Elf32_R_Info(s, t: LongWord): LongWord; inline;

function Elf64_R_Sym(i: QWord): QWord; inline;
function Elf64_R_Type(i: QWord): QWord; inline;  
function Elf64_R_Info(s, t: QWord): QWord; inline;  
  
implementation

function Elf32_R_Sym(i: LongWord): LongWord; inline;
begin
  Result := i shr 8;
end;

function Elf32_R_Type(i: LongWord): LongWord; inline;
begin
  Result := i and $FF;
end;

function Elf32_R_Info(s, t: LongWord): LongWord; inline;
begin
  Result := (s shl 8) + (t and $FF);
end;

function Elf64_R_Info(s, t: QWord): QWord; inline; 
begin
  Result := (s shl 32) + (t and $FFFFFFFF);
end;

function Elf64_R_Type(i: QWord): QWord; inline;
begin
  Result := i and $ffffffff;
end;

function Elf64_R_Sym(i: QWord): QWord; inline;
begin
  Result := i shr 32;
end;

end.

