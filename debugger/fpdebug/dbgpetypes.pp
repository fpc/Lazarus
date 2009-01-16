{ $Id$ }
{
 ---------------------------------------------------------------------------
 dbgpetypes.pp  -  Freepascal debugger - PE types
 ---------------------------------------------------------------------------

 This unit contains the types needed for reading PE images.
 At some time this may go to be part of the rtl ?

 ---------------------------------------------------------------------------

 @created(Thu May 4th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Project                                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit DbgPETypes;
{$mode objfpc}{$H+}{$inline on}
interface

uses
  Windows;

//
// Image Format
//

const
{$ifdef ENDIAN_LITTLE}

  IMAGE_DOS_SIGNATURE                = $5A4D;      // MZ
  IMAGE_OS2_SIGNATURE                = $454E;      // NE
  IMAGE_OS2_SIGNATURE_LE             = $454C;      // LE
  IMAGE_VXD_SIGNATURE                = $454C;      // LE
  IMAGE_NT_SIGNATURE                 = $00004550;  // PE00

{$else}

  IMAGE_DOS_SIGNATURE                = $4D5A;      // MZ
  IMAGE_OS2_SIGNATURE                = $4E45;      // NE
  IMAGE_OS2_SIGNATURE_LE             = $4C45;      // LE
  IMAGE_NT_SIGNATURE                 = $50450000;  // PE00

{$endif}

{$packrecords 2}
{$IFNDEF windows}
type
  _IMAGE_DOS_HEADER = record     // DOS .EXE header
    e_magic:    WORD;            // Magic number
    e_cblp:     WORD;            // Bytes on last page of file
    e_cp:       WORD;            // Pages in file
    e_crlc:     WORD;            // Relocations
    e_cparhdr:  WORD;            // Size of header in paragraphs
    e_minalloc: WORD;            // Minimum extra paragraphs needed
    e_maxalloc: WORD;            // Maximum extra paragraphs needed
    e_ss:       WORD;            // Initial (relative) SS value
    e_sp:       WORD;            // Initial SP value
    e_csum:     WORD;            // Checksum
    e_ip:       WORD;            // Initial IP value
    e_cs:       WORD;            // Initial (relative) CS value
    e_lfarlc:   WORD;            // File address of relocation table
    e_ovno:     WORD;            // Overlay number
    e_res: array[0..3] of WORD;  // Reserved words
    e_oemid:    WORD;            // OEM identifier (for e_oeminfo)
    e_oeminfo:  WORD;            // OEM information: e_oemid specific
    e_res2: array[0..9] of WORD; // Reserved words
    e_lfanew:   LONG;            // File address of new exe header
  end;
  IMAGE_DOS_HEADER = _IMAGE_DOS_HEADER;
  TImageDosHeader = _IMAGE_DOS_HEADER;
  PImageDosHeader = ^TImageDosHeader;
{$ENDIF}

type
  _IMAGE_OS2_HEADER = record // OS/2 .EXE header
    ne_magic:         WORD;  // Magic number
    ne_ver:           CHAR;  // Version number
    ne_rev:           CHAR;  // Revision number
    ne_enttab:        WORD;  // Offset of Entry Table
    ne_cbenttab:      WORD;  // Number of bytes in Entry Table
    ne_crc:           LONG;  // Checksum of whole file
    ne_flags:         WORD;  // Flag word
    ne_autodata:      WORD;  // Automatic data segment number
    ne_heap:          WORD;  // Initial heap allocation
    ne_stack:         WORD;  // Initial stack allocation
    ne_csip:          LONG;  // Initial CS:IP setting
    ne_sssp:          LONG;  // Initial SS:SP setting
    ne_cseg:          WORD;  // Count of file segments
    ne_cmod:          WORD;  // Entries in Module Reference Table
    ne_cbnrestab:     WORD;  // Size of non-resident name table
    ne_segtab:        WORD;  // Offset of Segment Table
    ne_rsrctab:       WORD;  // Offset of Resource Table
    ne_restab:        WORD;  // Offset of resident name table
    ne_modtab:        WORD;  // Offset of Module Reference Table
    ne_imptab:        WORD;  // Offset of Imported Names Table
    ne_nrestab:       LONG;  // Offset of Non-resident Names Table
    ne_cmovent:       WORD;  // Count of movable entries
    ne_align:         WORD;  // Segment alignment shift count
    ne_cres:          WORD;  // Count of resource segments
    ne_exetyp:        BYTE;  // Target Operating system
    ne_flagsothers:   BYTE;  // Other .EXE flags
    ne_pretthunks:    WORD;  // offset to return thunks
    ne_psegrefbytes:  WORD;  // offset to segment ref. bytes
    ne_swaparea:      WORD;  // Minimum code swap area size
    ne_expver:        WORD;  // Expected Windows version number
  end;
  IMAGE_OS2_HEADER = _IMAGE_OS2_HEADER;
  TImageOS2Header = _IMAGE_OS2_HEADER;
  PImageOS2Header = ^TImageOS2Header;

type
  _IMAGE_VXD_HEADER = record      // Windows VXD header
    e32_magic:        WORD;          // Magic number
    e32_border:       BYTE;          // The byte ordering for the VXD
    e32_worder:       BYTE;          // The word ordering for the VXD
    e32_level:        DWORD;         // The EXE format level for now = 0
    e32_cpu:          WORD;          // The CPU type
    e32_os:           WORD;          // The OS type
    e32_ver:          DWORD;         // Module version
    e32_mflags:       DWORD;         // Module flags
    e32_mpages:       DWORD;         // Module # pages
    e32_startobj:     DWORD;         // Object # for instruction pointer
    e32_eip:          DWORD;         // Extended instruction pointer
    e32_stackobj:     DWORD;         // Object # for stack pointer
    e32_esp:          DWORD;         // Extended stack pointer
    e32_pagesize:     DWORD;         // VXD page size
    e32_lastpagesize: DWORD;         // Last page size in VXD
    e32_fixupsize:    DWORD;         // Fixup section size
    e32_fixupsum:     DWORD;         // Fixup section checksum
    e32_ldrsize:      DWORD;         // Loader section size
    e32_ldrsum:       DWORD;         // Loader section checksum
    e32_objtab:       DWORD;         // Object table offset
    e32_objcnt:       DWORD;         // Number of objects in module
    e32_objmap:       DWORD;         // Object page map offset
    e32_itermap:      DWORD;         // Object iterated data map offset
    e32_rsrctab:      DWORD;         // Offset of Resource Table
    e32_rsrccnt:      DWORD;         // Number of resource entries
    e32_restab:       DWORD;         // Offset of resident name table
    e32_enttab:       DWORD;         // Offset of Entry Table
    e32_dirtab:       DWORD;         // Offset of Module Directive Table
    e32_dircnt:       DWORD;         // Number of module directives
    e32_fpagetab:     DWORD;         // Offset of Fixup Page Table
    e32_frectab:      DWORD;         // Offset of Fixup Record Table
    e32_impmod:       DWORD;         // Offset of Import Module Name Table
    e32_impmodcnt:    DWORD;         // Number of entries in Import Module Name Table
    e32_impproc:      DWORD;         // Offset of Import Procedure Name Table
    e32_pagesum:      DWORD;         // Offset of Per-Page Checksum Table
    e32_datapage:     DWORD;         // Offset of Enumerated Data Pages
    e32_preload:      DWORD;         // Number of preload pages
    e32_nrestab:      DWORD;         // Offset of Non-resident Names Table
    e32_cbnrestab:    DWORD;         // Size of Non-resident Name Table
    e32_nressum:      DWORD;         // Non-resident Name Table Checksum
    e32_autodata:     DWORD;         // Object # for automatic data object
    e32_debuginfo:    DWORD;         // Offset of the debugging information
    e32_debuglen:     DWORD;         // The length of the debugging info. in bytes
    e32_instpreload:  DWORD;         // Number of instance pages in preload section of VXD file
    e32_instdemand:   DWORD;         // Number of instance pages in demand load section of VXD file
    e32_heapsize:     DWORD;         // Size of heap - for 16-bit apps
    e32_res3: array[0..11] of BYTE;  // Reserved words
    e32_winresoff:    DWORD;  
    e32_winreslen:    DWORD;  
    e32_devid:        WORD;          // Device ID for VxD
    e32_ddkver:       WORD;          // DDK version for VxD
  end;
  IMAGE_VXD_HEADER = _IMAGE_VXD_HEADER;
  TImageVXDHeader = _IMAGE_VXD_HEADER;
  PImageVXDHeader = ^TImageVXDHeader;

{$packrecords 4}

//
// File header format.
//

type
  _IMAGE_FILE_HEADER = record
    Machine:              WORD; 
    NumberOfSections:     WORD; 
    TimeDateStamp:        DWORD;
    PointerToSymbolTable: DWORD;
    NumberOfSymbols:      DWORD;
    SizeOfOptionalHeader: WORD; 
    Characteristics:      WORD; 
  end;
  IMAGE_FILE_HEADER = _IMAGE_FILE_HEADER;
  TImageFileHeader = _IMAGE_FILE_HEADER;
  PImageFileHeader = ^TImageFileHeader;

const
  IMAGE_SIZEOF_FILE_HEADER            = 20;


  IMAGE_FILE_RELOCS_STRIPPED          = $0001;  // Relocation info stripped from file.
  IMAGE_FILE_EXECUTABLE_IMAGE         = $0002;  // File is executable  (i.e. no unresolved externel references).
  IMAGE_FILE_LINE_NUMS_STRIPPED       = $0004;  // Line nunbers stripped from file.
  IMAGE_FILE_LOCAL_SYMS_STRIPPED      = $0008;  // Local symbols stripped from file.
  IMAGE_FILE_AGGRESIVE_WS_TRIM        = $0010;  // Agressively trim working set
  IMAGE_FILE_LARGE_ADDRESS_AWARE      = $0020;  // App can handle >2gb addresses
  IMAGE_FILE_BYTES_REVERSED_LO        = $0080;  // Bytes of machine word are reversed.
  IMAGE_FILE_32BIT_MACHINE            = $0100;  // 32 bit word machine.
  IMAGE_FILE_DEBUG_STRIPPED           = $0200;  // Debugging info stripped from file in .DBG file
  IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP  = $0400;  // If Image is on removable media, copy and run from the swap file.
  IMAGE_FILE_NET_RUN_FROM_SWAP        = $0800;  // If Image is on Net, copy and run from the swap file.
  IMAGE_FILE_SYSTEM                   = $1000;  // System File.
  IMAGE_FILE_DLL                      = $2000;  // File is a DLL.
  IMAGE_FILE_UP_SYSTEM_ONLY           = $4000;  // File should only be run on a UP machine
  IMAGE_FILE_BYTES_REVERSED_HI        = $8000;  // Bytes of machine word are reversed.

  IMAGE_FILE_MACHINE_UNKNOWN          = 0;
  IMAGE_FILE_MACHINE_I386             = $014c;  // Intel 386.
  IMAGE_FILE_MACHINE_R3000            = $0162;  // MIPS little-endian, $160 big-endian
  IMAGE_FILE_MACHINE_R4000            = $0166;  // MIPS little-endian
  IMAGE_FILE_MACHINE_R10000           = $0168;  // MIPS little-endian
  IMAGE_FILE_MACHINE_WCEMIPSV2        = $0169;  // MIPS little-endian WCE v2
  IMAGE_FILE_MACHINE_ALPHA            = $0184;  // Alpha_AXP
  IMAGE_FILE_MACHINE_SH3              = $01a2;  // SH3 little-endian
  IMAGE_FILE_MACHINE_SH3DSP           = $01a3;
  IMAGE_FILE_MACHINE_SH3E             = $01a4;  // SH3E little-endian
  IMAGE_FILE_MACHINE_SH4              = $01a6;  // SH4 little-endian
  IMAGE_FILE_MACHINE_SH5              = $01a8;  // SH5
  IMAGE_FILE_MACHINE_ARM              = $01c0;  // ARM Little-Endian
  IMAGE_FILE_MACHINE_THUMB            = $01c2;
  IMAGE_FILE_MACHINE_AM33             = $01d3;
  IMAGE_FILE_MACHINE_POWERPC          = $01F0;  // IBM PowerPC Little-Endian
  IMAGE_FILE_MACHINE_POWERPCFP        = $01f1;
  IMAGE_FILE_MACHINE_IA64             = $0200;  // Intel 64
  IMAGE_FILE_MACHINE_MIPS16           = $0266;  // MIPS
  IMAGE_FILE_MACHINE_ALPHA64          = $0284;  // ALPHA64
  IMAGE_FILE_MACHINE_MIPSFPU          = $0366;  // MIPS
  IMAGE_FILE_MACHINE_MIPSFPU16        = $0466;  // MIPS
  IMAGE_FILE_MACHINE_AXP64            = IMAGE_FILE_MACHINE_ALPHA64;
  IMAGE_FILE_MACHINE_TRICORE          = $0520;  // Infineon
  IMAGE_FILE_MACHINE_CEF              = $0CEF;
  IMAGE_FILE_MACHINE_EBC              = $0EBC;  // EFI Byte Code
  IMAGE_FILE_MACHINE_AMD64            = $8664;  // AMD64 (K8)
  IMAGE_FILE_MACHINE_M32R             = $9041;  // M32R little-endian
  IMAGE_FILE_MACHINE_CEE              = $C0EE;

//
// Directory format.
//

type
  _IMAGE_DATA_DIRECTORY = record
    VirtualAddress: DWORD;
    Size:           DWORD;
  end;
  IMAGE_DATA_DIRECTORY = _IMAGE_DATA_DIRECTORY;
  TImageDataDirectory = _IMAGE_DATA_DIRECTORY;
  PImageDataDirectory = ^TImageDataDirectory;
  

const
  IMAGE_NUMBEROF_DIRECTORY_ENTRIES    = 16;

//
// Optional header format.
//

type
  _IMAGE_OPTIONAL_HEADER = record
    //
    // Standard fields.
    //

    Magic:                       WORD; 
    MajorLinkerVersion:          BYTE; 
    MinorLinkerVersion:          BYTE; 
    SizeOfCode:                  DWORD;
    SizeOfInitializedData:       DWORD;
    SizeOfUninitializedData:     DWORD;
    AddressOfEntryPoint:         DWORD;
    BaseOfCode:                  DWORD;
    BaseOfData:                  DWORD;

    //
    // NT additional fields.
    //

    ImageBase:                   DWORD;
    SectionAlignment:            DWORD;
    FileAlignment:               DWORD;
    MajorOperatingSystemVersion: WORD; 
    MinorOperatingSystemVersion: WORD; 
    MajorImageVersion:           WORD; 
    MinorImageVersion:           WORD; 
    MajorSubsystemVersion:       WORD; 
    MinorSubsystemVersion:       WORD; 
    Win32VersionValue:           DWORD;
    SizeOfImage:                 DWORD;
    SizeOfHeaders:               DWORD;
    CheckSum:                    DWORD;
    Subsystem:                   WORD; 
    DllCharacteristics:          WORD; 
    SizeOfStackReserve:          DWORD;
    SizeOfStackCommit:           DWORD;
    SizeOfHeapReserve:           DWORD;
    SizeOfHeapCommit:            DWORD;
    LoaderFlags:                 DWORD;
    NumberOfRvaAndSizes:         DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;
  IMAGE_OPTIONAL_HEADER32 = _IMAGE_OPTIONAL_HEADER;
  TImageOptionalHeader32 = _IMAGE_OPTIONAL_HEADER;
  PImageOptionalHeader32 = ^TImageOptionalHeader32;

type
  _IMAGE_ROM_OPTIONAL_HEADER = record
    Magic:                   WORD; 
    MajorLinkerVersion:      BYTE; 
    MinorLinkerVersion:      BYTE; 
    SizeOfCode:              DWORD;
    SizeOfInitializedData:   DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint:     DWORD;
    BaseOfCode:              DWORD;
    BaseOfData:              DWORD;
    BaseOfBss:               DWORD;
    GprMask:                 DWORD;
    CprMask: array[0..3] of  DWORD;
    GpValue:                 DWORD;
  end;
  IMAGE_ROM_OPTIONAL_HEADER = _IMAGE_ROM_OPTIONAL_HEADER;
  TImageRomOptionalHeader = _IMAGE_ROM_OPTIONAL_HEADER;
  PImageRomOptionalHeader = ^TImageRomOptionalHeader;

type
  _IMAGE_OPTIONAL_HEADER64 = record
    Magic:                       WORD;       
    MajorLinkerVersion:          BYTE;       
    MinorLinkerVersion:          BYTE;       
    SizeOfCode:                  DWORD;      
    SizeOfInitializedData:       DWORD;      
    SizeOfUninitializedData:     DWORD;      
    AddressOfEntryPoint:         DWORD;      
    BaseOfCode:                  DWORD;      
    ImageBase:                   ULONGLONG;  
    SectionAlignment:            DWORD;      
    FileAlignment:               DWORD;      
    MajorOperatingSystemVersion: WORD;       
    MinorOperatingSystemVersion: WORD;       
    MajorImageVersion:           WORD;       
    MinorImageVersion:           WORD;       
    MajorSubsystemVersion:       WORD;       
    MinorSubsystemVersion:       WORD;       
    Win32VersionValue:           DWORD;      
    SizeOfImage:                 DWORD;      
    SizeOfHeaders:               DWORD;      
    CheckSum:                    DWORD;      
    Subsystem:                   WORD;       
    DllCharacteristics:          WORD;       
    SizeOfStackReserve:          ULONGLONG;  
    SizeOfStackCommit:           ULONGLONG;  
    SizeOfHeapReserve:           ULONGLONG;  
    SizeOfHeapCommit:            ULONGLONG;  
    LoaderFlags:                 DWORD;      
    NumberOfRvaAndSizes:         DWORD;      
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;
  IMAGE_OPTIONAL_HEADER64 = _IMAGE_OPTIONAL_HEADER64;
  TImageOptionalHeader64 = _IMAGE_OPTIONAL_HEADER64;
  PImageOptionalHeader64 = ^TImageOptionalHeader64;

const
  IMAGE_SIZEOF_ROM_OPTIONAL_HEADER  =   56;
  IMAGE_SIZEOF_STD_OPTIONAL_HEADER  =   28;
  IMAGE_SIZEOF_NT_OPTIONAL32_HEADER =  224;
  IMAGE_SIZEOF_NT_OPTIONAL64_HEADER =  240;

  IMAGE_NT_OPTIONAL_HDR32_MAGIC     = $10b;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC     = $20b;
  IMAGE_ROM_OPTIONAL_HDR_MAGIC      = $107;

{$ifdef WIN64}
type
  IMAGE_OPTIONAL_HEADER             = IMAGE_OPTIONAL_HEADER64;
  TImageOptionalHeader              = TImageOptionalHeader64;
  PImagePptionalHeader              = PImageOptionalHeader64;
const
  IMAGE_SIZEOF_NT_OPTIONAL_HEADER   = IMAGE_SIZEOF_NT_OPTIONAL64_HEADER;
  IMAGE_NT_OPTIONAL_HDR_MAGIC       = IMAGE_NT_OPTIONAL_HDR64_MAGIC;
{$else}                              
type
  IMAGE_OPTIONAL_HEADER             = IMAGE_OPTIONAL_HEADER32;
  TImageOptionalHeader              = TImageOptionalHeader32;
  PImagePptionalHeader              = PImageOptionalHeader32;
const
  IMAGE_SIZEOF_NT_OPTIONAL_HEADER   = IMAGE_SIZEOF_NT_OPTIONAL32_HEADER;
  IMAGE_NT_OPTIONAL_HDR_MAGIC       = IMAGE_NT_OPTIONAL_HDR32_MAGIC;
{$endif}

type
  _IMAGE_NT_HEADERS64 = record
    Signature: DWORD;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader64;
  end;
  IMAGE_NT_HEADERS64 = _IMAGE_NT_HEADERS64;
  TImageNtHeaders64 = _IMAGE_NT_HEADERS64;
  PImageNtHeaders64 = ^TImageNtHeaders64;
  
  _IMAGE_NT_HEADERS32 = record
    Signature: DWORD;
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageOptionalHeader32;
  end;
  IMAGE_NT_HEADERS32 = _IMAGE_NT_HEADERS32;
  TImageNtHeaders32 = _IMAGE_NT_HEADERS32;
  PImageNtHeaders32 = ^TImageNtHeaders32;

  _IMAGE_ROM_HEADERS = record
    FileHeader: TImageFileHeader;
    OptionalHeader: TImageRomOptionalHeader;
  end;
  IMAGE_ROM_HEADERS = _IMAGE_ROM_HEADERS;
  TImageRomHeaders = _IMAGE_ROM_HEADERS;
  PImageRomHeaders = ^TImageRomHeaders;

{$ifdef WIN64}
  IMAGE_NT_HEADERS             = IMAGE_NT_HEADERS64;
  TImageNtHeaders             = TImageNtHeaders64;
  PImageNtHeaders             = PImageNtHeaders64;
{$else}
  IMAGE_NT_HEADERS             = IMAGE_NT_HEADERS32;
  TImageNtHeaders             = TImageNtHeaders32;
  PImageNtHeaders             = PImageNtHeaders32;
{$endif}

const
// Subsystem Values
  IMAGE_SUBSYSTEM_UNKNOWN                 = 0;   // Unknown subsystem.
  IMAGE_SUBSYSTEM_NATIVE                  = 1;   // Image doesn't require a subsystem.
  IMAGE_SUBSYSTEM_WINDOWS_GUI             = 2;   // Image runs in the Windows GUI subsystem.
  IMAGE_SUBSYSTEM_WINDOWS_CUI             = 3;   // Image runs in the Windows character subsystem.
  IMAGE_SUBSYSTEM_OS2_CUI                 = 5;   // image runs in the OS/2 character subsystem.
  IMAGE_SUBSYSTEM_POSIX_CUI               = 7;   // image runs in the Posix character subsystem.
  IMAGE_SUBSYSTEM_NATIVE_WINDOWS          = 8;   // image is a native Win9x driver.
  IMAGE_SUBSYSTEM_WINDOWS_CE_GUI          = 9;   // Image runs in the Windows CE subsystem.
  IMAGE_SUBSYSTEM_EFI_APPLICATION         = 10;  //
  IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER = 11;  //
  IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER      = 12;  //
  IMAGE_SUBSYSTEM_EFI_ROM                 = 13;
  IMAGE_SUBSYSTEM_XBOX                    = 14;

// DllCharacteristics Entries

  IMAGE_LIBRARY_PROCESS_INIT            = $0001;     // Reserved.
  IMAGE_LIBRARY_PROCESS_TERM            = $0002;     // Reserved.
  IMAGE_LIBRARY_THREAD_INIT             = $0004;     // Reserved.
  IMAGE_LIBRARY_THREAD_TERM             = $0008;     // Reserved.
  IMAGE_DLLCHARACTERISTICS_NO_ISOLATION = $0200;    // Image understands isolation and doesn't want it
  IMAGE_DLLCHARACTERISTICS_NO_SEH       = $0400;     // Image does not use SEH.  No SE handler may reside in this image
  IMAGE_DLLCHARACTERISTICS_NO_BIND      = $0800;     // Do not bind this image.
//                                          = $1000;     // Reserved.
  IMAGE_DLLCHARACTERISTICS_WDM_DRIVER   = $2000;     // Driver uses WDM model
//                                          = $4000;     // Reserved.
  IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE    = $8000;

// Directory Entries

  IMAGE_DIRECTORY_ENTRY_EXPORT         = 0;   // Export Directory
  IMAGE_DIRECTORY_ENTRY_IMPORT         = 1;   // Import Directory
  IMAGE_DIRECTORY_ENTRY_RESOURCE       = 2;   // Resource Directory
  IMAGE_DIRECTORY_ENTRY_EXCEPTION      = 3;   // Exception Directory
  IMAGE_DIRECTORY_ENTRY_SECURITY       = 4;   // Security Directory
  IMAGE_DIRECTORY_ENTRY_BASERELOC      = 5;   // Base Relocation Table
  IMAGE_DIRECTORY_ENTRY_DEBUG          = 6;   // Debug Directory
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT      = 7;   // (X86 usage)
  IMAGE_DIRECTORY_ENTRY_ARCHITECTURE   = 7;   // Architecture Specific Data
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR      = 8;   // RVA of GP
  IMAGE_DIRECTORY_ENTRY_TLS            = 9;   // TLS Directory
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG    = 10;  // Load Configuration Directory
  IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT   = 11;  // Bound Import Directory in headers
  IMAGE_DIRECTORY_ENTRY_IAT            = 12;  // Import Address Table
  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   = 13;  // Delay Load Import Descriptors
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR = 14;  // COM Runtime descriptor

//
// Non-COFF Object file header
//

type
  ANON_OBJECT_HEADER = record
    Sig1:          WORD;     // Must be IMAGE_FILE_MACHINE_UNKNOWN
    Sig2:          WORD;     // Must be $ffff;
    Version:       WORD;     // >= 1 (implies the CLSID field is present)
    Machine:       WORD;   
    TimeDateStamp: DWORD;  
    ClassID:       CLSID;    // Used to invoke CoCreateInstance
    SizeOfData:    DWORD;    // Size of data that follows the header
  end;
  TAnonObjectHeader = ANON_OBJECT_HEADER;
  PAnonObjectHeader = ^TAnonObjectHeader;

//
// Section header format.
//

const
  IMAGE_SIZEOF_SHORT_NAME             = 8;

type
  TISHMisc = record
    case Boolean of
      False: (PhysicalAddress: DWORD);
      True:  (VirtualSize:     DWORD);
  end;

  _IMAGE_SECTION_HEADER = record
    Name: array[0..IMAGE_SIZEOF_SHORT_NAME-1] of BYTE;
    Misc:                 TISHMisc;
    VirtualAddress:       DWORD;  
    SizeOfRawData:        DWORD;  
    PointerToRawData:     DWORD;  
    PointerToRelocations: DWORD;  
    PointerToLinenumbers: DWORD;  
    NumberOfRelocations:  WORD;   
    NumberOfLinenumbers:  WORD;   
    Characteristics:      DWORD;  
  end;
  IMAGE_SECTION_HEADER = _IMAGE_SECTION_HEADER;
  TImageSectionHeader = _IMAGE_SECTION_HEADER;
  PImageSectionHeader = ^TImageSectionHeader;

const
  IMAGE_SIZEOF_SECTION_HEADER         = 40;

//
// Section characteristics.
//
  IMAGE_SCN_TYPE_REG                  = $00000000;  // Reserved.
  IMAGE_SCN_TYPE_DSECT                = $00000001;  // Reserved.
  IMAGE_SCN_TYPE_NOLOAD               = $00000002;  // Reserved.
  IMAGE_SCN_TYPE_GROUP                = $00000004;  // Reserved.
  IMAGE_SCN_TYPE_NO_PAD               = $00000008;  // Reserved.
  IMAGE_SCN_TYPE_COPY                 = $00000010;  // Reserved.

  IMAGE_SCN_CNT_CODE                  = $00000020;  // Section contains code.
  IMAGE_SCN_CNT_INITIALIZED_DATA      = $00000040;  // Section contains initialized data.
  IMAGE_SCN_CNT_UNINITIALIZED_DATA    = $00000080;  // Section contains uninitialized data.

  IMAGE_SCN_LNK_OTHER                 = $00000100;  // Reserved.
  IMAGE_SCN_LNK_INFO                  = $00000200;  // Section contains comments or some other type of information.
  IMAGE_SCN_TYPE_OVER                 = $00000400;  // Reserved.
  IMAGE_SCN_LNK_REMOVE                = $00000800;  // Section contents will not become part of image.
  IMAGE_SCN_LNK_COMDAT                = $00001000;  // Section contents comdat.
//                                          = $00002000;  // Reserved.
  IMAGE_SCN_MEM_PROTECTED             = $00004000;  // Obsolete
  IMAGE_SCN_NO_DEFER_SPEC_EXC         = $00004000;  // Reset speculative exceptions handling bits in the TLB entries for this section.
  IMAGE_SCN_GPREL                     = $00008000;  // Section content can be accessed relative to GP
  IMAGE_SCN_MEM_FARDATA               = $00008000;
  IMAGE_SCN_MEM_SYSHEAP               = $00010000;  // Obsolete
  IMAGE_SCN_MEM_PURGEABLE             = $00020000;
  IMAGE_SCN_MEM_16BIT                 = $00020000;
  IMAGE_SCN_MEM_LOCKED                = $00040000;
  IMAGE_SCN_MEM_PRELOAD               = $00080000;

  IMAGE_SCN_ALIGN_1BYTES              = $00100000;  //
  IMAGE_SCN_ALIGN_2BYTES              = $00200000;  //
  IMAGE_SCN_ALIGN_4BYTES              = $00300000;  //
  IMAGE_SCN_ALIGN_8BYTES              = $00400000;  //
  IMAGE_SCN_ALIGN_16BYTES             = $00500000;  // Default alignment if no others are specified.
  IMAGE_SCN_ALIGN_32BYTES             = $00600000;  //
  IMAGE_SCN_ALIGN_64BYTES             = $00700000;  //
  IMAGE_SCN_ALIGN_128BYTES            = $00800000;  //
  IMAGE_SCN_ALIGN_256BYTES            = $00900000;  //
  IMAGE_SCN_ALIGN_512BYTES            = $00A00000;  //
  IMAGE_SCN_ALIGN_1024BYTES           = $00B00000;  //
  IMAGE_SCN_ALIGN_2048BYTES           = $00C00000;  //
  IMAGE_SCN_ALIGN_4096BYTES           = $00D00000;  //
  IMAGE_SCN_ALIGN_8192BYTES           = $00E00000;  //
// Unused                                   = $00F00000;
  IMAGE_SCN_ALIGN_MASK                = $00F00000;

  IMAGE_SCN_LNK_NRELOC_OVFL           = $01000000;  // Section contains extended relocations.
  IMAGE_SCN_MEM_DISCARDABLE           = $02000000;  // Section can be discarded.
  IMAGE_SCN_MEM_NOT_CACHED            = $04000000;  // Section is not cachable.
  IMAGE_SCN_MEM_NOT_PAGED             = $08000000;  // Section is not pageable.
  IMAGE_SCN_MEM_SHARED                = $10000000;  // Section is shareable.
  IMAGE_SCN_MEM_EXECUTE               = $20000000;  // Section is executable.
  IMAGE_SCN_MEM_READ                  = $40000000;  // Section is readable.
  IMAGE_SCN_MEM_WRITE                 = $80000000;  // Section is writeable.
  
  //
  // TLS Chaacteristic Flags
  //
  IMAGE_SCN_SCALE_INDEX               = $00000001;  // Tls index is scaled

{$packrecords 2}

  //
  // Symbol format.
  //
type
  TISName = record
    case Byte of
      0: (ShortName: array[0..7] of Char);
      1: (Name: record
            Short: DWORD;
            Long:  DWORD;
          end);
      2: (LongName: array[0..1] of DWORD)    ;
  end;

  _IMAGE_SYMBOL = record
      N:                  TISName;
      Value:              DWORD;
      SectionNumber:      SHORT;
      _Type:              WORD;
      StorageClass:       BYTE;
      NumberOfAuxSymbols: BYTE;
  end;
  IMAGE_SYMBOL = _IMAGE_SYMBOL;
  TImageSymbol = _IMAGE_SYMBOL;
  PImageSymbol = ^TImageSymbol;

const
  IMAGE_SIZEOF_SYMBOL                 = 18;

  //
  // Section values.
  //
  // Symbols have a section number of the section in which they are
  // defined. Otherwise, section numbers have the following meanings:
  //

  IMAGE_SYM_UNDEFINED           = SHORT(0);          // Symbol is undefined or is common.
  IMAGE_SYM_ABSOLUTE            = SHORT(-1);         // Symbol is an absolute value.
  IMAGE_SYM_DEBUG               = SHORT(-2);         // Symbol is a special debug item.
  IMAGE_SYM_SECTION_MAX         = $FEFF;             // Values = $FF00-= $FFFF are special

  //
  // Type (fundamental) values.
  //

  IMAGE_SYM_TYPE_NULL                 = $0000;  // no type.
  IMAGE_SYM_TYPE_VOID                 = $0001;  //
  IMAGE_SYM_TYPE_CHAR                 = $0002;  // type character.
  IMAGE_SYM_TYPE_SHORT                = $0003;  // type short integer.
  IMAGE_SYM_TYPE_INT                  = $0004;  //
  IMAGE_SYM_TYPE_LONG                 = $0005;  //
  IMAGE_SYM_TYPE_FLOAT                = $0006;  //
  IMAGE_SYM_TYPE_DOUBLE               = $0007;  //
  IMAGE_SYM_TYPE_STRUCT               = $0008;  //
  IMAGE_SYM_TYPE_UNION                = $0009;  //
  IMAGE_SYM_TYPE_ENUM                 = $000A;  // enumeration.
  IMAGE_SYM_TYPE_MOE                  = $000B;  // member of enumeration.
  IMAGE_SYM_TYPE_BYTE                 = $000C;  //
  IMAGE_SYM_TYPE_WORD                 = $000D;  //
  IMAGE_SYM_TYPE_UINT                 = $000E;  //
  IMAGE_SYM_TYPE_DWORD                = $000F;  //
  IMAGE_SYM_TYPE_PCODE                = $8000;  //
  //
  // Type (derived) values.
  //

  IMAGE_SYM_DTYPE_NULL                = 0;       // no derived type.
  IMAGE_SYM_DTYPE_POINTER             = 1;       // pointer.
  IMAGE_SYM_DTYPE_FUNCTION            = 2;       // function.
  IMAGE_SYM_DTYPE_ARRAY               = 3;       // array.

  //
  // Storage classes.
  //
  IMAGE_SYM_CLASS_END_OF_FUNCTION     = High(BYTE);
  IMAGE_SYM_CLASS_NULL                = $0000;
  IMAGE_SYM_CLASS_AUTOMATIC           = $0001;
  IMAGE_SYM_CLASS_EXTERNAL            = $0002;
  IMAGE_SYM_CLASS_STATIC              = $0003;
  IMAGE_SYM_CLASS_REGISTER            = $0004;
  IMAGE_SYM_CLASS_EXTERNAL_DEF        = $0005;
  IMAGE_SYM_CLASS_LABEL               = $0006;
  IMAGE_SYM_CLASS_UNDEFINED_LABEL     = $0007;
  IMAGE_SYM_CLASS_MEMBER_OF_STRUCT    = $0008;
  IMAGE_SYM_CLASS_ARGUMENT            = $0009;
  IMAGE_SYM_CLASS_STRUCT_TAG          = $000A;
  IMAGE_SYM_CLASS_MEMBER_OF_UNION     = $000B;
  IMAGE_SYM_CLASS_UNION_TAG           = $000C;
  IMAGE_SYM_CLASS_TYPE_DEFINITION     = $000D;
  IMAGE_SYM_CLASS_UNDEFINED_STATIC    = $000E;
  IMAGE_SYM_CLASS_ENUM_TAG            = $000F;
  IMAGE_SYM_CLASS_MEMBER_OF_ENUM      = $0010;
  IMAGE_SYM_CLASS_REGISTER_PARAM      = $0011;
  IMAGE_SYM_CLASS_BIT_FIELD           = $0012;

  IMAGE_SYM_CLASS_FAR_EXTERNAL        = $0044;  //

  IMAGE_SYM_CLASS_BLOCK               = $0064;
  IMAGE_SYM_CLASS_FUNCTION            = $0065;
  IMAGE_SYM_CLASS_END_OF_STRUCT       = $0066;
  IMAGE_SYM_CLASS_FILE                = $0067;
  // new
  IMAGE_SYM_CLASS_SECTION             = $0068;
  IMAGE_SYM_CLASS_WEAK_EXTERNAL       = $0069;

  IMAGE_SYM_CLASS_CLR_TOKEN           = $006B;

  // type packing constants

  N_BTMASK                            = $000F;
  N_TMASK                             = $0030;
  N_TMASK1                            = $00C0;
  N_TMASK2                            = $00F0;
  N_BTSHFT                            = 4;
  N_TSHIFT                            = 2;

  // MACROS

  // Basic Type of  x
  function BTYPE(x: Byte): Byte; inline;

  // Is x a pointer?
  function ISPTR(x: Byte): Boolean; inline;

  // Is x a function?
  function ISFCN(x: Byte): Boolean; inline;

  // Is x an array?
  function ISARY(x: Byte): Boolean; inline;

  // Is x a structure, union, or enumeration TAG?
  function ISTAG(x: Byte): Boolean; inline;

  function INCREF(x: Byte): Byte; inline;

  function DECREF(x: Byte): Byte; inline;

  //
  // Auxiliary entry format.
  //
type
  TIASMisc = record
    case Byte of
      0: (
        Linenumber: WORD;             // declaration line number
        Size:       WORD;             // size of struct, union, or enum
      );
      1: (
        LnSz: record
          Linenumber: WORD;             // declaration line number
          Size:       WORD;             // size of struct, union, or enum
        end;
      );
      2: (TotalSize: DWORD);
  end;

  TIASFcnAry = record
    case Byte of
      0: (
        _Function: record               // if ISFCN, tag, or .bb
          PointerToLinenumber:   DWORD;
          PointerToNextFunction: DWORD;
        end;
      );
      1: (
        _Array: record                  // if ISARY, up to 4 dimen.
          Dimension: array[0..3] of WORD;
        end;
      );
  end;

  _IMAGE_AUX_SYMBOL = record
    case Byte of
      0: (
        Sym: record
          TagIndex: DWORD;                       // struct, union, or enum tag index
          Misc:     TIASMisc;
          FcnAry:   TIASFcnAry;
          TvIndex:  WORD;                        // tv index
        end;
      );
      1: (
        _File: record
          Name: array[0..IMAGE_SIZEOF_SYMBOL-1] of BYTE;
        end;
      );
      2: (
        Section: record
          Length:              DWORD;              // section length
          NumberOfRelocations: WORD;               // number of relocation entries
          NumberOfLinenumbers: WORD;               // number of line numbers
          CheckSum:            DWORD;              // checksum for communal
          Number:              SHORT;              // section number to associate with
          Selection:           BYTE;               // communal selection type
        end;
      );
  end;
  IMAGE_AUX_SYMBOL = _IMAGE_AUX_SYMBOL;
  TImageAuxSymbol = _IMAGE_AUX_SYMBOL;
  PImageAuxSymbol = ^TImageAuxSymbol;


const
  IMAGE_SIZEOF_AUX_SYMBOL             = 18;

type
  IMAGE_AUX_SYMBOL_TYPE = (
    IMAGE_AUX_SYMBOL_TYPE_TOKEN_DEF = 1
  );
  TImageAuxSymbolType = IMAGE_AUX_SYMBOL_TYPE;

{$packRecords 2}

type
  IMAGE_AUX_SYMBOL_TOKEN_DEF = record
    bAuxType:         BYTE;           // IMAGE_AUX_SYMBOL_TYPE
    bReserved:        BYTE;           // Must be 0
    SymbolTableIndex: DWORD;
    rgbReserved: array [0..11] of BYTE;           // Must be 0
  end;
  TImageAuxSymbolTokenDef = IMAGE_AUX_SYMBOL_TOKEN_DEF;
  PImageAuxSymbolTokenDef = ^TImageAuxSymbolTokenDef;

{$packrecords 4}

implementation

function BTYPE(x: Byte): Byte; inline;
begin
  Result := x and N_BTMASK;
end;

function ISPTR(x: Byte): Boolean; inline;
begin
  Result := (x and N_TMASK) = (IMAGE_SYM_DTYPE_POINTER shl N_BTSHFT);
end;

function ISFCN(x: Byte): Boolean; inline;
begin
  Result := (x and N_TMASK) = (IMAGE_SYM_DTYPE_FUNCTION shl N_BTSHFT);
end;

function ISARY(x: Byte): Boolean; inline;
begin
  Result := (x and N_TMASK) = (IMAGE_SYM_DTYPE_ARRAY shl N_BTSHFT);
end;

function ISTAG(x: Byte): Boolean; inline;
begin
  Result := (x = IMAGE_SYM_CLASS_STRUCT_TAG) or (x = IMAGE_SYM_CLASS_UNION_TAG) or (x = IMAGE_SYM_CLASS_ENUM_TAG);
end;

function INCREF(x: Byte): Byte; inline;
begin
  Result := ((x and not N_BTMASK) shl N_TSHIFT) or (IMAGE_SYM_DTYPE_POINTER shl N_BTSHFT) or (x and N_BTMASK);
end;

function DECREF(x: Byte): Byte; inline;
begin
  Result := ((x shr N_TSHIFT) and not N_BTMASK) or (x and N_BTMASK);
end;
  
end.
