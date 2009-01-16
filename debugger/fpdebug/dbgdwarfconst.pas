{ $Id$ }
{
 ---------------------------------------------------------------------------
 dbgdwarfconst.pas  -  Freepascal debugger - Dwarf constants
 ---------------------------------------------------------------------------

 This unit contains the constants defined for the dward debugging format.

 ---------------------------------------------------------------------------

 @created(Fri Jul 7th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Project                                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit DbgDwarfConst;

{$mode objfpc}{$H+}

interface

const

  { tag encodings }
  
  DW_TAG_array_type               = $01;
  DW_TAG_class_type               = $02;
  DW_TAG_entry_point              = $03;
  DW_TAG_enumeration_type         = $04;
  DW_TAG_formal_parameter         = $05;
  DW_TAG_imported_declaration     = $08;
  DW_TAG_label                    = $0a;
  DW_TAG_lexical_block            = $0b;
  DW_TAG_member                   = $0d;
  DW_TAG_pointer_type             = $0f;
  DW_TAG_reference_type           = $10;
  DW_TAG_compile_unit             = $11;
  DW_TAG_string_type              = $12;
  DW_TAG_structure_type           = $13;
  DW_TAG_subroutine_type          = $15;
  DW_TAG_typedef                  = $16;
  DW_TAG_union_type               = $17;
  DW_TAG_unspecified_parameters   = $18;
  DW_TAG_variant                  = $19;
  DW_TAG_common_block             = $1a;
  DW_TAG_common_inclusion         = $1b;
  DW_TAG_inheritance              = $1c;
  DW_TAG_inlined_subroutine       = $1d;
  DW_TAG_module                   = $1e;
  DW_TAG_ptr_to_member_type       = $1f;
  DW_TAG_set_type                 = $20;
  DW_TAG_subrange_type            = $21;
  DW_TAG_with_stmt                = $22;
  DW_TAG_access_declaration       = $23;
  DW_TAG_base_type                = $24;
  DW_TAG_catch_block              = $25;
  DW_TAG_const_type               = $26;
  DW_TAG_constant                 = $27;
  DW_TAG_enumerator               = $28;
  DW_TAG_file_type                = $29;
  DW_TAG_friend                   = $2a;
  DW_TAG_namelist                 = $2b;
  DW_TAG_namelist_item            = $2c;
  DW_TAG_packed_type              = $2d;
  DW_TAG_subprogram               = $2e;
  DW_TAG_template_type_parameter  = $2f;
  DW_TAG_template_value_parameter = $30;
  DW_TAG_thrown_type              = $31;
  DW_TAG_try_block                = $32;
  DW_TAG_variant_part             = $33;
  DW_TAG_variable                 = $34;
  DW_TAG_volatile_type            = $35;
  // --- DWARF3 ---
  DW_TAG_dwarf_procedure          = $36;
  DW_TAG_restrict_type            = $37;
  DW_TAG_interface_type           = $38;
  DW_TAG_namespace                = $39;
  DW_TAG_imported_module          = $3a;
  DW_TAG_unspecified_type         = $3b;
  DW_TAG_partial_unit             = $3c;
  DW_TAG_imported_unit            = $3d;
  DW_TAG_condition                = $3f;
  DW_TAG_shared_type              = $40;
  // ---  ---
  DW_TAG_lo_user                  = $4080;
  DW_TAG_hi_user                  = $ffff;


  { Child determination encodings }
  
  DW_CHILDREN_no  = $00;
  DW_CHILDREN_yes = $01;


  { Attribute encodings }
  
  DW_AT_sibling              = $01  ;    // reference
  DW_AT_location             = $02  ;    // block, loclistptr
  DW_AT_name                 = $03  ;    // string
  DW_AT_ordering             = $09  ;    // constant
  DW_AT_byte_size            = $0b  ;    // block, constant, reference
  DW_AT_bit_offset           = $0c  ;    // block, constant, reference
  DW_AT_bit_size             = $0d  ;    // block, constant, reference
  DW_AT_stmt_list            = $10  ;    // lineptr
  DW_AT_low_pc               = $11  ;    // address
  DW_AT_high_pc              = $12  ;    // address
  DW_AT_language             = $13  ;    // constant
  DW_AT_discr                = $15  ;    // reference
  DW_AT_discr_value          = $16  ;    // constant
  DW_AT_visibility           = $17  ;    // constant
  DW_AT_import               = $18  ;    // reference
  DW_AT_string_length        = $19  ;    // block, loclistptr
  DW_AT_common_reference     = $1a  ;    // reference
  DW_AT_comp_dir             = $1b  ;    // string
  DW_AT_const_value          = $1c  ;    // block, constant, string
  DW_AT_containing_type      = $1d  ;    // reference
  DW_AT_default_value        = $1e  ;    // reference
  DW_AT_inline               = $20  ;    // constant
  DW_AT_is_optional          = $21  ;    // flag
  DW_AT_lower_bound          = $22  ;    // block, constant, reference
  DW_AT_producer             = $25  ;    // string
  DW_AT_prototyped           = $27  ;    // flag
  DW_AT_return_addr          = $2a  ;    // block, loclistptr
  DW_AT_start_scope          = $2c  ;    // constant
  DW_AT_bit_stride           = $2e  ;    // constant
  DW_AT_upper_bound          = $2f  ;    // block, constant, reference
  DW_AT_abstract_origin      = $31  ;    // reference
  DW_AT_accessibility        = $32  ;    // constant
  DW_AT_address_class        = $33  ;    // constant
  DW_AT_artificial           = $34  ;    // flag
  DW_AT_base_types           = $35  ;    // reference
  DW_AT_calling_convention   = $36  ;    // constant
  DW_AT_count                = $37  ;    // block, constant, reference
  DW_AT_data_member_location = $38  ;    // block, constant, loclistptr
  DW_AT_decl_column          = $39  ;    // constant
  DW_AT_decl_file            = $3a  ;    // constant
  DW_AT_decl_line            = $3b  ;    // constant
  DW_AT_declaration          = $3c  ;    // flag
  DW_AT_discr_list           = $3d  ;    // block
  DW_AT_encoding             = $3e  ;    // constant
  DW_AT_external             = $3f  ;    // flag
  DW_AT_frame_base           = $40  ;    // block, loclistptr
  DW_AT_friend               = $41  ;    // reference
  DW_AT_identifier_case      = $42  ;    // constant
  DW_AT_macro_info           = $43  ;    // macptr
  DW_AT_namelist_item        = $44  ;    // block
  DW_AT_priority             = $45  ;    // reference
  DW_AT_segment              = $46  ;    // block, loclistptr
  DW_AT_specification        = $47  ;    // reference
  DW_AT_static_link          = $48  ;    // block, loclistptr
  DW_AT_type                 = $49  ;    // reference
  DW_AT_use_location         = $4a  ;    // block, loclistptr
  DW_AT_variable_parameter   = $4b  ;    // flag
  DW_AT_virtuality           = $4c  ;    // constant
  DW_AT_vtable_elem_location = $4d  ;    // block, loclistptr
  // --- DWARF3 ---
  DW_AT_allocated            = $4e  ;    // block, constant, reference
  DW_AT_associated           = $4f  ;    // block, constant, reference
  DW_AT_data_location        = $50  ;    // block
  DW_AT_byte_stride          = $51  ;    // block, constant, reference
  DW_AT_entry_pc             = $52  ;    // address
  DW_AT_use_UTF8             = $53  ;    // flag
  DW_AT_extension            = $54  ;    // reference
  DW_AT_ranges               = $55  ;    // rangelistptr
  DW_AT_trampoline           = $56  ;    // address, flag, reference, string
  DW_AT_call_column          = $57  ;    // constant
  DW_AT_call_file            = $58  ;    // constant
  DW_AT_call_line            = $59  ;    // constant
  DW_AT_description          = $5a  ;    // string
  DW_AT_binary_scale         = $5b  ;    // constant
  DW_AT_decimal_scale        = $5c  ;    // constant
  DW_AT_small                = $5d  ;    // reference
  DW_AT_decimal_sign         = $5e  ;    // constant
  DW_AT_digit_count          = $5f  ;    // constant
  DW_AT_picture_string       = $60  ;    // string
  DW_AT_mutable              = $61  ;    // flag
  DW_AT_threads_scaled       = $62  ;    // flag
  DW_AT_explicit             = $63  ;    // flag
  DW_AT_object_pointer       = $64  ;    // reference
  DW_AT_endianity            = $65  ;    // constant
  DW_AT_elemental            = $66  ;    // flag
  DW_AT_pure                 = $67  ;    // flag
  DW_AT_recursive            = $68  ;    // flag
  // ---  ---
  DW_AT_lo_user              = $2000;    // ---
  DW_AT_hi_user              = $3fff;    // ---


  { Attribute form encodings }

  DW_FORM_addr      = $01;    // address
  DW_FORM_block2    = $03;    // block
  DW_FORM_block4    = $04;    // block
  DW_FORM_data2     = $05;    // constant
  DW_FORM_data4     = $06;    // constant, lineptr, loclistptr, macptr, rangelistptr
  DW_FORM_data8     = $07;    // constant, lineptr, loclistptr, macptr, rangelistptr
  DW_FORM_string    = $08;    // string
  DW_FORM_block     = $09;    // block
  DW_FORM_block1    = $0a;    // block
  DW_FORM_data1     = $0b;    // constant
  DW_FORM_flag      = $0c;    // flag
  DW_FORM_sdata     = $0d;    // constant
  DW_FORM_strp      = $0e;    // string
  DW_FORM_udata     = $0f;    // constant
  DW_FORM_ref_addr  = $10;    // reference
  DW_FORM_ref1      = $11;    // reference
  DW_FORM_ref2      = $12;    // reference
  DW_FORM_ref4      = $13;    // reference
  DW_FORM_ref8      = $14;    // reference
  DW_FORM_ref_udata = $15;    // reference
  DW_FORM_indirect  = $16;    //


  { DWARF operation encodings }
  
  DW_OP_addr                  = $03;    // 1 constant address (size target specific)
  DW_OP_deref                 = $06;    // 0
  DW_OP_const1u               = $08;    // 1 1-byte constant
  DW_OP_const1s               = $09;    // 1 1-byte constant
  DW_OP_const2u               = $0a;    // 1 2-byte constant
  DW_OP_const2s               = $0b;    // 1 2-byte constant
  DW_OP_const4u               = $0c;    // 1 4-byte constant
  DW_OP_const4s               = $0d;    // 1 4-byte constant
  DW_OP_const8u               = $0e;    // 1 8-byte constant
  DW_OP_const8s               = $0f;    // 1 8-byte constant
  DW_OP_constu                = $10;    // 1 ULEB128 constant
  DW_OP_consts                = $11;    // 1 SLEB128 constant
  DW_OP_dup                   = $12;    // 0
  DW_OP_drop                  = $13;    // 0
  DW_OP_over                  = $14;    // 0
  DW_OP_pick                  = $15;    // 1 1-byte stack index
  DW_OP_swap                  = $16;    // 0
  DW_OP_rot                   = $17;    // 0
  DW_OP_xderef                = $18;    // 0
  DW_OP_abs                   = $19;    // 0
  DW_OP_and                   = $1a;    // 0
  DW_OP_div                   = $1b;    // 0
  DW_OP_minus                 = $1c;    // 0
  DW_OP_mod                   = $1d;    // 0
  DW_OP_mul                   = $1e;    // 0
  DW_OP_neg                   = $1f;    // 0
  DW_OP_not                   = $20;    // 0
  DW_OP_or                    = $21;    // 0
  DW_OP_plus                  = $22;    // 0
  DW_OP_plus_uconst           = $23;    // 1 ULEB128 addend
  DW_OP_shl                   = $24;    // 0
  DW_OP_shr                   = $25;    // 0
  DW_OP_shra                  = $26;    // 0
  DW_OP_xor                   = $27;    // 0
  DW_OP_skip                  = $2f;    // 1 signed 2-byte constant
  DW_OP_bra                   = $28;    // 1 signed 2-byte constant
  DW_OP_eq                    = $29;    // 0
  DW_OP_ge                    = $2a;    // 0
  DW_OP_gt                    = $2b;    // 0
  DW_OP_le                    = $2c;    // 0
  DW_OP_lt                    = $2d;    // 0
  DW_OP_ne                    = $2e;    // 0
  DW_OP_lit0                  = $30;    // 0 literals 0..31 =    (DW_OP_lit0 + literal)
  DW_OP_lit1                  = $31;    // 0
  DW_OP_lit2                  = $32;    // 0
  DW_OP_lit3                  = $33;    // 0
  DW_OP_lit4                  = $34;    // 0
  DW_OP_lit5                  = $35;    // 0
  DW_OP_lit6                  = $36;    // 0
  DW_OP_lit7                  = $37;    // 0
  DW_OP_lit8                  = $38;    // 0
  DW_OP_lit9                  = $39;    // 0
  DW_OP_lit10                 = $3a;    // 0
  DW_OP_lit11                 = $3b;    // 0
  DW_OP_lit12                 = $3c;    // 0
  DW_OP_lit13                 = $3d;    // 0
  DW_OP_lit14                 = $3e;    // 0
  DW_OP_lit15                 = $3f;    // 0
  DW_OP_lit16                 = $40;    // 0
  DW_OP_lit17                 = $41;    // 0
  DW_OP_lit18                 = $42;    // 0
  DW_OP_lit19                 = $43;    // 0
  DW_OP_lit20                 = $44;    // 0
  DW_OP_lit21                 = $45;    // 0
  DW_OP_lit22                 = $46;    // 0
  DW_OP_lit23                 = $47;    // 0
  DW_OP_lit24                 = $48;    // 0
  DW_OP_lit25                 = $49;    // 0
  DW_OP_lit26                 = $4a;    // 0
  DW_OP_lit27                 = $4b;    // 0
  DW_OP_lit28                 = $4c;    // 0
  DW_OP_lit29                 = $4d;    // 0
  DW_OP_lit30                 = $4e;    // 0
  DW_OP_lit31                 = $4f;    // 0
  DW_OP_reg0                  = $50;    // 0 reg 0..31 =    (DW_OP_reg0 + regnum)
  DW_OP_reg1                  = $51;    // 0
  DW_OP_reg2                  = $52;    // 0
  DW_OP_reg3                  = $53;    // 0
  DW_OP_reg4                  = $54;    // 0
  DW_OP_reg5                  = $55;    // 0
  DW_OP_reg6                  = $56;    // 0
  DW_OP_reg7                  = $57;    // 0
  DW_OP_reg8                  = $58;    // 0
  DW_OP_reg9                  = $59;    // 0
  DW_OP_reg10                 = $5a;    // 0
  DW_OP_reg11                 = $5b;    // 0
  DW_OP_reg12                 = $5c;    // 0
  DW_OP_reg13                 = $5d;    // 0
  DW_OP_reg14                 = $5e;    // 0
  DW_OP_reg15                 = $5f;    // 0
  DW_OP_reg16                 = $60;    // 0
  DW_OP_reg17                 = $61;    // 0
  DW_OP_reg18                 = $62;    // 0
  DW_OP_reg19                 = $63;    // 0
  DW_OP_reg20                 = $64;    // 0
  DW_OP_reg21                 = $65;    // 0
  DW_OP_reg22                 = $66;    // 0
  DW_OP_reg23                 = $67;    // 0
  DW_OP_reg24                 = $68;    // 0
  DW_OP_reg25                 = $69;    // 0
  DW_OP_reg26                 = $6a;    // 0
  DW_OP_reg27                 = $6b;    // 0
  DW_OP_reg28                 = $6c;    // 0
  DW_OP_reg29                 = $6d;    // 0
  DW_OP_reg30                 = $6e;    // 0
  DW_OP_reg31                 = $6f;    // 0
  DW_OP_breg0                 = $70;    // 1 SLEB128 offsetbase register 0..31 =     (DW_OP_breg0 + regnum)
  DW_OP_breg1                 = $71;    // 1
  DW_OP_breg2                 = $72;    // 1
  DW_OP_breg3                 = $73;    // 1
  DW_OP_breg4                 = $74;    // 1
  DW_OP_breg5                 = $75;    // 1
  DW_OP_breg6                 = $76;    // 1
  DW_OP_breg7                 = $77;    // 1
  DW_OP_breg8                 = $78;    // 1
  DW_OP_breg9                 = $79;    // 1
  DW_OP_breg10                = $7a;    // 1
  DW_OP_breg11                = $7b;    // 1
  DW_OP_breg12                = $7c;    // 1
  DW_OP_breg13                = $7d;    // 1
  DW_OP_breg14                = $7e;    // 1
  DW_OP_breg15                = $7f;    // 1
  DW_OP_breg16                = $80;    // 1
  DW_OP_breg17                = $81;    // 1
  DW_OP_breg18                = $82;    // 1
  DW_OP_breg19                = $83;    // 1
  DW_OP_breg20                = $84;    // 1
  DW_OP_breg21                = $85;    // 1
  DW_OP_breg22                = $86;    // 1
  DW_OP_breg23                = $87;    // 1
  DW_OP_breg24                = $88;    // 1
  DW_OP_breg25                = $89;    // 1
  DW_OP_breg26                = $8a;    // 1
  DW_OP_breg27                = $8b;    // 1
  DW_OP_breg28                = $8c;    // 1
  DW_OP_breg29                = $8d;    // 1
  DW_OP_breg30                = $8e;    // 1
  DW_OP_breg31                = $8f;    // 1
  DW_OP_regx                  = $90;    // 1 ULEB128 register
  DW_OP_fbreg                 = $91;    // 1 SLEB128 offset
  DW_OP_bregx                 = $92;    // 2 ULEB128 register followed bySLEB128 offset
  DW_OP_piece                 = $93;    // 1 ULEB128 size of piece addressed
  DW_OP_deref_size            = $94;    // 1 1-byte size of data retrieved
  DW_OP_xderef_size           = $95;    // 1 1-byte size of data retrieved
  DW_OP_nop                   = $96;    // 0
  // --- DWARF3 ---
  DW_OP_push_object_address   = $97;    // 0
  DW_OP_call2                 = $98;    // 1 2-byte offset of DIE
  DW_OP_call4                 = $99;    // 1 4-byte offset of DIE
  DW_OP_call_ref              = $9a;    // 1 4- or 8-byte offset of DIE
  DW_OP_form_tls_address      = $9b;    // 0
  DW_OP_call_frame_cfa        = $9c;    // 0
  DW_OP_bit_piece             = $9d;    // 2
  // ---  ---
  DW_OP_lo_user               = $e0;    //
  DW_OP_hi_user               = $ff;    //
  
  
  { Base type encoding values }
  
  DW_ATE_address           = $01;
  DW_ATE_boolean           = $02;
  DW_ATE_complex_float     = $03;
  DW_ATE_float             = $04;
  DW_ATE_signed            = $05;
  DW_ATE_signed_char       = $06;
  DW_ATE_unsigned          = $07;
  DW_ATE_unsigned_char     = $08;
  // --- DWARF3 ---
  DW_ATE_imaginary_float   = $09;
  DW_ATE_packed_decimal    = $0a;
  DW_ATE_numeric_string    = $0b;
  DW_ATE_edited            = $0c;
  DW_ATE_signed_fixed      = $0d;
  DW_ATE_unsigned_fixed    = $0e;
  DW_ATE_decimal_float     = $0f;
  // ---  ---
  DW_ATE_lo_user           = $80;
  DW_ATE_hi_user           = $ff;
  

  { Decimal sign encodings }
  
  // --- DWARF3 ---
  DW_DS_unsigned           = $01;
  DW_DS_leading_overpunch  = $02;
  DW_DS_trailing_overpunch = $03;
  DW_DS_leading_separate   = $04;
  DW_DS_trailing_separate  = $05;
  // ---  ---


  { Endianity encodings }

  // --- DWARF3 ---
  DW_END_default  = $00;
  DW_END_big      = $01;
  DW_END_little   = $02;
  DW_END_lo_user  = $40;
  DW_END_hi_user  = $ff;
  // ---  ---


  { Accessibility encodings }
  
  DW_ACCESS_public    = $01;
  DW_ACCESS_protected = $02;
  DW_ACCESS_private   = $03;


  { Visibility encodings }
  
  DW_VIS_local     = $01;
  DW_VIS_exported  = $02;
  DW_VIS_qualified = $03;
  
  
  { Virtuality encodings }
  
  DW_VIRTUALITY_none         = $00;
  DW_VIRTUALITY_virtual      = $01;
  DW_VIRTUALITY_pure_virtual = $02;


  { Language names }

  DW_LANG_C89              = $0001;
  DW_LANG_C                = $0002;
  DW_LANG_Ada83            = $0003; // reserved
  DW_LANG_C_plus_plus      = $0004;
  DW_LANG_Cobol74          = $0005; // reserved
  DW_LANG_Cobol85          = $0006; // reserved
  DW_LANG_Fortran77        = $0007;
  DW_LANG_Fortran90        = $0008;
  DW_LANG_Pascal83         = $0009;
  DW_LANG_Modula2          = $000a;
  // --- DWARF3 ---
  DW_LANG_Java             = $000b;
  DW_LANG_C99              = $000c;
  DW_LANG_Ada95            = $000d; // reserved
  DW_LANG_Fortran95        = $000e;
  DW_LANG_PLI              = $000f; // reserved
  DW_LANG_ObjC             = $0010;
  DW_LANG_ObjC_plus_plus   = $0011;
  DW_LANG_UPC              = $0012;
  DW_LANG_D                = $0013;
  // ---  ---
  DW_LANG_lo_user          = $8000;
  DW_LANG_hi_user          = $ffff;
  

  { Address class encoding }
  
  DW_ADDR_none = $00;
  
  
  { Identifier case encodings }
  
  DW_ID_case_sensitive   = $00;
  DW_ID_up_case          = $01;
  DW_ID_down_case        = $02;
  DW_ID_case_insensitive = $03;


  { Calling convention encodings }
  
  DW_CC_normal  = $01;
  DW_CC_program = $02;
  DW_CC_nocall  = $03;
  DW_CC_lo_user = $40;
  DW_CC_hi_user = $ff;


  { Inline encodings }
  
  DW_INL_not_inlined          = $00;
  DW_INL_inlined              = $01;
  DW_INL_declared_not_inlined = $02;
  DW_INL_declared_inlined     = $03;


  { Ordering encodings }
  
  DW_ORD_row_major = $00;
  DW_ORD_col_major = $01;
  
  
  { Discriminant descriptor encodings }
  
  DW_DSC_label = $00;
  DW_DSC_range = $01;


  { Line Number Standard Opcode Encodings }
  DW_LNS_extended_opcode      = $00;
  
  DW_LNS_copy                 = $01;
  DW_LNS_advance_pc           = $02;
  DW_LNS_advance_line         = $03;
  DW_LNS_set_file             = $04;
  DW_LNS_set_column           = $05;
  DW_LNS_negate_stmt          = $06;
  DW_LNS_set_basic_block      = $07;
  DW_LNS_const_add_pc         = $08;
  DW_LNS_fixed_advance_pc     = $09;
  // --- DWARF3 ---
  DW_LNS_set_prologue_end     = $0a;
  DW_LNS_set_epilogue_begin   = $0b;
  DW_LNS_set_isa              = $0c;
  // ---  ---


  { Line Number Extended Opcode Encodings }
  
  DW_LNE_end_sequence = $01;
  DW_LNE_set_address  = $02;
  DW_LNE_define_file  = $03;
  // --- DWARF3 ---
  DW_LNE_lo_user      = $80;
  DW_LNE_hi_user      = $ff;
  // ---  ---

  
  { Macinfo Type Encodings }
  
  DW_MACINFO_define     = $01;
  DW_MACINFO_undef      = $02;
  DW_MACINFO_start_file = $03;
  DW_MACINFO_end_file   = $04;
  DW_MACINFO_vendor_ext = $ff;


  { Call frame instruction encodings }

  // Special codes, operand is encoded in bit 5..0
  DW_CFA_advance_loc          = $40;    // delta
  DW_CFA_offset               = $80;    // register  ULEB128 offset
  DW_CFA_restore              = $C0;    // register
  //--
  DW_CFA_nop                  = $00;
  DW_CFA_set_loc              = $01;    // address
  DW_CFA_advance_loc1         = $02;    // 1-byte delta
  DW_CFA_advance_loc2         = $03;    // 2-byte delta
  DW_CFA_advance_loc4         = $04;    // 4-byte delta
  DW_CFA_offset_extended      = $05;    // ULEB128 register, ULEB128 offset
  DW_CFA_restore_extended     = $06;    // ULEB128 register
  DW_CFA_undefined            = $07;    // ULEB128 register
  DW_CFA_same_value           = $08;    // ULEB128 register
  DW_CFA_register             = $09;    // ULEB128 register, ULEB128 register
  DW_CFA_remember_state       = $0a;
  DW_CFA_restore_state        = $0b;
  DW_CFA_def_cfa              = $0c;    // ULEB128 register, ULEB128 offset
  DW_CFA_def_cfa_register     = $0d;    // ULEB128 register
  DW_CFA_def_cfa_offset       = $0e;    // ULEB128 offset
  // --- DWARF3 ---
  DW_CFA_def_cfa_expression   = $0f;    // BLOCK
  DW_CFA_expression           = $10;    // ULEB128 register, BLOCK
  DW_CFA_offset_extended_sf   = $11;    // ULEB128 register, SLEB128 offset
  DW_CFA_def_cfa_sf           = $12;    // ULEB128 register, SLEB128 offset
  DW_CFA_def_cfa_offset_sf    = $13;    // SLEB128 offset
  DW_CFA_val_offset           = $14;    // ULEB128         , ULEB128
  DW_CFA_val_offset_sf        = $15;    // ULEB128         , SLEB128
  DW_CFA_val_expression       = $16;    // ULEB128         , BLOCK
  // ---  ---
  DW_CFA_lo_user              = $1c;
  DW_CFA_hi_user              = $3f;
  

implementation

end.

