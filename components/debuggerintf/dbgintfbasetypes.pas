unit DbgIntfBaseTypes;
(*                 DebuggerTypes

  Basic types for any Pascal debugger. (not just IDE)

*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  (* TDBGPtr
     datatype pointing to data on the target
  *)
  TDBGPtr = type QWord;
  PDBGPtr = ^TDBGPtr;
  TDBGPtrArray = Array of TDBGPtr;

  (* TDbgSymbolKind
     Enum of types that a value can have.
  *)

  TDbgSymbolKind = (
    skNone,          // undefined type
    //skType         // Not a value, but a type description
    //skUser,          // userdefined type, this sym refers to another sym defined elswhere
    //--------------------------------------------------------------------------
    skInstance,      // the main exe/dll, containing all other syms
    skUnit,          // contains syms defined in this unit
    skProcedure,     // an actual procedure, with an address // NOT just the type of a procedure
    skFunction,
    //--------------------------------------------------------------------------
    //----------------- Basic types, these cannot have references or children
    skSimple,        // Any of the below (in this group), the dbg does not know more detailed
    skPointer,
    skInteger,
    skCardinal,
    skBoolean,
    skChar,
    skFloat,
    skString,
    skAnsiString,
    skCurrency,
    skVariant,
    skWideString,
    //--------------------------------------------------------------------------
    skEnum,       // Variable holding an enum / enum type
    skEnumValue,  // a single element from an enum
    skSet,
    //--------------------------------------------------------------------------
    skRecord,        // the address member is the relative location within the
    skObject,        // structure: type TFoo=object end; // may also be reported as record
    skClass,
    skInterface,
    //--------------------------------------------------------------------------
    skArray,
    //--------------------------------------------------------------------------
    skRegister,       // the Address member is the register number
    //--------------------------------------------------------------------------
    skAddress       // untyped data at an address (differs from pointer, when being typecasted)
  );


implementation

end.

