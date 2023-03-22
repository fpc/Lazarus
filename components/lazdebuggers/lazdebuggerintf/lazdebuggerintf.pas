{***************************************************************************
 *                                                                         *
 * This unit is distributed under the LGPL version 2                       *
 *                                                                         *
 * Additionally this unit can be used under any newer version (3 or up)    *
 * of the LGPL                                                             *
 *                                                                         *
 * Users are also granted the same "linknig exception" as defined          *
 * for the LCL.                                                            *
 * See the LCL license for details                                         *
 *                                                                         *
 *                                                                         *
 ***************************************************************************
 @author(Martin Friebe)
}
unit LazDebuggerIntf;

{$mode objfpc}{$H+}
{$INTERFACES CORBA} // no ref counting needed

interface

uses
  Classes, SysUtils, Types, LazDebuggerValueConverter, LazDebuggerIntfBaseTypes;

type
  TDBGState = LazDebuggerIntfBaseTypes.TDBGState deprecated 'Use LazDebuggerIntfBaseTypes.TDBGState';

  {$REGION ***** Internal types ***** }

  IInternalDbgMonitorIntfType  = interface end;
  IInternalDbgSupplierIntfType = interface end;

  generic IInternalDbgMonitorIntf<_SUPPLIER_INTF> = interface(IInternalDbgMonitorIntfType)
    procedure RemoveSupplier(ASupplier: _SUPPLIER_INTF);
  end;

  generic IInternalDbgSupplierIntf<_MONITOR_INTF> = interface(IInternalDbgSupplierIntfType)
    procedure SetMonitor(AMonitor: _MONITOR_INTF);
  end;

  {$ENDREGION}

type

  TDebuggerDataState = (ddsUnknown,                    //
                        ddsRequested, ddsEvaluating,   //
                        ddsValid,                      // Got a valid value
                        ddsInvalid,                    // Does not have a value
                        ddsError                       // Error, but got some Value to display (e.g. error msg)
                       );

  IDbgDataRequestIntf = interface;

  TDbgDataRequestEventType = (
    weeCancel
  );
  TDbgDataRequestEventData = record
    case TDbgDataRequestEventType of
      weeCancel: ();
  end;
  TDbgDataRequestEvent = procedure(Sender: IDbgDataRequestIntf; Data: TDbgDataRequestEventData) of object;

  IDbgDataRequestIntf = interface
    procedure AddFreeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeNotification(ANotification: TNotifyEvent);

    procedure AddNotification(AnEventType: TDbgDataRequestEventType; AnEvent: TDbgDataRequestEvent);
    procedure RemoveNotification(AnEventType: TDbgDataRequestEventType; AnEvent: TDbgDataRequestEvent);

    (* Begin/EndUdate
       - shall indicate that the newly set values are now valid. Ready for display.
         (Indicated by EndUpdate)
       - shall protect the object from destruction.
         A debugger backend may access the object during this time, without further checks.
       - shall ensure changes outside the backend, will not affect calls by the
         backend to any method setting/adding/modifing requested data.
         ~ I.e. if the backend adds values to an array or structure, further calls
           by the backend to add more data must be accepted without failure.
         ~ However, further data may be discarded internally, if possible without
           causing later failures (e.g. if the requested data is no longer needed)
   (!) - does NOT affect, if read-only properties/functions can change their value.
         E.g., if the requested value is no longer needed, then "Expression" and
         other "passed in/provided values" may change (reset to default/empty)
     * When used in the IDE (Begin/EndUpdate themself shall only be valid in the main thread),
       shall
       - allow the backend to read "passed in/provided values" from another thread
       - allow the backend to set new values from another thread
         (I.e., if the IDE (or any non-backend code) makes changes, they must
          consider thread safety)
       // Any "frontend" outside the IDE (commandline / dbg-server) doens not
          need to consider thread safety, as long as it knows that this in not
          required by any of the backends it uses.
    *)
    procedure BeginUpdate;
    procedure EndUpdate;
  end;



{%region **********************************************************************
 ******************************************************************************
 **                                                                          **
 **   W A T C H T E S                                                        **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************}

  TWatchDisplayFormat =
    (wdfDefault,
     wdfStructure,
     wdfChar, wdfString,
     wdfDecimal, wdfUnsigned, wdfFloat, wdfHex,
     wdfPointer,
     wdfMemDump, wdfBinary
    );

  TWatcheEvaluateFlag =
    ( defClassAutoCast,     // Find real class of instance, and use, instead of declared class of variable
      defAllowFunctionCall, //
      defFunctionCallRunAllThreads, //
      defExtraDepth,        // Evaluate 1 extra level of sub-elements => i.e., evaluate each nested sub-item
      defSkipValConv,
      // deprecated
      defNoTypeInfo,        // No Typeinfo object will be returned // for structures that means a printed value will be returned
      defSimpleTypeInfo,    // Returns: Kind (skSimple, skClass, ..); TypeName (but does make no attempt to avoid an alias)
      defFullTypeInfo      // Get all typeinfo, resolve all anchestors
//     defRawMemory,         // returns Array of bytes for hex dump
//     defNoValue            // Skip the value, if returning raw mem
    );
  TWatcheEvaluateFlags = set of TWatcheEvaluateFlag;

  TDBGTypeBase = class(TObject)
  end;

  TLzDbgToken = QWord;
  {$If SizeOf(TLzDbgToken) < SizeOf(Pointer)} {$Error 'TLzDbgToken must be able to store pointers'} {$EndIf}


  TLzDbgFloatPrecission = (dfpSingle, dfpDouble, dfpExtended);
//  TLzDbgSetData = bitpacked array [0..255] of boolean;
  TLzDbgStructType      = (dstUnknown, dstRecord, dstObject, dstClass, dstInterface, dstInternal);
  TLzDbgArrayType       = (datUnknown, datDynArray, datStatArray);
  TLzDbgFieldVisibility = (dfvUnknown, dfvPrivate, dfvProtected, dfvPublic, dfvPublished);
  TLzDbgFieldFlag  = (dffClass, dffAbstract, dffVirtual, dffOverwritten, dffConstructor, dffDestructor, dffVariant);
  TLzDbgFieldFlags = set of TLzDbgFieldFlag;

  { IDbgWatchDataIntf:
    - Interface for providing result-data.
    - REQUIREMENTS (for the backend)
      ** INIT with Create...." **
         First call must be to one of the "Create...." methods.
         Other data can only be set/added after that.
      ** INIT exactly ONCE **
         Only one "Create...." method can be called.
         The type can't be changed after that.
         - Except for:
           CreateError may be called even if one of the non-erroc "Create..." had been called before
      ** All ARRAY elements must have the same type **
         - All entries of an array (added with "SetNextArrayData") must have the
           same type (i.e., be initialized using the same "Create...." method)
         - This includes all *nested* types (e.g. pointer deref)
      ** SetPCharShouldBeStringValue
         - Like array elements: The 2nd value must have the same type as the first.
         - Not allowed to be called on nested elements
    - Adding nested data (calling any method returning a new IDbgWatchDataIntf)
      The Frontend may return "nil" to indicate it does not want this particular data.
  }

  IDbgWatchDataIntf = interface
    procedure CreatePrePrinted(AVal: String); // ATypes: TLzDbgWatchDataTypes);
    procedure CreateString(AVal: String);// AnEncoding // "pchar data" // shortstring
    procedure CreateWideString(AVal: WideString);
    procedure CreateCharValue(ACharValue: QWord; AByteSize: Integer = 0);
    procedure CreateNumValue(ANumValue: QWord; ASigned: Boolean; AByteSize: Integer = 0);
    procedure CreatePointerValue(AnAddrValue: TDbgPtr);
    procedure CreateFloatValue(AFloatValue: Extended; APrecission: TLzDbgFloatPrecission);
    procedure CreateBoolValue(AnOrdBoolValue: QWord; AByteSize: Integer = 0);
    procedure CreateEnumValue(ANumValue: QWord; AName: String; AByteSize: Integer = 0; AnIsEnumIdent: Boolean = False);
//    //procedure CreateEnumValue(ANumValue: QWord; const ANames: TStringDynArray; const AOrdValues: TIntegerDynArray);
    procedure CreateSetValue(const ANames: TStringDynArray); //; const AOrdValues: array of Integer);
//    // CreateSetValue: "ASetVal" only has "length(ANames)" entries. Any higher value will be ignored / should be zero
//    procedure CreateSetValue(const ASetVal: TLzDbgSetData; const ANames: TStringDynArray); //; const AOrdValues: array of Integer);
    function CreateVariantValue(AName: String = ''; AVisibility: TLzDbgFieldVisibility = dfvUnknown): IDbgWatchDataIntf;

    //temporary
    function CreateProcedure(AVal: TDBGPtr; AnIsFunction: Boolean; ALoc, ADesc: String): IDbgWatchDataIntf;
    function CreateProcedureRef(AVal: TDBGPtr; AnIsFunction: Boolean; ALoc, ADesc: String): IDbgWatchDataIntf;

    // Returns Intf for setting element-type => for empty array
    function CreateArrayValue(AnArrayType: TLzDbgArrayType;
                              ATotalCount: Integer = 0;
                              ALowIdx: Integer = 0
                             ): IDbgWatchDataIntf;

    procedure CreateStructure(AStructType: TLzDbgStructType;
                              ADataAddress: TDBGPtr = 0
                              //AOwnFieldCount: Integer = 0;    // Fields declared in this structure (no anchestors)
                              //ARecurseFieldCount: Integer = 0 // Fields including anchestors
                             );
    // returns the intf for the converted result
    // Use SetDerefData to get the interface for the NON-converted result
    function CreateValueHandlerResult(AValueHandler: ILazDbgValueConverterIntf): IDbgWatchDataIntf;
    procedure CreateError(AVal: String);

    // For all Values
    (* SetPCharShouldBeStringValue:
       - Current Res should be "PChar-based"
       - Result of this function should be used for "String-based"
    *)
    function  SetPCharShouldBeStringValue: IDbgWatchDataIntf;
    // For all Values (except error)
    procedure SetTypeName(ATypeName: String);

    // For Array
    procedure SetDataAddress(AnAddr: TDbgPtr);

    // For Pointers:
    function  SetDerefData: IDbgWatchDataIntf;

    // For Arrays
    (* - The returned IDbgWatchDataIntf is only valid until the next call of SetNextItemData.
         For nested arrays, this includes calls to any outer arrays SetNextItemData.
       - Type related (ASigned, AByteSize, APrecission, ...) are taken from the
         proto-type or the  first Item only. They are ignored on subsequent items
    *)
    function  SetNextArrayData: IDbgWatchDataIntf;

    // For structures:
    function  SetAnchestor(ATypeName: String): IDbgWatchDataIntf; // Only: object, class, interface
    function  AddField(AFieldName: String;
                       AVisibility: TLzDbgFieldVisibility;
                       AFlags: TLzDbgFieldFlags
//                       AnAnchestor: IDbgWatchDataIntf  // nil => unknown
                      ): IDbgWatchDataIntf;
  end;

  { IDbgWatchValueIntf }

  IDbgWatchValueIntf = interface(IDbgDataRequestIntf)
    function ResData: IDbgWatchDataIntf;

    (* ***** Methods for the front-end to provide the request  ***** *)

    function GetDisplayFormat: TWatchDisplayFormat;
    function GetEvaluateFlags: TWatcheEvaluateFlags;
    function GetDbgValConverter: ILazDbgValueConvertSelectorIntf;
    function GetExpression: String;
    function GetFirstIndexOffs: Int64;
    function GetRepeatCount: Integer;
    function GetStackFrame: Integer;
    function GetThreadId: Integer;
    function GetValidity: TDebuggerDataState;
    procedure SetTypeInfo(AValue: TDBGTypeBase);
    function GetValue: String; // FpGdbmiDebugger
    procedure SetValidity(AValue: TDebuggerDataState);
    procedure SetValue(AValue: String);

    property DisplayFormat: TWatchDisplayFormat read GetDisplayFormat; // deprecated
    property EvaluateFlags: TWatcheEvaluateFlags read GetEvaluateFlags;
    property FirstIndexOffs: Int64 read GetFirstIndexOffs;
    property RepeatCount: Integer read GetRepeatCount;
    property ThreadId: Integer read GetThreadId;
    property StackFrame: Integer read GetStackFrame;
    property Expression: String read GetExpression;

    property Validity: TDebuggerDataState read GetValidity write SetValidity;
    property Value: String read GetValue write SetValue;
    property TypeInfo: TDBGTypeBase {read GetTypeInfo} write SetTypeInfo;
  end;



  IDbgWatchesSupplierIntf = interface;

  IDbgWatchesMonitorIntf  = interface(specialize IInternalDbgMonitorIntf<IDbgWatchesSupplierIntf>)
    procedure InvalidateWatchValues;
    procedure DoStateChange(const AOldState, ANewState: TDBGState); //deprecated;
  end;

  IDbgWatchesSupplierIntf = interface(specialize IInternalDbgSupplierIntf<IDbgWatchesMonitorIntf>)
    procedure RequestData(AWatchValue: IDbgWatchValueIntf);
  end;

{%endregion   ^^^^^  Watches  ^^^^^   }


{%region   ^^^^^  Locals  ^^^^^   }

  IDbgLocalsListIntf = interface(IDbgDataRequestIntf)
    function Add(AName: String): IDbgWatchDataIntf;

    function GetStackFrame: Integer;
    function GetThreadId: Integer;
    procedure SetValidity(AValue: TDebuggerDataState);

    property ThreadId: Integer read GetThreadId;
    property StackFrame: Integer read GetStackFrame;
    property Validity: TDebuggerDataState {read GetValidity} write SetValidity;
  end;


  IDbgLocalsSupplierIntf = interface;

  IDbgLocalsMonitorIntf  = interface(specialize IInternalDbgMonitorIntf<IDbgLocalsSupplierIntf>)
    procedure InvalidateLocalValues;
    procedure DoStateChange(const AOldState, ANewState: TDBGState); //deprecated;
  end;

  IDbgLocalsSupplierIntf = interface(specialize IInternalDbgSupplierIntf<IDbgLocalsMonitorIntf>)
    procedure RequestData(ALocalsList: IDbgLocalsListIntf);
  end;

{%endregion   ^^^^^  Locals  ^^^^^   }


implementation


end.

