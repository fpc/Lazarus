{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017-2020 by the Pas2JS development team.

    JS unit stubs

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Stub.JS;

{$mode objfpc}

interface


type
  JSValue = Variant;

  // We cannot use EConvertError or Exception, this would result in a circular dependency.

  { EJS }

  EJS = class(TObject)
  private
    FMessage: string;
  Public
    property Message : string Read FMessage Write FMessage;
  end;

  TJSObjectPropertyDescriptor = JSValue;
  Float32 = Double;
  Float64 = Double;

  { TJSObject }

  TJSObject = class 
  end;

  TJSObjectDynArray = Array of TJSObject;
  TJSObjectDynArrayArray = Array of TJSObjectDynArray;
  TJSStringDynArray = Array of String;


  { TJSIteratorValue }
  TJSIteratorValue = class (TJSObject)
  end;

  { TJSIterator }
  TJSIterator = class  (TJSObject)
  end;

   { TJSSet }
  TJSSet = class  (TJSObject)
  end;

  TJSMapFunctionCallBack =  function(arg : JSValue): JSValue;
  TJSMapProcCallBack = procedure(value: JSValue; key: JSValue);

  { TJSMap }
  TJSMap = class  (TJSObject)
  end;

  { TJSFunction }

  TJSFunction = class  (TJSObject)
  end;

  { TJSDate - wrapper for JavaScript Date }

  TJSDate = class (TJSObject)
  end;

  TLocaleCompareOptions = record
    localematched : string;
    usage: string;
    sensitivity : string;
    ignorePunctuation : Boolean;
    numeric : boolean;
    caseFirst : string;
  end;

  TJSRegexp = class  (TJSObject)
  end;


  TJSString = class  (TJSObject)
  end;

  TJSArray = Class;
  
  TJSArrayEventProc = procedure(element : JSValue; index: NativeInt; anArray : TJSArray);
  TJSArrayEvent = function (element : JSValue; index: NativeInt; anArray : TJSArray) : Boolean;
  TJSArrayMapEvent = function (element : JSValue; index: NativeInt; anArray : TJSArray) : JSValue;
  TJSArrayReduceEvent = function (accumulator, currentValue : JSValue; currentIndex : NativeInt; anArray : TJSArray) : JSValue;
  TJSArrayCompareEvent = function (a,b : JSValue) : NativeInt;
  TJSArrayCallback = TJSArrayEvent;
  TJSArrayMapCallback = TJSArrayMapEvent;
  TJSArrayReduceCallBack = TJSArrayReduceEvent;
  TJSArrayCompareCallBack = TJSArrayCompareEvent;

  { TJSArray }

  TJSArray = Class (TJSObject)
  end;

  TJSArrayBuffer = Class (TJSObject)
  end;

  TJSBufferSource = class (TJSObject)
  end;

  { TJSTypedArray }
  TJSTypedArray = Class;

  TJSTypedArrayCallBack = function (element : JSValue; index: NativeInt; anArray : TJSTypedArray) : Boolean;
  TJSTypedArrayEvent = function (element : JSValue; index: NativeInt; anArray : TJSTypedArray) : Boolean of object;
  TJSTypedArrayMapCallBack = function (element : JSValue; index: NativeInt; anArray : TJSTypedArray) : JSValue;
  TJSTypedArrayMapEvent = function (element : JSValue; index: NativeInt; anArray : TJSTypedArray) : JSValue of object;
  TJSTypedArrayReduceCallBack = function (accumulator, currentValue : JSValue; currentIndex : NativeInt; anArray : TJSTypedArray) : JSValue;
  TJSTypedArrayCompareCallBack = function (a,b : JSValue) : NativeInt;

  TJSTypedArray = class (TJSObject)
  end;

  { TJSInt8Array }

  TJSInt8Array = class  (TJSObject)
  end;

  TJSUint8Array  = class  (TJSObject)
  end;

  TJSUint8ClampedArray = class (TJSObject)
  end;

  TJSInt16Array = class  (TJSObject)
  end;

  TJSUint16Array = class (TJSObject)
  end;

  TJSInt32Array = class  (TJSObject)
  end;

  TJSUint32Array = class (TJSObject)
  end;

  TJSFloat32Array = class  (TJSObject)
  end;

  TJSFloat64Array = class  (TJSObject)
  end;

  TJSDataView = Class  (TJSObject)
  end;

  TJSJSON = class  (TJSObject)
  end;

  { TJSError }

  TJSError = Class  (TJSObject)
  end;


  TJSPromiseResolver = function (aValue : JSValue) : JSValue;
  TJSPromiseExecutor = procedure (resolve,reject : TJSPromiseResolver);
  TJSPromiseFinallyHandler = procedure;

  TJSPromise = Class (TJSObject);

  TJSPromiseArray = array of TJSPromise;



  TJSFunctionArguments = class (TJSObject)
  end;


  TJSIteratorResult = Class (TJSObject)
  end;

  TJSAsyncIterator = Class(TJSObject)
  end;

  TJSSyntaxError = class  (TJSError);

  TJSTextDecoderOptions = class(TJSObject)
  end;

  TJSTextDecodeOptions = class(TJSObject)
  end;

  TJSTextDecoder = class (TJSObject)
  end;


  TJSTextEncoderEncodeIntoResult = class(TJSObject)
  end;

  TJSTextEncoder = class(TJSObject)
  end;

Type
  TJSValueType = (jvtNull,jvtBoolean,jvtInteger,jvtFloat,jvtString,jvtObject,jvtArray);

implementation

end.
