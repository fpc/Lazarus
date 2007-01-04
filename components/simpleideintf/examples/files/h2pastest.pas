
type
  TMyClass = class;

  TMyEnums = (enum1, enum2);

  TMySet = set of TMyEnums;

  TMyClass = class(TObject)
    procedure DoSomething(var a: array[0..3] of char);
  end;
  
  TMyRecord = record
    i: integer;
    case b: boolean of
    true: (AsInt: integer);
    false: (AsWord: word);
  end;

// this procedure contains an explicit/anoymous type, which is not allowed
// in FreePascal. The TReplaceImplicitTypes tool will replace it with a
// named type
procedure DoSomething(var a: array[0..3] of TMyClass);


