unit h2pastest;

interface

const
  MinValue = 0;
  MaxValue = 5;

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

// these procedures contain explicit/anoymous types, which are not allowed
// in FreePascal. The TReplaceImplicitTypes tool will replace them with
// named types
procedure DoSomething1(var a: array[0..3] of TMyClass);

procedure DoSomething2(var a: array[MinValue..MaxValue] of char);

procedure DoSomething3(a: array of );
