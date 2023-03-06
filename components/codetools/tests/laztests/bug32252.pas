program bug32252;
{$MODE DELPHI}
{$MACRO ON}
{$DEFINE CUSTOM_DICTIONARY_CONSTRAINTS := TKey, TValue, THashFactory}
{$DEFINE OPEN_ADDRESSING_CONSTRAINTS := TKey, TValue, THashFactory, TProbeSequence}
type
  // The following types are copied and simplified from Generics.Collections.
  // They are enough to generate the cycle error.
  TDummy = class
  end;
  TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS> = class abstract
    procedure Foo; virtual; abstract;
  end;
  TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS> = class abstract(TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>)
  end;
  TOpenAddressingLP<OPEN_ADDRESSING_CONSTRAINTS> = class(TOpenAddressing<OPEN_ADDRESSING_CONSTRAINTS>)
    procedure Bar; virtual; abstract;
  end;
  TOpenAddressingLP<TKey, TValue, THashFactory> = class(TOpenAddressingLP<TKey, TValue, THashFactory, TDummy>);
  TOpenAddressingLP<TKey, TValue>  = class(TOpenAddressingLP<TKey, TValue, TDummy, TDummy>);
  TDictionary<TKey, TValue> = class(TOpenAddressingLP<TKey, TValue>);

  // This would be a user defined type when using Generics.Collections.
  // In mode Delphi there is no "specialize" keyword but it makes no difference for the error.
  TestType = TDictionary<string,string>;
var
  TestVar: TestType;
begin
  TestVar:=TestType.a{completion:Create;Bar;Foo;TKey;TValue;THashFactory}
end.


