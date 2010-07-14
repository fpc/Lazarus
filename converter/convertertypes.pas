unit ConverterTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  // types for errors

  { EConverterError }

  EDelphiConverterError = class(Exception)
    constructor Create(const AMessage: string);
  end;


implementation

{ EConverterError }

constructor EDelphiConverterError.Create(const AMessage: string);
begin
  inherited Create('Converter: '+AMessage);
end;


end.

