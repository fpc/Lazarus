unit ConverterTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TopOffset }

  // Used when fixing top coordinates of controls inside a visual container.
  TTopOffset = class
  private
    fParentType: string;
    fChildType: string;
    fStartPos: integer;
  public
    constructor Create(aParentType, aChildType: string; aStartPos: integer);
    destructor Destroy; override;
    property ParentType: string read fParentType;
    property ChildType: string read fChildType;
    property StartPos: integer read fStartPos;
  end;

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


{ TopOffset }

constructor TTopOffset.Create(aParentType, aChildType: string; aStartPos: integer);
begin
  fParentType:=aParentType;
  fChildType:=aChildType;
  fStartPos:=aStartPos;
end;

destructor TTopOffset.Destroy;
begin
  inherited Destroy;
end;


end.

