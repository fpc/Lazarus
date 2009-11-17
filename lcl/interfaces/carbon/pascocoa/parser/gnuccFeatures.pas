{ * This file is part of ObjCParser tool 
  * Copyright (C) 2008-2009 by Dmitry Boyarintsev under the GNU LGPL
  * license version 2.0 or 2.1.  You should have received a copy of the
  * LGPL license along with at http://www.gnu.org/                                              
}

unit gnuccFeatures;

{list of GNU CC features, that might be found at header files
 it's extermly possible that these language and precomipler features 
 are not to be compatible with MS C/C++ header files }

interface

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

uses
  ObjCParserTypes;

type
  TAttribute = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    Expression  : AnsiString;
  end;

function ParseAttribute(Parent: TEntity; Parser: TTextParser): TEntity;

implementation

function ParseAttribute(Parent: TEntity; Parser: TTextParser): TEntity;
var
  attr  : TAttribute;
begin
  attr := TAttribute.Create(nil);
  try
    if attr.Parse(Parser) then begin
      Parent.Items.Add(attr);
      attr.owner := Parent;
      Result:=attr;
    end else begin
      attr.Free;
      Result := nil;
    end;
  finally
  end;
end;

{ TAttribute }

function TAttribute.DoParse(AParser: TTextParser): Boolean;
var
  s   : string;
  tt  : TTokenType;
begin
  Result := AParser.FindNextToken(s, tt);

  if not Result or (s <> '__attribute__') then begin
    Result := false;
    Exit;
  end;
  Expression := ParseSeq(AParser, '(', ')');
  Result := true;
end;

initialization
  RegisterEntity( @ParseAttribute);

end.
