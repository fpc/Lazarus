unit girParser;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
interface

uses
  Classes, SysUtils, Dom;

type
  IgirParser = interface
    procedure ParseNode(ANode: TDomNode);
  end;

implementation

end.

