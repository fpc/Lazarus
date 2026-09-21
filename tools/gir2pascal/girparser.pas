unit girParser;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
interface

uses
  Classes, SysUtils, Dom;

type
  IgirParser = interface ['{88D9E0C3-7170-47FC-87A5-8A2CCDC6CB6C}']
    procedure ParseNode(ANode: TDomNode);
  end;

implementation

end.

