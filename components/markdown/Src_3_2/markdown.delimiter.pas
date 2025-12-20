unit markdown.delimiter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, markdown.elements, markdown.utils;

type
  TMarkDownDelimiterMode = (dmNeither, dmOpener, dmCloser, dmBoth);
  TMarkDownDelimiter = class
  private
    Fmode: TMarkDownDelimiterMode;
    Fdelimiter: String;
    Factive: boolean;
    FNode: TMarkDownTextNode;
  public
    constructor Create(node : TMarkDownTextNode; delimiter : String; mode : TMarkDownDelimiterMode);
    function isOpener : boolean;
    function isCloser : boolean;
    property node : TMarkDownTextNode read FNode;
    property delimiter : String read Fdelimiter write Fdelimiter;
    property active : boolean read Factive write Factive;
    property mode : TMarkDownDelimiterMode read Fmode write Fmode;
    function isEmph : boolean;
  end;
  TMarkDownDelimiterList = class (specialize TGFPObjectList<TMarkDownDelimiter>);
implementation

{ TMarkDownDelimiter }

constructor TMarkDownDelimiter.Create(node: TMarkDownTextNode; delimiter : String; mode: TMarkDownDelimiterMode);
begin
  inherited create;
  FNode := node;
  FMode := mode;
  Fdelimiter := delimiter;
  FActive := true;
end;

function TMarkDownDelimiter.isCloser: boolean;
begin
  result := mode in [dmCloser, dmBoth];
end;

function TMarkDownDelimiter.isEmph: boolean;
begin
  result := (delimiter <> '[') and (delimiter <> '![');
end;

function TMarkDownDelimiter.isOpener: boolean;
begin
  result := mode in [dmOpener, dmBoth];
end;


end.

