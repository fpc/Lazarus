unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Laz_XMLStreaming, DOM, laz_xmlcfg, Buttons;

type

  { TStreamAsXMLForm }

  TStreamAsXMLForm = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    procedure FormCreate(Sender: TObject);
  private
    FFilename: string;
    procedure SetFilename(const AValue: string);
  public
    procedure StreamComponents;
    property Filename: string read FFilename write SetFilename;
  end; 

var
  StreamAsXMLForm: TStreamAsXMLForm;

function CreateXMLWriter(ADoc: TDOMDocument;
  var DestroyDriver: boolean): TWriter;

implementation

function CreateXMLWriter(ADoc: TDOMDocument;
  var DestroyDriver: boolean): TWriter;
var
  Driver: TAbstractObjectWriter;
begin
  Driver:=TXMLObjectWriter.Create(ADoc);
  DestroyDriver:=true;
  Result:=TWriter.Create(Driver);
end;

{ TStreamAsXMLForm }

procedure TStreamAsXMLForm.FormCreate(Sender: TObject);
begin
  Filename:='test.xml';
  StreamComponents;
end;

procedure TStreamAsXMLForm.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

procedure TStreamAsXMLForm.StreamComponents;
var
  XMLConfig: TXMLConfig;
  Writer: TWriter;
  DestroyDriver: boolean;
begin
  XMLConfig:=TXMLConfig.Create(Filename);
  Writer:=nil;
  DestroyDriver:=false;
  try
    Writer:=CreateXMLWriter(XMLConfig.Document,DestroyDriver);
    Writer.WriteRootComponent(GroupBox1);
    XMLConfig.Flush;
  finally
    if DestroyDriver then
      Writer.Driver.Free;
    Writer.Free;
    XMLConfig.Free;
  end;
end;

initialization
  {$I mainunit.lrs}

end.

