unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Laz_XMLStreaming, Laz_DOM, laz_xmlcfg, Buttons;

type
  TMyEnum = (myEnum1, myEnum2, myEnum3);
  TMySet = set of TMyEnum;

  { TMyComponent }

  TMyComponent = class(TComponent)
  private
    FMyDouble: Double;
    FMyEnum: TMyEnum;
    FMyInteger: integer;
    FMySet: TMySet;
    FMyString: string;
    FMyWideString: widestring;
  public
    property MyDouble: Double read FMyDouble write FMyDouble;
    property MyWideString: widestring read FMyWideString write FMyWideString;
    property MyInteger: integer read FMyInteger write FMyInteger;
    property MyString: string read FMyString write FMyString;
    property MyEnum: TMyEnum read FMyEnum write FMyEnum;
    property MySet: TMySet read FMySet write FMySet;
  end;


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

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
  Append: Boolean; var DestroyDriver: boolean): TWriter;
procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  AComponent: TComponent);

implementation

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
  Append: Boolean; var DestroyDriver: boolean): TWriter;
var
  Driver: TAbstractObjectWriter;
begin
  Driver:=TXMLObjectWriter.Create(ADoc,Path,Append);
  DestroyDriver:=true;
  Result:=TWriter.Create(Driver);
end;

procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  AComponent: TComponent);
var
  Writer: TWriter;
  DestroyDriver: boolean;
begin
  Writer:=nil;
  DestroyDriver:=false;
  try
    Writer:=CreateXMLWriter(XMLConfig.Document,Path,false,DestroyDriver);
    XMLConfig.Modified:=true;
    Writer.WriteRootComponent(AComponent);
    XMLConfig.Flush;
  finally
    if DestroyDriver then
      Writer.Driver.Free;
    Writer.Free;
  end;
end;

{ TStreamAsXMLForm }

procedure TStreamAsXMLForm.FormCreate(Sender: TObject);
var
  MyComponent: TMyComponent;
begin
  Filename:='test.xml';

  MyComponent:=TMyComponent.Create(Self);
  with MyComponent do begin
    Name:='MyComponent';
    MyDouble:=-1.23456789;
    MyEnum:=myEnum2;
    MySet:=[myEnum1,myEnum3];
    MyString:='Some text as string';
    MyWideString:='Some text as widestring';
    MyInteger:=1234;
  end;

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
  sl: TStringList;
begin
  DebugLn('TStreamAsXMLForm.StreamComponents ',Filename);
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    WriteComponentToXMLConfig(XMLConfig,'Component',GroupBox1);
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
  
  sl:=TStringList.Create;
  sl.LoadFromFile(Filename);
  DebugLn('TStreamAsXMLForm.StreamComponents ',sl.Text);
  sl.Free;
end;

initialization
  {$I mainunit.lrs}

end.

