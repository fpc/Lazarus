unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Laz_XMLStreaming, Laz_DOM, laz_xmlcfg, Buttons;

type
  TMyEnum = (myEnum1, myEnum2, myEnum3);
  TMySet = set of TMyEnum;
  
  { TMyCollectionItem }

  TMyCollectionItem = class(TCollectionItem)
  private
    FMyString: string;
  published
    property MyString: string read FMyString write FMyString;
  end;
  
  { TMyComponent }

  TMyComponent = class(TComponent)
  private
    FMyBoolean: Boolean;
    FMyCollection: TCollection;
    FMyDouble: Double;
    FMyEnum: TMyEnum;
    FMyInteger: integer;
    FMySet: TMySet;
    FMySingle: Single;
    FMyString: string;
    FMyWideString: widestring;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property MyDouble: Double read FMyDouble write FMyDouble;
    property MySingle: Single read FMySingle write FMySingle;
    property MyWideString: widestring read FMyWideString write FMyWideString;
    property MyInteger: integer read FMyInteger write FMyInteger;
    property MyString: string read FMyString write FMyString;
    property MyEnum: TMyEnum read FMyEnum write FMyEnum;
    property MySet: TMySet read FMySet write FMySet;
    property MyBoolean: Boolean read FMyBoolean write FMyBoolean;
    property MyCollection: TCollection read FMyCollection write FMyCollection;
  end;


  { TStreamAsXMLForm }

  TStreamAsXMLForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    procedure FormCreate(Sender: TObject);
  private
    FFilename: string;
    procedure SetFilename(const AValue: string);
  public
    MyComponent: TMyComponent;
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
  MySubComponent: TMyComponent;
begin
  Filename:='test.xml';

  MyComponent:=TMyComponent.Create(Self);
  with MyComponent do begin
    Name:='MyComponent';
  end;
  MySubComponent:=TMyComponent.Create(MyComponent);
  with MySubComponent do begin
    Name:='MySubComponent';
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
    WriteComponentToXMLConfig(XMLConfig,'Component',Self);
    //WriteComponentToXMLConfig(XMLConfig,'Component',MyComponent);
    //WriteComponentToXMLConfig(XMLConfig,'Component',GroupBox1);
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
  
  sl:=TStringList.Create;
  sl.LoadFromFile(Filename);
  DebugLn('TStreamAsXMLForm.StreamComponents ',sl.Text);
  sl.Free;
end;

{ TMyComponent }

constructor TMyComponent.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  MyDouble:=-1.23456789;
  MySingle:=-1.98765432;
  MyEnum:=myEnum2;
  MySet:=[myEnum1,myEnum3];
  MyString:='Some text as string';
  MyWideString:='Some text as widestring';
  MyInteger:=1234;
  MyBoolean:=true;
  MyCollection:=TCollection.Create(TMyCollectionItem);
  TMyCollectionItem(MyCollection.Add).MyString:='First';
  TMyCollectionItem(MyCollection.Add).MyString:='Second';
  TMyCollectionItem(MyCollection.Add).MyString:='Third';
end;

initialization
  {$I mainunit.lrs}

end.

