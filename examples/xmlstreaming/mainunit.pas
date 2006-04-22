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
    SourceGroupBox: TGroupBox;
    GroupBox2: TGroupBox;
    DestinationGroupBox: TGroupBox;
    procedure FormCreate(Sender: TObject);
  private
    FFilename: string;
    procedure SetFilename(const AValue: string);
  public
    MyComponent: TMyComponent;
    procedure WriteComponents;
    procedure ReadComponents;
    procedure OnFindComponentClass(Reader: TReader; const AClassName: string;
                                   var ComponentClass: TComponentClass);
    property Filename: string read FFilename write SetFilename;
  end; 

var
  StreamAsXMLForm: TStreamAsXMLForm;

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
  Append: Boolean; var DestroyDriver: boolean): TWriter;
function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
  var DestroyDriver: boolean): TReader;
  
procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  AComponent: TComponent);
procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; TheOwner: TComponent);

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

function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
  var DestroyDriver: boolean): TReader;
var
  p: Pointer;
  Driver: TAbstractObjectReader;
  DummyStream: TMemoryStream;
begin
  DummyStream:=TMemoryStream.Create;
  try
    Result:=TReader.Create(DummyStream,256);
    DestroyDriver:=false;
    // hack to set a write protected variable.
    // DestroyDriver:=true; TReader will free it
    Driver:=TXMLObjectReader.Create(ADoc,Path);
    p:=@Result.Driver;
    Result.Driver.Free;
    TAbstractObjectReader(p^):=Driver;
  finally
    DummyStream:=nil;
  end;
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

procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; TheOwner: TComponent);
var
  DestroyDriver: Boolean;
  Reader: TReader;
  IsInherited: Boolean;
  AClassName: String;
  AClass: TComponentClass;
begin
  Reader:=nil;
  DestroyDriver:=false;
  try
    Reader:=CreateXMLReader(XMLConfig.Document,Path,DestroyDriver);

    // get root class
    AClassName:=(Reader.Driver as TXMLObjectReader).GetRootClassName(IsInherited);
    if IsInherited then begin
      // inherited is not supported by this simple function
      DebugLn('ReadComponentFromXMLConfig WARNING: "inherited" is not supported by this simple function');
    end;
    AClass:=nil;
    OnFindComponentClass(nil,AClassName,AClass);
    if AClass=nil then
      raise EClassNotFound.CreateFmt('Class "%s" not found', [AClassName]);

    if RootComponent=nil then begin
      // create root component
      // first create the new instance and set the variable ...
      RootComponent:=AClass.NewInstance as TComponent;
      // then call the constructor
      RootComponent.Create(TheOwner);
    end else begin
      // there is a root component, check if class is compatible
      if not RootComponent.InheritsFrom(AClass) then begin
        raise EComponentError.CreateFmt('Cannot assign a %s to a %s.',
                                        [AClassName,RootComponent.ClassName]);
      end;
    end;
    
    Reader.ReadRootComponent(RootComponent);
  finally
    if DestroyDriver then
      Reader.Driver.Free;
    Reader.Free;
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

  WriteComponents;
  ReadComponents;
end;

procedure TStreamAsXMLForm.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

procedure TStreamAsXMLForm.WriteComponents;
var
  XMLConfig: TXMLConfig;
  sl: TStringList;
begin
  DebugLn('TStreamAsXMLForm.WriteComponents ',Filename);
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    //WriteComponentToXMLConfig(XMLConfig,'Component',Self);
    WriteComponentToXMLConfig(XMLConfig,'Component',MyComponent);
    //WriteComponentToXMLConfig(XMLConfig,'Component',GroupBox2);
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
  
  sl:=TStringList.Create;
  sl.LoadFromFile(Filename);
  DebugLn('TStreamAsXMLForm.WriteComponents ',sl.Text);
  sl.Free;
end;

procedure TStreamAsXMLForm.ReadComponents;
var
  XMLConfig: TXMLConfig;
  sl: TStringList;
  NewComponent: TComponent;
begin
  DebugLn('TStreamAsXMLForm.ReadComponents ',Filename);
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    NewComponent:=nil;
    ReadComponentFromXMLConfig(XMLConfig,'Component',NewComponent,
      @OnFindComponentClass,DestinationGroupBox);
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;

  sl:=TStringList.Create;
  sl.LoadFromFile(Filename);
  DebugLn('TStreamAsXMLForm.StreamComponents ',sl.Text);
  sl.Free;
end;

procedure TStreamAsXMLForm.OnFindComponentClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName,'TGroupBox')=0 then
    ComponentClass:=TGroupBox
  else if CompareText(AClassName,'TButton')=0 then
    ComponentClass:=TButton
  else if CompareText(AClassName,'TMyComponent')=0 then
    ComponentClass:=TMyComponent;
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

