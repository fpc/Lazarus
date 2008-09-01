unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  FileUtil, StdCtrls, Laz_XMLStreaming, Laz_DOM, Laz_XMLCfg, Buttons, TypInfo;

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
    FMyInt64: int64;
    FMyInteger: integer;
    FMySet: TMySet;
    FMySingle: Single;
    FMyString: string;
    FMyStrings: TStrings;
    FMyWideString: widestring;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteDebugReport;
  published
    property MyDouble: Double read FMyDouble write FMyDouble;
    property MySingle: Single read FMySingle write FMySingle;
    property MyWideString: widestring read FMyWideString write FMyWideString;
    property MyInteger: integer read FMyInteger write FMyInteger;
    property MyString: string read FMyString write FMyString;
    property MyInt64: int64 read FMyInt64 write FMyInt64;
    property MySet: TMySet read FMySet write FMySet;
    property MyBoolean: Boolean read FMyBoolean write FMyBoolean;
    property MyEnum: TMyEnum read FMyEnum write FMyEnum;
    property MyCollection: TCollection read FMyCollection write FMyCollection;
    property MyStrings: TStrings read FMyStrings write FMyStrings;
  end;
  
  { TMyGroupBox }

  TMyGroupBox = class(TGroupBox)
  published
    procedure AnEvent(Sender: TObject);
  end;


  { TStreamAsXMLForm }

  TStreamAsXMLForm = class(TForm)
    Button1: TButton;
    SourceGroupBox: TGroupBox;
    DestinationGroupBox: TGroupBox;
    procedure FormCreate(Sender: TObject);
  private
    FFilename: string;
    procedure SetFilename(const AValue: string);
  public
    MyComponent: TMyComponent;
    DemoGroupBox: TMyGroupBox;

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
    Reader.OnFindComponentClass:=OnFindComponentClass;

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
  DemoGroupBox_1: TGroupBox;
  DemoGroupBox_2: TGroupBox;
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
  
  DemoGroupBox:=TMyGroupBox.Create(Self);
  with DemoGroupBox do begin
    Name:='DemoGroupBox';
    SetBounds(100,2,320,180);
    Parent:=SourceGroupBox;
    OnClick:=@DemoGroupBox.AnEvent;
  end;
  
  // create nested controls
  DemoGroupBox_1:=TGroupBox.Create(DemoGroupBox);
  with DemoGroupBox_1 do begin
    Name:='DemoGroupBox_1';
    Parent:=DemoGroupBox;
    SetBounds(5,5,150,150);
    with TButton.Create(DemoGroupBox) do begin
      Name:='Button1';
      Parent:=DemoGroupBox_1;
      SetBounds(10,20,80,30);
    end;
    with TButton.Create(DemoGroupBox) do begin
      Name:='Button2';
      Parent:=DemoGroupBox_1;
      SetBounds(10,60,80,20);
    end;
  end;
  DemoGroupBox_2:=TGroupBox.Create(DemoGroupBox);
  with DemoGroupBox_2 do begin
    Name:='DemoGroupBox_2';
    Parent:=DemoGroupBox;
    SetBounds(155,5,150,150);
    with TButton.Create(DemoGroupBox) do begin
      Name:='Button3';
      Parent:=DemoGroupBox_2;
      SetBounds(10,20,80,30);
    end;
    with TButton.Create(DemoGroupBox) do begin
      Name:='Button4';
      Parent:=DemoGroupBox_2;
      SetBounds(10,60,80,20);
    end;
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
    //WriteComponentToXMLConfig(XMLConfig,'Component',DemoGroupBox);
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
  
  sl:=TStringList.Create;
  sl.LoadFromFile(UTF8ToSys(Filename));
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
    if NewComponent is TMyComponent then
      TMyComponent(NewComponent).WriteDebugReport;
    if NewComponent is TControl then
      TControl(NewComponent).Parent:=DestinationGroupBox;
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
  
  sl:=TStringList.Create;
  sl.LoadFromFile(UTF8ToSys(Filename));
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
    ComponentClass:=TMyComponent
  else if CompareText(AClassName,'TMyGroupBox')=0 then
    ComponentClass:=TMyGroupBox;
  DebugLn('TStreamAsXMLForm.OnFindComponentClass ',AClassName,' ',dbgs(ComponentClass));
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
  MyInt64:=1234567890987654321;
  MyCollection:=TCollection.Create(TMyCollectionItem);
  TMyCollectionItem(MyCollection.Add).MyString:='First';
  TMyCollectionItem(MyCollection.Add).MyString:='Second';
  TMyCollectionItem(MyCollection.Add).MyString:='Third';
  FMyStrings:=TStringList.Create;
  FMyStrings.Text:='FirstLine'#10'NextLine';
end;

destructor TMyComponent.Destroy;
begin
  FreeAndNil(FMyStrings);
  inherited Destroy;
end;

procedure TMyComponent.WriteDebugReport;
var
  i: Integer;
  Item: TMyCollectionItem;
begin
  writeln('TMyComponent.WriteDebugReport ');
  writeln('  MyDouble=',MyDouble);
  writeln('  MySingle=',MySingle);
  writeln('  MyEnum=',GetEnumName(TypeInfo(TMyEnum),ord(MyEnum)));
  writeln('  MySet=',HexStr(Cardinal(MySet),8));
  writeln('  MyString=',MyString);
  writeln('  MyWideString=',MyWideString);
  writeln('  MyInteger=',MyInteger);
  writeln('  MyInt64=',MyInt64);
  writeln('  MyCollection.Count=',MyCollection.Count);
  for i:=0 to MyCollection.Count-1 do begin
    Item:=TMyCollectionItem(MyCollection.Items[i]);
    writeln('    ',i,' MyString=',Item.MyString);
  end;
  writeln('  MyStrings='+dbgstr(MyStrings.Text));
end;

{ TMyGroupBox }

procedure TMyGroupBox.AnEvent(Sender: TObject);
begin

end;

initialization
  {$I mainunit.lrs}

end.

