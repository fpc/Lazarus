unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, CompWriterPas, LazLogger;

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

  { TMyPersistent }

  TMyPersistent = class(TPersistent)
  private
    FMyInteger: integer;
  public
    constructor Create;
  published
    property MyInteger: integer read FMyInteger write FMyInteger;
  end;

  { TMyComponent }

  TMyComponent = class(TComponent)
  private
    FMyBoolean: Boolean;
    FMyCollection: TCollection;
    FMyDouble: Double;
    FMyEnum: TMyEnum;
    FMyEvent: TNotifyEvent;
    FMyInt64: int64;
    FMyInteger: integer;
    FMyPersistent: TMyPersistent;
    FMySet: TMySet;
    FMySingle: Single;
    FMyString: string;
    FMyStrings: TStrings;
    FMyWideString: widestring;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteDebugReport;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent);
            override;
  published
    {property MyDouble: Double read FMyDouble write FMyDouble;
    property MySingle: Single read FMySingle write FMySingle;
    property MyWideString: widestring read FMyWideString write FMyWideString;
    property MyInteger: integer read FMyInteger write FMyInteger;
    property MyString: string read FMyString write FMyString;
    property MyInt64: int64 read FMyInt64 write FMyInt64;
    property MySet: TMySet read FMySet write FMySet;
    property MyBoolean: Boolean read FMyBoolean write FMyBoolean;
    property MyEnum: TMyEnum read FMyEnum write FMyEnum;}
    //property MyPersistent: TMyPersistent read FMyPersistent write FMyPersistent;
    property MyCollection: TCollection read FMyCollection write FMyCollection;
    //property MyStrings: TStrings read FMyStrings write FMyStrings;
    property MyEvent: TNotifyEvent read FMyEvent write FMyEvent;
    procedure DoSomething(Sender: TObject);
  end;

  { TMyGroupBox }

  TMyGroupBox = class(TComponent)
  published
    procedure AnEvent(Sender: TObject);
  end;


  { TStreamAsPasForm }

  TStreamAsPasForm = class(TComponent)
    //Button1: TButton;
    //SourceGroupBox: TGroupBox;
    //DestinationGroupBox: TGroupBox;
    //procedure FormCreate(Sender: TObject);
  private
    FFilename: string;
    procedure SetFilename(const AValue: string);
  public
    MyComponent: TMyComponent;
    //DemoGroupBox: TMyGroupBox;

    constructor Create(AOwner: TComponent); override;

    procedure WriteComponents;
    procedure ReadComponents;
    procedure OnFindComponentClass({%H-}Reader: TReader; const AClassName: string;
                                   var ComponentClass: TComponentClass);
    property Filename: string read FFilename write SetFilename;
  end;

var
  StreamAsPasForm: TStreamAsPasForm;

implementation

{ TMyGroupBox }

procedure TMyGroupBox.AnEvent(Sender: TObject);
begin

end;

{ TStreamAsPasForm }

procedure TStreamAsPasForm.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

constructor TStreamAsPasForm.Create(AOwner: TComponent);
var
  MySubComponent: TMyComponent;
begin
  inherited Create(AOwner);

  Filename:='test.xml';

  MyComponent:=TMyComponent.Create(Self);
  with MyComponent do begin
    Name:='MyComponent';
  end;
  MySubComponent:=TMyComponent.Create(MyComponent);
  with MySubComponent do begin
    Name:='MySubComponent';
  end;

  {DemoGroupBox:=TMyGroupBox.Create(Self);
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
  end; }

  WriteComponents;
  ReadComponents;
end;

procedure TStreamAsPasForm.WriteComponents;
var
  ms: TMemoryStream;
  s: string;
begin
  ms:=TMemoryStream.Create;
  try
    WriteComponentToPasStream(MyComponent,ms);
    ms.Position:=0;
    SetLength(s,ms.Size);
    if s<>'' then
      ms.Read(s[1],length(s));
    DebugLn(['TStreamAsPasForm.WriteComponents ',s]);
  finally
    ms.Free;
  end;
end;

procedure TStreamAsPasForm.ReadComponents;
begin

end;

procedure TStreamAsPasForm.OnFindComponentClass(Reader: TReader;
  const AClassName: string; var ComponentClass: TComponentClass);
begin
  //if CompareText(AClassName,'TGroupBox')=0 then
  //  ComponentClass:=TGroupBox
  //else if CompareText(AClassName,'TButton')=0 then
  //  ComponentClass:=TButton
  //else
  if CompareText(AClassName,'TMyComponent')=0 then
    ComponentClass:=TMyComponent
  else if CompareText(AClassName,'TMyGroupBox')=0 then
    ComponentClass:=TMyGroupBox;
  DebugLn('TStreamAsPasForm.OnFindComponentClass ',AClassName,' ',dbgs(ComponentClass));
end;

{ TMyComponent }

constructor TMyComponent.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMyDouble:=-1.23456789;
  FMySingle:=-1.98765432;
  FMyEnum:=myEnum2;
  FMySet:=[myEnum1,myEnum3];
  FMyString:='Some text as string'#0'Test';
  FMyWideString:='Some text as widestring';
  FMyInteger:=1234;
  FMyBoolean:=true;
  FMyInt64:=1234567890987654321;
  FMyEvent:=@DoSomething;
  FMyPersistent:=TMyPersistent.Create;
  FMyCollection:=TCollection.Create(TMyCollectionItem);
  TMyCollectionItem(FMyCollection.Add).MyString:='First';
  TMyCollectionItem(FMyCollection.Add).MyString:='Second';
  TMyCollectionItem(FMyCollection.Add).MyString:='Third';
  FMyStrings:=TStringList.Create;
  FMyStrings.Text:='FirstLine'#10'NextLine';
end;

destructor TMyComponent.Destroy;
begin
  FreeAndNil(FMyStrings);
  FreeAndNil(FMyPersistent);
  inherited Destroy;
end;

procedure TMyComponent.WriteDebugReport;
var
  i: Integer;
  Item: TMyCollectionItem;
begin
  debugln('TMyComponent.WriteDebugReport ');
  debugln(['  MyDouble=',FMyDouble]);
  debugln(['  MySingle=',FMySingle]);
  debugln(['  MyEnum=',GetEnumName(TypeInfo(TMyEnum),ord(FMyEnum))]);
  debugln(['  MySet=',HexStr(Cardinal(FMySet),8)]);
  debugln(['  MyString=',FMyString]);
  debugln(['  MyWideString=',FMyWideString]);
  debugln(['  MyInteger=',FMyInteger]);
  debugln(['  MyInt64=',FMyInt64]);
  debugln(['  MyCollection.Count=',FMyCollection.Count]);
  for i:=0 to FMyCollection.Count-1 do begin
    Item:=TMyCollectionItem(FMyCollection.Items[i]);
    debugln(['    ',i,' MyString=',Item.MyString]);
  end;
  //debugln(['  MyStrings='+dbgstr(MyStrings.Text)]);
end;

procedure TMyComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  if Root=Self then
    for i:=0 to ComponentCount-1 do
      Proc(Components[i]);
end;

procedure TMyComponent.DoSomething(Sender: TObject);
begin

end;

{ TMyPersistent }

constructor TMyPersistent.Create;
begin
  FMyInteger:=12345;
end;

end.

