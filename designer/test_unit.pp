{

}
{$H+}
unit test_unit;

{$mode objfpc}

interface

uses
  Classes, StdCtrls, Forms, Buttons, Menus, ComCtrls, SysUtils, ExtCtrls,
  ObjectInspector, PropEdits, Graphics, TypInfo;

type
  TMyEnum = (MyEnum1,MyEnum2,MyEnum3);
  TMySet = set of TMyEnum;

  TMySubComponent = class(TComponent)
  public
    FMyInteger:integer;
  published
    property MyInteger:integer read FMyInteger write FMyInteger;
  end;

  TMyComponent = class(TComponent)
  public
    FMyInteger:integer;
    FMyCardinal:Cardinal;
    FMyInt64:int64;
    FMyEnum:TMyEnum;
    FMySet:TMySet;
    FMyFloat:extended;
    FMyAnsiString:AnsiString;
    FMyShortString:ShortString;
    FMyBool:boolean;
    FMySubComponent:TMySubComponent;
    FMyGraphicsObject:TGraphicsObject;
    FMyBrush:TBrush;
    FMyPen:TPen;
    FMyFont:TFont;
    FMyEvent:TNotifyEvent;
    procedure DoSomething(Sender:TObject);
    procedure SetMyAnsiString(const NewValue:AnsiString);
    procedure SetMyShortString(const NewValue:ShortString);
    constructor Create(AOwner:TComponent);  override;
    destructor Destroy;  override;
  published
    MySubComponent2:TMySubComponent;
    property MyInteger:integer read FMyInteger write FMyInteger;
    property MyCardinal:cardinal read FMyCardinal write FMyCardinal;
//    property MyInt64:int64 read FMyInt64 write FMyInt64;
    property MyEnum:TMyEnum read FMyEnum write FMyEnum;
    property MySet:TMySet read FMySet write FMySet;
    property MyFloat:Extended read FMyFloat write FMyFloat;
    property MyAnsiString:AnsiString read FMyAnsiString write SetMyAnsiString;
    property MyShortString:ShortString read FMyShortString write SetMyShortString;
    property MyBool:Boolean read FMyBool write FMyBool;
    property MySubComponent:TMySubComponent read FMySubComponent write FMySubComponent;
//    property MyGraphicsObject:TGraphicsObject read FMyGraphicsObject write FMyGraphicsObject;
//    property MyBrush:TBrush read FMyBrush write FMyBrush;
//    property MyPen:TPen read FMyPen write FMyPen;
//    property MyFont:TFont read FMyFont write FMyFont;
//    property MyEvent:TNotifyEvent read FMyEvent write FMyEvent;
  end;

	TForm1 = class(TFORM)
	public
	  Label1 : TLabel;
	  Label2 : TLabel;
	  Label3 : TLabel;
	  EditToComboButton: TButton;
	  AddItemButton: TButton;
	  ComboToEditButton: TButton;
	  SwitchEnabledButton: TButton;
	  DumpButton: TButton;
	  IndexButton: TButton;
    OIResizeButton: TButton;
    OIRefreshButton: TButton;
	  Edit1 : TEdit;
	  mnuMain: TMainMenu;
	  itmFileQuit: TMenuItem;
	  itmFile: TMenuItem;
    ComboBox1 : TComboBox;
    ComboBox2 : TComboBox;
    Memo1 : TMemo;
    WriteLFMButton:TButton;
    constructor Create(AOwner: TComponent); override;	
    procedure LoadMainMenu;
	  procedure FormKill(Sender : TObject);
	  procedure FormShow(Sender : TObject);
	  procedure mnuQuitClicked(Sender : TObject);
	protected
	  procedure EditToComboButtonCLick(Sender : TObject);
	  procedure AddItemButtonCLick(Sender : TObject);
	  procedure ComboToEditButtonCLick(Sender : TObject);
	  procedure SwitchEnabledButtonCLick(Sender : TObject);
	  procedure DumpButtonCLick(Sender : TObject);
	  procedure IndexButtonCLick(Sender : TObject);
	  procedure OIResizeButtonCLick(Sender : TObject);
	  procedure OIRefreshButtonCLick(Sender : TObject);
	  procedure ComboOnChange (Sender:TObject);
	  procedure ComboOnClick (Sender:TObject);
    procedure WriteLFMButtonClick(Sender:TObject);
  public
    FMyInteger:integer;
    FMyCardinal:Cardinal;
    FMyEnum:TMyEnum;
    FMySet:TMySet;
    FMyAnsiString:AnsiString;
    FMyShortString:ShortString;
    FMyBool:boolean;
    FMyBrush:TBrush;
    FMyPen:TPen;
    FMyFont:TFont;
    FMyComponent:TMyComponent;
    procedure SetMyAnsiString(const NewValue:AnsiString);
    procedure SetMyShortString(const NewValue:ShortString);
  published
    property MyInteger:integer read FMyInteger write FMyInteger;
    property MyCardinal:cardinal read FMyCardinal write FMyCardinal;
    property MyEnum:TMyEnum read FMyEnum write FMyEnum;
    property MySet:TMySet read FMySet write FMySet;
    property MyAnsiString:AnsiString read FMyAnsiString write SetMyAnsiString;
    property MyShortString:ShortString read FMyShortString write SetMyShortString;
    property MyBool:Boolean read FMyBool write FMyBool;
    property MyBrush:TBrush read FMyBrush write FMyBrush;
    property MyPen:TPen read FMyPen write FMyPen;
    property MyFont:TFont read FMyFont write FMyFont;
    property MyComponent:TMyComponent read FMyComponent write FMyComponent;
	end;

  TMatBinaryObjectWriter = class(TAbstractObjectWriter)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FBufSize: Integer;
    FBufPos: Integer;
    FSignatureWritten: Boolean;
    procedure FlushBuffer;
    procedure Write(const Buffer; Count: Longint);
    procedure WriteValue(Value: TValueType);
    procedure WriteStr(const Value: AnsiString);
  public
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;

    procedure BeginCollection; override;
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override;
    procedure BeginList; override;
    procedure EndList; override;
    procedure BeginProperty(const PropName: AnsiString); override;
    procedure EndProperty; override;

    procedure WriteBinary(const Buffer; Count: LongInt); override;
    procedure WriteBoolean(Value: Boolean); override;
    procedure WriteFloat(const Value: Extended); override;
    procedure WriteSingle(const Value: Single); override;
    {!!!: procedure WriteCurrency(const Value: Currency);  override;}
    procedure WriteDate(const Value: TDateTime); override;
    procedure WriteIdent(const Ident: Ansistring); override;
    procedure WriteInteger(Value: Int64); override;
    procedure WriteMethodName(const Name: AnsiString); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteString(const Value: AnsiString); override;
  end;


var
  Form1 : TForm1;
  OI: TObjectInspector;

implementation



//==============================================================================


procedure ObjectBinaryToText(Input, Output: TStream);

  procedure OutStr(s: String);
  begin
    writeln('OutStr '''+s+''' NewTotalLen='+IntToStr(OutPut.Size));
    if Length(s) > 0 then
      Output.Write(s[1], Length(s));
  end;

  procedure OutLn(s: String);
  begin
    OutStr(s + #10);
  end;

  procedure OutString(s: String);
  var
    res, NewStr: String;
    i: Integer;
    InString, NewInString: Boolean;
  begin
    writeln('OutString '''+s+'''');
    res := '';
    InString := False;
    for i := 1 to Length(s) do begin
      NewInString := InString;
      case s[i] of
        #0..#31: begin
            if InString then
              NewInString := False;
            NewStr := '#' + IntToStr(Ord(s[i]));
          end;
        '''':
            if InString then NewStr := ''''''
            else NewStr := '''''''';
        else begin
          if not InString then
            NewInString := True;
          NewStr := s[i];
        end;
      end;
      if NewInString <> InString then begin
        NewStr := '''' + NewStr;
        InString := NewInString;
      end;
      res := res + NewStr;
    end;
    if InString then res := res + '''';
    OutStr(res);
  end;

  function ReadInt(ValueType: TValueType): LongInt;
  begin
    writeln('ReadInt(ValueType)');
    case ValueType of
      vaInt8: Result := ShortInt(Input.ReadByte);
      vaInt16: Result := SmallInt(Input.ReadWord);
      vaInt32: Result := LongInt(Input.ReadDWord);
    end;
  end;

  function ReadInt: LongInt;
  begin
    writeln('ReadInt');
    Result := ReadInt(TValueType(Input.ReadByte));
  end;

  function ReadSStr: String;
  var
    len: Byte;
  begin
    writeln('ReadStr');
    len := Input.ReadByte;
    SetLength(Result, len);
    Input.Read(Result[1], len);
  end;

  procedure ReadPropList(indent: String);

    procedure ProcessValue(ValueType: TValueType; Indent: String);

      procedure Stop(s: String);
      begin
        WriteLn(s);
        Halt;
      end;

      procedure ProcessBinary;
      var
        ToDo, DoNow, i: LongInt;
        lbuf: array[0..31] of Byte;
        s: String;
      begin
        ToDo := Input.ReadDWord;
        OutLn('{');
        while ToDo > 0 do begin
          DoNow := ToDo;
          if DoNow > 32 then DoNow := 32;
          Dec(ToDo, DoNow);
          s := Indent + '  ';
          Input.Read(lbuf, DoNow);
          for i := 0 to DoNow - 1 do
            s := s + IntToHex(lbuf[i], 2);
          OutLn(s);
        end;
        OutLn(indent + '}');
      end;

    var
      s: String;
      len: LongInt;
      IsFirst: Boolean;
      ext: Extended;

    begin
    writeln('ProcessValue Indent='''+Indent+'''');
      OutStr('(' + IntToStr(Ord(Valuetype)) + ') ');
      case ValueType of
        vaList: begin
            OutStr('(');
            IsFirst := True;
            while True do begin
              ValueType := TValueType(Input.ReadByte);
              if ValueType = vaNull then break;
              if IsFirst then begin
                OutLn('');
                IsFirst := False;
              end;
              OutStr(Indent + '  ');
              ProcessValue(ValueType, Indent + '  ');
            end;
            OutLn(Indent + ')');
          end;
        vaInt8: OutLn(IntToStr(ShortInt(Input.ReadByte)));
        vaInt16: OutLn( IntToStr(SmallInt(Input.ReadWord)));
        vaInt32: OutLn(IntToStr(LongInt(Input.ReadDWord)));
        vaExtended: begin
            Input.Read(ext, SizeOf(ext));
            OutLn(FloatToStr(ext));
          end;
        vaString: begin
            OutString(ReadSStr);
            OutLn('');
          end;
        vaIdent: OutLn(ReadSStr);
        vaFalse: OutLn('False');
        vaTrue: OutLn('True');
        vaBinary: ProcessBinary;
        vaSet: begin
            OutStr('[');
            IsFirst := True;
            while True do begin
              s := ReadSStr;
              if Length(s) = 0 then break;
              if not IsFirst then OutStr(', ');
              IsFirst := False;
              OutStr(s);
            end;
            OutLn(']');
          end;
        vaLString: Stop('!!LString!!');
        vaNil: OutLn('nil'); // Stop('nil');
        vaCollection: begin
            OutStr('<');
            while Input.ReadByte <> 0 do begin
              OutLn(Indent);
              Input.Seek(-1, soFromCurrent);
              OutStr(indent + '  item');
              ValueType := TValueType(Input.ReadByte);
              if ValueType <> vaList then
                OutStr('[' + IntToStr(ReadInt(ValueType)) + ']');
              OutLn('');
              ReadPropList(indent + '    ');
              OutStr(indent + '  end');
            end;
            OutLn('>');
          end;
        {vaSingle: begin OutLn('!!Single!!'); exit end;
        vaCurrency: begin OutLn('!!Currency!!'); exit end;
        vaDate: begin OutLn('!!Date!!'); exit end;
        vaWString: begin OutLn('!!WString!!'); exit end;}
        else
          Stop(IntToStr(Ord(ValueType)));
      end;
    end;

  begin
    writeln('ReadPropList');
    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      OutStr(indent + ReadSStr + ' = ');
      ProcessValue(TValueType(Input.ReadByte), Indent);
    end;
  end;

  procedure ReadObject(indent: String);
  var
    b: Byte;
    ObjClassName, ObjName: String;
    ChildPos: LongInt;
  begin
    writeln('ReadObject');
    // Check for FilerFlags
    b := Input.ReadByte;
    if (b and $f0) = $f0 then begin
      if (b and 2) <> 0 then ChildPos := ReadInt;
    end else begin
      b := 0;
      Input.Seek(-1, soFromCurrent);
    end;

    ObjClassName := ReadSStr;
    ObjName := ReadSStr;

    OutStr(Indent);
    if (b and 1) <> 0 then OutStr('inherited')
    else OutStr('object');
    OutStr(' ');
    if ObjName <> '' then
      OutStr(ObjName + ': ');
    OutStr(ObjClassName);
    if (b and 2) <> 0 then OutStr('[' + IntToStr(ChildPos) + ']');
    OutLn('');

    ReadPropList(indent + '  ');

    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      ReadObject(indent + '  ');
    end;
    OutLn(indent + 'end');
  end;

type
  PLongWord = ^LongWord;
const
  signature: PChar = 'TPF0';
begin
  if Input.ReadDWord <> PLongWord(Pointer(signature))^ then
    raise EReadError.Create('Illegal stream image' {###SInvalidImage});
  ReadObject('');
end;

//==============================================================================


{ TMyComponent }

constructor TMyComponent.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Name:='MyComponent';
  FMyInteger:=-1234;
  FMyCardinal:=5678;
  FMySet:=[MyEnum1];
  FMyEnum:=MyEnum2;
  FMyFloat:=3.2;
  FMyBool:=true;
  FMyAnsiString:='Ansi';
  FMyShortString:='Short';
  FMySubComponent:=TMySubComponent.Create(Self);
  with FMySubComponent do begin
    MyInteger:=789;
  end;
  FMyGraphicsObject:=nil;
  FMyFont:=TFont.Create;
  FMyBrush:=TBrush.Create;
  FMyPen:=TPen.Create;
  FMyEvent:=@DoSomething;

  MySubComponent2:=TMySubComponent.Create(Self);
  with MySubComponent2 do begin
    MyInteger:=1928;
  end;
end;

destructor TMyComponent.Destroy;
begin
  FMyPen.Free;
  FMyBrush.Free;
  FMyFont.Free;
  FMySubComponent.Free;
  inherited Destroy;
end;

procedure TMyComponent.SetMyAnsiString(const NewValue:AnsiString);
begin
  FMyAnsiString:=NewValue;
end;

procedure TMyComponent.SetMyShortString(const NewValue:ShortString);
begin
  FMyShortString:=NewValue;
end;

procedure TMyComponent.DoSomething(Sender:TObject);
begin
  //
end;

//==============================================================================



{ TMatBinaryObjectWriter }

constructor TMatBinaryObjectWriter.Create(Stream: TStream; BufSize: Integer);
begin
writeln('MBOW: Create');
  inherited Create;
  FStream := Stream;
  FBufSize := BufSize;
  GetMem(FBuffer, BufSize);
end;

destructor TMatBinaryObjectWriter.Destroy;
begin
writeln('MBOW: Destroy');
  // Flush all data which hasn't been written yet
  FlushBuffer;

  if Assigned(FBuffer) then
    FreeMem(FBuffer, FBufSize);

  inherited Destroy;
end;

procedure TMatBinaryObjectWriter.BeginCollection;
begin
writeln('MBOW: BeginCollection');
  WriteValue(vaCollection);
end;

procedure TMatBinaryObjectWriter.BeginComponent(Component: TComponent;
  Flags: TFilerFlags; ChildPos: Integer);
var
  Prefix: Byte;
begin
writeln('MBOW: BeginComponent');
  if not FSignatureWritten then
  begin
    Write(FilerSignature, SizeOf(FilerSignature));
    FSignatureWritten := True;
  end;

  { Only write the flags if they are needed! }
  if Flags <> [] then
  begin
    Prefix := Integer(Flags) or $f0;
    Write(Prefix, 1);
    if ffChildPos in Flags then
      WriteInteger(ChildPos);
  end;

  WriteStr(Component.ClassName);
  WriteStr(Component.Name);
writeln('MBOW: BeginComponent end');
end;

procedure TMatBinaryObjectWriter.BeginList;
begin
writeln('MBOW: BeginList');
  WriteValue(vaList);
end;

procedure TMatBinaryObjectWriter.EndList;
begin
writeln('MBOW: EndList');
  WriteValue(vaNull);
end;

procedure TMatBinaryObjectWriter.BeginProperty(const PropName: AnsiString);
begin
writeln('MBOW: BeginProperty '+PropName);
  WriteStr(PropName);
end;

procedure TMatBinaryObjectWriter.EndProperty;
begin
writeln('MBOW: EndProperty');
end;

procedure TMatBinaryObjectWriter.WriteBinary(const Buffer; Count: LongInt);
begin
writeln('MBOW: WriteBinary');
  WriteValue(vaBinary);
  Write(Count, 4);
  Write(Buffer, Count);
end;

procedure TMatBinaryObjectWriter.WriteBoolean(Value: Boolean);
begin
writeln('MBOW: WriteBoolean');
  if Value then
    WriteValue(vaTrue)
  else
    WriteValue(vaFalse);
end;

procedure TMatBinaryObjectWriter.WriteFloat(const Value: Extended);
begin
writeln('MBOW: WriteFloat');
  WriteValue(vaExtended);
  Write(Value, SizeOf(Value));
end;

procedure TMatBinaryObjectWriter.WriteSingle(const Value: Single);
begin
writeln('MBOW: WriteSingle');
  WriteValue(vaSingle);
  Write(Value, SizeOf(Value));
end;

{!!!: procedure TMatBinaryObjectWriter.WriteCurrency(const Value: Currency);
begin
  WriteValue(vaCurrency);
  Write(Value, SizeOf(Value));
end;}

procedure TMatBinaryObjectWriter.WriteDate(const Value: TDateTime);
begin
writeln('MBOW: WriteDate');
  WriteValue(vaDate);
  Write(Value, SizeOf(Value));
end;

procedure TMatBinaryObjectWriter.WriteIdent(const Ident: Ansistring);
begin
writeln('MBOW: WriteIdent '+Ident);
  { Check if Ident is a special identifier before trying to just write
    Ident directly }
  if UpperCase(Ident) = 'NIL' then
    WriteValue(vaNil)
  else if UpperCase(Ident) = 'FALSE' then
    WriteValue(vaFalse)
  else if UpperCase(Ident) = 'TRUE' then
    WriteValue(vaTrue)
  else if UpperCase(Ident) = 'NULL' then
    WriteValue(vaNull) else
  begin
    WriteValue(vaIdent);
    WriteStr(Ident);
  end;
end;

procedure TMatBinaryObjectWriter.WriteInteger(Value: Int64);
begin
writeln('MBOW: WriteInteger '+IntToStr(Value));
  { Use the smallest possible integer type for the given value: }
  if (Value >= -128) and (Value <= 127) then
  begin
    WriteValue(vaInt8);
    Write(Value, 1);
  end else if (Value >= -32768) and (Value <= 32767) then
  begin
    WriteValue(vaInt16);
    Write(Value, 2);
  end else if (Value >= -$80000000) and (Value <= $7fffffff) then
  begin
    WriteValue(vaInt32);
    Write(Value, 4);
  end else
  begin
    WriteValue(vaInt64);
    Write(Value, 8);
  end;
end;

procedure TMatBinaryObjectWriter.WriteMethodName(const Name: AnsiString);
begin
writeln('MBOW: WriteMethodName '+Name);
  if Length(Name) > 0 then
  begin
    WriteValue(vaIdent);
    WriteStr(Name);
  end else
    WriteValue(vaNil);
end;

procedure TMatBinaryObjectWriter.WriteSet(Value: LongInt; SetType: Pointer);
var
  i: Integer;
  Mask: LongInt;
begin
writeln('MBOW: WriteSet');
  WriteValue(vaSet);
  Mask := 1;
  for i := 0 to 31 do
  begin
    if (Value and Mask) <> 0 then
      WriteStr(GetEnumName(PTypeInfo(SetType), i));
    Mask := Mask shl 1;
  end;
  WriteStr('');
end;

procedure TMatBinaryObjectWriter.WriteString(const Value: AnsiString);
var
  i: Integer;
begin
writeln('MBOW: WriteString '''+Value+'''');
  i := Length(Value);
  if i <= 255 then
  begin
    WriteValue(vaString);
    Write(i, 1);
  end else
  begin
    WriteValue(vaLString);
    Write(i, 4);
  end;
  if i > 0 then
    Write(Value[1], i);
end;

{!!!: procedure TMatBinaryObjectWriter.WriteWideString(const Value: WideString);
var
  i: Integer;
begin
  WriteValue(vaWString);
  i := Length(Value);
  Write(i, 4);
  Write(Value[1], i * 2);
end;}

procedure TMatBinaryObjectWriter.FlushBuffer;
begin
writeln('MBOW: FlushBuffer');
  FStream.WriteBuffer(FBuffer^, FBufPos);
  FBufPos := 0;
end;

procedure TMatBinaryObjectWriter.Write(const Buffer; Count: LongInt);
var
  CopyNow: LongInt;
begin
writeln('MBOW: Write (Count='+IntToStr(Count)+')');
  while Count > 0 do begin
    CopyNow := Count;
    if CopyNow > FBufSize - FBufPos then
      CopyNow := FBufSize - FBufPos;
    Move(Buffer, PChar(FBuffer)[FBufPos], CopyNow);
    Dec(Count, CopyNow);
    Inc(FBufPos, CopyNow);
    if FBufPos = FBufSize then
      FlushBuffer;
  end;
end;

procedure TMatBinaryObjectWriter.WriteValue(Value: TValueType);
begin
writeln('MBOW: WriteValue');
  Write(Value, 1);
end;

procedure TMatBinaryObjectWriter.WriteStr(const Value: AnsiString);
var
  i: Integer;
begin
writeln('MBOW: WriteStr '''+Value+'''');
  i := Length(Value);
  if i > 255 then
    i := 255;
  Write(i, 1);
  if i > 0 then
    Write(Value[1], i);
end;


//==============================================================================

{ TForm1 }

procedure TForm1.SetMyAnsiString(const NewValue:AnsiString);
begin
  FMyAnsiString:=NewValue;
end;

procedure TForm1.SetMyShortString(const NewValue:ShortString);
begin
  FMyShortString:=NewValue;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMySet:=[MyEnum1];
  FMyEnum:=MyEnum2;
  FMyFont:=TFont.Create;
  FMyBrush:=TBrush.Create;
  FMyPen:=TPen.Create;
  FMyComponent:=TMyComponent.Create(nil);
  with FMyComponent do begin
    Name:='FMyComponent';
  end;

  Name:='Form1';
  Caption := 'Test Form';
  OI:=nil;
  OnShow:=@FormShow;
  LoadMainMenu;
  ActiveControl:=AddItemButton;
  Left:=250;
  Top:=50;
  if OI=nil then begin
    OI:=TObjectInspector.Create(Application);
    OI.Name:='OI';
    OI.SetBounds(7,50,220,700);
    OI.Show;
    OI.RootComponent:=Self;
  end;
end;

procedure TForm1.FormKill(Sender : TObject);
Begin
  FMyComponent.Free;
  FMyBrush.Free;
  FMyPen.Free;
  FMyFont.Free;
  Application.Terminate;
End;

procedure TForm1.WriteLFMButtonClick(Sender:TObject);
var BinStream:TMemoryStream;
  Driver: TAbstractObjectWriter;
  Writer:TWriter;
  TxtStream:TFileStream;
  s:string;
begin
  BinStream:=TMemoryStream.Create;
  try
    Driver:=TMatBinaryObjectWriter.Create(BinStream,4096);
    try
      Writer:=TWriter.Create(Driver);
      try
        Writer.WriteDescendent(Self,nil);
      finally
        Writer.Free;
      end;
    finally
      Driver.Free;
    end;
    TxtStream:=TFileStream.Create(Name+'.lfm',fmCreate);
    try
      BinStream.Position:=0;
      ObjectBinaryToText(BinStream,TxtStream);
    finally
      TxtStream.Free;
    end;
  finally
    BinStream.Free;
  end;
writeln('Object written.');
end;

procedure TForm1.FormShow(Sender: TObject);
begin
end;

procedure TForm1.OIResizeButtonClick(Sender : TObject);
begin
  OI.DoInnerResize;
end;

procedure TForm1.OIRefreshButtonClick(Sender : TObject);
begin
  OI.RefreshPropertyValues;
end;

procedure TForm1.EditToComboButtonClick(Sender : TObject);
Begin
  if assigned (ComboBox1) and assigned (edit1) then
    ComboBox1.Text := edit1.text;
End;

procedure TForm1.AddItemButtonClick(Sender : TObject);
Begin
   if assigned (ComboBox1) 
      then Combobox1.Items.Add ('item ' + IntToStr (comboBox1.Items.Count));
   if assigned (ComboBox2) 
      then Combobox2.Items.Add ('item ' + IntToStr (comboBox2.Items.Count));
End;

procedure TForm1.ComboToEditButtonClick(Sender : TObject);
Begin
   if assigned (ComboBox1) and assigned (edit1) 
      then edit1.Text := ComboBox1.Text;
End;

procedure TForm1.SwitchEnabledButtonClick(Sender : TObject);
Begin
   if assigned (ComboBox1) 
      then ComboBox1.Enabled := not ComboBox1.Enabled;
End;

procedure TForm1.DumpButtonClick(Sender : TObject);
var
   i : integer;
Begin
   if assigned (ComboBox1) then
   begin
      i := 0;
      while i < ComboBox1.Items.Count do
      begin
         if assigned (Memo1)
            then Memo1.Lines.Add (ComboBox1.Items[i]);
	       inc (i);
      end;
   end;
End;

procedure TForm1.IndexButtonClick(Sender : TObject);
var
   s : shortstring;
Begin
   if assigned (ComboBox1) then
   begin
      s := Format ('%x', [ComboBox1.ItemIndex]);
      if assigned (Memo1)
         then Memo1.Lines.Add (s);
   end;
End;

procedure TForm1.ComboOnChange (Sender:TObject);
var
   s : shortstring;	
begin
   if sender is TEdit 
      then s := 'TEdit'
   else if sender is TComboBox
      then s := 'TComboBox'
   else
      s := 'UNKNOWN';
   if assigned (Memo1)
      then Memo1.Lines.Add (s + 'ONChange');
end;

procedure TForm1.ComboOnClick (Sender:TObject);
begin
   if assigned (Memo1)
      then Memo1.Lines.Add ('ONClick');
end;

{------------------------------------------------------------------------------}
procedure TForm1.LoadMainMenu;

begin
   OnDestroy := @FormKill;

   { set the height and width }
   Height := 400;
   Width := 700;

   OIResizeButton:=TButton.Create(Self);
   with OIResizeButton do begin
     Name:='OIResizeButton';
     Parent:=Self;
     SetBounds(200,10,100,30);
     Caption:='Resize OI';
     OnClick:=@OIResizeButtonClick;
     Show;
   end;

   OIRefreshButton:=TButton.Create(Self);
   with OIRefreshButton do begin
     Name:='OIRefreshButton';
     Parent:=Self;
     SetBounds(200,50,100,30);
     Caption:='Refresh OI';
     OnClick:=@OIRefreshButtonClick;
     Show;
   end;

   WriteLFMButton:=TButton.Create(Self);
   with WriteLFMButton do begin
     Name:='WriteLFMButton';
     Parent:=Self;
     SetBounds(200,90,100,30);
     Caption:='Write LFM file';
     OnClick:=@WriteLFMButtonClick;
     Show;
   end;

   { Create 2 buttons inside the groupbox }
   EditToComboButton := TButton.Create(Self);
   EditToComboButton.Name:='EditToComboButton';
   EditToComboButton.Parent := Self;
   EditToComboButton.Left := 50;
   EditToComboButton.Top := 80;
   EditToComboButton.Width := 120;
   EditToComboButton.Height := 30;
   EditToComboButton.Show;
   EditToComboButton.Caption := 'Edit->Combo';
   EditToComboButton.OnClick := @EditToComboButtonClick;

   AddItemButton := TButton.Create(Self);
   AddItemButton.Name:='AddItemButton';
   AddItemButton.Parent := Self;
   AddItemButton.Left := 50;
   AddItemButton.Top := 40;
   AddItemButton.Width := 120;
   AddItemButton.Height := 30;
   AddItemButton.Show;
   AddItemButton.Caption := 'Add item';
   AddItemButton.OnClick := @AddItemButtonClick;

   { Create 2 more buttons outside the groupbox }
   ComboToEditButton := TButton.Create(Self);
   ComboToEditButton.Name:='ComboToEditButton';
   ComboToEditButton.Parent := Self;
   ComboToEditButton.Left := 50;
   ComboToEditButton.Top := 120;
   ComboToEditButton.Width := 120;
   ComboToEditButton.Height := 30;
   ComboToEditButton.Show;
   ComboToEditButton.Caption := 'Combo->Edit';
   ComboToEditButton.OnClick := @ComboToEditButtonClick;


   SwitchEnabledButton := TButton.Create(Self);
   SwitchEnabledButton.Name:='SwitchEnabledButton';
   SwitchEnabledButton.Parent := Self;
   SwitchEnabledButton.Left := 50;
   SwitchEnabledButton.Top := 160;
   SwitchEnabledButton.Width := 120;
   SwitchEnabledButton.Height := 30;
   SwitchEnabledButton.Show;
   SwitchEnabledButton.Caption := 'Enabled On/Off';
   SwitchEnabledButton.OnClick := @SwitchEnabledButtonClick;

   DumpButton := TButton.Create(Self);
   DumpButton.Name:='DumpButton';
   DumpButton.Parent := Self;
   DumpButton.Left := 50;
   DumpButton.Top := 200;
   DumpButton.Width := 120;
   DumpButton.Height := 30;
   DumpButton.Show;
   DumpButton.Caption := 'Dump';
   DumpButton.OnClick := @DumpButtonClick;

   IndexButton := TButton.Create(Self);
   IndexButton.Name:='IndexButton';
   IndexButton.Parent := Self;
   IndexButton.Left := 50;
   IndexButton.Top := 240;
   IndexButton.Width := 120;
   IndexButton.Height := 30;
   IndexButton.Show;
   IndexButton.Caption := 'Index ?';
   IndexButton.OnClick := @IndexButtonClick;


   { Create a label for the edit field }
   label1 := TLabel.Create(Self);
   Label1.Name:='Label1';
   label1.Parent := self;
   label1.top	 := 50;
   label1.left	 := 320;
   label1.Height := 20;
   label1.Width  := 130;
   label1.Show;
   label1.Caption := 'TEdit :';


   Edit1 := TEdit.Create (self);
   with Edit1 do begin
      Name   := 'Edit1';
      Parent := self;
      Left   := 500;
      Top    := 50;
      Width  := 70;
      Height := 20;
      OnChange := @ComboOnChange;	
      OnClick  := @ComboOnClick;	
      Show;	
   end;

   { Create a label for the 1st combobox }
   label2 := TLabel.Create(Self);
   Label2.Name:='Label2';
   label2.Parent := self;
   label2.top	 := 100;
   label2.left	 := 320;
   label2.Height := 20;
   label2.Width  := 130;
   label2.Enabled:= true;
   label2.Show;
   label2.Caption := 'Combo (unsorted)';
   label2.Enabled:= true;


   { Create the menu now }
   { WARNING: If you do it after creation of the combo, the menu will not 
     appear. Reason is unknown by now!!!!!!}
   mnuMain := TMainMenu.Create(Self);
   mnuMain.Name:='mnuMain';
   Menu := mnuMain;
   itmFile := TMenuItem.Create(Self);
   itmFile.Name:='itmFile';
   itmFile.Caption := '&File';
   mnuMain.Items.Add(itmFile);
   itmFileQuit := TMenuItem.Create(Self);
   itmFileQuit.Name:='itmFileQuit';
   itmFileQuit.Caption := '&Quit';
   itmFileQuit.OnClick := @mnuQuitClicked;
   itmFile.Add(itmFileQuit);

   ComboBox1 := TComboBox.Create (self);
   with ComboBox1 do
   begin
     Name:='ComboBox1';
     Parent := self;
     Left := 500;
     Top	:= 100;
     Width := 170;
     Height := 20;
     Style := csDropDown;
     Items.Add ('wohhh!');
     Items.Add ('22222!');
     ItemIndex := 1;
     Items.Add ('33333!');
     Items.Add ('abcde!');
     OnChange := @ComboOnChange;
     OnClick  := @ComboOnClick;
     Show;
   end;


   { Create a label for the 2nd combobox }
   label3 := TLabel.Create(Self);
   Label3.Name:='Label3';
   label3.Parent := self;
   label3.top	 := 150;
   label3.left	 := 320;
   label3.Height := 20;
   label3.Width  := 130;
   label3.Show;
   label3.Caption := 'Combo (sorted)';


   ComboBox2 := TComboBox.Create (self);
   with ComboBox2 do begin
     Name:='ComboBox2';
     Parent := self;
     Left := 500;
     Top	:= 150;
     Width := 170;
     Height := 20;
     Items.Add ('wohhh!');
     Items.Add ('22222!');
     ItemIndex := 1;
     Items.Add ('33333!');
     Items.Add ('abcde!');
     Sorted := true;
     Show;
   end;

   Memo1 := TMemo.Create(Self);
   with Memo1 do begin
     Memo1.Name:='Memo1';
     Parent := Self;
     Scrollbars := ssBoth;
     Left := 200;
     Top := 200;
     Width := 335;
     Height := 155;
     Show;
   end;
end;

{------------------------------------------------------------------------------}
procedure TForm1.mnuQuitClicked(Sender : TObject);
begin
  Application.Terminate;
end;
{------------------------------------------------------------------------------}


end.
