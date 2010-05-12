{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Component serialisation drivers for pascal.

  Works:
    - simple properties: integer, strings, events, ...
    - nested components (e.g. the child controls of a form)
    - class properties (e.g. TControl.Font)

  ToDo:
    - TCollection needs a typecast to the item class
    - variants
    - widestrings need special encoding conversions, but the driver does not
      know, that a widestring is assigned
    - what to do with DefineProperties?
    - the 'with' can conflict
    - a reader
}

unit ComponentStreamPas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, typinfo;

type

  TPASObjectWriterStackElType = (
    elUnknown,
    elComponent,
    elPropertyList,
    elProperty,
    elChildrenList,
    elCollection,
    elCollectionItem
    );

  TPASObjectWriterStackEl = class
  public
    ElementName, ElementClass: string;
    ElemType: TPASObjectWriterStackElType;
  end;

  { TPASObjectWriter }

  TPASObjectWriter = class(TAbstractObjectWriter)
  private
    FStream: TStream;
    FStack: TFPList; // stack of TPASObjectWriterStackEl
    StackEl: TPASObjectWriterStackEl;
    procedure StackPush(const ElementName: string;
                        ElementType: TPASObjectWriterStackElType);
    procedure StackPop;
    procedure WriteIndent;
    procedure WriteAssignment(PropertyName, Value: string);
    procedure WritePropertyAssignment(Value: string);
    procedure WriteCreateComponent(CompName, CompClass, CompOwner: string);
    procedure WriteWithDoBegin(Expr: string);
    procedure WriteEnd;

    function StringToConstant(s: string): string;
  public
    constructor Create(AStream: TStream);

    { Begin/End markers. Those ones who don't have an end indicator, use
      "EndList", after the occurrence named in the comment. Note that this
      only counts for "EndList" calls on the same level; each BeginXXX call
      increases the current level. }
    procedure BeginCollection; override;{ Ends with the next "EndList" }
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override;{ Ends after the second "EndList" }
    procedure BeginList; override;
    procedure EndList; override;
    procedure BeginProperty(const PropName: String); override;
    procedure EndProperty; override;

    procedure WriteBinary(const Buffer; Count: Longint); override;
    procedure WriteBoolean(Value: Boolean); override;
    // procedure WriteChar(Value: Char);
    procedure WriteFloat(const Value: Extended); override;
    procedure WriteSingle(const Value: Single); override;
    procedure WriteCurrency(const Value: Currency); override;
    procedure WriteDate(const Value: TDateTime); override;
    procedure WriteIdent(const Ident: string); override;
    procedure WriteInteger(Value: Int64); override;
    procedure WriteMethodName(const Name: String); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteString(const Value: String); override;
    procedure WriteWideString(const Value: WideString); override;
    procedure WriteUInt64(Value: QWord); override;
    procedure WriteUnicodeString(const Value: UnicodeString); override;
    procedure WriteVariant(const VarValue: Variant); override;
    procedure Write(const Buffer; Count: Longint); override;
  public
    property Stream: TStream read FStream;
  end;
  TPASObjectWriterClass = class of TPASObjectWriter;

procedure WriteComponentToPasStream(AComponent: TComponent; AStream: TStream);

implementation

procedure WriteComponentToPasStream(AComponent: TComponent; AStream: TStream);
var
  Driver: TPASObjectWriter;
  Writer: TWriter;
begin
  Driver:=nil;
  Writer:=nil;
  try
    Driver:=TPASObjectWriter.Create(AStream);
    Writer:=TWriter.Create(Driver);
    Writer.WriteDescendent(AComponent,nil);
  finally
    Writer.Free;
    Driver.Free;
  end;
end;

procedure TPASObjectWriter.StackPush(const ElementName: string;
  ElementType: TPASObjectWriterStackElType);
begin
  if Assigned(FStack) then
  begin
    // append to stack
    FStack.Add(StackEl);
  end else
  begin
    // start stack
    FStack := TFPList.Create;
  end;
  // create element
  StackEl := TPASObjectWriterStackEl.Create;
  StackEl.ElementName:=ElementName;
  StackEl.ElemType:=ElementType;
  DebugLn('TPASObjectWriter.StackPush Element="',ElementName,'" FStack.Count=',dbgs(FStack.Count));
end;

procedure TPASObjectWriter.StackPop;
begin
  DebugLn('TPASObjectWriter.StackPop ',dbgs(FStack.Count),' ',StackEl.ElementName);
  if FStack=nil then
    raise Exception.Create('TPASObjectWriter.StackPop stack empty');
  StackEl.Free;
  if FStack.Count > 0 then
  begin
    StackEl := TPASObjectWriterStackEl(FStack[FStack.Count - 1]);
    FStack.Delete(FStack.Count - 1);
  end else
  begin
    FStack.Free;
    FStack := nil;
    StackEl := nil;
  end;
end;

procedure TPASObjectWriter.WriteIndent;
const
  Indent: PChar = '  ';
var
  i: Integer;
  Item: TPASObjectWriterStackEl;
begin
  if StackEl<>nil then begin
    if StackEl.ElemType in [elComponent,elCollection,elCollectionItem] then
      Stream.Write(Indent^,2);
  end;
  if FStack<>nil then begin
    for i:=0 to FStack.Count-1 do begin
      Item:=TPASObjectWriterStackEl(FStack[i]);
      if Item.ElemType in [elComponent,elCollection,elCollectionItem] then
        Stream.Write(Indent^,2);
    end;
  end;
end;

procedure TPASObjectWriter.WriteAssignment(PropertyName, Value: string);
var
  s: String;
begin
  WriteIndent;
  s:=PropertyName+' := '+Value+';'+LineEnding;
  Stream.Write(s[1],length(s));
end;

procedure TPASObjectWriter.WritePropertyAssignment(Value: string);
begin
  if (StackEl=nil) or (StackEl.ElemType<>elProperty) then
    raise Exception.Create('TPASObjectWriter.WritePropertyAssignment not in property');
  WriteAssignment(StackEl.ElementName,Value);
end;

procedure TPASObjectWriter.WriteCreateComponent(CompName, CompClass,
  CompOwner: string);
var
  s: String;
begin
  WriteIndent;
  s:='if '+CompName+'=nil then '+CompName+' := '+CompClass+'.Create('+CompOwner+');'+LineEnding;
  Stream.Write(s[1],length(s));
end;

procedure TPASObjectWriter.WriteWithDoBegin(Expr: string);
var
  s: String;
begin
  WriteIndent;
  s:='with '+Expr+' do begin'+LineEnding;
  Stream.Write(s[1],length(s));
end;

procedure TPASObjectWriter.WriteEnd;
const
  EndTxt: String = 'end;'+LineEnding;
begin
  WriteIndent;
  Stream.Write(EndTxt[1],length(EndTxt));
end;

function TPASObjectWriter.StringToConstant(s: string): string;
var
  i: Integer;
  InString: Boolean;
begin
  InString:=false;
  for i:=1 to length(s) do begin
    case s[i] of
    #0..#31,#127..#255:
      begin
        if InString then
          Result:=Result+'''';
        InString:=false;
        Result:=Result+'#'+IntToStr(ord(s[i]));
      end;
    else
      if not InString then
        Result:=Result+'''';
      InString:=true;
      Result:=Result+s[i];
    end;
  end;
  if InString then Result:=Result+'''';
end;

constructor TPASObjectWriter.Create(AStream: TStream);
begin
  inherited Create;
  FStream:=AStream;
end;

procedure TPASObjectWriter.BeginCollection;
begin
  debugln(['TPASObjectWriter.BeginCollection']);
  if StackEl.ElemType<>elProperty then
    raise Exception.Create('TPASObjectWriter.BeginCollection not supported');
  WriteWithDoBegin(StackEl.ElementName);
  StackPush('collection',elCollection);
end;

procedure TPASObjectWriter.BeginComponent(Component: TComponent; Flags: TFilerFlags;
  ChildPos: Integer);
// TWriter expects to push two elements on the stack, which are popped by
// two EndList calls.
var
  i: Integer;
  Item: TPASObjectWriterStackEl;
begin
  if (Component.Name='') or (not IsValidIdent(Component.Name)) then
    raise Exception.Create('TPASObjectWriter.BeginComponent not pascal identifier');
  if (FStack<>nil) and (FStack.Count>0) then begin
    // auto create child components
    for i:=0 to FStack.Count-1 do begin
      Item:=TPASObjectWriterStackEl(FStack[i]);
      if Item.ElemType=elComponent then begin
        if Item.ElementName<>'' then
          WriteCreateComponent(Component.Name,Component.ClassName,Item.ElementName);
        break;
      end;
    end;
  end;
  // enclose in "with" to create nicer code
  WriteWithDoBegin(Component.Name);

  StackPush(Component.Name,elComponent);
  StackEl.ElementClass := Component.ClassName;
  StackPush('properties',elPropertyList);
end;

procedure TPASObjectWriter.BeginList;
begin
  debugln(['TPASObjectWriter.BeginList ']);
  if (StackEl<>nil) and (StackEl.ElemType=elCollection) then begin
    // create collection item
    WriteWithDoBegin('Add');
    StackPush('collectionlist',elCollectionItem);
  end else
    raise Exception.Create('TPASObjectWriter.BeginList not supported');
end;

procedure TPASObjectWriter.EndList;
begin
  if StackEl.ElemType = elPropertyList then begin
    // end the property list and start the children list
    StackPop;
    StackPush('children',elChildrenList);
  end else if StackEl.ElemType = elChildrenList then begin
    // end the children list and the component
    StackPop; // end children
    StackPop; // end component
    WriteEnd;
  end else if StackEl.ElemType in [elCollection,elCollectionItem] then begin
    StackPop;
    WriteEnd;
  end else
    StackPop;
end;

procedure TPASObjectWriter.BeginProperty(const PropName: String);
begin
  DebugLn('TPASObjectWriter.BeginProperty "',PropName,'"');
  StackPush(PropName,elProperty);
end;

procedure TPASObjectWriter.EndProperty;
begin
  StackPop;
end;

procedure TPASObjectWriter.WriteBinary(const Buffer; Count: Longint);
var
  s: string;
begin
  SetLength(s,Count);
  if s<>'' then
    System.Move(Buffer,s[1],length(s));
  raise Exception.Create('TPASObjectWriter.WriteBinary not supported');
end;

procedure TPASObjectWriter.WriteBoolean(Value: Boolean);
begin
  if Value then
    WritePropertyAssignment('true')
  else
    WritePropertyAssignment('false');
end;

procedure TPASObjectWriter.WriteFloat(const Value: Extended);
begin
  WritePropertyAssignment(FloatToStr(Value));
end;

procedure TPASObjectWriter.WriteSingle(const Value: Single);
begin
  WritePropertyAssignment(FloatToStr(Value));
end;

procedure TPASObjectWriter.WriteCurrency(const Value: Currency);
begin
  WritePropertyAssignment(FloatToStr(Value));
end;

procedure TPASObjectWriter.WriteDate(const Value: TDateTime);
begin
  WritePropertyAssignment(FloatToStr(Value));
end;

procedure TPASObjectWriter.WriteIdent(const Ident: string);
begin
  WritePropertyAssignment(Ident);
end;

procedure TPASObjectWriter.WriteInteger(Value: Int64);
begin
  WritePropertyAssignment(IntToStr(Value));
end;

procedure TPASObjectWriter.WriteMethodName(const Name: String);
begin
  WritePropertyAssignment('@'+Name);
end;

procedure TPASObjectWriter.WriteSet(Value: LongInt; SetType: Pointer);
var
  i: Integer;
  Mask: LongInt;
  s: String;
begin
  Mask := 1;
  s:='';
  for i := 0 to 31 do begin
    if (Value and Mask) <> 0 then begin
      if s<>'' then s:=s+',';
      s:=s+GetEnumName(PTypeInfo(SetType), i);
    end;
    Mask := Mask shl 1;
  end;
  WritePropertyAssignment('['+s+']');
end;

procedure TPASObjectWriter.WriteString(const Value: String);
begin
  WritePropertyAssignment(StringToConstant(Value));
end;

procedure TPASObjectWriter.WriteWideString(const Value: WideString);
// save widestrings as utf8
begin
  WritePropertyAssignment('System.UTF8Decode('+StringToConstant(System.UTF8Encode(Value))+')');
end;

procedure TPASObjectWriter.WriteUInt64(Value: QWord);
begin
  WritePropertyAssignment(IntToStr(Value));
end;

procedure TPASObjectWriter.WriteUnicodeString(const Value: UnicodeString);
// save unicodestrings as utf8
begin
  WritePropertyAssignment(StringToConstant(Value));
end;

procedure TPASObjectWriter.WriteVariant(const VarValue: Variant);
begin
  case tvardata(VarValue).vtype of
    varEmpty:
      begin
        WritePropertyAssignment('nil'); // ToDo
      end;
    varNull:
      begin
        WritePropertyAssignment('nil'); // ToDo
      end;
    { all integer sizes must be split for big endian systems }
    varShortInt,varSmallInt,varInteger,varInt64:
      begin
        WriteInteger(VarValue);
      end;
    varQWord:
      begin
        WriteUInt64(VarValue);
      end;
    varBoolean:
      begin
        WriteBoolean(VarValue);
      end;
    varCurrency:
      begin
        WriteCurrency(VarValue);
      end;
    varSingle:
      begin
        WriteSingle(VarValue);
      end;
    varDouble:
      begin
        WriteFloat(VarValue);
      end;
    varDate:
      begin
        WriteDate(VarValue);
      end;
    varString:
      begin
        WriteString(VarValue);
      end;
    varOleStr:
      begin
        WriteWideString(VarValue);
      end;
    else
      raise EWriteError.CreateFmt('Unsupported property variant type %d', [Ord(tvardata(VarValue).vtype)]);
  end;
end;

procedure TPASObjectWriter.Write(const Buffer; Count: Longint);
begin
  // there can be arbitrary lots of Write calls
  raise Exception.Create('TPASObjectWriter.Write not supported');
end;

end.

