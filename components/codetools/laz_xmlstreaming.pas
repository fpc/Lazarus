{
    This file is part of the Free Component Library

    XML serialisation driver
    Copyright (c) 2000 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit Laz_XMLStreaming;

{$MODE objfpc}
{$H+}

interface

uses SysUtils, Classes, Laz_DOM;

type

  TXMLObjectWriterStackElType = (elUnknown, elPropertyList, elChildrenList);

  TXMLObjectWriterStackEl = class
  public
    Element, Parent: TDOMElement;
    ElType: TXMLObjectWriterStackElType;
    CurName: String;
  end;

  { TXMLObjectWriter }

  TXMLObjectWriter = class(TAbstractObjectWriter)
  private
    FDoc: TDOMDocument;
    FRootEl: TDOMElement;
    FStack: TList;
    StackEl: TXMLObjectWriterStackEl;
    procedure StackPush;
    procedure StackPop;
    function GetPropertyElement(const TypeName: String): TDOMElement;
  public
    constructor Create(ADoc: TDOMDocument);
    procedure BeginCollection; override;
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override;
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
  end;



implementation


procedure TXMLObjectWriter.StackPush;
var
  Parent: TDOMElement;
begin
  if Assigned(FStack) then
  begin
    Parent := StackEl.Element;
    FStack.Add(StackEl);
    StackEl := TXMLObjectWriterStackEl.Create;
    StackEl.Parent := Parent;
  end else
  begin
    FStack := TList.Create;
    StackEl := TXMLObjectWriterStackEl.Create;
    StackEl.Parent := FRootEl;
  end;
end;

procedure TXMLObjectWriter.StackPop;
begin
  StackEl.Free;
  if FStack.Count > 0 then
  begin
    StackEl := TXMLObjectWriterStackEl(FStack[FStack.Count - 1]);
    FStack.Delete(FStack.Count - 1);
  end else
  begin
    FStack.Free;
    FStack := nil;
    StackEl := nil;
  end;
end;

function TXMLObjectWriter.GetPropertyElement(const TypeName: String): TDOMElement;
begin
  if not Assigned(StackEl.Element) then
  begin
    StackEl.Element := FDoc.CreateElement(TypeName);
    StackEl.Parent.AppendChild(StackEl.Element);
    StackEl.Element['name'] := StackEl.CurName;
    Result := StackEl.Element;
  end else
    Result := nil;
end;

constructor TXMLObjectWriter.Create(ADoc: TDOMDocument);
begin
  inherited Create;
  FDoc := ADoc;
  FRootEl := FDoc.CreateElement('fcl-persistent');
  FDoc.AppendChild(FRootEl);
end;

procedure TXMLObjectWriter.BeginCollection;
begin
  WriteLn('BeginCollection');
end;

procedure TXMLObjectWriter.BeginComponent(Component: TComponent; Flags: TFilerFlags;
  ChildPos: Integer);
begin
  StackPush;
  StackEl.Element := FDoc.CreateElement('component');
  StackEl.Parent.AppendChild(StackEl.Element);

  if Length(Component.Name) > 0 then
    StackEl.Element['name'] := Component.Name;
  StackEl.Element['class'] := Component.ClassName;

  StackPush;
  StackEl.Element := FDoc.CreateElement('properties');
  StackEl.Parent.AppendChild(StackEl.Element);
  StackEl.ElType := elPropertyList;
end;

procedure TXMLObjectWriter.BeginList;
begin
  WriteLn('BeginList');
end;

procedure TXMLObjectWriter.EndList;
begin
  if StackEl.ElType = elPropertyList then
  begin
    if not StackEl.Element.HasChildNodes then
      StackEl.Parent.RemoveChild(StackEl.Element);
    StackPop;

    StackPush;
    StackEl.Element := FDoc.CreateElement('children');
    StackEl.Parent.AppendChild(StackEl.Element);
    StackEl.ElType := elChildrenList;
  end else if StackEl.ElType = elChildrenList then
  begin
    if not StackEl.Element.HasChildNodes then
      StackEl.Parent.RemoveChild(StackEl.Element);
    StackPop;
  end else
    StackPop;
end;

procedure TXMLObjectWriter.BeginProperty(const PropName: String);
begin
  StackPush;
  StackEl.CurName := PropName;
end;

procedure TXMLObjectWriter.EndProperty;
begin
  StackPop;
end;

procedure TXMLObjectWriter.WriteBinary(const Buffer; Count: Longint);
var
  s: string;
begin
  SetLength(s,Count);
  if s<>'' then
    System.Move(Buffer,s[1],length(s));
  GetPropertyElement('binary')['value'] := s;
end;

procedure TXMLObjectWriter.WriteBoolean(Value: Boolean);
begin
  if Value then
    GetPropertyElement('boolean')['value'] := 'true'
  else
    GetPropertyElement('boolean')['value'] := 'false';
end;

procedure TXMLObjectWriter.WriteFloat(const Value: Extended);
begin
  GetPropertyElement('extended')['value'] := FloatToStr(Value);
end;

procedure TXMLObjectWriter.WriteSingle(const Value: Single);
begin
  GetPropertyElement('single')['value'] := FloatToStr(Value);
end;

procedure TXMLObjectWriter.WriteCurrency(const Value: Currency);
begin
  GetPropertyElement('Currency')['value'] := FloatToStr(Value);
end;

procedure TXMLObjectWriter.WriteDate(const Value: TDateTime);
begin
  GetPropertyElement('date')['value'] := FloatToStr(Value);
end;

procedure TXMLObjectWriter.WriteIdent(const Ident: string);
begin
  GetPropertyElement('ident')['value'] := Ident;
end;

procedure TXMLObjectWriter.WriteInteger(Value: Int64);
begin
  GetPropertyElement('integer')['value'] := IntToStr(Value);
end;

procedure TXMLObjectWriter.WriteMethodName(const Name: String);
begin
  GetPropertyElement('method-name')['value'] := Name;
end;

procedure TXMLObjectWriter.WriteSet(Value: LongInt; SetType: Pointer);
begin
  writeln('TXMLObjectWriter.WriteSet ',Value);
end;

procedure TXMLObjectWriter.WriteString(const Value: String);
begin
  GetPropertyElement('string')['value'] := Value;
end;

procedure TXMLObjectWriter.WriteWideString(const Value: WideString);
var
  s: string;
begin
  SetLength(s,length(Value)*2);
  if s<>'' then
    System.Move(Value[1],s[1],length(s));
  GetPropertyElement('widestring')['value'] := s;
end;


end.
