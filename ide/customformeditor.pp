{
 /***************************************************************************
                               CustomFormEditor.pp
                             -------------------




 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
{$H+}
unit CustomFormEditor;

{$mode objfpc}

interface

uses
  classes, abstractformeditor, controls,propedits,Typinfo,ObjectInspector,forms,IDEComp;

Const OrdinalTypes = [tkInteger,tkChar,tkENumeration,tkbool];

type
{
TComponentInterface is derived from TIComponentInterface.  It gives access to
each control that's dropped onto the form
}

  TCustomFormEditor = class; //forward declaration


  TComponentInterface = class(TIComponentInterface)
  private
    FControl : TComponent;
    FFormEditor : TCustomFormEditor;  //used to call it's functions
    Function FSetProp(PRI : PPropInfo; const Value) : Boolean;
    Function FGetProp(PRI : PPropInfo; var Value) : Boolean;

  protected
    Function GetPPropInfobyIndex(Index : Integer) : PPropInfo;
    Function GetPPropInfobyName(Name : String) : PPropInfo;

  public
    constructor Create;
    destructor Destroy; override;

    Function GetComponentType    : String; override;
    Function GetComponentHandle  : LongInt; override;
    Function GetParent           : TIComponentInterface; override;
    Function IsTControl          : Boolean; override;
    Function GetPropCount	   : Integer; override;
    Function GetPropType(Index : Integer) : TTypeKind; override;
    Function GetPropTypeInfo(Index : Integer) : PTypeInfo;
    Function GetPropName(Index : Integer) : String; override;
    Function GetPropTypeName(Index : Integer) : String; override;
    Function GetPropTypebyName(Name : String) : TTypeKind; override;

    Function GetPropValue(Index : Integer; var Value) : Boolean; override;
    Function GetPropValuebyName(Name: String; var Value) : Boolean; override;
    Function SetProp(Index : Integer; const Value) : Boolean; override;
    Function SetPropbyName(Name : String; const Value) : Boolean; override;


    Function GetControlCount: Integer; override;
    Function GetControl(Index : Integer): TIComponentInterface; override;

    Function GetComponentCount: Integer; override;
    Function GetComponent(Index : Integer): TIComponentInterface; override;

    Function Select : Boolean; override;
    Function Focus : Boolean; override;
    Function Delete : Boolean; override;
    property Control : TComponent read FCOntrol;
  end;

{
TCustomFormEditor

}

 TControlClass = class of TControl;

 TCustomFormEditor = class(TAbstractFormEditor)
  private
   FModified     : Boolean;
   FComponentInterfaceList : TList; //used to track and find controls
   FSelectedComponents : TComponentSelectionList;
   FObj_Inspector : TObjectInspector;
  protected
    Procedure RemoveFromComponentInterfaceList(Value :TIComponentInterface);
  public
    constructor Create;
    destructor Destroy; override;

    Function AddSelected(Value : TComponent) : Integer;
    Procedure DeleteControl(Value : TComponent);
    Function Filename : String; override;
    Function FormModified : Boolean; override;
    Function FindComponentByName(const Name : String) : TIComponentInterface; override;
    Function FindComponent(AComponent: TComponent): TIComponentInterface; override;
    Function GetFormComponent : TIComponentInterface; override;
//    Function CreateComponent(CI : TIComponentInterface; TypeName : String;
    Function CreateControlComponentInterface(Control: TCOmponent) : TIComponentInterface;

    Function CreateComponent(ParentCI : TIComponentInterface;
      TypeClass : TComponentClass;  X,Y,W,H : Integer): TIComponentInterface; override;
    Function NewFormFromLFM(_Filename : String): TCustomform;
    Procedure ClearSelected;
    property SelectedComponents : TComponentSelectionList read FSelectedComponents write FSelectedComponents;
    property Obj_Inspector : TObjectInspector read FObj_Inspector write FObj_Inspector;

  end;


implementation

uses
  SysUtils,jitforms;

var
JITFormList : TJITForms;

{TComponentInterface}

constructor TComponentInterface.Create;
begin
  inherited Create;
end;

destructor TComponentInterface.Destroy;
begin
  inherited Destroy;
end;

Function TComponentInterface.FSetProp(PRI : PPropInfo;
const Value) : Boolean;
Begin
writeln('Index = '+inttostr(PRI^.index));
  case PRI^.PropType^.Kind of
  tkBool: Begin
             Writeln('Boolean....');
             SetOrdProp(FControl,PRI,longint(Value));
             Result := True;
             end;
  tkSString,
  tkLString,
  tkAString,
  tkWString : Begin
              Writeln('String...');
              SetStrProp(FControl,PRI,String(Value));
              Result := True;
             end;
  tkInteger,
  tkInt64   : Begin
              Writeln('Int64...');
              SetInt64Prop(FControl,PRI,Int64(Value));
              Result := True;
             end;
  tkFloat  : Begin
              Writeln('Float...');
              SetFloatProp(FControl,PRI,Extended(Value));
              Result := True;
             end;
  tkVariant  : Begin
              Writeln('Variant...');
              SetVariantProp(FControl,PRI,Variant(Value));
              Result := True;
             end;
  tkMethod  : Begin
              Writeln('Method...');
              SetMethodProp(FControl,PRI,TMethod(value));
              Result := True;
             end;
  else
    Result := False;
  end;//case
end;

Function TComponentInterface.FGetProp(PRI : PPropInfo; var Value) : Boolean;
Begin
Result := True;
       case PRI^.PropType^.Kind of
       tkBool    : Longint(Value) := GetOrdProp(FControl,PRI);
       tkSString,
       tkLString,
       tkAString,
       tkWString : Begin
                    Writeln('Get String...');
                    String(Value) := GetStrProp(FControl,PRI);
                    Writeln('The string returned is '+String(value));
                    Writeln('*Get String...');
                   end;
       tkInteger,
       tkInt64   : Begin
                    Writeln('Get Int64...');
                    Int64(Value) := GetInt64Prop(FControl,PRI);
                   end;
       tkFloat  : Begin
                    Writeln('Get Float...');
                    Extended(Value) := GetFloatProp(FControl,PRI);
                   end;
       tkVariant  : Begin
                    Writeln('Get Variant...');
                    Variant(Value) := GetVariantProp(FControl,PRI);
                   end;
       tkMethod  : Begin
                    Writeln('Get Method...');
                    TMethod(Value) := GetMethodProp(FControl,PRI);
                   end;
         else
          Result := False;
       end;//case
end;





Function TComponentInterface.GetPPropInfoByIndex(Index:Integer): PPropInfo;
var
  PT : PTypeData;
  PP : PPropList;
  PI : PTypeInfo;
Begin
  PI := FControl.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
    Result:=PP^[index]
    else
    Result := nil;

   Freemem(PP);
end;

Function TComponentInterface.GetPPropInfoByName(Name:String): PPropInfo;
var
  PT : PTypeData;
  PP : PPropList;
  PI : PTypeInfo;
  I  : Longint;
Begin
  Name := Uppercase(name);
  PI := FControl.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  I := -1;
  repeat
    inc(i);
  until (PP^[i]^.Name = Name) or (i = PT^.PropCount-1);

  if PP^[i]^.Name = Name then
    Result:=PP^[i]
  else
    Result := nil;

   Freemem(PP);
end;

Function TComponentInterface.GetComponentType    : String;
Begin
  Result:=FControl.ClassName;
end;

Function TComponentInterface.GetComponentHandle  : LongInt;
Begin
//return the TWinControl handle?
  if (FControl is TWinControl) then
  Result := TWinControl(FControl).Handle;
end;

Function TComponentInterface.GetParent : TIComponentInterface;
Begin
  result := nil;
  if (FCOntrol is TControl) then
  if TControl(FControl).Parent <> nil then
  begin
     Result := FFormEditor.FindComponent(TControl(FControl).Parent);
  end;
end;

Function TComponentInterface.IsTControl : Boolean;
Begin
  Result := (FControl is TControl);
end;

Function TComponentInterface.GetPropCount : Integer;
var
  PT : PTypeData;
Begin
  PT:=GetTypeData(FControl.ClassInfo);
  Result := PT^.PropCount;
end;

Function TComponentInterface.GetPropType(Index : Integer) : TTypeKind;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
Begin
  PI:=FControl.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
      Result := PP^[Index]^.PropType^.Kind
      else
      Result := tkUnknown;

  freemem(PP);
end;

Function TComponentInterface.GetPropTypeInfo(Index : Integer) : PTypeInfo;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
Begin
  PI:=FControl.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
      Result := PP^[Index]^.PropType
      else
      Result := nil;
  freemem(PP);
end;


{This returns "Integer" or "Boolean"}
Function TComponentInterface.GetPropTypeName(Index : Integer) : String;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
Begin
  PI:=FControl.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
      Result := PP^[Index]^.PropType^.Name
      else
      Result := '';
  freemem(PP);
end;


{This returns "Left" "Align" "Visible"}
Function TComponentInterface.GetPropName(Index : Integer) : String;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
Begin
  PI:=FControl.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
//      Result := PP^[Index]^.PropType^.Name
      Result := PP^[Index]^.Name
      else
      Result := '';
  freemem(PP);
end;

Function TComponentInterface.GetPropTypebyName(Name : String) : TTypeKind;
var
  PT  : PTypeData;
  PP  : PPropList;
  PI  : PTypeInfo;
  I   : Longint;
Begin
  PI:=FControl.ClassInfo;
  PT:=GetTypeData(PI);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);

  Result := tkUnknown;
  For I:=0 to PT^.PropCount-1 do
  If PP^[i]<>Nil then
  begin
    if PP^[i]^.Name = Name then
    begin
      Result := PP^[i]^.PropType^.Kind;
      Break;
    end;
  end;

  freemem(PP);
end;

Function TComponentInterface.GetPropValue(Index : Integer; var Value) : Boolean;
var
PP : PPropInfo;
Begin
PP := GetPPropInfoByIndex(Index);
Result := FGetProp(PP,Value);
end;

Function TComponentInterface.GetPropValuebyName(Name: String; var Value) : Boolean;
var
PRI : PPropInfo;
Begin
Result := False;
PRI := GetPPropInfoByName(Name);

if PRI <> nil then
Result := FGetProp(PRI,Value);
end;

Function TComponentInterface.SetProp(Index : Integer; const Value) : Boolean;
var
PRI : PPropInfo;
Begin
  Result := False;
  PRI := GetPPropInfoByIndex(Index);
  if PRI <> nil then
      Begin
        Result := FSetProp(PRI,Value);
      end;
end;


Function TComponentInterface.SetPropbyName(Name : String; const Value) : Boolean;
var
  PRI : PPropInfo;
Begin
  Writeln('SetPropByName Name='''+Name+'''');
  Result := False;

  PRI := GetPropInfo(FControl.ClassInfo,Name);
  if PRI <> nil then
  Begin
    Result :=FSetProp(PRI,Value);
  end;

if Result = true then
Writeln('SETPROPBYNAME result = true')
else
Writeln('SETPROPBYNAME result = false');

end;

Function TComponentInterface.GetControlCount: Integer;
Begin
  // XXX Todo:
  Result := -1;
end;

Function TComponentInterface.GetControl(Index : Integer): TIComponentInterface;
Begin
  // XXX Todo:
  Result := nil;
end;

Function TComponentInterface.GetComponentCount: Integer;
Begin
  // XXX Todo:
   Result := -1;
end;

Function TComponentInterface.GetComponent(Index : Integer): TIComponentInterface;
Begin
  // XXX Todo:
  Result := nil;
end;

Function TComponentInterface.Select : Boolean;
Begin
  // XXX Todo:
  Result := False;
end;

Function TComponentInterface.Focus : Boolean;
Begin
  Result := False;
  if (FCOntrol is TWinControl) and (TWinControl(FControl).CanFocus) then
  Begin
    TWinControl(FControl).SetFocus;
    Result := True;
  end;
end;

Function TComponentInterface.Delete : Boolean;
Begin
   Control.Destroy;
   Destroy;
   Result := True;
end;


{TCustomFormEditor}

constructor TCustomFormEditor.Create;
begin
  inherited Create;
  FComponentInterfaceList := TList.Create;
  FSelectedComponents := TComponentSelectionList.Create;
  JITFormList := TJITForms.Create;
  JITFormList.RegCompList := RegCompList;
end;

destructor TCustomFormEditor.Destroy;
begin
  JITFormList.Destroy;
  FComponentInterfaceList.Free;
  FSelectedComponents.Free;
  inherited;
end;

Function TCustomFormEditor.AddSelected(Value : TComponent) : Integer;
Begin
  FSelectedComponents.Add(Value);
  Result := FSelectedComponents.Count;
  // call the OI to update it's selected.
  writeln('[TCustomFormEditor.AddSelected] '+Value.Name);
  Obj_Inspector.Selections := FSelectedComponents;
end;

Procedure TCustomFormEditor.DeleteControl(Value : TComponent);
var
  Temp : TComponentInterface;
Begin
  Temp := TComponentInterface(FindComponent(Value));
  if Temp <> nil then
     begin
       Writeln('1');
       RemoveFromComponentInterfaceList(Temp);
       Writeln('2');
       if (Value is TCustomForm) then
           begin
            JITFormList.DestroyJITFOrm(TForm(Value));
            Temp.Destroy;
           end
           else
           Temp.Delete;
       Writeln('3');
     end;
end;


Function TCustomFormEditor.Filename : String;
begin
  Result := 'testing.pp';
end;

Function TCustomFormEditor.FormModified : Boolean;
Begin
  Result := FModified;
end;

Function TCustomFormEditor.FindComponentByName(const Name : String) : TIComponentInterface;
Var
  Num : Integer;
Begin
  Num := 0;
  While Num < FComponentInterfaceList.Count do
  Begin
    Result := TIComponentInterface(FComponentInterfaceList.Items[Num]);
    if Upcase(TComponentInterface(Result).FControl.Name) = UpCase(Name) then
      exit;
    inc(num);
  end;
  Result:=nil;
end;

Function TCustomFormEditor.FindComponent(AComponent:TComponent): TIComponentInterface;
Var
  Num : Integer;
Begin
  Num := 0;
  While Num < FComponentInterfaceList.Count do
  Begin
    Result := TIComponentInterface(FComponentInterfaceList.Items[Num]);
    if TComponentInterface(Result).FControl = AComponent then exit;
    inc(num);
  end;
  Result:=nil;
end;

Function TCustomFormEditor.CreateComponent(ParentCI : TIComponentInterface;
TypeClass : TComponentClass;  X,Y,W,H : Integer): TIComponentInterface;
Var
  Temp : TComponentInterface;
  TempName    : String;
  Found : Boolean;
  I, Num,NewFormIndex : Integer;
  CompLeft, CompTop, CompWidth, CompHeight: integer;
  DummyComponent:TComponent;
Begin
  writeln('[TCustomFormEditor.CreateComponent] Class='''+TypeClass.ClassName+'''');
  Temp := TComponentInterface.Create;
  if Assigned(ParentCI) then
  begin
    if (not(TComponentInterface(ParentCI).FControl is TCustomForm)) and
       Assigned(TComponentInterface(ParentCI).FControl.Owner) then
      Temp.FControl := TypeClass.Create(TComponentInterface(ParentCI).FControl.Owner)
    else
      Temp.FControl := TypeClass.Create(TComponentInterface(ParentCI).FControl)
  end
   else
  Begin
    //this should be a form
       NewFormIndex := JITFormList.AddNewJITForm;
       if NewFormIndex >= 0 then
       Temp.FControl := JITFormList[NewFormIndex];
  end;

  if Assigned(ParentCI) then
    Begin
      if (TComponentInterface(ParentCI).FControl is TWinControl)
      and (csAcceptsControls in
        TWinControl(TComponentInterface(ParentCI).FControl).ControlStyle) then
      begin
        TWinControl(Temp.FControl).Parent :=
          TWinControl(TComponentInterface(ParentCI).FControl);
        writeln('Parent is '''+TWinControl(Temp.FControl).Parent.Name+'''');
      end
      else
      begin
        TWinControl(Temp.FControl).Parent :=
          TWinControl(TComponentInterface(ParentCI).FControl).Parent;
        writeln('Parent is '''+TWinControl(Temp.FControl).Parent.Name+'''');
      end;
    end;

  if ParentCI <> nil then
   Begin
    Writeln('ParentCI <> nil');
    TempName := Temp.FControl.ClassName;
    delete(TempName,1,1);
    writeln('TempName is '''+TempName+'''');
    Num := 0;
    Found := True;
    While Found do
    Begin
      Found := False;
      inc(num);
      for I := 0 to FComponentInterfaceList.Count-1 do
      begin
        DummyComponent:=TComponent(TComponentInterface(FComponentInterfaceList.Items[i]).FControl);
        if UpCase(DummyComponent.Name)=UpCase(TempName+IntToStr(Num)) then
        begin
          Found := True;
          break;
        end;
      end;
    end;
    Temp.FControl.Name := TempName+IntToStr(Num);
    Writeln('TempName + num = '+TempName+Inttostr(num));
   end;

  if (Temp.FControl is TControl) then
  Begin
    CompLeft:=X;
    CompTop:=Y;
    CompWidth:=W;
    CompHeight:=H;
    if CompWidth<=0 then CompWidth:=TControl(Temp.FControl).Width;
    if CompHeight<=0 then CompHeight:=TControl(Temp.FControl).Height;
    if CompLeft<0 then
      CompLeft:=(TControl(Temp.FControl).Parent.Width + CompWidth) div 2;
    if CompTop<0 then
      CompTop:=(TControl(Temp.FControl).Parent.Height+ CompHeight) div 2;
    TControl(Temp.FControl).SetBounds(CompLeft,CompTop,CompWidth,CompHeight);
  end;

  FComponentInterfaceList.Add(Temp);

  Result := Temp;
end;

Procedure TCustomFormEditor.RemoveFromComponentInterfaceList(Value :TIComponentInterface);
Begin
  if (FComponentInterfaceList.IndexOf(Value) <> -1) then
      FComponentInterfaceList.Delete(FComponentInterfaceList.IndexOf(Value));

end;


Function TCustomFormEditor.GetFormComponent : TIComponentInterface;
Begin
  //this can only be used IF you have one FormEditor per form.  I currently don't
result := nil;
end;

Procedure TCustomFormEditor.ClearSelected;
Begin
  FSelectedComponents.Clear;
end;

Function TCustomFormEditor.NewFormFromLFM(_Filename : String): TCustomForm;
var
  BinStream: TMemoryStream;
  TxtStream : TFileStream;
  Index    : Integer;
Begin
  Writeln('[NewFormFromLFM]');
  result := nil;
  try
    BinStream := TMemoryStream.Create;
      try
        TxtStream:= TFileStream.Create(_Filename,fmOpenRead);
        try
          ObjectTexttoBinary(TxtStream,BinStream);
        finally
          TxtStream.Free;
        end;
        BinStream.Position := 0;
        Writeln('[NewFormFromLFM] calling AddJITFORMFromStream');
        Index := JITFormList.AddJITFormFromStream(binStream);
        Writeln('[NewFormFromLFM] index='+inttostr(index));
        Result := JITFormList[Index];
      finally
        BinStream.Free;
      end;
     except
        //some error raised
     end;


end;

Function TCustomFormEditor.CreateControlComponentInterface(Control: TComponent) :TIComponentInterface;
var
  Temp : TComponentInterface;

Begin
  Temp := TComponentInterface.Create;
  Temp.FControl := Control;
  FComponentInterfaceList.Add(Temp);
  Result := Temp;

end;

end.
