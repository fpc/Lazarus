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
  classes, abstractformeditor, controls,Typinfo;

Const OrdinalTypes = [tkInteger,tkChar,tkENumeration,tkbool];

type

{
TComponentInterface is derived from TIComponentInterface.  It gives access to
each control that's dropped onto the form
}

TCustomFormEditor = class; //forward declaration
TSetProc = Procedure (const Value) of Object;
TGetProc = Function : Variant of Object;


  TComponentInterface = class(TIComponentInterface)
      private
        FControl : TComponent;
        FFormEditor : TCustomFormEditor;  //used to call it's functions
      protected
        Function GetPPropInfobyIndex(Index : Integer) : PPropInfo;
        Function GetPPropInfobyName(Name : String) : PPropInfo;
        MySetProc : TSetPRoc;
        MyGetProc : TGetProc;
      public
        constructor Create;
        destructor Destroy; override;

        Function GetComponentType    : String; override;
        Function GetComponentHandle  : LongInt; override;
        Function GetParent           : TIComponentInterface; override;
        Function IsTControl          : Boolean; override;
        Function GetPropCount	   : Integer; override;
        Function GetPropType(Index : Integer) : TTypeKind; override;
        Function GetPropName(Index : Integer) : String; override;
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
  end;

{
TCustomFormEditor
  One is created whenever a "NEw Form" is created.  The Form is contained in the MainControl
  property.  FComponentClass tells whether this container is a TFORM or a TDataModule, or
  something else new.
}

 TControlClass = class of TControl;

 TCustomFormEditor = class(TAbstractFormEditor)
  private
    FModified     : Boolean;
    FComponentInterfaceList : TList; //used to track and find controls
    FMainControl : TComponent;  //this needs to be recorded here so when a control
                                //is created via the CreateComponent we can use this
                                //to set the owner property.
  protected

  public

    constructor Create;
    destructor Destroy; override;

    Function Filename : String; override;
    Function FormModified : Boolean; override;
    Function FindComponent(const Name : String) : TIComponentInterface; override;
    Function CreateComponent(CI : TIComponentInterface; TypeName : String;
                             X,Y,W,H : Integer): TIComponentInterface; override;
    property MainControl : TComponent read FMainControl write FMainControl;
  end;


implementation

{TComponentInterface}

constructor TComponentInterface.Create;
begin
inherited;
end;

destructor TComponentInterface.Destroy;
begin
inherited;
end;

Function TComponentInterface.GetPPropInfoByIndex(Index:Integer): PPropInfo;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
Begin
  PT:=GetTypeData(FControl.ClassInfo);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
        Result:=PP^[index]
        else
        Result := nil;

//does freeing this kill my result?  Check this...
//   Freemem(PP);
end;

Function TComponentInterface.GetPPropInfoByName(Name:String): PPropInfo;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
I  : Longint;
Begin
  PT:=GetTypeData(FControl.ClassInfo);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  I := -1;
  repeat
   inc(i);
  until (PP^[i]^.Name = Name) or (i > PT^.PropCount-1);

  if PP^[i]^.Name = Name then
        Result:=PP^[i]
        else
        Result := nil;

//does freeing this kill my result?  Check this...
//   Freemem(PP);
end;


Function TComponentInterface.GetComponentType    : String;
Begin
//???What do I return? TObject's Classtype?
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
   Result := FFormEditor.FindComponent(TControl(FControl).Parent.Name);
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
Num : Integer;
Begin
  PT:=GetTypeData(FControl.ClassInfo);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
      Result := PP^[Index]^.PropType^.Kind
      else
      Result := tkUnknown;

  freemem(PP);
end;

Function TComponentInterface.GetPropName(Index : Integer) : String;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
Num : Integer;
Begin
  PT:=GetTypeData(FControl.ClassInfo);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  if Index < PT^.PropCount then
      Result := PP^[Index]^.PropType^.Name
      else
      Result := '';
  freemem(PP);

end;

Function TComponentInterface.GetPropTypebyName(Name : String) : TTypeKind;
var
PT  : PTypeData;
PP  : PPropList;
PI  : PTypeInfo;
Num : Integer;
I   : Longint;
Begin
  PT:=GetTypeData(FControl.ClassInfo);
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

PRI : PPropInfo;
J : Longint;
Num : Integer;
Begin
PRI := GetPPropInfoByIndex(Index);
if PRI <> nil then
        with PRI^ do
            Begin
               Result := True;
               If (Proptype^.kind in Ordinaltypes) Then
                  begin
                    J:=GetOrdProp(FControl,pri);
                    If PropType^.Kind=tkenumeration then
                       String(Value) := GetEnumName(Proptype,J)
	            else
                       Integer(Value) := J;
                  end
               else
               Case pri^.proptype^.kind of
                  tkfloat :  begin
                     Real(Value) := GetFloatProp(FControl,pri);
                     end;
                  tkAstring : begin
                     AnsiString(Value) := GetStrProp(FControl,Pri);
                     end;
                   else
                     Begin
                      Real(Value) := -1;
                      Result := False;
                     end;
               end;  //end of the CASE

            end;  //end of the with PRI^...
end;

Function TComponentInterface.GetPropValuebyName(Name: String; var Value) : Boolean;
var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
PRI : PPropInfo;
I,J : Longint;
Num : Integer;
Begin

PRI := GetPPropInfoByName(Name);
if PRI <> nil then
        with PRI^ do
            Begin
               Result := True;
               If (Proptype^.kind in Ordinaltypes) Then
                  begin
                    J:=GetOrdProp(FControl,pri);
                    If PropType^.Kind=tkenumeration then
                       String(Value) := GetEnumName(Proptype,J)
	            else
                       Integer(Value) := J;
                  end
               else
               Case pri^.proptype^.kind of
                  tkfloat :  begin
                     Real(Value) := GetFloatProp(FControl,pri);
                     end;
                  tkAstring : begin
                     AnsiString(Value) := GetStrProp(FControl,Pri);
                     end;
                  else
                     Begin
                     Result := False;
                     Integer(Value) := -1;
                     end;
               end;  //end of the CASE
            end;  //end of the with PRI^...
end;

Function TComponentInterface.SetProp(Index : Integer; const Value) : Boolean;
var
PRI : PPropInfo;
Begin
  Result := False;
  PRI := GetPPropInfoByIndex(Index);
  if PRI <> nil then
        with PRI^ do
            Begin
             if SetProc <> nil then
                Begin  //call the procedure passing Value
                 MySetProc := TSetProc(SetProc^);
                 MySetProc(Value);
                 Result := True;
                end;
            end;
end;

Function TComponentInterface.SetPropbyName(Name : String; const Value) : Boolean;
var
PRI : PPropInfo;
Begin
  Result := False;
  PRI := GetPPropInfoByName(Name);
  if PRI <> nil then
        with PRI^ do
            Begin
             if SetProc <> nil then
                Begin  //call the procedure passing Value
                 MySetProc := TSetProc(SetProc^);
                 MySetProc(Value);
                 Result := True;
                end;
            end;

end;


Function TComponentInterface.GetControlCount: Integer;
Begin

end;

Function TComponentInterface.GetControl(Index : Integer): TIComponentInterface;
Begin

end;


Function TComponentInterface.GetComponentCount: Integer;
Begin

end;

Function TComponentInterface.GetComponent(Index : Integer): TIComponentInterface;
Begin

end;


Function TComponentInterface.Select : Boolean;
Begin

end;

Function TComponentInterface.Focus : Boolean;
Begin

end;

Function TComponentInterface.Delete : Boolean;
Begin

end;




{TCustomFormEditor}

constructor TCustomFormEditor.Create;
begin
FComponentInterfaceList := TList.Create;
inherited;
end;

destructor TCustomFormEditor.Destroy;
begin
FComponentInterfaceList.Destroy;
inherited;
end;


Function TCustomFormEditor.Filename : String;
begin
Result := 'testing.pp';
end;

Function TCustomFormEditor.FormModified : Boolean;
Begin
Result := FModified;
end;

Function TCustomFormEditor.FindComponent(const Name : String) : TIComponentInterface;
Var
  Num : Integer;
Begin
  Num := 0;
  While Num < FComponentInterfaceList.Count do
      Begin
        Result := TIComponentInterface(FComponentInterfaceList.Items[Num]);
        if TComponentInterface(Result).FControl.Name = Name then break;
        inc(num);
      end;
end;

Function TCustomFormEditor.CreateComponent(CI : TIComponentInterface; TypeName : String;
                             X,Y,W,H : Integer): TIComponentInterface;
Var
Temp : TComponentInterface;
Begin
Temp := TComponentInterface.Create;
Temp.FControl := TControlClass(TypeName).Create(MainControl);
  if Assigned(CI) then
     Begin
        if (TComponentInterface(CI).FControl is TWinControl) then
            begin
            if (csAcceptsControls in TWinControl(TComponentInterface(CI).FControl).COntrolStyle) then
                 Begin  //set CI the parent of the new one.
                   TWinControl(Temp.FControl).Parent := TWinControl(TComponentInterface(CI).FControl);
                 end;
            end;

     End;

if (Temp.FControl is TControl) then
Begin
if (X <> -1) and (Y <> -1) and (W <> -1) and (h <> -1) then
   TControl(Temp.FControl).SetBounds(X,Y,W,H)
else
   Begin
   if (W <> -1) then TControl(Temp.FControl).Width := W;  //if W=-1 then use default size otherwise use W

   if (H <> -1) then TControl(Temp.FControl).Height := H; //if H=-1 then use default size otherwise use H

   if (X <> -1) then TControl(Temp.FControl).Left := X //if X=-1 then center in parent otherwise use X
       else
       TControl(Temp.FControl).Left := (TControl(Temp.FControl).Parent.Width div 2) - (TControl(Temp.FControl).Width div 2);

   if (Y <> -1) then TControl(Temp.FControl).Top := Y //if Y=-1 then center in parent otherwise use Y
       else
       TControl(Temp.FControl).Top := (TControl(Temp.FControl).Parent.Height div 2) - (TControl(Temp.FControl).Height div 2);
   end;

end;

 Result := Temp;
end;



end.
