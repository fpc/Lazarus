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
  classes, abstractformeditor, controls,prop_edits,Typinfo,Object_Inspector;

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
        Function FSetProp(Name : String; PRI : PPropInfo; const Value) : Boolean;
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

  public

    constructor Create;
    destructor Destroy; override;

    Function AddSelected(Value : TComponent) : Integer;
    Function Filename : String; override;
    Function FormModified : Boolean; override;
    Function FindComponent(const Name : String) : TIComponentInterface; override;
    Function GetFormComponent : TIComponentInterface; override;
//    Function CreateComponent(CI : TIComponentInterface; TypeName : String;
    Function CreateComponent(CI : TIComponentInterface; TypeClass : TComponentClass;
                             X,Y,W,H : Integer): TIComponentInterface; override;
    Procedure ClearSelected;
    property SelectedComponents : TComponentSelectionList read FSelectedComponents write FSelectedComponents;
    property Obj_Inspector : TObjectInspector read FObj_Inspector write FObj_Inspector;

  end;


implementation
uses
  SysUtils;



{TComponentInterface}

constructor TComponentInterface.Create;
begin
inherited;
end;

destructor TComponentInterface.Destroy;
begin
inherited;
end;

Function TComponentInterface.FSetProp(Name : String; PRI : PPropInfo; const Value) : Boolean;
Begin
       case PRI^.PropType^.Kind of
       tkBool: SetOrdProp(FControl,PRI,longint(Value));
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
PRI := GetPropInfo(FControl.ClassInfo,Name);
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
      Begin
        Result := FSetProp(PRI^.Name,PRI,Value);
      end;
end;

Function TComponentInterface.SetPropbyName(Name : String; const Value) : Boolean;
var
PRI : PPropInfo;
Begin
Writeln('*************');
Writeln('SetPropByName');
Result := False;

PRI := GetPropInfo(FControl.ClassInfo,Name);
if PRI <> nil then
   Begin
   Result :=FSetProp(Name,PRI,Value);
   end;

Writeln('*************');

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
inherited Create;
FComponentInterfaceList := TList.Create;
FSelectedComponents := TComponentSelectionList.Create;
end;

destructor TCustomFormEditor.Destroy;
begin
inherited;
FComponentInterfaceList.Destroy;
FSelectedComponents.Destroy;
end;

Function TCustomFormEditor.AddSelected(Value : TComponent) : Integer;
Begin
Result := -1;
FSelectedComponents.Add(Value);
Result := FSelectedComponents.Count;
//call the OI to update it's selected.
Obj_Inspector.Selections := FSelectedComponents;
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

//Function TCustomFormEditor.CreateComponent(CI : TIComponentInterface; TypeName : String;
Function TCustomFormEditor.CreateComponent(CI : TIComponentInterface; TypeClass : TComponentClass;
                             X,Y,W,H : Integer): TIComponentInterface;
Var
Temp : TComponentInterface;
TempInterface : TComponentInterface;
TempClass    : TPersistentClass;
TempName    : String;
Found : Boolean;
I, Num : Integer;
Begin
Temp := TComponentInterface.Create;
Writeln('TComponentInterface created......');
if SelectedComponents.Count = 0 then
Temp.FControl := TypeClass.Create(nil)
else
Begin
Writeln('Selected Components > 0');
if (SelectedComponents.Items[0] is TWinControl) and (csAcceptsControls in TWinControl(SelectedComponents.Items[0]).ControlStyle) then
    Begin
       Writeln('The Control is a TWinControl and it accepts controls');
       Writeln('The owners name is '+TWinControl(SelectedComponents.Items[0]).Name);
       Temp.FControl := TypeClass.Create(SelectedComponents.Items[0]);
    end
    else
    Begin
       Writeln('The Control is not a TWinControl or it does not accept controls');
       Temp.FControl := TypeClass.Create(SelectedComponents.Items[0].Owner);
    end;
end;


Writeln('4');

  if Assigned(CI) then
     Begin
        if (TComponentInterface(CI).FControl is TWinControl) and
           (csAcceptsControls in TWinControl(TComponentInterface(CI).FControl).COntrolStyle)then
            begin
               TWinControl(Temp.FControl).Parent := TWinControl(TComponentInterface(CI).FControl);
            end
            else
               TWinControl(Temp.FControl).Parent := TWinControl(TComponentInterface(CI).FControl).Parent;


     End
     else
     Begin //CI is not assigned so check the selected control
     Writeln('CI is not assigned....');
     if SelectedComponents.Count > 0 then
        Begin
            Writeln('CI is not assigned but something is selected....');
            TempInterface := TComponentInterface(FindComponent(SelectedComponents.Items[0].Name));
            Writeln('The selected control is....'+TempInterface.FControl.Name);

            if (TempInterface.FControl is TWinControl) and
               (csAcceptsControls in TWinControl(TempInterface.FControl).ControlStyle)then
                  Begin
                  Writeln('The selected control IS a TWincontrol and accepts controls');
                  TWinControl(Temp.FControl).Parent := TWinControl(TempInterface.FControl);
                  end
                  else
                  TWinControl(Temp.FControl).Parent := TWinControl(TempInterface.FControl).Parent;
        end
     end;
Writeln('5');


TempName := Temp.FControl.ClassName;
delete(TempName,1,1);
writeln('TempName is ....'+TempName);
Found := True;
Num := 0;
While Found do
   Begin
   Found := False;
   inc(num);
   Writeln('NUm = '+inttostr(num));
   for I := 0 to FComponentInterfaceList.Count-1 do
       begin
       if TComponent(TComponentInterface(FComponentInterfaceList.Items[i]).FControl).Name = TempName+inttostr(Num) then
          begin
             Found := True;
             break;
          end;
       end;
   end;
Temp.FControl.Name := TempName+Inttostr(num);
Writeln('TempName + num = '+TempName+Inttostr(num));


if (Temp.FControl is TControl) then
 Begin
 if (X <> -1) and (Y <> -1) and (W <> -1) and (H <> -1) then
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


FComponentInterfaceList.Add(Temp);

 Result := Temp;
end;

Function TCustomFormEditor.GetFormComponent : TIComponentInterface;
Begin
//this can only be used IF you have one FormEditor per form.  I currently don't
end;

Procedure TCustomFormEditor.ClearSelected;
Begin
FSelectedComponents.Clear;
end;


end.
