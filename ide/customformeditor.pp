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
        Function GetPropbyIndex(Index : Integer) : PPropInfo;
        MySetProc : TSetPRoc;
        MyGetProc : TGetProc;
      public
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
    FControlClass : TControlClass;
    FMainControl  : TControl;
    FModified     : Boolean;
    FComponentInterfaceList : TList; //used to track and find controls on the form
    Function GetMainControl : TControl;
  protected

  public

    constructor Create; virtual;
    destructor Destroy; override;

    Function Filename : String; override;
    Function FormModified : Boolean; override;
    Function FindComponent(const Name : String) : TIComponentInterface; override;

    property ControlClass : TControlClass read FControlClass write FControlClass;
    property MainControl : TControl read GetMainControl;
  end;


implementation

{TComponentInterface}
Function TComponentInterface.GetPropByIndex(Index:Integer): PPropInfo;
Begin

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
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
PRI : PPropInfo;
J : Longint;
Num : Integer;
Begin
  PT:=GetTypeData(FControl.ClassInfo);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  result := False;
  if Index < PT^.PropCount then
      begin
        pri:=PP^[index];
        with PRI^ do
            Begin
               Result := True;
               If (Proptype^.kind in Ordinaltypes) Then
                  begin
                    J:=GetOrdProp(FControl,pri);
                    If PropType^.Kind=tkenumeration then
                       Value := GetEnumName(Proptype,J)
	            else
                       Value := J;
                  end
               else
               Case pri^.proptype^.kind of
                  tkfloat :  begin
                     Value := GetFloatProp(FControl,pri);
                     end;
                  tkAstring : begin
                     Value := GetStrProp(FControl,Pri);
                     end;
                   else
                     Begin
                      Value := -1;
                      Result := False;
                     end;
               end;  //end of the CASE

            end;  //end of the with PRI^...
      end;  //end of If Index < PT

  freemem(PP);
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
  PT:=GetTypeData(FControl.ClassInfo);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  result := -1;
  I := -1;
  repeat
   inc(i);
  until (PP^[i]^.Name = Name) or (i > PT^.PropCount-1);

  if PP^[i]^.Name = Name then
      begin
        pri:=PP^[i];
        with PRI^ do
            Begin
               If (Proptype^.kind in Ordinaltypes) Then
                  begin
                    J:=GetOrdProp(FControl,pri);
                    If PropType^.Kind=tkenumeration then
                       Result := GetEnumName(Proptype,J)
	            else
                       Result := J;
                  end
               else
               Case pri^.proptype^.kind of
                  tkfloat :  begin
                     Result := GetFloatProp(FControl,pri);
                     end;
                  tkAstring : begin
                     Result := GetStrProp(FControl,Pri);
                     end;
               end;  //end of the CASE
            end;  //end of the with PRI^...
      end;  //end of If Index < PT

  freemem(PP);

end;

Function TComponentInterface.SetProp(Index : Integer; const Value) : Boolean;

var
PT : PTypeData;
PP : PPropList;
PI : PTypeInfo;
PRI : PPropInfo;
J : Longint;
Num : Integer;
Begin
  PT:=GetTypeData(FControl.ClassInfo);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  result := -1;
  if Index < PT^.PropCount then
      begin
        pri:=PP^[i];
        with PRI^ do
            Begin
             if SetProc <> nil then
                Begin  //call the procedure passing Value
                 MySetProc := SetProc;
                 MySetProc(Value);
                end;
            end;
       end;

  freemem(PP);
end;

Function TComponentInterface.SetPropbyName(Name : String; const Value) : Boolean;
Begin

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

function TCustomFormEditor.GetMainControl: TControl;
begin
if not Assigned(FMainControl) then
   Begin
        FMainControl := FControlClass.Create(nil);
        FMainControl.Parent := nil;
   end;

result := FMainControl;
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


end.
