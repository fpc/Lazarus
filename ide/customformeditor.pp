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

      public
      Function GetComponentType    : String; override;
      Function GetComponentHandle  : LongInt; override;
      Function GetParent           : TIComponentInterface; override;
      Function IsTControl          : Boolean; override;
      Function GetPropCount	   : Integer; override;
      Function GetPropType(Index : Integer) : TPropertyType; override;
      Function GetPropName(Index : Integer) : String; override;
      Function GetPropTypebyName(Name : String) : TPropertyType; override;

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
TypeInfo : PTypeInfo;
TypeKinds : TTypeKinds;
PropList : TPropList;
Begin
TypeKinds := [tkInteger,tkChar,tkEnumeration,tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord];

Result := GetPropList(TypeInfo,TypeKinds,@Proplist);
end;

Function TComponentInterface.GetPropType(Index : Integer) : TPropertyType;
var
TypeInfo : PTypeInfo;
TypeKinds : TTypeKinds;
PropList : TPropList;
PropInfo : TPropInfo;

Num : Integer;
Begin
TypeKinds := [tkUnknown,tkInteger,tkChar,tkEnumeration,tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord];

Num := GetPropList(TypeInfo,TypeKinds,@Proplist);

If NUm > 0 then
   Begin
   PropInfo := PropList[Index]^;
   TypeInfo := PropInfo.PropType;
   case TypeInfo^.kind of
      tkUnknown : Result := ptUnknown;
      tkInteger : Result := ptInteger;
      tkChar : Result := ptChar;
      tkEnumeration : Result := ptEnumeration;
      tkFloat : Result := ptFloat;
      tkSet : Result := ptSet;
      tkMethod : Result := ptMethod;
      tkSString : Result := ptString;
      tkLString : Result := ptLString;
      tkAString : Result := ptLString;
      tkWString : Result := ptLString;
      tkVariant : Result := ptVariant;
      tkClass : Result := ptClass;
      tkWChar : Result := ptWChar;
      else
        Result := ptUnknown
   end;
   end;
end;

Function TComponentInterface.GetPropName(Index : Integer) : String;
var
TypeInfo : PTypeInfo;
TypeKinds : TTypeKinds;
PropList : TPropList;
PropInfo : TPropInfo;

Num : Integer;
Begin
TypeKinds := [tkUnknown,tkInteger,tkChar,tkEnumeration,tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord];

Num := GetPropList(TypeInfo,TypeKinds,@Proplist);

If NUm > 0 then
   Begin
   PropInfo := PropList[Index]^;
   TypeInfo := PropInfo.PropType;
   Result := TypeInfo^.Name;
   end;

end;

Function TComponentInterface.GetPropTypebyName(Name : String) : TPropertyType;
var
TypeInfo : PTypeInfo;
TypeKinds : TTypeKinds;
PropList : TPropList;
PropInfo : TPropInfo;

Num : Integer;
Begin
TypeKinds := [tkUnknown,tkInteger,tkChar,tkEnumeration,tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord];

Num := GetPropInfo(TypeInfo,Name);

If NUm > 0 then
   Begin
   PropInfo := PropList[Index]^;
   TypeInfo := PropInfo.PropType;
   case TypeInfo^.kind of
      tkUnknown : Result := ptUnknown;
      tkInteger : Result := ptInteger;
      tkChar : Result := ptChar;
      tkEnumeration : Result := ptEnumeration;
      tkFloat : Result := ptFloat;
      tkSet : Result := ptSet;
      tkMethod : Result := ptMethod;
      tkSString : Result := ptString;
      tkLString : Result := ptLString;
      tkAString : Result := ptLString;
      tkWString : Result := ptLString;
      tkVariant : Result := ptVariant;
      tkClass : Result := ptClass;
      tkWChar : Result := ptWChar;
      else
        Result := ptUnknown
   end;
   end;


end;


Function TComponentInterface.GetPropValue(Index : Integer; var Value) : Boolean;
Begin

end;

Function TComponentInterface.GetPropValuebyName(Name: String; var Value) : Boolean;
Begin

end;

Function TComponentInterface.SetProp(Index : Integer; const Value) : Boolean;
Begin

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
