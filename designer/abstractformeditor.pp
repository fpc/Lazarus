{
 /***************************************************************************
                               AbstractFormEditor.pp
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
unit AbstractFormEditor;

{$mode objfpc}
interface

uses
  classes,typinfo;

type

{
Should I include typinfo.pp and use TTypeKind instead of TPropertyType
or use TPropertyType
 }

//  TPropertyType = (ptUnknown, ptInteger, ptChar, ptEnumeration,ptFloat,ptString,ptSet,
//                   ptClass, ptMethod,ptWChar, ptLString, LWString, ptVariant);

  TIComponentInterface = class
    public
      Function GetComponentType    : String; virtual; abstract;
      Function GetComponentHandle  : LongInt; virtual; abstract;
      Function GetParent           : TIComponentInterface; virtual; abstract;
      Function IsTControl          : Boolean; virtual; abstract;
      Function GetPropCount	   : Integer; virtual; abstract;
      Function GetPropType(Index : Integer) : TTypeKind; virtual; abstract;
//      Function GetPropType(Index : Integer) : TPropertyType; virtual; abstract;
      Function GetPropName(Index : Integer) : String; virtual; abstract;
      Function GetPropTypebyName(Name : String) : TTypeKind; virtual; abstract;
//      Function GetPropTypebyName(Name : String) : TPropertyType; virtual; abstract;

      Function GetPropValue(Index : Integer; var Value) : Boolean; virtual; abstract;
      Function GetPropValuebyName(Name: String; var Value) : Boolean; virtual; abstract;
      Function SetProp(Index : Integer; const Value) : Boolean; virtual; abstract;
      Function SetPropbyName(Name : String; const Value) : Boolean; virtual; abstract;

      Function GetControlCount: Integer; virtual; abstract;
      Function GetControl(Index : Integer): TIComponentInterface; virtual; abstract;

      Function GetComponentCount: Integer; virtual; abstract;
      Function GetComponent(Index : Integer): TIComponentInterface; virtual; abstract;

      Function Select : Boolean; virtual; abstract;
      Function Focus : Boolean; virtual; abstract;
      Function Delete : Boolean; virtual; abstract;
  end;


  TIFormInterface = class
    public
      Function Filename            : String; virtual; abstract;
      Function FormModified        : Boolean; virtual; abstract;
      Function MArkModified        : Boolean; virtual; abstract;
      Function GetFormComponent    : TIComponentInterface; virtual; abstract;
      Function FindComponent	   : TIComponentInterface; virtual; abstract;
      Function GetComponentfromHandle(ComponentHandle:Pointer): TIComponentInterface; virtual; abstract;

      Function GetSelCount: Integer; virtual; abstract;
      Function GetSelComponent(Index : Integer): TIComponentInterface; virtual; abstract;
     Function CreateComponent(CI : TIComponentInterface; TypeClass : TComponentClass;
                             X,Y,W,H : Integer): TIComponentInterface; virtual; abstract;
    end;

{
  Created by Shane Miller
  This unit defines the layout for the forms editor.  The forms editor is responsible
  for creating a form, holding a list of selected controls, determining if the form was
  modified, holding the filename for the form, and working wit the object inspector.
}

  TAbstractFormEditor = class
   public
     Function Filename : String; virtual; abstract;
     Function FormModified : Boolean; virtual; abstract;
     Function FindComponentByName(const Name : String) : TIComponentInterface; virtual; abstract;
     Function FindComponent(AComponent: TComponent): TIComponentInterface; virtual; abstract;

     Function GetFormComponent: TIComponentInterface; virtual; abstract;
     Function GetComponentByHandle(const Value : Longint): TIComponentInterface; virtual; abstract;

     Function GetSelCount : Integer; virtual; abstract;
     Function GetSelComponent(Index : Integer) : TIComponentInterface; virtual; abstract;

//     Function CreateComponent(CI : TIComponentInterface; TypeName : String;
     Function CreateComponent(CI : TIComponentInterface; TypeClass : TComponentClass;
                             X,Y,W,H : Integer): TIComponentInterface; virtual; abstract;
  end;


implementation

end.
