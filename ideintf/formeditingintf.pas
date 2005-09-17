{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Shane Miller, Mattias Gaertner

  Abstract:
    Methods to access the form editing of the IDE.
}
unit FormEditingIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Forms, Controls, ComponentEditors;
  
const
  ComponentPaletteBtnWidth  = 25;
  ComponentPaletteBtnHeight = 25;

type
  { TIComponentInterface }

  TIComponentInterface = class
  protected
    FComponent : TComponent;
  public
    Function GetComponentType    : ShortString; virtual; abstract;
    Function GetComponentHandle  : LongInt; virtual; abstract;
    Function GetParent           : TIComponentInterface; virtual; abstract;
    Function IsTControl          : Boolean; virtual; abstract;
    Function GetPropCount	   : Integer; virtual; abstract;
    Function GetPropType(Index : Integer) : TTypeKind; virtual; abstract;
    // Function GetPropType(Index : Integer) : TPropertyType; virtual; abstract;
    Function GetPropName(Index : Integer) : Shortstring; virtual; abstract;
    Function GetPropTypeByName(Name : ShortString) : TTypeKind; virtual; abstract;
    // Function GetPropTypebyName(Name : ShortString) : TPropertyType; virtual; abstract;
    Function GetPropTypeName(Index : Integer) : ShortString; virtual; abstract;

    Function GetPropValue(Index : Integer; var Value) : Boolean; virtual; abstract;
    Function GetPropValuebyName(Name: Shortstring; var Value) : Boolean; virtual; abstract;
    Function SetProp(Index : Integer; const Value) : Boolean; virtual; abstract;
    Function SetPropbyName(Name : Shortstring; const Value) : Boolean; virtual; abstract;

    Function GetControlCount: Integer; virtual; abstract;
    Function GetControl(Index : Integer): TIComponentInterface; virtual; abstract;

    Function GetComponentCount: Integer; virtual; abstract;
    Function GetComponent(Index : Integer): TIComponentInterface; virtual; abstract;

    Function Select: Boolean; virtual; abstract;
    Function Focus: Boolean; virtual; abstract;
    Function Delete: Boolean; virtual; abstract;

    property Component: TComponent read FComponent;
  end;


  { TIFormInterface }

  TIFormInterface = class
  public
    Function Filename            : AnsiString; virtual; abstract;
    Function FormModified        : Boolean; virtual; abstract;
    Function MarkModified        : Boolean; virtual; abstract;
    Function GetFormComponent    : TIComponentInterface; virtual; abstract;
    Function FindComponent       : TIComponentInterface; virtual; abstract;
    Function GetComponentfromHandle(ComponentHandle:Pointer): TIComponentInterface; virtual; abstract;

    Function GetSelCount: Integer; virtual; abstract;
    Function GetSelComponent(Index : Integer): TIComponentInterface; virtual; abstract;
    Function CreateComponent(CI : TIComponentInterface; TypeClass : TComponentClass;
                             X,Y,W,H : Integer): TIComponentInterface; virtual; abstract;
  end;


  { TAbstractFormEditor }
  
  TAbstractFormEditor = class
  protected
    function GetDesigner(Index: integer): TIDesigner; virtual; abstract;
  public
    // components
    Function FindComponentByName(const Name: ShortString
                                 ): TIComponentInterface; virtual; abstract;
    Function FindComponent(AComponent: TComponent): TIComponentInterface; virtual; abstract;

    Function GetDefaultComponentParent(TypeClass: TComponentClass
                                       ): TIComponentInterface; virtual; abstract;
    Function GetDefaultComponentPosition(TypeClass: TComponentClass;
                                         ParentCI: TIComponentInterface;
                                         var X,Y: integer): boolean; virtual; abstract;
    Function CreateComponent(ParentCI: TIComponentInterface;
                             TypeClass: TComponentClass;
                             X,Y,W,H: Integer): TIComponentInterface; virtual; abstract;
    Function CreateComponentFromStream(BinStream: TStream;
                             AncestorType: TComponentClass;
                             const NewUnitName: ShortString;
                             Interactive: boolean): TIComponentInterface; virtual; abstract;
    Function CreateChildComponentFromStream(BinStream: TStream;
                                     ComponentClass: TComponentClass;
                                     Root: TComponent;
                                     ParentControl: TWinControl
                                     ): TIComponentInterface; virtual; abstract;

    // designers
    function DesignerCount: integer; virtual; abstract;
    property Designer[Index: integer]: TIDesigner read GetDesigner;
    function GetCurrentDesigner: TIDesigner; virtual; abstract;
    function GetDesignerForm(AComponent: TComponent): TCustomForm; virtual; abstract;
    function GetDesignerByComponent(AComponent: TComponent
                                    ): TIDesigner; virtual; abstract;

    // selection
    function SaveSelectionToStream(s: TStream): Boolean; virtual; abstract;
    function InsertFromStream(s: TStream; Parent: TWinControl;
                              Flags: TComponentPasteSelectionFlags
                              ): Boolean; virtual; abstract;
    function ClearSelection: Boolean; virtual; abstract;
    function DeleteSelection: Boolean; virtual; abstract;
    function CopySelectionToClipboard: Boolean; virtual; abstract;
    function CutSelectionToClipboard: Boolean; virtual; abstract;
    function PasteSelectionFromClipboard(Flags: TComponentPasteSelectionFlags
                                         ): Boolean; virtual; abstract;
  end;

type
  TDesignerIDECommandForm = class(TCustomForm)
    // dummy form class, use by the IDE commands for keys in the designers
  end;

var
  FormEditingHook: TAbstractFormEditor; // will be set by the IDE

implementation

end.

