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
  Classes, SysUtils, TypInfo, Forms, Controls, ProjectIntf, ComponentEditors;
  
const
  ComponentPaletteImageWidth = 24;
  ComponentPaletteImageHeight = 24;
  ComponentPaletteBtnWidth  = ComponentPaletteImageWidth + 3;
  ComponentPaletteBtnHeight = ComponentPaletteImageHeight + 3;

type
  { TIComponentInterface }

  TIComponentInterface = class
  protected
    FComponent : TComponent;
  public
    function GetComponentType    : ShortString; virtual; abstract;
    function GetComponentHandle  : LongInt; virtual; abstract;
    function GetParent           : TIComponentInterface; virtual; abstract;
    function IsTControl          : Boolean; virtual; abstract;
    function GetPropCount	   : Integer; virtual; abstract;
    function GetPropType(Index : Integer) : TTypeKind; virtual; abstract;
    // function GetPropType(Index : Integer) : TPropertyType; virtual; abstract;
    function GetPropName(Index : Integer) : Shortstring; virtual; abstract;
    function GetPropTypeByName(Name : ShortString) : TTypeKind; virtual; abstract;
    // function GetPropTypebyName(Name : ShortString) : TPropertyType; virtual; abstract;
    function GetPropTypeName(Index : Integer) : ShortString; virtual; abstract;

    function GetPropValue(Index : Integer; var Value) : Boolean; virtual; abstract;
    function GetPropValuebyName(Name: Shortstring; var Value) : Boolean; virtual; abstract;
    function SetProp(Index : Integer; const Value) : Boolean; virtual; abstract;
    function SetPropbyName(Name : Shortstring; const Value) : Boolean; virtual; abstract;

    function GetControlCount: Integer; virtual; abstract;
    function GetControl(Index : Integer): TIComponentInterface; virtual; abstract;

    function GetComponentCount: Integer; virtual; abstract;
    function GetComponent(Index : Integer): TIComponentInterface; virtual; abstract;

    function Select: Boolean; virtual; abstract;
    function Focus: Boolean; virtual; abstract;
    function Delete: Boolean; virtual; abstract;

    property Component: TComponent read FComponent;
  end;


  { TIFormInterface }

  TIFormInterface = class
  public
    function Filename            : AnsiString; virtual; abstract;
    function FormModified        : Boolean; virtual; abstract;
    function MarkModified        : Boolean; virtual; abstract;
    function GetFormComponent    : TIComponentInterface; virtual; abstract;
    function FindComponent       : TIComponentInterface; virtual; abstract;
    function GetComponentfromHandle(ComponentHandle:Pointer): TIComponentInterface; virtual; abstract;

    function GetSelCount: Integer; virtual; abstract;
    function GetSelComponent(Index : Integer): TIComponentInterface; virtual; abstract;
    function CreateComponent(CI : TIComponentInterface; TypeClass : TComponentClass;
                             X,Y,W,H : Integer): TIComponentInterface; virtual; abstract;
  end;


  { TAbstractFormEditor }
  
  TAbstractFormEditor = class
  protected
    function GetDesignerBaseClasses(Index: integer): TComponentClass; virtual; abstract;
    function GetDesigner(Index: integer): TIDesigner; virtual; abstract;
  public
    // components
    function FindComponentByName(const Name: ShortString
                                 ): TIComponentInterface; virtual; abstract;
    function FindComponent(AComponent: TComponent): TIComponentInterface; virtual; abstract;

    function GetDefaultComponentParent(TypeClass: TComponentClass
                                       ): TIComponentInterface; virtual; abstract;
    function GetDefaultComponentPosition(TypeClass: TComponentClass;
                                         ParentCI: TIComponentInterface;
                                         var X,Y: integer): boolean; virtual; abstract;
    function CreateComponent(ParentCI: TIComponentInterface;
                             TypeClass: TComponentClass;
                             const AUnitName: shortstring;
                             X,Y,W,H: Integer): TIComponentInterface; virtual; abstract;
    function CreateComponentFromStream(BinStream: TStream;
                      AncestorType: TComponentClass; AncestorBinStream: TStream;
                      const NewUnitName: ShortString;
                      Interactive: boolean;
                      Visible: boolean = true): TIComponentInterface; virtual; abstract;
    function CreateChildComponentFromStream(BinStream: TStream;
                                     ComponentClass: TComponentClass;
                                     Root: TComponent;
                                     ParentControl: TWinControl
                                     ): TIComponentInterface; virtual; abstract;

    // ancestors
    function GetAncestorLookupRoot(AComponent: TComponent): TComponent; virtual; abstract;
    function GetAncestorInstance(AComponent: TComponent): TComponent; virtual; abstract;
    function RegisterDesignerBaseClass(AClass: TComponentClass): integer; virtual; abstract;
    function DesignerBaseClassCount: Integer; virtual; abstract;
    property DesignerBaseClasses[Index: integer]: TComponentClass read GetDesignerBaseClasses;
    procedure UnregisterDesignerBaseClass(AClass: TComponentClass); virtual; abstract;
    function IndexOfDesignerBaseClass(AClass: TComponentClass): integer; virtual; abstract;
    function FindDesignerBaseClassByName(const AClassName: shortstring): TComponentClass; virtual; abstract;

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

