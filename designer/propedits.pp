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

  Author: Mattias Gaertner

  Abstract:
    This units defines the property editors used by the object inspector.
    A Property Editor is the interface between a row of the object inspector
    and a property in the RTTI.
    For more information see the big comment part below.

  ToDo:
    -digits for floattypes -> I hope, I have guessed right
    -TIntegerSet missing -> taking my own
    -Save ColorDialog settings
    -System.TypeInfo(Type) missing -> exists already in the fpc 1.1 version
         but because I want it now with the stable version I will use my
         workaround
    -StrToInt64 has a bug. It prints infinitly "something happened"
       -> taking my own
    -Message Dialogs on errors

    -many more... see XXX
}
unit PropEdits;

{$mode objfpc}{$H+}

interface

{$DEFINE NewListPropEdit}

uses 
  Classes, TypInfo, SysUtils, Forms, Controls, GraphType, Graphics, StdCtrls,
  Buttons, ComCtrls, Menus, LCLType, ExtCtrls, LCLLinux, Dialogs, ColumnDlg,
  ObjInspStrConsts;

const
  MaxIdentLength: Byte = 63;
  // XXX ToDo
  // this variable should be fetched from consts(x).inc
  // as in fcl/inc/classes.inc
  srUnknown = 'unknown';

type
  TGetStringProc = procedure(const s:ansistring) of object;

  TComponentSelectionList = class;

{ TPropertyEditor
  Edits a property of a component, or list of components, selected into the
  Object Inspector.  The property editor is created based on the type of the
  property being edited as determined by the types registered by
  RegisterPropertyEditor.  The Object Inspector uses a TPropertyEditor
  for all modification to a property. GetName and GetValue are called to display
  the name and value of the property. SetValue is called whenever the user
  requests to change the value.  Edit is called when the user double-clicks the
  property in the Object Inspector. GetValues is called when the drop-down
  list of a property is displayed. GetProperties is called when the property
  is expanded to show sub-properties. AllEqual is called to decide whether or
  not to display the value of the property when more than one component is
  selected.

  The following are methods that can be overridden to change the behavior of
  the property editor:

    Activate
      Called whenever the property becomes selected in the object inspector.
      This is potentially useful to allow certain property attributes to
      to only be determined whenever the property is selected in the object
      inspector. Only paSubProperties and paMultiSelect,returned from
      GetAttributes,need to be accurate before this method is called.
    Deactivate
      Called whenevr the property becomes unselected in the object inspector.
    AllEqual
      Called whenever there is more than one component selected.  If this
      method returns true,GetValue is called,otherwise blank is displayed
      in the Object Inspector.  This is called only when GetAttributes
      returns paMultiSelect.
    AutoFill
      Called to determine whether the values returned by GetValues can be
      selected incrementally in the Object Inspector.  This is called only when
      GetAttributes returns paValueList.
    Edit
      Called when the '...' button is pressed or the property is double-clicked.
      This can,for example,bring up a dialog to allow the editing the
      component in some more meaningful fashion than by text (e.g. the Font
      property).
    GetAttributes
      Returns the information for use in the Object Inspector to be able to
      show the appropriate tools.  GetAttributes returns a set of type
      TPropertyAttributes:
        paValueList:    The property editor can return an enumerated list of
                        values for the property.  If GetValues calls Proc
                        with values then this attribute should be set.  This
                        will cause the drop-down button to appear to the right
                        of the property in the Object Inspector.
        paSortList:     Object Inspector to sort the list returned by
                         GetValues.
        paSubProperties:The property editor has sub-properties that will be
                        displayed indented and below the current property in
                        standard outline format. If GetProperties will
                        generate property objects then this attribute should
                        be set.
        paDynamicSubProps:The sub properties can change. All designer tools
                        (e.g. property editors, component editors) that change
                        the list should call UpdateListPropertyEditors, so that
                        the object inspector will reread the subproperties.
        paDialog:       Indicates that the Edit method will bring up a
                        dialog.  This will cause the '...' button to be
                        displayed to the right of the property in the Object
                        Inspector.
        paMultiSelect:  Allows the property to be displayed when more than
                        one component is selected.  Some properties are not
                        appropriate for multi-selection (e.g. the Name
                        property).
        paAutoUpdate:   Causes the SetValue method to be called on each
                        change made to the editor instead of after the change
                        has been approved (e.g. the Caption property).
        paReadOnly:     Value is not allowed to change.
        paRevertable:   Allows the property to be reverted to the original
                        value.  Things that shouldn't be reverted are nested
                        properties (e.g. Fonts) and elements of a composite
                        property such as set element values.
        paFullWidthName:Tells the object inspector that the value does not
                        need to be rendered and as such the name should be
                        rendered the full width of the inspector.
        paVolatileSubProperties: Any change of property value causes any shown
                         subproperties to be recollected.
        paReference:     Property contains a reference to something else.  When
                         used in conjunction with paSubProperties the referenced
                         object should be displayed as sub properties to this
                         property.
        paNotNestable:   Indicates that the property is not safe to show when
                         showing the properties of an expanded reference.

    GetComponent
      Returns the Index'th component being edited by this property editor.  This
      is used to retrieve the components. A property editor can only refer to
      multiple components when paMultiSelect is returned from GetAttributes.
    GetEditLimit
      Returns the number of character the user is allowed to enter for the
      value. The inplace editor of the object inspector will be have its
      text limited set to the return value.  By default this limit is 255.
    GetName
      Returns the name of the property.  By default the value is retrieved
      from the type information with all underbars replaced by spaces.  This
      should only be overridden if the name of the property is not the name
      that should appear in the Object Inspector.
    GetProperties
      Should be overridden to call PropertyProc for every sub-property (or
      nested property) of the property begin edited and passing a new
      TPropertyEdtior for each sub-property. By default,PropertyProc is not
      called and no sub-properties are assumed. TClassPropertyEditor will pass a
      new property editor for each published property in a class.
      TSetPropertyEditor passes a new editor for each element in the set.
    GetPropType
      Returns the type information pointer for the property(s) being edited.
    GetValue
      Returns the string value of the property. By default this returns
      '(unknown)'.  This should be overridden to return the appropriate value.
    GetValues
      Called when paValueList is returned in GetAttributes.  Should call Proc
      for every value that is acceptable for this property.  TEnumPropertyEditor
      will pass every element in the enumeration.
    Initialize
      Called after the property editor has been created but before it is used.
      Many times property editors are created and because they are not a common
      property across the entire selection they are thrown away.  Initialize is
      called after it is determined the property editor is going to be used by
      the object inspector and not just thrown away.
    SetValue(Value)
      Called to set the value of the property.  The property editor should be
      able to translate the string and call one of the SetXxxValue methods. If
      the string is not in the correct format or not an allowed value,the
      property editor should generate an exception describing the problem. Set
      value can ignore all changes and allow all editing of the property be
      accomplished through the Edit method (e.g. the Picture property).
    ListMeasureWidth(Value,Canvas,AWidth)
      This is called during the width calculation phase of the drop down list
      preparation.
    ListMeasureHeight(Value,Canvas,AHeight)
      This is called during the item/value height calculation phase of the drop
      down list's render.  This is very similar to TListBox's OnMeasureItem,
      just slightly different parameters.
    ListDrawValue(Value,Canvas,Rect,Selected)
      This is called during the item/value render phase of the drop down list's
      render.  This is very similar to TListBox's OnDrawItem, just slightly
      different parameters.
    PropMeasureHeight(Value,Canvas,AHeight)
      This is called during the item/property height calculation phase of the
      object inspectors rows render. This is very similar to TListBox's
      OnMeasureItem, just slightly different parameters.
    PropDrawName(Canvas,Rect,Selected)
      Called during the render of the name column of the property list.  Its
      functionality is very similar to TListBox's OnDrawItem,but once again
      it has slightly different parameters.
    PropDrawValue(Canvas,Rect,Selected)
      Called during the render of the value column of the property list.  Its
      functionality is similar to PropDrawName.  If multiple items are selected
      and their values don't match this procedure will be passed an empty
      value.

  Properties and methods useful in creating new TPropertyEditor classes:

    Name property
      Returns the name of the property returned by GetName
    PrivateEditory property
      It is either the .EXE or the "working editory" as specified in
      the registry under the key:
        "HKEY_CURRENT_USER\Software\Borland\Delphi\*\Globals\PrivateDir"
      If the property editor needs auxiliary or state files (templates,
      examples, etc) they should be stored in this editory.
    Value property
      The current value,as a string,of the property as returned by GetValue.
    Modified
      Called to indicate the value of the property has been modified.  Called
      automatically by the SetXxxValue methods.  If you call a TProperty
      SetXxxValue method directly,you *must* call Modified as well.
    GetXxxValue
      Gets the value of the first property in the Properties property.  Calls
      the appropriate TProperty GetXxxValue method to retrieve the value.
    SetXxxValue
      Sets the value of all the properties in the Properties property.  Calls
      the approprate TProperty SetXxxxValue methods to set the value.
    GetVisualValue
      This function will return the displayable value of the property.  If
      only one item is selected or all the multi-selected items have the same
      property value then this function will return the actual property value.
      Otherwise this function will return an empty string.}

  TPropertyAttribute=(
    paValueList,
    paSubProperties,
    paDynamicSubProps,
    paDialog,
    paMultiSelect,
    paAutoUpdate,
    paSortList,
    paReadOnly,
    paRevertable,
    paFullWidthName,
    paVolatileSubProperties,
    paReference,
    paNotNestable
    );
  TPropertyAttributes=set of TPropertyAttribute;

  TPropertyEditor=class;

  TInstProp=record
    Instance:TPersistent;
    PropInfo:PPropInfo;
  end;

  PInstPropList=^TInstPropList;
  TInstPropList=array[0..1023] of TInstProp;

  TGetPropEditProc=procedure(Prop:TPropertyEditor) of object;

  TPropEditDrawStateType = (pedsSelected, pedsFocused, pedsInEdit,
       pedsInComboList, pedsPainted);
  TPropEditDrawState = set of TPropEditDrawStateType;

  TPropertyEditorHook = class;

  TPropertyEditor=class
  private
    FComponents:TComponentSelectionList;
    FOnSubPropertiesChanged: TNotifyEvent;
    FPropertyHook:TPropertyEditorHook;
    FPropCount:Integer;
    FPropList:PInstPropList;
    function GetPrivateDirectory:ansistring;
  protected
    function GetPropInfo:PPropInfo;
    function GetFloatValue:Extended;
    function GetFloatValueAt(Index:Integer):Extended;
    function GetInt64Value:Int64;
    function GetInt64ValueAt(Index:Integer):Int64;
    function GetMethodValue:TMethod;
    function GetMethodValueAt(Index:Integer):TMethod;
    function GetOrdValue:Longint;
    function GetOrdValueAt(Index:Integer):Longint;
    function GetStrValue:AnsiString;
    function GetStrValueAt(Index:Integer):AnsiString;
    function GetVarValue:Variant;
    function GetVarValueAt(Index:Integer):Variant;
    procedure SetFloatValue(NewValue:Extended);
    procedure SetMethodValue(const NewValue:TMethod);
    procedure SetInt64Value(NewValue:Int64);
    procedure SetOrdValue(NewValue:Longint);
    procedure SetStrValue(const NewValue:AnsiString);
    procedure SetVarValue(const NewValue:Variant);
    procedure Modified;
  public
    constructor Create(Hook:TPropertyEditorHook;
      ComponentList: TComponentSelectionList;  APropCount:Integer); virtual;
    destructor Destroy; override;
    procedure Activate; virtual;
    procedure Deactivate; virtual;
    function AllEqual:Boolean; virtual;
    function AutoFill:Boolean; virtual;
    procedure Edit; virtual;
    function GetAttributes:TPropertyAttributes; virtual;
    function GetComponent(Index:Integer):TPersistent;
    function GetEditLimit:Integer; virtual;
    function GetName:shortstring; virtual;
    procedure GetProperties(Proc:TGetPropEditProc); virtual;
    function GetPropType:PTypeInfo;
    function GetValue:ansistring; virtual;
    function GetVisualValue:ansistring;
    procedure GetValues(Proc:TGetStringProc); virtual;
    procedure Initialize; virtual;
    procedure Revert;
    procedure SetValue(const NewValue:ansistring); virtual;
    procedure SetPropEntry(Index:Integer; AInstance:TPersistent;
      APropInfo:PPropInfo);
    function ValueAvailable:Boolean;
    procedure ListMeasureWidth(const AValue:ansistring; Index:integer;
      ACanvas:TCanvas;  var AWidth:Integer); dynamic;
    procedure ListMeasureHeight(const AValue:ansistring; Index:integer;
      ACanvas:TCanvas;  var AHeight:Integer); dynamic;
    procedure ListDrawValue(const AValue:ansistring; Index:integer;
      ACanvas:TCanvas;  const ARect:TRect; AState: TPropEditDrawState); dynamic;
    procedure PropMeasureHeight(const NewValue:ansistring;  ACanvas:TCanvas;
      var AHeight:Integer); dynamic;
    procedure PropDrawName(ACanvas:TCanvas; const ARect:TRect;
      AState:TPropEditDrawState); dynamic;
    procedure PropDrawValue(ACanvas:TCanvas; const ARect:TRect;
      AState:TPropEditDrawState); dynamic;
    procedure UpdateSubProperties; virtual;
    function SubPropertiesNeedsUpdate: boolean; virtual;
    property PropertyHook:TPropertyEditorHook read FPropertyHook;
    property PrivateDirectory:ansistring read GetPrivateDirectory;
    property PropCount:Integer read FPropCount;
    property FirstValue:ansistring read GetValue write SetValue;
    property OnSubPropertiesChanged: TNotifyEvent
      read FOnSubPropertiesChanged write FOnSubPropertiesChanged;
  end;

  TPropertyEditorClass=class of TPropertyEditor;

{ TOrdinalPropertyEditor
  The base class of all ordinal property editors.  It establishes that ordinal
  properties are all equal if the GetOrdValue all return the same value. }

  TOrdinalPropertyEditor = class(TPropertyEditor)
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
  end;

{ TIntegerPropertyEditor
  Default editor for all Longint properties and all subtypes of the Longint
  type (i.e. Integer, Word, 1..10, etc.).  Restricts the value entered into
  the property to the range of the sub-type. }

  TIntegerPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring);  override;
  end;

{ TCharPropertyEditor
  Default editor for all Char properties and sub-types of Char (i.e. Char,
  'A'..'Z', etc.). }

  TCharPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TEnumPropertyEditor
  The default property editor for all enumerated properties (e.g. TShape =
  (sCircle, sTriangle, sSquare), etc.). }

  TEnumPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TBoolPropertyEditor
  Default property editor for all boolean properties }

  TBoolPropertyEditor = class(TEnumPropertyEditor)
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TInt64PropertyEditor
  Default editor for all Int64 properties and all subtypes of Int64.  }

  TInt64PropertyEditor = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TFloatPropertyEditor
  The default property editor for all floating point types (e.g. Float,
  Single, Double, etc.) }

  TFloatPropertyEditor = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TStringPropertyEditor
  The default property editor for all strings and sub types (e.g. string,
  string[20], etc.). }

  TStringPropertyEditor = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TNestedPropertyEditor
  A property editor that uses the PropertyHook, PropList and PropCount.
  The constructor and destructor do not call inherited, but all derived classes
  should.  This is useful for properties like the TSetElementPropertyEditor. }

  TNestedPropertyEditor = class(TPropertyEditor)
  public
    constructor Create(Parent: TPropertyEditor);
    destructor Destroy; override;
  end;

{ TSetElementPropertyEditor
  A property editor that edits an individual set element.  GetName is
  changed to display the set element name instead of the property name and
  Get/SetValue is changed to reflect the individual element state.  This
  editor is created by the TSetPropertyEditor editor. }

  TSetElementPropertyEditor = class(TNestedPropertyEditor)
  private
    FElement: Integer;
  public
    constructor Create(Parent: TPropertyEditor; AElement: Integer);
    function AllEqual: Boolean; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetName: shortstring; override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue: ansistring); override;
   end;

{ TSetPropertyEditor
  Default property editor for all set properties. This editor does not edit
  the set directly but will display sub-properties for each element of the
  set. GetValue displays the value of the set in standard set syntax. }

  TSetPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: AnsiString; override;
  end;
  
{ TClassPropertyEditor
  Default property editor for all objects.  Does not allow modifying the
  property but does display the class name of the object and will allow the
  editing of the object's properties as sub-properties of the property. }

  TClassPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: ansistring; override;
  end;

{ TMethodPropertyEditor
  Property editor for all method properties. }

  TMethodPropertyEditor = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue: ansistring); override;
    function GetFormMethodName: shortstring; virtual;
    function GetTrimmedEventName: shortstring;
  end;

{ TComponentPropertyEditor
  The default editor for TComponents.  It does not allow editing of the
  properties of the component.  It allow the user to set the value of this
  property to point to a component in the same form that is type compatible
  with the property being edited (e.g. the ActiveControl property). }

  TComponentPropertyEditor = class(TPropertyEditor)
  protected
    function FilterFunc(const ATestEditor: TPropertyEditor{IProperty}): Boolean;
    function GetComponentReference: TComponent; virtual;
    function GetSelections: TComponentSelectionList{IDesignerSelections}; virtual;
  public
    function AllEqual: Boolean; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc:TGetPropEditProc); override;
    function GetEditLimit: Integer; override;
    function GetValue: AnsiString; override;
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TInterfaceProperty
  The default editor for interface references.  It allows the user to set
  the value of this property to refer to an interface implemented by
  a component on the form (or via form linking) that is type compatible
  with the property being edited. }

  TInterfaceProperty = class(TComponentPropertyEditor)
  private
    //FGetValuesStrProc: TGetStrProc;
  protected
    procedure ReceiveComponentNames(const S: string);
    function GetComponent(const AInterface: Pointer {IInterface}): TComponent;
    function GetComponentReference: TComponent; override;
    function GetSelections: TComponentSelectionList{IDesignerSelections}; override;
  public
    function AllEqual: Boolean; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TComponentNamePropertyEditor
  Property editor for the Name property.  It restricts the name property
  from being displayed when more than one component is selected. }

  TComponentNamePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TDateProperty
  Property editor for date portion of TDateTime type. }

  TDateProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TTimePropertyEditor
  Property editor for time portion of TDateTime type. }

  TTimePropertyEditor = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TDateTimePropertyEditor
  Edits both date and time data simultaneously  }

  TDateTimePropertyEditor = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TVariantPropertyEditor }

  TVariantPropertyEditor = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetProperties(Proc:TGetPropEditProc); override;
  end;

{ TModalResultPropertyEditor }

  TModalResultPropertyEditor = class(TIntegerPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue:ansistring); override;
  end;

{ TShortCutPropertyEditor
  Property editor the ShortCut property.  Allows both typing in a short
  cut value or picking a short-cut value from a list. }

  TShortCutPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TTabOrderPropertyEditor
  Property editor for the TabOrder property.  Prevents the property from being
  displayed when more than one component is selected. }

  TTabOrderPropertyEditor = class(TIntegerPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TCaptionPropertyEditor
  Property editor for the Caption and Text properties.  Updates the value of
  the property for each change instead on when the property is approved. }

  TCaptionPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TCaptionMultilinePropertyEditor
  PropertyEditor editor for the Caption property when the Caption can be multiline.
  Brings up the dialog for entering text. }

  TCaptionMultilinePropertyEditor = class(TCaptionPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TStringsPropertyEditor
  PropertyEditor editor for the TStrings properties.
  Brings up the dialog for entering text. }

  TStringsPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TListColumnsPropertyEditor
  PropertyEditor editor for the TListColumns properties.
  Brings up the dialog for entering text. }

  TListColumnsPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TCursorPropertyEditor
  PropertyEditor editor for the TCursor properties.
  Displays cursor as constant name if exists, otherwise an integer. }

  TCursorPropertyEditor = class(TIntegerPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TListElementPropertyEditor
  A property editor for a single element of a TListPropertyEditor
  This editor simply redirects all methods to the TListPropertyEditor }
  TListPropertyEditor = class;

  TListElementPropertyEditor = class(TNestedPropertyEditor)
  private
    FIndex: integer;
    FList: TListPropertyEditor;
  public
    constructor Create(Parent: TListPropertyEditor; AnIndex: integer);
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetName:shortstring; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue: ansistring); override;
    property List: TListPropertyEditor read FList;
    property TheIndex: integer read FIndex;
  end;

{ TListPropertyEditor
  A property editor with dynamic sub properties representing a list of objects.
  UNDER CONSTRUCTION by Mattias}

  TListPropertyEditor = class(TPropertyEditor)
  private
    FSaveElementLock: integer;
    FSubPropertiesChanged: boolean;
  protected
    procedure BeginSaveElement;
    procedure EndSaveElement;
    function IsSaving: boolean;
    property SaveElementLock: integer read FSaveElementLock;
  protected
    // methods and variables usable for descendent property editors:
    SavedList: TObject;
    SavedElements: TList;
    SavedPropertyEditors: TList;
    function ReadElementCount: integer; virtual;
    function ReadElement(Index: integer): TObject; virtual;
    function CreateElementPropEditor(
      Index: integer): TListElementPropertyEditor; virtual;
    procedure DoSaveElements; virtual;
    procedure FreeElementPropertyEditors; virtual;
    function GetElementAttributes(
      Element: TListElementPropertyEditor): TPropertyAttributes; virtual;
    function GetElementName(
      Element: TListElementPropertyEditor):shortstring; virtual;
    procedure GetElementProperties(Element: TListElementPropertyEditor;
      Proc: TGetPropEditProc); virtual;
    function GetElementValue(
      Element: TListElementPropertyEditor): ansistring; virtual;
    procedure GetElementValues(Element: TListElementPropertyEditor;
      Proc: TGetStringProc); virtual;
    procedure SetElementValue(Element: TListElementPropertyEditor;
      NewValue: ansistring); virtual;
  public
    constructor Create(Hook:TPropertyEditorHook;
      ComponentList: TComponentSelectionList;  APropCount:Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetElementCount: integer;
    function GetElement(Index: integer): TObject;
    function GetElement(Element: TListElementPropertyEditor): TObject;
    function GetElementPropEditor(Index: integer): TListElementPropertyEditor;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: AnsiString; override;
    procedure Initialize; override;
    procedure SaveElements;
    function SubPropertiesNeedsUpdate: boolean; override;
  end;

{ TCollectionPropertyEditor
  Default property editor for all TCollections
  UNDER CONSTRUCTION by Mattias}

  TCollectionPropertyEditor = class(TListPropertyEditor)
  private
  protected
    function ReadElementCount: integer; override;
    function ReadElement(Index: integer): TObject; override;
    function GetElementAttributes(
      Element: TListElementPropertyEditor): TPropertyAttributes; override;
    function GetElementName(
      Element: TListElementPropertyEditor):shortstring; override;
    procedure GetElementProperties(Element: TListElementPropertyEditor;
      Proc: TGetPropEditProc); override;
    function GetElementValue(
      Element: TListElementPropertyEditor): ansistring; override;
    procedure GetElementValues(Element: TListElementPropertyEditor;
      Proc: TGetStringProc); override;
    procedure SetElementValue(Element: TListElementPropertyEditor;
      NewValue: ansistring); override;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

//==============================================================================

{ RegisterPropertyEditor
  Registers a new property editor for the given type.
  When a component is selected the Object Inspector will create a property
  editor for each of the component's properties. The property editor is created
  based on the type of the property. If, for example, the property type is an
  Integer, the property editor for Integer will be created (by default
  that would be TIntegerPropertyEditor). Most properties do not need specialized
  property editors.
  For example, if the property is an ordinal type the default property editor
  will restrict the range to the ordinal subtype range (e.g. a property of type
  TMyRange=1..10 will only allow values between 1 and 10 to be entered into the
  property).  Enumerated types will display a drop-down list of all the
  enumerated values (e.g. TShapes = (sCircle,sSquare,sTriangle) will be edited
  by a drop-down list containing only sCircle,sSquare and sTriangle).
  A property editor needs only be created if default property editor or none of
  the existing property editors are sufficient to edit the property.  This is
  typically because the property is an object.
  The registered types are looked up newest to oldest.
  This allows an existing property editor replaced by a custom property editor.

    PropertyEditorType
      The type information pointer returned by the TypeInfo built-in function
      (e.g. TypeInfo(TMyRange) or TypeInfo(TShapes)).

    ComponentClass
      Type of the component to which to restrict this type editor.  This
      parameter can be left nil which will mean this type editor applies to all
      properties of PropertyEditorType.

    PropertyEditorName
      The name of the property to which to restrict this type editor.  This
      parameter is ignored if ComponentClass is nil.  This parameter can be
      an empty string ('') which will mean that this editor applies to all
      properties of PropertyEditorType in ComponentClass.

    editorClass
      The class of the editor to be created whenever a property of the type
      passed in PropertyEditorTypeInfo is displayed in the Object Inspector.
      The class will be created by calling EditorClass.Create. }

procedure RegisterPropertyEditor(PropertyType:PTypeInfo;
  ComponentClass:TClass;  const PropertyName:shortstring;
  EditorClass:TPropertyEditorClass);

type
  TPropertyEditorMapperFunc=function(Obj:TPersistent;
    PropInfo:PPropInfo):TPropertyEditorClass;

procedure RegisterPropertyEditorMapper(Mapper:TPropertyEditorMapperFunc);

type
  TPropertyEditorFilterFunc =
    function(const ATestEditor: TPropertyEditor): Boolean of object;

procedure GetComponentProperties(Components: TComponentSelectionList;
  Filter: TTypeKinds; Hook: TPropertyEditorHook; Proc: TGetPropEditProc;
  EditorFilterFunc: TPropertyEditorFilterFunc);

function GetEditorClass(PropInfo:PPropInfo;
  Obj:TPersistent): TPropertyEditorClass;

//==============================================================================

procedure RegisterListPropertyEditor(AnEditor: TListPropertyEditor);
procedure UnregisterListPropertyEditor(AnEditor: TListPropertyEditor);
procedure UpdateListPropertyEditors(AnObject: TObject);

//==============================================================================
{
  The TComponentSelectionList is simply a list of TComponents references.
  It will never create or free any components. It is used by the property
  editors, the object inspector and the form editor.
}
type
  TComponentSelectionList = class
  private
    FComponents:TList;
    function GetItems(Index: integer): TComponent;
    procedure SetItems(Index: integer; const CompValue: TComponent);
    function GetCount: integer;
    function GetCapacity:integer;
    procedure SetCapacity(const NewCapacity:integer);
  public
    procedure Clear;
    function IsEqual(SourceSelectionList:TComponentSelectionList):boolean;
    property Count:integer read GetCount;
    property Capacity:integer read GetCapacity write SetCapacity;
    function Add(c:TComponent):integer;
    procedure Assign(SourceSelectionList:TComponentSelectionList);
    property Items[Index:integer]:TComponent read GetItems write SetItems; default;
    constructor Create;
    destructor Destroy;  override;
  end;

//==============================================================================
{
  TPropertyEditorHook

  This is the interface for methods, components and objects handling of all
  property editors. Just create such thing and give it the object inspector.
}
type
  // lookup root
  TPropHookChangeLookupRoot = procedure of object;
  // methods
  TPropHookCreateMethod = function(const Name:ShortString;
    ATypeInfo:PTypeInfo): TMethod of object;
  TPropHookGetMethodName = function(const Method:TMethod): ShortString of object;
  TPropHookGetMethods = procedure(TypeData:PTypeData; Proc:TGetStringProc) of object;
  TPropHookMethodExists = function(const Name:ShortString; TypeData: PTypeData;
    var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean of object;
  TPropHookRenameMethod = procedure(const CurName, NewName:ShortString) of object;
  TPropHookShowMethod = procedure(const Name:ShortString) of object;
  TPropHookMethodFromAncestor = function(const Method:TMethod):boolean of object;
  TPropHookChainCall = procedure(const MethodName, InstanceName,
    InstanceMethod:ShortString; TypeData:PTypeData) of object;
  // components
  TPropHookGetComponent = function(const Name:ShortString):TComponent of object;
  TPropHookGetComponentName = function(AComponent:TComponent):ShortString of object;
  TPropHookGetComponentNames = procedure(TypeData:PTypeData;
    Proc:TGetStringProc) of object;
  TPropHookGetRootClassName = function:ShortString of object;
  TPropHookComponentRenamed = procedure(AComponent: TComponent) of object;
  TPropHookComponentAdded = procedure(AComponent: TComponent; Select: boolean) of object;
  TPropHookDeleteComponent = procedure(AComponent: TComponent) of object;
  // persistent objects
  TPropHookGetObject = function(const Name:ShortString):TPersistent of object;
  TPropHookGetObjectName = function(Instance:TPersistent):ShortString of object;
  TPropHookGetObjectNames = procedure(TypeData:PTypeData; Proc:TGetStringProc) of object;
  // modifing
  TPropHookModified = procedure of object;
  TPropHookRevert = procedure(Instance:TPersistent; PropInfo:PPropInfo) of object;
  TPropHookRefreshPropertyValues = procedure of object;

  TPropertyEditorHook = class
  private
    // lookup root
    FLookupRoot: TComponent;
    FOnChangeLookupRoot: TPropHookChangeLookupRoot;
    // methods
    FOnCreateMethod: TPropHookCreateMethod;
    FOnGetMethodName: TPropHookGetMethodName;
    FOnGetMethods: TPropHookGetMethods;
    FOnMethodExists: TPropHookMethodExists;
    FOnRenameMethod: TPropHookRenameMethod;
    FOnShowMethod: TPropHookShowMethod;
    FOnMethodFromAncestor: TPropHookMethodFromAncestor;
    FOnChainCall: TPropHookChainCall;
    // components
    FOnGetComponent: TPropHookGetComponent;
    FOnGetComponentName: TPropHookGetComponentName;
    FOnGetComponentNames: TPropHookGetComponentNames;
    FOnGetRootClassName: TPropHookGetRootClassName;
    FOnComponentRenamed: TPropHookComponentRenamed;
    FOnComponentAdded: TPropHookComponentAdded;
    FOnDeleteComponent: TPropHookDeleteComponent;
    // persistent objects
    FOnGetObject: TPropHookGetObject;
    FOnGetObjectName: TPropHookGetObjectName;
    FOnGetObjectNames: TPropHookGetObjectNames;
    // modifing
    FOnModified: TPropHookModified;
    FOnRevert: TPropHookRevert;
    FOnRefreshPropertyValues: TPropHookRefreshPropertyValues;

    procedure SetLookupRoot(AComponent:TComponent);
  public
    GetPrivateDirectory:AnsiString;
    // lookup root
    property LookupRoot:TComponent read FLookupRoot write SetLookupRoot;
    // methods
    function CreateMethod(const Name:ShortString; ATypeInfo:PTypeInfo): TMethod;
    function GetMethodName(const Method:TMethod): ShortString;
    procedure GetMethods(TypeData:PTypeData; Proc:TGetStringProc);
    function MethodExists(const Name:ShortString; TypeData: PTypeData;
      var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;
    procedure RenameMethod(const CurName, NewName:ShortString);
    procedure ShowMethod(const Name:ShortString);
    function MethodFromAncestor(const Method:TMethod):boolean;
    procedure ChainCall(const AMethodName, InstanceName,
      InstanceMethod:ShortString;  TypeData:PTypeData);
    // components
    function GetComponent(const Name:ShortString):TComponent;
    function GetComponentName(AComponent:TComponent):ShortString;
    procedure GetComponentNames(TypeData:PTypeData; Proc:TGetStringProc);
    function GetRootClassName:ShortString;
    procedure ComponentRenamed(AComponent: TComponent);
    procedure ComponentAdded(AComponent: TComponent; Select: boolean);
    procedure DeleteComponent(AComponent: TComponent);
    // persistent objects
    function GetObject(const Name:ShortString):TPersistent;
    function GetObjectName(Instance:TPersistent):ShortString;
    procedure GetObjectNames(TypeData:PTypeData; Proc:TGetStringProc);
    // modifing
    procedure Modified;
    procedure Revert(Instance:TPersistent; PropInfo:PPropInfo);
    procedure RefreshPropertyValues;

    // lookup root
    property OnChangeLookupRoot:TPropHookChangeLookupRoot
      read FOnChangeLookupRoot write FOnChangeLookupRoot;
    // method events
    property OnCreateMethod:TPropHookCreateMethod read FOnCreateMethod write FOnCreateMethod;
    property OnGetMethodName:TPropHookGetMethodName read FOnGetMethodName write FOnGetMethodName;
    property OnGetMethods:TPropHookGetMethods read FOnGetMethods write FOnGetMethods;
    property OnMethodExists:TPropHookMethodExists read FOnMethodExists write FOnMethodExists;
    property OnRenameMethod:TPropHookRenameMethod read FOnRenameMethod write FOnRenameMethod;
    property OnShowMethod:TPropHookShowMethod read FOnShowMethod write FOnShowMethod;
    property OnMethodFromAncestor:TPropHookMethodFromAncestor read FOnMethodFromAncestor write FOnMethodFromAncestor;
    property OnChainCall:TPropHookChainCall read FOnChainCall write FOnChainCall;
    // component event
    property OnGetComponent:TPropHookGetComponent read FOnGetComponent write FOnGetComponent;
    property OnGetComponentName:TPropHookGetComponentName read FOnGetComponentName write FOnGetComponentName;
    property OnGetComponentNames:TPropHookGetComponentNames read FOnGetComponentNames write FOnGetComponentNames;
    property OnGetRootClassName:TPropHookGetRootClassName read FOnGetRootClassName write FOnGetRootClassName;
    property OnComponentRenamed:TPropHookComponentRenamed read FOnComponentRenamed write FOnComponentRenamed;
    property OnComponentAdded:TPropHookComponentAdded read FOnComponentAdded write FOnComponentAdded;
    property OnDeleteComponent:TPropHookDeleteComponent read FOnDeleteComponent write FOnDeleteComponent;
    // persistent object events
    property OnGetObject:TPropHookGetObject read FOnGetObject write FOnGetObject;
    property OnGetObjectName:TPropHookGetObjectName read FOnGetObjectName write FOnGetObjectName;
    property OnGetObjectNames:TPropHookGetObjectNames read FOnGetObjectNames write FOnGetObjectNames;
    // modifing events
    property OnModified:TPropHookModified read FOnModified write FOnModified;
    property OnRevert:TPropHookRevert read FOnRevert write FOnRevert;
    property OnRefreshPropertyValues:TPropHookRefreshPropertyValues read FOnRefreshPropertyValues write FOnRefreshPropertyValues;
  end;

//==============================================================================

{ TPropInfoList }

type
  TPropInfoList = class
  private
    FList: PPropList;
    FCount: Integer;
    FSize: Integer;
    function Get(Index: Integer): PPropInfo;
  public
    constructor Create(Instance: TPersistent; Filter: TTypeKinds);
    destructor Destroy; override;
    function Contains(P: PPropInfo): Boolean;
    procedure Delete(Index: Integer);
    procedure Intersect(List: TPropInfoList);
    property Count: Integer read FCount;
    property Items[Index: Integer]: PPropInfo read Get; default;
  end;

//==============================================================================

// Global flags:
const
  GReferenceExpandable: Boolean = True;
  GShowReadOnlyProps: Boolean = True;

//==============================================================================
// XXX
// This class is a workaround for the missing typeinfo function
type
  TDummyClassForPropTypes = class (TPersistent)
  private
    FDate: TDateProperty;
    FDateTime: TDateTimePropertyEditor;
    FList:PPropList;
    FCount:integer;
    FComponent:TComponent;
    FComponentName:TComponentName;
    FCursor: TCursor;
    FShortCut: TShortCut;
    FTabOrder:integer;
    FCaption: TCaption;
    FLines:TStrings;
    FColumns: TListColumns;
    FModalResult:TModalResult;
    FTime: TTimePropertyEditor;
  public
    function PTypeInfos(const PropName:shortstring):PTypeInfo;
    constructor Create;
    destructor Destroy;  override;
  published
    property PropCount:integer read FCount;
    property DummyComponent:TComponent read FComponent;
    property DummyName:TComponentName read FComponentName;
    property TabOrder:integer read FTabOrder;
    property Caption:TCaption read FCaption;
    property Cursor: TCursor read FCursor;
    property Lines:TStrings read FLines;
    property Columns:TListColumns read FColumns;
    property ModalResult:TModalResult read FModalResult;
    property ShortCut: TShortCut read FShortCut;
    property Date: TDateProperty read FDate;
    property Time: TTimePropertyEditor read FTime;
    property DateTime: TDateTimePropertyEditor read FDateTime;
  end;

//==============================================================================

  Function ClassTypeInfo(Value : TClass) : PTypeInfo;

implementation


const
  ListPropertyEditors: TList = nil;

procedure RegisterListPropertyEditor(AnEditor: TListPropertyEditor);
begin
  if ListPropertyEditors=nil then
    ListPropertyEditors:=TList.Create;
  ListPropertyEditors.Add(AnEditor);
end;

procedure UnregisterListPropertyEditor(AnEditor: TListPropertyEditor);
begin
  if ListPropertyEditors=nil then exit;
  ListPropertyEditors.Remove(AnEditor);
end;

procedure UpdateListPropertyEditors(AnObject: TObject);
var
  i: integer;
  Editor: TListPropertyEditor;
begin
  if ListPropertyEditors=nil then exit;
  for i:=0 to ListPropertyEditors.Count-1 do begin
    Editor:=TListPropertyEditor(ListPropertyEditors[i]);
    if (Editor.GetComponent(0)=AnObject)
    and (Editor.OnSubPropertiesChanged<>nil) then
      Editor.UpdateSubProperties;
  end;
end;


//------------------------------------------------------------------------------

const
{ TypeKinds  see typinfo.pp
       TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,
                   tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord,
                   tkDynArray,tkInterfaceRaw);
}

  PropClassMap:array[TypInfo.TTypeKind] of TPropertyEditorClass=(
    nil,                   // tkUnknown
    TIntegerPropertyEditor,// tkInteger
    TCharpropertyEditor,   // tkChar
    TEnumPropertyEditor,   // tkEnumeration
    TFloatPropertyEditor,  // tkFloat
    TSetPropertyEditor,    // tkSet
    TMethodPropertyEditor, // tkMethod
    TStringPropertyEditor, // tkSString
    TStringPropertyEditor, // tkLString
    TStringPropertyEditor, // tkAString
    TStringPropertyEditor, // tkWString
    TPropertyEditor,       // tkVariant
    nil,                   // tkArray
    nil,                   // tkRecord
    nil,                   // tkInterface
    TClassPropertyEditor,  // tkClass
    nil,                   // tkObject
    TPropertyEditor,       // tkWChar
    TBoolPropertyEditor,   // tkBool
    TInt64PropertyEditor,  // tkInt64
    nil,                   // tkQWord
    nil,                   // tkDynArray
    nil                    // tkInterfaceRaw
    );


// XXX ToDo: These variables/functions have bugs. Thus I provide my own ------

function StrToInt64(const s:ansistring):int64;
var p:integer;
  negated:boolean;
begin
  p:=1;
  while (p<=length(s)) and (s[p]=' ') do inc(p);
  if (p<=length(s)) and (s[p]='-') then begin
    negated:=true;
    inc(p);
    while (p<=length(s)) and (s[p]=' ') do inc(p);
  end else begin
    negated:=false;
  end;
  Result:=0;
  while (p<=length(s)) and (s[p]>='0') and (s[p]<='9') do begin
    Result:=Result*10+ord(s[p])-ord('0');
    inc(p);
  end;
  if negated then Result:=-Result;
end;

// -----------------------------------------------------------

var
  PropertyEditorMapperList:TList;
  PropertyClassList:TList;

type
  PPropertyClassRec=^TPropertyClassRec;
  TPropertyClassRec=record
    // XXX
    //Group:Integer;
    PropertyType:PTypeInfo;
    PropertyName:shortstring;
    ComponentClass:TClass;
    EditorClass:TPropertyEditorClass;
  end;

  PPropertyEditorMapperRec=^TPropertyEditorMapperRec;
  TPropertyEditorMapperRec=record
    // XXX
    //Group:Integer;
    Mapper:TPropertyEditorMapperFunc;
  end;

{ TPropInfoList }

constructor TPropInfoList.Create(Instance:TPersistent; Filter:TTypeKinds);
var 
  BigList: PPropList;
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  PropInfo: PPropInfo;
  CurCount, i: integer;
begin
  TypeInfo:=Instance.ClassInfo;
  TypeData:=GetTypeData(TypeInfo);
  GetMem(BigList,TypeData^.PropCount * SizeOf(Pointer));
  FCount:=0;
  repeat
    // read all property infos of current class
    PropInfo:=(@TypeData^.UnitName+Length(TypeData^.UnitName)+1);
    CurCount:=PWord(PropInfo)^;
    // Now point PropInfo to first propinfo record.
    inc(Longint(PropInfo),SizeOf(Word));
    while CurCount>0 do begin
      if PropInfo^.PropType^.Kind in Filter then begin
        // check if name already exists in list
        i:=FCount-1;
        while (i>=0) and (BigList^[i]^.Name<>PropInfo^.Name) do 
          dec(i);
        if (i<0) then begin
          // add property info to BigList
          BigList^[FCount]:=PropInfo;
          inc(FCount);
        end;
      end;
      // point PropInfo to next propinfo record.
      // Located at Name[Length(Name)+1] !
      PropInfo:=PPropInfo(pointer(@PropInfo^.Name)+PByte(@PropInfo^.Name)^+1);
      dec(CurCount);
    end;
    TypeInfo:=TypeData^.ParentInfo;
    if TypeInfo=nil then break;
    TypeData:=GetTypeData(TypeInfo);
  until false;
    
  // create FList
  FSize:=FCount * SizeOf(Pointer);
  GetMem(FList,FSize);
  Move(BigList^,FList^,FSize);
  FreeMem(BigList);
end;

destructor TPropInfoList.Destroy;
begin
  if FList<>nil then FreeMem(FList,FSize);
end;

function TPropInfoList.Contains(P:PPropInfo):Boolean;
var
  I:Integer;
begin
  for I:=0 to FCount-1 do begin
    with FList^[I]^ do begin
      if (PropType^.Kind=P^.PropType^.Kind)
      and (CompareText(Name,P^.Name)=0) then begin
        Result:=True;
        Exit;
      end;
    end;
  end;
  Result:=False;
end;

procedure TPropInfoList.Delete(Index:Integer);
begin
  Dec(FCount);
  if Index < FCount then
    Move(FList^[Index+1],FList^[Index],
      (FCount-Index) * SizeOf(Pointer));
end;

function TPropInfoList.Get(Index:Integer):PPropInfo;
begin
  Result:=FList^[Index];
end;

procedure TPropInfoList.Intersect(List:TPropInfoList);
var
  I:Integer;
begin
  for I:=FCount-1 downto 0 do
    if not List.Contains(FList^[I]) then Delete(I);
end;


//------------------------------------------------------------------------------


{ GetComponentProperties }

procedure RegisterPropertyEditor(PropertyType:PTypeInfo;
  ComponentClass: TClass;  const PropertyName:shortstring;
  EditorClass:TPropertyEditorClass);
var
  P:PPropertyClassRec;
begin
  if PropertyType=nil then exit;
  if PropertyClassList=nil then
    PropertyClassList:=TList.Create;
  New(P);
  // XXX
  //P^.Group:=CurrentGroup;
  P^.PropertyType:=PropertyType;
  P^.ComponentClass:=ComponentClass;
  P^.PropertyName:=PropertyName;
  //if Assigned(ComponentClass) then P^.PropertyName:=PropertyName;
  P^.EditorClass:=EditorClass;
  PropertyClassList.Insert(0,P);
end;

procedure RegisterPropertyEditorMapper(Mapper:TPropertyEditorMapperFunc);
var
  P:PPropertyEditorMapperRec;
begin
  if PropertyEditorMapperList=nil then
    PropertyEditorMapperList:=TList.Create;
  New(P);
  // XXX
  //P^.Group:=CurrentGroup;
  P^.Mapper:=Mapper;
  PropertyEditorMapperList.Insert(0,P);
end;

function GetEditorClass(PropInfo:PPropInfo;
  Obj:TPersistent): TPropertyEditorClass;
var
  PropType:PTypeInfo;
  P,C:PPropertyClassRec;
  I:Integer;
begin
  if PropertyEditorMapperList<>nil then begin
    for I:=0 to PropertyEditorMapperList.Count-1 do begin
      with PPropertyEditorMapperRec(PropertyEditorMapperList[I])^ do begin
        Result:=Mapper(Obj,PropInfo);
        if Result<>nil then Exit;
      end;
    end;
  end;
  PropType:=PropInfo^.PropType;
  I:=0;
  C:=nil;
  while I < PropertyClassList.Count do begin
    P:=PropertyClassList[I];

    if ((P^.PropertyType=PropType) or
         ((P^.PropertyType^.Kind=PropType^.Kind) and
          (P^.PropertyType^.Name=PropType^.Name)
         )
       ) or
       ( (PropType^.Kind=tkClass) and
         (P^.PropertyType^.Kind=tkClass) and
         GetTypeData(PropType)^.ClassType.InheritsFrom(
           GetTypeData(P^.PropertyType)^.ClassType)
       )
    then
      if ((P^.ComponentClass=nil) or (Obj.InheritsFrom(P^.ComponentClass))) and
         ((P^.PropertyName='')
         or (CompareText(PropInfo^.Name,P^.PropertyName)=0))
      then
        if (C=nil) or   // see if P is better match than C
           ((C^.ComponentClass=nil) and (P^.ComponentClass<>nil)) or
           ((C^.PropertyName='') and (P^.PropertyName<>''))
           or  // P's proptype match is exact,but C's does not
           ((C^.PropertyType<>PropType) and (P^.PropertyType=PropType))
           or  // P's proptype is more specific than C's proptype
           ((P^.PropertyType<>C^.PropertyType) and
            (P^.PropertyType^.Kind=tkClass) and
            (C^.PropertyType^.Kind=tkClass) and
            GetTypeData(P^.PropertyType)^.ClassType.InheritsFrom(
              GetTypeData(C^.PropertyType)^.ClassType))
           or // P's component class is more specific than C's component class
           ((P^.ComponentClass<>nil) and (C^.ComponentClass<>nil) and
            (P^.ComponentClass<>C^.ComponentClass) and
            (P^.ComponentClass.InheritsFrom(C^.ComponentClass)))
        then
          C:=P;
    Inc(I);
  end;
  if C<>nil then
    Result:=C^.EditorClass
  else begin
    if (PropType^.Kind<>tkClass)
    or (GetTypeData(PropType)^.ClassType.InheritsFrom(TPersistent)) then
      Result:=PropClassMap[PropType^.Kind]
    else
      Result:=nil;
  end;
end;

procedure GetComponentProperties(Components: TComponentSelectionList;
  Filter: TTypeKinds; Hook: TPropertyEditorHook; Proc: TGetPropEditProc;
  EditorFilterFunc: TPropertyEditorFilterFunc);
var
  I, J, CompCount: Integer;
  CompType: TClass;
  Candidates: TPropInfoList;
  PropLists: TList;
  PropEditor: TPropertyEditor;
  EdClass: TPropertyEditorClass;
  PropInfo: PPropInfo;
  AddEditor: Boolean;
  Obj: TComponent;
begin
  if (Components = nil) or (Components.Count = 0) then Exit;
  CompCount := Components.Count;
  Obj := Components[0];
  CompType := Components[0].ClassType;
  // Create a property candidate list of all properties that can be found in
  // every component in the list and in the Filter
  Candidates := TPropInfoList.Create(Components[0], Filter);
  try
    // check each property candidate
    for I := Candidates.Count - 1 downto 0 do
    begin
      PropInfo := Candidates[I];
      // check if property is readable
      if (PropInfo^.GetProc=nil)
      or (not GShowReadOnlyProps and ((PropInfo^.PropType^.Kind <> tkClass)
          and (PropInfo^.SetProc = nil)))
      then begin
        Candidates.Delete(I);
        continue;
      end;
      EdClass := GetEditorClass(PropInfo, Obj);
      if EdClass = nil then begin
        Candidates.Delete(I)
      end else
      begin
        // create a test property editor for the property
        PropEditor := EdClass.Create(Hook,Components,1);
        PropEditor.SetPropEntry(0, Components[0], PropInfo);
        PropEditor.Initialize;
        with PropInfo^ do begin
          // check for multiselection, ValueAvailable and customfilter
          if ((CompCount > 1)
              and not (paMultiSelect in PropEditor.GetAttributes))
          or not PropEditor.ValueAvailable
          or (Assigned(EditorFilterFunc) and not EditorFilterFunc(PropEditor))
          then begin
            Candidates.Delete(I);
          end;
        end;
        PropEditor.Free;
      end;
    end;
    PropLists := TList.Create;
    try
      PropLists.Capacity := CompCount;
      // Create a property info list for each component in the selection
      for I := 0 to CompCount - 1 do
        PropLists.Add(TPropInfoList.Create(Components[I], Filter));
      // Eliminate each property in Candidates that is not in all property lists
      for I := 0 to CompCount - 1 do
        Candidates.Intersect(TPropInfoList(PropLists[I]));
      // Eliminate each property in the property list that are not in Candidates
      for I := 0 to CompCount - 1 do
        TPropInfoList(PropLists[I]).Intersect(Candidates);
      // PropList now has a matrix of PropInfo's.
      // -> create property editors for each property
      //    with given each the array of PropInfos
      for I := 0 to Candidates.Count - 1 do
      begin
        EdClass := GetEditorClass(Candidates[I], Obj);
        if EdClass = nil then Continue;
        PropEditor := EdClass.Create(Hook, Components, CompCount);
        AddEditor := True;
        for J := 0 to CompCount - 1 do
        begin
          if (Components[J].ClassType <> CompType) and
            (GetEditorClass(TPropInfoList(PropLists[J])[I],
              Components[J]) <> EdClass) then
          begin
            AddEditor := False;
            Break;
          end;
          PropEditor.SetPropEntry(J, Components[J],
            TPropInfoList(PropLists[J])[I]);
        end;
        if AddEditor then
        begin
          PropEditor.Initialize;
          if not PropEditor.ValueAvailable then AddEditor:=false;
        end;
        if AddEditor then
          Proc(PropEditor)
        else
          PropEditor.Free;
      end;
    finally
      for I := 0 to PropLists.Count - 1 do TPropInfoList(PropLists[I]).Free;
      PropLists.Free;
    end;
  finally
    Candidates.Free;
  end;
end;


{ TPropertyEditor }

constructor TPropertyEditor.Create(Hook: TPropertyEditorHook;
  ComponentList: TComponentSelectionList;  APropCount:Integer);
begin
  FPropertyHook:=Hook;
  FComponents:=ComponentList;
  GetMem(FPropList,APropCount * SizeOf(TInstProp));
  FPropCount:=APropCount;
end;

destructor TPropertyEditor.Destroy;
begin
  if FPropList<>nil then
    FreeMem(FPropList,FPropCount * SizeOf(TInstProp));
end;

procedure TPropertyEditor.Activate;
begin
  //
end;

procedure TPropertyEditor.Deactivate;
begin
  //
end;

function TPropertyEditor.AllEqual:Boolean;
begin
  Result:=FPropCount=1;
end;

procedure TPropertyEditor.Edit;
type
  TGetStrFunc = function(const StrValue:ansistring):Integer of object;
var
  I:Integer;
  Values:TStringList;
  AddValue:TGetStrFunc;
begin
  if not AutoFill then Exit;
  Values:=TStringList.Create;
  Values.Sorted:=paSortList in GetAttributes;
  try
    AddValue := @Values.Add;
    GetValues(TGetStrProc((@AddValue)^));
    if Values.Count > 0 then begin
      I:=Values.IndexOf(FirstValue)+1;
      if I=Values.Count then I:=0;
      FirstValue:=Values[I];
    end;
  finally
    Values.Free;
  end;
end;

function TPropertyEditor.AutoFill:Boolean;
begin
  Result:=True;
end;

function TPropertyEditor.GetAttributes:TPropertyAttributes;
begin
  Result:=[paMultiSelect,paRevertable];
end;

function TPropertyEditor.GetComponent(Index:Integer):TPersistent;
begin
  Result:=FPropList^[Index].Instance;
end;

function TPropertyEditor.GetFloatValue:Extended;
begin
  Result:=GetFloatValueAt(0);
end;

function TPropertyEditor.GetFloatValueAt(Index:Integer):Extended;
begin
  with FPropList^[Index] do Result:=GetFloatProp(Instance,PropInfo);
end;

function TPropertyEditor.GetMethodValue:TMethod;
begin
  Result:=GetMethodValueAt(0);
end;

function TPropertyEditor.GetMethodValueAt(Index:Integer):TMethod;
begin
  with FPropList^[Index] do Result:=GetMethodProp(Instance,PropInfo);
end;

function TPropertyEditor.GetEditLimit:Integer;
begin
  Result:=255;
end;

function TPropertyEditor.GetName:shortstring;
begin
  Result:=FPropList^[0].PropInfo^.Name;
end;

function TPropertyEditor.GetOrdValue:Longint;
begin
  Result:=GetOrdValueAt(0);
end;

function TPropertyEditor.GetOrdValueAt(Index:Integer):Longint;
begin
  with FPropList^[Index] do Result:=GetOrdProp(Instance,PropInfo);
end;

function TPropertyEditor.GetPrivateDirectory:ansistring;
begin
  Result:='';
  if PropertyHook<>nil then
    Result:=PropertyHook.GetPrivateDirectory;
end;

procedure TPropertyEditor.GetProperties(Proc:TGetPropEditProc);
begin
end;

function TPropertyEditor.GetPropInfo:PPropInfo;
begin
  Result:=FPropList^[0].PropInfo;
end;

function TPropertyEditor.GetPropType:PTypeInfo;
begin
  Result:=FPropList^[0].PropInfo^.PropType;
end;

function TPropertyEditor.GetStrValue:AnsiString;
begin
  Result:=GetStrValueAt(0);
end;

function TPropertyEditor.GetStrValueAt(Index:Integer):AnsiString;
begin
  with FPropList^[Index] do Result:=GetStrProp(Instance,PropInfo);
end;

function TPropertyEditor.GetVarValue:Variant;
begin
  Result:=GetVarValueAt(0);
end;

function TPropertyEditor.GetVarValueAt(Index:Integer):Variant;
begin
  with FPropList^[Index] do Result:=GetVariantProp(Instance,PropInfo);
end;

function TPropertyEditor.GetValue:ansistring;
begin
  Result:=srUnknown;
end;

function TPropertyEditor.GetVisualValue:ansistring;
begin
  if AllEqual then
    Result:=GetValue
  else
    Result:='';
end;

procedure TPropertyEditor.GetValues(Proc:TGetStringProc);
begin
end;

procedure TPropertyEditor.Initialize;
begin
  //
end;

procedure TPropertyEditor.Modified;
begin
  if PropertyHook<>nil then
    PropertyHook.Modified;
end;

procedure TPropertyEditor.SetFloatValue(NewValue:Extended);
var
  I:Integer;
begin
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do SetFloatProp(Instance,PropInfo,NewValue);
  Modified;
end;

procedure TPropertyEditor.SetMethodValue(const NewValue:TMethod);
var
  I:Integer;
begin
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do SetMethodProp(Instance,PropInfo,NewValue);
  Modified;
end;

procedure TPropertyEditor.SetOrdValue(NewValue:Longint);
var
  I:Integer;
begin
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do SetOrdProp(Instance,PropInfo,NewValue);
  Modified;
end;

procedure TPropertyEditor.SetPropEntry(Index:Integer;
  AInstance:TPersistent; APropInfo:PPropInfo);
begin
  with FPropList^[Index] do begin
    Instance:=AInstance;
    PropInfo:=APropInfo;
  end;
end;

procedure TPropertyEditor.SetStrValue(const NewValue:AnsiString);
var
  I:Integer;
begin
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do SetStrProp(Instance,PropInfo,NewValue);
  Modified;
end;

procedure TPropertyEditor.SetVarValue(const NewValue:Variant);
var
  I:Integer;
begin
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do SetVariantProp(Instance,PropInfo,NewValue);
  Modified;
end;

procedure TPropertyEditor.Revert;
var I:Integer;
begin
  if PropertyHook<>nil then
    for I:=0 to FPropCount-1 do
      with FPropList^[I] do PropertyHook.Revert(Instance,PropInfo);
end;

procedure TPropertyEditor.SetValue(const NewValue:ansistring);
begin
end;

function TPropertyEditor.ValueAvailable:Boolean;
var
  I:Integer;
begin
  Result:=True;
  for I:=0 to FPropCount-1 do
  begin
    if (FPropList^[I].Instance is TComponent)
    and (csCheckPropAvail in TComponent(FPropList^[I].Instance).ComponentStyle)
    then begin
      try
        GetValue;
        AllEqual;
      except
        Result:=False;
      end;
      Exit;
    end;
  end;
end;

function TPropertyEditor.GetInt64Value:Int64;
begin
  Result:=GetInt64ValueAt(0);
end;

function TPropertyEditor.GetInt64ValueAt(Index:Integer):Int64;
begin
  with FPropList^[Index] do Result:=GetInt64Prop(Instance,PropInfo);
end;

procedure TPropertyEditor.SetInt64Value(NewValue:Int64);
var
  I:Integer;
begin
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do SetInt64Prop(Instance,PropInfo,NewValue);
  Modified;
end;

{ these three procedures implement the default render behavior of the
  object/property inspector's drop down list editor. You don't need to
  override the two measure procedures if the default width or height don't
  need to be changed. }
procedure TPropertyEditor.ListMeasureHeight(const AValue:ansistring;
  Index:integer;  ACanvas:TCanvas;  var AHeight:Integer);
begin
  //
end;

procedure TPropertyEditor.ListMeasureWidth(const AValue:ansistring;
  Index:integer; ACanvas:TCanvas; var AWidth:Integer);
begin
  //
end;

procedure TPropertyEditor.ListDrawValue(const AValue:ansistring; Index:integer;
  ACanvas:TCanvas; const ARect:TRect; AState: TPropEditDrawState);
var
  Style : TTextStyle;
  OldColor : TColor;
begin
  FillChar(Style,SizeOf(Style),0);
  With Style do begin
    Alignment := taLeftJustify;
    Layout := tlCenter;
    Opaque := (pedsInEdit in AState) and (ACanvas.Color <> clNone);
    Clipping := True;
    ShowPrefix := True;
    WordBreak := False;
    SingleLine := True;
    SystemFont := true;
  end;
  If (pedsInComboList in AState) and not (pedsInEdit in AState)
  then begin
    OldColor := ACanvas.Color;
    If pedsSelected in AState then begin
      ACanvas.Color := clHighlight;
      ACanvas.Font.Color := clHighlightText;
    end
    else begin
      ACanvas.Color := clWindow;
      ACanvas.Font.Color := clWindowText;
    end;
    ACanvas.FillRect(ARect);
    ACanvas.Color := OldColor;
  end;
  ACanvas.TextRect(ARect, 2,0,AValue, Style);
end;

{ these three procedures implement the default render behavior of the
  object/property inspector. You don't need to override the measure procedure
  if the default width or height don't need to be changed.  }
procedure TPropertyEditor.PropMeasureHeight(const NewValue:ansistring;
  ACanvas:TCanvas;  var AHeight:Integer);
begin
  //
end;

procedure TPropertyEditor.PropDrawName(ACanvas:TCanvas; const ARect:TRect;
  AState:TPropEditDrawState);
var
  Style : TTextStyle;
begin
  With Style do begin
    Alignment := taLeftJustify;
    Layout := tlCenter;
    Opaque := False;
    Clipping := True;
    ShowPrefix := False;
    WordBreak := False;
    SingleLine := True;
  end;
  ACanvas.TextRect(ARect, 2,0,GetName, Style);
end;

procedure TPropertyEditor.PropDrawValue(ACanvas:TCanvas; const ARect:TRect;
  AState:TPropEditDrawState);
var
  Style : TTextStyle;
begin
  FillChar(Style,SizeOf(Style),0);
  With Style do begin
    Alignment := taLeftJustify;
    Layout := tlCenter;
    Opaque := False;
    Clipping := True;
    ShowPrefix := True;
    WordBreak := False;
    SingleLine := True;
    SystemFont := true;
  end;
  ACanvas.TextRect(ARect, 3,0,GetVisualValue, Style);
end;

procedure TPropertyEditor.UpdateSubProperties;
begin
  if (OnSubPropertiesChanged<>nil) and SubPropertiesNeedsUpdate then
    OnSubPropertiesChanged(Self);
end;

function TPropertyEditor.SubPropertiesNeedsUpdate: boolean;
begin
  Result:=false;
end;

{ TOrdinalPropertyEditor }

function TOrdinalPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  V: Longint;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetOrdValue;
    for I := 1 to PropCount - 1 do
      if GetOrdValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TOrdinalPropertyEditor.GetEditLimit: Integer;
begin
  Result := 63;
end;


{ TIntegerPropertyEditor }

function TIntegerPropertyEditor.GetValue: ansistring;
begin
  with GetTypeData(GetPropType)^ do
    case OrdType of
      otSByte : Result:= IntToStr(ShortInt(GetOrdValue));
      otUByte : Result:= IntToStr(Byte(GetOrdValue));
      otSWord : Result:= IntToStr(SmallInt(GetOrdValue));
      otUWord : Result:= IntToStr(Word(GetOrdValue));
      otULong : Result:= IntToStr(Cardinal(GetOrdValue));
      else Result := IntToStr(GetOrdValue);
    end;
end;

procedure TIntegerPropertyEditor.SetValue(const NewValue: AnsiString);

  procedure Error(const Args: array of const);
  begin
    // XXX
    {raise EPropertyError.CreateResFmt(@SOutOfRange, Args);}
  end;

var
  L: Int64;
begin
  L := StrToInt64(NewValue);
  with GetTypeData(GetPropType)^ do
    if OrdType = otULong then
    begin   // unsigned compare and reporting needed
      if (L < Cardinal(MinValue)) or (L > Cardinal(MaxValue)) then begin
        // bump up to Int64 to get past the %d in the format string
        Error([Int64(Cardinal(MinValue)), Int64(Cardinal(MaxValue))]);
        exit;
      end
    end
    else if (L < MinValue) or (L > MaxValue) then begin
      Error([MinValue, MaxValue]);
      exit;
    end;
  SetOrdValue(L);
end;

{ TCharPropertyEditor }

function TCharPropertyEditor.GetValue: ansistring;
var
  Ch: Char;
begin
  Ch := Chr(GetOrdValue);
  if Ch in [#33..#127] then
    Result := Ch
  else
    Result:='#'+IntToStr(Ord(Ch));
end;

procedure TCharPropertyEditor.SetValue(const NewValue: ansistring);
var
  L: Longint;
begin
  if Length(NewValue) = 0 then L := 0 else
    if Length(NewValue) = 1 then L := Ord(NewValue[1]) else
      if NewValue[1] = '#' then L := StrToInt(Copy(NewValue, 2, Maxint)) else
      begin
        {raise EPropertyError.CreateRes(@SInvalidPropertyValue)};
        exit;
      end;
  with GetTypeData(GetPropType)^ do
    if (L < MinValue) or (L > MaxValue) then begin
      {raise EPropertyError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue])};
      exit;
    end;
  SetOrdValue(L);
end;

{ TEnumPropertyEditor }

function TEnumPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TEnumPropertyEditor.GetValue: ansistring;
var
  L: Longint;
begin
  L := GetOrdValue;
  with GetTypeData(GetPropType)^ do
    if (L < MinValue) or (L > MaxValue) then L := MaxValue;
  Result := GetEnumName(GetPropType, L);
end;

procedure TEnumPropertyEditor.GetValues(Proc: TGetStringProc);
var
  I: Integer;
  EnumType: PTypeInfo;
  s: ShortString;
begin
  EnumType := GetPropType;
  with GetTypeData(EnumType)^ do
    for I := MinValue to MaxValue do begin
      s := GetEnumName(EnumType, I);
      Proc(s);
    end;
end;

procedure TEnumPropertyEditor.SetValue(const NewValue: ansistring);
var
  I: Integer;
begin
  I := GetEnumValue(GetPropType, NewValue);
  if I < 0 then begin
    {raise EPropertyError.CreateRes(@SInvalidPropertyValue)};
//    exit;
  end;
  SetOrdValue(I);
end;

{ TBoolPropertyEditor  }

function TBoolPropertyEditor.GetValue: ansistring;
begin
  if GetOrdValue = 0 then
    Result := 'False'
  else
    Result := 'True';
end;

procedure TBoolPropertyEditor.GetValues(Proc: TGetStringProc);
begin
  Proc('False');
  Proc('True');
end;

procedure TBoolPropertyEditor.SetValue(const NewValue: ansistring);
var
  I: Integer;
begin
  if CompareText(NewValue, 'False') = 0 then
    I := 0
  else if CompareText(NewValue, 'True') = 0 then
    I := -1
  else
    I := StrToInt(NewValue);
  SetOrdValue(I);
end;

{ TInt64PropertyEditor }

function TInt64PropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  V: Int64;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetInt64Value;
    for I := 1 to PropCount - 1 do
      if GetInt64ValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TInt64PropertyEditor.GetEditLimit: Integer;
begin
  Result := 63;
end;

function TInt64PropertyEditor.GetValue: ansistring;
begin
  Result := IntToStr(GetInt64Value);
end;

procedure TInt64PropertyEditor.SetValue(const NewValue: ansistring);
begin
  SetInt64Value(StrToInt64(NewValue));
end;


{ TFloatPropertyEditor }

function TFloatPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  V: Extended;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetFloatValue;
    for I := 1 to PropCount - 1 do
      if GetFloatValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TFloatPropertyEditor.GetValue: ansistring;
const
  Precisions: array[TFloatType] of Integer = (7, 15, 19, 19, 19
{$ifdef VER1_0}
  , 15, 31
{$endif VER1_0}
  );
begin
  Result := FloatToStrF(GetFloatValue, ffGeneral,
    Precisions[GetTypeData(GetPropType)^.FloatType], 0);
end;

procedure TFloatPropertyEditor.SetValue(const NewValue: ansistring);
begin
  SetFloatValue(StrToFloat(NewValue));
end;

{ TStringPropertyEditor }

function TStringPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  V: ansistring;
begin
  Result := False;
  if PropCount > 1 then begin
    V := GetStrValue;
    for I := 1 to PropCount - 1 do
      if GetStrValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TStringPropertyEditor.GetEditLimit: Integer;
begin
  if GetPropType^.Kind = tkString then
    Result := GetTypeData(GetPropType)^.MaxLength else
    Result := 255;
end;

function TStringPropertyEditor.GetValue: ansistring;
begin
  Result := GetStrValue;
end;

procedure TStringPropertyEditor.SetValue(const NewValue: ansistring);
begin
  SetStrValue(NewValue);
end;

{ TNestedPropertyEditor }

constructor TNestedPropertyEditor.Create(Parent: TPropertyEditor);
begin
  FPropertyHook:=Parent.PropertyHook;
  FComponents:=Parent.FComponents;
  FPropList:=Parent.FPropList;
  FPropCount:=Parent.PropCount;
end;

destructor TNestedPropertyEditor.Destroy;
begin
end;

{ TSetElementPropertyEditor }

constructor TSetElementPropertyEditor.Create(Parent: TPropertyEditor;
 AElement: Integer);
begin
  inherited Create(Parent);
  FElement := AElement;
end;

// XXX
// The IntegerSet (a set of size of an integer)
// don't know if this is always valid
type
  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;

function TSetElementPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  S: TIntegerSet;
  V: Boolean;
begin
  Result := False;
  if PropCount > 1 then begin
    Integer(S) := GetOrdValue;
    V := FElement in S;
    for I := 1 to PropCount - 1 do begin
      Integer(S) := GetOrdValueAt(I);
      if (FElement in S) <> V then Exit;
    end;
  end;
  Result := True;
end;

function TSetElementPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList];
end;

function TSetElementPropertyEditor.GetName: shortstring;
begin
  Result := GetEnumName(GetTypeData(GetPropType)^.CompType, FElement);
end;

function TSetElementPropertyEditor.GetValue: ansistring;
var
  S: TIntegerSet;
begin
  Integer(S) := GetOrdValue;
  Result := BooleanIdents[FElement in S];
end;

procedure TSetElementPropertyEditor.GetValues(Proc: TGetStringProc);
begin
  Proc(BooleanIdents[False]);
  Proc(BooleanIdents[True]);
end;

procedure TSetElementPropertyEditor.SetValue(const NewValue: ansistring);
var
  S: TIntegerSet;
begin
  Integer(S) := GetOrdValue;
  if CompareText(NewValue, 'True') = 0 then
    Include(S, FElement) else
    Exclude(S, FElement);
  SetOrdValue(Integer(S));
end;

{ TSetPropertyEditor }

function TSetPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly, paRevertable];
end;

procedure TSetPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType)^ do
    for I := MinValue to MaxValue do
      Proc(TSetElementPropertyEditor.Create(Self, I));
end;

function TSetPropertyEditor.GetValue: ansistring;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Integer(S) := GetOrdValue;
  TypeInfo := GetTypeData(GetPropType)^.CompType;
  Result := '[';
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Length(Result) <> 1 then Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  Result := Result + ']';
end;

{ TListElementPropertyEditor }

constructor TListElementPropertyEditor.Create(Parent: TListPropertyEditor;
  AnIndex: integer);
begin
  inherited Create(Parent);
  FList:=Parent;
  FIndex:=AnIndex;
end;

destructor TListElementPropertyEditor.Destroy;
begin
  inherited Destroy;
end;

function TListElementPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=List.GetElementAttributes(Self);
end;

function TListElementPropertyEditor.GetName: shortstring;
begin
  Result:=List.GetElementName(Self);
end;

procedure TListElementPropertyEditor.GetProperties(Proc: TGetPropEditProc);
begin
  List.GetElementProperties(Self,Proc);
end;

function TListElementPropertyEditor.GetValue: ansistring;
begin
  Result:=List.GetElementValue(Self);
end;

procedure TListElementPropertyEditor.GetValues(Proc: TGetStringProc);
begin
  List.GetElementValues(Self,Proc);
end;

procedure TListElementPropertyEditor.SetValue(const NewValue: ansistring);
begin
  List.SetElementValue(Self,NewValue);
end;

{ TListPropertyEditor }

function TListPropertyEditor.GetElementCount: integer;
begin
  if not IsSaving then
    Result:=SavedElements.Count
  else
    Result:=ReadElementCount;
end;

function TListPropertyEditor.GetElement(Index: integer): TObject;
var
  ElementCount: integer;
begin
  // do some checks
  if (Index<0) then
    raise Exception('TListPropertyEditor.GetElement Index='+IntToStr(Index));
  ElementCount:=GetElementCount;
  if Index>=ElementCount then
    raise Exception('TListPropertyEditor.GetElement Index='+IntToStr(Index)
      +' Count='+IntToStr(ElementCount));
  // get element
  if not IsSaving then
    Result:=TObject(SavedElements[Index])
  else
    Result:=ReadElement(Index);
end;

function TListPropertyEditor.GetElement(Element: TListElementPropertyEditor
  ): TObject;
begin
  Result:=GetElement(Element.TheIndex);
end;

function TListPropertyEditor.GetElementPropEditor(Index: integer
  ): TListElementPropertyEditor;
begin
  if not IsSaving then
    Result:=TListElementPropertyEditor(SavedPropertyEditors[Index])
  else
    Result:=CreateElementPropEditor(Index);
end;

procedure TListPropertyEditor.SaveElements;
begin
  if IsSaving then exit;
  BeginSaveElement;
  FreeElementPropertyEditors;
  DoSaveElements;
  FSubPropertiesChanged:=false;
  EndSaveElement;
end;

function TListPropertyEditor.SubPropertiesNeedsUpdate: boolean;
var i: integer;
begin
  Result:=true;
  if FSubPropertiesChanged then exit;
  FSubPropertiesChanged:=true;
  if SavedList<>GetComponent(0) then exit;
  if ReadElementCount<>SavedElements.Count then exit;
  for i:=0 to SavedElements.Count-1 do
    if TObject(SavedElements[i])<>ReadElement(i) then exit;
  Result:=false;
  FSubPropertiesChanged:=false;
end;

function TListPropertyEditor.ReadElementCount: integer;
var
  TheList: TList;
begin
  TheList:=TList(GetOrdValue);
  if (TheList<>nil) and (TheList is TList) then
    Result:=TheList.Count
  else
    Result:=0;
end;

function TListPropertyEditor.ReadElement(Index: integer): TObject;
begin
  Result:=TObject(TList(GetOrdValue).Items[Index]);
end;

function TListPropertyEditor.CreateElementPropEditor(Index: integer
  ): TListElementPropertyEditor;
begin
  Result:=TListElementPropertyEditor.Create(Self,Index);
end;

procedure TListPropertyEditor.BeginSaveElement;
begin
  inc(FSaveElementLock);
end;

procedure TListPropertyEditor.EndSaveElement;
begin
  dec(FSaveElementLock);
  if FSaveElementLock<0 then
    writeln('TListPropertyEditor.EndSaveElement ERROR: FSaveElementLock=',FSaveElementLock);
end;

procedure TListPropertyEditor.DoSaveElements;
var
  i, ElementCount: integer;
begin
  SavedList:=GetComponent(0);
  ElementCount:=GetElementCount;
  SavedElements.Count:=ElementCount;
  for i:=0 to ElementCount-1 do
    SavedElements[i]:=GetElement(i);
  SavedPropertyEditors.Count:=ElementCount;
  for i:=0 to ElementCount-1 do
    SavedPropertyEditors[i]:=GetElementPropEditor(i);
end;

procedure TListPropertyEditor.FreeElementPropertyEditors;
var
  i: integer;
begin
  for i:=0 to SavedPropertyEditors.Count-1 do
    TObject(SavedPropertyEditors[i]).Free;
  SavedPropertyEditors.Clear;
end;

function TListPropertyEditor.GetElementAttributes(
  Element: TListElementPropertyEditor
  ): TPropertyAttributes;
begin
  Result:= [paReadOnly];
end;

function TListPropertyEditor.GetElementName(Element: TListElementPropertyEditor
  ): shortstring;
begin
  Result:='Item '+IntToStr(Element.TheIndex);
end;

procedure TListPropertyEditor.GetElementProperties(
  Element: TListElementPropertyEditor; Proc: TGetPropEditProc);
begin

end;

function TListPropertyEditor.GetElementValue(Element: TListElementPropertyEditor
  ): ansistring;
begin
  Result:='';
end;

procedure TListPropertyEditor.GetElementValues(
  Element: TListElementPropertyEditor; Proc: TGetStringProc);
begin

end;

procedure TListPropertyEditor.SetElementValue(
  Element: TListElementPropertyEditor; NewValue: ansistring);
begin

end;

function TListPropertyEditor.IsSaving: boolean;
begin
  Result:=SaveElementLock>0;
end;

constructor TListPropertyEditor.Create(Hook: TPropertyEditorHook;
  ComponentList: TComponentSelectionList; APropCount: Integer);
begin
  inherited Create(Hook, ComponentList, APropCount);
  SavedElements:=TList.Create;
  SavedPropertyEditors:=TList.Create;
end;

destructor TListPropertyEditor.Destroy;
begin
  UnregisterListPropertyEditor(Self);
  FreeElementPropertyEditors;
  FreeAndNil(SavedPropertyEditors);
  FreeAndNil(SavedElements);
  inherited Destroy;
end;

function TListPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paSubProperties, paDynamicSubProps, paReadOnly];
end;

procedure TListPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  i, ElementCount: integer;
begin
  SaveElements;
  ElementCount:=GetElementCount;
  for i:=0 to ElementCount-1 do
    Proc(GetElementPropEditor(i));
end;

function TListPropertyEditor.GetValue: AnsiString;
var
  ElementCount: integer;
begin
  ElementCount:=GetElementCount;
  if ElementCount<>1 then
    Result:=IntToStr(GetElementCount)+' items'
  else
    Result:='1 item';
end;

procedure TListPropertyEditor.Initialize;
begin
  inherited Initialize;
  RegisterListPropertyEditor(Self);
  SaveElements;
end;

{ TCollectionPropertyEditor }

Type
  TCollectionPropertyEditorForm = Class(TForm)
  protected
    CollectionList : TLISTBOX;
    ButtonPanel: TPANEL;
    AddButton: TSPEEDBUTTON;
    DeleteButton: TSPEEDBUTTON;
    procedure ListCLICK(Sender: TObject);
    procedure AddCLICK(Sender: TObject);
    procedure DeleteCLICK(Sender: TObject);
    procedure UpdateCaption;
  public
    Collection : TCollection;
    ComponentName,
    PropertyName : String;
    Procedure PropagateList;
    Constructor Create(AOwner : TComponent); Override;
  end;

const
  CollectionForm : TCollectionPropertyEditorForm = nil;

Constructor TCollectionPropertyEditorForm.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);

  Position := poDefault;

  Height:= 216;
  Width:= 166;

  ButtonPanel := TPanel.Create(Self);
  With ButtonPanel do begin
    Parent := Self;
    Align:= altop;
    BevelOuter:= bvRaised;
    BevelInner:= bvLowered;
    BorderWidth:= 2;
    Height:= 41;
  end;

  AddButton:= TSpeedButton.Create(Self);
  With AddButton do begin
    Parent:= ButtonPanel;
    Caption:= oiscAdd;
    OnClick:= @AddClick;
    SetBounds(6, 6, 43, 27);
  end;

  DeleteButton := TSpeedButton.Create(Self);
  With DeleteButton do begin
    Parent:= ButtonPanel;
    Caption:= oiscDelete;
    OnClick:= @DeleteCLICK;
    SetBounds(56, 6, 43, 27);
  end;

  CollectionList := TListBox.Create(Self);
  With CollectionList do begin
    Parent:= Self;
    Align:= alclient;
//  MultiSelect:= true;
    OnClick:= @ListClick;
  end;
end;

procedure TCollectionPropertyEditorForm.UpdateCaption;
begin
  //I think to match Delphi this should be formated like
  //"Editing ComponentName.PropertyName[Index]"
  Caption:= 'Editing ' + ComponentName + '.' + PropertyName;
  If CollectionList.ItemIndex > -1 then
    Caption := Caption + '[' +
      IntToStr(CollectionList.ItemIndex) + ']';
end;

procedure TCollectionPropertyEditorForm.PropagateList;
var
  I : Longint;
begin
  CollectionList.Items.Clear;
  for I:= 0 to Collection.Count - 1 do
    CollectionList.Items.Add(Collection.Items[I].DisplayName);
  DeleteButton.Enabled:= CollectionList.ItemIndex > -1;
  UpdateCaption;
end;

procedure TCollectionPropertyEditorForm.ListClick(Sender: TObject);
//var i : integer;
begin
  DeleteButton.Enabled := CollectionList.ItemIndex > -1;
  UpdateCaption;
  //XXX - Select Collection.Items[CollectionList.ItemIndex]
  //in OI - once it supports TPersistent
{  if CollectionList.SelCount > 0 then begin
    TheControlSelection.BeginUpdate;
    TheControlSelection.Clear;
    for i:= 0 to CollectionList.Items.Count - 1 do begin
      if CollectionList.Selected[i] then
        TheControlSelection.Add(Collection.Items[i]);
    end;
  end;}
end;

procedure TCollectionPropertyEditorForm.AddCLICK(Sender: TObject);
var
  I : Integer;
begin
  Collection.Add;
  I := CollectionList.ItemIndex;
  PropagateList;
  If I > -1 then
    CollectionList.ItemIndex := I;
  DeleteButton.Enabled := CollectionList.ItemIndex > -1;
  UpdateCaption;
end;

procedure TCollectionPropertyEditorForm.DeleteCLICK(Sender: TObject);
var
  I : Integer;
begin
  Collection.Items[CollectionList.ItemIndex].Free;
  I := CollectionList.ItemIndex;
  PropagateList;
  If I >= CollectionList.Items.Count then
    I := I - 1;
  If I > -1 then
    CollectionList.ItemIndex := I;
  DeleteButton.Enabled := CollectionList.ItemIndex > -1;
end;

//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

function TCollectionPropertyEditor.ReadElement(Index: integer): TObject;
var
  Collection: TCollection;
begin
  Collection:=TCollection(GetOrdValue);
  Result:=Collection.Items[Index];
end;

function TCollectionPropertyEditor.GetElementAttributes(
  Element: TListElementPropertyEditor): TPropertyAttributes;
begin
  Result := [paSubProperties, paReadOnly];
end;

function TCollectionPropertyEditor.GetElementName(
  Element: TListElementPropertyEditor): shortstring;
begin
  Result:=inherited GetElementName(Element);
end;

procedure TCollectionPropertyEditor.GetElementProperties(
  Element: TListElementPropertyEditor; Proc: TGetPropEditProc);
var
  Components: TComponentSelectionList;
begin
  Components := TComponentSelectionList.Create;
  Components.Add(TComponent(GetElement(Element)));
  GetComponentProperties(Components,tkProperties,PropertyHook,Proc,nil);
  Components.Free;
end;

function TCollectionPropertyEditor.GetElementValue(
  Element: TListElementPropertyEditor): ansistring;
begin
  Result:=IntToStr(TCollectionItem(GetElement(Element)).ID);
end;

procedure TCollectionPropertyEditor.GetElementValues(
  Element: TListElementPropertyEditor; Proc: TGetStringProc);
begin
  inherited GetElementValues(Element, Proc);
end;

procedure TCollectionPropertyEditor.SetElementValue(
  Element: TListElementPropertyEditor; NewValue: ansistring);
begin
  inherited SetElementValue(Element, NewValue);
end;

function TCollectionPropertyEditor.ReadElementCount: integer;
var
  Collection: TCollection;
begin
  Collection:=TCollection(GetOrdValue);
  if (Collection<>nil) and (Collection is TCollection) then
    Result:=Collection.Count
  else
    Result:=0;
end;

function TCollectionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

Procedure TCollectionPropertyEditor.Edit;
begin
  If Assigned(CollectionForm) then
    CollectionForm.Free;
  CollectionForm := TCollectionPropertyEditorForm.Create(Application);
  CollectionForm.Collection := TCollection(GetOrdValue);
  CollectionForm.PropertyName := GetPropInfo^.Name;
  CollectionForm.ComponentName := '';//What if its in a Persistent child?
  CollectionForm.Caption := 'Editing ' + GetPropInfo^.Name;
  CollectionForm.PropagateList;
  CollectionForm.Show;
end;

{ TClassPropertyEditor }

function TClassPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

procedure TClassPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
  SubComponent: TComponent;
  Components: TComponentSelectionList;
begin
  Components := TComponentSelectionList.Create;
  try
    for I := 0 to PropCount - 1 do begin
      SubComponent:=TComponent(GetOrdValueAt(I));
      if SubComponent<>nil then
        Components.Add(SubComponent);
    end;
    GetComponentProperties(Components,tkProperties,PropertyHook,Proc,nil);
  finally
    Components.Free;
  end;
end;

function TClassPropertyEditor.GetValue: ansistring;
begin
  Result:='('+GetPropType^.Name+')';
end;

{ TMethodPropertyEditor }

function TMethodPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  V, T: TMethod;
begin
  Result := False;
  if PropCount > 1 then begin
    V := GetMethodValue;
    for I := 1 to PropCount - 1 do begin
      T := GetMethodValueAt(I);
      if (T.Code <> V.Code) or (T.Data <> V.Data) then Exit;
    end;
  end;
  Result := True;
end;

procedure TMethodPropertyEditor.Edit;
var
  FormMethodName: shortstring;
begin
  FormMethodName := GetValue;
  writeln('### TMethodPropertyEditor.Edit A OldValue=',FormMethodName);
  if (not IsValidIdent(FormMethodName))
  or PropertyHook.MethodFromAncestor(GetMethodValue) then begin
    if not IsValidIdent(FormMethodName) then
      FormMethodName := GetFormMethodName;
    writeln('### TMethodPropertyEditor.Edit B FormMethodName=',FormMethodName);
    if not IsValidIdent(FormMethodName) then begin
      raise EPropertyError.Create('Method name must be an identifier'{@SCannotCreateName});
      exit;
    end;
    SetValue(FormMethodName);
    PropertyHook.RefreshPropertyValues;
  end;
  PropertyHook.ShowMethod(FormMethodName);
end;

function TMethodPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paSortList, paRevertable];
end;

function TMethodPropertyEditor.GetEditLimit: Integer;
begin
  Result := MaxIdentLength;
end;

function TMethodPropertyEditor.GetFormMethodName: shortstring;
var I: Integer;
begin
  Result:='';
  if PropertyHook.LookupRoot=nil then exit;
  if GetComponent(0) = PropertyHook.LookupRoot then begin
    Result := PropertyHook.GetRootClassName;
    if (Result <> '') and (Result[1] = 'T') then
      System.Delete(Result, 1, 1);
  end else begin
    Result := PropertyHook.GetObjectName(GetComponent(0));
    for I := Length(Result) downto 1 do
      if Result[I] in ['.','[',']'] then
        System.Delete(Result, I, 1);
  end;
  if Result = '' then begin
    {raise EPropertyError.CreateRes(@SCannotCreateName);}
    exit;
  end;
  Result := Result + GetTrimmedEventName;
end;

function TMethodPropertyEditor.GetTrimmedEventName: shortstring;
begin
  Result := GetName;
  if (Length(Result) >= 2)
  and (Result[1] in ['O','o']) and (Result[2] in ['N','n'])
  then
    System.Delete(Result,1,2);
end;

function TMethodPropertyEditor.GetValue: ansistring;
begin
  Result:=PropertyHook.GetMethodName(GetMethodValue);
end;

procedure TMethodPropertyEditor.GetValues(Proc: TGetStringProc);
begin
writeln('### TMethodPropertyEditor.GetValues');
  Proc('(None)');
  PropertyHook.GetMethods(GetTypeData(GetPropType), Proc);
end;

procedure TMethodPropertyEditor.SetValue(const NewValue: ansistring);
  {
  procedure CheckChainCall(const MethodName: shortstring; Method: TMethod);
  var
    Persistent: TPersistent;
    Component: TComponent;
    InstanceMethod: shortstring;
    Instance: TComponent;
  begin
    Persistent := GetComponent(0);
    if Persistent is TComponent then begin
      Component := TComponent(Persistent);
      if (Component.Name <> '')
      and (TObject(Method.Data) <> PropertyHook.LookupRoot)
      and (TObject(Method.Data) is TComponent) then
      begin
        Instance := TComponent(Method.Data);
        InstanceMethod := Instance.MethodName(Method.Code);
        if InstanceMethod <> '' then begin
          PropertyHook.ChainCall(MethodName, Instance.Name, InstanceMethod,
            GetTypeData(GetPropType));
        end;
      end;
    end;
  end;
  }
var
  CreateNewMethod: Boolean;
  CurValue: ansistring;
  //OldMethod: TMethod;
  NewMethodExists,NewMethodIsCompatible,NewMethodIsPublished,
  NewIdentIsMethod: boolean;
begin
  CurValue:=GetValue;
  if CurValue=NewValue then exit;
  writeln('### TMethodPropertyEditor.SetValue A OldValue="',CurValue,'" NewValue=',NewValue);
  NewMethodExists:=IsValidIdent(NewValue)
               and PropertyHook.MethodExists(NewValue,GetTypeData(GetPropType),
                   NewMethodIsCompatible,NewMethodIsPublished,NewIdentIsMethod);
  //writeln('### TMethodPropertyEditor.SetValue B NewMethodExists=',NewMethodExists,' NewMethodIsCompatible=',NewMethodIsCompatible,' ',NewMethodIsPublished,' ',NewIdentIsMethod);
  if NewMethodExists then begin
    if not NewIdentIsMethod then begin
      if MessageDlg('Incompatible Identifier',
        'The identifier "'+NewValue+'" is not a method.'#13
        +'Press OK to undo,'#13
        +'press Ignore to force it.',mtWarning,[mbOk,mbIgnore],0)=mrOk
      then
        exit;
    end;
    if not NewMethodIsPublished then begin
      if MessageDlg('Incompatible Method',
        'The method "'+NewValue+'" is not published.'#13
        +'Press OK to undo,'#13
        +'press Ignore to force it.',mtWarning,[mbOk,mbIgnore],0)=mrOk
      then
        exit;
    end;
    if not NewMethodIsCompatible then begin
      if MessageDlg('Incompatible Method',
        'The method "'+NewValue+'" is incompatible to this event.'#13
        +'Press OK to undo,'#13
        +'press Ignore to force it.',mtWarning,[mbOk,mbIgnore],0)=mrOk
      then
        exit;
    end;
  end;
  //writeln('### TMethodPropertyEditor.SetValue C');
  if IsValidIdent(CurValue) and IsValidIdent(NewValue)
  and (not NewMethodExists)
  and (not PropertyHook.MethodFromAncestor(GetMethodValue)) then begin
    // rename the method
    // Note:
    //   All other not selected properties that use this method, contains just
    //   the TMethod record. So, changing the name in the jitform will change
    //   all other event names in all other components automatically.
    //writeln('### TMethodPropertyEditor.SetValue D');
    PropertyHook.RenameMethod(CurValue, NewValue)
  end else
  begin
    //writeln('### TMethodPropertyEditor.SetValue E');
    CreateNewMethod := IsValidIdent(NewValue) and not NewMethodExists;
    //OldMethod := GetMethodValue;
    SetMethodValue(PropertyHook.CreateMethod(NewValue,GetPropType));
    //writeln('### TMethodPropertyEditor.SetValue F NewValue=',GetValue);
    if CreateNewMethod then begin
      {if (PropCount = 1) and (OldMethod.Data <> nil) and (OldMethod.Code <> nil)
      then
        CheckChainCall(NewValue, OldMethod);}
      //writeln('### TMethodPropertyEditor.SetValue G');
      PropertyHook.ShowMethod(NewValue);
    end;
  end;
writeln('### TMethodPropertyEditor.SetValue END  NewValue=',GetValue);
end;

{ TComponentPropertyEditor }

function TComponentPropertyEditor.FilterFunc(
  const ATestEditor: TPropertyEditor{IProperty}): Boolean;
begin
  Result := not (paNotNestable in ATestEditor.GetAttributes);
end;

function TComponentPropertyEditor.GetComponentReference: TComponent;
begin
  Result := TComponent(GetOrdValue);
end;

function TComponentPropertyEditor.GetSelections:
  TComponentSelectionList{IDesignerSelections};
var
  I: Integer;
begin
  Result := nil;
  if (GetComponentReference <> nil) and AllEqual then
  begin
    Result := TComponentSelectionList.Create;
    for I := 0 to PropCount - 1 do
      Result.Add(TComponent(GetOrdValueAt(I)));
  end;
end;

function TComponentPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  LInstance: TComponent;
begin
  Result := False;
  LInstance := TComponent(GetOrdValue);
  if PropCount > 1 then
    for I := 1 to PropCount - 1 do
      if TComponent(GetOrdValueAt(I)) <> LInstance then
        Exit;
  Result := FindRootDesigner(LInstance)<>nil;
            //Supports(FindRootDesigner(LInstance), IDesigner);
end;

procedure TComponentPropertyEditor.Edit;
var
  Temp: TComponent;
  Designer: TIDesigner;
begin
  Temp := GetComponentReference;
  if Temp<>nil then begin
    Designer:=FindRootDesigner(Temp);
    if (Designer.GetShiftState * [ssCtrl, ssLeft] = [ssCtrl, ssLeft]) then
      Designer.SelectOnlyThisComponent(Temp)
    else
      inherited Edit;
  end else
    inherited Edit;
end;

function TComponentPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect];
  if Assigned(GetPropInfo^.SetProc) then
    Result := Result + [paValueList, paSortList, paRevertable]
  else
    Result := Result + [paReadOnly];
  if GReferenceExpandable and (GetComponentReference <> nil) and AllEqual then
    Result := Result + [paSubProperties, paVolatileSubProperties];
end;

procedure TComponentPropertyEditor.GetProperties(Proc:TGetPropEditProc);
var
  LComponents: TComponentSelectionList;
  //LDesigner: TIDesigner;
begin
  LComponents := GetSelections;
  if LComponents <> nil then
  begin
    //if not Supports(FindRootDesigner(LComponents[0]), IDesigner, LDesigner) then
    //  LDesigner := Designer;
    GetComponentProperties(LComponents, tkAny, PropertyHook, Proc, nil);
  end;
end;

function TComponentPropertyEditor.GetEditLimit: Integer;
begin
  Result := MaxIdentLength;
end;

function TComponentPropertyEditor.GetValue: AnsiString;
var Component: TComponent;
begin
  Component:=TComponent(GetOrdValue);
  if Assigned(PropertyHook) then begin
    Result:=PropertyHook.GetComponentName(Component);
  end else begin
    if Assigned(Component) then
      Result:=Component.Name
    else
      Result:='';
  end;
end;

procedure TComponentPropertyEditor.GetValues(Proc: TGetStringProc);
begin
  Proc('(none)');
  if Assigned(PropertyHook) then
    PropertyHook.GetComponentNames(GetTypeData(GetPropType), Proc);
end;

procedure TComponentPropertyEditor.SetValue(const NewValue: ansistring);
var Component: TComponent;
begin
  if (NewValue = '') or (NewValue='(none)') then
    Component := nil
  else begin
    if Assigned(PropertyHook) then begin
      Component := PropertyHook.GetComponent(NewValue);
      if not (Component is GetTypeData(GetPropType)^.ClassType) then begin
        raise EPropertyError.Create('Invalid property value'{@SInvalidPropertyValue});
      end;
    end;
  end;
  SetOrdValue(Longint(Component));
end;


{ TInterfaceProperty }

function TInterfaceProperty.AllEqual: Boolean;
{var
  I: Integer;
  LInterface: IInterface;}
begin
  Result := False;
{  LInterface := GetIntfValue;
  if PropCount > 1 then
    for I := 1 to PropCount - 1 do
      if GetIntfValueAt(I) <> LInterface then
        Exit;
  Result := Supports(FindRootDesigner(GetComponent(LInterface)), IDesigner);}
end;

function TInterfaceProperty.GetComponent(
  const AInterface: Pointer {IInterface}): TComponent;
{var
  ICR: IInterfaceComponentReference;}
begin
{  if (AInterface <> nil) and
     Supports(AInterface, IInterfaceComponentReference, ICR) then
    Result := ICR.GetComponent
  else}
    Result := nil;
end;

function TInterfaceProperty.GetComponentReference: TComponent;
begin
  Result := nil; //GetComponent(GetIntfValue);
end;

function TInterfaceProperty.GetSelections: TComponentSelectionList{IDesignerSelections};
{var
  I: Integer;}
begin
  Result := nil;
{  if (GetIntfValue <> nil) and AllEqual then
  begin
    Result := TDesignerSelections.Create;
    for I := 0 to PropCount - 1 do
      Result.Add(GetComponent(GetIntfValueAt(I)));
  end;}
end;

procedure TInterfaceProperty.ReceiveComponentNames(const S: string);
{var
  Temp: TComponent;
  Intf: IInterface;}
begin
{  Temp := Designer.GetComponent(S);
  if Assigned(FGetValuesStrProc) and
     Assigned(Temp) and
     Supports(TObject(Temp), GetTypeData(GetPropType)^.Guid, Intf) then
    FGetValuesStrProc(S);}
end;

procedure TInterfaceProperty.GetValues(Proc: TGetStrProc);
begin
{  FGetValuesStrProc := Proc;
  try
    Designer.GetComponentNames(GetTypeData(TypeInfo(TComponent)), ReceiveComponentNames);
  finally
    FGetValuesStrProc := nil;
  end;}
end;

procedure TInterfaceProperty.SetValue(const Value: string);
{var
  Intf: IInterface;
  Component: TComponent;}
begin
{  if Value = '' then
    Intf := nil
  else
  begin
    Component := Designer.GetComponent(Value);
    if (Component = nil) or
      not Supports(TObject(Component), GetTypeData(GetPropType)^.Guid, Intf) then
      raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  end;
  SetIntfValue(Intf);}
end;


{ TComponentNamePropertyEditor }

function TComponentNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [];
end;

function TComponentNamePropertyEditor.GetEditLimit: Integer;
begin
  Result := MaxIdentLength;
end;

function TComponentNamePropertyEditor.GetValue: ansistring;
begin
  Result:=inherited GetValue;
end;

procedure TComponentNamePropertyEditor.SetValue(const NewValue: ansistring);
begin
  if (not IsValidIdent(NewValue)) or (NewValue='') then
    raise Exception.Create('Component name "'+NewValue+'" is not a valid identifier');
  inherited SetValue(NewValue);
  PropertyHook.ComponentRenamed(TComponent(GetComponent(0)));
end;

{ TDateProperty }

function TDateProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

function TDateProperty.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then Result := '' else
  Result := DateToStr(DT);
end;

procedure TDateProperty.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then DT := 0.0
  else DT := StrToDate(Value);
  SetFloatValue(DT);
end;

{ TTimePropertyEditor }

function TTimePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

function TTimePropertyEditor.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then Result := '' else
  Result := TimeToStr(DT);
end;

procedure TTimePropertyEditor.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then DT := 0.0
  else DT := StrToTime(Value);
  SetFloatValue(DT);
end;

{ TDateTimePropertyEditor }

function TDateTimePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

function TDateTimePropertyEditor.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue;
  if DT = 0.0 then Result := '' else
  Result := DateTimeToStr(DT);
end;

procedure TDateTimePropertyEditor.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then DT := 0.0
  else DT := StrToDateTime(Value);
  SetFloatValue(DT);
end;

{ TVariantPropertyEditor }

function TVariantPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties];
end;

procedure TVariantPropertyEditor.GetProperties(Proc:TGetPropEditProc);
begin
  //Proc(TVariantTypeProperty.Create(Self));
end;

function TVariantPropertyEditor.GetValue: string;
{
  function GetVariantStr(const Value: Variant): string;
  begin
    case VarType(Value) of
      varBoolean:
        Result := BooleanIdents[Value = True];
      varCurrency:
        Result := CurrToStr(Value);
    else
      Result := VarToStrDef(Value, SNull);
    end;
  end;

var
  Value: Variant;}
begin
  Result:='';
{  Value := GetVarValue;
  if VarType(Value) <> varDispatch then
    Result := GetVariantStr(Value)
  else
    Result := 'ERROR';}
end;

procedure TVariantPropertyEditor.SetValue(const Value: string);
{
  function Cast(var Value: Variant; NewType: Integer): Boolean;
  var
    V2: Variant;
  begin
    Result := True;
    if NewType = varCurrency then
      Result := AnsiPos(CurrencyString, Value) > 0;
    if Result then
    try
      VarCast(V2, Value, NewType);
      Result := (NewType = varDate) or (VarToStr(V2) = VarToStr(Value));
      if Result then Value := V2;
    except
      Result := False;
    end;
  end;

var
  V: Variant;
  OldType: Integer;}
begin
{  OldType := VarType(GetVarValue);
  V := Value;
  if Value = '' then
    VarClear(V) else
  if (CompareText(Value, SNull) = 0) then
    V := NULL else
  if not Cast(V, OldType) then
    V := Value;
  SetVarValue(V);}
end;


{ TModalResultPropertyEditor }

const
  ModalResults: array[mrNone..mrLast] of shortstring = (
    'mrNone',
    'mrOk',
    'mrCancel',
    'mrAbort',
    'mrRetry',
    'mrIgnore',
    'mrYes',
    'mrNo',
    'mrAll',
    'mrNoToAll',
    'mrYesToAll');

function TModalResultPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TModalResultPropertyEditor.GetValue: ansistring;
var
  CurValue: Longint;
begin
  CurValue := GetOrdValue;
  case CurValue of
    Low(ModalResults)..High(ModalResults):
      Result := ModalResults[CurValue];
  else
    Result := IntToStr(CurValue);
  end;
end;

procedure TModalResultPropertyEditor.GetValues(Proc: TGetStringProc);
var
  I: Integer;
begin
  for I := Low(ModalResults) to High(ModalResults) do Proc(ModalResults[I]);
end;

procedure TModalResultPropertyEditor.SetValue(const NewValue: ansistring);
var
  I: Integer;
begin
  if NewValue = '' then begin
    SetOrdValue(0);
    Exit;
  end;
  for I := Low(ModalResults) to High(ModalResults) do
    if CompareText(ModalResults[I], NewValue) = 0 then
    begin
      SetOrdValue(I);
      Exit;
    end;
  inherited SetValue(NewValue);
end;

{ TShortCutPropertyEditor }

// MG: this is the Delphi way. Not very useful. This needs a Edit override
// and a nice dialog with grab, checkboxes...
// XXX
const
  ShortCuts: array[0..108] of TShortCut = (
    scNone,
    Byte('A') or scCtrl,
    Byte('B') or scCtrl,
    Byte('C') or scCtrl,
    Byte('D') or scCtrl,
    Byte('E') or scCtrl,
    Byte('F') or scCtrl,
    Byte('G') or scCtrl,
    Byte('H') or scCtrl,
    Byte('I') or scCtrl,
    Byte('J') or scCtrl,
    Byte('K') or scCtrl,
    Byte('L') or scCtrl,
    Byte('M') or scCtrl,
    Byte('N') or scCtrl,
    Byte('O') or scCtrl,
    Byte('P') or scCtrl,
    Byte('Q') or scCtrl,
    Byte('R') or scCtrl,
    Byte('S') or scCtrl,
    Byte('T') or scCtrl,
    Byte('U') or scCtrl,
    Byte('V') or scCtrl,
    Byte('W') or scCtrl,
    Byte('X') or scCtrl,
    Byte('Y') or scCtrl,
    Byte('Z') or scCtrl,
    Byte('A') or scCtrl or scAlt,
    Byte('B') or scCtrl or scAlt,
    Byte('C') or scCtrl or scAlt,
    Byte('D') or scCtrl or scAlt,
    Byte('E') or scCtrl or scAlt,
    Byte('F') or scCtrl or scAlt,
    Byte('G') or scCtrl or scAlt,
    Byte('H') or scCtrl or scAlt,
    Byte('I') or scCtrl or scAlt,
    Byte('J') or scCtrl or scAlt,
    Byte('K') or scCtrl or scAlt,
    Byte('L') or scCtrl or scAlt,
    Byte('M') or scCtrl or scAlt,
    Byte('N') or scCtrl or scAlt,
    Byte('O') or scCtrl or scAlt,
    Byte('P') or scCtrl or scAlt,
    Byte('Q') or scCtrl or scAlt,
    Byte('R') or scCtrl or scAlt,
    Byte('S') or scCtrl or scAlt,
    Byte('T') or scCtrl or scAlt,
    Byte('U') or scCtrl or scAlt,
    Byte('V') or scCtrl or scAlt,
    Byte('W') or scCtrl or scAlt,
    Byte('X') or scCtrl or scAlt,
    Byte('Y') or scCtrl or scAlt,
    Byte('Z') or scCtrl or scAlt,
    VK_F1,
    VK_F2,
    VK_F3,
    VK_F4,
    VK_F5,
    VK_F6,
    VK_F7,
    VK_F8,
    VK_F9,
    VK_F10,
    VK_F11,
    VK_F12,
    VK_F1 or scCtrl,
    VK_F2 or scCtrl,
    VK_F3 or scCtrl,
    VK_F4 or scCtrl,
    VK_F5 or scCtrl,
    VK_F6 or scCtrl,
    VK_F7 or scCtrl,
    VK_F8 or scCtrl,
    VK_F9 or scCtrl,
    VK_F10 or scCtrl,
    VK_F11 or scCtrl,
    VK_F12 or scCtrl,
    VK_F1 or scShift,
    VK_F2 or scShift,
    VK_F3 or scShift,
    VK_F4 or scShift,
    VK_F5 or scShift,
    VK_F6 or scShift,
    VK_F7 or scShift,
    VK_F8 or scShift,
    VK_F9 or scShift,
    VK_F10 or scShift,
    VK_F11 or scShift,
    VK_F12 or scShift,
    VK_F1 or scShift or scCtrl,
    VK_F2 or scShift or scCtrl,
    VK_F3 or scShift or scCtrl,
    VK_F4 or scShift or scCtrl,
    VK_F5 or scShift or scCtrl,
    VK_F6 or scShift or scCtrl,
    VK_F7 or scShift or scCtrl,
    VK_F8 or scShift or scCtrl,
    VK_F9 or scShift or scCtrl,
    VK_F10 or scShift or scCtrl,
    VK_F11 or scShift or scCtrl,
    VK_F12 or scShift or scCtrl,
    VK_INSERT,
    VK_INSERT or scShift,
    VK_INSERT or scCtrl,
    VK_DELETE,
    VK_DELETE or scShift,
    VK_DELETE or scCtrl,
    VK_BACK or scAlt,
    VK_BACK or scShift or scAlt);
    
function TShortCutPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TShortCutPropertyEditor.GetValue: string;
var
  CurValue: TShortCut;
begin
  CurValue := GetOrdValue;
  if CurValue = scNone then
    Result := '(None)'//srNone
  else
    Result := ShortCutToText(CurValue);
end;

procedure TShortCutPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc('(none)'{srNone});
  for I := 1 to High(ShortCuts) do Proc(ShortCutToText(ShortCuts[I]));
end;

procedure TShortCutPropertyEditor.SetValue(const Value: string);
var
  NewValue: TShortCut;
begin
  NewValue := 0;
  if (Value <> '') and (AnsiCompareText(Value, '(none)'{srNone}) <> 0) then
  begin
    NewValue := TextToShortCut(Value);
    if NewValue = 0 then
      raise EPropertyError.Create('Invalid Property Value'{@SInvalidPropertyValue});
  end;
  SetOrdValue(NewValue);
end;

{ TTabOrderPropertyEditor }

function TTabOrderPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [];
end;

{ TCaptionPropertyEditor }

function TCaptionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paAutoUpdate, paRevertable];
end;

{ TStringsPropertyEditor }

type
  TStringsPropEditorDlg = class(TForm)
  private
    procedure MemoChanged(Sender: TObject);
  public
    Memo : TMemo;
    OKButton, CancelButton : TBitBtn;
    Bevel : TBevel;
    StatusLabel : TLabel;
    constructor Create(AOwner : TComponent); override;
  end;

constructor TStringsPropEditorDlg.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Position := poScreenCenter;
  Height := 250;
  Width := 350;
  Caption := 'Strings Editor Dialog';

  Bevel:= TBevel.Create(Self);
  with Bevel do begin
    Parent:= Self;
    SetBounds(4, 4, 342, 213);
    Anchors:= [akLeft, akTop, akRight, akBottom];
    Shape:= bsFrame;
    Visible:= true;
  end;

  Memo := TMemo.Create(self);
  with Memo do begin
    Parent:= Self;
    SetBounds(12, 32, 326, 176);
    Anchors:= [akLeft, akTop, akRight, akBottom];
//    Scrollbars:= ssVertical;   // GTK 1.x does not implement horizontal scrollbars for GtkText
    Visible:= true;
    Memo.OnChange:= @MemoChanged;
  end;

  StatusLabel:= TLabel.Create(Self);
  with StatusLabel do begin
    Parent:= Self;
    SetBounds(12, 12, 326, 17);
    Caption:= '0 lines, 0 chars';
    Visible := true;
  end;

  OKButton := TBitBtn.Create(Self);
  with OKButton do Begin
    Parent := Self;
    Kind:= bkOK;
    Left := 192;
    Top := 221;
    Anchors:= [akRight, akBottom];
    Visible := true;
  end;

  CancelButton := TBitBtn.Create(self);
  with CancelButton do Begin
    Parent := self;
    Kind:= bkCancel;
    Left := 271;
    Top := 221;
    Anchors:= [akRight, akBottom];
    Visible := true;
  end;
end;

procedure TStringsPropEditorDlg.MemoChanged(Sender : TObject);
begin
  StatusLabel.Text:= Format('%d lines, %d chars', [Memo.Lines.Count,
    (Length(Memo.Lines.Text) - Memo.Lines.Count * Length(LineEnding))]);
end;

procedure TStringsPropertyEditor.Edit;
var
  TheDialog : TStringsPropEditorDlg;
  Strings : TStrings;
begin
  Strings:= TStrings(GetOrdValue);
  TheDialog:= TStringsPropEditorDlg.Create(Application);
  try
    TheDialog.Memo.Text:= Strings.Text;
    if (TheDialog.ShowModal = mrOK) then
      Strings.Text:=TheDialog.Memo.Text;
  finally
    TheDialog.Free;
  end;
end;

function TStringsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paRevertable, paReadOnly];
end;

{ TCaptionMultilinePropertyEditor }

procedure TCaptionMultilinePropertyEditor.Edit;
var
  TheDialog : TStringsPropEditorDlg;
  AString : string;
begin
  AString:= GetStrValue;
  TheDialog:= TStringsPropEditorDlg.Create(Application);
  try
    TheDialog.Memo.Text:= AString;
    if (TheDialog.ShowModal = mrOK) then
      SetStrValue(TheDialog.Memo.Text);
  finally
    TheDialog.Free;
  end;
end;

function TCaptionMultilinePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paRevertable, paAutoUpdate];
end;

{ TListColumnsPropertyEditor }

procedure TListColumnsPropertyEditor.Edit;
var
  ListColumns : TListColumns;
  ColumnDlg: TColumnDlg;
begin
  ColumnDlg:=TColumnDlg.Create(Application);
  try
    ListColumns := TListColumns(GetOrdValue);
    ColumnDlg.Columns.Assign(ListColumns);
       
    if ColumnDlg.ShowModal = mrOK 
    then ListColumns.Assign(ColumnDlg.Columns);
  finally
    ColumnDlg.Free;
  end;
end;

function TListColumnsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paReadOnly];
end;

//==============================================================================

{ TCursorPropertyEditor }

function TCursorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSortList, paValueList, paRevertable];
end;

function TCursorPropertyEditor.GetValue: ansistring;
begin
  Result := CursorToString(TCursor(GetOrdValue));
end;

procedure TCursorPropertyEditor.GetValues(Proc: TGetStringProc);
begin
  GetCursorValues(Proc);
end;

procedure TCursorPropertyEditor.SetValue(const NewValue: ansistring);
var
  CValue: Longint;
begin
  if IdentToCursor(NewValue, CValue) then
    SetOrdValue(CValue)
  else
    inherited SetValue(NewValue);
end;

//==============================================================================


{ TComponentSelectionList }

function TComponentSelectionList.Add(c: TComponent): integer;
begin
  Result:=FComponents.Add(c);
end;

procedure TComponentSelectionList.Clear;
begin
  FComponents.Clear;
end;

constructor TComponentSelectionList.Create;
begin
  inherited Create;
  FComponents:=TList.Create;
end;

destructor TComponentSelectionList.Destroy;
begin
  FComponents.Free;
  inherited Destroy;
end;

function TComponentSelectionList.GetCount: integer;
begin
  Result:=FComponents.Count;
end;

function TComponentSelectionList.GetItems(Index: integer): TComponent;
begin
  Result:=TComponent(FComponents[Index]);
end;

procedure TComponentSelectionList.SetItems(Index: integer;
  const CompValue: TComponent);
begin
  FComponents[Index]:=CompValue;
end;

function TComponentSelectionList.GetCapacity:integer;
begin
  Result:=FComponents.Capacity;
end;

procedure TComponentSelectionList.SetCapacity(const NewCapacity:integer);
begin
  FComponents.Capacity:=NewCapacity;
end;

procedure TComponentSelectionList.Assign(
  SourceSelectionList:TComponentSelectionList);
var a:integer;
begin
  if SourceSelectionList=Self then exit;
  Clear;
  if (SourceSelectionList<>nil) and (SourceSelectionList.Count>0) then begin
    FComponents.Capacity:=SourceSelectionList.Count;
    for a:=0 to SourceSelectionList.Count-1 do
      Add(SourceSelectionList[a]);
  end;
end;

function TComponentSelectionList.IsEqual(
 SourceSelectionList:TComponentSelectionList):boolean;
var a:integer;
begin
  Result:=false;
  if FComponents.Count<>SourceSelectionList.Count then exit;
  for a:=0 to FComponents.Count-1 do
    if Items[a]<>SourceSelectionList[a] then exit;
  Result:=true;
end;


//==============================================================================


{ TPropertyEditorHook }

function TPropertyEditorHook.CreateMethod(const Name:Shortstring;
  ATypeInfo:PTypeInfo): TMethod;
begin
  if IsValidIdent(Name) and (ATypeInfo<>nil) and Assigned(FOnCreateMethod) then
    Result:=FOnCreateMethod(Name,ATypeInfo)
  else begin
    Result.Code:=nil;
    Result.Data:=nil;
  end;
end;

function TPropertyEditorHook.GetMethodName(const Method:TMethod): ShortString;
begin
  if Assigned(FOnGetMethodName) then
    Result:=FOnGetMethodName(Method)
  else begin
    // search the method name with the given code pointer
    if Assigned(Method.Code) then begin
      if Assigned(LookupRoot) then begin
        Result:=LookupRoot.MethodName(Method.Code);
        if Result='' then
          Result:='<Unpublished>';
      end else
        Result:='<No LookupRoot>';
    end else
      Result:='';
  end;
end;

procedure TPropertyEditorHook.GetMethods(TypeData:PTypeData;
  Proc:TGetStringProc);
begin
  if Assigned(FOnGetMethods) then
    FOnGetMethods(TypeData,Proc);
end;

function TPropertyEditorHook.MethodExists(const Name:Shortstring;
  TypeData: PTypeData;
  var MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean):boolean;
begin
  // check if a published method with given name exists in LookupRoot
  if IsValidIdent(Name) and Assigned(FOnMethodExists) then
    Result:=FOnMethodExists(Name,TypeData,
                            MethodIsCompatible,MethodIsPublished,IdentIsMethod)
  else begin
    Result:=IsValidIdent(Name) and Assigned(LookupRoot)
                               and (LookupRoot.MethodAddress(Name)<>nil);
    MethodIsCompatible:=Result;
    MethodIsPublished:=Result;
    IdentIsMethod:=Result;
  end;
end;

procedure TPropertyEditorHook.RenameMethod(const CurName, NewName:ShortString);
begin
  // rename published method in LookupRoot object and source
  if Assigned(FOnRenameMethod) then
    FOnRenameMethod(CurName,NewName);
end;

procedure TPropertyEditorHook.ShowMethod(const Name:Shortstring);
begin
  // jump cursor to published method body
  if Assigned(FOnShowMethod) then
    FOnShowMethod(Name);
end;

function TPropertyEditorHook.MethodFromAncestor(const Method:TMethod):boolean;
var AncestorClass: TClass;
begin
  // check if given Method is not in LookupRoot source,
  // but in one of its ancestors
  if Assigned(FOnMethodFromAncestor) then
    Result:=FOnMethodFromAncestor(Method)
  else begin
    if (Method.Data<>nil) then begin
      AncestorClass:=TObject(Method.Data).ClassParent;
      Result:=(AncestorClass<>nil)
              and (AncestorClass.MethodName(Method.Code)<>'');
    end else
      Result:=false;
  end;
end;

procedure TPropertyEditorHook.ChainCall(const AMethodName, InstanceName,
InstanceMethod:Shortstring;  TypeData:PTypeData);
begin
  if Assigned(FOnChainCall) then
    FOnChainCall(AMethodName,InstanceName,InstanceMethod,TypeData);
end;

function TPropertyEditorHook.GetComponent(const Name:Shortstring):TComponent;
begin
  if Assigned(FOnGetComponent) then
    Result:=FOnGetComponent(Name)
  else begin
    if Assigned(LookupRoot) then begin
      Result:=LookupRoot.FindComponent(Name);
    end else begin
      Result:=nil;
    end;
  end;
end;

function TPropertyEditorHook.GetComponentName(
  AComponent:TComponent):Shortstring;
begin
  if Assigned(FOnGetComponentName) then
    Result:=FOnGetComponentName(AComponent)
  else begin
   if Assigned(AComponent) then
     Result:=AComponent.Name
   else
     Result:='';
  end;
end;

procedure TPropertyEditorHook.GetComponentNames(TypeData:PTypeData;
  Proc:TGetStringProc);
var i: integer;
begin
  if Assigned(FOnGetComponentNames) then
    FOnGetComponentNames(TypeData,Proc)
  else begin
    if Assigned(LookupRoot) then
      for i:=0 to LookupRoot.ComponentCount-1 do
        if (LookupRoot.Components[i] is TypeData^.ClassType) then
          Proc(LookupRoot.Components[i].Name);
  end;
end;

function TPropertyEditorHook.GetRootClassName:Shortstring;
begin
  if Assigned(FOnGetRootClassName) then begin
    Result:=FOnGetRootClassName();
  end else begin
    if Assigned(LookupRoot) then
      Result:=LookupRoot.ClassName
    else
      Result:='';
  end;
end;

procedure TPropertyEditorHook.ComponentRenamed(AComponent: TComponent);
begin
  if Assigned(OnComponentRenamed) then
    OnComponentRenamed(AComponent);
end;

procedure TPropertyEditorHook.ComponentAdded(AComponent: TComponent;
  Select: boolean);
begin
  if Assigned(OnComponentAdded) then
    OnComponentAdded(AComponent,Select);
end;

procedure TPropertyEditorHook.DeleteComponent(AComponent: TComponent);
begin
  if Assigned(OnDeleteComponent) then
    OnDeleteComponent(AComponent)
  else
    AComponent.Free;
end;

function TPropertyEditorHook.GetObject(const Name:Shortstring):TPersistent;
begin
  if Assigned(FOnGetObject) then
    Result:=FOnGetObject(Name)
  else
    Result:=nil;
end;

function TPropertyEditorHook.GetObjectName(Instance:TPersistent):Shortstring;
begin
  if Assigned(FOnGetObjectName) then
    Result:=FOnGetObjectName(Instance)
  else begin
    if Instance is TComponent then
      Result:=TComponent(Instance).Name;
  end;
end;

procedure TPropertyEditorHook.GetObjectNames(TypeData:PTypeData;
  Proc:TGetStringProc);
begin
  if Assigned(FOnGetObjectNames) then
    FOnGetObjectNames(TypeData,Proc);
end;

procedure TPropertyEditorHook.Modified;
begin
  if Assigned(FOnModified) then
    FOnModified();
end;

procedure TPropertyEditorHook.Revert(Instance:TPersistent;
PropInfo:PPropInfo);
begin
  if Assigned(FOnRevert) then
    FOnRevert(Instance,PropInfo);
end;

procedure TPropertyEditorHook.RefreshPropertyValues;
begin
  if Assigned(FOnRefreshPropertyValues) then
    FOnRefreshPropertyValues();
end;

procedure TPropertyEditorHook.SetLookupRoot(AComponent:TComponent);
begin
  if FLookupRoot=AComponent then exit;
  FLookupRoot:=AComponent;
  if Assigned(FOnChangeLookupRoot) then
    FOnChangeLookupRoot();
end;

//******************************************************************************
// XXX
// workaround for missing typeinfo function
constructor TDummyClassForPropTypes.Create;
var TypeInfo : PTypeInfo;
begin
  inherited Create;
  TypeInfo:=ClassInfo;
  FCount:=GetTypeData(TypeInfo)^.Propcount;
  GetMem(FList,FCount * SizeOf(Pointer));
  GetPropInfos(TypeInfo,FList);
end;

destructor TDummyClassForPropTypes.Destroy;
begin
  FreeMem(FList);
  inherited Destroy;
end;

function TDummyClassForPropTypes.PTypeInfos(
  const PropName:shortstring):PTypeInfo;
var Index:integer;
begin
  Index:=FCount-1;
  while (Index>=0) do begin
    Result:=FList^[Index]^.PropType;
    if (AnsiCompareText(Result^.Name,PropName)=0) then exit;
    dec(Index);
  end;
  Result:=nil;
end;

var
  DummyClassForPropTypes: TDummyClassForPropTypes;

//******************************************************************************

Function ClassTypeInfo(Value : TClass) : PTypeInfo;
begin
  Result := PTypeInfo(Value.ClassInfo);
end;

procedure InitPropEdits;
begin
  PropertyClassList:=TList.Create;
  PropertyEditorMapperList:=TList.Create;
  // register the standard property editors

  // XXX workaround for missing typeinfo function
  // Normaly it should use be something like this;
  // RegisterPropertyEditor(TypeInfo(TColor),nil,'',TColorPropertyEditor);
  DummyClassForPropTypes:=TDummyClassForPropTypes.Create;
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('AnsiString'),
    nil,'Name',TComponentNamePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('AnsiString'),
    TCustomLabel, 'Caption', TCaptionMultilinePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('longint'),
    nil,'Tag',TTabOrderPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('shortstring'),
    nil,'',TCaptionPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TStrings'),
    nil,'',TStringsPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TModalResult'),
    nil,'ModalResult',TModalResultPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TShortCut'),
    nil,'',TShortCutPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TDate'),
    nil,'',TShortCutPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TTime'),
    nil,'',TShortCutPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TDateTime'),
    nil,'',TShortCutPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TCursor'),
    nil,'',TCursorPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TComponent),nil
    ,'',TComponentPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TListColumns'),
    nil,'',TListColumnsPropertyEditor);

  RegisterPropertyEditor(ClassTypeInfo(TCollection),
    nil,'',TCollectionPropertyEditor);
end;

procedure FinalPropEdits;
var i: integer;
  pm: PPropertyEditorMapperRec;
  pc: PPropertyClassRec;
begin
  for i:=0 to PropertyEditorMapperList.Count-1 do begin
    pm:=PPropertyEditorMapperRec(PropertyEditorMapperList.Items[i]);
    Dispose(pm);
  end;
  PropertyEditorMapperList.Free;
  PropertyEditorMapperList:=nil;

  for i:=0 to PropertyClassList.Count-1 do begin
    pc:=PPropertyClassRec(PropertyClassList[i]);
    Dispose(pc);
  end;
  PropertyClassList.Free;
  PropertyClassList:=nil;
  
  FreeAndNil(ListPropertyEditors);

  // XXX workaround for missing typeinfo function
  DummyClassForPropTypes.Free;
end;


initialization
  InitPropEdits;

finalization
  FinalPropEdits;

end.

