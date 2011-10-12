{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
    -TIntegerSet missing -> taking my own

    -many more... see XXX
}
unit PropEdits;

{$mode objfpc}{$H+}

// This unit contains a lot of base type conversions. Disable range checking.
{$R-}

interface

uses
  Classes, TypInfo, SysUtils,
  FPCAdds, // for StrToQWord in older fpc versions
  LCLProc, Forms, Controls, GraphType, StringHashList, ButtonPanel,
  Graphics, StdCtrls, Buttons, Menus, LCLType, ExtCtrls, LCLIntf,
  Dialogs, Grids, EditBtn, PropertyStorage, TextTools, FrmSelectProps,
  StringsPropEditDlg, ColumnDlg, FileUtil, FileCtrl, ObjInspStrConsts,
  CollectionPropEditForm, PropEditUtils, ComCtrls;

const
  MaxIdentLength: Byte = 63;

type

  TPersistentSelectionList = PropEditUtils.TPersistentSelectionList;
  // For backwards compatibility only. Use TGetStrProc directly.
  TGetStringProc = Classes.TGetStrProc;

{ TPropertyEditor
  Edits a property of a component, or list of components, selected into the
  Object Inspector. The property editor is created based on the type of the
  property being edited as determined by the types registered by
  RegisterPropertyEditor. The Object Inspector uses a TPropertyEditor
  for all modification to a property. GetName and GetValue are called to
  display the name and value of the property. SetValue is called whenever the
  user requests to change the value. Edit is called when the user
  double-clicks the property in the Object Inspector. GetValues is called when
  the drop-down list of a property is displayed. GetProperties is called when
  the property is expanded to show sub-properties. AllEqual is called to decide
  whether or not to display the value of the property when more than one
  component is selected.

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
      Called whenever there is more than one component selected. If this
      method returns true,GetValue is called,otherwise blank is displayed
      in the Object Inspector. This is called only when GetAttributes
      returns paMultiSelect.
    AutoFill
      Called to determine whether the values returned by GetValues can be
      selected incrementally in the Object Inspector. This is called only when
      GetAttributes returns paValueList.
    Edit
      Called when the '...' button is pressed or the property is double-clicked.
      This can,for example,bring up a dialog to allow the editing the
      component in some more meaningful fashion than by text (e.g. the Font
      property).
    GetAttributes
      Returns the information for use in the Object Inspector to be able to
      show the appropriate tools. GetAttributes returns a set of type
      TPropertyAttributes:
        paValueList:    The property editor can return an enumerated list of
                        values for the property. If GetValues calls Proc
                        with values then this attribute should be set. This
                        will cause the drop-down button to appear to the right
                        of the property in the Object Inspector.
        paSortList:     Object Inspector to sort the list returned by
                        GetValues.
        paPickList:     Usable together with paValueList. The text field is
                        readonly. The user can still select values from drop
                        list. Unless paReadOnly.
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
                        dialog. This will cause the '...' button to be
                        displayed to the right of the property in the Object
                        Inspector.
        paMultiSelect:  Allows the property to be displayed when more than
                        one component is selected. Some properties are not
                        appropriate for multi-selection (e.g. the Name
                        property).
        paAutoUpdate:   Causes the SetValue method to be called on each
                        change made to the editor instead of after the change
                        has been approved (e.g. the Caption property).
        paReadOnly:     Value is not allowed to change. But if paDialog is set
                        a Dialog can change the value. This disables only the
                        edit and combobox in the object inspector.
        paRevertable:   Allows the property to be reverted to the original
                        value. Things that shouldn't be reverted are nested
                        properties (e.g. Fonts) and elements of a composite
                        property such as set element values.
        paFullWidthName:Tells the object inspector that the value does not
                        need to be rendered and as such the name should be
                        rendered the full width of the inspector.
        paVolatileSubProperties: Any change of property value causes any shown
                        subproperties to be recollected.
        paDisableSubProperties: All subproperties are readonly
                        (not even via Dialog).
        paReference:    property contains a reference to something else. When
                        used in conjunction with paSubProperties the referenced
                        object should be displayed as sub properties to this
                        property.
        paNotNestable:  Indicates that the property is not safe to show when
                        showing the properties of an expanded reference.

    GetComponent
      Returns the Index'th component being edited by this property editor. This
      is used to retrieve the components. A property editor can only refer to
      multiple components when paMultiSelect is returned from GetAttributes.
    GetEditLimit
      Returns the number of character the user is allowed to enter for the
      value. The inplace editor of the object inspector will be have its
      text limited set to the return value. By default this limit is 255.
    GetName
      Returns the name of the property. By default the value is retrieved
      from the type information with all underbars replaced by spaces. This
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
      '(unknown)'. This should be overridden to return the appropriate value.
    GetValues
      Called when paValueList is returned in GetAttributes. Should call Proc
      for every value that is acceptable for this property. TEnumPropertyEditor
      will pass every element in the enumeration.
    Initialize
      Called after the property editor has been created but before it is used.
      Many times property editors are created and because they are not a common
      property across the entire selection they are thrown away. Initialize is
      called after it is determined the property editor is going to be used by
      the object inspector and not just thrown away.
    SetValue(Value)
      Called to set the value of the property. The property editor should be
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
      down list's render. This is very similar to TListBox's OnMeasureItem,
      just slightly different parameters.
    ListDrawValue(Value,Canvas,Rect,Selected)
      This is called during the item/value render phase of the drop down list's
      render. This is very similar to TListBox's OnDrawItem, just slightly
      different parameters.
    PropMeasureHeight(Value,Canvas,AHeight)
      This is called during the item/property height calculation phase of the
      object inspectors rows render. This is very similar to TListBox's
      OnMeasureItem, just slightly different parameters.
    PropDrawName(Canvas,Rect,Selected)
      Called during the render of the name column of the property list. Its
      functionality is very similar to TListBox's OnDrawItem,but once again
      it has slightly different parameters.
    PropDrawValue(Canvas,Rect,Selected)
      Called during the render of the value column of the property list. Its
      functionality is similar to PropDrawName. If multiple items are selected
      and their values don't match this procedure will be passed an empty
      value.

  Properties and methods useful in creating new TPropertyEditor classes:

    Name property
      Returns the name of the property returned by GetName
    PrivateEditory property
      This is the configuration directory of lazarus.
      If the property editor needs auxiliary or state files (templates,
      examples, etc) they should be stored in this editory.
    Value property
      The current value,as a string,of the property as returned by GetValue.
    Modified
      Called to indicate the value of the property has been modified. Called
      automatically by the SetXxxValue methods. If you call a TProperty
      SetXxxValue method directly,you *must* call Modified as well.
    GetXxxValue
      Gets the value of the first property in the Properties property. Calls
      the appropriate TProperty GetXxxValue method to retrieve the value.
    SetXxxValue
      Sets the value of all the properties in the Properties property. Calls
      the approprate TProperty SetXxxxValue methods to set the value.
    GetVisualValue
      This function will return the displayable value of the property. If
      only one item is selected or all the multi-selected items have the same
      property value then this function will return the actual property value.
      Otherwise this function will return an empty string.}

  TPropertyAttribute=(
    paValueList,
    paPickList,
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
    paDisableSubProperties,
    paReference,
    paNotNestable,
    paHasDefaultValue,
    paCustomDrawn
    );
  TPropertyAttributes=set of TPropertyAttribute;

  TPropertyEditor=class;

  TInstProp=record
    Instance:TPersistent;
    PropInfo:PPropInfo;
  end;
  PInstProp = ^TInstProp;

  TInstPropList = array[0..999999] of TInstProp;
  PInstPropList = ^TInstPropList;

  TGetPropEditProc = procedure(Prop: TPropertyEditor) of object;

  TPropEditDrawStateType = (pedsSelected, pedsFocused, pedsInEdit,
       pedsInComboList, pedsPainted);
  TPropEditDrawState = set of TPropEditDrawStateType;
  
  TPropEditHint = (
    pehNone,
    pehTree,
    pehName,
    pehValue,
    pehEditButton
    );

  TPropertyEditorHook = class;

  { TPropertyEditor }

  TPropertyEditor=class
  private
    FOnSubPropertiesChanged: TNotifyEvent;
    FPropertyHook: TPropertyEditorHook;
    FPropCount: Integer;
    FPropList: PInstPropList;
    function GetPrivateDirectory: ansistring;
  public
    constructor Create(Hook:TPropertyEditorHook; APropCount:Integer); virtual;
    destructor Destroy; override;
    procedure Activate; virtual;
    procedure Deactivate; virtual;
    function AllEqual: Boolean; virtual;
    function AutoFill: Boolean; virtual;
    procedure Edit; virtual; // called when clicking on OI property button or double clicking on value
    procedure ShowValue; virtual; // called when Ctrl-Click on value
    function GetAttributes: TPropertyAttributes; virtual;
    function IsReadOnly: boolean; virtual;
    function GetComponent(Index: Integer): TPersistent;// for Delphi compatibility it is called GetComponent instead of GetPersistent
    function GetUnitName(Index: Integer = 0): string;
    function GetPropTypeUnitName(Index: Integer = 0): string;
    function GetPropertyPath(Index: integer = 0): string;// e.g. 'TForm1.Color'
    function GetEditLimit: Integer; virtual;
    function GetName: shortstring; virtual;
    procedure GetProperties(Proc: TGetPropEditProc); virtual;
    function GetPropType: PTypeInfo;
    function GetPropInfo: PPropInfo;
    function GetInstProp: PInstProp;
    function GetFloatValue: Extended;
    function GetFloatValueAt(Index: Integer): Extended;
    function GetInt64Value: Int64;
    function GetInt64ValueAt(Index: Integer): Int64;
    function GetMethodValue: TMethod;
    function GetMethodValueAt(Index: Integer): TMethod;
    function GetOrdValue: Longint;
    function GetOrdValueAt(Index: Integer): Longint;
    function GetObjectValue: TObject;
    function GetObjectValue(MinClass: TClass): TObject;
    function GetObjectValueAt(Index: Integer): TObject;
    function GetObjectValueAt(Index: Integer; MinClass: TClass): TObject;
    function GetDefaultOrdValue: Longint;
    function GetStrValue: AnsiString;
    function GetStrValueAt(Index: Integer): AnsiString;
    function GetVarValue: Variant;
    function GetVarValueAt(Index: Integer):Variant;
    function GetWideStrValue: WideString;
    function GetWideStrValueAt(Index: Integer): WideString;
    function GetValue: ansistring; virtual;
    function GetHint(HintType: TPropEditHint; x, y: integer): string; virtual;
    function GetDefaultValue: ansistring; virtual;
    function GetVisualValue: ansistring;
    procedure GetValues(Proc: TGetStrProc); virtual;
    procedure Initialize; virtual;
    procedure Revert; virtual;
    procedure SetValue(const NewValue: ansistring); virtual;
    procedure SetPropEntry(Index: Integer; AnInstance: TPersistent;
                           APropInfo: PPropInfo);
    procedure SetFloatValue(const NewValue: Extended);
    procedure SetMethodValue(const NewValue: TMethod);
    procedure SetInt64Value(const NewValue: Int64);
    procedure SetOrdValue(const NewValue: Longint);
    procedure SetPtrValue(const NewValue: Pointer);
    procedure SetStrValue(const NewValue: AnsiString);
    procedure SetVarValue(const NewValue: Variant);
    procedure SetWideStrValue(const NewValue: WideString);
    procedure Modified;
    function ValueAvailable: Boolean;
    procedure ListMeasureWidth(const AValue: ansistring; Index:integer;
                               ACanvas:TCanvas; var AWidth: Integer); virtual;
    procedure ListMeasureHeight(const AValue: ansistring; Index:integer;
                                ACanvas:TCanvas; var AHeight: Integer); virtual;
    procedure ListDrawValue(const AValue: ansistring; Index:integer;
                            ACanvas:TCanvas; const ARect: TRect;
                            AState: TPropEditDrawState); virtual;
    procedure PropMeasureHeight(const NewValue: ansistring;  ACanvas: TCanvas;
                                var AHeight:Integer); virtual;
    procedure PropDrawName(ACanvas: TCanvas; const ARect:TRect;
                           AState: TPropEditDrawState); virtual;
    procedure PropDrawValue(ACanvas:TCanvas; const ARect:TRect;
                            AState:TPropEditDrawState); virtual;
    procedure UpdateSubProperties; virtual;
    function SubPropertiesNeedsUpdate: boolean; virtual;
    function IsDefaultValue: boolean; virtual;
    function IsNotDefaultValue: boolean; virtual;
    property PropertyHook: TPropertyEditorHook read FPropertyHook;
    property PrivateDirectory: ansistring read GetPrivateDirectory;
    property PropCount:Integer read FPropCount;
    property FirstValue: ansistring read GetValue write SetValue;
    property OnSubPropertiesChanged: TNotifyEvent
                     read FOnSubPropertiesChanged write FOnSubPropertiesChanged;
  end;

  TPropertyEditorClass=class of TPropertyEditor;
  
{ THiddenPropertyEditor
  A property editor, to hide a published property. If you can't unpublish it,
  hide it. }
  
  THiddenPropertyEditor = class(TPropertyEditor)
  end;

{ TOrdinalPropertyEditor
  The base class of all ordinal property editors. It establishes that ordinal
  properties are all equal if the GetOrdValue all return the same value and
  provide methods to retrieve the default value. }

  TOrdinalPropertyEditor = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    function GetDefaultValue: ansistring; override;
    function OrdValueToVisualValue(OrdValue: longint): string; virtual;
  end;

{ TIntegerPropertyEditor
  Default editor for all Longint properties and all subtypes of the Longint
  type (i.e. Integer, Word, 1..10, etc.). Restricts the value entered into
  the property to the range of the sub-type. }

  TIntegerPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure SetValue(const NewValue: ansistring);  override;
  end;

{ TCharPropertyEditor
  Default editor for all Char properties and sub-types of Char (i.e. Char,
  'A'..'Z', etc.). }

  TCharPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TEnumPropertyEditor
  The default property editor for all enumerated properties (e.g. TShape =
  (sCircle, sTriangle, sSquare), etc.). }

  TEnumPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TBoolPropertyEditor
  Default property editor for all boolean properties }

  { TBoolPropertyEditor }

  TBoolPropertyEditor = class(TEnumPropertyEditor)
  public
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TInt64PropertyEditor
  Default editor for all Int64 properties and all subtypes of Int64. }

  TInt64PropertyEditor = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TQWordPropertyEditor
  Default editor for all QWord properties }

  TQWordPropertyEditor = class(TInt64PropertyEditor)
  public
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

{ TWideStringPropertyEditor
  The default property editor for widestrings}

  TWideStringPropertyEditor = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TNestedPropertyEditor
  A property editor that uses the PropertyHook, PropList and PropCount.
  The constructor and destructor do not call inherited, but all derived classes
  should. This is useful for properties like the TSetElementPropertyEditor. }

  TNestedPropertyEditor = class(TPropertyEditor)
  private
    FParentEditor: TPropertyEditor;
  public
    constructor Create(Parent: TPropertyEditor); overload;
    destructor Destroy; override;
    property ParentEditor: TPropertyEditor read FParentEditor;
  end;

{ TSetElementPropertyEditor
  A property editor that edits an individual set element. GetName is
  changed to display the set element name instead of the property name and
  Get/SetValue is changed to reflect the individual element state. This
  editor is created by the TSetPropertyEditor editor. }

  TSetElementPropertyEditor = class(TNestedPropertyEditor)
  private
    FElement: Integer;
  public
    constructor Create(Parent: TPropertyEditor; AElement: Integer); overload;
    function AllEqual: Boolean; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetName: shortstring; override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
    function IsNotDefaultValue: boolean; override;
   end;

{ TSetPropertyEditor
  Default property editor for all set properties. This editor does not edit
  the set directly but will display sub-properties for each element of the
  set. GetValue displays the value of the set in standard set syntax. }

  TSetPropertyEditor = class(TOrdinalPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function OrdValueToVisualValue(OrdValue: longint): string; override;
  end;

{ TClassPropertyEditor
  Default property editor for all objects. Does not allow modifying the
  property but does display the class name of the object and will allow the
  editing of the object's properties as sub-properties of the property. }

  TClassPropertyEditor = class(TPropertyEditor)
  private
    FSubPropsTypeFilter: TTypeKinds;
    procedure SetSubPropsTypeFilter(const AValue: TTypeKinds);
    function EditorFilter(const AEditor: TPropertyEditor): Boolean;
  protected
    function GetSelections: TPersistentSelectionList; virtual;
  public
    constructor Create(
      Hook: TPropertyEditorHook; APropCount: Integer); override;

    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: ansistring; override;

    property SubPropsTypeFilter: TTypeKinds
      read FSubPropsTypeFilter write SetSubPropsTypeFilter default tkAny;
  end;

{ TMethodPropertyEditor
  Property editor for all method properties. }

  TMethodPropertyEditor = class(TPropertyEditor)
  public
    function AllEqual: Boolean; override;
    procedure Edit; override;
    procedure ShowValue; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
    function GetFormMethodName: shortstring; virtual;
    function GetTrimmedEventName: shortstring;
    class function GetDefaultMethodName(Root, Component: TComponent;
        const RootClassName, ComponentName, PropName: shortstring): shortstring;
  end;
  
{ TPersistentPropertyEditor
  A base editor for TPersistent. It does allow editing of the properties.
  It allows the user to set the value of this property to point to a component
  in the same form that is type compatible with the property being edited
  (e.g. the ActiveControl property). }

  TPersistentPropertyEditor = class(TClassPropertyEditor)
  protected
    function FilterFunc(const ATestEditor: TPropertyEditor): Boolean;
    function GetPersistentReference: TPersistent; virtual;
    function GetSelections: TPersistentSelectionList; override;
    function CheckNewValue(APersistent: TPersistent): boolean; virtual;
  public
    function AllEqual: Boolean; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: AnsiString; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TComponentPropertyEditor
  The default editor for TComponents. It does allow editing of the
  properties of the component. It allows the user to set the value of this
  property to point to a component in the same form that is type compatible
  with the property being edited (e.g. the ActiveControl property). }

  TComponentPropertyEditor = class(TPersistentPropertyEditor)
  protected
    function GetComponentReference: TComponent; virtual;
  public
    function AllEqual: Boolean; override;
  end;

{ TInterfacePropertyEditor
  The default editor for interface references. It allows the user to set
  the value of this property to refer to an interface implemented by
  a component on the form (or via form linking) that is type compatible
  with the property being edited. }

  TInterfacePropertyEditor = class(TComponentPropertyEditor)
  private
  protected
    procedure ReceiveComponentNames(const S: string);
    function GetComponent(const AInterface: Pointer {IInterface}): TComponent;
    function GetComponentReference: TComponent; override;
    function GetSelections: TPersistentSelectionList{IDesignerSelections}; override;
  public
    function AllEqual: Boolean; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TNoteBookActiveControlPropertyEditor }

  TNoteBookActiveControlPropertyEditor = class(TComponentPropertyEditor)
  protected
    function CheckNewValue(APersistent: TPersistent): boolean; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TComponentNamePropertyEditor
  Property editor for the Name property. It restricts the name property
  from being displayed when more than one component is selected. }

  TComponentNamePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TDatePropertyEditor
  Property editor for date portion of TDateTime type. }

  TDatePropertyEditor = class(TPropertyEditor)
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
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue:ansistring); override;
  end;

{ TShortCutPropertyEditor
  Property editor the ShortCut property. Allows both typing in a short
  cut value or picking a short-cut value from a list. }

  TShortCutPropertyEditor = class(TOrdinalPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TTabOrderPropertyEditor
  Property editor for the TabOrder property. Prevents the property from being
  displayed when more than one component is selected. }

  TTabOrderPropertyEditor = class(TIntegerPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;
  

{ TCaptionPropertyEditor
  Property editor for the Caption and Text properties. Updates the value of
  the property for each change instead on when the property is approved. }

  TCaptionPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;


{ TStringMultilinePropertyEditor
  PropertyEditor editor for a string property when the string can be
  multiline (e.g. TLabel.Caption, TControl.Hint).
  Brings up the dialog for entering text. }

  TStringMultilinePropertyEditor = class(TCaptionPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


{ TStringsPropertyEditor
  PropertyEditor editor for the TStrings properties.
  Brings up the dialog for entering text. }
  
  TStringsPropEditorDlg = class;

  TStringsPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function CreateDlg(s: TStrings): TStringsPropEditorDlg; virtual;
    function GetAttributes: TPropertyAttributes; override;
  end;


{ TCursorPropertyEditor
  PropertyEditor editor for the TCursor properties.
  Displays cursor as constant name if exists, otherwise an integer. }

  TCursorPropertyEditor = class(TIntegerPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;
  
  
{ TFileNamePropertyEditor
  PropertyEditor editor for filename properties.
  Show an TOpenDialog on Edit. }

  TFileNamePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetFilter: String; virtual;
    function GetDialogOptions: TOpenOptions; virtual;
    function GetDialogTitle: string; virtual;
    function GetInitialDirectory: string; virtual;
    procedure SetFilename(const Filename: string); virtual;
    function CreateFileDialog: TOpenDialog; virtual;
  end;


{ TDirectoryPropertyEditor
  PropertyEditor editor for directory properties.
  Show an TSelectDirectoryDialog on Edit. }

  TDirectoryPropertyEditor = class(TFileNamePropertyEditor)
  public
    function CreateFileDialog: TOpenDialog; override;
  end;


{ TURLPropertyEditor
  PropertyEditor editor for URL properties.
  Show an TOpenDialog on Edit. }

  TURLPropertyEditor = class(TFileNamePropertyEditor)
  public
    procedure SetFilename(const Filename: string); override;
  end;


{ TURLDirectoryPropertyEditor
  PropertyEditor editor for URL properties.
  Show an TOpenDialog on Edit. }

  TURLDirectoryPropertyEditor = class(TURLPropertyEditor)
  public
    function CreateFileDialog: TOpenDialog; override;
  end;
  

{ TFileDlgFilterProperty
  PropertyEditor editor for TFileDialog filter properties.
  Show a dialog on Edit. }

  TFileDlgFilterProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


{ TSessionPropertiesPropertyEditor
  PropertyEditor editor for TControl.SessionProperties properties.
  Show a dialog on Edit. }

  TSessionPropertiesPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
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
    constructor Create(Parent: TListPropertyEditor; AnIndex: integer); overload;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetName:shortstring; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
    property List: TListPropertyEditor read FList;
    property TheIndex: integer read FIndex;
  end;

{ TListPropertyEditor
  A property editor with dynamic sub properties representing a list of objects.
  The items are shown imbedded in the OI and if the user presses the Edit button
  as extra window to select items, which are then shown in the OI.
  UNDER CONSTRUCTION by Mattias
  The problem with all properties is, that we don't get notified, when something
  changes. In this case, the list can change, which means the property editors
  for the list elements must be deleted or created.
  }

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
    // MWE: hmm... don't like "public" objects
    // TODO: change this ?
    SavedList: TObject;
    SavedElements: TList;
    SavedPropertyEditors: TList;
    function ReadElementCount: integer; virtual;
    function ReadElement(Index: integer): TPersistent; virtual;
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
      Proc: TGetStrProc); virtual;
    procedure SetElementValue(Element: TListElementPropertyEditor;
      NewValue: ansistring); virtual;
  public
    constructor Create(Hook:TPropertyEditorHook; APropCount:Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetElementCount: integer;
    function GetElement(Index: integer): TPersistent;
    function GetElement(Element: TListElementPropertyEditor): TPersistent;
    function GetElementPropEditor(Index: integer): TListElementPropertyEditor;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: AnsiString; override;
    procedure Initialize; override;
    procedure SaveElements;
    function SubPropertiesNeedsUpdate: boolean; override;
  end;

{ TCollectionPropertyEditor
  Default property editor for all TCollections, imbedded in the OI
  UNDER CONSTRUCTION by Mattias}

  TCollectionPropertyEditor = class(TListPropertyEditor)
  private
  protected
    function ReadElementCount: integer; override;
    function ReadElement(Index: integer): TPersistent; override;
    function GetElementAttributes(
      Element: TListElementPropertyEditor): TPropertyAttributes; override;
    function GetElementName(
      Element: TListElementPropertyEditor):shortstring; override;
    procedure GetElementProperties(Element: TListElementPropertyEditor;
      Proc: TGetPropEditProc); override;
    function GetElementValue(
      Element: TListElementPropertyEditor): ansistring; override;
    procedure GetElementValues(Element: TListElementPropertyEditor;
      Proc: TGetStrProc); override;
    procedure SetElementValue(Element: TListElementPropertyEditor;
      NewValue: ansistring); override;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    class function ShowCollectionEditor(ACollection: TCollection; 
      OwnerPersistent: TPersistent; const PropName: String): TCustomForm; virtual;
  end;

//==============================================================================
// Delphi Compatible Property Editor Classnames

type
  TOrdinalProperty =        TOrdinalPropertyEditor;
  TIntegerProperty =        TIntegerPropertyEditor;
  TCharProperty =           TCharPropertyEditor;
  TEnumProperty =           TEnumPropertyEditor;
  TBoolProperty =           TBoolPropertyEditor;
  TInt64Property =          TInt64PropertyEditor;
  TFloatProperty =          TFloatPropertyEditor;
  TStringProperty =         TStringPropertyEditor;
  TNestedProperty =         TNestedPropertyEditor;
  TSetElementProperty =     TSetElementPropertyEditor;
  TSetProperty =            TSetPropertyEditor;
  TClassProperty =          TClassPropertyEditor;
  TMethodProperty =         TMethodPropertyEditor;
  TComponentProperty =      TPersistentPropertyEditor;
  TComponentNameProperty =  TComponentNamePropertyEditor;
//  TImeNameProperty =        TImeNamePropertyEditor;
  TCursorProperty =         TCursorPropertyEditor;
  TModalResultProperty =    TModalResultPropertyEditor;
  TShortCutProperty =       TShortCutPropertyEditor;
//  TMPFilenameProperty =     TMPFilenamePropertyEditor;
  TTabOrderProperty =       TTabOrderPropertyEditor;
  TCaptionProperty =        TCaptionPropertyEditor;
  TDateProperty =           TDatePropertyEditor;
  TTimeProperty =           TTimePropertyEditor;
  TDateTimeProperty =       TDateTimePropertyEditor;


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
  property). Enumerated types will display a drop-down list of all the
  enumerated values (e.g. TShapes = (sCircle,sSquare,sTriangle) will be edited
  by a drop-down list containing only sCircle,sSquare and sTriangle).
  A property editor needs only be created if default property editor or none of
  the existing property editors are sufficient to edit the property. This is
  typically because the property is an object.
  The registered types are looked up newest to oldest.
  This allows an existing property editor replaced by a custom property editor.

    PropertyEditorType
      The type information pointer returned by the TypeInfo built-in function
      (e.g. TypeInfo(TMyRange) or TypeInfo(TShapes)).

    PersistentClass
      Type of the persistent object to which to restrict this type editor. This
      parameter can be left nil which will mean this type editor applies to all
      properties of PropertyEditorType.

    PropertyEditorName
      The name of the property to which to restrict this type editor. This
      parameter is ignored if PersistentClass is nil. This parameter can be
      an empty string ('') which will mean that this editor applies to all
      properties of PropertyEditorType in PersistentClass.

    editorClass
      The class of the editor to be created whenever a property of the type
      passed in PropertyEditorTypeInfo is displayed in the Object Inspector.
      The class will be created by calling EditorClass.Create. }

procedure RegisterPropertyEditor(PropertyType: PTypeInfo;
  PersistentClass: TClass;  const PropertyName: shortstring;
  EditorClass: TPropertyEditorClass);

type
  TPropertyEditorMapperFunc=function(Obj: TPersistent;
    PropInfo: PPropInfo): TPropertyEditorClass;
const
  AllTypeKinds = [tkInteger..High(TTypeKind)];

procedure RegisterPropertyEditorMapper(Mapper:TPropertyEditorMapperFunc);

type
  TPropertyEditorFilterFunc =
    function(const ATestEditor: TPropertyEditor): Boolean of object;
  TPropInfoFilterFunc =
    function(const APropInfo: PPropInfo): Boolean of object;

procedure GetPersistentProperties(ASelection: TPersistentSelectionList;
  AFilter: TTypeKinds; AHook: TPropertyEditorHook; AProc: TGetPropEditProc;
  APropInfoFilterFunc: TPropInfoFilterFunc;
  AEditorFilterFunc: TPropertyEditorFilterFunc);

procedure GetPersistentProperties(ASelection: TPersistentSelectionList;
  AFilter: TTypeKinds; AHook: TPropertyEditorHook; AProc: TGetPropEditProc;
  AEditorFilterFunc: TPropertyEditorFilterFunc);

procedure GetPersistentProperties(AItem: TPersistent;
  AFilter: TTypeKinds; AHook: TPropertyEditorHook; AProc: TGetPropEditProc;
  AEditorFilterFunc: TPropertyEditorFilterFunc);

function GetEditorClass(PropInfo:PPropInfo;
  Obj: TPersistent): TPropertyEditorClass;

//==============================================================================

procedure RegisterListPropertyEditor(AnEditor: TListPropertyEditor);
procedure UnregisterListPropertyEditor(AnEditor: TListPropertyEditor);
procedure UpdateListPropertyEditors(AnObject: TObject);

type
  TSelectableComponentFlag = (
    scfWithoutRoot,
    scfWithoutInlineChilds
  );
  TSelectableComponentFlags = set of TSelectableComponentFlag;

procedure GetSelectableComponents(Root: TComponent;
  Flags: TSelectableComponentFlags; var ComponentList: TFPList);

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
  TPropHookCreateMethod = function(const Name: ShortString; ATypeInfo: PTypeInfo;
      APersistent: TPersistent; const APropertyPath: string): TMethod of object;
  TPropHookGetMethodName = function(const Method: TMethod;
                                    CheckOwner: TObject): String of object;
  TPropHookGetCompatibleMethods = procedure(InstProp: PInstProp; const Proc: TGetStrProc) of object;
  TPropHookGetMethods = procedure(TypeData: PTypeData; Proc: TGetStrProc) of object;
  TPropHookCompatibleMethodExists = function(const Name: String; InstProp: PInstProp;
                 var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean
                 ):boolean of object;
  TPropHookMethodExists = function(const Name: String; TypeData: PTypeData;
                 var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean
                 ):boolean of object;
  TPropHookRenameMethod = procedure(const CurName, NewName: String) of object;
  TPropHookShowMethod = procedure(const Name: String) of object;
  TPropHookMethodFromAncestor = function(const Method:TMethod):boolean of object;
  TPropHookChainCall = procedure(const AMethodName, InstanceName,
                      InstanceMethod:ShortString; TypeData:PTypeData) of object;
  // components
  TPropHookGetComponent = function(const ComponentPath: String):TComponent of object;
  TPropHookGetComponentName = function(AComponent: TComponent):ShortString of object;
  TPropHookGetComponentNames = procedure(TypeData: PTypeData;
                                         Proc: TGetStrProc) of object;
  TPropHookGetRootClassName = function:ShortString of object;
  TPropHookBeforeAddPersistent = function(Sender: TObject;
                                         APersistentClass: TPersistentClass;
                                         Parent: TPersistent): boolean of object;
  TPropHookComponentRenamed = procedure(AComponent: TComponent) of object;
  TPropHookPersistentAdded = procedure(APersistent: TPersistent; Select: boolean
                                      ) of object;
  TPropHookPersistentDeleting = procedure(APersistent: TPersistent) of object;
  TPropHookDeletePersistent = procedure(var APersistent: TPersistent) of object;
  TPropHookGetSelection = procedure(const ASelection: TPersistentSelectionList
                                             ) of object;
  TPropHookSetSelection = procedure(const ASelection: TPersistentSelectionList
                                             ) of object;
  TPropHookAddDependency = procedure(const AClass: TClass;
                                     const AnUnitName: shortstring) of object;
  // persistent objects
  TPropHookGetObject = function(const Name:ShortString):TPersistent of object;
  TPropHookGetObjectName = function(Instance:TPersistent):ShortString of object;
  TPropHookGetObjectNames = procedure(TypeData:PTypeData;
                                      Proc: TGetStrProc) of object;
  TPropHookObjectPropertyChanged = procedure(Sender: TObject;
                                             NewObject: TPersistent) of object;
  // modifing
  TPropHookModified = procedure(Sender: TObject) of object;
  TPropHookRevert = procedure(Instance:TPersistent; PropInfo:PPropInfo) of object;
  TPropHookRefreshPropertyValues = procedure of object;

  TPropHookType = (
    // lookup root
    htChangeLookupRoot,
    // methods
    htCreateMethod,
    htGetMethodName,
    htGetCompatibleMethods,
    htGetMethods,
    htCompatibleMethodExists,
    htMethodExists,
    htRenameMethod,
    htShowMethod,
    htMethodFromAncestor,
    htChainCall,
    // components
    htGetComponent,
    htGetComponentName,
    htGetComponentNames,
    htGetRootClassName,
    htComponentRenamed,
    // persistent selection
    htBeforeAddPersistent,
    htPersistentAdded,
    htPersistentDeleting,
    htDeletePersistent,
    htGetSelectedPersistents,
    htSetSelectedPersistents,
    // persistent objects
    htGetObject,
    htGetObjectName,
    htGetObjectNames,
    htObjectPropertyChanged,
    // modifing
    htModified,
    htRevert,
    htRefreshPropertyValues,
    // dependencies
    htAddDependency
    );

  { TPropertyEditorHook }

  TPropertyEditorHook = class
  private
    FHandlers: array[TPropHookType] of TMethodList;
    // lookup root
    FLookupRoot: TPersistent;

    procedure SetLookupRoot(APersistent: TPersistent);
    procedure AddHandler(HookType: TPropHookType; const Handler: TMethod);
    procedure RemoveHandler(HookType: TPropHookType; const Handler: TMethod);
    function GetHandlerCount(HookType: TPropHookType): integer;
    function GetNextHandlerIndex(HookType: TPropHookType;
                                 var i: integer): boolean;
  public
    GetPrivateDirectory: AnsiString;
    constructor Create;
    destructor Destroy; override;

    // lookup root
    property LookupRoot: TPersistent read FLookupRoot write SetLookupRoot;
    // methods
    function CreateMethod(const Name: ShortString; ATypeInfo:PTypeInfo;
                          APersistent: TPersistent;
                          const APropertyPath: string): TMethod;
    function GetMethodName(const Method: TMethod; PropOwner: TObject): String;
    procedure GetMethods(TypeData: PTypeData; const Proc: TGetStrProc);
    procedure GetCompatibleMethods(InstProp: PInstProp; const Proc: TGetStrProc);
    function MethodExists(const Name: String; TypeData: PTypeData;
      var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;
    function CompatibleMethodExists(const Name: String; InstProp: PInstProp;
      var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;
    procedure RenameMethod(const CurName, NewName: String);
    procedure ShowMethod(const Name: String);
    function MethodFromAncestor(const Method: TMethod):boolean;
    procedure ChainCall(const AMethodName, InstanceName,
                        InstanceMethod: ShortString;  TypeData: PTypeData);
    // components
    function GetComponent(const ComponentPath: string): TComponent;
    function GetComponentName(AComponent: TComponent): ShortString;
    procedure GetComponentNames(TypeData: PTypeData; const Proc: TGetStrProc);
    function GetRootClassName: ShortString;
    function BeforeAddPersistent(Sender: TObject;
                                 APersistentClass: TPersistentClass;
                                 Parent: TPersistent): boolean;
    procedure ComponentRenamed(AComponent: TComponent);
    procedure PersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure PersistentDeleting(APersistent: TPersistent);
    procedure DeletePersistent(var APersistent: TPersistent);
    procedure GetSelection(const ASelection: TPersistentSelectionList);
    procedure SetSelection(const ASelection: TPersistentSelectionList);
    procedure Unselect(const APersistent: TPersistent);
    procedure SelectOnlyThis(const APersistent: TPersistent);
    // persistent objects
    function GetObject(const Name: ShortString): TPersistent;
    function GetObjectName(Instance: TPersistent): ShortString;
    procedure GetObjectNames(TypeData: PTypeData; const Proc: TGetStrProc);
    procedure ObjectReferenceChanged(Sender: TObject; NewObject: TPersistent);
    // modifing
    procedure Modified(Sender: TObject);
    procedure Revert(Instance: TPersistent; PropInfo: PPropInfo);
    procedure RefreshPropertyValues;
    // dependencies
    procedure AddDependency(const AClass: TClass;
                            const AnUnitname: shortstring);
  public
    // Handlers
    procedure RemoveAllHandlersForObject(const HandlerObject: TObject);

    // lookup root
    procedure AddHandlerChangeLookupRoot(
                           const OnChangeLookupRoot: TPropHookChangeLookupRoot);
    procedure RemoveHandlerChangeLookupRoot(
                           const OnChangeLookupRoot: TPropHookChangeLookupRoot);
    // method events
    procedure AddHandlerCreateMethod(
                                   const OnCreateMethod: TPropHookCreateMethod);
    procedure RemoveHandlerCreateMethod(
                                   const OnCreateMethod: TPropHookCreateMethod);
    procedure AddHandlerGetMethodName(
                                 const OnGetMethodName: TPropHookGetMethodName);
    procedure RemoveHandlerGetMethodName(
                                 const OnGetMethodName: TPropHookGetMethodName);
    procedure AddHandlerGetCompatibleMethods(
                             const OnGetMethods: TPropHookGetCompatibleMethods);
    procedure RemoveHandlerGetCompatibleMethods(
                             const OnGetMethods: TPropHookGetCompatibleMethods);
    procedure AddHandlerGetMethods(const OnGetMethods: TPropHookGetMethods);
    procedure RemoveHandlerGetMethods(const OnGetMethods: TPropHookGetMethods);
    procedure AddHandlerCompatibleMethodExists(
                         const OnMethodExists: TPropHookCompatibleMethodExists);
    procedure RemoveHandlerCompatibleMethodExists(
                         const OnMethodExists: TPropHookCompatibleMethodExists);
    procedure AddHandlerMethodExists(
                                   const OnMethodExists: TPropHookMethodExists);
    procedure RemoveHandlerMethodExists(
                                   const OnMethodExists: TPropHookMethodExists);
    procedure AddHandlerRenameMethod(
                                   const OnRenameMethod: TPropHookRenameMethod);
    procedure RemoveHandlerRenameMethod(
                                   const OnRenameMethod: TPropHookRenameMethod);
    procedure AddHandlerShowMethod(const OnShowMethod: TPropHookShowMethod);
    procedure RemoveHandlerShowMethod(const OnShowMethod: TPropHookShowMethod);
    procedure AddHandlerMethodFromAncestor(
                       const OnMethodFromAncestor: TPropHookMethodFromAncestor);
    procedure RemoveHandlerMethodFromAncestor(
                       const OnMethodFromAncestor: TPropHookMethodFromAncestor);
    procedure AddHandlerChainCall(const OnChainCall: TPropHookChainCall);
    procedure RemoveHandlerChainCall(const OnChainCall: TPropHookChainCall);
    // component event
    procedure AddHandlerGetComponent(
                                   const OnGetComponent: TPropHookGetComponent);
    procedure RemoveHandlerGetComponent(
                                   const OnGetComponent: TPropHookGetComponent);
    procedure AddHandlerGetComponentName(
                           const OnGetComponentName: TPropHookGetComponentName);
    procedure RemoveHandlerGetComponentName(
                           const OnGetComponentName: TPropHookGetComponentName);
    procedure AddHandlerGetComponentNames(
                         const OnGetComponentNames: TPropHookGetComponentNames);
    procedure RemoveHandlerGetComponentNames(
                         const OnGetComponentNames: TPropHookGetComponentNames);
    procedure AddHandlerGetRootClassName(
                           const OnGetRootClassName: TPropHookGetRootClassName);
    procedure RemoveHandlerGetRootClassName(
                           const OnGetRootClassName: TPropHookGetRootClassName);
    // component create, delete, rename
    procedure AddHandlerComponentRenamed(
                           const OnComponentRenamed: TPropHookComponentRenamed);
    procedure RemoveHandlerComponentRenamed(
                           const OnComponentRenamed: TPropHookComponentRenamed);
    procedure AddHandlerBeforeAddPersistent(
                     const OnBeforeAddPersistent: TPropHookBeforeAddPersistent);
    procedure RemoveHandlerBeforeAddPersistent(
                     const OnBeforeAddPersistent: TPropHookBeforeAddPersistent);
    procedure AddHandlerPersistentAdded(
                             const OnPersistentAdded: TPropHookPersistentAdded);
    procedure RemoveHandlerPersistentAdded(
                             const OnPersistentAdded: TPropHookPersistentAdded);
    procedure AddHandlerPersistentDeleting(
                       const OnPersistentDeleting: TPropHookPersistentDeleting);
    procedure RemoveHandlerPersistentDeleting(
                       const OnPersistentDeleting: TPropHookPersistentDeleting);
    procedure AddHandlerDeletePersistent(
                           const OnDeletePersistent: TPropHookDeletePersistent);
    procedure RemoveHandlerDeletePersistent(
                           const OnDeletePersistent: TPropHookDeletePersistent);
    // persistent selection
    procedure AddHandlerGetSelection(
                                   const OnGetSelection: TPropHookGetSelection);
    procedure RemoveHandlerGetSelection(
                                   const OnGetSelection: TPropHookGetSelection);
    procedure AddHandlerSetSelection(
                                   const OnSetSelection: TPropHookSetSelection);
    procedure RemoveHandlerSetSelection(
                                   const OnSetSelection: TPropHookSetSelection);
    // persistent object events
    procedure AddHandlerGetObject(const OnGetObject: TPropHookGetObject);
    procedure RemoveHandlerGetObject(const OnGetObject: TPropHookGetObject);
    procedure AddHandlerGetObjectName(
                                 const OnGetObjectName: TPropHookGetObjectName);
    procedure RemoveHandlerGetObjectName(
                                 const OnGetObjectName: TPropHookGetObjectName);
    procedure AddHandlerGetObjectNames(
                               const OnGetObjectNames: TPropHookGetObjectNames);
    procedure RemoveHandlerGetObjectNames(
                               const OnGetObjectNames: TPropHookGetObjectNames);
    procedure AddHandlerObjectPropertyChanged(
                 const OnObjectPropertyChanged: TPropHookObjectPropertyChanged);
    procedure RemoveHandlerObjectPropertyChanged(
                 const OnObjectPropertyChanged: TPropHookObjectPropertyChanged);
    // modifing events
    procedure AddHandlerModified(const OnModified: TPropHookModified);
    procedure RemoveHandlerModified(const OnModified: TPropHookModified);
    procedure AddHandlerRevert(const OnRevert: TPropHookRevert);
    procedure RemoveHandlerRevert(const OnRevert: TPropHookRevert);
    procedure AddHandlerRefreshPropertyValues(
                 const OnRefreshPropertyValues: TPropHookRefreshPropertyValues);
    procedure RemoveHandlerRefreshPropertyValues(
                 const OnRefreshPropertyValues: TPropHookRefreshPropertyValues);
    procedure AddHandlerAddDependency(
                                 const OnAddDependency: TPropHookAddDependency);
    procedure RemoveHandlerAddDependency(
                                 const OnAddDependency: TPropHookAddDependency);
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
    procedure Sort;
    property Count: Integer read FCount;
    property Items[Index: Integer]: PPropInfo read Get; default;
  end;

//==============================================================================

const
  UnknownVKPrefix = 'Word(''';
  UnknownVKPostfix = ''')';
function KeyAndShiftStateToKeyString(Key: word; ShiftState: TShiftState): String;
function KeyStringIsIrregular(const s: string): boolean;
function KeyStringToVKCode(const s: string): word;

type
  TStringsPropEditorDlg = class(TStringsPropEditorFrm)
  public
    Editor: TPropertyEditor;
  end;

  { TCustomShortCutGrabBox }

  TCustomShortCutGrabBox = class(TCustomPanel)
  private
    FAllowedShifts: TShiftState;
    FGrabButton: TButton;
    FKey: Word;
    FKeyComboBox: TComboBox;
    FShiftButtons: TShiftState;
    FShiftState: TShiftState;
    FCheckBoxes: array[TShiftStateEnum] of TCheckBox;
    FGrabForm: TForm;
    function GetShiftCheckBox(Shift: TShiftStateEnum): TCheckBox;
    procedure SetAllowedShifts(const AValue: TShiftState);
    procedure SetKey(const AValue: Word);
    procedure SetShiftButtons(const AValue: TShiftState);
    procedure SetShiftState(const AValue: TShiftState);
    procedure OnGrabButtonClick(Sender: TObject);
    procedure OnShitCheckBoxClick(Sender: TObject);
    procedure OnGrabFormKeyDown(Sender: TObject; var AKey: Word;
      AShift: TShiftState);
    procedure OnKeyComboboxEditingDone(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure UpdateShiftButons;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
           override;
    function ShiftToStr(s: TShiftStateEnum): string;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetDefaultShiftButtons: TShiftState;
    property ShiftState: TShiftState read FShiftState write SetShiftState;
    property Key: Word read FKey write SetKey;
    property ShiftButtons: TShiftState read FShiftButtons write SetShiftButtons;
    property AllowedShifts: TShiftState read FAllowedShifts write SetAllowedShifts;
    property KeyComboBox: TComboBox read FKeyComboBox;
    property GrabButton: TButton read FGrabButton;
    property ShiftCheckBox[Shift: TShiftStateEnum]: TCheckBox read GetShiftCheckBox;
  end;


  { TShortCutGrabBox }

  TShortCutGrabBox = class(TCustomShortCutGrabBox)
  published
    property Align;
    property Alignment;
    property AllowedShifts;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property Key;
    property OnClick;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetDockCaption;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShiftButtons;
    property ShiftState;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
  end;

//==============================================================================


// Global flags:
var
  GReferenceExpandable: Boolean = true;
  GShowReadOnlyProps: Boolean = true;

// default Hook - set by IDE
var
  GlobalDesignHook: TPropertyEditorHook;

function ClassTypeInfo(Value: TClass): PTypeInfo;
function GetClassUnitName(Value: TClass): string;
procedure CreateComponentEvent(AComponent: TComponent; const EventName: string);
function ClassNameToComponentName(const AClassName: string): string;

procedure LazSetMethodProp(Instance : TObject;PropInfo : PPropInfo; Value : TMethod);
procedure WritePublishedProperties(Instance: TPersistent);
procedure EditCollection(AComponent: TComponent; ACollection: TCollection; APropertyName: String);

// Returns true if given property should be displayed on the property list
// filtered by AFilter.
function IsInteresting(
  const AEditor: TPropertyEditor; const AFilter: TTypeKinds): Boolean;

function dbgs(peh: TPropEditHint): string; overload;

const
  NoDefaultValue = Longint($80000000); // magic number for properties with nodefault modifier

implementation

type
  TPersistentAccess = class(TPersistent);

var
  ListPropertyEditors: TList = nil;
  VirtualKeyStrings: TStringHashList = nil;

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

type

  { TSelectableComponentEnumerator }

  TSelectableComponentEnumerator = class(TComponent)
  public
    List: TFPList;
    Flags: TSelectableComponentFlags;
    Root: TComponent;
    procedure GetSelectableComponents(ARoot: TComponent);
    procedure Gather(Child: TComponent);
  end;

{ TSelectableComponentEnumerator }

procedure TSelectableComponentEnumerator.GetSelectableComponents(
  ARoot: TComponent);
begin
  Root:=ARoot;
  if List=nil then
    List:=TFPList.Create;
  if Root=nil then exit;
  if not (scfWithoutRoot in Flags) then List.Add(Root);
  TSelectableComponentEnumerator(Root).GetChildren(@Gather,Root);
end;

procedure TSelectableComponentEnumerator.Gather(Child: TComponent);
var
  OldRoot: TComponent;
begin
  if not ((Child is TControl)
          and (csNoDesignSelectable in TControl(Child).ControlStyle))
  then
    List.Add(Child);
  OldRoot:=Root;
  try
    if csInline in Child.ComponentState then begin
      if scfWithoutInlineChilds in Flags then exit;
      if (Child is TControl)
      and (csOwnedChildrenNotSelectable in TControl(Child).ControlStyle) then
        exit;
      Root:=Child;
    end;
    TSelectableComponentEnumerator(Child).GetChildren(@Gather,Root);
  finally
    Root:=OldRoot;
  end;
end;

procedure GetSelectableComponents(Root: TComponent;
  Flags: TSelectableComponentFlags; var ComponentList: TFPList);
var
  e: TSelectableComponentEnumerator;
begin
  e:=TSelectableComponentEnumerator.Create(nil);
  try
    e.List:=ComponentList;
    e.Flags:=Flags;
    e.GetSelectableComponents(Root);
    ComponentList:=e.List;
  finally
    e.Free;
  end;
end;

Procedure LazSetMethodProp(Instance : TObject;PropInfo : PPropInfo; Value : TMethod);
type
  PMethod = ^TMethod;
  TSetMethodProcIndex=procedure(index:longint;p:TMethod) of object;
  TSetMethodProc=procedure(p:TMethod) of object;
var
  AMethod : TMethod;
begin
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptfield:
      PMethod(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^ := Value;
    ptstatic,
    ptvirtual :
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if (Value.Code=nil) and (Value.Data<>nil) then begin
          // this is a fake method
          // Comparing fake methods with OldValue=NewValue results always in
          // true. Therefore this will fail:
          //   if FMethod=NewValue then exit;
          //   FMethod:=NewValue;
          // Change the method two times
          try
            Value.Code:=Pointer(1);
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              TSetMethodProcIndex(AMethod)(PropInfo^.Index,Value)
            else
              TSetMethodProc(AMethod)(Value);
          except
          end;
          Value.Code:=nil;
        end;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          TSetMethodProcIndex(AMethod)(PropInfo^.Index,Value)
        else
          TSetMethodProc(AMethod)(Value);
      end;
  end;
end;

// -----------------------------------------------------------

procedure WritePublishedProperties(Instance: TPersistent);
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  PropInfo: PPropInfo;
  PropData: ^TPropData;
  CurCount: integer;
begin
  TypeInfo:=Instance.ClassInfo;
  TypeData:=GetTypeData(TypeInfo);
  debugln('WritePublishedProperties Instance=',DbgS(Instance),' ',Instance.ClassName,' TypeData^.PropCount=',dbgs(TypeData^.PropCount));
  if Instance is TComponent then
    debugln('  TComponent(Instance).Name=',TComponent(Instance).Name);

  // read all properties and remove doubles
  TypeInfo:=Instance.ClassInfo;
  repeat
    // read all property infos of current class
    TypeData:=GetTypeData(TypeInfo);
    // skip unitname
    PropData:=AlignToPtr(PByte(@TypeData^.UnitName)+Length(TypeData^.UnitName)+1);
    // read property count
    CurCount:=PWord(PropData)^;
    PropInfo:=PPropInfo(@PropData^.PropList);
    debugln('    UnitName=',TypeData^.UnitName,' Type=',TypeInfo^.Name,' CurPropCount=',dbgs(CurCount));

    {writeln('TPropInfoList.Create D ',CurCount,' TypeData^.ClassType=',DbgS(TypeData^.ClassType));
    writeln('TPropInfoList.Create E ClassName="',TypeData^.ClassType.ClassName,'"',
    ' TypeInfo=',DbgS(TypeInfo),
    ' TypeData^.ClassType.ClassInfo=',DbgS(TypeData^.ClassType.ClassInfo),
    ' TypeData^.ClassType.ClassParent=',DbgS(TypeData^.ClassType.ClassParent),
    ' TypeData^.ParentInfo=',DbgS(TypeData^.ParentInfo),
    '');
    CurParent:=TypeData^.ClassType.ClassParent;
    if CurParent<>nil then begin
      writeln('TPropInfoList.Create F CurParent.ClassName=',CurParent.ClassName,
        ' CurParent.ClassInfo=',DbgS(CurParent.ClassInfo),
        '');
    end;}

    // read properties
    while CurCount>0 do begin
      // point PropInfo to next propinfo record.
      // Located at Name[Length(Name)+1] !
      debugln('      Property ',PropInfo^.Name,' Type=',PropInfo^.PropType^.Name);
      PropInfo:=PPropInfo(AlignToPtr(pointer(@PropInfo^.Name)+PByte(@PropInfo^.Name)^+1));
      dec(CurCount);
    end;
    TypeInfo:=TypeData^.ParentInfo;
    if TypeInfo=nil then break;
  until false;
end;


//------------------------------------------------------------------------------

const
{ TypeKinds  see typinfo.pp
       TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,tkFloat,
                   tkSet,tkMethod,tkSString,tkLString,tkAString,
                   tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                   tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord,
                   tkDynArray,tkInterfaceRaw,tkProcVar,tkUString,tkUChar,
                   tkHelper);
}

  PropClassMap:array[TypInfo.TTypeKind] of TPropertyEditorClass=(
    nil,                       // tkUnknown
    TIntegerPropertyEditor,    // tkInteger
    TCharpropertyEditor,       // tkChar
    TEnumPropertyEditor,       // tkEnumeration
    TFloatPropertyEditor,      // tkFloat
    TSetPropertyEditor,        // tkSet
    TMethodPropertyEditor,     // tkMethod
    TStringPropertyEditor,     // tkSString
    TStringPropertyEditor,     // tkLString
    TStringPropertyEditor,     // tkAString
    TWideStringPropertyEditor, // tkWString
    TPropertyEditor,           // tkVariant
    nil,                       // tkArray
    nil,                       // tkRecord
    nil,                       // tkInterface
    TClassPropertyEditor,      // tkClass
    nil,                       // tkObject
    TPropertyEditor,           // tkWChar
    TBoolPropertyEditor,       // tkBool
    TInt64PropertyEditor,      // tkInt64
    TQWordPropertyEditor,      // tkQWord
    nil,                       // tkDynArray
    nil                        // tkInterfaceRaw,
{$IF declared(tkUString)}
// can be replaced by {$IFNDEF VER2_2} later, if grace period of older 2.3.1 ends
    ,nil,                      // tkProcVar
    nil,                       // tkUString
    nil                        // tkUChar
{$ENDIF}
{$IF declared(tkHelper)}
    ,nil                       // tkHelper
{$ENDIF}
    );

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
    PersistentClass:TClass;
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
  PropData: ^TPropData;
  CurCount, i: integer;
  //CurParent: TClass;
begin
  TypeInfo:=Instance.ClassInfo;
  TypeData:=GetTypeData(TypeInfo);
  GetMem(BigList,TypeData^.PropCount * SizeOf(Pointer));

  // read all properties and remove doubles
  TypeInfo:=Instance.ClassInfo;
  FCount:=0;
  repeat
    // read all property infos of current class
    TypeData:=GetTypeData(TypeInfo);
    // skip unitname
    PropData:=AlignToPtr(Pointer(@TypeData^.UnitName)+Length(TypeData^.UnitName)+1);
    // read property count
    CurCount:=PropData^.PropCount;
    PropInfo:=PPropInfo(@PropData^.PropList);

    {writeln('TPropInfoList.Create D ',CurCount,' TypeData^.ClassType=',DbgS(TypeData^.ClassType));
    writeln('TPropInfoList.Create E ClassName="',TypeData^.ClassType.ClassName,'"',
    ' TypeInfo=',DbgS(TypeInfo),
    ' TypeData^.ClassType.ClassInfo=',DbgS(TypeData^.ClassType.ClassInfo),
    ' TypeData^.ClassType.ClassParent=',DbgS(TypeData^.ClassType.ClassParent),
    ' TypeData^.ParentInfo=',DbgS(TypeData^.ParentInfo),
    '');
    CurParent:=TypeData^.ClassType.ClassParent;
    if CurParent<>nil then begin
      writeln('TPropInfoList.Create F CurParent.ClassName=',CurParent.ClassName,
        ' CurParent.ClassInfo=',DbgS(CurParent.ClassInfo),
        '');
    end;}

    // read properties
    while CurCount>0 do begin
      if PropInfo^.PropType^.Kind in Filter then begin
        // check if name already exists in list
        i:=FCount-1;
        while (i>=0) and (CompareText(BigList^[i]^.Name,PropInfo^.Name)<>0) do
          dec(i);
        if (i<0) then begin
          // add property info to BigList
          BigList^[FCount]:=PropInfo;
          inc(FCount);
        end;
      end;
      // point PropInfo to next propinfo record.
      // Located at Name[Length(Name)+1] !
      PropInfo:=PPropInfo(AlignToPtr(pointer(@PropInfo^.Name)+PByte(@PropInfo^.Name)^+1));
      dec(CurCount);
    end;
    TypeInfo:=TypeData^.ParentInfo;
    if TypeInfo=nil then break;
  until false;

  // create FList
  FSize:=FCount * SizeOf(Pointer);
  GetMem(FList,FSize);
  Move(BigList^,FList^,FSize);
  FreeMem(BigList);
  Sort;
end;

destructor TPropInfoList.Destroy;
begin
  if FList<>nil then FreeMem(FList,FSize);
end;

function TPropInfoList.Contains(P:PPropInfo):Boolean;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    with FList^[I]^ do
    begin
      if (PropType^.Kind=P^.PropType^.Kind) and (CompareText(Name,P^.Name)=0) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
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

procedure TPropInfoList.Sort;
  procedure QuickSort(L, R: Integer);
  var
    I, J: Longint;
    P, Q: PPropInfo;
  begin
    repeat
      I := L;
      J := R;
      P := FList^[(L + R) div 2];
      repeat
        while CompareText(P^.Name, FList^[i]^.Name) > 0 do
          inc(I);
        while CompareText(P^.Name, FList^[J]^.Name) < 0 do
          dec(J);
        if I <= J then
        begin
          Q := FList^[I];
          Flist^[I] := FList^[J];
          FList^[J] := Q;
          inc(I);
          dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;
begin
  if Count > 0 then
    QuickSort(0, Count - 1);
end;

//------------------------------------------------------------------------------


{ GetComponentProperties }

procedure RegisterPropertyEditor(PropertyType:PTypeInfo;
  PersistentClass: TClass;  const PropertyName:shortstring;
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
  P^.PersistentClass:=PersistentClass;
  P^.PropertyName:=PropertyName;
  //if Assigned(PersistentClass) then P^.PropertyName:=PropertyName;
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
  Result := nil;
  if PropertyEditorMapperList<>nil then begin
    for I:=0 to PropertyEditorMapperList.Count-1 do begin
      with PPropertyEditorMapperRec(PropertyEditorMapperList[I])^ do begin
        Result:=Mapper(Obj,PropInfo);
        if Result<>nil then break;
      end;
    end;
  end;
  if Result=nil then begin
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
        if ((P^.PersistentClass=nil) or (Obj.InheritsFrom(P^.PersistentClass))) and
           ((P^.PropertyName='')
           or (CompareText(PropInfo^.Name,P^.PropertyName)=0))
        then
          if (C=nil) or   // see if P is better match than C
             ((C^.PersistentClass=nil) and (P^.PersistentClass<>nil)) or
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
             ((P^.PersistentClass<>nil) and (C^.PersistentClass<>nil) and
              (P^.PersistentClass<>C^.PersistentClass) and
              (P^.PersistentClass.InheritsFrom(C^.PersistentClass)))
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
  if (Result<>nil) and Result.InheritsFrom(THiddenPropertyEditor) then
    Result:=nil;
end;

procedure GetPersistentProperties(ASelection: TPersistentSelectionList;
  AFilter: TTypeKinds; AHook: TPropertyEditorHook; AProc: TGetPropEditProc;
  APropInfoFilterFunc: TPropInfoFilterFunc;
  AEditorFilterFunc: TPropertyEditorFilterFunc);
var
  I, J, SelCount: Integer;
  ClassTyp: TClass;
  Candidates: TPropInfoList;
  PropLists: TList;
  PropEditor: TPropertyEditor;
  EdClass: TPropertyEditorClass;
  PropInfo: PPropInfo;
  AddEditor: Boolean;
  Instance: TPersistent;
begin
  if (ASelection = nil) or (ASelection.Count = 0) then Exit;
  SelCount := ASelection.Count;
  Instance := ASelection[0];
  ClassTyp := Instance.ClassType;
  // Create a property candidate list of all properties that can be found in
  // every component in the list and in the Filter
  Candidates := TPropInfoList.Create(Instance, AFilter);
  try
    // check each property candidate
    for I := Candidates.Count - 1 downto 0 do
    begin
      PropInfo := Candidates[I];
      // check if property is readable
      if (PropInfo^.GetProc=nil)
      or ((not GShowReadOnlyProps) and (PropInfo^.PropType^.Kind <> tkClass)
          and (PropInfo^.SetProc = nil))
      or (Assigned(APropInfoFilterFunc) and (not APropInfoFilterFunc(PropInfo)))
      then begin
        Candidates.Delete(I);
        Continue;
      end;

      EdClass := GetEditorClass(PropInfo, Instance);
      if EdClass = nil
      then begin
        Candidates.Delete(I);
        Continue;
      end;

      // create a test property editor for the property
      PropEditor := EdClass.Create(AHook,1);
      PropEditor.SetPropEntry(0, Instance, PropInfo);
      PropEditor.Initialize;
      // check for multiselection, ValueAvailable and customfilter
      if ((SelCount > 1)
          and not (paMultiSelect in PropEditor.GetAttributes))
      or not PropEditor.ValueAvailable
      or (Assigned(AEditorFilterFunc) and not AEditorFilterFunc(PropEditor))
      then begin
        Candidates.Delete(I);
      end;
      PropEditor.Free;
    end;

    PropLists := TList.Create;
    try
      PropLists.Count := SelCount;
      // Create a property info list for each component in the selection
      for I := 0 to SelCount - 1 do
        PropLists[i] := TPropInfoList.Create(ASelection[I], AFilter);

      // Eliminate each property in Candidates that is not in all property lists
      for I := 0 to SelCount - 1 do
        Candidates.Intersect(TPropInfoList(PropLists[I]));

      // Eliminate each property in the property list that are not in Candidates
      for I := 0 to SelCount - 1 do
        TPropInfoList(PropLists[I]).Intersect(Candidates);

      // PropList now has a matrix of PropInfo's.
      // -> create a property editor for each property
      for I := 0 to Candidates.Count - 1 do
      begin
        EdClass := GetEditorClass(Candidates[I], Instance);
        if EdClass = nil then Continue;
        PropEditor := EdClass.Create(AHook, SelCount);
        AddEditor := True;
        for J := 0 to SelCount - 1 do
        begin
          if (ASelection[J].ClassType <> ClassTyp) and
            (GetEditorClass(TPropInfoList(PropLists[J])[I],
              ASelection[J]) <> EdClass) then
          begin
            AddEditor := False;
            Break;
          end;
          PropEditor.SetPropEntry(J, ASelection[J],
            TPropInfoList(PropLists[J])[I]);
        end;
        if AddEditor then
        begin
          PropEditor.Initialize;
          if not PropEditor.ValueAvailable then AddEditor:=false;
        end;
        if AddEditor then
          AProc(PropEditor)
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

procedure GetPersistentProperties(ASelection: TPersistentSelectionList;
  AFilter: TTypeKinds; AHook: TPropertyEditorHook; AProc: TGetPropEditProc;
  AEditorFilterFunc: TPropertyEditorFilterFunc);
begin
  GetPersistentProperties(ASelection,AFilter,AHook,AProc,nil,AEditorFilterFunc);
end;

procedure GetPersistentProperties(AItem: TPersistent;
  AFilter: TTypeKinds; AHook: TPropertyEditorHook; AProc: TGetPropEditProc;
  AEditorFilterFunc: TPropertyEditorFilterFunc);
var
  Selection: TPersistentSelectionList;
begin
  if AItem = nil then Exit;
  Selection := TPersistentSelectionList.Create;
  try
    Selection.Add(AItem);
    GetPersistentProperties(Selection,AFilter,AHook,AProc,AEditorFilterFunc);
  finally
    Selection.Free;
  end;
end;


{ TPropertyEditor }

constructor TPropertyEditor.Create(Hook: TPropertyEditorHook;
  APropCount:Integer);
var
  PropListSize: Integer;
begin
  FPropertyHook:=Hook;
  PropListSize:=APropCount * SizeOf(TInstProp);
  GetMem(FPropList,PropListSize);
  FillChar(FPropList^,PropListSize,0);
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
  Values: TStringList;
  AddValue: TGetStrFunc;
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

procedure TPropertyEditor.ShowValue;
begin

end;

function TPropertyEditor.AutoFill:Boolean;
begin
  Result:=True;
end;

function TPropertyEditor.GetAttributes:TPropertyAttributes;
begin
  Result:=[paMultiSelect,paRevertable];
end;

function TPropertyEditor.IsReadOnly: boolean;
begin
  Result:=paReadOnly in GetAttributes;
end;

function TPropertyEditor.GetComponent(Index: Integer): TPersistent;
begin
  Result:=FPropList^[Index].Instance;
end;

function TPropertyEditor.GetUnitName(Index: Integer): string;
begin
  Result:=GetClassUnitName(GetComponent(Index).ClassType);
end;

function TPropertyEditor.GetPropTypeUnitName(Index: Integer): string;
type
  PPropData = ^TPropData;
var
  AComponent: TPersistent;
  CurPropInfo: PPropInfo;
  hp: PTypeData;
  pd: PPropData;
  i: Integer;
  UpperName: ShortString;
  ATypeInfo: PTypeInfo;
  NameFound: Boolean;
  ThePropType: PTypeInfo;
begin
  Result:='';
  AComponent:=GetComponent(Index);
  UpperName:=UpCase(GetName);
  ThePropType:=GetPropType;
  ATypeInfo:=PTypeInfo(AComponent.ClassInfo);
  while Assigned(ATypeInfo) do begin
    // skip the name
    hp:=GetTypeData(ATypeInfo);
    // the class info rtti the property rtti follows immediatly
    pd:=AlignToPtr(Pointer(Pointer(@hp^.UnitName)+Length(hp^.UnitName)+1));
    CurPropInfo:=PPropInfo(@pd^.PropList);
    NameFound:=false;
    for i:=1 to pd^.PropCount do begin
      // found a property of that name ?
      if Upcase(CurPropInfo^.Name)=UpperName then begin
        DebugLn(['TPropertyEditor.GetPropTypeUnitName ',hp^.UnitName,' IsSamePropInfo=',CurPropInfo^.PropType=ThePropType]);
        NameFound:=true;
        if CurPropInfo^.PropType=ThePropType then
          Result:=hp^.UnitName;
      end;
      // skip to next property
      CurPropInfo:=PPropInfo(AlignToPtr(Pointer(@CurPropInfo^.Name)+Byte(CurPropInfo^.Name[0])+1));
    end;
    if not NameFound then break;
    // parent class
    ATypeInfo:=hp^.ParentInfo;
  end;
end;

function TPropertyEditor.GetPropertyPath(Index: integer): string;
begin
  Result:=GetComponent(Index).ClassName+'.'+GetName;
end;

function TPropertyEditor.GetFloatValue:Extended;
begin
  Result:=GetFloatValueAt(0);
end;

Procedure SetIndexValues (P: PPRopInfo; Var Index, IValue : Longint);

begin
  Index:=((P^.PropProcs shr 6) and 1);
  If Index<>0 then
    IValue:=P^.Index
  else
    IValue:=0;
end;

function TPropertyEditor.GetFloatValueAt(Index:Integer):Extended;
begin
  with FPropList^[Index] do Result:=GetFloatProp(Instance,PropInfo);
end;

function TPropertyEditor.GetMethodValue:TMethod;
begin
  Result:=GetMethodValueAt(0);
end;

// workaround for buggy rtl function
function LazGetMethodProp(Instance: TObject; PropInfo: PPropInfo): TMethod;
type
  TGetMethodProcIndex=function(Index: Longint): TMethod of object;
  TGetMethodProc=function(): TMethod of object;
  PMethod = ^TMethod;
var
  value: PMethod;
  AMethod : TMethod;
begin
  Result.Code:=nil;
  Result.Data:=nil;
  case (PropInfo^.PropProcs) and 3 of
    ptfield:
      begin
        Value:=PMethod(Pointer(Instance)+PtrUInt(PropInfo^.GetProc));
        if Value<>nil then
          Result:=Value^;
      end;
    ptstatic,
    ptvirtual :
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PPointer(Pointer(Instance.ClassType)
                        +PtrUInt(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          Result:=TGetMethodProcIndex(AMethod)(PropInfo^.Index)
        else
          Result:=TGetMethodProc(AMethod)();
      end;
  end;
end;

function TPropertyEditor.GetMethodValueAt(Index:Integer):TMethod;
begin
  with FPropList^[Index] do Result:=LazGetMethodProp(Instance,PropInfo);
end;

function TPropertyEditor.GetEditLimit: Integer;
begin
  Result := 255;
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

function TPropertyEditor.GetObjectValue: TObject;
begin
  Result:=GetObjectValueAt(0);
end;

function TPropertyEditor.GetObjectValue(MinClass: TClass): TObject;
begin
  Result:=GetObjectValueAt(0, MinClass);
end;

function TPropertyEditor.GetObjectValueAt(Index: Integer): TObject;
begin
  with FPropList^[Index] do
    Result:=GetObjectProp(Instance,PropInfo,nil); // nil for fpc 1.0.x
end;

function TPropertyEditor.GetObjectValueAt(Index: Integer; MinClass: TClass): TObject;
begin
  with FPropList^[Index] do
    Result:=GetObjectProp(Instance,PropInfo,MinClass);
end;

function TPropertyEditor.GetDefaultOrdValue: Longint;
var
  APropInfo: PPropInfo;
begin
  APropInfo:=FPropList^[0].PropInfo;
  {if HasAncestor then
    Result:=GetOrdValue(Ancestor,APropInfo)
  else}
  Result:=APropInfo^.Default;
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

function TPropertyEditor.GetInstProp: PInstProp;
begin
  Result:=@FPropList^[0];
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

function TPropertyEditor.GetWideStrValue: WideString;
begin
  Result:=GetWideStrValueAt(0);
end;

function TPropertyEditor.GetWideStrValueAt(Index: Integer): WideString;
begin
  with FPropList^[Index] do Result:=GetWideStrProp(Instance,PropInfo);
end;

function TPropertyEditor.GetValue:ansistring;
begin
  Result:=oisUnknown;
end;

function TPropertyEditor.GetHint(HintType: TPropEditHint; x, y: integer
  ): string;
var
  TypeHint: String;
begin
  Result := GetName + #13 + oisValue + ' ' + GetVisualValue;
  case GetPropType^.Kind of
   tkInteger : TypeHint:=oisInteger;
   tkInt64 : TypeHint:=oisInt64;
   tkBool : TypeHint:=oisBoolean;
   tkEnumeration : TypeHint:=oisEnumeration;
   tkChar, tkWChar : TypeHint:=oisChar;
   tkUnknown : TypeHint:=oisUnknown;
   tkObject : TypeHint:=oisObject;
   tkClass : TypeHint:=oisClass;
   tkQWord : TypeHint:=oisWord;
   tkString, tkLString, tkAString, tkWString : TypeHint:=oisString;
   tkFloat : TypeHint:=oisFloat;
   tkSet : TypeHint:=oisSet;
   tkMethod : TypeHint:=oisMethod;
   tkVariant : TypeHint:=oisVariant;
   tkArray : TypeHint:=oisArray;
   tkRecord : TypeHint:=oisRecord;
   tkInterface : TypeHint:=oisInterface;
  else
    TypeHint:='';
  end;
  if TypeHint<>'' then
    Result:=Result+#13+TypeHint;
end;

function TPropertyEditor.GetDefaultValue: ansistring;
begin
  if not (paHasDefaultValue in GetAttributes) then
    raise EPropertyError.Create('No default property available');
  Result:='';
end;

function TPropertyEditor.GetVisualValue:ansistring;
begin
  if AllEqual then
    Result:=GetValue
  else
    Result:='';
end;

procedure TPropertyEditor.GetValues(Proc:TGetStrProc);
begin
end;

procedure TPropertyEditor.Initialize;

  procedure RaiseNoInstance;
  begin
    raise Exception.Create('TPropertyEditor.Initialize '+dbgsName(Self));
  end;

begin
  if FPropList^[0].Instance=nil then
    RaiseNoInstance;
end;

procedure TPropertyEditor.Modified;
begin
  if PropertyHook <> nil then
    PropertyHook.Modified(Self);
end;

procedure TPropertyEditor.SetFloatValue(const NewValue:Extended);
var
  I:Integer;
  Changed: boolean;
begin
  Changed:=false;
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do
      Changed:=Changed or (GetFloatProp(Instance,PropInfo)<>NewValue);
  if Changed then begin
    for I:=0 to FPropCount-1 do
      with FPropList^[I] do SetFloatProp(Instance,PropInfo,NewValue);
    Modified;
  end;
end;

procedure TPropertyEditor.SetMethodValue(const NewValue:TMethod);
var
  I:Integer;
  Changed: boolean;
  AMethod: TMethod;
begin
  Changed:=false;
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do begin
      AMethod:=LazGetMethodProp(Instance,PropInfo);
      Changed:=Changed or not CompareMem(@AMethod,@NewValue,SizeOf(TMethod));
    end;
  if Changed then begin
    for I:=0 to FPropCount-1 do
      with FPropList^[I] do LazSetMethodProp(Instance,PropInfo,NewValue);
    Modified;
  end;
end;

procedure TPropertyEditor.SetOrdValue(const NewValue: Longint);
var
  I:Integer;
  Changed: boolean;
begin
  Changed := False;
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      Changed := Changed or (GetOrdProp(Instance, PropInfo) <> NewValue);
  if Changed then begin
    for I:=0 to FPropCount-1 do
      with FPropList^[I] do SetOrdProp(Instance, PropInfo, NewValue);
    Modified;
  end;
end;

procedure TPropertyEditor.SetPtrValue(const NewValue:Pointer);
var
  I: Integer;
  Changed: boolean;
begin
  Changed := False;
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      Changed := Changed or (GetOrdProp(Instance, PropInfo) <> PtrInt(PtrUInt(NewValue)));
  if Changed then
  begin
    for I := 0 to FPropCount - 1 do
      with FPropList^[I] do SetOrdProp(Instance, PropInfo, PtrInt(PtrUInt(NewValue)));
    Modified;
  end;
end;

procedure TPropertyEditor.SetPropEntry(Index:Integer;
  AnInstance:TPersistent; APropInfo:PPropInfo);
begin
  with FPropList^[Index] do begin
    Instance:=AnInstance;
    PropInfo:=APropInfo;
  end;
end;

procedure TPropertyEditor.SetStrValue(const NewValue:AnsiString);
var
  I:Integer;
  Changed: boolean;
begin
  Changed:=false;
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do
      Changed:=Changed or (GetStrProp(Instance,PropInfo)<>NewValue);
  if Changed then begin
    for I:=0 to FPropCount-1 do
      with FPropList^[I] do SetStrProp(Instance,PropInfo,NewValue);
    Modified;
  end;
end;

procedure TPropertyEditor.SetVarValue(const NewValue:Variant);
var
  I:Integer;
  Changed: boolean;
begin
  Changed:=false;
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do
      Changed:=Changed or (GetVariantProp(Instance,PropInfo)<>NewValue);
  if Changed then begin
    for I:=0 to FPropCount-1 do
      with FPropList^[I] do SetVariantProp(Instance,PropInfo,NewValue);
    Modified;
  end;
end;

procedure TPropertyEditor.SetWideStrValue(const NewValue: WideString);
var
  I:Integer;
  Changed: boolean;
begin
  Changed:=false;
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do
      Changed:=Changed or (GetWideStrProp(Instance,PropInfo)<>NewValue);
  if Changed then begin
    for I:=0 to FPropCount-1 do
      with FPropList^[I] do SetWideStrProp(Instance,PropInfo,NewValue);
    Modified;
  end;
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

procedure TPropertyEditor.SetInt64Value(const NewValue:Int64);
var
  I:Integer;
  Changed: boolean;
begin
  Changed:=false;
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do
      Changed:=Changed or (GetInt64Prop(Instance,PropInfo)<>NewValue);
  if Changed then begin
    for I:=0 to FPropCount-1 do
      with FPropList^[I] do SetInt64Prop(Instance,PropInfo,NewValue);
    Modified;
  end;
end;

{ these three procedures implement the default render behavior of the
  object inspector's drop down list editor. You don't need to
  override the two measure procedures if the default width or height don't
  need to be changed. }
procedure TPropertyEditor.ListMeasureHeight(const AValue: AnsiString;
  Index: Integer; ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := ACanvas.TextHeight(AValue);
end;

procedure TPropertyEditor.ListMeasureWidth(const AValue: AnsiString;
  Index: Integer; ACanvas: TCanvas; var AWidth: Integer);
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
    Opaque := (pedsInEdit in AState) and (ACanvas.Brush.Color <> clNone);
    Clipping := True;
    ShowPrefix := True;
    WordBreak := False;
    SingleLine := True;
    SystemFont := False;
  end;
  If (pedsInComboList in AState) and not (pedsInEdit in AState)
  then begin
    OldColor := ACanvas.Brush.Color;
    If pedsSelected in AState then begin
      ACanvas.Brush.Color := clHighlight;
      ACanvas.Font.Color := clHighlightText;
    end
    else begin
      ACanvas.Brush.Color := clwhite{clWindow};
      ACanvas.Font.Color := clWindowText;
    end;
    ACanvas.FillRect(ARect);
    ACanvas.Brush.Color := OldColor;
  end;
  ACanvas.TextRect(ARect, ARect.Left+2,ARect.Top,AValue, Style);
end;

{ these three procedures implement the default render behavior of the
  object inspector's property row. You don't need to override the measure
  procedure if the default height don't need to be changed. }
procedure TPropertyEditor.PropMeasureHeight(const NewValue:ansistring;
  ACanvas:TCanvas;  var AHeight:Integer);
begin
  //
end;

procedure TPropertyEditor.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  AState: TPropEditDrawState);
var
  Style : TTextStyle;
begin
  FillChar(Style,SizeOf(Style),0);
  With Style do begin
    Alignment := taLeftJustify;
    Layout := tlCenter;
    Opaque := False;
    Clipping := True;
    ShowPrefix := False;
    WordBreak := False;
    SingleLine := True;
    ExpandTabs := True;
    SystemFont := False;
  end;
  ACanvas.TextRect(ARect,ARect.Left+2,ARect.Top,GetName,Style);
end;

procedure TPropertyEditor.PropDrawValue(ACanvas:TCanvas; const ARect: TRect;
  AState: TPropEditDrawState);
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
    ExpandTabs := True;
    SystemFont := False;
  end;
  ACanvas.TextRect(ARect,ARect.Left+3,ARect.Top,GetVisualValue, Style);
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

function TPropertyEditor.IsDefaultValue: boolean;
begin
  Result:=(paHasDefaultValue in GetAttributes)
      and (GetDefaultValue=GetVisualValue);
end;

function TPropertyEditor.IsNotDefaultValue: boolean;
begin
  Result:=(paHasDefaultValue in GetAttributes)
           and (GetDefaultValue<>GetVisualValue);
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

function TOrdinalPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=(inherited GetAttributes);
  if GetDefaultOrdValue <> NoDefaultValue then
    Result := Result + [paHasDefaultValue];
end;

function TOrdinalPropertyEditor.GetValue: ansistring;
begin
  Result:=OrdValueToVisualValue(GetOrdValue);
end;

function TOrdinalPropertyEditor.GetDefaultValue: ansistring;
begin
  Result:=OrdValueToVisualValue(GetDefaultOrdValue);
end;

function TOrdinalPropertyEditor.OrdValueToVisualValue(OrdValue: longint
  ): string;
begin
  Result:=IntToStr(OrdValue);
end;

{ TIntegerPropertyEditor }

function TIntegerPropertyEditor.OrdValueToVisualValue(OrdValue: longint
  ): string;
begin
  with GetTypeData(GetPropType)^ do begin
    {debugln('TIntegerPropertyEditor.OrdValueToVisualValue ',GetName,' ',dbgs(ord(OrdType)),' ',dbgs(OrdValue));
    case OrdType of
      otSByte : debugln('TIntegerPropertyEditor.OrdValueToVisualValue otSByte ',dbgs(ShortInt(OrdValue)));
      otUByte : debugln('TIntegerPropertyEditor.OrdValueToVisualValue otUByte ',dbgs(Byte(OrdValue)));
      otSWord : debugln('TIntegerPropertyEditor.OrdValueToVisualValue otSWord ',dbgs(SmallInt(OrdValue)));
      otUWord : debugln('TIntegerPropertyEditor.OrdValueToVisualValue otUWord ',dbgs(Word(OrdValue)));
      otULong : debugln('TIntegerPropertyEditor.OrdValueToVisualValue otULong ',dbgs(Cardinal(OrdValue)));
      else debugln('TIntegerPropertyEditor.OrdValueToVisualValue ??? ',dbgs(OrdValue));
    end;}

    case OrdType of
      otSByte : Result:= IntToStr(ShortInt(OrdValue));
      otUByte : Result:= IntToStr(Byte(OrdValue));
      otSWord : Result:= IntToStr(Integer(SmallInt(OrdValue)));// double conversion needed due to compiler bug 3534
      otUWord : Result:= IntToStr(Word(OrdValue));
      otULong : Result:= IntToStr(Cardinal(OrdValue));
      else Result := IntToStr(OrdValue);
    end;
    //debugln('TIntegerPropertyEditor.OrdValueToVisualValue ',Result);
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
  SetOrdValue(integer(L));
end;

{ TCharPropertyEditor }

function TCharPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
var
  Ch: Char;
begin
  Ch := Chr(OrdValue);
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
  if GetDefaultOrdValue <> NoDefaultValue then
    Result := Result + [paHasDefaultValue];
end;

function TEnumPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
var
  L: Longint;
  TypeData: PTypeData;
begin
  L := OrdValue;
  TypeData := GetTypeData(GetPropType);
  with TypeData^ do
    if (L < MinValue) or (L > MaxValue) then L := MaxValue;
  Result := GetEnumName(GetPropType, L);
end;

procedure TEnumPropertyEditor.GetValues(Proc: TGetStrProc);
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

function TBoolPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
begin
  if OrdValue = 0 then
    Result := 'False'
  else
    Result := 'True';
end;

procedure TBoolPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('False');
  Proc('True');
end;

procedure TBoolPropertyEditor.SetValue(const NewValue: ansistring);
var
  I: Integer;
begin
  if (CompareText(NewValue, 'False') = 0) or (CompareText(NewValue, 'F') = 0) then
    I := 0
  else 
  if (CompareText(NewValue, 'True') = 0) or (CompareText(NewValue, 'T') = 0) then
    I := 1
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


{ TQWordPropertyEditor }

function TQWordPropertyEditor.GetValue: ansistring;
begin
  Result := IntToStr(QWord(GetInt64Value));
end;

procedure TQWordPropertyEditor.SetValue(const NewValue: ansistring);
begin
  SetInt64Value(Int64(StrToQWord(NewValue)));
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
  Precisions: array[TFloatType] of Integer = (7, 15, 19, 19, 19);
begin
  Result := FloatToStrF(GetFloatValue, ffGeneral,
    Precisions[GetTypeData(GetPropType)^.FloatType], 0);
end;

procedure TFloatPropertyEditor.SetValue(const NewValue: ansistring);
begin
  //writeln('TFloatPropertyEditor.SetValue A ',NewValue,'  ',StrToFloat(NewValue));
  SetFloatValue(StrToFloat(NewValue));
  //writeln('TFloatPropertyEditor.SetValue B ',GetValue);
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
  if GetPropType^.Kind = tkSString then
    Result := GetTypeData(GetPropType)^.MaxLength
  else
    Result := $0FFF;
end;

function TStringPropertyEditor.GetValue: ansistring;
begin
  Result := GetStrValue;
end;

procedure TStringPropertyEditor.SetValue(const NewValue: ansistring);
begin
  SetStrValue(NewValue);
end;


{ TWideStringPropertyEditor }

function TWideStringPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  V: widestring;
begin
  Result := False;
  if PropCount > 1 then begin
    V := GetWideStrValue;
    for I := 1 to PropCount - 1 do
      if GetWideStrValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TWideStringPropertyEditor.GetValue: ansistring;
begin
  Result:=UTF8Encode(GetWideStrValue);
end;

procedure TWideStringPropertyEditor.SetValue(const NewValue: ansistring);
begin
  SetWideStrValue(UTF8Decode(NewValue));
end;

{ TNestedPropertyEditor }

constructor TNestedPropertyEditor.Create(Parent: TPropertyEditor);
begin
  FParentEditor:=Parent;
  FPropertyHook:=Parent.PropertyHook;
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
  if GetDefaultOrdValue <> NoDefaultValue then
    Result := Result + [paHasDefaultValue];
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

procedure TSetElementPropertyEditor.GetValues(Proc: TGetStrProc);
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
    Include(S, FElement)
  else
    Exclude(S, FElement);
  SetOrdValue(Integer(S));
end;

function TSetElementPropertyEditor.IsNotDefaultValue: boolean; 
var
  S1, S2: TIntegerSet;
begin
  Result := (paHasDefaultValue in GetAttributes);
  if Result then
  begin
    Integer(S1) := GetOrdValue;
    Integer(S2) := GetDefaultOrdValue;
    Result := (FElement in S1) <> (FElement in S2);
  end;
end;

{ TSetPropertyEditor }

function TSetPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly, paRevertable];
  if GetDefaultOrdValue <> NoDefaultValue then
    Result := Result + [paHasDefaultValue];
end;

function TSetPropertyEditor.GetEditLimit: Integer;
begin
  Result := 0;
end;

procedure TSetPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType)^ do
    for I := MinValue to MaxValue do
      Proc(TSetElementPropertyEditor.Create(Self, I));
end;

function TSetPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Integer(S) := OrdValue;
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

procedure TListElementPropertyEditor.GetValues(Proc: TGetStrProc);
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

function TListPropertyEditor.GetElement(Index: integer): TPersistent;
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
    Result:=TPersistent(SavedElements[Index])
  else
    Result:=ReadElement(Index);
end;

function TListPropertyEditor.GetElement(Element: TListElementPropertyEditor
  ): TPersistent;
begin
  Result:=GetElement(Element.TheIndex);
end;

function TListPropertyEditor.GetElementPropEditor(Index: integer
  ): TListElementPropertyEditor;
// called by GetProperties to get the element property editors
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
    if TPersistent(SavedElements[i])<>ReadElement(i) then exit;
  Result:=false;
  FSubPropertiesChanged:=false;
end;

function TListPropertyEditor.ReadElementCount: integer;
var
  TheList: TObject;
begin
  TheList := GetObjectValue;
  if (TheList <> nil) and (TheList is TList) then
    Result := TList(TheList).Count
  else
    Result := 0;
end;

function TListPropertyEditor.ReadElement(Index: integer): TPersistent;
var
  obj: TObject;
begin
  obj := TObject(TList(GetObjectValue).Items[Index]);
  if obj is TPersistent then
    Result:=TPersistent(obj)
  else
    raise EInvalidOperation.CreateFmt('List element %d is not a TPersistent decendant', [Index]);
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
    DebugLn('TListPropertyEditor.EndSaveElement ERROR: FSaveElementLock=',
      IntToStr(FSaveElementLock));
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
  Result:='';
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
  Element: TListElementPropertyEditor; Proc: TGetStrProc);
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
  APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
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
  Result:= [paSubProperties, paDynamicSubProps, paReadOnly, paDialog];
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


const
  CollectionForm: TCollectionPropertyEditorForm = nil;

//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

function TCollectionPropertyEditor.ReadElement(Index: integer): TPersistent;
var
  Collection: TCollection;
begin
  Collection:=TCollection(GetObjectValue);
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
begin
  GetPersistentProperties(GetElement(Element),tkProperties,PropertyHook,Proc,nil);
end;

function TCollectionPropertyEditor.GetElementValue(
  Element: TListElementPropertyEditor): ansistring;
begin
  Result:=IntToStr(TCollectionItem(GetElement(Element)).ID);
end;

procedure TCollectionPropertyEditor.GetElementValues(
  Element: TListElementPropertyEditor; Proc: TGetStrProc);
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
  Collection: TObject;
begin
  Collection := GetObjectValue;
  if (Collection <> nil) and (Collection is TCollection) then
    Result := TCollection(Collection).Count
  else
    Result := 0;
end;

function TCollectionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

class function TCollectionPropertyEditor.ShowCollectionEditor(
  ACollection: TCollection; 
  OwnerPersistent: TPersistent; 
  const PropName: String): TCustomForm;
begin
  if CollectionForm = nil then
    CollectionForm := TCollectionPropertyEditorForm.Create(Application);
  CollectionForm.SetCollection(ACollection, OwnerPersistent, PropName);
  CollectionForm.EnsureVisible;
  Result:=CollectionForm;
end;

procedure TCollectionPropertyEditor.Edit;
var
  TheCollection: TCollection;
begin
  TheCollection := TCollection(GetObjectValue);
  if TheCollection = nil then
    raise Exception.Create('Collection=nil');
  ShowCollectionEditor(TheCollection, GetComponent(0), GetName);
end;

{ TClassPropertyEditor }

constructor TClassPropertyEditor.Create(Hook: TPropertyEditorHook;
  APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
  FSubPropsTypeFilter := tkAny;
end;

function TClassPropertyEditor.EditorFilter(
  const AEditor: TPropertyEditor): Boolean;
begin
  Result := IsInteresting(AEditor, SubPropsTypeFilter);
end;

function TClassPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

procedure TClassPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  selection: TPersistentSelectionList;
begin
  selection := GetSelections;
  if selection = nil then exit;
  GetPersistentProperties(
    selection, SubPropsTypeFilter + [tkClass], PropertyHook, Proc, @EditorFilter);
  selection.Free;
end;

function TClassPropertyEditor.GetSelections: TPersistentSelectionList;
var
  i: Integer;
  subItem: TPersistent;
begin
  Result := TPersistentSelectionList.Create;
  try
    for i := 0 to PropCount - 1 do begin
      subItem := TPersistent(GetObjectValueAt(i));
      if subItem <> nil then
        Result.Add(subItem);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TClassPropertyEditor.GetValue: ansistring;
begin
  Result:='(' + GetPropType^.Name + ')';
end;

procedure TClassPropertyEditor.SetSubPropsTypeFilter(const AValue: TTypeKinds);
begin
  if FSubPropsTypeFilter = AValue then exit;
  FSubPropsTypeFilter := AValue;
end;

{ TMethodPropertyEditor }

function TMethodPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  CurFirstValue, AnotherValue: TMethod;
begin
  Result := False;
  if PropCount > 1 then begin
    CurFirstValue := GetMethodValue;
    for I := 1 to PropCount - 1 do begin
      AnotherValue := GetMethodValueAt(I);
      // Note: compare Code and Data
      if (AnotherValue.Code <> CurFirstValue.Code)
      or (AnotherValue.Data <> CurFirstValue.Data) then
        Exit;
    end;
  end;
  Result := True;
end;

procedure TMethodPropertyEditor.Edit;
{ If the method does not exist in current lookuproot: create it
  Then jump to the source.
  
  For inherited methods this means: A new method is created and a call of
  the ancestor value is added. Then the IDE jumps to the new method body.
}
var
  NewMethodName: shortstring;
begin
  NewMethodName:=GetValue;
  //DebugLn('### TMethodPropertyEditor.Edit A OldValue=',NewMethodName);
  if (not IsValidIdent(NewMethodName))
  or PropertyHook.MethodFromAncestor(GetMethodValue) then begin
    // the current method is from the ancestor
    // -> add an override with the default name
    NewMethodName := GetFormMethodName;
    //DebugLn('### TMethodPropertyEditor.Edit B FormMethodName=',NewMethodName);
    if not IsValidIdent(NewMethodName) then
      raise EPropertyError.Create('Method name "'+NewMethodName+'" must be an identifier');
    SetValue(NewMethodName); // this will jump to the method
    PropertyHook.RefreshPropertyValues;
  end else
    PropertyHook.ShowMethod(NewMethodName);
end;

procedure TMethodPropertyEditor.ShowValue;
var
  CurMethodName: String;
begin
  CurMethodName:=GetValue;
  PropertyHook.ShowMethod(CurMethodName);
end;

function TMethodPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paSortList, paRevertable];
end;

function TMethodPropertyEditor.GetEditLimit: Integer;
begin
  Result := 2*MaxIdentLength+1; // clasname.methodname
end;

function TMethodPropertyEditor.GetFormMethodName: shortstring;
// returns the default name for a new method
var I: Integer;
  Root: TPersistent;
begin
  Result:='';
  if PropertyHook.LookupRoot=nil then exit;
  if GetComponent(0) = PropertyHook.LookupRoot then begin
    Root:=PropertyHook.LookupRoot;
    if Root is TCustomForm then
      Result := 'Form'
    else 
    if Root is TDataModule then
      Result := 'DataModule'
    else
    if Root is TFrame then
      Result := 'Frame'
    else 
    begin
      Result := ClassNameToComponentName(PropertyHook.GetRootClassName);
    end;
  end else begin
    Result := PropertyHook.GetObjectName(GetComponent(0));
    for I := Length(Result) downto 1 do
      if
        not (
          (Result[I] in ['a'..'z', 'A'..'Z', '_']) or
          (I > 1) and (Result[I] in ['0'..'9']))
      then
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

class function TMethodPropertyEditor.GetDefaultMethodName(Root,
  Component: TComponent; const RootClassName, ComponentName,
  PropName: shortstring): shortstring;
// returns the default name for a new method
var I: Integer;
  Postfix: String;
begin
  Result:='';
  if Root=nil then exit;
  if Component = Root then begin
    if Root is TCustomForm then
      Result := 'Form'
    else 
    if Root is TDataModule then
      Result := 'DataModule'
    else
    if Root is TFrame then
      Result := 'Frame'
    else 
    begin
      Result := ClassNameToComponentName(RootClassName);
    end;
  end else begin
    Result := ComponentName;
    for I := Length(Result) downto 1 do
      if Result[I] in ['.','[',']'] then
        System.Delete(Result, I, 1);
  end;
  if Result = '' then begin
    DebugLn(['TMethodPropertyEditor.GetDefaultMethodName can not create name - this should never happen']);
    exit;
  end;
  Postfix := PropName;
  if (Length(Postfix) >= 2)
  and (Postfix[1] in ['O','o']) and (Postfix[2] in ['N','n'])
  then
    System.Delete(Postfix,1,2);
  Result:=Result+Postfix;
end;

function TMethodPropertyEditor.GetValue: ansistring;
begin
  Result:=PropertyHook.GetMethodName(GetMethodValue,GetComponent(0));
  //debugln(['TMethodPropertyEditor.GetValue Name=',GetName,' Result=',Result,' Data=',dbgs(GetMethodValue.Data)]);
end;

procedure TMethodPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  //DebugLn('### TMethodPropertyEditor.GetValues');
  Proc(oisNone);
  PropertyHook.GetCompatibleMethods(GetInstProp, Proc);
end;

procedure TMethodPropertyEditor.SetValue(const NewValue: ansistring);
var
  CreateNewMethod: Boolean;
  CurValue: string;
  NewMethodExists, NewMethodIsCompatible, NewMethodIsPublished,
  NewIdentIsMethod: boolean;
  IsNil: Boolean;
  NewMethod: TMethod;
begin
  CurValue:=GetValue;
  if CurValue=NewValue then exit;
  //DebugLn('### TMethodPropertyEditor.SetValue A OldValue="',CurValue,'" NewValue=',NewValue);
  IsNil:=(NewValue='') or (NewValue=oisNone);
  
  if (not IsNil) and (not IsValidIdent(NewValue))
  then begin
    MessageDlg(oisIncompatibleIdentifier,
      Format(oisIsNotAValidMethodName,['"',NewValue,'"']), mtError,
      [mbCancel, mbIgnore], 0);
    exit;
  end;
  
  NewMethodExists:=(not IsNil)
      and PropertyHook.CompatibleMethodExists(NewValue,GetInstProp,
                   NewMethodIsCompatible,NewMethodIsPublished,NewIdentIsMethod);
  //DebugLn('### TMethodPropertyEditor.SetValue B NewMethodExists=',NewMethodExists,' NewMethodIsCompatible=',NewMethodIsCompatible,' ',NewMethodIsPublished,' ',NewIdentIsMethod);
  if NewMethodExists then begin
    if not NewIdentIsMethod then begin
      if MessageDlg(oisIncompatibleIdentifier,
        Format(oisTheIdentifierIsNotAMethodPressCancelToUndoPressIgn, ['"',
          NewValue, '"', #13, #13]), mtWarning, [mbCancel, mbIgnore], 0)<>
          mrIgnore
      then
        exit;
    end;
    if not NewMethodIsPublished then begin
      if MessageDlg(oisIncompatibleMethod,
        Format(oisTheMethodIsNotPublishedPressCancelToUndoPressIgnor, ['"',
          NewValue, '"', #13, #13]), mtWarning, [mbCancel, mbIgnore], 0)<>
          mrIgnore
      then
        exit;
    end;
    if not NewMethodIsCompatible then begin
      if MessageDlg(oisIncompatibleMethod,
        Format(oisTheMethodIsIncompatibleToThisEventPressCancelToUnd, ['"',
          NewValue, '"', GetName, #13, #13]), mtWarning, [mbCancel, mbIgnore], 0
          )<>mrIgnore
      then
        exit;
    end;
  end;
  //DebugLn('### TMethodPropertyEditor.SetValue C');
  if IsNil then begin
    NewMethod.Data:=nil;
    NewMethod.Code:=nil;
    SetMethodValue(NewMethod);
  end else
  if IsValidIdent(CurValue)
  and (not NewMethodExists)
  and (not PropertyHook.MethodFromAncestor(GetMethodValue)) then begin
    // rename the method
    // Note:
    //   All other not selected properties that use this method, contain just
    //   the TMethod record. So, changing the name in the jitform will change
    //   all other event names in all other components automatically.
    //writeln('### TMethodPropertyEditor.SetValue D');
    PropertyHook.RenameMethod(CurValue, NewValue)
  end else
  begin
    //DebugLn('### TMethodPropertyEditor.SetValue E');
    CreateNewMethod := not NewMethodExists;
    SetMethodValue(
       PropertyHook.CreateMethod(NewValue,GetPropType,
                                 GetComponent(0),GetPropertyPath(0)));
    //DebugLn('### TMethodPropertyEditor.SetValue F NewValue=',GetValue);
    if CreateNewMethod then begin
      //DebugLn('### TMethodPropertyEditor.SetValue G');
      PropertyHook.ShowMethod(NewValue);
    end;
  end;
  //DebugLn('### TMethodPropertyEditor.SetValue END  NewValue=',GetValue);
end;

{ TPersistentPropertyEditor }

function TPersistentPropertyEditor.FilterFunc(
  const ATestEditor: TPropertyEditor): Boolean;
begin
  Result := not (paNotNestable in ATestEditor.GetAttributes);
end;

function TPersistentPropertyEditor.GetPersistentReference: TPersistent;
begin
  Result := TPersistent(GetObjectValue);
end;

function TPersistentPropertyEditor.GetSelections: TPersistentSelectionList;
begin
  if (GetPersistentReference <> nil) and AllEqual then
    Result := inherited GetSelections
  else
    Result := nil;
end;

function TPersistentPropertyEditor.CheckNewValue(APersistent: TPersistent): boolean;
begin
  Result:=true;
end;

function TPersistentPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  LInstance: TPersistent;
begin
  Result := False;
  LInstance := TPersistent(GetObjectValue);
  if PropCount > 1 then
    for I := 1 to PropCount - 1 do
      if TPersistent(GetObjectValueAt(I)) <> LInstance then
        Exit;
  Result := LInstance<>nil;
end;

procedure TPersistentPropertyEditor.Edit;
var
  Temp: TPersistent;
  Designer: TIDesigner;
  AComponent: TComponent;
begin
  Temp := GetPersistentReference;
  if Temp is TComponent then begin
    AComponent:=TComponent(Temp);
    Designer:=FindRootDesigner(AComponent);
    if (Designer<>nil)
    and (Designer.GetShiftState * [ssCtrl, ssLeft] = [ssCtrl, ssLeft]) then
      Designer.SelectOnlyThisComponent(AComponent)
    else
      inherited Edit;
  end else
    inherited Edit;
end;

function TPersistentPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect];
  if Assigned(GetPropInfo^.SetProc) then
    Result := Result + [paValueList, paSortList, paRevertable, paVolatileSubProperties]
  else
    Result := Result + [paReadOnly];
  if GReferenceExpandable and (GetPersistentReference <> nil) and AllEqual then
    Result := Result + [paSubProperties];
end;

function TPersistentPropertyEditor.GetEditLimit: Integer;
begin
  Result := MaxIdentLength;
end;

function TPersistentPropertyEditor.GetValue: AnsiString;
var
  Component: TComponent;
  APersistent: TPersistent;
begin
  Result := '';
  APersistent := GetPersistentReference;
  if APersistent is TComponent then begin
    Component := TComponent(APersistent);
    if Assigned(PropertyHook) then
      Result := PropertyHook.GetComponentName(Component)
    else begin
      if Assigned(Component) then
        Result := Component.Name;
    end;
  end else if APersistent <> nil then
    Result := inherited GetValue;
end;

procedure TPersistentPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc(oisNone);
  if Assigned(PropertyHook) then
    PropertyHook.GetComponentNames(GetTypeData(GetPropType), Proc);
end;

procedure TPersistentPropertyEditor.SetValue(const NewValue: ansistring);
var Persistent: TPersistent;
begin
  if NewValue=GetValue then exit;
  if (NewValue = '') or (NewValue=oisNone) then
    Persistent := nil
  else begin
    if Assigned(PropertyHook) then begin
      Persistent := PropertyHook.GetComponent(NewValue);
      if not (Persistent is GetTypeData(GetPropType)^.ClassType) then begin
        raise EPropertyError.Create(oisInvalidPropertyValue);
      end;
    end;
  end;
  if GetPersistentReference=Persistent then exit;
  if not CheckNewValue(Persistent) then exit;
  SetPtrValue(Persistent);
  if Assigned(PropertyHook) then begin
    PropertyHook.ObjectReferenceChanged(Self,Persistent);
  end;
end;

{ TComponentPropertyEditor }

function TComponentPropertyEditor.GetComponentReference: TComponent;
begin
  Result := TComponent(GetObjectValue);
end;

function TComponentPropertyEditor.AllEqual: Boolean;
var
  AComponent: TComponent;
begin
  Result:=false;
  if not (inherited AllEqual) then exit;
  AComponent:=GetComponentReference;
  if AComponent=nil then exit;
  Result:=csDesigning in AComponent.ComponentState;
end;


{ TInterfacePropertyEditor }

function TInterfacePropertyEditor.AllEqual: Boolean;
begin
  Result := False;
end;

function TInterfacePropertyEditor.GetComponent(
  const AInterface: Pointer {IInterface}): TComponent;
begin
  Result := nil;
end;

function TInterfacePropertyEditor.GetComponentReference: TComponent;
begin
  Result := nil; //GetComponent(GetIntfValue);
end;

function TInterfacePropertyEditor.GetSelections: TPersistentSelectionList{IDesignerSelections};
begin
  Result := nil;
end;

procedure TInterfacePropertyEditor.ReceiveComponentNames(const S: string);
begin

end;

procedure TInterfacePropertyEditor.GetValues(Proc: TGetStrProc);
begin

end;

procedure TInterfacePropertyEditor.SetValue(const Value: string);
begin

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
    raise Exception.Create(Format(oisComponentNameIsNotAValidIdentifier, ['"',
      NewValue, '"']));
  inherited SetValue(NewValue);
  PropertyHook.ComponentRenamed(TComponent(GetComponent(0)));
end;

{ TDatePropertyEditor }

function TDatePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;

function TDatePropertyEditor.GetValue: string;
var
  DT: TDateTime;
begin
  DT := TDateTime(GetFloatValue);
  if DT = 0.0 then
    Result := ''
  else
    Result := DateToStr(DT);
end;

procedure TDatePropertyEditor.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := StrToDate(Value);
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
  DT := TDateTime(GetFloatValue);
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
  DT := TDateTime(GetFloatValue);
  if DT = 0.0 then Result := '' else
  Result := DateTimeToStr(DT);
end;

procedure TDateTimePropertyEditor.SetValue(const Value: string);
var
  DT: TDateTime;
  ok: Boolean;
begin
  if Value = '' then DT := 0.0
  else begin
    ok:=false;
    // first try date+time
    try
      DT := StrToDateTime(Value);
      ok:=true;
    except
    end;
    // then try date without time
    if not ok then
      try
        DT := StrToDate(Value);
        ok:=true;
      except
      end;
    // then try time without date
    if not ok then
      try
        DT := StrToTime(Value);
        ok:=true;
      except
      end;
    // if all fails then raise exception
    if not ok then
      StrToDateTime(Value);
  end;
  SetFloatValue(DT);
end;

{ TVariantPropertyEditor }

function TVariantPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties];
end;

procedure TVariantPropertyEditor.GetProperties(Proc:TGetPropEditProc);
begin

end;

function TVariantPropertyEditor.GetValue: string;
begin
  Result:='';
end;

procedure TVariantPropertyEditor.SetValue(const Value: string);
begin
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
    'mrYesToAll',
    'mrClose');

function TModalResultPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
  if GetDefaultOrdValue <> NoDefaultValue then
    Result := Result + [paHasDefaultValue];
end;

function TModalResultPropertyEditor.OrdValueToVisualValue(OrdValue: longint
  ): string;
var
  CurValue: Longint;
begin
  CurValue := OrdValue;
  case CurValue of
    Low(ModalResults)..High(ModalResults):
      Result := ModalResults[CurValue];
  else
    Result := IntToStr(CurValue);
  end;
end;

procedure TModalResultPropertyEditor.GetValues(Proc: TGetStrProc);
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
  ShortCuts: array[0..134] of TShortCut = (
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
    Byte('A') or scMeta,
    Byte('B') or scMeta,
    Byte('C') or scMeta,
    Byte('D') or scMeta,
    Byte('E') or scMeta,
    Byte('F') or scMeta,
    Byte('G') or scMeta,
    Byte('H') or scMeta,
    Byte('I') or scMeta,
    Byte('J') or scMeta,
    Byte('K') or scMeta,
    Byte('L') or scMeta,
    Byte('M') or scMeta,
    Byte('N') or scMeta,
    Byte('O') or scMeta,
    Byte('P') or scMeta,
    Byte('Q') or scMeta,
    Byte('R') or scMeta,
    Byte('S') or scMeta,
    Byte('T') or scMeta,
    Byte('U') or scMeta,
    Byte('V') or scMeta,
    Byte('W') or scMeta,
    Byte('X') or scMeta,
    Byte('Y') or scMeta,
    Byte('Z') or scMeta,
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

procedure TShortCutPropertyEditor.Edit;
var
  Box: TShortCutGrabBox;
  OldValue, NewValue: TShortCut;
  OldKey: Word;
  OldShift: TShiftState;
  Dlg: TForm;
  BtnPanel: TButtonPanel;
begin
  try
    Dlg:=TForm.Create(nil);
    Dlg.BorderStyle:=bsToolWindow;
    Dlg.Caption:=oisSelectShortCut;
    Dlg.Position:=poScreenCenter;
    Dlg.Constraints.MinWidth:=350;
    Dlg.Constraints.MinHeight:=30;
    Dlg.Width:=350;
    Dlg.Height:=120;

    Box:=TShortCutGrabBox.Create(Dlg);
    Box.BorderSpacing.Around:=6;
    Box.Parent:=Dlg;
    Box.Align:=alClient;
    OldValue := TShortCut(GetOrdValue);
    ShortCutToKey(OldValue,OldKey,OldShift);
    Box.ShiftState:=OldShift;
    Box.Key:=OldKey;

    BtnPanel:=TButtonPanel.Create(Dlg);
    BtnPanel.Parent:=Dlg;
    BtnPanel.Align:=alBottom;
    BtnPanel.ShowButtons:=[pbOk,pbCancel];

    Dlg.AutoSize:=true;
    if Dlg.ShowModal=mrOk then begin
      NewValue:=Menus.ShortCut(Box.Key,Box.ShiftState);
      if OldValue<>NewValue then
        SetOrdValue(NewValue);
    end;
  finally
    Dlg.Free;
  end;
end;

function TShortCutPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable, paDialog];
  if GetDefaultOrdValue <> NoDefaultValue then
    Result := Result + [paHasDefaultValue];
end;

function TShortCutPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
var
  CurValue: TShortCut;
begin
  CurValue := TShortCut(OrdValue);
  if CurValue = scNone then
    Result := oisNone
  else
    Result := ShortCutToText(CurValue);
end;

procedure TShortCutPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc(oisNone);
  for I := 1 to High(ShortCuts) do Proc(ShortCutToText(ShortCuts[I]));
end;

procedure TShortCutPropertyEditor.SetValue(const Value: string);
var
  NewValue: TShortCut;
begin
  NewValue := 0;
  if (Value <> '') and (AnsiCompareText(Value, oisNone) <> 0) then
  begin
    NewValue := TextToShortCut(Value);
    if NewValue = 0 then
      raise EPropertyError.Create(oisInvalidPropertyValue);
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

procedure TStringsPropertyEditor.Edit;
var
  TheDialog: TStringsPropEditorDlg;
begin
  TheDialog := CreateDlg(TStrings(GetObjectValue));
  try
    if (TheDialog.ShowModal = mrOK) then 
      SetPtrValue(TheDialog.Memo.Lines);
  finally
    TheDialog.Free;
  end;
end;

function TStringsPropertyEditor.CreateDlg(s: TStrings): TStringsPropEditorDlg;
begin
  Result := TStringsPropEditorDlg.Create(Application);
  Result.Editor := Self;
  Result.Memo.Text := s.Text;
  Result.MemoChange(nil); // force call OnChange event
end;

function TStringsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paRevertable, paReadOnly];
end;

{ TStringMultilinePropertyEditor }

procedure TStringMultilinePropertyEditor.Edit;
var
  TheDialog : TStringsPropEditorDlg;
  AString : string;
begin
  AString := GetStrValue;
  TheDialog := TStringsPropEditorDlg.Create(nil);
  try
    TheDialog.Editor := Self;
    TheDialog.Memo.Text := AString;
    TheDialog.MemoChange(nil);
    if (TheDialog.ShowModal = mrOK) then
    begin
      AString := TheDialog.Memo.Text;
      //erase the last lineending if any
      if Copy(AString, length(AString) - length(LineEnding) + 1, length(LineEnding)) = LineEnding then
        Delete(AString, length(AString) - length(LineEnding) + 1, length(LineEnding));
      SetStrValue(AString);
    end;
  finally
    TheDialog.Free;
  end;
end;

function TStringMultilinePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paRevertable, paAutoUpdate];
end;

{ TCursorPropertyEditor }

function TCursorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSortList, paValueList, paRevertable];
  if GetDefaultOrdValue <> NoDefaultValue then
    Result := Result + [paHasDefaultValue];
end;

function TCursorPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
begin
  Result := CursorToString(TCursor(OrdValue));
end;

procedure TCursorPropertyEditor.GetValues(Proc: TGetStrProc);
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

{ TFileNamePropertyEditor }

function TFileNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog,paRevertable];
end;

procedure TFileNamePropertyEditor.Edit;
begin
  With CreateFileDialog do
    Try
      Filter:=GetFilter;
      Options:=GetDialogOptions;
      FileName:=GetStrValue;
      InitialDir:=GetInitialDirectory;
      Title:=GetDialogTitle;
      If Execute then
        SetFilename(Filename);
    Finally
      Free;
    end;
end;

function TFileNamePropertyEditor.GetFilter: String;
begin
  Result:=oisAllFiles+' ('+GetAllFilesMask+')|'+GetAllFilesMask;
end;

function TFileNamePropertyEditor.GetDialogOptions: TOpenOptions;
begin
  Result:=DefaultOpenDialogOptions;
end;

function TFileNamePropertyEditor.GetDialogTitle: string;
begin
  Result:=oisSelectAFile;
end;

function TFileNamePropertyEditor.GetInitialDirectory: string;
begin
  Result:='';
end;

procedure TFileNamePropertyEditor.SetFilename(const Filename: string);
begin
  SetStrValue(Filename);
end;

function TFileNamePropertyEditor.CreateFileDialog: TOpenDialog;
begin
  Result:=TOpenDialog.Create(nil);
end;

{ TDirectoryPropertyEditor }

function TDirectoryPropertyEditor.CreateFileDialog: TOpenDialog;
begin
  Result:=TSelectDirectoryDialog.Create(nil);
  Result.Options:=Result.Options+[ofFileMustExist];
end;

{ TURLPropertyEditor }

procedure TURLPropertyEditor.SetFilename(const Filename: string);

  function FilenameToURL(const Filename: string): string;
  var
    i: Integer;
  begin
    Result:=Filename;
    {$warnings off}
    if PathDelim<>'/' then
      for i:=1 to length(Result) do
        if Result[i]=PathDelim then
          Result[i]:='/';
    {$warnings on}
    if Result<>'' then
      Result:='file://'+Result;
  end;

begin
  inherited SetFilename(FilenameToURL(Filename));
end;

{ TURLDirectoryPropertyEditor }

function TURLDirectoryPropertyEditor.CreateFileDialog: TOpenDialog;
begin
  Result:=TSelectDirectoryDialog.Create(nil);
  Result.Options:=Result.Options+[ofFileMustExist];
end;

type
  TFileFilterPropertyEditorForm = class(TForm)
  private
    StringGrid1: TStringGrid;
    function GetFilter: string;
    procedure SetFilter(const AValue: string);
  public
    constructor Create(AOwner : TComponent); Override;
    property Filter:string read GetFilter write SetFilter;
  end;

{ TFileFilterPropertyEditorForm }

function TFileFilterPropertyEditorForm.GetFilter: string;
var
  i:integer;
begin
  Result:='';
  for i:=1 to StringGrid1.RowCount-1 do
  begin
    if StringGrid1.Cells[1,i]<>'' then
    begin
      if Result<>'' then Result:=Result+'|';
      if StringGrid1.Cells[0,i]<>'' then
        Result:=Result+StringGrid1.Cells[0,i]+'|'+StringGrid1.Cells[1,i]
      else
        Result:=Result+StringGrid1.Cells[1,i]+'|'+StringGrid1.Cells[1,i];
    end
    else
      break;
  end;
end;

procedure TFileFilterPropertyEditorForm.SetFilter(const AValue: string);
var
  S:string;
  C1, i:integer;
begin
  S:=AValue;
  I:=1;
  while (S<>'') do
  begin
    C1:=Pos('|',S);
    if C1>0 then
    begin
      StringGrid1.Cells[0,i]:=Copy(S, 1, C1-1);
      Delete(S, 1, C1);
      C1:=Pos('|',S);
      if (C1>0) then
      begin
        StringGrid1.Cells[1,i]:=Copy(S, 1, C1-1);
        Delete(S, 1, C1);
      end
      else
      begin
        StringGrid1.Cells[1,i]:=S;
        S:='';
      end;
    end
    else
    begin
      StringGrid1.Cells[0,i]:=S;
      StringGrid1.Cells[1,i]:=S;
      S:='';
    end;
    inc(i);
  end;
end;

constructor TFileFilterPropertyEditorForm.Create(AOwner: TComponent);
var
  BtnPanel: TPanel;
begin
  inherited CreateNew(AOwner, 1);
  Caption:=peFilterEditor;
  Height:=295;
  Width:=417;
  Position:=poScreenCenter;
  BorderStyle:=bsDialog;
  BtnPanel:=TPanel.Create(Self);
  with BtnPanel do begin
    Name:='BtnPanel';
    Caption:='';
    BevelOuter:=bvNone;
    Align:=alBottom;
    AutoSize:=true;
    Parent:=Self;
  end;
  with TBitBtn.Create(Self) do
  begin
    BorderSpacing.Around:=6;
    Kind := bkOK;
    Align:=alRight;
    Parent:=BtnPanel;
  end;
  with TBitBtn.Create(Self) do
  begin
    BorderSpacing.Around:=6;
    Kind := bkCancel;
    Align:=alRight;
    Parent:=BtnPanel;
  end;
  StringGrid1:=TStringGrid.Create(Self);
  with StringGrid1 do begin
    BorderSpacing.Around:=6;
    ColCount:=2;
    DefaultColWidth:=190;
    Options:=StringGrid1.Options + [goEditing, goAlwaysShowEditor];
    RowCount:= 100;
    FixedCols := 0;
    Align:=alClient;
    Parent:=Self;
    Cells[0, 0]:=peFilterName;
    Cells[1, 0]:=peFilter;
  end;
end;

{ TFileDlgFilterProperty }

function TFileDlgFilterProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=inherited GetAttributes + [paDialog];
end;

procedure TFileDlgFilterProperty.Edit;
begin
  with TFileFilterPropertyEditorForm.Create(Application) do
  try
    Filter:=GetStrProp(GetComponent(0), 'Filter');
    if ShowModal=mrOk then begin
      SetStrProp(GetComponent(0), 'Filter', Filter);
      Modified;
    end;
  finally
    Free;
  end;
end;

{ TSessionPropertiesPropertyEditor }

function TSessionPropertiesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog,paRevertable,paReadOnly];
end;

procedure TSessionPropertiesPropertyEditor.Edit;
begin
  With TSelectPropertiesForm.Create(Application) do
    Try
      PropertyComponent:=GetComponent(0) as TComponent;
      SelectedProperties:=GetStrValue;
      Caption:=Format(oisPropertiesOf, [TComponent(GetComponent(0)).Name]);
      If (ShowModal=mrOK) then
        SetStrValue(SelectedProperties);
    Finally
      Free;
    end;
end;

//==============================================================================


{ TPropertyEditorHook }

function TPropertyEditorHook.CreateMethod(const Name: Shortstring;
  ATypeInfo: PTypeInfo;
  APersistent: TPersistent; const APropertyPath: string): TMethod;
var
  i: Integer;
  Handler: TPropHookCreateMethod;
begin
  Result.Code:=nil;
  Result.Data:=nil;
  if IsValidIdent(Name) and (ATypeInfo<>nil) then begin
    i:=GetHandlerCount(htCreateMethod);
    while GetNextHandlerIndex(htCreateMethod,i) do begin
      Handler:=TPropHookCreateMethod(FHandlers[htCreateMethod][i]);
      Result:=Handler(Name,ATypeInfo,APersistent,APropertyPath);
      if (Result.Data<>nil) or (Result.Code<>nil) then exit;
    end;
  end;
end;

function TPropertyEditorHook.GetMethodName(const Method: TMethod;
  PropOwner: TObject): String;
var
  i: Integer;
begin
  i:=GetHandlerCount(htGetMethodName);
  if GetNextHandlerIndex(htGetMethodName,i) then begin
    Result:=TPropHookGetMethodName(FHandlers[htGetMethodName][i])(Method,PropOwner);
  end else begin
    // search the method name with the given code pointer
    if Assigned(Method.Code) then begin
      if Method.Data<>nil then begin
        Result:=TObject(Method.Data).MethodName(Method.Code);
        if Result='' then
          Result:='<Unpublished>';
      end else
        Result:='<No LookupRoot>';
    end else
      Result:='';
  end;
end;

procedure TPropertyEditorHook.GetMethods(TypeData: PTypeData;
  const Proc: TGetStrProc);
var
  i: Integer;
begin
  i:=GetHandlerCount(htGetMethods);
  while GetNextHandlerIndex(htGetMethods,i) do
    TPropHookGetMethods(FHandlers[htGetMethods][i])(TypeData,Proc);
end;

procedure TPropertyEditorHook.GetCompatibleMethods(InstProp: PInstProp;
  const Proc: TGetStrProc);
var
  i: Integer;
begin
  i:=GetHandlerCount(htGetCompatibleMethods);
  while GetNextHandlerIndex(htGetCompatibleMethods,i) do
    TPropHookGetCompatibleMethods(FHandlers[htGetCompatibleMethods][i])(InstProp,Proc);
end;

function TPropertyEditorHook.MethodExists(const Name: String;
  TypeData: PTypeData;
  var MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean):boolean;
var
  i: Integer;
  Handler: TPropHookMethodExists;
begin
  // check if a published method with given name exists in LookupRoot
  Result:=IsValidIdent(Name) and Assigned(FLookupRoot);
  if not Result then exit;
  i:=GetHandlerCount(htMethodExists);
  if i>=0 then begin
    while GetNextHandlerIndex(htMethodExists,i) do begin
      Handler:=TPropHookMethodExists(FHandlers[htMethodExists][i]);
      Result:=Handler(Name,TypeData,
                            MethodIsCompatible,MethodIsPublished,IdentIsMethod);
    end;
  end else begin
    Result:=(LookupRoot.MethodAddress(Name)<>nil);
    MethodIsCompatible:=Result;
    MethodIsPublished:=Result;
    IdentIsMethod:=Result;
  end;
end;

function TPropertyEditorHook.CompatibleMethodExists(const Name: String;
  InstProp: PInstProp; var MethodIsCompatible, MethodIsPublished,
  IdentIsMethod: boolean): boolean;
var
  i: Integer;
  Handler: TPropHookCompatibleMethodExists;
begin
  // check if a published method with given name exists in LookupRoot
  Result:=IsValidIdent(Name) and Assigned(FLookupRoot);
  if not Result then exit;
  i:=GetHandlerCount(htCompatibleMethodExists);
  if i>=0 then begin
    while GetNextHandlerIndex(htCompatibleMethodExists,i) do begin
      Handler:=TPropHookCompatibleMethodExists(FHandlers[htCompatibleMethodExists][i]);
      Result:=Handler(Name,InstProp,
                            MethodIsCompatible,MethodIsPublished,IdentIsMethod);
    end;
  end else begin
    Result:=(LookupRoot.MethodAddress(Name)<>nil);
    MethodIsCompatible:=Result;
    MethodIsPublished:=Result;
    IdentIsMethod:=Result;
  end;
end;

procedure TPropertyEditorHook.RenameMethod(const CurName, NewName: String);
// rename published method in LookupRoot object and source
var
  i: Integer;
begin
  i:=GetHandlerCount(htRenameMethod);
  while GetNextHandlerIndex(htRenameMethod,i) do
    TPropHookRenameMethod(FHandlers[htRenameMethod][i])(CurName,NewName);
end;

procedure TPropertyEditorHook.ShowMethod(const Name:String);
// jump cursor to published method body
var
  i: Integer;
begin
  i:=GetHandlerCount(htShowMethod);
  while GetNextHandlerIndex(htShowMethod,i) do
    TPropHookShowMethod(FHandlers[htShowMethod][i])(Name);
end;

function TPropertyEditorHook.MethodFromAncestor(const Method:TMethod):boolean;
var AncestorClass: TClass;
  i: Integer;
  Handler: TPropHookMethodFromAncestor;
begin
  // check if given Method is not in LookupRoot source,
  // but in one of its ancestors
  i:=GetHandlerCount(htMethodFromAncestor);
  if GetNextHandlerIndex(htMethodFromAncestor,i) then begin
    Handler:=TPropHookMethodFromAncestor(FHandlers[htMethodFromAncestor][i]);
    Result:=Handler(Method);
  end else begin
    if (Method.Data<>nil) and (Method.Code<>nil) then begin
      AncestorClass:=TObject(Method.Data).ClassParent;
      Result:=(AncestorClass<>nil)
              and (AncestorClass.MethodName(Method.Code)<>'');
    end else
      Result:=false;
  end;
end;

procedure TPropertyEditorHook.ChainCall(const AMethodName, InstanceName,
  InstanceMethod:Shortstring;  TypeData:PTypeData);
var
  i: Integer;
  Handler: TPropHookChainCall;
begin
  i:=GetHandlerCount(htChainCall);
  while GetNextHandlerIndex(htChainCall,i) do begin
    Handler:=TPropHookChainCall(FHandlers[htChainCall][i]);
    Handler(AMethodName,InstanceName,InstanceMethod,TypeData);
  end;
end;

function TPropertyEditorHook.GetComponent(const ComponentPath: string
  ): TComponent;
var
  i: Integer;
begin
  Result := nil;
  if not Assigned(LookupRoot) then
    Exit;
  i := GetHandlerCount(htGetComponent);
  while GetNextHandlerIndex(htGetComponent, i) and (Result = nil) do
    Result := TPropHookGetComponent(FHandlers[htGetComponent][i])(ComponentPath);
  // Note: TWriter only allows pascal identifiers for names, but in general
  // there is no restriction.
  if (Result = nil) and (LookupRoot is TComponent) then
    Result := TComponent(LookupRoot).FindComponent(ComponentPath);
end;

function TPropertyEditorHook.GetComponentName(
  AComponent: TComponent): Shortstring;
var
  i: Integer;
  Handler: TPropHookGetComponentName;
begin
  Result := '';
  if AComponent = nil then
    Exit;
  i := GetHandlerCount(htGetComponentName);
  while GetNextHandlerIndex(htGetComponentName, i) and (Result = '') do
  begin
    Handler := TPropHookGetComponentName(FHandlers[htGetComponentName][i]);
    Result := Handler(AComponent);
  end;
  if Result = '' then begin
    Result := AComponent.Name;
    if (AComponent.Owner<>LookupRoot) and (AComponent.Owner<>nil) then
      Result:=AComponent.Owner.Name+'.'+Result;
  end;
end;

procedure TPropertyEditorHook.GetComponentNames(TypeData: PTypeData;
  const Proc: TGetStrProc);
  
  procedure TraverseComponents(Root: TComponent);
  var
    i: integer;
  begin
    for i := 0 to Root.ComponentCount - 1 do
      if (Root.Components[i] is TypeData^.ClassType) then
        Proc(Root.Components[i].Name);
  end;
  
var
  i: integer;
  Handler: TPropHookGetComponentNames;
begin
  if not Assigned(LookupRoot) then
    Exit;
  i := GetHandlerCount(htGetComponentNames);
  if i > 0 then
  begin
    while GetNextHandlerIndex(htGetComponentNames, i) do
    begin
      Handler := TPropHookGetComponentNames(FHandlers[htGetComponentNames][i]);
      Handler(TypeData, Proc);
    end;
  end
  else
  if LookupRoot is TComponent then
  begin
    // only traverse local form/datamodule components
    TraverseComponents(TComponent(LookupRoot));
  end;
end;

function TPropertyEditorHook.GetRootClassName: Shortstring;
var
  i: Integer;
  Handler: TPropHookGetRootClassName;
begin
  Result := '';
  i := GetHandlerCount(htGetRootClassName);
  while GetNextHandlerIndex(htGetRootClassName, i) and (Result = '') do
  begin
    Handler := TPropHookGetRootClassName(FHandlers[htGetRootClassName][i]);
    Result := Handler();
  end;
  if (Result='') and Assigned(LookupRoot) then
    Result := LookupRoot.ClassName;
end;

function TPropertyEditorHook.BeforeAddPersistent(Sender: TObject;
  APersistentClass: TPersistentClass; Parent: TPersistent): boolean;
var
  i: Integer;
  Handler: TPropHookBeforeAddPersistent;
begin
  i := GetHandlerCount(htBeforeAddPersistent);
  while GetNextHandlerIndex(htBeforeAddPersistent, i) do
  begin
    Handler := TPropHookBeforeAddPersistent(FHandlers[htBeforeAddPersistent][i]);
    Result := Handler(Sender,APersistentClass,Parent);
    if not Result then exit;
  end;
  Result := True;
end;

procedure TPropertyEditorHook.ComponentRenamed(AComponent: TComponent);
var
  i: Integer;
begin
  i := GetHandlerCount(htComponentRenamed);
  while GetNextHandlerIndex(htComponentRenamed, i) do
    TPropHookComponentRenamed(FHandlers[htComponentRenamed][i])(AComponent);
end;

procedure TPropertyEditorHook.PersistentAdded(APersistent: TPersistent; Select: boolean);
var
  i: Integer;
begin
  i := GetHandlerCount(htPersistentAdded);
  while GetNextHandlerIndex(htPersistentAdded, i) do
    TPropHookPersistentAdded(FHandlers[htPersistentAdded][i])(APersistent, Select);
end;

procedure TPropertyEditorHook.PersistentDeleting(APersistent: TPersistent);
// call this to tell all IDE parts to remove all references from the APersistent
var
  i: Integer;
begin
  i:=GetHandlerCount(htPersistentDeleting);
  while GetNextHandlerIndex(htPersistentDeleting,i) do
    TPropHookPersistentDeleting(FHandlers[htPersistentDeleting][i])(APersistent);
end;

procedure TPropertyEditorHook.DeletePersistent(var APersistent: TPersistent);
// Call this to actually free APersistent
// One of the hooks will free it.
var
  i: Integer;
begin
  if APersistent=nil then exit;
  i:=GetHandlerCount(htDeletePersistent);
  if i>0 then begin
    while (APersistent<>nil) and GetNextHandlerIndex(htDeletePersistent,i) do
      TPropHookDeletePersistent(FHandlers[htDeletePersistent][i])(APersistent);
  end else
    FreeThenNil(APersistent);
end;

procedure TPropertyEditorHook.GetSelection(
  const ASelection: TPersistentSelectionList);
var
  i: Integer;
  Handler: TPropHookGetSelection;
begin
  if ASelection=nil then exit;
  ASelection.Clear;
  i:=GetHandlerCount(htGetSelectedPersistents);
  while GetNextHandlerIndex(htGetSelectedPersistents,i) do begin
    Handler:=TPropHookGetSelection(FHandlers[htGetSelectedPersistents][i]);
    Handler(ASelection);
  end;
end;

procedure TPropertyEditorHook.SetSelection(
  const ASelection: TPersistentSelectionList);
var
  i: Integer;
  Handler: TPropHookSetSelection;
  APersistent: TPersistent;
  NewLookupRoot: TPersistent;
begin
  // update LookupRoot
  NewLookupRoot:=LookupRoot;
  if (ASelection<>nil) and (ASelection.Count>0) then begin
    APersistent:=ASelection[0];
    if APersistent<>nil then
      NewLookupRoot:=GetLookupRootForComponent(APersistent);
  end;
  LookupRoot:=NewLookupRoot;
  // set selection
  if ASelection=nil then exit;
  //writeln('TPropertyEditorHook.SetSelection A ASelection.Count=',ASelection.Count);
  i:=GetHandlerCount(htSetSelectedPersistents);
  while GetNextHandlerIndex(htSetSelectedPersistents,i) do begin
    Handler:=TPropHookSetSelection(FHandlers[htSetSelectedPersistents][i]);
    Handler(ASelection);
  end;
  //writeln('TPropertyEditorHook.SetSelection END ASelection.Count=',ASelection.Count);
end;

procedure TPropertyEditorHook.Unselect(const APersistent: TPersistent);
var
  Selection: TPersistentSelectionList;
begin
  Selection := TPersistentSelectionList.Create;
  try
    GetSelection(Selection);
    if Selection.IndexOf(APersistent)>=0 then begin
      Selection.Remove(APersistent);
      SetSelection(Selection);
    end;
  finally
    Selection.Free;
  end;
end;

procedure TPropertyEditorHook.SelectOnlyThis(const APersistent: TPersistent);
var
  NewSelection: TPersistentSelectionList;
begin
  NewSelection := TPersistentSelectionList.Create;
  try
    if APersistent<>nil then
      NewSelection.Add(APersistent);
    SetSelection(NewSelection);
  finally
    NewSelection.Free;
  end;
end;

procedure TPropertyEditorHook.AddDependency(const AClass: TClass;
  const AnUnitname: shortstring);
var
  i: Integer;
begin
  i:=GetHandlerCount(htAddDependency);
  while GetNextHandlerIndex(htAddDependency,i) do
    TPropHookAddDependency(FHandlers[htAddDependency][i])(AClass,AnUnitName);
end;

function TPropertyEditorHook.GetObject(const Name: Shortstring): TPersistent;
var
  i: Integer;
begin
  Result:=nil;
  i:=GetHandlerCount(htGetObject);
  while GetNextHandlerIndex(htGetObject,i) and (Result=nil) do
    Result:=TPropHookGetObject(FHandlers[htGetObject][i])(Name);
end;

function TPropertyEditorHook.GetObjectName(Instance: TPersistent): Shortstring;
var
  i: Integer;
begin
  Result:='';
  i:=GetHandlerCount(htGetObjectName);
  if i>0 then begin
    while GetNextHandlerIndex(htGetObjectName,i) and (Result='') do
      Result:=TPropHookGetObjectName(FHandlers[htGetObject][i])(Instance);
  end else
    if Instance is TComponent then
      Result:=TComponent(Instance).Name
    else if instance is TCollectionItem then 
      Result:=TCollectionItem(Instance).GetNamePath;
end;

procedure TPropertyEditorHook.GetObjectNames(TypeData: PTypeData;
  const Proc: TGetStrProc);
var
  i: Integer;
begin
  i:=GetHandlerCount(htGetObjectNames);
  while GetNextHandlerIndex(htGetObjectNames,i) do
    TPropHookGetObjectNames(FHandlers[htGetObjectNames][i])(TypeData,Proc);
end;

procedure TPropertyEditorHook.ObjectReferenceChanged(Sender: TObject;
  NewObject: TPersistent);
var
  i: Integer;
begin
  i:=GetHandlerCount(htObjectPropertyChanged);
  while GetNextHandlerIndex(htObjectPropertyChanged,i) do
    TPropHookObjectPropertyChanged(FHandlers[htObjectPropertyChanged][i])(
                  Sender,NewObject);
end;

procedure TPropertyEditorHook.Modified(Sender: TObject);
var
  i: Integer;
  AForm: TCustomForm;
  Editor: TPropertyEditor;
  List: TFPList;
  APersistent: TPersistent;
  ARoot: TPersistent;
begin
  i:=GetHandlerCount(htModified);
  while GetNextHandlerIndex(htModified,i) do
    TPropHookModified(FHandlers[htModified][i])(Sender);
  if Sender is TPropertyEditor then begin
    // mark the designer form of every selected persistent
    Editor:=TPropertyEditor(Sender);
    List:=TFPList.Create;
    try
      for i:=0 to Editor.PropCount-1 do begin
        // for every selected persistent ...
        APersistent:=Editor.GetComponent(i);
        if APersistent=nil then continue;
        if List.IndexOf(APersistent)>=0 then continue;
        List.Add(APersistent);
        // ... get the lookuproot ...
        ARoot:=GetLookupRootForComponent(APersistent);
        if ARoot=nil then continue;
        if (ARoot<>APersistent) and (List.IndexOf(ARoot)>=0) then continue;
        List.Add(ARoot);
        // ... get the designer ...
        AForm:=GetDesignerForm(ARoot);
        if (AForm <> nil) and (AForm.Designer <> nil) then
          AForm.Designer.Modified; // ... and mark it modified
      end;
    finally
      List.Free;
    end;
  end
  else 
  if (FLookupRoot <> nil) then
  begin
    AForm := GetDesignerForm(FLookupRoot);
    if (AForm <> nil) and (AForm.Designer <> nil) then
      AForm.Designer.Modified;
  end;
end;

procedure TPropertyEditorHook.Revert(Instance:TPersistent;
  PropInfo:PPropInfo);
var
  i: Integer;
begin
  i:=GetHandlerCount(htRevert);
  while GetNextHandlerIndex(htRevert,i) do
    TPropHookRevert(FHandlers[htRevert][i])(Instance,PropInfo);
end;

procedure TPropertyEditorHook.RefreshPropertyValues;
var
  i: Integer;
begin
  i:=GetHandlerCount(htRefreshPropertyValues);
  while GetNextHandlerIndex(htRefreshPropertyValues,i) do
    TPropHookRefreshPropertyValues(FHandlers[htRefreshPropertyValues][i])();
end;

procedure TPropertyEditorHook.RemoveAllHandlersForObject(const HandlerObject: TObject);
var
  HookType: TPropHookType;
begin
  for HookType:=Low(FHandlers) to High(FHandlers) do
    if FHandlers[HookType]<>nil then
      FHandlers[HookType].RemoveAllMethodsOfObject(HandlerObject);
end;

procedure TPropertyEditorHook.AddHandlerChangeLookupRoot(
  const OnChangeLookupRoot: TPropHookChangeLookupRoot);
begin
  AddHandler(htChangeLookupRoot,TMethod(OnChangeLookupRoot));
end;

procedure TPropertyEditorHook.RemoveHandlerChangeLookupRoot(
  const OnChangeLookupRoot: TPropHookChangeLookupRoot);
begin
  RemoveHandler(htChangeLookupRoot,TMethod(OnChangeLookupRoot));
end;

procedure TPropertyEditorHook.AddHandlerCreateMethod(
  const OnCreateMethod: TPropHookCreateMethod);
begin
  AddHandler(htCreateMethod,TMethod(OnCreateMethod));
end;

procedure TPropertyEditorHook.RemoveHandlerCreateMethod(
  const OnCreateMethod: TPropHookCreateMethod);
begin
  RemoveHandler(htCreateMethod,TMethod(OnCreateMethod));
end;

procedure TPropertyEditorHook.AddHandlerGetMethodName(
  const OnGetMethodName: TPropHookGetMethodName);
begin
  AddHandler(htGetMethodName,TMethod(OnGetMethodName));
end;

procedure TPropertyEditorHook.RemoveHandlerGetMethodName(
  const OnGetMethodName: TPropHookGetMethodName);
begin
  RemoveHandler(htGetMethodName,TMethod(OnGetMethodName));
end;

procedure TPropertyEditorHook.AddHandlerGetMethods(
  const OnGetMethods: TPropHookGetMethods);
begin
  AddHandler(htGetMethods,TMethod(OnGetMethods));
end;

procedure TPropertyEditorHook.RemoveHandlerGetMethods(
  const OnGetMethods: TPropHookGetMethods);
begin
  RemoveHandler(htGetMethods,TMethod(OnGetMethods));
end;

procedure TPropertyEditorHook.AddHandlerCompatibleMethodExists(
  const OnMethodExists: TPropHookCompatibleMethodExists);
begin
  AddHandler(htCompatibleMethodExists,TMethod(OnMethodExists));
end;

procedure TPropertyEditorHook.RemoveHandlerCompatibleMethodExists(
  const OnMethodExists: TPropHookCompatibleMethodExists);
begin
  RemoveHandler(htCompatibleMethodExists,TMethod(OnMethodExists));
end;

procedure TPropertyEditorHook.AddHandlerGetCompatibleMethods(
  const OnGetMethods: TPropHookGetCompatibleMethods);
begin
  AddHandler(htGetCompatibleMethods,TMethod(OnGetMethods));
end;

procedure TPropertyEditorHook.RemoveHandlerGetCompatibleMethods(
  const OnGetMethods: TPropHookGetCompatibleMethods);
begin
  RemoveHandler(htGetCompatibleMethods,TMethod(OnGetMethods));
end;

procedure TPropertyEditorHook.AddHandlerMethodExists(
  const OnMethodExists: TPropHookMethodExists);
begin
  AddHandler(htMethodExists,TMethod(OnMethodExists));
end;

procedure TPropertyEditorHook.RemoveHandlerMethodExists(
  const OnMethodExists: TPropHookMethodExists);
begin
  RemoveHandler(htMethodExists,TMethod(OnMethodExists));
end;

procedure TPropertyEditorHook.AddHandlerRenameMethod(
  const OnRenameMethod: TPropHookRenameMethod);
begin
  AddHandler(htRenameMethod,TMethod(OnRenameMethod));
end;

procedure TPropertyEditorHook.RemoveHandlerRenameMethod(
  const OnRenameMethod: TPropHookRenameMethod);
begin
  RemoveHandler(htRenameMethod,TMethod(OnRenameMethod));
end;

procedure TPropertyEditorHook.AddHandlerShowMethod(
  const OnShowMethod: TPropHookShowMethod);
begin
  AddHandler(htShowMethod,TMethod(OnShowMethod));
end;

procedure TPropertyEditorHook.RemoveHandlerShowMethod(
  const OnShowMethod: TPropHookShowMethod);
begin
  RemoveHandler(htShowMethod,TMethod(OnShowMethod));
end;

procedure TPropertyEditorHook.AddHandlerMethodFromAncestor(
  const OnMethodFromAncestor: TPropHookMethodFromAncestor);
begin
  AddHandler(htMethodFromAncestor,TMethod(OnMethodFromAncestor));
end;

procedure TPropertyEditorHook.RemoveHandlerMethodFromAncestor(
  const OnMethodFromAncestor: TPropHookMethodFromAncestor);
begin
  RemoveHandler(htMethodFromAncestor,TMethod(OnMethodFromAncestor));
end;

procedure TPropertyEditorHook.AddHandlerChainCall(
  const OnChainCall: TPropHookChainCall);
begin
  AddHandler(htChainCall,TMethod(OnChainCall));
end;

procedure TPropertyEditorHook.RemoveHandlerChainCall(
  const OnChainCall: TPropHookChainCall);
begin
  RemoveHandler(htChainCall,TMethod(OnChainCall));
end;

procedure TPropertyEditorHook.AddHandlerGetComponent(
  const OnGetComponent: TPropHookGetComponent);
begin
  AddHandler(htGetComponent,TMethod(OnGetComponent));
end;

procedure TPropertyEditorHook.RemoveHandlerGetComponent(
  const OnGetComponent: TPropHookGetComponent);
begin
  RemoveHandler(htGetComponent,TMethod(OnGetComponent));
end;

procedure TPropertyEditorHook.AddHandlerGetComponentName(
  const OnGetComponentName: TPropHookGetComponentName);
begin
  AddHandler(htGetComponentName,TMethod(OnGetComponentName));
end;

procedure TPropertyEditorHook.RemoveHandlerGetComponentName(
  const OnGetComponentName: TPropHookGetComponentName);
begin
  RemoveHandler(htGetComponentName,TMethod(OnGetComponentName));
end;

procedure TPropertyEditorHook.AddHandlerGetComponentNames(
  const OnGetComponentNames: TPropHookGetComponentNames);
begin
  AddHandler(htGetComponentNames,TMethod(OnGetComponentNames));
end;

procedure TPropertyEditorHook.RemoveHandlerGetComponentNames(
  const OnGetComponentNames: TPropHookGetComponentNames);
begin
  RemoveHandler(htGetComponentNames,TMethod(OnGetComponentNames));
end;

procedure TPropertyEditorHook.AddHandlerGetRootClassName(
  const OnGetRootClassName: TPropHookGetRootClassName);
begin
  AddHandler(htGetRootClassName,TMethod(OnGetRootClassName));
end;

procedure TPropertyEditorHook.RemoveHandlerGetRootClassName(
  const OnGetRootClassName: TPropHookGetRootClassName);
begin
  RemoveHandler(htGetRootClassName,TMethod(OnGetRootClassName));
end;

procedure TPropertyEditorHook.AddHandlerBeforeAddPersistent(
  const OnBeforeAddPersistent: TPropHookBeforeAddPersistent);
begin
  AddHandler(htBeforeAddPersistent,TMethod(OnBeforeAddPersistent));
end;

procedure TPropertyEditorHook.RemoveHandlerBeforeAddPersistent(
  const OnBeforeAddPersistent: TPropHookBeforeAddPersistent);
begin
  RemoveHandler(htBeforeAddPersistent,TMethod(OnBeforeAddPersistent));
end;

procedure TPropertyEditorHook.AddHandlerComponentRenamed(
  const OnComponentRenamed: TPropHookComponentRenamed);
begin
  AddHandler(htComponentRenamed,TMethod(OnComponentRenamed));
end;

procedure TPropertyEditorHook.RemoveHandlerComponentRenamed(
  const OnComponentRenamed: TPropHookComponentRenamed);
begin
  RemoveHandler(htComponentRenamed,TMethod(OnComponentRenamed));
end;

procedure TPropertyEditorHook.AddHandlerPersistentAdded(
  const OnPersistentAdded: TPropHookPersistentAdded);
begin
  AddHandler(htPersistentAdded,TMethod(OnPersistentAdded));
end;

procedure TPropertyEditorHook.RemoveHandlerPersistentAdded(
  const OnPersistentAdded: TPropHookPersistentAdded);
begin
  RemoveHandler(htPersistentAdded,TMethod(OnPersistentAdded));
end;

procedure TPropertyEditorHook.AddHandlerPersistentDeleting(
  const OnPersistentDeleting: TPropHookPersistentDeleting);
begin
  AddHandler(htPersistentDeleting,TMethod(OnPersistentDeleting));
end;

procedure TPropertyEditorHook.RemoveHandlerPersistentDeleting(
  const OnPersistentDeleting: TPropHookPersistentDeleting);
begin
  RemoveHandler(htPersistentDeleting,TMethod(OnPersistentDeleting));
end;

procedure TPropertyEditorHook.AddHandlerDeletePersistent(
  const OnDeletePersistent: TPropHookDeletePersistent);
begin
  AddHandler(htDeletePersistent,TMethod(OnDeletePersistent));
end;

procedure TPropertyEditorHook.RemoveHandlerDeletePersistent(
  const OnDeletePersistent: TPropHookDeletePersistent);
begin
  RemoveHandler(htDeletePersistent,TMethod(OnDeletePersistent));
end;

procedure TPropertyEditorHook.AddHandlerGetSelection(
  const OnGetSelection: TPropHookGetSelection);
begin
  AddHandler(htGetSelectedPersistents,TMethod(OnGetSelection));
end;

procedure TPropertyEditorHook.RemoveHandlerGetSelection(
  const OnGetSelection: TPropHookGetSelection);
begin
  RemoveHandler(htGetSelectedPersistents,TMethod(OnGetSelection));
end;

procedure TPropertyEditorHook.AddHandlerSetSelection(
  const OnSetSelection: TPropHookSetSelection);
begin
  AddHandler(htSetSelectedPersistents,TMethod(OnSetSelection));
end;

procedure TPropertyEditorHook.RemoveHandlerSetSelection(
  const OnSetSelection: TPropHookSetSelection);
begin
  RemoveHandler(htSetSelectedPersistents,TMethod(OnSetSelection));
end;

procedure TPropertyEditorHook.AddHandlerGetObject(
  const OnGetObject: TPropHookGetObject);
begin
  AddHandler(htGetObject,TMethod(OnGetObject));
end;

procedure TPropertyEditorHook.RemoveHandlerGetObject(
  const OnGetObject: TPropHookGetObject);
begin
  RemoveHandler(htGetObject,TMethod(OnGetObject));
end;

procedure TPropertyEditorHook.AddHandlerGetObjectName(
  const OnGetObjectName: TPropHookGetObjectName);
begin
  AddHandler(htGetObjectName,TMethod(OnGetObjectName));
end;

procedure TPropertyEditorHook.RemoveHandlerGetObjectName(
  const OnGetObjectName: TPropHookGetObjectName);
begin
  RemoveHandler(htGetObjectName,TMethod(OnGetObjectName));
end;

procedure TPropertyEditorHook.AddHandlerGetObjectNames(
  const OnGetObjectNames: TPropHookGetObjectNames);
begin
  AddHandler(htGetObjectNames,TMethod(OnGetObjectNames));
end;

procedure TPropertyEditorHook.RemoveHandlerGetObjectNames(
  const OnGetObjectNames: TPropHookGetObjectNames);
begin
  RemoveHandler(htGetObjectNames,TMethod(OnGetObjectNames));
end;

procedure TPropertyEditorHook.AddHandlerObjectPropertyChanged(
  const OnObjectPropertyChanged: TPropHookObjectPropertyChanged);
begin
  AddHandler(htObjectPropertyChanged,TMethod(OnObjectPropertyChanged));
end;

procedure TPropertyEditorHook.RemoveHandlerObjectPropertyChanged(
  const OnObjectPropertyChanged: TPropHookObjectPropertyChanged);
begin
  RemoveHandler(htObjectPropertyChanged,TMethod(OnObjectPropertyChanged));
end;

procedure TPropertyEditorHook.AddHandlerModified(
  const OnModified: TPropHookModified);
begin
  AddHandler(htModified,TMethod(OnModified));
end;

procedure TPropertyEditorHook.RemoveHandlerModified(
  const OnModified: TPropHookModified);
begin
  RemoveHandler(htModified,TMethod(OnModified));
end;

procedure TPropertyEditorHook.AddHandlerRevert(const OnRevert: TPropHookRevert);
begin
  AddHandler(htRevert,TMethod(OnRevert));
end;

procedure TPropertyEditorHook.RemoveHandlerRevert(const OnRevert: TPropHookRevert);
begin
  RemoveHandler(htRevert,TMethod(OnRevert));
end;

procedure TPropertyEditorHook.AddHandlerRefreshPropertyValues(
  const OnRefreshPropertyValues: TPropHookRefreshPropertyValues);
begin
  AddHandler(htRefreshPropertyValues,TMethod(OnRefreshPropertyValues));
end;

procedure TPropertyEditorHook.RemoveHandlerRefreshPropertyValues(
  const OnRefreshPropertyValues: TPropHookRefreshPropertyValues);
begin
  RemoveHandler(htRefreshPropertyValues,TMethod(OnRefreshPropertyValues));
end;

procedure TPropertyEditorHook.AddHandlerAddDependency(
  const OnAddDependency: TPropHookAddDependency);
begin
  AddHandler(htAddDependency,TMethod(OnAddDependency));
end;

procedure TPropertyEditorHook.RemoveHandlerAddDependency(
  const OnAddDependency: TPropHookAddDependency);
begin
  RemoveHandler(htAddDependency,TMethod(OnAddDependency));
end;

procedure TPropertyEditorHook.SetLookupRoot(APersistent: TPersistent);
var
  i: Integer;
begin
  if FLookupRoot=APersistent then exit;
  FLookupRoot:=APersistent;
  i:=GetHandlerCount(htChangeLookupRoot);
  while GetNextHandlerIndex(htChangeLookupRoot,i) do
    TPropHookChangeLookupRoot(FHandlers[htChangeLookupRoot][i])();
end;

procedure TPropertyEditorHook.AddHandler(HookType: TPropHookType;
  const Handler: TMethod);
begin
  if Handler.Code=nil then RaiseGDBException('TPropertyEditorHook.AddHandler');
  if FHandlers[HookType]=nil then
    FHandlers[HookType]:=TMethodList.Create;
  FHandlers[HookType].Add(Handler);
end;

procedure TPropertyEditorHook.RemoveHandler(HookType: TPropHookType;
  const Handler: TMethod);
begin
  FHandlers[HookType].Remove(Handler);
end;

function TPropertyEditorHook.GetHandlerCount(HookType: TPropHookType): integer;
begin
  Result:=FHandlers[HookType].Count;
end;

function TPropertyEditorHook.GetNextHandlerIndex(HookType: TPropHookType;
  var i: integer): boolean;
begin
  Result:=FHandlers[HookType].NextDownIndex(i);
end;

constructor TPropertyEditorHook.Create;
begin
  inherited Create;
end;

destructor TPropertyEditorHook.Destroy;
var
  HookType: TPropHookType;
begin
  for HookType:=Low(FHandlers) to high(FHandlers) do
    FreeThenNil(FHandlers[HookType]);
  inherited Destroy;
end;

function KeyAndShiftStateToKeyString(Key: word; ShiftState: TShiftState): String;
var
  p: integer;

  procedure AddStr(const s: string);
  begin
    if s <> '' then
    begin
      inc(p);
      Result := Result + s;
    end;
  end;

  procedure AddAttribute(const s: string);
  begin
    if p > 0 then
      AddStr('+');
    AddStr(s);
  end;

  // Tricky routine. This only works for western languages
  // TODO: This should be replaces by the winapi VKtoChar functions
  //
  procedure AddKey;
  begin
    if p > 0 then
      AddStr('+');

    case Key of
      VK_UNKNOWN    :AddStr(srVK_UNKNOWN);
      VK_LBUTTON    :AddStr(srVK_LBUTTON);
      VK_RBUTTON    :AddStr(srVK_RBUTTON);
      VK_CANCEL     :AddStr(srVK_CANCEL);
      VK_MBUTTON    :AddStr(srVK_MBUTTON);
      VK_BACK       :AddStr(srVK_BACK);
      VK_TAB        :AddStr(srVK_TAB);
      VK_CLEAR      :AddStr(srVK_CLEAR);
      VK_RETURN     :AddStr(srVK_RETURN);
      VK_SHIFT      :AddStr(srVK_SHIFT);
      VK_CONTROL    :AddStr(srVK_CONTROL);
      VK_MENU       :AddStr(srVK_MENU);
      VK_PAUSE      :AddStr(srVK_PAUSE);
      VK_CAPITAL    :AddStr(srVK_CAPITAL);
      VK_KANA       :AddStr(srVK_KANA);
    //  VK_HANGUL     :AddStr('Hangul');
      VK_JUNJA      :AddStr(srVK_JUNJA);
      VK_FINAL      :AddStr(srVK_FINAL);
      VK_HANJA      :AddStr(srVK_HANJA );
    //  VK_KANJI      :AddStr('Kanji');
      VK_ESCAPE     :AddStr(srVK_ESCAPE);
      VK_CONVERT    :AddStr(srVK_CONVERT);
      VK_NONCONVERT :AddStr(srVK_NONCONVERT);
      VK_ACCEPT     :AddStr(srVK_ACCEPT);
      VK_MODECHANGE :AddStr(srVK_MODECHANGE);
      VK_SPACE      :AddStr(srVK_SPACE);
      VK_PRIOR      :AddStr(srVK_PRIOR);
      VK_NEXT       :AddStr(srVK_NEXT);
      VK_END        :AddStr(srVK_END);
      VK_HOME       :AddStr(srVK_HOME);
      VK_LEFT       :AddStr(srVK_LEFT);
      VK_UP         :AddStr(srVK_UP);
      VK_RIGHT      :AddStr(srVK_RIGHT);
      VK_DOWN       :AddStr(srVK_DOWN);
      VK_SELECT     :AddStr(srVK_SELECT);
      VK_PRINT      :AddStr(srVK_PRINT);
      VK_EXECUTE    :AddStr(srVK_EXECUTE);
      VK_SNAPSHOT   :AddStr(srVK_SNAPSHOT);
      VK_INSERT     :AddStr(srVK_INSERT);
      VK_DELETE     :AddStr(srVK_DELETE);
      VK_HELP       :AddStr(srVK_HELP);
      VK_0..VK_9    :AddStr(IntToStr(Key-VK_0));
      VK_A..VK_Z    :AddStr(chr(ord('A')+Key-VK_A));
      VK_LWIN       :AddStr(srVK_LWIN);
      VK_RWIN       :AddStr(srVK_RWIN);
      VK_APPS       :AddStr(srVK_APPS);
      VK_NUMPAD0..VK_NUMPAD9: AddStr(Format(srVK_NUMPAD,[Key-VK_NUMPAD0]));
      VK_MULTIPLY   :AddStr('*');
      VK_ADD        :AddStr('+');
      VK_SEPARATOR  :AddStr('|');
      VK_SUBTRACT   :AddStr('-');
      VK_DECIMAL    :AddStr('.');
      VK_DIVIDE     :AddStr('/');
      VK_F1..VK_F24 :AddStr('F'+IntToStr(Key-VK_F1+1));
      VK_NUMLOCK    :AddStr(srVK_NUMLOCK);
      VK_SCROLL     :AddStr(srVK_SCROLL);
      VK_OEM_1      :AddStr(lisOEM1);
      VK_OEM_PLUS   :AddStr(lisOEMPlus);
      VK_OEM_COMMA  :AddStr(lisOEMComma);
      VK_OEM_MINUS  :AddStr(lisOEMMinus);
      VK_OEM_PERIOD :AddStr(lisOEMPeriod);
      VK_OEM_2      :AddStr(lisOEM2);
      VK_OEM_3      :AddStr(lisOEM3);
      VK_OEM_4      :AddStr(lisOEM4);
      VK_OEM_5      :AddStr(lisOEM5);
      VK_OEM_6      :AddStr(lisOEM6);
      VK_OEM_7      :AddStr(lisOEM7);
      VK_OEM_8      :AddStr(lisOEM8);
//    VK_EQUAL      :AddStr('=');
//    VK_COMMA      :AddStr(',');
//    VK_POINT      :AddStr('.');
//    VK_SLASH      :AddStr('/');
//    VK_AT         :AddStr('@');
    else
      AddStr(UnknownVKPrefix);
      AddStr(IntToStr(Key));
      AddStr(UnknownVKPostfix);
    end;
  end;

begin
  Result := '';
  p := 0;
  if ssCtrl in ShiftState then AddAttribute(srkm_Ctrl);
  if ssAlt in ShiftState then AddAttribute(srkm_Alt);
  if ssShift in ShiftState then AddAttribute(srVK_SHIFT);
  if ssMeta in ShiftState then
    {$IFDEF LCLcarbon}
    AddAttribute(srVK_CMD);
    {$ELSE}
    AddAttribute(srVK_META);
    {$ENDIF}
  if ssSuper in ShiftState then AddAttribute(srVK_SUPER);
  AddKey;
end;

function KeyStringIsIrregular(const s: string): boolean;
begin
  if (length(UnknownVKPrefix)<length(s))
  and (AnsiStrLComp(PChar(s),PChar(UnknownVKPrefix),length(UnknownVKPrefix))=0)
  then
    Result:=true
  else
    Result:=false;
end;

function KeyStringToVKCode(const s: string): word;
var
  i: PtrInt;
  Data: Pointer;
begin
  Result:=VK_UNKNOWN;
  if KeyStringIsIrregular(s) then begin
    Result:=word(StrToIntDef(copy(s,7,length(s)-8),VK_UNKNOWN));
    exit;
  end;
  if (s<>'none') and (s<>'') then begin
    if VirtualKeyStrings=nil then begin
      VirtualKeyStrings:=TStringHashList.Create(true);
      for i:=1 to 255 do
        VirtualKeyStrings.Add(KeyAndShiftStateToKeyString(word(i),[]), Pointer(i));
    end;
  end else
    exit;
  Data:=VirtualKeyStrings.Data[s];
  if Data<>nil then
    Result:=word(PtrUInt(Data));
end;

function GetClassUnitName(Value: TClass): string;
var
  TheTypeInfo: PTypeInfo;
  TheTypeData: PTypeData;
begin
  Result:='';
  TheTypeInfo:=ClassTypeInfo(Value);
  if TheTypeInfo=nil then exit;
  TheTypeData:=GetTypeData(TheTypeInfo);
  if TheTypeData=nil then exit;
  Result:=TheTypeData^.UnitName;
  //debugln('GetClassUnitName A Result="',Result,'"');
end;

procedure CreateComponentEvent(AComponent: TComponent; const EventName: string);
var
  CurDesigner: TIDesigner;
  PropInfo: PPropInfo;
  Hook: TPropertyEditorHook;
  PersistentList: TPersistentSelectionList;
  MethodPropEditor: TMethodPropertyEditor;
begin
  CurDesigner:=FindRootDesigner(AComponent);
  if CurDesigner=nil then exit;
  // search method
  PropInfo:=GetPropInfo(AComponent,EventName);
  //writeln('CreateComponentEvent B ',PropInfo<>nil,' ',PropInfo^.PropType<>nil,' ',PropInfo^.PropType^.Kind=tkMethod,' ',(PropInfo^.GetProc<>nil),' ',(PropInfo^.SetProc<>nil));
  if (PropInfo=nil)
  or (PropInfo^.PropType=nil)
  or (PropInfo^.PropType^.Kind<>tkMethod)
  or (PropInfo^.GetProc=nil)
  or (PropInfo^.SetProc=nil) then
    exit;
  
  MethodPropEditor:=nil;
  PersistentList:=nil;
  try
    PersistentList := TPersistentSelectionList.Create;
    PersistentList.Add(AComponent);
    Hook:=GlobalDesignHook;
    MethodPropEditor := TMethodPropertyEditor.Create(Hook,1);
    MethodPropEditor.SetPropEntry(0, AComponent, PropInfo);
    MethodPropEditor.Initialize;
    MethodPropEditor.Edit;
  finally
    MethodPropEditor.Free;
    PersistentList.Free;
  end;
end;

function ClassNameToComponentName(const AClassName: string): string;
begin
  Result:=AClassName;
  if (length(Result)>2) and (Result[1] in ['T','t'])
  and (not (Result[2] in ['0'..'9'])) then
    System.Delete(Result,1,1);
end;

Function ClassTypeInfo(Value: TClass): PTypeInfo;
begin
  Result := PTypeInfo(Value.ClassInfo);
end;

procedure InitPropEdits;
begin
  PropertyClassList:=TList.Create;
  PropertyEditorMapperList:=TList.Create;
  // register the standard property editors

  RegisterPropertyEditor(TypeInfo(AnsiString), TComponent, 'Name', TComponentNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTranslateString), TCustomLabel, 'Caption', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTranslateString), TCustomStaticText, 'Caption', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTranslateString), TCustomCheckBox, 'Caption', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTranslateString), TComponent, 'Hint', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabOrder), TControl, 'TabOrder', TTabOrderPropertyEditor);
  RegisterPropertyEditor(TypeInfo(ShortString), nil, '', TCaptionPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), nil, '', TStringsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), nil, 'SessionProperties', TSessionPropertiesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TModalResult), nil, 'ModalResult', TModalResultPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TShortCut), nil, '', TShortCutPropertyEditor);
  //RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TDate'),
  //  nil,'',TDatePropertyEditor);
  //RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TTime'),
  //  nil,'',TTimePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDateTime), nil, '', TDateTimePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), nil, '', TCursorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TComponent), nil, '', TComponentPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCollection), nil, '', TCollectionPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFileDialog, 'Filter', TFileDlgFilterProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFilterComboBox, 'Filter', TFileDlgFilterProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFileNameEdit, 'Filter', TFileDlgFilterProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCustomPropertyStorage, 'Filename', TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TAnchorSide), TControl, 'AnchorSideLeft', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TAnchorSide), TControl, 'AnchorSideTop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TAnchorSide), TControl, 'AnchorSideRight', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TAnchorSide), TControl, 'AnchorSideBottom', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(LongInt), TControl, 'ClientWidth', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(LongInt), TControl, 'ClientHeight', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCustomForm, 'LCLVersion', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCustomFrame, 'LCLVersion', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCustomPage), TCustomTabControl, 'ActivePage', TNoteBookActiveControlPropertyEditor);
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
  FreeAndNil(VirtualKeyStrings);
end;

procedure EditCollection(AComponent: TComponent; ACollection: TCollection; APropertyName: String);
begin
  TCollectionPropertyEditor.ShowCollectionEditor(ACollection, AComponent, APropertyName);
end;

function IsInteresting(
  const AEditor: TPropertyEditor; const AFilter: TTypeKinds): Boolean;
var
  visited: TFPList;

  procedure Rec(A: TPropertyEditor);
  var
    propList: PPropList;
    i: Integer;
    ti: PTypeInfo;
    edClass: TPropertyEditorClass;
    ed: TPropertyEditor;
    obj: TPersistent;
    PropCnt: LongInt;
  begin
    ti := A.GetPropInfo^.PropType;
    //DebugLn('IsInteresting: ', ti^.Name);
    Result := ti^.Kind <> tkClass;
    if Result then exit;

    // Subroperties can change if user selects another object =>
    // we must show the property, even if it is not interesting currently.
    Result := paVolatileSubProperties in A.GetAttributes;
    if Result then exit;

    if tkClass in AFilter then begin
      // We want classes => any non-trivial editor is immediately interesting.
      Result := A.ClassType <> TClassPropertyEditor;
      if Result then exit;
    end
    else if
      A.GetAttributes * [paSubProperties, paVolatileSubProperties] = []
    then exit;

    obj := TPersistent(A.GetObjectValue);
    // At this stage, there is nothing interesting left in empty objects.
    if obj = nil then exit;

    // Class properties may directly or indirectly refer to the same class,
    // so we must avoid infinite recursion.
    if visited.IndexOf(ti) >= 0 then exit;
    visited.Add(ti);
    PropCnt:=GetPropList(ti, propList);
    try
      for i := 0 to PropCnt - 1 do begin
        if not (propList^[i]^.PropType^.Kind in AFilter + [tkClass]) then continue;
        edClass := GetEditorClass(propList^[i], obj);
        if edClass = nil then continue;
        ed := edClass.Create(AEditor.FPropertyHook, 1);
        try
          ed.SetPropEntry(0, obj, propList^[i]);
          ed.Initialize;
          Rec(ed);
        finally
          ed.Free;
        end;
        if Result then break;
      end;
    finally
      FreeMem(propList);
    end;
    visited.Delete(visited.Count - 1);
  end;

begin
  visited := TFPList.Create;
  try
    //DebugLn('IsInteresting -> ', AEditor.GetPropInfo^.Name, ': ', AEditor.GetPropInfo^.PropType^.Name);
    Rec(AEditor);
    //DebugLn('IsInteresting <- ', BoolToStr(Result, true));
  finally
    visited.Free;
  end;
end;

function dbgs(peh: TPropEditHint): string;
begin
  writestr(Result,peh);
end;

{ TNoteBookActiveControlPropertyEditor }

function TNoteBookActiveControlPropertyEditor.CheckNewValue(
  APersistent: TPersistent): boolean;
var
  AComponent: TPersistent;
  Notebook: TCustomTabControl;
begin
  Result:=true;
  if APersistent=nil then exit;
  AComponent:=GetComponent(0);
  if not (AComponent is TCustomTabControl) then
    raise Exception.Create('invalid instance for this property editor');
  Notebook:=TCustomTabControl(AComponent);
  if Notebook.IndexOf(APersistent)<0 then
    raise Exception.Create('only children are allowed for this property');
end;

function TNoteBookActiveControlPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=(inherited GetAttributes)-[paMultiSelect];
end;

procedure TNoteBookActiveControlPropertyEditor.GetValues(Proc: TGetStrProc);
var
  AComponent: TPersistent;
  Notebook: TCustomTabControl;
  i: Integer;
begin
  Proc(oisNone);
  AComponent:=GetComponent(0);
  if not (AComponent is TCustomTabControl) then exit;
  Notebook:=TCustomTabControl(AComponent);
  for i:=0 to Notebook.PageCount-1 do
    Proc(Notebook.Page[i].Name);
end;

{ TCustomShortCutGrabBox }

procedure TCustomShortCutGrabBox.SetKey(const AValue: Word);
var
  s: String;
  i: LongInt;
begin
  if FKey=AValue then exit;
  FKey:=AValue;
  s:=KeyAndShiftStateToKeyString(FKey,[]);
  i:=KeyComboBox.Items.IndexOf(s);
  if i>=0 then
    KeyComboBox.ItemIndex:=i
  else if KeyStringIsIrregular(s) then begin
    KeyComboBox.Items.Add(s);
    KeyComboBox.ItemIndex:=KeyComboBox.Items.IndexOf(s);
  end else
    KeyComboBox.ItemIndex:=0;
end;

procedure TCustomShortCutGrabBox.OnGrabButtonClick(Sender: TObject);
begin
  FGrabForm:=TForm.Create(Self);
  FGrabForm.BorderStyle:=bsToolWindow;
  FGrabForm.KeyPreview:=true;
  FGrabForm.Position:=poScreenCenter;
  FGrabForm.OnKeyDown:=@OnGrabFormKeyDown;
  FGrabForm.Caption:='Press a key ...';
  with TLabel.Create(Self) do begin
    Caption:='Press a key ...';
    BorderSpacing.Around:=25;
    Parent:=FGrabForm;
  end;
  FGrabForm.Width:=200;
  FGrabForm.Height:=50;
  FGrabForm.AutoSize:=true;
  FGrabForm.ShowModal;
  FreeAndNil(FGrabForm);
end;

procedure TCustomShortCutGrabBox.OnShitCheckBoxClick(Sender: TObject);
var
  s: TShiftStateEnum;
begin
  for s:=Low(TShiftStateEnum) to High(TShiftStateEnum) do
    if FCheckBoxes[s]=Sender then
      if FCheckBoxes[s].Checked then
        Include(FShiftState,s)
      else
        Exclude(FShiftState,s);
end;

procedure TCustomShortCutGrabBox.OnGrabFormKeyDown(Sender: TObject;
  var AKey: Word; AShift: TShiftState);
begin
  //DebugLn(['TCustomShortCutGrabBox.OnGrabFormKeyDown ',AKey,' ',dbgs(AShift)]);
  if not (AKey in [VK_CONTROL, VK_LCONTROL, VK_RCONTROL,
             VK_SHIFT, VK_LSHIFT, VK_RSHIFT,
             VK_MENU, VK_LMENU, VK_RMENU,
             VK_LWIN, VK_RWIN,
             VK_UNKNOWN, VK_UNDEFINED])
  then begin
    if (AKey=VK_ESCAPE) and (AShift=[]) then begin
      Key:=VK_UNKNOWN;
      ShiftState:=[];
    end else begin
      Key:=AKey;
      ShiftState:=AShift;
    end;
    FGrabForm.ModalResult:=mrOk;
  end;
end;

procedure TCustomShortCutGrabBox.OnKeyComboboxEditingDone(Sender: TObject);
begin
  Key:=KeyStringToVKCode(KeyComboBox.Text);
end;

function TCustomShortCutGrabBox.GetShiftCheckBox(Shift: TShiftStateEnum
  ): TCheckBox;
begin
  Result:=FCheckBoxes[Shift];
end;

procedure TCustomShortCutGrabBox.SetAllowedShifts(const AValue: TShiftState);
begin
  if FAllowedShifts=AValue then exit;
  FAllowedShifts:=AValue;
  ShiftState:=ShiftState*FAllowedShifts;
end;

procedure TCustomShortCutGrabBox.SetShiftButtons(const AValue: TShiftState);
begin
  if FShiftButtons=AValue then exit;
  FShiftButtons:=AValue;
  UpdateShiftButons;
end;

procedure TCustomShortCutGrabBox.SetShiftState(const AValue: TShiftState);
var
  s: TShiftStateEnum;
begin
  if FShiftState=AValue then exit;
  FShiftState:=AValue;
  for s:=low(TShiftStateEnum) to High(TShiftStateEnum) do
    if FCheckBoxes[s]<>nil then
      FCheckBoxes[s].Checked:=s in FShiftState;
end;

procedure TCustomShortCutGrabBox.Loaded;
begin
  inherited Loaded;
  UpdateShiftButons;
end;

procedure TCustomShortCutGrabBox.RealSetText(const Value: TCaption);
begin
  // do not allow to set caption
end;

procedure TCustomShortCutGrabBox.UpdateShiftButons;
var
  s: TShiftStateEnum;
  LastCheckBox: TCheckBox;
begin
  if [csLoading,csDestroying]*ComponentState<>[] then exit;
  LastCheckBox:=nil;
  DisableAlign;
  try
    for s:=low(TShiftStateEnum) to High(TShiftStateEnum) do begin
      if s in FShiftButtons then begin
        if FCheckBoxes[s]=nil then begin
          FCheckBoxes[s]:=TCheckBox.Create(Self);
          with FCheckBoxes[s] do begin
            Name:='CheckBox'+ShiftToStr(s);
            Caption:=ShiftToStr(s);
            AutoSize:=true;
            Checked:=s in FShiftState;
            if LastCheckBox<>nil then
              AnchorToNeighbour(akLeft,6,LastCheckBox)
            else
              AnchorParallel(akLeft,0,Self);
            AnchorParallel(akTop,0,Self);
            AnchorParallel(akBottom,0,Self);
            Parent:=Self;
            OnClick:=@OnShitCheckBoxClick;
          end;
        end;
        LastCheckBox:=FCheckBoxes[s];
      end else begin
        FreeAndNil(FCheckBoxes[s]);
      end;
    end;
    if LastCheckBox<>nil then
      FKeyComboBox.AnchorToNeighbour(akLeft,6,LastCheckBox)
    else
      FKeyComboBox.AnchorParallel(akLeft,0,Self);
  finally
    EnableAlign;
  end;
end;

procedure TCustomShortCutGrabBox.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  s: TShiftStateEnum;
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if AComponent=FGrabButton then
      FGrabButton:=nil;
    if AComponent=FKeyComboBox then
      FKeyComboBox:=nil;
    if AComponent=FGrabForm then
      FGrabForm:=nil;
    for s:=Low(TShiftStateEnum) to High(TShiftStateEnum) do
      if FCheckBoxes[s]=AComponent then begin
        FCheckBoxes[s]:=nil;
        Exclude(FShiftButtons,s);
      end;
  end;
end;

function TCustomShortCutGrabBox.ShiftToStr(s: TShiftStateEnum): string;
begin
  case s of
  ssShift: Result:='Shift';
  ssAlt: Result:='Alt';
  ssCtrl: Result:='Ctrl';
  ssMeta: Result:='Meta';
  ssSuper: Result:='Super';
  ssHyper: {$IFDEF Darwin}
           Result:='Cmd';
           {$ELSE}
           Result:='Hyper';
           {$ENDIF}
  ssAltGr: Result:='AltGr';
  ssCaps: Result:='Caps';
  ssNum: Result:='Numlock';
  ssScroll: Result:='Scroll';
  else Result:='Modifier'+IntToStr(ord(s));
  end;
end;

constructor TCustomShortCutGrabBox.Create(TheOwner: TComponent);
var
  i: Integer;
  s: String;
begin
  inherited Create(TheOwner);

  FAllowedShifts:=[ssShift, ssAlt, ssCtrl,
    ssMeta, ssSuper, ssHyper, ssAltGr,
    ssCaps, ssNum, ssScroll];

  FGrabButton:=TButton.Create(Self);
  with FGrabButton do begin
    Name:='GrabButton';
    Caption:=srGrabKey;
    Align:=alRight;
    AutoSize:=true;
    Parent:=Self;
    OnClick:=@OnGrabButtonClick;
  end;

  FKeyComboBox:=TComboBox.Create(Self);
  with FKeyComboBox do begin
    Name:='FKeyComboBox';
    AutoSize:=true;
    Items.BeginUpdate;
    for i:=0 to 145 do begin
      s := KeyAndShiftStateToKeyString(i, []);
      if not KeyStringIsIrregular(s) then
        Items.Add(s);
    end;
    Items.EndUpdate;
    OnEditingDone:=@OnKeyComboboxEditingDone;
    Parent:=Self;
    AnchorToNeighbour(akRight,6,FGrabButton);
    AnchorVerticalCenterTo(FGrabButton);
    Constraints.MinWidth:=130;
  end;

  BevelOuter:=bvNone;
  ShiftButtons:=GetDefaultShiftButtons;
  ShiftState:=[];
  Key:=VK_UNKNOWN;
  KeyComboBox.Text:=KeyAndShiftStateToKeyString(Key,[]);
end;

function TCustomShortCutGrabBox.GetDefaultShiftButtons: TShiftState;
begin
  {$IFDEF Darwin}
  Result:=[ssCtrl,ssShift,ssAlt,ssMeta];
  {$ELSE}
  Result:=[ssCtrl,ssShift,ssAlt];
  {$ENDIF}
end;

initialization

  InitPropEdits;

finalization
  FinalPropEdits;

end.

