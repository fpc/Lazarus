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
    -TIntegerSet missing -> taking my own

    -many more... see XXX
}
unit PropEdits;

{$mode objfpc}{$H+}

// This unit contains a lot of base type conversions. Disable range checking.
{$R-}

interface

uses
  Classes, TypInfo, SysUtils, LResources,
  FPCAdds, // for StrToQWord in older fpc versions
  LCLProc, Forms, Controls, GraphType,
  Graphics, StdCtrls, Buttons, ComCtrls, Menus, LCLType, ExtCtrls, LCLIntf,
  Dialogs, Grids, EditBtn, PropertyStorage, TextTools, FrmSelectProps,
  StringsPropEditDlg, ColumnDlg, FileUtil, ObjInspStrConsts, IDEImagesIntf;

const
  MaxIdentLength: Byte = 63;

type
  TGetStringProc = procedure(const s:ansistring) of object;

  TPersistentSelectionList = class;

{ TPropertyEditor
  Edits a property of a component, or list of components, selected into the
  Object Inspector.  The property editor is created based on the type of the
  property being edited as determined by the types registered by
  RegisterPropertyEditor.  The Object Inspector uses a TPropertyEditor
  for all modification to a property. GetName and GetValue are called to
  display the name and value of the property. SetValue is called whenever the
  user requests to change the value.  Edit is called when the user
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
        paReadOnly:     Value is not allowed to change. But if paDialog is set
                        a Dialog can change the value. This disbales only the
                        edit and combobox in the object inspector.
        paRevertable:   Allows the property to be reverted to the original
                        value.  Things that shouldn't be reverted are nested
                        properties (e.g. Fonts) and elements of a composite
                        property such as set element values.
        paFullWidthName:Tells the object inspector that the value does not
                        need to be rendered and as such the name should be
                        rendered the full width of the inspector.
        paVolatileSubProperties: Any change of property value causes any shown
                        subproperties to be recollected.
        paDisableSubProperties: All subproperties are readonly
                        (not even via Dialog).
        paReference:    property contains a reference to something else.  When
                        used in conjunction with paSubProperties the referenced
                        object should be displayed as sub properties to this
                        property.
        paNotNestable:  Indicates that the property is not safe to show when
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
      This is the configuration directory of lazarus.
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
    procedure Edit; virtual;
    function GetAttributes: TPropertyAttributes; virtual;
    function IsReadOnly: boolean; virtual;
    function GetComponent(Index: Integer): TPersistent;// for Delphi compatibility
    function GetUnitName(Index: Integer = 0): string;
    function GetPropTypeUnitName(Index: Integer = 0): string;
    function GetPropertyPath(Index: integer = 0): string;// e.g. 'TForm1.Color'
    function GetEditLimit: Integer; virtual;
    function GetName: shortstring; virtual;
    procedure GetProperties(Proc: TGetPropEditProc); virtual;
    function GetPropType: PTypeInfo;
    function GetPropInfo: PPropInfo;
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
    function GetValue: ansistring; virtual;
    function GetHint(HintType: TPropEditHint; x, y: integer): string; virtual;
    function GetDefaultValue: ansistring; virtual;
    function GetVisualValue: ansistring;
    procedure GetValues(Proc: TGetStringProc); virtual;
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
    procedure Modified;
    function ValueAvailable: Boolean;
    procedure ListMeasureWidth(const AValue: ansistring; Index:integer;
                               ACanvas:TCanvas; var AWidth: Integer); dynamic;
    procedure ListMeasureHeight(const AValue: ansistring; Index:integer;
                                ACanvas:TCanvas; var AHeight: Integer); dynamic;
    procedure ListDrawValue(const AValue: ansistring; Index:integer;
                            ACanvas:TCanvas; const ARect: TRect;
                            AState: TPropEditDrawState); dynamic;
    procedure PropMeasureHeight(const NewValue: ansistring;  ACanvas: TCanvas;
                                var AHeight:Integer); dynamic;
    procedure PropDrawName(ACanvas: TCanvas; const ARect:TRect;
                           AState: TPropEditDrawState); dynamic;
    procedure PropDrawValue(ACanvas:TCanvas; const ARect:TRect;
                            AState:TPropEditDrawState); dynamic;
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
  The base class of all ordinal property editors.  It establishes that ordinal
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
  type (i.e. Integer, Word, 1..10, etc.).  Restricts the value entered into
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
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TBoolPropertyEditor
  Default property editor for all boolean properties }

  { TBoolPropertyEditor }

  TBoolPropertyEditor = class(TEnumPropertyEditor)
  public
    function OrdValueToVisualValue(OrdValue: longint): string; override;
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

{ TNestedPropertyEditor
  A property editor that uses the PropertyHook, PropList and PropCount.
  The constructor and destructor do not call inherited, but all derived classes
  should.  This is useful for properties like the TSetElementPropertyEditor. }

  TNestedPropertyEditor = class(TPropertyEditor)
  private
    FParentEditor: TPropertyEditor;
  public
    constructor Create(Parent: TPropertyEditor); overload;
    destructor Destroy; override;
    property ParentEditor: TPropertyEditor read FParentEditor;
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
    constructor Create(Parent: TPropertyEditor; AElement: Integer); overload;
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
    function OrdValueToVisualValue(OrdValue: longint): string; override;
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
    class function GetDefaultMethodName(Root, Component: TComponent;
        const RootClassName, ComponentName, PropName: shortstring): shortstring;
  end;
  
{ TPersistentPropertyEditor
  A base editor for TPersistent.  It does allow editing of the properties.
  It allows the user to set the value of this property to point to a component
  in the same form that is type compatible with the property being edited
  (e.g. the ActiveControl property). }

  TPersistentPropertyEditor = class(TPropertyEditor)
  protected
    function FilterFunc(const ATestEditor: TPropertyEditor): Boolean;
    function GetPersistentReference: TPersistent; virtual;
    function GetSelections: TPersistentSelectionList; virtual;
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

{ TComponentPropertyEditor
  The default editor for TComponents.  It does allow editing of the
  properties of the component.  It allows the user to set the value of this
  property to point to a component in the same form that is type compatible
  with the property being edited (e.g. the ActiveControl property). }

  TComponentPropertyEditor = class(TPersistentPropertyEditor)
  protected
    function GetComponentReference: TComponent; virtual;
  public
    function AllEqual: Boolean; override;
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
    function GetSelections: TPersistentSelectionList{IDesignerSelections}; override;
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
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue:ansistring); override;
  end;

{ TShortCutPropertyEditor
  Property editor the ShortCut property.  Allows both typing in a short
  cut value or picking a short-cut value from a list. }

  TShortCutPropertyEditor = class(TOrdinalPropertyEditor)
  public
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
    procedure GetValues(Proc: TGetStringProc); override;
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
    procedure GetValues(Proc: TGetStringProc); override;
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
      Proc: TGetStringProc); virtual;
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
      Proc: TGetStringProc); override;
    procedure SetElementValue(Element: TListElementPropertyEditor;
      NewValue: ansistring); override;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
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

    PersistentClass
      Type of the persistent object to which to restrict this type editor. This
      parameter can be left nil which will mean this type editor applies to all
      properties of PropertyEditorType.

    PropertyEditorName
      The name of the property to which to restrict this type editor.  This
      parameter is ignored if PersistentClass is nil.  This parameter can be
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
  TPropertyEditorMapperFunc=function(Obj:TPersistent;
    PropInfo:PPropInfo):TPropertyEditorClass;
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

//==============================================================================
{
  The TPersistentSelectionList is simply a list of TPersistent references.
  It will never create or free any object. It is used by the property
  editors, the object inspector and the form editor.
}
type

  { TPersistentSelectionList }

  TPersistentSelectionList = class
  protected
    FUpdateLock: integer;
    FPersistentList: TFPList;
    function GetItems(AIndex: integer): TPersistent;
    procedure SetItems(AIndex: integer; const APersistent: TPersistent);
    function GetCount: integer;
    function GetCapacity:integer;
    procedure SetCapacity(const NewCapacity:integer);
  public
    constructor Create;
    destructor Destroy;  override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function UpdateLock: integer;
    function IndexOf(APersistent: TPersistent): integer;
    procedure Clear;
    function IsEqual(SourceSelectionList: TPersistentSelectionList): boolean;
    procedure SortLike(SortedList: TPersistentSelectionList);
    property Count:integer read GetCount;
    property Capacity:integer read GetCapacity write SetCapacity;
    function Add(APersistent: TPersistent): integer;
    function Remove(APersistent: TPersistent): integer;
    procedure Delete(Index: Integer);
    procedure Assign(SourceSelectionList: TPersistentSelectionList);
    property Items[AIndex: integer]: TPersistent read GetItems write SetItems; default;
    procedure WriteDebugReport;
  end;

  TBackupComponentList = class
  private
    FComponentList: TList;
    FLookupRoot: TPersistent;
    FSelection: TPersistentSelectionList;
    function GetComponents(Index: integer): TComponent;
    procedure SetComponents(Index: integer; const AValue: TComponent);
    procedure SetLookupRoot(const AValue: TPersistent);
    procedure SetSelection(const AValue: TPersistentSelectionList);
  protected
  public
    constructor Create;
    destructor Destroy;  override;
    function IndexOf(AComponent: TComponent): integer;
    procedure Clear;
    function ComponentCount: integer;
    function IsEqual(ALookupRoot: TPersistent;
                     ASelection: TPersistentSelectionList): boolean;
  public
    property LookupRoot: TPersistent read FLookupRoot write SetLookupRoot;
    property Components[Index: integer]: TComponent read GetComponents write SetComponents;
    property Selection: TPersistentSelectionList read FSelection write SetSelection;
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
  TPropHookCreateMethod = function(const Name: ShortString; ATypeInfo: PTypeInfo;
      APersistent: TPersistent; const APropertyPath: string): TMethod of object;
  TPropHookGetMethodName = function(const Method: TMethod;
                                    CheckOwner: TObject): ShortString of object;
  TPropHookGetMethods = procedure(TypeData:PTypeData; Proc:TGetStringProc) of object;
  TPropHookMethodExists = function(const Name:ShortString; TypeData: PTypeData;
                 var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean
                 ):boolean of object;
  TPropHookRenameMethod = procedure(const CurName, NewName:ShortString) of object;
  TPropHookShowMethod = procedure(const Name:ShortString) of object;
  TPropHookMethodFromAncestor = function(const Method:TMethod):boolean of object;
  TPropHookChainCall = procedure(const AMethodName, InstanceName,
                      InstanceMethod:ShortString; TypeData:PTypeData) of object;
  // components
  TPropHookGetComponent = function(const Name:ShortString):TComponent of object;
  TPropHookGetComponentName = function(AComponent:TComponent):ShortString of object;
  TPropHookGetComponentNames = procedure(TypeData:PTypeData;
                                         Proc:TGetStringProc) of object;
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
                                      Proc:TGetStringProc) of object;
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
    htGetMethods,
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
    function GetMethodName(const Method: TMethod; CheckOwner: TObject): ShortString;
    procedure GetMethods(TypeData: PTypeData; Proc: TGetStringProc);
    function MethodExists(const Name: ShortString; TypeData: PTypeData;
      var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;
    procedure RenameMethod(const CurName, NewName: ShortString);
    procedure ShowMethod(const Name: ShortString);
    function MethodFromAncestor(const Method: TMethod):boolean;
    procedure ChainCall(const AMethodName, InstanceName,
                        InstanceMethod: ShortString;  TypeData: PTypeData);
    // components
    function GetComponent(const Name: ShortString):TComponent;
    function GetComponentName(AComponent: TComponent): ShortString;
    procedure GetComponentNames(TypeData:PTypeData; const Proc:TGetStringProc);
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
    procedure GetObjectNames(TypeData: PTypeData; const Proc: TGetStringProc);
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
    procedure AddHandlerGetMethods(const OnGetMethods: TPropHookGetMethods);
    procedure RemoveHandlerGetMethods(const OnGetMethods: TPropHookGetMethods);
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
    procedure AddHandlerComponentRenamed(
                           const OnComponentRenamed: TPropHookComponentRenamed);
    procedure RemoveHandlerComponentRenamed(
                           const OnComponentRenamed: TPropHookComponentRenamed);
    // persistent selection
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

function GetLookupRootForComponent(APersistent: TPersistent): TPersistent;

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


type
  TStringsPropEditorDlg = class(TStringsPropEditorFrm)
  public
    Editor: TPropertyEditor;
  end;

//==============================================================================


// Global flags:
var
  GReferenceExpandable: Boolean;
  GShowReadOnlyProps: Boolean;

// default Hook - set by IDE
var
  GlobalDesignHook: TPropertyEditorHook;

function ClassTypeInfo(Value: TClass): PTypeInfo;
function GetClassUnitName(Value: TClass): string;
procedure CreateComponentEvent(AComponent: TComponent; const EventName: string);


//==============================================================================
// XXX
// This class is a workaround for the broken typeinfo function
type

  { TDummyClassForPropTypes }

  TDummyClassForPropTypes = class(TPersistent)
  private
    FAnchorSide: TAnchorSide;
    FDateTime: TDateTime;
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
  public
    function PTypeInfos(const PropName:shortstring): PTypeInfo;
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
    property DateTime: TDateTime read FDateTime;
    property AnchorSide: TAnchorSide read FAnchorSide;
  end;


procedure WritePublishedProperties(Instance: TPersistent);


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

procedure WritePublishedProperties(Instance: TPersistent);
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  PropInfo: PPropInfo;
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
    PropInfo:=PPropInfo(PByte(@TypeData^.UnitName)+Length(TypeData^.UnitName)+1);
    // read property count
    CurCount:=PWord(PropInfo)^;
    inc(PtrUInt(PropInfo),SizeOf(Word));
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
      PropInfo:=PPropInfo(pointer(@PropInfo^.Name)+PByte(@PropInfo^.Name)^+1);
      dec(CurCount);
    end;
    TypeInfo:=TypeData^.ParentInfo;
    if TypeInfo=nil then break;
  until false;
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
    TQWordPropertyEditor,  // tkQWord
    nil,                   // tkDynArray
    nil                    // tkInterfaceRaw
    );

// -----------------------------------------------------------

function AlignToPtr(const p: Pointer): Pointer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(p, SizeOf(Pointer));
{$ELSE}
  Result := p;
{$ENDIF}
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

procedure TPropertyEditor.GetValues(Proc:TGetStringProc);
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
  if PropertyHook<>nil then
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
      with FPropList^[I] do SetMethodProp(Instance,PropInfo,NewValue);
    Modified;
  end;
end;

procedure TPropertyEditor.SetOrdValue(const NewValue:Longint);
var
  I:Integer;
  Changed: boolean;
begin
  Changed:=false;
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do
      Changed:=Changed or (GetOrdProp(Instance,PropInfo)<>NewValue);
  if Changed then begin
    for I:=0 to FPropCount-1 do
      with FPropList^[I] do SetOrdProp(Instance,PropInfo,NewValue);
    Modified;
  end;
end;

procedure TPropertyEditor.SetPtrValue(const NewValue:Pointer);
var
  I:Integer;
  Changed: boolean;
begin
  Changed:=false;
  for I:=0 to FPropCount-1 do
    with FPropList^[I] do
      Changed:=Changed or (GetOrdProp(Instance,PropInfo)<>PtrInt(PtrUInt(NewValue)));
  if Changed then begin
    for I:=0 to FPropCount-1 do
      with FPropList^[I] do SetOrdProp(Instance,PropInfo,PtrInt(PtrUInt(NewValue)));
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
    SystemFont := False;
  end;
  If (pedsInComboList in AState) and not (pedsInEdit in AState)
  then begin
    OldColor := ACanvas.Color;
    If pedsSelected in AState then begin
      ACanvas.Color := clHighlight;
      ACanvas.Font.Color := clHighlightText;
    end
    else begin
      ACanvas.Color := clwhite{clWindow};
      ACanvas.Font.Color := clWindowText;
    end;
    ACanvas.FillRect(ARect);
    ACanvas.Color := OldColor;
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
  Result:=(inherited GetAttributes)+[paHasDefaultValue];
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
  Result:=[paMultiSelect,paValueList,paSortList,paRevertable,paHasDefaultValue];
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

function TBoolPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
begin
  if OrdValue = 0 then
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
    Include(S, FElement)
  else
    Exclude(S, FElement);
  SetOrdValue(Integer(S));
end;

{ TSetPropertyEditor }

function TSetPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect,paSubProperties,paReadOnly,paRevertable,
             paHasDefaultValue];
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
  TheList: TList;
begin
  TheList:=TList(GetObjectValue);
  if (TheList<>nil) and (TheList is TList) then
    Result:=TheList.Count
  else
    Result:=0;
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


type
  { TCollectionPropertyEditor }
  
  TCollectionPropertyEditorForm = class(TForm)
    CollectionListBox: TListBox;
    ToolBar1: TToolBar;
    AddButton: TToolButton;
    DeleteButton: TToolButton;
    ToolButton3: TToolButton;
    MoveUpButton: TToolButton;
    MoveDownButton: TToolButton;
    procedure AddButtonClick(Sender: TObject);
    procedure CollectionListBoxClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
  private
    FCollection: TCollection;
    FOwnerPersistent: TPersistent;
    FPropertyName: String;
  protected
    procedure UpdateCaption;
    procedure UpdateButtons;
    procedure ComponentRenamed(AComponent: TComponent);
    procedure PersistentDeleting(APersistent: TPersistent);
    procedure RefreshPropertyValues;
  public
    procedure FillCollectionListBox;
    procedure SelectInObjectInspector(UnselectAll: Boolean);
    procedure SetCollection(NewCollection: TCollection;
                    NewOwnerPersistent: TPersistent; const NewPropName: String);
    procedure Modified;
  public
    property Collection: TCollection read FCollection;
    property OwnerPersistent: TPersistent read FOwnerPersistent;
    property PropertyName: String read FPropertyName;
  end;

const
  CollectionForm: TCollectionPropertyEditorForm = nil;

procedure TCollectionPropertyEditorForm.FormCreate(Sender: TObject);
begin
  ToolBar1.Images := IDEImages.Images_16;
  AddButton.Caption := oiColEditAdd;
  DeleteButton.Caption := oiColEditDelete;
  MoveUpButton.Caption := oiColEditUp;
  MoveDownButton.Caption := oiColEditDown;
  AddButton.ImageIndex := IDEImages.LoadImage(16, 'add');
  DeleteButton.ImageIndex := IDEImages.LoadImage(16, 'delete');
  MoveUpButton.ImageIndex := IDEImages.LoadImage(16, 'arrow_up');
  MoveDownButton.ImageIndex := IDEImages.LoadImage(16, 'arrow_down');
end;

procedure TCollectionPropertyEditorForm.FormDestroy(Sender: TObject);
begin
  if GlobalDesignHook <> nil then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TCollectionPropertyEditorForm.MoveDownButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if I >= Collection.Count - 1 then Exit;

  Collection.Items[I].Index := I + 1;
  CollectionListBox.ItemIndex := I + 1;

  FillCollectionListBox;
  SelectInObjectInspector(False);
  Modified;
end;

procedure TCollectionPropertyEditorForm.MoveUpButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if I < 0 then Exit;

  Collection.Items[I].Index := I - 1;
  CollectionListBox.ItemIndex := I - 1;

  FillCollectionListBox;
  SelectInObjectInspector(False);
  Modified;
end;

procedure TCollectionPropertyEditorForm.AddButtonClick(Sender: TObject);
begin
  if Collection = nil then Exit;
  Collection.Add;

  FillCollectionListBox;
  if CollectionListBox.Items.Count > 0 then
    CollectionListBox.ItemIndex := CollectionListBox.Items.Count - 1;
  SelectInObjectInspector(False);
  UpdateButtons;
  UpdateCaption;
  Modified;
end;

procedure TCollectionPropertyEditorForm.CollectionListBoxClick(Sender: TObject);
begin
  UpdateButtons;
  UpdateCaption;
  SelectInObjectInspector(False);
end;

procedure TCollectionPropertyEditorForm.DeleteButtonClick(Sender: TObject);
var
  I : Integer;
  NewItemIndex: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if (I >= 0) and (I < Collection.Count) then
  begin
    if MessageDlg(oisConfirmDelete,
      Format(oisDeleteItem, ['"', Collection.Items[I].DisplayName, '"']),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      // select other item, or unselect
      NewItemIndex := I + 1;
      while (NewItemIndex < CollectionListBox.Items.Count)
      and (CollectionListBox.Selected[NewItemIndex]) do Inc(NewItemIndex);

      if NewItemIndex = CollectionListBox.Items.Count then
      begin
        NewItemIndex := 0;
        while (NewItemIndex < Pred(I))
        and not (CollectionListBox.Selected[NewItemIndex]) do Inc(NewItemIndex);

        if NewItemIndex = I then NewItemIndex := -1;
      end;

      CollectionListBox.ItemIndex := -1;

      if NewItemIndex > I then Dec(NewItemIndex);
      //debugln('TCollectionPropertyEditorForm.DeleteClick A NewItemIndex=',dbgs(NewItemIndex),' ItemIndex=',dbgs(CollectionListBox.ItemIndex),' CollectionListBox.Items.Count=',dbgs(CollectionListBox.Items.Count),' Collection.Count=',dbgs(Collection.Count));
      // unselect all items in OI (collections can act strange on delete)
      SelectInObjectInspector(True);
      // now delete
      Collection.Items[I].Free;
      // update listbox after whatever happened
      FillCollectionListBox;
      // set NewItemIndex
      if NewItemIndex < CollectionListBox.Items.Count then
      begin
        CollectionListBox.ItemIndex := NewItemIndex;
        SelectInObjectInspector(False);
      end;
      //debugln('TCollectionPropertyEditorForm.DeleteClick B');
      Modified;
    end;
  end;
  UpdateButtons;
  UpdateCaption;
end;

procedure TCollectionPropertyEditorForm.UpdateCaption;
var
  NewCaption: String;
begin
  //I think to match Delphi this should be formatted like
  //"Editing ComponentName.PropertyName[Index]"
  if OwnerPersistent is TComponent then
    NewCaption := TComponent(OwnerPersistent).Name
  else
    if OwnerPersistent <> nil then
      NewCaption := OwnerPersistent.GetNamePath
    else
      NewCaption := '';

  if NewCaption <> '' then NewCaption := NewCaption + '.';
  NewCaption := oiColEditEditing + ' ' + NewCaption + PropertyName;

  if CollectionListBox.ItemIndex > -1 then
    NewCaption := NewCaption + '[' + IntToStr(CollectionListBox.ItemIndex) + ']';
  Caption := NewCaption;
end;

procedure TCollectionPropertyEditorForm.UpdateButtons;
var
  I: Integer;
begin
  I := CollectionListBox.ItemIndex;
  AddButton.Enabled := Collection <> nil;
  DeleteButton.Enabled := I > -1;
  MoveUpButton.Enabled := I > 0;
  MoveDownButton.Enabled := (I >= 0) and (I < Collection.Count - 1);
end;

procedure TCollectionPropertyEditorForm.ComponentRenamed(AComponent: TComponent);
begin
  if AComponent = OwnerPersistent then UpdateCaption;
end;

procedure TCollectionPropertyEditorForm.PersistentDeleting(APersistent: TPersistent);
var
  OldCollection: TCollection;
begin
  //debugln('TCollectionPropertyEditorForm.PersistentDeleting A APersistent=',dbgsName(APersistent),' OwnerPersistent=',dbgsName(OwnerPersistent));
  if APersistent = OwnerPersistent then
  begin
    OldCollection := Collection;
    SetCollection(nil, nil, '');
    GlobalDesignHook.Unselect(OldCollection);
    if GlobalDesignHook.LookupRoot = OldCollection then
      GlobalDesignHook.LookupRoot := nil;

    Hide;
  end;
end;

procedure TCollectionPropertyEditorForm.RefreshPropertyValues;
begin
  FillCollectionListBox;
end;

procedure TCollectionPropertyEditorForm.FillCollectionListBox;
var
  I: Integer;
  CurItem: String;
  Cnt: Integer;
begin
  CollectionListBox.Items.BeginUpdate;
  try
    if Collection <> nil then Cnt := Collection.Count
    else Cnt := 0;

    // add or replace list items
    for I := 0 to Cnt - 1 do
    begin
      CurItem := IntToStr(I) + ' - ' + Collection.Items[I].DisplayName;
      if I >= CollectionListBox.Items.Count then
        CollectionListBox.Items.Add(CurItem)
      else
        CollectionListBox.Items[I] := CurItem;
    end;

    // delete unneeded list items
    if Cnt > 0 then
    begin
      while CollectionListBox.Items.Count > Cnt do
      begin
        CollectionListBox.Items.Delete(CollectionListBox.Items.Count - 1);
      end;
    end
    else
    begin
      CollectionListBox.Items.Clear;
    end;
  finally
    CollectionListBox.Items.EndUpdate;
    UpdateButtons;
    UpdateCaption;
  end;
end;

procedure TCollectionPropertyEditorForm.SelectInObjectInspector(UnselectAll: Boolean);
var
  I: Integer;
  NewSelection: TPersistentSelectionList;
begin
  if Collection = nil then Exit;
  // select in OI
  NewSelection := TPersistentSelectionList.Create;
  try
    if not UnselectAll then
    begin
      for I := 0 to CollectionListBox.Items.Count - 1 do
        if CollectionListBox.Selected[I] then
          NewSelection.Add(Collection.Items[I]);
    end;
    GlobalDesignHook.SetSelection(NewSelection);
    GlobalDesignHook.LookupRoot := GetLookupRootForComponent(OwnerPersistent);
  finally
    NewSelection.Free;
  end;
end;

procedure TCollectionPropertyEditorForm.SetCollection(NewCollection: TCollection;
  NewOwnerPersistent: TPersistent; const NewPropName: String);
begin
  if (FCollection = NewCollection) and (FOwnerPersistent = NewOwnerPersistent)
    and (FPropertyName = NewPropName) then Exit;

  FCollection := NewCollection;
  FOwnerPersistent := NewOwnerPersistent;
  FPropertyName := NewPropName;
  //debugln('TCollectionPropertyEditorForm.SetCollection A Collection=',dbgsName(FCollection),' OwnerPersistent=',dbgsName(OwnerPersistent),' PropName=',PropertyName);
  if GlobalDesignHook <> nil then
  begin
    if FOwnerPersistent <> nil then
    begin
      GlobalDesignHook.AddHandlerComponentRenamed(@ComponentRenamed);
      GlobalDesignHook.AddHandlerPersistentDeleting(@PersistentDeleting);
      GlobalDesignHook.AddHandlerRefreshPropertyValues(@RefreshPropertyValues);
    end
    else
    begin
      GlobalDesignHook.RemoveAllHandlersForObject(Self);
    end;
  end;

  FillCollectionListBox;
  UpdateCaption;
end;

procedure TCollectionPropertyEditorForm.Modified;
begin
  GlobalDesignHook.Modified(Self);
end;

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
  Collection:=TCollection(GetObjectValue);
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
var
  TheCollection: TCollection;
begin
  TheCollection := TCollection(GetObjectValue);
  if TheCollection=nil then
    raise Exception.Create('Collection=nil');
  If CollectionForm=nil then
    CollectionForm := TCollectionPropertyEditorForm.Create(Application);
  CollectionForm.SetCollection(TheCollection,GetComponent(0),GetName);
  CollectionForm.EnsureVisible;
end;

{ TClassPropertyEditor }

function TClassPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

procedure TClassPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
  SubItem: TPersistent;
  Selection: TPersistentSelectionList;
begin
  Selection := TPersistentSelectionList.Create;
  try
    for I := 0 to PropCount - 1 do begin
      SubItem := TPersistent(GetObjectValueAt(I));
      if SubItem<>nil then
        Selection.Add(SubItem);
    end;
    GetPersistentProperties(Selection,tkProperties,PropertyHook,Proc,nil);
  finally
    Selection.Free;
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
var
  FormMethodName: shortstring;
begin
  FormMethodName := GetValue;
  DebugLn('### TMethodPropertyEditor.Edit A OldValue=',FormMethodName);
  if (not IsValidIdent(FormMethodName))
  or PropertyHook.MethodFromAncestor(GetMethodValue) then begin
    if not IsValidIdent(FormMethodName) then
      FormMethodName := GetFormMethodName;
    DebugLn('### TMethodPropertyEditor.Edit B FormMethodName=',FormMethodName);
    if not IsValidIdent(FormMethodName) then begin
      raise EPropertyError.Create('Method name must be an identifier'{@SCannotCreateName});
      exit;
    end;
    SetValue(FormMethodName); // this will jump to the method
    PropertyHook.RefreshPropertyValues;
  end else
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
// returns the default name for a new method
var I: Integer;
  Root: TPersistent;
begin
  Result:='';
  if PropertyHook.LookupRoot=nil then exit;
  if GetComponent(0) = PropertyHook.LookupRoot then begin
    Root:=PropertyHook.LookupRoot;
    if Root is TCustomForm then
      Result:='Form'
    else if Root is TDataModule then
      Result:='DataModule'
    else begin;
      Result := PropertyHook.GetRootClassName;
      if (Result <> '') and (Result[1] = 'T') then
        System.Delete(Result, 1, 1);
    end;
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
      Result:='Form'
    else if Root is TDataModule then
      Result:='DataModule'
    else begin;
      Result := RootClassName;
      if (Result <> '') and (Result[1] = 'T') then
        System.Delete(Result, 1, 1);
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
  Result:=PropertyHook.GetMethodName(GetMethodValue,nil);
end;

procedure TMethodPropertyEditor.GetValues(Proc: TGetStringProc);
begin
  //DebugLn('### TMethodPropertyEditor.GetValues');
  Proc(oisNone);
  PropertyHook.GetMethods(GetTypeData(GetPropType), Proc);
end;

procedure TMethodPropertyEditor.SetValue(const NewValue: ansistring);
var
  CreateNewMethod: Boolean;
  CurValue: ansistring;
  //OldMethod: TMethod;
  NewMethodExists,NewMethodIsCompatible,NewMethodIsPublished,
  NewIdentIsMethod: boolean;
begin
  CurValue:=GetValue;
  if CurValue=NewValue then exit;
  //DebugLn('### TMethodPropertyEditor.SetValue A OldValue="',CurValue,'" NewValue=',NewValue);
  NewMethodExists:=IsValidIdent(NewValue)
               and PropertyHook.MethodExists(NewValue,GetTypeData(GetPropType),
                   NewMethodIsCompatible,NewMethodIsPublished,NewIdentIsMethod);
  //writeln('### TMethodPropertyEditor.SetValue B NewMethodExists=',NewMethodExists,' NewMethodIsCompatible=',NewMethodIsCompatible,' ',NewMethodIsPublished,' ',NewIdentIsMethod);
  if NewMethodExists then begin
    if not NewIdentIsMethod then begin
      if MessageDlg('Incompatible Identifier',
        'The identifier "'+NewValue+'" is not a method.'#13
        +'Press Cancel to undo,'#13
        +'press Ignore to force it.',mtWarning,[mbCancel,mbIgnore],0)<>mrIgnore
      then
        exit;
    end;
    if not NewMethodIsPublished then begin
      if MessageDlg('Incompatible Method',
        'The method "'+NewValue+'" is not published.'#13
        +'Press Cancel to undo,'#13
        +'press Ignore to force it.',mtWarning,[mbCancel,mbIgnore],0)<>mrIgnore
      then
        exit;
    end;
    if not NewMethodIsCompatible then begin
      if MessageDlg('Incompatible Method',
        'The method "'+NewValue+'" is incompatible to this event ('+GetName+').'#13
        +'Press Cancel to undo,'#13
        +'press Ignore to force it.',mtWarning,[mbCancel,mbIgnore],0)<>mrIgnore
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
    //   All other not selected properties that use this method, contain just
    //   the TMethod record. So, changing the name in the jitform will change
    //   all other event names in all other components automatically.
    //writeln('### TMethodPropertyEditor.SetValue D');
    PropertyHook.RenameMethod(CurValue, NewValue)
  end else
  begin
    //writeln('### TMethodPropertyEditor.SetValue E');
    CreateNewMethod := IsValidIdent(NewValue) and not NewMethodExists;
    SetMethodValue(
       PropertyHook.CreateMethod(NewValue,GetPropType,
                                 GetComponent(0),GetPropertyPath(0)));
    //writeln('### TMethodPropertyEditor.SetValue F NewValue=',GetValue);
    if CreateNewMethod then begin
      {if (PropCount = 1) and (OldMethod.Data <> nil) and (OldMethod.Code <> nil)
      then
        CheckChainCall(NewValue, OldMethod);}
      //writeln('### TMethodPropertyEditor.SetValue G');
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

function TPersistentPropertyEditor.GetSelections:
  TPersistentSelectionList;
var
  I: Integer;
begin
  Result := nil;
  if (GetPersistentReference <> nil) and AllEqual then
  begin
    Result := TPersistentSelectionList.Create;
    for I := 0 to PropCount - 1 do
      Result.Add(TPersistent(GetObjectValueAt(I)));
  end;
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
    if (Designer.GetShiftState * [ssCtrl, ssLeft] = [ssCtrl, ssLeft]) then
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
    Result := Result + [paValueList, paSortList, paRevertable]
  else
    Result := Result + [paReadOnly];
  if GReferenceExpandable and (GetPersistentReference <> nil) and AllEqual then
    Result := Result + [paSubProperties, paVolatileSubProperties];
end;

procedure TPersistentPropertyEditor.GetProperties(Proc:TGetPropEditProc);
var
  LPersistents: TPersistentSelectionList;
begin
  LPersistents := GetSelections;
  if LPersistents <> nil then
  begin
    GetPersistentProperties(LPersistents, tkAny, PropertyHook, Proc, nil);
  end;
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
  Result:='';
  APersistent:=GetPersistentReference;
  if APersistent is TComponent then begin
    Component:=TComponent(APersistent);
    if Assigned(PropertyHook) then begin
      Result:=PropertyHook.GetComponentName(Component);
    end else begin
      if Assigned(Component) then
        Result:=Component.Name;
    end;
  end else if APersistent<>nil then begin
    Result:='('+APersistent.ClassName+')';
  end;
end;

procedure TPersistentPropertyEditor.GetValues(Proc: TGetStringProc);
begin
  Proc(oisNone);
  if Assigned(PropertyHook) then
    PropertyHook.GetComponentNames(GetTypeData(GetPropType), Proc);
end;

procedure TPersistentPropertyEditor.SetValue(const NewValue: ansistring);
var Component: TComponent;
begin
  if NewValue=GetValue then exit;
  if (NewValue = '') or (NewValue=oisNone) then
    Component := nil
  else begin
    if Assigned(PropertyHook) then begin
      Component := PropertyHook.GetComponent(NewValue);
      if not (Component is GetTypeData(GetPropType)^.ClassType) then begin
        raise EPropertyError.Create(oisInvalidPropertyValue);
      end;
    end;
  end;
  SetPtrValue(Component);
end;

{ TComponentPropertyEditor }

function TComponentPropertyEditor.GetComponentReference: TComponent;
begin
  Result := TComponent(GetObjectValue);
end;

function TComponentPropertyEditor.AllEqual: Boolean;
begin
  Result:=(inherited AllEqual)
          and (FindRootDesigner(GetComponentReference)<>nil);
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

function TInterfaceProperty.GetSelections: TPersistentSelectionList{IDesignerSelections};
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
  Result := [paMultiSelect, paValueList, paRevertable, paHasDefaultValue];
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
  Result := [paMultiSelect, paValueList, paRevertable, paHasDefaultValue];
end;

function TShortCutPropertyEditor.OrdValueToVisualValue(OrdValue: longint
  ): string;
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
  TheDialog : TStringsPropEditorDlg;
  Strings : TStrings;
begin
  Strings:= TStrings(GetObjectValue);
  TheDialog:= CreateDlg(Strings);
  try
    if (TheDialog.ShowModal = mrOK) then begin
      Strings.Text:=TheDialog.Memo.Text;
      Modified;
    end;
  finally
    TheDialog.Free;
  end;
end;

function TStringsPropertyEditor.CreateDlg(s: TStrings): TStringsPropEditorDlg;
begin
  if s=nil then ;
  Result:=TStringsPropEditorDlg.Create(Application);
  Result.Editor:=Self;
  Result.Memo.Text:=s.Text;
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
  AString:= GetStrValue;
  TheDialog:= TStringsPropEditorDlg.Create(nil);
  try
    TheDialog.Editor:=Self;
    TheDialog.Memo.Text:=AString;
    if (TheDialog.ShowModal = mrOK) then begin
      AString:=TheDialog.Memo.Text;
      //erase the last lineending if any
      if copy(AString,length(AString)-length(LineEnding)+1,length(LineEnding))=LineEnding then
        delete(AString, length(AString)-length(LineEnding)+1,length(LineEnding));
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
  Result:=[paMultiSelect,paSortList,paValueList,paRevertable,paHasDefaultValue];
end;

function TCursorPropertyEditor.OrdValueToVisualValue(OrdValue: longint
  ): string;
begin
  Result := CursorToString(TCursor(OrdValue));
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
begin
  inherited Create(AOwner);
  Caption:='Filter editor';
  Height:=295;
  Width:=417;
  Position:=poScreenCenter;
  BorderStyle:=bsDialog;
  StringGrid1:=TStringGrid.Create(Self);
  StringGrid1.ColCount:=2;
  StringGrid1.DefaultColWidth:=190;
  StringGrid1.Options:=StringGrid1.Options + [goEditing, goAlwaysShowEditor];
  StringGrid1.RowCount:= 100;
  StringGrid1.Left:= 8;
  StringGrid1.Height := 248;
  StringGrid1.Top := 8;
  StringGrid1.Width := 408;
  StringGrid1.Parent:=Self;
  StringGrid1.FixedCols := 0;
  with TBitBtn.Create(Self) do
  begin
    Parent:=Self;
    Kind := bkOK;
    Left := 256;
    Height := 25;
    Top := 264;
    Width := 75;
  end;
  with TBitBtn.Create(Self) do
  begin
    Parent:=Self;
    Kind := bkCancel;
    Left := 341;
    Height := 25;
    Top := 264;
    Width := 75;
  end;
  StringGrid1.Cells[0,0]:='Filter name';
  StringGrid1.Cells[1,0]:='Filter';
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
  Result:=[paDialog,paRevertable];
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


{ TPersistentSelectionList }

function TPersistentSelectionList.Add(APersistent: TPersistent): integer;
begin
  Result:=FPersistentList.Add(APersistent);
end;

function TPersistentSelectionList.Remove(APersistent: TPersistent): integer;
begin
  Result:=IndexOf(APersistent);
  if Result>=0 then
    FPersistentList.Delete(Result);
end;

procedure TPersistentSelectionList.Delete(Index: Integer);
begin
  FPersistentList.Delete(Index);
end;

procedure TPersistentSelectionList.Clear;
begin
  FPersistentList.Clear;
end;

constructor TPersistentSelectionList.Create;
begin
  inherited Create;
  FPersistentList:=TFPList.Create;
end;

destructor TPersistentSelectionList.Destroy;
begin
  FreeAndNil(FPersistentList);
  inherited Destroy;
end;

function TPersistentSelectionList.GetCount: integer;
begin
  Result:=FPersistentList.Count;
end;

function TPersistentSelectionList.GetItems(AIndex: integer): TPersistent;
begin
  Result:=TPersistent(FPersistentList[AIndex]);
end;

procedure TPersistentSelectionList.SetItems(AIndex: integer;
  const APersistent: TPersistent);
begin
  FPersistentList[AIndex]:=APersistent;
end;

function TPersistentSelectionList.GetCapacity:integer;
begin
  Result:=FPersistentList.Capacity;
end;

procedure TPersistentSelectionList.SetCapacity(const NewCapacity:integer);
begin
  FPersistentList.Capacity:=NewCapacity;
end;

procedure TPersistentSelectionList.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TPersistentSelectionList.EndUpdate;
begin
  dec(FUpdateLock);
end;

function TPersistentSelectionList.UpdateLock: integer;
begin
  Result:=FUpdateLock;
end;

function TPersistentSelectionList.IndexOf(APersistent: TPersistent): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result]<>APersistent) do dec(Result);
end;

procedure TPersistentSelectionList.Assign(
  SourceSelectionList:TPersistentSelectionList);
var a:integer;
begin
  if SourceSelectionList=Self then exit;
  Clear;
  if (SourceSelectionList<>nil) and (SourceSelectionList.Count>0) then begin
    FPersistentList.Count:=SourceSelectionList.Count;
    for a:=0 to SourceSelectionList.Count-1 do
      FPersistentList[a] := SourceSelectionList[a];
  end;
end;

procedure TPersistentSelectionList.WriteDebugReport;
var
  i: Integer;
begin
  DebugLn(['TPersistentSelectionList.WriteDebugReport Count=',Count]);
  for i:=0 to Count-1 do
    DebugLn(['  ',i,' ',dbgsName(Items[i])]);
end;

function TPersistentSelectionList.IsEqual(
 SourceSelectionList:TPersistentSelectionList):boolean;
var a:integer;
begin
  if (SourceSelectionList=nil) and (Count=0) then begin
    Result:=true;
    exit;
  end;
  Result:=false;
  if FPersistentList.Count<>SourceSelectionList.Count then exit;
  for a:=0 to FPersistentList.Count-1 do
    if Items[a]<>SourceSelectionList[a] then exit;
  Result:=true;
end;

procedure TPersistentSelectionList.SortLike(SortedList: TPersistentSelectionList
  );
// sort this list
var
  NewIndex: Integer;
  j: Integer;
  OldIndex: LongInt;
begin
  NewIndex:=0;
  j:=0;
  while (j<SortedList.Count) do begin
    OldIndex:=IndexOf(SortedList[j]);
    if OldIndex>=0 then begin
      // the j-th element of SortedList exists here
      if OldIndex<>NewIndex then
        FPersistentList.Move(OldIndex,NewIndex);
      inc(NewIndex);
    end;
    inc(j);
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
  CheckOwner: TObject): ShortString;
var
  i: Integer;
begin
  i:=GetHandlerCount(htGetMethodName);
  if GetNextHandlerIndex(htGetMethodName,i) then begin
    Result:=TPropHookGetMethodName(FHandlers[htGetMethodName][i])(Method,CheckOwner);
  end else begin
    // search the method name with the given code pointer
    if Assigned(Method.Code) then begin
      if Method.Data<>nil then begin
        if (CheckOwner<>nil) and (TObject(Method.Data)<>CheckOwner) then
          Result:=''
        else begin
          Result:=TObject(Method.Data).MethodName(Method.Code);
          if Result='' then
            Result:='<Unpublished>';
        end;
      end else
        Result:='<No LookupRoot>';
    end else
      Result:='';
  end;
end;

procedure TPropertyEditorHook.GetMethods(TypeData:PTypeData;
  Proc:TGetStringProc);
var
  i: Integer;
begin
  i:=GetHandlerCount(htGetMethods);
  while GetNextHandlerIndex(htGetMethods,i) do
    TPropHookGetMethods(FHandlers[htGetMethods][i])(TypeData,Proc);
end;

function TPropertyEditorHook.MethodExists(const Name:Shortstring;
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

procedure TPropertyEditorHook.RenameMethod(const CurName, NewName:ShortString);
// rename published method in LookupRoot object and source
var
  i: Integer;
begin
  i:=GetHandlerCount(htRenameMethod);
  while GetNextHandlerIndex(htRenameMethod,i) do
    TPropHookRenameMethod(FHandlers[htRenameMethod][i])(CurName,NewName);
end;

procedure TPropertyEditorHook.ShowMethod(const Name:Shortstring);
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

function TPropertyEditorHook.GetComponent(const Name:Shortstring):TComponent;
var
  i: Integer;
begin
  Result:=nil;
  if not Assigned(LookupRoot) then exit;
  i:=GetHandlerCount(htGetComponent);
  while GetNextHandlerIndex(htGetComponent,i) and (Result=nil) do
    Result:=TPropHookGetComponent(FHandlers[htGetComponent][i])(Name);
  if (Result=nil) and (LookupRoot is TComponent) then
    Result:=TComponent(LookupRoot).FindComponent(Name);
end;

function TPropertyEditorHook.GetComponentName(
  AComponent:TComponent):Shortstring;
var
  i: Integer;
  Handler: TPropHookGetComponentName;
begin
  Result:='';
  if AComponent=nil then exit;
  i:=GetHandlerCount(htGetComponentName);
  while GetNextHandlerIndex(htGetComponentName,i) and (Result='') do begin
    Handler:=TPropHookGetComponentName(FHandlers[htGetComponentName][i]);
    Result:=Handler(AComponent);
  end;
  if Result='' then
    Result:=AComponent.Name;
end;

procedure TPropertyEditorHook.GetComponentNames(TypeData:PTypeData;
  const Proc:TGetStringProc);
var i: integer;
  Handler: TPropHookGetComponentNames;
begin
  if not Assigned(LookupRoot) then exit;
  i:=GetHandlerCount(htGetComponentNames);
  if i>0 then begin
    while GetNextHandlerIndex(htGetComponentNames,i) do begin
      Handler:=TPropHookGetComponentNames(FHandlers[htGetComponentNames][i]);
      Handler(TypeData,Proc);
    end;
  end else if LookupRoot is TComponent then begin
    for i:=0 to TComponent(LookupRoot).ComponentCount-1 do
      if (TComponent(LookupRoot).Components[i] is TypeData^.ClassType) then
        Proc(TComponent(LookupRoot).Components[i].Name);
  end;
end;

function TPropertyEditorHook.GetRootClassName:Shortstring;
var
  i: Integer;
  Handler: TPropHookGetRootClassName;
begin
  Result:='';
  i:=GetHandlerCount(htGetRootClassName);
  while GetNextHandlerIndex(htGetRootClassName,i) and (Result='') do begin
    Handler:=TPropHookGetRootClassName(FHandlers[htGetRootClassName][i]);
    Result:=Handler();
  end;
  if (Result='') and Assigned(LookupRoot) then
    Result:=LookupRoot.ClassName;
end;

function TPropertyEditorHook.BeforeAddPersistent(Sender: TObject;
  APersistentClass: TPersistentClass; Parent: TPersistent): boolean;
var
  i: Integer;
  Handler: TPropHookBeforeAddPersistent;
begin
  i:=GetHandlerCount(htBeforeAddPersistent);
  while GetNextHandlerIndex(htBeforeAddPersistent,i) do begin
    Handler:=TPropHookBeforeAddPersistent(FHandlers[htBeforeAddPersistent][i]);
    Result:=Handler(Sender,APersistentClass,Parent);
    if not Result then exit;
  end;
  Result:=true;
end;

procedure TPropertyEditorHook.ComponentRenamed(AComponent: TComponent);
var
  i: Integer;
begin
  i:=GetHandlerCount(htComponentRenamed);
  while GetNextHandlerIndex(htComponentRenamed,i) do
    TPropHookComponentRenamed(FHandlers[htComponentRenamed][i])(AComponent);
end;

procedure TPropertyEditorHook.PersistentAdded(APersistent: TPersistent;
  Select: boolean);
var
  i: Integer;
begin
  i:=GetHandlerCount(htPersistentAdded);
  while GetNextHandlerIndex(htPersistentAdded,i) do
    TPropHookPersistentAdded(FHandlers[htPersistentAdded][i])(APersistent,Select);
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
  AComponent: TComponent;
  NewLookupRoot: TPersistent;
begin
  // update LookupRoot
  NewLookupRoot:=LookupRoot;
  if (ASelection<>nil) and (ASelection.Count>0) then begin
    APersistent:=ASelection[0];
    if APersistent<>nil then begin
      if (APersistent is TComponent) then begin
        AComponent:=TComponent(APersistent);
        if AComponent.Owner<>nil then
          NewLookupRoot:=AComponent.Owner
        else
          NewLookupRoot:=AComponent;
      end else begin
        NewLookupRoot:=APersistent;
      end;
    end;
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
  Selection:=TPersistentSelectionList.Create;
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
  NewSelection:=TPersistentSelectionList.Create;
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
      Result:=TCollectionItem(Instance).DisplayName;
end;

procedure TPropertyEditorHook.GetObjectNames(TypeData: PTypeData;
  const Proc: TGetStringProc);
var
  i: Integer;
begin
  i:=GetHandlerCount(htGetObjectNames);
  while GetNextHandlerIndex(htGetObjectNames,i) do
    TPropHookGetObjectNames(FHandlers[htGetObjectNames][i])(TypeData,Proc);
end;

procedure TPropertyEditorHook.Modified(Sender: TObject);
var
  i: Integer;
  AForm: TCustomForm;
begin
  i:=GetHandlerCount(htModified);
  while GetNextHandlerIndex(htModified,i) do
    TPropHookModified(FHandlers[htModified][i])(Sender);
  if (FLookupRoot<>nil) and (FLookupRoot is TComponent) then begin
    AForm:=GetDesignerForm(TComponent(FLookupRoot));
    if (AForm<>nil) and (AForm.Designer<>nil) then
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

procedure TPropertyEditorHook.RemoveAllHandlersForObject(const HandlerObject: TObject
  );
var
  HookType: TPropHookType;
begin
  for HookType:=Low(TPropHookType) to High(TPropHookType) do
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
  for HookType:=Low(TPropHookType) to high(TPropHookType) do
    FreeThenNil(FHandlers[HookType]);
  inherited Destroy;
end;


{ TBackupComponentList }

function TBackupComponentList.GetComponents(Index: integer): TComponent;
begin
  Result:=TComponent(FComponentList[Index]);
end;

procedure TBackupComponentList.SetComponents(Index: integer;
  const AValue: TComponent);
begin
  FComponentList[Index]:=AValue;
end;

procedure TBackupComponentList.SetLookupRoot(const AValue: TPersistent);
var
  i: Integer;
begin
  FLookupRoot:=AValue;
  FComponentList.Clear;
  if (FLookupRoot<>nil) and (FLookupRoot is TComponent) then
    for i:=0 to TComponent(FLookupRoot).ComponentCount-1 do
      FComponentList.Add(TComponent(FLookupRoot).Components[i]);
  FSelection.Clear;
end;

procedure TBackupComponentList.SetSelection(
  const AValue: TPersistentSelectionList);
begin
  if FSelection=AValue then exit;
  FSelection.Assign(AValue);
end;

constructor TBackupComponentList.Create;
begin
  FSelection:=TPersistentSelectionList.Create;
  FComponentList:=TList.Create;
end;

destructor TBackupComponentList.Destroy;
begin
  FreeAndNil(FSelection);
  FreeAndNil(FComponentList);
  inherited Destroy;
end;

function TBackupComponentList.IndexOf(AComponent: TComponent): integer;
begin
  Result:=FComponentList.IndexOf(AComponent);
end;

procedure TBackupComponentList.Clear;
begin
  LookupRoot:=nil;
end;

function TBackupComponentList.ComponentCount: integer;
begin
  Result:=FComponentList.Count;
end;

function TBackupComponentList.IsEqual(ALookupRoot: TPersistent;
  ASelection: TPersistentSelectionList): boolean;
var
  i: Integer;
begin
  Result:=false;
  if ALookupRoot<>LookupRoot then exit;
  if not FSelection.IsEqual(ASelection) then exit;
  if (ALookupRoot<>nil) and (FLookupRoot is TComponent) then begin
    if ComponentCount<>TComponent(ALookupRoot).ComponentCount then exit;
    for i:=0 to FComponentList.Count-1 do
      if TComponent(FComponentList[i])<>TComponent(ALookupRoot).Components[i]
      then exit;
  end;
  Result:=true;
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

function GetLookupRootForComponent(APersistent: TPersistent): TPersistent;
begin
  Result:=APersistent;
  if (Result<>nil) and (Result is TComponent)
  and (TComponent(Result).Owner<>nil) then
    Result:=TComponent(Result).Owner;
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
    PersistentList:=TPersistentSelectionList.Create;
    PersistentList.Add(AComponent);
    Hook:=GlobalDesignHook;
    MethodPropEditor:=TMethodPropertyEditor.Create(Hook,1);
    MethodPropEditor.SetPropEntry(0, AComponent, PropInfo);
    MethodPropEditor.Initialize;
    MethodPropEditor.Edit;
  finally
    MethodPropEditor.Free;
    PersistentList.Free;
  end;
end;

Function ClassTypeInfo(Value: TClass): PTypeInfo;
begin
  Result := PTypeInfo(Value.ClassInfo);
end;

procedure InitPropEdits;
begin
  GReferenceExpandable:=true;
  GShowReadOnlyProps:=true;

  PropertyClassList:=TList.Create;
  PropertyEditorMapperList:=TList.Create;
  // register the standard property editors

  // XXX workaround for buggy typinfo function
  // Normally it should use something like this;
  // RegisterPropertyEditor(TypeInfo(TColor),nil,'',TColorPropertyEditor);
  DummyClassForPropTypes:=TDummyClassForPropTypes.Create;
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('AnsiString'),
    nil,'Name',TComponentNamePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TTranslateString'),
    TCustomLabel, 'Caption', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TTranslateString'),
    TCustomStaticText, 'Caption', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TTranslateString'),
    TCustomCheckBox, 'Caption', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TTranslateString'),
    TControl, 'Hint', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('longint'),
    nil,'Tag',TTabOrderPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('shortstring'),
    nil,'',TCaptionPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TStrings'),
    nil,'',TStringsPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('AnsiString'),
    nil,'SessionProperties',TSessionPropertiesPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TModalResult'),
    nil,'ModalResult',TModalResultPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TShortCut'),
    nil,'',TShortCutPropertyEditor);
  //RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TDate'),
  //  nil,'',TDatePropertyEditor);
  //RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TTime'),
  //  nil,'',TTimePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TDateTime'),
    nil,'',TDateTimePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TCursor'),
    nil,'',TCursorPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TComponent),nil
    ,'',TComponentPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TCollection),
    nil,'',TCollectionPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('AnsiString'),
    TFileDialog, 'Filter', TFileDlgFilterProperty);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('AnsiString'),
    TFileNameEdit, 'Filter', TFileDlgFilterProperty);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TAnchorSide'),
    TControl, 'AnchorSideLeft', THiddenPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TAnchorSide'),
    TControl, 'AnchorSideTop', THiddenPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TAnchorSide'),
    TControl, 'AnchorSideRight', THiddenPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TAnchorSide'),
    TControl, 'AnchorSideBottom', THiddenPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('AnsiString'),
    TCustomPropertyStorage, 'Filename', TFileNamePropertyEditor);
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

  // XXX workaround for buggy typeinfo function
  DummyClassForPropTypes.Free;
end;

initialization
  {$I collectionpropeditform.lrs}
  
  InitPropEdits;

finalization
  FinalPropEdits;

end.

