{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
   Provides LCL controls that access properties of TPersistent objects via RTTI
   - the FreePascal Run Time Type Information.
   Every published property can be edited in the Object Inspector. There you
   have a TOIPropertyGrid working with TEdit, TComboBox and TButton.
   These controls extends the possibilities to edit single properties and the
   developer can choose how to represent the property.

  ToDo:
    - ploReadOnly
}
unit RTTICtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, LResources, LCLProc, LCLType, LCLIntf, Forms,
  Controls, Graphics, MaskEdit, Calendar, Spin, Dialogs, CheckLst, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, GraphPropEdits, PropEdits;

type
  { TAliasStrings }
  { Maps strings to alias strings.
    Some RTTI controls uses this to map RTTI values to shown values.
    Eventually accelerate search for Names and Values }

  TAliasStrings = class(TStringList)
  public
    function IndexOfValue(const AValue: string): integer; virtual;
    function ValueAt(Index: integer): string; virtual;
    function ValueToAlias(const AValue: string): string; virtual;
    function AliasToValue(const Alias: string): string; virtual;
  end;
  
  
  { TPropertyLinkNotifier }
  
  TCustomPropertyLink = class;
  
  TPropertyLinkNotifier = class(TComponent)
  private
    FLink: TCustomPropertyLink;
  protected
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
  public
    constructor Create(TheLink: TCustomPropertyLink); reintroduce;
    property Link: TCustomPropertyLink read FLink;
  end;


  { TCustomPropertyLink
    The connection between an RTTI control and a property editor }
    
  TPropertyLinkOption = (
    ploReadOnIdle,
    ploAutoSave // auto save on EditingDone
    //ToDo: ploDisableOnNil // disable control, if link not connected
    //ToDo: ploReadOnly
    );
  TPropertyLinkOptions = set of TPropertyLinkOption;

Const
  DefaultLinkOptions = [ploReadOnIdle,ploAutoSave];
   
Type
  
  TTestEditing = function(Sender: TObject): boolean of object;
  TBeforeWriteProperty = procedure(Sender: TObject; var AllowWrite: boolean) of object;

  { TCustomPropertyLink }

  TCustomPropertyLink = class(TPersistent)
  private
    FAliasValues: TAliasStrings;
    FCollectedValues: TStrings;
    FCollectValues: boolean;
    FEditor: TPropertyEditor;
    FFilter: TTypeKinds;
    FHook: TPropertyEditorHook;
    FIdleHandlerConnected: boolean;
    FLinkNotifier: TPropertyLinkNotifier;
    FOnAfterWrite: TNotifyEvent;
    FOnBeforeWrite: TBeforeWriteProperty;
    FOnEditorChanged: TNotifyEvent;
    FOnLoadFromProperty: TNotifyEvent;
    FOnSaveToProperty: TNotifyEvent;
    FOnTestEditing: TTestEditing;
    FOnTestEditor: TPropertyEditorFilterFunc;
    FOptions: TPropertyLinkOptions;
    FOwner: TComponent;
    FPropertyLoaded: boolean;
    FSaveEnabled: boolean;
    FTIElementName: string;
    FTIObject: TPersistent;
    FTIPropertyName: string;
    procedure SetCollectValues(const AValue: boolean);
    procedure SetEditor(const AValue: TPropertyEditor);
    procedure SetFilter(const AValue: TTypeKinds);
    procedure SetOptions(const NewOptions: TPropertyLinkOptions);
    procedure SetTIElementName(const AValue: string);
    procedure SetTIObject(const AValue: TPersistent);
    procedure SetTIPropertyName(const AValue: string);
  protected
    function GetCanModify: boolean; virtual;
    procedure EditorChanged; virtual;
    procedure SetPropertyEditor(APropertyEditor: TPropertyEditor); virtual;
    function CheckPropInfo(const APropInfo: PPropInfo): boolean; virtual;
    procedure CreateHook; virtual;
    procedure UpdateIdleHandler; virtual;
    procedure OnApplicationIdle(Sender: TObject; var Done: Boolean); virtual;
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); virtual;
    procedure GetEditorValues(const NewValue: string); virtual;
  public
    constructor Create;
    constructor Create(TheOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetObjectAndProperty(NewPersistent: TPersistent;
                                   const NewPropertyName: string);
    procedure SetObjectAndProperty(NewPersistent: TPersistent;
                                 const NewPropertyName, NewElementName: string);
    procedure InvalidateEditor; virtual;
    procedure CreateEditor; virtual;
    procedure FetchValues; virtual;
    procedure LoadFromProperty; virtual;
    procedure SaveToProperty; virtual;
    procedure EditingDone; virtual;
    procedure SetAsText(const NewText: string);
    function GetAsText: string;
    procedure SetAsInt(const NewInt: integer);
    function GetAsInt: integer;
    function CheckBeforeWrite: boolean;
    procedure CheckAfterWrite;
    procedure DoError(Writing: boolean; E: Exception); virtual;
  public
    // alias values
    procedure MapValues(Values, AliasStrings: TStrings;
                        var MappedValues: TStrings;
                        UseAllExistingAlias, AddValuesWithoutAlias,
                        IfNoValuesAvailableAddAllAlias: boolean);
    procedure MapCollectedValues(AliasStrings: TStrings;
                                 var MappedValues: TStrings;
                                 UseAllExistingAlias, AddValuesWithoutAlias,
                                 IfNoValuesAvailableAddAllAlias: boolean);
    procedure AssignCollectedAliasValuesTo(DestList: TStrings;
                                           KeepIfNoneCollected: boolean = true);
    function HasAliasValues: boolean;
    procedure BuildEnumAliasValues(AStringArray: PString);
  public
    // for Set property editors
    procedure AssignSetEnumsAliasTo(DestList: TStrings);
    function GetSetElementValue(const AliasName: string): boolean;
    procedure SetSetElementValue(const AliasName: string; NewValue: boolean);
    function GetIndexOfSetElement(const AliasName: string): integer;
    function GetSetTypeData(out CompData: PTypeInfo;
                            out TypeData: PTypeData): boolean;
  public
    property AliasValues: TAliasStrings read FAliasValues;
    property CanModify: boolean read GetCanModify;
    property CollectedValues: TStrings read FCollectedValues write FCollectedValues;
    property CollectValues: boolean read FCollectValues write SetCollectValues;
    property Editor: TPropertyEditor read FEditor write SetEditor;
    property Filter: TTypeKinds read FFilter write SetFilter default AllTypeKinds;
    property Hook: TPropertyEditorHook read FHook;
    property LinkNotifier: TPropertyLinkNotifier read FLinkNotifier;
    property OnEditorChanged: TNotifyEvent read FOnEditorChanged write FOnEditorChanged;
    property OnLoadFromProperty: TNotifyEvent read FOnLoadFromProperty write FOnLoadFromProperty;// do not publish, it is used by the TTI controls
    property OnSaveToProperty: TNotifyEvent read FOnSaveToProperty write FOnSaveToProperty;// do not publish, it is used by the TTI controls
    property OnBeforeWrite: TBeforeWriteProperty read FOnBeforeWrite write FOnBeforeWrite;
    property OnAfterWrite: TNotifyEvent read FOnAfterWrite write FOnAfterWrite;
    property OnTestEditing: TTestEditing read FOnTestEditing write FOnTestEditing;
    property OnTestEditor: TPropertyEditorFilterFunc read FOnTestEditor write FOnTestEditor;
    property Options: TPropertyLinkOptions read FOptions write SetOptions default DefaultLinkOptions;
    property Owner: TComponent read FOwner;
    property SaveEnabled: boolean read FSaveEnabled write FSaveEnabled;
    property PropertyLoaded: boolean read FPropertyLoaded write FPropertyLoaded;
    property TIObject: TPersistent read FTIObject write SetTIObject;
    property TIPropertyName: string read FTIPropertyName write SetTIPropertyName;
    property TIElementName: string read FTIElementName write SetTIElementName;
  end;
  

  { TPropertyLink }
  
  TPropertyLink = class(TCustomPropertyLink)
  private
    procedure ReadAliasValuesData(Reader: TReader);
    procedure WriteAliasValuesData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  published
    property AliasValues;
    property OnBeforeWrite;
    property OnAfterWrite;
    property Options;
    property TIObject;
    property TIPropertyName;
    property TIElementName;
  end;
  
  
  { TPropertyLinkPropertyEditor }
  
  TPropertyLinkPropertyEditor = class(TClassPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;
  
  
  { TTIObjectPropertyEditor }

  TTIObjectPropertyEditor = class(TPersistentPropertyEditor)
  end;


  { TPropertyNamePropertyEditor
    Property editor for TCustomPropertyLink.TIPropertyName, showing
    all compatible properties. }
  
  TPropertyNamePropertyEditor = class(TStringPropertyEditor)
  protected
    FPropEdits: TList; // list of TPropertyEditor
    procedure GetCompatiblePropEdits(Prop: TPropertyEditor);
    function TestEditor(const Prop: TPropertyEditor): boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    procedure GetValues(Proc: TGetStringProc); override;
  end;
  

  { TTIElementNamePropertyEditor
    Property editor for TCustomPropertyLink.TIElementName, showing
    all elements. }

  TTIElementNamePropertyEditor = class(TStringPropertyEditor)
  protected
    FPropEdits: TList; // list of TPropertyEditor for TIPropertyName
    FElementPropEdits: TList; // list of TPropertyEditor for TIElementName
    procedure GetCompatiblePropEdits(Prop: TPropertyEditor);
    procedure GetElementPropEdits(Prop: TPropertyEditor);
    function TestEditor(const Prop: TPropertyEditor): boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    procedure GetValues(Proc: TGetStringProc); override;
  end;


  { TAliasStringsPropEditorDlg }
  
  TAliasStringsPropEditorDlg = class(TStringsPropEditorDlg)
    GetDefaultValuesButton: TButton;
    procedure GetDefaultValuesButtonClick(Sender: TObject);
  protected
    FCollectedValues: TAliasStrings;
    procedure AddValue(const s: string); virtual;
  public
    procedure AddButtons; override;
  end;

  
  { TPropLinkAliasPropertyEditor
    Property Editor for TCustomPropertyLink.AliasValues, providing a dialog
    to edit }
    
  TPropLinkAliasPropertyEditor = class(TStringsPropertyEditor)
  public
    function CreateDlg(s: TStrings): TStringsPropEditorDlg; override;
  end;
  
  
  { TMultiPropertyLink
    A component to switch the TIObjects of multiple RTTI controls at once. }
  
  TMultiPropertyLink = class(TComponent)
  private
    FTIObject: TPersistent;
    FMaintainGrandChilds: boolean;
    FMaintainSiblings: boolean;
    FOnSetTIObject: TNotifyEvent;
    FParentControl: TWinControl;
    FRootComponent: TComponent;
    procedure SetTIObject(const AValue: TPersistent);
    procedure SetMaintainGrandChilds(const AValue: boolean);
    procedure SetMaintainSiblings(const AValue: boolean);
    procedure SetParentControl(const AValue: TWinControl);
    procedure SetRootComponent(const AValue: TComponent);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetLinks;
    procedure SetLinksForChildControls(AParent: TWinControl;
                                       WithGrandChilds: boolean);
    procedure SetLinksForChildComponents(AComponent: TComponent);
    procedure Loaded; override;
  published
    property TIObject: TPersistent read FTIObject write SetTIObject;
    property OnSetTIObject: TNotifyEvent Read FOnSetTIObject Write FOnSetTIObject;
    property ParentControl: TWinControl read FParentControl write SetParentControl;
    property RootComponent: TComponent read FRootComponent write SetRootComponent;
    property MaintainGrandChilds: boolean read FMaintainGrandChilds
                                          write SetMaintainGrandChilds;
    property MaintainSiblings: boolean read FMaintainSiblings
                                       write SetMaintainSiblings default true;
  end;


  { TTICustomEdit }

  TTICustomEdit = class(TCustomEdit)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;
  
  
  { TTIEdit }
  
  TTIEdit = class(TTICustomEdit)
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Constraints;
    property CharCase;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Link;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    Property OnKeyDown;
    property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
  end;
  
  
  { TTICustomMaskEdit }

  TTICustomMaskEdit = class(TCustomMaskEdit)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;


  { TTIMaskEdit }

  TTIMaskEdit = class(TTICustomMaskEdit)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property Link;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


  { TTICustomComboBox }

  TTICustomComboBox = class(TCustomComboBox)
  private
    FHistoryCaseSensitive: boolean;
    FLink: TPropertyLink;
    FMaxHistoryCount: integer;
    procedure SetLink(const AValue: TPropertyLink);
    procedure SetMaxHistoryCount(const AValue: integer);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure LinkEditorChanged(Sender: TObject); virtual;
    function LinkTestEditing(Sender: TObject): boolean;
    procedure GetItems; override;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
    property MaxHistoryCount: integer read FMaxHistoryCount
           write SetMaxHistoryCount;// set this to a value > 0 to enable history
    property HistoryCaseSensitive: boolean read FHistoryCaseSensitive
                                           write FHistoryCaseSensitive;
  end;
  
  
  { TTIComboBox }
  
  TTIComboBox = class(TTICustomComboBox)
  public
    property ItemIndex;
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property BorderSpacing;
    property DropDownCount;
    property Enabled;
    property Font;
    property HistoryCaseSensitive;
    property Link;
    property MaxHistoryCount;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnDrawItem;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelect;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
  end;
  
  
  { TTICustomRadioGroup }

  TTICustomRadioGroup = class(TCustomRadioGroup)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure LinkEditorChanged(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;


  { TTIRadioGroup }

  TTIRadioGroup = class(TTICustomRadioGroup)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property ChildSizing;
    property Color;
    property ColumnLayout;
    property Columns;
    property Constraints;
    property Enabled;
    property ItemIndex;
    property Link;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;
  
  
  { TTICustomCheckGroup }

  TTICustomCheckGroup = class(TCustomCheckGroup)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure LinkEditorChanged(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;


  { TTICheckGroup }

  TTICheckGroup = class(TTICustomCheckGroup)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property Color;
    property ColumnLayout;
    property Columns;
    property Constraints;
    property Enabled;
    property Items;
    property Link;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnItemClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;


  { TTICustomCheckListBox }

  TTICustomCheckListBox = class(TCustomCheckListBox)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure LinkEditorChanged(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;


  { TTICheckListBox }

  TTICheckListBox = class(TTICustomCheckListBox)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property Constraints;
    property ExtendedSelect;
    property Items;
    property ItemHeight;
    property Link;
    property MultiSelect;
    property OnClick;
    property OnDblClick;
    property OnDrawItem;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnResize;
    property ParentShowHint;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;
  
  
  { TTICustomListBox }

  TTICustomListBox = class(TCustomListBox)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure LinkEditorChanged(Sender: TObject); virtual;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;


  { TTIListBox }

  TTIListBox = class(TTICustomListBox)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property ClickOnSelChange;
    property Constraints;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property MultiSelect;
    property Link;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDrawItem;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectionChange;
    property ParentShowHint;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;


  { TTICustomCheckBox }

  TTICustomCheckBox = class(TCustomCheckBox)
  private
    FLink: TPropertyLink;
    FLinkValueFalse: string;
    FLinkValueTrue: string;
    FPropertyNameAsCaption: boolean;
    procedure SetLink(const AValue: TPropertyLink);
    procedure SetPropertyNameAsCaption(const AValue: boolean);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure LinkEditorChanged(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property LinkValueTrue: string read FLinkValueTrue;
    property LinkValueFalse: string read FLinkValueFalse;
    property Link: TPropertyLink read FLink write SetLink;
    property PropertyNameAsCaption: boolean read FPropertyNameAsCaption
                                            write SetPropertyNameAsCaption;
  end;


  { TTICheckBox }

  TTICheckBox = class(TTICustomCheckBox)
  published
    property Action;
    property Align;
    property AllowGrayed;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Link;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property ParentShowHint;
    property PopupMenu;
    property PropertyNameAsCaption;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


  { TTICustomButton }

  TTICustomButton = class(TCustomButton)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    function LinkTestEditor(const ATestEditor: TPropertyEditor): Boolean; virtual;
    procedure Click; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;


  { TTIButton }

  TTIButton = class(TTICustomButton)
  published
    property Action;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Cancel;
    property Caption;
    property Constraints;
    property Default;
    property Enabled;
    property Font;
    property Link;
    property ModalResult;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


  { TTICustomLabel }
  
  TTICustomLabel = class(TCustomLabel)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;
  
  
  { TTILabel }
  
  TTILabel = class(TTICustomLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Constraints;
    property FocusControl;
    property Font;
    property Layout;
    property Link;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentFont;
    property ShowAccelChar;
    property Visible;
    property WordWrap;
  end;


  { TTICustomGroupbox }

  TTICustomGroupbox = class(TCustomGroupBox)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;


  { TTIGroupBox }

  TTIGroupBox = class(TTICustomGroupbox)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property Link;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


  { TTICustomMemo }
  
  TTICustomMemo = class(TCustomMemo)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    function LinkTestEditor(const ATestEditor: TPropertyEditor): Boolean; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;
  
  
  { TTIMemo }
  
  TTIMemo = class(TTICustomMemo)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Font;
    property Lines;
    property Link;
    property MaxLength;
    property OnChange;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property ParentFont;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property Tabstop;
    property Visible;
    property WordWrap;
  end;
  
  
  { TTICustomCalendar }
  
  TTICustomCalendar = class(TCustomCalendar)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    function LinkTestEditor(const ATestEditor: TPropertyEditor): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;
  
  
  { TTICalendar }
  
  TTICalendar = class(TTICustomCalendar)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DisplaySettings;
    property Link;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDayChanged;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMonthChanged;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnYearChanged;
    property PopupMenu;
    property Tabstop;
    property Visible;
  end;
  
  
  { TTICustomImage }

  TTICustomImage = class(TCustomImage)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    function LinkTestEditor(const ATestEditor: TPropertyEditor): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;


  { TTIImage }

  TTIImage = class(TTICustomImage)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Center;
    property Constraints;
    property Link;
    property OnChangeBounds;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property Proportional;
    property Stretch;
    property Transparent;
    property Visible;
  end;


  { TTICustomFloatSpinEdit }
  
  TTICustomFloatSpinEdit = class(TCustomFloatSpinEdit)
  private
    FLink: TPropertyLink;
    FUseRTTIMinMax: boolean;
    procedure SetLink(const AValue: TPropertyLink);
    procedure SetUseRTTIMinMax(const AValue: boolean);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure LinkEditorChanged(Sender: TObject); virtual;
    procedure GetRTTIMinMax; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
    property UseRTTIMinMax: boolean read FUseRTTIMinMax write SetUseRTTIMinMax default true;
  end;
  
  
  { TTIFloatSpinEdit }
  
  TTIFloatSpinEdit = class(TTICustomFloatSpinEdit)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DecimalPlaces;
    property Enabled;
    property Increment;
    property Link;
    property MaxValue;
    property MinValue;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    Property OnKeyDown;
    property OnKeyPress;
    Property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property UseRTTIMinMax;
    property Visible;
  end;
  
  
  { TTICustomSpinEdit }

  TTICustomSpinEdit = class(TCustomSpinEdit)
  private
    FLink: TPropertyLink;
    FUseRTTIMinMax: boolean;
    procedure SetLink(const AValue: TPropertyLink);
    procedure SetUseRTTIMinMax(const AValue: boolean);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure LinkEditorChanged(Sender: TObject); virtual;
    procedure GetRTTIMinMax; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
    property UseRTTIMinMax: boolean read FUseRTTIMinMax write SetUseRTTIMinMax default true;
  end;


  { TTISpinEdit }

  TTISpinEdit = class(TTICustomSpinEdit)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property Increment;
    property Link;
    property MaxValue;
    property MinValue;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    Property OnKeyDown;
    property OnKeyPress;
    Property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property UseRTTIMinMax;
    property Visible;
  end;


  { TTICustomTrackBar }

  TTICustomTrackBar = class(TCustomTrackBar)
  private
    FLink: TPropertyLink;
    FUseRTTIMinMax: boolean;
    procedure SetLink(const AValue: TPropertyLink);
    procedure SetUseRTTIMinMax(const AValue: boolean);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure LinkEditorChanged(Sender: TObject); virtual;
    procedure GetRTTIMinMax; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
    property UseRTTIMinMax: boolean read FUseRTTIMinMax write SetUseRTTIMinMax default true;
  end;

  
  { TTITrackBar }
  
  TTITrackBar = class(TTICustomTrackBar)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Frequency;
    property Hint;
    property LineSize;
    property Link;
    property Max;
    property Min;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnStartDrag;
    property Orientation;
    property PageSize;
    property ParentShowHint;
    property PopupMenu;
    property ScalePos;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TickMarks;
    property TickStyle;
    property UseRTTIMinMax;
    property Visible;
  end;
  

  { TTICustomProgressBar }

  TTICustomProgressBar = class(TCustomProgressBar)
  private
    FLink: TPropertyLink;
    FUseRTTIMinMax: boolean;
    procedure SetLink(const AValue: TPropertyLink);
    procedure SetUseRTTIMinMax(const AValue: boolean);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure LinkEditorChanged(Sender: TObject); virtual;
    procedure GetRTTIMinMax; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
    property UseRTTIMinMax: boolean read FUseRTTIMinMax write SetUseRTTIMinMax default true;
  end;


  { TTIProgressBar }

  TTIProgressBar = class(TTICustomProgressBar)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderWidth;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Max;
    property Min;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property Smooth;
    property Step;
    property TabOrder;
    property TabStop;
    property Visible;
    property BarShowText;
  end;


  { TTICustomColorButton }

  TTICustomColorButton = class(TColorButton)
  private
    FLink: TPropertyLink;
    procedure SetLink(const AValue: TPropertyLink);
  protected
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    function LinkTestEditor(const ATestEditor: TPropertyEditor): Boolean;
    procedure ShowColorDialog; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure EditingDone; override;
    property Link: TPropertyLink read FLink write SetLink;
  end;


  { TTIColorButton }

  TTIColorButton = class(TTICustomColorButton)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderWidth;
    property ButtonColor;
    property Hint;
    property OnChangeBounds;
    property OnColorChanged;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;


function GetPropertyLinkOfComponent(AComponent: TComponent
  ): TCustomPropertyLink;
procedure SaveActivePropertyLink(AForm: TCustomForm);
procedure CreateEnumAliasValues(EnumType: PTypeInfo; List: TStrings;
  AStringArray: PString);

procedure Register;


implementation

uses
  ComponentEditors, MaskPropEdit;

procedure SaveActivePropertyLink(AForm: TCustomForm);
var
  CurControl: TWinControl;
  Link: TCustomPropertyLink;
begin
  CurControl:=AForm.ActiveControl;
  if CurControl<>nil then begin
    Link:=GetPropertyLinkOfComponent(CurControl);
    if Link<>nil then
      Link.SaveToProperty;
  end;
end;

procedure CreateEnumAliasValues(EnumType: PTypeInfo; List: TStrings;
  AStringArray: PString);
var
  AName: String;
  AnAliasName: String;
  i: LongInt;
begin
  List.BeginUpdate;
  List.Clear;
  //debugln('CreateEnumAliasValues ',EnumType^.Name);
  with GetTypeData(EnumType)^ do
    for i := MinValue to MaxValue do begin
      AName := GetEnumName(EnumType, i);
      AnAliasName := AStringArray[i];
      //debugln('CreateEnumAliasValues ',AName+'='+AnAliasName);
      List.Add(AName+'='+AnAliasName);
    end;
  List.EndUpdate;
end;

function GetPropertyLinkOfComponent(AComponent: TComponent
  ): TCustomPropertyLink;
begin
  Result:=nil;
  if AComponent=nil then exit;
  try
    Result:=TCustomPropertyLink(GetObjectProp(AComponent,'Link',
                                              TCustomPropertyLink));
  except
    on E: EPropertyError do ;// ignore exception on not found
  end;
end;

procedure Register;
begin
  RegisterComponents('RTTI',[TTIEdit,TTIComboBox,TTIButton,TTICheckBox,
    TTILabel,TTIGroupBox,TTIRadioGroup,TTICheckGroup,TTICheckListBox,
    TTIListBox,TTIMemo,TTICalendar,TTIImage,TTIFloatSpinEdit,TTISpinEdit,
    TTITrackBar,TTIProgressBar,TTIMaskEdit,TTIColorButton,TMultiPropertyLink]);
end;

{ TAliasStrings }

function TAliasStrings.IndexOfValue(const AValue: string): integer;
var
  S : String;
  Start: Integer;
begin
  Result:=Count-1;
  while (Result>=0) do begin
    S:=Strings[Result];
    Start:=pos('=',S)+1;
    if (Start>0) and (CompareText(AValue,Copy(S,Start,length(S)))=0) then
      exit;
    dec(Result);
  end;
end;

function TAliasStrings.ValueAt(Index: integer): string;
var
  S: string;
  Start: Integer;
begin
  S:=Strings[Index];
  Start:=pos('=',S)+1;
  if (Start>0) then
    Result:=Copy(S,Start,length(S))
  else
    Result:='';
end;

function TAliasStrings.ValueToAlias(const AValue: string): string;
begin
  Result:=Values[AValue];
  if Result='' then Result:=AValue;
end;

function TAliasStrings.AliasToValue(const Alias: string): string;
var
  i: LongInt;
begin
  i:=IndexOfValue(Alias);
  if i>=0 then
    Result:=Names[i]
  else
    Result:=Alias;
end;

{ TCustomPropertyLink }

procedure TCustomPropertyLink.SetEditor(const AValue: TPropertyEditor);
begin
  if FEditor=AValue then exit;
  FEditor:=AValue;
  EditorChanged;
end;

procedure TCustomPropertyLink.SetCollectValues(const AValue: boolean);
begin
  if FCollectValues=AValue then exit;
  FCollectValues:=AValue;
  if FCollectValues then FetchValues;
end;

procedure TCustomPropertyLink.SetFilter(const AValue: TTypeKinds);
begin
  if FFilter=AValue then exit;
  FFilter:=AValue;
  InvalidateEditor;
end;

procedure TCustomPropertyLink.SetOptions(
  const NewOptions: TPropertyLinkOptions);
var
  ChangedOptions: TPropertyLinkOptions;
begin
  if FOptions=NewOptions then exit;
  ChangedOptions:=(FOptions-NewOptions)+(NewOptions-FOptions);
  //debugln('TCustomPropertyLink.SetOptions Old=',dbgs(ploReadOnIdle in FOptions),
  //  ' New=',dbgs(ploReadOnIdle in NewOptions),' Changed=',dbgs(ploReadOnIdle in ChangedOptions));
  FOptions:=NewOptions;
  if (ploReadOnIdle in ChangedOptions) then UpdateIdleHandler;
end;

procedure TCustomPropertyLink.SetTIElementName(const AValue: string);
begin
  if FTIElementName=AValue then exit;
  SetObjectAndProperty(TIObject,TIPropertyName,AValue);
end;

function TCustomPropertyLink.GetCanModify: boolean;
begin
  Result:=(FEditor<>nil) and (not FEditor.IsReadOnly);
end;

procedure TCustomPropertyLink.SetTIObject(const AValue: TPersistent);
begin
  if FTIObject=AValue then exit;
  SetObjectAndProperty(AValue,TIPropertyName,TIElementName);
end;

procedure TCustomPropertyLink.SetTIPropertyName(const AValue: string);
begin
  if FTIPropertyName=AValue then exit;
  SetObjectAndProperty(TIObject,AValue,TIElementName);
end;

procedure TCustomPropertyLink.EditorChanged;
begin
  if FEditor=nil then begin
    FTIObject:=nil;
    FTIPropertyName:='';
  end else begin
    FTIObject:=FEditor.GetComponent(0);
    FTIPropertyName:=FEditor.GetName;
  end;
end;

procedure TCustomPropertyLink.InvalidateEditor;
begin
  FreeThenNil(FEditor);
end;

procedure TCustomPropertyLink.SetPropertyEditor(
  APropertyEditor: TPropertyEditor);
begin
  if FEditor=nil then
    FEditor:=APropertyEditor;
end;

function TCustomPropertyLink.CheckPropInfo(const APropInfo: PPropInfo): boolean;
begin
  Result:=CompareText(APropInfo^.Name,FTIPropertyName)=0;
end;

destructor TCustomPropertyLink.Destroy;
begin
  InvalidateEditor;
  if (Application<>nil) and FIdleHandlerConnected then
    Application.RemoveOnIdleHandler(@OnApplicationIdle);
  FreeThenNil(FLinkNotifier);
  FreeThenNil(FAliasValues);
  FreeThenNil(FHook);
  FreeThenNil(FCollectedValues);
  inherited Destroy;
end;

procedure TCustomPropertyLink.Assign(Source: TPersistent);
var
  SrcLink: TCustomPropertyLink;
begin
  if Source is TCustomPropertyLink then begin
    SrcLink:=TCustomPropertyLink(Source);
    SetObjectAndProperty(SrcLink.TIObject,SrcLink.TIPropertyName,
                         SrcLink.TIElementName);
  end else begin
    inherited Assign(Source);
  end;
end;

procedure TCustomPropertyLink.SetObjectAndProperty(NewPersistent: TPersistent;
  const NewPropertyName: string);
begin
  SetObjectAndProperty(NewPersistent,NewPropertyName,'');
end;

procedure TCustomPropertyLink.SetObjectAndProperty(NewPersistent: TPersistent;
  const NewPropertyName, NewElementName: string);
var
  AComponent: TComponent;
begin
  // Note: checking for IsValidIdent is not needed, because
  // an identifier is is only needed for streaming. So every string as Name is
  // allowed.
  if (NewPersistent<>TIObject) or (NewPropertyName<>TIPropertyName) then begin
    FPropertyLoaded:=false;
    if (FTIObject is TComponent) then begin
      AComponent:=TComponent(FTIObject);
      AComponent.RemoveFreeNotification(FLinkNotifier);
    end;
    FTIObject:=NewPersistent;
    if FTIObject is TComponent then begin
      AComponent:=TComponent(FTIObject);
      if not (csDestroying in AComponent.ComponentState) then
        AComponent.FreeNotification(FLinkNotifier)
      else
        FTIObject:=nil;
    end;
    FTIPropertyName:=NewPropertyName;
  end
  else if FTIElementName=NewElementName then begin
    // no change
    exit;
  end;
  FTIElementName:=NewElementName;
  InvalidateEditor;
  LoadFromProperty;
end;

procedure TCustomPropertyLink.CreateEditor;
var
  Selection: TPersistentSelectionList;
  OldEditorExisted: Boolean;
begin
  if (FEditor<>nil) or (FTIObject=nil) or (FTIPropertyName='') then exit;
  FPropertyLoaded:=false;
  //debugln('TCustomPropertyLink.CreateEditor A ',FTIObject.ClassName+':'+FTIPropertyName);
  OldEditorExisted:=FEditor<>nil;
  CreateHook;
  Selection := TPersistentSelectionList.Create;
  try
    Selection.Add(FTIObject);
    GetPersistentProperties(Selection,Filter,Hook,@SetPropertyEditor,
      @CheckPropInfo,OnTestEditor);
  finally
    Selection.Free;
  end;
  //debugln('TCustomPropertyLink.CreateEditor B ',dbgsName(FEditor));
  {if FEditor=nil then begin
    raise Exception.Create('Unable to create property editor for '
                           +FTIObject.ClassName+':'+FTIPropertyName);
  end;}

  //debugln('TCustomPropertyLink.CreateEditor C ',FTIObject.ClassName+':'+FTIPropertyName,' ',dbgs(FCollectValues),' ',dbgsName(FEditor));
  if CollectValues then FetchValues;
  if ((FEditor<>nil) or OldEditorExisted) and Assigned(OnEditorChanged) then
    OnEditorChanged(Self);
  UpdateIdleHandler;
end;

procedure TCustomPropertyLink.FetchValues;
begin
  FreeThenNil(FCollectedValues);
  //debugln('TCustomPropertyLink.FetchValues A ',dbgsName(Editor));
  if Editor<>nil then
    Editor.GetValues(@GetEditorValues);
end;

procedure TCustomPropertyLink.CreateHook;
begin
  if FHook=nil then FHook:=TPropertyEditorHook.Create;
  FHook.LookupRoot:=TIObject;
end;

procedure TCustomPropertyLink.UpdateIdleHandler;
begin
  if (Application<>nil)
  and ((ploReadOnIdle in Options)<>FIdleHandlerConnected) then begin
    if ploReadOnIdle in Options then begin
      FIdleHandlerConnected:=true;
      Application.AddOnIdleHandler(@OnApplicationIdle,true);
    end else begin
      FIdleHandlerConnected:=false;
      Application.RemoveOnIdleHandler(@OnApplicationIdle);
    end;
    //debugln('TCustomPropertyLink.UpdateIdleHandler ploReadOnIdle=',dbgs(ploReadOnIdle in Options));
  end;
end;

procedure TCustomPropertyLink.OnApplicationIdle(Sender: TObject;
  var Done: Boolean);
begin
  if Sender=nil then ;
  if Done then ;
  if (ploReadOnIdle in FOptions) then begin
    // only update if not editing
    // => check for editing
    if Assigned(OnTestEditing) then begin
      // custom check
      if (OnTestEditing(Self)) then exit;
    end else begin
      // default checks
      if (Owner is TWinControl) and (TWinControl(Owner).Focused) then exit;
    end;
    LoadFromProperty;
  end;
end;

procedure TCustomPropertyLink.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation=opRemove) then begin
    if (AComponent=FTIObject) then
      SetObjectAndProperty(nil,FTIPropertyName,TIElementName);
  end;
end;

procedure TCustomPropertyLink.GetEditorValues(const NewValue: string);
begin
  if FCollectedValues=nil then FCollectedValues:=TStringList.Create;
  FCollectedValues.Add(NewValue);
end;

constructor TCustomPropertyLink.Create(TheOwner: TComponent);
begin
  inherited Create;
  FOwner:=TheOwner;
  FSaveEnabled:=true;
  FFilter:=AllTypeKinds;
  FAliasValues:=TAliasStrings.Create;
  FLinkNotifier:=TPropertyLinkNotifier.Create(Self);
  FOptions:=DefaultLinkOptions;
end;

procedure TCustomPropertyLink.SaveToProperty;
begin
  if Self=nil then exit;
  if (not SaveEnabled) then exit;
  if (Owner<>nil)
  and ([csDesigning,csDestroying,csLoading]*Owner.ComponentState<>[]) then exit;
  CreateEditor;
  if Assigned(OnSaveToProperty) then OnSaveToProperty(Self);
end;

procedure TCustomPropertyLink.EditingDone;
begin
  if Self=nil then exit;
  if (ploAutoSave in Options) and PropertyLoaded then
    SaveToProperty;
end;

procedure TCustomPropertyLink.SetAsText(const NewText: string);
begin
  try
    if not CheckBeforeWrite then exit;
    if (FTIElementName='') then
      FEditor.SetValue(AliasValues.AliasToValue(NewText))
    else
      SetSetElementValue(FTIElementName,CompareText(NewText,'True')=0);
    CheckAfterWrite;
  except
    on E: Exception do DoError(true,E);
  end;
end;

function TCustomPropertyLink.GetAsText: string;
begin
  Result:='';
  try
    if (FTIElementName='') then
      Result:=AliasValues.ValueToAlias(FEditor.GetVisualValue)
    else begin
      if GetSetElementValue(FTIElementName) then
        Result:='True'
      else
        Result:='False';
    end;
  except
    on E: Exception do DoError(false,E);
  end;
end;

procedure TCustomPropertyLink.SetAsInt(const NewInt: integer);
begin
  try
    if not CheckBeforeWrite then exit;
    FEditor.SetValue(IntToStr(NewInt));
    CheckAfterWrite;
  except
    on E: Exception do DoError(true,E);
  end;
end;

function TCustomPropertyLink.GetAsInt: integer;
begin
  Result:=0;
  try
    Result:=FEditor.GetOrdValue;
  except
    on E: Exception do DoError(false,E);
  end;
end;

function TCustomPropertyLink.CheckBeforeWrite: boolean;
begin
  Result:=true;
  if Assigned(OnBeforeWrite) then
    OnBeforeWrite(Self,Result);
end;

procedure TCustomPropertyLink.CheckAfterWrite;
begin
  if Assigned(OnAfterWrite) then OnAfterWrite(Self);
end;

procedure TCustomPropertyLink.DoError(Writing: boolean; E: Exception);
var
  ACaption: String;
  AText: String;
begin
  ACaption:='Error';
  if Writing then
    AText:='Error while writing property'#13+E.Message
  else
    AText:='Error while reading property'#13+E.Message;
  MessageDlg(ACaption,AText,mtError,[mbCancel],0);
  if Writing then
    LoadFromProperty;
end;

constructor TCustomPropertyLink.create;
begin
  Create(nil);
end;

procedure TCustomPropertyLink.MapValues(Values, AliasStrings: TStrings;
  var MappedValues: TStrings; UseAllExistingAlias, AddValuesWithoutAlias,
  IfNoValuesAvailableAddAllAlias: boolean);
var
  AValue: string;
  MappedValue: string;
  i: Integer;
begin
  if (Values=nil) or (Values.Count=0) then begin
    // no values provided by current property editor
    if IfNoValuesAvailableAddAllAlias and (AliasStrings<>nil) then begin
      MappedValues:=TStringList.Create;
      for i:=0 to AliasStrings.Count-1 do
        MappedValues.Add(AliasStrings.Names[i]);
    end else begin
      MappedValues:=nil;
    end;
  end else if AliasStrings<>nil then begin
    // current property editor has provided values
    // => map values via AliasStrings
    MappedValues:=TStringList.Create;
    if UseAllExistingAlias then begin
      // add all existing alias
      for i:=0 to AliasStrings.Count-1 do begin
        AValue:=AliasStrings.Names[i];
        MappedValue:=AliasStrings.Values[AValue];
        //writeln('TCustomPropertyLink.MapValues MappedValue=',MappedValue,' AValue=',AValue,' ',Values.IndexOf(AValue));
        if Values.IndexOf(AValue)>=0 then
          MappedValues.Add(MappedValue);
      end;
      // add all values without alias
      if AddValuesWithoutAlias then begin
        for i:=0 to Values.Count-1 do begin
          AValue:=Values[i];
          MappedValue:=AliasStrings.Values[AValue];
          if MappedValue='' then
            // value has no alias
            MappedValues.Add(AValue);
        end;
      end;
    end else begin
      // add all values mapped
      for i:=0 to Values.Count-1 do begin
        AValue:=Values[i];
        MappedValue:=AliasStrings.Values[AValue];
        if MappedValue<>'' then
          // value has alias
          AValue:=MappedValue;
        MappedValues.Add(AValue);
      end;
    end;
  end else begin
    // no alias => simply return a copy of the values
    MappedValues:=TStringList.Create;
    MappedValues.Assign(Values);
  end;
end;

procedure TCustomPropertyLink.MapCollectedValues(AliasStrings: TStrings;
  var MappedValues: TStrings; UseAllExistingAlias, AddValuesWithoutAlias,
  IfNoValuesAvailableAddAllAlias: boolean);
begin
  MapValues(FCollectedValues,AliasStrings,MappedValues,UseAllExistingAlias,
            AddValuesWithoutAlias,IfNoValuesAvailableAddAllAlias);
end;

procedure TCustomPropertyLink.AssignCollectedAliasValuesTo(DestList: TStrings;
  KeepIfNoneCollected: boolean);
var
  MappedValues: TStrings;
begin
  MappedValues:=nil;
  MapCollectedValues(AliasValues,MappedValues,true,true,true);
  try
    if (MappedValues.Count>0) or (not KeepIfNoneCollected) then
      DestList.Assign(MappedValues);
  finally
    MappedValues.Free;
  end;
end;

function TCustomPropertyLink.HasAliasValues: boolean;
begin
  Result:=(AliasValues<>nil) and (AliasValues.Count>0);
end;

procedure TCustomPropertyLink.BuildEnumAliasValues(AStringArray: PString);
{ Example:

  type
    TMyEnum = (enum1,enum2);
  const
    MyEnumNamesArray: array[TMyEnum] of string = ('Enum1','Enum2');

  MyTIComboBox.Link.BuildEnumAliasValues(@MyEnumNamesArray[TMyEnum(0)]);
}
begin
  CreateEditor;
  if (Editor=nil) or (not (Editor is TEnumPropertyEditor)) then exit;
  CreateEnumAliasValues(Editor.GetPropType,AliasValues,AStringArray);
  if Assigned(OnEditorChanged) then
    OnEditorChanged(Self);
  LoadFromProperty;
end;

procedure TCustomPropertyLink.AssignSetEnumsAliasTo(DestList: TStrings);
var
  Enums: TStringList;
  CompData: PTypeInfo;
  TypeData: PTypeData;
  MappedValues: TStrings;
  i: LongInt;
begin
  Enums:=nil;
  MappedValues:=nil;
  try
    // retrieve all set enums
    if GetSetTypeData(CompData,TypeData) then begin
      Enums:=TStringList.Create;
      for i := TypeData^.MinValue to TypeData^.MaxValue do
        Enums.Add(GetEnumName(CompData,i));
      // map values
      MapValues(Enums,AliasValues,MappedValues,true,true,true);
    end;
    // assign values
    if MappedValues<>nil then
      DestList.Assign(MappedValues)
    else
      DestList.Clear;
  finally
    Enums.Free;
    MappedValues.Free;
  end;
end;

function TCustomPropertyLink.GetSetElementValue(const AliasName: string
  ): boolean;
var
  CompData: PTypeInfo;
  TypeData: PTypeData;
  i: LongInt;
  IntegerSet: TIntegerSet;
begin
  Result:=false;
  if not GetSetTypeData(CompData,TypeData) then exit;
  if (CompData=nil) or (TypeData=nil) then ;
  i:=GetIndexOfSetElement(AliasName);
  if i>=0 then begin
    Integer(IntegerSet) := Editor.GetOrdValue;
    Result:=byte(i) in IntegerSet;
  end;
end;

procedure TCustomPropertyLink.SetSetElementValue(const AliasName: string;
  NewValue: boolean);
var
  CompData: PTypeInfo;
  TypeData: PTypeData;
  i: LongInt;
  IntegerSet: TIntegerSet;
begin
  if not GetSetTypeData(CompData,TypeData) then exit;
  if (CompData=nil) or (TypeData=nil) then ;
  i:=GetIndexOfSetElement(AliasName);
  if i>=0 then begin
    Integer(IntegerSet) := Editor.GetOrdValue;
    if NewValue then
      Include(IntegerSet,i)
    else
      Exclude(IntegerSet,i);
    Editor.SetOrdValue(Integer(IntegerSet));
  end;
end;

function TCustomPropertyLink.GetIndexOfSetElement(const AliasName: string
  ): integer;
var
  CompData: PTypeInfo;
  TypeData: PTypeData;
begin
  if not GetSetTypeData(CompData,TypeData) then exit;
  for Result := TypeData^.MinValue to TypeData^.MaxValue do
    if CompareText(AliasName,
                   AliasValues.ValueToAlias(GetEnumName(CompData,Result)))=0
    then
      exit;
  Result:=-1;
end;

function TCustomPropertyLink.GetSetTypeData(out CompData: PTypeInfo;
  out TypeData: PTypeData): boolean;
begin
  Result:=false;
  CompData:=nil;
  TypeData:=nil;
  CreateEditor;
  if (Editor=nil) or (not (Editor is TSetPropertyEditor)) then exit;
  CompData:=GetTypeData(Editor.GetPropType)^.CompType;
  TypeData:=GetTypeData(CompData);
  Result:=(CompData<>nil) and (TypeData<>nil);
end;

procedure TCustomPropertyLink.LoadFromProperty;
begin
  if Self=nil then exit;
  if (Owner<>nil) and (csDestroying in Owner.ComponentState) then exit;
  CreateEditor;
  FPropertyLoaded:=true;
  if Assigned(OnLoadFromProperty) then OnLoadFromProperty(Self);
end;

{ TPropertyLinkPropertyEditor }

function TPropertyLinkPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

{ TPropertyNamePropertyEditor }

procedure TPropertyNamePropertyEditor.GetCompatiblePropEdits(
  Prop: TPropertyEditor);
begin
  if FPropEdits=nil then FPropEdits:=TList.Create;
  FPropEdits.Add(Prop);
end;

function TPropertyNamePropertyEditor.TestEditor(const Prop: TPropertyEditor
  ): boolean;
var
  i: Integer;
  CurPersistent: TPersistent;
  ALink: TCustomPropertyLink;
begin
  Result:=false;
  for i:=0 to PropCount-1 do begin
    CurPersistent:=GetComponent(i);
    if (CurPersistent is TCustomPropertyLink) then begin
      ALink:=TCustomPropertyLink(CurPersistent);
      if Assigned(ALink.OnTestEditor) and (not ALink.OnTestEditor(Prop)) then
        exit;
    end;
  end;
  Result:=true;
end;

function TPropertyNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paMultiSelect,paValueList,paSortList,paRevertable,paHasDefaultValue];
end;

function TPropertyNamePropertyEditor.GetEditLimit: Integer;
begin
  Result:=255;
end;

procedure TPropertyNamePropertyEditor.GetValues(Proc: TGetStringProc);
var
  ALink: TCustomPropertyLink;
  ASelection: TPersistentSelectionList;
  i: Integer;
  CurPersistent: TPersistent;
  CurTIObject: TPersistent;
  Filter: TTypeKinds;
begin
  ASelection:=TPersistentSelectionList.Create;
  try
    // get every TIObject of every TCustomPropertyLink in the selection
    Filter:=AllTypeKinds;
    for i:=0 to PropCount-1 do begin
      CurPersistent:=GetComponent(i);
      if (CurPersistent is TCustomPropertyLink) then begin
        ALink:=TCustomPropertyLink(CurPersistent);
        CurTIObject:=ALink.TIObject;
        if CurTIObject<>nil then begin
          ASelection.Add(CurTIObject);
          Filter:=Filter*ALink.Filter;
        end;
      end;
    end;
    if ASelection.Count=0 then exit;
    // get properties of TIObjects
    GetPersistentProperties(ASelection,Filter,PropertyHook,
      @GetCompatiblePropEdits,nil,@TestEditor);
    if FPropEdits<>nil then begin
      for i:=0 to FPropEdits.Count-1 do
        Proc(TPropertyEditor(FPropEdits[i]).GetName);
    end;
  finally
    ASelection.Free;
    if FPropEdits<>nil then begin
      for i:=0 to FPropEdits.Count-1 do
        TPropertyEditor(FPropEdits[i]).Free;
      FreeThenNil(FPropEdits);
    end;
  end;
end;

{ TAliasStringsPropEditorDlg }

procedure TAliasStringsPropEditorDlg.GetDefaultValuesButtonClick(Sender: TObject);
var
  ALink: TCustomPropertyLink;
  i: Integer;
  CurPersistent: TPersistent;
begin
  if Sender=nil then ;
  try
    // get every TIObject of every TCustomPropertyLink in the selection
    FCollectedValues:=TAliasStrings.Create;
    FCollectedValues.Text:=Memo.Text;
    for i:=0 to Editor.PropCount-1 do begin
      CurPersistent:=Editor.GetComponent(i);
      if (CurPersistent is TCustomPropertyLink) then begin
        ALink:=TCustomPropertyLink(CurPersistent);
        ALink.CreateEditor;
        if ALink.Editor<>nil then begin
          ALink.Editor.GetValues(@AddValue);
        end;
      end;
    end;
    Memo.Text:=FCollectedValues.Text;
  finally
    FreeThenNil(FCollectedValues);
  end;
end;

procedure TAliasStringsPropEditorDlg.AddValue(const s: string);
begin
  if FCollectedValues.IndexOfName(s)<0 then
    FCollectedValues.Values[s]:=s;
end;

procedure TAliasStringsPropEditorDlg.AddButtons;
begin
  inherited AddButtons;

  GetDefaultValuesButton := TButton.Create(Self);
  with GetDefaultValuesButton do Begin
    Parent:=SortButton.Parent;
    Caption:='Get Defaults';
    OnClick:=@GetDefaultValuesButtonClick;
    AutoSize:=true;
    AnchorToCompanion(akTop, 6, SortButton);
  end;
end;

{ TPropLinkAliasPropertyEditor }

function TPropLinkAliasPropertyEditor.CreateDlg(s: TStrings
  ): TStringsPropEditorDlg;
begin
  if s=nil then ;
  Result:=TAliasStringsPropEditorDlg.Create(Application);
  Result.Editor:=Self;
  Result.Memo.Text:=s.Text;
end;

{ TTICustomEdit }

procedure TTICustomEdit.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  //writeln('TTICustomEdit.LinkLoadFromProperty A ',Name,
  //  ' FLink.GetAsText=',FLink.GetAsText,' Text=',Text,
  //  ' PropName=',FLink.TIPropertyName);
  Text:=FLink.GetAsText;
end;

procedure TTICustomEdit.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if FLink.Editor=nil then exit;
  //writeln('TTICustomEdit.LinkSaveToProperty A ',Name,
  //  ' FLink.GetAsText=',FLink.GetAsText,' Text=',Text,
  //  ' PropName=',FLink.TIPropertyName);
  FLink.SetAsText(Text);
end;

procedure TTICustomEdit.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

constructor TTICustomEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,tkChar,tkEnumeration,
                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString,tkVariant,{tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,}tkWChar,tkBool,tkInt64,
                 tkQWord{,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
end;

destructor TTICustomEdit.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomEdit.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomEdit.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomComboBox }

procedure TTICustomComboBox.LinkEditorChanged(Sender: TObject);
begin
  if Sender=nil then ;
  if Link=nil then exit;
  FLink.AssignCollectedAliasValuesTo(Items);
end;

procedure TTICustomComboBox.GetItems;
var
  MaxItemWidth: LongInt;
  Cnt: LongInt;
  i: Integer;
  ItemValue: string;
  CurItemWidth: LongInt;
begin
  if (Link.Editor<>nil) and (not Link.HasAliasValues) then begin
    MaxItemWidth:=Width;
    Cnt:=Items.Count;
    for i:=0 to Cnt-1 do begin
      ItemValue:=Items[i];
      CurItemWidth:=Canvas.TextWidth(ItemValue);
      Link.Editor.ListMeasureWidth(ItemValue,i,Canvas,CurItemWidth);
      if MaxItemWidth<CurItemWidth then
        MaxItemWidth:=CurItemWidth;
    end;
    ItemWidth:=MaxItemWidth;
  end;
  inherited GetItems;
end;

procedure TTICustomComboBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  AState: TPropEditDrawState;
  ItemValue: string;
begin
  if (Link.Editor=nil) or Link.HasAliasValues then
    inherited DrawItem(Index,ARect,State)
  else begin
    if (Index>=0) and (Index<Items.Count) then
      ItemValue:=Items[Index]
    else
      ItemValue:=Text;

    AState:=[];
    if odPainted in State then Include(AState,pedsPainted);
    if odSelected in State then Include(AState,pedsSelected);
    if odFocused in State then Include(AState,pedsFocused);
    if odComboBoxEdit in State then
      Include(AState,pedsInEdit)
    else
      Include(AState,pedsInComboList);

    // clear background
    with Canvas do begin
      Brush.Color:=clWhite;
      Pen.Color:=clBlack;
      Font.Color:=Pen.Color;
      FillRect(ARect);
    end;

    Link.Editor.ListDrawValue(ItemValue,Index,Canvas,ARect,AState);

    // custom draw
    if Assigned(OnDrawItem) then
      OnDrawItem(Self, Index, ARect, State);
  end;
end;

function TTICustomComboBox.LinkTestEditing(Sender: TObject): boolean;
begin
  if Sender=nil then ;
  Result:=Focused or DroppedDown;
  //DebugLn(['TTICustomComboBox.LinkTestEditing ',dbgsName(Self),' Result=',Result,' CanTab=',CanTab,' Handle=',HandleAllocated,' ',dbgsname(FindOwnerControl(GetFocus))]);
end;

procedure TTICustomComboBox.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomComboBox.SetMaxHistoryCount(const AValue: integer);
begin
  if FMaxHistoryCount=AValue then exit;
  FMaxHistoryCount:=AValue;
end;

procedure TTICustomComboBox.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  //writeln('TTICustomComboBox.LinkLoadFromProperty A FLink.GetAsText=',FLink.GetAsText,' Text=',Text);
  if (FLink.Editor=nil) then exit;
  //debugln('TTICustomComboBox.LinkLoadFromProperty B ',dbgsName(Self),' FLink.Editor=',FLink.Editor.ClassName,' FLink.GetAsText=',FLink.GetAsText);
  Text:=FLink.GetAsText;
end;

procedure TTICustomComboBox.LinkSaveToProperty(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  if Sender=nil then ;
  //debugln('TTICustomComboBox.LinkSaveToProperty ',dbgsName(Self),' FLink.GetAsText=',FLink.GetAsText,' Text=',Text);
  if (FLink.Editor=nil) then exit;
  s:=Text;
  FLink.SetAsText(s);
  
  // update history
  if (MaxHistoryCount>0) and ((Items.Count=0) or (Items[0]<>s)) then begin
    Items.BeginUpdate;
    Items.Insert(0,s);
    for i:=Items.Count-1 downto 1 do
      if (i>=MaxHistoryCount) or (Items[i]=s)
      or ((not HistoryCaseSensitive) and (AnsiCompareText(Items[i],s)=0))
      then
        Items.Delete(i);
    Items.EndUpdate;
  end;
end;

constructor TTICustomComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,tkChar,tkEnumeration,
                 tkFloat,{tkSet,}tkMethod,tkSString,tkLString,tkAString,
                 tkWString,tkVariant,{tkArray,tkRecord,tkInterface,}
                 tkClass,tkObject,tkWChar,tkBool,tkInt64,
                 tkQWord{,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.OnEditorChanged:=@LinkEditorChanged;
  FLink.CollectValues:=true;
  FLink.OnTestEditing:=@LinkTestEditing;
end;

destructor TTICustomComboBox.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomComboBox.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomComboBox.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomCheckBox }

procedure TTICustomCheckBox.LinkEditorChanged(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink<>nil) and (FLink.Editor<>nil) then begin
    if (FLink.Editor is TBoolPropertyEditor)
    or (FLink.Editor is TSetPropertyEditor) then begin
      FLinkValueFalse:='False';
      FLinkValueTrue:='True';
    end else if FLink.Editor is TOrdinalPropertyEditor then begin
      FLinkValueFalse:='0';
      FLinkValueTrue:='-1';
    end else begin
      FLinkValueFalse:='';
      FLinkValueTrue:='True';
    end;
  end;
end;

procedure TTICustomCheckBox.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomCheckBox.SetPropertyNameAsCaption(const AValue: boolean);
begin
  if FPropertyNameAsCaption=AValue then exit;
  FPropertyNameAsCaption:=AValue;
  if FPropertyNameAsCaption and (FLink.Editor<>nil) then
    Caption:=FLink.Editor.GetName;
end;

procedure TTICustomCheckBox.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  Checked:=FLink.GetAsText<>FLinkValueFalse;
  if FPropertyNameAsCaption then
    Caption:=FLink.Editor.GetName;
end;

procedure TTICustomCheckBox.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if FLink.Editor=nil then exit;
  if Checked then
    FLink.SetAsText(FLinkValueTrue)
  else
    FLink.SetAsText(FLinkValueFalse);
end;

constructor TTICustomCheckBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLinkValueFalse:='False';
  FLinkValueTrue:='True';
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger{,tkChar},tkEnumeration,
                 {tkFloat,}tkSet,{tkMethod,}tkSString,tkLString,tkAString,
                 tkWString,tkVariant,{tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,}tkWChar,tkBool,tkInt64,
                 tkQWord{,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.OnEditorChanged:=@LinkEditorChanged;
end;

destructor TTICustomCheckBox.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomCheckBox.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomCheckBox.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomLabel }

procedure TTICustomLabel.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomLabel.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  Caption:=FLink.GetAsText;
end;

constructor TTICustomLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,tkChar,tkEnumeration,
                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString,tkVariant,{tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,}tkWChar,tkBool,tkInt64,
                 tkQWord{,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
end;

destructor TTICustomLabel.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomLabel.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

{ TTICustomGroupbox }

procedure TTICustomGroupbox.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomGroupbox.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  Caption:=FLink.GetAsText;
end;

constructor TTICustomGroupbox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,tkChar,tkEnumeration,
                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString,tkVariant,{tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,}tkWChar,tkBool,tkInt64,
                 tkQWord{,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
end;

destructor TTICustomGroupbox.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomGroupbox.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

{ TTICustomRadioGroup }

procedure TTICustomRadioGroup.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomRadioGroup.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  ItemIndex:=Items.IndexOf(FLink.GetAsText);
end;

procedure TTICustomRadioGroup.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  if ItemIndex>=0 then
    FLink.SetAsText(Items[ItemIndex]);
end;

procedure TTICustomRadioGroup.LinkEditorChanged(Sender: TObject);
begin
  if Sender=nil then ;
  if Link=nil then exit;
  FLink.AssignCollectedAliasValuesTo(Items);
end;

constructor TTICustomRadioGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,{tkChar,}tkEnumeration,
                 {tkFloat,tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString,{tkVariant,tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,tkWChar,}tkBool{,tkInt64,}
                 {tkQWord,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.CollectValues:=true;
  FLink.OnEditorChanged:=@LinkEditorChanged;
end;

destructor TTICustomRadioGroup.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomRadioGroup.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomRadioGroup.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomCheckGroup }

procedure TTICustomCheckGroup.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomCheckGroup.LinkLoadFromProperty(Sender: TObject);
var
  i: Integer;
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
  for i:=0 to Items.Count-1 do
    Checked[i]:=Link.GetSetElementValue(Items[i]);
end;

procedure TTICustomCheckGroup.LinkSaveToProperty(Sender: TObject);
var
  i: Integer;
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
  for i:=0 to Items.Count-1 do
    Link.SetSetElementValue(Items[i],Checked[i]);
end;

procedure TTICustomCheckGroup.LinkEditorChanged(Sender: TObject);
begin
  if Sender=nil then ;
  if Link=nil then exit;
  Link.AssignSetEnumsAliasTo(Items);
end;

constructor TTICustomCheckGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,tkInteger,tkChar,tkEnumeration,}
                 {tkFloat,}tkSet{,tkMethod,tkSString,tkLString,tkAString,}
                 {tkWString,tkVariant,tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,tkWChar,tkBool,tkInt64,}
                 {tkQWord,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.CollectValues:=true;
  FLink.OnEditorChanged:=@LinkEditorChanged;
end;

destructor TTICustomCheckGroup.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomCheckGroup.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomCheckGroup.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomMemo }

function TTICustomMemo.LinkTestEditor(const ATestEditor: TPropertyEditor
  ): Boolean;
begin
  Result:=(ATestEditor is TStringPropertyEditor)
       or (ATestEditor is TStringsPropertyEditor);
end;

procedure TTICustomMemo.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomMemo.LinkLoadFromProperty(Sender: TObject);
var
  PropKind: TTypeKind;
  CurObject: TObject;
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  PropKind:=FLink.Editor.GetPropType^.Kind;
  if PropKind=tkClass then begin
    CurObject:=FLink.Editor.GetObjectValue;
    if CurObject is TStrings then
      Lines.Assign(TStrings(CurObject))
    else
      Lines.Clear;
  end else if PropKind in [tkSString,tkLString,tkAString,tkWString] then begin
    Lines.Text:=FLink.GetAsText;
  end else
    Lines.Clear;
end;

procedure TTICustomMemo.LinkSaveToProperty(Sender: TObject);
var
  PropKind: TTypeKind;
  CurObject: TObject;
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  PropKind:=FLink.Editor.GetPropType^.Kind;
  if PropKind=tkClass then begin
    CurObject:=FLink.Editor.GetObjectValue;
    if CurObject is TStrings then
      TStrings(CurObject).Assign(Lines);
  end else if PropKind in [tkSString,tkLString,tkAString,tkWString] then begin
    FLink.SetAsText(Lines.Text);
  end;
end;

constructor TTICustomMemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,tkInteger,tkChar,tkEnumeration,}
                 {tkFloat,tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString,{tkVariant,tkArray,tkRecord,tkInterface,}
                 tkClass{,tkObject,tkWChar,tkBool,tkInt64,}
                 {tkQWord,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.OnTestEditor:=@LinkTestEditor;
end;

destructor TTICustomMemo.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomMemo.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomMemo.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomCalendar }

function TTICustomCalendar.LinkTestEditor(const ATestEditor: TPropertyEditor
  ): Boolean;
begin
  Result:=(ATestEditor is TDatePropertyEditor)
       or (ATestEditor is TStringPropertyEditor);
end;

procedure TTICustomCalendar.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomCalendar.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  try
    Date:=FLink.GetAsText;
  except
    // ignore invalid dates
    on E: EInvalidDate do ;
  end;
end;

procedure TTICustomCalendar.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  FLink.SetAsText(Date);
end;

constructor TTICustomCalendar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,tkInteger,tkChar,tkEnumeration,}
                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString{,tkVariant,tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,tkWChar,tkBool,tkInt64,}
                 {tkQWord,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.OnTestEditor:=@LinkTestEditor;
end;

destructor TTICustomCalendar.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomCalendar.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomCalendar.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomFloatSpinEdit }

procedure TTICustomFloatSpinEdit.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomFloatSpinEdit.SetUseRTTIMinMax(const AValue: boolean);
begin
  if FUseRTTIMinMax=AValue then exit;
  FUseRTTIMinMax:=AValue;
  if UseRTTIMinMax then GetRTTIMinMax;
end;

procedure TTICustomFloatSpinEdit.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  try
    Value:=Single(StrToFloat(FLink.GetAsText));
  except
  end;
end;

procedure TTICustomFloatSpinEdit.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if FLink.Editor=nil then exit;
  FLink.SetAsText(FloatToStr(Value));
end;

procedure TTICustomFloatSpinEdit.LinkEditorChanged(Sender: TObject);
var
  TypeData: PTypeData;
  PropKind: TTypeKind;
  OldLinkSaveEnabled: Boolean;
  f: Extended;
begin
  if Sender=nil then ;
  if FLink.Editor=nil then exit;
  OldLinkSaveEnabled:=FLink.SaveEnabled;
  FLink.SaveEnabled:=false;
  try
    PropKind:=FLink.Editor.GetPropType^.Kind;
    case PropKind of

    tkInteger,tkChar,tkEnumeration,tkWChar:
      begin
        TypeData:=GetTypeData(FLink.Editor.GetPropType);
        MinValue:=TypeData^.MinValue;
        MaxValue:=TypeData^.MaxValue;
        Increment:=1;
        DecimalPlaces:=0;
      end;

    tkInt64:
      begin
        TypeData:=GetTypeData(FLink.Editor.GetPropType);
        MinValue:=single(TypeData^.MinInt64Value);
        MaxValue:=single(TypeData^.MaxInt64Value);
        Increment:=1;
        DecimalPlaces:=0;
      end;
      
    tkQWord:
      begin
        TypeData:=GetTypeData(FLink.Editor.GetPropType);
        MinValue:=single(TypeData^.MinQWordValue);
        MaxValue:=single(TypeData^.MaxQWordValue);
        Increment:=1;
        DecimalPlaces:=0;
      end;

    else
      begin
        try
          f:=StrToFloat(FLink.GetAsText);
        except
        end;
        if f<MinValue then MinValue:=Single(f);
        if f>MaxValue then MaxValue:=Single(f);
      end;
      
    end;
  finally
    FLink.SaveEnabled:=OldLinkSaveEnabled;
  end;
end;

procedure TTICustomFloatSpinEdit.GetRTTIMinMax;
begin
  if UseRTTIMinMax then GetRTTIMinMax;
end;

constructor TTICustomFloatSpinEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FUseRTTIMinMax:=true;
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,{tkChar,tkEnumeration,}
                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString{,tkVariant,tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,tkWChar,tkBool},tkInt64,
                 tkQWord{,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.OnEditorChanged:=@LinkEditorChanged;
end;

destructor TTICustomFloatSpinEdit.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomFloatSpinEdit.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomFloatSpinEdit.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomSpinEdit }

procedure TTICustomSpinEdit.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomSpinEdit.SetUseRTTIMinMax(const AValue: boolean);
begin
  if FUseRTTIMinMax=AValue then exit;
  FUseRTTIMinMax:=AValue;
  if UseRTTIMinMax then GetRTTIMinMax;
end;

procedure TTICustomSpinEdit.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  try
    Value:=StrToInt(FLink.GetAsText);
  except
  end;
end;

procedure TTICustomSpinEdit.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if FLink.Editor=nil then exit;
  FLink.SetAsText(IntToStr(Value));
end;

procedure TTICustomSpinEdit.LinkEditorChanged(Sender: TObject);
var
  TypeData: PTypeData;
  PropKind: TTypeKind;
  OldLinkSaveEnabled: Boolean;
  f: integer;
begin
  if Sender=nil then ;
  if FLink.Editor=nil then exit;
  OldLinkSaveEnabled:=FLink.SaveEnabled;
  FLink.SaveEnabled:=false;
  try
    PropKind:=FLink.Editor.GetPropType^.Kind;
    case PropKind of

    tkInteger,tkChar,tkEnumeration,tkWChar:
      begin
        TypeData:=GetTypeData(FLink.Editor.GetPropType);
        MinValue:=TypeData^.MinValue;
        MaxValue:=TypeData^.MaxValue;
        Increment:=1;
        DecimalPlaces:=0;
      end;

    tkInt64:
      begin
        TypeData:=GetTypeData(FLink.Editor.GetPropType);
        MinValue:=integer(TypeData^.MinInt64Value);
        MaxValue:=integer(TypeData^.MaxInt64Value);
        Increment:=1;
        DecimalPlaces:=0;
      end;

    tkQWord:
      begin
        TypeData:=GetTypeData(FLink.Editor.GetPropType);
        MinValue:=integer(TypeData^.MinQWordValue);
        MaxValue:=integer(TypeData^.MaxQWordValue);
        Increment:=1;
        DecimalPlaces:=0;
      end;

    else
      begin
        try
          f:=StrToInt(FLink.GetAsText);
        except
        end;
        if f<MinValue then MinValue:=f;
        if f>MaxValue then MaxValue:=f;
      end;

    end;
  finally
    FLink.SaveEnabled:=OldLinkSaveEnabled;
  end;
end;

procedure TTICustomSpinEdit.GetRTTIMinMax;
begin
  if UseRTTIMinMax then GetRTTIMinMax;
end;

constructor TTICustomSpinEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FUseRTTIMinMax:=true;
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,{tkChar,tkEnumeration,}
                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString{,tkVariant,tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,tkWChar,tkBool},tkInt64,
                 tkQWord{,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.OnEditorChanged:=@LinkEditorChanged;
end;

destructor TTICustomSpinEdit.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomSpinEdit.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomSpinEdit.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomImage }

function TTICustomImage.LinkTestEditor(const ATestEditor: TPropertyEditor
  ): Boolean;
begin
  Result:=(ATestEditor is TGraphicPropertyEditor);
end;

procedure TTICustomImage.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomImage.LinkLoadFromProperty(Sender: TObject);
var
  AnObject: TObject;
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  if FLink.Editor is TClassPropertyEditor then begin
    AnObject:=FLink.Editor.GetObjectValue;
    if AnObject is TImage then begin
      Picture.Assign(TImage(AnObject).Picture);
    end else if AnObject is TPicture then begin
      Picture.Assign(TPicture(AnObject));
    end else if AnObject is TGraphic then begin
      Picture.Assign(TGraphic(AnObject));
    end;
  end;
end;

constructor TTICustomImage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,tkInteger,tkChar,tkEnumeration,}
                 {tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,}
                 {tkWString,tkVariant,tkArray,tkRecord,tkInterface,}
                 tkClass{,tkObject,tkWChar,tkBool,tkInt64,}
                 {tkQWord,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnTestEditor:=@LinkTestEditor;
end;

destructor TTICustomImage.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomImage.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

{ TTICustomTrackBar }

procedure TTICustomTrackBar.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomTrackBar.SetUseRTTIMinMax(const AValue: boolean);
begin
  if FUseRTTIMinMax=AValue then exit;
  FUseRTTIMinMax:=AValue;
  if UseRTTIMinMax then GetRTTIMinMax;
end;

procedure TTICustomTrackBar.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  try
    Position:=StrToInt(FLink.GetAsText);
  except
  end;
end;

procedure TTICustomTrackBar.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  FLink.SetAsText(IntToStr(Position));
end;

procedure TTICustomTrackBar.LinkEditorChanged(Sender: TObject);
begin
  if Sender=nil then ;
  if UseRTTIMinMax then GetRTTIMinMax;
end;

procedure TTICustomTrackBar.GetRTTIMinMax;
var
  TypeData: PTypeData;
  PropKind: TTypeKind;
  OldLinkSaveEnabled: Boolean;
  i: Integer;
begin
  if FLink.Editor=nil then exit;
  OldLinkSaveEnabled:=FLink.SaveEnabled;
  FLink.SaveEnabled:=false;
  try
    PropKind:=FLink.Editor.GetPropType^.Kind;
    case PropKind of

    tkInteger,tkChar,tkEnumeration,tkWChar:
      begin
        TypeData:=GetTypeData(FLink.Editor.GetPropType);
        Min:=TypeData^.MinValue;
        Max:=TypeData^.MaxValue;
      end;

    else
      begin
        try
          i:=StrToInt(FLink.GetAsText);
        except
        end;
        if i<Min then Min:=i;
        if i>Max then Max:=i;
      end;

    end;
  finally
    FLink.SaveEnabled:=OldLinkSaveEnabled;
  end;
end;

constructor TTICustomTrackBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FUseRTTIMinMax:=true;
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,{tkChar,tkEnumeration,}
                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString{,tkVariant,tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,tkWChar,tkBool,tkInt64,}
                 {tkQWord,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.OnEditorChanged:=@LinkEditorChanged;
end;

destructor TTICustomTrackBar.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomTrackBar.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomTrackBar.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomMaskEdit }

procedure TTICustomMaskEdit.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  Text:=FLink.GetAsText;
end;

procedure TTICustomMaskEdit.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if FLink.Editor=nil then exit;
  FLink.SetAsText(Text);
end;

procedure TTICustomMaskEdit.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

constructor TTICustomMaskEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,tkChar,tkEnumeration,
                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString,tkVariant,{tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,}tkWChar,tkBool,tkInt64,
                 tkQWord{,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
end;

destructor TTICustomMaskEdit.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomMaskEdit.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomMaskEdit.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomButton }

function TTICustomButton.LinkTestEditor(const ATestEditor: TPropertyEditor
  ): Boolean;
begin
  Result:=paDialog in ATestEditor.GetAttributes;
end;

procedure TTICustomButton.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomButton.Click;
begin
  inherited Click;
  if Link.Editor<>nil then
    Link.Editor.Edit;
end;

constructor TTICustomButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=AllTypeKinds;
  FLink.OnTestEditor:=@LinkTestEditor;
end;

destructor TTICustomButton.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

{ TTICustomCheckListBox }

procedure TTICustomCheckListBox.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomCheckListBox.LinkLoadFromProperty(Sender: TObject);
var
  i: Integer;
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
  for i:=0 to Items.Count-1 do
    Checked[i]:=Link.GetSetElementValue(Items[i]);
end;

procedure TTICustomCheckListBox.LinkSaveToProperty(Sender: TObject);
var
  i: Integer;
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
  for i:=0 to Items.Count-1 do
    Link.SetSetElementValue(Items[i],Checked[i]);
end;

procedure TTICustomCheckListBox.LinkEditorChanged(Sender: TObject);
begin
  if Sender=nil then ;
  if Link=nil then exit;
  Link.AssignSetEnumsAliasTo(Items);
end;

constructor TTICustomCheckListBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,tkInteger,tkChar,tkEnumeration,}
                 {tkFloat,}tkSet{,tkMethod,tkSString,tkLString,tkAString,}
                 {tkWString,tkVariant,tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,tkWChar,tkBool,tkInt64,}
                 {tkQWord,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.CollectValues:=true;
  FLink.OnEditorChanged:=@LinkEditorChanged;
end;

destructor TTICustomCheckListBox.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomCheckListBox.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomCheckListBox.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TTICustomListBox }

procedure TTICustomListBox.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomListBox.LinkLoadFromProperty(Sender: TObject);
var
  i: Integer;
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
  if Link.Editor is TSetPropertyEditor then begin
    for i:=0 to Items.Count-1 do
      Selected[i]:=Link.GetSetElementValue(Items[i]);
  end else begin
    ItemIndex:=Items.IndexOf(Link.GetAsText);
  end;
end;

procedure TTICustomListBox.LinkSaveToProperty(Sender: TObject);
var
  i: Integer;
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
  if Link.Editor is TSetPropertyEditor then begin
    for i:=0 to Items.Count-1 do
      Link.SetSetElementValue(Items[i],Selected[i]);
  end else begin
    if ItemIndex>=0 then
      Link.SetAsText(Items[ItemIndex]);
  end;
end;

procedure TTICustomListBox.LinkEditorChanged(Sender: TObject);
begin
  if Sender=nil then ;
  if Link=nil then exit;
  if Link.Editor is TSetPropertyEditor then begin
    MultiSelect:=true;
    Link.AssignSetEnumsAliasTo(Items);
  end else begin
    Link.AssignCollectedAliasValuesTo(Items);
  end;
end;

procedure TTICustomListBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  AState: TPropEditDrawState;
  ItemValue: string;
begin
  if (Link.Editor=nil) or Link.HasAliasValues then
    inherited DrawItem(Index,ARect,State)
  else begin
    if (Index>=0) and (Index<Items.Count) then
      ItemValue:=Items[Index]
    else
      ItemValue:=Text;

    AState:=[];
    if odPainted in State then Include(AState,pedsPainted);
    if odSelected in State then Include(AState,pedsSelected);
    if odFocused in State then Include(AState,pedsFocused);
    Include(AState,pedsInEdit);

    // clear background
    with Canvas do begin
      if odSelected in State then
        Brush.Color:=clLtGray
      else
        Brush.Color:=clWhite;
      Pen.Color:=clBlack;
      Font.Color:=Pen.Color;
      FillRect(ARect);
    end;

    Link.Editor.ListDrawValue(ItemValue,Index,Canvas,ARect,AState);

    // custom draw
    if Assigned(OnDrawItem) then
      OnDrawItem(Self, Index, ARect, State);
  end;
end;

constructor TTICustomListBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[tkUnknown,tkInteger,tkChar,tkEnumeration,
                 tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                 tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                 tkClass,tkObject,tkWChar,tkBool,tkInt64,
                 tkQWord,tkDynArray,tkInterfaceRaw];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.CollectValues:=true;
  FLink.OnEditorChanged:=@LinkEditorChanged;
end;

destructor TTICustomListBox.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomListBox.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomListBox.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TPropertyLinkNotifier }

procedure TPropertyLinkNotifier.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if FLink<>nil then FLink.Notification(AComponent,Operation);
end;

constructor TPropertyLinkNotifier.Create(TheLink: TCustomPropertyLink);
begin
  inherited Create(nil);
  FLink:=TheLink;
end;

{ TTICustomColorButton }

procedure TTICustomColorButton.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomColorButton.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  ButtonColor:=TColor(FLink.GetAsInt);
end;

procedure TTICustomColorButton.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  FLink.SetAsInt(ButtonColor);
end;

function TTICustomColorButton.LinkTestEditor(const ATestEditor: TPropertyEditor
  ): Boolean;
begin
  Result:=(ATestEditor is TColorPropertyEditor)
          and (paDialog in ATestEditor.GetAttributes);
end;

procedure TTICustomColorButton.ShowColorDialog;
begin
  if Link.Editor<>nil then
    Link.Editor.Edit;
  FLink.LoadFromProperty;
end;

constructor TTICustomColorButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger{,tkChar,tkEnumeration,
                 tkFloat,tkSet,tkMethod,tkSString,tkLString,tkAString,
                 tkWString,tkVariant,tkArray,tkRecord,tkInterface,
                 tkClass,tkObject,tkWChar,tkBool,tkInt64,
                 tkQWord,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.OnTestEditor:=@LinkTestEditor;
end;

destructor TTICustomColorButton.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomColorButton.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomColorButton.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TMultiPropertyLink }

procedure TMultiPropertyLink.SetTIObject(const AValue: TPersistent);
begin
  if FTIObject=AValue then exit;
  FTIObject:=AValue;
  if Assigned(OnSetTIObject) then OnSetTIObject(Self);
  SetLinks;
end;

procedure TMultiPropertyLink.SetMaintainGrandChilds(const AValue: boolean);
begin
  if FMaintainGrandChilds=AValue then exit;
  FMaintainGrandChilds:=AValue;
  if FMaintainGrandChilds then SetLinks;
end;

procedure TMultiPropertyLink.SetMaintainSiblings(const AValue: boolean);
begin
  if FMaintainSiblings=AValue then exit;
  FMaintainSiblings:=AValue;
  if FMaintainSiblings then SetLinks;
end;

procedure TMultiPropertyLink.SetParentControl(const AValue: TWinControl);
begin
  if FParentControl=AValue then exit;
  FParentControl:=AValue;
  if FParentControl<>nil then SetLinks;
end;

procedure TMultiPropertyLink.SetRootComponent(const AValue: TComponent);
begin
  if FRootComponent=AValue then exit;
  FRootComponent:=AValue;
  if FRootComponent<>nil then SetLinks;
end;

constructor TMultiPropertyLink.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMaintainSiblings:=true;
end;

procedure TMultiPropertyLink.SetLinks;
begin
  if [csLoading,csDestroying]*ComponentState<>[] then exit;
  if RootComponent<>nil then
    SetLinksForChildComponents(RootComponent);
  if ParentControl<>nil then
    SetLinksForChildControls(ParentControl,MaintainGrandChilds);
  if MaintainSiblings and (Owner<>nil) then
    SetLinksForChildComponents(Owner);
end;

procedure TMultiPropertyLink.SetLinksForChildControls(AParent: TWinControl;
  WithGrandChilds: boolean);
var
  i: Integer;
  CurControl: TControl;
  CurLink: TCustomPropertyLink;
begin
  if AParent<>nil then begin
    for i:=0 to AParent.ControlCount-1 do begin
      CurControl:=AParent.Controls[i];
      CurLink:=GetPropertyLinkOfComponent(CurControl);
      if CurLink<>nil then
        CurLink.TIObject:=TIObject;
      if WithGrandChilds and (CurControl is TWinControl) then
        SetLinksForChildControls(TWinControl(CurControl),true);
    end;
  end;
end;

procedure TMultiPropertyLink.SetLinksForChildComponents(AComponent: TComponent);
var
  i: Integer;
  CurComponent: TComponent;
  CurLink: TCustomPropertyLink;
begin
  if AComponent<>nil then begin
    for i:=0 to AComponent.ComponentCount-1 do begin
      CurComponent:=AComponent.Components[i];
      CurLink:=GetPropertyLinkOfComponent(CurComponent);
      if CurLink<>nil then
        CurLink.TIObject:=TIObject;
    end;
  end;
end;

procedure TMultiPropertyLink.Loaded;
begin
  inherited Loaded;
  SetLinks;
end;

{ TTIElementNamePropertyEditor }

procedure TTIElementNamePropertyEditor.GetCompatiblePropEdits(
  Prop: TPropertyEditor);
begin
  if FPropEdits=nil then FPropEdits:=TList.Create;
  FPropEdits.Add(Prop);
end;

procedure TTIElementNamePropertyEditor.GetElementPropEdits(Prop: TPropertyEditor);
begin
  if FElementPropEdits=nil then FElementPropEdits:=TList.Create;
  FElementPropEdits.Add(Prop);
end;

function TTIElementNamePropertyEditor.TestEditor(const Prop: TPropertyEditor
  ): boolean;
var
  i: Integer;
  CurPersistent: TPersistent;
  ALink: TCustomPropertyLink;
begin
  Result:=false;
  for i:=0 to PropCount-1 do begin
    CurPersistent:=GetComponent(i);
    if (CurPersistent is TCustomPropertyLink) then begin
      ALink:=TCustomPropertyLink(CurPersistent);
      //debugln('TTIElementNamePropertyEditor.TestEditor ',ALink.TIPropertyName,' ',Prop.GetName);
      if (CompareText(ALink.TIPropertyName,Prop.GetName)<>0) then exit;
      if Assigned(ALink.OnTestEditor) and (not ALink.OnTestEditor(Prop)) then
        exit;
      //debugln('TTIElementNamePropertyEditor.TestEditor ok ',ALink.TIPropertyName);
    end;
  end;
  Result:=true;
end;

function TTIElementNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paMultiSelect,paValueList,paSortList,paRevertable,paHasDefaultValue];
end;

function TTIElementNamePropertyEditor.GetEditLimit: Integer;
begin
  Result:=255;
end;

procedure TTIElementNamePropertyEditor.GetValues(Proc: TGetStringProc);
var
  ALink: TCustomPropertyLink;
  ASelection: TPersistentSelectionList;
  i: Integer;
  CurPersistent: TPersistent;
  CurTIObject: TPersistent;
  Filter: TTypeKinds;
  CurPropEdit: TPropertyEditor;
  j: Integer;
begin
  ASelection:=TPersistentSelectionList.Create;
  try
    // get every TIObject of every TCustomPropertyLink in the selection
    Filter:=AllTypeKinds;
    for i:=0 to PropCount-1 do begin
      CurPersistent:=GetComponent(i);
      if (CurPersistent is TCustomPropertyLink) then begin
        ALink:=TCustomPropertyLink(CurPersistent);
        CurTIObject:=ALink.TIObject;
        if CurTIObject<>nil then begin
          ASelection.Add(CurTIObject);
          Filter:=Filter*ALink.Filter;
        end;
      end;
    end;
    if ASelection.Count=0 then exit;
    // get properties of all TIObjects
    GetPersistentProperties(ASelection,Filter,PropertyHook,
      @GetCompatiblePropEdits,nil,@TestEditor);
    if FPropEdits<>nil then begin
      // get the possible element values:
      for i:=0 to FPropEdits.Count-1 do begin
        CurPropEdit:=TPropertyEditor(FPropEdits[i]);
        if paValueList in CurPropEdit.GetAttributes then
        begin
          // get value list
          CurPropEdit.GetValues(Proc);
          break;
        end else if paSubProperties in CurPropEdit.GetAttributes then begin
          // get names of sub property editors
          CurPropEdit.GetProperties(@GetElementPropEdits);
          if FElementPropEdits<>nil then begin
            for j:=0 to FElementPropEdits.Count-1 do
              Proc(TPropertyEditor(FElementPropEdits[j]).GetName);
            break;
          end;
        end;
      end;
    end;
  finally
    ASelection.Free;
    if FPropEdits<>nil then begin
      for i:=0 to FPropEdits.Count-1 do
        TPropertyEditor(FPropEdits[i]).Free;
      FreeThenNil(FPropEdits);
    end;
    if FElementPropEdits<>nil then begin
      for i:=0 to FElementPropEdits.Count-1 do
        TPropertyEditor(FElementPropEdits[i]).Free;
      FreeThenNil(FElementPropEdits);
    end;
  end;
end;

{ TTICustomProgressBar }

procedure TTICustomProgressBar.SetLink(const AValue: TPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TTICustomProgressBar.SetUseRTTIMinMax(const AValue: boolean);
begin
  if FUseRTTIMinMax=AValue then exit;
  FUseRTTIMinMax:=AValue;
  if UseRTTIMinMax then GetRTTIMinMax;
end;

procedure TTICustomProgressBar.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  try
    Position:=StrToInt(FLink.GetAsText);
  except
  end;
end;

procedure TTICustomProgressBar.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (FLink.Editor=nil) then exit;
  FLink.SetAsText(IntToStr(Position));
end;

procedure TTICustomProgressBar.LinkEditorChanged(Sender: TObject);
begin
  if Sender=nil then ;
  if UseRTTIMinMax then GetRTTIMinMax;
end;

procedure TTICustomProgressBar.GetRTTIMinMax;
var
  TypeData: PTypeData;
  PropKind: TTypeKind;
  OldLinkSaveEnabled: Boolean;
  i: Integer;
begin
  if FLink.Editor=nil then exit;
  OldLinkSaveEnabled:=FLink.SaveEnabled;
  FLink.SaveEnabled:=false;
  try
    PropKind:=FLink.Editor.GetPropType^.Kind;
    case PropKind of

    tkInteger,tkChar,tkEnumeration,tkWChar:
      begin
        TypeData:=GetTypeData(FLink.Editor.GetPropType);
        Min:=TypeData^.MinValue;
        Max:=TypeData^.MaxValue;
      end;

    else
      begin
        try
          i:=StrToInt(FLink.GetAsText);
        except
        end;
        if i<Min then Min:=i;
        if i>Max then Max:=i;
      end;

    end;
  finally
    FLink.SaveEnabled:=OldLinkSaveEnabled;
  end;
end;

constructor TTICustomProgressBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FUseRTTIMinMax:=true;
  FLink:=TPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,{tkChar,tkEnumeration,}
                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString{,tkVariant,tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,tkWChar,tkBool,tkInt64,}
                 {tkQWord,tkDynArray,tkInterfaceRaw}];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
  FLink.OnEditorChanged:=@LinkEditorChanged;
end;

destructor TTICustomProgressBar.Destroy;
begin
  FreeThenNil(FLink);
  inherited Destroy;
end;

procedure TTICustomProgressBar.Loaded;
begin
  inherited Loaded;
  FLink.LoadFromProperty;
end;

procedure TTICustomProgressBar.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

{ TPropertyLink }

procedure TPropertyLink.ReadAliasValuesData(Reader: TReader);
begin
  Reader.ReadListBegin;
  AliasValues.BeginUpdate;
  try
    AliasValues.Clear;
    while not Reader.EndOfList do
      AliasValues.Add(Reader.ReadString);
  finally
    AliasValues.EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TPropertyLink.WriteAliasValuesData(Writer: TWriter);
var
  i: Integer;
begin
  Writer.WriteListBegin;
  for i := 0 to AliasValues.Count - 1 do
    Writer.WriteString(AliasValues[i]);
  Writer.WriteListEnd;
end;

procedure TPropertyLink.DefineProperties(Filer: TFiler);
var
  HasAliasValuesData: Boolean;
  AncestorPropList: TCustomPropertyLink;
begin
  inherited DefineProperties(Filer);
  HasAliasValuesData := AliasValues.Count > 0;
  if Assigned(Filer.Ancestor) then begin
    // Only serialize if string list is different from ancestor
    if Filer.Ancestor.InheritsFrom(TCustomPropertyLink) then begin
      AncestorPropList:=TCustomPropertyLink(Filer.Ancestor);
      HasAliasValuesData := not AliasValues.Equals(AncestorPropList.AliasValues);
    end;
  end;
  Filer.DefineProperty('AliasValuesStrings',
               @ReadAliasValuesData, @WriteAliasValuesData, HasAliasValuesData);
end;

initialization
  {$I rttictrls.lrs}
  // TPropertyLink
  RegisterPropertyEditor(ClassTypeInfo(TPropertyLink),
    nil, '', TPropertyLinkPropertyEditor);
  // property editor for TCustomPropertyLink.TIObject
  RegisterPropertyEditor(ClassTypeInfo(TPersistent),
    TCustomPropertyLink, 'TIObject', TTIObjectPropertyEditor);
  // property editor for TCustomPropertyLink.TIPropertyName
  RegisterPropertyEditor(TypeInfo(string),
    TCustomPropertyLink, 'TIPropertyName', TPropertyNamePropertyEditor);
  // property editor for TCustomPropertyLink.TIElementName
  RegisterPropertyEditor(TypeInfo(string),
    TCustomPropertyLink, 'TIElementName', TTIElementNamePropertyEditor);
  // property editor for TCustomPropertyLink.AliasValues
  RegisterPropertyEditor(ClassTypeInfo(TAliasStrings),
    TCustomPropertyLink, 'AliasValues', TPropLinkAliasPropertyEditor);
  // property editor for TMultiPropertyLink.TIObject
  RegisterPropertyEditor(ClassTypeInfo(TPersistent),
    TMultiPropertyLink, 'TIObject', TTIObjectPropertyEditor);
  RegisterComponentEditor(TTIMaskEdit, TMaskEditEditor);
end.
