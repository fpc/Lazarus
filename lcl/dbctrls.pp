{ $Id$}
{
 /***************************************************************************
                               DbCtrls.pp
                               ----------
                     An interface to DB aware Controls
                     Initial Revision : Sun Sep 14 2003


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{
@abstract(common db aware controls, as in Delphi)
@author(Andrew Johnson <acjgenius@@earthlink.net>)
@created(Sun Sep 14 2003)
@lastmod($Date$)
}
unit DBCtrls;

{$mode objfpc}
{$H+}

interface          

uses
  Types, Classes, SysUtils, DB, Variants,
  // LazUtils
  LazTracer, LazUtilities,
  // LCL
  LCLStrConsts, LMessages, LCLType, LCLIntf, LResources, GraphType, Controls, Graphics,
  Dialogs, StdCtrls, Buttons, MaskEdit, ExtCtrls, Calendar, ImgList;

Type
  { TFieldDataLink }

  TFieldDataLink = class(TDataLink)
  private
    FField: TField;
    FFieldName: string;
    FControl: TComponent;
    // Callbacks
    FOnDataChange: TNotifyEvent;
    FOnEditingChange: TNotifyEvent;
    FOnUpdateData: TNotifyEvent;
    FOnActiveChange: TNotifyEvent;
    // Curent State of Affairs
    FEditing: Boolean;
    FEditingSourceSet: boolean;
    FEditingSource: Boolean;
    IsModified: Boolean;
    function FieldCanModify: boolean;
    function IsKeyField(aField: TField): Boolean;
    function GetCanModify: Boolean;
    // set current field
    procedure SetFieldName(const Value: string);
    procedure UpdateField;
    // make sure the field/fieldname is valid before we do stuff with it
    procedure ValidateField;
    procedure ResetEditingSource;
  protected
    // Testing Events
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(aField: TField); override;
    procedure UpdateData; override;

    procedure FocusControl(aField: TFieldRef); Override;
  public
    constructor Create;
    // for control intitiating db changes etc
    function Edit: Boolean;

    procedure Modified;
    procedure Reset;

    // Attached control
    property Control: TComponent read FControl write FControl;

    // Basic DB interfaces
    property Field: TField read FField;
    property FieldName: string read FFieldName write SetFieldName;

    // Current State of DB
    property CanModify: Boolean read GetCanModify;
    property Editing: Boolean read FEditing;
    property EditingSource: boolean read FEditingSource;

    // Our Callbacks
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnEditingChange: TNotifyEvent read FOnEditingChange write FOnEditingChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;



  { TDBLookup }
  {
  TDBLookup component is typically owned by a Lookup control like
  TDBLookupListBox or TDBLookupComboBox.
  The ListSource is the other dataset TDataSource from which to retrieve the lookup data
  The KeyField is the lookup key in the ListSource which corresponds to the DataField value
  The ListField is the name of the field in the ListSource to list into the
  Items property of the lookup control.
  which  data
  }

  TDBLookup = class(TComponent)
  private
    FControlLink: TFieldDataLink;
    FControlItems: TStrings;
    FListLink: TDataLink;
    FListSource: TDataSource;
    FLookupSource: TDataSource;
    FDataFieldNames: string;
    FKeyFieldNames: string;
    FListFieldName: string;
    FListFieldIndex: Integer;
    FDataFields: TList;  // Data Fields to lookup/edit
    FKeyFields: TList;   // Keyfields in lookup dataset
    FListField: TField;  // Result field in lookup dataset
    FListKeys: array of Variant;
    FNullValueKey: TShortcut;
    FHasLookUpField: Boolean;
    FLookUpFieldIsCached: Boolean;
    FLookupCache: Boolean;
    FInitializing: Boolean;
    {$IF FPC_FULLVERSION < 30000}
    FFetchingLookupData: Boolean;
    {$ENDIF}
    procedure ActiveChange(Sender: TObject);
    procedure DatasetChange(Sender: TObject);
    procedure DoInitialize;
    procedure FetchLookupData;
    function GetKeyFieldName: string;
    function GetListSource: TDataSource;
    procedure SetKeyFieldName(const Value: string);
    procedure SetListFieldName(const Value: string);
    procedure SetListSource(Value: TDataSource);
    procedure SetLookupCache(const Value: boolean);
    function HandleNullKey(var Key: Word; Shift: TShiftState): Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(AControlDataLink: TFieldDataLink; AControlItems: TStrings);
    function KeyFieldValue: Variant;
    procedure UpdateData(ValueIndex: Integer; ScrollDataset: Boolean);
    function  GetKeyValue(ValueIndex: Integer): Variant;
    function  GetKeyIndex: Integer;
    function  GetKeyIndex(const AKeyValue: Variant): Integer;
    property ControlItems: TStrings read FControlItems write FControlItems;
    property LookupCache: boolean read FLookupCache  write SetLookupCache;
    // properties to be published by owner control
    // these are not used where data control Field is dbLookup
    property KeyField: string read GetKeyFieldName write SetKeyFieldName;
    property ListField: string read FListFieldName write SetListFieldName;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex default 0;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property NullValueKey: TShortcut read FNullValueKey write FNullValueKey;
  end;

  { TDBEdit }

  TDBEdit = class(TCustomMaskEdit)
  private
    FDataLink: TFieldDataLink;
    FCustomEditMask: Boolean;
    FFocusedDisplay: boolean;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function EditCanModify: Boolean; override;
    function GetEditText: string; override;

    procedure Change; override;
    procedure Reset; override;

    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WndProc(var Message: TLMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property CustomEditMask: Boolean read FCustomEditMask write FCustomEditMask default False;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;


  { TDBText }

  TDBText = class(TCustomLabel)
  private
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    class procedure WSRegisterClass; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnChangeBounds;
    property OnContextPopup;
    property OnResize;
    property OnStartDrag;
    property OptimalFill;
  end;

  { TCustomDBListBox }

  TCustomDBListBox = class(TCustomListBox)
  private
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);

    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject); virtual; abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateData(Sender: TObject); virtual; abstract;
    // we need to override the Items Write method for db aware.
    procedure SetItems(Values : TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Field: TField read GetField;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    //same as dbedit need to match the datalink status
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

  { TDBListBox }

  TDBListBox = class(TCustomDBListBox)
  protected
    procedure DataChange(Sender: TObject); override;
    procedure DoSelectionChange(User: Boolean); override;
    procedure UpdateData(Sender: TObject); override;
  public
    procedure EditingDone; override;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property DataField;
    property DataSource;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property Options;
    property ParentBiDiMode;
    property ParentDoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;


  { TDBLookupListBox }

  TDBLookupListBox = class(TCustomDBListBox)
  private
    FLookup: TDBLookup;
    FScrollListDataset: Boolean;
    procedure ActiveChange(Sender: TObject);
    function GetKeyField: string;
    function GetKeyValue: Variant;
    function GetListField: string;
    function GetListFieldIndex: Integer;
    function GetListSource: TDataSource;
    function GetLookupCache: boolean;
    function GetNullValueKey: TShortCut;
    procedure SetKeyField(const Value: string);
    procedure SetKeyValue(const AValue: Variant);
    procedure SetListField(const Value: string);
    procedure SetListFieldIndex(const Value: Integer);
    procedure SetListSource(const Value: TDataSource);
    procedure SetLookupCache(const Value: boolean);
    procedure SetNullValueKey(const AValue: TShortCut);
    procedure UpdateLookup;
  protected
    procedure DataChange(Sender: TObject); override;
    procedure DoSelectionChange(User: Boolean); override;
    procedure InitializeWnd; override;
    procedure DestroyWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure UpdateData(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property DataField;
    property DataSource;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
//    property ExtendedSelect;
//    property ItemHeight;
    property Enabled;
    property Font;
    property KeyField: string read GetKeyField write SetKeyField;
    property ListField: string read GetListField write SetListField;
    property ListFieldIndex: Integer read GetListFieldIndex write SetListFieldIndex;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property LookupCache: boolean read GetLookupCache  write SetLookupCache;
    property NullValueKey: TShortCut read GetNullValueKey write SetNullValueKey default 0;
//    property MultiSelect;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
//    property OnDrawItem;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property Options;
    property ParentBiDiMode;
    property ParentDoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollListDataset: Boolean read FScrollListDataset write FScrollListDataset default False;
    property ShowHint;
    property Sorted;
//    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;


  { TDBRadioGroup }

  TDBRadioGroup = class(TCustomRadioGroup)
  private
    FDataLink: TFieldDataLink;
    FOnChange: TNotifyEvent;
    FValue: string;
    FValues: TStrings;
    FInSetValue: boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetItems(const AValue: TStrings);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetValue(const AValue: string);
    procedure SetValues(const AValue: TStrings);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    procedure Change; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    property DataLink: TFieldDataLink read FDataLink;
    function GetButtonValue(Index: Integer): string;
    procedure UpdateRadioButtonStates; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Field: TField read GetField;
    property ItemIndex;
    property Value: string read FValue write SetValue;
  published
    property Align;
    property Anchors;
    property AutoFill;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property ChildSizing;
    property Color;
    property ColumnLayout;
    property Columns;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DoubleBuffered;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Items write SetItems;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Values: TStrings read FValues write SetValues;
    property Visible;
  end;


  { TDBCheckBox }

  TDBCheckBox = class(TCustomCheckBox)
  private
    FDataLink: TFieldDataLink;
    FValueChecked: string;
    FValueUnchecked: string;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetValueChecked(const AValue: string);
    procedure SetValueUnchecked(const AValue: string);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    function GetFieldCheckState: TCheckBoxState; virtual;
    procedure DataChange(Sender: TObject);
    procedure DoOnChange; override;
    procedure UpdateData(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Checked;
    property Field: TField read GetField;
    property State;
  published
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property OnChange;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ValueChecked: string read FValueChecked write SetValueChecked;
    property ValueUnchecked: string read FValueUnchecked write SetValueUnchecked;
    property Visible;
  end;
  
  
  { TCustomDBComboBox }

  TCustomDBComboBox = class(TCustomComboBox)
  private
    FDataLink: TFieldDataLink;
    FDetectedEvents: Word;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetReadOnly(const AValue: Boolean);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    function DoEdit: boolean; virtual;
    procedure DoOnCloseUp; virtual;
    procedure DoOnSelect; virtual;
    procedure LMDeferredEdit(var Message: TLMessage); message LM_DEFERREDEDIT;
    property DetectedEvents: Word read FDetectedEvents;
  protected
    procedure CloseUp; override;
    Procedure Select; override;
    procedure DataChange(Sender: TObject); virtual; abstract;
    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UpdateData(Sender: TObject); virtual; abstract;
    procedure UpdateRecord;
    procedure WndProc(var Message: TLMessage); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    procedure EditingDone; override;
    property Field: TField read GetField;
    property Text;
    property ItemIndex;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;


  { TDBComboBox }

  TDBComboBox = class(TCustomDBComboBox)
  protected
    procedure DataChange(Sender: TObject); override;
    procedure KeyPress(var Key: char); override;
    procedure UpdateData(Sender: TObject); override;
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property DataField;
    property DataSource;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property Items;
    property ItemWidth;
    property MaxLength default -1;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelect;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


  { TDBLookupComboBox }

  TDBLookupComboBox = class(TCustomDBComboBox)
  protected
    function DoEdit: boolean; override;
  private
    FLookup: TDBLookup;
    FScrollListDataset: Boolean;
    procedure ActiveChange(Sender: TObject);
    function GetKeyField: string;
    function GetKeyValue: variant;
    function GetListField: string;
    function GetListFieldIndex: Integer;
    function GetListSource: TDataSource;
    function GetLookupCache: boolean;
    function GetNullValueKey: TShortCut;
    procedure SetKeyField(const Value: string);
    procedure SetKeyValue(const AValue: variant);
    procedure SetListField(const Value: string);
    procedure SetListFieldIndex(const Value: Integer);
    procedure SetListSource(const Value: TDataSource);
    procedure SetLookupCache(const Value: boolean);
    procedure SetNullValueKey(const AValue: TShortCut);
    procedure UpdateLookup;
  protected
    procedure InitializeWnd; override;
    procedure DestroyWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure Loaded; override;
    procedure UpdateData(Sender: TObject); override;
    procedure DataChange(Sender: TObject); override;
    procedure DoOnSelect; override;
  public
    constructor Create(AOwner: TComponent); override;
    property KeyValue: variant read GetKeyValue write SetKeyValue;
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    //property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DataField;
    property DataSource;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
//    property ItemHeight;
//    property ItemWidth;
    property KeyField: string read GetKeyField write SetKeyField;
    property ListField: string read GetListField write SetListField;
    property ListFieldIndex: Integer read GetListFieldIndex write SetListFieldIndex;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property LookupCache: boolean read GetLookupCache  write SetLookupCache;
//    property MaxLength default -1;
    property NullValueKey: TShortCut read GetNullValueKey write SetNullValueKey default 0;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelect;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollListDataset: Boolean read FScrollListDataset write FScrollListDataset default False;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

  { TDBMemo }

  TDBMemo = class(TCustomMemo)
  private
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FDBMemoFocused: Boolean;
    FDBMemoLoaded: Boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetAutoDisplay(const AValue: Boolean);
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(AValue: Boolean); override;
    procedure DataChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateData(Sender: TObject);
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key:Char); override;
    procedure WndProc(var Message : TLMessage); override;
    class procedure WSRegisterClass; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    procedure LoadMemo; virtual;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property OnChange;
    property OnClick;
    property OnContextPopup;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBiDiMode;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Tabstop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
  end;
  
  
  { TDBGroupBox }
  
  TDBGroupBox = class(TCustomGroupBox)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    procedure DataChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Field: TField read GetField;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property Cursor;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;
  

  { TDBImage }

  TOnDBImageRead = procedure(Sender: TObject; S: TStream; var GraphExt : string) of object;
  TOnDBImageWrite = procedure(Sender: TObject; S: TStream; GraphExt : string) of object;

  TDBImage = class(TCustomImage)
  private
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FOnDBImageRead: TOnDBImageRead;
    FOnDBImageWrite: TOnDBImageWrite;
    FQuickDraw: Boolean;
    FPictureLoaded: boolean;
    FUpdatingRecord: boolean;
    FWriteHeader: Boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetAutoDisplay(const AValue: Boolean);
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetReadOnly(const AValue: Boolean);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject); virtual;
    procedure PictureChanged(Sender: TObject); override;
    class procedure WSRegisterClass; override;
    procedure DoCopyToClipboard;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    property Field: TField read GetField;
    procedure Change; virtual;

    procedure LoadPicture; virtual;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    property PictureLoaded : boolean read FPictureLoaded;
  published
    property Align;
    property Anchors;
    property AntialiasingMode;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property AutoSize;
    property BorderSpacing;
    property Center;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property KeepOriginXWhenClipped;
    property KeepOriginYWhenClipped;
    property OnClick;
    property OnDblClick;
    property OnDBImageRead: TOnDBImageRead read  FOnDBImageRead write FOnDBImageRead;
    property OnDBImageWrite: TOnDBImageWrite read FOnDBImageWrite write FOnDBImageWrite;
    property PopupMenu;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property ParentShowHint;
    property Proportional;
    property QuickDraw: Boolean read FQuickDraw write FQuickDraw default True;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property Stretch;
    property StretchInEnabled;
    property StretchOutEnabled;
    property Transparent;
    property Visible;
    property WriteHeader: Boolean read FWriteHeader write FWriteHeader default True;
  end;

  { TDBCalendar }

  TDBCalendar = class(TCalendar)
  private
    FDataLink: TFieldDataLink;

    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);

    procedure SetDate(const AValue: String);

    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure UpdateDate(const AValue: string);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;

    property Field: TField read GetField;
  published
    property BorderSpacing;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    Property Date write SetDate stored False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;

    property DisplaySettings stored False;
    property DoubleBuffered;
    property DragCursor;
    property DragMode;
    property ParentDoubleBuffered;
    property Visible;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseMove;
    property OnMouseDown;
    property OnDayChanged;
    property OnMonthChanged;
    property OnStartDrag;
    property OnYearChanged;
  end;


  { TDBCustomNavigator }

type
  TDBNavButton = class;
  TDBNavFocusableButton = class;
  TDBNavDataLink = class;

  TDBNavGlyph = (ngEnabled, ngDisabled);
  TDBNavButtonType = (nbFirst, nbPrior, nbNext, nbLast,
                  nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh);
  TDBNavButtonSet = set of TDBNavButtonType;
  TDBNavButtonStyle = set of (nsAllowTimer, nsFocusRect);
  TDBNavButtonDirection = (nbdHorizontal,nbdVertical);
  TDBNavigatorOption = (navFocusableButtons);
  TDBNavigatorOptions = set of TDBNavigatorOption;

  // for Delphi compatibility
  TNavigateBtn = TDBNavButtonType;

  TDBNavClickEvent = procedure(Sender: TObject;
                                Button: TDBNavButtonType) of object;
  
const
  DefaultDBNavigatorButtons = [nbFirst, nbPrior, nbNext, nbLast,
    nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  DBNavButtonResourceName: array[TDBNavButtonType] of string = (
 { nbFirst   } 'DBNavFirst',
 { nbPrior   } 'DBNavPrior',
 { nbNext    } 'DBNavNext',
 { nbLast    } 'DBNavLast',
 { nbInsert  } 'DBNavInsert',
 { nbDelete  } 'DBNavDelete',
 { nbEdit    } 'DBNavEdit',
 { nbPost    } 'DBNavPost',
 { nbCancel  } 'DBNavCancel',
 { nbRefresh } 'DBNavRefresh'
    );

type

  { TDBCustomNavigator }

  TDBCustomNavigator = class(TCustomPanel)
  private
    FBeforeAction: TDBNavClickEvent;
    FDataLink: TDBNavDataLink;
    FDirection: TDBNavButtonDirection;
    FOnNavClick: TDBNavClickEvent;
    FVisibleButtons: TDBNavButtonSet;
    FDefaultHints: TStrings;
    FHints: TStrings;
    FUpdateButtonsLock: integer;
    FOriginalHints: String;
    FOptions: TDBNavigatorOptions;
    FFlat: Boolean;
    FConfirmDelete: Boolean;
    FUpdateButtonsNeeded: boolean;
    FShowButtonHints: boolean;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    procedure DefaultHintsChanged(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetHints: TStrings;
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetDirection(const AValue: TDBNavButtonDirection);
    procedure SetFlat(const AValue: Boolean);
    procedure SetHints(const AValue: TStrings);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetOptions(AValue: TDBNavigatorOptions);
    procedure SetShowButtonHints(const AValue: boolean);
    procedure SetVisibleButtons(const AValue: TDBNavButtonSet);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    procedure ImageListChange(Sender: TObject);
  protected
    Buttons: array[TDBNavButtonType] of TDBNavButton;
    FocusableButtons: array[TDBNavButtonType] of TDBNavFocusableButton;
    procedure DataChanged; virtual;
    procedure EditingChanged; virtual;
    procedure ActiveChanged; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateButtons; virtual;
    procedure UpdateHints; virtual;
    procedure HintsChanged(Sender: TObject); virtual;
    procedure ButtonClickHandler(Sender: TObject); virtual;
    class function GetControlClassDefaultSize: TSize; override;
    procedure BeginUpdateButtons; virtual;
    procedure EndUpdateButtons; virtual;
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BtnClick(Index: TNavigateBtn); virtual;
    function VisibleButtonCount: integer; virtual;
  public
    property BeforeAction: TDBNavClickEvent read FBeforeAction write FBeforeAction;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Direction: TDBNavButtonDirection read FDirection write SetDirection default nbdHorizontal;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Hints: TStrings read GetHints write SetHints;
    property Options: TDBNavigatorOptions read FOptions write SetOptions;
    property OnClick: TDBNavClickEvent read FOnNavClick write FOnNavClick;
    property VisibleButtons: TDBNavButtonSet read FVisibleButtons
                             write SetVisibleButtons default DefaultDBNavigatorButtons;
    property ShowButtonHints: boolean read FShowButtonHints write SetShowButtonHints default true;
    property Images: TCustomImageList read FImages write SetImages;
  end;
  
  
  { TDBNavButton }

  TDBNavButton = class(TSpeedButton)
  private
    FIndex: TDBNavButtonType;
    FNavStyle: TDBNavButtonStyle;
  protected
  public
    destructor Destroy; override;
    property NavStyle: TDBNavButtonStyle read FNavStyle write FNavStyle;
    property Index: TDBNavButtonType read FIndex write FIndex;
  end;

  { TDBNavFocusableButton }

  TDBNavFocusableButton = class(TBitBtn)
  private
    FIndex: TDBNavButtonType;
    FNavStyle: TDBNavButtonStyle;
  public
    property NavStyle: TDBNavButtonStyle read FNavStyle write FNavStyle;
    property Index: TDBNavButtonType read FIndex write FIndex;
  end;

  { TNavDataLink }

  TDBNavDataLink = class(TDataLink)
  private
    FNavigator: TDBCustomNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(TheNavigator: TDBCustomNavigator);
  end;


  { TDBNavigator }

  TDBNavigator = class(TDBCustomNavigator)
  published
    property Align default alNone;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BeforeAction;
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
    property Color default clBackground;
    property ConfirmDelete;
    property DataSource;
    property Direction;
    property DoubleBuffered;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property Hints;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property Options;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default False;
    property Visible;
    property VisibleButtons;
    property Images;
  end;

procedure ChangeDataSource(AControl: TControl; Link: TDataLink;
  NewDataSource: TDataSource);

procedure Register;

implementation

{$R lcl_dbnav_images.res}

uses
  InterfaceBase, Clipbrd;

var
  FieldClasses: TFpList;

procedure RegField(const FieldClass: TFieldClass);
begin
  if FieldClasses = nil then FieldClasses := TFpList.Create;
  if (FieldClass <> Nil) And (FieldClasses.IndexOf(FieldClass) = -1) then
  begin
    FieldClasses.Add(FieldClass);
    RegisterNoIcon([FieldClass]);
    RegisterClass(FieldClass);
  end;
end;

procedure RegFields(const AFieldClasses: array of TFieldClass);
var I: Integer;
begin
  for I := Low(AFieldClasses) to High(AFieldClasses) do
    RegField(AFieldClasses[I]);
end;

procedure ChangeDataSource(AControl: TControl; Link: TDataLink;
  NewDataSource: TDataSource);
begin
  if Link.DataSource=NewDataSource then exit;
  if Link.DataSource<>nil then
    Link.DataSource.RemoveFreeNotification(AControl);
  Link.DataSource:=NewDataSource;
  if Link.DataSource<>nil then
    Link.DataSource.FreeNotification(AControl);
end;

function FieldIsEditable(Field: TField): boolean;
begin
  result := (Field<>nil) and (not Field.Calculated) and
            (Field.DataType<>ftAutoInc) and (Field.FieldKind<>fkLookup)
end;

function FieldCanAcceptKey(Field: TField; AKey: char): boolean;
begin
  Result := FieldIsEditable(Field) and Field.IsValidChar(AKey);
end;

procedure Register;
begin
  RegisterComponents('Data Controls',[TDBNavigator,TDBText,TDBEdit,TDBMemo,
    TDBImage,TDBListBox,TDBLookupListBox,TDBComboBox,TDBLookupComboBox,
    TDBCheckBox, TDBRadioGroup, TDBCalendar,TDBGroupBox]);
  RegFields(DefaultFieldClasses);
  RegField(TIntegerField);
end;

function TFieldDataLink.FieldCanModify: boolean;
var
  FieldList: TList;
  i: Integer;
begin
  result := Assigned(FField);
  if not result then
    exit;

  if FField.FieldKind=fkLookup then
  begin
    FieldList := TList.Create;
    try
      DataSet.GetFieldList(FieldList, FField.KeyFields);
      result := (FieldList.Count>0);
      i := 0;
      while result and (i<FieldList.Count) do
      begin
        result := TField(FieldList[i]).CanModify;
        inc(i);
      end;
    finally
      FieldList.Free;
    end;
  end else
    result := FField.CanModify;
end;

function TFieldDataLink.IsKeyField(aField: TField): Boolean;
var
  KeyFieldName, KeyFields: String;
  StrPos: Integer;
begin
  KeyFields := FField.KeyFields;
  StrPos := 1;
  while StrPos <= Length(KeyFields) do
  begin
    KeyFieldName := ExtractFieldName(KeyFields, StrPos);
    if SameText(aField.FieldName, KeyFieldName) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

{TFieldDataLink  Private Methods}
{
  If the field exists and can be modified, then
  we CanModify as long as this hasn't been set
  ReadOnly somewhere else. Do we need any extra tests here?
}
function TFieldDataLink.GetCanModify: Boolean;
begin
  if FieldCanModify then
    Result := not ReadOnly
  else
    Result := False;
end;

{
  Set the FieldName and then notify the changes though EditingChanged and Reset
  Ensure FField is nil if something goes wrong or FieldName is empty
}
procedure TFieldDataLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    UpdateField;
    if Active then
    begin
      EditingChanged;
      Reset;
    end;
  end;
end;

procedure TFieldDataLink.UpdateField;
begin
  if Active and (FFieldName <> '') then
    FField := DataSet.FieldByName(FFieldName)
  else
    FField := nil;
end;

{
  This function checks if FField is still associated with the dataset
  If not update the field
}
procedure TFieldDataLink.ValidateField;
begin
  if not (DataSet.FindField(FFieldName) = FField) then
    UpdateField;
end;

procedure TFieldDataLink.ResetEditingSource;
begin
  FEditingSource := false;
  FEditingSourceSet := false;
end;

{TFieldDataLink  Protected Methods}

{ Delphi Help ->
    Changes to the Active property trigger the ActiveChanged method.
    If an OnActiveChange event handler is assigned, ActiveChanged calls
    this event handler. If ActiveChanged is triggered by a transition into
    an active state, then before calling the event handler, ActiveChanged makes
    sure that the Field for this TFieldDataLink is still valid.
  <-- Delphi Help

   Update the field instance. When not Active field will be set to nil
   Call OnActiveChange
}
procedure TFieldDataLink.ActiveChanged;
begin
  if FFieldName <> '' then
  begin
    UpdateField;
    EditingChanged;
    Reset;
  end;
  if Assigned(FOnActiveChange) then
    FOnActiveChange(Self);
end;

{ Delphi Help ->
    Changing the field binding can change the validity of the CanModify
    property, since individual field components can disallow edits. If
    TFieldDataLink is in an editing state when the Field property is changed,
    EditingChanged checks the CanModify property. If CanModify is False, it
    changes back out of the editing state.

    Note: This differs significantly from the inherited EditingChanged method
    of TDataLink. The functionality of the inherited method is replaced in
    TFieldDataLink by the OnEditingChange event handler.
  <-- Delphi Help

  ok so another event... but this time we simply change modified state
  if Editing and not CanModify? or do we also change to match if
  if not Editing and CanModify? i.e If Editing <> CanModify??  Will assume
  the latter just in case. easy to change back if I am wrong.

  Also based on this we replace parent routine, so do we need to keep track
  of Editing state ourself? I hope this is right. Anyone know for sure?

  OK .. based on the Modified routine we need to turn off
  our IsModified routine when succesfull right? so for now just turn
  it off as per my example.
}
procedure TFieldDataLink.EditingChanged;
var
  RealEditState : Boolean;
begin
  RealEditState := (CanModify and Inherited Editing);

  if (FEditing <> RealEditState) then
  begin
    FEditing := RealEditState;
    if not FEditing then
    begin
      IsModified := False;
      ResetEditingSource;
    end;
    if Assigned(FOnEditingChange) then
      FOnEditingChange(Self);
  end;
end;

{ Delphi Help ->
    LayoutChanged is called after changes in the layout of one of the
    containers of the Control for this TFieldDataLink that might change the
    validity of its field binding. For example, if the Control is embedded
    within a TCustomDBGrid, and one of the columns is deleted, the Field
    property for the Control might become invalid.
  <-- Delphi Help

  Ensure FField is valid and notify
}
procedure TFieldDataLink.LayoutChanged;
begin
  ValidateField;
  if FField <> nil then
  begin
    EditingChanged;
    RecordChanged(nil);
  end;
end;

{ Delphi Help ->
    Applications can not call this protected method. It is triggered
    automatically when the contents of the current record change.
    RecordChanged calls the OnDataChange event handler if there is one.

    TDataLink.RecordChanged:
    The Field parameter indicates which field of the current record has changed in value.
    If Field is nil (Delphi) or NULL (C++), any number of fields within the current record may have changed.
  <-- Delphi Help

  Call Reset if AField = FField or aField = nil
}
procedure TFieldDataLink.RecordChanged(aField: TField);
begin
  if (aField = nil) or (aField = FField) or
   ((FField <> nil) and (FField.FieldKind = fkLookup) and IsKeyField(aField)) then
    Reset;
end;

{ Delphi Help ->
    UpdateData overrides the default UpdateData method to call the
    OnUpdateData event handler where the data-aware control can write any
    pending edits to the record in the dataset.
  <-- Delphi Help

  where..can write pending events. So I guess when we have already
  called Modified? Aka if not IsModified exit otherwise call event?
  works for me.
}
procedure TFieldDataLink.UpdateData;
begin
  if not IsModified then
    exit;
  try
    if Assigned(FOnUpdateData) then
      FOnUpdateData(Self);
  finally
    IsModified := False;
  end;
end;

{ Delphi Help ->
    Call FocusControl to give the Control associated with this TFieldDataLink
    object the input focus. FocusControl checks whether the Control can receive
    input focus, and if so, calls its SetFocus method to move focus to the
    Control.
  <-- Delphi Help

  Check if the field matches and if Control is TWinControl than call SetFocus
  Set the FieldRef to nil so no other control get focus
}

procedure TFieldDataLink.FocusControl(aField: TFieldRef);
var
  WinControl: TWinControl;
begin
  if Assigned(aField) and (aField^ = FField) and (FControl is TWinControl) then
  begin
    WinControl := TWinControl(FControl);
    if WinControl.CanFocus then
    begin
      aField^ := nil;
      WinControl.SetFocus;
    end;
  end;
end;

{TFieldDataLink  Public Methods}

constructor TFieldDataLink.Create;
begin
  inherited Create;
  VisualControl := True;
  //FField := nil;
  //FFieldname := '';
end;

{ Delphi Help ->
    Use Edit to try to ensure that the contents of the field can be modified.
    A return value of True indicates that the field was already in an editing
    state, or that the DataSource was successfully changed to allow editing.
    A return value of False indicates that the DataSource could not be changed
    to allow editing. For example, if the CanModify property is False, Edit
    fails, and returns False.
  <-- Delphi Help

  ok so the way I see it, since the inherited function calls EditingChanged,
  which we have already overriden to modify our own Editing state if its invalid,
  I should just be calling the inherited routine here, but only if CanModify,
  since there is no point otherwise. But since we _are_ keeping track of editing
  state ourselves we return our own state, not the inherited. If anyone know
  better please fix.
}
function TFieldDataLink.Edit: Boolean;
var
  editingSrc: Boolean;
begin
  editingSrc := (not FEditing) and (Dataset<>nil) and not(Dataset.State in dsEditModes);

  if (not FEditing) and CanModify then
    inherited Edit;

  Result := FEditing;

  if not FEditingSourceSet then
  begin
    // should be triggered one time only if editing succeeded
    FEditingSource := FEditing and editingSrc;
    FEditingSourceSet := true;
  end;
end;

{ Delphi Help ->
    Call Modified when the Control for this TFieldDataLink begins processing
    edits.
  <-- Delphi Help

  ok so. well _that's_ helpfull. for the moment going to keep track
  by adding an IsModified... based on the other functions thus far
  we need to know whether we are in state, so I am assuming it goes

  Call Modified ->
    IsModified:=True;//Waiting for modifications

  Call SomeFunction->
    If IsModified then begin
      (do something)
      IsModified := False;//All modifications complete
    end
    else
     (do something else? exit?);
}
procedure TFieldDataLink.Modified;
begin
  IsModified := True;
end;

{ Delphi Help ->
    The Control that owns a TFieldDataLink object calls its Reset method to
    process a UI action that cancels edits to the field. Reset calls the
    OnDataChange event handler without writing any pending changes to the
    record in the dataset.
  <-- Delphi Help

  Just call to the OnDataChange Event, and turn off IsModified
}
procedure TFieldDataLink.Reset;
begin
  if Assigned(FOnDataChange) then
    FOnDataChange(Self);

  IsModified := False;
  ResetEditingSource;
end;

CONST
  DBCBEVENT_CHANGE   = 1;   // CustomDBCombobox Detected change event
  DBCBEVENT_SELECT   = 2;   // CustomDBCombobox Detected select event
  DBCBEVENT_CLOSEUP  = 4;   // CustomDBCombobox Detected closeup event
  DBCBEVENT_WHEEL    = 8;   // CustomDBCombobox Detected mousewheel event

{$Include dblookup.inc}
{$Include dbedit.inc}
{$Include dbtext.inc}
{$Include customdblistbox.inc}
{$Include dblistbox.inc}
{$Include dblookuplistbox.inc}
{$Include dbradiogroup.inc}
{$Include dbcheckbox.inc}
{$Include customdbcombobox.inc}
{$Include dbcombobox.inc}
{$Include dblookupcombobox.inc}
{$Include dbmemo.inc}
{$Include dbgroupbox.inc}
{$Include dbimage.inc}
{$Include dbcalendar.inc}
{$Include dbcustomnavigator.inc}


initialization
  RegisterPropertyToSkip(TField,'Calculated','VCL compatibility property', '');

finalization
  FieldClasses.Free;

end.
