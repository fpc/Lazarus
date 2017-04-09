{
 /***************************************************************************
                                  SpinEx.pp
                                 -----------

  Provides a T(Float)SpinEdit like control that allows to have a NullValue and
  a text indicating the control does not have a valid Value whenever the
  control looses focus.

  Initial implementation 2016 by Bart Broersma

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ ----------------------------------------------------------------------------

  ++++++++++  Notes for developers  ++++++++++

  1. Why yet another (Float)SpinEdit control?
     (Which problems does it solve?)

  The standard T(Float)SpinEdit does not support a NullValue mechanism.
  Also, it's impelementation is widgetset dependant. While this provides a
  control that, on widgetsets that have a native implementation of such a
  control, has the look and feel as users of this widgetset are acustomed to,
  the downside is that it's behaviour may also depend on the widgetset.
  This is especially the case if the text inside the control becomes invalid
  (empty or otherwise not a number).
  In such a case, when situation querying the control for it's Value, the results
  are not cross-platform consistent.

  This difference in behaviour across widgetsets also prevents the implementation
  of a NullValue, especially the possibility to leave the control empty
  or display an informative text inside it in such a case.

  Note: unlike T(Float)SpinEdit GetValue is always derived from the actual
  text in the control.
  This is by design, and it should not be altered.


  2. Why not simply associate a TUpDown with a TEdit instead?

  This has several disadvantages:
  * It does not allow floating point values
  * It's range is limited to the range of SmallInt
  * It does not properly anchor and align

  So, whilst the new implementation of T(Float)SpinEditEx uses a TUpDown
  control, it does not use it's Associate property.
  The 2 controls (an edit and an updown) are embedded in a TCustomControl
  (like TEditButton is) in oreder to have proper align and anchor behaviour.

  ---------------------------------------------------------------------------- }

unit SpinEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, LCLProc, Controls, ClipBrd, ComCtrls, GroupedEdit;

type
  { TCustomFloatSpinEdit }

  TNullValueBehaviour = (
    //This applies when the Text in the control is not a number.
    //If the Text is a number then it will be bound by Min/MaxValue
    nvbShowTextHint,       // Value becomes NullValue, Text becomes empty, TextHint will show when focus is lost
    nvbLimitedNullValue,   // Value becomes GetLimitedValue(NullValue), Text becomes Value
    nvbMinValue,           // Value becomes MinValue, Text becomes Value  NOTE: Default, since this is how Delphi seems to work
    nvbMaxValue,           // Value becomes MaxValue, Text becomes Value
    nvbInitialValue        // Value becomes InitialValue (OnEnter), Text becomes Value
    );

  { TCustomFloatSpinEditEx }

  TCustomFloatSpinEditEx = class(TCustomAbstractGroupedEdit)
  private
    FArrowKeys: Boolean;
    FIncrement: Double;
    FDecimals: Integer;
    FMaxValue: Double;
    FMinValue: Double;
    FInitialValue: Double;
    FMinRepeatValue: Byte;
    FNullValue: Double;
    FNullValueBehaviour: TNullValueBehaviour;
    //FNullValueText: String;
    FValue: Double;
    FUpdatePending: Boolean;
    FSettingValue: Boolean;
    //FValueChanged: Boolean;
    FFS: TFormatSettings;
    function GetDecimalSeparator: Char;
    function GetEdit: TGEEdit;
    procedure SetMinRepeatValue(AValue: Byte);
    procedure SpinUpDown(Up: Boolean);
    function GetNullValue: Double;
    function GetUpDown: TUpDown;
    function GetValue: Double;
    function IsLimited: Boolean;
    function IsOutOfLimits(AValue: Double): Boolean;
    procedure SetDecimalSeparator(AValue: Char);
    procedure UpdateControl;
    procedure UpDownChangingEx(Sender: TObject; var {%H-}AllowChange: Boolean;
                               {%H-}NewValue: SmallInt; Direction: TUpDownDirection);
    procedure UpDownClick(Sender: TObject; {%H-}Button: TUDBtnType);
  protected
    function GetBuddyClassType: TControlClass; override;
    procedure DoEnter; override;
    function RealGetText: TCaption; override;
    procedure Reset; override;
    procedure EditChange; override;
    procedure EditKeyDown(var Key: word; Shift: TShiftState); override;
    procedure EditKeyPress(var Key: char); override;
    procedure SetDecimals(ADecimals: Integer); virtual;
    procedure SetValue(const AValue: Double); virtual;
    procedure SetNullValue(AValue: Double); virtual;
    procedure SetMaxValue(const AValue: Double); virtual;
    procedure SetMinValue(const AValue: Double); virtual;
    procedure SetIncrement(const AIncrement: Double); virtual;
    function TextIsNumber(const S: String; out D: Double): Boolean; virtual;
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;
    procedure Loaded; override;

    property ArrowKeys: Boolean read FArrowKeys write FArrowKeys default True;
    property DecimalSeparator: Char read GetDecimalSeparator write SetDecimalSeparator default '.';
    property Edit: TGEEdit read GetEdit;
    property UpDown: TUpDown read GetUpDown;
    property MinRepeatValue: Byte read FMinRepeatValue write SetMinRepeatValue default 100;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetLimitedValue(const AValue: Double): Double; virtual;
    function ValueToStr(const AValue: Double): String; virtual;
    function StrToValue(const S: String): Double; virtual;
    procedure EditEditingDone; override;
  public
    property DecimalPlaces: Integer read FDecimals write SetDecimals default 2;
    property Increment: Double read FIncrement write SetIncrement;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property NullValue: Double read GetNullValue write SetNullValue;
    property NullValueBehaviour: TNullValueBehaviour read FNullValueBehaviour write FNullValueBehaviour default nvbMinValue;
    //property NullValueText: String read FNullValueText write FNullValueText;
    property Value: Double read GetValue write SetValue;
  end;

  { TFloatSpinEdit }

  TFloatSpinEditEx = class(TCustomFloatSpinEditEx)
  public
    property AutoSelected;
  published
    //From TCustomEdit
    property AutoSelect;
    property AutoSizeHeightIsEditHeight;
    property AutoSize default True;
    property Action;
    property Align;
    property Alignment default taRightJustify;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle default bsNone;
    property CharCase;
    property Color;
    property Constraints;
    property Cursor;
    property DirectInput;
    property EchoMode;
    property Enabled;
    property FocusOnBuddyClick;
    property Font;
    property Hint;
    property Layout;
    property MaxLength;
    property NumbersOnly;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    //property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    //property Text;
    property TextHint;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnContextPopup;
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

    //From TCustomFloatSpinEdit
    property ArrowKeys;
    property DecimalSeparator;
    property DecimalPlaces;
    property Increment;
    property MaxValue;
    property MinValue;
    property MinRepeatValue;
    property NullValue;
    property NullValueBehaviour;
    //property NullValueText;
    property Spacing;
    property Value;
  end;


  { TCustomSpinEdit }

  TCustomSpinEditEx = class(TCustomFloatSpinEditEx)
  private
    function GetIncrement: integer;
    function GetMaxValue: integer;
    function GetMinValue: integer;
    function GetNullValue: integer;
    function GetValue: integer;
  protected
    procedure SetMaxValue(const AValue: integer); overload; virtual;
    procedure SetMinValue(const AValue: integer); overload; virtual;
    procedure SetIncrement(const AValue: integer); overload; virtual;
    procedure SetNullValue(AValue: integer); overload; virtual;
    procedure SetValue(const AValue: integer); overload; virtual;
    function TextIsNumber(const S: String; out D: Double): Boolean; override;
  public
    constructor Create(TheOwner: TComponent); override;
  public
    property Value: integer read GetValue write SetValue default 0;
    property MinValue: integer read GetMinValue write SetMinValue default 0;
    property MaxValue: integer read GetMaxValue write SetMaxValue default 100;
    property NullValue: integer read GetNullValue write SetNullValue;
    property NullValueBehaviour;
    //property NullValueText;
    property Increment: integer read GetIncrement write SetIncrement default 1;
  end;


  { TSpinEdit }

  TSpinEditEx = class(TCustomSpinEditEx)
  public
    property AutoSelected;
  published
    //From TCustomEdit
    property AutoSelect;
    property AutoSizeHeightIsEditHeight;
    property AutoSize default True;
    property Action;
    property Align;
    property Alignment default taRightJustify;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle default bsNone;
    property CharCase;
    property Color;
    property Constraints;
    property Cursor;
    property DirectInput;
    property EchoMode;
    property Enabled;
    property FocusOnBuddyClick;
    property Font;
    property Hint;
    property Layout;
    property MaxLength;
    property NumbersOnly default True;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    //property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    //property Text;
    property TextHint;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnContextPopup;
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

    //From TCustomFloatSpinEdit
    property ArrowKeys;
    property Increment;
    property MaxValue;
    property MinValue;
    property MinRepeatValue;
    property NullValue;
    property NullValueBehaviour;
    //property NullValueText;
    property Spacing;
    property Value;
  end;

function DbgS(ANvb: TNullValueBehaviour): String; overload;

implementation

{$I spinex.inc}

end.

