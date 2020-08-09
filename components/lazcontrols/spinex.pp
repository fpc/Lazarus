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
  In such a case, when querying the control for it's Value, the results
  are not cross-platform consistent.
  This difference in behaviour across widgetsets also prevents the implementation
  of a NullValue, especially the possibility to leave the control empty
  or display an informative text inside it in such a case.

  SpinEditEx handles Int64 values, whereas TSpinEdit is limited to LongInt values,
  this is because TSpinEdit inherites from TCustomFloatSpinEdit and the internal
  FValue is stored as Double: this has not enough significant digits to handle
  the total range of Int64.

  FloatSpinEditEx can set DecimalSeparator independent of DefaultFormatSettings.DecimalSeparator.

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
  (like TEditButton is) in order to have proper align- and anchororing behaviour.

  ---------------------------------------------------------------------------- }

unit SpinEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // LCL
  LCLType, LCLProc, Controls, ClipBrd, ComCtrls, GroupedEdit;


{.$define debugspinex}

type
  { TSpinEditExBase }

  TNullValueBehaviour = (
    //This applies when the Text in the control is not a number.
    //If the Text is a number then it will be bound by Min/MaxValue
    nvbShowTextHint,       // Value becomes NullValue, Text becomes empty, TextHint will show when focus is lost
    nvbLimitedNullValue,   // Value becomes GetLimitedValue(NullValue), Text becomes Value
    nvbMinValue,           // Value becomes MinValue, Text becomes Value  NOTE: Default, since this is how Delphi seems to work
    nvbMaxValue,           // Value becomes MaxValue, Text becomes Value
    nvbInitialValue        // Value becomes InitialValue (OnEnter), Text becomes Value
    );



  { TSpinEditExBase }

  generic TSpinEditExBase<T> = class(TCustomAbstractGroupedEdit)
  private const
    DefIncrement = 1;
    DefMaxValue = 100;
    DefMinRepeatValue = 100;
  private
    FArrowKeys: Boolean;
    FIncrement: T;
    FMaxValue: T;
    FMinValue: T;
    FInitialValue: T;
    FMinRepeatValue: Byte;
    FNullValue: T;
    FNullValueBehaviour: TNullValueBehaviour;
    FValue: T;
    FUpdatePending: Boolean;
    FSettingValue: Boolean;
    function GetEdit: TGEEdit;
    procedure SetMinRepeatValue(AValue: Byte);
    procedure SpinUpDown(Up: Boolean);
    function GetNullValue: T;
    function GetUpDown: TUpDown;
    function GetValue: T;
    function IsLimited: Boolean;
    function IsOutOfLimits(AValue: T): Boolean;
    procedure UpdateControl;
    procedure UpDownChangingEx(Sender: TObject; var {%H-}AllowChange: Boolean;
                               {%H-}NewValue: SmallInt; Direction: TUpDownDirection);
    procedure UpDownClick(Sender: TObject; {%H-}Button: TUDBtnType);
    function IncrementStored: Boolean;
    function MaxValueStored: Boolean;
  protected
    function GetBuddyClassType: TControlClass; override;
    procedure DoEnter; override;
    function RealGetText: TCaption; override;
    procedure Reset; override;
    procedure EditChange; override;
    procedure EditKeyDown(var Key: word; Shift: TShiftState); override;
    procedure EditMouseWheelUp(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); override;
    procedure EditMouseWheelDown(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); override;
    function SafeInc(AValue: T): T; virtual; abstract;
    function SafeDec(AValue: T): T; virtual abstract;
    procedure SetValue(const AValue: T); virtual;
    procedure SetNullValue(AValue: T); virtual;
    procedure SetMaxValue(const AValue: T); virtual;
    procedure SetMinValue(const AValue: T); virtual;
    procedure SetIncrement(const AIncrement: T); virtual;
    function TextIsNumber(const S: String; out ANumber: T): Boolean; virtual; abstract;
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;
    procedure Loaded; override;

    property ArrowKeys: Boolean read FArrowKeys write FArrowKeys default True;
    property Edit: TGEEdit read GetEdit;
    property UpDown: TUpDown read GetUpDown;
    property UpDownVisible: Boolean read GetBuddyVisible write SetBuddyVisible default True;
    property MinRepeatValue: Byte read FMinRepeatValue write SetMinRepeatValue default DefMinRepeatValue;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetLimitedValue(const AValue: T): T; virtual;
    function ValueToStr(const AValue: T): String; virtual; abstract;
    function StrToValue(const S: String): T; virtual; abstract;
    procedure EditEditingDone; override;
  public
    property Increment: T read FIncrement write SetIncrement stored IncrementStored nodefault;
    property MinValue: T read FMinValue write SetMinValue;
    property MaxValue: T read FMaxValue write SetMaxValue stored MaxValueStored nodefault;
    property NullValue: T read GetNullValue write SetNullValue;
    property NullValueBehaviour: TNullValueBehaviour read FNullValueBehaviour write FNullValueBehaviour default nvbMinValue;
    property Value: T read GetValue write SetValue;
  end;

  { TCustomFloatSpinEditEx }

  TDisplayMode = (dmFixed, dmScientific, dmAuto);

  TCustomFloatSpinEditEx = class(specialize TSpinEditExBase<Double>)
  private const
    DefDecimals = 2;
    DefDecimalSeparator = '.';
  private
    FDecimals: Integer;
    FDisplayMode: TDisplayMode;
    FExponentDigits: Integer;
    FExponentialFormatLimitNeg: Integer;
    FExponentialFormatLimitPos: Integer;
    FFS: TFormatSettings;
    FPrecision: Integer;
    function GetDecimalSeparator: Char;
    procedure SetDecimalSeparator(AValue: Char);
    procedure SetDisplayMode(AValue: TDisplayMode);
    procedure SetExponentDigits(AValue: Integer);
    procedure SetExponentialFormatLimitNeg(AValue: Integer);
    procedure SetExponentialFormatLimitPos(AValue: Integer);
    procedure SetPrecision(AValue: Integer);
  protected
    procedure EditKeyPress(var Key: char); override;
    function TextIsNumber(const S: String; out ANumber: Double): Boolean; override;
    function SafeInc(AValue: Double): Double; override;
    function SafeDec(AValue: Double): Double; override;
    procedure SetDecimals(ADecimals: Integer); virtual;
  public
    function ValueToStr(const AValue: Double): String; override;
    function StrToValue(const S: String): Double; override;
    constructor Create(TheOwner: TComponent); override;
    property DecimalSeparator: Char read GetDecimalSeparator write SetDecimalSeparator default DefDecimalSeparator;
    property DecimalPlaces: Integer read FDecimals write SetDecimals default DefDecimals;
    property DisplayMode: TDisplayMode read FDisplayMode write SetDisplayMode default dmFixed;
    property ExponentialFormatLimitPos: Integer read FExponentialFormatLimitPos write SetExponentialFormatLimitPos default 6;  //used for scientific notation only
    property ExponentialFormatLimitNeg: Integer read FExponentialFormatLimitNeg write SetExponentialFormatLimitNeg default -6; //used for scientific notation only
    property Precision: Integer read FPrecision write SetPrecision default 6; //used for scientific notation only
    property ExponentDigits: Integer read FExponentDigits write SetExponentDigits default 2; //used for scientific notation only
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
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
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

    //From TCustomFloatSpinEditEx
    property ArrowKeys;
    property DecimalSeparator;
    property DecimalPlaces;
    property Increment;
    property MaxValue;
    property MinValue;
    property MinRepeatValue;
    property NullValue;
    property NullValueBehaviour;
    property Spacing;
    property UpDownVisible;
    property Value;
  end;


  { TCustomSpinEditEx }

  TCustomSpinEditEx = class(specialize TSpinEditExBase<Int64>)
  private
    FThousandSeparator: String;
    procedure SetThousandSeparator(AValue: String);
  protected
    procedure EditKeyPress(var Key: char); override;
    function SafeInc(AValue: Int64): Int64; override;
    function SafeDec(AValue: Int64): Int64; override;
    function TextIsNumber(const S: String; out ANumber: Int64): Boolean; override;
  public
    function ValueToStr(const AValue: Int64): String; override;
    function StrToValue(const S: String): Int64; override;
  public
    property Increment default 1;
    property ThousandSeparator: String read FThousandSeparator write SetThousandSeparator; //string so you can use Utf8
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
    property NumbersOnly;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
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

    //From TCustomFloatSpinEditEx
    property ArrowKeys;
    property Increment;
    property MaxValue;
    property MinValue;
    property MinRepeatValue;
    property NullValue;
    property NullValueBehaviour;
    property Spacing;
    property UpDownVisible;
    property Value;
  end;

function DbgS(ANvb: TNullValueBehaviour): String; overload;


implementation

{$I spinex.inc}

end.

