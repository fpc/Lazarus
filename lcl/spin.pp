{
 /***************************************************************************
                                  Spin.pp
                                  --------

                   Initial Revision  : Fri Apr 23 1999 10:29am
			Shane Miller
			mailing list:lazarus@miraclec.com

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit Spin;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, Controls, SysUtils, LCLType, LCLProc, StdCtrls, Math;

type
  { TCustomFloatSpinEdit }

  TCustomFloatSpinEdit = class(TCustomEdit)
  private const
    DefIncrement = 1;
    DefDecimals = 2;
    DefMaxValue = 100;
  private
    FIncrement: Double;
    FDecimals: Integer;
    FMaxValue: Double;
    FMinValue: Double;
    FValue: Double;
    FValueEmpty: Boolean;
    FUpdatePending: Boolean;
    FValueChanged: Boolean;
    function GetValue: Double;
    procedure UpdateControl;
    function MaxValueStored: Boolean;
    function IncrementStored: Boolean;
  protected
    class procedure WSRegisterClass; override;
    function  RealGetText: TCaption; override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure TextChanged; override;
    procedure SetDecimals(ADecimals: Integer); virtual;
    procedure SetValue(const AValue: Double); virtual;
    procedure SetMaxValue(const AValue: Double); virtual;
    procedure SetMinValue(const AValue: Double); virtual;
    procedure SetValueEmpty(const AValue: Boolean); virtual;
    procedure SetIncrement(const AIncrement: Double); virtual;
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;
    procedure Loaded; override;
    procedure KeyPress(var Key: char); override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetLimitedValue(const AValue: Double): Double; virtual;
    function ValueToStr(const AValue: Double): String; virtual;
    function StrToValue(const S: String): Double; virtual;
  public
    property DecimalPlaces: Integer read FDecimals write SetDecimals default DefDecimals;
    property Increment: Double read FIncrement write SetIncrement stored IncrementStored nodefault;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue stored MaxValueStored nodefault;
    property Value: Double read GetValue write SetValue;
    property ValueEmpty: Boolean read FValueEmpty write SetValueEmpty default False;
  end;
  
  { TFloatSpinEdit }
  
  TFloatSpinEdit = class(TCustomFloatSpinEdit)
  public
    property AutoSelected;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DecimalPlaces;
    property Enabled;
    property Font;
    property Increment;
    property MaxValue;
    property MinValue;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
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
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Value;
    property Visible;
  end;
  
  
  { TCustomSpinEdit }
  
  TCustomSpinEdit = class(TCustomFloatSpinEdit)
  private
    function GetIncrement: integer;
    function GetMaxValue: integer;
    function GetMinValue: integer;
    function GetValue: integer;
  protected
    procedure SetMaxValue(const AValue: integer); overload; virtual;
    procedure SetMinValue(const AValue: integer); overload; virtual;
    procedure SetIncrement(const AValue: integer); overload; virtual;
    procedure SetValue(const AValue: integer); overload; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
  public
    property Value: integer read GetValue write SetValue default 0;
    property MinValue: integer read GetMinValue write SetMinValue default 0;
    property MaxValue: integer read GetMaxValue write SetMaxValue default 100;
    property Increment: integer read GetIncrement write SetIncrement default 1;
  end;
  
  
  { TSpinEdit }

  TSpinEdit = class(TCustomSpinEdit)
  public
    property AutoSelected;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property Increment;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Value;
    property Visible;
  end;
  

procedure Register;

implementation

uses
  WSSpin;

procedure Register;
begin
  RegisterComponents('Misc', [TSpinEdit, TFloatSpinEdit]);
end;

{$I spinedit.inc}

end.

