{
 /***************************************************************************
                                  Spin.pp
                                  --------

                   Initial Revision  : Fri Apr 23 1999 10:29am
			Shane Miller
			mailing list:lazarus@miraclec.com

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit Spin;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, LCLType, LCLProc, LMessages, ClipBrd, StdCtrls;

type
  { TCustomFloatSpinEdit }

  TCustomFloatSpinEdit = class(TCustomEdit)
  private
    FIncrement: Single;
    FDecimals: Integer;
    FMaxValue: Single;
    FMinValue: Single;
    FValue: Single;
    FValueEmpty: Boolean;
    FUpdatePending: Boolean;
    FValueChanged: Boolean;
    procedure SetMaxValue(const AValue: Single);
    procedure SetMinValue(const AValue: Single);
    procedure SetValueEmpty(const AValue: Boolean);
    procedure UpdateControl;
    function IsStored: Boolean; // FPC bug workaround
  protected
    function  RealGetText: TCaption; override;
    procedure TextChanged; override;
    procedure SetDecimals(ADecimals: Integer);
    function GetValue: Single;
    procedure SetValue(const AValue: Single);
    procedure SetIncrement(const AIncrement: Single);
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;
    procedure Loaded; override;
    class function GetControlClassDefaultSize: TPoint; override;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetLimitedValue(const AValue: Single): Single;
    function ValueToStr(const AValue: Single): String;
    function StrToValue(const S: String): Single;
  public
    property DecimalPlaces: Integer read FDecimals write SetDecimals default 2;
    property Increment: Single read FIncrement write SetIncrement stored IsStored default 1;
    property MinValue: Single read FMinValue write SetMinValue stored IsStored default 0;
    property MaxValue: Single read FMaxValue write SetMaxValue stored IsStored default 100;
    property Value: Single read GetValue write SetValue stored IsStored default 0;
    property ValueEmpty: Boolean read FValueEmpty write SetValueEmpty default False;
  end;
  
  { TFloatSpinEdit }
  
  TFloatSpinEdit = class(TCustomFloatSpinEdit)
  public
    property AutoSelected;
  published
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderSpacing;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
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
    procedure SetIncrement(const AValue: integer);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
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
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderSpacing;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
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

