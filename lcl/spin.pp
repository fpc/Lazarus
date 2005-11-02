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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  Classes, Controls, SysUtils, LCLType, LMessages, ClipBrd, StdCtrls;

type
  { TCustomFloatSpinEdit }

  TCustomFloatSpinEdit = class(TWinControl)
  private
    fClimbRate: Single;
    fDecimals: Integer;
    fLastValueOnChange: single;
    FMaxValue: single;
    FMinValue: single;
    FModified: boolean;
    FOnChange: TNotifyEvent;
    FSelLength: integer;
    FSelStart: integer;
    fValue: Single;
    FValueEmpty: boolean;
    fValueNeedsUpdate: boolean;
    function ClimbRateIsStored: boolean;
    function GetModified: Boolean;
    function GetSelLength: integer;
    function GetSelStart: integer;
    function GetSelText: String;
    function MaxValueIsStored: boolean;
    function MinValueIsStored: boolean;
    procedure SetMaxValue(const AValue: single);
    procedure SetMinValue(const AValue: single);
    procedure SetModified(const AValue: Boolean);
    procedure SetSelLength(const AValue: integer);
    procedure SetSelStart(const AValue: integer);
    procedure SetSelText(const AValue: String);
    procedure SetValueEmpty(const AValue: boolean);
    Procedure UpdateControl;
    function ValueIsStored: boolean;
  protected
    procedure TextChanged; override;
    procedure SetDecimals(Num: Integer);
    function GetValue: Single;
    procedure SetValue(const Num: Single);
    procedure SetClimbRate(const Num: Single);
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;
    procedure Loaded; override;
    procedure Change; dynamic;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectAll;
    procedure ClearSelection; virtual;
    procedure CopyToClipboard; virtual;
    procedure CutToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
    property Modified: Boolean read GetModified write SetModified;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Text;
  public
    property DecimalPlaces: Integer read FDecimals write SetDecimals default 2;
    property ClimbRate: Single read FClimbRate write SetClimbRate default 1;
    property MinValue: single read FMinValue write SetMinValue default 0;
    property MaxValue: single read FMaxValue write SetMaxValue default 100;
    property TabStop default true;
    property Value: Single read GetValue write SetValue default 0;
    property ValueEmpty: boolean read FValueEmpty write SetValueEmpty default False;
  end;
  
  { TFloatSpinEdit }
  
  TFloatSpinEdit = class(TCustomFloatSpinEdit)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DecimalPlaces;
    property Enabled;
    property MaxValue;
    property MinValue;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
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
    property TabStop;
    property TabOrder;
    property Value;
    property Visible;
  end;
  
  
  { TCustomSpinEdit }
  
  TCustomSpinEdit = class(TCustomFloatSpinEdit)
  private
    function GetClimbRate: integer;
    function GetMaxValue: integer;
    function GetMinValue: integer;
    function GetValue: integer;
    procedure SetClimbRate(const AValue: integer);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
  public
    constructor Create(TheOwner: TComponent); override;
  public
    property Value: integer read GetValue write SetValue;
    property MinValue: integer read GetMinValue write SetMinValue;
    property MaxValue: integer read GetMaxValue write SetMaxValue;
    property ClimbRate: integer read GetClimbRate write SetClimbRate;
  end;
  
  
  { TSpinEdit }

  TSpinEdit = class(TCustomSpinEdit)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property ClimbRate;
    property Constraints;
    property Enabled;
    property MaxValue;
    property MinValue;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
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
    property Value;
    property Visible;
  end;
  

procedure Register;

implementation

uses
  WSSpin;

procedure Register;
begin
  RegisterComponents('Misc',[TSpinEdit,TFloatSpinEdit]);
end;

{$I spinedit.inc}

end.


