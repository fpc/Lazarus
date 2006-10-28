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
  Classes, Controls, SysUtils, LCLType, LMessages, ClipBrd, StdCtrls;

type
  { TCustomFloatSpinEdit }

  TCustomFloatSpinEdit = class(TWinControl)
  private
    FIncrement: single;
    FDecimals: integer;
    FMaxValue: single;
    FMinValue: single;
    FModified: boolean;
    FOnChange: TNotifyEvent;
    FSelLength: integer;
    FSelStart: integer;
    FValue: Single;
    FValueEmpty: boolean;
    FUpdatePending: boolean;
    FValueChanged: boolean;
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
    procedure SetIncrement(const NewIncrement: single);
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
    property Increment: Single read FIncrement write SetIncrement default 1;
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
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ClimbRate: integer write SetIncrement stored false; // TODO: remove, deprecated
    property Constraints;
    property Enabled;
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

