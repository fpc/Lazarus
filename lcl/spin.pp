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
  { TCustomSpinEdit }

  TCustomSpinEdit = class(TWinControl)
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
    fValueNeedsUpdate: boolean;
    function Climb_RateIsStored: boolean;
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
    Procedure UpdateControl;
    function ValueIsStored: boolean;
  protected
    procedure CMTextChanged(Var Message: TLMessage); message CM_TextChanged;
    procedure SetDecimals(Num: Integer);
    Function GetValue: Single;
    procedure SetValue(Num: Single);
    procedure SetClimbRate(Num: Single);
    procedure InitializeWnd; override;
    procedure Loaded; override;
    procedure Change; dynamic;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyComponent; override;
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
  published
    property Decimal_Places: Integer read fDecimals write SetDecimals default 2;
    property Climb_Rate : Single read fClimbRate write SetClimbRate stored Climb_RateIsStored;
    property MinValue: single read FMinValue write SetMinValue stored MinValueIsStored;
    property MaxValue: single read FMaxValue write SetMaxValue stored MaxValueIsStored;
    property Value: Single read GetValue write SetValue stored ValueIsStored;
  end;
  
  
  { TSpinEdit }
  
  TSpinEdit = class(TCustomSpinEdit)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Climb_Rate;
    property Constraints;
    property Decimal_Places;
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

Implementation

uses
  WSSpin;

procedure Register;
begin
  RegisterComponents('Misc',[TSpinEdit]);
end;

{$I spinedit.inc}

end.


