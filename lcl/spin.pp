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
  Classes, Controls, SysUtils, StdCtrls, VCLGlobals, LMessages;


type
  TSpinEdit = class(TWinControl)
  private
    fDecimals : Integer;
    FMaxValue: single;
    FMinValue: single;
    fValue    : Single;
    fClimbRate : Single;
    fValueNeedsUpdate: boolean;
    function Climb_RateIsStored: boolean;
    function MaxValueIsStored: boolean;
    function MinValueIsStored: boolean;
    procedure SetMaxValue(const AValue: single);
    procedure SetMinValue(const AValue: single);
    Procedure UpdateControl;
    function ValueIsStored: boolean;
  protected
    procedure SetDecimals(num : Integer);
    Function GetValue : Single;
    procedure SetValue(Num : Single);
    procedure SetClimbRate(num : Single);
    procedure InitializeWnd; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Decimal_Places: Integer read fDecimals write SetDecimals default 2;
    property Enabled;
    property Climb_Rate : Single read fClimbRate write SetClimbRate stored Climb_RateIsStored;
    property MinValue: single read FMinValue write SetMinValue stored MinValueIsStored;
    property MaxValue: single read FMaxValue write SetMaxValue stored MaxValueIsStored;
    property OnEnter;
    property OnExit;
    property PopupMenu;
    property ShowHint;
    property Value: Single read GetValue write SetValue stored ValueIsStored;
    property Visible;
  end;

procedure Register;

Implementation

procedure Register;
begin
  RegisterComponents('Misc',[TSpinEdit]);
end;

{$I spinedit.inc}

end.


