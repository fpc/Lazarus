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
    {$IFDEF TestFloatRTTI}
    FMydouble1: double;
    FMydouble2: double;
    FMydouble3: double;
    FMyExtended1: Extended;
    FMyExtended2: Extended;
    FMyExtended3: Extended;
    FMySingle1: single;
    FMySingle2: single;
    FMySingle3: single;
    function GetMyExtended2: Extended;
    function GetMyExtendeds3(Index: integer): Extended;
    function GetMySingle2: single;
    function GetMySingles3(Index: integer): single;
    function GetMydouble2: double;
    function GetMydoubles3(Index: integer): double;
    procedure SetMyExtended2(const AValue: Extended);
    procedure SetMyExtendeds3(Index: integer; const AValue: Extended);
    procedure SetMySingle2(const AValue: single);
    procedure SetMySingles3(Index: integer; const AValue: single);
    procedure SetMydouble2(const AValue: double);
    procedure SetMydoubles3(Index: integer; const AValue: double);
    {$ENDIF}
    procedure SetMaxValue(const AValue: single);
    procedure SetMinValue(const AValue: single);
    Procedure UpdateControl;
  protected
    procedure SetDecimals(num : Integer);
    Function GetValue : Single;
    procedure SetValue(Num : Single);
    procedure SetClimbRate(num : Single);
    procedure InitializeWnd; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Decimal_Places: Integer read fDecimals write SetDecimals;
    property Enabled;
    property Climb_Rate : Single read fClimbRate write SetClimbRate;
    property MinValue: single read FMinValue write SetMinValue;
    property MaxValue: single read FMaxValue write SetMaxValue;
    property OnEnter;
    property OnExit;
    property PopupMenu;
    property ShowHint;
    property Value: Single read GetValue write SetValue;
    property Visible;
    {$IFDEF TestFloatRTTI}
    property MySingle1: single read FMySingle1 write FMySingle1;
    property MySingle2: single read GetMySingle2 write SetMySingle2;
    property MySingle3: single index 0 read GetMySingles3 write SetMySingles3;
    property MyDouble1: double read FMydouble1 write FMydouble1;
    property MyDouble2: double read GetMydouble2 write SetMydouble2;
    property MyDouble3: double index 0 read GetMydoubles3 write SetMydoubles3;
    property MyExtended1: Extended read FMyExtended1 write FMyExtended1;
    property MyExtended2: Extended read GetMyExtended2 write SetMyExtended2;
    property MyExtended3: Extended index 0 read GetMyExtendeds3 write SetMyExtendeds3;
    {$ENDIF}
  end;

Implementation

{$I spinedit.inc}

end.


