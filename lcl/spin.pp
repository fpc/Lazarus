 {
 /***************************************************************************
                          Spin.pp  -
                             -------------------

                   Initial Revision  : Fri Apr 23 1999 10:29am
			Shane Miller
			mailing list:lazarus@miraclec.com

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}

unit spin;

{$mode objfpc}{$H+}

interface

uses classes,controls,sysutils, stdCtrls,vclGlobals,LMessages;


type
  TSpinEdit = class(TWinControl)
  private
   fdecimals : Integer;
   fValue    : Single;
   fClimbRate : Single;
   Procedure UpdateControl;
	protected
   procedure SetDecimals(num : Integer);
   Function GetValue : Single;
   procedure SetValue(Num : Single);
   procedure SetClimbRate(num : Single);
  public
   constructor Create(AOwner : TComponent); override;
   destructor Destroy; override;
  published
   property Align;
   property Decimal_Places : Integer read fDecimals write SetDecimals;
   property Enabled;
   property Value : Single read GetValue write SetValue;
   property Climb_Rate : Single read fClimbRate write SetClimbRate;
   property Visible;
   property OnEnter;
   property OnExit;
   property PopupMenu;
   property ShowHint;
	end;

Implementation

{$I spinedit.inc}



end.


