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

{$mode objfpc}

interface

uses classes,controls,sysutils, stdCtrls,vclGlobals;


type
	TSpinEdit = class(TWinControl)
	private
	 fdecimals : Integer;
	 fValue    : Single;
	 fClimbRate : Single;
	protected
         procedure SetDecimals(num : Integer);
	 Function GetValue : Single;
	 procedure SetValue(Num : Single);
	 procedure SetClimbRate(num : Single);	 
	public

	 constructor Create(AOwner : TComponent); override;
	 destructor Destroy; override;
	 property Decimal_Places : Integer read fDecimals write SetDecimals;
	 property Value : Single read GetValue write SetValue;
	 property Climb_Rate : Single read fClimbRate write SetClimbRate;

	end;

Implementation

{$I spinedit.inc}



end.


