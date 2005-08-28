{ $Id$
                  ------------------------------------------
                  carbonutils.pp  -  Common carbon utilities
                  ------------------------------------------
 
 @created(Wed Aug 26st WET 2005)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains common carbon untilities (usable by other projects)
 Procs needed for the Carbon <-> LCL interface go to CarbonProc
 
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

unit CarbonUtils;
{$mode objfpc}{$H+}

interface       

uses
  Carbon;

type
  TFourCC = packed array[0..3] of Char;

// in eventhandlers UInt32 and FourCCs are mixed, so we provide here some fixup wrappers
function MakeEventSpec(AClass: TFourCC; AKind: UInt32): EventTypeSpec; inline;
function MakeEventSpec(AClass, AKind: TFourCC): EventTypeSpec; inline;
function MakeEventSpec(AClass, AKind: UInt32): EventTypeSpec; inline;  

function MakeFourCC(AFourCC: TFourCC): FourCharCode; inline; 

implementation

function MakeEventSpec(AClass, AKind: TFourCC): EventTypeSpec; inline;
begin    
  Result.eventClass := FourCharCode(AClass);
  Result.eventKind := FourCharCode(AKind);
end;

function MakeEventSpec(AClass, AKind: UInt32): EventTypeSpec; inline;
begin    
  Result.eventClass := AClass;
  Result.eventKind := AKind;
end;

function MakeEventSpec(AClass: TFourCC; AKind: UInt32): EventTypeSpec; inline;
begin    
  Result.eventClass := FourCharCode(AClass);
  Result.eventKind := AKind;
end;

function MakeFourCC(AFourCC: TFourCC): FourCharCode; inline;
begin
  Result := FourCharCode(AFourCC);
end;

end.