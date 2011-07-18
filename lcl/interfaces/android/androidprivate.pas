{
                    -----------------------------------------
                    androidprivate.pas
                    -----------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit androidprivate;

{$mode objfpc}{$H+}

interface

// defines
//{$I carbondefines.inc}

uses
  // libs
  android_all,
  // wdgetset
  WSLCLClasses, LCLClasses,
  // LCL + RTL
  Types, Classes, SysUtils, Controls, LCLType, LCLProc, Graphics, Math, Contnrs,
  AVL_Tree, LMessages, LCLMessageGlue;

type

  { TAndroidWindow }
  
  TAndroidWindow = class
  public
    constructor Create(const AObject: TWinControl; const AParams: TCreateParams);
    destructor Destroy; override;
  end;

//function CheckHandle(const AWinControl: TWinControl; const AClass: TClass; const DbgText: String): Boolean;
//function CheckWidget(const Handle: HWND; const AMethodName: String; AParamName: String = ''): Boolean;
//function CheckWidget(const Handle: HWND; const AMethodName: String; AClass: TClass): Boolean;

implementation



{ TCarbonWidget }

constructor TAndroidWindow.Create(const AObject: TWinControl;
  const AParams: TCreateParams);
begin

end;

destructor TAndroidWindow.Destroy;
begin
  inherited Destroy;
end;

end.
