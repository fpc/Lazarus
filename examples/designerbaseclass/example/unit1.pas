{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Important:
    Before you open the designer of this unit, you must install the package
    DesignBaseClassDemoPkg, because it registers the TMyComponentClass.
    Read the README.txt.
    
  Abstract:
    When you open the designer, you can see the property 'DemoProperty' in the
    Object Inspector. This property ws inherited from TMyComponentClass.
}
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  CustomComponentClass;

type

  { TMyComponent1 }

  TMyComponent1 = class(TMyComponentClass)
    OpenDialog1: TOpenDialog;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MyComponent1: TMyComponent1;

implementation

initialization
  {$I unit1.lrs}

end.

