{
 /***************************************************************************
                               AbstractCompiler.pp
                             -------------------




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
unit AbstractCompiler;

{$mode objfpc}

interface

uses
  classes, Controls, forms,buttons,sysutils, Graphics,Extctrls;

type

  TAbstractCompiler = class
   public
     procedure Compile; abstract; virtual; //Starts the compiler
     property Flags: TCompilerFlags; abstract; virtual; //All avalable flags of the compiler
     property OnOutput: TCompilerOutputEvent; abstract; virtual; //Event procedure to get responces back from the compiler
     property FileName: TString; abstract; virtual; //Name of the file to compile
  end;


implementation

end.
