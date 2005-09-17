{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Abstract:
    Defines interface to source editors.
}
unit SrcEditorIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;
  
type
  TSourceEditorInterface = class
  end;

  TSourceEditorWindowInterface = class(TForm)
  end;

implementation

end.

