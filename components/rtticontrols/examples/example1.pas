{
 *****************************************************************************
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

  Abstract:
    Example for RTTI controls.
    Demonstrates a few RTTI controls without any code.
}
unit Example1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, RTTICtrls,
  RTTIGrids, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    TICheckBox1: TTICheckBox;
    TICheckBox2: TTICheckBox;
    TIComboBox_Enabled: TTIComboBox;
    TIEdit1: TTIEdit;
    TIListBox_Color: TTIListBox;
    TIPropertyGrid1: TTIPropertyGrid;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

initialization
  {$I example1.lrs}

end.

