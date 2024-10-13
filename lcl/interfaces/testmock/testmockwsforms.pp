{ $Id$}
{
 *****************************************************************************
 *                              TestMockWSForms.pp                           *
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit TestMockWSForms;

{$mode objfpc}{$H+}
{$OPTIMIZATION NOREMOVEEMPTYPROCS}

interface

uses
  Forms, Controls, LCLType, Classes, Types,
  WSForms, WSProc, WSLCLClasses,
  InterfaceBase;

type

  { TTestMockWSScrollingWinControl }

  TTestMockWSScrollingWinControl = class(TWSScrollingWinControl)
  published
  end;

  { TTestMockWSScrollBox }

  TTestMockWSScrollBox = class(TWSScrollBox)
  published
  end;

  { TTestMockWSCustomFrame }

  TTestMockWSCustomFrame = class(TWSCustomFrame)
  published
  end;

  { TTestMockWSFrame }

  TTestMockWSFrame = class(TWSFrame)
  published
  end;

  { TTestMockWSCustomForm }

  TTestMockWSCustomForm = class(TWSCustomForm)
  published
  end;

  { TTestMockWSForm }

  TTestMockWSForm = class(TWSForm)
  published
  end;

  { TTestMockWSHintWindow }

  TTestMockWSHintWindow = class(TWSHintWindow)
  end;

  { TTestMockWSScreen }

  TTestMockWSScreen = class(TWSScreen)
  published
  end;

  { TTestMockWSApplicationProperties }

  TTestMockWSApplicationProperties = class(TWSApplicationProperties)
  published
  end;

implementation

end.
