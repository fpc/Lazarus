{ $Id$}
{
 *****************************************************************************
 *                               WSExtCtrls.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit WSExtCtrls;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls, ExtCtrls,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSStdCtrls;

type
  { TWSCustomPage }

  TWSCustomPage = class(TWSWinControl)
  end;

  { TWSCustomNotebook }

  TWSCustomNotebook = class(TWSWinControl)
  public
    class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; virtual;
    class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; virtual;
    class procedure SetTabCaption(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AText: string); virtual;
  end;
  TWSCustomNotebookClass = class of TWSCustomNotebook;

  { TWSPage }

  TWSPage = class(TWSCustomPage)
  end;

  { TWSNotebook }

  TWSNotebook = class(TWSCustomNotebook)
  end;

  { TWSShape }

  TWSShape = class(TWSGraphicControl)
  end;

  { TWSCustomSplitter }

  TWSCustomSplitter = class(TWSCustomControl)
  end;

  { TWSSplitter }

  TWSSplitter = class(TWSCustomSplitter)
  end;

  { TWSPaintBox }

  TWSPaintBox = class(TWSGraphicControl)
  end;

  { TWSCustomImage }

  TWSCustomImage = class(TWSGraphicControl)
  end;

  { TWSImage }

  TWSImage = class(TWSCustomImage)
  end;

  { TWSBevel }

  TWSBevel = class(TWSGraphicControl)
  end;

  { TWSCustomRadioGroup }

  TWSCustomRadioGroup = class(TWSCustomGroupBox)
  end;

  { TWSRadioGroup }

  TWSRadioGroup = class(TWSCustomRadioGroup)
  end;

  { TWSCustomCheckGroup }

  TWSCustomCheckGroup = class(TWSCustomGroupBox)
  end;

  { TWSCheckGroup }

  TWSCheckGroup = class(TWSCustomCheckGroup)
  end;

  { TWSBoundLabel }

  TWSBoundLabel = class(TWSCustomLabel)
  end;

  { TWSCustomLabeledEdit }

  TWSCustomLabeledEdit = class(TWSCustomEdit)
  end;

  { TWSLabeledEdit }

  TWSLabeledEdit = class(TWSCustomLabeledEdit)
  end;

  { TWSCustomPanel }

  TWSCustomPanel = class(TWSCustomControl)
  end;

  { TWSPanel }

  TWSPanel = class(TWSCustomPanel)
  end;


implementation

{ TWSCustomNotebook }

{-------------------------------------------------------------------------------
  function TWSCustomNotebook.GetNotebookMinTabHeight(
    const AWinControl: TWinControl): integer;

  Returns the minimum height of the horizontal tabs of a notebook. That is the
  Notebook with TabPosition in [tpTop,tpBottom] without the client panel.
-------------------------------------------------------------------------------}
function TWSCustomNotebook.GetNotebookMinTabHeight(
  const AWinControl: TWinControl): integer;
begin
  Result:=30;
end;

{-------------------------------------------------------------------------------
  function TWSCustomNotebook.GetNotebookMinTabWidth(
    const AWinControl: TWinControl): integer;

  Returns the minimum width of the vertical tabs of a notebook. That is the
  Notebook with TabPosition in [tpLeft,tpRight] without the client panel.
-------------------------------------------------------------------------------}
function TWSCustomNotebook.GetNotebookMinTabWidth(const AWinControl: TWinControl
  ): integer;
begin
  Result:=60;
end;

procedure TWSCustomNotebook.SetTabCaption(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const AText: string);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomPage, TWSCustomPage);
  RegisterWSComponent(TCustomNotebook, TWSCustomNotebook);
//  RegisterWSComponent(TPage, TWSPage);
//  RegisterWSComponent(TNotebook, TWSNotebook);
//  RegisterWSComponent(TShape, TWSShape);
//  RegisterWSComponent(TCustomSplitter, TWSCustomSplitter);
//  RegisterWSComponent(TSplitter, TWSSplitter);
//  RegisterWSComponent(TPaintBox, TWSPaintBox);
//  RegisterWSComponent(TCustomImage, TWSCustomImage);
//  RegisterWSComponent(TImage, TWSImage);
//  RegisterWSComponent(TBevel, TWSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TWSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TWSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TWSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TWSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TWSCustomPanel);
//  RegisterWSComponent(TPanel, TWSPanel);
////////////////////////////////////////////////////
end.
