{ $Id$}
{
 *****************************************************************************
 *                               wsextctrls.pp                               * 
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
unit wsextctrls;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  extctrls,
////////////////////////////////////////////////////
  wslclclasses, wscontrols, wsstdctrls;

type

  { TWSCustomPage }

  TWSCustomPage = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSCustomNotebook }

  TWSCustomNotebook = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSPage }

  TWSPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TWSNotebook }

  TWSNotebook = class(TWSCustomNotebook)
  private
  protected
  public
  end;

  { TWSShape }

  TWSShape = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TWSCustomSplitter }

  TWSCustomSplitter = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWSSplitter }

  TWSSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TWSPaintBox }

  TWSPaintBox = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TWSCustomImage }

  TWSCustomImage = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TWSImage }

  TWSImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TWSBevel }

  TWSBevel = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TWSCustomRadioGroup }

  TWSCustomRadioGroup = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TWSRadioGroup }

  TWSRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TWSCustomCheckGroup }

  TWSCustomCheckGroup = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TWSCheckGroup }

  TWSCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TWSBoundLabel }

  TWSBoundLabel = class(TWSCustomLabel)
  private
  protected
  public
  end;

  { TWSCustomLabeledEdit }

  TWSCustomLabeledEdit = class(TWSCustomEdit)
  private
  protected
  public
  end;

  { TWSLabeledEdit }

  TWSLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TWSCustomPanel }

  TWSCustomPanel = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWSPanel }

  TWSPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;


implementation

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomPage, TWSCustomPage);
//  RegisterWSComponent(TCustomNotebook, TWSCustomNotebook);
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
