{ $Id: FpGuiwsextctrls.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSExtCtrls.pp                              * 
 *                              ---------------                              * 
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
unit FpGuiWSExtCtrls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  ExtCtrls,
////////////////////////////////////////////////////
  WSExtCtrls, WSLCLClasses;

type

  { TFpGuiWSCustomPage }

  TFpGuiWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TFpGuiWSCustomNotebook }

  TFpGuiWSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
  end;

  { TFpGuiWSPage }

  TFpGuiWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TFpGuiWSNotebook }

  TFpGuiWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TFpGuiWSShape }

  TFpGuiWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TFpGuiWSCustomSplitter }

  TFpGuiWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TFpGuiWSSplitter }

  TFpGuiWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TFpGuiWSPaintBox }

  TFpGuiWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TFpGuiWSCustomImage }

  TFpGuiWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TFpGuiWSImage }

  TFpGuiWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TFpGuiWSBevel }

  TFpGuiWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TFpGuiWSCustomRadioGroup }

  TFpGuiWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TFpGuiWSRadioGroup }

  TFpGuiWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TFpGuiWSCustomCheckGroup }

  TFpGuiWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TFpGuiWSCheckGroup }

  TFpGuiWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TFpGuiWSBoundLabel }

  TFpGuiWSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;

  { TFpGuiWSCustomLabeledEdit }

  TFpGuiWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TFpGuiWSLabeledEdit }

  TFpGuiWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TFpGuiWSCustomPanel }

  TFpGuiWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TFpGuiWSPanel }

  TFpGuiWSPanel = class(TWSPanel)
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
//  RegisterWSComponent(TCustomPage, TFpGuiWSCustomPage);
//  RegisterWSComponent(TCustomNotebook, TFpGuiWSCustomNotebook);
//  RegisterWSComponent(TPage, TFpGuiWSPage);
//  RegisterWSComponent(TNotebook, TFpGuiWSNotebook);
//  RegisterWSComponent(TShape, TFpGuiWSShape);
//  RegisterWSComponent(TCustomSplitter, TFpGuiWSCustomSplitter);
//  RegisterWSComponent(TSplitter, TFpGuiWSSplitter);
//  RegisterWSComponent(TPaintBox, TFpGuiWSPaintBox);
//  RegisterWSComponent(TCustomImage, TFpGuiWSCustomImage);
//  RegisterWSComponent(TImage, TFpGuiWSImage);
//  RegisterWSComponent(TBevel, TFpGuiWSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TFpGuiWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TFpGuiWSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TFpGuiWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TFpGuiWSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TFpGuiWSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TFpGuiWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TFpGuiWSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TFpGuiWSCustomPanel);
//  RegisterWSComponent(TPanel, TFpGuiWSPanel);
////////////////////////////////////////////////////
end.
