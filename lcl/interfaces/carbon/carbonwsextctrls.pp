{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSExtCtrls.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonWSExtCtrls;

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

  { TCarbonWSCustomPage }

  TCarbonWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TCarbonWSCustomNotebook }

  TCarbonWSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
  end;

  { TCarbonWSPage }

  TCarbonWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TCarbonWSNotebook }

  TCarbonWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TCarbonWSShape }

  TCarbonWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TCarbonWSCustomSplitter }

  TCarbonWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TCarbonWSSplitter }

  TCarbonWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TCarbonWSPaintBox }

  TCarbonWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomImage }

  TCarbonWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TCarbonWSImage }

  TCarbonWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TCarbonWSBevel }

  TCarbonWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TCarbonWSCustomRadioGroup }

  TCarbonWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TCarbonWSRadioGroup }

  TCarbonWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TCarbonWSCustomCheckGroup }

  TCarbonWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TCarbonWSCheckGroup }

  TCarbonWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TCarbonWSBoundLabel }

  TCarbonWSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;

  { TCarbonWSCustomLabeledEdit }

  TCarbonWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TCarbonWSLabeledEdit }

  TCarbonWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TCarbonWSCustomPanel }

  TCarbonWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TCarbonWSPanel }

  TCarbonWSPanel = class(TWSPanel)
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
//  RegisterWSComponent(TCustomPage, TCarbonWSCustomPage);
//  RegisterWSComponent(TCustomNotebook, TCarbonWSCustomNotebook);
//  RegisterWSComponent(TPage, TCarbonWSPage);
//  RegisterWSComponent(TNotebook, TCarbonWSNotebook);
//  RegisterWSComponent(TShape, TCarbonWSShape);
//  RegisterWSComponent(TCustomSplitter, TCarbonWSCustomSplitter);
//  RegisterWSComponent(TSplitter, TCarbonWSSplitter);
//  RegisterWSComponent(TPaintBox, TCarbonWSPaintBox);
//  RegisterWSComponent(TCustomImage, TCarbonWSCustomImage);
//  RegisterWSComponent(TImage, TCarbonWSImage);
//  RegisterWSComponent(TBevel, TCarbonWSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TCarbonWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TCarbonWSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TCarbonWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TCarbonWSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TCarbonWSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TCarbonWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TCarbonWSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TCarbonWSCustomPanel);
//  RegisterWSComponent(TPanel, TCarbonWSPanel);
////////////////////////////////////////////////////
end.