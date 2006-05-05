{ $Id$}
{
 *****************************************************************************
 *                            GnomeWSExtCtrls.pp                             * 
 *                            ------------------                             * 
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
unit GnomeWSExtCtrls;

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

  { TGnomeWSCustomPage }

  TGnomeWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TGnomeWSCustomNotebook }

  TGnomeWSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
  end;

  { TGnomeWSPage }

  TGnomeWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TGnomeWSNotebook }

  TGnomeWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TGnomeWSShape }

  TGnomeWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TGnomeWSCustomSplitter }

  TGnomeWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TGnomeWSSplitter }

  TGnomeWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TGnomeWSPaintBox }

  TGnomeWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TGnomeWSCustomImage }

  TGnomeWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TGnomeWSImage }

  TGnomeWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TGnomeWSBevel }

  TGnomeWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TGnomeWSCustomRadioGroup }

  TGnomeWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TGnomeWSRadioGroup }

  TGnomeWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TGnomeWSCustomCheckGroup }

  TGnomeWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TGnomeWSCheckGroup }

  TGnomeWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TGnomeWSBoundLabel }

  TGnomeWSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;

  { TGnomeWSCustomLabeledEdit }

  TGnomeWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TGnomeWSLabeledEdit }

  TGnomeWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TGnomeWSCustomPanel }

  TGnomeWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TGnomeWSPanel }

  TGnomeWSPanel = class(TWSPanel)
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
//  RegisterWSComponent(TCustomPage, TGnomeWSCustomPage);
//  RegisterWSComponent(TCustomNotebook, TGnomeWSCustomNotebook);
//  RegisterWSComponent(TPage, TGnomeWSPage);
//  RegisterWSComponent(TNotebook, TGnomeWSNotebook);
//  RegisterWSComponent(TShape, TGnomeWSShape);
//  RegisterWSComponent(TCustomSplitter, TGnomeWSCustomSplitter);
//  RegisterWSComponent(TSplitter, TGnomeWSSplitter);
//  RegisterWSComponent(TPaintBox, TGnomeWSPaintBox);
//  RegisterWSComponent(TCustomImage, TGnomeWSCustomImage);
//  RegisterWSComponent(TImage, TGnomeWSImage);
//  RegisterWSComponent(TBevel, TGnomeWSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TGnomeWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TGnomeWSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TGnomeWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TGnomeWSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TGnomeWSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TGnomeWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TGnomeWSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TGnomeWSCustomPanel);
//  RegisterWSComponent(TPanel, TGnomeWSPanel);
////////////////////////////////////////////////////
end.