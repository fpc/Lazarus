{ $Id$}
{
 *****************************************************************************
 *                            win32wsextctrls.pp                             * 
 *                            ------------------                             * 
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
unit win32wsextctrls;

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
  wsextctrls, wslclclasses;

type

  { TWin32WSCustomPage }

  TWin32WSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TWin32WSCustomNotebook }

  TWin32WSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
  end;

  { TWin32WSPage }

  TWin32WSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TWin32WSNotebook }

  TWin32WSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TWin32WSShape }

  TWin32WSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TWin32WSCustomSplitter }

  TWin32WSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TWin32WSSplitter }

  TWin32WSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TWin32WSPaintBox }

  TWin32WSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TWin32WSCustomImage }

  TWin32WSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TWin32WSImage }

  TWin32WSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TWin32WSBevel }

  TWin32WSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TWin32WSCustomRadioGroup }

  TWin32WSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TWin32WSRadioGroup }

  TWin32WSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TWin32WSCustomCheckGroup }

  TWin32WSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TWin32WSCheckGroup }

  TWin32WSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TWin32WSBoundLabel }

  TWin32WSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;

  { TWin32WSCustomLabeledEdit }

  TWin32WSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TWin32WSLabeledEdit }

  TWin32WSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TWin32WSCustomPanel }

  TWin32WSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TWin32WSPanel }

  TWin32WSPanel = class(TWSPanel)
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
//  RegisterWSComponent(TCustomPage, TWin32WSCustomPage);
//  RegisterWSComponent(TCustomNotebook, TWin32WSCustomNotebook);
//  RegisterWSComponent(TPage, TWin32WSPage);
//  RegisterWSComponent(TNotebook, TWin32WSNotebook);
//  RegisterWSComponent(TShape, TWin32WSShape);
//  RegisterWSComponent(TCustomSplitter, TWin32WSCustomSplitter);
//  RegisterWSComponent(TSplitter, TWin32WSSplitter);
//  RegisterWSComponent(TPaintBox, TWin32WSPaintBox);
//  RegisterWSComponent(TCustomImage, TWin32WSCustomImage);
//  RegisterWSComponent(TImage, TWin32WSImage);
//  RegisterWSComponent(TBevel, TWin32WSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TWin32WSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TWin32WSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TWin32WSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TWin32WSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TWin32WSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TWin32WSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TWin32WSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TWin32WSCustomPanel);
//  RegisterWSComponent(TPanel, TWin32WSPanel);
////////////////////////////////////////////////////
end.
