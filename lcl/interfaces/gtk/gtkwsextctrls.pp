{ $Id$}
{
 *****************************************************************************
 *                             GtkWSExtCtrls.pp                              * 
 *                             ----------------                              * 
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
unit GtkWSExtCtrls;

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

  { TGtkWSCustomPage }

  TGtkWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TGtkWSCustomNotebook }

  TGtkWSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
  end;

  { TGtkWSPage }

  TGtkWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TGtkWSNotebook }

  TGtkWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TGtkWSShape }

  TGtkWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TGtkWSCustomSplitter }

  TGtkWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TGtkWSSplitter }

  TGtkWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TGtkWSPaintBox }

  TGtkWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TGtkWSCustomImage }

  TGtkWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TGtkWSImage }

  TGtkWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TGtkWSBevel }

  TGtkWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TGtkWSCustomRadioGroup }

  TGtkWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TGtkWSRadioGroup }

  TGtkWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TGtkWSCustomCheckGroup }

  TGtkWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TGtkWSCheckGroup }

  TGtkWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TGtkWSBoundLabel }

  TGtkWSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;

  { TGtkWSCustomLabeledEdit }

  TGtkWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TGtkWSLabeledEdit }

  TGtkWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TGtkWSCustomPanel }

  TGtkWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TGtkWSPanel }

  TGtkWSPanel = class(TWSPanel)
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
//  RegisterWSComponent(TCustomPage, TGtkWSCustomPage);
//  RegisterWSComponent(TCustomNotebook, TGtkWSCustomNotebook);
//  RegisterWSComponent(TPage, TGtkWSPage);
//  RegisterWSComponent(TNotebook, TGtkWSNotebook);
//  RegisterWSComponent(TShape, TGtkWSShape);
//  RegisterWSComponent(TCustomSplitter, TGtkWSCustomSplitter);
//  RegisterWSComponent(TSplitter, TGtkWSSplitter);
//  RegisterWSComponent(TPaintBox, TGtkWSPaintBox);
//  RegisterWSComponent(TCustomImage, TGtkWSCustomImage);
//  RegisterWSComponent(TImage, TGtkWSImage);
//  RegisterWSComponent(TBevel, TGtkWSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TGtkWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TGtkWSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TGtkWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TGtkWSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TGtkWSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TGtkWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TGtkWSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TGtkWSCustomPanel);
//  RegisterWSComponent(TPanel, TGtkWSPanel);
////////////////////////////////////////////////////
end.
