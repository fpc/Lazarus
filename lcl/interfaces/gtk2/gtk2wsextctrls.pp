{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSExtCtrls.pp                             * 
 *                             -----------------                             * 
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
unit Gtk2WSExtCtrls;

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

  { TGtk2WSCustomPage }

  TGtk2WSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TGtk2WSCustomNotebook }

  TGtk2WSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
  end;

  { TGtk2WSPage }

  TGtk2WSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TGtk2WSNotebook }

  TGtk2WSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TGtk2WSShape }

  TGtk2WSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TGtk2WSCustomSplitter }

  TGtk2WSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TGtk2WSSplitter }

  TGtk2WSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TGtk2WSPaintBox }

  TGtk2WSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomImage }

  TGtk2WSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TGtk2WSImage }

  TGtk2WSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TGtk2WSBevel }

  TGtk2WSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TGtk2WSCustomRadioGroup }

  TGtk2WSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TGtk2WSRadioGroup }

  TGtk2WSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TGtk2WSCustomCheckGroup }

  TGtk2WSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TGtk2WSCheckGroup }

  TGtk2WSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TGtk2WSBoundLabel }

  TGtk2WSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;

  { TGtk2WSCustomLabeledEdit }

  TGtk2WSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TGtk2WSLabeledEdit }

  TGtk2WSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TGtk2WSCustomPanel }

  TGtk2WSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TGtk2WSPanel }

  TGtk2WSPanel = class(TWSPanel)
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
//  RegisterWSComponent(TCustomPage, TGtk2WSCustomPage);
//  RegisterWSComponent(TCustomNotebook, TGtk2WSCustomNotebook);
//  RegisterWSComponent(TPage, TGtk2WSPage);
//  RegisterWSComponent(TNotebook, TGtk2WSNotebook);
//  RegisterWSComponent(TShape, TGtk2WSShape);
//  RegisterWSComponent(TCustomSplitter, TGtk2WSCustomSplitter);
//  RegisterWSComponent(TSplitter, TGtk2WSSplitter);
//  RegisterWSComponent(TPaintBox, TGtk2WSPaintBox);
//  RegisterWSComponent(TCustomImage, TGtk2WSCustomImage);
//  RegisterWSComponent(TImage, TGtk2WSImage);
//  RegisterWSComponent(TBevel, TGtk2WSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TGtk2WSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TGtk2WSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TGtk2WSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TGtk2WSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TGtk2WSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TGtk2WSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TGtk2WSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TGtk2WSCustomPanel);
//  RegisterWSComponent(TPanel, TGtk2WSPanel);
////////////////////////////////////////////////////
end.
