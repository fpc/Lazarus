{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSImgList.pp                              * 
 *                             ----------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2WSImgList;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  ImgList,
////////////////////////////////////////////////////
  WSImgList, WSLCLClasses;

type

  { TGtk2WSCustomImageListResolution }

  TGtk2WSCustomImageListResolution = class(TWSCustomImageListResolution)
  private
  protected
  public
  end;


implementation

end.
