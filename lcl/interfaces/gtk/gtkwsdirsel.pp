{ $Id$}
{
 *****************************************************************************
 *                              GtkWSDirSel.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit GtkWSDirSel;

{$mode objfpc}{$H+}

interface

uses
  DirSel, WSDirSel, WSLCLClasses;

type

  { TGtkWSDirSelDlg }

  TGtkWSDirSelDlg = class(TWSDirSelDlg)
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
//  RegisterWSComponent(TDirSelDlg, TGtkWSDirSelDlg);
////////////////////////////////////////////////////
end.