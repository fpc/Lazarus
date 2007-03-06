{ $Id$}
{
 *****************************************************************************
 *                           GtkWSPairSplitter.pp                            * 
 *                           --------------------                            * 
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
unit GtkWSPairSplitter;

{$mode objfpc}{$H+}

interface

uses
  PairSplitter, WSPairSplitter, WSLCLClasses;

type

  { TGtkWSPairSplitterSide }

  TGtkWSPairSplitterSide = class(TWSPairSplitterSide)
  private
  protected
  public
  end;

  { TGtkWSCustomPairSplitter }

  TGtkWSCustomPairSplitter = class(TWSCustomPairSplitter)
  private
  protected
  public
    class function AddSide(ASplitter: TCustomPairSplitter; ASide: TPairSplitterSide; Side: integer): Boolean; override;
    class function SetPosition(ASplitter: TCustomPairSplitter; var NewPosition: integer): Boolean; override;
  end;

  { TGtkWSPairSplitter }

  TGtkWSPairSplitter = class(TWSPairSplitter)
  private
  protected
  public
  end;


implementation
uses
  WSProc, gtk;

{ TGtkWSCustomPairSplitter }

class function TGtkWSCustomPairSplitter.AddSide(ASplitter: TCustomPairSplitter;
  ASide: TPairSplitterSide; Side: integer): Boolean;
begin
  Result:=false;
  
  if not (WSCheckHandleAllocated(ASplitter, 'AddSide - splitter') and
          WSCheckHandleAllocated(ASide, 'AddSide - side'))
  then Exit;

  if (Side<0) or (Side>1) then exit;
  
  if Side=0 then
    gtk_paned_add1(PGtkPaned(ASplitter.Handle),PGtkWidget(ASide.Handle))
  else
    gtk_paned_add2(PGtkPaned(ASPlitter.Handle),PGtkWidget(ASide.Handle));
    
  Result:=true;
end;

class function TGtkWSCustomPairSplitter.SetPosition(
  ASplitter: TCustomPairSplitter; var NewPosition: integer): Boolean;
begin
  Result:=false;
  if not WSCheckHandleAllocated(ASplitter, 'SetPosition')
  then Exit;
  if NewPosition>=0 then
    gtk_paned_set_position(PGtkPaned(ASplitter.Handle),NewPosition);
  NewPosition:=PGtkPaned(ASplitter.Handle)^.child1_size;
  Result:=true;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TPairSplitterSide, TGtkWSPairSplitterSide);
  RegisterWSComponent(TCustomPairSplitter, TGtkWSCustomPairSplitter);
//  RegisterWSComponent(TPairSplitter, TGtkWSPairSplitter);
////////////////////////////////////////////////////
end.
