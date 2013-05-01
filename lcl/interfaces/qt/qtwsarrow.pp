{ $Id$}
{
 *****************************************************************************
 *                               QtWSArrow.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit QtWSArrow;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  Types,
  // Bindings
  qt4,
  qtwidgets, qtobjects,
  // LCL
  SysUtils, Controls, LCLType, Graphics, Arrow,
////////////////////////////////////////////////////
  WSArrow, WSLCLClasses;

type

  { TQtWSArrow }

  TQtWSArrow = class(TWSArrow)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetType(const AArrow: TArrow; const AArrowType: TArrowType;
      const AShadowType: TShadowType); override;
  end;


implementation

{------------------------------------------------------------------------------
  Method: TQtWSArrow.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function  TQtWSArrow.CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle;
var
  QtArrow: TQtArrow;
begin
  {$ifdef VerboseQt}
    WriteLn('> TQtWSArrow.CreateHandle for ',dbgsname(AWinControl));
  {$endif}

  QtArrow := TQtArrow.Create(AWinControl, AParams);
  
  QtArrow.ArrowType := Ord(TArrow(AWinControl).ArrowType);
  
  QtArrow.AttachEvents;
  
  Result := TLCLIntfHandle(QtArrow);

  {$ifdef VerboseQt}
    WriteLn('< TQtWSArrow.CreateHandle for ',dbgsname(AWinControl),' Result: ', dbgHex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TQtWSArrow.SetType
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSArrow.SetType(const AArrow: TArrow; const AArrowType: TArrowType;
      const AShadowType: TShadowType);
var
  QtArrow: TQtArrow;
begin
  QtArrow := TQtArrow(AArrow.Handle);
  QtArrow.ArrowType := Ord(AArrowType);
end;

end.
