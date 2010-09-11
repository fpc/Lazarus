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
  SysUtils, Controls, LCLType, LCLProc, Graphics, Arrow,
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
    class procedure DrawArrow(const AArrow: TArrow; const ACanvas: TCanvas);
       override;
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

class procedure TQtWSArrow.DrawArrow(const AArrow: TArrow;
  const ACanvas: TCanvas);
const
  QtArrowTypeMap: array[TArrowType] of QStylePrimitiveElement =
  (
{atUp   } QStylePE_IndicatorArrowUp,
{atDown } QStylePE_IndicatorArrowDown,
{atLeft } QStylePE_IndicatorArrowLeft,
{atRight} QStylePE_IndicatorArrowRight
  );
var
  DC: TQtDeviceContext;
  ARect: TRect;
  StyleOption: QStyleOptionH;
begin
  DC := TQtDeviceContext(ACanvas.Handle);
  ARect := AArrow.ClientRect;

  StyleOption := QStyleOption_create(1, integer(QStyleOptionSO_Default));
  try
    // I do not know the reason, but under windows down arrow size is very small
    // and is not dependent on passed ARect.
    // There is nothing in qt source that can cause such bad painting.
    // Other styles draw down arrow very well.
    QStyleOption_initFrom(StyleOption, DC.Parent);
    QStyleOption_setRect(StyleOption, @ARect);
    QStyle_drawPrimitive(QApplication_style, QtArrowTypeMap[AArrow.ArrowType],
      StyleOption, DC.Widget, DC.Parent);
  finally
    QStyleOption_destroy(StyleOption);
  end;
end;

end.
