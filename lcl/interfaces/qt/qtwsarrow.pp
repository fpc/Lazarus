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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

uses
  // Bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtwidgets,
  // LCL
  SysUtils, Controls, LCLType, Graphics, Arrow,
////////////////////////////////////////////////////
  WSArrow, WSLCLClasses;

type

  { TQtWSArrow }

  TQtWSArrow = class(TWSArrow)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
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
          const AParams: TCreateParams): HWND;
var
  QtArrow: TQtArrow;
begin
  {$ifdef VerboseQt}
    WriteLn('> TQtWSArrow.CreateHandle for ',dbgsname(AWinControl));
  {$endif}

  QtArrow := TQtArrow.Create(AWinControl, AParams);
  
  case TArrow(AWinControl).ShadowType of
    stNone:QFrame_setFrameShape(QFrameH(QtArrow.Widget), QFrameNoFrame);
    stIn:
    begin
      QFrame_setFrameShape(QFrameH(QtArrow.Widget), QFramePanel);
      QFrame_setFrameShadow(QFrameH(QtArrow.Widget), QFrameSunken);
    end;
    stOut:
    begin
      QFrame_setFrameShape(QFrameH(QtArrow.Widget), QFramePanel);
      QFrame_setFrameShadow(QFrameH(QtArrow.Widget), QFrameRaised);
    end;
    stEtchedIn:
    begin
      QFrame_setFrameShape(QFrameH(QtArrow.Widget), QFrameBox);
      QFrame_setFrameShadow(QFrameH(QtArrow.Widget), QFrameSunken);
    end;
    stEtchedOut:
    begin
      QFrame_setFrameShape(QFrameH(QtArrow.Widget), QFrameBox);
      QFrame_setFrameShadow(QFrameH(QtArrow.Widget), QFrameRaised);
    end;
  end;

  QtArrow.ArrowType := Ord(TArrow(AWinControl).ArrowType);
  
  QtArrow.AttachEvents;
  
  Result := THandle(QtArrow);

  {$ifdef VerboseQt}
    WriteLn('< TQtWSArrow.CreateHandle for ',dbgsname(AWinControl),' Result: ', dbgHex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TQtWSArrow.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSArrow.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtArrow(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
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
  if Assigned(QtArrow) then
  begin
  
    case AShadowType of
      stNone: QFrame_setFrameShape(QFrameH(QtArrow.Widget), QFrameNoFrame);
      stIn:
      begin
        QFrame_setFrameShape(QFrameH(QtArrow.Widget), QFramePanel);
        QFrame_setFrameShadow(QFrameH(QtArrow.Widget), QFrameSunken);
      end;
      stOut:
      begin
        QFrame_setFrameShape(QFrameH(QtArrow.Widget), QFramePanel);
        QFrame_setFrameShadow(QFrameH(QtArrow.Widget), QFrameRaised);
      end;
      stEtchedIn:
      begin
        QFrame_setFrameShape(QFrameH(QtArrow.Widget), QFrameBox);
        QFrame_setFrameShadow(QFrameH(QtArrow.Widget), QFrameSunken);
      end;
      stEtchedOut:
      begin
        QFrame_setFrameShape(QFrameH(QtArrow.Widget), QFrameBox);
        QFrame_setFrameShadow(QFrameH(QtArrow.Widget), QFrameRaised);
      end;
    end;

    QtArrow.ArrowType := Ord(AArrowType);
   
  end;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TArrow, TQtWSArrow);
////////////////////////////////////////////////////
end.
