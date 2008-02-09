{ $Id$}
{
 *****************************************************************************
 *                              QtWSCheckLst.pp                              * 
 *                              ---------------                              * 
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
unit QtWSCheckLst;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
  qtprivate, qtwidgets,
  // LCL
  SysUtils, Classes, StdCtrls, Controls, Graphics, CheckLst,  LCLType,
  // Widgetset
  WSCheckLst, WSLCLClasses;

type

  { TQtWSCheckListBox }

  { TQtWSCustomCheckListBox }

  TQtWSCustomCheckListBox = class(TWSCustomCheckListBox)
  private
  protected
  public
    class function  GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState); override;
  end;


implementation

const
  LCLCheckStateToQtCheckStateMap: array[TCheckBoxState] of QtCheckState =
  (
{cbUnchecked} QtUnchecked,
{cbChecked  } QtChecked,
{cbGrayed   } QtPartiallyChecked
  );
  
  QtCheckStateToLCLCheckStateMap: array[QtCheckState] of TCheckBoxState =
  (
{QtUnchecked       } cbUnchecked,
{QtPartiallyChecked} cbGrayed,
{QtChecked         } cbChecked
  );

class function TQtWSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
var
  QtListWidget: TQtListWidget;
  AListWidget: QListWidgetH;
  AItem: QListWidgetItemH;
begin
  QtListWidget := TQtListWidget(ACheckListBox.Handle);
  AListWidget := QListWidgetH(QtListWidget.Widget);
  AItem := QListWidget_item(AListWidget, AIndex);
  Result := QtCheckStateToLCLCheckStateMap[QListWidgetItem_checkState(AItem)];
end;

class procedure TQtWSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
var
  QtListWidget: TQtListWidget;
  AListWidget: QListWidgetH;
  AItem: QListWidgetItemH;
begin
  QtListWidget := TQtListWidget(ACheckListBox.Handle);
  AListWidget := QListWidgetH(QtListWidget.Widget);
  AItem := QListWidget_item(AListWidget, AIndex);
  QListWidgetItem_setCheckState(AItem, LCLCheckStateToQtCheckStateMap[AState]);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomCheckListBox, TQtWSCustomCheckListBox);
////////////////////////////////////////////////////
end.
