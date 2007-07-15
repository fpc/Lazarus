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

uses
  // Bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtprivate, qtwidgets,
  // LCL
  SysUtils, Classes, StdCtrls, Controls, Graphics, CheckLst,  LCLType,
  // Widgetset
  WSCheckLst, WSLCLClasses;

type

  { TQtWSCheckListBox }

  TQtWSCustomCheckListBox = class(TWSCustomCheckListBox)
  private
  protected
  public
    class function  GetChecked(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): boolean; override;
    class procedure SetChecked(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AChecked: boolean); override;
  end;


implementation

class function  TQtWSCustomCheckListBox.GetChecked(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): boolean;
var
  QtListWidget: TQtListWidget;
  AListWidget: QListWidgetH;
  AItem: QListWidgetItemH;
begin
  QtListWidget := TQtListWidget(ACheckListBox.Handle);
  AListWidget := QListWidgetH(QtListWidget.Widget);
  AItem := QListWidget_item(AListWidget, AIndex);
  Result := QListWidgetItem_checkState(AItem) = QtChecked;
end;

class procedure TQtWSCustomCheckListBox.SetChecked(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AChecked: boolean);
var
  QtListWidget: TQtListWidget;
  AListWidget: QListWidgetH;
  AItem: QListWidgetItemH;
begin
  QtListWidget := TQtListWidget(ACheckListBox.Handle);
  AListWidget := QListWidgetH(QtListWidget.Widget);
  AItem := QListWidget_item(AListWidget, AIndex);
  
  if AChecked then
    QListWidgetItem_setCheckState(AItem, QtChecked)
  else QListWidgetItem_setCheckState(AItem, QtUnchecked);
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
