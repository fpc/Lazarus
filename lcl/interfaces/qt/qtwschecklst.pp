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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
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
  published
    class function CreateHandle(const AWinControl: TWinControl;
     const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean; override;
    class function GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; override;
    class procedure SetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AEnabled: Boolean); override;
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

class function TQtWSCustomCheckListBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  QtListWidget: TQtCheckListBox;
  SelMode: QAbstractItemViewSelectionMode;
begin
  QtListWidget := TQtCheckListBox.Create(AWinControl, AParams);

  QtListWidget.Checkable := True;

  if TCheckListBox(AWinControl).MultiSelect then
    if TCheckListBox(AWinControl).ExtendedSelect then
      SelMode := QAbstractItemViewExtendedSelection
    else
      SelMode := QAbstractItemViewMultiSelection
  else
    SelMode := QAbstractItemViewSingleSelection;

  QtListWidget.setSelectionMode(SelMode);

  QtListWidget.AttachEvents;

  // create our FList helper
  QtListWidget.FList := TQtListStrings.Create(AWinControl, QtListWidget);

  QtListWidget.OwnerDrawn := TCheckListBox(AWinControl).Style in [lbOwnerDrawFixed, lbOwnerDrawVariable];

  Result := TLCLIntfHandle(QtListWidget);
end;

class function TQtWSCustomCheckListBox.GetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
var
  QtListWidget: TQtCheckListBox;
  AListWidget: QListWidgetH;
  AItem: QListWidgetItemH;
begin
  QtListWidget := TQtCheckListBox(ACheckListBox.Handle);
  AListWidget := QListWidgetH(QtListWidget.Widget);
  AItem := QListWidget_item(AListWidget, AIndex);
  Result := (QListWidgetItem_flags(AItem) and QtItemIsEnabled) <> 0;
end;

class function TQtWSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
var
  QtListWidget: TQtCheckListBox;
  AListWidget: QListWidgetH;
  AItem: QListWidgetItemH;
begin
  QtListWidget := TQtCheckListBox(ACheckListBox.Handle);
  AListWidget := QListWidgetH(QtListWidget.Widget);
  AItem := QListWidget_item(AListWidget, AIndex);
  Result := QtCheckStateToLCLCheckStateMap[QListWidgetItem_checkState(AItem)];
end;

class procedure TQtWSCustomCheckListBox.SetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AEnabled: Boolean);
var
  QtListWidget: TQtCheckListBox;
  AListWidget: QListWidgetH;
  AItem: QListWidgetItemH;
  Flags: QtItemFlags;
begin
  QtListWidget := TQtCheckListBox(ACheckListBox.Handle);
  AListWidget := QListWidgetH(QtListWidget.Widget);
  AItem := QListWidget_item(AListWidget, AIndex);
  Flags := QListWidgetItem_flags(AItem);
  if AEnabled then
    Flags := Flags or QtItemIsEnabled
  else
    Flags := Flags and not QtItemIsEnabled;
  QListWidgetItem_setFlags(AItem, Flags);
end;

class procedure TQtWSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
var
  QtListWidget: TQtCheckListBox;
  AListWidget: QListWidgetH;
  AItem: QListWidgetItemH;
begin
  QtListWidget := TQtCheckListBox(ACheckListBox.Handle);
  AListWidget := QListWidgetH(QtListWidget.Widget);
  AItem := QListWidget_item(AListWidget, AIndex);
  QtListWidget.BeginUpdate;
  QListWidgetItem_setCheckState(AItem, LCLCheckStateToQtCheckStateMap[AState]);
  QtListWidget.EndUpdate;
end;

end.
