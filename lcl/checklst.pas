{ $Id$
 /***************************************************************************
                               checklst.pp
                               -----------

                   Initial Revision  : Thu Jun 19 CST 2003

 ***************************************************************************/

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

unit CheckLst;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, LCLType, GraphType, Graphics, LMessages, Controls,
  StdCtrls;
  

type
  { TCustomCheckListBox }

  TCustomCheckListBox = class(TCustomListBox)
  private
    FItemDataOffset: Integer;
    function GetChecked(const AIndex: Integer): Boolean;
    procedure SetChecked(const AIndex: Integer; const AValue: Boolean);
    procedure SendItemChecked(const AIndex: Integer; const AChecked: Boolean);
  protected
    procedure AssignItemDataToCache(const AIndex: Integer; const AData: Pointer); override;
    procedure AssignCacheToItemData(const AIndex: Integer; const AData: Pointer); override;
    function  GetCachedDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Checked[const AIndex: Integer]: Boolean read GetChecked write SetChecked;
  end;
  
  
  { TCheckListBox }
  
  TCheckListBox = class(TCustomCheckListBox)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property Constraints;
    property ExtendedSelect;
    property Items;
    property ItemHeight;
    property MultiSelect;
    property OnClick;
    property OnDblClick;
    property OnDrawItem;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnResize;
    property ParentShowHint;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;


procedure Register;

implementation

uses
  WSCheckLst;

procedure Register;
begin
  RegisterComponents('Additional',[TCheckListBox]);
end;

type
  PCachedItemData = ^TCachedItemData;
  TCachedItemData = Boolean;

{ TCustomCheckListBox }

procedure TCustomCheckListBox.AssignCacheToItemData(const AIndex: Integer; const AData: Pointer);
begin
  inherited AssignCacheToItemData(AIndex, AData);
  if PCachedItemData(AData + FItemDataOffset)^
  then SendItemChecked(AIndex, True);
end;

procedure TCustomCheckListBox.AssignItemDataToCache(const AIndex: Integer; const AData: Pointer);
begin
  inherited AssignItemDataToCache(AIndex, AData);
  PCachedItemData(AData + FItemDataOffset)^ := Checked[AIndex];
end;

constructor TCustomCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCompStyle := csCheckListBox;
  FItemDataOffset := inherited GetCachedDataSize;
end;

function TCustomCheckListBox.GetCachedDataSize: Integer;
begin
  FItemDataOffset := inherited GetCachedDataSize;
  Result := FItemDataOffset + SizeOf(TCachedItemData);
end;

function TCustomCheckListBox.GetChecked(const AIndex: Integer): Boolean;
begin
  CheckIndex(AIndex);

  if HandleAllocated
  then Result := TWSCustomCheckListBoxClass(WidgetSetClass).GetChecked(Self, AIndex)
  else Result := PCachedItemData(GetCachedData(AIndex) + FItemDataOffset)^;
end;

procedure TCustomCheckListBox.SendItemChecked(const AIndex: Integer; const AChecked: Boolean);
begin
  if HandleAllocated then
    TWSCustomCheckListBoxClass(WidgetSetClass).SetChecked(Self, AIndex, AChecked);
end;

procedure TCustomCheckListBox.SetChecked(const AIndex: Integer; const AValue: Boolean);
begin
  CheckIndex(AIndex);

  if HandleAllocated
  then SendItemChecked(AIndex, AValue)
  else PCachedItemData(GetCachedData(AIndex) + FItemDataOffset)^ := AValue;
end;

end.

{ =============================================================================

  $Log$
  Revision 1.9  2004/12/27 19:40:59  mattias
  published BorderSpacing for many controls

  Revision 1.8  2004/09/10 20:19:13  micha
  convert LM_CLB_G/SETCHECKED to interface methods

  Revision 1.7  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.6  2004/07/13 10:34:15  mattias
  fixed lcl package unit file name checklist.pas

  Revision 1.5  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.3  2003/07/09 00:13:18  marc
  * fixed cached items.object storage if TCustomCheckListBox
  * Changed DebuggerOptions dialog to use new TCustomCheckListBox

  Revision 1.2  2003/07/07 23:58:43  marc
  + Implemented TCustomCheckListBox.Checked[] property


}
