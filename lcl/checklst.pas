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
  Classes, SysUtils, StdCtrls, Graphics, GraphType, Controls, VCLGlobals, LMessages;
  

type
  { TCheckListBox }

  TCheckListBox = class(TCustomListBox)
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
  published
    property Align;
    property Anchors;
    property BorderStyle;
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

procedure Register;
begin
  RegisterComponents('Additional',[TCheckListBox]);
end;

type
  PCachedItemData = ^TCachedItemData;
  TCachedItemData = Boolean;

{ TCheckListBox }

procedure TCheckListBox.AssignCacheToItemData(const AIndex: Integer; const AData: Pointer);
begin
  inherited AssignCacheToItemData(AIndex, AData);
  if PCachedItemData(AData + FItemDataOffset)^
  then SendItemChecked(AIndex, True);
end;

procedure TCheckListBox.AssignItemDataToCache(const AIndex: Integer; const AData: Pointer);
begin
  inherited AssignItemDataToCache(AIndex, AData);
  PCachedItemData(AData + FItemDataOffset)^ := Checked[AIndex];
end;

constructor TCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCompStyle := csCheckListBox;
  FItemDataOffset := inherited GetCachedDataSize;
end;

function TCheckListBox.GetCachedDataSize: Integer;
begin
  FItemDataOffset := inherited GetCachedDataSize;
  Result := FItemDataOffset + SizeOf(TCachedItemData);
end;

function TCheckListBox.GetChecked(const AIndex: Integer): Boolean;
begin
  CheckIndex(AIndex);

  if HandleAllocated
  then Result := (CNSendMessage(LM_CLB_GETCHECKED, Self, @AIndex) <> 0)
  else Result := PCachedItemData(GetCachedData(AIndex) + FItemDataOffset)^;
end;

procedure TCheckListBox.SendItemChecked(const AIndex: Integer; const AChecked: Boolean);
var
  Msg : TLMSetChecked;
begin
  if HandleAllocated
  then begin
    Msg.Index:= AIndex;
    Msg.Checked := AChecked;
    CNSendMessage(LM_CLB_SETCHECKED, Self, @Msg);
  end;
end;

procedure TCheckListBox.SetChecked(const AIndex: Integer; const AValue: Boolean);
begin
  CheckIndex(AIndex);

  if HandleAllocated
  then SendItemChecked(AIndex, AValue)
  else PCachedItemData(GetCachedData(AIndex) + FItemDataOffset)^ := AValue;
end;

end.

{ =============================================================================

  $Log$
  Revision 1.5  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.3  2003/07/09 00:13:18  marc
  * fixed cached items.object storage if TCheckListBox
  * Changed DebuggerOptions dialog to use new TCheckListBox

  Revision 1.2  2003/07/07 23:58:43  marc
  + Implemented TCheckListBox.Checked[] property


}
