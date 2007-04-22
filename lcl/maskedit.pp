{
 /***************************************************************************
                                 maskedit.pp
                                 -----------
                           Component Library Code
                           
        Does not yet support charsets that use multiple bytes per char

 ***************************************************************************/

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

{ ***************************************************************************** }
{ ***************************************************************************** }
{ ***************************************************************************** }
{ ***************************************************************************** }
{
  MWE: Code removed since almost all was copyrighted material.
}
{ ***************************************************************************** }
{ ***************************************************************************** }
{ ***************************************************************************** }
{ ***************************************************************************** }

unit MaskEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, LMessages, LCLType, Graphics;
  
type

  EDBEditError = class(Exception);

  TEditMask = type string;

  { TCustomMaskEdit }

  TCustomMaskEdit = class(TCustomEdit)
  private    
    FEditMask: TEditMask;
    procedure SetEditText(const AValue: string);
  protected
    function EditCanModify: Boolean; virtual;
    function GetEditText: string; virtual;
    procedure Reset; virtual;

    property EditMask: TEditMask read FEditMask write FEditMask;
  public
    procedure ValidateEdit; virtual;
    property EditText: string read GetEditText write SetEditText;
  end;
  
  
  { TMaskEdit }

  TMaskEdit = class(TCustomMaskEdit)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BorderSpacing;
    property Color;
    property Constraints;
    property CharCase;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property EditMask;
    property Enabled;
    property Font;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    Property OnKeyDown;
    property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Text;
    property Visible;
  end;
  
procedure Register;


implementation


procedure Register;
begin
//  RegisterComponents('Additional',[TMaskEdit]);
end;

function TCustomMaskEdit.EditCanModify: Boolean; 
begin
  Result := True;
end;

function TCustomMaskEdit.GetEditText: string; 
begin
  Result := Text;
end;

procedure TCustomMaskEdit.Reset;
begin
end;

procedure TCustomMaskEdit.SetEditText(const AValue: string);
begin
  Text := AValue;
end;

procedure TCustomMaskEdit.ValidateEdit;
begin
end;

end.
