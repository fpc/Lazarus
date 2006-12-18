{
 wscommontrayicon.pas

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Authors: Felipe Monteiro de Carvalho and Andrew Haines

 Common code to all widgetsets.
}
unit WSCommonTrayIcon;

{$ifdef FPC}
  {$mode delphi}{$H+}
  {$PACKRECORDS C}
{$endif}

{*******************************************************************
*  Compatibility code for Delphi for Windows.
*******************************************************************}
{$ifndef FPC}
  {$define Windows}
{$endif}

interface

uses
  {$IFDEF Windows}
    Windows,
  {$ENDIF}
  Graphics, Classes, SysUtils, Controls, Menus;

type
  { TCustomWidgetTrayIcon }

  TCustomWidgetTrayIcon = class(TObject)
    private
    protected
      vVisible: Boolean;
    public
      uID: Cardinal;
      Icon: TIcon;
      ShowIcon, ShowHint: Boolean;
      PopUpMenu: TPopUpMenu;
      Hint: string;
      OnPaint, OnClick, OnDblClick: TNotifyEvent;
      OnMouseDown, OnMouseUp: TMouseEvent;
      OnMouseMove: TMouseMoveEvent;
      constructor Create;  virtual;
      destructor Destroy; override;
      procedure InternalUpdate; virtual; abstract;
      function Hide: Boolean; virtual; abstract;
      function Show: Boolean; virtual; abstract;
      function GetPosition: TPoint; virtual; abstract;
    published
  end;

implementation

{ TCustomWidgetTrayIcon }

{*******************************************************************
*  TCustomWidgetTrayIcon.Create ()
*
*  DESCRIPTION:    Creates a object from the TWidgetTrayIcon class
*
*  PARAMETERS:     None
*
*  RETURNS:        A pointer to the newly created object
*
*******************************************************************}
constructor TCustomWidgetTrayIcon.Create;
begin
  inherited Create;

  Icon := TIcon.Create;

  uID := 3;
end;

{*******************************************************************
*  TCustomWidgetTrayIcon.Destroy ()
*
*  DESCRIPTION:    Destroys a object derived from the TWidgetTrayIcon class
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
destructor TCustomWidgetTrayIcon.Destroy;
begin
  Hide;

  Icon.Free;

  inherited Destroy;
end;

end.


