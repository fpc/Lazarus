{   Component(s):
    tcyBevel

    Description:
    A bevel with multi-bevels

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * No contributors for now ...
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
    
{**********************************************************************
 Package pl_Cindy.pkg
 for CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit cyBevel;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages,
     cyTypes, cyClasses,  Graphics, ExtCtrls, classes, Messages, Controls;

type
  TcyCustomBevel = class(TGraphicControl)
  private
    FBevels: TcyBevels;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure CmMouseEnter(var Msg: TLMessage); message CM_MOUSEENTER; // 9999 for CodeTyphon
    procedure CmMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE; // 9999 for CodeTyphon
    procedure SetBevels(const Value: TcyBevels);
  protected
    procedure Paint; override;
    property Bevels: TcyBevels read FBevels write SetBevels;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property Height default 105;
    property Width default 105;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
  end;

  TcyBevel = class(TcyCustomBevel)
  private
  protected
  public
  published
    property Align;
    property Anchors;
//    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
//    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // Herited from TcyCustomBevel :
    property Bevels;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPaint;
  end;

implementation

constructor TcyCustomBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 105;
  Height := 105;
  FBevels := TcyBevels.Create(self, cyClasses.TcyBevel);

  // Determine at design time if
  // the form is loading or if we have just added the component at design time :
  if csDesigning in ComponentState
  then
    if Owner <> nil
    then
      if not (csLoading in Owner.ComponentState)  // we have just added the component at design time
      then FBevels.Add.Style := bcLowered;
end;

destructor TcyCustomBevel.Destroy;
begin
  FBevels.Free;
  FBevels := Nil;

  inherited Destroy;
end;

procedure TcyCustomBevel.Paint;
var Rect: TRect;
begin
  // inherited;
  Rect := ClientRect;
  Bevels.DrawBevels(Canvas, Rect, false);

  if csDesigning in ComponentState
  then
    if Bevels.Count = 0
    then
      with Canvas do
      begin
        Pen.Style := psDash;
        Brush.Style := bsClear;
        Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      end;

  if Assigned(FOnPaint)
  then FOnPaint(Self);
end;

procedure TcyCustomBevel.CmMouseEnter(var Msg: TLMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TcyCustomBevel.CmMouseLeave(var Msg: TLMessage);
begin
  inherited;
  if Assigned(FonMouseLeave) then FOnMouseLeave(Self);
end;

procedure TcyCustomBevel.SetBevels(const Value: TcyBevels);
begin
  FBevels := Value;
end;

end.
