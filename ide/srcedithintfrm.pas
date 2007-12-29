{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    The base class for hint windows for the source editor for the online help.
    For example for the fpdoc and comment help.
}
unit SrcEditHintFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, SysUtils, LCLProc, LCLType, LCLIntf, Forms, Controls, Graphics,
  SynEdit, SynEditKeyCmds,
  SrcEditorIntf;
  
type

  { TCodeHintProvider }

  TCodeHintProvider = class(TComponent)
  private
    FControl: TControl;
  protected
    procedure SetControl(const AValue: TControl); virtual;
  public
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer); virtual;
    procedure Paint(Canvas: TCanvas; const ARect: TRect); virtual; abstract;
    procedure UpdateHint; virtual;
    property Control: TControl read FControl write SetControl;
  end;

  { TCodeHelpFrm }

  TCodeHelpFrm = class(THintWindow)
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    FAnchorForm: TCustomForm;
    FHelpEnabled: boolean;
    FPreferredHeight: integer;
    FPreferredWidth: integer;
    FProvider: TCodeHintProvider;
    FSrcEditCaret: TPoint;
    procedure SetAnchorForm(const AValue: TCustomForm);
    procedure OnAnchorFormChangeBounds(Sender: TObject);
    procedure SetHelpEnabled(const AValue: boolean);
    procedure SetPreferredHeight(const AValue: integer);
    procedure SetPreferredWidth(const AValue: integer);
    procedure SetProvider(const AValue: TCodeHintProvider);
    procedure SetSrcEditCaret(const AValue: TPoint);
    procedure UpdatePosition;
  protected
    procedure Paint; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;// update content
    function NeedVisible: boolean;
    property AnchorForm: TCustomForm read FAnchorForm write SetAnchorForm;
    property HelpEnabled: boolean read FHelpEnabled write SetHelpEnabled;
    property SrcEditCaret: TPoint read FSrcEditCaret write SetSrcEditCaret;// 0,0 means use current position, should be ScreenXY, not TextXY
    property PreferredWidth: integer read FPreferredWidth write SetPreferredWidth;
    property PreferredHeight: integer read FPreferredHeight write SetPreferredHeight;
    property Provider: TCodeHintProvider read FProvider write SetProvider;
  end;
  
var
  CodeHelpFrm: TCodeHelpFrm = nil;

implementation

{ TCodeHelpFrm }

procedure TCodeHelpFrm.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  //DebugLn(['TCodeHelpFrm.ApplicationIdle NeedVisible=',NeedVisible]);
  if not NeedVisible then begin
    Hide;
    exit;
  end;
  UpdatePosition;
end;

procedure TCodeHelpFrm.FormCreate(Sender: TObject);
begin
  Application.AddOnIdleHandler(@ApplicationIdle);
end;

procedure TCodeHelpFrm.FormDestroy(Sender: TObject);
begin

end;

procedure TCodeHelpFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  SrcEdit: TSourceEditorInterface;
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
    Hide
  else if SourceEditorWindow<>nil then begin
    SrcEdit:=SourceEditorWindow.ActiveEditor;
    if SrcEdit=nil then
      Hide
    else begin
      // redirect keys
      SrcEdit.EditorControl.KeyDown(Key,Shift);
      SetActiveWindow(SourceEditorWindow.Handle);
    end;
  end;
end;

procedure TCodeHelpFrm.FormPaint(Sender: TObject);
begin
  if Provider<>nil then begin
    Provider.Paint(Canvas,Rect(0,0,ClientWidth,ClientHeight));
  end else begin
    with Canvas do begin
      Font.SetDefault;
      TextOut(6,20,'No help available (missing provider)');
    end;
  end;
end;

procedure TCodeHelpFrm.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char
  );
var
  SrcEdit: TSourceEditorInterface;
  ASynEdit: TCustomSynEdit;
begin
  SrcEdit:=SourceEditorWindow.ActiveEditor;
  if SrcEdit=nil then begin
    Hide;
  end else begin
    ASynEdit:=(SrcEdit.EditorControl as TCustomSynEdit);
    ASynEdit.CommandProcessor(ecChar,UTF8Key,nil);
  end;
end;

procedure TCodeHelpFrm.SetAnchorForm(const AValue: TCustomForm);
begin
  if FAnchorForm=AValue then exit;
  if FAnchorForm<>nil then
    FAnchorForm.RemoveAllHandlersOfObject(Self);
  FAnchorForm:=AValue;
  if FAnchorForm<>nil then
    FAnchorForm.AddHandlerOnChangeBounds(@OnAnchorFormChangeBounds);
  UpdatePosition;
end;

procedure TCodeHelpFrm.OnAnchorFormChangeBounds(Sender: TObject);
begin
  //DebugLn(['TCodeHelpFrm.OnAnchorFormChangeBounds ',dbgs(BoundsRect),' Sender=',dbgsName(Sender),' SenderVisible=',TControl(Sender).Visible,' SenderBounds=',dbgs(TControl(Sender).BoundsRect)]);
  UpdatePosition;
end;

procedure TCodeHelpFrm.SetHelpEnabled(const AValue: boolean);
begin
  if FHelpEnabled=AValue then exit;
  FHelpEnabled:=AValue;
  UpdatePosition;
end;

procedure TCodeHelpFrm.SetPreferredHeight(const AValue: integer);
begin
  if FPreferredHeight=AValue then exit;
  FPreferredHeight:=AValue;
end;

procedure TCodeHelpFrm.SetPreferredWidth(const AValue: integer);
begin
  if FPreferredWidth=AValue then exit;
  FPreferredWidth:=AValue;
end;

procedure TCodeHelpFrm.SetProvider(const AValue: TCodeHintProvider);
begin
  if FProvider=AValue then exit;
  if FProvider<>nil then begin
    FProvider.Control:=nil;
  end;
  FProvider:=AValue;
  if FProvider<>nil then begin
    FProvider.Control:=Self;
    FProvider.GetPreferredSize(FPreferredWidth,FPreferredHeight);
  end;
end;

procedure TCodeHelpFrm.SetSrcEditCaret(const AValue: TPoint);
begin
  if ComparePoints(FSrcEditCaret,AValue)=0 then exit;
  FSrcEditCaret:=AValue;
end;

procedure TCodeHelpFrm.UpdatePosition;
var
  NewBounds: TRect;
  DesktopBounds: TRect;

  procedure TryPosition(TryBounds: TRect; TheAnchors: TAnchors);
  begin
    TryBounds.Right:=Max(TryBounds.Left,TryBounds.Right);
    TryBounds.Bottom:=Max(TryBounds.Top,TryBounds.Bottom);
    if TryBounds.Right>DesktopBounds.Right then begin
      if not (akLeft in TheAnchors) then begin
        // move to the left
        dec(TryBounds.Left,TryBounds.Right-DesktopBounds.Right);
        TryBounds.Left:=Max(TryBounds.Left,DesktopBounds.Left);
      end;
      TryBounds.Right:=DesktopBounds.Right;
    end;
    if TryBounds.Left<DesktopBounds.Left then begin
      if not (akRight in TheAnchors) then begin
        // move to the right
        inc(TryBounds.Right,DesktopBounds.Left-TryBounds.Left);
        TryBounds.Left:=Min(TryBounds.Right,DesktopBounds.Right);
      end;
      TryBounds.Left:=DesktopBounds.Left;
    end;
    if TryBounds.Bottom>DesktopBounds.Bottom then begin
      if not (akTop in TheAnchors) then begin
        // move to the top
        dec(TryBounds.Top,TryBounds.Bottom-DesktopBounds.Bottom);
        TryBounds.Top:=Max(TryBounds.Top,DesktopBounds.Top);
      end;
      TryBounds.Bottom:=DesktopBounds.Bottom;
    end;
    if TryBounds.Top<DesktopBounds.Top then begin
      if not (akBottom in TheAnchors) then begin
        // move to the bottom
        inc(TryBounds.Bottom,DesktopBounds.Top-TryBounds.Top);
        TryBounds.Bottom:=Min(TryBounds.Bottom,DesktopBounds.Bottom);
      end;
      TryBounds.Top:=DesktopBounds.Top;
    end;
    // check if TryBounds are better than NewBounds
    if (TryBounds.Right-TryBounds.Left)*(TryBounds.Bottom-TryBounds.Top)
     > (NewBounds.Right-NewBounds.Left)*(NewBounds.Bottom-NewBounds.Top)
    then
      NewBounds:=TryBounds;
  end;

var
  CurCaret: TPoint;
  SrcEdit: TSourceEditorInterface;
  AnchorBounds: TRect;
begin
  if (not NeedVisible) or Visible then exit;
  DesktopBounds:=Rect(30,30,Screen.DesktopWidth-30,Screen.DesktopHeight-50);
  NewBounds:=Bounds(DesktopBounds.Left,DesktopBounds.Top,30,30);
  
  if AnchorForm<>nil then begin
    // place near the AnchorForm
    AnchorBounds:=AnchorForm.BoundsRect;
    // try right of AnchorForm
    TryPosition(Bounds(AnchorBounds.Right+6,AnchorBounds.Top,
                      PreferredWidth,PreferredHeight),[akLeft,akTop]);
    // try left of AnchorForm
    TryPosition(Bounds(AnchorBounds.Left-6-PreferredWidth,AnchorBounds.Top,
                      PreferredWidth,PreferredHeight),[akRight,akTop]);
    // try below
    TryPosition(Bounds(AnchorBounds.Left,AnchorBounds.Bottom+6,
                       PreferredWidth,PreferredHeight),[akTop]);
  end else begin
    // place near the source editor caret
    CurCaret:=SrcEditCaret;
    SrcEdit:=SourceEditorWindow.ActiveEditor;
    if CurCaret.Y<1 then
      CurCaret:=SrcEdit.CursorScreenXY;
    CurCaret:=SrcEdit.EditorControl.ClientToScreen(SrcEdit.ScreenToPixelPosition(CurCaret));

    // try below
    TryPosition(Bounds(CurCaret.X-(PreferredWidth div 2),CurCaret.Y+6,
                       PreferredWidth,PreferredHeight),[akTop]);
    // try above
    TryPosition(Bounds(CurCaret.X-(PreferredWidth div 2),
                       CurCaret.Y-6-PreferredHeight,
                       PreferredWidth,PreferredHeight),[akBottom]);
  end;
  
  //DebugLn(['TCodeHelpFrm.UpdatePosition NewBounds=',dbgs(NewBounds),' BoundsRect=',dbgs(BoundsRect)]);
  BoundsRect:=NewBounds;
  Visible:=true;
end;

procedure TCodeHelpFrm.Paint;
begin
  FormPaint(Self);
end;

constructor TCodeHelpFrm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnDestroy:=@FormDestroy;
  OnKeyDown:=@FormKeyDown;
  OnUTF8KeyPress:=@FormUTF8KeyPress;
  FPreferredWidth:=300;
  FPreferredHeight:=200;
  FormCreate(Self);
end;

destructor TCodeHelpFrm.Destroy;
begin
  inherited Destroy;
  if CodeHelpFrm=Self then
    CodeHelpFrm:=nil;
end;

procedure TCodeHelpFrm.UpdateHints;
begin
  if not Visible then exit;
  //DebugLn(['TCodeHelpFrm.UpdateHints ']);
  if Provider<>nil then Provider.UpdateHint;
end;

function TCodeHelpFrm.NeedVisible: boolean;
begin
  if not HelpEnabled then exit(false);
  if (AnchorForm<>nil) then begin
    Result:=AnchorForm.Visible;
  end else begin
    Result:=(SourceEditorWindow<>nil)
        and (SourceEditorWindow.ActiveEditor<>nil);
  end;
end;

{ TCodeHintProvider }

procedure TCodeHintProvider.SetControl(const AValue: TControl);
begin
  if FControl=AValue then exit;
  FControl:=AValue;
end;

procedure TCodeHintProvider.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer);
begin

end;

procedure TCodeHintProvider.UpdateHint;
begin

end;

end.

