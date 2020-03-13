{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Željan Rikalo
}

unit qlclopenglwidget;

// this unit can be used only with Qt5 widgetset.

interface

uses
  qt5, qtobjects, qtwidgets,
  Classes, SysUtils, Types, Controls, LCLType;

type
  QLCLOpenGLWidget_gl_Override = procedure of object cdecl;

function QLCLOpenGLWidget_Create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QWidgetH; cdecl; external Qt5PasLib name 'QLCLOpenGLWidget_Create';
procedure QLCLOpenGLWidget_Destroy(handle: QWidgetH); cdecl; external Qt5PasLib name 'QLCLOpenGLWidget_Destroy';
procedure QLCLOpenGLWidget_override_paintGL(handle: QLCLOpenGLWidgetH; hook: QLCLOpenGLWidget_gl_Override); cdecl; external Qt5PasLib name 'QLCLOpenGLWidget_override_paintGL';
procedure QLCLOpenGLWidget_InheritedPaintGL(handle: QLCLOpenGLWidgetH); cdecl; external Qt5PasLib name 'QLCLOpenGLWidget_InheritedPaintGL';

type

  { TQtOpenGLWidget }

  TQtOpenGLWidget = class(TQtWidget)
  protected
    function CreateWidget(const Params: TCreateParams):QWidgetH; override;
    procedure paintGL(); cdecl; virtual;
  public
    function GetContainerWidget: QWidgetH; override;
    procedure AttachEvents; override;
    procedure DetachEvents; override;
    procedure InitializeWidget; override;
    procedure SlotPaintBg(Sender: QObjectH; Event: QEventH); cdecl; override;
    procedure SlotPaint(Sender: QObjectH; Event: QEventH); cdecl; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
  end;

implementation

{ TQtOpenGLWidget }

function TQtOpenGLWidget.CreateWidget(const Params: TCreateParams): QWidgetH;
var
  Parent: QWidgetH;
begin
  if Params.WndParent <> 0 then
    Parent := TQtWidget(Params.WndParent).GetContainerWidget
  else
    Parent := nil;
  Widget := QLCLOpenGLWidget_Create(Parent);
  Result := Widget;
end;

procedure TQtOpenGLWidget.paintGL(); cdecl;
begin
  //writeln('TLCLOpenGLWidget.paintGL() ');
  QLCLOpenGLWidget_InheritedPaintGL(QLCLOpenGLWidgetH(Widget));
end;

function TQtOpenGLWidget.GetContainerWidget: QWidgetH;
begin
  Result := Widget;
end;

procedure TQtOpenGLWidget.AttachEvents;
begin
   QLCLOpenGLWidget_override_paintGL(QLCLOpenGLWidgetH(Widget),
    @paintGL);
  inherited AttachEvents;
end;

procedure TQtOpenGLWidget.DetachEvents;
begin
  inherited DetachEvents;
  QLCLOpenGLWidget_override_paintGL(QLCLOpenGLWidgetH(Widget),
      QLCLOpenGLWidget_gl_Override(NilMethod));
end;

procedure TQtOpenGLWidget.InitializeWidget;
begin
  inherited InitializeWidget;
end;

procedure TQtOpenGLWidget.SlotPaintBg(Sender: QObjectH; Event: QEventH); cdecl;
begin
  //
end;

procedure TQtOpenGLWidget.SlotPaint(Sender: QObjectH; Event: QEventH); cdecl;
begin
  //
end;

function TQtOpenGLWidget.EventFilter(Sender: QObjectH; Event: QEventH
  ): Boolean; cdecl;
begin
  Result := inherited EventFilter(Sender, Event);
end;

end.
