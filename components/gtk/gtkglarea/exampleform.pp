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

}
unit ExampleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTKGlArea, Forms, LResources, Buttons, StdCtrls,
  gtkglarea_int, gtk, glib, gl;

type
  TExampleForm = class(TForm)
    GTKGLAreaControl1: TGTKGLAreaControl;
    ExitButton1: TButton;
    HintLabel1: TLabel;
    procedure ExitButton1Click(Sender: TObject);
    procedure GTKGLAreaControl1Paint(Sender: TObject);
    procedure GTKGLAreaControl1Resize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  private
    AreaInitialized: boolean;
  end;
  
var AnExampleForm: TExampleForm;

implementation

constructor TExampleForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-400) div 2,(Screen.Height-300) div 2,400,300);
    Caption:='LCL Example for the gtkglarea component';
    
    ExitButton1:=TButton.Create(Self);
    with ExitButton1 do begin
      Name:='ExitButton1';
      Parent:=Self;
      SetBounds(300,10,80,25);
      Caption:='Exit';
      OnClick:=@ExitButton1Click;
      Visible:=true;
    end;
    
    HintLabel1:=TLabel.Create(Self);
    with HintLabel1 do begin
      Name:='HintLabel1';
      Parent:=Self;
      SetBounds(10,10,280,50);
      Caption:='Demo';
      Visible:=true;
    end;
    
    AreaInitialized:=false;
    GTKGLAreaControl1:=TGTKGLAreaControl.Create(Self);
    with GTKGLAreaControl1 do begin
      Name:='GTKGLAreaControl1';
      Parent:=Self;
      SetBounds(10,90,380,200);
      OnPaint:=@GTKGLAreaControl1Paint;
      OnResize:=@GTKGLAreaControl1Resize;
      Visible:=true;
    end;
  end;
end;

procedure TExampleForm.ExitButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TExampleForm.GTKGLAreaControl1Paint(Sender: TObject);
begin
  if (gint(True) = gtk_gl_area_make_current(GTKGLAreaControl1.Widget)) then
  begin
    if not AreaInitialized then begin
      glViewport(0,0, PGtkWidget(GTKGLAreaControl1.Widget)^.allocation.width,
        PGtkWidget(GTKGLAreaControl1.Widget)^.allocation.height);
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      glFrustum( -20.0, 20.0, -20.0, 20.0, 1.0, 145.0 );
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();
      glTranslatef(0, 0, -5.5);
      AreaInitialized:=true;
    end;

    glClearColor(0,0,0,1);
    glClear(GL_COLOR_BUFFER_BIT);
    glColor3f(1,0,1);
    glBegin(GL_TRIANGLES);
    glVertex3f(10,10,-2);
    glColor3f(0,1,1);
    glVertex3f(10,90,-1);
    glColor3f(0,1,0);
    glVertex3f(90,90,-2);
    glEnd();

    // Swap backbuffer to front
    gtk_gl_area_swap_buffers(PGtkGLArea(GTKGLAreaControl1.Widget));
  end;
end;

procedure TExampleForm.GTKGLAreaControl1Resize(Sender: TObject);
begin
  if (gint(True) = gtk_gl_area_make_current(GTKGLAreaControl1.widget)) then
    glViewport(0, 0, PGtkWidget(GTKGLAreaControl1.Widget)^.allocation.width,
      PGtkWidget(GTKGLAreaControl1.Widget)^.allocation.height);
end;

end.
