unit imguimain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Graphics, ExtCtrls, OpenGLContext, LazLogger,
  GL, GLExt, LCLType, fpImage, SysUtils;

type
  TGLColor = record
    alpha: GLushort;
    blue: GLushort;
    green: GLushort;
    red: GLushort;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    first: Boolean;
  public
    { public declarations }
  end;

  { UIStateType }

  UIStateType = object
    activeitem: integer;
    hotitem: integer;
    kbditem: integer;
    keychar: integer;
    keyentered: integer;
    keymod: TShiftState;
    lastwidget: integer;
    mousedown: integer;
    mousex: integer;
    mousey: integer;

    procedure Init;
    procedure SetMousePos(X, Y: integer);
  end;

var
  Form1: TForm1;
  uistate: UIStateType;

implementation

uses
  uglyfont;

var
  bgcolor: integer;
  sometext: string;
  genid: integer;

{ UIStateType }

procedure UIStateType.Init;
begin
  mousex := 0;
  mousey := 0;
  mousedown := 0;

  hotitem := 0;
  activeitem := 0;

  kbditem := 0;
  keyentered := 0;
  keymod := [];

  lastwidget := 0;
end;

procedure UIStateType.SetMousePos(X, Y: integer);
begin
  mousex := X;
  mousey := Y;
end;

{$R *.lfm}

function GLColor(color: GLuint): TGLColor;
begin
  Result.red := (color shr 16) and $0000ff;
  Result.green := (color shr 8) and $0000ff;
  Result.blue := (color shr 0) and $0000ff;
  //Result.alpha := (color shr 0) and $000000ff;
  Result.alpha := $ff;
end;

//Draw the string. Characters are fixed width, so this is also
//deadly simple.
procedure drawstring(str: string; x, y: double);
begin
  glTextOut(x, y + 14, 0, 14, 14, 1, 0, str);
end;

//Simplified interface to OpenGL's fillrect call
procedure drawrect(x, y, w, h: integer; AColor: TGLColor);
begin
  glColor3ub(AColor.red, AColor.green, AColor.blue);
  glRectf(x, y, x + w, y + h);
end;

//Check whether current mouse position is within a rectangle
function regionhit(x, y, w, h: integer): integer;
begin
  if (uistate.mousex < x) or
    (uistate.mousey < y) or
    (uistate.mousex >= x + w) or
    (uistate.mousey >= y + h) then
    Result := 0
  else
    Result := 1;
end;

//Simple button IMGUI widget
function button(id: integer; x: integer; y: integer): integer;
begin
  //Check whether the button should be hot
  if regionhit(x, y, 64, 48) = 1 then
  begin
    uistate.hotitem := id;
    if (uistate.activeitem = 0) and (uistate.mousedown = 1) then
      uistate.activeitem := id;
  end;

  //If no widget has keyboard focus, take it
  if uistate.kbditem = 0 then
    uistate.kbditem := id;

  //If we have keyboard focus, show it
  if uistate.kbditem = id then
    drawrect(x - 6, y - 6, 84, 68, GLColor($ff0000));

  drawrect(x + 8, y + 8, 64, 48, GLColor($000000));

  //Render button
  if uistate.hotitem = id then
  begin
    if uistate.activeitem = id then
      //Button is both 'hot' and 'active'
      drawrect(x + 2, y + 2, 64, 48, GLColor($ffffff))
    else
      //Button is merely 'hot'
      drawrect(x, y, 64, 48, GLColor($ffffff));
  end
  else
    //button is not hot, but it may be active
    drawrect(x, y, 64, 48, GLColor($aaaaaa));

  //If we have keyboard focus, we'll need to process the keys
  if uistate.kbditem = id then
  begin
    case uistate.keyentered of
      VK_TAB:
      begin
        //If tab is pressed, lose keyboard focus.
        //Next widget will grab the focus.
        uistate.kbditem := 0;

        //If shift was also pressed, we want to move focus
        //to the previous widget instead.
        if ssShift in uistate.keymod then
          uistate.kbditem := uistate.lastwidget;

        //Also clear the key so that next widget
        //won't process it
        uistate.keyentered := 0;
      end;
      VK_RETURN:
      begin
        //Had keyboard focus, received return,
        //so we'll act as if we were clicked.
        Result := 1;
        exit;
      end;
    end;
  end;
  uistate.lastwidget := id;

  //If button is hot and active, but mouse button is not
  //down, the user must have clicked the button.
  if (uistate.mousedown = 0) and
    (uistate.hotitem = id) and
    (uistate.activeitem = id) then
    Result := 1
  else
    //Otherwise, no clicky.
    Result := 0;
end;

//Simple scroll bar IMGUI widget
function slider(id: integer; x: integer; y: integer; max: integer; var Value: integer): integer;
var
  ypos: integer;
  mousepos: integer;
  v: integer;
begin
  //Calculate mouse cursor's relative y offset
  ypos := ((256 - 16) * Value) div max;

  //Check for hotness
  if regionhit(x + 8, y + 8, 16, 255) = 1 then
  begin
    uistate.hotitem := id;
    if (uistate.activeitem = 0) and (uistate.mousedown = 1) then
      uistate.activeitem := id;
  end;

  //If no widget has keyboard focus, take it
  if uistate.kbditem = 0 then
    uistate.kbditem := id;

  //If we have keyboard focus, show it
  if uistate.kbditem = id then
    drawrect(x - 4, y - 4, 40, 280, GLColor($ff0000));

  drawrect(x, y, 32, 256 + 16, GLColor($777777));

  //Render the scrollbar
  if (uistate.activeitem = id) or (uistate.hotitem = id) then
    drawrect(x + 8, y + 8 + ypos, 16, 16, GLColor($ffffff))
  else
    drawrect(x + 8, y + 8 + ypos, 16, 16, GLColor($aaaaaa));

  //If we have keyboard focus, we'll need to process the keys
  if uistate.kbditem = id then
  begin
    case uistate.keyentered of
      VK_TAB:
      begin
        //If tab is pressed, lose keyboard focus.
        //Next widget will grab the focus.
        uistate.kbditem := 0;

        //If shift was also pressed, we want to move focus
        //to the previous widget instead.
        if ssShift in uistate.keymod then
          uistate.kbditem := uistate.lastwidget;

        //Also clear the key so that next widget
        //won't process it
        uistate.keyentered := 0;
      end;
      VK_UP:
      begin
        //Slide slider up (if not at zero)
        if Value > 0 then
        begin
          Dec(Value);

          Result := 1;
          exit;
        end;
      end;
      VK_DOWN:
      begin
        //Slide slider down (if not at max)
        if Value < max then
        begin
          Inc(Value);

          Result := 1;
          exit;
        end;
      end;
    end;
  end;
  uistate.lastwidget := id;

  //Update widget value
  if uistate.activeitem = id then
  begin
    mousepos := uistate.mousey - (y + 8);

    if mousepos < 0 then
      mousepos := 0;

    if mousepos > 255 then
      mousepos := 255;

    v := (mousepos * max) div 255;

    if v <> Value then
    begin
      Value := v;

      Result := 1;
      exit;
    end;
  end;

  Result := 0;
end;

function GetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
end;

function textfield(id: integer; x: integer; y: integer; var buffer: string): integer;
var
  len: integer;
  changed: integer;
begin
  len := Length(buffer);
  changed := 0;

  //Check for hotness
  if regionhit(x - 4, y - 4, 30 * 14 + 8, 24 + 8) = 1 then
  begin
    uistate.hotitem := id;

    if (uistate.activeitem = 0) and (uistate.mousedown = 1) then
      uistate.activeitem := id;
  end;

  //If no widget has keyboard focus, take it
  if uistate.kbditem = 0 then
    uistate.kbditem := id;

  //If we have keyboard focus, show it
  if uistate.kbditem = id then
    drawrect(x - 6, y - 6, 30 * 14 + 12, 24 + 12, GLColor($ff0000));

  //Render the text field
  if (uistate.activeitem = id) or (uistate.hotitem = id) then
    drawrect(x - 4, y - 4, 30 * 14 + 8, 24 + 8, GLColor($aaaaaa))
  else
    drawrect(x - 4, y - 4, 30 * 14 + 8, 24 + 8, GLColor($777777));

  glColor3ub($00, $00, $00);
  drawstring(buffer, x, y);

  //Render cursor if we have keyboard focus
  if (uistate.kbditem = id) and (((GetTickCount shr 8) and 1) = 1) then
    drawstring('_', x + len * 14, y);

  //If we have keyboard focus, we'll need to process the keys
  if uistate.kbditem = id then
  begin
    case uistate.keyentered of
      VK_TAB:
      begin
        //If tab is pressed, lose keyboard focus.
        //Next widget will grab the focus.
        uistate.kbditem := 0;

        //If shift was also pressed, we want to move focus
        //to the previous widget instead.
        if ssShift in uistate.keymod then
          uistate.kbditem := uistate.lastwidget;

        uistate.keyentered := 0;
      end;
      VK_BACK:
      begin
        //Also clear the key so that next widget
        //won't process it
        if len > 0 then
        begin
          Delete(buffer, len, 1);
          Dec(len);
          changed := 1;
        end;
      end;
    end;

    if (uistate.keychar >= 32) and (uistate.keychar < 127) and (len < 30) then
    begin
      buffer := buffer + Chr(uistate.keyentered);
      Inc(len);
      changed := 1;
    end;
  end;

  //If button is hot and active, but mouse button is not
  //down, the user must have clicked the widget; give it
  //keyboard focus.
  if (uistate.mousedown = 0) and (uistate.hotitem = id) and (uistate.activeitem = id) then
    uistate.kbditem := id;

  uistate.lastwidget := id;

  Result := changed;
end;

procedure imgui_prepare;
begin
  uistate.hotitem := 0;
  genid := 0;
end;

procedure imgui_finish;
begin
  if uistate.mousedown = 0 then
    uistate.activeitem := 0
  else
    if uistate.activeitem = 0 then
      uistate.activeitem := -1;

  //If no widget grabbed tab, clear focus
  if uistate.keyentered = VK_TAB then
    uistate.kbditem := 0;

  //Clear the entered key
  uistate.keyentered := 0;
  uistate.keychar := 0;
end;

function GEN_ID: integer;
begin
  Inc(genid);
  Result := genid;
end;

//Rendering function
procedure render;
var
  slidervalue: integer;
begin
  imgui_prepare;

  button(GEN_ID, 50, 50);

  button(GEN_ID, 150, 50);

  if button(GEN_ID, 50, 150) = 1 then
    bgcolor := Round(Random * $ffffff);

  if button(GEN_ID, 150, 150) = 1 then
    halt(0);

  textfield(GEN_ID, 50, 250, sometext);

  slidervalue := bgcolor and $ff;
  if slider(GEN_ID, 500, 40, 255, slidervalue) = 1 then
    bgcolor := (bgcolor and $ffff00) or slidervalue;

  slidervalue := ((bgcolor shr 10) and $3f);
  if slider(GEN_ID, 550, 40, 63, slidervalue) = 1 then
    bgcolor := (bgcolor and $ff00ff) or (slidervalue shl 10);

  slidervalue := ((bgcolor shr 20) and $f);
  if slider(GEN_ID, 600, 40, 15, slidervalue) = 1 then
    bgcolor := (bgcolor and $00ffff) or (slidervalue shl 20);

  imgui_finish;
end;

{ TForm1 }

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
var
  samples, samplebuffers: integer;
begin
  if first then begin
    glGetIntegerv(GL_SAMPLE_BUFFERS,@samplebuffers);
    glGetIntegerv(GL_SAMPLES,@samples);
    DebugLn(['SampleBuffers: ',samplebuffers]);
    Debugln(['Samples: ',samples]);
    first:=false;
  end;
  //setup 2D projection
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, OpenGLControl1.Width, OpenGLControl1.Height, 0, 0, 1);
  glMatrixMode(GL_MODELVIEW);

  //clear screen
  drawrect(0, 0, 640, 480, GLColor(bgcolor));

  render;

  glColor3ub($ff, $00, $00);
  glTextOut(10, 20, 0, 10, 10, 1, 0, Format('%f FPS', [1000 / OpenGLControl1.FrameDiffTimeInMSecs]));

  OpenGLControl1.SwapBuffers;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  OpenGLControl1.Paint;
end;

procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  uistate.SetMousePos(X, Y);
end;

procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  uistate.mousedown := 1;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  uistate.keymod := Shift;
  uistate.keyentered := Key;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  //if escape is pressed, quit the application
  if Ord(Key) = VK_ESCAPE then
    halt(0);

  uistate.keyentered := Ord(Key);

  //if key is ASCII, accept it as character input
  if (uistate.keyentered and $FF80) = 0 then
    uistate.keychar := uistate.keyentered and $7f;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bgcolor := $77;
  sometext := 'Some text';
  first:=true;
end;

procedure TForm1.OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  uistate.mousedown := 0;
end;

initialization
  uistate.Init;

end.

