
{*************************************************************}
{            callback routines                                }
{*************************************************************}

function Win32showCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;
begin
   EventTrace('show', data);
   MessI := LM_SHOW;
   TObject(data).Dispatch(MessI);
end;

function Win32activateCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('activate', data);
   MessI := LM_ACTIVATE;
   TObject(data).Dispatch(MessI);
end;

function Win32changedCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('changed', data);
   MessI := LM_CHANGED;
   TObject(data).Dispatch(MessI);
end;


function Win32draw( Win32Control: PWin32Control; area : TRect; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('DRAW', data);
   MessI := LM_DRAW;
   TObject(data).Dispatch(MessI);
end;


function Win32configureevent( Win32Control: PWin32Control; {!}{event : PgdkEventConfigure;} data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;
   DC : HDC;
   Win32Control2 : PWin32Control;
   PenColor : TColor;
begin
   EventTrace('<Configure Event>', data);
   MessI := LM_CONFIGUREEVENT;

        //Get the widget owner because the 'fixed' widget called the signal
        //create a pixmap for drawing behind the scenes

        //assign it to the object data for this widget so it's stored there
        //Clear the canvas area

   MessI := LM_DRAW;  //Should I be sending the Draw event??????????
   TObject(data).Dispatch(MessI);
   Assert(False, 'Trace:Exiting Configure');
end;

function Win32exposeevent( Win32Control: PWin32Control; {!}{event : PgdkEventExpose;} data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

   P1 : PWin32Control;
   UpdateRec : TRect;
   fWindow : Hwnd;
   Win32Control2: PWin32Control;
   DC : HDC;
begin
  EventTrace('!@Expose Event@!', data);
  MessI := LM_EXPOSEEVENT;
  Win32Control2 := Win32Control;
  fWindow := PWin32Control(Win32Control)^.window;
  TObject(data).Dispatch(MessI);
end;


function Win32keydown( Win32Control: PWin32Control; {!}{event : pgdkeventkey;} data: wPointer) : Boolean; cdecl;
var
   MessI : TLMKeyEvent;

begin
   EventTrace('key down', data);
   MessI.msg := LM_KEYDOWN;
{!}//   MessI.State := event^.state;
{!}//   MessI.Key := Event^.KeyVal;
{!}//   MessI.Length := Event^.Length;
//   MessI.Str := Event^.String;
   MessI.UserData := data;
   TObject(data).Dispatch(MessI);
end;

function Win32keyup( Win32Control: PWin32Control; {!}{event : pgdkEventKey;} data: wPointer) : Boolean; cdecl;
var
   MessI : TLMKeyEvent;

begin
   EventTrace('Key Up', data);
   MessI.msg := LM_KEYUP;
{!}//   MessI.State := event^.state;
{!}//   MessI.Key := Event^.KeyVal;
{!}//   MessI.Length := Event^.Length;
//   MessI.Str := Event^.String;
   MessI.UserData := data;

   TObject(data).Dispatch(MessI);
end;


function Win32focusCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('focus', data);
   MessI := LM_FOCUS;
   TObject(data).Dispatch(MessI);
end;

function Win32destroyCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('destroy', data);
   MessI := LM_DESTROY;
   TObject(data).Dispatch(MessI);
end;

function Win32resizeCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('resize', data);
   MessI := LM_RESIZE;
   TObject(data).Dispatch(MessI);
end;

function Win32MouseBtnPress( Win32Control: PWin32Control; {!}{event : pgdkEventButton;} data: wPointer) : Boolean; cdecl;
var
   MessI : TLMMouseEvent;
begin
   EventTrace('Mouse button Press', data);
   MessI.Msg := LM_MOUSEBTNPRESS;
   MessI.Button := -1;
   MessI.WheelDelta:= 0;
{!}//   MessI.State := Event^.State;
{!}//   MessI.X := Trunc(Event^.X);
{!}//   MessI.Y := trunc(Event^.Y);
   MessI.UserData := Data;
{!}//   case event^.Button of
{!}//    1 :
{!}//      MessI.Button := 0;
{!}//    2 :
{!}//      MessI.Button := 2;
{!}//    3 :
{!}//      MessI.Button := 1;
{!}//    4 :
{!}//      begin
{!}//        MessI.Msg := LM_MOUSEWHEEL;
{!}//        MessI.WheelDelta:=1;
{!}//      end;
{!}//    5 :
{!}//      begin
{!}//        MessI.Msg := LM_MOUSEWHEEL;
{!}//        MessI.WheelDelta:=-1;
{!}//      end;
{!}//   end;
   TObject(data).Dispatch(MessI);
end;

function Win32MouseBtnRelease( Win32Control: PWin32Control; {!}{event : pgdkEventButton;} data: wPointer) : Boolean; cdecl;
var
   MessI : TLMMouseEvent;

begin
   EventTrace('Mouse button release', data);
   MessI.Msg := LM_MOUSEBTNRELEASE;
   MessI.Button := -1;
   MessI.WheelDelta:= 0;
{!}//   MessI.State := Event^.State;
{!}//   MessI.X := Trunc(Event^.X);
{!}//   MessI.Y := trunc(Event^.Y);
   MessI.UserData := Data;
{!}//   case event^.Button of
{!}//    1 :
{!}//      MessI.Button := 0;
{!}//    2 :
{!}//      MessI.Button := 2;
{!}//    3 :
{!}//      MessI.Button := 1;
{!}//   end;
   TObject(data).Dispatch(MessI);
end;

function Win32clickedCB( OwnerObject: Pointer; AMessage, wParam, lParam: LongInt) : Boolean; cdecl;
var
   MessI : Integer;

begin
 Assert(False, 'Trace:Callback Function - Win32clickedCB');
 EventTrace('clicked', OwnerObject);
 MessI := LM_CLICKED;
 TObject(OwnerObject).Dispatch(MessI);
end;

function Win32DialogOKclickedCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
begin
end;

function Win32DialogCancelclickedCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   theDialog : TCustomDialog;

begin
   theDialog := TCustomDialog(data);
end;

function Win32DialogDestroyCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   theDialog : TCustomDialog;

begin
   theDialog := TCustomDialog(data);
// removed
   theDialog.UserChoice := -1;
end;

function Win32pressedCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
MessI : Integer;

begin
   EventTrace('pressed', data);
   MessI := LM_PRESSED;
   TObject(data).Dispatch(MessI);
end;

function Win32enterCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('enter', data);
   MessI := LM_ENTER;
   TObject(data).Dispatch(MessI);
end;

function Win32leaveCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('leave', data);
   MessI := LM_LEAVE;
   TObject(data).Dispatch(MessI);
end;

function Win32movecursorCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
MessI : Integer;

begin
   EventTrace('move-cursor', data);
   MessI := LM_MOVECURSOR;
   TObject(data).Dispatch(MessI);
end;

function Win32size_allocateCB( Win32Control: PWin32Control; {!}{size :pGtkAllocation;} data: wPointer) : Boolean; cdecl;
var
MessI : Integer;
msg : TLMResize;
begin
   EventTrace('size-allocate', data);
   MessI := LM_SIZEALLOCATE;
   msg.msg := MessI;
{!}//   Msg.Left := Size^.X;
{!}//   Msg.Top := Size^.Y;
{!}//   Msg.Width := Size^.width;
{!}//   Msg.Height := Size^.height;
   Msg.Userdata := Data;
   TObject(data).Dispatch(msg);
end;

function Win32releasedCB( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('released', data);
   MessI := LM_RELEASED;
   TObject(data).Dispatch(MessI);
end;

function Win32InsertText( Win32Control: PWin32Control; char : pChar; NewTextLength : Integer; {!}{Position : pgint;} data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;
   Msg : TLMInsertText;
   I : Integer;
begin
   EventTrace('Insert Text', data);
   MessI := LM_INSERTTEXT;

   Msg.Msg := MessI;
   Msg.NewText := '';
   For I := 1 to NewTextLength do
    Msg.NewText := Msg.Newtext+Char[i-1];
//   Msg.NewText := String(Char);
   Msg.Length := NewTextLength;
{!}//   Msg.Position := Position^;
   Msg.Userdata := data;
   TObject(data).Dispatch(Msg);
end;

function Win32DeleteText( Win32Control: PWin32Control; Startpos, EndPos : Integer; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Delete Text', data);
   MessI := LM_DELETETEXT;
   TObject(data).Dispatch(MessI);
end;

function Win32SetEditable( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Set Editable', data);
   MessI := LM_SETEDITABLE;
   TObject(data).Dispatch(MessI);
end;

function Win32MoveWord( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Move Word', data);
   MessI := LM_MOVEWORD;
   TObject(data).Dispatch(MessI);
end;

function Win32MovePAge( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Move Page', data);
   MessI := LM_MOVEPAGE;
   TObject(data).Dispatch(MessI);
end;

function Win32MoveToRow( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Move To Row!!', data);
   MessI := LM_MOVETOROW;
   TObject(data).Dispatch(MessI);
end;

function Win32MoveToColumn( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('MoveToColumn', data);
   MessI := LM_MOVETOCOLUMN;
   TObject(data).Dispatch(MessI);
end;

function Win32KillChar( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Kill Char', data);
   MessI := LM_KILLCHAR;
   TObject(data).Dispatch(MessI);
end;

function Win32KillWord( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Kill Word', data);
   MessI := LM_KILLWORD;
   TObject(data).Dispatch(MessI);
end;

function Win32KillLine( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Kill Line', data);
   MessI := LM_KILLLINE;
   TObject(data).Dispatch(MessI);
end;

function Win32CutToClip( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Cut To Clip', data);
   MessI := LM_CUTTOCLIP;
   TObject(data).Dispatch(MessI);
end;

function Win32CopyToClip( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Copy to Clip', data);
   MessI := LM_COPYTOCLIP;
   TObject(data).Dispatch(MessI);
end;

function Win32PasteFromClip( Win32Control: PWin32Control; data: wPointer) : Boolean; cdecl;
var
   MessI : Integer;

begin
   EventTrace('Paste From Clip', data);
   MessI := LM_PASTEFROMCLIP;
   TObject(data).Dispatch(MessI);
end;

function Win32MoveResize( Win32Control: PWin32Control; {!}{X,Y,Width,Height : pgInt;} data: wPointer) : Boolean; cdecl;
var
MessI : Integer;
msg : TLMResize;
begin
   EventTrace('size-allocate', data);
   MessI := LM_MOVERESIZE;
   msg.msg := MessI;
{!}//   Msg.Left := X^;
{!}//   Msg.Top := Y^;
{!}//   Msg.Width := Width^;
{!}//   Msg.Height := Height^;
   Msg.Userdata := Data;

   TObject(data).Dispatch(msg);
end;

{
  Directly call the Timer function of the TTimer object.
  (As far as I know this can't be dispatched like it's done in the other callbacks!)
}
function Win32TimerCB (data : wPointer) : Integer; cdecl;
var
   P : ^TTimer;
begin
  EventTrace('timer', data);
  P := @data;
  P^.Timer(TTimer(data));
  result := 1;                  { returning 0 would stop the timer, 1 will restart it }
end;

procedure EventTrace(message : string; data : pointer);
begin
   Assert(False, 'Trace:Event [' + message + '] fired for ' + TControl(data).Caption);
end;
 {
  $Log$
  Revision 1.1  2000/07/13 10:28:30  michael
  + Initial import

  Revision 1.1  2000/03/30 22:53:37  lazarus
  MWE:
    Moved form ../..

  Revision 1.2  1999/08/19 01:14:29  lazarus
  Changed the clicked callback as a test.  Someone help me out here... daworm@cdc.net

 }
