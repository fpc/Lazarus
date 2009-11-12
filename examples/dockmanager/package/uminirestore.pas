unit uMiniRestore;
(* Attempt to fix taskbar issues, related to application minimize/restore.

Symptoms
--------
  On Linux/KDE all forms are listed in the taskbar, while only the main form
  should be found there. This makes the restauration of an entire application
  almost impossible, because all forms must be restored manually! :-(

  The Application.OnMinimize/Restore handlers are never invoked, because the
  according methods in TApplication are never invoked.

Usage
-----
  In your MainForm.OnWindowStateChange call DoMiniRestore,
  or set the handler to DummyInstance.MiniRestore.

Approach
--------
  This workaround hides all forms when the main form is minimized, and
  restores them when the main form is restored. [Note: hide, not minimize!]
  (a single click on the taskbar entry of the application is sufficient :-)

  This feature is enabled on the first minimize of the main form, and eventually
  should be disabled when the main form is closed, to prevent side effects.

  Currently the auto-hidden windows are only hidden (Visbible=False).
  A combination with other properties could be used, to distinguish these from
  intentionally hidden windows.

  Screen.ActiveForm seems to reflect the focused form in OnWindowStateChange
  of the main form. This form is remembered and focused after a restore.
*)

{$mode objfpc}{$H+}

interface

uses
  Forms;

type
  TMiniRestore = class
    class procedure MiniRestore(Sender: TObject);
  end;

procedure DoMiniRestore;

var
  DummyInstance: TMiniRestore;//never instantiated
  EnableMinRestore: boolean;  //eventually clear this before application shutdown
  OldFocus: TCustomForm;      //the focused form on minimize

implementation

class procedure TMiniRestore.MiniRestore(Sender: TObject);
begin
  DoMiniRestore;
end;

procedure DoMiniRestore;
var
  i: integer;
  f, mf: TCustomForm;
  ws: TWindowState;
begin
(* Depending on the state of the main form, hide or restore all application forms.
*)
//check enabled and action
  mf := Application.MainForm;
  ws := mf.WindowState;
  case ws of
  wsMaximized: exit; //do nothing
  wsMinimized:
    begin
      EnableMinRestore := True;  //do minimize, allow for restore
      OldFocus := Screen.ActiveCustomForm;  //save focus window
    end
  else
    if not EnableMinRestore then
      exit; //disabled
  end;
//for all forms...
  for i := 0 to Screen.CustomFormCount - 1 do begin
    f := Screen.CustomForms[i];
    case ws of
    wsNormal:
      if (f <> mf) and (f.WindowState = wsNormal)
      and (f.ShowInTaskBar = stDefault)
      and not f.Visible then begin
        //f.WindowState := wsNormal;
        f.Show;
      end;
    wsMinimized:
      if (f <> mf) and (f.WindowState = wsNormal) and f.Visible then begin
        //f.WindowState := wsMinimized;
        //f.ShowInTaskBar := stDefault;
        f.Hide;
      end;
    end;
  end;
//set the focus
  if (ws = wsNormal) and assigned(OldFocus) then
    OldFocus.SetFocus;
end;

end.

