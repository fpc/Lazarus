unit DebugOptionsFrm;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, Menus;

type
  TDebuggerOptionsForm = class (TForm )
    Button1: TBUTTON;
    Button2: TBUTTON;
    cmdCancel: TBUTTON;
    cmdOK: TBUTTON;
    Button5: TBUTTON;
    Button6: TBUTTON;
    Button7: TBUTTON;
    Button8: TBUTTON;
    Checkbox1: TCHECKBOX;
    Combobox1: TCOMBOBOX;
    cmbPath: TCOMBOBOX;
    Edit1: TEDIT;
    Groupbox1: TGROUPBOX;
    Groupbox2: TGROUPBOX;
    Groupbox3: TGROUPBOX;
    Groupbox4: TGROUPBOX;
    Groupbox5: TGROUPBOX;
    Groupbox6: TGROUPBOX;
    Groupbox7: TGROUPBOX;
    lvSignals: TLISTVIEW;
    mnuResumeUnhandled: TMENUITEM;
    mnuHandledByProgram: TMENUITEM;
    mnuiHandledByDebugger: TMENUITEM;
    mnuResumeHandled: TMENUITEM;
    nbDebugOptions: TNOTEBOOK;
    Page1: TPAGE;
    pgExceptions: TPAGE;
    pgEventLog: TPAGE;
    pgGeneral: TPAGE;
    popSignal: TPOPUPMENU;
    Scrollbox1: TSCROLLBOX;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DebuggerOptionsForm: TDebuggerOptionsForm;

implementation

initialization
  {$I debugoptionsfrm.lrs}

end.

