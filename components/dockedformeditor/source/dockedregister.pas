{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit DockedRegister;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  // LCL
  LazIDEIntf, ComCtrls, Controls, Forms, Buttons, ExtCtrls, Graphics,
  // IdeIntf
  SrcEditorIntf, IDEWindowIntf, PropEdits, ComponentEditors, IDEOptEditorIntf,
  IDEOptionsIntf,
  // DockedFormEditor
  DockedMainIDE, DockedOptionsIDE, DockedOptionsFrame;

var
  DockedOptionsFrameID: Integer = 1000;

procedure Register;

implementation

procedure Register;
begin
  Screen.AddHandlerFormAdded(TDockedMainIDE.Screen_FormAdded);
  Screen.AddHandlerRemoveForm(TDockedMainIDE.Screen_FormDel);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowCreate, TDockedMainIDE.WindowCreate);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowDestroy, TDockedMainIDE.WindowDestroy);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowShow, TDockedMainIDE.WindowShow);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowHide, TDockedMainIDE.WindowHide);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate, TDockedMainIDE.EditorActivated);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorDestroy, TDockedMainIDE.EditorDestroyed);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorCreate, TDockedMainIDE.EditorCreate);

  LazarusIDE.AddHandlerOnShowDesignerFormOfSource(TDockedMainIDE.OnShowDesignerForm);
  LazarusIDE.AddHandlerOnShowSourceOfActiveDesignerForm(TDockedMainIDE.OnShowSrcEditor);

  GlobalDesignHook.AddHandlerShowMethod(TDockedMainIDE.OnDesignShowMethod);
  GlobalDesignHook.AddHandlerModified(TDockedMainIDE.OnDesignModified);
  GlobalDesignHook.AddHandlerPersistentAdded(TDockedMainIDE.OnDesignPersistentAdded);
  GlobalDesignHook.AddHandlerPersistentDeleted(TDockedMainIDE.OnDesignPersistentDeleted);
  GlobalDesignHook.AddHandlerRefreshPropertyValues(TDockedMainIDE.OnDesignRefreshPropertyValues);
  GlobalDesignHook.AddHandlerDesignerMouseDown(TDockedMainIDE.OnDesignMouseDown);
  GlobalDesignHook.AddHandlerSetSelection(TDockedMainIDE.OnDesignSetSelection);

  DockedOptions := TDockedOptions.Create;
  DockedOptionsFrameID := RegisterIDEOptionsEditor(GroupEnvironment, TFrameDockedOptions, DockedOptionsFrameID)^.Index;
  DockedOptions.LoadSafe;

  IDETabMaster := TDockedTabMaster.Create;
end;

finalization
  Screen.RemoveHandlerFormAdded(TDockedMainIDE.Screen_FormAdded);
  Screen.RemoveHandlerRemoveForm(TDockedMainIDE.Screen_FormDel);

  FreeAndNil(IDETabMaster);
  FreeAndNil(DockedOptions);
end.

