{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_reg_DockedFormEditor;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, LazIDEIntf, ComCtrls, Controls, Forms, IDEImagesIntf,
  Buttons, ExtCtrls, Graphics, IDEWindowIntf, sparta_MainIDE,
  PropEdits, PropEditUtils, FormEditingIntf, ComponentEditors, EditBtn, TypInfo,
  LCLIntf, LCLType, sparta_FakeForm, sparta_FakeNonControl, sparta_FakeFrame;

procedure Register;

implementation

procedure Register;
begin
  FormEditingHook.StandardDesignerBaseClasses[DesignerBaseClassId_TForm] := TFakeForm;
  FormEditingHook.StandardDesignerBaseClasses[DesignerBaseClassId_TFrame] := THookFrame;

  FormEditingHook.NonFormProxyDesignerForm[NonControlProxyDesignerFormId] := TFakeNonControl;
  FormEditingHook.NonFormProxyDesignerForm[FrameProxyDesignerFormId] := TFakeFrame;

  Screen.AddHandlerFormAdded(TSpartaMainIDE.Screen_FormAdded);
  Screen.AddHandlerRemoveForm(TSpartaMainIDE.Screen_FormDel);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowCreate, TSpartaMainIDE.WindowCreate);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowDestroy, TSpartaMainIDE.WindowDestroy);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowShow, TSpartaMainIDE.WindowShow);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowHide, TSpartaMainIDE.WindowHide);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate, TSpartaMainIDE.EditorActivated);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorDestroy, TSpartaMainIDE.EditorDestroyed);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorCreate, TSpartaMainIDE.EditorCreate);

  LazarusIDE.AddHandlerOnShowDesignerFormOfSource(TSpartaMainIDE.OnShowDesignerForm);
  LazarusIDE.AddHandlerOnShowSourceOfActiveDesignerForm(TSpartaMainIDE.OnShowSrcEditor);

  GlobalDesignHook.AddHandlerShowMethod(TSpartaMainIDE.OnShowMethod);
  GlobalDesignHook.AddHandlerModified(TSpartaMainIDE.OnModifiedSender);
  GlobalDesignHook.AddHandlerPersistentAdded(TSpartaMainIDE.OnModifiedPersistentAdded);
  GlobalDesignHook.AddHandlerPersistentDeleted(TSpartaMainIDE.OnModified);
  GlobalDesignHook.AddHandlerRefreshPropertyValues(TSpartaMainIDE.OnDesignRefreshPropertyValues);
  GlobalDesignHook.AddHandlerDesignerMouseDown(TSpartaMainIDE.OnDesignMouseDown);

  IDETabMaster := TDTXTabMaster.Create;
  IDEComponentsMaster := TDTXComponentsMaster.Create;
end;

finalization
  Screen.RemoveHandlerFormAdded(TSpartaMainIDE.Screen_FormAdded);
  Screen.RemoveHandlerRemoveForm(TSpartaMainIDE.Screen_FormDel);

  IDETabMaster.Free;
  IDEComponentsMaster.Free;
end.

