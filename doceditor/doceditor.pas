unit DocEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls,
  LazarusIDEStrConsts, IDEProcs, EnvironmentOpts, IDEOptionDefs;

type
  TDocEditorWnd = class(TForm)
    ButtonsPanel: TPANEL;
    ContextTreeview: TTREEVIEW;
    FilesNotebook: TNOTEBOOK;
    ButtonImagelist: TIMAGELIST;
    procedure DocEditorWndCREATE(Sender: TObject);
  private
  public
  end;

var
  DocEditorWnd: TDocEditorWnd;

implementation

{ TDocEditorWnd }

procedure TDocEditorWnd.DocEditorWndCREATE(Sender: TObject);
begin
  Name:=NonModalIDEWindowNames[nmiwDocEditor];
  Caption := lisDocumentationEditor;
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self,Name);

  ButtonImageList:=TImageList.Create(Self);
  with ButtonImageList do
  begin
    Name:='ButtonImageList';
    Width:=24
    Height:=24;
  end;
end;

initialization
  {$I doceditor.lrs}

end.

