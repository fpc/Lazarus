{
 *****************************************************************************
  This file is part of a Lazarus Package, IconLib.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Extended graphic property editor form from which icons can be searched
 by keywords.
}

unit GraphicPropEditEx;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GraphicPropEdit, IconFinderStrConstsIDE, IconFinderFrm;

type

  { TGraphicPropertyEditorFormEx }

  TGraphicPropertyEditorFormEx = class(TGraphicPropertyEditorForm)
    IconFinderButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure IconFinderButtonClick(Sender: TObject);
  private
    FIconFinderForm: TIconFinderForm;
    function CreateIconFinder: TIconFinderForm;
    procedure IconFinderDblClick(Sender: TObject);
  public
  end;

implementation

{$R *.lfm}

{ TGraphicPropertyEditorFormEx }

function TGraphicPropertyEditorFormEx.CreateIconFinder: TIconFinderForm;
var
  L, T: Integer;
  R: TRect;
begin
  Result := TIconFinderForm.Create(self);
  R := Screen.DesktopRect;
  L := Left + Width;
  if L + Result.Width > R.Right then
  begin
    L := Left - Result.Width;
    if L < R.Left then
      L := Left + (Width - Result.Width) div 2;
  end;
  T := Top;
  Result.Left := L;
  Result.Top := T;
  Result.OnIconDblClick := @IconFinderDblClick;
  Result.ReadSettings('GraphicPropertyEditor');
end;

procedure TGraphicPropertyEditorFormEx.FormCreate(Sender: TObject);
begin
  inherited;
  IconFinderButton.Caption := RSGraphPropEditor_IconFinder;
  LoadSaveBtnPanel.AutoSize := true;
end;

procedure TGraphicPropertyEditorFormEx.IconFinderButtonClick(Sender: TObject);
begin
  FIconFinderForm := CreateIconFinder;
  try
    if FIconFinderForm.Execute then
      FIconFinderForm.LoadPictureFromIconFinder(ImagePreview.Picture);
  finally
    FreeAndNil(FIconFinderForm);
  end;
end;

procedure TGraphicPropertyEditorFormEx.IconFinderDblClick(Sender: TObject);
begin
  if FIconFinderForm <> nil then
    FIconFinderForm.ModalResult := mrOK;
end;


end.

