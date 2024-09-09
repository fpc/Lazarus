{
 *****************************************************************************
  This file is part of a Lazarus Package, IconFinder.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Unit for registration of
 - new property editors for TGraphic, TImage, TSpeedButton.Glyph, TBitBtn.Glyph
 - new component editor for TImageList
 - a settings page in the Options form of the IDE for setting up IconFinder.
}

unit IconFinderReg;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Controls, Graphics, Buttons,
  // IDEIntf
  PropEdits, ComponentEditors, IDEOptEditorIntf,
  // IconFinder
  ImageListEditorEx, GraphPropEditsEx;

procedure Register;

implementation

{$R iconfinder_ide_img.res}

uses
  IconFinderSettings;

procedure Register;
begin
  // Register new property editors for TGraphic, TImage, button glyphs
  RegisterPropertyEditor(ClassTypeInfo(TGraphic), nil, '', TGraphicPropertyEditorEx);
  RegisterPropertyEditor(ClassTypeInfo(TPicture), nil, '', TPicturePropertyEditorEx);
  RegisterPropertyEditor(ClassTypeInfo(TBitmap), TSpeedButton, 'Glyph', TButtonGlyphPropEditorEx);
  RegisterPropertyEditor(ClassTypeInfo(TBitmap), TBitBtn, 'Glyph', TButtonGlyphPropEditorEx);

  // Register new component editor for TImageList
  RegisterComponentEditor(TImageList, TImageListComponentEditorEx);

  // Register options page in IDE
  IconFinderOptionsFrameID := RegisterIDEOptionsEditor(IconFinderOptionsGroup,
    TIconFinderSettingsFrame, IconFinderOptionsIndex)^.Index;
end;


end.

