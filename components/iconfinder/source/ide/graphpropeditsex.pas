{
 *****************************************************************************
  This file is part of a Lazarus Package, IconFinder.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Extended graphic property editors for searching icons by keywords.
}

unit GraphPropEditsEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  GraphPropEdits, GraphicPropEdit, GraphicPropEditEx;

type
  TGraphicPropertyEditorEx = class(TGraphicPropertyEditor)
  protected
    class function GetEditorFormClass: TGraphicPropertyEditorFormClass; override;
  end;

  TPicturePropertyEditorEx = class(TPicturePropertyEditor)
  protected
    class function GetEditorFormClass: TGraphicPropertyEditorFormClass; override;
  end;

  TButtonGlyphPropEditorEx = class(TButtonGlyphPropEditor)
  protected
    class function GetEditorFormClass: TGraphicPropertyEditorFormClass; override;
  end;

implementation

class function TGraphicPropertyEditorEx.GetEditorFormClass: TGraphicPropertyEditorFormClass;
begin
  Result := TGraphicPropertyEditorFormEx;
end;

class function TPicturePropertyEditorEx.GetEditorFormClass: TGraphicPropertyEditorFormClass;
begin
  Result := TGraphicPropertyEditorFormEx;
end;

class function TButtonGlyphPropEditorEx.GetEditorFormClass: TGraphicPropertyEditorFormClass;
begin
  Result := TGraphicPropertyEditorFormEx;
end;


end.

