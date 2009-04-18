{%MainUnit ../dialogs.pp}
{******************************************************************************
                                  TColorDialog
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

function ExtractColorIndexAndColor(const AColorList: TStrings;
  const AIndex: Integer; out ColorIndex: Integer; out ColorValue: TColor
  ): Boolean;
var
  Name, Value: String;
  Code: Integer;
begin
  // extract Index and Color from string like: ColorA = 10FF30
  Name := AColorList.Names[AIndex];
  Value := AColorList.ValueFromIndex[AIndex];

  Result := Pos('Color', Name) = 1;
  if not Result then
    Exit;

  Delete(Name, 1, 5); // delete Color
  Result := Length(Name) = 1;

  if not Result then
    Exit;

  ColorIndex := Ord(Name[1]) - Ord('A');
  Val('$' + Value, ColorValue, Code);

  Result := Code = 0;
end;

{------------------------------------------------------------------------------
  Method: TColorDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TColorDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCustomColors := TStringList.Create;
  // add default colors
  GetColorValues(@AddDefaultColor);
  fCompStyle := csColorDialog;
end;

destructor TColorDialog.Destroy;
begin
  FCustomColors.Free;
  inherited Destroy;
end;

procedure TColorDialog.SetCustomColors(const AValue: TStrings);
begin
  FCustomColors.Assign(AValue);
end;

procedure TColorDialog.AddDefaultColor(const s: AnsiString);
var
  AColor: TColor;
  Index: Integer;
begin
  if IdentToColor(s, AColor) and
     ColorIndex(AColor, Index) and
     (Index < StandardColorsCount + ExtendedColorCount) then
    FCustomColors.Values['Color' + Chr(Ord('A') + Index)] := IntToHex(AColor, 6);
end;

class procedure TColorDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterColorDialog;
end;

function TColorDialog.DefaultTitle: string;
begin
  Result := rsSelectcolorTitle;
end;


