unit DialogRes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  LCLType, Graphics, Themes, Controls, ImgList, InterfaceBase, LCLIntf, LCLProc;

type
  TDialogImage = idDialogWarning..idDialogShield;
const
  // dialog images. supported scalings = 150, 200
  DialogResName: array[TDialogImage] of string =
  (
    {idDialogWarning} 'dialog_warning',
    {idDialogError  } 'dialog_error',
    {idDialogInfo   } 'dialog_information',
    {idDialogConfirm} 'dialog_confirmation',
    {idDialogShield } 'dialog_shield'
  );

type
  TDialogImageList = class(TLCLGlyphs)
  private
    fDialogIndexes: array[TDialogImage] of Integer;
    procedure LoadImage(AIndex: TDialogImage);
    function GetDialogIcon(AIndex: TDialogImage): Integer;
  public
    constructor Create(AOwner: TComponent); override;
  public
    property DialogIcon[AIndex: TDialogImage]: Integer read GetDialogIcon;
  end;

function DialogGlyphs: TDialogImageList;

implementation

{$R dialog_icons.res}
{ $R forms/finddlgunit.lfm}
{ $R forms/replacedlgunit.lfm}

var
  DialogImages: TDialogImageList;

function DialogGlyphs: TDialogImageList;
begin
  if not Assigned(DialogImages) then
    DialogImages := TDialogImageList.Create(nil);
  Result := DialogImages;
end;

{ TDialogImageList }

constructor TDialogImageList.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);

  for I in TDialogImage do
    fDialogIndexes[I] := -1;

  Width := 32;
  Height := 32;
  RegisterResolutions([16, 24, 32, 48, 64]);
end;

function TDialogImageList.GetDialogIcon(AIndex: TDialogImage): Integer;
begin
  if fDialogIndexes[AIndex]=-1 then
    LoadImage(AIndex);
  Result := fDialogIndexes[AIndex];
end;

procedure TDialogImageList.LoadImage(AIndex: TDialogImage);
var
  R: TCustomImageListResolution;
  Bmp: TCustomBitmap;
  Bmps: array of TCustomBitmap;
  Image, Mask: HBitmap;
begin
  Bmps := [];
  try
    for R in Resolutions do
    begin
      if ThemeServices.GetStockImage(AIndex, R.Width, R.Height, Image, Mask) then
      begin
        Bmp := TBitmap.Create;
        Bmp.SetHandles(Image, Mask);
        SetLength(Bmps, Length(Bmps)+1);
        Bmps[High(Bmps)] := Bmp;
      end else
        break; // goto default handling
    end;
    if Length(Bmps)>0 then
    begin
      fDialogIndexes[AIndex] := AddMultipleResolutions(Bmps);
      Exit; // we are good to go
    end;
  finally
    for Bmp in Bmps do
      Bmp.Free;
  end;

  // default handling
  fDialogIndexes[AIndex] := GetImageIndex(DialogResName[AIndex]);
end;

procedure InterfaceFinal;
begin
  FreeAndNil(DialogImages);
end;

initialization
  RegisterInterfaceFinalizationHandler(@InterfaceFinal);

end.

