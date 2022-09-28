unit DialogRes;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, ctypes,
  {$ENDIF}
  LCLType, Graphics, Themes, Controls, ImgList, InterfaceBase, LCLIntf, SysUtils, Classes;

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

{$IFDEF MSWINDOWS}
  {$EXTERNALSYM IDI_HAND}
  IDI_HAND = MakeIntResourceW(32513);
  {$EXTERNALSYM IDI_QUESTION}
  IDI_QUESTION = MakeIntResourceW(32514);
  {$EXTERNALSYM IDI_EXCLAMATION}
  IDI_EXCLAMATION = MakeIntResourceW(32515);
  {$EXTERNALSYM IDI_ASTERISK}
  IDI_ASTERISK = MakeIntResourceW(32516);
  {$EXTERNALSYM IDI_WINLOGO}
  IDI_WINLOGO = MakeIntResourceW(32517);
  {$EXTERNALSYM IDI_SHIELD}
  IDI_SHIELD = MakeIntResourceW(32518);
  {$EXTERNALSYM IDI_WARNING}
  IDI_WARNING = IDI_EXCLAMATION;
  {$EXTERNALSYM IDI_ERROR}
  IDI_ERROR = IDI_HAND;
  {$EXTERNALSYM IDI_INFORMATION}
  IDI_INFORMATION = IDI_ASTERISK;

{$ENDIF MSWINDOWS}

type
  TDialogImageList = class(TLCLGlyphs)
  private
    fDialogIndexes: array[TDialogImage] of Integer;
    function GetDialogIndexes(AIndex: TDialogImage): Integer;
    procedure LoadImage(AIndex: TDialogImage);
  public
    constructor Create(AOwner: TComponent); override;
  public
    property DialogIndexes[AIndex: TDialogImage]: Integer read GetDialogIndexes;
  end;

function GetDialogImages: TDialogImageList;

implementation

{$R dialog_icons.res}
{ $R forms/finddlgunit.lfm}
{ $R forms/replacedlgunit.lfm}

{$IFDEF MSWINDOWS}
Function LoadIconWithScaleDown( hinst:HINST; pszName:LPCWStr;cx:cint;cy:cint;var phico: HICON ):HRESULT; stdcall; external 'comctl32.dll' name 'LoadIconWithScaleDown';
{$ENDIF MSWINDOWS}


var
  DialogImages: TDialogImageList;

function GetDialogImages: TDialogImageList;
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

function TDialogImageList.GetDialogIndexes(AIndex: TDialogImage): Integer;
begin
  if fDialogIndexes[AIndex]=-1 then
    LoadImage(AIndex);
  Result := fDialogIndexes[AIndex];
end;

procedure TDialogImageList.LoadImage(AIndex: TDialogImage);
{$IFDEF MSWINDOWS}
const
  WIN_ICONS: array[TDialogImage] of PWideChar = (IDI_WARNING, IDI_ERROR, IDI_INFORMATION, IDI_QUESTION, IDI_SHIELD);
var
  R: TCustomImageListResolution;
  IconHandle: HICON;
  Ico: TRasterImage;
  Icos: array of TRasterImage;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if (WidgetSet.GetLCLCapability(lcNativeTaskDialog) = LCL_CAPABILITY_YES) and (WIN_ICONS[AIndex]<>nil) then
  begin
    Icos := [];
    try
      for R in Resolutions do
      begin
        if LoadIconWithScaleDown(0, WIN_ICONS[AIndex], R.Width, R.Height, IconHandle)=S_OK then
        begin
          Ico := TIcon.Create;
          TIcon(Ico).Handle := IconHandle;
          SetLength(Icos, Length(Icos)+1);
          Icos[High(Icos)] := Ico;
        end;
      end;
      if Length(Icos)>0 then
      begin
        fDialogIndexes[AIndex] := AddMultipleResolutions(Icos);
        Exit;
      end;
    finally
      for Ico in Icos do
        Ico.Free;
    end;
  end;
  {$ENDIF}
  fDialogIndexes[AIndex] := GetImageIndex(DialogResName[AIndex]);
end;

finalization
  FreeAndNil(DialogImages);

end.

