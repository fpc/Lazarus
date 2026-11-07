unit ImagesDatamodule;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, Controls;

type

  { TImgDatamodule }

  TImgDatamodule = class(TDataModule)
    TreeImages: TImageList;
  private

  public

  end;

var
  ImgDatamodule: TImgDatamodule;

implementation

{$R *.lfm}

end.

