unit dmImages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls;

type

  { TImgDatamodule }

  TImgDatamodule = class(TDataModule)
    AppImages: TImageList;
    DBNavigatorImages: TImageList;
  private

  public

  end;

var
  ImgDatamodule: TImgDatamodule;

implementation

{$R *.lfm}

end.

