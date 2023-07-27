unit dmImages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls;

// Some selected image indices to simplify updating images.
const
  iiConnection   = 0;
  iiTables       = 1;
  iiTable        = 2;
  iiFields       = 3;
  iiField        = 4;
  iiIndexes      = 5;
  iiIndex        = 27;
  iiTableData    = 7;
  iiIndexFields  = 8;
  iiIndexOptions = 9;
  iiConnections  = 30;
  iiDataDict     = 32;

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

