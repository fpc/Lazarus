unit opkman_maindm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TMainDM }

  TMainDM = class(TDataModule)
    Images: TImageList;
  private

  public

  end;

var
  MainDM: TMainDM;

const
  IMG_REPOSITORY = 0;
  IMG_PKG_PLUS = 1;
  IMG_PKG_FILE = 2;
  IMG_DESCRIPTION = 3;
  IMG_AUTHOR = 4;
  IMG_LAZ_COMPATIBILITY = 5;
  IMG_FPC_COMPATIBILITY = 6;
  IMG_WIDGETSET = 7;
  IMG_PKG_TYPE = 8;
  IMG_LICENSE = 9;
  IMG_DEPENDENCIES = 10;
  IMG_REPO_FILE = 13;
  IMG_PKG_MINUS = 27;
  IMG_EXPAND = 28;
  IMG_COLLAPSE = 29;
  IMG_CLEAR = 30;
  IMG_INFO = 31;
  IMG_OK = 32;
  IMG_ERROR = 33;
  IMG_SVN = 34;
  IMG_FILE_VERSION = 35;

implementation

{$R *.lfm}

end.

