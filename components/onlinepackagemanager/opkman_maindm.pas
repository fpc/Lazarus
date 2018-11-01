unit opkman_maindm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TMainDM }

  TMainDM = class(TDataModule)
    ImageList1: TImageList;
  private

  public

  end;

var
  MainDM: TMainDM;

implementation

{$R *.lfm}

end.

