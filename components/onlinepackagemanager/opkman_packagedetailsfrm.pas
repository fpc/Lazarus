unit opkman_packagedetailsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TPackageDetailsFrm }

  TPackageDetailsFrm = class(TForm)
    mDetails: TMemo;
  private

  public

  end;

var
  PackageDetailsFrm: TPackageDetailsFrm;

implementation

{$R *.lfm}

end.

