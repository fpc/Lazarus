unit opkman_packagedetailsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TPackageDetailsFrm }

  TPackageDetailsFrm = class(TForm)
    bOk: TButton;
    mDetails: TMemo;
    pnButtons: TPanel;
  private

  public

  end;

var
  PackageDetailsFrm: TPackageDetailsFrm;

implementation

{$R *.lfm}

end.

