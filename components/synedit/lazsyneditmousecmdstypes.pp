unit LazSynEditMouseCmdsTypes;

{$mode objfpc}{$H+}

interface

(* For streaming compatibility the enum members of TSynMouseButton must have the
   same names as Controls.TMouseButton
   To avoid conflicts the definiton will be hidden here and aliases be defind for
   common usage
*)

type
  TSynMouseButton = (mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2, mbWheelUp, mbWheelDown, mbWheelLeft, mbWheelRight);

implementation

end.

