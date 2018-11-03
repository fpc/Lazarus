unit registervirtualtreeview; 

{$Mode ObjFpc}
{$H+}

interface
  
procedure Register;

implementation

{$R ideicons.res}

uses
  Classes, SysUtils, LResources, LazarusPackageIntf,
  VirtualTrees, VTHeaderPopup, VTIDEEditors, ComponentEditors;


procedure RegisterUnitVirtualTrees;
begin
  RegisterComponents('Virtual Controls', [TVirtualDrawTree, TVirtualStringTree]);
end;  

procedure RegisterUnitVTHeaderPopup;
begin
  RegisterComponents('Virtual Controls', [TVTHeaderPopupMenu]);
end;

procedure Register;

begin
  RegisterComponentEditor([TVirtualDrawTree, TVirtualStringTree], TVirtualTreeEditor);
  RegisterUnit('VirtualTrees', @RegisterUnitVirtualTrees);
  RegisterUnit('VTHeaderPopup', @RegisterUnitVTHeaderPopup);
end;

end.
