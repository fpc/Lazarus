unit Fppkg_Interface;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  PackageIntf,
  FppkgIntf,
  Fppkg_EnvironmentOptions,
  fppkg_packagevariant;

type

  { TFppkgInterfaceEx }

  TFppkgInterfaceEx = class(TFppkgInterface)
  private
    function GetComponentName(AName: string): String;
  protected
    function GetInstallFPMakeDependencies: Boolean; override;
    function GetUseFPMakeWhenPossible: Boolean; override;
  public
    function ConstructFpMakeImplementationSection(APackage: TIDEPackage): string; override;
    function ConstructFpMakeInterfaceSection(APackage: TIDEPackage): string; override;
    function ConstructFpMakeDependenciesFileSection(APackage: TIDEPackage): string; override;
  end;

implementation

{ TFppkgInterfaceEx }

function TFppkgInterfaceEx.GetComponentName(AName: string): String;
begin
  Result := StringsReplace(AName, ['/',' ', '\', ':', ';', '.', ',','(',')'], ['_','_', '_', '_', '_', '_', '_', '_', '_'], [rfReplaceAll]);
end;

function TFppkgInterfaceEx.GetInstallFPMakeDependencies: Boolean;
begin
  Result := TFppkgEnvironmentOptions(TFppkgEnvironmentOptions.GetInstance).InstallFPMakeDependencies;
end;

function TFppkgInterfaceEx.GetUseFPMakeWhenPossible: Boolean;
begin
  Result := TFppkgEnvironmentOptions(TFppkgEnvironmentOptions.GetInstance).UseFPMakeWhenPossible;
end;

function TFppkgInterfaceEx.ConstructFpMakeInterfaceSection(APackage: TIDEPackage): string;
var
  VariantList: TFppkgPackageVariantList;
  Variant: TFppkgPackageVariant;
  i, j: Integer;
begin
  Result := '';
  VariantList := TFppkgPackageVariantList.Create(True);
  try
    APackage.CustomOptions.AppendBasePath('Fppkg/');
    APackage.CustomOptions.AppendBasePath('PackageVariants/');
    try
      VariantList.Load(APackage.CustomOptions);

      for i := 0 to VariantList.Count -1 do
        begin
        Variant := VariantList.Items[i];
        result := Result + '  ' + GetComponentName(Variant.Name) + 'Variant: TPackageVariants;' + LineEnding +'  ';
        if Variant.Items.Count > 0 then
          begin
          for j := 0 to Variant.Items.Count -1 do
            begin
            result := result + GetComponentName(Variant.Items[j].Name) + 'VariantItem, ';
            end;
          SetLength(Result, length(Result) -2);
          Result := Result + ': TPackageVariant;' + LineEnding;
          end;
        end;

      if Result <> '' then
        Result := 'var' + LineEnding + Result;
    finally
      APackage.CustomOptions.UndoAppendBasePath;
      APackage.CustomOptions.UndoAppendBasePath;
    end;
  finally
    VariantList.Free;
  end;
end;

function TFppkgInterfaceEx.ConstructFpMakeDependenciesFileSection(APackage: TIDEPackage): string;
var
  VariantList: TFppkgPackageVariantList;
  Variant: TFppkgPackageVariant;
  i, j, k: Integer;
  Found: Boolean;
begin
  Result := '';
  VariantList := TFppkgPackageVariantList.Create(True);
  try
    APackage.CustomOptions.AppendBasePath('Fppkg/');
    APackage.CustomOptions.AppendBasePath('PackageVariants/');
    try
      VariantList.Load(APackage.CustomOptions);

      for i := 0 to APackage.FileCount -1 do
        if APackage.Files[i].FileType = pftUnit then
          begin
          Found := False;

          for j := 0 to VariantList.Count -1 do
            begin
            Variant := VariantList.Items[j];
            for k := 0 to Variant.Items.Count -1 do
              begin
              if Variant.Items[k].PackageFiles.IndexOf(APackage.Files[i].GetShortFilename(False)) > -1 then
                begin
                Found := True;
                //Result := Result + '    ' + GetComponentName(Variant.Items[k].Name) +'VariantItem.Targets.AddImplicitUnit('''+APackage.Files[i].GetShortFilename(False)+''');' + LineEnding;
                end;
              end;
            end;

          if not Found then
            Result := Result + '    t.Dependencies.AddUnit('''+APackage.files[i].Unit_Name+''');' + LineEnding;
          end;

      for i := 0 to APackage.FileCount-1 do
        if (APackage.Files[i].FileType=pftUnit) then
          begin
          Found := False;

          for j := 0 to VariantList.Count -1 do
            begin
            Variant := VariantList.Items[j];
            for k := 0 to Variant.Items.Count -1 do
              begin
              if Variant.Items[k].PackageFiles.IndexOf(APackage.Files[i].GetShortFilename(False)) > -1 then
                begin
                Found := True;
                //if (pffAddToPkgUsesSection in APackage.Files[i].Flags) then
                //  Result := Result + '    ' + GetComponentName(Variant.Items[k].Name) +'VariantItem.Targets.AddUnit('''+APackage.Files[i].GetShortFilename(False)+''');' + LineEnding;
                //else
                  Result := Result + '    ' + GetComponentName(Variant.Items[k].Name) +'VariantItem.Targets.AddImplicitUnit('''+APackage.Files[i].GetShortFilename(False)+''');' + LineEnding;
                end;
              end;
            end;

          if not found then
            begin
            //if (pffAddToPkgUsesSection in APackage.Files[i].Flags) then
            //  Result:=Result+'    T:=P.Targets.AddUnit('''+CreateRelativePath(APackage.Files[i].Filename,APackage.Directory)+''');'+LineEnding)
            //else
              Result:=Result+'    P.Targets.AddImplicitUnit('''+APackage.Files[i].GetShortFilename(False)+''');'+LineEnding;
            end;
          end;
    finally
      APackage.CustomOptions.UndoAppendBasePath;
      APackage.CustomOptions.UndoAppendBasePath;
    end;
  finally
    VariantList.Free;
  end;
end;

function TFppkgInterfaceEx.ConstructFpMakeImplementationSection(APackage: TIDEPackage): string;
var
  VariantList: TFppkgPackageVariantList;
  Variant: TFppkgPackageVariant;
  ItemName: string;
  i, j, k: Integer;
  CustomCode: TStrings;
begin
  Result := '';
  VariantList := TFppkgPackageVariantList.Create(True);
  try
    APackage.CustomOptions.AppendBasePath('Fppkg/');
    try
    APackage.CustomOptions.AppendBasePath('PackageVariants/');
      try
        VariantList.Load(APackage.CustomOptions);

        for i := 0 to VariantList.Count -1 do
          begin
          Variant := VariantList.Items[i];
          result := result + '    ' + GetComponentName(Variant.Name) + 'Variant := AddPackageVariant('''+Variant.Name+''',true);' + LineEnding;
          result := result + '    P.AddPackageVariant('+GetComponentName(Variant.Name)+'Variant);' + LineEnding;
          if Variant.Items.Count > 0 then
            begin
            for j := 0 to Variant.Items.Count -1 do
              begin
              ItemName := GetComponentName(Variant.Items[j].Name) + 'VariantItem';
              result := result + '    ' + ItemName + ' := ' + GetComponentName(Variant.Name) + 'Variant.add('''+Variant.Items[j].Name+''');' + LineEnding;
              for k := 0 to Variant.Items[j].CompilerOptions.Count -1 do
                begin
                Result := Result + '    ' + ItemName + '.Options.Add(''' + Variant.Items[j].CompilerOptions[k] + ''');' + LineEnding;
                end;
              end;
            end;
          end;
      finally
        APackage.CustomOptions.UndoAppendBasePath;
      end;
      CustomCode := TStringList.Create;
      try
        APackage.CustomOptions.GetValue('CustomCode', CustomCode);
        result := result + CustomCode.Text;
      finally
        CustomCode.Free;
      end;
    finally
      APackage.CustomOptions.UndoAppendBasePath;
    end;
  finally
    VariantList.Free;
  end;
end;

initialization
  FppkgInterface := TFppkgInterfaceEx.Create;
finalization
  FreeAndNil(FppkgInterface);
end.

