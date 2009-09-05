{ <description>

  Copyright (C) <year> <name of author> <contact>

  Estes fontes são software livre; você pode redistribuir e/ou modificá-los
  sob os termos da GNU Library General Public License como publicada pela Free
  Software Foundation; ou a versão 2 da Licença, ou (a sua escolha) qualquer
  versão posterior.

  Este código é distribuído na esperança de que seja útil, mas SEM
  QUALQUER GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE ou
  ADEQUAÇÃO A UMA FINALIDADE PARTICULAR. Veja a licença GNU General Public
  License para maiores detalhes.

  Você deve ter recebido uma cópia da licença GNU Library General Public
  License juntamente com esta biblioteca; senão, escreva a Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit ldd_consts;

{$mode objfpc}{$H+}

interface

resourcestring

 //Set Project Data Dictionary form
 ldd_SetProjectDataDictionary = 'Set project Data Dictionary';
 ldd_UseDatadictionary        = '&Use Data dictionary for this project';
 ldd_Knowndatadictionary      = 'Known data dictionary';
 ldd_Datadictionary           = 'Data dictionary &file:';
 ldd_Openexistingfile         = 'Open existing file';
 ldd_Filenameedit             = 'Data dictionary|*.fdd|Any file|*.*';
 ldd_Ok                       = '&OK';
 ldd_Cancel                   = '&Cancel';
 //
 SErrSelectDD   = 'Please select a known data dictionary';
 SErrSelectFile = 'Please select an existing data dictionary file';

 // idedatadict
 SLoadingDataDict = 'Loading data dictionary ';
 SFromfile = 'from file: ';

 // reglazdatadict
 SMenuDataDict          = 'Data dictionary';
 SWrongSelection        = 'Wrong selection count : %d';
 SMenuConfProjDataDict  = 'Set...';
 SMenuOpenDataDict      = 'Open';
 SMenuConfDataDict      = 'Configuration';
 SMenuDatadesktop       = 'Database Desktop';
 SErrNoDataDesktopDoSelect
                        = 'Could not locate the database desktop application.'+LineEnding
                          +'Would you like to select it ?';
 SApplicationFilter     = 'Applications|*';
 SMenuDatadictApply     = 'Apply';
 SMenuDatadictDesignSQL = 'Design SQL';
 SMenuDatadictCreateCode = 'Create code from dataset';
 SErrNoDataDictActive   = 'No datadictionary active.'+LineEnding+
                          'Please set a data dictionary for the project';
 SApplyingDDToDataset   = 'Applying data dictionary to dataset %s';
 SApplyingDDToField     = 'Applying data dictionary to field %s';
 SWarningNoDDForField   = 'Warning: No definition in data dictionary for field %s';
 SNotYetImplemted       = 'Not yet implemented';

 // Configure Data Dictionary form
 ldd_Configuredatadictionary = 'Configure Data Dictionary';
 ldd_Databasedesktopapplication = 'Database desktop application';
 ldd_Filenameapplicationsfilter = 'Applications|*.exe|Any file|*.*';
 ldd_Defaultdatadictdirectory   = 'Default data dictionary directory';
 //
 SErrSelectExe = 'Please select an existing database desktop application';
 SErrSelectDir = 'Please select an existing directory';

 // Select code to be generated form
 ldd_Selectcodetobegenerated = 'Select code to be generated';
 ldd_Availablecodegenerators = 'Available code generators';

 // Configure generated code form
 ldd_Configuregeneratedcode  = 'Configure generated code';
 ldd_Saveto                  = 'Save to';
 ldd_Showgeneratedcode       = 'Show generated code';
 ldd_Fields                  = 'Fields';
 ldd_Options                 = 'Options';
 ldd_Fieldstogeneratecodefor = 'Fields to generate code for';
 ldd_Propertiesforselected   = 'Properties for selected fields';

 // Generated code form
 ldd_Generatedcode           = 'Generated code';
 ldd_Close                   = '&Close';
 ldd_Save                    = '&Save';
 ldd_SDCodetitle             = 'Save file as';
 ldd_SDCodefilter            = 'Pascal files|*.pp;*.pas;*.inc|All files|*.*';


implementation

end.

