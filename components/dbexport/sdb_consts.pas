{ DB Export component resource strings

  Copyright (C) 2009 Marcelo B Paula

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
unit sdb_consts;

{$mode objfpc}{$H+}

interface

resourcestring

  // RegDBExport
  SExecute = 'Execute...';
  SErrNoDatasetAssigned = 'DataExporter has no Dataset assigned.';
  SErrOpeningDataset = 'Could not open dataset for exporting: %s';

  // fpdataexporter
  SNRecordsExported = 'Succesfully exported %d records.';
  SCancelRecordsExported = 'Exported %d before user canceled.';

  // Select Data Export Format form
  sdb_Selectdataexportformat = 'Select data export Format';
  sdb_Availableexportformats = 'Available export formats:';

  // Export progress form
  SProgress = 'Exporting %d records';
  //
  sdb_Exportprogress = 'Export progress';
  sdb_Cancel         = '&Cancel';

  // Configure Data Export form
  sdb_Configuredataexport = 'Configure data export';
  sdb_Filename            = 'Filename:';
  sdb_Fields              = 'Fields';
  sdb_Formatting          = 'Formatting';
  sdb_Selectfieldstoexport= 'Select fi&elds to export';
  sdb_Propertiesforselectedfield = 'Properties for selected field:';
  sdb_Moveselectedfieldup = 'Move selected field up';
  sdb_Moveselectedfielddown = 'Move selected field down';


implementation

end.

