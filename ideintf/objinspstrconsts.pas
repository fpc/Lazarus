{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit ObjInspStrConsts;

{$mode objfpc}{$H+}

interface

resourcestring
  //
  oisObjectInspector = 'Object Inspector';
  oisAll = 'All';
  oisError = 'Error';
  oisItemsSelected = '%u items selected';
  
  oiscAdd = '&Add';
  oiscDelete = '&Delete';
  oisUnknown = 'Unknown';

  oisProperties='Properties';
  oisEvents='Events';
  oisSetToDefault = 'Set to default: %s';
  oisSetToDefaultValue = 'Set to default value';
  
  // typeinfo
  oisValue = 'Value:';
  

  //ListView items editor
  sccsLvEdtCaption        = 'ListView editor';
  sccsLvEdtGrpLCaption    = ' Items ';
  sccsLvEdtGrpRCaption    = ' Item property ';
  sccsLvEdtlabCaption     = 'Label';
  sccsLvEdtImgIndexCaption= 'Image index';
  sccsLvEdtBtnAdd         = 'New';
  sccsLvEdtBtnDel         = 'Delete';
  sccsLvEdtBtnAddSub      = 'Sub item';

  //Image editor strings
  sccsILEdtCaption = 'Image list editor';
  sccsILCmbImgSel  = ' Selected image ';
  sccsILCmbImgList = ' Images ';
  sccsILBtnAdd     = 'Add ...';
  sccsILBtnClear   = 'Clear';
  sccsILConfirme   = 'Confirme clear all images ?';

  // component editors
  cesStringGridEditor = 'StringGrid Editor ...';
  cesStringGridEditor2 = 'StringGrid Editor';


implementation

end.

