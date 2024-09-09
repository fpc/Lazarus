unit IconFinderStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  // IconThumbnail
  RSIconViewer_AskSaveMetadata = 'Metadata have been changed. Save?';
  RSIconViewer_ShowAll = 'Show all';
  RSIconViewer_HideAll = 'Hide all';

  // IconViewer frame
  RSIconViewer_FilterByIconSizeHint = 'Filter by icon size';
  RSIconViewer_FilterByIconStyleHint = 'Filter by icon style';
  RSIconViewer_ExpressionToFilterByKeywordsHint = 'Expression to filter by keywords';
  RSIconViewer_EditKeywordFilterHint = 'Enter/edit a keyword filter';
  RSIconViewer_ClearFilterHint = 'Clear the keyword filter';
  RSIconViewer_FilterByKeywordsHint = 'Filters by keywords';
  RSIconViewer_EnterKeywordsHere = 'Enter keywords here...';
  RSIconViewer_FileNameLbl = 'File name:';
  RSIconViewer_SizeLbl = 'Size:';
  RSIconViewer_StyleLbl = 'Style:';
  RSIconViewer_KeywordsLbl = 'Keywords:';
  RSIconViewer_ConfirmDeleteIconMsg = 'Do you really want to delete the selected icon from the library?';

  // IconFinderMetadata
  RSMetadata_Caption = 'Edit Icon Metadata';
  RSMetadata_Keywords = 'Keywords (one keyword per line)';
  RSMetadata_Style = 'Style';
  RSMetadata_ClassicStyle = 'Classic';
  RSMetadata_FlatStyle = 'Flat';
  RSMetadata_OutlineStyle = 'Outline';
  RSMetadata_Outline2Style = 'Outline 2-color';
  RSMetadata_AnyStyle = '(any style)';
  RSMetadata_AnySize = '(any size)';

  // IconFinderFolders
  RSFolders_IconFinderFolders = 'Icon Folders';
  RSFolders_Add = 'Add...';
  RSFolders_Delete = 'Delete';
  RSFolders_MoveUp = 'Move up';
  RSFolders_MoveDown = 'Move down';
  RSFolders_ConfirmDeleteFolderMsg = 'Do you really want to remove this folder from the icon library?';
  RSFolders_NoImages = 'Aborted. No supported images in folder "%s".';

  // IconKeywordFilter editor
  RSKeywordEditor_Caption = 'Keyword Filter Editor';
  RSKeywordEditor_NewKeyword = 'New keyword';
  RSKeywordEditor_Keyword = 'Keyword';
  RSKeywordEditor_KeywordExists = 'Keyword already defined.';
  RSKeywordEditor_Filter = 'Filter';
  RSKeywordEditor_Keywords = 'Keywords';
  RSKeywordEditor_New = 'New';
  RSKeywordEditor_Edit = 'Edit';
  RSKeywordEditor_Columns = 'Columns';
  RSKeywordEditor_Add = 'Add';
  RSKeywordEditor_Clear = 'Clear';

implementation

end.

