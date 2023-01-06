unit instantsearchstrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Resourcestring
  lrsInstantSearch = 'Instant search';
  lrsInstantSearchMenu = 'Instant search';
  lrsCurrentProject = 'Active project';
  lrsErrorMinLengthIs2 = 'There should be at least two characters to search.';
  lrsConfigNeeded = 'Configuration needed';
  lrsConfigNeededReason = 'Instant search is not yet configured:'+LineEnding+'%s';
  lrsNotConfigured = 'User did not configure anything.';
  lrsCancelSearch = 'Cancel search';
  lrsConfigure = 'Configure Instant Search';
  lrsNoSearchTrees = 'No source trees were defined.';
  lrsNoServerTrees = 'Source trees were not yet indexed.';
  lrsNoTransport = 'No transport protocol was selected.';
  lrsNoMysqlVersion = 'No MySQL Client library version was selected for MySQL transport.';
  lrsConnectionTestOK = 'Connection test succeeded!';
  lrsConnectionTestFailed = 'Connection test failed, reason:'+LineEnding+'%s';
  lrsCreateIndexFailed = 'Creation of index %s failed, reason:'+LineEnding+'%s';
  lrsDeleteIndexFailed = 'Deletion of index %s failed, reason:'+LineEnding+'%s';
  lrsSearchTextHint = 'Enter search text, at least %d characters';
  lrsIndexAlreadyExists = 'Index table "%s" already exists.';
  lrsCannotDeleteIndexWhileIndexing = 'Cannot delete an index while an indexing operation is in progress.';
  lrsDeleteIndex = 'Delete index';
  lrsCreateIndex2 = 'Create index';
  lrsDeleteIndexHint = 'Prompt for index name and delete the index.';
  lrsIndexToDelete = 'Enter the name of the index to delete. This operation cannot be undone.';
  lrsNoSuchIndex = 'No such index exists: %s';
  lrsIndexDeleted = 'Index %s successfully deleted.';
  lrsCreateIndexWithName = 'Create index with name %s?';
  lrsYesCreateIndex = 'Yes, create the index';
  lrsDoNotCreateIndex = 'No, do not create the index';

  lrsTreeName        = 'Name';
  lrsTreeNameHint    = 'Unique name for this tree';
  lrsTreeBaseDir     = 'Path';
  lrsTreeBaseDirHint = 'Directory with source files';
  lrsTreeRecurse     = 'Recurse';
  lrsTreeAllFiles    = 'All files';
  lrsTreeExtensions  = 'Extensions';
  lrsTreeExtensionsHint = 'Comma-separated list of extensions of files';
  lrsErrorDuplicateName = 'Duplicate tree name: %s.'+LineEnding+'Please choose another name.';
  lrsErrNameCannotBeEmpty = 'Source tree name cannot be empty.';
  lrsErrPathDoesNotExist = 'Directory "%s" does not exist.'+LineEnding+'Please select an existing directory.';
  lrsErrorConnecting = 'Error "%s" attempting to connect to Manticore server.';
  lrsErrorSearching  = 'Error "%s" when searching using Manticore server.';

  lrsTreeEnabled = 'Enabled';
  lrsTreeNotFoundResfresh = 'Source tree %s not found.'+LineEnding+'Please refresh search results.';
  lrsAllTreesNeedindexing = 'All source trees must be indexed.';
  lrsTheseTreesNeedIndexing = 'The following source trees need to be indexed:'+LineEnding+'%s';
  lrsIndexNow = 'Would you like to start indexing them now?';
  lrsThisNeedsSavingSettings = 'This operation runs in the background, and will cause the settings to be saved now.';

  lrsNeedIndexing = 'Source trees must be indexed';
  lrsSaveAndIndex = 'Yes, index needed source trees';
  lrsSaveAndIndexAll = 'Yes, index all source trees';
  lrsIndexLater = 'No, do not index now';

  lrsNewTree = 'New source tree %d';
  lrsConfirm = 'Confirmation';
  lrsConfirmDeleteTree = 'Delete tree "%s", are you sure?'+LineEnding+'This action cannot be undone.';

  lrsTestConnection = 'Test connection';
  lrsSourceTreeAdd  = 'Add';
  lrsSourceTreeAddHint  = 'Add a new source tree definition.';
  lrsSourceTreeDelete = 'Delete';
  lrsSourceTreeDeleteHint  = 'Delete the highlighted source tree definition.';
  lrsSourceTreeIndex = 'Index';
  lrsSourceTreeEdit = 'Edit';
  lrsSourceTreeEditHint = 'Edit the highlighted source tree definition.';
  lrsSourceTreeClear = 'Clear';
  lrsSourceTreeClearHint = 'Remove all words in the highlighted source tree definition.';
  lrsSourceTreeClearAll = 'Clear all';
  lrsSourceTreeClearAllHint = 'Remove all words in all source tree definitions.';

  lrsManticoreOptions = 'Manticore server';
  lrsIndexingOptions = 'Indexing and searching';
  lrsMinSearchLength = 'Min. searchterm length';
  lrsMaxClipbrdSearchTermLength = 'Max. clipboard length';
  lrsResultsLimit = 'Max. result count';
  lrsProtocol = 'Protocol';
  lrsPort = 'Port';
  lrsHost = 'Hostname';
  lrsEnterHostnameLeaveEmptyForLocalhost = 'Enter hostname, leave empty for '
    +'localhost';
  lrsMysqlversion = 'MySQL client version';
  lrsIdleDisconnectTime = 'Idle disconnect time';
  lrsSearchEngine = 'Search engine';
  lrsSourceTrees = 'Source trees';
  lrsIndexName = 'Index name';

  lrsCannotConnectToManticore = 'Error "%s" trying to connect to Manticore server: %s';
  lrsNoIndex = 'Search index table not found';
  lrsIndexNotFoundCreate = 'Search index table "%s" was not found.'+LineEnding+'Create it?';
  lrsCreateIndex = 'Create index table';
  lrsAbortOperation = 'No, abort operation';
  lrsSaveNeeded = 'Save settings required.';
  lrsIndexNeedsSave = 'In order to index a source tree, the settings must be saved.'+LineEnding+'Save settings?';
  lrsSave = 'Yes, save settings';
  lrsIndexOperationFailed = 'Failed to start index operation.';
  lrsCannotIndexIndexInProgress = 'Cannot start an index operation while another one is in progress.';
  lrsFinishedIndexingTree = 'Finished indexing tree "%s". Processed %d files.';
  lrsStartIndexingTree = 'Start indexing tree "%s", directory: "%s"';
  lrsFinishedIndexingProject = 'Finished indexing project "%s". Processed %d files.';
  lrsIndexingProjectTerminated = 'Indexing project "%s" was terminated. Processed %d files.';
  lrsStartIndexingProject = 'Start indexing project "%s", directory: "%s"';
  lrsIndexingOperationFinished = 'Finished indexing operation.';
  lrsMarkingIndexable = 'Marking project "%s" as indexable.';
  lrsTestButtonHint = 'Test the connection parameters.';
  lrsCreateIndexButtonHint = 'Create the index table. You need to test the connection first.';
  lrsNotProperlyConfigured = 'Instant search is not properly configured to connect to Manticore server: %s';

  lrsOnOpen = 'Project is opened';
  lrsManual = 'Manual only';
  lrsOnFirstSave = 'Project is first saved';
  lrsAllProjects = 'All projects';
  lrsTimed = 'Delayed after project open';

  lrsIndexProjectStrategy = 'Mark project indexable';
  lrsAutomation = 'Automation';
  lrsIndexProjectMoment = 'Index project when';
  lrsIndexProjectDelay1 = 'in';
  lrsIndexProjectDelay2 = 'minutes after open';
  lrsMarkProjectIndexable = 'Mark project indexable for Instant Search';
  lrsIndexProject = 'Index project for Instant Search';

implementation

end.

