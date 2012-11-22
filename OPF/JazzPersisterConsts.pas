unit JazzPersisterConsts;

interface

uses
  JazzConsts;

const
  SNull = 'NULL';
  SParamName = 'P%d_%s';
  SFKPrefix = 'FK_';
  SCantDeleteFile = 'Can''t delete file ''%s''';
  SColumnNotMapped = 'Column %s not mapped';
  SConnectionNotDefined = 'Connection not defined';
  SDatabaseNotDefined = 'Database not defined';
  SFileNotFound = 'File ''%s'' not found';
  SFileToSaveExists = 'File ''%s'' exists.' + EOL + 'Override?';
  SFromMemberNotDefined = 'FromMembers columns not defined yet for member %s mapping.'; 
  SInvalidCriteria = 'Invalid Criteria, check Grouping.';
  SMemberMetatadaNotFound = 'Member ''%s'' Meta not Found.';
  SMemberNameRequired = 'MemberName is required.';
  SNoActiveSession = 'No Session Active.';
  SNoIDDefined = 'No Object ID defined to Class ''%s''.';
  SNoMechanism = 'No Mechanism defined.';
  SNotDataGeneratorClass = '''%s'' Class does not inherits from TDataGenerator';
  SOnlyListCanBeLoaded = 'Only IObjectListType can be loaded with StreamMechanism';
  SOnlyListCanBeSaved = 'Only IObjectListType can be saved with StreamMechanism';
  SOrderNotDefined = 'Order not defined';
  SPersisterLoaded = 'Persister already loaded.';
  SSessionParams = 'Session Name and Mechanism Class required.';
  STransactionNotSupported = 'Transaction not supported in ''%s'' Mechanism.';
  SMappingNotDefined = 'Mapping not registered for class %s';
  SSessionNotDefined = 'Session not defined.';

// Captions
  SWarning = 'Warning';

var
  SDatabaseFolder: string = '.\';
  SFileExtension: string = 'dat';

implementation

end.
