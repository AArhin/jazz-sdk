unit JazzConsts;

{$I Jazz.inc}

interface

const
  GUID_NULL = '{00000000-0000-0000-0000-000000000000}';

  NotFound = -1;
  EmptyStr = '';
  CharSpace = ' ';
  EOL = #13#10;
  EOL2= EOL + EOL;
  NullStr = 'NULL';

// VTF
  SItemsCount = '%d items';
  SClassCantBeNil = 'Class can''t be nil.';
  SAddDoubleNamed = 'User overloaded Add method to DoubleNamed.';
  SAddNotDoubleNamed = 'User overloaded Add method to not DoubleNamed';
  SMemberCantBeNil = 'Member can''t be nil';
  SListCantBeString = 'A ObjectList can'' be SetAsString';
  SClassNotRegisteredForMember = 'Class not registered for member ''%s''';

// TypeInfo
  STypeInfoNotFound = 'TypeInfo ''%s'' not found.';
  SIntfNotSupported = 'Interface not suported!';
  SBlobNotSupported = 'Blob conversion not supported';
  
var
//masks
  STimeStampFormat: string = 'mm/dd/yyyy hh:mm:ss';
{$IFDEF DELPHI6}
  {$IFNDEF DELPHI7}
  NullAsStringValue: string = '';
  {$ENDIF}
{$ENDIF}


implementation

end.






