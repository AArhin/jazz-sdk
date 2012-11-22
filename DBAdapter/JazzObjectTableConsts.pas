unit JazzObjectTableConsts;

interface
                                  
uses
  DB;
  
resourcestring
  SIndexOutOfRange = 'Index out of range';
  SNoRecords = 'No Records';
  SClassNotRegistered = 'Class %s not registered in TypeRegister.';

const
  FieldWithSize = [
    ftBCD,
    ftFMTBcd,
    ftGuid,
    ftInterface,
    ftString,
    ftVariant,
    ftWideString
  ];


implementation

end.

