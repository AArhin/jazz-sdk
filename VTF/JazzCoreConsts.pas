unit JazzCoreConsts;

interface

uses
  JazzConsts;

const
//resourcestrings
  SStreamNil = 'Stream or Reader can''t be nil!';
  STypesNotMatch = 'Incompatible types or items %s and %s';
  SCountNotMatch = 'Count does not match';
  SObjectAsString = 'ValueObject cannot (yet) be set as string';
  SCommandNotFound = 'Command ''%s'' not found';
  SControlNil = 'Control can''t be nil';
  SIntfNotSupported = 'Interface not supported';
  SObjectClassNotDefined = 'ObjectClass not defined.';
  STargetIntfSelection = 'Target deve suportar a interface ISelection';
  SValueAndPresenterClassNil = 'IMenuExposer: ValueClass or PresenterClass can''t be nil';
  SMemberNotFound = EOL2 + 'Error: Member ''%s'' not found in ''%s'' Class.' +
    EOL + '- Tip: Check InitInstance method of class.' +
    EOL + '- Tip: Check Execute method of mapping.' +
    EOL + '- Tip: Check MemberName charcase Uppercase/Lowercase.';
  SItemCannotBeInserted = 'Item cannot be inserted';
   
implementation

end.







