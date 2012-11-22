unit OneToManyObjectIntf;

interface

uses JazzValueTypeIntf;

type
  IOneToManyObject = interface(IObjectType)
  ['{5FF51457-1855-4D1F-AACF-66B08339A637}']
    function GetID: String;
    function GetIDOwner: String;
    procedure SetID(const Value: String);
    procedure SetIDOwner(const Value: String);
    
    property ID: String read GetID write SetID;
    property IDOwner: String read GetIDOwner write SetIDOwner;
  end;

implementation

end.
