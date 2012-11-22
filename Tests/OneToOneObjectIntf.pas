unit OneToOneObjectIntf;

interface

uses JazzValueTypeIntf;

type
  IOneToOneObject = interface(IObjectType)
  ['{FC3DDE63-77DB-43B2-93AA-5BC999D225A2}']
    function GetID: String;
    function GetIDOwner: String;
    procedure SetID(const Value: String);
    procedure SetIDOwner(const Value: String);
    
    property ID: String read GetID write SetID;
    property IDOwner: String read GetIDOwner write SetIDOwner;
  end;

implementation

end.
