unit DatabaseSchemaClassesIntf;

interface

uses
  Classes,
  JazzValueTypeIntf;

type
  IDatabaseSchemaType = interface(IObjectType)
    ['{E820404F-144D-4FBC-91E5-9B0D3D9957CF}']
    function GetSchema: TStrings;
    procedure SetSchema(const Value: TStrings);
    property Schema: TStrings read GetSchema write SetSchema;
  end;

implementation

end.

