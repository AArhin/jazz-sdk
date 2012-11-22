unit PersonBO;

interface

uses
  JazzValueTypeIntf,
  JazzValueType;

type
  IPerson = interface(IObjectType)
    ['{8B85C5F4-79A1-4314-BBA4-5E1B5FFE7B64}']
    function GetName: string;
    procedure SetName(const Value: string);

    property Name: string read GetName write SetName;
  end;

  TPerson = class(TObjectType, IPerson)
  private
    FName: IStringType;
    function GetName: string;
    procedure SetName(const Value: string);
  protected
    procedure InitInstance; override;
  end;

implementation

{ TPerson }

function TPerson.GetName: string;
begin
  Result := FName.Value;
end;

procedure TPerson.InitInstance;
begin
  inherited;
  AddMember(FName, 'Name', TStringType);
end;

procedure TPerson.SetName(const Value: string);
begin
  FName.Value := Value;
end;

initialization
  RegisterType(IPerson, 'TPerson', TPerson);

end.

