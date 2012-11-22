unit DatabaseSchemaClasses;

interface

uses
  JazzValueType,
  JazzValueTypeIntf,
  DatabaseSchemaClassesIntf,
  Classes;

type
  TDatabaseSchemaType = class(TObjectType, IDatabaseSchemaType)
  private
    FSchema: IMemoType;
  protected
    function GetSchema: TStrings;
    procedure SetSchema(const Value: TStrings);
  public
    constructor Create(const AOwner: IValueType = nil); override;
    property Schema: TStrings read GetSchema write SetSchema;
  end;

implementation

{ TDatabaseSchemaType }

constructor TDatabaseSchemaType.Create(const AOwner: IValueType);
begin
  inherited;
  AddMember(FSchema, 'Schema', TMemoType);
end;

function TDatabaseSchemaType.GetSchema: TStrings;
begin
  Result:= FSchema.Value;
end;

procedure TDatabaseSchemaType.SetSchema(const Value: TStrings);
begin
  FSchema.Value:= Value;
end;

end.
