unit JazzDBXFirebirdMechanism;


interface

uses
  DB,
{$WARN UNIT_DEPRECATED OFF}
  SqlExpr,
{$WARN UNIT_DEPRECATED ON}
  JazzDatabaseMechanismIntf,
  JazzDataFactory,
  JazzDBXMechanism,
  JazzMappingIntf,
  JazzValueTypeIntf;

type
  TDBXFirebirdMechanism = class(TDBXMechanism)
  protected
    function DriverClass: TDatabaseDriverClass; override;
  end;

  TDBXFirebidDriver = class(TDBXDriver)
  protected
    function GetFieldType(const MemberType: TItemType): TFieldType; override;
    function GetFieldSchema(const FieldSchemaList: IObjectMeta): boolean; override;
    function GetPrimaryKeysSchema(const PrimaryKeysSchema: IIndexMeta): boolean; override;
    function GetForeignKeysSchema(const ForeignKeysSchema: IIndexMeta): boolean; override;
    function GetIndexesSchema(const IndexesSchema: IIndexMeta): boolean; override;
  public
    constructor Create(const Mechanism: IInterface); override;
  end;

  TFirebirdGenerator = class(TDataGenerator)
  protected
    function Next(const Member: IMemberType): boolean; override;
  public
    class function GetContext: TGeneratorContext; override;
  end;

implementation

uses
  JazzFirebirdConsts;

function TDBXFirebidDriver.GetFieldType(const MemberType: TItemType): TFieldType;
begin
  if MemberType in [itDate, itDateTime, itTimeStamp] then
    Result:= ftTimeStamp
  else
    Result:= inherited GetFieldType(MemberType);
end;

{ TDBXFirebirdMechanism }

function TDBXFirebirdMechanism.DriverClass: TDatabaseDriverClass;
begin
  Result:= TDBXFirebidDriver;
end;

function TDBXFirebidDriver.GetFieldSchema(const FieldSchemaList: IObjectMeta): boolean;
begin
  // TODO: Populate Mapping - SQLFieldsSchema
  Result:= False;
end;

function TDBXFirebidDriver.GetForeignKeysSchema(const ForeignKeysSchema: IIndexMeta): boolean;
begin
  // TODO: Populate Mapping - SQLForeignKeys
  Result:= False;
end;

function TDBXFirebidDriver.GetIndexesSchema(const IndexesSchema: IIndexMeta): boolean;
begin
  // TODO: Populate Mapping = SQLIndexesSchema
  Result:= False;
end;

function TDBXFirebidDriver.GetPrimaryKeysSchema(const PrimaryKeysSchema: IIndexMeta): boolean;
begin
  // TODO: Populate Mapping = SQLPrimaryKeysSchema
  Result:= False;
end;

constructor TDBXFirebidDriver.Create(const Mechanism: IInterface);
begin
  inherited;
  SetFirebirdColumnsType(Self);  
end;

{ TFirebirdGenerator }

class function TFirebirdGenerator.GetContext: TGeneratorContext;
begin
  Result:= gcClass;
end;

function TFirebirdGenerator.Next(const Member: IMemberType): boolean;
begin
// TODO: firebird generator 
// name -> generator name
// name -> procedure name
// mechanism -> connection
// execute Command -> stored procedure
// get result
  Result:= False;
end;

end.

