unit JazzSQLEngine;

interface

uses
  Classes,
  JazzPersisterIntf,
  JazzDatabaseMechanismIntf,
  JazzMappingIntf,
  JazzConsts,
  JazzValueTypeIntf,
  JazzCriteriaIntf,
  JazzDataSetIntf;

type
  ISQLEngine = interface(IInterface)
    ['{764E06CE-DDE9-471E-87EC-3240DF69C617}']
    function GetCriteria: ICriteria;
    function GetFieldList: TStrings;
    function GetMeta: IObjectMeta;
    function GetMechanism: IDatabaseMechanism;
    function GetQuery: IObjectQuery;
    function GetSQL: string;
    function GetSQLWhere: string;

    property Criteria: ICriteria read GetCriteria;
    property Mechanism: IDatabaseMechanism read GetMechanism;
    property Meta: IObjectMeta read GetMeta;
    property Query: IObjectQuery read GetQuery;
    property SQL: string read GetSQL;
    property FieldList: TStrings read GetFieldList;
  end;

  ISQLSelectStatement = interface(ISQLEngine)
    ['{BC6FCEAC-819E-4B29-BF5C-C7F60DB480F5}']
    function GetSQLOrderBy: string;
    function GetSQLSelectFrom: string;
  end;

  ISQLObjectStatement = interface(ISQLEngine)
    ['{2A4452AB-B232-4A99-A8D3-03CA8F224F6C}']
    function GetSQLWhere: string;
    function GeTObjectType: IObjectType;
    property ValueObject: IObjectType read GeTObjectType;
  end;

  ISQLInsertStatement = interface(ISQLObjectStatement)
    ['{762C9E0E-A03E-4415-B82D-C7D8CD9AB0FC}']
    function GetSQL: string;
  end;

  ISQLUpdateStatement = interface(ISQLObjectStatement)
    ['{87BC4C50-1FF4-40C9-B363-B4A732872E0F}']
    function GetSQL: string;
  end;

  ISQLDeleteStatement = interface(ISQLObjectStatement)
    ['{CD01527F-5142-429E-ABFA-7A86E95048E2}']
    function GetSQL: string;
  end;

  ISQLSelectCountStatement = interface(ISQLSelectStatement)
    ['{DEE8B458-9C5E-4E71-BF8D-24C451521ABF}']
    function GetSQLOrderBy: string;
    function GetSQLSelectFrom: string;
  end;

  ISQLDriver = interface(IDatabaseDriver)
    ['{BCE07AC2-9504-4805-A13D-D57774D7FFDA}']
  end;

  TSQLEngine = class(TInterfacedObject, ISQLEngine)
  private
    FMechanism: Pointer;
    FQuery: Pointer;
    FMeta: Pointer;
    FUseFullPath: boolean;
    FFullPathChecked: boolean;
    FFieldList: TStrings;
  protected
    function Driver: ISQLDriver;
    function GetFieldList: TStrings;
    function GetCriteria: ICriteria;
    function GetMechanism: IDatabaseMechanism;
    function GetMeta: IObjectMeta;
    function GetQuery: IObjectQuery;
    function GetJoinFields: string; virtual; abstract;
    function GetJoinClause: string; virtual; abstract;

    function GetSQL: string; virtual; abstract;
    function GetSQLWhere: string; virtual;
    function UseFullPath: boolean;

    property Criteria: ICriteria read GetCriteria;
    property Mechanism: IDatabaseMechanism read GetMechanism;
    property Meta: IObjectMeta read GetMeta;
    property Query: IObjectQuery read GetQuery;
    property SQL: string read GetSQL;
    property FieldList: TStrings read GetFieldList;
  public
    constructor Create(const Mechanism: IDatabaseMechanism; const Query: IObjectQuery; const Meta: IObjectMeta); virtual;
    destructor Destroy; override;
  end;

  TSQLSelectStatement = class(TSQLEngine, ISQLSelectStatement)
  private
    procedure AddToFieldList(Name: string);
  protected
    function GetJoinFields: string; override;
    function GetJoinClause: string; override;
    function GetDistinct: string;

    function GetSQL: string; override;
    function GetSQLSelectFrom: string; virtual;
    function GetSQLOrderBy: string; virtual;
  end;

  TSQLSelectCountStatement = class(TSQLSelectStatement, ISQLSelectCountStatement)
  protected
    function GetSQLOrderBy: string; override;
    function GetSQLSelectFrom: string; override;
  end;

  TSQLSelectObjectStatement = class(TSQLEngine, ISQLObjectStatement)
  private
    FObject: Pointer;
  protected
    function GetJoinFields: string; override;
    function GetJoinClause: string; override;
    function GetSQLWhere: string; override;
    function GetObjectType: IObjectType;
    property ValueObject: IObjectType read GeTObjectType;
  public
    constructor Create(const Mechanism: IDatabaseMechanism; const ValueObject: IObjectType; const Meta: IObjectMeta); reintroduce; virtual;
  end;

  TSQLInsertStatement = class(TSQLSelectObjectStatement, ISQLInsertStatement)
  protected
    function GetSQL: string; override;
  end;

  TSQLUpdateStatement = class(TSQLSelectObjectStatement, ISQLUpdateStatement)
  protected
    function GetSQL: string; override;
  end;

  TSQLDeleteStatement = class(TSQLSelectObjectStatement, ISQLDeleteStatement)
  protected
    function GetSQL: string; override;
  end;

  TSQLDriver = class(TDatabaseDriver, ISQLDriver)
  protected
    function DatabaseMechanism: IDatabaseMechanism;
    function CreateTable(const TableSchema: IObjectMeta): string; override;
    function DropTable(const TableSchema: IObjectMeta): string; override;

    function AddField(const FieldSchema: IMemberMeta; const Prefix: string = EmptyStr): string; override;
    function DropField(const FieldSchema: IMemberMeta; const Prefix: string = EmptyStr): string; override;
    function CopyFromTmpField(const FieldSchema: IMemberMeta; const Prefix: string): string; override;
    function CopyToTmpField(const FieldSchema: IMemberMeta; const Prefix: string): string; override;

    function AddPrimaryKey(const TableSchema: IObjectMeta): string; override;
    function DropPrimaryKey(const TableSchema: IObjectMeta): string; override;

    function AddForeignKey(const RelationshipSchema: IRelationshipMeta): string; override;
    function DropForeignKey(const RelationshipSchema: IRelationshipMeta): string; override;

    function AddIndex(const IndexesSchema: IIndexMeta): string; override;
    function DropIndex(const IndexesSchema: IIndexMeta): string; override;

    function InsertStatement(const AObject: IObjectType; const Meta: IObjectMeta): string; override;
    function DeleteStatement(const AObject: IObjectType; const Meta: IObjectMeta): string; override;
    function UpdateStatement(const AObject: IObjectType; const Meta: IObjectMeta): string; override;
    function SelectStatement(const Query: IObjectQuery; const Meta: IObjectMeta;
      FieldList: TStrings = nil): string; override;
    function SelectCountStatement(const Query: IObjectQuery; const Meta: IObjectMeta): string; override;
  end;

implementation

uses
  Math,
  SysUtils,
  JazzMechanismIntf,
  JazzPersisterConsts,
  JazzSessionIntf,
  JazzUtils;

const
  Separator = ', ';

  { TSQLEngine }

constructor TSQLEngine.Create(const Mechanism: IDatabaseMechanism;
  const Query: IObjectQuery; const Meta: IObjectMeta);
begin
  inherited Create;
  FMechanism:= Pointer(Mechanism);
  FQuery:= Pointer(Query);
  FMeta:= Pointer(Meta);
  FFieldList:= TStringList.Create;
end;

destructor TSQLEngine.Destroy;
begin
  FFieldList.Free;
  inherited;
end;

function TSQLEngine.Driver: ISQLDriver;
begin
  Result:= (IMechanism(FMechanism) as IDatabaseMechanism).DatabaseDriver as
    ISQLDriver;
end;

function TSQLEngine.GetCriteria: ICriteria;
begin
  Result:= Query.Criteria;
end;

function TSQLEngine.GetFieldList: TStrings;
begin
  Result:= FFieldList;
end;

function TSQLEngine.GetMechanism: IDatabaseMechanism;
begin
  Result:= IDatabaseMechanism(FMechanism);
end;

function TSQLEngine.GetMeta: IObjectMeta;
begin
  Result:= IObjectMeta(FMeta);
end;

function TSQLEngine.GetQuery: IObjectQuery;
begin
  Result:= IObjectQuery(FQuery);
end;

function TSQLEngine.GetSQLWhere: string;
var
  I: Integer;
  LAndOr: string;
  LColName: string;
  LCriteriaCount: Integer;
  LCriterion: ICriterion;
  LMemberMeta: IMemberMeta;
  LNot: string;
  LParamName: string;
  LGroupingCount: Integer;
  LGroupingStarted: boolean;
begin
  Result:= EmptyStr;

  if Criteria = nil then Exit;
  LCriteriaCount:= 0;
  LGroupingCount:= 0;
  LGroupingStarted:= False;

  if (Criteria.Count > 0) then
  begin
    for I:= 0 to (Criteria.Count - 1) do
    begin
      LCriterion:= Criteria[I];
      if (LCriterion.CriterionType in [ctOrderBy, ctOrderByDesc]) then Continue;

      LAndOr:= EmptyStr;
      LNot:= EmptyStr;
      LColName:= EmptyStr;
      LParamName:= EmptyStr;

      if (LCriterion.CriterionType in [Low(WhereCriteria)..High(WhereCriteria)]) then
      begin
        LMemberMeta:= Meta.FindMember(LCriterion.MemberName);
        if (LMemberMeta = nil) then Continue;
        LColName:= Driver.GetColumnName(LMemberMeta, UseFullPath, True);
        if LCriterion.CriterionType <> ctIsNull then
          LParamName:= ':' + Format('P%d_%s', [LCriteriaCount,
            Driver.GetColumnName(LMemberMeta, False, False)]);
      end;

      if Result <> EmptyStr then
      begin
        if LCriterion.IsOr then
          LAndOr:= 'OR'
        else
          LAndOr:= 'AND';
      end
      else
        LAndOr:= 'WHERE';

      if LCriterion.IsNot then
        LNot:= 'NOT'
      else
        LNot:= EmptyStr;

      case LCriterion.CriterionType of
        ctStartGrouping:
          begin
            Inc(LGroupingCount);
            LGroupingStarted:= True;
            Result:= Format('%s %s %s %s', [
              Result,
                LAndOr,
                LNot,
                CriteriaOperators[LCriterion.CriterionType]]);

            Continue
          end;
        ctEndGrouping:
          begin
            if LGroupingCount > 0 then
            begin
              Dec(LGroupingCount);
              LGroupingStarted:= False;
              Result:= Result + CriteriaOperators[LCriterion.CriterionType];
            end;
            Continue;
          end;
      end;

      if LGroupingStarted then
      begin
        LAndOr:= EmptyStr;
        LGroupingStarted:= False;
      end;

      Result:= Format('%s %s %s (%s %s %s)', [
        Result,
          LAndOr,
          LNot,
          LColName,
          CriteriaOperators[LCriterion.CriterionType],
          LParamName
          ]);

      if LCriterion.CriterionType <> ctIsNull then Inc(LCriteriaCount);
    end;
  end;

  if (Result <> EmptyStr) and (LGroupingCount > 0) then
  begin
    while LGroupingCount > 0 do
    begin
      Dec(LGroupingCount);
      Result:= Result + ')';
    end;
  end;
end;

function TSQLEngine.UseFullPath: boolean;
var
  I: Integer;
begin
  if not FFullPathChecked then
  begin
    if Meta.Super <> nil then
      FUseFullPath:= True
    else
    begin
      for I:= 0 to Meta.Relationships.Count - 1 do
      begin
        with (Meta.Relationships[I] as IRelationshipMeta) do
        begin
          if RelationshipType = rtOneToOne then
          begin
            FUseFullPath:= True;
            Break;
          end;
        end;
      end;
    end;
    FFullPathChecked:= True;
  end;

  Result:= FUseFullPath;
end;

{ TSQLSelectStatement }

function TSQLSelectStatement.GetDistinct: string;
begin
  if (Criteria <> nil) and (Criteria.Distinct) then
    Result:= 'DISTINCT'
  else
    Result:= EmptyStr;
end;

function TSQLSelectStatement.GetJoinClause: string;
const
  LJoin = ' %s ON %s ';
  LJoinItem = '(%s = %s) AND';
  JoinTypeAsString: array[TJoinType] of string = (' LEFT JOIN ',
    ' LEFT OUTER JOIN ', ' RIGHT OUTER JOIN ', ' FULL OUTER JOIN ');
var
  I: Integer;
  J: Integer;
  LJoinStr: string;
  LJoinClause: string;
  LObjectMeta: IObjectMeta;
  LRelationship: IRelationshipMeta;
  LFromMember: IMemberMeta;
  LToMember: IMemberMeta;
begin
  Result:= EmptyStr;
  if Meta.Relationships.Count > 0 then
  begin
    for I:= 0 to Meta.Relationships.Count - 1 do
    begin
      LRelationship:= Meta.Relationships[I] as IRelationshipMeta;
      if (LRelationship.RelationshipType = rtInheritance) or
        ((LRelationship.RelationshipType = rtOneToOne) and
          LRelationship.CascadeLoad) then
      begin
        LJoinStr:= EmptyStr;

        LObjectMeta:= Mechanism.Mapping.Find(LRelationship.ToClassName);

        for J:= 0 to LRelationship.FromFields.Count - 1 do
        begin
          LFromMember:= Meta.FindMember(LRelationship.FromMembers[J]);
          LToMember:= LObjectMeta.FindMember(LRelationship.ToMembers[J]);

          LJoinStr:= LJoinStr + Format(LJoinItem, [
            Driver.GetColumnName(LFromMember),
              Driver.GetColumnName(LToMember)
              ]);
        end;

        LJoinClause:= LJoinClause + JoinTypeAsString[LRelationship.JoinType] +
          Format(LJoin, [Driver.GetTableName(LObjectMeta),
          RemoveChars(LJoinStr, 4)]);
      end;
    end;
  end;
  Result:= LJoinClause;
end;

function TSQLSelectStatement.GetJoinFields: string;
var
  I: Integer;
  J: Integer;
  LJoinFields: string;
  LObjectMeta: IObjectMeta;
  LMemberMeta: IMemberMeta;
  LRelationship: IRelationshipMeta;
  LName: string;
  LPrefix: string;
begin
  Result:= EmptyStr;

  if Meta.Relationships.Count > 0 then
  begin
    for I:= 0 to Meta.Relationships.Count - 1 do
    begin
      LRelationship:= Meta.Relationships[I] as IRelationshipMeta;
      if (LRelationship.RelationshipType = rtInheritance) or
        ((LRelationship.RelationshipType = rtOneToOne) and
          LRelationship.CascadeLoad) then
      begin
        LObjectMeta:= Mechanism.Mapping.Find(LRelationship.ToClassName);

        if LRelationship.MasterAttribute <> EmptyStr then
          LPrefix:= LRelationship.MasterAttribute + '.'
        else
          LPrefix:= EmptyStr;
          
        for J:= 0 to LObjectMeta.Count - 1 do
        begin
          LMemberMeta:= LObjectMeta.Items[J] as IMemberMeta;
          if (LMemberMeta.IsOID and (LRelationship.RelationshipType = rtInheritance)) or
            LMemberMeta.LazyLoad then Continue;

          LName:= Driver.GetColumnName(LMemberMeta, UseFullPath);
          LJoinFields:= LJoinFields + LName + Separator;
          AddToFieldList(LPrefix + LMemberMeta.MemberName);
        end;
      end;
    end;
  end;

  if LJoinFields <> EmptyStr then Result:= LJoinFields;
end;

function TSQLSelectStatement.GetSQL: string;
var
  LSQL: TStrings;
  S: string;
begin
  FFieldList.Clear;
  LSQL:= TStringList.Create;
  try
    LSQL.Add(GetSQLSelectFrom); (* SELECT %s FROM %s *)

    S:= GetSQLWhere; (* WHERE %s JOIN %s *)
    if S <> EmptyStr then LSQL.Add(S);

    S:= GetSQLOrderBy; (* ORDER BY %s *)
    if S <> EmptyStr then LSQL.Add(S);

    Result:= LSQL.Text;
  finally
    LSQL.Free;
  end;
end;

function TSQLSelectStatement.GetSQLOrderBy: string;
var
  I: Integer;
  LColName: string;
begin
  Result:= EmptyStr;
  if Criteria = nil then Exit;

  if (Criteria.Count > 0) then
  begin
    for I:= 0 to (Criteria.Count - 1) do
    begin
      if Criteria[I].CriterionType in [ctOrderBy, ctOrderByDesc] then
      begin
        LColName:= Driver.GetColumnName(Meta, Criteria[I].MemberName,
          UseFullPath);
        if Result <> EmptyStr then Result:= Result + Separator;

        if Criteria[I].CriterionType = ctOrderByDesc then
          Result:= Result + LColName + ' DESC'
        else
          Result:= Result + LColName + ' ASC';
      end;
    end;

    if Result <> EmptyStr then Result:= ' ORDER BY ' + Result;
  end;
end;

procedure TSQLSelectStatement.AddToFieldList(Name: string);
begin
  FFieldList.Add(Name);
end;

function TSQLSelectStatement.GetSQLSelectFrom: string;
const
  LSQL = 'SELECT %s %s FROM %S';
var
  I: Integer;
  LField: string;
  LFields: string;
  LMemberMeta: IMemberMeta;
begin
  LFields:= EmptyStr;
  if (Criteria <> nil) and (Criteria.Proxy.Count > 0) then
  begin
    for I:= 0 to Criteria.Proxy.Count - 1 do
    begin
      LMemberMeta:= Meta.GetMember(Criteria.Proxy[I]);
      LField:= Driver.GetColumnName(LMemberMeta, UseFullPath);
      LFields:= LFields + LField + Separator;
      AddToFieldList(LMemberMeta.MemberName);
    end
  end
  else
  begin
    for I:= 0 to Meta.Count - 1 do
    begin
      LMemberMeta:= Meta.Items[I] as IMemberMeta;
      if LMemberMeta.LazyLoad then Continue;
      LField:= Driver.GetColumnName(LMemberMeta, UseFullPath);
      LFields:= LFields + LField + Separator;
      AddToFieldList(LMemberMeta.MemberName);
    end;
  end;

  Result:= Format(LSQL, [GetDistinct, RemoveChars(LFields + GetJoinFields, 2),
    Driver.GetTableName(Meta) + GetJoinClause]);
end;

{ TSQLSelectCountStatement }

function TSQLSelectCountStatement.GetSQLOrderBy: string;
begin
  Result:= EmptyStr;
end;

function TSQLSelectCountStatement.GetSQLSelectFrom: string;
const
  LSQL = 'SELECT %s COUNT(*) FROM %S';
begin
  Result:= Format(LSQL, [GetDistinct, Driver.GetTableName(Meta)]);
end;

{ TSQLSelectObjectStatement }

constructor TSQLSelectObjectStatement.Create(const Mechanism:
  IDatabaseMechanism; const ValueObject: IObjectType; const Meta: IObjectMeta);
begin
  inherited Create(Mechanism, nil, Meta);
  FObject:= Pointer(ValueObject);
end;

function TSQLSelectObjectStatement.GetSQLWhere: string;
const
  LSQL = '%s (%s = :%s)';
var
  I: Integer;
  LColName: string;
  LParamIndex: Integer;
  LParamName: string;
  LItem: IMemberMeta;
begin
  Result:= '';
  LParamIndex:= 0;

  for I:= 0 to Meta.OID.Count - 1 do
  begin
    LItem:= Meta.OID[I];
    LColName:= Driver.GetColumnName(Meta, LItem.MemberName, False);
    LParamName:= Format('P%d_%s', [LParamIndex, LItem.ColumnName]);
    if Result = '' then
      Result:= Format(LSQL, ['WHERE', LColName, LParamName])
    else
      Result:= Result + Format(LSQL, [' AND', LColName, LParamName]);
    Inc(LParamIndex);
  end;
end;

function TSQLSelectObjectStatement.GetObjectType: IObjectType;
begin
  Result:= IObjectType(FObject);
end;

function TSQLSelectObjectStatement.GetJoinClause: string;
begin
  Result:= EmptyStr;
end;

function TSQLSelectObjectStatement.GetJoinFields: string;
begin
  Result:= EmptyStr;
end;

{ TSQLInsertStatement }

function TSQLInsertStatement.GetSQL: string;
var
  I: Integer;
  LSQL: TStrings;
  LFields: TStrings;
  LValues: TStrings;
  LMemberMeta: IMemberMeta;
begin
  LSQL:= TStringList.Create;
  LFields:= TStringList.Create;
  LValues:= TStringList.Create;
  try
    LSQL.Add('INSERT INTO ' + Driver.GetTableName(Meta) + '(');

    for I:= 0 to Meta.Count - 1 do
    begin
      LMemberMeta:= Meta.Items[I] as IMemberMeta;
      if ValueObject.Member[LMemberMeta.MemberName].IsNull then Continue;
      LFields.Add(Driver.GetColumnName(LMemberMeta, False) + Separator);
      LValues.Add(':' + Format('P%d_%s', [I, LMemberMeta.ColumnName]) +
        Separator);
    end;

    LSQL.Add(RemoveChars(LFields.Text, 2) + ')');
    LSQL.Add(' VALUES (' + RemoveChars(LValues.Text, 2) + ')');
    Result:= LSQL.Text;
  finally
    LSQL.Free;
    LFields.Free;
    LValues.Free;
  end;
end;

{ TSQLUpdateStatement }

function TSQLUpdateStatement.GetSQL: string;
var
  I: Integer;
  LParamIndex: Integer;
  LColName: string;
  LFields: TStrings;
  LMember: IMemberType;
  LMemberMeta: IMemberMeta;
  LParamName: string;
  LSQL: TStrings;

  function CanUpdateMember: boolean;
  begin
    Result:= not LMemberMeta.IsOID and (LMember as IMemberState).State.Modified;
  end;
begin
  LSQL:= TStringList.Create;
  LFields:= TStringList.Create;
  try
    LSQL.Add('UPDATE ' + Driver.GetTableName(Meta) + ' SET ');
    LSQL.Add(' ');

    LParamIndex:= Meta.OID.Count; 
    for I:= 0 to Meta.Count - 1 do
    begin
      LMemberMeta:= Meta.Items[I] as IMemberMeta;
      LMember:= ValueObject.Member[LMemberMeta.MemberName];
      if CanUpdateMember then
        with Driver, LMemberMeta do
        begin
          LColName:= FormatIdentifier(ColumnName);
          LParamName:= Format('P%d_%s', [LParamIndex, ColumnName]);
          LFields.Add(Format(' %s = :%s, ', [LColName, LParamName]));
          Inc(LParamIndex);
        end;
    end;

    if LFields.Count > 0 then
    begin
      LSQL.Add(RemoveChars(LFields.Text, 2));
      LSQL.Add(' ');
      LSQL.Add(GetSQLWhere);
      Result:= LSQL.Text;
    end
    else
      Result:= EmptyStr;
  finally
    LSQL.Free;
    LFields.Free;
  end;
end;

{ TSQLDeleteStatement }

function TSQLDeleteStatement.GetSQL: string;
const
  LSQL = 'DELETE FROM %s %s';
begin
  Result:= Format(LSQL, [Driver.GetTableName(Meta), GetSQLWhere]);
end;

{ TSQLDriver }

function TSQLDriver.AddField(const FieldSchema: IMemberMeta;
  const Prefix: string): string;
const
  LSQL = 'ALTER TABLE %s ADD %s %s %s;'; //TABLENAME, FIELDNAME, FIELDTYPE;
var
  LNotNull: string;
begin
  if FieldSchema.Required then
    LNotNULL:= ' NOT NULL'
  else
    LNotNull:= EmptyStr;

  Result:= Format(LSQL, [
    GetTableName(FieldSchema.Owner as IObjectMeta),
      GetColumnName(FieldSchema),
      GetColumnTypeFor(FieldSchema.ColumnType),
      LNotNull
      ]);
  if Pos(WildChar, Result) > 0 then Result:= Format(Result, [FieldSchema.Size]);
end;

function TSQLDriver.AddForeignKey(const RelationshipSchema: IRelationshipMeta): string;
const
  LSQL = 'ALTER TABLE %s ADD CONSTRAINT %s FOREIGN  KEY (%s) REFERENCES %s (%s);'; 
begin
  with RelationshipSchema do
  begin
    Result:= Format(LSQL, [FromTable, ForeignKeyName, FromFields.CommaText,
      ToTable, ToFields.CommaText]);
  end;
end;

function TSQLDriver.AddIndex(const IndexesSchema: IIndexMeta): string;
begin
  // TODO: AddIndex
end;

function TSQLDriver.AddPrimaryKey(const TableSchema: IObjectMeta): string;
const
  LSQL = 'ALTER TABLE %s ADD CONSTRAINT %s PRIMARY KEY (%s);';
var
  I: Integer;
  LFields: string;
  LTable: string;
  LConstraint: string;
begin
  LTable:= GetTableName(TableSchema);
  LConstraint:= FormatIdentifier(Format('PK_%s', [GetTableName(TableSchema,
    False)]));

  if not TableSchema.OID.IsMapped then
  begin
    Result:= EmptyStr;
    Exit;
  end;

  for I:= 0 to TableSchema.OID.Count - 1 do
    LFields:= LFields + FormatIdentifier(TableSchema.OID[I].ColumnName) +
      Separator;
  LFields:= RemoveChars(LFields, 2);

  Result:= Format(LSQL, [LTable, LConstraint, LFields]);
end;

function TSQLDriver.CopyFromTmpField(const FieldSchema: IMemberMeta;
  const Prefix: string): string;
begin
  Result:= 'UPDATE %s SET %s = %s;';
  Result:= Format(Result, [
    GetTableName(FieldSchema.Owner as IObjectMeta),
      GetColumnName(FieldSchema),
      FormatIdentifier(Prefix + GetColumnName(FieldSchema, True, False))
      ]);
end;

function TSQLDriver.CopyToTmpField(const FieldSchema: IMemberMeta;
  const Prefix: string): string;
begin
  Result:= 'UPDATE %s SET %s = %s;';
  Result:= Format(Result, [
    GetTableName(FieldSchema.Owner as IObjectMeta),
      FormatIdentifier(Prefix + GetColumnName(FieldSchema, True, False)),
      GetColumnName(FieldSchema)
      ]);
end;

function TSQLDriver.CreateTable(const TableSchema: IObjectMeta): string;
const
  LSQL = 'CREATE TABLE %s (%s);';
var
  I: Integer;
  LFieldSchema: IMemberMeta;
  LField: string;
  LFieldList: string;
begin
  for I:= 0 to TableSchema.Count - 1 do
  begin
    LFieldSchema:= TableSchema.Items[I] as IMemberMeta;
    LField:= Format('%s %s', [FormatIdentifier(LFieldSchema.ColumnName),
      GetColumnTypeFor(LFieldSchema.ColumnType)]);

    if Pos(WildChar, LField) > 0 then
      LField:= Format(LField, [LFieldSchema.Size]);

    if LFieldSchema.Required or LFieldSchema.IsOID then
      LField:= LField + ' NOT NULL';

    if (I = TableSchema.Count - 1) then
      LFieldList:= LFieldList + LField
    else
      LFieldList:= LFieldList + LField + Separator;
  end;

  Result:= Format(LSQL, [GetTableName(TableSchema), LFieldList]);
end;

function TSQLDriver.DatabaseMechanism: IDatabaseMechanism;
begin
  Result:= Mechanism as IDatabaseMechanism
end;

function TSQLDriver.DeleteStatement(const AObject: IObjectType;
  const Meta: IObjectMeta): string;
var
  LSQL: ISQLDeleteStatement;
begin
  LSQL:= TSQLDeleteStatement.Create(DatabaseMechanism, AObject, Meta);
  Result:= LSQL.SQL;
end;

function TSQLDriver.DropField(const FieldSchema: IMemberMeta;
  const Prefix: string): string;
begin
  Result:= 'ALTER TABLE %s DROP %s;'; // TABLENAME, FIELDNAME;
  Result:= Format(Result, [
    GetTableName(FieldSchema.Owner as IObjectMeta),
      FormatIdentifier(Prefix + GetColumnName(FieldSchema, True, False))
      ]);
end;

function TSQLDriver.DropForeignKey(const RelationshipSchema: IRelationshipMeta): string;
begin
  // TODO: DropForeignKey
end;

function TSQLDriver.DropIndex(const IndexesSchema: IIndexMeta): string;
begin
  // TODO: DropIndex
end;

function TSQLDriver.DropPrimaryKey(const TableSchema: IObjectMeta): string;
begin
  Result:= 'ALTER TABLE %s DROP CONSTRAINT %s;'; // TABLENAME, IDXNAME
  Result:= Format(Result, [GetTableName(TableSchema),
    FormatIdentifier(Format('PK_%s', [GetTableName(TableSchema, False)]))]);
end;

function TSQLDriver.DropTable(const TableSchema: IObjectMeta): string;
begin
  Result:= Format('DROP TABLE %s;', [TableSchema.TableName]);
end;

function TSQLDriver.InsertStatement(const AObject: IObjectType;
  const Meta: IObjectMeta): string;
var
  LSQL: ISQLInsertStatement;
begin
  LSQL:= TSQLInsertStatement.Create(Mechanism as IDatabaseMechanism, AObject,
    Meta);
  Result:= LSQL.SQL;
end;

function TSQLDriver.SelectCountStatement(const Query: IObjectQuery;
  const Meta: IObjectMeta): string;
var
  LSQL: ISQLSelectCountStatement;
begin
  LSQL:= TSQLSelectCountStatement.Create(DatabaseMechanism, Query, Meta);
  Result:= LSQL.SQL;
end;

function TSQLDriver.SelectStatement(const Query: IObjectQuery; const Meta:
  IObjectMeta; FieldList: TStrings): string;
var
  LSQL: ISQLSelectStatement;
begin
  LSQL:= TSQLSelectStatement.Create(DatabaseMechanism, Query, Meta);
  Result:= LSQL.SQL;
  if FieldList <> nil then FieldList.AddStrings(LSQL.FieldList);
end;

function TSQLDriver.UpdateStatement(const AObject: IObjectType;
  const Meta: IObjectMeta): string;
var
  LSQL: ISQLUpdateStatement;
begin
  LSQL:= TSQLUpdateStatement.Create(Mechanism as IDatabaseMechanism, AObject, Meta);
  Result:= LSQL.SQL;
end;

end.

