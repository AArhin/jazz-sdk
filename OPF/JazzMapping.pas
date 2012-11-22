unit JazzMapping;

interface

uses
  Classes,
  DB,
  JazzClasses,
  JazzMappingIntf,
  JazzDatabaseMechanismIntf;

type
  TMemberMeta = class(TCustomObject, IMemberMeta)
  private
    FColumnName: string;
    FColumnType: TItemType;
    FFieldType: TFieldType;
    FGeneratorClass: TClass;
    FIsOID: Boolean;
    FLazyLoad: Boolean;
    FMappingType: TMappingType;
    FMemberName: string;
    FPrecision: SmallInt;
    FRequired: Boolean;
    FSize: SmallInt;
  protected
    function GetColumnName: string;
    function GetColumnType: TItemType;
    function GetDriver: IDatabaseDriver;
    function GetFieldType: TFieldType;
    function GetGeneratorClass: TClass;
    function GetIsOID: Boolean;
    function GetLazyLoad: Boolean;
    function GetMappingType: TMappingType;
    function GetMemberName: string;
    function GetPrecision: SmallInt;
    function GetRequired: Boolean;
    function GetSize: SmallInt;
    procedure SetColumnName(const Value: string);
    procedure SetColumnType(const Value: TItemType);
    procedure SetFieldType(const Value: TFieldType);
    procedure SetGeneratorClass(const Value: TClass);
    procedure SetIsOID(const Value: Boolean);
    procedure SetLazyLoad(const Value: Boolean);
    procedure SetMappingType(const Value: TMappingType);
    procedure SetMemberName(const Value: string);
    procedure SetPrecision(const Value: SmallInt);
    procedure SetRequired(const Value: Boolean);
    procedure SetSize(const Value: SmallInt);

    procedure Assign(const Source: IMemberMeta);
    function FullName(const Mapped: Boolean = False): string;
    // TODO: add mapping to stored procedure
    // TODO: add mapping to sub select
    // TODO: add aux aditional mapping
    property ColumnName: string read GetColumnName write SetColumnName;
    property ColumnType: TItemType read GetColumnType write SetColumnType;
    property FieldType: TFieldType read GetFieldType write SetFieldType;
    property GeneratorClass: TClass read GetGeneratorClass write SetGeneratorClass;
    property IsOID: Boolean read GetIsOID write SetIsOID;
    property LazyLoad: Boolean read GetLazyLoad write SetLazyLoad;
    property MappingType: TMappingType read GetMappingType write SetMappingType;
    property MemberName: string read GetMemberName write SetMemberName;
    property Precision: SmallInt read GetPrecision write SetPrecision;
    property Required: Boolean read GetRequired write SetRequired;
    property Size: SmallInt read GetSize write SetSize;
  public
    constructor Create(const AOwner: IInterface); override;
  end;

  TRelationshipMeta = class(TCustomObject, IRelationshipMeta)
  private
    FCascadeDelete: Boolean;
    FCascadeLoad: Boolean;
    FCascadeSave: Boolean;
    FForeignKey: Boolean;
    FForeignKeyName: string;
    FFromClassName: string;
    FFromFields: TStrings;
    FFromMembers: TStrings;
    FFromTable: string;
    FJoinType: TJoinType;
    FMasterAttribute: string;
    FRelationshipType: TRelationshipType;
    FToClassName: string;
    FToFields: TStrings;
    FToMembers: TStrings;
    FToTable: string;
    FUpdated: Boolean;
  protected
    function GetCascadeDelete: Boolean;
    function GetCascadeLoad: Boolean;
    function GetCascadeSave: Boolean;
    function GetForeignKeyName: string;
    function GetFromClassName: string;
    function GetFromFields: TStrings;
    function GetFromMembers: TStrings;
    function GetFromTable: string;
    function GetJoinType: TJoinType;
    function GetMasterAttribute: string;
    function GetRelationshipType: TRelationshipType;
    function GetToClassName: string;
    function GetToFields: TStrings;
    function GetToMembers: TStrings;
    function GetToTable: string;
    function GetUpdated: Boolean;
    procedure SetCascadeDelete(const Value: Boolean);
    procedure SetCascadeLoad(const Value: Boolean);
    procedure SetCascadeSave(const Value: Boolean);
    procedure SetForeignKeyName(const Value: string);
    procedure SetFromClassName(const Value: string);
    procedure SetFromTable(const Value: string);
    procedure SetJoinType(const Value: TJoinType);
    procedure SetMasterAttribute(const Value: string);
    procedure SetRelationshipType(const Value: TRelationshipType);
    procedure SetToClassName(const Value: string);
    procedure SetToTable(const Value: string);
    procedure SetUpdated(const Value: Boolean);

    procedure Assign(const Source: IRelationshipMeta);
    function GetForeignKey: Boolean;
    procedure SetForeignKey(Value: Boolean);

    property RelationshipType: TRelationshipType read GetRelationshipType write SetRelationshipType;
    property JoinType: TJoinType read GetJoinType write SetJoinType;
    property FromClassName: string read GetFromClassName write SetFromClassName;
    property FromMembers: TStrings read GetFromMembers;
    property FromTable: string read GetFromTable write SetFromTable;
    property FromFields: TStrings read GetFromFields;

    property MasterAttribute: string read GetMasterAttribute write SetMasterAttribute;
    property ToClassName: string read GetToClassName write SetToClassName;
    property ToMembers: TStrings read GetToMembers;
    property ToTable: string read GetToTable write SetToTable;
    property ToFields: TStrings read GetToFields;

    property CascadeDelete: Boolean read GetCascadeDelete write SetCascadeDelete;
    property CascadeLoad: Boolean read GetCascadeLoad write SetCascadeLoad;
    property CascadeSave: Boolean read GetCascadeSave write SetCascadeSave;
    property ForeignKey: Boolean read GetForeignKey write SetForeignKey;

    property ForeignKeyName: string read GetForeignKeyName write SetForeignKeyName;
    property Updated: Boolean read GetUpdated write SetUpdated;
  public
    constructor Create(const RelationshipType: TRelationshipType); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TIndexMeta = class(TCustomObject, IIndexMeta)
  private
    FIndexName: string;
    FIndexType: TIndexType;
    FIndexFields: TStrings;
  protected
    function GetIndexName: string;
    function GetIndexType: TIndexType;
    function GetIndexFields: TStrings;
    procedure SetIndexName(const Value: string);
    procedure SetIndexType(const Value: TIndexType);

    procedure Assign(const Source: IIndexMeta);

    property IndexName: string read GetIndexName write SetIndexName;
    property IndexType: TIndexType read GetIndexType write SetIndexType;
    property IndexFields: TStrings read GetIndexFields;
  public
    constructor Create(const AOwner: IInterface; const IndexName: string; const IndexType: TIndexType); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TMemberMetaList = class(TNamedInterfaceList, IMemberMetaList)
  end;

  TOIDMeta = class(TCustomObject, IOIDMeta)
  private
    FItems: IMemberMetaList;
    FItem: IMemberMeta;
    FSingle: boolean;
  protected
    function GetCount: Integer;
    function GetItem: IMemberMeta;
    function GetItems(const Index: Integer): IMemberMeta;
    function GetMember(const Name: string): IMemberMeta;
    function GetMeta(const Name: string): IMemberMeta;
    function GetSingle: boolean;
    procedure SetSingle(const Value: boolean);

    function GetNames(const Separator: string = ', '; const Meta: boolean = False): string;
    procedure Clear;

    function Add(const Item: IMemberMeta): Integer;
    procedure Remove(const Item: IMemberMeta);

    function IsMapped: boolean;
    function IsMemberOID(const Item: IMemberMeta): boolean;
    function IndexOf(const Item: IMemberMeta): Integer;

    property Count: Integer read GetCount;
    property Item: IMemberMeta read GetItem;
    property Items[const Index: Integer]: IMemberMeta read GetItems; default;
    property Member[const Name: string]: IMemberMeta read GetMember;
    property Meta[const Name: string]: IMemberMeta read GetMeta;
    property Single: boolean read GetSingle write SetSingle;
  public
    constructor Create(const AOwner: IInterface = nil); override;
  end;

  TObjectMeta = class(TCustomObject, IObjectMeta)
  private
    FIndexes: IInterfaceList;
    FMembersItems: IMemberMetaList;
    FObjectClassName: string;
    FOID: IOIDMeta;
    FRelationships: IInterfaceList;
    FRelationshipsUpdated: Boolean;
    FSuper: IObjectMeta;
    FTableName: string;
  protected
    function GetIndexes: IInterfaceList;
    function GetItems: IMemberMetaList;
    function GetRelationships: IInterfaceList;

    function GetObjectClassName: string;
    function GetOID: IOIDMeta;
    function GetSuper: IObjectMeta;
    function GetTableName: string;
    procedure GetLazyItems(var LazyItems: TStrings);
    procedure SetObjectClassName(const Value: string);
    procedure SetSuper(const Value: IObjectMeta);
    procedure SetTableName(const Value: string);

    function Add: IMemberMeta; overload;
    function Add(const Member: IMemberMeta): Integer; overload;
    function Add(MemberName, ColumnName: string; ColumnType: TItemType = itString; ColumnSize: Integer = 38): IMemberMeta; overload;
    function GetCount: Integer;
    function FindMember(const Name: string; const LookupSuper: boolean = True; const ColumnName: Boolean = False): IMemberMeta;
    function GetMember(const Name: string; const LookupSuper: boolean = True; const ColumnName: Boolean = False): IMemberMeta;
    function FullName(const Mapped: Boolean = False): string;
    function IndexOf(const Member: IInterface): Integer;

    function UpdateRelationships: Boolean;
    function AddRelationship(const RelationshipType: TRelationshipType = rtOneToMany): IRelationshipMeta;

    property ObjectClassName: string read GetObjectClassName write SetObjectClassName;
    property TableName: string read GetTableName write SetTableName;

    property Count: Integer read GetCount;
    property OID: IOIDMeta read GetOID;
    property Super: IObjectMeta read GetSuper write SetSuper;

    property Items: IMemberMetaList read GetItems;
    property Indexes: IInterfaceList read GetIndexes;
    property Relationships: IInterfaceList read GetRelationships;
  public
    constructor Create(const AOwner: IInterface); override;
    destructor Destroy; override;
  end;

  TMapping = class(TCustomObject, IMapping)
  private
    FImporting: Boolean;
    FLock: boolean;
    FLoaded: boolean;
    FMappingItems: IMemberMetaList;
  protected
    function GetItem(const Name: string): IObjectMeta;
    function GetItems(const Index: Integer): IObjectMeta;
    function GetLoaded: boolean;
    procedure SetLoaded(const Value: boolean);

    function AddObject(const ClassName, TableName: string): IObjectMeta; overload;
    function AddObject(const ObjectClass: TClass; const TableName: string): IObjectMeta; overload;
    function Count: Integer;
    function Find(const Name: string; const TableName: Boolean = False): IObjectMeta; overload;
    function Find(const Name: string; out ObjectMeta; const TableName: Boolean = False): boolean; overload;
    function GetDriver: IInterface;

    procedure Clear;
    procedure LoadMapping(const MappingClass: TClass = nil);
    property Item[const Name: string]: IObjectMeta read GetItem;
    property Items[const Index: Integer]: IObjectMeta read GetItems; default;
    property Loaded: boolean read GetLoaded write SetLoaded;
  public
    constructor Create(const AOwner: IInterface; Importing: Boolean = False); reintroduce;
  end;

  TMappingLoaderClass = class of TMappingLoader;
  TMappingLoader = class(TInterfacedObject, IMappingLoader)
  protected
    procedure Execute(const Mapping: IMapping); virtual; abstract;
  end;

  TMappingRegisterClass = class(TJazzRegisterClass, IMappingRegisterClass)
  private
    FMapping: Pointer;
    procedure ImportSuperMembers(const AObject, ASuper: IObjectMeta);
  protected
    function GetMapping: IMapping;
    procedure AddInheritanceRelation(const AObject, ASuper: IObjectMeta);
    procedure SetMapping(const Value: IMapping);

    procedure LoadMapping;
    procedure UpdateObjectID;
    procedure UpdateRelations;
    property Mapping: IMapping read GetMapping write SetMapping;
  end;

var
  ActiveMapping: IMapping;

procedure RegisterMapping(const MappingLoaderClass: TMappingLoaderClass);
function MappingManager: IMappingRegisterClass;

implementation

uses
  SysUtils,
  JazzConsts,
  JazzPersisterConsts,
  JazzValueType,
  JazzTypeInfo,
  JazzPersisterIntf,
  JazzSessionIntf;

var
  FMappingManager: IMappingRegisterClass;

function MappingManager: IMappingRegisterClass;
begin
  if FMappingManager = nil then FMappingManager:= TMappingRegisterClass.Create;
  Result:= FMappingManager;
end;

procedure RegisterMapping(const MappingLoaderClass: TMappingLoaderClass);
begin
  MappingManager.RegisterClass(MappingLoaderClass);
end;

{ TMapping }

function TMapping.AddObject(const ClassName, TableName: string):
  IObjectMeta;
var
  LMeta: IObjectMeta;
begin
  LMeta:= Find(ClassName);

  if LMeta = nil then
  begin
    LMeta:= TObjectMeta.Create(Self);
    LMeta.ObjectClassName:= ClassName;
    LMeta.TableName:= TableName;
    FMappingItems.Add(ClassName, TableName, LMeta);
  end;

  Result:= LMeta;
end;

function TMapping.AddObject(const ObjectClass: TClass;
  const TableName: string): IObjectMeta;
begin
  Assert(ObjectClass <> nil, SClassCantBeNil);
  Result:= AddObject(ObjectClass.ClassName, TableName);
end;

procedure TMapping.Clear;
begin
  FMappingItems.Clear;
end;

function TMapping.Count: Integer;
begin
  if FMappingItems <> nil then
    Result:= FMappingItems.Count
  else
    Result:= 0;
end;

constructor TMapping.Create(const AOwner: IInterface; Importing: Boolean =
  False);
begin
  inherited Create(AOwner);
  FMappingItems:= TMemberMetaList.Create(True, 64);
  FImporting:= Importing;
  if not FImporting and (ActiveMapping = nil) then ActiveMapping:= Self;
end;

function TMapping.Find(const Name: string; out ObjectMeta;
  const TableName: Boolean): boolean;
begin
  Pointer(ObjectMeta):= Pointer(Find(Name, TableName));
  Result:= Pointer(ObjectMeta) <> nil;
end;

function TMapping.Find(const Name: string; const TableName: Boolean):
  IObjectMeta;
var
  LIndex: Integer;
begin
  if not FLock then
  begin
    FLock:= True;
    try
      if not FImporting and not Loaded then Self.LoadMapping;
    finally
      FLock:= False; 
    end;
  end;

  LIndex:= FMappingItems.IndexOf(Name, TableName);

  if LIndex <> NotFound then
    Result:= FMappingItems[LIndex] as IObjectMeta
  else
    Result:= nil;
end;

function TMapping.GetDriver: IInterface;
begin
  Result:= ((Owner as ISession).Mechanism as IDatabaseMechanism).DatabaseDriver; 
end;

function TMapping.GetItem(const Name: string): IObjectMeta;
begin
  Result:= Find(Name);
end;

function TMapping.GetItems(const Index: Integer): IObjectMeta;
begin
  if Count > 0 then
    Result:= FMappingItems[Index] as IObjectMeta
  else
    Result:= nil;
end;

function TMapping.GetLoaded: boolean;
begin
  Result:= FLoaded;
end;

procedure TMapping.LoadMapping(const MappingClass: TClass);
begin
  MappingManager.Mapping:= Self;
  MappingManager.LoadMapping;
end;

{ TRegisterMappingClass }

function TMappingRegisterClass.GetMapping: IMapping;
begin
  Result:= IMapping(FMapping);
end;

procedure TMappingRegisterClass.LoadMapping;
var
  I: Integer;
  LItem: IMappingLoader;
  LClass: TMappingLoaderClass;
  LMappingLock: boolean;
begin
  Lock;
  LMappingLock:= not TMapping(Mapping.Implementor).FLock;
  try
    if LMappingLock then TMapping(Mapping.Implementor).FLock:= True; 
    for I:= 0 to Count - 1 do
    begin
      LClass:= TMappingLoaderClass(Items[I]);
      LItem:= LClass.Create;
      LItem.Execute(Mapping);
    end;
    UpdateObjectID;
    UpdateRelations;
  finally
    Mapping.Loaded:= True;
    if LMappingLock then TMapping(Mapping.Implementor).FLock:= False; 
    Unlock;
  end;
end;

procedure TMappingRegisterClass.SetMapping(const Value: IMapping);
begin
  FMapping:= Pointer(Value);
end;

procedure TMappingRegisterClass.ImportSuperMembers(const AObject, ASuper:
  IObjectMeta);
var
  I: Integer;
  LItem: IMemberMeta;
  LSuperItem: IMemberMeta;
  LRelationship: IRelationshipMeta;
begin
  (* Members *)
  for I:= 0 to ASuper.Items.Count - 1 do
  begin
    LSuperItem:= ASuper.Items[I] as IMemberMeta;
    if AObject.FindMember(LSuperItem.MemberName) = nil then
    begin
      LItem:= AObject.Add(LSuperItem.MemberName, LSuperItem.ColumnName);
      LItem.Assign(ASuper.Items[I] as IMemberMeta);
    end;
  end;

  (* Primary Keys *)
  for I:= 0 to ASuper.OID.Count -1 do
    AObject.OID.Add(ASuper.OID[I]);

  (* Relationships *)
  for I:= 0 to ASuper.Relationships.Count - 1 do
  begin
    LRelationship:= ASuper.Relationships[I] as IRelationshipMeta;
    with AObject.AddRelationship(LRelationship.RelationshipType) do
    begin
      Assign(LRelationship);
      FromClassName:= AObject.GetClassName;
    end;
  end;
end;

procedure TMappingRegisterClass.UpdateRelations;
var
  I: Integer;
  LIndex: Integer;
  LMember: IMemberMeta;
  LObject: IObjectMeta;
  LSuper: IObjectMeta;
  LTypeInfo: ITypeInfo;
begin
  for I:= 0 to MappingManager.Mapping.Count - 1 do
  begin
    LObject:= MappingManager.Mapping.Items[I];
    LTypeInfo:= TypeRegister.FindTypeInfo(LObject.ObjectClassName, True);
    if (LTypeInfo <> nil) and (LTypeInfo.TypeClassParent <> nil) then
    begin
      LSuper:= MappingManager.Mapping.Item[LTypeInfo.TypeClassParent.ClassName];
      if LSuper = nil then Continue;

      if LSuper.TableName = LObject.TableName then
        ImportSuperMembers(LObject, LSuper)
      else
      begin
        LObject.Super:= LSuper;
        AddInheritanceRelation(LObject, LSuper);
      end;

      // reorganize OID
      for LIndex:= 0 to LObject.OID.Count - 1 do
      begin
        LMember:= LObject.FindMember(LObject.OID[LIndex].MemberName);
        TObjectMeta(LObject.Implementor).Items.
          Exchange(LObject.IndexOf(LMember), LIndex);
      end;
    end;
  end;
end;

procedure TMapping.SetLoaded(const Value: boolean);
begin
  FLoaded:= Value;
end;

{ TRelationshipMeta }

constructor TRelationshipMeta.Create(const RelationshipType:
  TRelationshipType);
begin
  inherited Create(nil);

  FFromFields:= TStringList.Create;
  FFromMembers:= TStringList.Create;
  FToFields:= TStringList.Create;
  FToMembers:= TStringList.Create;

  CascadeDelete:= False;
  CascadeLoad:= False;
  CascadeSave:= False;
  FForeignKey:= True;


  FRelationshipType:= RelationshipType;
  FJoinType:= jtInner;
end;

function TRelationshipMeta.GetCascadeDelete: Boolean;
begin
  Result:= FCascadeDelete;
end;

function TRelationshipMeta.GetCascadeLoad: Boolean;
begin
  Result:= FCascadeLoad;
end;

function TRelationshipMeta.GetCascadeSave: Boolean;
begin
  Result:= FCascadeSave;
end;

function TRelationshipMeta.GetForeignKeyName: string;
var
  I: Integer;
  LFields: string;
begin
  if ForeignKey and (FForeignKeyName = EmptyStr) then
  begin
    LFields:= EmptyStr;
    for I := 0 to FromFields.Count - 1 do
      LFields:= '_' + FromFields[I];

    if (LFields <> EmptyStr) or (LFields <> '_') then
      FForeignKeyName:= SFKPrefix + FromTable + LFields
    else
      raise Exception.CreateFmt(SFromMemberNotDefined, [FromClassName]);
  end;

  Result:= FForeignKeyName;
end;

function TRelationshipMeta.GetFromClassName: string;
begin
  Result:= FFromClassName;
end;

function TRelationshipMeta.GetFromFields: TStrings;
begin
  Result:= FFromFields;
end;

function TRelationshipMeta.GetFromMembers: TStrings;
begin
  Result:= FFromMembers;
end;

function TRelationshipMeta.GetRelationshipType: TRelationshipType;
begin
  Result:= FRelationshipType;
end;

{ TClassMeta }

function TObjectMeta.Add(MemberName, ColumnName: string; ColumnType: TItemType;
  ColumnSize: Integer): IMemberMeta;
begin
  Result:= FindMember(MemberName, False);

  if Result = nil then
  begin
    Result:= Add;
    FMembersItems.Add(MemberName, ColumnName, Result);
    Result.MemberName:= MemberName;
    Result.ColumnName:= ColumnName;
    Result.ColumnType:= ColumnType;
    Result.Size:= ColumnSize;
  end;
end;

function TObjectMeta.GetCount: Integer;
begin
  if FMembersItems <> nil then
    Result:= FMembersItems.Count
  else
    Result:= 0;
end;

constructor TObjectMeta.Create(const AOwner: IInterface);
begin
  inherited Create(AOwner);
  FRelationshipsUpdated:= False;
  FMembersItems:= TMemberMetaList.Create(True, 32);
  FIndexes:= TInterfaceList.Create;
  FRelationships:= TInterfaceList.Create;
  FOID:= TOIDMeta.Create(Self); 
end;

function TObjectMeta.FindMember(const Name: string; const LookupSuper:
  boolean; const ColumnName: Boolean): IMemberMeta;
var
  LIndex: Integer;
begin
  Result:= nil;
  LIndex:= FMembersItems.IndexOf(Name, ColumnName);

  if LIndex <> NotFound then
    Result:= FMembersItems.Items[LIndex] as IMemberMeta
  else
  if LookupSuper and (Super <> nil) then
    Result:= Super.FindMember(Name, LookupSuper, ColumnName);
end;

function TObjectMeta.GetObjectClassName: string;
begin
  Result:= FObjectClassName;
end;

function TObjectMeta.FullName(const Mapped: Boolean = False): string;
var
  LOwner: IObjectMeta;
begin
  if not Mapped then
  begin
    if Supports(Owner, IObjectMeta, LOwner) then
      Result:= LOwner.FullName(Mapped) + '.' + ObjectClassName
    else
      Result:= ObjectClassName;
  end
  else
  begin
    if Supports(Owner, IObjectMeta, LOwner) then
      Result:= LOwner.FullName(Mapped) + '.' + TableName
    else
      Result:= TableName;
  end;
end;

function TObjectMeta.GetTableName: string;
begin
  Result:= FTableName;
end;

procedure TObjectMeta.SetObjectClassName(const Value: string);
begin
  FObjectClassName:= Value;
end;

procedure TObjectMeta.SetTableName(const Value: string);
begin
  FTableName:= Value;
end;

function TRelationshipMeta.GetToClassName: string;
begin
  Result:= FToClassName;
end;

function TRelationshipMeta.GetToFields: TStrings;
begin
  Result:= FToFields;
end;

function TRelationshipMeta.GetToMembers: TStrings;
begin
  Result:= FToMembers;
end;

function TRelationshipMeta.GetToTable: string;
begin
  Result:= FToTable;
end;

procedure TRelationshipMeta.SetCascadeDelete(const Value: Boolean);
begin
  FCascadeDelete:= Value;
end;

procedure TRelationshipMeta.SetCascadeLoad(const Value: Boolean);
begin
  FCascadeLoad:= Value;
end;

procedure TRelationshipMeta.SetCascadeSave(const Value: Boolean);
begin
  FCascadeSave:= Value;
end;

procedure TRelationshipMeta.SetForeignKeyName(const Value: string);
begin
  FForeignKeyName:= Value;
end;

procedure TRelationshipMeta.SetFromClassName(const Value: string);
begin
  FFromClassName:= Value;
end;

procedure TRelationshipMeta.SetToClassName(const Value: string);
begin
  FToClassName:= Value;
end;

procedure TRelationshipMeta.SetToTable(const Value: string);
begin
  FToTable:= Value;
end;

function TRelationshipMeta.GetFromTable: string;
begin
  Result:= FFromTable;
end;

procedure TRelationshipMeta.SetFromTable(const Value: string);
begin
  FFromTable:= Value;
end;

procedure TRelationshipMeta.SetRelationshipType(
  const Value: TRelationshipType);
begin
  FRelationshipType:= Value;
end;

function TRelationshipMeta.GetUpdated: Boolean;
begin
  Result:= FUpdated;
end;

procedure TRelationshipMeta.SetUpdated(const Value: Boolean);
begin
  FUpdated:= Value;
end;

function TRelationshipMeta.GetMasterAttribute: string;
begin
  Result:= FMasterAttribute;
end;

procedure TRelationshipMeta.SetMasterAttribute(const Value: string);
begin
  FMasterAttribute:= Value;
end;

function TRelationshipMeta.GetJoinType: TJoinType;
begin
  Result:= FJoinType;
end;

procedure TRelationshipMeta.SetJoinType(const Value: TJoinType);
begin
  FJoinType:= Value;
end;

destructor TRelationshipMeta.Destroy;
begin
  FFromFields.Free;
  FFromMembers.Free;
  FToFields.Free;
  FToMembers.Free;
  inherited;
end;

procedure TRelationshipMeta.Assign(const Source: IRelationshipMeta);
begin
  FCascadeDelete:= Source.CascadeDelete;
  FCascadeLoad:= Source.CascadeLoad;
  FCascadeSave:= Source.CascadeSave;
  FForeignKeyName:= Source.ForeignKeyName;
  FFromClassName:= Source.FromClassName;
  FFromFields:= Source.FromFields;
  FFromMembers:= Source.FromMembers;
  FFromTable:= Source.FromTable;
  FJoinType:= Source.JoinType;
  FMasterAttribute:= Source.MasterAttribute;
  FRelationshipType:= Source.RelationshipType;
  FToClassName:= Source.ToClassName;
  FToFields:= Source.ToFields;
  FToMembers:= Source.ToMembers;
  FToTable:= Source.ToTable;
  FUpdated:= Source.Updated;
end;

function TRelationshipMeta.GetForeignKey: Boolean;
begin
  Result := FForeignKey;
end;

procedure TRelationshipMeta.SetForeignKey(Value: Boolean);
begin
  FForeignKey := Value;
end;

{ TMemberMeta }

function TMemberMeta.GetColumnName: string;
begin
  Result:= FColumnName;
end;

function TMemberMeta.GetColumnType: TItemType;
begin
  Result:= FColumnType;
end;

function TMemberMeta.FullName(const Mapped: Boolean = False): string;
begin
  if not Mapped then
    Result:= (Owner as IObjectMeta).FullName + '.' + Self.MemberName
  else
    Result:= (Owner as IObjectMeta).FullName + '.' + ColumnName;
end;

function TMemberMeta.GetMemberName: string;
begin
  Result:= FMemberName;
end;

function TMemberMeta.GetPrecision: SmallInt;
begin
  Result:= FPrecision;
end;

function TMemberMeta.GetRequired: Boolean;
begin
  Result:= FRequired;
end;

function TMemberMeta.GetSize: SmallInt;
begin
  Result:= FSize;
end;

procedure TMemberMeta.SetColumnName(const Value: string);
begin
  FColumnName:= Value;
end;

procedure TMemberMeta.SetColumnType(const Value: TItemType);
begin
  if Value in [itBlob, itMemo] then LazyLoad:= True;
  FColumnType:= Value;
end;

procedure TMemberMeta.SetMemberName(const Value: string);
begin
  FMemberName:= Value;
end;

procedure TMemberMeta.SetPrecision(const Value: SmallInt);
begin
  FPrecision:= Value;
end;

procedure TMemberMeta.SetRequired(const Value: Boolean);
begin
  FRequired:= Value;
end;

procedure TMemberMeta.SetSize(const Value: SmallInt);
begin
  FSize:= Value;
end;

function TObjectMeta.GetItems: IMemberMetaList;
begin
  Result:= FMembersItems;
end;

function TMemberMeta.GetMappingType: TMappingType;
begin
  Result:= FMappingType;
end;

procedure TMemberMeta.SetMappingType(const Value: TMappingType);
begin
  FMappingType:= Value;
end;

function TMemberMeta.GetLazyLoad: Boolean;
begin
  Result:= FLazyLoad;
end;

procedure TMemberMeta.SetLazyLoad(const Value: Boolean);
begin
  FLazyLoad:= Value;
end;

function TMemberMeta.GetFieldType: TFieldType;
begin
  if FFieldType = ftUnknown then
    FFieldType:= GetDriver.GetFieldType(TItemType(FColumnType));

  Result:= FFieldType;
end;

constructor TMemberMeta.Create(const AOwner: IInterface);
begin
  inherited Create(AOwner);
  ColumnType:= itString;
  FieldType:= ftUnknown;
end;

procedure TMemberMeta.SetFieldType(const Value: TFieldType);
begin
  FFieldType:= Value;
end;

function TMemberMeta.GetIsOID: Boolean;
begin
  Result:= FIsOID;
end;

procedure TMemberMeta.SetIsOID(const Value: Boolean);
begin
  FIsOID:= Value;
end;

procedure TMemberMeta.Assign(const Source: IMemberMeta);
begin
  FColumnName:= Source.ColumnName;
  FColumnType:= Source.ColumnType;
  FFieldType:= Source.FieldType;
  FIsOID:= Source.IsOID;
  FLazyLoad:= Source.LazyLoad;
  FMappingType:= Source.MappingType;
  FMemberName:= Source.MemberName;
  FPrecision:= Source.Precision;
  FRequired:= Source.Required;
  FSize:= Source.Size;
end;

function TMemberMeta.GetDriver: IDatabaseDriver;
begin
  Result:= ((Owner as IObjectMeta).Owner as IMapping).GetDriver as IDatabaseDriver;
end;

function TMemberMeta.GetGeneratorClass: TClass;
begin
  Result:= FGeneratorClass;
end;

procedure TMemberMeta.SetGeneratorClass(const Value: TClass);
begin
  FGeneratorClass:= Value;
end;

{ TIndexMeta }

procedure TIndexMeta.Assign(const Source: IIndexMeta);
begin
  FIndexName:= Source.IndexName;
  FIndexType:= Source.IndexType;
  FIndexFields:= Source.IndexFields;
end;

constructor TIndexMeta.Create(const AOwner: IInterface; const IndexName: string;
  const IndexType: TIndexType);
begin
  inherited Create(AOwner);
  FIndexFields:= TStringList.Create;
  Self.IndexName:= IndexName;
  Self.IndexType:= IndexType;
end;

destructor TIndexMeta.Destroy;
begin
  FIndexFields.Free;
  inherited;
end;

function TIndexMeta.GetIndexFields: TStrings;
begin
  Result:= FIndexFields;
end;

function TIndexMeta.GetIndexName: string;
begin
  Result:= FIndexName;
end;

function TIndexMeta.GetIndexType: TIndexType;
begin
  Result:= FIndexType;
end;

procedure TIndexMeta.SetIndexName(const Value: string);
begin
  FIndexName:= Value;
end;

procedure TIndexMeta.SetIndexType(const Value: TIndexType);
begin
  FIndexType:= Value;
end;

function TObjectMeta.GetIndexes: IInterfaceList;
begin
  Result:= FIndexes;
end;

function TObjectMeta.GetRelationships: IInterfaceList;
begin
  if not FRelationshipsUpdated then
    FRelationshipsUpdated:= UpdateRelationships;

  Result:= FRelationships;
end;

function TObjectMeta.UpdateRelationships: Boolean;
var
  I: Integer;
  J: Integer;
  LRelationship: IRelationshipMeta;
  LRelated: IObjectMeta;
  LMember: IMemberMeta;
begin
  for I:= 0 to FRelationships.Count - 1 do
  begin
    LRelationship:= FRelationships[I] as IRelationshipMeta;
    if LRelationship.Updated then Continue;

    if LRelationship.FromClassName = EmptyStr then
      LRelationship.FromClassName:= Self.ObjectClassName;
    if LRelationship.FromTable = EmptyStr then
      LRelationship.FromTable:= Self.TableName;

    if LRelationship.FromFields.Count = 0 then
      for J:= 0 to LRelationship.FromMembers.Count - 1 do
      begin
        LMember:= FindMember(LRelationship.FromMembers[J]);
        if LMember <> nil then
          LRelationship.FromFields.Add(LMember.ColumnName);
      end;

    LRelated:= (Owner as IMapping).Find(LRelationship.ToClassName);
    if LRelated <> nil then
    begin
      if LRelationship.ToTable = EmptyStr then
        LRelationship.ToTable:= LRelated.TableName;

      if LRelationship.ToFields.Count = 0 then
      begin
        for J:= 0 to LRelationship.ToMembers.Count - 1 do
        begin
          LMember:= LRelated.FindMember(LRelationship.ToMembers[J]);
          if LMember <> nil then
            LRelationship.ToFields.Add(LMember.ColumnName);
        end;
      end;
    end;
    LRelationship.Updated:= True;
  end;
  Result:= (FRelationships.Count > 0);
end;

function TObjectMeta.Add(const Member: IMemberMeta): Integer;
begin
  if Member.MemberName = EmptyStr then
    raise exception.Create(SMemberNameRequired)
  else
  begin
    if Member.IsOID then
    begin
      Result:= OID.Count; 
      FMembersItems.Insert(OID.Count, Member.MemberName, Member);
    end
    else
      Result:= FMembersItems.Add(Member.MemberName, Member);
  end;
end;

function TObjectMeta.Add: IMemberMeta;
begin
  Result:= TMemberMeta.Create(Self);
end;

function TObjectMeta.AddRelationship(const RelationshipType:
  TRelationshipType): IRelationshipMeta;
begin
  Result:= TRelationshipMeta.Create(RelationshipType);
  if RelationshipType = rtInheritance then
    FRelationships.Insert(0, Result)
  else
    FRelationships.Add(Result);
end;

procedure TMappingRegisterClass.AddInheritanceRelation(const AObject,
  ASuper: IObjectMeta);
var
  I: Integer;
  LIDMember: IMemberMeta;
  LMember: IMemberMeta;
  LRelationship: IRelationshipMeta;
begin
  (* Primary Keys *)
  for I:= 0 to ASuper.OID.Count - 1 do
  begin
    LIDMember:= ASuper.FindMember(ASuper.OID[I].MemberName, True, True);
    LMember:= AObject.Add(LIDMember.MemberName, LIDMember.ColumnName);
    LMember.Assign(LIDMember);
    LMember.IsOID:= True;
  end;

  LRelationship:= AObject.AddRelationship(rtInheritance);
  LRelationship.FromClassName:= AObject.ObjectClassName;
  LRelationship.ToClassName:= ASuper.ObjectClassName;

  LRelationship.FromMembers.Text:= AObject.OID.GetNames(EOL);
  LRelationship.ToMembers.Text:= AObject.OID.GetNames(EOL);

  LRelationship.CascadeDelete:= True;
  LRelationship.CascadeLoad:= True;
  LRelationship.CascadeSave:= True;
  AObject.UpdateRelationships;
end;

function TObjectMeta.GetSuper: IObjectMeta;
begin
  Result:= FSuper;
end;

procedure TObjectMeta.SetSuper(const Value: IObjectMeta);
begin
  FSuper:= Value
end;

function TObjectMeta.IndexOf(const Member: IInterface): Integer;
var
  I: Integer;
  LMember: IMemberMeta;
begin
  Result:= NotFound;
  if Supports(Member, IMemberMeta, LMember) then
  begin
    for I:= 0 to FMembersItems.Count - 1 do
    begin
      if (FMembersItems[I] as IMemberMeta).MemberName = LMember.MemberName then
      begin
        Result:= I;
        Break;
      end;
    end;
  end;
end;

destructor TObjectMeta.Destroy;
begin
  FOID.Clear;
  inherited;
end;

{ TOIDMeta }

function TOIDMeta.Add(const Item: IMemberMeta): Integer;
begin
  if (FItem = nil) and (FItems = nil) then
  begin
    Single:= True;
    FItem:= Item;
    Result:= 0;
  end
  else
  begin
    Single:= False;
    if FItems = nil then FItems:= TMemberMetaList.Create(True, 4);
    if FItem <> nil then
    begin
      FItems.Add(FItem.MemberName, FItem.ColumnName, FItem);
      FItem:= nil;
    end;

    Result:= IndexOf(Item);
    if Result = NotFound then
      Result:= FItems.Add(Item.MemberName, Item.ColumnName, Item);
  end;
end;

procedure TOIDMeta.Clear;
begin
  FItem:= nil;
  if FItems <> nil then
  begin
    FItems.Clear;
    FItems:= nil;
  end;
end;

constructor TOIDMeta.Create(const AOwner: IInterface);
begin
  inherited;
  FSingle:= True;
end;

function TOIDMeta.GetCount: Integer;
begin
  if Single and (FItem <> nil) then
    Result:= 1
  else
  if FItems <> nil then
    Result:= FItems.Count
  else
    Result:= 0
end;

function TOIDMeta.GetItem: IMemberMeta;
begin
  Result:= FItem;
end;

function TOIDMeta.GetItems(const Index: Integer): IMemberMeta;
begin
  if Single and (FItem <> nil) and (Index = 0) then
    Result:= FItem
  else
  if FItems <> nil then
    Result:= FItems[Index] as IMemberMeta
  else
    Result:= nil;
end;

function TOIDMeta.GetMember(const Name: string): IMemberMeta;
begin
  if Single and (FItem <> nil) and (Uppercase(FItem.MemberName) = Uppercase(Name)) then
    Result:= FItem
  else
    Result:= FItems.Item[Name] as IMemberMeta;
end;

function TOIDMeta.GetMeta(const Name: string): IMemberMeta;
var
  LIndex: Integer;
begin
  if Single and (FItem <> nil) and
    (Uppercase(FItem.ColumnName) = Uppercase(Name)) then
    Result:= FItem
  else
  begin
    LIndex:= FItems.IndexOf(Name, True);
    if LIndex = NotFound then
      Result:= nil
    else
      Result:= Items[LIndex];
  end;
end;

function TOIDMeta.GetNames(const Separator: string; const Meta: boolean): string;
  function GetItemName(const Index: Integer): string;
  var
    LItem: IMemberMeta;
  begin
    LItem:= Items[Index];
    if LItem <> nil then
    begin
      if Meta then
        Result:= LItem.ColumnName
      else
        Result:= LItem.MemberName;
    end;
  end;

var
  LIndex: Integer;
begin
  Result:= EmptyStr;
  LIndex:= 0;

  if Single then
    Result:= GetItemName(LIndex)
  else
  begin
    for LIndex:= 0 to Count -1 do
    begin
      if Result = EmptyStr then
        Result:= GetItemName(LIndex)
      else
        Result:= Result + Separator + GetItemName(LIndex);
    end;
  end
end;

function TOIDMeta.GetSingle: boolean;
begin
  Result:= FSingle;
end;

function TOIDMeta.IndexOf(const Item: IMemberMeta): Integer;
begin
  if Single and (FItem = Item) then
    Result:= 0
  else
  if FItems <> nil then
    Result:= FItems.IndexOf(Item)
  else
    Result:= NotFound;
end;

function TOIDMeta.IsMapped: boolean;
begin
  Result:= (FItem <> nil) or ((FItems <> nil) and (FItems.Count > 0));
end;

function TOIDMeta.IsMemberOID(const Item: IMemberMeta): boolean;
begin
  Result:= IndexOf(Item) <> NotFound; 
end;

procedure TOIDMeta.Remove(const Item: IMemberMeta);
begin
  if Single and (FItem = Item) then
    FItem:= nil
  else
  if FItems <> nil then
    FItems.Remove(Item);  
end;

procedure TOIDMeta.SetSingle(const Value: boolean);
begin
  FSingle:= Value;
end;

function TObjectMeta.GetOID: IOIDMeta;
var
  I: Integer;
  LItem: IMemberMeta;
begin
  if not FOID.IsMapped then
  begin
    for I:= 0 to Count -1 do
    begin
      LItem:= Items[I] as IMemberMeta;
      if LItem.IsOID then FOID.Add(LItem);
    end;
  end;

  Result:= FOID;
end;

procedure TMappingRegisterClass.UpdateObjectID;
var
  I: Integer;
begin
  for I:= 0 to MappingManager.Mapping.Count - 1 do
    MappingManager.Mapping.Items[I].OID;
end;

procedure TObjectMeta.GetLazyItems(var LazyItems: TStrings);
var
  I: Integer;
  LItem: IMemberMeta;
begin
  LazyItems.Clear;
  for I:= 0 to Count -1 do
  begin
    LItem:= Items[I] as IMemberMeta;
    if LItem.LazyLoad then LazyItems.Add(LItem.MemberName);
  end;
end;

function TObjectMeta.GetMember(const Name: string; const LookupSuper,
  ColumnName: Boolean): IMemberMeta;
begin
  Result:= FindMember(Name, LookupSuper, ColumnName);
  if Result = nil then raise Exception.Create(SMemberMetatadaNotFound);
end;

end.



