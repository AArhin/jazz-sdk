unit JazzMappingIntf;

interface

uses
  Classes,
  DB,
  JazzClasses;

type
  IObjectMeta = interface;
  IRelationshipMeta = interface;
  TItemType = (
    itUnknow,
    // numeric types
    itAutoInc,
    itBoolean,
    itCurrency,
    itFloat,
    itInteger,
    itLongInt,
    itSmallInt,

    // string types
    itChar,
    itString,
    itWideString,

    // date time type
    itDate,
    itDateTime,
    itTime,
    itTimeStamp,

    // stream types
    itBlob,
    itMemo,
    itImage,

    itObject,     // relationship OneToOne
    itObjectList  // relationship OneToMany
  );

  TDateTimeTypes = itDate..itTimeStamp;
  TNumericTypes = itAutoInc..itSmallInt;
  TTextTypes = itString..itWideString;

  TIndexType = (
    itUnique,
    itAscending,
    itDescending
  );

  TRelationshipType = (
    rtNone,
    rtOneToOne,
    rtOneToMany,
    rtManyToOne,  // I dont think that this should be necessary
    rtManyToMany, // should be 2 OneToMany
    rtInheritance // same that rtOneToOne
  );

  TJoinType = (
    jtInner,       // INNER JOIN - default 
    jtLeft,        // LEFT OUTER JOIN
    jtRight,       // RIGHT OUTER JOIN
    jtFull         // FULL OUTER JOIN
  );

  TMappingType = (
    mtColumn,
    mtProcedure,
    mtSubSelect
  );

  TShemaOperation = (
    soNone,
    soCreateTable,
    soDropTable,
    soRecreateTable,
    soAdd,
    soDrop,
    soDropAdd,
    soAlterType
  );
  
  IMapping = interface(ICustomObject)
    ['{9E8B98A1-5941-42C1-AB35-FB42F1EDF25E}']
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
  end;

  IMappingLoader = interface(IInterface)
    ['{AAAB49BA-E523-4F99-9C95-C58F685820EB}']
    procedure Execute(const Mapping: IMapping);
  end;

  IMappingRegisterClass = interface(IRegisterClass)
  ['{2A4D0F13-4BE5-4028-8C1E-1C2A4CDB8FF0}']
    function GetMapping: IMapping;
    procedure SetMapping(const Value: IMapping);
    procedure LoadMapping;
    procedure UpdateRelations;
    property Mapping: IMapping read GetMapping write SetMapping;
  end;

  IMemberMeta = interface(ICustomObject)
    ['{9A38C2E3-7FC5-46F5-ABEC-7FA4FF2EE152}']
    function GetColumnName: string;
    function GetColumnType: TItemType;
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
  end;

  IRelationshipMeta = interface(ICustomObject)
    ['{6286911F-5BD3-4594-A363-2BABDE579298}']
    function GetCascadeDelete: Boolean;
    function GetCascadeLoad: Boolean;
    function GetCascadeSave: Boolean;
    function GetForeignKey: Boolean;
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
    procedure SetForeignKey(Value: Boolean);
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
  end;

  IIndexMeta = interface(ICustomObject)
    ['{E43D4EB0-78F3-4C1A-A531-8E82C698F750}']
    function GetIndexName: string;
    function GetIndexType: TIndexType;
    function GetIndexFields: TStrings;
    procedure SetIndexName(const Value: string);
    procedure SetIndexType(const Value: TIndexType);

    property IndexName: string read GetIndexName write SetIndexName;
    property IndexType: TIndexType read GetIndexType write SetIndexType;
    property IndexFields: TStrings read GetIndexFields;
  end;

  IMemberMetaList = INamedInterfaceList;

  IOIDMeta = interface(ICustomObject)
    ['{9362C804-61DC-43BF-99FB-0CA3D4234156}']
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
  end;

  IObjectMeta = interface(ICustomObject)
  ['{85F5CB78-6CAD-4CB5-81F0-537CF38337A2}']
    function GetIndexes: IInterfaceList;
    function GetItems: INamedInterfaceList;
    function GetRelationships: IInterfaceList;

    function GetObjectClassName: string;
    function GetSuper: IObjectMeta;
    function GetTableName: string;
    procedure SetObjectClassName(const Value: string);
    procedure SetSuper(const Value: IObjectMeta);
    procedure SetTableName(const Value: string);

    function Add: IMemberMeta; overload;
    function Add(const Member: IMemberMeta): Integer; overload;
    function Add(MemberName, ColumnName: string; ColumnType: TItemType = itString; ColumnSize: Integer = 38): IMemberMeta; overload;
    function GetCount: Integer;
    function FindMember(const Name: string; const LookupSuper: boolean = True; const ColumnName: Boolean = False): IMemberMeta;
    function GetMember(const Name: string; const LookupSuper: boolean = True; const ColumnName: Boolean = False): IMemberMeta;
    function FullName(const Mapped: boolean = False): string;
    function IndexOf(const Member: IInterface): Integer; 

    function UpdateRelationships: boolean;
    function AddRelationship(const RelationshipType: TRelationshipType = rtOneToMany): IRelationshipMeta;
    function GetOID: IOIDMeta;
    procedure GetLazyItems(var LazyItems: TStrings);

    property Count: Integer read GetCount;
    property ObjectClassName: string read GetObjectClassName write SetObjectClassName;
    property TableName: string read GetTableName write SetTableName;
    property OID: IOIDMeta read GetOID;
    property Super: IObjectMeta read GetSuper write SetSuper;

    property Items: INamedInterfaceList read GetItems;
    property Indexes: IInterfaceList read GetIndexes;
    property Relationships: IInterfaceList read GetRelationships;
  end;

implementation

end.





