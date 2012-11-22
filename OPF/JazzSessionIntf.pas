unit JazzSessionIntf;

interface

uses
  JazzValueTypeIntf,
  JazzMechanismIntf,
  JazzMappingIntf,
  JazzClasses,
  JazzCriteriaIntf,
  JazzDataFactory;

type
  TCacheMode = (
    cmShared,
    cmExclusive,
    cmNoCache
  );
  
  ICache = interface(ICustomObject)
    ['{DDA9FCF9-A3AE-463A-A250-561F182794C5}']
    function Count: Integer;
    function GetItems(Index: Integer): IInterface;
    procedure Clear;
    function Find(const Path: string): IObjectType; overload;
    function Find(const ObjectClassName: string; const OID: string): IObjectType; overload;
    function Find(const AObject: IObjectType; const OID: string): boolean; overload;
    function Find(const AObject: IObjectType): boolean; overload;
    procedure Add(const AObject: IObjectType);
    procedure Remove(const AObject: IObjectType);
    property Items[Index: Integer]: IInterface read GetItems;
  end;

  ISession = interface(ICustomObject)
    ['{ECE09B05-BEEF-47DC-9C25-BAF1275D6C3E}']
    function GetCache: ICache;
    function GetCacheMode: TCacheMode;
    function GetMapping: IMapping;
    function GetMechanism: IMechanism;
    function GetName: string;
    procedure SetName(const Value: string);

    procedure ClearCache;
    function Load(const AObjectList: IObjectListType; const Criteria: ICriteria = nil): boolean; overload;
    function Load(const AObject: IObjectType): boolean; overload;
    procedure Save(const AObjectList: IObjectListType; const DeleteList: boolean = False); overload;
    procedure Save(const AObject: IObjectType); overload;
    function GetDataFactory: IDataFactory;

    property Cache: ICache read GetCache;
    property CacheMode: TCacheMode read GetCacheMode;
    property Mechanism: IMechanism read GetMechanism;
    property Mapping: IMapping read GetMapping;
    property Name: string read GetName write SetName;
    property DataFactory: IDataFactory read GetDataFactory;
  end;

implementation

end.

