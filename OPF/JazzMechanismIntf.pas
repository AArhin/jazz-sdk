unit JazzMechanismIntf;

interface

uses
  Classes,
  JazzMappingIntf,
  JazzValueTypeIntf,
  JazzCriteriaIntf;

type
  IMechanism = interface(IInterface)
    ['{9BA073E3-50A4-4EFD-8A8D-C08DE833B81C}']
    function GetConnected: boolean;
    function GetLazyLoad: boolean;
    function GetMapping: IMapping;
    function GetOnlyModified: boolean;
    function GetSession: IInterface;
    procedure SetConnected(const Value: boolean);
    procedure SetLazyLoad(const Value: boolean);
    procedure SetOnlyModified(const Value: boolean);
    procedure SetSession(const Value: IInterface);

    function LoadMember(const Member: IMemberType; const AutoLoad: boolean = False): boolean;
    function LoadObject(const AObject: IObjectType; const Meta: IObjectMeta; const MembersProxy: TStrings = nil): boolean;
    function LoadObjectList(const AObjectList: IObjectListType; const Criteria: ICriteria = nil): boolean;

    procedure NewOID(const AObject: IObjectType);
    procedure DeleteObjectList(const  AObjectList: IObjectListType);
    procedure SaveObject(const AObject: IObjectType; const Meta: IObjectMeta = nil);
    procedure SaveObjectList(const AObjectList: IObjectListType);

    function InTransaction: boolean;
    procedure StartTransaction;
    procedure CommitTransaction(const ForceCommit: boolean = False);
    procedure RollbackTransaction;

    property Connected: boolean read GetConnected write SetConnected;
    property LazyLoad: boolean read GetLazyLoad write SetLazyLoad;
    property Mapping: IMapping read GetMapping;
    property OnlyModified: boolean read GetOnlyModified write SetOnlyModified;
    property Session: IInterface read GetSession write SetSession;
  end;

implementation

end.
