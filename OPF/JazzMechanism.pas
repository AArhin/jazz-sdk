unit JazzMechanism;

interface

uses
  Classes,
  JazzMechanismIntf,
  JazzMappingIntf,
  JazzValueTypeIntf,
  JazzCriteriaIntf;

type
  TMechanismClass = class of TMechanism;
  TMechanism = class(TInterfacedObject, IMechanism)
  private
    FConnected: boolean;
    FLazyLoad: boolean;
    FOnlyModified: boolean;
    FPersister: Pointer;
  protected
    function GetConnected: boolean;
    function GetLazyLoad: boolean;
    function GetMapping: IMapping;
    function GetOnlyModified: boolean;
    function GetSession: IInterface;
    procedure SetConnected(const Value: boolean); virtual;
    procedure SetLazyLoad(const Value: boolean); virtual;
    procedure SetOnlyModified(const Value: boolean); virtual;
    procedure SetSession(const Value: IInterface);

    function LoadMember(const Member: IMemberType; const AutoLoad: boolean = False): boolean; virtual; abstract;
    function LoadObject(const AObject: IObjectType; const Meta: IObjectMeta; const Members: TStrings = nil): boolean; virtual; abstract;
    function LoadObjectList(const AObjectList: IObjectListType; const Criteria: ICriteria = nil): boolean; virtual; abstract;

    procedure NewOID(const AObject: IObjectType); virtual;
    procedure DeleteObjectList(const  AObjectList: IObjectListType); virtual; abstract;
    procedure SaveObject(const AObject: IObjectType; const Meta: IObjectMeta = nil); virtual;
    procedure SaveObjectList(const  AObjectList: IObjectListType); virtual; abstract;

    function InTransaction: boolean; virtual; abstract;
    procedure StartTransaction; virtual; abstract;
    procedure CommitTransaction(const ForceCommit: boolean = False); virtual; abstract;
    procedure RollbackTransaction; virtual; abstract; 

    property Connected: boolean read GetConnected write SetConnected;
    property LazyLoad: boolean read GetLazyLoad write SetLazyLoad;
    property Mapping: IMapping read GetMapping;
    property OnlyModified: boolean read GetOnlyModified write SetOnlyModified;
    property Session: IInterface read GetSession write SetSession;
  public
    constructor Create(const Session: IInterface); virtual;
  end;

implementation

uses
  JazzPersisterIntf,
  JazzSessionIntf;

{ TMechanism }

constructor TMechanism.Create(const Session: IInterface);
begin
  inherited Create;
  Self.Session:= Session;
  FOnlyModified:= True;
  FLazyLoad:= True;
end;

function TMechanism.GetConnected: boolean;
begin
  Result:= FConnected;
end;

function TMechanism.GetLazyLoad: boolean;
begin
  Result:= FLazyLoad;
end;

function TMechanism.GetMapping: IMapping;
begin
  Result:= (Session as ISession).Mapping;
end;

function TMechanism.GetOnlyModified: boolean;
begin
  Result:= FOnlyModified;
end;

function TMechanism.GetSession: IInterface;
begin
  Result:= IInterface(FPersister);
end;

procedure TMechanism.NewOID(const AObject: IObjectType);
begin
  (Session as ISession).DataFactory.Next(AObject);
end;

procedure TMechanism.SaveObject(const AObject: IObjectType;
  const Meta: IObjectMeta);
begin
  if not (AObject as IObjectState).State.Loaded and
     not (AObject as IObjectState).State.Persisted then
    NewOID(AObject);
end;

procedure TMechanism.SetConnected(const Value: boolean);
begin
  FConnected:= Value;
end;

procedure TMechanism.SetLazyLoad(const Value: boolean);
begin
  FLazyLoad:= True;
end;

procedure TMechanism.SetOnlyModified(const Value: boolean);
begin
  FOnlyModified:= Value;
end;

procedure TMechanism.SetSession(const Value: IInterface);
begin
  FPersister:= Pointer(Value);
end;


end.


