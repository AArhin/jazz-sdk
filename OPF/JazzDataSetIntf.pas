unit JazzDataSetIntf;

interface

uses
  Classes,
  DB,
  JazzCriteriaIntf,
  JazzPersisterIntf,
  JazzMappingIntf,
  JazzValueTypeIntf,
  JazzMechanismIntf;

type
  TCommandType = (
    ctNone,                                 
    ctSelect,
    ctSelectCount,
    ctDelete,
    ctInsert,
    ctUpdate,
    ctStoredProcedure
  );

  IObjectDataSet = interface(IInterface)
    ['{5C1C6202-7B65-47DF-AEFC-29C1117D46A6}']
    function GetCriteria: ICriteria;
    function GetHandler: TObject;
    function GetMechanism: IMechanism;
    function GetMeta: IObjectMeta;
    function GetOwnHandler: boolean;
    function GetParams: TParams;
    procedure SetCriteria(const Value: ICriteria);
    procedure SetMechanism(const Value: IMechanism);
    procedure SetMeta(const Value: IObjectMeta);
    procedure SetOwnHandler(const Value: boolean);
    procedure SetParamList(const ParamList: IObjectType; const CommandType: TCommandType);
    procedure SetStatement(const Value: string);

    procedure SetParamValue(Param: TParam; const Member: IMemberType);
    procedure LoadBlobParam(Param: TParam; const Member: IMemberType);

    function CreateHandler: TObject; 

    property Criteria: ICriteria read GetCriteria write SetCriteria;
    property Handler: TObject read GetHandler;
    property Mechanism: IMechanism read GetMechanism write SetMechanism;
    property Meta: IObjectMeta read GetMeta write SetMeta;
    property OwnHandler: boolean read GetOwnHandler write SetOwnHandler;
    property Params: TParams read GetParams;
  end;

  IObjectCommand = interface(IObjectDataSet)
    ['{F77BE1E2-3082-4893-9639-2CB544A0BDAB}']
    function GetRowsAffected: Integer;
    procedure SetRowsAffected(const Value: Integer); 
    function ExecuteCommand: boolean;
    function Execute(const SQL: string): boolean; overload;
    function Execute(const AObject: IObjectType; const CommandType: TCommandType): boolean; overload;
    property RowsAffected: Integer read GetRowsAffected write SetRowsAffected;
  end;

  IObjectQuery = interface(IObjectDataSet)
    ['{9166AA50-643C-4B91-BACD-CC9B3B67D760}']
    function GetActive: boolean;
    function GetFieldList: TStrings;
    function GetRecordCount: Integer;
    function GetCurrentLoaded: IObjectType;
    procedure SetActive(const Value: boolean);
    procedure SetCurrentLoaded(const Value: IObjectType);

    function GetCurrentFromCache: IObjectType;
    function GetFieldValue(const FieldName: string): Variant;

    function GetFieldCount: Integer;
    procedure FieldIndexToMember(const AObject: IObjectType; const FieldIndex: Integer);
    procedure FieldToMember(const AObject: IObjectType; const MemberMeta: IMemberMeta);

    procedure LoadMemberValue(const Member: IMemberType; const NewValue: Variant);
    procedure LoadObject(const AObject: IObjectType);
    procedure RowToObject(const AObject: IObjectType);

    procedure Open;
    procedure Close;

    function EOF: boolean;
    function GetIsEmpty: boolean;

    procedure First;
    procedure Next;

    property Active: boolean read GetActive write SetActive;
    property IsEmpty: boolean read GetIsEmpty;
    property CountRecord: Integer read GetRecordCount;
    property CurrentLoaded: IObjectType read GetCurrentLoaded write SetCurrentLoaded;
    property FieldList: TStrings read GetFieldList;
  end;

  IDatabaseParamList = interface(IObjectType)
    ['{D28356CD-FE80-4395-9CCA-C541FEC2D3F6}']
    function Add(const Name: string; const ParamType: TItemType): IMemberType;
  end;

implementation

end.
