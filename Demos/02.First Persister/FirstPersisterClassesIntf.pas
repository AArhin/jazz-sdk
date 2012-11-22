unit FirstPersisterClassesIntf;

interface

uses
  JazzValueTypeIntf,
  Classes;

type
  IFirstPersisterType = interface(IObjectType)
    ['{68DF8202-9D48-4FC8-A25F-E913BA868E62}']
    function GetActiveSession: string;
    function GetAddressListDetail: IObjectListType;
    function GetFileName: string;
    function GetHistory: TStrings;
    function GetLogHistory: boolean;
    function GetPersonListMaster: IObjectListType;
    procedure SetActiveSession(const Value: string);
    procedure SetFileName(const Value: string);
    procedure SetHistory(const Value: TStrings);
    procedure SetLogHistory(const Value: boolean);

    property ActiveSession: string read GetActiveSession write SetActiveSession;
    property FileName: string read GetFileName write SetFileName;
    property History: TStrings read GetHistory write SetHistory;
    property LogHistory: boolean read GetLogHistory write SetLogHistory;

    property PersonListMaster: IObjectListType read GetPersonListMaster;
    property AddressListDetail: IObjectListType read GetAddressListDetail;
  end;

implementation

end.
