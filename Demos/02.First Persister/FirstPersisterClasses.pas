unit FirstPersisterClasses;

interface

uses
  JazzValueType,
  JazzValueTypeIntf,
  Classes,
  FirstPersisterClassesIntf;
  
type
  TFirstPersisterType = class(TObjectType, IFirstPersisterType)
  private
    FActiveSession: IStringType;
    FFileName: IStringType;
    FHistory: IMemoType;
    FLogHistory: IBooleanType;
    
    FPersonListMaster: IObjectListType;
    FAddressListDetail: IObjectListType;
  protected
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
  public
    constructor Create; reintroduce; virtual;
  end;

implementation

uses
  BOPersonWithAddress,
  BOAddress;


{ TFirstPersisterType }

constructor TFirstPersisterType.Create;
begin
  inherited Create(nil);
  // create members
  AddMember(FActiveSession, 'ActiveSession', TStringType);
  AddMember(FFileName, 'FileName', TStringType);
  AddMember(FHistory, 'History', TMemoType);
  AddMember(FLogHistory, 'LogHistory', TBooleanType);

  AddMember(FPersonListMaster, 'PersonListMaster', TObjectListType, TPersonWithAddress);
  AddMember(FAddressListDetail, 'AddressListDetail', TObjectListType, TAddress);

  LogHistory:= True;
end;

function TFirstPersisterType.GetAddressListDetail: IObjectListType;
begin
  Result:= FAddressListDetail;
end;

function TFirstPersisterType.GetPersonListMaster: IObjectListType;
begin
  Result:= FPersonListMaster;
end;

procedure TFirstPersisterType.SetFileName(const Value: string);
begin
  FFileName.Value:= Value;
end;

function TFirstPersisterType.GetFileName: string;
begin
  Result:= FFileName.Value;
end;

function TFirstPersisterType.GetHistory: TStrings;
begin
  Result:= FHistory.Value; 
end;

procedure TFirstPersisterType.SetHistory(const Value: TStrings);
begin
  FHistory.Value:= Value;
end;

function TFirstPersisterType.GetActiveSession: string;
begin
  Result:= FActiveSession.Value;
end;

procedure TFirstPersisterType.SetActiveSession(const Value: string);
begin
  FActiveSession.Value:= Value;
end;

function TFirstPersisterType.GetLogHistory: boolean;
begin
  Result:= FLogHistory.Value;
end;

procedure TFirstPersisterType.SetLogHistory(const Value: boolean);
begin
  FLogHistory.Value:= Value;
end;

end.
