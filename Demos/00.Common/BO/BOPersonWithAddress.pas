unit BOPersonWithAddress;

interface

uses
  JazzValueTypeIntf,
  JazzTypeInfo,
  BOPersonWithAddressIntf,
  BOPerson,
  BOAddress;

type
  TPersonWithAddress = class(TPerson, IPersonWithAddress)
  private
    FAddressList: IObjectListType;
    FCredit: ICurrencyType;
    FIsActive: IBooleanType;
    FLastUpdate: IDateType;
  protected
    function GetAddressList: IObjectListType;
    function GetCredit: Currency;
    function GetIsActive: boolean;
    function GetLastUpdate: TDateTime;
    procedure SetCredit(const Value: Currency);
    procedure SetIsActive(const Value: boolean);
    procedure SetLastUpdate(const Value: TDateTime);

    property AddressList: IObjectListType read GetAddressList;
    property Credit: Currency read GetCredit write SetCredit;
    property IsActive: boolean read GetIsActive write SetIsActive;
    property LastUpdate: TDateTime read GetLastUpdate write SetLastUpdate;
  public
    procedure InitInstance; override;
  end;

implementation

uses
  JazzValueType;

procedure TPersonWithAddress.InitInstance;
begin
  inherited;
  AddMember(FAddressList, 'AddressList', TObjectListType, TAddress);
  AddMember(FCredit, 'Credit', TCurrencyType);
  AddMember(FIsActive, 'IsActive', TBooleanType);
  AddMember(FLastUpdate, 'LastUpdate', TDateType);
end;

function TPersonWithAddress.GetAddressList: IObjectListType;
begin
  Result:= FAddressList;
end;

function TPersonWithAddress.GetCredit: Currency;
begin
  Result:= FCredit.Value;
end;

function TPersonWithAddress.GetIsActive: boolean;
begin
  Result:= FIsActive.Value;
end;

function TPersonWithAddress.GetLastUpdate: TDateTime;
begin
  Result:= FLastUpdate.Value;
end;

procedure TPersonWithAddress.SetCredit(const Value: Currency);
begin
  FCredit.Value:= Value;
end;

procedure TPersonWithAddress.SetIsActive(const Value: boolean);
begin
  FIsActive.Value:= Value;
end;

procedure TPersonWithAddress.SetLastUpdate(const Value: TDateTime);
begin
  FLastUpdate.Value:= Value;
end;

end.

