unit BOAddress;

interface

uses JazzValueTypeIntf, JazzValueType, JazzTypeInfo, BOAddressIntf;

type
  TAddress = class(TObjectType, IAddress)
  private
    FAddress1: IStringType;
    FAddress2: IStringType;
    FCity: IStringType;
    FCountry: IStringType;
    FID: IStringType;
    FIDOwner: IStringType;
    FPostalCode: IStringType;
    FAddressState: IStringType;
  protected
    function GetAddress1: string;
    function GetAddress2: string;
    function GetAddressState: string;
    function GetCity: string;
    function GetCountry: string;
    function GetID: string;
    function GetIDOwner: string;
    function GetPostalCode: string;
    procedure SetAddress1(const Value: string);
    procedure SetAddress2(const Value: string);
    procedure SetAddressState(const Value: string);
    procedure SetCity(const Value: string);
    procedure SetCountry(const Value: string);
    procedure SetID(const Value: string);
    procedure SetIDOwner(const Value: string);
    procedure SetPostalCode(const Value: string);

    property ID: string read GetID write SetID;
    property IDOwner: string read GetIDOwner write SetIDOwner;
    property Address1: string read GetAddress1 write SetAddress1;
    property Address2: string read GetAddress2 write SetAddress2;
    property City: string read GetCity write SetCity;
    property AddressState: string read GetAddressState write SetAddressState;
    property PostalCode: string read GetPostalCode write SetPostalCode;
    property Country: string read GetCountry write SetCountry;
  public
    procedure InitInstance; override;
  end;

implementation

{ TAddress }

function TAddress.GetAddress1: string;
begin
  Result:= FAddress1.Value;
end;

function TAddress.GetAddress2: string;
begin
  Result:= FAddress2.Value;
end;

function TAddress.GetCity: string;
begin
  Result:= FCity.Value;
end;

function TAddress.GetCountry: string;
begin
  Result:= FCountry.Value;
end;

function TAddress.GetID: string;
begin
  Result:= FID.Value;
end;

function TAddress.GetPostalCode: string;
begin
  Result:= FPostalCode.Value;
end;

function TAddress.GetAddressState: string;
begin
  Result:= FAddressState.Value;
end;

procedure TAddress.SetAddress1(const Value: string);
begin
  FAddress1.Value:= Value;
end;

procedure TAddress.SetAddress2(const Value: string);
begin
  FAddress2.Value:= Value;
end;

procedure TAddress.SetCity(const Value: string);
begin
  FCity.Value:= Value;
end;

procedure TAddress.SetCountry(const Value: string);
begin
  FCountry.Value:= Value;
end;

procedure TAddress.SetID(const Value: string);
begin
  FID.Value:= Value;
end;

procedure TAddress.SetPostalCode(const Value: string);
begin
  FPostalCode.Value:= Value;
end;

procedure TAddress.SetAddressState(const Value: string);
begin
  FAddressState.Value:= Value;
end;

function TAddress.GetIDOwner: string;
begin
  Result:= FIDOwner.Value;
end;

procedure TAddress.SetIDOwner(const Value: string);
begin
  FIDOwner.Value:= Value;
end;

procedure TAddress.InitInstance;
begin
  inherited;
  AddMember(FID, 'ID', TStringType);
  AddMember(FIDOwner, 'IDOwner', TStringType);
  AddMember(FAddress1, 'Address1', TStringType);
  AddMember(FAddress2, 'Address2', TStringType);
  AddMember(FCity, 'City', TStringType);
  AddMember(FAddressState, 'AddressState', TStringType);
  AddMember(FPostalCode, 'PostalCode', TStringType);
  AddMember(FCountry, 'Country', TStringType);
end;

end.


