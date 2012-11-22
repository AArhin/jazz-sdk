unit BOAddressMapping;

interface

uses
  JazzMapping,
  JazzMappingIntf;

type
  TAddressMapping = class(TMappingLoader)
  protected
    procedure Execute(const Mapping: IMapping); override;
  end;

implementation

{ TPersonMapping }

procedure TAddressMapping.Execute(const Mapping: IMapping);
begin
  inherited;
  with Mapping do
  begin
    with AddObject('TAddress', 'ADDRESS') do
    begin
      with Add('ID', 'ID') do
      begin
        Size:= 38;
        IsOID:= True;
      end;

      with Add('IDOwner', 'IDOWNER') do
      begin
        Size:= 38;
        Required:= True;
      end;

      with Add('Address1', 'ADDRESS1') do
      begin
        Size:= 100;
        Required:= True;
      end;

      with Add('Address2', 'ADDRESS2') do
      begin
        Size:= 100;
      end;

      with Add('City', 'CITY') do
      begin
        Size:= 50;
        Required:= True;
      end;

      with Add('AddressState', 'ADDRESS_STATE') do
      begin
        Size:= 2;
        Required:= True;
      end;

      with Add('PostalCode', 'POSTAL_CODE') do
      begin
        Size:= 10;
      end;

      with Add('Country', 'COUNTRY') do
      begin
        Size:= 50;
        Required:= True;
      end;
    end;
  end;
end;

initialization
  RegisterMapping(TAddressMapping);

end.

