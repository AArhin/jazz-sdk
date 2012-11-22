unit BOPersonMapping;

interface

uses
  JazzMapping,
  JazzMappingIntf;

type
  TPersonMapping = class(TMappingLoader)
  protected
    procedure Execute(const Mapping: IMapping); override;
  end;

implementation

{ TPersonMapping }

procedure TPersonMapping.Execute(const Mapping: IMapping);
begin
  inherited;
  with Mapping do
  begin
    with AddObject('TPerson', 'PERSON') do
    begin
      with Add('ID', 'ID') do
      begin
        Size:= 38;
        IsOID:= True;
      end;

      with Add('Name', 'NAME') do
      begin
        Required:= True;
        Size:= 100;
      end;

      with Add('BirthDate', 'BIRTH_DATE') do
      begin
        ColumnType:= itTimeStamp;
      end;

      with Add('Document', 'DOCUMENT') do
      begin
        Size:= 30;
      end;

      with Add('Picture', 'PICTURE') do
      begin
        ColumnType:= itBlob;
      end;
    end;

    with AddObject('TPersonWithAddress', 'CUSTOMER') do
    begin
      Add('Credit', 'CREDIT', itCurrency);
      Add('IsActive', 'IS_ACTIVE', itInteger);
      Add('LastUpdate', 'LAST_UPDATE', itTimeStamp);

      with AddRelationship do
      begin
        ToClassName  := 'TAddress';            // detail class name
        MasterAttribute:= 'AddressList';       // master attribute

        FromMembers.Add('ID');                 // master members relationship key - SQL WHERE clause
        ToMembers.Add('IDOwner');              // detail members relationship key - SQL WHERE clause

        CascadeDelete:= True;                  // default False
        CascadeLoad:= True;                    // default False
        CascadeSave:= True;                    // default False

        ForeignKeyName:= 'FK_ADDRESS_IDOWNER'; // Generate schema will create the SQL to create FK, if you dont want, just let empty
        RelationshipType:= rtOneToMany;        // Default rtOneToMany: master-detail way
      end;
    end;
  end;
end;

initialization
  RegisterMapping(TPersonMapping);

end.

