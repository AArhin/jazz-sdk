unit ClassesMapping;

interface

uses
  JazzMapping,
  JazzMappingIntf;

type
  TPessoaMapping = class(TMappingLoader)
  protected
    procedure Execute(const Mapping: IMapping); override;
  end;

implementation

uses
  ClassesImpl;

{ TPessoaMapping }

procedure TPessoaMapping.Execute(const Mapping: IMapping);
begin
  inherited;
  
  with Mapping do
  begin
    with AddObject(TEndereco, 'ENDERECO') do
    begin
      with Add('ID', 'ID') do
      begin
        Size:= 38;
        IsOID:= True;
      end;

      Add('Rua', 'RUA', itString, 100);
      Add('Numero', 'NUMERO', itInteger);
      Add('Bairro', 'BAIRRO', itString, 50);
      Add('Cidade', 'CIDADE', itString, 100);
      Add('UF', 'UF', itString, 2);
      Add('IdOwner', 'ID_OWNER', itString, 38);
    end;

    with AddObject(TPessoa, 'PESSOA') do
    begin
      with Add('ID', 'ID') do
      begin
        Size:= 38;
        IsOID:= True;
      end;

      with Add('Nome', 'NOME') do
      begin
        Required:= True;
        Size:= 100;
      end;

      Add('DataNascimento', 'DATA_NASCIMENTO', itTimeStamp);
      with Add('Foto', 'FOTO', itImage) do
      begin
        LazyLoad:= False;
      end;

      with AddRelationship do
      begin
        ToClassName  := 'TEndereco';           // detail class name
        MasterAttribute:= 'EnderecoList';      // master attribute

        FromMembers.Add('ID');                 // master members relationship key - SQL WHERE clause
        ToMembers.Add('IdOwner');              // detail members relationship key - SQL WHERE clause

        CascadeDelete:= True;                  // default False
        CascadeLoad:= True;                    // default False
        CascadeSave:= True;                    // default False

        RelationshipType:= rtOneToMany;        // Default rtOneToMany: master-detail way
      end;
    end;
  end;
end;

initialization
  RegisterMapping(TPessoaMapping);

end.
